------------------------------------------------------------------------------
--                              C O D E P E E R                             --
--                                                                          --
--                     Copyright (C) 2008-2026, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- The CodePeer technology was originally developed by SofCheck, Inc.       --
------------------------------------------------------------------------------

with Ada.Text_IO;    use Ada.Text_IO;

with Sax.Readers;    use Sax.Readers;
with Sax.Attributes; use Sax.Attributes;
with Unicode.CES;
with Input_Sources;
with Input_Sources.File;

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with GNAT.OS_Lib;    use GNAT.OS_Lib;

package body BT.Xml.Reader is

   use Ada.Containers;

   Debug_On : constant Boolean := False;

   Inspection_Output_Directory : Unbounded_String := Null_Unbounded_String;
   --  Save a copy for use with subsequent XML files.

   function Get_Variable_Vn_Value
     (File          : String;
      Variable      : String;
      Srcpos        : Source_Position;
      Closest_Match : out Source_Position) return String;
   --  Returns the value_set associated with Variable on the same line
   --  as Srcpos, and the closest column available.

   function Hash (Key_Type : Natural) return Hash_Type
     is (Hash_Type (Key_Type));
   --  Hash function on the VN_Id

   function Hash (Key_Type : Source_Position) return Hash_Type;
   --  Hash function on source locations

   --  mapping srcpos -> (vn_image, vn_vals)
   package Srcpos_Vals_Mappings is new Ada.Containers.Hashed_Maps
     (Key_Type        => Source_Position,
      Element_Type    => Vn_Values_Seqs.Vector,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => Vn_Values_Seqs."=");  --  elements "="

   File_Vals : Srcpos_Vals_Mappings.Map;

   package Name_Set is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   Files_Read : Name_Set.List;
   --  Keep track of XML files that have been read

   --  extra precondition_event information
   type Callee_Info_Record is record
      Bt_Id            : Natural;
      Callee_File_Name : Unbounded_String;
      Callee_Name      : Unbounded_String;
      Callee_Vn        : Natural;
      Callee_Pre_Index : Natural;
      Callee_Srcpos    : Source_Position;
   end record;

   package Callee_Mappings is new Ada.Containers.Hashed_Maps
     (Key_Type        => Natural,
      Element_Type    => Callee_Info_Record,
      Hash            => Hash,
      Equivalent_Keys => "=");

   Callee_Mapping : Callee_Mappings.Map;

   type Vals_Reader is new Sax.Readers.Reader with record
      File_Name    : Unbounded_String;
      Current_Proc : Unbounded_String;
      Current_Src  : Source_Position;
      Current_Vals : Vn_Values_Seqs.Vector;
   end record;

   overriding procedure Start_Element
     (Self          : in out Vals_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence;
      Attrs         : Sax.Attributes.Attributes'Class);

   overriding procedure End_Element
     (Self          : in out Vals_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence);

   procedure Read_File_Vals_Xml
     (Output_Dir  : String;
      File_Name   : String;
      File_Exists : out Boolean);

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Inspection_Output_Directory := Null_Unbounded_String;
      File_Vals.Clear;
      Files_Read.Clear;
      Callee_Mapping.Clear;
   end Clear;

   ----------
   -- Hash --
   ----------

   function Hash (Key_Type : Source_Position) return Hash_Type is
   begin
      return Hash_Type (Key_Type.Line + Key_Type.Column);
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Output_Directory : String) is
   begin
      Inspection_Output_Directory := To_Unbounded_String (Output_Directory);
   end Initialize;

   overriding procedure Start_Element
     (Self          : in out Vals_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence;
      Attrs         : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Namespace_URI, Local_Name);
   begin
      if Qname = File_Tag then
         declare
            File_Name : constant String := Attrs.Get_Value (Name_Attribute);
         begin
            if Debug_On then
               Put_Line ("reading xml for file " & File_Name);
            end if;

            Set_Unbounded_String (Self.File_Name, File_Name);
         end;

      elsif Qname = Proc_Tag then
         null;

      elsif Qname = Srcpos_Tag then
         declare
            type Source_Attribute_Enum is (Line, Col);
         begin
            for J in 0 .. Get_Length (Attrs) - 1 loop
               declare
                  Attr_Value : constant String := Get_Value (Attrs, J);
               begin
                  case Source_Attribute_Enum'Value (Get_Qname (Attrs, J)) is
                  when Line =>
                     Self.Current_Src.Line := Positive'Value (Attr_Value);
                  when Col =>
                     Self.Current_Src.Column := Positive'Value (Attr_Value);
                  end case;
               end;
            end loop;
         end;
      elsif Qname = Vn_Tag then
         declare
            type Source_Attribute_Enum is (Name, Vals);
            Vn_Vals : Vn_Values;
         begin
            for J in 0 .. Get_Length (Attrs) - 1 loop
               declare
                  Attr_Value : constant String := Get_Value (Attrs, J);
               begin
                  case Source_Attribute_Enum'Value (Get_Qname (Attrs, J)) is
                  when Name =>
                     Vn_Vals.Vn_Image := To_Unbounded_String (Attr_Value);
                  when Vals =>
                     Vn_Vals.Set_Image := To_Unbounded_String (Attr_Value);
                  end case;
               end;
            end loop;
            Vn_Values_Seqs.Append (Self.Current_Vals, Vn_Vals);
         end;
      else
         null;
      end if;
   end Start_Element;

   -------------------
   -- End_Element --
   -------------------

   overriding procedure End_Element
     (Self          : in out Vals_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Namespace_URI, Local_Name);
   begin
      if Qname = File_Tag then
         --  we are done
         null;
      elsif Qname = Srcpos_Tag then
         --  store seq of vals into mapping
         pragma Assert (Self.Current_Src.Line /= 0);
         File_Vals.Include
           (Self.Current_Src, Self.Current_Vals);
         Self.Current_Vals := Vn_Values_Seqs.Empty_Vector;
         Self.Current_Src.Line := 0;

      else
         null;
      end if;
   end End_Element;

   procedure Read_File_Vals_Xml
     (Output_Dir  : String;
      File_Name   : String;
      File_Exists : out Boolean)
   is
      use Input_Sources.File;

      File_To_Read : constant String :=
        BT.Xml.Xml_Vals_File_Name (Output_Dir, File_Name);

      Input  : Input_Sources.File.File_Input;
      Reader : Vals_Reader;

   begin
      if not Is_Regular_File (File_To_Read) then
         File_Exists := False;
         return;
      end if;

      File_Exists := True;

      if Files_Read.Contains (File_To_Read) then
         --  already processed
         return;
      end if;

      Input_Sources.Set_Public_Id
        (Input_Sources.Input_Source (Input), File_To_Read);
      Input_Sources.Set_System_Id
        (Input_Sources.Input_Source (Input), File_To_Read);
      Input_Sources.File.Open (File_To_Read, Input);
      Set_Feature (Reader, Validation_Feature, False);
      Parse (Reader, Input);
      Close (Input);
      Files_Read.Append (File_To_Read);
   end Read_File_Vals_Xml;

   procedure Read_Xml_File (File_Name : String;
      File_Exists : out Boolean);
   --  Read the *_vals XML file corresponding to File_Name if it has not yet
   --  been read

   -------------------
   -- Read_Xml_File --
   -------------------

   procedure Read_Xml_File (File_Name : String;
      File_Exists : out Boolean) is
   begin
      if not Files_Read.Contains (
         BT.Xml.Xml_Vals_File_Name (
            To_String (Inspection_Output_Directory),
            File_Name))
      then
         Read_File_Vals_Xml
            (To_String (Inspection_Output_Directory),
               File_Name,
               File_Exists);
      else
         File_Exists := True;
      end if;
   end Read_Xml_File;

   ---------------------------
   -- Get_Variable_Vn_Value --
   ---------------------------

   function Get_Variable_Vn_Value
     (File          : String;
      Variable      : String;
      Srcpos        : Source_Position;
      Closest_Match : out Source_Position) return String
   is
      File_Exists  : Boolean;
      Curr         : Srcpos_Vals_Mappings.Cursor;
      Pos          : Source_Position;
      D            : Natural;
      Min_Distance : Natural := Integer'Last;
      Set_Image    : Unbounded_String;

      function Distance (A, B : Integer) return Natural is (abs (A - B));
      --  Return the distance between A and B

   begin
      --  Make sure we have read the corresponding Xml file
      Read_Xml_File (File, File_Exists);

      Closest_Match := No_Source_Position;

      if not File_Exists then
         if Debug_On then
            Put_Line ("Get_Variable_Vn_Value ("
              & Variable & "), no values found for file "
              & File);
         end if;

         return "";
      end if;

      if Debug_On then
         Put_Line ("Get_Variable_Vn_Value for " & Variable &
           " at" &
            Integer'Image (Srcpos.Line) & ":" &
            Integer'Image (Srcpos.Column) & ":");
      end if;

      Curr := Srcpos_Vals_Mappings.First (File_Vals);

      while Srcpos_Vals_Mappings.Has_Element (Curr) loop
         Pos := Srcpos_Vals_Mappings.Key (Curr);

         if Pos.Line = Srcpos.Line then
            for Info of Srcpos_Vals_Mappings.Element (Curr) loop
               if Info.Vn_Image = Variable then
                  if Pos.Column = Srcpos.Column then
                     --  exact match
                     if Debug_On then
                        Put_Line ("found an exact match: " &
                          To_String (Info.Set_Image));
                     end if;

                     Closest_Match := Srcpos;
                     return To_String (Info.Set_Image);
                  else
                     D := Distance (Pos.Column, Srcpos.Column);

                     if D < Min_Distance then
                        if Debug_On then
                           Put_Line ("found a match at" &
                             Integer'Image (Pos.Line) & ":" &
                             Integer'Image (Pos.Column) & ": " &
                             To_String (Info.Set_Image));
                        end if;

                        Closest_Match := Srcpos;
                        Min_Distance := D;
                        Set_Image := Info.Set_Image;
                     end if;
                  end if;
               end if;
            end loop;
         end if;

         Curr := Srcpos_Vals_Mappings.Next (Curr);
      end loop;

      return To_String (Set_Image);
   end Get_Variable_Vn_Value;

   --------------------------
   -- Get_Srcpos_Vn_Values --
   --------------------------

   function Get_Srcpos_Vn_Values
     (File_Name : String;
      Line      : Line_Number) return Vn_Values_Seqs.Vector is

      File_Exists  : Boolean;
      Curr         : Srcpos_Vals_Mappings.Cursor;
      Result       : Vn_Values_Seqs.Vector;

   begin
      --  Make sure we have read the corresponding Xml file
      Read_Xml_File (File_Name, File_Exists);

      if not File_Exists then
         return Vn_Values_Seqs.Empty_Vector;
      end if;

      Curr := Srcpos_Vals_Mappings.First (File_Vals);

      while Srcpos_Vals_Mappings.Has_Element (Curr) loop
         if Srcpos_Vals_Mappings.Key (Curr).Line = Line then
            --  Collect values on this line
            for Info of Srcpos_Vals_Mappings.Element (Curr) loop
               Vn_Values_Seqs.Append (Result, Info);
            end loop;
         end if;

         Curr := Srcpos_Vals_Mappings.Next (Curr);
      end loop;

      return Result;
   end Get_Srcpos_Vn_Values;

   function Get_Srcpos_Vn_Values
     (File_Name : String;
      Srcpos    : Source_Position) return Vn_Values_Seqs.Vector
   is
      File_Exists       : Boolean;
      Line              : constant Line_Number := Srcpos.Line;
      Variables_On_Line : Name_Set.List;
      Curr              : Srcpos_Vals_Mappings.Cursor;
      Result            : Vn_Values_Seqs.Vector;

   begin
      --  Make sure we have read the corresponding Xml file
      Read_Xml_File (File_Name, File_Exists);

      if not File_Exists then
         return Vn_Values_Seqs.Empty_Vector;
      end if;

      --  find the VN images on this line
      Curr := Srcpos_Vals_Mappings.First (File_Vals);
      while Srcpos_Vals_Mappings.Has_Element (Curr) loop
         if Srcpos_Vals_Mappings.Key (Curr).Line = Line then
            for Info of Srcpos_Vals_Mappings.Element (Curr) loop
               declare
                  S : constant String := To_String (Info.Vn_Image);
               begin
                  if not Variables_On_Line.Contains (S) then
                     Variables_On_Line.Append (S);
                  end if;
               end;
            end loop;
         end if;

         Curr := Srcpos_Vals_Mappings.Next (Curr);
      end loop;

      --  now find the value_sets associated with the closest source position
      --  for each of these VNs
      declare
         procedure Find_One_Variable (Position : Name_Set.Cursor);

         -----------------------
         -- Find_One_Variable --
         -----------------------

         procedure Find_One_Variable (Position : Name_Set.Cursor) is
            Var_Name      : constant String := Name_Set.Element (Position);
            Closest_Match : Source_Position;
            Var_Values    : constant String :=
              Get_Variable_Vn_Value
                (File_Name, Var_Name, Srcpos, Closest_Match);

         begin
            if Debug_On then
               Put_Line ("checking variable " & Var_Name);
            end if;

            if Closest_Match /= No_Source_Position then
               if Debug_On then
                  Put_Line (" => " & Var_Values);
               end if;

               Vn_Values_Seqs.Append
                 (Result,
                  Vn_Values'(To_Unbounded_String (Var_Name),
                             To_Unbounded_String (Var_Values)));
            end if;
         end Find_One_Variable;

      begin
         Name_Set.Iterate (Variables_On_Line, Find_One_Variable'Access);
         return Result;
      end;
   end Get_Srcpos_Vn_Values;

end BT.Xml.Reader;
