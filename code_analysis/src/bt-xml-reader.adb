------------------------------------------------------------------------------
--                              C O D E P E E R                             --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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
with Ada.Strings.Unbounded.Hash_Case_Insensitive;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;

with GNAT.OS_Lib;    use GNAT.OS_Lib;

package body BT.Xml.Reader is

   use Ada.Containers;
   use Message_Kinds;

   Debug_On : constant Boolean := False;

   Inspection_Output_Directory : Unbounded_String := Null_Unbounded_String;
      --  save a copy for use with subsequent XML files.

   function Hash (Key_Type : Natural) return Hash_Type
     is (Hash_Type (Key_Type));
   --  Hash function on the VN_Id

   package VN_To_BT_Mappings is new Ada.Containers.Hashed_Maps
     (Key_Type        => Natural,
      Element_Type    => BT_Info_Seqs.Vector,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => BT_Info_Seqs."=");

   package BT_File_Mappings is new Ada.Containers.Hashed_Maps
     (Key_Type        => Natural,  --  bt_Id
      Element_Type    => Unbounded_String,  --  file name
      Hash            => Hash,
      Equivalent_Keys => "=");

   BT_Files : BT_File_Mappings.Map;

   --  mapping check_id => BE_Subkind
   package Check_Kind_Mappings is new Ada.Containers.Hashed_Maps
     (Key_Type        => Natural,
      Element_Type    => Message_Kinds.BE_Message_Subkind,
      Hash            => Hash,
      Equivalent_Keys => "=");

   --  mapping check_id => readable_subkind
   package Check_Text_Mappings is new Ada.Containers.Hashed_Maps
     (Key_Type        => Natural,
      Element_Type    => Unbounded_String,
      Hash            => Hash,
      Equivalent_Keys => "=");

   --  mapping event_id => Event_Enum
   package Event_Kind_Mappings is new Ada.Containers.Hashed_Maps
     (Key_Type        => Natural,
      Element_Type    => Event_Enum,
      Hash            => Hash,
      Equivalent_Keys => "=");

   package Event_Text_Mappings is new Ada.Containers.Hashed_Maps
     (Key_Type        => Natural,
      Element_Type    => Unbounded_String,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Mapping_Ptr is access VN_To_BT_Mappings.Map;

   --  mapping proc => (vn_id => backtraces)
   package Procs_To_Vns_Mappings is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Mapping_Ptr,
      Hash            => Ada.Strings.Unbounded.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Unbounded.Equal_Case_Insensitive);

   Proc_Vns : Procs_To_Vns_Mappings.Map;

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
      Callee_Srcpos    : Source_Position;
   end record;

   package Callee_Mappings is new Ada.Containers.Hashed_Maps
     (Key_Type        => Natural,
      Element_Type    => Callee_Info_Record,
      Hash            => Hash,
      Equivalent_Keys => "=");

   Callee_Mapping : Callee_Mappings.Map;
   Current_BT_Seq : BT_Info_Seqs.Vector;
   Current_Bt_Id  : Natural := 0;

   function LT (E1, E2 : BT_Info) return Boolean;
   --  compare 2 backtrace records

   package Sort_Backtraces is new BT_Info_Seqs.Generic_Sorting (LT);

   type Backtrace_Reader is new Sax.Readers.Reader with record
      File_Name    : Unbounded_String;
      Current_Proc : Unbounded_String;
      Current_VN   : Natural;
      Check_Kind   : Check_Kind_Mappings.Map;
      Check_Text   : Check_Text_Mappings.Map;
      Event_Kind   : Event_Kind_Mappings.Map;
      Event_Text   : Event_Text_Mappings.Map;
   end record;

   type Vals_Reader is new Sax.Readers.Reader with record
      File_Name    : Unbounded_String;
      Current_Proc : Unbounded_String;
      Current_Src  : Source_Position;
      Current_Vals : Vn_Values_Seqs.Vector;
   end record;

   overriding procedure Start_Element
     (Self          : in out Backtrace_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence;
      Attrs         : Sax.Attributes.Attributes'Class);

   overriding procedure End_Element
     (Self          : in out Backtrace_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence);

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

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Inspection_Output_Directory := Null_Unbounded_String;
      BT_Files.Clear;
      Proc_Vns.Clear;
      File_Vals.Clear;
      Files_Read.Clear;
      Callee_Mapping.Clear;
      Current_BT_Seq.Clear;
      Current_Bt_Id := 0;
   end Clear;

   --------
   -- LT --
   --------

   function LT (E1, E2 : BT_Info) return Boolean is
   begin
      if E1.Event = E2.Event then
         if E1.Kind = E2.Kind then
            if E1.Sloc.Line = E2.Sloc.Line then
               return E1.Sloc.Column < E2.Sloc.Column;
            else
               return E1.Sloc.Line < E2.Sloc.Line;
            end if;
         else
            if E1.Sloc.Line = E2.Sloc.Line then
               return E1.Kind < E2.Kind;
            else
               return E1.Sloc.Line < E2.Sloc.Line;
            end if;
         end if;
      else
         return E1.Event < E2.Event;
      end if;
   end LT;

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

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self          : in out Backtrace_Reader;
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

      elsif Qname = Bt_Tag then
         declare
            BT : BT_Info;
            type BT_Attribute_Enum is (Line, Col, Check, Event);

         begin
            Current_Bt_Id := Current_Bt_Id + 1;
            BT.Bt_Id := Current_Bt_Id;
            --  default
            BT.Event := Check_Event;
            BT.Kind := Module_Annotation;
            for J in 0 .. Get_Length (Attrs) - 1 loop
               declare
                  Attr_Value : constant String := Get_Value (Attrs, J);
               begin
                  case BT_Attribute_Enum'Value (Get_Qname (Attrs, J)) is
                  when Line =>
                     BT.Sloc.Line := Positive'Value (Attr_Value);
                  when Col =>
                     BT.Sloc.Column := Positive'Value (Attr_Value);
                  when Check =>
                     BT.Kind := Self.Check_Kind (Natural'Value (Attr_Value));
                     BT.Text := Self.Check_Text (Natural'Value (Attr_Value));
                  when Event =>
                     BT.Event := Self.Event_Kind (Natural'Value (Attr_Value));
                     BT.Text := Self.Event_Text (Natural'Value (Attr_Value));
                  end case;
               end;
            end loop;
            BT_Info_Seqs.Append (Current_BT_Seq, BT);
            BT_File_Mappings.Insert (BT_Files, BT.Bt_Id, Self.File_Name);
         end;
      elsif Qname = Vn_Tag then
         Current_BT_Seq := BT_Info_Seqs.Empty_Vector;
         Self.Current_VN :=
           Natural'Value (Attrs.Get_Value (Id_Attribute));

      elsif Qname = Proc_Tag then
         declare
            Proc_Name : constant String := Attrs.Get_Value
               (Name_Attribute);
         begin
            if Debug_On then
               Put_Line ("reading proc " & Proc_Name);
            end if;
            Set_Unbounded_String (Self.Current_Proc, Proc_Name);
            if not Procs_To_Vns_Mappings.Contains
              (Proc_Vns, Self.Current_Proc)
            then
               --  TBD: there are instances of 'Elab_Subp_Body that
               --  appear in both the SCIL for the spec and the body. Is it
               --  ok to combine the backtraces?
               Procs_To_Vns_Mappings.Insert
                 (Proc_Vns, Self.Current_Proc, new VN_To_BT_Mappings.Map);
            end if;
         end;

      elsif Qname = Callee_Tag then
         declare
            Info : Callee_Info_Record;
         begin
            Info.Bt_Id := Current_Bt_Id;
            Set_Unbounded_String
              (Info.Callee_Name, Attrs.Get_Value (Name_Attribute));
            Set_Unbounded_String
              (Info.Callee_File_Name, Attrs.Get_Value (File_Name_Attribute));
            Info.Callee_Vn   := Positive'Value
               (Attrs.Get_Value (Vn_Attribute));
            Info.Callee_Srcpos.Line := Positive'Value
               (Attrs.Get_Value (Line_Attribute));
            Info.Callee_Srcpos.Column := Positive'Value
               (Attrs.Get_Value (Col_Attribute));
            Callee_Mappings.Insert (Callee_Mapping,
              Current_Bt_Id, Info);
         end;
      elsif Qname = Checks_Tag then
         --  nothing to do
         null;
      elsif Qname = Check_Tag then
         declare
            Check_Name : constant String := Attrs.Get_Value (Name_Attribute);
            Check_Text : constant String := Attrs.Get_Value (Text_Attribute);
            Check_Id   : constant Natural :=
              Natural'Value (Attrs.Get_Value (Id_Attribute));

         begin
            Self.Check_Kind.Insert
              (Check_Id, Message_Kinds.BE_Message_Subkind'Value (Check_Name));
            Self.Check_Text.Insert
              (Check_Id, To_Unbounded_String (Check_Text));
         end;

      elsif Qname = Event_Tag then
         declare
            Event_Name : constant String := Attrs.Get_Value (Name_Attribute);
            Event_Text : constant String := Attrs.Get_Value (Text_Attribute);
            Event_Id   : constant Natural :=
              Natural'Value (Attrs.Get_Value (Id_Attribute));

         begin
            Self.Event_Kind.Insert (Event_Id, Event_Enum'Value (Event_Name));
            Self.Event_Text.Insert
              (Event_Id, To_Unbounded_String (Event_Text));
         end;
      elsif Qname = Events_Tag then
         --  nothing to do
         null;
      else
         null;
      end if;
   end Start_Element;

   -------------------
   -- End_Element --
   -------------------

   overriding procedure End_Element
     (Self          : in out Backtrace_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Namespace_URI, Local_Name);
   begin
      if Qname = File_Tag then
         --  we are done
         null;
      elsif Qname = Bt_Tag then
         --  nothing to do
         null;
      elsif Qname = Vn_Tag then
         --  store seq of backtraces into mapping
         pragma Assert (Self.Current_VN /= 0);
         Proc_Vns.Element (Self.Current_Proc).Include
           (Self.Current_VN, Current_BT_Seq);
         Current_BT_Seq := BT_Info_Seqs.Empty_Vector;
         Self.Current_VN := 0;

      elsif Qname = Proc_Tag then
         --  nothing to do
         null;
      elsif Qname = Checks_Tag then
         null;
      elsif Qname = Check_Tag then
         null;
      elsif Qname = Events_Tag then
         null;
      elsif Qname = Event_Tag then
         null;
      else
         null;
      end if;
   end End_Element;

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
                     Vn_Vals.Set_Image := To_Unbounded_String
                       (Improve_Number_Readability_In_Messages
                         (Attr_Value, For_HTML_Output => False));
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

   -----------------------------
   -- Read_File_Backtrace_Xml --
   -----------------------------

   procedure Read_File_Backtrace_Xml
     (Output_Dir  : String;
      File_Name   : String;
      File_Exists : out Boolean)
   is
      use Input_Sources.File;

      File_To_Read : constant String :=
        BT.Xml.Xml_File_Name (Output_Dir, File_Name, For_Backtraces => True);

      Input  : Input_Sources.File.File_Input;
      Reader : Backtrace_Reader;

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

      if Inspection_Output_Directory  = Null_Unbounded_String then
         Inspection_Output_Directory := To_Unbounded_String (Output_Dir);
      else
         pragma Assert (Output_Dir = To_String (Inspection_Output_Directory));
         null;
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
   end Read_File_Backtrace_Xml;

   procedure Read_File_Vals_Xml
     (Output_Dir  : String;
      File_Name   : String;
      File_Exists : out Boolean)
   is
      use Input_Sources.File;

      File_To_Read : constant String :=
        BT.Xml.Xml_File_Name (Output_Dir, File_Name, For_Backtraces => False);

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

   -----------------------
   -- Get_Vn_Backtraces --
   -----------------------

   procedure Get_Vn_Backtraces
     (Proc_Name  : String;
      Vn_Id      : Natural;
      Msg_Loc    : Source_Position;
      Backtraces : in out BT.BT_Info_Seqs.Vector)
   is
      Proc : constant Unbounded_String := To_Unbounded_String (Proc_Name);
      All_Checks     : Check_Kinds_Array := Check_Kinds_Array_Default;

      Debug_On : constant Boolean := False;

      procedure Get_Vn_Backtraces_Rec
         (Rec_Proc_Name  : String;
          Vn_Id          : Natural;
          Rec_Backtraces : out BT_Info_Seqs.Vector);
      --  Recursive version.

      procedure Display_BT (Info : BT_Info);
      --  Print out the backtrace for debugging.

      ----------------
      -- Display_BT --
      ----------------

      procedure Display_BT (Info : BT_Info) is
      begin
         Put
            ("     "
               & Natural'Image (Info.Sloc.Line) & ":"
               & Natural'Image (Info.Sloc.Column) & " - "
               & To_String (Info.Text) & " - "
               & BE_Message_Subkind'Image (Info.Kind));
         New_Line;
      end Display_BT;

      procedure Get_Vn_Backtraces_Rec
         (Rec_Proc_Name  : String;
          Vn_Id          : Natural;
          Rec_Backtraces : out BT_Info_Seqs.Vector) is

         Proc : constant Unbounded_String
           := To_Unbounded_String (Rec_Proc_Name);
         VN_BTs : VN_To_BT_Mappings.Cursor;

         This_Level_BTs : BT_Info_Seqs.Vector := BT_Info_Seqs.Empty_Vector;
         use BT_Info_Seqs;

         procedure Add_One_Backtrace (Info : BT_Info);
         --  Add backtrace to the seq of relevant backtrace for this
         --  VN. For precondition events, recursively import the backtraces
         --  contributing to the corresponding precondition.

         -----------------------
         -- Add_One_Backtrace --
         -----------------------

         procedure Add_One_Backtrace (Info : BT_Info) is
         begin
            if BT.BT_Info_Seqs.Contains (Backtraces, Info) then
               if Debug_On then
                  Put ("not adding BT");
                  Display_BT (Info);
               end if;

               return;
            end if;
            Append (This_Level_BTs, Info);

            if Debug_On then
               Put ("Adding BT");
               Display_BT (Info);
            end if;

            if Info.Event = Check_Event then
               All_Checks (Info.Kind) := True;
            end if;
         end Add_One_Backtrace;

         use VN_To_BT_Mappings;

      begin
         if not Procs_To_Vns_Mappings.Contains (Proc_Vns, Proc) then
            --  no known backtraces for this proc.
            return;
         end if;

         VN_BTs := Proc_Vns.Element (Proc).Find (Vn_Id);

         if VN_BTs /= VN_To_BT_Mappings.No_Element then
            for BT of VN_To_BT_Mappings.Element (VN_BTs) loop
               Add_One_Backtrace (BT);
            end loop;
         end if;

         if Debug_On then
            Put_Line ("before sort");
            for Info of This_Level_BTs loop
               Display_BT (Info);
            end loop;
         end if;
         Sort_Backtraces.Sort (This_Level_BTs);

         if Debug_On then
            Put_Line ("after sort");
            for Info of This_Level_BTs loop
               Display_BT (Info);
            end loop;
         end if;

         --  now import the precondition backtraces
         for Info of This_Level_BTs loop
            if not Contains (Rec_Backtraces, Info) then
               Append (Rec_Backtraces, Info);
            end if;
            if Info.Event = Precondition_Event then
               declare
                  Callee_BTs : BT.BT_Info_Seqs.Vector;
                  Callee_File_Name : constant String :=
                    Get_Callee_File_Name (Info.Bt_Id);
                  Callee_Proc_Name : constant String :=
                    Get_Precondition_Callee_Name (Info.Bt_Id);
                  File_Exists : Boolean;
               begin
                  if not Files_Read.Contains (
                     BT.Xml.Xml_File_Name (
                       To_String (Inspection_Output_Directory),
                       Callee_File_Name,
                       For_Backtraces => True))
                  then
                     Read_File_Backtrace_Xml
                        (To_String (Inspection_Output_Directory),
                         Callee_File_Name,
                         File_Exists);
                  end if;
                  --  Import the backtraces contributing to that
                  --  precondition.
                  if Callee_Proc_Name /= Proc_Name
                    and then Callee_Proc_Name /= Rec_Proc_Name
                  then
                     Get_Vn_Backtraces_Rec
                      (Get_Precondition_Callee_Name (Info.Bt_Id),
                        Get_Precondition_VN (Info.Bt_Id),
                        Callee_BTs);
                     --  Add to list if not already present
                     for Info of Callee_BTs loop
                        if not Contains (Rec_Backtraces, Info) then
                           Append (Rec_Backtraces, Info);
                        end if;
                     end loop;
                  end if;
               end;
            end if;
         end loop;
      end Get_Vn_Backtraces_Rec;

      Primary_Checks : Check_Kinds_Array;

      use BT_Info_Seqs;

   begin
      if not Proc_Vns.Contains (Proc) or else
         not Proc_Vns.Element (Proc).Contains (Vn_Id)
      then
         if Debug_On then
            Put_Line ("No backtraces for this vn " & Natural'Image (Vn_Id)
              & " in " & Proc_Name);
         end if;

         return;
      end if;

      --  Collect all backtraces (including the backtraces imported
      --  at precondition_check events.
      --  Uniquify and sort (by increasing line numbers) before
      --  returning the sequence.
      if Debug_On then
         Put_Line ("get_backtraces for vn " & Natural'Image (Vn_Id)
            & " in " & Proc_Name & ":" & Integer'Image (Msg_Loc.Line)
            & ":" & Integer'Image (Msg_Loc.Column));
      end if;

      if Backtraces /= BT_Info_Seqs.Empty_Vector then
         declare
            New_BTs : BT_Info_Seqs.Vector;
         begin
            Get_Vn_Backtraces_Rec  (Proc_Name, Vn_Id, New_BTs);
            BT_Info_Seqs.Append (Backtraces, New_BTs);
         end;
      else
         Get_Vn_Backtraces_Rec  (Proc_Name, Vn_Id, Backtraces);
      end if;

      --  Cleanup the new sequence by removing duplicates and
      --  backtraces associated with non-primary checks
      Primary_Checks := Primary_Original_Checks (All_Checks);
      if Debug_On then
         Put_Line ("removing primary checks");
         declare
            procedure Display_Checks (Checks : Check_Kinds_Array);

            --------------------
            -- Display_Checks --
            --------------------

            procedure Display_Checks (Checks : Check_Kinds_Array) is
               First : Boolean := True;
            begin
               Put ("(");
               for Check in Check_Kind_Enum loop
                  if Checks (Check) then
                     if not First then
                        Put (", ");
                     else
                        First := False;
                     end if;
                     Put (Check_Kind_Enum'Image (Check));
                  end if;
               end loop;
               Put_Line (")");
            end Display_Checks;
         begin
            Put_Line ("All_Checks => ");
            Display_Checks (All_Checks);

            Put_Line ("Primary_Checks => ");
            Display_Checks (Primary_Checks);
         end;
      end if;

      declare
         Result_BTs : BT_Info_Seqs.Vector;
         Prev       : BT_Info := No_BT_Info;
         Elem       : BT_Info_Seqs.Cursor := Backtraces.First;
      begin
         while BT_Info_Seqs.Has_Element (Elem) loop
            declare
               Info : BT_Info := BT_Info_Seqs.Element (Elem);
            begin
               if not (Info.Event = Prev.Event
                  and then Info.Kind = Prev.Kind
                  and then Info.Sloc.Line = Prev.Sloc.Line)
               then
                  --  we need to add this backtrace, but first check if
                  --  there are multiple similar backtraces for the same
                  --  line. If there are, only add the one corresponding
                  --  to the message column
                  if Info.Sloc.Line = Msg_Loc.Line and then
                     Info.Sloc.Column /= Msg_Loc.Column
                  then
                     --  Is there another backtrace on the same line that
                     --  would be more appropriate?
                     declare
                        Next_Cursor : BT_Info_Seqs.Cursor :=
                           BT_Info_Seqs.Next (Elem);
                        Next_Info : BT_Info;
                     begin
                        while BT_Info_Seqs.Has_Element (Next_Cursor) loop
                           Next_Info := BT_Info_Seqs.Element (Next_Cursor);
                           if Next_Info.Event = Info.Event
                              and then Next_Info.Kind = Info.Kind
                              and then Next_Info.Sloc.Line = Info.Sloc.Line
                           then
                              if Next_Info.Sloc.Column = Msg_Loc.Column then
                                 Info := Next_Info;
                                 Elem := Next_Cursor;
                              end if;
                              Elem := Next_Cursor;
                           else
                              exit;
                           end if;
                           Next_Cursor := BT_Info_Seqs.Next (Next_Cursor);
                        end loop;
                     end;
                  end if;
                  Prev := Info;

                  if Info.Event /= Check_Event
                     or else Primary_Checks (Info.Kind)
                  then
                     Append (Result_BTs, Info);
                  end if;
               end if;
            end;
            Elem := BT_Info_Seqs.Next (Elem);
         end loop;

         Backtraces := Result_BTs;
      end;
      if Debug_On then
         Put_Line ("after primary and sort");
         for Info of Backtraces loop
            Display_BT (Info);
         end loop;
      end if;
   end Get_Vn_Backtraces;

   ----------------------------------
   -- Get_Precondition_Callee_Name --
   ----------------------------------

   function Get_Precondition_Callee_Name (Bt_Id : Natural) return String is
      Info : constant Callee_Info_Record := Callee_Mapping.Element (Bt_Id);
   begin
      return To_String (Info.Callee_Name);
   end Get_Precondition_Callee_Name;

   -------------------------
   -- Get_Precondition_VN --
   -------------------------

   function Get_Precondition_VN (Bt_Id : Natural) return Natural is
      Info : constant Callee_Info_Record := Callee_Mapping.Element (Bt_Id);
   begin
      return Info.Callee_Vn;
   end Get_Precondition_VN;

   --------------------------
   -- Get_BT_File_Name --
   --------------------------

   function Get_BT_File_Name (Bt_Id : Natural) return String is
   begin
      return To_String (BT_File_Mappings.Element (BT_Files, Bt_Id));
   end Get_BT_File_Name;

   -----------------------
   -- Get_Callee_Srcpos --
   -----------------------

   function Get_Callee_Srcpos (Bt_Id : Natural) return Source_Position is
      Info : constant Callee_Info_Record := Callee_Mapping.Element (Bt_Id);
   begin
      return Info.Callee_Srcpos;
   end Get_Callee_Srcpos;

   --------------------------
   -- Get_Callee_File_Name --
   --------------------------

   function Get_Callee_File_Name (Bt_Id : Natural) return String is
      Info : constant Callee_Info_Record := Callee_Mapping.Element (Bt_Id);
   begin
      return To_String (Info.Callee_File_Name);
   end Get_Callee_File_Name;

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
         BT.Xml.Xml_File_Name (
            To_String (Inspection_Output_Directory),
            File_Name,
            For_Backtraces => False))
      then
         Read_File_Vals_Xml
            (To_String (Inspection_Output_Directory),
               File_Name,
               File_Exists);
      else
         File_Exists := True;
      end if;
   end Read_Xml_File;

   package List_Of_Source_Positions is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Element_Type   => Source_Position,
        "="        => "=");

   package Map_Of_Source_Positions is new
     Ada.Containers.Hashed_Maps
     (Key_Type        => Source_Position,
      Element_Type    => Vn_Values,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");  --  elements "="

   ---------------------------
   -- Get_Variable_Vn_Value --
   ---------------------------

   function Get_Variable_Vn_Value
     (File          : String;
      Variable      : String;
      Srcpos        : Source_Position;
      Closest_Match : out Source_Position) return String
   is
      File_Exists        : Boolean;
      Curr               : Srcpos_Vals_Mappings.Cursor;
      Variable_Positions : List_Of_Source_Positions.List;
      Variable_Vals      : Map_Of_Source_Positions.Map;

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
           " at " &
            Integer'Image (Srcpos.Line) & ":" &
            Integer'Image (Srcpos.Column) & ":" &
           ":");
      end if;
      Curr := Srcpos_Vals_Mappings.First (File_Vals);
      while Srcpos_Vals_Mappings.Has_Element (Curr) loop
         declare
            Pos : constant Source_Position :=
               Srcpos_Vals_Mappings.Key (Curr);

            E  : constant Vector :=
               Srcpos_Vals_Mappings.Element (Curr);

         begin
            for Info of E loop
               if Info.Vn_Image = Variable then
                  if Pos = Srcpos then
                     --  exact match
                     if Debug_On then
                        Put_Line ("Found an exact match : " &
                           To_String (Info.Set_Image));
                     end if;
                     Closest_Match := Srcpos;
                     return To_String (Info.Set_Image);
                  else
                     if Debug_On then
                        Put_Line ("add a possible match at " &
                           Integer'Image (Pos.Line) & ":" &
                           Integer'Image (Pos.Column) & ":" &
                           To_String (Info.Set_Image));
                     end if;
                     Variable_Positions.Append (Pos);
                     Variable_Vals.Include (Pos, Info);
                  end if;
               end if;
            end loop;
         end;
         Curr := Srcpos_Vals_Mappings.Next (Curr);
      end loop;

      --  Check the found values, if any.
      if List_Of_Source_Positions.Is_Empty (Variable_Positions) then
         --  no known values
         if Debug_On then
            Put_Line ("Get_Variable_Vn_Value, "
            & Variable & " was not found");
         end if;
         return "";
      elsif List_Of_Source_Positions.Length (Variable_Positions) = 1 then
         if Debug_On then
            Put_Line ("return only possible match");
         end if;
         Closest_Match := List_Of_Source_Positions.First_Element
            (Variable_Positions);
         return To_String (Variable_Vals.Element
                (List_Of_Source_Positions.First_Element
                  (Variable_Positions)).Set_Image);
      end if;
      --  No exact match, find the closest to Srcpos
      declare
         function "<" (Left, Right : Source_Position) return Boolean;
         --  compare 2 source_positions

         -------
         --  "<"
         -------
         function "<" (Left, Right : Source_Position) return Boolean
         is
         begin
            if Left.Line = Right.Line then
               return Left.Column < Right.Column;
            else
               return Left.Line < Right.Line;
            end if;
         end "<";

         package Sort_Positions is new
              List_Of_Source_Positions.Generic_Sorting ("<");

         Cursor, Prev_Cursor, Next_Cursor : List_Of_Source_Positions.Cursor;
      begin
         Sort_Positions.Sort (Variable_Positions);

         --  Check first if Srcpos is outside the bounds of known values
         if List_Of_Source_Positions.First_Element (Variable_Positions).
            Line > Srcpos.Line
         then
            --  the first value is already past the line we are
            --  interested in, return the first
            Closest_Match := List_Of_Source_Positions.First_Element
               (Variable_Positions);
            if Debug_On then
               Put_Line ("use first entry at " &
                  Integer'Image (Closest_Match.Line) & ":" &
                  Integer'Image (Closest_Match.Column) & " :" &
                  To_String (Variable_Vals.Element
                      (List_Of_Source_Positions.First_Element
                        (Variable_Positions)).Set_Image));
            end if;
            return To_String (Variable_Vals.Element
                (List_Of_Source_Positions.First_Element
                  (Variable_Positions)).Set_Image);
         elsif List_Of_Source_Positions.Last_Element (Variable_Positions).
            Line < Srcpos.Line
         then
            --  the last value is still before the line we are
            --  interested in, return the last
            Closest_Match := List_Of_Source_Positions.Last_Element
               (Variable_Positions);
            if Debug_On then
               Put_Line ("use last entry at " &
                  Integer'Image (Closest_Match.Line) & ":" &
                  Integer'Image (Closest_Match.Column) & " :" &
                  To_String (Variable_Vals.Element
                      (List_Of_Source_Positions.Last_Element
                        (Variable_Positions)).Set_Image));
            end if;
            return To_String (Variable_Vals.Element
               (List_Of_Source_Positions.Last_Element
               (Variable_Positions)).Set_Image);
         end if;

         --  Find the values closest to srcpos. We'll choose the closest
         --  preceding values.
         Cursor := List_Of_Source_Positions.First (Variable_Positions);
         Prev_Cursor := List_Of_Source_Positions.No_Element;
         Next_Cursor := List_Of_Source_Positions.Next (Cursor);
         declare
            use List_Of_Source_Positions;
            Has_Prev : Boolean;
            Has_Next : Boolean;
            Pos, Prev_Pos, Next_Pos : Source_Position := No_Source_Position;
         begin
            while List_Of_Source_Positions.Has_Element (Cursor) loop
               Pos := List_Of_Source_Positions.Element (Cursor);
               Has_Prev := Prev_Cursor /= List_Of_Source_Positions.No_Element
                 and then List_Of_Source_Positions.Has_Element (Prev_Cursor);
               Has_Next := Next_Cursor /= List_Of_Source_Positions.No_Element
                 and then List_Of_Source_Positions.Has_Element (Next_Cursor);
               if Has_Prev then
                  Prev_Pos := List_Of_Source_Positions.Element (Prev_Cursor);
               end if;
               if Has_Next then
                  Next_Pos := List_Of_Source_Positions.Element (Next_Cursor);
               end if;
               if Pos.Line = Srcpos.Line then
                  --  found a possible entry
                  if (Prev_Pos /= No_Source_Position and then
                     Prev_Pos.Line < Srcpos.Line)

                     or else (Next_Pos /= No_Source_Position and then
                     Next_Pos.Line > Srcpos.Line)
                  then
                     --  use entry on the same line
                     if Debug_On then
                        Put_Line ("Only match on line " &
                           Integer'Image (Pos.Line) & ":" &
                           Integer'Image (Pos.Column) & " : " &
                           To_String (Variable_Vals.Element
                              (Pos).Set_Image));
                     end if;
                     Closest_Match := Pos;
                     return To_String (Variable_Vals.Element
                              (Pos).Set_Image);
                  elsif Prev_Pos /= No_Source_Position and then
                     Prev_Pos.Line = Srcpos.Line
                  then
                     if Next_Pos = No_Source_Position or else
                       Next_Pos.Line > Srcpos.Line
                     then
                        --  Choose between Pos and Prev_Pos
                        if (Prev_Pos.Column < Srcpos.Column
                          and then Pos.Column > Srcpos.Column)
                          or else Pos.Column > Srcpos.Column
                        then
                           --  return previous
                           if Debug_On then
                              Put_Line ("Match on same line " &
                                 Integer'Image (Prev_Pos.Line) & ":" &
                                 Integer'Image (Prev_Pos.Column) & " : " &
                                 To_String (Variable_Vals.Element
                                    (Prev_Pos).Set_Image));
                           end if;
                           Closest_Match := Prev_Pos;
                           return To_String (Variable_Vals.Element
                              (Prev_Pos).Set_Image);
                        else
                           --  return current
                           if Debug_On then
                              Put_Line ("Match on same line " &
                                 Integer'Image (Pos.Line) & ":" &
                                 Integer'Image (Pos.Column) & " : " &
                                 To_String (Variable_Vals.Element
                                    (Pos).Set_Image));
                           end if;
                           Closest_Match := Pos;
                           return To_String (Variable_Vals.Element
                              (Pos).Set_Image);
                        end if;
                     end if;
                  elsif Next_Pos.Line > Srcpos.Line then
                     null;
                  else
                     null;
                  end if;
               elsif Pos.Line > Srcpos.Line then
                  --  we passed the desired line.
                  if Prev_Pos /= No_Source_Position and then
                     Prev_Pos.Line < Srcpos.Line
                  then
                     --  return previous
                     if Debug_On then
                        Put_Line ("Match on closest prev line " &
                           Integer'Image (Prev_Pos.Line) & ":" &
                           Integer'Image (Prev_Pos.Column) & " : " &
                           To_String (Variable_Vals.Element
                              (Prev_Pos).Set_Image));
                     end if;
                     Closest_Match := Prev_Pos;
                     return To_String (Variable_Vals.Element
                        (Prev_Pos).Set_Image);
                  end if;
               else
                  --  keep looking
                  null;
               end if;
               Prev_Cursor := Cursor;
               Cursor := List_Of_Source_Positions.Next (Cursor);
               Next_Cursor := List_Of_Source_Positions.Next (Cursor);
            end loop;
         end;
         if Debug_On then
            Put_Line ("Get_Variable_Vn_Value, " & Variable & " was not found");
         end if;
         return "";
      end;
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
         declare
            Pos : constant Source_Position :=
               Srcpos_Vals_Mappings.Key (Curr);

            E  : constant Vector :=
               Srcpos_Vals_Mappings.Element (Curr);

         begin
            if Pos.Line = Line then
               --  Collect values on this line
               for Info of E loop
                  Vn_Values_Seqs.Append (Result, Info);
               end loop;
            end if;
         end;
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

      if not File_Exists or else not File_Vals.Contains (Srcpos) then
         return Vn_Values_Seqs.Empty_Vector;
      end if;

      --  find the VN images on this line
      Curr := Srcpos_Vals_Mappings.First (File_Vals);
      while Srcpos_Vals_Mappings.Has_Element (Curr) loop
         declare
            Pos : constant Source_Position :=
               Srcpos_Vals_Mappings.Key (Curr);

            E  : constant Vector :=
               Srcpos_Vals_Mappings.Element (Curr);
         begin
            if Pos.Line = Line then
               for Info of E loop
                  if not Variables_On_Line.Contains
                    (To_String (Info.Vn_Image))
                  then
                     Variables_On_Line.Append
                       (To_String (Info.Vn_Image));
                  end if;
               end loop;
            end if;
         end;
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
            Var_Name      : constant String :=
               Name_Set.Element (Position);
            Closest_Match : Source_Position;
            Var_Values : constant String :=
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
               Vn_Values_Seqs.Append (Result,
                 (To_Unbounded_String (Var_Name),
                  To_Unbounded_String (Var_Values)));
            end if;
         end Find_One_Variable;

      begin
         Name_Set.Iterate (Variables_On_Line,
           Find_One_Variable'Access);

         return Result;
      end;
   end Get_Srcpos_Vn_Values;

end BT.Xml.Reader;
