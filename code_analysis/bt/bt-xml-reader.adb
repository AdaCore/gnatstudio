------------------------------------------------------------------------------
--                              C O D E P E E R                             --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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
      Callee_Pre_Index : Natural;
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

            type BT_Attribute_Enum is (Line, Col, Check, Event, File_Name);

            BT_File_Name : Unbounded_String := Null_Unbounded_String;
         begin
            Current_Bt_Id := Current_Bt_Id + 1;
            BT.Bt_Id := Current_Bt_Id;
            --  default
            BT.Event := Check_Event;
            BT.Kind := Module_Annotation;

            if Debug_On then
               Put ("BT--> " & Natural'Image (Current_Bt_Id) & ",");
            end if;

            for J in 0 .. Get_Length (Attrs) - 1 loop
               declare
                  Attr_Value : constant String := Get_Value (Attrs, J);
               begin
                  case BT_Attribute_Enum'Value (Get_Qname (Attrs, J)) is
                  when Line =>
                     BT.Sloc.Line := Positive'Value (Attr_Value);

                     if Debug_On then
                        Put (" line:" & Attr_Value);
                     end if;
                  when Col =>
                     BT.Sloc.Column := Positive'Value (Attr_Value);

                     if Debug_On then
                        Put (" col:" & Attr_Value);
                     end if;
                  when Check =>
                     BT.Kind := Self.Check_Kind (Natural'Value (Attr_Value));
                     BT.Text := Self.Check_Text (Natural'Value (Attr_Value));

                     if Debug_On then
                        Put (" kind:" & Attr_Value);
                     end if;
                  when Event =>
                     BT.Event := Self.Event_Kind (Natural'Value (Attr_Value));
                     BT.Text := Self.Event_Text (Natural'Value (Attr_Value));

                     if Debug_On then
                        Put (" event:" & Attr_Value);
                     end if;

                     if BT.Event = Precondition_Event then
                        BT.Kind := Precondition_Check;
                     end if;
                  when File_Name =>
                     --  A file name can be present in cases where the source
                     --  file for check is different from the current unit's
                     --  source file (such as for instantiations, where we want
                     --  the generic unit's source file name).

                     Set_Unbounded_String (BT_File_Name, Attr_Value);

                     if Debug_On then
                        Put_Line (" file:" & Attr_Value);
                     end if;
                  end case;
               end;
            end loop;

            if Debug_On then
               Put_Line ("");
            end if;

            BT_Info_Seqs.Append (Current_BT_Seq, BT);
            if BT_File_Name = Null_Unbounded_String then
               BT_File_Mappings.Insert (BT_Files, BT.Bt_Id, Self.File_Name);
            else
               BT_File_Mappings.Insert (BT_Files, BT.Bt_Id, BT_File_Name);
            end if;
         end;
      elsif Qname = Vn_Tag then
         Current_BT_Seq := BT_Info_Seqs.Empty_Vector;
         Self.Current_VN :=
           Natural'Value (Attrs.Get_Value (Id_Attribute));

         if Debug_On then
            Put_Line ("VN--> " & Natural'Image (Self.Current_VN));
         end if;

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
            Info.Callee_Pre_Index := Positive'Value
               (Attrs.Get_Value (Pre_Index_Attribute));
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

   ------------------------------
   -- Read_File_Backtraces_Xml --
   ------------------------------

   procedure Read_File_Backtrace_Xml
     (Output_Dir  : String;
      File_Name   : String;
      File_Exists : out Boolean)
   is
      use Input_Sources.File;

      File_To_Read : constant String :=
        BT.Xml.Xml_BT_File_Name (Output_Dir, File_Name);

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

   -----------------------
   -- Get_Vn_Backtraces --
   -----------------------

   procedure Get_Vn_Backtraces
     (Proc_Name     : String;
      Vn_Id         : Natural;
      Msg_Loc       : Source_Position;
      Msg_Kind      : Message_Kinds.BE_Message_Subkind;
      Precond_Index : Natural := 0;
      Backtraces    : in out BT.BT_Info_Seqs.Vector)
   is
      Proc : constant Unbounded_String := To_Unbounded_String (Proc_Name);
      Top_Level : Boolean := True;

      Debug_On : constant Boolean := False;

      procedure Get_Vn_Backtraces_Rec
         (Rec_Proc_Name  : String;
          Vn_Id          : Natural;
          Rec_Backtraces : out BT_Info_Seqs.Vector);
      --  Recursive version

      function BT_Is_Important (Info : BT_Info) return Boolean;
      --  Returns True if BT is a precondition event corresponding to
      --  a precondition_check message at same source location, message kind,
      --  with a matching precondition index, at the top level. Unconditionally
      --  returns True for other cases.

      ---------------------
      -- BT_Is_Important --
      ---------------------

      function BT_Is_Important (Info : BT_Info) return Boolean is
      begin
         if Top_Level and then Msg_Kind = Precondition_Check then
            return Info.Kind = Msg_Kind
              and then Info.Sloc = Msg_Loc
              and then Get_Precondition_Index (Info.Bt_Id) = Precond_Index;
         end if;

         return True;
      end BT_Is_Important;

      procedure Display_BT (Info : BT_Info);
      --  Print out the backtrace for debugging.

      ----------------
      -- Display_BT --
      ----------------

      procedure Display_BT (Info : BT_Info) is
      begin
         Put ("     "
              & Natural'Image (Info.Sloc.Line) & ":"
              & Natural'Image (Info.Sloc.Column) & " - "
              & To_String (Info.Text));

         if Info.Kind = Precondition_Check then
            Put (" - call to "
                 & Get_Precondition_Callee_Name (Info.Bt_Id)
                 & ", vn"
                 & Natural'Image (Get_Precondition_VN (Info.Bt_Id)));
         end if;

         New_Line;
      end Display_BT;

      procedure Get_Vn_Backtraces_Rec
        (Rec_Proc_Name  : String;
         Vn_Id          : Natural;
         Rec_Backtraces : out BT_Info_Seqs.Vector)
      is
         Proc           : constant Unbounded_String
                            := To_Unbounded_String (Rec_Proc_Name);
         All_VN_BTs     : BT_Info_Seqs.Vector;
         VN_BTs         : BT_Info_Seqs.Vector := BT_Info_Seqs.Empty_Vector;
         All_Checks     : Check_Kinds_Array := Check_Kinds_Array_Default;
         Primary_Checks : Check_Kinds_Array;

         use BT_Info_Seqs;

         use VN_To_BT_Mappings;

      begin
         if not Procs_To_Vns_Mappings.Contains (Proc_Vns, Proc)
           or else not Proc_Vns.Element (Proc).Contains (Vn_Id)
         then
            --  no known backtraces for this proc
            return;
         end if;

         --  backtraces associated with Vn_Id in proc
         All_VN_BTs := VN_To_BT_Mappings.Element
           (Proc_Vns.Element (Proc).Find (Vn_Id));

         --  Delete unneeded backtraces if Msg_Check is a precondition_check.
         --  We are only interested in the backtraces associated with the
         --  precondition.
         for Info of All_VN_BTs loop
            if BT_Is_Important (Info) then
               Append (VN_BTs, Info);
            end if;
         end loop;

         Sort_Backtraces.Sort (VN_BTs);

         if Debug_On then
            Put_Line ("VN_BTs after sort");
            for Info of VN_BTs loop
               Display_BT (Info);
            end loop;
         end if;

         --  Now import the precondition backtraces
         for Info of VN_BTs loop
            if not Contains (Rec_Backtraces, Info) then
               Append (Rec_Backtraces, Info);
            end if;

            if Info.Event = Precondition_Event then
               Top_Level := False;

               declare
                  Callee_BTs       : BT.BT_Info_Seqs.Vector;
                  Callee_File_Name : constant String :=
                    Get_Callee_File_Name (Info.Bt_Id);
                  Callee_Proc_Name : constant String :=
                    Get_Precondition_Callee_Name (Info.Bt_Id);
                  Precondition_VN  : Natural;
                  File_Exists      : Boolean;

               begin
                  if not Files_Read.Contains
                    (BT.Xml.Xml_BT_File_Name
                       (To_String (Inspection_Output_Directory),
                        Callee_File_Name))
                  then
                     Read_File_Backtrace_Xml
                       (To_String (Inspection_Output_Directory),
                        Callee_File_Name,
                        File_Exists);
                  end if;

                  --  Import the backtraces contributing to that precondition

                  if Callee_Proc_Name /= Proc_Name
                    and then Callee_Proc_Name /= Rec_Proc_Name
                  then
                     if Debug_On then
                        Put_Line
                          ("get precondition bts for " & Callee_Proc_Name);
                     end if;

                     Precondition_VN := Get_Precondition_VN (Info.Bt_Id);
                     Get_Vn_Backtraces_Rec
                       (Callee_Proc_Name,
                        Precondition_VN,
                        Callee_BTs);

                     if Debug_On then
                        Put_Line ("Adding precondition bts for call to "
                                    & Callee_Proc_Name);
                        for Info of Callee_BTs loop
                           Display_BT (Info);
                        end loop;
                     end if;

                     --  Remove non-primary checks at this level.
                     --  First, collect checks.

                     for Info of Callee_BTs loop
                        if Info.Event = Check_Event then
                           All_Checks (Info.Kind) := True;
                        end if;
                     end loop;

                     Primary_Checks := Primary_Original_Checks (All_Checks);

                     declare
                        Result_BTs : BT_Info_Seqs.Vector;
                        Elem       : BT_Info_Seqs.Cursor := Callee_BTs.First;
                     begin
                        while BT_Info_Seqs.Has_Element (Elem) loop
                           declare
                              Info : constant BT_Info
                                       := BT_Info_Seqs.Element (Elem);
                           begin
                              if Info.Event /= Check_Event
                                or else Primary_Checks (Info.Kind)
                              then
                                 Append (Result_BTs, Info);
                              end if;
                           end;
                           Elem := BT_Info_Seqs.Next (Elem);
                        end loop;
                        Callee_BTs := Result_BTs;
                     end;
                     if Debug_On then
                        Put_Line ("after removing primary checks");
                        for Info of Callee_BTs loop
                           Display_BT (Info);
                        end loop;
                     end if;
                     --  Add to list if not already present
                     for Info of Callee_BTs loop
                        if not Contains (Rec_Backtraces, Info) then
                           Append (Rec_Backtraces, Info);
                        end if;
                     end loop;
                  end if;
               end;
            end if;
            if Debug_On then
               Put_Line ("Rec_Backtraces");
               for Info of Rec_Backtraces loop
                  Display_BT (Info);
               end loop;
            end if;
         end loop;
      end Get_Vn_Backtraces_Rec;

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
         New_Line;
         Put_Line ("get_backtraces for vn " & Natural'Image (Vn_Id)
            & " in " & Proc_Name & ":" & Integer'Image (Msg_Loc.Line)
            & ":" & Integer'Image (Msg_Loc.Column));
      end if;

      if Backtraces /= BT_Info_Seqs.Empty_Vector then
         declare
            New_BTs : BT_Info_Seqs.Vector;
         begin
            Get_Vn_Backtraces_Rec (Proc_Name, Vn_Id, New_BTs);
            BT_Info_Seqs.Append (Backtraces, New_BTs);
         end;
      else
         Get_Vn_Backtraces_Rec (Proc_Name, Vn_Id, Backtraces);
      end if;
      if Debug_On then
         Put_Line ("Backtraces after recursion");
         for Info of Backtraces loop
            Display_BT (Info);
         end loop;
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
                  --  We need to add this backtrace, but first check if there
                  --  are multiple similar backtraces for the same line. If
                  --  there are, only add the one corresponding to the message
                  --  column.
                  if Info.Sloc.Line = Msg_Loc.Line and then
                     Info.Sloc.Column /= Msg_Loc.Column
                  then
                     --  Is there another backtrace on the same line that would
                     --  be more appropriate?
                     declare
                        Next_Cursor : BT_Info_Seqs.Cursor :=
                           BT_Info_Seqs.Next (Elem);
                        Next_Info : BT_Info;
                     begin
                        while BT_Info_Seqs.Has_Element (Next_Cursor) loop
                           Next_Info :=
                             BT_Info_Seqs.Element (Next_Cursor);
                           if Next_Info.Event = Info.Event
                             and then Next_Info.Kind = Info.Kind
                             and then Next_Info.Sloc.Line = Info.Sloc.Line
                           then
                              if Next_Info.Sloc.Column = Msg_Loc.Column then
                                 Info := Next_Info;
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

                  Append (Result_BTs, Info);
               end if;
            end;
            Elem := BT_Info_Seqs.Next (Elem);
         end loop;

         Backtraces := Result_BTs;
      end;
      if Debug_On then
         Put_Line ("Final Backtraces:");
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

   ----------------------------
   -- Get_Precondition_Index --
   ----------------------------

   function Get_Precondition_Index (Bt_Id : Natural) return Natural is
      Info : constant Callee_Info_Record := Callee_Mapping.Element (Bt_Id);
   begin
      return Info.Callee_Pre_Index;
   end Get_Precondition_Index;

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
                  (To_Unbounded_String (Var_Name),
                   To_Unbounded_String (Var_Values)));
            end if;
         end Find_One_Variable;

      begin
         Name_Set.Iterate (Variables_On_Line, Find_One_Variable'Access);
         return Result;
      end;
   end Get_Srcpos_Vn_Values;

end BT.Xml.Reader;
