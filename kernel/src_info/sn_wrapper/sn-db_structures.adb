-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002                            --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with SN.Symbols;  use SN.Symbols;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Strings.Fixed;

package body SN.DB_Structures is

   Bad_Input : exception;
   --  Raised by internal procedures in the case of bad input data

   procedure Parse_Position (Position_Str : in String; Position : out Point);
   --  Parse file position information from strings like "00001233.11" (that
   --  could appear in the SourceNavigator DB)

   function Parse_Hex (Hex_Str : String) return SN_Attributes;
   --  Converts C-style hexadecimal string like "0xffff" to integer number

   procedure Remove_Brackets
      (Str     :        String;
       Buffer  :        GNAT.OS_Lib.String_Access;
       Cur_Pos : in out Integer;
       Seg     : out    Segment);
   pragma Inline (Remove_Brackets);
   --  Copies content of brackets into buffer starting with Cur_Pos
   --  Sets Seg to the beginning and end of the copied piece of data
   --  increments Cur_Pos

--  function Make_Array_From_String (input : String) return Arg_String_Array;
   --  Parses string of tokens separated by comma

   procedure Make_Vector_From_String
     (Input       : String;
      Buffer      : GNAT.OS_Lib.String_Access;
      Buffer_Pos  : in out Integer;
      Vector_Root : out Segment_Vector.Node_Access);
   pragma Unreferenced (Make_Vector_From_String);
   --  Parses string of tokens separated by comma and stores parsed
   --  values in vector

   function Get_Position_From_Comment
     (Comment  : Segment;
      Buffer   : GNAT.OS_Lib.String_Access;
      Name     : String) return Point;
   --  Parses comment string to find Name=Value pair, then
   --  parses value into point

   function Get_Segment_From_Comment
     (Comment  : Segment;
      Buffer   : GNAT.OS_Lib.String_Access;
      Name     : String) return Segment;
   --  Parses comment string to find Name=Value pair, then
   --  returns segment coordinates that spans upto next semicolon or
   --  end of string

   -------------------------------------------------------------------------
   --                   Parse_Pair function bodies                        --
   -------------------------------------------------------------------------

   function Parse_Pair (Key_Data_Pair : Pair) return BY_Table is
      Tab : BY_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Referred_Class.First := Cur_Pos;
      Tab.Referred_Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      Tab.Referred_Symbol_Name.First := Cur_Pos;
      Tab.Referred_Symbol_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Tab.Referred_Symbol :=
         Parse_Symbol (Get_Field (Key_Data_Pair.Key, 3));
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      Tab.Class.First := Cur_Pos;
      Tab.Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 5);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 5);
      Tab.Symbol_Name.First := Cur_Pos;
      Tab.Symbol_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Tab.Symbol :=
         Parse_Symbol (Get_Field (Key_Data_Pair.Key, 6));
      Len := Get_Field_Length (Key_Data_Pair.Key, 7);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 7);
      Tab.Access_Type.First := Cur_Pos;
      Tab.Access_Type.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 9);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 9);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 8), Tab.Position);
      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 1),
         Tab.Buffer,
         Cur_Pos,
         Tab.Referred_Argument_Types);
      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 2),
         Tab.Buffer,
         Cur_Pos,
         Tab.Caller_Argument_Types);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return CL_Table is
      Tab : CL_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), Tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 4),
         Tab.Buffer,
         Cur_Pos,
         Tab.Template_Parameters);
      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);
      Cur_Pos := Cur_Pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return COM_Table is
      Tab : COM_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), Tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return CON_Table is
      Tab : CON_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), Tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 3),
         Tab.Buffer,
         Cur_Pos,
         Tab.Declared_Type);

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      Tab.Type_Start_Position := Get_Position_From_Comment
        (Tab.Comments,
         Tab.Buffer,
         "type_beg");
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return COV_Table is
      Tab : COV_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Common_Block.First := Cur_Pos;
      Tab.Common_Block.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 3), Tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return E_Table is
      Tab : E_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), Tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return EC_Table is
      Tab : EC_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), Tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 3),
         Tab.Buffer,
         Cur_Pos,
         Tab.Enumeration_Name);

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return F_Table is
      Tab : F_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Data, 1);
      Tab.Group.First := Cur_Pos;
      Tab.Group.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 2);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Data, 2);
      Tab.Parsing_Time.First := Cur_Pos;
      Tab.Parsing_Time.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Data, 3);
      Tab.Highlight_File.First := Cur_Pos;
      Tab.Highlight_File.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return FD_Table is
      Tab : FD_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), Tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 3),
         Tab.Buffer,
         Cur_Pos,
         Tab.Return_Type);

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 4),
         Tab.Buffer,
         Cur_Pos,
         Tab.Arg_Types);

      --  Make_Vector_From_String (
      --        Remove_Brackets (Get_Field (Key_Data_Pair.Data, 5)),
      --        Tab.Buffer, Cur_Pos, Tab.Arg_Names);

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);

      Tab.Template_Parameters := Get_Segment_From_Comment
        (Tab.Comments,
         Tab.Buffer,
         "template_args");
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return FIL_Table is
      Tab : FIL_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      Tab.Class.First := Cur_Pos;
      Tab.Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      Tab.Identifier.First := Cur_Pos;
      Tab.Identifier.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Tab.Symbol :=
         Parse_Symbol (Get_Field (Key_Data_Pair.Key, 5));
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 2), Tab.Highlight_Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 3), Tab.Highlight_End_Position);
      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 4),
         Tab.Buffer,
         Cur_Pos,
         Tab.Types_Of_Arguments);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return FR_Table is
      Tab : FR_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), Tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 3),
         Tab.Buffer,
         Cur_Pos,
         Tab.Return_Type);

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 4),
         Tab.Buffer,
         Cur_Pos,
         Tab.Arg_Types);

      --  Make_Vector_From_String (
      --        Remove_Brackets (Get_Field (Key_Data_Pair.Data, 5)),
      --        Tab.Buffer, Cur_Pos, Tab.Arg_Names);

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return FU_Table is
      Tab            : FU_Table;
      Cur_Pos        : Integer;
      Len            : Integer;
      Num_Of_Fileds  : constant Integer := Get_Field_Count (Key_Data_Pair.Key);
      Field_Offset   : Integer := 0;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);

      case Num_Of_Fileds is
         when 3      => -- .fu table
            Tab.Buffer (1) := '#';
            Cur_Pos := 2;
            Tab.Class.First := 1;
            Tab.Class.Last  := 1;
         when 4      => -- .mi table
            Len := Get_Field_Length (Key_Data_Pair.Key, 1);
            Tab.Buffer (1 .. Len) :=
               Get_Field (Key_Data_Pair.Key, 1);
            Tab.Class.First := 1;
            Tab.Class.Last  := Len;
            Cur_Pos := 1 + Len;
            Field_Offset := 1;
         when others =>
            raise Bad_Input;
      end case;

      Len := Get_Field_Length (Key_Data_Pair.Key, Field_Offset + 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, Field_Offset + 1);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, Field_Offset + 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, Field_Offset + 3);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Parse_Position (
         Get_Field (Key_Data_Pair.Key, Field_Offset + 2), Tab.Start_Position);

      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 3),
         Tab.Buffer,
         Cur_Pos,
         Tab.Return_Type);

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 4),
         Tab.Buffer,
         Cur_Pos,
         Tab.Arg_Types);

      --  Make_Vector_From_String (
      --        Remove_Brackets (Get_Field (Key_Data_Pair.Data, 5)),
      --        Tab.Buffer, Cur_Pos, Tab.Arg_Names);

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);

      Tab.Template_Parameters := Get_Segment_From_Comment
        (Tab.Comments,
         Tab.Buffer,
         "template_args");
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return GV_Table is
      Tab : GV_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), Tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 3),
         Tab.Buffer,
         Cur_Pos,
         Tab.Value_Type);

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);

      Tab.Type_Start_Position := Get_Position_From_Comment
        (Tab.Comments,
         Tab.Buffer,
         "type_beg");

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return IN_Table is
      Tab : IN_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Class.First := Cur_Pos;
      Tab.Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      Tab.Base_Class.First := Cur_Pos;
      Tab.Base_Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 3), Tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return IU_Table is
      Tab : IU_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Included_File.First := Cur_Pos;
      Tab.Included_File.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      Tab.Included_From_File.First := Cur_Pos;
      Tab.Included_From_File.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), Tab.Included_At_Position);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return IV_Table is
      Tab : IV_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Class.First := Cur_Pos;
      Tab.Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 3), Tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 3),
         Tab.Buffer,
         Cur_Pos,
         Tab.Value_Type);

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return LV_Table is
      Tab : LV_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Function_Name.First := Cur_Pos;
      Tab.Function_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 3), Tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 3),
         Tab.Buffer,
         Cur_Pos,
         Tab.Class);

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 4),
         Tab.Buffer,
         Cur_Pos,
         Tab.Value_Type);

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 5),
         Tab.Buffer,
         Cur_Pos,
         Tab.Arg_Types);

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      Tab.Type_Start_Position := Get_Position_From_Comment
        (Tab.Comments,
         Tab.Buffer,
         "type_beg");
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return MA_Table is
      Tab : MA_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), Tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return MD_Table is
      Tab : MD_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Class.First := Cur_Pos;
      Tab.Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 3), Tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 3),
         Tab.Buffer,
         Cur_Pos,
         Tab.Return_Type);

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 4),
         Tab.Buffer,
         Cur_Pos,
         Tab.Arg_Types);

      --  Make_Vector_From_String (
      --        Remove_Brackets (Get_Field (Key_Data_Pair.Data, 5)),
      --        Tab.Buffer, Cur_Pos, Tab.Arg_Names);

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);

      Tab.Template_Parameters := Get_Segment_From_Comment
        (Tab.Comments,
         Tab.Buffer,
         "template_args");
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return REM_Table is
      Tab : REM_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), Tab.Position);
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      Tab.Class.First := Cur_Pos;
      Tab.Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      Tab.Method_Or_Function.First := Cur_Pos;
      Tab.Method_Or_Function.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 1),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return SU_Table is
      Tab : SU_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), Tab.Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 1));
      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 4),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return T_Table is
      Tab : T_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), Tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 3),
         Tab.Buffer,
         Cur_Pos,
         Tab.Original);

      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);

      Tab.Class_Name := Get_Segment_From_Comment
        (Tab.Comments,
         Tab.Buffer,
         "class");

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return TA_Table is
      Tab : TA_Table;
      Cur_Pos : Integer;
      Len : Integer := Get_Total_Length (Key_Data_Pair.Key)
        + Get_Total_Length (Key_Data_Pair.Data);
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Tab.Buffer := new String (1 .. Len);

      --  Scope
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Scope.First := Cur_Pos;
      Tab.Scope.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      --  Name
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      --  File_Name
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      --  Start_Position
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 3), Tab.Start_Position);

      --  Type_Position
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), Tab.Type_Position);

      --  Attributes
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      --  Value_Type
      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 3),
         Tab.Buffer,
         Cur_Pos,
         Tab.Value_Type);

      --  Template_Args
      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 4),
         Tab.Buffer,
         Cur_Pos,
         Tab.Template_Parameters);

      --  Comments
      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);

      Tab.Class_Name := Get_Segment_From_Comment
        (Tab.Comments,
         Tab.Buffer,
         "class");

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return TO_Table is
      Tab : TO_Table;
      Cur_Pos : Integer;
      Len : Integer;
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      Tab.Class.First := Cur_Pos;
      Tab.Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      Tab.Symbol_Name.First := Cur_Pos;
      Tab.Symbol_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Tab.Symbol :=
         Parse_Symbol (Get_Field (Key_Data_Pair.Key, 3));
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      Tab.Referred_Class.First := Cur_Pos;
      Tab.Referred_Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 5);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 5);
      Tab.Referred_Symbol_Name.First := Cur_Pos;
      Tab.Referred_Symbol_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Tab.Referred_Symbol :=
         Parse_Symbol (Get_Field (Key_Data_Pair.Key, 6));
      Len := Get_Field_Length (Key_Data_Pair.Key, 7);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 7);
      Tab.Access_Type.First := Cur_Pos;
      Tab.Access_Type.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 9);
      Tab.Buffer (Cur_Pos .. (Cur_Pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 9);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 8), Tab.Position);
      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 1),
         Tab.Buffer,
         Cur_Pos,
         Tab.Caller_Argument_Types);
      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 2),
         Tab.Buffer,
         Cur_Pos,
         Tab.Referred_Argument_Types);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   ----------
   -- Free --
   ----------

   procedure Free (target : in out BY_Table) is
   begin
      Free (target.Buffer);
      --  Segment_Vector.Release_Vector (target.Caller_Argument_Types);
      --  Segment_Vector.Release_Vector (target.Referred_Argument_Types);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out CL_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out COM_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out CON_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out COV_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out E_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out EC_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out F_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out FD_Table) is
   begin
      Free (target.Buffer);
      --  Segment_Vector.Release_Vector (target.Arg_Types);
      --  Segment_Vector.Release_Vector (target.Arg_Names);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out FIL_Table) is
   begin
      Free (target.Buffer);
      --  Segment_Vector.Release_Vector (target.Types_Of_Arguments);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out FR_Table) is
   begin
      Free (target.Buffer);
      --  Segment_Vector.Release_Vector (target.Arg_Types);
      --  Segment_Vector.Release_Vector (target.Arg_Names);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out FU_Table) is
   begin
      Free (target.Buffer);
      --  Segment_Vector.Release_Vector (target.Arg_Types);
      --  Segment_Vector.Release_Vector (target.Arg_Names);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out GV_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out IN_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out IU_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out IV_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out LV_Table) is
   begin
      Free (target.Buffer);
      --  Segment_Vector.Release_Vector (target.Arg_Types);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out MA_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out MD_Table) is
   begin
      Free (target.Buffer);
      --  Segment_Vector.Release_Vector (target.Arg_Types);
      --  Segment_Vector.Release_Vector (target.Arg_Names);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out REM_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out SU_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out T_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out TA_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out TO_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   -------------------------------------------------------------------------

   procedure Parse_Position (Position_Str : in String;
                             Position : out Point) is
      n, num1, num2 : Integer;
      c : Character;
      dot_already_found : Boolean := False;
   begin
      num1 := 0;
      num2 := 0;
      n := 1;
      for i in reverse Position_Str'First .. Position_Str'Last loop
         c := Position_Str (i);
         if ((c < '0') or (c > '9')) and (c /= '.') then
            raise Bad_Input;
         elsif c = '.' then
            dot_already_found := True;
            n := 1;
         else
            if dot_already_found then
               num1 := num1 + (Character'Pos (c) - Character'Pos ('0')) * n;
            else
               num2 := num2 + (Character'Pos (c) - Character'Pos ('0')) * n;
            end if;
            n := n * 10;
         end if;
      end loop;
      if dot_already_found then
         Position.Line := num1;
         Position.Column := num2;
      else
         Position.Line := num2;
         Position.Column := 0;
      end if;
   end Parse_Position;


   function Parse_Hex (Hex_Str : String) return SN_Attributes is
      val : Integer;
      c : Character;
   begin
      if (Hex_Str (1 .. 2) /= "0x") and (Hex_Str (1 .. 2) /= "0X") then
         raise Bad_Input;
      end if;
      val := 0;
      for i in 3 .. Hex_Str'Length loop
         c := Hex_Str (i);
         if (c >= '0' and c <= '9') then
            val := val * 16 + (Character'Pos (c) - Character'Pos ('0'));
         elsif (c >= 'a' and c <= 'f') then
            val := val * 16 + (Character'Pos (c) - Character'Pos ('a')) + 10;
         elsif (c >= 'A' and c <= 'F') then
            val := val * 16 + (Character'Pos (c) - Character'Pos ('A')) + 10;
         else
            raise Bad_Input;
         end if;
      end loop;
      return SN_Attributes (val);
   end Parse_Hex;

   procedure Remove_Brackets
      (Str     :        String;
       Buffer  :        GNAT.OS_Lib.String_Access;
       Cur_Pos : in out Integer;
       Seg     : out    Segment)
   is
   begin
      Seg.First := Cur_Pos;
      Seg.Last  := Cur_Pos + Str'Length - 3;
      Buffer (Seg.First .. Seg.Last) := Str (Str'First + 1 .. Str'Last - 1);
      Cur_Pos   := Seg.Last + 1;
   end Remove_Brackets;

   -----------------------------
   -- Make_Vector_From_String --
   -----------------------------

   procedure Make_Vector_From_String
     (Input       : in String;
      Buffer      : GNAT.OS_Lib.String_Access;
      Buffer_Pos  : in out Integer;
      Vector_Root : out Segment_Vector.Node_Access)
   is
      n, pos : Integer;
      tmp_seg : Segment;
   begin
      Vector_Root := null;
      n := 0;
      if Input'Length = 0 then
         return;
      end if;
      pos := Input'First;
      for i in Input'First .. Input'Last loop
         if (Input (i) = ',' and n > 0) or i = Input'Last then
            if i = Input'Last then
               n := n + 1;
            end if;
            Buffer (Buffer_Pos .. (Buffer_Pos + n - 1)) :=
                  Input (pos .. (pos + n - 1));
            pos := pos + n + 1;
            tmp_seg.First := Buffer_Pos;
            tmp_seg.Last := Buffer_Pos + n - 1;
            Segment_Vector.Append (Vector_Root, tmp_seg);
            Buffer_Pos := Buffer_Pos + n;
            n := 0;
         else
            n := n + 1;
         end if;
      end loop;
   end Make_Vector_From_String;

   -------------------------------
   -- Get_Position_From_Comment --
   -------------------------------

   function Get_Position_From_Comment
     (Comment  : Segment;
      Buffer   : GNAT.OS_Lib.String_Access;
      Name     : String) return Point
   is
      I, J     : Natural;
      Pos      : Point := Invalid_Point;
   begin
      I := Ada.Strings.Fixed.Index
         (Buffer (Comment.First .. Comment.Last),
          Name & "=");
      if I /= 0 then
         I := I + Name'Length + 1;
         J := I;
         while J <= Comment.Last loop
            exit when (('0' > Buffer (J) or Buffer (J) > '9')
               and Buffer (J) /= '.');
            J := J + 1;
         end loop;

         Parse_Position (Buffer (I .. J - 1), Pos);
      end if;
      return Pos;
   end Get_Position_From_Comment;

   ------------------------------
   -- Get_Segment_From_Comment --
   ------------------------------

   function Get_Segment_From_Comment
     (Comment  : Segment;
      Buffer   : GNAT.OS_Lib.String_Access;
      Name     : String) return Segment
   is
      I, J     : Natural;
   begin
      I := Ada.Strings.Fixed.Index
         (Buffer (Comment.First .. Comment.Last),
          Name & "=");
      if I /= 0 then
         I := I + Name'Length + 1;
         J := I;

         while J <= Comment.Last and Buffer (J) /= ';' loop
            J := J + 1;
         end loop;

         return (I, J - 1);
      end if;

      return Empty_Segment;
   end Get_Segment_From_Comment;
end SN.DB_Structures;
