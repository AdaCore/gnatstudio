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

package body SN.DB_Structures is

   Bad_Input : exception;
   --  Raised by internal procedures in the case of bad input data

   procedure Parse_Position (Position_Str : in String; Position : out Point);
   --  Parse file position information from strings like "00001233.11" (that
   --  could appear in the SourceNavigator DB)

   function Parse_Hex (Hex_Str : String) return Integer;
   --  Converts C-style hexadecimal string like "0xffff" to Integer number

   function Remove_Brackets (String_With_Brackets : String) return String;
   --  Removes outwards brackets from string

--  function Make_Array_From_String (input : String) return Arg_String_Array;
   --  Parses string of tokens separated by comma

   procedure Make_Vector_From_String
     (Input       : String;
      Buffer      : GNAT.OS_Lib.String_Access;
      Buffer_Pos  : in out Integer;
      Vector_Root : out Segment_Vector.Node_Access);
   --  Parses string of tokens separated by comma and stores parsed
   --  values in vector

   -------------------------------------------------------------------------
   --                   Parse_Pair function bodies                        --
   -------------------------------------------------------------------------

   function Parse_Pair (Key_Data_Pair : Pair) return BY_Table is
      tab : BY_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Referred_Class.First := cur_pos;
      tab.Referred_Class.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      tab.Referred_Symbol_Name.First := cur_pos;
      tab.Referred_Symbol_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      tab.Referred_Symbol :=
         Parse_Symbol (Get_Field (Key_Data_Pair.Key, 3));
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      tab.Class.First := cur_pos;
      tab.Class.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 5);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 5);
      tab.Symbol_Name.First := cur_pos;
      tab.Symbol_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      tab.Symbol :=
         Parse_Symbol (Get_Field (Key_Data_Pair.Key, 6));
      Len := Get_Field_Length (Key_Data_Pair.Key, 7);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 7);
      tab.Access_Type.First := cur_pos;
      tab.Access_Type.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 9);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 9);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 8), tab.Position);
      Make_Vector_From_String (
            Remove_Brackets (Get_Field (Key_Data_Pair.Data, 1)),
            tab.Buffer, cur_pos, tab.Referred_Argument_Types);
      Make_Vector_From_String (
            Remove_Brackets (Get_Field (Key_Data_Pair.Data, 2)),
            tab.Buffer, cur_pos, tab.Caller_Argument_Types);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return CL_Table is
      tab : CL_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 4) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 4));
      tab.Template_Parameters.First := cur_pos;
      tab.Template_Parameters.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return COM_Table is
      tab : COM_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return CON_Table is
      tab : CON_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 3) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 3));
      tab.Declared_Type.First := cur_pos;
      tab.Declared_Type.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return COV_Table is
      tab : COV_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Common_Block.First := cur_pos;
      tab.Common_Block.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 3), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return E_Table is
      tab : E_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return EC_Table is
      tab : EC_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 3) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 3));
      tab.Enumeration_Name.First := cur_pos;
      tab.Enumeration_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return F_Table is
      tab : F_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Data, 1);
      tab.Group.First := cur_pos;
      tab.Group.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 2);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Data, 2);
      tab.Parsing_Time.First := cur_pos;
      tab.Parsing_Time.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Data, 3);
      tab.Highlight_File.First := cur_pos;
      tab.Highlight_File.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return FD_Table is
      tab : FD_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 3) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 3));
      tab.Return_Type.First := cur_pos;
      tab.Return_Type.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Make_Vector_From_String (
            Remove_Brackets (Get_Field (Key_Data_Pair.Data, 4)),
            tab.Buffer, cur_pos, tab.Arg_Types);
      Make_Vector_From_String (
            Remove_Brackets (Get_Field (Key_Data_Pair.Data, 5)),
            tab.Buffer, cur_pos, tab.Arg_Names);
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return FIL_Table is
      tab : FIL_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.Class.First := cur_pos;
      tab.Class.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      tab.Identifier.First := cur_pos;
      tab.Identifier.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      tab.Symbol :=
         Parse_Symbol (Get_Field (Key_Data_Pair.Key, 5));
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 2), tab.Highlight_Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 3), tab.Highlight_End_Position);
      Make_Vector_From_String (
            Remove_Brackets (Get_Field (Key_Data_Pair.Data, 4)),
            tab.Buffer, cur_pos, tab.Types_Of_Arguments);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return FR_Table is
      tab : FR_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 3) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 3));
      tab.Return_Type.First := cur_pos;
      tab.Return_Type.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Make_Vector_From_String (
            Remove_Brackets (Get_Field (Key_Data_Pair.Data, 4)),
            tab.Buffer, cur_pos, tab.Arg_Types);
      Make_Vector_From_String (
            Remove_Brackets (Get_Field (Key_Data_Pair.Data, 5)),
            tab.Buffer, cur_pos, tab.Arg_Names);
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return FU_Table is
      tab : FU_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 3) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 3));
      tab.Return_Type.First := cur_pos;
      tab.Return_Type.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Make_Vector_From_String (
            Remove_Brackets (Get_Field (Key_Data_Pair.Data, 4)),
            tab.Buffer, cur_pos, tab.Arg_Types);
      Make_Vector_From_String (
            Remove_Brackets (Get_Field (Key_Data_Pair.Data, 5)),
            tab.Buffer, cur_pos, tab.Arg_Names);
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return GV_Table is
      tab : GV_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 3) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 3));
      tab.Value_Type.First := cur_pos;
      tab.Value_Type.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return IN_Table is
      tab : IN_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Class.First := cur_pos;
      tab.Class.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      tab.Base_Class.First := cur_pos;
      tab.Base_Class.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 3), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return IU_Table is
      tab : IU_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Included_File.First := cur_pos;
      tab.Included_File.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.Included_From_File.First := cur_pos;
      tab.Included_From_File.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Included_At_Position);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return IV_Table is
      tab : IV_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Class.First := cur_pos;
      tab.Class.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 3), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 3) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 3));
      tab.Value_Type.First := cur_pos;
      tab.Value_Type.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return LV_Table is
      tab : LV_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Function_Name.First := cur_pos;
      tab.Function_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.Start_Position.First := cur_pos;
      tab.Start_Position.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 4) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 4));
      tab.Value_Type.First := cur_pos;
      tab.Value_Type.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return MA_Table is
      tab : MA_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return MD_Table is
      tab : MD_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Class.First := cur_pos;
      tab.Class.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 3), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 3) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 3));
      tab.Return_Type.First := cur_pos;
      tab.Return_Type.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Make_Vector_From_String (
            Remove_Brackets (Get_Field (Key_Data_Pair.Data, 4)),
            tab.Buffer, cur_pos, tab.Arg_Types);
      Make_Vector_From_String (
            Remove_Brackets (Get_Field (Key_Data_Pair.Data, 5)),
            tab.Buffer, cur_pos, tab.Arg_Names);
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return MI_Table is
      tab : MI_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Class.First := cur_pos;
      tab.Class.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 3), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 3) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 3));
      tab.Return_Type.First := cur_pos;
      tab.Return_Type.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Make_Vector_From_String (
            Remove_Brackets (Get_Field (Key_Data_Pair.Data, 4)),
            tab.Buffer, cur_pos, tab.Arg_Types);
      Make_Vector_From_String (
            Remove_Brackets (Get_Field (Key_Data_Pair.Data, 5)),
            tab.Buffer, cur_pos, tab.Arg_Names);
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return REM_Table is
      tab : REM_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Position);
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.Class.First := cur_pos;
      tab.Class.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      tab.Method_Or_Function.First := cur_pos;
      tab.Method_Or_Function.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 1) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 1));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return SU_Table is
      tab : SU_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 1));
      Len := Get_Field_Length (Key_Data_Pair.Data, 4) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 4));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return T_Table is
      tab : T_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 3) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 3));
      tab.Original.First := cur_pos;
      tab.Original.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return TO_Table is
      tab : TO_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Class.First := cur_pos;
      tab.Class.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 2);
      tab.Symbol_Name.First := cur_pos;
      tab.Symbol_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      tab.Symbol :=
         Parse_Symbol (Get_Field (Key_Data_Pair.Key, 3));
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 4);
      tab.Referred_Class.First := cur_pos;
      tab.Referred_Class.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 5);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 5);
      tab.Referred_Symbol_Name.First := cur_pos;
      tab.Referred_Symbol_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      tab.Referred_Symbol :=
         Parse_Symbol (Get_Field (Key_Data_Pair.Key, 6));
      Len := Get_Field_Length (Key_Data_Pair.Key, 7);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 7);
      tab.Access_Type.First := cur_pos;
      tab.Access_Type.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 9);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 9);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 8), tab.Position);
      Make_Vector_From_String (
            Remove_Brackets (Get_Field (Key_Data_Pair.Data, 1)),
            tab.Buffer, cur_pos, tab.Caller_Argument_Types);
      Make_Vector_From_String (
            Remove_Brackets (Get_Field (Key_Data_Pair.Data, 2)),
            tab.Buffer, cur_pos, tab.Referred_Argument_Types);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return UN_Table is
      tab : UN_Table;
      cur_pos : Integer;
      Len : Integer;
   begin
      Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      tab.Buffer := new String (1 .. Len);
      cur_pos := tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 1);
      tab.Name.First := cur_pos;
      tab.Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Get_Field (Key_Data_Pair.Key, 3);
      tab.File_Name.First := cur_pos;
      tab.File_Name.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Parse_Position (
         Get_Field (Key_Data_Pair.Key, 2), tab.Start_Position);
      Parse_Position (
         Get_Field (Key_Data_Pair.Data, 1), tab.End_Position);
      tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 6) - 2;
      tab.Buffer (cur_pos .. (cur_pos + Len - 1)) :=
         Remove_Brackets (Get_Field (Key_Data_Pair.Data, 6));
      tab.Comments.First := cur_pos;
      tab.Comments.Last := cur_pos + Len - 1;
      cur_pos := cur_pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return tab;
   end Parse_Pair;

   ----------
   -- Free --
   ----------

   procedure Free (target : in out BY_Table) is
   begin
      Free (target.Buffer);
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
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out FIL_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out FR_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out FU_Table) is
   begin
      Free (target.Buffer);
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
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out MI_Table) is
   begin
      Free (target.Buffer);
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

   procedure Free (target : in out TO_Table) is
   begin
      Free (target.Buffer);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers - 1;
   end Free;

   procedure Free (target : in out UN_Table) is
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
      for i in reverse 1 .. Position_Str'Length loop
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


   function Parse_Hex (Hex_Str : String) return Integer is
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
      return val;
   end Parse_Hex;

   function Remove_Brackets (String_With_Brackets : String) return String is
      result_str : String (1 .. (String_With_Brackets'Length - 2));
   begin
      result_str :=
         String_With_Brackets (2 .. (String_With_Brackets'Length - 1));
      return result_str;
   end Remove_Brackets;

--    function Make_Array_From_String (input : String)
--          return Arg_String_Array is
--       n, j : Integer;
--       acc : String (1 .. STRING_MAX_SIZE);
--       acc_big : Arg_String_Array;
--    begin
--       n := 1;
--       j := 1;
--       for i in 1 .. acc'Length loop
--          acc (i) := ' ';
--       end loop;
--       for i in 1 .. input'Length loop
--          if input (i) = ',' then
--             acc_big (j) := To_Unbounded_String (acc (1 .. (n - 1)));
--             n := 1;
--             j := j + 1;
--          else
--             acc (n) := input (i);
--             if i = input'Length then
--                acc_big (j) := To_Unbounded_String (acc (1 .. n));
--             end if;
--             n := n + 1;
--          end if;
--       end loop;
--       acc_big (j + 1) := Null_Unbounded_String;
--       return acc_big;
--    end Make_Array_From_String;

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

end SN.DB_Structures;
