-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

   procedure Parse_Position (Position_Str : String; Position : out Point);
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

   function Get_Position_From_Comment
     (Comment  : Segment;
      Buffer   : GNAT.OS_Lib.String_Access;
      Name     : String) return Point;
   --  Parses comment string to find Name=Value pair, then
   --  parses value into point
   --  For efficiency, it is assumed that Name ends with '='.

   function Get_Segment_From_Comment
     (Comment  : Segment;
      Buffer   : GNAT.OS_Lib.String_Access;
      Name     : String) return Segment;
   --  Parses comment string to find Name=Value pair, then
   --  returns segment coordinates that spans upto next semicolon or
   --  end of string
   --  For efficiency, it is assumed that Name ends with '='.

   -------------------------------------------------------------------------
   --                   Parse_Pair function bodies                        --
   -------------------------------------------------------------------------

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out BY_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := 1;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);

      Tab.Referred_Class.First := Cur_Pos;
      Tab.Referred_Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Referred_Symbol_Name.First := Cur_Pos;
      Tab.Referred_Symbol_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Tab.Referred_Symbol :=
         Parse_Symbol (Get_Field (Key_Data_Pair.Key, 3));
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Get_Field (Key_Data_Pair.Key, 4, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Class.First := Cur_Pos;
      Tab.Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 5);
      Get_Field (Key_Data_Pair.Key, 5, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Symbol_Name.First := Cur_Pos;
      Tab.Symbol_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Tab.Symbol :=
         Parse_Symbol (Get_Field (Key_Data_Pair.Key, 6));
      Len := Get_Field_Length (Key_Data_Pair.Key, 7);
      Get_Field (Key_Data_Pair.Key, 7, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Access_Type.First := Cur_Pos;
      Tab.Access_Type.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 9);
      Get_Field (Key_Data_Pair.Key, 9, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 8);
      Get_Field (Key_Data_Pair.Key, 8, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Position);

      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Remove_Brackets
        (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Referred_Argument_Types);
      Len := Get_Field_Length (Key_Data_Pair.Data, 2);
      Get_Field (Key_Data_Pair.Data, 2, Buffer, Len);
      Remove_Brackets
        (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Caller_Argument_Types);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out CL_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 4);
      Get_Field (Key_Data_Pair.Data, 4, Buffer, Len);
      Remove_Brackets
        (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Template_Parameters);
      Len := Get_Field_Length (Key_Data_Pair.Data, 6);
      Get_Field (Key_Data_Pair.Data, 6, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out COM_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 6);
      Get_Field (Key_Data_Pair.Data, 6, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out CON_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Len := Get_Field_Length (Key_Data_Pair.Data, 3);
      Get_Field (Key_Data_Pair.Data, 3, Buffer, Len);
      Remove_Brackets
        (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Declared_Type);

      Len := Get_Field_Length (Key_Data_Pair.Data, 6);
      Get_Field (Key_Data_Pair.Data, 6, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      Tab.Type_Start_Position :=
        Get_Position_From_Comment (Tab.Comments, Tab.Buffer, "type_beg=");
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out COV_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Common_Block.First := Cur_Pos;
      Tab.Common_Block.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Get_Field (Key_Data_Pair.Key, 4, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 6);
      Get_Field (Key_Data_Pair.Data, 6, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out E_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));
      Len := Get_Field_Length (Key_Data_Pair.Data, 6);
      Get_Field (Key_Data_Pair.Data, 6, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out EC_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Len := Get_Field_Length (Key_Data_Pair.Data, 3);
      Get_Field (Key_Data_Pair.Data, 3, Buffer, Len);
      Remove_Brackets
        (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Enumeration_Name);

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out F_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field
        (Key_Data_Pair.Data, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Group.First := Cur_Pos;
      Tab.Group.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 2);
      Get_Field
        (Key_Data_Pair.Data, 2, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Parsing_Time.First := Cur_Pos;
      Tab.Parsing_Time.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Data, 3);
      Get_Field
        (Key_Data_Pair.Data, 3, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Highlight_File.First := Cur_Pos;
      Tab.Highlight_File.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out FD_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Len := Get_Field_Length (Key_Data_Pair.Data, 3);
      Get_Field (Key_Data_Pair.Data, 3, Buffer, Len);
      Remove_Brackets
        (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Return_Type);

      Len := Get_Field_Length (Key_Data_Pair.Data, 4);
      Get_Field (Key_Data_Pair.Data, 4, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Arg_Types);

      Len := Get_Field_Length (Key_Data_Pair.Data, 6);
      Get_Field (Key_Data_Pair.Data, 6, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);

      Tab.Template_Parameters :=
        Get_Segment_From_Comment (Tab.Comments, Tab.Buffer, "template_args=");
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out FIL_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);

      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Class.First := Cur_Pos;
      Tab.Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Get_Field (Key_Data_Pair.Key, 4, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Identifier.First := Cur_Pos;
      Tab.Identifier.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Tab.Symbol :=
         Parse_Symbol (Get_Field (Key_Data_Pair.Key, 5));

      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 2);
      Get_Field (Key_Data_Pair.Data, 2, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Highlight_Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 3);
      Get_Field (Key_Data_Pair.Data, 3, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Highlight_End_Position);

      Len := Get_Field_Length (Key_Data_Pair.Data, 4);
      Get_Field (Key_Data_Pair.Data, 4, Buffer, Len);
      Remove_Brackets
        (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Types_Of_Arguments);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out FR_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Len := Get_Field_Length (Key_Data_Pair.Data, 3);
      Get_Field (Key_Data_Pair.Data, 3, Buffer, Len);
      Remove_Brackets
        (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Return_Type);

      Len := Get_Field_Length (Key_Data_Pair.Data, 4);
      Get_Field (Key_Data_Pair.Data, 4, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Arg_Types);

      Len := Get_Field_Length (Key_Data_Pair.Data, 6);
      Get_Field (Key_Data_Pair.Data, 6, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   function Parse_Pair (Key_Data_Pair : Pair) return FU_Table is
      Tab            : FU_Table;
      Cur_Pos        : Integer;
      Len            : Integer;
      Total_Len      : Integer;
      Num_Of_Fields  : constant Integer := Get_Field_Count (Key_Data_Pair.Key);
      Field_Offset   : Integer := 0;

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
        Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);

      case Num_Of_Fields is
         when 3      => -- .fu table
            Tab.Buffer (1) := '#';
            Cur_Pos := 2;
            Tab.Class.First := 1;
            Tab.Class.Last  := 1;
         when 4      => -- .mi table
            Len := Get_Field_Length (Key_Data_Pair.Key, 1);
            Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (1 .. Total_Len), Len);
            Tab.Class.First := 1;
            Tab.Class.Last  := Len;
            Cur_Pos := 1 + Len;
            Field_Offset := 1;
         when others =>
            raise Bad_Input;
      end case;

      Len := Get_Field_Length (Key_Data_Pair.Key, Field_Offset + 1);
      Get_Field
        (Key_Data_Pair.Key,
         Field_Offset + 1,
         Tab.Buffer (Cur_Pos .. Total_Len),
         Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, Field_Offset + 3);
      Get_Field
        (Key_Data_Pair.Key,
         Field_Offset + 3,
         Tab.Buffer (Cur_Pos .. Total_Len),
         Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      --  ??? if we add a call to Get_Field here, it breaks things (see
      --  regression suite), which is worrisome.
      --  Put_Line ("val1=" & Get_Field (Key_Data_Pair.Key, Field_Offset + 2));

      Parse_Position
       (Get_Field (Key_Data_Pair.Key, Field_Offset + 2),
        Tab.Start_Position);
      Parse_Position (Get_Field (Key_Data_Pair.Data, 1), Tab.End_Position);

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
      Remove_Brackets
        (Get_Field (Key_Data_Pair.Data, 6),
         Tab.Buffer,
         Cur_Pos,
         Tab.Comments);

      Tab.Template_Parameters :=
        Get_Segment_From_Comment (Tab.Comments, Tab.Buffer, "template_args=");
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      return Tab;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out GV_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Len := Get_Field_Length (Key_Data_Pair.Data, 3);
      Get_Field (Key_Data_Pair.Data, 3, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Value_Type);

      Len := Get_Field_Length (Key_Data_Pair.Data, 6);
      Get_Field (Key_Data_Pair.Data, 6, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);

      Tab.Type_Start_Position :=
        Get_Position_From_Comment (Tab.Comments, Tab.Buffer, "type_beg=");
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out IN_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Class.First := Cur_Pos;
      Tab.Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Base_Class.First := Cur_Pos;
      Tab.Base_Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Get_Field (Key_Data_Pair.Key, 4, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Len := Get_Field_Length (Key_Data_Pair.Data, 6);
      Get_Field (Key_Data_Pair.Data, 6, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out IU_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Included_File.First := Cur_Pos;
      Tab.Included_File.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Included_From_File.First := Cur_Pos;
      Tab.Included_From_File.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Included_At_Position);

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out IV_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Class.First := Cur_Pos;
      Tab.Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Get_Field (Key_Data_Pair.Key, 4, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Len := Get_Field_Length (Key_Data_Pair.Data, 3);
      Get_Field (Key_Data_Pair.Data, 3, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Value_Type);

      Len := Get_Field_Length (Key_Data_Pair.Data, 6);
      Get_Field (Key_Data_Pair.Data, 6, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out LV_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Function_Name.First := Cur_Pos;
      Tab.Function_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Get_Field (Key_Data_Pair.Key, 4, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Len := Get_Field_Length (Key_Data_Pair.Data, 3);
      Get_Field (Key_Data_Pair.Data, 3, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Class);

      Len := Get_Field_Length (Key_Data_Pair.Data, 4);
      Get_Field (Key_Data_Pair.Data, 4, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Value_Type);

      Len := Get_Field_Length (Key_Data_Pair.Data, 5);
      Get_Field (Key_Data_Pair.Data, 5, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Arg_Types);

      Len := Get_Field_Length (Key_Data_Pair.Data, 6);
      Get_Field (Key_Data_Pair.Data, 6, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
      Tab.Type_Start_Position :=
        Get_Position_From_Comment (Tab.Comments, Tab.Buffer, "type_beg=");
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out MA_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Len := Get_Field_Length (Key_Data_Pair.Data, 6);
      Get_Field (Key_Data_Pair.Data, 6, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out MD_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Class.First := Cur_Pos;
      Tab.Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Get_Field (Key_Data_Pair.Key, 4, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Len := Get_Field_Length (Key_Data_Pair.Data, 3);
      Get_Field (Key_Data_Pair.Data, 3, Buffer, Len);
      Remove_Brackets
        (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Return_Type);

      Len := Get_Field_Length (Key_Data_Pair.Data, 4);
      Get_Field (Key_Data_Pair.Data, 4, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Arg_Types);

      Len := Get_Field_Length (Key_Data_Pair.Data, 6);
      Get_Field (Key_Data_Pair.Data, 6, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);

      Tab.Template_Parameters :=
        Get_Segment_From_Comment (Tab.Comments, Tab.Buffer, "template_args=");
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out REM_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Position);

      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Class.First := Cur_Pos;
      Tab.Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Get_Field (Key_Data_Pair.Key, 4, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Method_Or_Function.First := Cur_Pos;
      Tab.Method_Or_Function.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);

      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out SU_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 1));
      Len := Get_Field_Length (Key_Data_Pair.Data, 4);
      Get_Field (Key_Data_Pair.Data, 4, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out T_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.End_Position);

      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      Len := Get_Field_Length (Key_Data_Pair.Data, 3);
      Get_Field (Key_Data_Pair.Data, 3, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Original);

      Len := Get_Field_Length (Key_Data_Pair.Data, 6);
      Get_Field (Key_Data_Pair.Data, 6, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);

      Tab.Class_Name :=
        Get_Segment_From_Comment (Tab.Comments, Tab.Buffer, "class=");
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out TA_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : constant Integer := Get_Total_Length (Key_Data_Pair.Key)
        + Get_Total_Length (Key_Data_Pair.Data);
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Tab.Buffer := new String (1 .. Total_Len);

      --  Scope
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Scope.First := Cur_Pos;
      Tab.Scope.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      --  Name
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Name.First := Cur_Pos;
      Tab.Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      --  File_Name
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Get_Field (Key_Data_Pair.Key, 4, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      --  Start_Position
      Len := Get_Field_Length (Key_Data_Pair.Key, 3);
      Get_Field (Key_Data_Pair.Key, 3, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Start_Position);

      --  Type_Position
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Type_Position);

      --  Attributes
      Tab.Attributes := Parse_Hex (Get_Field (Key_Data_Pair.Data, 2));

      --  Value_Type
      Len := Get_Field_Length (Key_Data_Pair.Data, 3);
      Get_Field (Key_Data_Pair.Data, 3, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Value_Type);

      --  Template_Args
      Len := Get_Field_Length (Key_Data_Pair.Data, 4);
      Get_Field (Key_Data_Pair.Data, 4, Buffer, Len);
      Remove_Brackets
        (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Template_Parameters);

      --  Comments
      Len := Get_Field_Length (Key_Data_Pair.Data, 6);
      Get_Field (Key_Data_Pair.Data, 6, Buffer, Len);
      Remove_Brackets (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Comments);

      Tab.Class_Name :=
        Get_Segment_From_Comment (Tab.Comments, Tab.Buffer, "class=");
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
   end Parse_Pair;

   procedure Parse_Pair
     (Key_Data_Pair : Pair;
      Tab           : out TO_Table)
   is
      Cur_Pos   : Integer;
      Len       : Integer;
      Total_Len : Integer;
      Buffer    : String (1 .. 4096);

   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Total_Len := Get_Total_Length (Key_Data_Pair.Key) +
         Get_Total_Length (Key_Data_Pair.Data);
      Tab.Buffer := new String (1 .. Total_Len);
      Cur_Pos := Tab.Buffer'First;
      Len := Get_Field_Length (Key_Data_Pair.Key, 1);
      Get_Field (Key_Data_Pair.Key, 1, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Class.First := Cur_Pos;
      Tab.Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 2);
      Get_Field (Key_Data_Pair.Key, 2, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Symbol_Name.First := Cur_Pos;
      Tab.Symbol_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Tab.Symbol :=
         Parse_Symbol (Get_Field (Key_Data_Pair.Key, 3));
      Len := Get_Field_Length (Key_Data_Pair.Key, 4);
      Get_Field (Key_Data_Pair.Key, 4, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Referred_Class.First := Cur_Pos;
      Tab.Referred_Class.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 5);
      Get_Field (Key_Data_Pair.Key, 5, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Referred_Symbol_Name.First := Cur_Pos;
      Tab.Referred_Symbol_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Tab.Referred_Symbol :=
         Parse_Symbol (Get_Field (Key_Data_Pair.Key, 6));
      Len := Get_Field_Length (Key_Data_Pair.Key, 7);
      Get_Field (Key_Data_Pair.Key, 7, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.Access_Type.First := Cur_Pos;
      Tab.Access_Type.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;
      Len := Get_Field_Length (Key_Data_Pair.Key, 9);
      Get_Field (Key_Data_Pair.Key, 9, Tab.Buffer (Cur_Pos .. Total_Len), Len);
      Tab.File_Name.First := Cur_Pos;
      Tab.File_Name.Last := Cur_Pos + Len - 1;
      Cur_Pos := Cur_Pos + Len;

      Len := Get_Field_Length (Key_Data_Pair.Key, 8);
      Get_Field (Key_Data_Pair.Key, 8, Buffer, Len);
      Parse_Position (Buffer (1 .. Len), Tab.Position);
      Len := Get_Field_Length (Key_Data_Pair.Data, 1);
      Get_Field (Key_Data_Pair.Data, 1, Buffer, Len);
      Remove_Brackets
        (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Caller_Argument_Types);
      Len := Get_Field_Length (Key_Data_Pair.Data, 2);
      Get_Field (Key_Data_Pair.Data, 2, Buffer, Len);
      Remove_Brackets
        (Buffer (1 .. Len), Tab.Buffer, Cur_Pos, Tab.Referred_Argument_Types);
      Number_Of_Allocated_Buffers := Number_Of_Allocated_Buffers + 1;
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

   procedure Parse_Position
     (Position_Str : String;
      Position     : out Point)
   is
      N, Num1, Num2 : Integer;
      C : Character;
      Dot_Already_Found : Boolean := False;

   begin
      Num1 := 0;
      Num2 := 0;
      N := 1;

      for J in reverse Position_Str'Range loop
         C := Position_Str (J);

         if ((C < '0') or (C > '9')) and (C /= '.') then
            raise Bad_Input;
         elsif C = '.' then
            Dot_Already_Found := True;
            N := 1;
         else
            if Dot_Already_Found then
               Num1 := Num1 + (Character'Pos (C) - Character'Pos ('0')) * N;
            else
               Num2 := Num2 + (Character'Pos (C) - Character'Pos ('0')) * N;
            end if;

            N := N * 10;
         end if;
      end loop;

      if Dot_Already_Found then
         Position.Line := Num1;
         Position.Column := Num2;
      else
         Position.Line := Num2;
         Position.Column := 0;
      end if;
   end Parse_Position;


   function Parse_Hex (Hex_Str : String) return SN_Attributes is
      Val : Integer;
      C   : Character;

   begin
      if (Hex_Str (1 .. 2) /= "0x") and then (Hex_Str (1 .. 2) /= "0X") then
         raise Bad_Input;
      end if;

      Val := 0;

      for J in 3 .. Hex_Str'Length loop
         C := Hex_Str (J);

         if C in '0' .. '9' then
            Val := Val * 16 + (Character'Pos (C) - Character'Pos ('0'));
         elsif C in 'a' .. 'f' then
            Val := Val * 16 + (Character'Pos (C) - Character'Pos ('a')) + 10;
         elsif C in 'A' .. 'F' then
            Val := Val * 16 + (Character'Pos (C) - Character'Pos ('A')) + 10;
         else
            raise Bad_Input;
         end if;
      end loop;

      return SN_Attributes (Val);
   end Parse_Hex;

   procedure Remove_Brackets
      (Str     :        String;
       Buffer  :        GNAT.OS_Lib.String_Access;
       Cur_Pos : in out Integer;
       Seg     : out    Segment) is
   begin
      Seg.First := Cur_Pos;
      Seg.Last  := Cur_Pos + Str'Length - 3;
      Buffer (Seg.First .. Seg.Last) := Str (Str'First + 1 .. Str'Last - 1);
      Cur_Pos   := Seg.Last + 1;
   end Remove_Brackets;

   -------------------------------
   -- Get_Position_From_Comment --
   -------------------------------

   function Get_Position_From_Comment
     (Comment : Segment;
      Buffer  : GNAT.OS_Lib.String_Access;
      Name    : String) return Point
   is
      J, K : Natural;
      Pos  : Point := Invalid_Point;
   begin
      J := Ada.Strings.Fixed.Index
         (Buffer (Comment.First .. Comment.Last), Name);

      if J /= 0 then
         J := J + Name'Length;
         K := J;

         while K <= Comment.Last loop
            exit when Buffer (K) not in '0' .. '9'
               and then Buffer (K) /= '.';

            K := K + 1;
         end loop;

         Parse_Position (Buffer (J .. K - 1), Pos);
      end if;

      return Pos;
   end Get_Position_From_Comment;

   ------------------------------
   -- Get_Segment_From_Comment --
   ------------------------------

   function Get_Segment_From_Comment
     (Comment : Segment;
      Buffer  : GNAT.OS_Lib.String_Access;
      Name    : String) return Segment
   is
      J, K : Natural;
   begin
      J := Ada.Strings.Fixed.Index
         (Buffer (Comment.First .. Comment.Last), Name);

      if J /= 0 then
         J := J + Name'Length;
         K := J;

         while K <= Comment.Last and then Buffer (K) /= ';' loop
            K := K + 1;
         end loop;

         return (J, K - 1);
      end if;

      return Empty_Segment;
   end Get_Segment_From_Comment;
end SN.DB_Structures;
