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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Strings.Fixed;
with Traces; use Traces;

package body SN.DB_Structures is

   Me : constant Debug_Handle := Create ("SN");

   Bad_Input : exception;
   --  Raised by internal procedures in the case of bad input data

   procedure Get_Position
     (Key : CSF; Field : Integer; Position : out Point);
   --  Parse the Field-th field in Key as a position ("Line.Column" or "Line")

   procedure Parse_Position (Buffer : String; Position : out Point);
   --  Parse Buffer ("Line.Column" or "Line") to extract the position info.

   procedure Get_Hex
     (Key : CSF; Field : Integer; Attr : out SN_Attributes);
   --  Converts C-style hexadecimal string like "0xffff" to integer number

   procedure Get_No_Brackets
     (Key : CSF; Field : Integer; Len : Integer;
      Buffer : String_Access; Pos : in out Integer; Result : out Segment);
   --  Get the value of a specific field, omitting the surrouding {}.

   procedure Get_Field
     (Key : CSF; Field : Integer; Len : Integer;
      Buffer : String_Access; Pos : in out Integer; Result : out Segment);
   --  Get the value of a specific field

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

   procedure Parse_Key
     (Key            : Pair;
      Name           : out Segment;
      File_Name      : out Segment;
      Start_Position : out Point;
      Buffer         : out GNAT.OS_Lib.String_Access;
      Extra_Length   : Integer := 0;
      Start_Index    : Integer := 1);
   --  Parse the key of Key, providing it has the following format:
   --     key  => name?start_position?filename
   --  Extra_Length is added to the length allocated for Buffer. It is the
   --  responsability of the caller to free Buffer.
   --  On exit, Buffer (1 .. File_Name.Last - 1) has been filled with
   --  information pointed to by Name, File_Name and Start_Position.
   --
   --  Start_Index is the field index for the "name" field. This can be used
   --  if the index is of the form
   --      class?name?start_position?filename
   --  for instance.

   function Get_Symbol (Key : CSF; Field : Integer) return Symbol_Type;
   --  Return the Field-th field as a symbol_type

   ------------------
   -- Parse_Symbol --
   ------------------

   function Get_Symbol (Key : CSF; Field : Integer) return Symbol_Type is
      Len : constant Integer := Get_Field_Length (Key, Field);
      Buffer : String (1 .. Len);
   begin
      Get_Field (Key, Field, Buffer, Len);

      case Buffer (1) is
         when 'c' =>
            case Buffer (2) is
               when 'l' =>
                  if Len = 2 then
                     return CL;
                  else
                     return Undef;
                  end if;

               when 'o' =>
                  case Buffer (3) is
                     when 'm' =>
                        if Len = 3 then
                           return COM;
                        else
                           return Undef;
                        end if;

                     when 'n' =>
                        if Len = 3 then
                           return CON;
                        else
                           return Undef;
                        end if;

                     when 'v' =>
                        if Len = 3 then
                           return COV;
                        else
                           return Undef;
                        end if;

                     when others =>
                        return Undef;
                  end case;

               when others =>
                  return Undef;
            end case;

         when 'e' =>
            if Len = 1 then
               return E;
            elsif Len = 2 and then Buffer (2) = 'c' then
               return EC;
            else
               return Undef;
            end if;

         when 'f' =>
            case Buffer (2) is
               when 'd' =>
                  if Len = 2 then
                     return FD;
                  else
                     return Undef;
                  end if;

               when 'r' =>
                  if Len = 2 then
                     return FR;
                  else
                     return Undef;
                  end if;

               when 'u' =>
                  if Len = 2 then
                     return FU;
                  else
                     return Undef;
                  end if;

               when others =>
                  return Undef;
            end case;

         when 'g' =>
            case Buffer (2) is
               when 'v' =>
                  if Len = 2 then
                     return GV;
                  else
                     return Undef;
                  end if;

               when others =>
                  return Undef;
            end case;

         when 'i' =>
            case Buffer (2) is
               when 'n' =>
                  if Len = 2 then
                     return SN_IN;
                  else
                     return Undef;
                  end if;

               when 'u' =>
                  if Len = 2 then
                     return IU;
                  else
                     return Undef;
                  end if;

               when 'v' =>
                  if Len = 2 then
                     return IV;
                  else
                     return Undef;
                  end if;

               when others =>
                  return Undef;
            end case;

         when 'l' =>
            case Buffer (2) is
               when 'v' =>
                  if Len = 2 then
                     return LV;
                  else
                     return Undef;
                  end if;

               when others =>
                  return Undef;
            end case;

         when 'm' =>
            case Buffer (2) is
               when 'a' =>
                  if Len = 2 then
                     return MA;
                  else
                     return Undef;
                  end if;

               when 'd' =>
                  if Len = 2 then
                     return MD;
                  else
                     return Undef;
                  end if;

               when 'i' =>
                  if Len = 2 then
                     return MI;
                  else
                     return Undef;
                  end if;

               when others                => return Undef;
            end case;

         when 's' =>
            case Buffer (2) is
               when 'u' =>
                  if Len = 2 then
                     return SU;
                  else
                     return Undef;
                  end if;

               when others =>
                  return Undef;
            end case;

         when 't' =>
            if Len = 1 then
               return T;
            else
               return Undef;
            end if;

         when 'u' =>
            case Buffer (2) is
               when 'n' =>
                  if Len = 2 then
                     return UN;
                  else
                     return Undef;
                  end if;

               when others =>
                  return Undef;
            end case;

         when others =>
            return Undef;
      end case;
   end Get_Symbol;

   ---------------
   -- Parse_Key --
   ---------------

   procedure Parse_Key
     (Key            : Pair;
      Name           : out Segment;
      File_Name      : out Segment;
      Start_Position : out Point;
      Buffer         : out GNAT.OS_Lib.String_Access;
      Extra_Length   : Integer := 0;
      Start_Index    : Integer := 1)
   is
      Len1 : constant Integer := Get_Field_Length (Key.Key, Start_Index);
      Len3 : constant Integer := Get_Field_Length (Key.Key, Start_Index + 2);
   begin
      Buffer := new String (1 .. Len1 + Len3 + Extra_Length);

      Get_Field (Key.Key, Start_Index, Buffer.all, Len1);
      Name := (1, Len1);

      Get_Field (Key.Key, Start_Index + 2,
                 Buffer (Len1 + 1 .. Buffer'Last), Len3);
      File_Name := (Len1 + 1, Len1 + Len3);

      Get_Position (Key.Key, Start_Index + 1, Start_Position);
   end Parse_Key;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out CL_Table) is
      Pos : Integer;
      Len : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 4);
   begin
      Tab.DBI    := Key_Data_Pair.DBI;
      Parse_Key
        (Key_Data_Pair, Tab.Name, Tab.File_Name, Tab.Start_Position,
         Tab.Buffer, Len);
      Pos := Tab.File_Name.Last + 1;

      Get_Position (Key_Data_Pair.Data, 1, Tab.End_Position);
      Get_Hex      (Key_Data_Pair.Data, 2, Tab.Attributes);
      Get_No_Brackets
        (Key_Data_Pair.Data, 4, Len, Tab.Buffer, Pos, Tab.Template_Parameters);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out CON_Table) is
      Pos   : Integer;
      Len3d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 3);
      Len6d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 6);
   begin
      Tab.DBI    := Key_Data_Pair.DBI;
      Parse_Key
        (Key_Data_Pair, Tab.Name, Tab.File_Name, Tab.Start_Position,
         Tab.Buffer, Len3d + Len6d);
      Pos := Tab.File_Name.Last + 1;

      Get_Position (Key_Data_Pair.Data, 1, Tab.End_Position);
      Get_Hex      (Key_Data_Pair.Data, 2, Tab.Attributes);
      Get_No_Brackets
        (Key_Data_Pair.Data, 3, Len3d, Tab.Buffer, Pos, Tab.Declared_Type);
      Get_No_Brackets
        (Key_Data_Pair.Data, 6, Len6d, Tab.Buffer, Pos, Tab.Comments);
      Tab.Type_Start_Position :=
        Get_Position_From_Comment (Tab.Comments, Tab.Buffer, "type_beg=");
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out E_Table) is
   begin
      Tab.DBI    := Key_Data_Pair.DBI;
      Parse_Key
        (Key_Data_Pair, Tab.Name, Tab.File_Name, Tab.Start_Position,
         Tab.Buffer, 0);
      Get_Position (Key_Data_Pair.Data, 1, Tab.End_Position);
      Get_Hex      (Key_Data_Pair.Data, 2, Tab.Attributes);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out EC_Table) is
      Pos   : Integer;
      Len3d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 3);
   begin
      Tab.DBI    := Key_Data_Pair.DBI;
      Parse_Key
        (Key_Data_Pair, Tab.Name, Tab.File_Name, Tab.Start_Position,
         Tab.Buffer, Len3d);
      Pos := Tab.File_Name.Last + 1;

      Get_Position (Key_Data_Pair.Data, 1, Tab.End_Position);
      Get_Hex      (Key_Data_Pair.Data, 2, Tab.Attributes);
      Get_No_Brackets
        (Key_Data_Pair.Data, 3, Len3d, Tab.Buffer, Pos, Tab.Enumeration_Name);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out FD_Table) is
      Pos       : Integer;
      Len3d     : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 3);
      Len4d     : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 4);
      Len6d     : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 6);

   begin
      Tab.DBI    := Key_Data_Pair.DBI;
      Parse_Key
        (Key_Data_Pair, Tab.Name, Tab.File_Name, Tab.Start_Position,
         Tab.Buffer, Len3d + Len4d + Len6d);
      Pos := Tab.File_Name.Last + 1;

      Get_Position (Key_Data_Pair.Data, 1, Tab.End_Position);
      Get_Hex      (Key_Data_Pair.Data, 2, Tab.Attributes);
      Get_No_Brackets
        (Key_Data_Pair.Data, 3, Len3d, Tab.Buffer, Pos, Tab.Return_Type);
      Get_No_Brackets
        (Key_Data_Pair.Data, 4, Len4d, Tab.Buffer, Pos, Tab.Arg_Types);
      Get_No_Brackets
        (Key_Data_Pair.Data, 6, Len6d, Tab.Buffer, Pos, Tab.Comments);
      Tab.Template_Parameters :=
        Get_Segment_From_Comment (Tab.Comments, Tab.Buffer, "template_args=");
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out FIL_Table) is
      Pos  : Integer := 1;
      Len1  : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 1);
      Len3  : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 3);
      Len4  : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 4);
      Len4d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 4);
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Tab.Buffer  := new String (1 .. Len1 + Len3 + Len4 + Len4d);

      Get_Field (Key_Data_Pair.Key, 1, Len1, Tab.Buffer, Pos, Tab.File_Name);
      Get_Position (Key_Data_Pair.Key, 2, Tab.Start_Position);
      Get_Field (Key_Data_Pair.Key, 3, Len3, Tab.Buffer, Pos, Tab.Class);
      Get_Field (Key_Data_Pair.Key, 4, Len4, Tab.Buffer, Pos, Tab.Identifier);
      Tab.Symbol := Get_Symbol (Key_Data_Pair.Key, 5);

      Get_Position (Key_Data_Pair.Data, 1, Tab.End_Position);
      Get_Position (Key_Data_Pair.Data, 2, Tab.Highlight_Start_Position);
      Get_Position (Key_Data_Pair.Data, 3, Tab.Highlight_End_Position);
      Get_No_Brackets
        (Key_Data_Pair.Data, 4, Len4d, Tab.Buffer, Pos,
         Tab.Types_Of_Arguments);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out FR_Table) is
      Pos   : Integer;
      Len3d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 3);
      Len4d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 4);
   begin
      Tab.DBI    := Key_Data_Pair.DBI;
      Parse_Key
        (Key_Data_Pair, Tab.Name, Tab.File_Name, Tab.Start_Position,
         Tab.Buffer, Len3d + Len4d);
      Pos := Tab.File_Name.Last + 1;

      Get_Position (Key_Data_Pair.Data, 1, Tab.End_Position);
      Get_Hex      (Key_Data_Pair.Data, 2, Tab.Attributes);
      Get_No_Brackets
        (Key_Data_Pair.Data, 3, Len3d, Tab.Buffer, Pos, Tab.Return_Type);
      Get_No_Brackets
        (Key_Data_Pair.Data, 4, Len4d, Tab.Buffer, Pos, Tab.Arg_Types);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out FU_Table) is
      Pos   : Integer;
      Len1  : Integer;
      Len3d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 3);
      Len4d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 4);
      Len6d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 6);
      Num_Of_Fields  : constant Integer := Get_Field_Count (Key_Data_Pair.Key);
   begin
      Tab.DBI    := Key_Data_Pair.DBI;

      --  Do we have a ".fu" table ?

      if Num_Of_Fields = 3 then
         Parse_Key
           (Key_Data_Pair, Tab.Name, Tab.File_Name, Tab.Start_Position,
            Tab.Buffer, Len3d + Len4d + Len6d + 1);
         Pos := Tab.File_Name.Last + 1;

         Tab.Buffer (Pos) := '#';
         Tab.Class := (Pos, Pos);
         Pos := Pos + 1;

      --  Else we have a ".mi" table
      else
         Len1 := Get_Field_Length (Key_Data_Pair.Key, 1);
         Parse_Key
           (Key_Data_Pair, Tab.Name, Tab.File_Name, Tab.Start_Position,
            Tab.Buffer, Len1 + Len3d + Len4d + Len6d, Start_Index => 2);
         Pos := Tab.File_Name.Last + 1;

         Get_Field (Key_Data_Pair.Key, 1, Len1, Tab.Buffer, Pos, Tab.Class);
      end if;

      Get_Position (Key_Data_Pair.Data, 1, Tab.End_Position);
      Get_Hex      (Key_Data_Pair.Data, 2, Tab.Attributes);
      Get_No_Brackets
        (Key_Data_Pair.Data, 3, Len3d, Tab.Buffer, Pos, Tab.Return_Type);
      Get_No_Brackets
        (Key_Data_Pair.Data, 4, Len4d, Tab.Buffer, Pos, Tab.Arg_Types);
      Get_No_Brackets
        (Key_Data_Pair.Data, 6, Len6d, Tab.Buffer, Pos, Tab.Comments);
      Tab.Template_Parameters := Get_Segment_From_Comment
        (Tab.Comments, Tab.Buffer, "template_args=");
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out GV_Table) is
      Pos : Integer;
      Len3d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 3);
      Len6d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 6);
   begin
      Tab.DBI    := Key_Data_Pair.DBI;
      Parse_Key
        (Key_Data_Pair, Tab.Name, Tab.File_Name, Tab.Start_Position,
         Tab.Buffer, Len3d + Len6d);
      Pos := Tab.File_Name.Last + 1;

      Get_Position (Key_Data_Pair.Data, 1, Tab.End_Position);
      Get_Hex      (Key_Data_Pair.Data, 2, Tab.Attributes);
      Get_No_Brackets
        (Key_Data_Pair.Data, 3, Len3d, Tab.Buffer, Pos, Tab.Value_Type);
      Get_No_Brackets
        (Key_Data_Pair.Data, 6, Len6d, Tab.Buffer, Pos, Tab.Comments);
      Tab.Type_Start_Position :=
        Get_Position_From_Comment (Tab.Comments, Tab.Buffer, "type_beg=");
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out IN_Table) is
      Pos : Integer;
      Len1 : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 1);
   begin
      Tab.DBI    := Key_Data_Pair.DBI;
      Parse_Key
        (Key_Data_Pair, Tab.Base_Class, Tab.File_Name, Tab.Start_Position,
         Tab.Buffer, Len1, Start_Index => 2);
      Pos := Tab.File_Name.Last + 1;

      Get_Field (Key_Data_Pair.Key, 1, Len1, Tab.Buffer, Pos, Tab.Class);
      Get_Position (Key_Data_Pair.Data, 1, Tab.End_Position);
      Get_Hex      (Key_Data_Pair.Data, 2, Tab.Attributes);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out IU_Table) is
      Pos  : Integer := 1;
      Len1 : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 1);
      Len3 : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 3);
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Tab.Buffer := new String (1 .. Len1 + Len3);

      Get_Field
        (Key_Data_Pair.Key, 1, Len1, Tab.Buffer, Pos, Tab.Included_File);
      Get_Field
        (Key_Data_Pair.Key, 3, Len3, Tab.Buffer, Pos, Tab.Included_From_File);
      Get_Position (Key_Data_Pair.Key, 3, Tab.Included_At_Position);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out IV_Table) is
      Pos : Integer;
      Len1  : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 1);
      Len3d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 3);
   begin
      Tab.DBI    := Key_Data_Pair.DBI;
      Parse_Key
        (Key_Data_Pair, Tab.Name, Tab.File_Name, Tab.Start_Position,
         Tab.Buffer, Len1 + Len3d, Start_Index => 2);
      Pos := Tab.File_Name.Last + 1;

      Get_Field (Key_Data_Pair.Key, 1, Len1, Tab.Buffer, Pos, Tab.Class);
      Get_Position (Key_Data_Pair.Data, 1, Tab.End_Position);
      Get_Hex      (Key_Data_Pair.Data, 2, Tab.Attributes);
      Get_No_Brackets
        (Key_Data_Pair.Data, 3, Len3d, Tab.Buffer, Pos, Tab.Value_Type);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out LV_Table) is
      Pos : Integer;
      Len1  : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 1);
      Len3d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 3);
      Len4d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 4);
      Len5d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 5);
      Len6d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 6);
   begin
      Tab.DBI    := Key_Data_Pair.DBI;
      Parse_Key
        (Key_Data_Pair, Tab.Name, Tab.File_Name, Tab.Start_Position,
         Tab.Buffer, Len1 + Len3d + Len4d + Len5d + Len6d, Start_Index => 2);
      Pos := Tab.File_Name.Last + 1;

      Get_Field
        (Key_Data_Pair.Key, 1, Len1, Tab.Buffer, Pos, Tab.Function_Name);
      Get_Position (Key_Data_Pair.Data, 1, Tab.End_Position);
      Get_Hex      (Key_Data_Pair.Data, 2, Tab.Attributes);
      Get_No_Brackets
        (Key_Data_Pair.Data, 3, Len3d, Tab.Buffer, Pos, Tab.Class);
      Get_No_Brackets
        (Key_Data_Pair.Data, 4, Len4d, Tab.Buffer, Pos, Tab.Value_Type);
      Get_No_Brackets
        (Key_Data_Pair.Data, 5, Len5d, Tab.Buffer, Pos, Tab.Arg_Types);
      Get_No_Brackets
        (Key_Data_Pair.Data, 6, Len6d, Tab.Buffer, Pos, Tab.Comments);
      Tab.Type_Start_Position :=
        Get_Position_From_Comment (Tab.Comments, Tab.Buffer, "type_beg=");
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out MA_Table) is
   begin
      Tab.DBI    := Key_Data_Pair.DBI;
      Parse_Key
        (Key_Data_Pair, Tab.Name, Tab.File_Name, Tab.Start_Position,
         Tab.Buffer);
      Get_Position (Key_Data_Pair.Data, 1, Tab.End_Position);
      Get_Hex      (Key_Data_Pair.Data, 2, Tab.Attributes);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out MD_Table) is
      Pos   : Integer;
      Len1  : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 1);
      Len3d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 3);
      Len4d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 4);
      Len6d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 6);
   begin
      Tab.DBI    := Key_Data_Pair.DBI;
      Parse_Key
        (Key_Data_Pair, Tab.Name, Tab.File_Name, Tab.Start_Position,
         Tab.Buffer, Len1 + Len3d + Len4d + Len6d, Start_Index => 2);
      Pos := Tab.File_Name.Last + 1;

      Get_Field (Key_Data_Pair.Key, 1, Len1, Tab.Buffer, Pos, Tab.Class);
      Get_Position (Key_Data_Pair.Data, 1, Tab.End_Position);
      Get_Hex      (Key_Data_Pair.Data, 2, Tab.Attributes);
      Get_No_Brackets
        (Key_Data_Pair.Data, 3, Len3d, Tab.Buffer, Pos, Tab.Return_Type);
      Get_No_Brackets
        (Key_Data_Pair.Data, 4, Len4d, Tab.Buffer, Pos, Tab.Arg_Types);
      Get_No_Brackets
        (Key_Data_Pair.Data, 6, Len6d, Tab.Buffer, Pos, Tab.Comments);
      Tab.Template_Parameters :=
        Get_Segment_From_Comment (Tab.Comments, Tab.Buffer, "template_args=");
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out T_Table) is
      Pos   : Integer;
      Len3d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 3);
      Len6d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 6);
   begin
      Tab.DBI    := Key_Data_Pair.DBI;
      Parse_Key
        (Key_Data_Pair, Tab.Name, Tab.File_Name, Tab.Start_Position,
         Tab.Buffer, Len3d + Len6d);
      Pos := Tab.File_Name.Last + 1;

      Get_Position (Key_Data_Pair.Data, 1, Tab.End_Position);
      Get_Hex      (Key_Data_Pair.Data, 2, Tab.Attributes);
      Get_No_Brackets
        (Key_Data_Pair.Data, 3, Len3d, Tab.Buffer, Pos, Tab.Original);
      Get_No_Brackets
        (Key_Data_Pair.Data, 6, Len6d, Tab.Buffer, Pos, Tab.Comments);

      Tab.Class_Name :=
        Get_Segment_From_Comment (Tab.Comments, Tab.Buffer, "class=");
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out TA_Table) is
      Pos : Integer := 1;
      Len1  : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 1);
      Len3d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 3);
      Len4d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 4);
      Len6d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 6);
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Parse_Key
        (Key_Data_Pair, Tab.Name, Tab.File_Name, Tab.Start_Position,
         Tab.Buffer, Len1 + Len3d + Len4d + Len6d, Start_Index => 2);
      Pos := Tab.File_Name.Last + 1;

      Get_Field (Key_Data_Pair.Key, 1, Len1, Tab.Buffer, Pos, Tab.Scope);
      Get_Position (Key_Data_Pair.Data, 1, Tab.Type_Position);
      Get_Hex      (Key_Data_Pair.Data, 2, Tab.Attributes);
      Get_No_Brackets
        (Key_Data_Pair.Data, 3, Len3d, Tab.Buffer, Pos, Tab.Value_Type);
      Get_No_Brackets
        (Key_Data_Pair.Data, 4, Len4d, Tab.Buffer, Pos,
         Tab.Template_Parameters);
      Get_No_Brackets
        (Key_Data_Pair.Data, 6, Len6d, Tab.Buffer, Pos, Tab.Comments);
      Tab.Class_Name :=
        Get_Segment_From_Comment (Tab.Comments, Tab.Buffer, "class=");
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out TO_Table) is
      Pos : Integer := 1;
      Len1  : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 1);
      Len2  : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 2);
      Len4  : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 4);
      Len5  : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 5);
      Len7  : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 7);
      Len9  : constant Integer := Get_Field_Length (Key_Data_Pair.Key, 9);
      Len1d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 1);
      Len2d : constant Integer := Get_Field_Length (Key_Data_Pair.Data, 2);
   begin
      Tab.DBI := Key_Data_Pair.DBI;
      Tab.Buffer := new String
        (1 .. Len1 + Len2 + Len4 + Len5 + Len7 + Len9 + Len1d + Len2d);

      Get_Field (Key_Data_Pair.Key, 1, Len1, Tab.Buffer, Pos, Tab.Class);
      Get_Field (Key_Data_Pair.Key, 2, Len2, Tab.Buffer, Pos, Tab.Symbol_Name);
      Tab.Symbol := Get_Symbol (Key_Data_Pair.Key, 3);
      Get_Field
        (Key_Data_Pair.Key, 4, Len4, Tab.Buffer, Pos, Tab.Referred_Class);
      Get_Field
        (Key_Data_Pair.Key, 5, Len5, Tab.Buffer, Pos,
         Tab.Referred_Symbol_Name);
      Tab.Referred_Symbol := Get_Symbol (Key_Data_Pair.Key, 6);
      Get_Field
        (Key_Data_Pair.Key, 7, Len7, Tab.Buffer, Pos, Tab.Access_Type);
      Get_Field
        (Key_Data_Pair.Key, 9, Len9, Tab.Buffer, Pos, Tab.File_Name);
      Get_Position (Key_Data_Pair.Key, 8, Tab.Position);
      Get_No_Brackets
        (Key_Data_Pair.Data, 1, Len1d, Tab.Buffer, Pos,
         Tab.Caller_Argument_Types);
      Get_No_Brackets
        (Key_Data_Pair.Data, 2, Len2d, Tab.Buffer, Pos,
         Tab.Referred_Argument_Types);
   end Parse_Pair;

   ----------
   -- Free --
   ----------

   procedure Free (target : in out CL_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out CON_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out E_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out EC_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out FD_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out FIL_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out FR_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out FU_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out GV_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out IN_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out IU_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out IV_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out LV_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out MA_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out MD_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out T_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out TA_Table) is
   begin
      Free (target.Buffer);
   end Free;

   procedure Free (target : in out TO_Table) is
   begin
      Free (target.Buffer);
   end Free;

   ------------------
   -- Get_Position --
   ------------------

   procedure Get_Position
     (Key : CSF; Field : Integer; Position : out Point)
   is
      Len    : constant Integer := Get_Field_Length (Key, Field);
      Buffer : String (1 .. Len);
   begin
      Get_Field (Key, Field, Buffer, Len);
      Parse_Position (Buffer, Position);
   end Get_Position;

   --------------------
   -- Parse_Position --
   --------------------

   procedure Parse_Position (Buffer : String; Position : out Point) is
      Num1   : Integer := 0;
      Num2   : Integer := 0;
      C      : Character;
      Dot_Already_Found : Boolean := False;
   begin
      for J in Buffer'Range loop
         C := Buffer (J);

         if C = '.' then
            Dot_Already_Found := True;

         elsif C >= '0' and then C <= '9' then
            if Dot_Already_Found then
               Num2 := 10 * Num2 + Character'Pos (C) - Character'Pos ('0');
            else
               Num1 := 10 * Num1 + Character'Pos (C) - Character'Pos ('0');
            end if;

         else
            Trace (Me, "Parse_Position: " & Buffer
                   & "--" & C'Img & Num1'Img & Num2'Img);
            raise Bad_Input;
         end if;
      end loop;

      Position := (Line => Num1, Column => Num2);
   end Parse_Position;

   -------------
   -- Get_Hex --
   -------------

   procedure Get_Hex (Key : CSF; Field : Integer; Attr : out SN_Attributes) is
      Len    : constant Integer := Get_Field_Length (Key, Field);
      Buffer : String (1 .. Len);
      Val    : Integer := 0;
      C      : Character;
   begin
      Get_Field (Key, Field, Buffer, Len);

      if Len < 2
        or else Buffer (1) /= '0'
        or else (Buffer (2) /= 'x' and then Buffer (2) /= 'X')
      then
         raise Bad_Input;
      end if;

      for J in 3 .. Len loop
         C := Buffer (J);

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

      Attr := SN_Attributes (Val);
   end Get_Hex;

   ---------------------
   -- Get_No_Brackets --
   ---------------------

   procedure Get_No_Brackets
     (Key : CSF; Field : Integer; Len : Integer;
      Buffer : String_Access; Pos : in out Integer; Result : out Segment) is
   begin
      Get_Field (Key, Field, Buffer (Pos .. Buffer'Last), Len);
      Result := (Pos + 1, Pos + Len - 2);
      Pos := Pos + Len;
   end Get_No_Brackets;

   ---------------
   -- Get_Field --
   ---------------

   procedure Get_Field
     (Key : CSF; Field : Integer; Len : Integer;
      Buffer : String_Access; Pos : in out Integer; Result : out Segment) is
   begin
      Get_Field (Key, Field, Buffer (Pos .. Buffer'Last), Len);
      Result := (Pos, Pos + Len - 1);
      Pos := Pos + Len;
   end Get_Field;

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
