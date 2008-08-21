-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2008, AdaCore             --
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

with Ada.Strings.Fixed;
with Interfaces.C.Strings;
with System;               use System;
with Traces;               use Traces;

package body SN.DB_Structures is

   Me : constant Debug_Handle := Create ("DB_Structs");

   Bad_Input : exception;
   --  Raised by internal procedures in the case of bad input data

   procedure Get_Position
     (Key : CSF; Field : Integer;
      Buffer : Unchecked_String_Access; Position : out Point);
   --  Parse the Field-th field in Key as a position ("Line.Column" or "Line")

   procedure Parse_Position
     (Buffer : Unchecked_String_Access; Seg : Segment; Position : out Point);
   --  Parse Buffer ("Line.Column" or "Line") to extract the position info.

   procedure Get_Hex
     (Key    : CSF;
      Field  : Integer;
      Buffer : Unchecked_String_Access;
      Attr   : out SN_Attributes);
   --  Converts C-style hexadecimal string like "0xffff" to integer number

   procedure Get_Field (Key : CSF; Index : Positive; Result : out Segment);
   pragma Inline (Get_Field);
   --  Return the Index-th field of Key, starting at index 1.

   procedure Get_No_Brackets
     (Key : CSF; Field : Integer; Result : out Segment);
   pragma Inline (Get_No_Brackets);
   --  Same as Get_Field, but omits the surrounding {}.

   function Get_Position_From_Comment
     (Buffer   : Unchecked_String_Access;
      Comment  : Segment;
      Name     : String) return Point;
   --  Parses comment string to find Name=Value pair, then
   --  parses value into point
   --  For efficiency, it is assumed that Name ends with '='.

   procedure Parse_Key
     (Key            : CSF;
      Buffer         : Buffer_String;
      Name           : out Segment;
      File_Name      : out Segment;
      Start_Position : out Point;
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

   function Get_Symbol
     (Key : CSF; Field : Integer; Buffer : Buffer_String) return Symbol_Type;
   --  Return the Field-th field as a symbol_type

   procedure Copy
     (To   : out Buffer_String;
      From : Interfaces.C.Strings.chars_ptr;
      Size : Natural);
   --  Copy From C key or data pointer to To. Note that we do not have to care
   --  about the string ending. The fields indexes are computed using the C
   --  pointer From in the Parse_Pair procedures and they will be used to
   --  reference the To Buffer_String.

   ----------
   -- Copy --
   ----------

   procedure Copy
     (To   : out Buffer_String;
      From : Interfaces.C.Strings.chars_ptr;
      Size : Natural)
   is
      use Interfaces.C.Strings;

      procedure strcpy (To : Address; From : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, strcpy);

   begin
      if Size >= Buffer_String_Size then
         To (To'First) := ASCII.NUL;
      else
         strcpy (To'Address, From);
      end if;
   end Copy;

   ---------------
   -- Get_Field --
   ---------------

   procedure Get_Field (Key : CSF; Index : Positive; Result : out Segment) is
   begin
      Result.First := Key.Fields (Index - 1) + Buffer_String'First;
      Result.Last  := Key.Fields (Index) - 2 + Buffer_String'First;
   end Get_Field;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Key : CSF; Field : Integer; Buffer : Buffer_String) return Symbol_Type
   is
      Seg : Segment;
   begin
      Get_Field (Key, Field, Seg);

      case Buffer (Seg.First) is
         when 'c' =>
            case Buffer (Seg.First + 1) is
               when 'l' =>
                  if Seg.Last = Seg.First + 1 then
                     return CL;
                  else
                     Trace (Me, "Unknown symbol: "
                            & Buffer (Seg.First .. Seg.Last));
                     return Undef;
                  end if;

               when 'o' =>
                  case Buffer (Seg.First + 2) is
                     when 'm' =>
                        if Seg.Last = Seg.First + 2 then
                           return COM;
                        else
                           Trace (Me, "Unknown symbol: "
                                  & Buffer (Seg.First .. Seg.Last));
                           return Undef;
                        end if;

                     when 'n' =>
                        if Seg.Last = Seg.First + 2 then
                           return CON;
                        else
                           Trace (Me, "Unknown symbol: "
                                  & Buffer (Seg.First .. Seg.Last));
                           return Undef;
                        end if;

                     when 'v' =>
                        if Seg.Last = Seg.First + 2 then
                           return COV;
                        else
                           Trace (Me, "Unknown symbol: "
                                  & Buffer (Seg.First .. Seg.Last));
                           return Undef;
                        end if;

                     when others =>
                        Trace (Me, "Unknown symbol: "
                               & Buffer (Seg.First .. Seg.Last));
                        return Undef;
                  end case;

               when others =>
                  Trace (Me, "Unknown symbol: "
                         & Buffer (Seg.First .. Seg.Last));
                  return Undef;
            end case;

         when 'e' =>
            if Seg.First = Seg.Last then
               return E;
            elsif Seg.First + 1 = Seg.Last
              and then Buffer (Seg.Last) = 'c'
            then
               return EC;
            else
               Trace (Me, "Unknown symbol: "
                      & Buffer (Seg.First .. Seg.Last));
               return Undef;
            end if;

         when 'f' =>
            case Buffer (Seg.First + 1) is
               when 'd' =>
                  if Seg.First + 1 = Seg.Last then
                     return FD;
                  else
                     Trace (Me, "Unknown symbol: "
                            & Buffer (Seg.First .. Seg.Last));
                     return Undef;
                  end if;

               when 'r' =>
                  if Seg.First + 1 = Seg.Last then
                     return FR;
                  else
                     Trace (Me, "Unknown symbol: "
                            & Buffer (Seg.First .. Seg.Last));
                     return Undef;
                  end if;

               when 'u' =>
                  if Seg.First + 1 = Seg.Last then
                     return FU;
                  else
                     Trace (Me, "Unknown symbol: "
                            & Buffer (Seg.First .. Seg.Last));
                     return Undef;
                  end if;

               when others =>
                  Trace (Me, "Unknown symbol: "
                         & Buffer (Seg.First .. Seg.Last));
                  return Undef;
            end case;

         when 'g' =>
            case Buffer (Seg.First + 1) is
               when 'v' =>
                  if Seg.First + 1 = Seg.Last then
                     return GV;
                  else
                     Trace (Me, "Unknown symbol: "
                            & Buffer (Seg.First .. Seg.Last));
                     return Undef;
                  end if;

               when others =>
                  Trace (Me, "Unknown symbol: "
                         & Buffer (Seg.First .. Seg.Last));
                  return Undef;
            end case;

         when 'i' =>
            case Buffer (Seg.First + 1) is
               when 'n' =>
                  if Seg.First + 1 = Seg.Last then
                     return SN_IN;
                  else
                     Trace (Me, "Unknown symbol: "
                            & Buffer (Seg.First .. Seg.Last));
                     return Undef;
                  end if;

               when 'u' =>
                  if Seg.First + 1 = Seg.Last then
                     return IU;
                  else
                     Trace (Me, "Unknown symbol: "
                            & Buffer (Seg.First .. Seg.Last));
                     return Undef;
                  end if;

               when 'v' =>
                  if Seg.First + 1 = Seg.Last then
                     return IV;
                  else
                     Trace (Me, "Unknown symbol: "
                            & Buffer (Seg.First .. Seg.Last));
                     return Undef;
                  end if;

               when others =>
                  Trace (Me, "Unknown symbol: "
                         & Buffer (Seg.First .. Seg.Last));
                  return Undef;
            end case;

         when 'l' =>
            case Buffer (Seg.First + 1) is
               when 'v' =>
                  if Seg.First + 1 = Seg.Last then
                     return LV;
                  else
                     Trace (Me, "Unknown symbol: "
                            & Buffer (Seg.First .. Seg.Last));
                     return Undef;
                  end if;

               when others =>
                  Trace (Me, "Unknown symbol: "
                         & Buffer (Seg.First .. Seg.Last));
                  return Undef;
            end case;

         when 'm' =>
            case Buffer (Seg.First + 1) is
               when 'a' =>
                  if Seg.First + 1 = Seg.Last then
                     return MA;
                  else
                     Trace (Me, "Unknown symbol: "
                            & Buffer (Seg.First .. Seg.Last));
                     return Undef;
                  end if;

               when 'd' =>
                  if Seg.First + 1 = Seg.Last then
                     return MD;
                  else
                     Trace (Me, "Unknown symbol: "
                            & Buffer (Seg.First .. Seg.Last));
                     return Undef;
                  end if;

               when 'i' =>
                  if Seg.First + 1 = Seg.Last then
                     return MI;
                  else
                     Trace (Me, "Unknown symbol: "
                            & Buffer (Seg.First .. Seg.Last));
                     return Undef;
                  end if;

               when others =>
                  Trace (Me, "Unknown symbol: "
                         & Buffer (Seg.First .. Seg.Last));
                  return Undef;
            end case;

         when 's' =>
            case Buffer (Seg.First + 1) is
               when 'u' =>
                  if Seg.First + 1 = Seg.Last then
                     return SU;
                  else
                     Trace (Me, "Unknown symbol: "
                            & Buffer (Seg.First .. Seg.Last));
                     return Undef;
                  end if;

               when others =>
                  Trace (Me, "Unknown symbol: "
                         & Buffer (Seg.First .. Seg.Last));
                  return Undef;
            end case;

         when 't' =>
            if Seg.First = Seg.Last then
               return T;
            else
               if Buffer (Seg.First + 1) = 'a'
                 and then Seg.First + 1 = Seg.Last
               then
                  return TA;
               end if;

               Trace (Me, "Unknown symbol: "
                      & Buffer (Seg.First .. Seg.Last));
               return Undef;
            end if;

         when 'u' =>
            case Buffer (Seg.First + 1) is
               when 'n' =>
                  if Seg.First + 1 = Seg.Last then
                     return UN;
                  else
                     Trace (Me, "Unknown symbol: "
                            & Buffer (Seg.First .. Seg.Last));
                     return Undef;
                  end if;

               when 'd' =>
                  if Seg.First + 1 = Seg.Last then
                     return UD;
                  else
                     Trace (Me, "Unknown symbol: "
                            & Buffer (Seg.First .. Seg.Last));
                     return Undef;
                  end if;

               when others =>
                  Trace (Me, "Unknown symbol: "
                         & Buffer (Seg.First .. Seg.Last));
                  return Undef;
            end case;

         when others =>
            Trace (Me, "Unknown symbol: "
                   & Buffer (Seg.First .. Seg.Last));
            return Undef;
      end case;
   end Get_Symbol;

   ---------------
   -- Parse_Key --
   ---------------

   procedure Parse_Key
     (Key            : CSF;
      Buffer         : Buffer_String;
      Name           : out Segment;
      File_Name      : out Segment;
      Start_Position : out Point;
      Start_Index    : Integer := 1) is
   begin
      Get_Field    (Key, Start_Index,     Name);
      Get_Field    (Key, Start_Index + 2, File_Name);
      Get_Position
        (Key, Start_Index + 1,
         To_Unchecked_String (Buffer'Address), Start_Position);
   end Parse_Key;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out CL_Table) is
      Key, Data : CSF;
      Buffer    : Unchecked_String_Access;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 2);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);

      Buffer       := To_Unchecked_String (Key_Data_Pair.Data);
      Parse_Key    (Key, Tab.Key, Tab.Name, Tab.File_Name, Tab.Start_Position);
      Get_Position (Data, 1, Buffer, Tab.End_Position);
      Get_Hex      (Data, 2, Buffer, Tab.Attributes);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out CON_Table) is
      Key, Data : CSF;
      Comments : Segment;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 6);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);
      Copy (Tab.Data, Key_Data_Pair.Data, Key_Data_Pair.Data_Size);

      Parse_Key    (Key, Tab.Key, Tab.Name, Tab.File_Name, Tab.Start_Position);
      Get_No_Brackets (Data, 3, Tab.Declared_Type);
      Get_No_Brackets (Data, 6, Comments);
      Tab.Type_Start_Position := Get_Position_From_Comment
          (To_Unchecked_String (Key_Data_Pair.Data), Comments, "type_beg=");
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out E_Table) is
      Key, Data : CSF;
      Buffer    : Unchecked_String_Access;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 1);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);

      Buffer := To_Unchecked_String (Key_Data_Pair.Data);
      Parse_Key    (Key, Tab.Key, Tab.Name, Tab.File_Name, Tab.Start_Position);
      Get_Position (Data, 1, Buffer, Tab.End_Position);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out EC_Table) is
      Key, Data : CSF;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 3);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);
      Copy (Tab.Data, Key_Data_Pair.Data, Key_Data_Pair.Data_Size);

      Parse_Key    (Key, Tab.Key, Tab.Name, Tab.File_Name, Tab.Start_Position);
      Get_No_Brackets (Data, 3, Tab.Enumeration_Name);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out F_Table) is
      Key, Data : CSF;
   begin
      CSF_Init (Key_Data_Pair.Key,  Key);
      CSF_Init (Key_Data_Pair.Data, Data, 2);

      Copy (Tab.Key,  Key_Data_Pair.Key,  Key_Data_Pair.Key_Size);
      Copy (Tab.Data, Key_Data_Pair.Data, Key_Data_Pair.Data_Size);

      Get_Field    (Key,  1, Tab.File_Name);
      Get_Field    (Data, 1, Tab.Language);
      Get_Field    (Data, 2, Tab.Timestamp);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out FD_Table) is
      Key, Data : CSF;
      Buffer : Unchecked_String_Access;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 4);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);
      Copy (Tab.Data, Key_Data_Pair.Data, Key_Data_Pair.Data_Size);

      Buffer := To_Unchecked_String (Key_Data_Pair.Data);
      Parse_Key    (Key, Tab.Key, Tab.Name, Tab.File_Name, Tab.Start_Position);
      Get_Position    (Data, 1, Buffer, Tab.End_Position);
      Get_Hex         (Data, 2, Buffer, Tab.Attributes);
      Get_No_Brackets (Data, 3, Tab.Return_Type);
      Get_No_Brackets (Data, 4, Tab.Arg_Types);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out FIL_Table) is
      Key, Data : CSF;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 4);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);
      Copy (Tab.Data, Key_Data_Pair.Data, Key_Data_Pair.Data_Size);

      Get_Field    (Key, 1, Tab.File_Name);
      Get_Position
        (Key, 2, To_Unchecked_String (Key_Data_Pair.Key), Tab.Start_Position);
      Get_Field    (Key, 3, Tab.Class);
      Get_Field    (Key, 4, Tab.Identifier);
      Tab.Symbol := Get_Symbol (Key, 5, Tab.Key);

      Get_Position
        (Data, 1, To_Unchecked_String (Key_Data_Pair.Data), Tab.End_Position);
      Get_Position
        (Data, 2, To_Unchecked_String (Key_Data_Pair.Data),
         Tab.Highlight_Start_Position);
      Get_Position
        (Data, 3, To_Unchecked_String (Key_Data_Pair.Data),
         Tab.Highlight_End_Position);
      Get_No_Brackets (Data, 4, Tab.Types_Of_Arguments);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out FR_Table) is
      Key, Data : CSF;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 4);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);
      Copy (Tab.Data, Key_Data_Pair.Data, Key_Data_Pair.Data_Size);

      Parse_Key    (Key, Tab.Key, Tab.Name, Tab.File_Name, Tab.Start_Position);
      Get_Position
        (Data, 1, To_Unchecked_String (Key_Data_Pair.Data), Tab.End_Position);
      Get_Hex
        (Data, 2, To_Unchecked_String (Key_Data_Pair.Data), Tab.Attributes);
      Get_No_Brackets (Data, 3, Tab.Return_Type);
      Get_No_Brackets (Data, 4, Tab.Arg_Types);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out FU_Table) is
      Key, Data : CSF;
      Num_Of_Fields : Integer;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 5);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);
      Copy (Tab.Data, Key_Data_Pair.Data, Key_Data_Pair.Data_Size);

      Num_Of_Fields := Get_Field_Count (Key);

      --  Do we have a ".fu" table ?

      if Num_Of_Fields = 3 then
         Parse_Key (Key, Tab.Key, Tab.Name, Tab.File_Name, Tab.Start_Position);
         Tab.Class := Empty_Segment;

      --  Else we have a ".mi" table
      else
         Parse_Key
           (Key, Tab.Key, Tab.Name, Tab.File_Name, Tab.Start_Position, 2);
         Get_Field (Key, 1, Tab.Class);
      end if;

      Get_Position
        (Data, 1, To_Unchecked_String (Key_Data_Pair.Data), Tab.End_Position);
      Get_Hex         (Data, 2, To_Unchecked_String (Key_Data_Pair.Data),
                       Tab.Attributes);
      Get_No_Brackets (Data, 3, Tab.Return_Type);
      Get_No_Brackets (Data, 4, Tab.Arg_Types);
      Get_No_Brackets (Data, 5, Tab.Arg_Names);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out GV_Table) is
      Key, Data : CSF;
      Comments  : Segment;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 6);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);
      Copy (Tab.Data, Key_Data_Pair.Data, Key_Data_Pair.Data_Size);

      Parse_Key   (Key,  Tab.Key, Tab.Name, Tab.File_Name, Tab.Start_Position);
      Get_No_Brackets (Data, 3, Tab.Value_Type);
      Get_No_Brackets (Data, 4, Tab.Class);
      Get_No_Brackets (Data, 6, Comments);
      Tab.Type_Start_Position := Get_Position_From_Comment
        (To_Unchecked_String (Key_Data_Pair.Data), Comments, "type_beg=");
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out IN_Table) is
      Key, Data : CSF;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 2);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);

      Parse_Key
        (Key, Tab.Key, Tab.Base_Class, Tab.File_Name, Tab.Start_Position, 2);
      Get_Field    (Key,  1, Tab.Class);
      Get_Position
        (Data, 1, To_Unchecked_String (Key_Data_Pair.Data), Tab.End_Position);
      Get_Hex
        (Data, 2, To_Unchecked_String (Key_Data_Pair.Data), Tab.Attributes);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out IU_Table) is
      Key : CSF;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);

      Get_Field    (Key, 1, Tab.Included_File);
      Get_Field    (Key, 3, Tab.Included_From_File);
      Get_Position
        (Key, 3, To_Unchecked_String (Key_Data_Pair.Key),
         Tab.Included_At_Position);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out IV_Table) is
      Key, Data : CSF;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 3);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);
      Copy (Tab.Data, Key_Data_Pair.Data, Key_Data_Pair.Data_Size);

      Parse_Key
        (Key,  Tab.Key, Tab.Name, Tab.File_Name, Tab.Start_Position, 2);
      Get_Field       (Key,  1, Tab.Class);
      Get_No_Brackets (Data, 3, Tab.Value_Type);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out LV_Table) is
      Key, Data : CSF;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 5);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);
      Copy (Tab.Data, Key_Data_Pair.Data, Key_Data_Pair.Data_Size);

      Parse_Key (Key, Tab.Key, Tab.Name, Tab.File_Name, Tab.Start_Position, 2);
      Get_Field       (Key,  1, Tab.Function_Name);
      Get_No_Brackets (Data, 3, Tab.Class);
      Get_No_Brackets (Data, 4, Tab.Value_Type);
      Get_No_Brackets (Data, 5, Tab.Arg_Types);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out MA_Table) is
      Key, Data : CSF;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 1);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);

      Parse_Key    (Key, Tab.Key, Tab.Name, Tab.File_Name, Tab.Start_Position);
      Get_Position
        (Data, 1, To_Unchecked_String (Key_Data_Pair.Data), Tab.End_Position);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out MD_Table) is
      Key, Data : CSF;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 5);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);
      Copy (Tab.Data, Key_Data_Pair.Data, Key_Data_Pair.Data_Size);

      Parse_Key (Key, Tab.Key, Tab.Name, Tab.File_Name, Tab.Start_Position, 2);
      Get_Field       (Key,  1, Tab.Class);
      Get_Position
        (Data, 1, To_Unchecked_String (Key_Data_Pair.Data), Tab.End_Position);
      Get_Hex
        (Data, 2, To_Unchecked_String (Key_Data_Pair.Data), Tab.Attributes);
      Get_No_Brackets (Data, 3, Tab.Return_Type);
      Get_No_Brackets (Data, 4, Tab.Arg_Types);
      Get_No_Brackets (Data, 5, Tab.Arg_Names);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out T_Table) is
      Key, Data : CSF;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 3);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);
      Copy (Tab.Data, Key_Data_Pair.Data, Key_Data_Pair.Data_Size);
      Tab.Data_Size := Key_Data_Pair.Data_Size;

      Parse_Key   (Key,  Tab.Key, Tab.Name, Tab.File_Name, Tab.Start_Position);
      Get_No_Brackets (Data, 3, Tab.Original);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out TA_Table) is
      Key, Data : CSF;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 3);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);
      Copy (Tab.Data, Key_Data_Pair.Data, Key_Data_Pair.Data_Size);

      Parse_Key
        (Key, Tab.Key, Tab.Name, Tab.File_Name, Tab.Start_Position, 2);
      Get_Field       (Key,  1, Tab.Class);
      Get_Position
        (Data, 1, To_Unchecked_String (Key_Data_Pair.Data), Tab.Type_Position);
      Get_No_Brackets (Data, 3, Tab.Value_Type);
   end Parse_Pair;

   ----------------
   -- Parse_Pair --
   ----------------

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out TO_Table) is
      Key, Data : CSF;
   begin
      CSF_Init (Key_Data_Pair.Key, Key);
      CSF_Init (Key_Data_Pair.Data, Data, 2);

      Tab.DBI  := Key_Data_Pair.DBI;
      Copy (Tab.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);
      Copy (Tab.Data, Key_Data_Pair.Data, Key_Data_Pair.Data_Size);

      Get_Field       (Key,  1, Tab.Class);
      Get_Field       (Key,  2, Tab.Symbol_Name);
      Tab.Symbol := Get_Symbol (Key, 3, Tab.Key);
      Get_Field       (Key,  4, Tab.Referred_Class);
      Get_Field       (Key,  5, Tab.Referred_Symbol_Name);
      Tab.Referred_Symbol := Get_Symbol (Key, 6, Tab.Key);
      Get_Field       (Key,  7, Tab.Access_Type);
      Get_Field       (Key,  9, Tab.File_Name);
      Get_Position
        (Key,  8, To_Unchecked_String (Key_Data_Pair.Key), Tab.Position);
      Get_No_Brackets (Data, 1, Tab.Caller_Argument_Types);
      Get_No_Brackets (Data, 2, Tab.Referred_Argument_Types);
   end Parse_Pair;

   ------------------
   -- Get_Position --
   ------------------

   procedure Get_Position
     (Key      : CSF;
      Field    : Integer;
      Buffer   : Unchecked_String_Access;
      Position : out Point)
   is
      Seg : Segment;
   begin
      Get_Field      (Key, Field, Seg);
      Parse_Position (Buffer, Seg, Position);
   end Get_Position;

   --------------------
   -- Parse_Position --
   --------------------

   procedure Parse_Position
     (Buffer : Unchecked_String_Access; Seg : Segment; Position : out Point)
   is
      Num1   : Integer := 0;
      Num2   : Integer := 0;
      C      : Character;
      Dot_Already_Found : Boolean := False;
   begin
      for J in Seg.First .. Seg.Last loop
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
            raise Bad_Input;
         end if;
      end loop;

      Position := (Line => Num1, Column => Visible_Column_Type (Num2));
      --  ??? Is Num2 really a visible_column_type ?
   end Parse_Position;

   -------------
   -- Get_Hex --
   -------------

   procedure Get_Hex
     (Key    : CSF;
      Field  : Integer;
      Buffer : Unchecked_String_Access;
      Attr   : out SN_Attributes)
   is
      Seg    : Segment;
      Val    : Integer := 0;
      C      : Character;
   begin
      Get_Field (Key, Field, Seg);

      if Seg.Last - Seg.First < 1
        or else Buffer (Seg.First) /= '0'
        or else (Buffer (Seg.First + 1) /= 'x'
                 and then Buffer (Seg.First + 1) /= 'X')
      then
         raise Bad_Input;
      end if;

      for J in Seg.First + 2 .. Seg.Last loop
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
     (Key : CSF; Field : Integer; Result : out Segment) is
   begin
      Get_Field (Key, Field, Result);
      Result := (Result.First + 1, Result.Last - 1);
   end Get_No_Brackets;

   -------------------------------
   -- Get_Position_From_Comment --
   -------------------------------

   function Get_Position_From_Comment
     (Buffer  : Unchecked_String_Access;
      Comment : Segment;
      Name    : String) return Point
   is
      J, K : Natural;
      Pos  : Point := Invalid_Point;
   begin
      J := Ada.Strings.Fixed.Index
         (String (Buffer (Comment.First .. Comment.Last)), Name);

      if J /= 0 then
         J := J + Name'Length;
         K := J;

         while K <= Comment.Last loop
            exit when Buffer (K) not in '0' .. '9'
               and then Buffer (K) /= '.';
            K := K + 1;
         end loop;

         Parse_Position (Buffer, (J, K - 1), Pos);
      end if;

      return Pos;
   end Get_Position_From_Comment;

   --------------------
   -- Get_Class_Name --
   --------------------

   function Get_Class_Name
     (Key : Buffer_String; Seg : Segment) return String is
   begin
      if Seg = Empty_Segment then
         return "#";
      else
         return String (Key (Seg.First .. Seg.Last));
      end if;
   end Get_Class_Name;

   ---------------
   -- Parse_Key --
   ---------------

   procedure Parse_Key (Key_Data_Pair : Pair; Key : out Entity_Key) is
      Data : CSF;
   begin
      CSF_Init  (Key_Data_Pair.Key, Data);
      Copy      (Key.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);
      Parse_Key (Data, Key.Key, Key.Name, Key.File_Name, Key.Start_Position);
   end Parse_Key;

   ---------------
   -- Parse_Key --
   ---------------

   procedure Parse_Key (Key_Data_Pair : Pair; Key : out Entity_Class_Key) is
      Data : CSF;
   begin
      CSF_Init  (Key_Data_Pair.Key, Data);
      Copy      (Key.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);
      Parse_Key
        (Data, Key.Key, Key.Name, Key.File_Name, Key.Start_Position, 2);
      Get_Field (Data, 1, Key.Class);
   end Parse_Key;

   ---------------
   -- Parse_Key --
   ---------------

   procedure Parse_Key (Key_Data_Pair : Pair; Key : out Entity_Function_Key) is
      Data : CSF;
   begin
      CSF_Init (Key_Data_Pair.Key, Data);
      Copy     (Key.Key, Key_Data_Pair.Key, Key_Data_Pair.Key_Size);

      Parse_Key
        (Data, Key.Key, Key.Name, Key.File_Name, Key.Start_Position, 2);
      Get_Field (Data,  1, Key.Function_Name);
   end Parse_Key;

end SN.DB_Structures;
