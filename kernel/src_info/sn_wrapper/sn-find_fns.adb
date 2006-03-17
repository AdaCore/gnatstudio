-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2006                       --
--                            AdaCore                                --
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

package body SN.Find_Fns is

   Null_Position   : constant String := "000000.000";
   Position_Length : constant Integer := Null_Position'Length;

   procedure To_String
     (P : Point; Str : in out String; Where : in out Integer);
   pragma Inline (To_String);
   --  Store, in Str, at position Where, a 000000.000 representation of
   --  Point. Where is left to the first character following this
   --  representation.

   -------------------
   -- Set_Cursor_At --
   -------------------

   procedure Set_Cursor_At
     (DB             : DB_File;
      Name           : String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String)
   is
      Key  : String (1 .. Name'Length + Position_Length + Filename'Length + 2);
      Pos  : Integer := 1;
   begin
      if Name /= Invalid_String then
         Key (1 .. Name'Length) := Name;
         Pos := Pos + Name'Length;
         Key (Pos) := Field_Sep;
         Pos := Pos + 1;

         if Start_Position /= Invalid_Point then
            To_String (Start_Position, Key, Pos);
            Key (Pos) := Field_Sep;
            Pos := Pos + 1;

            if Filename /= Invalid_String then
               Key (Pos .. Key'Last) := Filename;
               Pos := Key'Last + 1;
            end if;
         end if;
      end if;

      Set_Cursor
        (DB, By_Key, Key (Key'First .. Pos - 1), Exact_Match => False);
   end Set_Cursor_At;

   -------------------
   -- Set_Cursor_At --
   -------------------

   procedure Set_Cursor_At
     (DB             : DB_File;
      Filename       : String) is
   begin
      if Filename = "" then
         Set_Cursor (DB, By_Key, Filename, Exact_Match => False);
      else
         Set_Cursor (DB, By_Key, Filename, Exact_Match => True);
      end if;
   end Set_Cursor_At;

   -------------------
   -- Set_Cursor_At --
   -------------------

   procedure Set_Cursor_At
     (DB             : DB_File;
      Class          : String;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String)
   is
      Key  : String (1 .. Class'Length + Name'Length
                     + Position_Length + Filename'Length + 3);
      Pos  : Integer := 1;
   begin
      if Class /= Invalid_String then
         Key (1 .. Class'Length) := Class;
         Pos := Pos + Class'Length;
         Key (Pos) := Field_Sep;
         Pos := Pos + 1;

         if Name /= Invalid_String then
            Key (Pos .. Pos + Name'Length - 1) := Name;
            Pos := Pos + Name'Length;
            Key (Pos) := Field_Sep;
            Pos := Pos + 1;

            if Start_Position /= Invalid_Point then
               To_String (Start_Position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;

               if Filename /= Invalid_String then
                  Key (Pos .. Key'Last) := Filename;
                  Pos := Key'Last + 1;
               end if;
            end if;
         end if;
      end if;

      Set_Cursor
        (DB, By_Key, Key (Key'First .. Pos - 1), Exact_Match => False);
   end Set_Cursor_At;

   --------------
   -- Get_Pair --
   --------------

   procedure Get_Pair
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Result         : out Pair) is
   begin
      Set_Cursor_At (DB, Name, Start_Position, Filename);
      Get_Pair (DB, Next_By_Key, Result);
      Release_Cursor (DB);
   end Get_Pair;

   --------------
   -- Get_Pair --
   --------------

   procedure Get_Pair
     (DB                : DB_File;
      Class_Or_Function : String := Invalid_String;
      Name              : String := Invalid_String;
      Start_Position    : Point  := Invalid_Point;
      Filename          : String := Invalid_String;
      Result            : out Pair) is
   begin
      Set_Cursor_At (DB, Class_Or_Function, Name, Start_Position, Filename);
      Get_Pair (DB, Next_By_Key, Result);
      Release_Cursor (DB);
   end Get_Pair;

   --------------
   -- Find_Key --
   --------------

   procedure Find_Key
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Key            : out Entity_Key;
      Success        : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair (DB, Name, Start_Position, Filename, Result => P);
      Success := P /= No_Pair;
      if Success then
         Parse_Key (P, Key);
      end if;
   end Find_Key;

   --------------
   -- Find_Key --
   --------------

   procedure Find_Key
     (DB             : DB_File;
      Class          : String := Invalid_String;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Key            : out Entity_Class_Key;
      Success        : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair (DB, Class, Name, Start_Position, Filename, Result => P);
      Success := P /= No_Pair;
      if Success then
         Parse_Key (P, Key);
      end if;
   end Find_Key;

   --------------
   -- Find_Key --
   --------------

   procedure Find_Key
     (DB             : DB_File;
      Function_Name  : String := Invalid_String;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Key            : out Entity_Function_Key;
      Success        : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair
        (DB, Function_Name, Name, Start_Position, Filename, Result => P);
      Success := P /= No_Pair;
      if Success then
         Parse_Key (P, Key);
      end if;
   end Find_Key;

   ----------
   -- Find --
   ----------

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out CL_Table;
      Success        : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair (DB, Name, Start_Position, Filename, Result => P);
      Success := P /= No_Pair;
      if Success then
         Parse_Pair (P, Tab);
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out CON_Table;
      Success        : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair (DB, Name, Start_Position, Filename, Result => P);
      Success := P /= No_Pair;
      if Success then
         Parse_Pair (P, Tab);
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out E_Table;
      Success        : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair (DB, Name, Start_Position, Filename, Result => P);
      Success := P /= No_Pair;
      if Success then
         Parse_Pair (P, Tab);
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out EC_Table;
      Success        : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair (DB, Name, Start_Position, Filename, Result => P);
      Success := P /= No_Pair;
      if Success then
         Parse_Pair (P, Tab);
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out FD_Table;
      Success        : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair (DB, Name, Start_Position, Filename, Result => P);
      Success := P /= No_Pair;
      if Success then
         Parse_Pair (P, Tab);
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out FU_Table;
      Success        : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair (DB, Name, Start_Position, Filename, Result => P);
      Success := P /= No_Pair;
      if Success then
         Parse_Pair (P, Tab);
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   procedure Find
     (DB       : DB_File;
      Name     : String := Invalid_String;
      Position : Point  := Invalid_Point;
      Filename : String := Invalid_String;
      Tab      : out GV_Table;
      Success  : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair (DB, Name, Position, Filename, Result => P);
      Success := P /= No_Pair;
      if Success then
         Parse_Pair (P, Tab);
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   procedure Find
     (DB             : DB_File;
      Class          : String := Invalid_String;
      Variable_Name  : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out IV_Table;
      Success        : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair (DB, Class, Variable_Name, Start_Position, Filename, P);
      Success := P /= No_Pair;
      if Success then
         Parse_Pair (P, Tab);
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out MA_Table;
      Success        : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair (DB, Name, Start_Position, Filename, Result => P);
      Success := P /= No_Pair;
      if Success then
         Parse_Pair (P, Tab);
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   procedure Find
     (DB             : DB_File;
      Class          : String := Invalid_String;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out MD_Table;
      Success        : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair (DB, Class, Name, Start_Position, Filename, Result => P);
      Success := P /= No_Pair;
      if Success then
         Parse_Pair (P, Tab);
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   procedure Find
     (DB             : DB_File;
      Class          : String := Invalid_String;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out TA_Table;
      Success        : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair (DB, Class, Name, Start_Position, Filename, Result => P);
      Success := P /= No_Pair;
      if Success then
         Parse_Pair (P, Tab);
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   procedure Find
     (DB       : DB_File;
      Name     : String := Invalid_String;
      Position : Point  := Invalid_Point;
      Filename : String := Invalid_String;
      Tab      : out T_Table;
      Success  : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair (DB, Name, Position, Filename, Result => P);
      Success := P /= No_Pair;
      if Success then
         Parse_Pair (P, Tab);
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   procedure Find
     (DB             : DB_File;
      Class          : String;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out FU_Table;
      Success        : out Boolean)
   is
      P : Pair;
   begin
      Get_Pair (DB, Class, Name, Start_Position, Filename, Result => P);
      Success := P /= No_Pair;
      if Success then
         Parse_Pair (P, Tab);
      end if;
   end Find;

   ---------------
   -- To_String --
   ---------------

   procedure To_String
     (P : Point; Str : in out String; Where : in out Integer)
   is
      Line_Img : constant String := Integer'Image (P.Line);
      Col_Img  : constant String := Visible_Column_Type'Image (P.Column);
   begin
      Str (Where .. Where + Position_Length - 1) := Null_Position;
      Str (Where + 5 - Line_Img'Length + 2 .. Where + 5)
        := Line_Img (2 .. Line_Img'Length);
      Where := Where + 7;
      Str (Where + 2 - Col_Img'Length + 2 .. Where + 2)
        := Col_Img (2 .. Col_Img'Length);
      Where := Where + 3;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Sym_Type : Symbol_Type) return String is
   begin
      case Sym_Type is
         when CL     => return "cl";
         when COM    => return "com";
         when COV    => return "cov";
         when CON    => return "con";
         when E      => return "e";
         when EC     => return "ec";
         when FD     => return "fd";
         when FR     => return "fr";
         when FU     => return "fu";
         when GV     => return "gv";
         when IV     => return "iv";
         when LV     => return "lv";
         when MA     => return "ma";
         when MD     => return "md";
         when MI     => return "mi";
         when SU     => return "su";
         when T      => return "t";
         when UN     => return "un";
         when IU     => return "iu";
         when TA     => return "ta";
         when others => raise Invalid_Symbol_Type;
      end case;
   end To_String;
end SN.Find_Fns;
