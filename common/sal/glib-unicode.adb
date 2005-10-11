-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2005 AdaCore                      --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

package body Glib.Unicode is

   --------------------
   -- UTF8_Next_Char --
   --------------------

   type Byte is range 1 .. 6;
   type Byte_Array is array (Character) of Byte;

   UTF8_Skip_Data : constant Byte_Array :=
     (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 1, 1);

   function UTF8_Next_Char
     (Str : UTF8_String; Index : Natural) return Natural is
   begin
      return Index + Natural (UTF8_Skip_Data (Str (Index)));
   end UTF8_Next_Char;

   -------------------------
   -- UTF8_Find_Prev_Char --
   -------------------------

   function UTF8_Find_Prev_Char
     (Str : UTF8_String; Index : Natural) return Natural is
   begin
      return Index - 1;
   end UTF8_Find_Prev_Char;

   -------------------
   -- UTF8_Get_Char --
   -------------------

   function UTF8_Get_Char (Str : UTF8_String) return Gunichar is
   begin
      return Character'Pos (Str (Str'First));
   end UTF8_Get_Char;

   --------------
   -- Is_Space --
   --------------

   function Is_Space (Char : Gunichar) return Boolean is
   begin
      return Char = Character'Pos (' ')
        or else Char = Character'Pos (ASCII.HT);
   end Is_Space;

   --------------
   -- Is_Alnum --
   --------------

   function Is_Alnum (Char : Gunichar) return Boolean is
   begin
      return Char in Character'Pos ('0') .. Character'Pos ('9')
        or else Char in Character'Pos ('A') .. Character'Pos ('Z')
        or else Char in Character'Pos ('a') .. Character'Pos ('z');
   end Is_Alnum;

   ---------------------
   -- Unichar_To_UTF8 --
   ---------------------

   procedure Unichar_To_UTF8
     (Char : Gunichar;
      Str  : out UTF8_String;
      Last : out Natural)
   is
      First : Gunichar;
      C     : Gunichar := Char;
   begin
      if C < 16#80# then
         First := 0;
         Last  := Str'First;

      elsif C < 16#800# then
         First := 16#C0#;
         Last  := Str'First + 1;

      elsif C < 16#10000# then
         First := 16#E0#;
         Last  := Str'First + 2;

      elsif C < 16#200000# then
         First := 16#F0#;
         Last  := Str'First + 3;

      elsif C < 16#4000000# then
         First := 16#F8#;
         Last  := Str'First + 4;

      else
         First := 16#FC#;
         Last  := Str'First + 5;
      end if;
    
      for J in reverse Str'First + 1 .. Last loop
         Str (J) := Character'Val ((C and 16#3F#) or 16#80#);
         C := C / (2 ** 6);
      end loop;

      Str (Str'First) := Character'Val (C or First);
   end Unichar_To_UTF8;

end Glib.Unicode;
