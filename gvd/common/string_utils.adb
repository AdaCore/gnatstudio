-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Odd.Strings is

   -----------------
   -- Skip_Blanks --
   -----------------

   procedure Skip_Blanks (Type_Str : String;
                          Index    : in out Natural)
   is
   begin
      while Index <= Type_Str'Last
        and then (Type_Str (Index) = ' '
                  or else Type_Str (Index) = ASCII.HT
                  or else Type_Str (Index) = ASCII.LF
                  or else Type_Str (Index) = ASCII.CR)
      loop
         Index := Index + 1;
      end loop;
   end Skip_Blanks;

   ---------------------
   -- Skip_Hexa_Digit --
   ---------------------

   procedure Skip_Hexa_Digit (Type_Str : String;
                              Index    : in out Natural)
   is
   begin
      --  skips initial 0x if present
      if Index + 1 <= Type_Str'Last
        and then Type_Str (Index) = '0'
        and then Type_Str (Index + 1) = 'x'
      then
         Index := Index + 2;
      end if;

      while Index <= Type_Str'Last
        and then Is_Hexadecimal_Digit (Type_Str (Index))
      loop
         Index := Index + 1;
      end loop;
   end Skip_Hexa_Digit;

   ------------------
   -- Skip_To_Char --
   ------------------

   procedure Skip_To_Char (Type_Str : String;
                           Index    : in out Natural;
                           Char     : Character)
   is
   begin
      while Index <= Type_Str'Last
        and then Type_Str (Index) /= Char
      loop
         Index := Index + 1;
      end loop;
   end Skip_To_Char;

   --------------------
   -- Skip_To_String --
   --------------------

   procedure Skip_To_String (Type_Str  : String;
                             Index     : in out Natural;
                             Substring : String)
   is
      L : constant Natural := Substring'Length - 1;
   begin
      while Index + L <= Type_Str'Last
        and then Type_Str (Index .. Index + L) /= Substring
      loop
         Index := Index + 1;
      end loop;
   end Skip_To_String;

   ---------------
   -- Parse_Num --
   ---------------

   procedure Parse_Num (Type_Str : String;
                        Index    : in out Natural;
                        Result   : out Long_Integer)
   is
      I : constant Natural := Index;
   begin

      --  Recognize negative numbers as well

      if Type_Str (Index) = '-' then
         Index := Index + 1;
      end if;

      while Index <= Type_Str'Last
        and then Type_Str (Index) in '0' .. '9'
      loop
         Index := Index + 1;
      end loop;

      --  If at least one valid character was found, we have a number.

      if Index > I then
         Result := Long_Integer'Value (Type_Str (I .. Index - 1));
      else
         Result := 0;
      end if;
   end Parse_Num;

   ----------------
   -- Looking_At --
   ----------------

   function Looking_At (Type_Str  : String;
                        Index     : Natural;
                        Substring : String)
                       return Boolean
   is
   begin
      return Index + Substring'Length - 1 <= Type_Str'Last
        and then Type_Str (Index .. Index + Substring'Length - 1) = Substring;
   end Looking_At;

   ----------------------
   -- Parse_Cst_String --
   ----------------------

   procedure Parse_Cst_String (Type_Str : String;
                               Index    : in out Natural;
                               Str      : out String)
   is
      S_Index : Natural := Str'First;
      Int : Natural;
   begin
      Index := Index + 1;
      while S_Index <= Str'Last loop

         --  Special characters are represented as ["00"] or ["""]

         if Index + 4 <= Type_Str'Last
           and then Type_Str (Index) = '['
           and then Type_Str (Index + 1) = '"'
         then
            if Type_Str (Index + 2) = '"' then
               Index := Index + 5;
               Str (S_Index) := '"';

            else

               if Type_Str (Index + 2) in 'a' .. 'f' then
                  Int := 16 * (Character'Pos (Type_Str (Index + 2))
                               - Character'Pos ('a') + 10);
               else
                  Int := 16 * (Character'Pos (Type_Str (Index + 2))
                               - Character'Pos ('0'));
               end if;

               if Type_Str (Index + 3) in 'a' .. 'f' then
                  Int := Int + Character'Pos (Type_Str (Index + 3))
                    - Character'Pos ('a') + 10;
               else
                  Int := Int + Character'Pos (Type_Str (Index + 3))
                    - Character'Pos ('0');
               end if;

               Str (S_Index) := Character'Val (Int);
               Index := Index + 6;
            end if;

         --  Else, a standard character.

         else
            Str (S_Index) := Type_Str (Index);
            Index := Index + 1;
         end if;
         S_Index := S_Index + 1;
      end loop;
      Index := Index + 1;
   end Parse_Cst_String;

end Odd.Strings;

