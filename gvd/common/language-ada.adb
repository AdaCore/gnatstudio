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

with GNAT.Regpat;  use GNAT.Regpat;

package body Language.Debugger.Ada is

   Keywords : Pattern_Matcher (1292);
   --  The size is hard-coded to save a little bit on the compilation time
   --  for the regular expression (we need to compile the regexp only once).

   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
     (Lang : access Ada_Language; Str : String) return Boolean is
   begin
      return Str = "boolean"
        or else Str = "integer"
        or else Str = "natural"
        or else Str = "character";
   end Is_Simple_Type;

   ----------------
   -- Looking_At --
   ----------------

   procedure Looking_At (Lang      : access Ada_Language;
                         Buffer    : String;
                         Entity    : out Language_Entity;
                         Next_Char : out Positive)
   is
      Matched : Match_Array (0 .. 1);

   begin
      --  Do we have a keyword ?

      Match (Keywords, Buffer, Matched);
      if Matched (0) /= No_Match then
         Next_Char := Matched (0).Last + 1;
         Entity := Keyword_Text;
         return;
      end if;

      --  Do we have a comment ?

      if Buffer'Length > 2
        and then Buffer (Buffer'First) = '-'
        and then Buffer (Buffer'First + 1) = '-'
      then
         Entity := Comment_Text;
         Next_Char := Buffer'First + 1;
         while Next_Char <= Buffer'Last
           and then Buffer (Next_Char) /= ASCII.LF
         loop
            Next_Char := Next_Char + 1;
         end loop;
         Next_Char := Next_Char + 1;
         return;
      end if;

      --  Do we have a string ?

      if Buffer (Buffer'First) = '"' then
         Entity := String_Text;
         Next_Char := Buffer'First + 1;
         while Next_Char <= Buffer'Last
           and then Buffer (Next_Char) /= '"'
         loop
            Next_Char := Next_Char + 1;
         end loop;
         Next_Char := Next_Char + 1;
         return;
      end if;

      --  A constant character

      if Buffer'Length > 3
        and then Buffer (Buffer'First) = '''
        and then Buffer (Buffer'First + 2) = '''
      then
         Entity := String_Text;
         Next_Char := Buffer'First + 3;
         return;
      end if;

      --  If no, skip to the next meaningfull character

      Next_Char := Buffer'First + 1;
      while Next_Char <= Buffer'Last
        and then Buffer (Next_Char) /= ' '
        and then Buffer (Next_Char) /= ASCII.LF
        and then Buffer (Next_Char) /= '"'
        and then Buffer (Next_Char) /= '-'
      loop
         Next_Char := Next_Char + 1;
      end loop;

      if Buffer (Next_Char) = ' ' then
         Next_Char := Next_Char + 1;
      end if;
      Entity := Normal_Text;
   end Looking_At;

begin
   Compile (Keywords,
            "^(a(b(ort|s(tract)?)|cce(pt|ss)|l(iased|l)|nd|rray|t)|b"
            & "(egin|ody)|c(ase|onstant)|d(e(clare|l(ay|ta))|igits|o)|"
            & "e(ls(e|if)|n(d|try)|x(ception|it))|f(or|unction)|g(eneric|"
            & "oto)|i[fns]|l(imited|oop)|mod|n(ew|ot|ull)|o(thers|ut|[fr]"
            & ")|p(ackage|r(agma|ivate|o(cedure|tected)))|r(a(ise|nge)|e("
            & "cord|m|names|queue|turn|verse))|s(e(lect|parate)|ubtype)|t"
            & "(a(gged|sk)|erminate|hen|ype)|u(ntil|se)|w(h(en|ile)|ith)|"
            & "xor)\W",
            Case_Insensitive);
end Language.Debugger.Ada;
