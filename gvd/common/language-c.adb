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
with Odd.Pixmaps;  use Odd.Pixmaps;

package body Language.Debugger.C is

   Keywords : Pattern_Matcher := Compile
     ("^(do|e(lse|xtern)|for|if|s(t(atic|ruct)|witch)|union|while)\W");
   --  List of words: ("struct" "union" "extern" "for" "if" "do" "else"
   --  "while" "switch" "static")
   --
   --  for c++: ("class" "interface" "namespace" "try" "catch" "friend"
   --  "virtual" "template" "public" "protected" "private" "const" "abstract"
   --  "synchronized" "final"
   --
   --  for java: ("finally" "synchronized" "implements" "extends" "throws"
   --  "threadsafe" "transient" "native" "volatile"

   function Make_Entry_Subprogram
     (Str : String; Matched : Match_Array;
      Category : access Category_Index) return String;
   --  Function used to create an entry in the explorer, for subprograms.
   --  See the description of Explorer_Categories for more information.

   Subprogram_RE : aliased Pattern_Matcher :=
     Compile
       ("^\w+\s+"           --  type specs; there can be no
        & "([\w_*]+\s+)?"   --  more than 3 tokens, right?
        & "([\w_*]+\s+)?"
        & "([*&]+\s*)?"     --  pointer
        & "([\w_*]+)\s*\(", --  Name
        Multiple_Lines);

   C_Explorer_Categories : constant Explorer_Categories (1 .. 1) :=
     (1 => (Name           => new String' ("Functions"),
            Regexp         => Subprogram_RE'Access,
            Position_Index => 4,
            Icon           => subprogram_xpm'Access,
            Make_Entry     => Make_Entry_Subprogram'Access));

   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
     (Lang : access C_Language; Str : String) return Boolean is
   begin
      return Str = "int"
        or else Str = "unsigned int"
        or else Str = "char"
        or else Str = "float"
        or else Str = "long"
        or else Str = "short"
        or else Str = "void";
   end Is_Simple_Type;

   ----------------
   -- Looking_At --
   ----------------

   procedure Looking_At
     (Lang      : access C_Language;
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

      --  Do we have a comment ? (only C comments)

      if Buffer'Length > 2
        and then Buffer (Buffer'First) = '/'
        and then Buffer (Buffer'First + 1) = '*'
      then
         Entity := Comment_Text;
         Next_Char := Buffer'First + 1;

         while Next_Char < Buffer'Last
           and then (Buffer (Next_Char) /= '*'
                     or else Buffer (Next_Char + 1) /= '/')
         loop
            Next_Char := Next_Char + 1;
         end loop;

         Next_Char := Next_Char + 2;
         return;
      end if;

      --  Do we have a string ?

      if Buffer (Buffer'First) = '"' then
         Entity := String_Text;
         Next_Char := Buffer'First + 1;

         while Next_Char <= Buffer'Last
           and then (Buffer (Next_Char) /= '"'
                     or else Buffer (Next_Char - 1) = '\')
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

      if Buffer (Next_Char) = ' ' or else Buffer (Next_Char) = ASCII.HT then
         while Next_Char <= Buffer'Last
           and then (Buffer (Next_Char) = ' '
                     or else Buffer (Next_Char) = ASCII.HT)
         loop
            Next_Char := Next_Char + 1;
         end loop;

      --  It is better to return a pointer to the newline, so that the icons
      --  on the side might be displayed properly.

      else
         while Next_Char <= Buffer'Last
           and then Buffer (Next_Char) /= ' '
           and then Buffer (Next_Char) /= ASCII.LF
           and then Buffer (Next_Char) /= ASCII.HT
           and then Buffer (Next_Char) /= '"'
           and then Buffer (Next_Char) /= '/'   --  for comments
         loop
            Next_Char := Next_Char + 1;
         end loop;

         if Buffer (Next_Char) = ' ' then
            Next_Char := Next_Char + 1;
         end if;
      end if;
      Entity := Normal_Text;
   end Looking_At;

   ----------------------
   -- Dereference_Name --
   ----------------------

   function Dereference_Name
     (Lang : access C_Language;
      Name : String) return String is
   begin
      return "(*" & Name & ")";
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   function Array_Item_Name
     (Lang  : access C_Language;
      Name  : String;
      Index : String) return String is
   begin
      return Name & '[' & Index & ']';
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   function Record_Field_Name
     (Lang  : access C_Language;
      Name  : String;
      Field : String) return String is
   begin
      --  Simplify the expression by replacing (*foo).bar by foo->bar

      if Name'Length > 3
        and then Name (Name'First) = '('
        and then Name (Name'First + 1) = '*'
        and then Name (Name'Last) = ')'
      then
         return Name (Name'First + 2 .. Name'Last - 1) & "->" & Field;
      else
         return Name & '.' & Field;
      end if;
   end Record_Field_Name;

   -----------
   -- Start --
   -----------

   function Start (Debugger  : access C_Language) return String is
   begin
      return "tbreak main" & ASCII.LF & "run";
   end Start;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   function Explorer_Regexps
     (Lang : access C_Language) return Explorer_Categories is
   begin
      return C_Explorer_Categories;
   end Explorer_Regexps;

   ---------------------------
   -- Make_Entry_Subprogram --
   ---------------------------

   function Make_Entry_Subprogram
     (Str     : String;
      Matched : Match_Array;
      Category : access Category_Index) return String is
   begin
      return Str (Matched (4).First .. Matched (4).Last);
   end Make_Entry_Subprogram;

end Language.Debugger.C;
