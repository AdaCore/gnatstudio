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
with Odd.Strings;  use Odd.Strings;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Language.Debugger.Ada is

   Keywords : Pattern_Matcher (1292);
   --  The size is hard-coded to save a little bit on the compilation time
   --  for the regular expression (we need to compile the regexp only once).

   --  Make_Entry functions for the explorer.

   function Make_Entry_Subprogram
     (Str      : String;
      Matched  : Match_Array;
      Category : access Category_Index) return String;
   --  Function used to create an entry in the explorer, for subprograms.
   --  See the description of Explorer_Categories for more information.

   function Make_Entry_Package
     (Str      : String;
      Matched  : Match_Array;
      Category : access Category_Index) return String;
   --  Function used to create an entry in the explorer, for packages.
   --  See the description of Explorer_Categories for more information.

   function Make_Entry_Type
     (Str      : String;
      Matched  : Match_Array;
      Category : access Category_Index) return String;
   --  Function used to create an entry in the explorer, for types.
   --  See the description of Explorer_Categories for more information.

   function Make_Entry_Task
     (Str      : String;
      Matched  : Match_Array;
      Category : access Category_Index) return String;
   --  Function used to create an entry in the explorer, for tasks.
   --  See the description of Explorer_Categories for more information.

   Subprogram_RE : aliased Pattern_Matcher :=
     Compile
       ("^[ \t]*(procedure|function)\s+" &
        "(\w+)(\s*|\s*\([^\)]+\))\s*" &
        "(return\s+(\w|\.)+\s*)?(is\s|;)", Multiple_Lines);

   Package_RE    : aliased Pattern_Matcher :=
     Compile
       ("^[ \t]*package[ \t]+((body[ \t]+)?((\w|\.)+))", Multiple_Lines);

   Type_Def_RE   : aliased Pattern_Matcher :=
     Compile ("^[ \t]*(sub)?type[ \t]+(\w+)", Multiple_Lines);

   Task_RE       : aliased Pattern_Matcher :=
     Compile ("^[ \t]*task[ \t]+((body|type)[ \t]+)?(\w+)", Multiple_Lines);

   --  The Specs are not parsed specifically. Instead, all the work is done
   --  while parsing for subprograms, and the function Make_Entry_Subprogram
   --  distinguishes between the two cases.

   Ada_Explorer_Categories : constant Explorer_Categories :=
     ((Name           => new String'("Subprograms"),
       Regexp         => Subprogram_RE'Access,
       Position_Index => 2,
       Icon           => subprogram_xpm'Access,
       Make_Entry     => Make_Entry_Subprogram'Access),

      (Name           => new String'("Specs"),
       Regexp         => Subprogram_RE'Access,
       Position_Index => 2,
       Icon           => subprogram_xpm'Access,
       Make_Entry     => null),

      (Name           => new String'("Packages"),
       Regexp         => Package_RE'Access,
       Position_Index => 3,
       Icon           => package_xpm'Access,
       Make_Entry     => Make_Entry_Package'Access),

      (Name           => new String'("Types"),
       Regexp         => Type_Def_RE'Access,
       Position_Index => 2,
       Icon           => var_xpm'Access,
       Make_Entry     => Make_Entry_Type'Access),

      (Name           => new String'("Tasks"),
       Regexp         => Task_RE'Access,
       Position_Index => 3,
       Icon           => package_xpm'Access,
       Make_Entry     => Make_Entry_Task'Access));


   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
     (Lang : access Ada_Language; Str : String) return Boolean is
   begin
      return Str = "boolean"
        or else Str = "integer"
        or else Str = "natural"
        or else Str = "system.address"
        or else Str = "character";
   end Is_Simple_Type;

   ----------------
   -- Looking_At --
   ----------------

   procedure Looking_At
     (Lang      : access Ada_Language;
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

         --  First skip the current word
         if Is_Letter (Buffer (Next_Char)) then
            while Next_Char <= Buffer'Last
              and then (Is_Letter (Buffer (Next_Char))
                        or else Buffer (Next_Char) = '_')
            loop
               Next_Char := Next_Char + 1;
            end loop;
         end if;

         --  Then the strange characters
         while Next_Char <= Buffer'Last
           and then Buffer (Next_Char) /= ' '
           and then Buffer (Next_Char) /= ASCII.LF
           and then Buffer (Next_Char) /= ASCII.HT
           and then Buffer (Next_Char) /= '"'
           and then Buffer (Next_Char) /= '-'   --  for comments
           and then Buffer (Next_Char) /= '''   --  constant character
           and then not Is_Letter (Buffer (Next_Char))
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

   function Dereference_Name (Lang : access Ada_Language;
                              Name : String)
                             return String
   is
   begin
      return Name & ".all";
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   function Array_Item_Name (Lang  : access Ada_Language;
                             Name  : String;
                             Index : String)
                            return String
   is
   begin
      --  Simplify the expression by getting rid of unnecessary ".all"
      if Name'Length > 4
        and then Name (Name'Last - 3 .. Name'Last) = ".all"
      then
         return Name (Name'First .. Name'Last - 4) & '(' & Index & ')';
      else
         return Name & '(' & Index & ')';
      end if;
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   function Record_Field_Name (Lang  : access Ada_Language;
                               Name  : String;
                               Field : String)
                              return String
   is
   begin
      --  Simplify the expression by getting rid of unnecessary ".all"
      if Name'Length > 4
        and then Name (Name'Last - 3 .. Name'Last) = ".all"
      then
         return Name (Name'First .. Name'Last - 4) & '.' & Field;
      else
         return Name & '.' & Field;
      end if;
   end Record_Field_Name;

   -----------
   -- Start --
   -----------

   function Start
     (Debugger  : access Ada_Language)
     return String
   is
   begin
      return "begin";
   end Start;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   function Explorer_Regexps (Lang : access Ada_Language)
                             return Explorer_Categories
   is
   begin
      return Ada_Explorer_Categories;
   end Explorer_Regexps;

   ---------------------------
   -- Make_Entry_Subprogram --
   ---------------------------

   function Make_Entry_Subprogram
     (Str : String; Matched : Match_Array; Category : access Category_Index)
     return String
   is
   begin
      if Str (Matched (6).First) = ';' then
         Category.all := 2;  --  specs
      end if;

      if Matched (4) = No_Match then
         return Str (Matched (2).First .. Matched (2).Last);
      else
         return (Str (Matched (2).First .. Matched (2).Last) & ' ' &
                 Reduce (Str (Matched (3).First .. Matched (3).Last)));
      end if;
   end Make_Entry_Subprogram;

   ------------------------
   -- Make_Entry_Package --
   ------------------------

   function Make_Entry_Package
     (Str : String; Matched : Match_Array; Category : access Category_Index)
     return String
   is
   begin
      return Str (Matched (3).First .. Matched (3).Last);
   end Make_Entry_Package;

   ---------------------
   -- Make_Entry_Type --
   ---------------------

   function Make_Entry_Type
     (Str : String; Matched : Match_Array; Category : access Category_Index)
     return String
   is
   begin
      return Str (Matched (2).First .. Matched (2).Last);
   end Make_Entry_Type;

   ---------------------
   -- Make_Entry_Task --
   ---------------------

   function Make_Entry_Task
     (Str : String; Matched : Match_Array; Category : access Category_Index)
     return String
   is
   begin
      return Str (Matched (3).First .. Matched (3).Last) & " (" &
        Reduce (Str (Matched (2).First .. Matched (2).Last)) & ")";
   end Make_Entry_Task;

   --------------------
   -- Is_System_File --
   --------------------

   function Is_System_File
     (Lang : access Ada_Language;
      File_Name : String)
     return Boolean
   is
      Name : constant String := Base_File_Name (File_Name);
   begin
      return (Name'Length > 2
              and then Name (Name'First + 1) = '-'
              and then (Name (Name'First) = 'a'
                        or else Name (Name'First) = 'g'
                        or else Name (Name'First) = 's'
                        or else Name (Name'First) = 'i'))
        or else Name = "gnat.ads"
        or else Name = "ada.ads"
        or else Name = "interfac.ads"
        or else Name = "system.ads";
   end Is_System_File;

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
