-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with GNAT.Regpat; use GNAT.Regpat;
with GVD.Pixmaps; use GVD.Pixmaps;
with Language.C;  use Language.C;

package body Language.Cpp is

   Keywords_List : Pattern_Matcher := Compile
     ("^(" & C_Keywords_Regexp &
      "|abstract|c(atch|lass|onst)|f(inal|riend)|interface|namespace|"
      & "p(r(ivate|otected)|ublic)|synchronized|t(emplate|ry)|virtual"
      & ")\W");
   --  Adds: ("class" "interface" "namespace" "try" "catch" "friend"
   --  "virtual" "template" "public" "protected" "private" "const" "abstract"
   --  "synchronized" "final"

   Classes_RE : aliased Pattern_Matcher :=
     Compile ("^\s*(class|struct)\s+([\w_]+)\s*(:[^{]+)?\{", Multiple_Lines);

   function Make_Entry_Class
     (Str      : String;
      Matched  : Match_Array;
      Category : access Category_Index) return String;
   --  Function used to create an entry in the explorer, for classes.
   --  See the description of Explorer_Categories for more information.

   Cpp_Explorer_Categories : constant Explorer_Categories (1 .. 1) :=
     (1 => (Name           => new String' ("Classes"),
            Regexp         => Classes_RE'Access,
            Position_Index => 2,
            Icon           => package_xpm'Access,
            Make_Entry     => Make_Entry_Class'Access));

   ----------------------
   -- Make_Entry_Class --
   ----------------------

   function Make_Entry_Class
     (Str      : String;
      Matched  : Match_Array;
      Category : access Category_Index) return String is
   begin
      return Str (Matched (1).First .. Matched (1).Last)
        & " " & Str (Matched (2).First .. Matched (2).Last);
   end Make_Entry_Class;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   function Explorer_Regexps
     (Lang : access Cpp_Language) return Explorer_Categories is
   begin
      return Explorer_Regexps (C_Language (Lang.all)'Access)
        & Cpp_Explorer_Categories;
   end Explorer_Regexps;

   --------------
   -- Keywords --
   --------------

   function Keywords
     (Lang : access Cpp_Language) return GNAT.Regpat.Pattern_Matcher is
   begin
      return Keywords_List;
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   function Get_Language_Context
     (Lang : access Cpp_Language) return Language_Context is
   begin
      return (Comment_Start_Length          => 2,
              Comment_End_Length            => 2,
              New_Line_Comment_Start_Length => 2,
              Comment_Start                 => "/*",
              Comment_End                   => "*/",
              New_Line_Comment_Start        => "//",
              String_Delimiter              => '"',
              Quote_Character               => '\',
              Constant_Character            => ''');
   end Get_Language_Context;

end Language.Cpp;
