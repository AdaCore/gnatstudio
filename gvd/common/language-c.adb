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

with GNAT.Regpat;  use GNAT.Regpat;
with GVD.Pixmaps;  use GVD.Pixmaps;

package body Language.C is

   Keywords_List : Pattern_Matcher := Compile
     ("^(" & C_Keywords_Regexp & ")\W");
   --  for java: ("finally" "synchronized" "implements" "extends" "throws"
   --  "threadsafe" "transient" "native" "volatile"

   function Make_Entry_Subprogram
     (Str : String; Matched : Match_Array;
      Category : access Category_Index) return String;
   --  Function used to create an entry in the explorer, for subprograms.
   --  See the description of Explorer_Categories for more information.

   Subprogram_RE : aliased Pattern_Matcher :=
     Compile
       ("^\w+\s+"             --  type specs; there can be no
        & "([\w_*]+\s+)?"     --  more than 3 tokens, right?
        & "([\w_*]+\s+)?"
        & "([*&]+\s*)?"       --  pointer
        & "([\w_*]+)\s*"
        & "(\s[\w_]+\s*\()?" --  handling of macros, as in
                              --  "void pa_exit PARAMS ((int))"
        & "\([^(]",           --  Name
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
      return    Str = "int"
        or else Str = "char"
        or else Str = "float"
        or else Str = "double"
        or else Str = "long"
        or else Str = "short"

         --  "unsigned int", "unsigned char"
        or else (Str'Length >= 9
                 and then Str (Str'First .. Str'First + 8) = "unsigned ")

         --  "long int", "long unsigned int"
        or else (Str'Length >= 5
                 and then Str (Str'First .. Str'First + 4) = "long ")

         --  "short int", "short unsigned int"
        or else (Str'Length >= 6
                 and then Str (Str'First .. Str'First + 5) = "short ")

        or else Str = "void";
   end Is_Simple_Type;

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

   --------------
   -- Keywords --
   --------------

   function Keywords
     (Lang : access C_Language) return GNAT.Regpat.Pattern_Matcher is
   begin
      return Keywords_List;
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   function Get_Language_Context
     (Lang : access C_Language) return Language_Context is
   begin
      return (Comment_Start_Length          => 2,
              Comment_End_Length            => 2,
              New_Line_Comment_Start_Length => 0,
              Comment_Start                 => "/*",
              Comment_End                   => "*/",
              New_Line_Comment_Start        => "",
              String_Delimiter              => '"',
              Quote_Character               => '\',
              Constant_Character            => ''');
   end Get_Language_Context;

end Language.C;
