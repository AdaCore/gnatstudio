-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
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

with GNAT.Regpat;       use GNAT.Regpat;
with Pixmaps_IDE;       use Pixmaps_IDE;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Basic_Types;       use Basic_Types;

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
     (Lang : access C_Language; Str : String) return Boolean
   is
      pragma Unreferenced (Lang);
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
      Name : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return "(*" & Name & ")";
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   function Array_Item_Name
     (Lang  : access C_Language;
      Name  : String;
      Index : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Name & '[' & Index & ']';
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   function Record_Field_Name
     (Lang  : access C_Language;
      Name  : String;
      Field : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      if Index (Name, "*") = 0 then
         return Name & '.' & Field;
      else
         --  Name is complex, protect it
         return '(' & Name & ")." & Field;
      end if;
   end Record_Field_Name;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   function Explorer_Regexps
     (Lang : access C_Language) return Explorer_Categories
   is
      pragma Unreferenced (Lang);
   begin
      return C_Explorer_Categories;
   end Explorer_Regexps;

   ---------------------------
   -- Make_Entry_Subprogram --
   ---------------------------

   function Make_Entry_Subprogram
     (Str     : String;
      Matched : Match_Array;
      Category : access Category_Index) return String
   is
      pragma Unreferenced (Category);
   begin
      return Str (Matched (4).First .. Matched (4).Last);
   end Make_Entry_Subprogram;

   --------------
   -- Keywords --
   --------------

   function Keywords
     (Lang : access C_Language) return GNAT.Regpat.Pattern_Matcher
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_List;
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   function Get_Language_Context
     (Lang : access C_Language) return Language_Context
   is
      pragma Unreferenced (Lang);
   begin
      return
        (Comment_Start_Length          => 2,
         Comment_End_Length            => 2,
         New_Line_Comment_Start_Length => 0,
         Comment_Start                 => "/*",
         Comment_End                   => "*/",
         New_Line_Comment_Start        => "",
         String_Delimiter              => '"',
         Quote_Character               => '\',
         Constant_Character            => ''');
   end Get_Language_Context;

   ----------------------
   -- Next_Indentation --
   ----------------------

   procedure Next_Indentation
     (Lang          : access C_Language;
      Buffer        : Interfaces.C.Strings.chars_ptr;
      Buffer_Length : Natural;
      Indent        : out Natural;
      Next_Indent   : out Natural;
      Indent_Params : Indent_Parameters := Default_Indent_Parameters)
   is
      pragma Unreferenced (Lang);

      S           : Unchecked_String_Access := To_Unchecked_String (Buffer);
      First       : Natural := Buffer_Length - 1;
      Index       : Natural;
      Offset      : Integer := 0;
      No_Contents : Boolean := True;

   begin
      --  Go to beginning of line

      while First > 1 and then S (First - 1) /= ASCII.LF loop
         First := First - 1;
      end loop;

      Index := First;

      while Index < Buffer_Length
        and then (S (Index) = ' ' or else S (Index) = ASCII.HT)
      loop
         Index := Index + 1;
      end loop;

      Indent := Index - First;

      while Index < Buffer_Length loop
         case S (Index) is
            when ASCII.NUL .. ' ' =>
               null;

            when '{' | '(' =>
               Offset := Offset + Indent_Params.Indent_Level;

            when '}' | ')' =>
               Offset := Offset - Indent_Params.Indent_Level;

            when '"' =>
               No_Contents := False;

               --  Skip string

               Index := Index + 1;

               while Index < Buffer_Length
                 and then (S (Index) /= '"' or else S (Index - 1) = '\')
                 and then S (Index) /= ASCII.LF
               loop
                  Index := Index + 1;
               end loop;

            when ''' =>
               No_Contents := False;

               --  Skip character

               Index := Index + 1;

               while Index < Buffer_Length
                 and then (S (Index) /= ''' or else S (Index - 1) = '\')
                 and then S (Index) /= ASCII.LF
               loop
                  Index := Index + 1;
               end loop;

            when '/' =>
               No_Contents := False;

               --  Comment ?

               if S (Index + 1) = '/' then
                  --  C++ style comment, skip whole line

                  Index := Index + 2;

                  while Index < Buffer_Length
                    and then S (Index) /= ASCII.LF
                  loop
                     Index := Index + 1;
                  end loop;

               elsif S (Index + 1) = '*' then
                  --  Skip comment

                  Index := Index + 3;

                  while Index < Buffer_Length
                    and then (S (Index - 1) /= '*'
                      or else S (Index) /= '/')
                  loop
                     Index := Index + 1;
                  end loop;
               end if;

            when others =>
               No_Contents := False;
         end case;

         Index := Index + 1;
      end loop;

      if -Offset > Indent then
         Next_Indent := 0;

         if No_Contents then
            Indent := 0;
         end if;

      else
         Next_Indent := Indent + Offset;

         if Offset < 0 and then No_Contents then
            Indent := Next_Indent;
         end if;
      end if;
   end Next_Indentation;

end Language.C;
