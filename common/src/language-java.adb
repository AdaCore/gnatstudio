-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                     Copyright (C) 2000-2006                       --
--                             AdaCore                               --
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
with Language.C;  use Language.C;

package body Language.Java is

   Keywords_Regexp : aliased String :=
                       Language.Keywords (C_Lang).all &
                       "finally|synchronized|implements|extends" &
                       "t(h(rows|readsafe)|ransient)|native|volatile";

   Keywords_List : aliased Pattern_Matcher :=
                     Compile ("^(" & Keywords_Regexp & ")\W");

   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
     (Lang : access Java_Language; Str : String) return Boolean
   is
      pragma Unreferenced (Lang, Str);
   begin
      return False;
   end Is_Simple_Type;

   ----------------------
   -- Dereference_Name --
   ----------------------

   function Dereference_Name
     (Lang : access Java_Language;
      Name : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Name;
      --  This notion does not exist in Java, since all types
      --  are references.
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   function Array_Item_Name
     (Lang  : access Java_Language;
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
     (Lang  : access Java_Language;
      Name  : String;
      Field : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Name & '.' & Field;
   end Record_Field_Name;

   --------------
   -- Keywords --
   --------------

   function Keywords
     (Lang : access Java_Language) return Strings.String_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_Regexp'Access;
   end Keywords;

   function Keywords
     (Lang : access Java_Language) return Pattern_Matcher_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_List'Access;
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   Java_Context  : aliased Language_Context :=
     (Comment_Start_Length          => 2,
      Comment_End_Length            => 2,
      Comment_Start                 => "/*",
      Comment_End                   => "*/",
      New_Line_Comment_Start        => new String'("//"),
      New_Line_Comment_Start_Regexp => null,
      String_Delimiter              => '"',
      Quote_Character               => '\',
      Constant_Character            => ''',
      Can_Indent                    => True,
      Syntax_Highlighting           => True,
      Case_Sensitive                => True);

   function Get_Language_Context
     (Lang : access Java_Language) return Language_Context_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Java_Context'Access;
   end Get_Language_Context;

   ------------------------
   -- Get_Project_Fields --
   ------------------------

   function Get_Project_Fields
     (Lang : access Java_Language) return Project_Field_Array
   is
      pragma Unreferenced (Lang);
   begin
      return (1 .. 0 => No_Project_Field);
   end Get_Project_Fields;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Lang : access Java_Language) return String is
      pragma Unreferenced (Lang);
   begin
      return "Java";
   end Get_Name;

end Language.Java;
