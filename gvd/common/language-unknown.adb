-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                 Copyright (C) 2000-2003 ACT-Europe                --
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

package body Language.Unknown is

   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
     (Lang : access Unknown_Language; Str : String) return Boolean
   is
      pragma Unreferenced (Lang, Str);
   begin
      return True;
   end Is_Simple_Type;

   ----------------------
   -- Dereference_Name --
   ----------------------

   function Dereference_Name
     (Lang : access Unknown_Language;
      Name : String) return String
   is
      pragma Unreferenced (Lang);
      pragma Unreferenced (Name);
   begin
      return "";
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   function Array_Item_Name
     (Lang  : access Unknown_Language;
      Name  : String;
      Index : String) return String
   is
      pragma Unreferenced (Lang);
      pragma Unreferenced (Name);
      pragma Unreferenced (Index);
   begin
      return "";
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   function Record_Field_Name
     (Lang  : access Unknown_Language;
      Name  : String;
      Field : String) return String
   is
      pragma Unreferenced (Lang);
      pragma Unreferenced (Name);
      pragma Unreferenced (Field);
   begin
      return "";
   end Record_Field_Name;

   --------------
   -- Keywords --
   --------------

   function Keywords
     (Lang : access Unknown_Language) return GNAT.Regpat.Pattern_Matcher
   is
      pragma Unreferenced (Lang);
   begin
      return GNAT.Regpat.Never_Match;
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   function Get_Language_Context
     (Lang : access Unknown_Language) return Language_Context
   is
      pragma Unreferenced (Lang);
   begin
      return
        (Comment_Start_Length          => 0,
         Comment_End_Length            => 0,
         New_Line_Comment_Start_Length => 0,
         Comment_Start                 => "",
         Comment_End                   => "",
         New_Line_Comment_Start        => "",
         String_Delimiter              => ASCII.NUL,
         Quote_Character               => ASCII.NUL,
         Constant_Character            => ASCII.NUL,
         Can_Indent                    => False,
         Syntax_Highlighting           => False,
         Case_Sensitive                => True);
   end Get_Language_Context;

end Language.Unknown;
