-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
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

package body Language.Debugger.Java is

   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
     (Lang : access Java_Language; Str : String) return Boolean is
   begin
      return False;
   end Is_Simple_Type;

   ----------------------
   -- Dereference_Name --
   ----------------------

   function Dereference_Name
     (Lang : access Java_Language;
      Name : String) return String is
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
      Index : String) return String is
   begin
      return Name & '[' & Index & ']';
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   function Record_Field_Name
     (Lang  : access Java_Language;
      Name  : String;
      Field : String) return String is
   begin
      return Name & '.' & Field;
   end Record_Field_Name;

   --------------
   -- Keywords --
   --------------

   function Keywords
     (Lang : access Java_Language) return GNAT.Regpat.Pattern_Matcher is
   begin
      return Compile ("^@@@@$");
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   function Get_Language_Context
     (Lang : access Java_Language) return Language_Context is
   begin
      return (Comment_Start_Length => 2,
              Comment_End_Length   => 2,
              Comment_Start        => "/*",
              Comment_End          => "*/",
              String_Delimiter     => '"',
              Quote_Character      => '\',
              Constant_Character   => ''');
   end Get_Language_Context;


end Language.Debugger.Java;
