-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                   Copyright (C) 2000-2008, AdaCore                --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

--  This is the general Java (non debugger specific) support package.
--  See language.ads for a complete spec.

package Language.Java is

   type Java_Language is new Language_Root with private;

   Java_Lang : constant Language_Access;
   --  Class constant for the Java language.

   overriding function Is_Simple_Type
     (Lang : access Java_Language;
      Str : String) return Boolean;

   overriding function Keywords
     (Lang : access Java_Language) return Strings.String_Access;

   overriding function Keywords
     (Lang : access Java_Language) return GNAT.Expect.Pattern_Matcher_Access;

   overriding function Keywords
     (Lang : access Java_Language) return GNAT.Strings.String_List;

   overriding function Get_Language_Context
     (Lang : access Java_Language) return Language_Context_Access;

   overriding function Dereference_Name
     (Lang : access Java_Language;
      Name : String) return String;

   overriding function Array_Item_Name
     (Lang  : access Java_Language;
      Name  : String;
      Index : String) return String;

   overriding function Record_Field_Name
     (Lang  : access Java_Language;
      Name  : String;
      Field : String) return String;

   overriding function Get_Project_Fields
     (Lang : access Java_Language) return Project_Field_Array;

private
   type Java_Language is new Language_Root with null record;

   overriding function Get_Name (Lang : access Java_Language) return String;
   --  See inherited documentation

   Java_Lang : constant Language_Access := new Java_Language;
end Language.Java;
