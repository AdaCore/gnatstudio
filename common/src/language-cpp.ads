-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                  Copyright (C) 2000-2008, AdaCore                 --
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

--  This is the general C++ (non debugger specific) support package.
--  See language.ads for a complete spec.

with Language.C;

package Language.Cpp is

   type Cpp_Language is new Language.C.C_Language with private;

   Cpp_Lang : constant Language_Access;
   --  Class constant for the C++ language.

   ------------------
   -- Highlighting --
   ------------------

   function Keywords
     (Lang : access Cpp_Language) return Pattern_Matcher_Access;

   function Get_Language_Context
     (Lang : access Cpp_Language) return Language_Context_Access;

   --------------
   -- Explorer --
   --------------

   function Explorer_Regexps
     (Lang : access Cpp_Language) return Explorer_Categories;

   ---------------------
   -- Project support --
   ---------------------

   function Get_Project_Fields
     (Lang : access Cpp_Language) return Project_Field_Array;

   ----------------------
   -- Source Analyzing --
   ----------------------

   overriding
   procedure Parse_Entities
     (Lang     : access Cpp_Language;
      Buffer   : String;
      Callback : Entity_Callback);

private
   type Cpp_Language is new Language.C.C_Language with null record;

   function Get_Name (Lang : access Cpp_Language) return String;

   Cpp_Lang : constant Language_Access := new Cpp_Language;
end Language.Cpp;
