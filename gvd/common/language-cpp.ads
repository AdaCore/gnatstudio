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

--  This is the general C++ (non debugger specific) support package.
--  See language.ads and language-debugger.ads for a complete spec.

with Language.Debugger.C;

package Language.Debugger.Cpp is

   type Cpp_Language is new Language.Debugger.C.C_Language with private;

   ------------------
   -- Highlighting --
   ------------------

   function Keywords
     (Lang : access Cpp_Language) return GNAT.Regpat.Pattern_Matcher;

   function Get_Language_Context
     (Lang : access Cpp_Language) return Language_Context;

   --------------
   -- Explorer --
   --------------

   function Explorer_Regexps
     (Lang : access Cpp_Language) return Explorer_Categories;

private
   type Cpp_Language is new Language.Debugger.C.C_Language with null record;
end Language.Debugger.Cpp;
