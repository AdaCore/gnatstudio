------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

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

   overriding function Keywords
     (Lang : access Cpp_Language) return GNAT.Expect.Pattern_Matcher_Access;

   overriding function Get_Language_Context
     (Lang : access Cpp_Language) return Language_Context_Access;

   --------------
   -- Explorer --
   --------------

   overriding function Explorer_Regexps
     (Lang : access Cpp_Language) return Explorer_Categories;

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

   overriding function Get_Name (Lang : access Cpp_Language) return String;

   overriding function Entities_Indexed (Self : Cpp_Language) return Boolean;
   --  Unconditionally return True. This enables storing all the C++ entities
   --  in the structure Entities_Search_Tries, and it is required to give
   --  support for entities completion (see Completion-C packages) and
   --  navigation in Ada sources through entities imported from C++.

   Cpp_Lang : constant Language_Access := new Cpp_Language;
end Language.Cpp;
