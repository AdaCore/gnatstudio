------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2015-2019, AdaCore                   --
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

--  Utilities for integrating with libclang

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Utils;    use GNATCOLL.Utils;

package Language.Libclang.Utils is

   function Get_Compiler_Search_Paths_Switches
     (Kernel   : Core_Kernel;
      Project  : Project_Type;
      Language : String) return Unbounded_String_Array;
   --  Return the search path for the given language as a set of "-I" switches

   function Get_Project_Source_Dirs
     (Kernel   : Core_Kernel;
      Project  : Project_Type;
      Language : String) return Unbounded_String_Array;
   --  Go through all the source directories in the project hierarchy, and
   --  return a list of all those who might contain sources of the given
   --  language. The result array is returned as a set of "-I" switches.

end Language.Libclang.Utils;
