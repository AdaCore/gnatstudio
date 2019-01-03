------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with GNAT.OS_Lib;

with Ada.Containers.Indefinite_Vectors;

package String_List_Utils is

   package String_List is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   function Longest_Prefix (L : String_List.Vector) return String;
   --  Return the longest prefix of all the strings in L. The empty string is
   --  returned if there is no common suffix.

   function List_To_Argument_List
     (L : String_List.Vector) return GNAT.OS_Lib.Argument_List;
   --  Convert the list.
   --  The returned memory must be freed by the user.

end String_List_Utils;
