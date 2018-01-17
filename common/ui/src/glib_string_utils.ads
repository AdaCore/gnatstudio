------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2013-2018, AdaCore                   --
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

--  This package contains a series of subprograms that can be used
--  for string hadnling using glib C-types.

with Glib;

package Glib_String_Utils is

   function Is_Operator_Letter (Char : Glib.Gunichar) return Boolean;
   --  Return True if the given letter is a valid operator

   function Is_Entity_Letter (Char : Glib.Gunichar) return Boolean;
   pragma Inline (Is_Entity_Letter);
   --  Return True if the given letter is a valid letter for an entity name
   --  (ie if the letter is either alphanumeric or an '_').

   function Is_File_Letter (Char : Glib.Gunichar) return Boolean;
   pragma Inline (Is_File_Letter);
   --  Return True if the given letter is a valid letter for a file name.

   function Compare (A, B : String) return Glib.Gint;
   function Compare (A, B : Integer) return Glib.Gint;
   --  Return -1 if A<B, 1 if A>B and 0 otherwise. This routine is useful for
   --  model specific sorting. The second version does the same comparing
   --  integers. Even if not using string, it is better to keep this routine
   --  next to the compare based on strings.

end Glib_String_Utils;
