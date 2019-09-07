------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2013-2019, AdaCore                   --
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

with String_Utils;             use String_Utils;

package body Glib_String_Utils is

   function Compare (A, B : String) return Glib.Gint is
   begin
      return Glib.Gint (String_Utils.Compare (A, B));
   end Compare;

   -------------
   -- Compare --
   -------------

   function Compare (A, B : Integer) return Glib.Gint is
   begin
      return Glib.Gint (String_Utils.Compare (A, B));
   end Compare;

   ----------------------
   -- Is_Entity_Letter --
   ----------------------

   function Is_Entity_Letter (Char : Glib.Gunichar) return Boolean is
   begin
      return Is_Entity_Letter (Wide_Wide_Character'Val (Char));
   end Is_Entity_Letter;

   --------------------
   -- Is_File_Letter --
   --------------------

   function Is_File_Letter (Char : Glib.Gunichar) return Boolean is
   begin
      return Is_File_Letter (Wide_Wide_Character'Val (Char));
   end Is_File_Letter;

   ------------------------
   -- Is_Operator_Letter --
   ------------------------

   function Is_Operator_Letter (Char : Glib.Gunichar) return Boolean is
   begin
      return Is_Operator_Letter (Wide_Wide_Character'Val (Char));
   end Is_Operator_Letter;

end Glib_String_Utils;
