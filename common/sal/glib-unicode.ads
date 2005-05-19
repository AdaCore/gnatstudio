-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2005 AdaCore                      --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--
--  This package provides functions for handling of unicode characters and
--  utf8 strings. See also Glib.Convert.
--
--  </description>
--  <c_version>2.2.1</c_version>

package Glib.Unicode is
   pragma Preelaborate;

   -----------------------------
   -- Manipulating characters --
   -----------------------------

   function Is_Space (Char : Gunichar) return Boolean;
   --  True if Char is a space character

   function Is_Alnum (Char : Gunichar) return Boolean;
   --  True if Char is an alphabetical or numerical character

   --------------------------
   -- Manipulating strings --
   --------------------------

   function UTF8_Next_Char
     (Str : UTF8_String; Index : Natural) return Natural;
   pragma Inline (UTF8_Next_Char);
   --  Find the start of the next UTF8 character after the Index-th byte.
   --  Index has to be on the start of a character.
   --  Index is set to a value greater than Str'Last if there is no more
   --  character.

   function UTF8_Find_Prev_Char
     (Str : UTF8_String; Index : Natural) return Natural;
   --  Find the start of the previous UTF8 character after the Index-th byte.
   --  Index doesn't need to be on the start of a character.
   --  Index is set to a value smaller than Str'First if there is no
   --  previous character.

   -----------------
   -- Conversions --
   -----------------

   function UTF8_Get_Char (Str : UTF8_String) return Gunichar;
   --  Converts a sequence of bytes encoded as UTF8 to a unicode character.
   --  If Str doesn't point to a valid UTF8 encoded character, the result is
   --  undefined.

end Glib.Unicode;
