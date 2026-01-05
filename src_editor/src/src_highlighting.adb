------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2026, AdaCore                     --
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

with Gtk.Text_Tag; use Gtk.Text_Tag;

package body Src_Highlighting is

   -----------
   -- Unref --
   -----------

   procedure Unref (Tags : in out Highlighting_Tags) is
   begin
      for T in Tags'Range loop
         if Tags (T) /= null then
            Unref (Tags (T));
            Tags (T) := null;
         end if;
      end loop;
   end Unref;

end Src_Highlighting;
