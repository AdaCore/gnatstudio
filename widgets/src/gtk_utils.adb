------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
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

with Glib; use Glib;

package body Gtk_Utils is

   -----------------------
   -- Gtk_Major_Version --
   -----------------------

   function Gtk_Major_Version return Integer is
      function Version return Guint;
      pragma Import (C, Version, "gtk_get_major_version");
   begin
      return Integer (Version);
   end Gtk_Major_Version;

   -----------------------
   -- Gtk_Minor_Version --
   -----------------------

   function Gtk_Minor_Version return Integer is
      function Version return Guint;
      pragma Import (C, Version, "gtk_get_minor_version");
   begin
      return Integer (Version);
   end Gtk_Minor_Version;

   -----------------------
   -- Gtk_Micro_Version --
   -----------------------

   function Gtk_Micro_Version return Integer is
      function Version return Guint;
      pragma Import (C, Version, "gtk_get_micro_version");
   begin
      return Integer (Version);
   end Gtk_Micro_Version;

end Gtk_Utils;
