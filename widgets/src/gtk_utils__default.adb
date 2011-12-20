------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

--  Default implementation (non Windows systems)

package body Gtk_Utils is

   -----------------------
   -- Gtk_Major_Version --
   -----------------------

   function Gtk_Major_Version return Integer is
      Version : Integer;
      pragma Import (C, Version, "gtk_major_version");
   begin
      return Version;
   end Gtk_Major_Version;

   -----------------------
   -- Gtk_Minor_Version --
   -----------------------

   function Gtk_Minor_Version return Integer is
      Version : Integer;
      pragma Import (C, Version, "gtk_minor_version");
   begin
      return Version;
   end Gtk_Minor_Version;

   -----------------------
   -- Gtk_Micro_Version --
   -----------------------

   function Gtk_Micro_Version return Integer is
      Version : Integer;
      pragma Import (C, Version, "gtk_micro_version");
   begin
      return Version;
   end Gtk_Micro_Version;

   -----------------
   -- Have_Render --
   -----------------

   function Have_Render (Window : Gdk.Gdk_Window) return Boolean is
      function Internal (Window : Gdk.Gdk_Window) return Integer;
      pragma Import (C, Internal, "gps_have_render");
   begin
      return Internal (Window) /= 0;
   end Have_Render;

end Gtk_Utils;
