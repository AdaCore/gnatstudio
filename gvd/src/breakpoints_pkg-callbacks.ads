------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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

with Gtk.Widget; use Gtk.Widget;

package Breakpoints_Pkg.Callbacks is
   procedure On_Location_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Subprogam_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Address_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Regexp_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class);

end Breakpoints_Pkg.Callbacks;
