------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

--  This package describes the Welcome page which is the starting point
--  when beginning with GPS.

with GPS.Kernel; use GPS.Kernel;
with Gtk.Box;    use Gtk.Box;
with Gtkada.MDI; use Gtkada.MDI;

package Welcome_Page is

   type Welcome_Page_Record is new Gtk_Vbox_Record with null record;
   type Welcome_Page_Access is access all Welcome_Page_Record'Class;

   procedure Display_Welcome_Page (Kernel : Kernel_Handle);
   --  Display the welcome page in the central GPS dialog

   function Create_Welcome_Page (Kernel : Kernel_Handle) return MDI_Child;
   --  Internal function to create the welcome page

end Welcome_Page;
