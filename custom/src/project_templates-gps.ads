------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2015, AdaCore                     --
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

--  This package handles interaction between the project templates
--  engine and GPS.

with Gtk.Widget; use Gtk.Widget;
with GPS.Kernel; use GPS.Kernel;

package Project_Templates.GPS is

   procedure Register_Module (Kernel : access Kernel_Handle_Record'Class);
   --  Register the module

   procedure Launch_Dialog
     (Kernel    : access Kernel_Handle_Record'Class;
      Cancelled : out Boolean);
   --  Launch the "project from template" dialog.
   --  Cancelled indicates whether the user has cancelled the dialog.

end Project_Templates.GPS;
