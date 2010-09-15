-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2010, AdaCore                  --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package handles interaction between the project templates
--  engine and GPS.

with Gtk.Widget; use Gtk.Widget;
with GPS.Kernel; use GPS.Kernel;

package Project_Templates.GPS is

   procedure Register_Module (Kernel : access Kernel_Handle_Record'Class);
   --  Register the module

   procedure Launch_Dialog
     (Kernel    : access Kernel_Handle_Record'Class;
      Widget    : Gtk_Widget;
      Cancelled : out Boolean);
   --  Launch the "project from template" dialog.
   --  Cancelled indicates whether the user has cancelled the dialog.
   --  Widget is used for rendering pixbufs.

end Project_Templates.GPS;
