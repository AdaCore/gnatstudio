-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2004                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Gtk.Box;
with Gtk.Progress_Bar;

package GVD.Status_Bar is

   type GVD_Status_Bar_Record is new Gtk.Box.Gtk_Box_Record with private;
   type GVD_Status_Bar is access all GVD_Status_Bar_Record'Class;

   procedure Gtk_New (Status : out GVD_Status_Bar);
   --  Create a new status bar

   procedure Initialize (Status : access GVD_Status_Bar_Record'Class);
   --  Internal initialization function

   function Get_Progress_Area
     (Status : access GVD_Status_Bar_Record) return Gtk.Box.Gtk_Hbox;
   --  Return the area to which you can add progress bars, using Pack_End.
   --  Calling this function hides the integrated progress bar.

private
   type GVD_Status_Bar_Record is new Gtk.Box.Gtk_Box_Record with record
      Progress     : Gtk.Progress_Bar.Gtk_Progress_Bar;
   end record;
end GVD.Status_Bar;
