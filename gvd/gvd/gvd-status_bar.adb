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

with Glib;             use Glib;
with Gtk.Box;          use Gtk.Box;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;

package body GVD.Status_Bar is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Status : out GVD_Status_Bar) is
   begin
      Status := new GVD_Status_Bar_Record;
      Initialize (Status);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Status : access GVD_Status_Bar_Record'Class) is
   begin
      Initialize_Hbox (Status, Homogeneous => False, Spacing => 4);
      Set_Size_Request (Status, 0, -1);

      --  Avoid resizing the main window whenever a label is changed.
      Set_Resize_Mode (Status, Resize_Queue);

      Gtk_New (Status.Progress);
      Set_Text (Status.Progress, " ");
      --  ??? This is a tweak : it seems that the gtk progress bar doesn't
      --  have a size that is the same when it has text than when it does not,
      --  but we do want to insert and remove text from this bar, without
      --  the annoying change in size, so we make sure there is always some
      --  text displayed.

      Pack_Start (Status, Status.Progress, False, False, 0);

      --  ??? We set the default width to 0 so that the progress bar appears
      --  only as a vertical separator.
      --  This should be removed when another way to keep the size of the
      --  status bar acceptable is found.
      Set_Size_Request (Status.Progress, 0, -1);

      Show_All (Status);
   end Initialize;

   -----------------------
   -- Get_Progress_Area --
   -----------------------

   function Get_Progress_Area
     (Status : access GVD_Status_Bar_Record) return Gtk.Box.Gtk_Hbox
   is
   begin
      return Gtk_Hbox (Status);
   end Get_Progress_Area;

end GVD.Status_Bar;
