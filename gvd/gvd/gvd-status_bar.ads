-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
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

--  This package implements a status bar to display error, help messages and
--  progress status.
--
--  It encapsulates the behavior of both a Gtk_Status_Bar and a
--  Gtk_Progress_Bar, but is easier to use.
--  It provides a separate window to display the list of messages displayed.
--
--  The messages are hidden after a delay.

with Glib;
with Gtk.Arrow;
with Gtk.Box;
with Gtk.Button;
with Gtk.Main;
with Gtk.Progress_Bar;
with Gtk.Status_Bar;
with Gtk.Window;

package GVD.Status_Bar is

   type GVD_Status_Bar_Record is new Gtk.Box.Gtk_Box_Record with private;
   type GVD_Status_Bar is access all GVD_Status_Bar_Record'Class;

   type Category is (Help, Error);

   procedure Gtk_New (Status : out GVD_Status_Bar);
   --  Create a new status bar

   procedure Initialize (Status : access GVD_Status_Bar_Record'Class);
   --  Internal initialization function

   procedure Print_Message
     (Status  : access GVD_Status_Bar_Record;
      Context : Category;
      Msg     : String);
   --  Print a new message in the status bar.
   --  The message will be automatically hidden after a while.

   procedure Hide_History (Status : access GVD_Status_Bar_Record);
   --  Hide the history window

   procedure Set_Fraction
     (Status   : access GVD_Status_Bar_Record;
      Fraction : Glib.Gdouble);
   --  Cause the progress bar to "fill in" the given fraction of the bar.
   --  The fraction should be between 0.0 and 1.0, inclusive.

   procedure Set_Progress_Text
     (Status : access GVD_Status_Bar_Record;
      Text   : String);
   --  Set the text to superimpose in the progress bar.

private
   type Id_Array is array (Category'Range) of Gtk.Status_Bar.Context_Id;

   type GVD_Status_Bar_Record is new Gtk.Box.Gtk_Box_Record with record
      Arrow_Button : Gtk.Button.Gtk_Button;
      History_Win  : Gtk.Window.Gtk_Window;
      Status       : Gtk.Status_Bar.Gtk_Status_Bar;
      Arrow        : Gtk.Arrow.Gtk_Arrow;
      Progress     : Gtk.Progress_Bar.Gtk_Progress_Bar;

      Timeout_Id   : Gtk.Main.Timeout_Handler_Id;
      Ids          : Id_Array;

      Is_Blank     : Boolean := False;
      --  True if the message currently displayed is a blank message.
   end record;
end GVD.Status_Bar;
