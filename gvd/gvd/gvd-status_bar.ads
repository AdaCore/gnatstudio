-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
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

--  This package implements a status bar to display error and help
--  messages.
--  It encapsulates the behavior of the Gtk_Status_Bar, but is easier to use.
--  It provides a separate window to display the historic of message.
--
--  The messages are hidden after a delay.

with Gtk.Arrow;
with Gtk.Box;
with Gtk.Button;
with Gtk.Main;
with Gtk.Status_Bar;
with Gtk.Window;

package GVD.Status_Bar is

   type Odd_Status_Bar_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Odd_Status_Bar is access all Odd_Status_Bar_Record'Class;

   type Category is (Help, Error);

   procedure Gtk_New (Status : out Odd_Status_Bar);
   --  Create a new status bar

   procedure Initialize (Status : access Odd_Status_Bar_Record'Class);
   --  Internal initialization function

   procedure Print_Message
     (Status  : access Odd_Status_Bar_Record;
      Context : Category;
      Msg     : String);
   --  Print a new message in the status bar.
   --  The message will be automatically hidden after a while.

   procedure Hide_Historic (Status : access Odd_Status_Bar_Record);
   --  Hide the history window

private
   type Id_Array is array (Category'Range) of Gtk.Status_Bar.Context_Id;

   type Odd_Status_Bar_Record is new Gtk.Box.Gtk_Box_Record with record
      Arrow_Button : Gtk.Button.Gtk_Button;
      Historic_Win : Gtk.Window.Gtk_Window;
      Status       : Gtk.Status_Bar.Gtk_Status_Bar;

      Arrow        : Gtk.Arrow.Gtk_Arrow;

      Timeout_Id   : Gtk.Main.Timeout_Handler_Id;
      Ids          : Id_Array;

      Is_Blank     : Boolean := False;
      --  True if the message currently displayed is a blank message.
   end record;
end GVD.Status_Bar;
