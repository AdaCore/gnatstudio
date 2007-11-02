-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2001-2007, AdaCore                 --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

pragma Warnings (Off);
with GNAT.Expect.TTY.Remote; use GNAT.Expect.TTY.Remote;
pragma Warnings (On);

with Glib.Messages;          use Glib.Messages;
with Glib.Object;            use Glib.Object;
with Glib.Values;            use Glib.Values;

with Gtk;                    use Gtk;
with Gtk.Arguments;          use Gtk.Arguments;
with Gtk.Enums;              use Gtk.Enums;

with Gtkada.MDI;             use Gtkada.MDI;

with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Main_Window;        use GPS.Main_Window;
with VFS;                    use VFS;

package GPS.Callbacks is

   GPS_Main : GPS_Window;
   Started  : Boolean := False;
   Exiting  : Boolean := False;
   --  Whether GPS is exiting

   function On_GPS_Started return Boolean;
   --  Called when GPS is started and visible on the screen

   procedure Ctrl_C_Handler;
   pragma Convention (C, Ctrl_C_Handler);
   --  Handler for Ctrl-C events

   procedure Set_Main_Title
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : MDI_Child);
   --  Set the title of the main window

   procedure Child_Selected
     (Mdi    : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Kernel : Kernel_Handle);
   --  Called when a new child is selected

   procedure Title_Changed
     (MDI    : access GObject_Record'Class;
      Child  : Gtk_Args;
      Kernel : Kernel_Handle);
   --  Called when the title of an MDI child has changed

   procedure Gtk_Log
     (Log_Domain : String;
      Log_Level  : Log_Level_Flags;
      Message    : String);
   --  Log level glib handler for redirecting Gtk+ messages to our log file.

end GPS.Callbacks;
