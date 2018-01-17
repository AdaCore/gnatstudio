------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

pragma Warnings (Off);
with GNAT.Expect.TTY.Remote; use GNAT.Expect.TTY.Remote;
pragma Warnings (On);

with Glib.Messages;          use Glib.Messages;
with Glib.Object;            use Glib.Object;
with Gtk;                    use Gtk;
with Gtk.Enums;              use Gtk.Enums;
with GPS.Kernel;             use GPS.Kernel;

package GPS.Callbacks is

   procedure Ctrl_C_Handler;
   pragma Convention (C, Ctrl_C_Handler);
   --  Handler for Ctrl-C events

   procedure Child_Selected
     (Mdi    : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Called when a new child is selected

   procedure Title_Changed
     (MDI    : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Called when the title of an MDI child has changed

   procedure Gtk_Log
     (Log_Domain : String;
      Log_Level  : Log_Level_Flags;
      Message    : String);
   --  Log level glib handler for redirecting Gtk+ messages to our log file.

end GPS.Callbacks;
