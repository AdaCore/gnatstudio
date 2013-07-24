------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
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

with GNATCOLL.VFS;     use GNATCOLL.VFS;

with Gtk.Accel_Group;  use Gtk.Accel_Group;
with Gtk.Box;          use Gtk.Box;
with Gtk.Icon_Factory; use Gtk.Icon_Factory;
with Gtk.Menu_Bar;     use Gtk.Menu_Bar;
with Gtk.Tool_Item;    use Gtk.Tool_Item;
with Gtk.Window;       use Gtk.Window;
with Gdk.Event;        use Gdk.Event;
with Gtk.Toolbar;      use Gtk.Toolbar;

with Gtkada.MDI;       use Gtkada.MDI;

with GPS.Kernel;

package GPS.Main_Window is

   type GPS_Window_Record is new Gtk_Window_Record with record
      Kernel            : GPS.Kernel.Kernel_Handle;
      Main_Accel_Group  : Gtk_Accel_Group;
      --  The default accelerators for the window

      Icon_Factory      : Gtk_Icon_Factory;
      --  The icon factory specific to GPS

      Menu_Bar          : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Toolbar_Box       : Gtk_Box;
      Toolbar           : Gtk_Toolbar;
      MDI               : Gtkada.MDI.MDI_Window;

      Build_Separator   : Gtk_Tool_Item;
      --  The item just before the build items in the toolbar

      Debug_Separator   : Gtk_Tool_Item;
      --  The item just before the debug items

      Last_Event_For_Contextual : Gdk_Event;
      --  The event triggering the last contextual menu

      State_Level       : Integer := 0;
      Busy_Level        : Integer := 0;
      Desktop_Loaded    : Boolean := False;
      Public_Version    : Boolean := True;
   end record;
   type GPS_Window is access all GPS_Window_Record'Class;

   procedure Gtk_New
     (Main_Window      : out GPS_Window;
      Home_Dir         : Virtual_File;
      Prefix_Directory : Virtual_File);
   --  Create a new main window.
   --  Home_Dir is the home directory (e.g ~/.gps) under which configuration
   --  files will be saved.
   --  Prefix_Directory is the prefix where GPS is installed (e.g /opt/gps).

   procedure Initialize
     (Main_Window      : access GPS_Window_Record'Class;
      Home_Dir         : Virtual_File;
      Prefix_Directory : Virtual_File);
   --  Internal initialization function

   procedure Register_Keys (Main_Window : access GPS_Window_Record'Class);
   --  Register the key bindings associated with the window

   procedure Quit
     (Main_Window : access GPS_Window_Record'Class;
      Force       : Boolean := False;
      Status      : Integer := 0);
   --  Exit GPS. Ask for confirmation if there are unsaved files and Force is
   --  False. If Force is True, nothing is saved, and GPS exists immediately.
   --  Save the desktop if needed.
   --  Status is the exit status (0 is success)

   procedure Reset_Title
     (Window : access GPS_Window_Record;
      Info   : String := "");
   --  Reset the title of the main window.
   --  Info is an extra information to be displayed, in addition of the name
   --  of the root project which is always displayed.

   function Is_Any_Menu_Open
     (Window : access GPS_Window_Record) return Boolean;
   --  Returns True if any of the main menu bar's menus are visible

end GPS.Main_Window;
