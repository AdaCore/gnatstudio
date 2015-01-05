------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

with Gtk.Accel_Group;         use Gtk.Accel_Group;
with Gtk.Application_Window;  use Gtk.Application_Window;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Icon_Factory;        use Gtk.Icon_Factory;
with Gtk.Menu_Bar;            use Gtk.Menu_Bar;
with Gdk.Event;               use Gdk.Event;
with Gtk.Toolbar;             use Gtk.Toolbar;

with Gtkada.Application;      use Gtkada.Application;
with Gtkada.MDI;              use Gtkada.MDI;

with GPS.Kernel;

package GPS.Main_Window is

   type GPS_Application_Record is new Gtkada_Application_Record with record
      Kernel            : GPS.Kernel.Kernel_Handle;
   end record;
   type GPS_Application is access all GPS_Application_Record'Class;

   type GPS_Window_Record is new Gtk_Application_Window_Record with record
      Application       : access GPS_Application_Record'Class;
      --  The GPS Application (not owned by the window)

      Main_Accel_Group  : Gtk_Accel_Group;
      --  The default accelerators for the window

      Icon_Factory      : Gtk_Icon_Factory;
      --  The icon factory specific to GPS

      Menu_Bar          : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Toolbar_Box       : Gtk_Box;
      Toolbar           : Gtk_Toolbar;
      MDI               : Gtkada.MDI.MDI_Window;

      Is_Destroyed      : Boolean := False;

      Last_Event_For_Contextual : Gdk_Event;
      --  The event triggering the last contextual menu

      Desktop_Loaded    : Boolean := False;
      Public_Version    : Boolean := True;
   end record;
   type GPS_Window is access all GPS_Window_Record'Class;

   procedure Gtk_New
     (Main_Window : out GPS_Window;
      Application : not null access GPS_Application_Record'Class;
      Menubar     : not null access Gtk.Menu_Bar.Gtk_Menu_Bar_Record'Class);
   --  Create a new main window.
   --  Home_Dir is the home directory (e.g ~/.gps) under which configuration
   --  files will be saved.
   --  Prefix_Directory is the prefix where GPS is installed (e.g /opt/gps).
   --  Application is the GPS Application instance

   function Kernel
     (Self : not null access GPS_Window_Record'Class)
      return GPS.Kernel.Kernel_Handle;
   --  The kernel for the application

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

   function Create_MDI_Window_Instance
     (Script : not null access Scripting_Language_Record'Class;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Child  : access MDI_Child_Record'Class) return Class_Instance;
   --  Create a class instance wrapping child

   function Get_Child (Inst : Class_Instance) return MDI_Child;
   --  Return the child stored in an instance of GPS.MDIWindow

end GPS.Main_Window;
