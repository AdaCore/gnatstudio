-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2008, AdaCore              --
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

with Gtk.Accel_Group;  use Gtk.Accel_Group;
with Gtk.Box;          use Gtk.Box;
with Gtk.Menu_Bar;     use Gtk.Menu_Bar;
with Gtk.Window;       use Gtk.Window;
with Gdk.Pixbuf;       use Gdk.Pixbuf;
with Gtk.Frame;        use Gtk.Frame;
with Gtk.Image;        use Gtk.Image;
with Gtk.Toolbar;      use Gtk.Toolbar;
with Gtk.Main;

with Gtkada.MDI;       use Gtkada.MDI;

with GPS.Kernel;

package GPS.Main_Window is

   type GPS_Window_Record is new Gtk_Window_Record with record
      Kernel            : GPS.Kernel.Kernel_Handle;
      Main_Accel_Group  : Gtk_Accel_Group;
      --  The default accelerators for the window

      Menu_Box          : Gtk.Box.Gtk_Hbox;
      Menu_Bar          : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Toolbar_Box       : Gtk_Vbox;
      Toolbar           : Gtk_Toolbar;
      MDI               : Gtkada.MDI.MDI_Window;
      Statusbar         : Gtk.Box.Gtk_Hbox;

      Animation_Frame   : Gtk_Frame;
      Static_Image      : Gdk_Pixbuf;
      Animation         : Gdk_Pixbuf_Animation;
      Animation_Iter    : Gdk_Pixbuf_Animation_Iter;
      Animation_Image   : Gtk_Image;
      Animation_Timeout : Gtk.Main.Timeout_Handler_Id := 0;

      State_Level       : Integer := 0;
      Busy_Level        : Integer := 0;
      Desktop_Loaded    : Boolean := False;
      Public_Version    : Boolean := True;
   end record;
   type GPS_Window is access all GPS_Window_Record'Class;

   procedure Gtk_New
     (Main_Window      : out GPS_Window;
      Home_Dir         : String;
      Prefix_Directory : String);
   --  Create a new main window.
   --  Home_Dir is the home directory (e.g ~/.gps) under which configuration
   --  files will be saved.
   --  Prefix_Directory is the prefix where GPS is installed (e.g /opt/gps).

   procedure Initialize
     (Main_Window      : access GPS_Window_Record'Class;
      Home_Dir         : String;
      Prefix_Directory : String);
   --  Internal initialization function

   procedure Register_Keys (Main_Window : access GPS_Window_Record'Class);
   --  Register the key bindings associated with the window

   function Anim_Cb (Kernel : GPS.Kernel.Kernel_Handle) return Boolean;
   --  Function called when the GPS animation needs to be updated

   procedure Display_Default_Image (Kernel : GPS.Kernel.Kernel_Handle);
   --  Display the default image in the top right corner of the main window

   function GPS_Name (Window : access GPS_Window_Record) return String;
   --  Return the name of this GPS release

   procedure Quit
     (Main_Window : access GPS_Window_Record'Class;
      Force       : Boolean := False;
      Status      : Integer := 0);
   --  Exit GPS. Ask for confirmation if there are unsaved files and Force is
   --  False. If Force is True, nothing is saved, and GPS exists immediately.
   --  Save the desktop if needed.
   --  Status is the exit status (0 is success)

   procedure Load_Desktop (Window : access GPS_Window_Record'Class);
   --  Load a saved desktop, if any, and create the console if needed

   procedure Reset_Title
     (Window : access GPS_Window_Record;
      Info   : String := "");
   --  Reset the title of the main window.
   --  Info is an extra information to be displayed, in addition of the name
   --  of the root project which is always displayed.

end GPS.Main_Window;
