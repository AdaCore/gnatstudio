------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Glib;                    use Glib;
with Gtk.Accel_Group;         use Gtk.Accel_Group;
with Gtk.Application;         use Gtk.Application;
with Gtk.Application_Window;  use Gtk.Application_Window;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Menu_Bar;            use Gtk.Menu_Bar;
with Gdk.Event;               use Gdk.Event;
with Gtk.Toolbar;             use Gtk.Toolbar;
with Gtk.Window;              use Gtk.Window;

with Gtkada.Application;       use Gtkada.Application;
with Gtkada.Combo_Tool_Button; use Gtkada.Combo_Tool_Button;
with Gtkada.MDI;               use Gtkada.MDI;

with GPS.Kernel;

package GPS.Main_Window is

   -----------------
   -- Application --
   -----------------
   --  An application is a concept similar to the GPS kernel, at the gtk+
   --  level.

   type GPS_Application_Record is new Gtkada_Application_Record with record
      Kernel         : GPS.Kernel.Kernel_Handle;
   end record;
   type GPS_Application is access all GPS_Application_Record'Class;

   function Is_Any_Menu_Open
     (App : not null access GPS_Application_Record'Class) return Boolean;
   --  Returns True if any menu has been clicked on by the user (from the
   --  menu bars of any window)

   overriding procedure Quit (Self : not null access GPS_Application_Record);
   --  Called when quitting GPS.

   -------------
   -- Windows --
   -------------
   --  Utilities that apply to any window

   procedure Set_Default_Size_From_History
     (Win           : not null access Gtk_Window_Record'Class;
      Name          : String;
      Kernel        : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Width, Height : Glib.Gint);
   --  Set the default size for a window.
   --  This should be called before the window is displayed, and replaces
   --  the standard Gtk.Window.Set_Default_Size procedure. As opposed to the
   --  latter, this one will look in past GPS sessions (or earlier in this
   --  session) for the size that a user has set, and reuse that one.
   --  This procedure ensures that the size fits on the screen.

   ----------------------------
   -- GPS Application Window --
   ----------------------------

   type GPS_Application_Window_Record is new Gtk_Application_Window_Record
   with record
      Application       : access GPS_Application_Record'Class;
      --  The GPS Application (not owned by the window)

      Menu_Bar          : Gtk.Menu_Bar.Gtk_Menu_Bar;

      Toolbar_Box       : Gtk_Box;
      Toolbar           : Gtk_Toolbar;
      --  The toolbar, and the box that contains it. The box is used to display
      --  additional items, like the omni-search or progress bars.

      Main_Box          : Gtk.Box.Gtk_Box;
      --  The box that contains the menubar, toolbar, and the rest of the
      --  window's content.
   end record;
   type GPS_Application_Window
     is access all GPS_Application_Window_Record'Class;
   --  An application window with special handling for menubars:
   --  when not using the syste menus, an explicit menubar widget can be
   --  created and associated with the window.

   procedure Initialize
     (Self        : not null access GPS_Application_Window_Record'Class;
      Application : not null access GPS_Application_Record'Class);
   --  Initialize Self.
   --  A menubar is automatically added to the window is necessary (i.e. when
   --  this would not be done automatically by gtk+ when using system menus).

   procedure Setup_Menu_Bar
     (Self : not null access GPS_Application_Window_Record'Class);
   --  Setup menubar and toolbar for the window

   function Kernel
     (Self : not null access GPS_Application_Window_Record'Class)
      return GPS.Kernel.Kernel_Handle
      is (if Self.Application = null then null else Self.Application.Kernel)
      with Inline;
   --  The kernel for the application

   procedure For_All_Open_Windows
     (App      : not null access Gtk_Application_Record'Class;
      Callback : not null access procedure
        (Win : not null access GPS_Application_Window_Record'Class));
   --  Calls Callback for all open windows

   ---------------------
   -- GPS main window --
   ---------------------

   type GPS_Window_Record is new GPS_Application_Window_Record with record
      Main_Accel_Group  : Gtk_Accel_Group;
      --  The default accelerators for the window

      MDI               : Gtkada.MDI.MDI_Window;

      Is_Destroyed      : Boolean := False;

      Perspective_Selector : Gtkada_Combo_Tool_Button;
      --  The combo box to select the current perspective

      Last_Event_For_Contextual : Gdk_Event;
      --  The event triggering the last contextual menu

      Desktop_Loaded    : Boolean := False;
      Public_Version    : Boolean := True;
   end record;
   type GPS_Window is access all GPS_Window_Record'Class;

   procedure Gtk_New
     (Main_Window : out GPS_Window;
      Application : not null access GPS_Application_Record'Class);
   --  Create a new main window.
   --  The Menubar can be null if none was created

   procedure Register_Keys (Main_Window : access GPS_Window_Record'Class);
   --  Register the key bindings associated with the window

   procedure Setup_Perspective_Selector
     (Self : not null access GPS_Window_Record'Class);
   --  Setup and add the perspectives selector to the toolbar

   procedure Setup_VCS_Selector
     (Self : not null access GPS_Window_Record'Class);
   --  Setup and add the VCS selector to the toolbar

   procedure Quit
     (Main_Window : access GPS_Window_Record'Class;
      Force       : Boolean := False;
      Status      : Integer := 0);
   --  Exit GPS. Ask for confirmation if there are unsaved files and Force is
   --  False. If Force is True, nothing is saved, and GPS exists immediately.
   --  Save the desktop if needed.
   --  Status is the exit status (0 is success)

   procedure Reset_Title
     (Kernel  : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Reset the title of the main window or the current floating window,
   --  depending on which child currently has the focus.

   ----------------------
   -- Python interface --
   ----------------------

   function Create_MDI_Window_Instance
     (Script : not null access Scripting_Language_Record'Class;
      Child  : access MDI_Child_Record'Class) return Class_Instance;
   --  Create a class instance wrapping child

   function Get_Child (Inst : Class_Instance) return MDI_Child;
   --  Return the child stored in an instance of GPS.MDIWindow

end GPS.Main_Window;
