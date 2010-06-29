-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005-2010, AdaCore              --
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

--  This package contains various constants and subprograms used for the
--  GPS-specific usage of the MDI.

with Commands;
with Gtkada.MDI;         use Gtkada.MDI;
with Gtk.Accel_Group;
with Gtk.Handlers;       use Gtk.Handlers;
with Gtk.Icon_Factory;
with Gtk.Main;
with Gtk.Menu;
with Gtk.Tooltips;
with Gtk.Toolbar;
with Gtk.Widget;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;

package GPS.Kernel.MDI is

   ----------------------
   -- Desktop handling --
   ----------------------

   package Kernel_Desktop is new Gtkada.MDI.Desktop (Kernel_Handle);

   type Save_Desktop_Function is access function
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return XML_Utils.Node_Ptr;

   type Load_Desktop_Function is access function
     (MDI  : Gtkada.MDI.MDI_Window;
      Node : XML_Utils.Node_Ptr;
      User : Kernel_Handle) return Gtkada.MDI.MDI_Child;

   procedure Register_Desktop_Functions
     (Save : Save_Desktop_Function;
      Load : Load_Desktop_Function);

   function Get_XML_Content
     (MDI : Gtkada.MDI.MDI_Window;
      Tag : String) return XML_Utils.Node_Ptr;
   --  Wrapper around Kernel_Desktop functions

   function Has_User_Desktop
     (Handle : access Kernel_Handle_Record'Class) return Boolean;
   --  Return True if an user-defined desktop is present, and False
   --  if the default desktop is used.

   procedure Save_Desktop
     (Handle : access Kernel_Handle_Record'Class);
   --  Save the current desktop.

   function Load_Desktop
     (Handle      : access Kernel_Handle_Record'Class;
      For_Project : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File)
      return Boolean;
   --  Reload a saved desktop.
   --  Calls "Show_All" on Handle.Main_Window before loading the desktop.
   --  Return False if no desktop could be loaded (in which case the default
   --  desktop was loaded).

   function Get_Context_For_Child
     (Child : Gtkada.MDI.MDI_Child) return Selection_Context;
   --  Return the context associated with Child.
   --  The user should free the returned value.

   ---------
   -- MDI --
   ---------

   procedure Gtk_New
     (MDI    : out MDI_Window;
      Group  : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class);
   --  Create the MDI and do GPS-specific initializations

   ---------------------
   -- Child positions --
   ---------------------

   --  This is a list of predefined Child_Positions used by various elements
   --  in GPS.

   Group_Graphs         : constant Child_Group := 101;
   Group_VCS_Explorer   : constant Child_Group := 102;
   Group_Debugger_Stack : constant Child_Group := 103;
   Group_Debugger_Data  : constant Child_Group := 104;
   Group_VCS_Activities : constant Child_Group := 105;
   Group_View           : constant Child_Group := 106;
   Group_Consoles       : constant Child_Group := 107;

   function Get_Current_Window
     (Handle : access Kernel_Handle_Record'Class) return Gtk.Window.Gtk_Window;
   --  Return the window containing the current MDI Child.
   --  The main usage for this function should be to display the dialogs
   --  centered with regards to this window.

   type GPS_MDI_Child_Record is new Gtkada.MDI.MDI_Child_Record with private;
   type GPS_MDI_Child is access all GPS_MDI_Child_Record'Class;
   --  Base record for all MDI children that go into the MDI

   procedure Gtk_New
     (Child               : out GPS_MDI_Child;
      Widget              : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags               : Child_Flags := All_Buttons;
      Group               : Child_Group := Group_Default;
      Focus_Widget        : Gtk.Widget.Gtk_Widget := null;
      Default_Width, Default_Height : Glib.Gint := -1;
      Module              : access Module_ID_Record'Class; --  can be null
      Desktop_Independent : Boolean := False);
   --  Recommended version of Gtk_New to use, instead of the one in
   --  GtkAda.MDI. This version has several new parameters:
   --    - Module : used to associate a module with a widget. This is used to
   --               get the current context for instance
   --    - Desktop_Independent: if this is true, then the window will not be
   --               closed  when a new desktop is loaded.
   --    - Use_Scrolled : if this is true, then the widget will be included
   --               inside a scrolled window

   procedure Initialize
     (Child               : access GPS_MDI_Child_Record'Class;
      Widget              : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags               : Child_Flags := All_Buttons;
      Group               : Child_Group := Group_Default;
      Focus_Widget        : Gtk.Widget.Gtk_Widget := null;
      Default_Width, Default_Height : Glib.Gint := -1;
      Module              : access Module_ID_Record'Class;
      Desktop_Independent : Boolean := False);
   --  Internal version of Gtk_New

   procedure Load_Perspective
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String);
   --  Change the current perspective to another one.
   --  Nothing is done if Name does not exist

   procedure Configure_MDI (Kernel : access Kernel_Handle_Record'Class);
   --  Configure the MDI based on the preferences

   procedure Create_MDI_Preferences
     (Kernel : access Kernel_Handle_Record'Class);
   --  Create the preferences for the MDI

   function Get_MDI
     (Handle : access Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Window;
   --  Return the MDI associated with Handle.
   --  Use the Put function below instead of the one in GtkAda.MDI to
   --  associated a widget with a GPS module

   function Get_Module_From_Child
     (Child : Gtkada.MDI.MDI_Child) return Module_ID;
   --  Return the module that created Child, or null if no module was found.

   procedure Tab_Contextual
     (Child : access GPS_MDI_Child_Record;
      Menu  : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add entries to the contextual menu when the user right-click on a tab.
   --  By default, this does nothing.

   function Get_File_Editor
     (Handle : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Gtkada.MDI.MDI_Child;
   --  Return the first MDI child associated to an editor for File.
   --  Return null if no such editor was found.

   function Save_MDI_Children
     (Handle   : access Kernel_Handle_Record'Class;
      Children : Gtkada.MDI.MDI_Child_Array := Gtkada.MDI.No_Children;
      Force    : Boolean := False) return Boolean;
   --  Save all the MDI children, as well as the current project
   --  If Force is False, ask the user first.
   --  If Children is specified, only ask to save these specific children.
   --  The return value is False if the user has cancelled the action, True if
   --  the user has selected OK (whatever the number of children that were
   --  saved).

   procedure Close_All_Children (Handle : access Kernel_Handle_Record'Class);
   --  Close all the MDI children. No confirmation is asked, call
   --  Save_All_MDI_Children first if needed.

   function Get_Command_Queue
     (Child : access GPS_MDI_Child_Record) return Commands.Command_Queue;
   --  Return the command queue associated with the current context. In
   --  particular, this can be used for undo, for instance through
   --     Start_Group (Get_Command_Queue (Child))
   --  By default, it returns Null_Command_Queue

   function Interrupt
     (Child : access GPS_MDI_Child_Record) return Boolean;
   --  The user has selected the /Tools/Interrupt menu while this Child has
   --  the focus. If this function returns False (the default), this indicates
   --  the control-C could not be handled by the child itself, and we
   --  proceed with the default implementation of /Tools/Interrupt which is to
   --  kill the last process that was started.
   --  But the child can decide to process the interrupt itself (and do
   --  something less drastic than killing the whole process), and return
   --  True.

   procedure Set_Font_And_Colors
     (Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Fixed_Font : Boolean);
   --  Change the style of the widget based on the preferences

   -----------------------------------
   -- Misc Gtk+ Related Subprograms --
   -----------------------------------

   package Object_Idle is new Gtk.Main.Idle (Glib.Object.GObject);
   --  General Idle loop for a GObject

   function Get_Current_Focus_Widget
     (Kernel : access Kernel_Handle_Record'Class) return Gtk.Widget.Gtk_Widget;
   --  Return the widget which currently has the keyboard focus. null is
   --  returned if no widget has the focus, or if GPS itself doesn't have
   --  it.

   function Get_Default_Accelerators
     (Handle : access Kernel_Handle_Record'Class)
      return Gtk.Accel_Group.Gtk_Accel_Group;
   --  Returns the defauls accelerators group for the main window

   function Get_Icon_Factory
     (Handle : access Kernel_Handle_Record'Class)
      return Gtk.Icon_Factory.Gtk_Icon_Factory;
   --  Return the default icon factory

   function Get_Tooltips
     (Handle : access Kernel_Handle_Record'Class)
      return Gtk.Tooltips.Gtk_Tooltips;

   function Get_Toolbar
     (Handle : access Kernel_Handle_Record'Class)
      return Gtk.Toolbar.Gtk_Toolbar;
   --  Return the main toolbar associated with the kernel

   function Get_Current_Context
     (Kernel : access Kernel_Handle_Record'Class) return Selection_Context;
   --  Return the context associated with the current MDI child.
   --  The caller should not free the returned value, this is taken care of by
   --  the kernel automatically. If the caller needs to keep the context valid
   --  throughout its execution, it should first call Ref, and then Unref on
   --  the context, similarly for any caller that need to keep a valid context.
   --  The returned value might be null, if the current child doesn't support
   --  selection contexts.
   --  This function is mostly intended to be called for the callbacks in the
   --  menu bar.
   --  The context returned will be that of the active contextual menu if there
   --  is one at that point in time (therefore, we ignore cases where for
   --  instance a new child has been selected automatically at that point)

   ---------------------
   -- Signal emission --
   ---------------------

   procedure Setup (Data : Glib.Object.GObject; Id : Gtk.Handlers.Handler_Id);
   --  Make sure that when Data is destroyed, Id is properly removed

   package Object_User_Callback is new Gtk.Handlers.User_Callback_With_Setup
     (Glib.Object.GObject_Record, Glib.Object.GObject, Setup);
   --  Generic callback that can be used to connect a signal to a kernel

   package Object_Return_Callback is new Gtk.Handlers.Return_Callback
     (Glib.Object.GObject_Record, Boolean);
   --  Generic callback that can be used to connect a signal to a kernel

   package Object_User_Return_Callback
     is new Gtk.Handlers.User_Return_Callback_With_Setup
     (Widget_Type => Glib.Object.GObject_Record,
      User_Type   => Glib.Object.GObject,
      Return_Type => Boolean,
      Setup       => Setup);
   --  Generic callback that can be used to connect a signal to a kernel

   package Kernel_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Kernel_Handle);
   --  Generic callback that can be used to connect a signal to a kernel

   type File_Project_Record is record
      Project : GNATCOLL.Projects.Project_Type;
      File    : aliased GNATCOLL.VFS.Virtual_File;
   end record;

   package File_Project_Cb is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, File_Project_Record);
   --  Generic callback that can be used to connect a signal to a kernel

   package Entity_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Entities.Entity_Information);

private

   type GPS_MDI_Child_Record is new Gtkada.MDI.MDI_Child_Record with record
      Module              : Abstract_Module_ID;
      Desktop_Independent : Boolean;
   end record;

end GPS.Kernel.MDI;
