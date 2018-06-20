------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
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

--  This package contains various constants and subprograms used for the
--  GPS-specific usage of the MDI.

with Ada.Calendar;          use Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Commands;
with Default_Preferences;
with GNAT.SHA1;             use GNAT.SHA1;
with GNATCOLL.Scripts;
with GNATCOLL.Utils;        use GNATCOLL.Utils;
with GPS.Kernel.Modules;    use GPS.Kernel.Modules;
with GPS.Markers;           use GPS.Markers;
with Gdk.Event;             use Gdk.Event;
with Glib.Main;
with Glib.Object;
with Glib.Xml_Int;
with Gtk.Accel_Group;
with Gtk.Container;         use Gtk.Container;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Handlers;          use Gtk.Handlers;
with Gtk.Menu;
with Gtk.Toolbar;
with Gtk.Widget;
with Gtk.Window;            use Gtk.Window;
with Gtkada.MDI;            use Gtkada.MDI;

package GPS.Kernel.MDI is

   type General_UI_Module_Record is new Module_ID_Record with private;
   type General_UI_Module is access all General_UI_Module_Record'Class;

   overriding function Bookmark_Handler
     (Module : access General_UI_Module_Record;
      Load   : XML_Utils.Node_Ptr := null) return Location_Marker;

   overriding procedure Destroy (Module : in out General_UI_Module_Record);

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the General_UI_Module

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
     (Handle              : access Kernel_Handle_Record'Class;
      Desktop_Perspective : String := "");
   --  Save the current desktop.
   --  Current perspective will be replaced to passed in parameter in
   --  XML file and will be used as first perspective when GPS starts
   --  next time. This does not modify perspective for current session.
   --  Next attempts to call this procedure will do nothing if
   --  Desktop_Perspective is set.

   function Load_Desktop
     (Handle      : access Kernel_Handle_Record'Class;
      For_Project : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File)
      return Boolean;
   --  Reload a saved desktop.
   --  Calls "Show_All" on Handle.Main_Window before loading the desktop.
   --  Return False if no desktop could be loaded (in which case the default
   --  desktop was loaded).

   ---------
   -- MDI --
   ---------

   procedure Gtk_New
     (MDI    : out MDI_Window;
      Kernel : not null access Kernel_Handle_Record'Class;
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
      Kernel              : not null access Kernel_Handle_Record'Class;
      Flags               : Child_Flags := All_Buttons;
      Group               : Child_Group := Group_Default;
      Focus_Widget        : Gtk.Widget.Gtk_Widget := null;
      Default_Width, Default_Height : Glib.Gint := -1;
      Module              : access Module_ID_Record'Class := null;
      Desktop_Independent : Boolean := False;
      Areas               : Allowed_Areas := Both);
   --  Recommended version of Gtk_New to use, instead of the one in
   --  GtkAda.MDI. This version has several new parameters:
   --    - Module : used to associate a module with a widget. This is used to
   --      get the current context for instance
   --    - Desktop_Independent: if this is true, then the window will not be
   --      closed  when a new desktop is loaded.
   --    - Use_Scrolled : if this is true, then the widget will be included
   --      inside a scrolled window

   procedure Initialize
     (Child               : access GPS_MDI_Child_Record'Class;
      Widget              : access Gtk.Widget.Gtk_Widget_Record'Class;
      Kernel              : not null access Kernel_Handle_Record'Class;
      Flags               : Child_Flags := All_Buttons;
      Group               : Child_Group := Group_Default;
      Focus_Widget        : Gtk.Widget.Gtk_Widget := null;
      Default_Width, Default_Height : Glib.Gint := -1;
      Module              : access Module_ID_Record'Class := null;
      Desktop_Independent : Boolean := False;
      Areas               : Allowed_Areas := Both);
   --  Internal version of Gtk_New

   overriding function Save_Desktop
     (Self : not null access GPS_MDI_Child_Record)
      return Glib.Xml_Int.Node_Ptr;
   function Save_Desktop
     (Self : not null access GPS_MDI_Child_Record) return XML_Utils.Node_Ptr;
   --  Replaces the version from GtkAda, since Node_Ptr is of a different type.
   --  By default, this calls the subprogram set via Set_Save_Desktop_Callback.

   procedure Set_Save_Desktop_Callback
     (Self     : not null access GPS_MDI_Child_Record;
      Callback : GNATCOLL.Scripts.Subprogram_Type);
   --  Set the subprogram to be called by the default Save_Desktop. This will
   --  have no effect if you override Save_Desktop

   overriding procedure Set_Title
     (Child       : access GPS_MDI_Child_Record;
      Title       : String;
      Short_Title : String := "");
   overriding procedure Set_Default_Size_For_Floating_Window
     (Child : not null access GPS_MDI_Child_Record;
      Win   : not null access Gtk.Window.Gtk_Window_Record'Class;
      Width, Height : Glib.Gint);
   overriding procedure Create_Float_Window_For_Child
      (Child     : not null access GPS_MDI_Child_Record;
       Win       : out Gtk_Window;
       Container : out Gtk_Container);
   --  see inherited documentation

   procedure Load_Perspective
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String);
   --  Change the current perspective to another one.
   --  Nothing is done if Name does not exist

   function Perspective_Exists
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String) return Boolean;
   --  Chech whether a perspective with that name exists.

   procedure Configure_MDI
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference := null);
   --  Configure the MDI based on the preferences.
   --  If specified, Pref is used to find out whether any reconfiguration needs
   --  to be done. It is intended to be used when the user has changed a
   --  preference.

   procedure Create_MDI_Preferences
     (Kernel : access Kernel_Handle_Record'Class);
   --  Create the preferences for the MDI

   function Get_MDI
     (Handle : access Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Window;
   --  Return the MDI associated with Handle.
   --  Use the Put function below instead of the one in GtkAda.MDI to
   --  associated a widget with a GPS module

   function Kernel
     (Self : not null access GPS_MDI_Child_Record) return Kernel_Handle;
   --  Return a handle to the GPS kernel.

   function Get_Child_Class
     (Self : not null access GPS_MDI_Child_Record)
     return GNATCOLL.Scripts.Class_Type;
   --  Return the class to use for instances representing the widget
   --  contained in Self.
   --  Might return No_Class to use the default GPS.GUI class

   function Get_Module_From_Child
     (Child : not null access Gtkada.MDI.MDI_Child_Record'Class)
      return Module_ID;
   --  Return the module that created Child, or null if no module was found.

   procedure Set_Toolbar
     (Child   : not null access GPS_MDI_Child_Record'Class;
      Toolbar : access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   function Get_Toolbar
     (Child : not null access GPS_MDI_Child_Record'Class)
      return Gtk.Toolbar.Gtk_Toolbar;
   --  Return the local toolbar for the MDI child, if there is one.

   function Has_Menu_Bar_When_Floating
      (Child : not null access GPS_MDI_Child_Record) return Boolean
      is (False) with Inline;
   --  Whether to add a menubar when the child is made floating

   function Build_Context
     (Self        : not null access GPS_MDI_Child_Record;
      Dummy_Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context
     is (New_Context (Self.Kernel, Get_Module_From_Child (Self)));
   --  Return the current context for Self.
   --  When no event is specified, the context should related to the current
   --  selection in the view (for a tree view, this is a description of the
   --  selected row for instance).
   --  When an event is specified, this function should take the event into
   --  account and change the selection in the view. It should then return the
   --  new context. An event is only given before we display a contextual menu.

   function Get_Actual_Widget
     (Self : not null access GPS_MDI_Child_Record)
      return Gtk.Widget.Gtk_Widget
     is (Self.Get_Widget);
   --  Returns the actual widget that was put in the MDI.
   --  When using the Generic_Views package, it is possible that this widget
   --  has been encapsulated to provide a local menubar and other decorations,
   --  so the actual MDI children will override this primitive appropriately.

   procedure Tab_Contextual
     (Child : access GPS_MDI_Child_Record;
      Menu  : access Gtk.Menu.Gtk_Menu_Record'Class) is null;
   --  Add entries to the contextual menu when the user right-click on a tab.

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

   --------------
   -- Tooltips --
   --------------
   --  GPS provides tooltips in notebook tabs. The text of those tooltips can
   --  be controlled by overridden via Gtkada.MDI.Get_Tooltips.
   --  The following provides standard tooltips for some of the data types
   --  manipulated by GPS:

   function Get_Tooltip_For_File
     (Kernel  : not null access Kernel_Handle_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type := GNATCOLL.Projects.No_Project;
      With_VCS : Boolean := True)
      return String;
   --  Return the tooltip text for a file (which belongs to a specific
   --  project, possibly looked up dynamically if unspecified and
   --  unambiguous).
   --  Information on VCS status is included if With_VCS is true.
   --  This is markup text (including <b> special markup)

   function Get_Tooltip_For_Directory
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Directory : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type := GNATCOLL.Projects.No_Project)
      return String;
   --  Return the tooltip text for a directory.
   --  This is markup text (including <b> special markup)

   ---------------------
   -- Views and files --
   ---------------------
   --  The following operations are used to associate a view to some files on
   --  the disk, with the following results:
   --  * warn the user when the file has changed on disk (or was removed), so
   --    that he can choose to reload the file).
   --  * mark the view as modified and ask the user to save it before a build,
   --    before exit, and so on.

   procedure Monitor_File
     (Self : not null access GPS_MDI_Child_Record;
      File : GNATCOLL.VFS.Virtual_File);
   --  Indicates that the child is somehow displaying the contents of the file.
   --  As a result, user will get warnings when the file changes on the disk.
   --  You must override Reload to perform any useful operation when the file
   --  was changed and the user decides to reload it.
   --  A view that monitors a file automatically gets an icon to indicate the
   --  status of the view (modified or not for instance).

   procedure Update_File_Info (Self : not null access GPS_MDI_Child_Record);
   --  Update cached information about the monitored files, like their
   --  timestamps and checksums. This should be called after saving the file
   --  explicitly, for instance.

   procedure Reload (Self : not null access GPS_MDI_Child_Record) is null;
   --  Reload the contents of the view after the monitored files have changed
   --  on the disk.
   --  The file might no longer exist on disk, in which case the view should be
   --  closed.

   function Report_Deleted_File
     (Self : not null access GPS_MDI_Child_Record) return Boolean is (True);
   --  Views can chose not to let users know when a file has been removed on
   --  disk (for instance temporary files for source editors).

   function Check_Monitored_Files
     (Kernel       : not null access Kernel_Handle_Record'Class;
      Interactive  : Boolean := True;
      Only_On_File : Virtual_File := No_File)
     return Boolean;
   procedure Check_Monitored_Files_In_Background
     (Kernel      : not null access Kernel_Handle_Record'Class);
   --  For each MDI child that monitors files, checks whether the file has been
   --  updated on the disk (including computing checksums, so that simple
   --  timestamp changes do not impact GPS), and either automatically reload
   --  them or display an interactive dialog to the user.
   --  A single dialog is displayed for all modified files.
   --  If Only_On_File is not No_File, check only for that given file.
   --  Returns True if some files were modified and either Interactive was
   --  False, or Interactive was True and the user chose not to reload at
   --  least one file.
   --  Automatic reloading is performed if Interactive is False.

   --------------------------
   -- MDI Location markers --
   --------------------------

   --  These location markers allow placing a location mark on an MDI child.

   type MDI_Location_Marker_Data is new Location_Marker_Data with private;

   function Create_MDI_Marker
     (Kernel : not null access Kernel_Handle_Record'Class;
      Name   : String) return Location_Marker;
   --  Create a location marker from the name of an MDI child

   overriding function Go_To
     (Marker : not null access MDI_Location_Marker_Data) return Boolean;
   overriding function To_String
     (Marker : not null access MDI_Location_Marker_Data) return String;
   overriding function Save
     (Marker : not null access MDI_Location_Marker_Data)
     return XML_Utils.Node_Ptr;
   overriding function Similar
     (Left  : not null access MDI_Location_Marker_Data;
      Right : not null access Location_Marker_Data'Class) return Boolean;
   overriding function Distance
     (Left  : not null access MDI_Location_Marker_Data;
      Right : not null access Location_Marker_Data'Class) return Integer;

   -----------------------------------
   -- Misc Gtk+ Related Subprograms --
   -----------------------------------

   package Object_Idle is new Glib.Main.Generic_Sources (Glib.Object.GObject);
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
   package Kernel_Return_Callback is new Gtk.Handlers.User_Return_Callback
     (Glib.Object.GObject_Record, Boolean, Kernel_Handle);
   --  Generic callback that can be used to connect a signal to a kernel

   type File_Project_Record is record
      Project : GNATCOLL.Projects.Project_Type;
      File    : aliased GNATCOLL.VFS.Virtual_File;
   end record;

   package File_Project_Cb is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, File_Project_Record);
   --  Generic callback that can be used to connect a signal to a kernel

private

   type General_UI_Module_Record is new Module_ID_Record with record
      Desktop_Saved : Boolean := False;
      --  Control whether desktop already saved and no more needed to save it
   end record;

   type Monitored_File is record
      File      : GNATCOLL.VFS.Virtual_File;
      Timestamp : Ada.Calendar.Time := GNATCOLL.Utils.No_Time;
      Sha1      : GNAT.SHA1.Message_Digest;
   end record;
   No_Monitored_File : constant Monitored_File :=
     (File      => GNATCOLL.VFS.No_File,
      Timestamp => GNATCOLL.Utils.No_Time,
      Sha1      => (others => '-'));

   type GPS_MDI_Child_Record is new Gtkada.MDI.MDI_Child_Record with record
      Module              : Abstract_Module_ID;
      Desktop_Independent : Boolean;
      Save_Desktop        : GNATCOLL.Scripts.Subprogram_Type;
      Kernel              : Kernel_Handle;
      Toolbar             : Gtk.Toolbar.Gtk_Toolbar := null;

      Default_Width, Default_Height : Glib.Gint := -1;

      Files               : Monitored_File := No_Monitored_File;
   end record;

   type MDI_Location_Marker_Data is new Location_Marker_Data with record
      Title  : Unbounded_String;
      Kernel : access Kernel_Handle_Record'Class;
   end record;

end GPS.Kernel.MDI;
