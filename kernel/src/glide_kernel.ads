-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you  can redistribute it and/or modify  it --
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

--  This package is the root of the GPS' kernel API.

with Basic_Mapper;
with GNAT.OS_Lib;
with Generic_List;
with Glib.Object;  use Glib;
with Glib.Xml_Int;
with Gdk;
with Gtk.Handlers;
with Gtk.Accel_Group;
with Gtk.Main;
with Gtk.Menu;
with Gtk.Toolbar;
with Gtk.Tooltips;
with Gtk.Widget;
with Gtk.Window;
with Gtkada.MDI;
with Language_Handlers;
with Src_Info;
with Src_Info.Queries;
with String_Hash;
with System;
with Ada.Unchecked_Conversion;
with Default_Preferences;
with Histories;
with Projects.Registry;
with Task_Manager;
with Generic_List;
with VFS;

package Glide_Kernel is

   type Kernel_Handle_Record is new Glib.Object.GObject_Record with private;
   type Kernel_Handle is access all Kernel_Handle_Record'Class;
   --  A kernel handle used to share information throughout Glide.

   package Kernel_Desktop is new Gtkada.MDI.Desktop (Kernel_Handle);

   procedure Gtk_New
     (Handle      : out Kernel_Handle;
      Main_Window : Gtk.Window.Gtk_Window;
      Home_Dir    : String);
   --  Create a new Glide kernel.
   --  By default, it isn't associated with any project, nor any source editor.
   --  Home_Dir is the directory under which config files can be loaded/saved.

   procedure Load_Preferences (Handle : access Kernel_Handle_Record);
   --  Load the preferences from the user's file ~/.gps/preferences.

   procedure Destroy (Handle : access Kernel_Handle_Record);
   --  Free the memory occupied by the kernel.

   function Get_Default_Accelerators
     (Handle : access Kernel_Handle_Record)
      return Gtk.Accel_Group.Gtk_Accel_Group;
   --  Returns the defauls accelerators group for the main window.

   function Has_User_Desktop
     (Handle : access Kernel_Handle_Record) return Boolean;
   --  Return True if an user-defined desktop is present, and False
   --  if the default desktop is used.

   procedure Save_Desktop
     (Handle             : access Kernel_Handle_Record;
      As_Default_Desktop : Boolean := False);
   --  Save the current desktop.
   --  If As_Default_Desktop is true, then this desktop will be loaded any time
   --  no other valid desktop is found in the future.

   function Load_Desktop (Handle : access Kernel_Handle_Record) return Boolean;
   --  Reload a saved desktop.
   --  Calls "Show_All" on Handle.Main_Window before loading the desktop.
   --  Return False if no desktop could be loaded (in which case the default
   --  desktop was loaded).

   function Get_Main_Window
     (Handle : access Kernel_Handle_Record) return Gtk.Window.Gtk_Window;
   --  Return the main window associated with the kernel.
   --  The main usage for this function should be to display the dialogs
   --  centered with regards to this window.

   function Get_Tooltips
     (Handle : access Kernel_Handle_Record) return Gtk.Tooltips.Gtk_Tooltips;
   --  Return the widget used to register tooltips for the graphical interface.

   function Get_Toolbar
     (Handle : access Kernel_Handle_Record) return Gtk.Toolbar.Gtk_Toolbar;
   --  Return the main toolbar associated with the kernel.

   function Get_History
     (Handle : access Kernel_Handle_Record) return Histories.History;
   --  Return the history database

   procedure Add_To_History
     (Handle    : access Kernel_Handle_Record;
      Key       : Histories.History_Key;
      New_Entry : String);
   --  Add a new entry in the history database

   type Kernel_State is
     (Idle,
      --  Kernel is idle, waiting for input

      Processing,
      --  Kernel is processing, other processing are possible

      Busy
      --  Kernel is busy, no other processing possible
     );
   --  Possible states of the kernel.

   subtype Action_Kernel_State is Kernel_State range Processing .. Busy;

   procedure Push_State
     (Handle : Kernel_Handle;
      State  : Action_Kernel_State);
   --  Push a new state for kernel.
   --  If State is Busy, no further action and calls to Push_State are
   --  allowed. Several Processing states can be pushed.
   --  This procedure usually involves changing the cursor appearance and
   --  displaying an animation.
   --  If Handle is null, do nothing.

   procedure Pop_State (Handle : Kernel_Handle);
   --  Undo previous state.

   function Get_Home_Dir (Handle : access Kernel_Handle_Record) return String;
   --  Return the Home directory. (eg ~/.gps/).
   --  The directory ends with a directory separator

   function Get_System_Dir
     (Handle : access Kernel_Handle_Record) return String;
   --  Return the installation directory for GPS. This always ends up with a
   --  directory separator.

   function Get_Logs_Mapper
     (Handle : access Kernel_Handle_Record)
      return Basic_Mapper.File_Mapper_Access;
   --  Return the mapper for file logs.

   procedure Set_Logs_Mapper
     (Handle : access Kernel_Handle_Record;
      Mapper : Basic_Mapper.File_Mapper_Access);
   --  Set the mapper for file logs.

   function Get_Language_Handler
     (Handle : access Kernel_Handle_Record)
      return Language_Handlers.Language_Handler;
   --  Return the language handler used by this kernel.

   function GNAT_Version
     (Handle : access Kernel_Handle_Record) return String;
   --  Return a string containing the GNAT version number.
   --  The string has the form "3.16w (20020610)"

   -----------
   -- Files --
   -----------
   --  The following subprograms are provided in addition to the ones provided
   --  in vfs.ads.

   function Create
     (Name            : Glib.UTF8_String;
      Kernel          : access Kernel_Handle_Record;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True) return VFS.Virtual_File;
   --  Create a new file. This will automatically try to solve Name to an
   --  absolute path if it currently is a base name.
   --
   --  If Name contains a relative path, the editor will open it as is. It
   --  thus depends on the current directory, and should only be used for files
   --  opened from the command line. As a result, Name might be found even
   --  if it doesn't directly belong to a project.

   -------------
   -- Queries --
   -------------
   --  The following programs are provided as proxies for the ones in
   --  Src_Info.Queries. They should be used instead of the other ones so that
   --  the list of parsed LI files can be kept in the kernel

   procedure Reset_LI_File_List (Handle : access Kernel_Handle_Record);
   --  Reset the LI files currently in memory, and free associated memory

   function Locate_From_Source_And_Complete
     (Handle          : access Kernel_Handle_Record;
      Source_Filename : VFS.Virtual_File;
      Check_Timestamp : Boolean := True) return Src_Info.LI_File_Ptr;
   --  Find the ALI file for Source_Filename, and return a handle to it.
   --  null is returned if the LI file couldn't be parsed. It is guaranteed
   --  that the returned LI file has been fully parsed.
   --  If Check_Timestamp is False, and there is already a version of this
   --  LI file in memory, that version is not updated, and just returned as is.

   function Other_File_Name
     (Kernel          : access Kernel_Handle_Record;
      Source_Filename : VFS.Virtual_File) return VFS.Virtual_File;
   --  Return the other file associated with Source_Filename (the spec if
   --  Source_Filename is a body or separate, the body if Source_Filename is
   --  the spec).
   --  No_File is returned if the file wasn't found (and error
   --  messages are printed to the console appropriately).

   procedure Find_Declaration_Or_Overloaded
     (Kernel        : access Kernel_Handle_Record;
      Lib_Info      : Src_Info.LI_File_Ptr;
      File_Name     : VFS.Virtual_File;
      Entity_Name   : String;
      Line          : Positive;
      Column        : Positive;
      Entity        : out Src_Info.Queries.Entity_Information;
      Status        : out Src_Info.Queries.Find_Decl_Or_Body_Query_Status);
   --  See Src_Info.Queries.Find_Declaration.
   --  If the request to Src_Info.Queries.Find_Declaration returns an
   --  unresolved overloaded entity, this subprogram automatically asks the
   --  user to chose among the possible completions. The declaration for the
   --  user's choice is returned, and Status set to Success if necessary.

   procedure Find_Next_Body
     (Kernel      : access Kernel_Handle_Record;
      Lib_Info    : Src_Info.LI_File_Ptr;
      File_Name   : VFS.Virtual_File;
      Entity_Name : String;
      Line        : Positive;
      Column      : Positive;
      Location    : out Src_Info.File_Location;
      Status      : out Src_Info.Queries.Find_Decl_Or_Body_Query_Status);
   --  See Src_Info.Queries.
   --  If the request to Src_Info.Queries.Find_Next_Body returns an unresolved
   --  overloaded entity, this subprogram automatically asks the user to chose
   --  among the possible completions.

   procedure Find_Next_Body
     (Kernel      : access Kernel_Handle_Record;
      Lib_Info    : Src_Info.LI_File_Ptr;
      Entity      : Src_Info.Queries.Entity_Information;
      Location    : out Src_Info.File_Location;
      Status      : out Src_Info.Queries.Find_Decl_Or_Body_Query_Status);
   --  Same as above, but with a slightly simpler parameter list.
   --  This only gives access to the first body.

   procedure Parse_All_LI_Information
     (Kernel       : access Kernel_Handle_Record;
      In_Directory : String);
   --  Parse all the LI information in In_Directory, for all the supported
   --  languages. This can be used in cases where there is no obvious way to
   --  find the LI file matching a given source file (for instance, with a
   --  separate krunched file in Ada).

   procedure Get_Scope_Tree
     (Kernel : access Kernel_Handle_Record;
      Entity : Src_Info.Queries.Entity_Information;
      Node   : out Src_Info.Scope_Tree_Node);
   --  Create the scope tree for the entity, and set node to point to the node
   --  for Entity.
   --  The returned Tree must be freed by the caller, if not Null_Scope_Tree.

   function Scope_To_String (Scope : Src_Info.E_Scope) return String;
   --  Return a printable string for the scope.

   function Is_Open
     (Kernel   : access Kernel_Handle_Record;
      Filename : VFS.Virtual_File) return Boolean;
   --  Whether Filename is currently opened in an editor.

   function Open_Files
     (Kernel : access Kernel_Handle_Record) return VFS.File_Array;
   --  Return a list of currently open files.

   ---------------
   -- Module ID --
   ---------------

   type Module_ID_Information (<>) is private;
   type Module_ID_Record is tagged private;
   type Module_ID is access all Module_ID_Record'Class;
   --  Module identifier. Each of the registered module in Glide has such a
   --  identifier, that contains its name and all the callbacks associated with
   --  the module.

   procedure Destroy (Id : in out Module_ID_Record);
   --  Free the memory associated with the module. By default, this does
   --  nothing.

   function Get_Current_Module
     (Kernel : access Kernel_Handle_Record) return Module_ID;
   --  Return the module the currently selected MDI child belongs to.
   --  null might be returned if there is either no selected child or GPS
   --  couldn't find its module

   procedure Free (Module : in out Module_ID);
   --  Free memory associated to a Module_ID.

   package Module_List is new Generic_List (Module_ID);

   --------------
   -- Contexts --
   --------------

   type Selection_Context is tagged private;
   type Selection_Context_Access is access all Selection_Context'Class;
   --  This type contains all the information about the selection in any
   --  module. Note that this is a tagged type, so that it can easily be
   --  extended for modules external to Glide

   function To_Selection_Context_Access is new
     Ada.Unchecked_Conversion (System.Address, Selection_Context_Access);

   procedure Set_Context_Information
     (Context : access Selection_Context;
      Kernel  : access Kernel_Handle_Record'Class;
      Creator : Module_ID);
   --  Set the information in the context

   function Get_Kernel
     (Context : access Selection_Context) return Kernel_Handle;
   --  Return the kernel associated with the context

   function Get_Creator (Context : access Selection_Context) return Module_ID;
   --  Return the module ID for the module that created the context

   procedure Destroy (Context : in out Selection_Context);
   --  Destroy the information contained in the context, and free the memory.
   --  Note that implementations of this subprogram should always call the
   --  parent's Destroy subprogram as well

   procedure Ref (Context : Selection_Context_Access);
   --  Increments the reference counter for the context

   procedure Unref (Context : in out Selection_Context_Access);
   --  Free the memory occupied by Context. It automatically calls the
   --  primitive subprogram Destroy as well.
   --  It in fact decrements the reference counter for Context, and frees the
   --  memory only if this counter reaches 0.

   function Get_Current_Context
     (Kernel : access Kernel_Handle_Record) return Selection_Context_Access;
   --  Return the context associated with the current MDI child.
   --  The caller should not free the returned value, this is taken care of by
   --  the kernel automatically. If the caller needs to keep the context valid
   --  throughout its execution, it should first call Ref, and then Unref on
   --  the context, similarly for any caller that need to keep a valid context.
   --  The returned value might be null, if the current child doesn't support
   --  selection contexts.
   --  This function is mostly intended to be called for the callbacks in the
   --  menu bar.

   function Get_Context_For_Child
     (Kernel : access Kernel_Handle_Record;
      Child  : Gtkada.MDI.MDI_Child) return Selection_Context_Access;
   --  Return the context associated with Child.
   --  The user should free the returned value.

   function Get_Current_Focus_Widget
     (Kernel : access Kernel_Handle_Record) return Gtk.Widget.Gtk_Widget;
   --  Return the widget which currently has the keyboard focus. null is
   --  returned if no widget has the focus, or if GPS itself doesn't have
   --  it.

   -------------
   -- Modules --
   -------------
   --  See documentation in Glide_Kernel.Modules

   type Module_Priority is new Natural;
   Low_Priority     : constant Module_Priority := Module_Priority'First;
   Default_Priority : constant Module_Priority := 500;
   High_Priority    : constant Module_Priority := Module_Priority'Last;
   --  The priority of the module.
   --  Modules with a higher priority are always called before modules with
   --  lower priority, for instance when computing the contents of a contextual
   --  menu.

   type Module_Menu_Handler is access procedure
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Callback used every time some contextual menu event happens in GPS.
   --  The module that initiated the event (ie the one that is currently
   --  displaying the contextual menu) can be found by reading Get_Creator for
   --  the context.
   --
   --  The object that is displaying the contextual menu is Object. Note that
   --  this isn't necessarily the widget in which the mouse event occurs.
   --
   --  Context contains all the information about the current selection.
   --
   --  The callback should add the relevant items to Menu. It is recommended to
   --  use Glide_Kernel.Modules.Context_Callback below to connect signals to
   --  the items.

   type Module_Default_Context_Factory is access function
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  A function called when the kernel needs to get the current context for
   --  an MDI child. This is used mostly when generating a context for the
   --  menubar menu items.
   --  Child is the widget that was put directly in the MDI. It is always of
   --  the type MDI_Child_Tag registered with Register_Module.

   type Save_Function_Mode is (Query, Action);
   --  The two types of use for Module_Save_Function.
   --  If Query, then the save_function should return whether the corresponding
   --  child has been modified, and not saved yet, and thus whether we should
   --  ask the user whether to save it.
   --  If Action, the save_function *must* save the child. The return value
   --  then indicates whether the save was successful (True), or failed (False)

   type Module_Save_Function is access function
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget;
      Mode   : Save_Function_Mode) return Boolean;
   --  A function called when the kernel asks a MDI child to save itself.
   --  See the description of Mode for the description of the modes.
   --  Child is the widget that put directly in the MDI.

   type Module_Tooltip_Handler is access procedure
     (Context : access Selection_Context'Class;
      Pixmap  : out Gdk.Gdk_Pixmap;
      Width   : out Glib.Gint;
      Height  : out Glib.Gint);
   --  Callback used every time some tooltip event happens in GPS.
   --  Context contains all the information about the context of the tooltip.
   --
   --  The first callback that will decide to handle the tooltip will set
   --  pixmap, width and height (of the pixmap), which will stop the
   --  propagation of the tooltip message (since only one module can display
   --  a tooltip at a time).
   --
   --  Since only one module will handle the tooltip, putting proper priorities
   --  when registering the modules is very important.
   --
   --  See the function GUI_Utils.Create_Pixmap_From_Text for an easy way to
   --  create a tooltip that only contains text

   type Customization_Level is
     (Hard_Coded, System_Wide, Project_Wide, User_Specific, Themes);
   --  The various level of customization (See Glide_Kernel.Custom).
   --  Hard_Coded is used for customization that are hard-coded in the GPS code
   --  System_Wide is used if customization comes from a custom file found in
   --  the installation directory of GPS.
   --  Project_Wide is used if the customization comes from a custom file found
   --  in one of the directories lists in GPS_CUSTOM_PATH.
   --  User_Specific is used if the customization comes from a custom file
   --  found in the user's own directory (see GPS_HOME/.gps/customize).
   --  Themes is used if the customization was found in a theme definition,
   --  wherever that definition was found.

   type Module_Customization_Handler is access procedure
     (Kernel : access Kernel_Handle_Record'Class;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level);
   --  Subprogram called when a new customization has been parsed.
   --  It is initially called just after all modules have been registered,
   --  and gets passed the first child of the XML file (that is you must go
   --  through the list, following the Next nodes).

   --------------------
   -- Action filters --
   --------------------

   type Action_Filter_Record is abstract tagged private;
   type Action_Filter is access all Action_Filter_Record'Class;

   function Filter_Matches_Primitive
     (Filter  : access Action_Filter_Record;
      Context : Selection_Context_Access;
      Kernel  : access Kernel_Handle_Record'Class) return Boolean is abstract;
   --  Whether the current context matches Filter.
   --  Context doesn't need to be Ref-ed or Unref-ed.

   type Base_Action_Filter_Record (<>)
      is new Action_Filter_Record with private;
   type Base_Action_Filter is access Base_Action_Filter_Record'Class;

   function Filter_Matches_Primitive
     (Filter  : access Base_Action_Filter_Record;
      Context : Selection_Context_Access;
      Kernel  : access Kernel_Handle_Record'Class) return Boolean;
   --  See docs for inherited subprograms

   function Create
     (Language   : String := "";
      Shell      : String := "";
      Shell_Lang : String := "Shell";
      Module     : String := "") return Base_Action_Filter;
   --  Create a new filter.
   --  It does a logical AND for all its attributes specified as parameters.
   --  The default values for the parameters indicate that no special filter
   --  is done for this particular parameter.

   function "and"
     (Filter1, Filter2 : access Action_Filter_Record'Class)
      return Base_Action_Filter;
   function "or"
     (Filter1, Filter2 : access Action_Filter_Record'Class)
      return Base_Action_Filter;
   --  Execute logical operations between filters

   procedure Set_Error_Message (Filter : Action_Filter; Msg : String);
   --  Set the error message to display if Filter doesn't match

   function Get_Error_Message (Filter : Action_Filter) return String;
   --  Return the error message to display if the filter doesn't match.

   function Get_Name (Filter : Action_Filter) return String;
   --  Return the description of the filter (a short string suitable for
   --  display in the key manager GUI

   function Filter_Matches
     (Filter  : Action_Filter;
      Context : Selection_Context_Access;
      Kernel  : access Kernel_Handle_Record'Class) return Boolean;
   --  Same as Filter_Matches_Primitive, except it matches if Filter is null


   procedure Register_Filter
     (Kernel  : access Kernel_Handle_Record;
      Filter  : access Action_Filter_Record'Class;
      Name    : String);
   --  Record the filter in the kernel, so that it is can be referenced in
   --  other places.

   function Lookup_Filter
     (Kernel : access Kernel_Handle_Record;
      Name   : String) return Action_Filter;
   --  Lookup a filter by name. Return null if no such filter has been
   --  registered.

   type Action_Filter_Iterator is private;

   function Start (Kernel : access Kernel_Handle_Record'Class)
      return Action_Filter_Iterator;
   --  Return the first filter registered in the kernel (this is in no
   --  particular order).

   procedure Next
     (Kernel : access Kernel_Handle_Record'Class;
      Iter   : in out Action_Filter_Iterator);
   --  Move to the next action

   function Get (Iter : Action_Filter_Iterator) return Action_Filter;
   --  Return the current filter

   -----------
   -- Hooks --
   -----------
   --  See the package GlidE_Kernel.Hooks for more subprograms applying to
   --  hooks.

   type Hook_Function_Record is abstract tagged private;
   type Hook_Function is access all Hook_Function_Record'Class;
   --  Hooks are defined as tagged types, so that the user can easily store
   --  his own data to be memorized with the hook.

   procedure Destroy (Hook : in out Hook_Function_Record);
   --  Destroy the memory associated with Hook.
   --  By default, this does nothing.

   function Get_Name (Hook : Hook_Function_Record) return String;
   --  Return the name to use for that function when listing all functions
   --  attached to a hook.
   --  The default is to use <internal>

   -----------
   -- Tools --
   -----------
   --  The following subprograms are used to register the properties of the
   --  various external tools declared by the user in the customization files.
   --  These are associated with the <tool> tag.
   --  Not all the information is stored here, since some of the modules have
   --  their own handling. This is the case for the <switches> attribute, which
   --  is handled internally by the prj_editor module. This is also the case
   --  for the <language> tag.

   type Tool_Properties_Record is record
      Project_Package   : GNAT.OS_Lib.String_Access;
      Project_Attribute : GNAT.OS_Lib.String_Access;
      Project_Index     : GNAT.OS_Lib.String_Access;
      Initial_Cmd_Line  : GNAT.OS_Lib.String_Access;
   end record;
   --  (Project_Package, Project_Attribute, Project_Index) describe where its
   --  switches are stored in a project.
   --  Initial_Cmd_Line are the switches when the user hasn't edited them
   --  explicitely.
   --  Any of these field can be left to null if it has no special
   --  signification for this tool.

   No_Tool : constant Tool_Properties_Record;

   procedure Register_Tool
     (Kernel    : access Kernel_Handle_Record;
      Tool_Name : String;
      Tool      : Tool_Properties_Record);
   --  Register a new tool.
   --  No copy is made for Tool, which must therefore not be freed by the
   --  caller

   function Get_Tool_Properties
     (Kernel    : access Kernel_Handle_Record;
      Tool_Name : String) return Tool_Properties_Record;
   --  Return the properties of the tool.
   --  The resulting record must not be freed by the caller.

   function Get_Tool_Name
     (Kernel    : access Kernel_Handle_Record;
      Pkg_Name  : String;
      Attribute : String;
      Index     : String) return String;
   --  Return the name of the tool associated with a specific attribute to save
   --  the switches. The empty string is returned if there is no such tool

   ------------------
   -- Key handlers --
   ------------------

   procedure Bind_Default_Key
     (Kernel         : access Kernel_Handle_Record;
      Action         : String;
      Default_Key    : String);
   --  Associate a default key binding with an action.
   --  Default_Key is ignored if the key was previously overriden by the user.
   --  Its format is something like "control-o" or "control-x control-k", the
   --  second form specifies that it uses a secondary keymap.
   --  Action need not exist when the key is bound. This is why we require
   --  a string instead of an Action_Record.

   ------------
   -- Saving --
   ------------

   type GPS_MDI_Child_Record is new Gtkada.MDI.MDI_Child_Record with private;
   --  Base record for all MDI children that go into the MDI

   function Get_MDI
     (Handle : access Kernel_Handle_Record) return Gtkada.MDI.MDI_Window;
   --  Return the MDI associated with Handle.
   --  Use the Put function below instead of the one in GtkAda.MDI to
   --  associated a widget with a GPS module

   function Put
     (Handle       : access Kernel_Handle_Record;
      Child        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags        : Gtkada.MDI.Child_Flags := Gtkada.MDI.All_Buttons;
      Focus_Widget : Gtk.Widget.Gtk_Widget := null;
      Default_Width, Default_Height : Glib.Gint := -1;
      Module       : access Module_ID_Record'Class;
      Desktop_Independent : Boolean := False) return Gtkada.MDI.MDI_Child;
   --  Recommended version of Put to use, instead of the one in
   --  GtkAda.MDI. This version has several new parameters:
   --    - Module : used to associate a module with a widget. This is used to
   --               get the current context for instance
   --    - Desktop_Independent: if this is true, then the window will not be
   --               closed  when a new desktop is loaded.

   function Get_Module_From_Child
     (Child  : Gtkada.MDI.MDI_Child) return Module_ID;
   --  Return the module that created Child, or null if no module was found.

   function Get_File_Editor
     (Handle : access Kernel_Handle_Record;
      File   : VFS.Virtual_File) return Gtkada.MDI.MDI_Child;
   --  Return the first MDI child associated to an editor for File.
   --  Return null if no such editor was found.

   function Save_MDI_Children
     (Handle   : access Kernel_Handle_Record;
      Children : Gtkada.MDI.MDI_Child_Array := Gtkada.MDI.No_Children;
      Force    : Boolean := False) return Boolean;
   --  Save all the MDI children, as well as the current project
   --  If Force is False, ask the user first.
   --  If Children is specified, only ask to save these specific children.
   --  The return value is False if the user has cancelled the action, True if
   --  the user has selected OK (whatever the number of children that were
   --  saved).

   procedure Close_All_Children (Handle : access Kernel_Handle_Record);
   --  Close all the MDI children. No confirmation is asked, call
   --  Save_All_MDI_Children first if needed.

   ---------------------
   -- Signal emission --
   ---------------------

   procedure Setup (Data : Glib.Object.GObject; Id : Gtk.Handlers.Handler_Id);
   --  Make sure that when Data is destroyed, Id is properly removed

   package Object_User_Callback is new Gtk.Handlers.User_Callback_With_Setup
     (Glib.Object.GObject_Record, Glib.Object.GObject, Setup);
   --  Generic callback that can be used to connect a signal to a kernel.

   package Object_Return_Callback is new Gtk.Handlers.Return_Callback
     (Glib.Object.GObject_Record, Boolean);
   --  Generic callback that can be used to connect a signal to a kernel.

   package Object_User_Return_Callback
     is new Gtk.Handlers.User_Return_Callback_With_Setup
     (Widget_Type => Glib.Object.GObject_Record,
      User_Type   => Glib.Object.GObject,
      Return_Type => Boolean,
      Setup       => Setup);
   --  Generic callback that can be used to connect a signal to a kernel.

   package Kernel_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Kernel_Handle);
   --  Generic callback that can be used to connect a signal to a kernel.

   package Object_Idle is new Gtk.Main.Idle (Glib.Object.GObject);
   --  General Idle loop for a GObject.

   type File_Project_Record is record
      Project : Projects.Project_Type;
      File    : aliased VFS.Virtual_File;
   end record;

   package File_Project_Cb is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, File_Project_Record);
   --  Generic callback that can be used to connect a signal to a kernel.

   -----------
   -- Hooks --
   -----------

   procedure Context_Changed
     (Handle  : access Kernel_Handle_Record;
      Context : access Selection_Context'Class);
   --  Runs the "context_changed" hook

   procedure Source_Lines_Revealed
     (Handle  : access Kernel_Handle_Record;
      Context : access Selection_Context'Class);
   --  Runs the "source_lines_revealed" hook.

   procedure File_Edited
     (Handle : access Kernel_Handle_Record;
      File   : VFS.Virtual_File);
   --  Runs the "file_edited" hook.

   procedure File_Saved
     (Handle : access Kernel_Handle_Record;
      File   : VFS.Virtual_File);
   --  Runs the "file_saved" hook

   procedure File_Closed
     (Handle : access Kernel_Handle_Record;
      File   : VFS.Virtual_File);
   --  Runs the "file_closed" hook

   procedure File_Changed_On_Disk
     (Handle : access Kernel_Handle_Record;
      File   : VFS.Virtual_File);
   --  Runs the "file_changed_on_disk" hook

   procedure Compilation_Finished
     (Handle   : access Kernel_Handle_Record;
      File     : VFS.Virtual_File;
      Category : String);
   --  Runs the "compilation_finished" hook
   --  The Category corresponds to the location/highlighting category that
   --  contains the compilation output.

   --  Hooks with no arguments
   Preferences_Changed_Hook      : constant String := "preferences_changed";
   Search_Reset_Hook             : constant String := "search_reset";
   Search_Functions_Changed_Hook : constant String :=
     "search_functions_changed";
   Search_Regexps_Changed_Hook   : constant String := "search_regexps_changed";
   Variable_Changed_Hook         : constant String := "variable_changed";
   Project_View_Changed_Hook     : constant String := "project_view_changed";
   Project_Changed_Hook          : constant String := "project_changed";

   --  Hooks with File_Hooks_Args argument
   File_Edited_Hook              : constant String := "file_edited";
   File_Saved_Hook               : constant String := "file_saved";
   File_Closed_Hook              : constant String := "file_closed";
   File_Changed_On_Disk_Hook     : constant String := "file_changed_on_disk";
   Compilation_Finished_Hook     : constant String := "compilation_finished";

   --  Hooks with Context_Hooks_Args argument
   Context_Changed_Hook          : constant String := "context_changed";

   --  Hooks with Context_Hooks_Args argument ( a File_Area_Context_Access)
   Source_Lines_Revealed_Hook    : constant String := "source_lines_revealed";

private

   type Filter_Type is (Filter_And, Filter_Or, Standard_Filter);

   type Action_Filter_Record is abstract tagged record
      Error_Msg : GNAT.OS_Lib.String_Access;
      Name      : GNAT.OS_Lib.String_Access;
   end record;

   type Base_Action_Filter_Record (Kind : Filter_Type)
      is new Action_Filter_Record
   with record
      case Kind is
         when Standard_Filter =>
            Language   : GNAT.OS_Lib.String_Access;
            Shell      : GNAT.OS_Lib.String_Access;
            Shell_Lang : GNAT.OS_Lib.String_Access;
            Module     : GNAT.OS_Lib.String_Access;

         when Filter_And =>
            And1, And2 : Action_Filter;

         when Filter_Or =>
            Or1, Or2 : Action_Filter;
      end case;
   end record;

   type Module_ID_Information (Name_Length : Natural) is record
      Name                  : String (1 .. Name_Length);
      Priority              : Module_Priority;
      Contextual_Menu       : Module_Menu_Handler;
      Default_Factory       : Module_Default_Context_Factory;
      Save_Function         : Module_Save_Function;
      Tooltip_Handler       : Module_Tooltip_Handler;
      Customization_Handler : Module_Customization_Handler;
   end record;

   type Module_ID_Information_Access is access Module_ID_Information;

   type Module_ID_Record is tagged record
      Info : Module_ID_Information_Access;
   end record;

   type Selection_Context is tagged record
      Kernel    : Kernel_Handle;
      Creator   : Module_ID;
      Ref_Count : Natural := 1;
   end record;

   type Kernel_Scripting_Data_Record is abstract tagged null record;
   type Kernel_Scripting_Data is access all Kernel_Scripting_Data_Record'Class;
   --  Derived in Glide_Kernel.Scripts to store internal data

   No_Tool : constant Tool_Properties_Record := (null, null, null, null);

   procedure Free (Tool : in out Tool_Properties_Record);
   package Tools_Htable is new String_Hash
     (Tool_Properties_Record, Free, No_Tool);

   ------------------------------------
   -- Abstract type defining a table --
   ------------------------------------

   type Root_Table is abstract tagged null record;
   type Root_Table_Access is access all Root_Table'Class;

   procedure Reset (X : access Root_Table) is abstract;
   --  Reset the table.


   procedure Do_Nothing (Filter : in out Action_Filter);
   --  Do nothing

   package Action_Filters_Htable is new String_Hash
     (Action_Filter, Do_Nothing, null);

   type Action_Filter_Iterator is record
      Iterator : Action_Filters_Htable.String_Hash_Table.Iterator;
   end record;

   type Hook_Function_Record is abstract tagged record
      Ref_Count : Natural := 0;
   end record;

   type Hook_Description_Base is abstract tagged null record;
   type Hook_Description_Base_Access is access all Hook_Description_Base'Class;

   procedure Free (Hook : in out Hook_Description_Base);
   --  Free the memory occupied by Hook

   procedure Free (L : in out Hook_Description_Base_Access);
   package Hooks_Hash is new String_Hash
     (Hook_Description_Base_Access, Free, null);

   type GPS_MDI_Child_Record is new Gtkada.MDI.MDI_Child_Record with record
      Module              : Module_ID;
      Desktop_Independent : Boolean;
   end record;
   type GPS_MDI_Child is access all GPS_MDI_Child_Record'Class;

   type Kernel_Handle_Record is new Glib.Object.GObject_Record with record
      Tools   : Tools_Htable.String_Hash_Table.HTable;
      --  The tools registered in the kernel

      Actions : Root_Table_Access;
      --  The actions registered in the kernel

      Hooks : Hooks_Hash.String_Hash_Table.HTable;
      --  The hooks registered in the kernel

      Action_Filters : Action_Filters_Htable.String_Hash_Table.HTable;
      --  The action contexts registered in the kernel

      Modules_List : Module_List.List;
      --  The list of all the modules that have been registered in this kernel.

      Main_Window : Gtk.Window.Gtk_Window;
      --  The main glide window

      Tooltips : Gtk.Tooltips.Gtk_Tooltips;
      --  The widget used to register all tooltips

      Registry : Projects.Registry.Project_Registry_Access;
      --  The project registry

      Scripts : Kernel_Scripting_Data;
      --  Data used to store information for the scripting languages

      GNAT_Version : GNAT.OS_Lib.String_Access;
      --  Full GNAT Version, if relevant

      Gnatls_Cache : GNAT.OS_Lib.String_Access;
      --  The name of the gnatls command used to get the predefined source
      --  path.

      Preferences : Default_Preferences.Preferences_Manager;
      --  The current setting for the preferences.

      Last_Context_For_Contextual : Selection_Context_Access := null;
      --  The context used in the last contextual menu.

      Current_Context : Selection_Context_Access := null;
      --  The selection for the current MDI child. It is recomputed every time
      --  Get_Current_Context is called, and is kept only for memory
      --  management reasons.

      Home_Dir : GNAT.OS_Lib.String_Access;
      --  The home directory (e.g ~/.glide).

      Logs_Mapper : Basic_Mapper.File_Mapper_Access;
      --  Mapping between files and logs.

      Lang_Handler : Language_Handlers.Language_Handler;
      --  The type used to convert from file names to languages

      Default_Desktop : Glib.Xml_Int.Node_Ptr;
      --  The tree describing the default desktop.

      Open_Files : VFS.File_Array_Access;
      --  The list of currently open files.

      History : Histories.History;
      --  The various histories used throughout GPS

      Tasks : Task_Manager.Task_Manager_Access;
      --  The GPS task manager.

      Custom_Files_Loaded : Boolean := False;
      --  Whether all custom files have already been loaded

      Customization_Strings : Glib.Xml_Int.Node_Ptr;
      --  The customization strings hard-coded by the modules, and they have
      --  been registered before all modules are loaded.
   end record;

end Glide_Kernel;
