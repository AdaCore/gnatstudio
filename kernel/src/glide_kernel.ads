-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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
with Glib.Values;
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
with Commands.Interactive;
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
     (Name : Glib.UTF8_String;
      Kernel : access Kernel_Handle_Record;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True) return VFS.Virtual_File;
   --  Create a new file. This will automatically try to solve Name to an
   --  absolute path if it currently is a base name.
   --
   --  If Name contains a relative path, the editor will open it as is. It
   --  thus depends on the current directory, and should only be used for files
   --  opened from the command line. As a result, Name might be found even
   --  if it doesn't directly belong to a project.

   function Create_Html
     (Name   : Glib.UTF8_String;
      Kernel : access Kernel_Handle_Record) return VFS.Virtual_File;
   --  Filename can be a full name or a base name, and can include ancors (e.g
   --  "foo.html#anchor").

   -------------
   -- Queries --
   -------------
   --  The following programs are provided as proxies for the ones in
   --  Src_Info.Queries. They should be used instead of the other ones so that
   --  the list of parsed LI files can be kept in the kernel

   function Get_LI_File_List (Handle : access Kernel_Handle_Record)
      return Src_Info.LI_File_List;
   --  Return the list of all LI file parsed so far.

   function Locate_From_Source_And_Complete
     (Handle          : access Kernel_Handle_Record;
      Source_Filename : VFS.Virtual_File) return Src_Info.LI_File_Ptr;
   --  Find the ALI file for Source_Filename, and return a handle to it.
   --  null is returned if the LI file couldn't be parsed. It is guaranteed
   --  that the returned LI file has been fully parsed.

   function Other_File_Name
     (Kernel          : access Kernel_Handle_Record;
      Source_Filename : VFS.Virtual_File) return VFS.Virtual_File;
   --  Return the other file associated with Source_Filename (the spec if
   --  Source_Filename is a body or separate, the body if Source_Filename is
   --  the spec).
   --  The empty string is returned if the file wasn't found (and error
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
      Tree   : out Src_Info.Queries.Scope_Tree;
      Node   : out Src_Info.Queries.Scope_Tree_Node;
      Declarations_Only : Boolean := False);
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

   type Mime_Mode is (Read_Only, Read_Write);
   --  How the data should be opened. In some cases, it might happen that some
   --  module is the best to display data read-only, but cannot handle the
   --  edition (think of a web browser for instance).

   type GValue_Array is array (Natural range <>) of Glib.Values.GValue;

   type Module_Mime_Handler is access function
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean;
   --  A function that can be registered by the module if it wants to process
   --  MIME types.
   --  If the module knows how to process some data of type Mime_Type, then it
   --  should act on Data, and return True.
   --  Otherwise, if it doesn't know how to act on Mime_Type, it should return
   --  False. In that case, the next module will be queried.

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
     (Hard_Coded, System_Wide, Project_Wide, User_Specific);
   --  The various level of customization (See Glide_Kernel.Custom).
   --  Hard_Coded is used for customization that are hard-coded in the GPS code
   --  System_Wide is used if customization comes from a custom file found in
   --  the installation directory of GPS.
   --  Project_Wide is used if the customization comes from a custom file found
   --  in one of the directories lists in GPS_CUSTOM_PATH.
   --  User_Specific is used if the customization comes from a custom file
   --  found in the user's own directory (see GPS_HOME/.gps/customize).

   type Module_Customization_Handler is access procedure
     (Kernel : access Kernel_Handle_Record'Class;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level);
   --  Subprogram called when a new customization has been parsed.
   --  It is initially called just after all modules have been registered,
   --  and gets passed the first child of the XML file (that is you must go
   --  through the list, following the Next nodes).

   ---------------------
   -- Action contexts --
   ---------------------
   --  These contexts describe when an action (see below) can take place.

   type Action_Context_Record is abstract tagged null record;
   type Action_Context is access all Action_Context_Record'Class;

   function Get_Name
     (Context : access Action_Context_Record) return String is abstract;
   --  Return the description of the context (a short string suitable for
   --  display in the key manager GUI

   function Context_Matches
     (Context : access Action_Context_Record;
      Kernel  : access Kernel_Handle_Record'Class)
     return Boolean is abstract;
   --  Whether the current widget in Event matches the context

   procedure Register_Context
     (Kernel  : access Kernel_Handle_Record;
      Context : access Action_Context_Record'Class);
   --  Record the context in the kernel, so that it is available through the
   --  key manager GUI.

   function Lookup_Context
     (Kernel : access Kernel_Handle_Record;
      Name   : String) return Action_Context;
   --  Lookup a context by name. Return null if no such action has been
   --  registered.

   type Action_Context_Iterator is private;

   function Start (Kernel : access Kernel_Handle_Record'Class)
      return Action_Context_Iterator;
   --  Return the first context registered in the kernel (this is in no
   --  particular order).

   procedure Next
     (Kernel : access Kernel_Handle_Record'Class;
      Iter   : in out Action_Context_Iterator);
   --  Move to the next action

   function Get (Iter : Action_Context_Iterator) return Action_Context;
   --  Return the current context

   --------------
   -- Actions --
   -------------
   --  Actions are named commands (or list of commands) in GPS. These can
   --  be associated with menus, keys and toolbar buttons among other things.

   type Action_Record is record
      Command     : Commands.Interactive.Interactive_Command_Access;
      Context     : Action_Context;
      Description : GNAT.OS_Lib.String_Access;
   end record;
   No_Action : constant Action_Record := (null, null, null);
   --  Command is freed automatically by the kernel.
   --  Context indicates when the action can be executed. If null, this means
   --  the action can always be executed. The context mustn't deallocated
   --  in the life of GPS, since there might be actions bound to it at any
   --  time.

   procedure Register_Action
     (Kernel      : access Kernel_Handle_Record;
      Name        : String;
      Command     : access Commands.Interactive.Interactive_Command'Class;
      Description : String := "";
      Context     : Action_Context := null);
   --  Register a new named action in GPS.
   --  Only the actions that can be executed interactively by the user
   --  should be registered.
   --  Name must be unique in GPS.
   --  Action will be freed automatically by the kernel.

   function Lookup_Action
     (Kernel : access Kernel_Handle_Record;
      Name   : String) return Action_Record;
   --  Lookup a command by name. Return No_Action if no such action has been
   --  registered.

   type Action_Iterator is private;

   function Start (Kernel : access Kernel_Handle_Record'Class)
      return Action_Iterator;
   --  Return the first action registered in the kernel (this is in no
   --  particular order).

   procedure Next
     (Kernel : access Kernel_Handle_Record'Class;
      Iter   : in out Action_Iterator);
   --  Move to the next action

   function Get (Iter : Action_Iterator) return String;
   function Get (Iter : Action_Iterator) return Action_Record;
   --  Return the current action. The empty string or No_Action is returned if
   --  there are no more actions.

   ------------
   --  Tools --
   ------------
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
      Default_Switches  : GNAT.OS_Lib.String_Access;
   end record;
   --  (Project_Package, Project_Attribute, Project_Index) describe where its
   --  switches are stored in a project.
   --  Default_Switches are the switches when the user hasn't edited them
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
     (Handle : access Kernel_Handle_Record;
      Children : Gtkada.MDI.MDI_Child_Array := Gtkada.MDI.No_Children;
      Force  : Boolean := False) return Boolean;
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
      File    : VFS.Virtual_File;
   end record;

   package File_Project_Cb is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, File_Project_Record);
   --  Generic callback that can be used to connect a signal to a kernel.

   procedure Project_Changed (Handle : access Kernel_Handle_Record);
   --  Emits the "project_changed" signal

   procedure Project_View_Changed (Handle : access Kernel_Handle_Record);
   --  Emits the "project_view_changed" signal

   procedure Context_Changed
     (Handle  : access Kernel_Handle_Record;
      Context : access Selection_Context'Class);
   --  Emits the "context_changed" signal

   procedure Variable_Changed (Handle : access Kernel_Handle_Record);
   --  Emits the "variable_changed" signal

   procedure Source_Lines_Revealed
     (Handle      : access Kernel_Handle_Record;
      Context     : access Selection_Context'Class);
   --  Emits the "source_lines_revealed" signal.

   procedure File_Edited
     (Handle  : access Kernel_Handle_Record;
      File    : VFS.Virtual_File);
   --  Emits the "file_edited" signal.

   procedure File_Saved
     (Handle  : access Kernel_Handle_Record;
      File    : VFS.Virtual_File);
   --  Emits the "file_saved" signal

   procedure File_Closed
     (Handle  : access Kernel_Handle_Record;
      File    : VFS.Virtual_File);
   --  Emits the "file_closed" signal

   procedure File_Changed_On_Disk
     (Handle  : access Kernel_Handle_Record;
      File    : VFS.Virtual_File);
   --  Emits the "file_changed_on_disk" signal

   procedure Compilation_Finished
     (Handle  : access Kernel_Handle_Record;
      File    : VFS.Virtual_File);
   --  Emits the "compilation_finished" signal

   procedure Preferences_Changed (Handle : access Kernel_Handle_Record);
   --  Emits the "preferences_changed" signal.

   procedure Search_Regexps_Changed (Handle : access Kernel_Handle_Record);
   --  Emits the "search_regexps_changed" signal

   procedure Search_Reset (Handle : access Kernel_Handle_Record);
   --  Emits the "search_reset" signal

   procedure Search_Functions_Changed (Handle : access Kernel_Handle_Record);
   --  Emits the "search_functions_changed" signal

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "project_changed"
   --    procedure Handler (Handle : access Kernel_Handle_Record'Class);
   --
   --    Emitted when the project has changed. This means that a new project
   --    has been loaded in Glide, and that all the previous settings and
   --    caches are now obsolete.
   --    Note: when this signal is emitted, the project view hasn't necessarily
   --    been created yet.
   --
   --  - "project_view_changed"
   --    procedure Handler (Handle : access Kernel_Handle_Record'Class);
   --
   --    Emitted when the project view has been changed (for instance because
   --    one of the environment variables has changed). This means that the
   --    list of directories, files or switches might now be different).
   --
   --  - "context_changed"
   --    procedure Handler (Handle  : access Kernel_Handle_Record'Class;
   --                       Context : Selection_Context_Access);
   --
   --    Emitted when a context has changed, like a new file/directory/project
   --    selection.
   --
   --  - "variable_changed"
   --    procedure Handler (Handle : access Kernel_Handle_Record'Class);
   --
   --    Emitted when one of the scenario variables has been renamed, removed,
   --    or when one of its possible values has changed.
   --
   --  - "preferences_changed"
   --    procedure Handler (Handle : access Kernel_Handle_Record'Class);
   --
   --    Emitted when the preferences have been changed. When possible, the
   --    widgets should refresh themselves with the new preferences
   --
   --  - "source_lines_revealed"
   --    procedure Handler (Handle     : access Kernel_Handle_Record'Class;
   --                       File       : String;
   --                       Start_Line : Natural;
   --                       End_Line   : Natural);
   --
   --    Indicates that lines between Start_Line and End_Line from File
   --    have been displayed on screen.
   --
   --  - "file_edited"
   --    procedure Handler (Handle : access Kernel_Handle_Record'Class;
   --                       File   : String);
   --
   --    Emitted when a file editor has been opened for a file that wasn't
   --    already open before.
   --
   --  - "file_closed"
   --    procedure Handler (Handle : access Kernel_Handle_Record'Class;
   --                       File   : Virtual_File);
   --
   --    Emitted when the last editor for File has been closed.
   --
   --  - "file_changed_on_disk"
   --    procedure Handler (Handle : access Kernel_Handle_Record'Class;
   --                       File   : Virtual_File);
   --    Emitted when an external action has changed the contents of a
   --    file on the disk, such as VCS operations for example.
   --    File can be the absolute name of a file, or a directory, ending
   --    with a directory_separator. In that case, the meaning of this signal
   --    is that any file in that directory might have been modified.
   --
   --  - "search_regexps_changed"
   --    procedure Handler (Handle : access Kernel_Handle_Record'Class);
   --
   --    Emitted when a new regexp has been added to the list of predefined
   --    search patterns.
   --
   --  - "search_reset"
   --    procedure Handler (Handle : access Kernel_Handle_Record'Class);
   --
   --    Emitted when the current search pattern has been reset or changed by
   --    the user, or when the current search is no longer possible because the
   --    setup of GPS has changed.
   --
   --  - "search_functions_changed"
   --    procedure Handler (Handle : access Kernel_Handle_Record'Class);
   --
   --    Emitted when the list of registered search functions has changed.
   --
   --  - "compilation_finished"
   --    procedure Handler (Handle : access Kernel_Handle_Record'Class;
   --                       File   : Virtual_File);
   --    Emitted when a compile operation has finished.
   --    File indicates the file that has just been compiled, in the case
   --    when only one file is compiled.
   --
   --  </signals>

   Project_Changed_Signal        : constant String := "project_changed";
   Project_View_Changed_Signal   : constant String := "project_view_changed";
   Context_Changed_Signal        : constant String := "context_changed";
   Variable_Changed_Signal       : constant String := "variable_changed";
   Source_Lines_Revealed_Signal  : constant String := "source_lines_revealed";
   File_Edited_Signal            : constant String := "file_edited";
   File_Saved_Signal             : constant String := "file_saved";
   File_Closed_Signal            : constant String := "file_closed";
   File_Changed_On_Disk_Signal   : constant String := "file_changed_on_disk";
   Compilation_Finished_Signal   : constant String := "compilation_finished";
   Preferences_Changed_Signal    : constant String := "preferences_changed";
   Search_Regexps_Changed_Signal : constant String := "search_regexps_changed";
   Search_Reset_Signal           : constant String := "search_reset";
   Search_Functions_Changed_Signal : constant String :=
     "search_functions_changed";

private

   type Module_ID_Information (Name_Length : Natural) is record
      Name                  : String (1 .. Name_Length);
      Priority              : Module_Priority;
      Contextual_Menu       : Module_Menu_Handler;
      Mime_Handler          : Module_Mime_Handler;
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
      Kernel  : Kernel_Handle;
      Creator : Module_ID;
      Ref_Count : Natural := 1;
   end record;

   type Kernel_Scripting_Data_Record is abstract tagged null record;
   type Kernel_Scripting_Data is access all Kernel_Scripting_Data_Record'Class;
   --  Derived in Glide_Kernel.Scripts to store internal data

   No_Tool : constant Tool_Properties_Record := (null, null, null, null);

   procedure Free (Tool : in out Tool_Properties_Record);
   package Tools_Htable is new String_Hash
     (Tool_Properties_Record, Free, No_Tool);

   procedure Free (Action : in out Action_Record);
   --  Free the memory occupied by the action

   package Actions_Htable is new String_Hash
     (Action_Record, Free, No_Action);

   procedure Do_Nothing (Context : in out Action_Context);
   --  Do nothing

   package Action_Contexts_Htable is new String_Hash
     (Action_Context, Do_Nothing, null);

   type Action_Context_Iterator is record
      Iterator : Action_Contexts_Htable.String_Hash_Table.Iterator;
   end record;

   type Action_Iterator is record
      Iterator : Actions_Htable.String_Hash_Table.Iterator;
   end record;

   type GPS_MDI_Child_Record is new Gtkada.MDI.MDI_Child_Record with record
      Module              : Module_ID;
      Desktop_Independent : Boolean;
   end record;
   type GPS_MDI_Child is access all GPS_MDI_Child_Record'Class;

   type Kernel_Handle_Record is new Glib.Object.GObject_Record with record
      Tools   : Tools_Htable.String_Hash_Table.HTable;
      --  The tools registered in the kernel

      Actions : Actions_Htable.String_Hash_Table.HTable;
      --  The actions registered in the kernel

      Action_Contexts : Action_Contexts_Htable.String_Hash_Table.HTable;
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

      Source_Info_List : Src_Info.LI_File_List;
      --  The semantic information associated with the files for the current
      --  project.

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
