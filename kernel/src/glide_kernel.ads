-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Ada.Tags;
with Basic_Mapper;
with Basic_Types;
with GNAT.OS_Lib;
with Generic_List;
with Glib.Object;
with Glib.Values;
with Glib.Xml_Int;
with Gdk;
with Gtk.Handlers;
with Gtk.Accel_Group;
with Gtk.Menu;
with Gtk.Toolbar;
with Gtk.Tooltips;
with Gtk.Widget;
with Gtk.Window;
with Gtkada.MDI;
with Language_Handlers;
with Project_Hash;
with Prj.Tree;
with Prj;
with Prj_API;
with Src_Info;
with Src_Info.Queries;
with System;
with Ada.Unchecked_Conversion;
with Default_Preferences;

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
     (Handle : access Kernel_Handle_Record);
   --  Save the current desktop.

   function Load_Desktop (Handle : access Kernel_Handle_Record) return Boolean;
   --  Reload a saved desktop.
   --  Return False if no desktop could be loaded (in which case the default
   --  desktop is loaded).

   function Get_MDI
     (Handle : access Kernel_Handle_Record) return Gtkada.MDI.MDI_Window;
   --  Return the MDI associated with Handle

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

   procedure Set_Search_Module
     (Handle : access Kernel_Handle_Record;
      Search : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Register a new search module

   function Get_Search_Module
     (Handle : access Kernel_Handle_Record)
      return Gtk.Widget.Gtk_Widget;
   --  Return the search module.

   function Get_Home_Dir
     (Handle : access Kernel_Handle_Record)
     return String;
   --  Return the Home directory. (eg ~/.glide).

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

   function Get_Predefined_Source_Path
     (Handle : access Kernel_Handle_Record) return String;
   --  Return the predefined Source_Path associated to the given Kernel Handle.
   --  Return the current directory if no source path has been set yet.

   function Get_VCS_List
     (Handle : access Kernel_Handle_Record)
     return Basic_Types.String_Array_Access;
   --  Return the list of recognized VCS systems.

   procedure Register_VCS
     (Handle         : access Kernel_Handle_Record;
      VCS_Identifier : String);
   --  Add a VCS identifier to the list of recognized VCS systems.
   --  ??? This is temporary, until the VCS module can directly add a page in
   --  the wizard or the project properties editor.

   -------------
   -- Queries --
   -------------
   --  The following programs are provided as proxies for the ones in
   --  Src_Info.Queries. They should be used instead of the other ones so that
   --  the list of parsed LI files can be kept in the kernel

   function Locate_From_Source_And_Complete
     (Handle            : access Kernel_Handle_Record;
      Source_Filename   : String)
      return Src_Info.LI_File_Ptr;
   --  Find the ALI file for Source_Filename, and return a handle to it.

   function Get_Other_File_Of
     (Kernel          : access Kernel_Handle_Record;
      Source_Filename : String;
      Full_Name       : Boolean := True) return String;
   --  Return the full path name (or base name if Full_Name is False) to the
   --  other file associated with Source_Filename (the spec if Source_Filename
   --  is a body or separate, the body if Source_Filename is the spec).
   --  The empty string is returned if the file wasn't found (and error
   --  messages are printed to the console appropriately).

   procedure Find_All_References
     (Kernel       : access Kernel_Handle_Record;
      Entity       : Src_Info.Queries.Entity_Information;
      Iterator     : out Src_Info.Queries.Entity_Reference_Iterator;
      Project      : Prj.Project_Id := Prj.No_Project;
      LI_Once      : Boolean := False);
   --  See Src_Info.Queries.
   --  This function needs to be in this package, since it requires access to
   --  the list of LI files.

   procedure Next
     (Kernel   : access Kernel_Handle_Record;
      Iterator : in out Src_Info.Queries.Entity_Reference_Iterator);
   --  See Src_Info.Queries.

   procedure Find_Ancestor_Dependencies
     (Kernel          : access Kernel_Handle_Record;
      Source_Filename : String;
      Iterator        : out Src_Info.Queries.Dependency_Iterator;
      Project         : Prj.Project_Id := Prj.No_Project);
   --  See Src_Info.Queries.
   --  This function needs to be in this package, since it requires access to
   --  the list of LI files.

   procedure Next
     (Kernel   : access Kernel_Handle_Record;
      Iterator : in out Src_Info.Queries.Dependency_Iterator);
   --  See Src_Info.Queries.

   procedure Renaming_Of
     (Kernel         : access Kernel_Handle_Record;
      Entity         : Src_Info.Queries.Entity_Information;
      Is_Renaming    : out Boolean;
      Renamed_Entity : out Src_Info.Queries.Entity_Information);
   --  See Src_Info.Queries.
   --  You must call Destroy on the returned entity.

   procedure Find_Declaration_Or_Overloaded
     (Kernel        : access Kernel_Handle_Record;
      Lib_Info      : Src_Info.LI_File_Ptr;
      File_Name     : String;
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
      Lib_Info    : Src_Info.Li_File_Ptr;
      File_Name   : String;
      Entity_Name : String;
      Line        : Positive;
      Column      : Positive;
      Location    : out Src_Info.File_Location;
      Status      : out Src_Info.Queries.Find_Decl_Or_Body_Query_Status);
   --  See Src_Info.Queries.
   --  If the request to Src_Info.Queries.Find_Next_Body returns an unresolved
   --  overloaded entity, this subprogram automatically asks the user to chose
   --  among the possible completions.

   procedure Parse_All_LI_Information
     (Kernel       : access Kernel_Handle_Record;
      In_Directory : String);
   --  Parse all the LI information in In_Directory, for all the supported
   --  languages. This can be used in cases where there is no obvious way to
   --  find the LI file matching a given source file (for instance, with a
   --  separate krunched file in Ada).

   --------------
   -- Contexts --
   --------------

   type Module_ID_Information (<>) is private;
   type Module_ID_Record is tagged private;
   type Module_ID is access all Module_Id_Record'Class;
   --  Module identifier. Each of the registered module in Glide has such a
   --  identifier, that contains its name and all the callbacks associated with
   --  the module.

   procedure Destroy (Id : in out Module_ID_Record);
   --  Free the memory associated with the module. By default, this does
   --  nothing.

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

   procedure Free (Context : in out Selection_Context_Access);
   --  Free the memory occupied by Context. It automatically calls the
   --  primitive subprogram Destroy as well

   function Get_Current_Context
     (Kernel : access Kernel_Handle_Record) return Selection_Context_Access;
   --  Return the context associated with the current MDI child.
   --  The caller should not free the returned value, this is taken care of by
   --  the kernel automatically.
   --  The returned value might be null, if the current child doesn't support
   --  selection contexts.
   --  This function is mostly intended to be called for the callbacks in the
   --  menu bar.

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

   type Save_Return_Value is
     (Saved, Not_Saved, Save_All, Cancel);
   --  All possible return values from the module's save function.
   --    - Saved: The child associated with the module was saved
   --    - Not_Saved: the child associated with the mdoule was not saved.
   --    - Save_All: The child was saved, as should all following children.
   --    - Cancel: Abort the operation that requested the save.

   type Module_Save_Function is access function
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget;
      Force  : Boolean := False) return Save_Return_Value;
   --  A function called when the kernel asks a MDI child to save itself.
   --  If Force is True, this function should save the child state
   --  automatically, and if Force is False, the widgets may decide to
   --  ask the user first.
   --  If the user has refused to save the widget, return False,
   --  otherwise return True.
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

   ------------
   -- Saving --
   ------------

   function Get_Module_From_Child
     (Handle : access Kernel_Handle_Record;
      Child  : Gtkada.MDI.MDI_Child) return Module_ID;
   --  Return the module that created Child, or null if no module was found.

   function Get_File_Editor
     (Handle : access Kernel_Handle_Record;
      File   : String) return Gtkada.MDI.MDI_Child;
   --  Return the first MDI child associated to an editor for File.
   --  Return null if no such editor was found.

   function Save_Child
     (Handle : access Kernel_Handle_Record;
      Child  : Gtkada.MDI.MDI_Child;
      Force  : Boolean := True) return Save_Return_Value;
   --  Save the given Child.
   --  If Force is False, then ask the user first.
   --  Return False if the action has been canceled.

   function Save_All_MDI_Children
     (Handle : access Kernel_Handle_Record;
      Force  : Boolean := False) return Boolean;
   --  Save all the MDI children, as well as the current project
   --  If Force is False, ask the user first.
   --  If at any time the user answers "no", the function stops asking
   --  the children and returns False.
   --  Return True otherwise.

   function Save_All_Editors
     (Handle : access Kernel_Handle_Record;
      Force  : Boolean) return Boolean;
   --  Save all open file editors.
   --  If Force is False, ask the user first.
   --  If at any time the user answers "no", the function stops asking
   --  the children and returns False.
   --  Return True otherwise.

   ---------------------
   -- Signal emission --
   ---------------------

   package Object_User_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Glib.Object.GObject);
   --  Generic callback that can be used to connect a signal to a kernel.

   package Object_Return_Callback is new Gtk.Handlers.Return_Callback
     (Glib.Object.GObject_Record, Boolean);
   --  Generic callback that can be used to connect a signal to a kernel.

   package Object_User_Return_Callback is new Gtk.Handlers.User_Return_Callback
     (Widget_Type => Glib.Object.GObject_Record,
      User_Type   => Glib.Object.GObject,
      Return_Type => Boolean);
   --  Generic callback that can be used to connect a signal to a kernel.

   package Kernel_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Kernel_Handle);
   --  Generic callback that can be used to connect a signal to a kernel.

   type File_Project_Record (Length : Natural) is record
      Project : Prj.Project_Id;
      File    : String (1 .. Length);
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
      File    : String);
   --  Emits the "file_edited" signal.

   procedure Preferences_Changed (Handle : access Kernel_Handle_Record);
   --  Emits the "preferences_changed" signal.

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
   --  - "file_edited"
   --    procedure Handler (Handle : access Kernel_Handle_Record'Class;
   --                       File   : String);
   --
   --  </signals>

   Project_Changed_Signal       : constant String := "project_changed";
   Project_View_Changed_Signal  : constant String := "project_view_changed";
   Context_Changed_Signal       : constant String := "context_changed";
   Variable_Changed_Signal      : constant String := "variable_changed";
   Source_Lines_Revealed_Signal : constant String := "source_lines_revealed";
   File_Edited_Signal           : constant String := "file_edited";
   Preferences_Changed_Signal   : constant String := "preferences_changed";

private

   function Get_Predefined_Object_Path
     (Handle : access Kernel_Handle_Record) return String;
   --  Return the predefined Object_Path associated to the given Kernel Handle.
   --  Return the current directory if no object path has been set yet.

   type Module_ID_Information (Name_Length : Natural) is record
      Name            : String (1 .. Name_Length);
      Priority        : Module_Priority;
      Contextual_Menu : Module_Menu_Handler;
      Mime_Handler    : Module_Mime_Handler;
      Default_Factory : Module_Default_Context_Factory;
      Save_Function   : Module_Save_Function;
      Child_Tag       : Ada.Tags.Tag;
      Tooltip_Handler : Module_Tooltip_Handler;
   end record;

   type Module_ID_Information_Access is access Module_ID_Information;

   type Module_ID_Record is tagged record
      Info : Module_ID_Information_Access;
   end record;

   type Selection_Context is tagged record
      Kernel  : Kernel_Handle;
      Creator : Module_ID;
   end record;

   procedure Free (Module : in out Module_ID);
   --  Free memory associated to a Module_ID.

   package Module_List is new Generic_List (Module_ID);

   type Kernel_Handle_Record is new Glib.Object.GObject_Record with record
      Modules_List : Module_List.List;
      --  The list of all the modules that have been registered in this kernel.

      Project : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node;
      --  The project currently loaded. This is the tree form, independent of
      --  the current value of the environment variables.

      Project_Is_Default : Boolean;
      --  True when the current project is still the default project. This is
      --  set to False as soon as the user loads a new project

      Project_View : Prj.Project_Id := Prj.No_Project;
      --  The current project view. This is the same Project, after it has been
      --  evaluated based on the current value of the environment variables.

      Projects_Data : Project_Hash.Project_Htable.Htable;
      --  Information stored about each loaded project (and the imported
      --  projects).
      --  ??? This wouldn't be necessary if we could save user data in
      --  Project_Node_Record.

      Main_Window : Gtk.Window.Gtk_Window;
      --  The main glide window

      Tooltips : Gtk.Tooltips.Gtk_Tooltips;
      --  The widget used to register all tooltips

      Predefined_Source_Path : GNAT.OS_Lib.String_Access;
      --  The path of the sources used to compile the project which are not
      --  directly part of the project (eg the Ada run-time files).

      Predefined_Object_Path : GNAT.OS_Lib.String_Access;
      --  The path for the object files associated the sources in the
      --  Source Path above.

      Gnatls_Cache : GNAT.OS_Lib.String_Access;
      --  The name of the gnatls command used to get the predefined source
      --  path.

      Source_Info_List : Src_Info.LI_File_List;
      --  The semantic information associated to the files for the current
      --  project.

      Preferences : Default_Preferences.Preferences_Manager;
      --  The current setting for the preferences.

      Scenario_Variables : Prj_API.Project_Node_Array_Access := null;
      --  The (cached) list of scenario variables for the current project. Note
      --  that this list depends on which project was loaded in Glide, and
      --  might change when new dependencies are added.

      Last_Context_For_Contextual : Selection_Context_Access := null;
      --  The context used in the last contextual menu.

      Current_Context : Selection_Context_Access := null;
      --  The selection for the current MDI child. It is recomputed every time
      --  Get_Current_Context is called, and is kept only for memory
      --  management reasons.

      Home_Dir : GNAT.OS_Lib.String_Access;
      --  The home directory (e.g ~/.glide).

      Search : Gtk.Widget.Gtk_Widget;
      --  The search module

      Logs_Mapper : Basic_Mapper.File_Mapper_Access;
      --  Mapping between files and logs.

      Lang_Handler : Language_Handlers.Language_Handler;
      --  The type used to convert from file names to languages

      VCS_List : Basic_Types.String_Array_Access;
      --  The list of all VCS systems recognized by the kernel.

      Default_Desktop : Glib.Xml_Int.Node_Ptr;
      --  The tree describing the default desktop.
   end record;

end Glide_Kernel;
