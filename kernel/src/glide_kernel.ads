-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

--  This package is the root of the glide's kernel API.

with Basic_Mapper;
with GNAT.OS_Lib;
with Generic_List;
with Gint_Xml;
with Glib.Object;
with Glib.Values;
with Gtk.Handlers;
with Gtk.Accel_Group;
with Gtk.Menu;
with Gtk.Toolbar;
with Gtk.Tooltips;
with Gtk.Widget;
with Gtk.Window;
with Gtkada.MDI;
with Language_Handlers;
with Prj.Tree;
with Prj;
with Prj_API;
with Src_Info;
with Src_Info.Queries;
with System;
with Unchecked_Conversion;

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

   function Get_Default_Accelerators
     (Handle : access Kernel_Handle_Record)
      return Gtk.Accel_Group.Gtk_Accel_Group;
   --  Returns the defauls accelerators group for the main window.

   procedure Initialize_All_Modules (Handle : access Kernel_Handle_Record);
   --  Initialize all the modules that are registered. This should be called
   --  only after the main window and the MDI have been initialized, so that
   --  the modules can add entries in the menus and the MDI.
   --  Only the modules that haven't been initialized yet are processed.

   procedure Save_Desktop
     (Handle : access Kernel_Handle_Record);
   --  Save the current desktop.

   function Load_Desktop (Handle : access Kernel_Handle_Record) return Boolean;
   --  Reload a saved desktop.
   --  Return False if no desktop could be loaded.

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

   --------------
   -- Contexts --
   --------------

   type Module_ID_Information (<>) is private;
   type Module_ID is access Module_ID_Information;
   --  Module identifier. Each of the registered module in Glide has such a
   --  identifier, that contains its name and all the callbacks associated with
   --  the module.

   type Selection_Context is tagged private;
   type Selection_Context_Access is access all Selection_Context'Class;
   --  This type contains all the information about the selection in any
   --  module. Note that this is a tagged type, so that it can easily be
   --  extended for modules external to Glide

   function To_Selection_Context_Access is new Unchecked_Conversion
     (System.Address, Selection_Context_Access);

   procedure Set_Context_Information
     (Context : access Selection_Context;
      Kernel  : access Kernel_Handle_Record'Class;
      Creator : Module_ID);
   --  Set the information in the context

   function Get_Current_Explorer_Context
     (Handle : access Kernel_Handle_Record'Class)
      return Selection_Context_Access;
   --  Return the currently selected project/directory/file in the
   --  explorer. This value is cached, and not computed directly from the
   --  explorer.

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
   --  Callback used every time some contextual menu event happens in Glide.
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

   type Module_Initializer is access procedure
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  General type for the module initialization subprograms.

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

   ---------------------
   -- Signal emission --
   ---------------------

   package Object_User_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Glib.Object.GObject);
   --  Generic callback that can be used to connect a signal to a kernel.

   package Kernel_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Kernel_Handle);
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
   --  </signals>

   Project_Changed_Signal      : constant String := "project_changed";
   Project_View_Changed_Signal : constant String := "project_view_changed";
   Context_Changed_Signal      : constant String := "context_changed";
   Variable_Changed_Signal     : constant String := "variable_changed";

private

   function Get_Predefined_Object_Path
     (Handle : access Kernel_Handle_Record) return String;
   --  Return the predefined Object_Path associated to the given Kernel Handle.
   --  Return the current directory if no object path has been set yet.

   type Module_ID_Information (Name_Length : Natural) is record
      Name            : String (1 .. Name_Length);
      Priority        : Module_Priority;
      Initializer     : Module_Initializer;
      Contextual_Menu : Module_Menu_Handler;
      Mime_Handler    : Module_Mime_Handler;
      Was_Initialized : Boolean := False;
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

      Preferences : Gint_Xml.Node_Ptr;
      --  The XML tree that contains the current preferences

      Scenario_Variables : Prj_API.Project_Node_Array_Access := null;
      --  The (cached) list of scenario variables for the current project. Note
      --  that this list depends on which project was loaded in Glide, and
      --  might change when new dependencies are added.

      Last_Context_For_Contextual : Selection_Context_Access := null;
      --  The context used in the last contextual menu.

      Explorer_Context : Selection_Context_Access := null;
      --  The last context emitted by the explorer.
      --  This implies knowledge of the explorer (at least to check the module
      --  ID, but there is no way around that).

      Home_Dir : GNAT.OS_Lib.String_Access;
      --  The home directory (e.g ~/.glide).

      Search : Gtk.Widget.Gtk_Widget;
      --  The search module

      Logs_Mapper : Basic_Mapper.File_Mapper_Access;
      --  Mapping between files and logs.

      Lang_Handler : Language_Handlers.Language_Handler;
      --  The type used to convert from file names to languages
   end record;

end Glide_Kernel;
