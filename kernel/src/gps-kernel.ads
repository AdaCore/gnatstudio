------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

--  This package is the root of the GPS' kernel API

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with System;

with GNAT.Strings;
with GNATCOLL.Projects;
with GNAT.Regpat;
with GNATCOLL.Refcount;
with GNATCOLL.Scripts;
with GNATCOLL.Traces;
with GNATCOLL.Tribooleans;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;
with GNATCOLL.Xref;                   use GNATCOLL.Xref;

with Glib.Main;
with Glib;                            use Glib;
with Glib.Object;
with Gdk.Types;
with Gtk.Application;                 use Gtk.Application;
with Gtk.Dialog;
with Gtk.Widget;
with Gtk.Window;
with Gtkada.Canvas_View;

with Basic_Types;
with Basic_Mapper;
with Commands;
with Language_Handlers;
with Projects;
with Default_Preferences;
with Histories;
with Refactoring;
with Switches_Chooser;
with String_List_Utils;
with Task_Manager;
with XML_Utils;
with Xref;

with GPS.Editors;
with GPS.Environments;                use GPS.Environments;
with GPS.Core_Kernels;                use GPS.Core_Kernels;
with GPS.Markers;                     use GPS.Markers;
limited with GPS.Kernel.Messages;
with GPS.Messages_Windows;
with GPS.Process_Launchers;
with GPS.Process_Launchers.Implementation;
use GPS.Process_Launchers.Implementation;
with GPS.Scripts;                     use GPS.Scripts;
with GPS.VCS;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;

package GPS.Kernel is

   Testsuite_Handle : constant GNATCOLL.Traces.Trace_Handle :=
      GNATCOLL.Traces.Create ("TESTSUITE", Default => GNATCOLL.Traces.Off);

   type Kernel_Handle_Record is
     new GPS.Core_Kernels.Core_Kernel_Record with private;

   type Kernel_Handle is access all Kernel_Handle_Record'Class;
   pragma No_Strict_Aliasing (Kernel_Handle);
   --  A kernel handle used to share information throughout GPS

   -------------------
   -- Kernel_Handle --
   -------------------

   procedure Gtk_New
     (Handle           : out Kernel_Handle;
      Application      : not null access Gtk_Application_Record'Class;
      Home_Dir         : Virtual_File;
      Prefix_Directory : Virtual_File);
   --  Create a new GPS kernel.
   --  By default, it isn't associated with any project, nor any source editor.
   --  Home_Dir is the directory under which config files can be loaded/saved.

   procedure Set_Main_Window
     (Self : not null access Kernel_Handle_Record;
      Win  : not null access Gtk.Window.Gtk_Window_Record'Class);
   --  Set the main window for GPS. Id is the result of calling
   --  Gtk.Application_Window.Get_Id.

   procedure Load_Preferences (Handle : access Kernel_Handle_Record);
   --  Load the preferences from the user's file ~/.gps/preferences

   procedure Destroy (Handle : access Kernel_Handle_Record);
   --  Free the memory occupied by the kernel

   function Get_Preferences
     (Handle : access Kernel_Handle_Record)
      return Default_Preferences.Preferences_Manager;
   --  Return the preference manager associated with Handle

   function Preferences_File
     (Self : access Kernel_Handle_Record)
      return GNATCOLL.VFS.Virtual_File;
   --  Return the name of the preferences file.
   --  This is the file that GPS will modify when the user modifies the
   --  preferences. But the default value for preferences could be set from any
   --  plugin.

   function Get_Application
      (Self : not null access Kernel_Handle_Record'Class)
      return not null access Gtk_Application_Record'Class;
   --  Return the handle on the application (i.e. the non-graphical part of
   --  the process).

   function Get_Main_Window
     (Handle : access Kernel_Handle_Record) return Gtk.Window.Gtk_Window;
   --  Return the main window associated with the kernel

   function Get_History
     (Handle : access Kernel_Handle_Record) return Histories.History;
   --  Return the history database

   procedure Set_VCS
      (Self : not null access Kernel_Handle_Record;
       Repo : not null access GPS.VCS.Abstract_VCS_Repository'Class);
   function VCS
      (Self : not null access Kernel_Handle_Record)
      return access GPS.VCS.Abstract_VCS_Repository'Class;
   --  The VCS system

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
   --  Possible states of the kernel

   subtype Action_Kernel_State is Kernel_State range Processing .. Busy;

   function Get_Home_Dir
     (Handle : access Kernel_Handle_Record)
     return Virtual_File;
   --  Return the Home directory. (eg ~/.gps/).
   --  The directory ends with a directory separator

   function Get_System_Dir
     (Handle : access Kernel_Handle_Record)
     return Virtual_File;
   --  Return the installation directory for GPS. This always ends up with a
   --  directory separator.

   overriding function Get_Share_Dir
     (Self : not null access Kernel_Handle_Record)
     return GNATCOLL.VFS.Virtual_File;
   --  Return share/gps/ in Get_System_Dir directory. This always ends up with
   --  a directory separator.

   function Get_Logs_Mapper
     (Handle : access Kernel_Handle_Record)
      return Basic_Mapper.File_Mapper_Access;
   --  Return the mapper for file logs

   procedure Set_Logs_Mapper
     (Handle : access Kernel_Handle_Record;
      Mapper : Basic_Mapper.File_Mapper_Access);
   --  Set the mapper for file logs

   function Get_Language_Handler
     (Handle : access Kernel_Handle_Record)
      return Language_Handlers.Language_Handler;
   --  Return the language handler used by this kernel

   function GNAT_Version
     (Handle : access Kernel_Handle_Record) return String;
   --  Return a string containing the GNAT version number.
   --  The string has the form "Pro 6.4.0w (20100727-43)"
   --  See also Require_GNAT_Date below.

   function Require_GNAT_Date
     (Handle : access Kernel_Handle_Record;
      Date   : Basic_Types.Date_Type) return Boolean;
   --  Return True if the version of GNAT associated with Handle is at
   --  least Date.

   procedure Set_Destruction_Flag
     (Handle : access Kernel_Handle_Record;
      Flag   : Boolean);
   --  Set the destruction flag in the kernel

   function Is_In_Destruction
     (Handle : access Kernel_Handle_Record) return Boolean;
   --  When return True, the kernel is in the process of being destroyed

   procedure Report_Preference_File_Error
     (Handle   : access Kernel_Handle_Record;
      Filename : Virtual_File);
   --  Print out an error message in messages window, or display a dialog
   --  if GPS is exiting.

   function Get_Contextual_Menu_Open
     (Handle : access Kernel_Handle_Record) return Boolean;

   function Get_Environment
     (Self : access Kernel_Handle_Record) return Environment;
   --  Return list of environment variables overwritten by GPS
   procedure Set_Environment
     (Self  : access Kernel_Handle_Record;
      Value : Environment);
   --  Assign list of environment variables overwritten by GPS

   ------------------
   -- Key managing --
   ------------------

   type Key_Setter is access procedure
     (Kernel     : access Kernel_Handle_Record'Class;
      Action     : String;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type);
   type Key_Getter is access function
     (Kernel          : access Kernel_Handle_Record'Class;
      Action          : String;
      Use_Markup      : Boolean := True;
      Return_Multiple : Boolean := True) return String;
   type Key_Getter_Simple is access procedure
     (Kernel     : access Kernel_Handle_Record'Class;
      Action     : String;
      Key        : out Gdk.Types.Gdk_Key_Type;
      Mods       : out Gdk.Types.Gdk_Modifier_Type);

   procedure Set_Key_Setter
     (Kernel        : access Kernel_Handle_Record;
      Setter        : Key_Setter;
      Getter        : Key_Getter;
      Getter_Simple : Key_Getter_Simple);
   --  Register a key setter. This function should be called by key manager
   --  modules.

   function Get_Shortcut
     (Kernel          : access Kernel_Handle_Record'Class;
      Action          : String;
      Use_Markup      : Boolean := True;
      Return_Multiple : Boolean := True) return String;
   --  Return the key shortcut(s) for the given action.
   --  The return value is suitable for display, but cannot be used as a key
   --  binding (since it might includes special system-specific symbols).

   procedure Get_Shortcut_Simple
     (Kernel     : access Kernel_Handle_Record'Class;
      Action     : String;
      Key        : out Gdk.Types.Gdk_Key_Type;
      Mods       : out Gdk.Types.Gdk_Modifier_Type);
   --  If the action has a simple keybinding associated with it, return it.
   --  Otherwise, set Key to 0 to indicate there is no simple shortcut.

   procedure Set_Default_Key
     (Kernel     : access Kernel_Handle_Record'Class;
      Action     : String;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type);
   --  Set a default key for the registered action.

   -----------
   -- Files --
   -----------
   --  The following subprograms are provided in addition to the ones provided
   --  in vfs.ads.

   pragma Suppress (Container_Checks);
   package File_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type        => GNATCOLL.VFS.Virtual_File,
       Hash                => GNATCOLL.VFS.Full_Name_Hash,
       Equivalent_Elements => GNATCOLL.VFS."=");

   function Create
     (Name            : Filesystem_String;
      Kernel          : access Kernel_Handle_Record;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True) return GNATCOLL.VFS.Virtual_File;
   --  Create a new file. This will automatically try to solve Name to an
   --  absolute path if it currently is a base name.
   --
   --  If Name contains a relative path, the editor will open it as is. It
   --  thus depends on the current directory, and should only be used for files
   --  opened from the command line. As a result, Name might be found even
   --  if it doesn't directly belong to a project.

   function Is_Open
     (Kernel   : access Kernel_Handle_Record;
      Filename : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Whether Filename is currently opened in an editor

   function Open_Files
     (Kernel : access Kernel_Handle_Record) return access File_Sets.Set;
   --  Return a list of currently open files

   function Is_Hidden
     (Kernel    : access Kernel_Handle_Record;
      File      : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Whether the file or directory should considered hidden in the GUI, for
   --  instance in the Projects and Files view.

   procedure Make_File_Writable
     (Kernel   : not null access Kernel_Handle_Record;
      File     : GNATCOLL.VFS.Virtual_File;
      Writable : Boolean := True);
   --  Make the file writable on the disk.
   --  This changes the disk permissions by using the appropriate VCS operation

   --------------
   -- Contexts --
   --------------

   type Selection_Context is private;
   No_Context : constant Selection_Context;
   --  This type contains all the information about the selection in any
   --  module.

   function New_Context
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Creator : access Abstract_Module_Record'Class := null)
     return Selection_Context;
   --  Creates a new context, with basic information set.
   --  In general, this function should only be called from a GPS_MDI_Child's
   --  Build_Context primitive.

   function Get_Kernel (Context : Selection_Context) return Kernel_Handle;
   --  Return the kernel associated with the context

   function Get_Creator
     (Context : Selection_Context) return Abstract_Module;
   --  Return the module ID for the module that created the context

   procedure Context_Changed
     (Handle  : access Kernel_Handle_Record;
      Context : Selection_Context);
   --  Runs the "context_changed" hook.
   --  Context is the current context at that point, and will be returned by
   --  Get_Current_Context until the next call to Context_Changed.

   function Get_Current_Context
     (Kernel : access Kernel_Handle_Record'Class) return Selection_Context;
   --  Returns the context last set by Context_Changed.
   --  The context returned will be that of the active contextual menu if there
   --  is one at that point in time (therefore, we ignore cases where for
   --  instance a new child has been selected automatically at that point)

   procedure Refresh_Context
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Force a refresh of the current context, based on which window currently
   --  has the focus. This also forces a "context_changed" hook.
   --  This function should be used rarely, since in theory the views are
   --  supposed to update the context when their selection changes.

   -------------
   -- Markers --
   -------------
   --  The following subprograms provide the required support for representing
   --  and storing locations in a list, so that the user can move back and
   --  forward from places where he was before.
   --  Location in this sense is to be taken as a broad term, since it might
   --  represent a location in a source editor, but also a location within
   --  a browser,...

   procedure Push_Marker_In_History
     (Kernel : access Kernel_Handle_Record'Class;
      Marker : Location_Marker);
   --  Push a new marker in the list of previous locations the user has
   --  visited. This is the basic interface for the handling of the history of
   --  locations. It emits the hook Marker_Added_To_History.

   --------------------
   -- Action filters --
   --------------------

   type Action_Filter_Record is abstract tagged private;
   type Action_Filter is access all Action_Filter_Record'Class;

   function Filter_Matches_Primitive
     (Filter  : access Action_Filter_Record;
      Context : Selection_Context) return Boolean is abstract;
   --  Whether the context matches Filter.
   --  Context doesn't need to be Ref-ed or Unref-ed.

   procedure Register_Filter
     (Kernel : access Kernel_Handle_Record'Class;
      Filter : access Action_Filter_Record;
      Name   : String);
   --  Makes the filter accessible from other parts of GPS via a name,
   --  including scripts.

   procedure Free (Filter : in out Action_Filter_Record) is null;
   --  Free the memory associated with the filter. This must never be called
   --  directly and is only needed for the kernel itself. But it needs to be
   --  overridable for new filter types.

   function "and"
     (Filter1, Filter2 : access Action_Filter_Record'Class)
      return Action_Filter;
   function "or"
     (Filter1, Filter2 : access Action_Filter_Record'Class)
      return Action_Filter;
   function "not"
     (Filter : access Action_Filter_Record'Class)
      return Action_Filter;
   --  Execute logical operations between filters

   procedure Set_Error_Message
     (Filter : Action_Filter;
      Msg    : Unbounded_String);
   --  Set the error message to display if Filter doesn't match

   function Get_Error_Message
     (Filter : access Action_Filter_Record'Class) return Unbounded_String;
   --  Return the error message to display if the filter doesn't match

   function Get_Debug_Name
     (Filter : access Action_Filter_Record) return String;
   --  Return the description of the filter (a short string suitable for
   --  display in the logs)

   function Filter_Matches
     (Filter  : access Action_Filter_Record'Class;
      Context : Selection_Context) return Boolean;
   --  Same as Filter_Matches_Primitive, except it matches if Filter is null

   function Lookup_Filter
     (Kernel : access Kernel_Handle_Record;
      Name   : String) return Action_Filter;
   --  Lookup a filter by name. Return null if no such filter has been
   --  registered.

   function Create
     (Language   : String := "";
      Shell      : String := "";
      Shell_Lang : String := "Shell";
      Module     : String := "") return Action_Filter;
   --  Create a new filter.
   --  It does a logical AND for all its attributes specified as parameters.
   --  The default values for the parameters indicate that no special filter
   --  is done for this particular parameter.
   --  It might return null if no criteria is specified.

   type Base_Action_Filter_Record (<>)
      is new Action_Filter_Record with private;
   type Base_Action_Filter is access all Base_Action_Filter_Record'Class;

   overriding procedure Register_Filter
     (Kernel : access Kernel_Handle_Record'Class;
      Filter : access Base_Action_Filter_Record;
      Name   : String);
   overriding function Filter_Matches_Primitive
     (Filter  : access Base_Action_Filter_Record;
      Context : Selection_Context) return Boolean;
   --  See docs for inherited subprograms

   -----------
   -- Hooks --
   -----------

   type Hook_Function is abstract tagged private;
   type Hook_Function_Access is access all Hook_Function'Class;
   --  An object, whose Execute method is called when a hook is run.
   --  Actual subtypes will define different profiles for Execute,
   --  depending on the expected type of parameters or result value for
   --  this hook.
   --  The same object (or access to object) can be attached to multiple
   --  hooks. It will be freed when it is detached from the last hook.

   procedure Destroy (Self : in out Hook_Function) is null;
   --  Called when Self is disconnected from its hook.

   type Hook_Types is abstract tagged private;
   --  A type that describes hooks and family of hooks.
   --  The type hierarchy is expected to be three levels deep:
   --     * Hook_Types is the abstract ancestor
   --     * child types represent a family of hooks, with their parameters
   --       and return type. They provide a Type_Name.
   --     * grand-children types represent specific hooks within those
   --       those families, with a name given in the discriminant.
   --  The string used for the discriminant is never freed. For
   --  efficiency, it is recommended to pass an access to an
   --  "aliased constant String".

   function Type_Name (Self : Hook_Types) return String is abstract;
   --  Return the type name for this family of hooks.
   --  This is overridden by direct children of Hook_Types, but not by
   --  grand-children.
   --  This name is only useful when creating new hooks from python.

   procedure Run_From_Python
      (Self  : in out Hook_Types;
       Data  : in out GNATCOLL.Scripts.Callback_Data'Class) is null;
   --  This procedure is called when a python scripts "run"s a hook from
   --  python. It is responsible for decoding the parameters (encoded in
   --  Data) and then call Self.Run (the latter is defined for each child
   --  type of Hook_Types, since it has a different profile every time).
   --  The first parameter is always an instance of GPS.Hook, and should
   --  be ignored.

   procedure Register
      (Self    : not null access Hook_Types'Class;
       Kernel  : not null access Kernel_Handle_Record'Class);
   --  Export both the family of hooks and the specific hook to python, if not
   --  done yet. Once this is done, users will be able to create new hooks in
   --  python with the same profile (parameters + return type), and to add
   --  callbacks to the specific hook.
   --
   --  It is not possible to export a family of hooks if there is no specific
   --  hook that is part of it.
   --
   --  This procedure does not need to be called if the hook is only needed
   --  for Ada, and does not need to be queried by name.
   --
   --  Self is never freed. In general, it should point to an aliased
   --  global variable that represents the hook itself and can most
   --  efficiently be used by Ada code, without having to look up the
   --  hook by name.

   procedure Remove
       (Self       : in out Hook_Types;
        If_Matches : not null access function
           (F : not null access Hook_Function'Class) return Boolean);
   --  Remove the first attached function for which the function returns True.
   --  This also frees the corresponding function, unless it is attached to
   --  multiple hooks.

   function List_Functions
      (Self : not null access Hook_Types)
      return GNAT.Strings.String_List;
   --  Return the list of functions (by name) connected to this hook.
   --  Result must be freed by caller.

   type Debounce_Hook_Types is abstract new Hook_Types with private;
   --  Abstract type for asynchronouse hooks

   type Debounce_Hook_Access is access all Debounce_Hook_Types'Class;

   overriding procedure Remove
       (Self       : in out Debounce_Hook_Types;
        If_Matches : not null access function
           (F : not null access Hook_Function'Class) return Boolean);
   --  The same as above.

   overriding function List_Functions
      (Self : not null access Debounce_Hook_Types)
      return GNAT.Strings.String_List;

   type File_Status is (Modified, Unmodified, Unsaved, Saved, Readonly);

   -----------
   -- Tools --
   -----------
   --  The following subprograms are used to register the properties of the
   --  various external tools declared by the user in the customization files.
   --  These are associated with the <tool> tag.

   type Tool_Properties_Record is record
      Tool_Name         : Unbounded_String;
      Project_Package   : Unbounded_String;
      Project_Attribute : Unbounded_String;
      Project_Index     : Unbounded_String;
      Initial_Cmd_Line  : Unbounded_String;
      Override          : Boolean := False;
      Config            : Switches_Chooser.Switches_Editor_Config;
      Languages         : GNAT.Strings.String_List_Access;
   end record;
   type Tool_Properties is access all Tool_Properties_Record;
   --  (Project_Package, Project_Attribute, Project_Index) describe where its
   --  switches are stored in a project.
   --  Initial_Cmd_Line are the switches when the user hasn't edited them
   --  explicitely.
   --  Any of these field can be left to null if it has no special
   --  signification for this tool.

   type Tool_Properties_Array is array (Natural range <>) of Tool_Properties;

   procedure Register_Tool
     (Kernel : access Kernel_Handle_Record;
      Tool   : not null Tool_Properties);
   --  Register a new tool.
   --  No copy is made for Tool, which must therefore not be freed by the
   --  caller

   function Get_Tool_Properties
     (Kernel    : access Kernel_Handle_Record;
      Tool_Name : String) return Tool_Properties;
   --  Return the properties of the tool.
   --  The result must not be freed by the caller.

   function Get_All_Tools
     (Kernel : access Kernel_Handle_Record) return Tool_Properties_Array;
   --  Return all registered tools

   ------------------
   -- Key handlers --
   ------------------

   procedure Bind_Default_Key
     (Kernel      : access Kernel_Handle_Record;
      Action      : String;
      Default_Key : String;
      Exclusive   : Boolean := True);
   --  Associate a default key binding with an action.
   --  Default_Key is ignored if the key was previously overridden by the user.
   --  Its format is something like "control-o" or "control-x control-k", the
   --  second form specifies that it uses a secondary keymap.
   --  Action need not exist when the key is bound. This is why we require
   --  a string instead of an Action_Record.
   --
   --  If Exclusive is True, the shortcut will no longer apply for any action
   --  that it might currently be associated with.

   ---------------------
   --  Editor_Factory --
   ---------------------

   overriding function Get_Buffer_Factory
     (Kernel : not null access Kernel_Handle_Record)
      return GPS.Editors.Editor_Buffer_Factory_Access;

   procedure Set_Buffer_Factory
     (Kernel  : access Kernel_Handle_Record;
      Factory : GPS.Editors.Editor_Buffer_Factory_Access);

   procedure Set_Default_Line_Number_Click
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Action    : String);
   --  Register the action to execute when clicking on a line number and there
   --  is no message's action set on that line number.

   procedure Execute_Default_Line_Number_Click
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Context   : Selection_Context);
   --  Execute the default action when clicking on a line number

   -----------------
   -- Refactoring --
   -----------------

   function Refactoring_Context
     (Kernel : access Kernel_Handle_Record) return Refactoring.Factory_Context;

   overriding function Default_Language_Tree_Provider
     (Kernel : not null access Kernel_Handle_Record)
      return Semantic_Tree_Provider_Access;

   --------------
   -- Commands --
   --------------

   overriding function Get_Scheduled_Command
     (Kernel  : not null access Kernel_Handle_Record;
      Command : access Commands.Root_Command'Class)
      return Commands.Command_Access;

   ---------------------
   -- Messages window --
   ---------------------

   subtype Message_Type is GPS.Messages_Windows.Message_Type;

   function Info    return Message_Type renames GPS.Messages_Windows.Info;
   function Error   return Message_Type renames GPS.Messages_Windows.Error;
   function Verbose return Message_Type renames GPS.Messages_Windows.Verbose;

   type Abstract_Messages_Window is abstract new
     GPS.Messages_Windows.Abstract_Messages_Window with null record;
   type Abstract_Messages_Window_Access is
     access all Abstract_Messages_Window'Class;

   function Get_Console_Window
     (Self : not null access Abstract_Messages_Window)
      return Gtk.Widget.Gtk_Widget is abstract;
   --  Return the widget (if any) representing the console. It will be an
   --  instance of Interactive_Console, but we cannot make that explicit here
   --  to avoid circularities.

   procedure Set_Messages_Window
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Console : not null access Abstract_Messages_Window'Class);
   function Get_Messages_Window
     (Kernel  : not null access Kernel_Handle_Record'Class)
     return GNATCOLL.Scripts.Virtual_Console;
   --  Set the messages window

   function Get_Messages_Console
     (Kernel  : not null access Kernel_Handle_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   --  Return the widget (if any) representing the console.

   procedure Insert
     (Kernel : not null access Kernel_Handle_Record'Class;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info);
   procedure Insert_UTF8
     (Kernel : not null access Kernel_Handle_Record'Class;
      UTF8   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info);
   --  Insert a message in the Messages window.

   procedure Clear_Messages
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Clear the messaged window.

   procedure Raise_Console
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Raise the Messages window if it exists.

   ----------------
   -- Hyper_Mode --
   ----------------

   --  Handle the global state of the Hyper Mode

   procedure Enter_Hyper_Mode (Kernel : access Kernel_Handle_Record);
   --  Enter hyper mode

   procedure Leave_Hyper_Mode (Kernel : access Kernel_Handle_Record);
   --  Leave hyper mode

   function In_Hyper_Mode
     (Kernel : access Kernel_Handle_Record) return Boolean;
   --  Return True if we are in Hyper mode

   -----------------
   -- Build modes --
   -----------------

   procedure Set_Build_Mode
     (Kernel : access Kernel_Handle_Record'Class;
      New_Mode : String);
   --  Called when a new build mode is being selected. The name of that mode is
   --  passed as parameter to the Build_Mode_Changed_Hook. At the time the hook
   --  is run, various settings like the object's subdir might not have been
   --  set yet, since they are set by listeners on that hook.

   overriding function Get_Build_Mode
     (Kernel : not null access Kernel_Handle_Record) return String;
   --  Returns the current build mode.
   --  This build mode is in fact stored as a property of the root project by
   --  the builder module, so this function is a convenient to retrieve that
   --  property.

   overriding function Get_Target
     (Self : not null access Kernel_Handle_Record) return String;
   --  Return the current target

   overriding function Get_Runtime
     (Self : not null access Kernel_Handle_Record) return String;
   --  Return the current runtime

   subtype Abstract_Module_ID        is Abstract_Module;
   subtype Abstract_Module_ID_Record is Abstract_Module_Record;
   --  Type aliases for compability

   ------------------------
   -- Messages container --
   ------------------------

   type Messages_Container_Access is
     access all GPS.Kernel.Messages.Messages_Container'Class;

   function Get_Messages_Container
     (Kernel : not null access Kernel_Handle_Record'Class)
      return not null Messages_Container_Access;
   --  Returns messages conntainer for the specified instance of the kernel.

private

   type Filter_Type is (Filter_And, Filter_Or, Filter_Not, Standard_Filter);

   type Action_Filter_Record is abstract tagged record
      Error_Msg  : Unbounded_String;
      Name       : Unbounded_String;

      Registered : Boolean := False;
      --  For proper memory management, all filters are kept on an internal
      --  list, and freed on exit (we never need to deallocate a filter
      --  apart from that in general). They are added automatically to the list
      --  the first time they are used, so users do not need to do anything
      --  special except use them
   end record;

   type Base_Action_Filter_Record (Kind : Filter_Type)
      is new Action_Filter_Record
   with record
      case Kind is
         when Standard_Filter =>
            Language   : Unbounded_String;
            Shell      : Unbounded_String;
            Shell_Lang : Unbounded_String;
            Module     : Unbounded_String;

         when Filter_And =>
            And1, And2 : Action_Filter;

         when Filter_Or =>
            Or1, Or2 : Action_Filter;

         when Filter_Not =>
            Not1 : Action_Filter;
      end case;
   end record;
   overriding function Get_Debug_Name
     (Filter : access Base_Action_Filter_Record) return String;

   package Filter_Result_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => System.Address,
      Element_Type => Boolean,
      "<"          => System."<");

   type Addresses_Array is array (Positive range <>) of System.Address;
   type Addresses_Array_Access is access all Addresses_Array;

   Context_Class_Name : aliased constant String := "Context";
   type Context_Proxy is new Script_Proxy with null record;
   overriding function Class_Name (Self : Context_Proxy) return String
      is (Context_Class_Name) with Inline;

   type Entity_Locations_Type (Is_Fetched : Boolean := False) is record
      case Is_Fetched is
         when True =>
            Spec_Location : Xref.General_Location;
            Body_Location : Xref.General_Location;
         when False =>
            null;
      end case;
   end record;

   type Selection_Context_Data_Record is record
      Kernel    : Kernel_Handle;
      Creator   : Abstract_Module;
      Instances : Context_Proxy;

      Files             : GNATCOLL.VFS.File_Array_Access := null;
      --  The current selected files
      File_Lang         : Unbounded_String;

      Project           : GNATCOLL.Projects.Project_Type :=
        GNATCOLL.Projects.No_Project;
      Importing_Project : GNATCOLL.Projects.Project_Type :=
        GNATCOLL.Projects.No_Project;
      Line              : Integer := 0;
      File_Line         : Natural := 0;
      Column            : Basic_Types.Visible_Column_Type := 0;

      Browser_Details : Gtkada.Canvas_View.Canvas_Event_Details;
      Has_Browser_Details : Boolean := False;

      Messages       : Addresses_Array_Access := null;
      --  The current selected messages

      Revision       : Unbounded_String;
      Other_Revision : Unbounded_String;
      Tag            : Unbounded_String;
      --  In the location window

      Start_Line : Integer := 0;
      End_Line   : Integer := 0;
      --  A start and end line set to 0 means that the context does not have
      --  area information.

      Text       : Unbounded_String;
      --  When several lines are selected in a file. The selection starts
      --  at Line. Text is the current selection.

      Entity_Name      : Unbounded_String;
      Entity_Column    : Basic_Types.Visible_Column_Type := 0;
      Entity_Locations : Entity_Locations_Type;

      Expression    : Unbounded_String;

      Activities   : String_List_Utils.String_List.Vector;
      --  Activities

      File_Checked    : Boolean := False;
      --  The current file is sometimes a virtual file (one temporarily
      --  generated for a diff for instance). In such cases, it is converted to
      --  the actual reference file. File_Checked indicates whether this
      --  conversion already took place

      Creator_Provided_Project : Boolean := False;
      --  Set to True if the project_view was given by the creator, instead of
      --  being computed automatically

      Is_Dispatching_Call : GNATCOLL.Tribooleans.Triboolean :=
        GNATCOLL.Tribooleans.Indeterminate;
      --  Whether we clicked on a dispatching call.

      Xref_Entity : Xref.Root_Entity_Ref;
      --  The Entity for xref purposes. This is computed when the context
      --  is created. When the source location only results in a possible
      --  candidate (overloaded entities and not up-to-date ALI files), the
      --  xref engines are responsible for making a best guess, or setting
      --  this to empty.

      Xref_Entity_Resolution_Attempted : Boolean := False;
      --  Whether we have already attempted to get the Xref_Entity for this
      --  context.

      Xref_Entity_Type_Of : Xref.Root_Entity_Ref;
      --  The type of Xref_Entity, cached.
      --  No_General_Entity indicates that the entity hasn't been computed.

      Xref_Entity_Has_Parent_Types : GNATCOLL.Tribooleans.Triboolean :=
        GNATCOLL.Tribooleans.Indeterminate;
      --  Whether the entity has at least one parent type.

      Xref_Closest_Ref  : Xref.Root_Entity_Reference_Ref :=
        Xref.Root_Entity_Reference_Refs.To_Holder
          (Xref.No_Root_Entity_Reference);
      --  The reference on which the user has clicked. This is slightly
      --  redundant with the location above, but since this was computed at the
      --  same time as the entity anyway, we might as well store it.

      Computed_Filters : Filter_Result_Map.Map;
      --  Cache the result of each filter object applied to this context.
   end record;

   procedure Free (Self : in out Selection_Context_Data_Record);
   --  Free the context and its data

   package Selection_Pointers is new GNATCOLL.Refcount.Shared_Pointers
      (Element_Type           => Selection_Context_Data_Record,
       Release                => Free,
       Atomic_Counters        => True);

   type Selection_Context is record
      Ref : Selection_Pointers.Ref;
   end record;
   type Weak_Selection_Context is record
      Weak : Selection_Pointers.Weak_Ref;
   end record;
   No_Context : constant Selection_Context :=
      (Ref => Selection_Pointers.Null_Ref);
   No_Weak_Context : constant Weak_Selection_Context :=
      (Weak => Selection_Pointers.Null_Weak_Ref);

   package Context_Proxies is new Script_Proxies
      (Weak_Selection_Context, Context_Proxy);

   No_Tool : constant Tool_Properties_Record :=
     (Null_Unbounded_String,
      Null_Unbounded_String,
      Null_Unbounded_String,
      Null_Unbounded_String,
      Null_Unbounded_String,
      False, null, null);

   package Tools_List is new Ada.Containers.Doubly_Linked_Lists
     (Tool_Properties);
   --  Tools are stored in a list (we expect only a limited number of tools in
   --  any case), so that we also preserve the order in which they were
   --  registered. This is important when displaying the project properties
   --  dialog for instance.

   ------------------------------------
   -- Abstract type defining a table --
   ------------------------------------

   type Root_Table is abstract tagged null record;
   type Root_Table_Access is access all Root_Table'Class;

   procedure Reset (X : access Root_Table) is abstract;
   --  Reset the table

   package Action_Filters_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Action_Filter);
   --  All filters (named or unnamed) that are registered

   package Action_Filters_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Action_Filter, Ada.Strings.Hash_Case_Insensitive, "=");
   --  We never free the filter from this hash-table, since the filters might
   --  be shared between actions.

   type Action_Filter_Iterator is record
      Iterator : Action_Filters_Maps.Cursor;
   end record;

   ----------
   -- Hook --
   ----------

   type Hook_Function is abstract tagged record
      Refcount : Natural := 0;
      --  The number of time this object is added to a hook. This is used
      --  to properly free memory, in particular when the same object is
      --  connected to multiple hooks.
   end record;

   type Hook_Func_Info is record
      Func : not null access Hook_Function'Class;
   end record;
   package Hook_Func_Lists is new Ada.Containers.Doubly_Linked_Lists
      (Hook_Func_Info);
   --  We use a list, not a vector: when a hook is run, it is possible that
   --  the list of functions is modified by one of the callbacks. The Run
   --  subprograms are implemented so that this is safe with a list, but if
   --  we were using a vector, the cursor might become invalid.

   type Hook_Types is abstract tagged record
      Name  : access constant String;
      Funcs : Hook_Func_Lists.List;
   end record;

   type Hook_Types_Access is access all Hook_Types'Class;

   package Hooks_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Hook_Types_Access,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");

   Hook_Type_Prefix : constant String := "__type__";
   --  Both hooks and hook types are stored in the kernel's map.
   --  To distinguish, a prefix is added to the name of the types.

   procedure Add_Hook_Func
      (Self  : in out Hook_Types'Class;
       Func  : not null access Hook_Function'Class;
       Last  : Boolean := True;
       Watch : access Glib.Object.GObject_Record'Class := null);
   --  Add a new callback to the hook.
   --  This function should not be used directly, use the specific Add
   --  function for each hook, which checks the function has the right profile

   procedure Remove_Hook_Func
      (Self  : in out Hook_Types'Class;
       Func  : not null access Hook_Function'Class);
   --  Delete Func from the hook.
   --  This also frees the corresponding function, unless it is attached to
   --  multiple hooks.

   type Hook_Function_Params is abstract tagged null record;
   --  An abstract object which represents hook's parameters
   --  for asynchronouse call

   type Hook_Function_Params_Access is access all Hook_Function_Params'Class;
   package Hook_Func_Params_Lists is new Ada.Containers.Doubly_Linked_Lists
      (Hook_Function_Params_Access);

   type Debounce_Hook_Types is abstract new Hook_Types with record
      Asynch_Funcs : Hook_Func_Lists.List;
      Asynch_Data  : Hook_Func_Params_Lists.List;
   end record;

   procedure Add_Debounce_Hook_Func
      (Self  : in out Debounce_Hook_Types'Class;
       Func  : not null access Hook_Function'Class;
       Last  : Boolean := True;
       Watch : access Glib.Object.GObject_Record'Class := null);
   --  Add a new asynchronous callback to the hook.

   type Custom_Load_State is (None, System_Level, User_Level);
   --  None         : loading not started
   --  System_Level : system custom files loaded
   --  User_Level   : system and user custom files loaded

   type Pattern_Matcher_Access is access GNAT.Regpat.Pattern_Matcher;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, Pattern_Matcher_Access);

   function Hash
     (Element : Commands.Command_Access) return Ada.Containers.Hash_Type;
   package Command_Set is new Ada.Containers.Hashed_Sets
     (Element_Type        => Commands.Command_Access,
      Hash                => Hash,
      Equivalent_Elements => Commands."=",
      "="                 => Commands."=");

   type Kernel_Handle_Record is new GPS.Core_Kernels.Core_Kernel_Record with
   record
      Tools   : Tools_List.List;
      --  The tools registered in the kernel

      Actions : Root_Table_Access;
      --  The actions registered in the kernel

      Startup_Scripts : Root_Table_Access;
      --  The list of startup scripts and whether they should be loaded

      Hooks       : Hooks_Maps.Map;
      --  The hooks registered in the kernel. The hooks are indexed by
      --  their names. A second entry will be created for the family
      --  of hooks. So for instance, the hook "task_started", which is
      --  of type "task_hooks" ends up with two entries in this table:
      --      "task_started" => points to the hook object itself
      --      "type:task_hooks" => points also to the hook, but only
      --         its operations will be used, not the list of functions
      --         connected to it. This is used to create new hook types
      --         from python.

      Action_Filters     : Action_Filters_Maps.Map;
      All_Action_Filters : Action_Filters_Lists.List;
      --  The action contexts registered in the kernel (only named ones are
      --  stored in Action_Filters)

      Application : access Gtk_Application_Record'Class;
      Main_Window : access Gtk.Window.Gtk_Window_Record'Class;
      --  The main GPS window
      --  We used to store the id of the main window (from the application),
      --  but this gets invalid early when GPS exists, and we no longer have
      --  access to the main window while destroying its children

      GNAT_Version : Unbounded_String;
      --  Full GNAT Version, if relevant

      Preferences : Default_Preferences.Preferences_Manager;
      --  The current setting for the preferences

      Style_Manager : System.Address;
      --  A pointer to the color manager.

      VCS   : GPS.VCS.Abstract_VCS_Repository_Access;

      ----------------------
      -- Context handling --
      ----------------------

      Current_Context : Selection_Context := No_Context;
      --  The current context, as set by the last call to Context_Changed.

      Last_Context_For_Contextual : Selection_Context := No_Context;
      --  The context used in the last contextual menu.
      --  This variable should remain not null and unchanged while a contextual
      --  menu or standard menu is displayed and executed, so that user scripts
      --  have access to it.

      Last_Context_From_Contextual : Boolean := False;
      --  Whether Last_Context_For_Contextual has been obtain from a contextual
      --  menu.

      Home_Dir : Virtual_File;
      --  The home directory (e.g ~/.gps)

      Prefix   : Virtual_File;
      --  Prefix directory (e.g. /opt/gps)

      Logs_Mapper : Basic_Mapper.File_Mapper_Access;
      --  Mapping between files and logs

      Open_Files : aliased File_Sets.Set;
      --  The list of currently open files

      History : Histories.History;
      --  The various histories used throughout GPS

      Tasks : Task_Manager.Task_Manager_Access;
      --  The GPS task manager

      Custom_Files_Loaded : Custom_Load_State := None;
      --  Whether all custom files have already been loaded

      Customization_Strings : XML_Utils.Node_Ptr;
      --  The customization strings hard-coded by the modules, and they have
      --  been registered before all modules are loaded.

      Contextual : System.Address := System.Null_Address;
      --  The contextual menus registered by the user. This is only used in
      --  GPS.Kernel.Modules, and cast to the appropriate type in that
      --  package.

      Clipboard  : System.Address := System.Null_Address;
      --  The clipboard used in GPS (See GPS.Kernel.Clipboard on how to use
      --  this field).

      Is_In_Destruction : Boolean := False;
      --  Determies wether the kernel is being destroyed

      Hidden_File_Matcher : Pattern_Matcher_Access;

      Editor_Factory               : GPS.Editors.Editor_Buffer_Factory_Access;

      Hyper_Mode                   : Boolean := False;
      --  Whether we are in hyper mode

      Messages_Container : Messages_Container_Access;
      --  The message container for this instance of kernel

      Key_Setter_Function : Key_Setter;
      Key_Getter_Function : Key_Getter;
      Key_Getter_Simple_Function : Key_Getter_Simple;
      --  The function to set or retrieve default keys

      Refactoring : Standard.Refactoring.Factory_Context;

      Messages : Abstract_Messages_Window_Access;

      Default_Line_Click_Action : GNAT.Strings.String_Access;
      --  default action to execute when clicking on a line

      Launcher : aliased GPS_Process_Launcher_Record;
      --  External process launcher

      Pending_Messages : Ada.Strings.Unbounded.Unbounded_String
         := Ada.Strings.Unbounded.Null_Unbounded_String;
      --  Messages that should be inserted in the Messages window.
      --  We use this to store messages until the Messages window is
      --  created.

      Contextual_Menu_Open : Boolean := False;

      Check_Monitored_Files_Dialog : access Gtk.Dialog.Gtk_Dialog_Record'Class
         := null;
      --  The dialog that let's users synchronize edited files with the disk.

      Check_Monitored_Files_Id : Glib.Main.G_Source_Id :=
         Glib.Main.No_Source_Id;
      --  An idle callback  used to check whether any file currently edited
      --  has been changed on disk.

      Env : Environment;
      --  List of environment variables overwritten by GPS

      Construct_Tree : Semantic_Tree_Provider_Access;
      --  Handle to the default language tree in GPS kernel, which is a
      --  construct tree at the moment
   end record;

   package Kernel_Sources is new Glib.Main.Generic_Sources (Kernel_Handle);

   overriding procedure Create_Registry
     (Self   : not null access Kernel_Handle_Record;
      Result : out Projects.Project_Registry_Access);

   overriding procedure Create_Database
     (Self   : not null access Kernel_Handle_Record;
      Result : out Xref.General_Xref_Database);

   overriding function Messages_Window
     (Self : not null access Kernel_Handle_Record)
      return GPS.Messages_Windows.Abstract_Messages_Window_Access;

   overriding function Process_Launcher
     (Self : not null access Kernel_Handle_Record)
      return GPS.Process_Launchers.Process_Launcher;

   function Get_Contextual_Menu_Open
     (Handle : access Kernel_Handle_Record) return Boolean
   is
      (Handle.Contextual_Menu_Open);

end GPS.Kernel;
