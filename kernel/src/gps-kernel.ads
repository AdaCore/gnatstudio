------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
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
with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with System;

with GNAT.Strings;
with GNATCOLL.Projects;
with GNAT.Regpat;
with GNATCOLL.Scripts;
with GNATCOLL.Traces;
with GNATCOLL.Tribooleans;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNATCOLL.Xref; use GNATCOLL.Xref;

with Glib.Main;
with Glib.Object;       use Glib;
with Gdk.Types;
with Gtk.Application;   use Gtk.Application;
with Gtk.Dialog;
with Gtk.Widget;
with Gtk.Window;

with Basic_Types;
with Basic_Mapper;
with Commands;
with Generic_List;
with HTables;
with Language_Handlers;
with Language.Tree.Database;
with Projects;
with String_Hash;
with Default_Preferences;
with Histories;
with Refactoring;
with Switches_Chooser;
with String_List_Utils;
with Task_Manager;
with XML_Utils;
with Xref;

with GPS.Editors;
with GPS.Environments;                 use GPS.Environments;
with GPS.Core_Kernels;                 use GPS.Core_Kernels;
with GPS.Messages_Windows;
with GPS.Process_Launchers;
with GPS.Process_Launchers.Implementation;
use GPS.Process_Launchers.Implementation;

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
   --  plug-in.

   function Get_Main_Window
     (Handle : access Kernel_Handle_Record) return Gtk.Window.Gtk_Window;
   --  Return the main window associated with the kernel

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
     (Kernel : access Kernel_Handle_Record) return GNATCOLL.VFS.File_Array;
   --  Return a list of currently open files

   function Is_Hidden
     (Kernel    : access Kernel_Handle_Record;
      Base_Name : Filesystem_String) return Boolean;
   --  Return whether File or Directory identified by its Base_Name should be
   --  considered as hidden for all GUI purposes, such as the Project/File
   --  explorer or the VCS operations.

   -------------
   -- Queries --
   -------------

   function Get_Construct_Database
     (Kernel : access Kernel_Handle_Record)
      return Language.Tree.Database.Construct_Database_Access;
   --  Return the database storing the construct information

   --------------
   -- Contexts --
   --------------

   type Selection_Context is private;

   No_Context : constant Selection_Context;
   --  This type contains all the information about the selection in any
   --  module. Note that this is a tagged type, so that it can easily be
   --  extended for modules external to GPS.

   function New_Context return Selection_Context;
   --  Return a new context, no property is set

   procedure Set_Context_Information
     (Context : in out Selection_Context;
      Kernel  : access Kernel_Handle_Record'Class;
      Creator : Abstract_Module);
   --  Set the information in the context

   function Get_Kernel (Context : Selection_Context) return Kernel_Handle;
   --  Return the kernel associated with the context

   function Get_Creator
     (Context : Selection_Context) return Abstract_Module;
   --  Return the module ID for the module that created the context

   procedure Set_Is_Dispatching_Call
     (Context : Selection_Context; Is_Dispatching : Boolean);
   function Is_Dispatching_Call
     (Context : Selection_Context) return GNATCOLL.Tribooleans.Triboolean;
   --  Whether the user clicked on a dispatching call. This information is
   --  cached in the context the first time it is computed.

   -------------
   -- Markers --
   -------------
   --  The following subprograms provide the required support for representing
   --  and storing locations in a list, so that the user can move back and
   --  forward from places where he was before.
   --  Location in this sense is to be taken as a broad term, since it might
   --  represent a location in a source editor, but also a location within
   --  a browser,...

   type Location_Marker_Record is abstract tagged private;
   type Location_Marker is access all Location_Marker_Record'Class;

   function Go_To
     (Marker : access Location_Marker_Record;
      Kernel : access Kernel_Handle_Record'Class) return Boolean is abstract;
   --  Move the focus in GPS to the location marked by M.
   --  If this function returns False, it is assumed the marker is no longer
   --  legal, and should be removed from the history.

   function Clone
     (Marker : access Location_Marker_Record)
      return Location_Marker is abstract;
   --  Return a clone of Marker

   procedure Destroy (Marker : in out Location_Marker_Record);
   --  Free the memory used by Marker. By default, this does nothing.

   function To_String
     (Marker : access Location_Marker_Record) return String is abstract;
   --  Return a displayable string describing marker.
   --  This string doesn't need to be unique for each marker, it is used in the
   --  user interface to allow the user to select a specific marker.

   function Save
     (Marker : access Location_Marker_Record)
      return XML_Utils.Node_Ptr is abstract;
   --  Saves the marker to an XML node, so that it can be reloaded later on,
   --  possibly in a different GPS session.

   procedure Push_Marker_In_History
     (Kernel : access Kernel_Handle_Record'Class;
      Marker : access Location_Marker_Record'Class);
   --  Push a new marker in the list of previous locations the user has
   --  visited. This is the basic interface for the handling of the history of
   --  locations. It emits the hook Marker_Added_To_History.

   function Similar
     (Left  : access Location_Marker_Record;
      Right : access Location_Marker_Record'Class) return Boolean is abstract;
   --  Return True if Left and Right point to the same location in the sense
   --  that GPS should not add a new marker in history for two locations that
   --  are the same.

   function Distance
     (Left  : access Location_Marker_Record;
      Right : access Location_Marker_Record'Class) return Integer is abstract;
   --  Return a value represented distance between two locations.
   --  Return Integer'Last if locations are not comparable, for example marks
   --  are in different files.

   --------------
   -- Commands --
   --------------

   procedure Register_Perma_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : access Commands.Root_Command'Class);
   --  Register a command to be freed when GPS exits. Such commands must not be
   --  added to command queues, and therefore this is mostly intended for
   --  commands used in actions or menus (but in such case the command is
   --  automatically added already). A given command can be registered
   --  several times though.
   --  The current reference to Command is stolen, ie you must not call Unref
   --  on the command before first calling Ref yourself.

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
   --  The goal of this procedure is twofold:
   --    * If a name is specified, the filter will be accessible from other
   --      places in GPS, including scripts, through calls to Lookup_Filter
   --    * Whether the name is specified or not, this ensures that the filter
   --      will be properly freed when GPS exits. When you create new filter
   --      types that embed other filters, you should override this procedure
   --      so that it calls the inherited version and calls Register_Filter on
   --      each of the filters it embeds

   type Base_Action_Filter_Record (<>)
      is new Action_Filter_Record with private;
   type Base_Action_Filter is access all Base_Action_Filter_Record'Class;

   overriding
   function Filter_Matches_Primitive
     (Filter  : access Base_Action_Filter_Record;
      Context : Selection_Context) return Boolean;
   --  See docs for inherited subprograms

   function Create
     (Language   : String := "";
      Shell      : String := "";
      Shell_Lang : String := "Shell";
      Module     : String := "") return Action_Filter;
   --  Create a new filter.
   --  It does a logical AND for all its attributes specified as parameters.
   --  The default values for the parameters indicate that no special filter
   --  is done for this particular parameter.

   procedure Free (Filter : in out Action_Filter_Record);
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

   procedure Set_Error_Message (Filter : Action_Filter; Msg : String);
   --  Set the error message to display if Filter doesn't match

   function Get_Error_Message
     (Filter : access Action_Filter_Record'Class) return String;
   --  Return the error message to display if the filter doesn't match

   function Get_Name
     (Filter : access Action_Filter_Record'Class) return String;
   --  Return the description of the filter (a short string suitable for
   --  display in the key manager GUI

   function Filter_Matches
     (Filter  : access Action_Filter_Record'Class;
      Context : Selection_Context) return Boolean;
   --  Same as Filter_Matches_Primitive, except it matches if Filter is null

   overriding procedure Register_Filter
     (Kernel : access Kernel_Handle_Record'Class;
      Filter : access Base_Action_Filter_Record;
      Name   : String);

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
   --  See the package GPS.Kernel.Hooks for more subprograms applying to
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

   type Tool_Properties_Record is record
      Tool_Name         : GNAT.Strings.String_Access;
      Project_Package   : GNAT.Strings.String_Access;
      Project_Attribute : GNAT.Strings.String_Access;
      Project_Index     : GNAT.Strings.String_Access;
      Initial_Cmd_Line  : GNAT.Strings.String_Access;
      Override          : Boolean := False;
      Config            : Switches_Chooser.Switches_Editor_Config;
      Languages         : GNAT.Strings.String_List_Access;
   end record;
   --  (Project_Package, Project_Attribute, Project_Index) describe where its
   --  switches are stored in a project.
   --  Initial_Cmd_Line are the switches when the user hasn't edited them
   --  explicitely.
   --  Any of these field can be left to null if it has no special
   --  signification for this tool.

   type Tool_Properties_Array
     is array (Natural range <>) of Tool_Properties_Record;

   No_Tool : constant Tool_Properties_Record;

   procedure Register_Tool
     (Kernel : access Kernel_Handle_Record;
      Tool   : Tool_Properties_Record);
   --  Register a new tool.
   --  No copy is made for Tool, which must therefore not be freed by the
   --  caller

   function Get_Tool_Properties
     (Kernel    : access Kernel_Handle_Record;
      Tool_Name : String) return Tool_Properties_Record;
   --  Return the properties of the tool.
   --  The resulting record must not be freed by the caller.

   function Get_All_Tools
     (Kernel : access Kernel_Handle_Record) return Tool_Properties_Array;
   --  Return all registered tools

   ------------------
   -- Key handlers --
   ------------------

   procedure Bind_Default_Key
     (Kernel      : access Kernel_Handle_Record;
      Action      : String;
      Default_Key : String);
   --  Associate a default key binding with an action.
   --  Default_Key is ignored if the key was previously overriden by the user.
   --  Its format is something like "control-o" or "control-x control-k", the
   --  second form specifies that it uses a secondary keymap.
   --  Action need not exist when the key is bound. This is why we require
   --  a string instead of an Action_Record.

   ---------------------
   --  Editor_Factory --
   ---------------------

   overriding function Get_Buffer_Factory
     (Kernel : not null access Kernel_Handle_Record)
      return GPS.Editors.Editor_Buffer_Factory_Access;

   procedure Set_Buffer_Factory
     (Kernel  : access Kernel_Handle_Record;
      Factory : GPS.Editors.Editor_Buffer_Factory_Access);

   -----------------
   -- Refactoring --
   -----------------

   function Refactoring_Context
     (Kernel : access Kernel_Handle_Record) return Refactoring.Factory_Context;

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

   -----------
   -- Hooks --
   -----------

   procedure Context_Changed (Handle : access Kernel_Handle_Record);
   --  Runs the "context_changed" hook

   procedure Source_Lines_Revealed
     (Handle  : access Kernel_Handle_Record;
      Context : Selection_Context);
   --  Runs the "source_lines_revealed" hook

   procedure Source_Lines_Folded
     (Handle     : access Kernel_Handle_Record;
      Context    : Selection_Context;
      Start_Line : Natural;
      End_Line   : Natural);
   --  Runs the "source_lines_folded" hook

   procedure Source_Lines_Unfolded
     (Handle     : access Kernel_Handle_Record;
      Context    : Selection_Context;
      Start_Line : Natural;
      End_Line   : Natural);
   --  Runs the "source_lines_unfolded" hook

   procedure File_Edited
     (Handle : access Kernel_Handle_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Force_Hook : Boolean := False);
   --  Runs the "file_edited" hook.
   --  If Force_Hook is True, the hook will always be emitted, otherwise it is
   --  only emitted if the file wasn't opened yet.

   procedure Before_File_Saved
     (Handle : access Kernel_Handle_Record;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Runs the "before_file_saved" hook

   procedure File_Saved
     (Handle : access Kernel_Handle_Record;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Runs the "file_saved" hook

   procedure File_Closed
     (Handle : access Kernel_Handle_Record;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Runs the "file_closed" hook

   procedure File_Deleted
     (Handle : access Kernel_Handle_Record;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Runs the "file_deleted" hook

   procedure File_Renamed
     (Handle   : access Kernel_Handle_Record;
      File     : GNATCOLL.VFS.Virtual_File;
      New_Path : GNATCOLL.VFS.Virtual_File);
   --  Runs the "file_renamed" hook

   procedure File_Changed_On_Disk
     (Handle : access Kernel_Handle_Record;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Runs the "file_changed_on_disk" hook

   function Compilation_Starting
     (Handle   : access Kernel_Handle_Record;
      Category : String;
      Quiet    : Boolean;
      Shadow   : Boolean;
      Background : Boolean) return Boolean;
   --  Runs the "compilation_starting" hook.
   --  The Category corresponds to the location/highlighting category that
   --  will contain the compilation output.
   --  Quiet is true if the compilation should not ask the user any question,
   --  nor, generally, change the MDI setup.
   --  Return True if the compilation should be started.

   procedure Compilation_Finished
     (Handle      : access Kernel_Handle_Record;
      Category    : String;
      Target_Name : String;
      Mode_Name   : String;
      Shadow      : Boolean;
      Background  : Boolean;
      Status      : Integer);
   --  Runs the "compilation_finished" hook
   --  The Category corresponds to the location/highlighting category that
   --  contains the compilation output.

   type Hook_Name is new Ada.Strings.Unbounded.Unbounded_String;
   --  The name/key of the hook as registered into GPS

   type Hook_Type is new String;
   --  The hook data type

   type Hook_List is array (Positive range <>) of Hook_Name;

   function To_Hook_Name (Item : String) return Hook_Name
     renames To_Unbounded_String;

   --  Hooks with no arguments
   Preference_Changed_Hook      : constant Hook_Name :=
                                     To_Hook_Name ("preferences_changed");
   Search_Reset_Hook             : constant Hook_Name :=
                                     To_Hook_Name ("search_reset");
   Search_Functions_Changed_Hook : constant Hook_Name :=
                                     To_Hook_Name ("search_functions_changed");
   Search_Regexps_Changed_Hook   : constant Hook_Name :=
                                     To_Hook_Name ("search_regexps_changed");
   Variable_Changed_Hook         : constant Hook_Name :=
                                     To_Hook_Name ("variable_changed");
   Project_View_Changed_Hook     : constant Hook_Name :=
                                     To_Hook_Name ("project_view_changed");
   Project_Changed_Hook          : constant Hook_Name :=
                                     To_Hook_Name ("project_changed");
   Project_Editor_Hook           : constant Hook_Name :=
                                     To_Hook_Name ("project_editor");
   Contextual_Menu_Open_Hook     : constant Hook_Name :=
                                     To_Hook_Name ("contextual_menu_open");
   Contextual_Menu_Close_Hook    : constant Hook_Name :=
                                     To_Hook_Name ("contextual_menu_close");
   Desktop_Loaded_Hook           : constant Hook_Name :=
                                     To_Hook_Name ("desktop_loaded");

   --  Hooks with File_Hooks_Args argument
   Project_Changing_Hook         : constant Hook_Name :=
                                     To_Hook_Name ("project_changing");
   File_Edited_Hook              : constant Hook_Name :=
                                     To_Hook_Name ("file_edited");
   Before_File_Saved_Hook        : constant Hook_Name :=
                                     To_Hook_Name ("before_file_saved");
   File_Saved_Hook               : constant Hook_Name :=
                                     To_Hook_Name ("file_saved");
   File_Closed_Hook              : constant Hook_Name :=
                                     To_Hook_Name ("file_closed");
   File_Deleted_Hook             : constant Hook_Name :=
                                     To_Hook_Name ("file_deleted");
   File_Renamed_Hook             : constant Hook_Name :=
                                     To_Hook_Name ("file_renamed");
   File_Changed_Detected_Hook    : constant Hook_Name :=
                                     To_Hook_Name ("file_changed_detected");
   File_Changed_On_Disk_Hook     : constant Hook_Name :=
                                     To_Hook_Name ("file_changed_on_disk");
   Compilation_Finished_Hook     : constant Hook_Name :=
                                     To_Hook_Name ("compilation_finished");
   Compilation_Starting_Hook     : constant Hook_Name :=
                                     To_Hook_Name ("compilation_starting");

   Buffer_Modified_Hook : constant Hook_Name := To_Hook_Name ("buffer_edited");
   --  Hook called after a buffer has been edited.

   --  Hooks with Context_Hooks_Args argument
   Context_Changed_Hook          : constant Hook_Name :=
                                     To_Hook_Name ("context_changed");

   --  Hooks with Context_Hooks_Args argument (a File_Area_Context_Access)
   Source_Lines_Revealed_Hook    : constant Hook_Name :=
                                     To_Hook_Name ("source_lines_revealed");

   Source_Lines_Folded_Hook    : constant Hook_Name :=
                                     To_Hook_Name ("source_lines_folded");

   Source_Lines_Unfolded_Hook    : constant Hook_Name :=
                                     To_Hook_Name ("source_lines_unfolded");

   --  Hooks with Project_Hooks_Args argument
   Project_Saved_Hook            : constant Hook_Name :=
                                     To_Hook_Name ("project_saved");

   --  Hooks with Marker_Hooks_Args argument
   Marker_Added_In_History_Hook : constant Hook_Name :=
                                    To_Hook_Name ("marker_added_to_history");
   --  Called when a new marker has been added in the history. For now, this
   --  marker isn't exported to the shell

   File_Status_Changed_Hook      : constant Hook_Name :=
                                     To_Hook_Name ("file_status_changed");
   --  Called when the status of a file is changed : Modified, Unmodified...

   --  Hooks with String_Hooks_Args argument
   Compute_Build_Targets_Hook : constant Hook_Name :=
                                  To_Hook_Name ("compute_build_targets");
   --  Called when computing list of build targets, e.g. list of mains, or list
   --  of Makefile targets. The string parameter gives the kind of target to
   --  be computed (e.g. "main", "makefile").

   -------------------
   -- Sets of files --
   -------------------

   package File_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type        => GNATCOLL.VFS.Virtual_File,
       Hash                => GNATCOLL.VFS.Full_Name_Hash,
       Equivalent_Elements => GNATCOLL.VFS."=",
       "="                 => GNATCOLL.VFS."=");
   -----------------
   -- Build modes --
   -----------------

   Build_Mode_Changed_Hook : constant Hook_Name :=
                               To_Hook_Name ("build_mode_changed");
   --  Hook run to request the change of the build mode

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

private

   type Filter_Type is (Filter_And, Filter_Or, Filter_Not, Standard_Filter);

   type Action_Filter_Record is abstract tagged record
      Error_Msg : GNAT.Strings.String_Access;
      Name      : GNAT.Strings.String_Access;

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
            Language   : GNAT.Strings.String_Access;
            Shell      : GNAT.Strings.String_Access;
            Shell_Lang : GNAT.Strings.String_Access;
            Module     : GNAT.Strings.String_Access;

         when Filter_And =>
            And1, And2 : Action_Filter;

         when Filter_Or =>
            Or1, Or2 : Action_Filter;

         when Filter_Not =>
            Not1 : Action_Filter;
      end case;
   end record;

   overriding procedure Free (Filter : in out Base_Action_Filter_Record);

   package Filter_Result_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => System.Address,
      Element_Type => Boolean,
      "<"          => System."<");

   type Selection_Context_Data_Record is record
      Kernel    : Kernel_Handle;
      Creator   : Abstract_Module;
      Ref_Count : Natural := 1;

      Instances : GNATCOLL.Scripts.Instance_List_Access;

      Files             : GNATCOLL.VFS.File_Array_Access := null;
      --  The current selected files

      Project           : GNATCOLL.Projects.Project_Type :=
        GNATCOLL.Projects.No_Project;
      Importing_Project : GNATCOLL.Projects.Project_Type :=
        GNATCOLL.Projects.No_Project;
      Line              : Integer := 0;
      Column            : Basic_Types.Visible_Column_Type := 0;

      Message        : System.Address := System.Null_Address;
      Revision       : GNAT.Strings.String_Access := null;
      Other_Revision : GNAT.Strings.String_Access := null;
      Tag            : GNAT.Strings.String_Access := null;
      --  In the location window

      Start_Line : Integer;
      End_Line   : Integer;
      Text       : GNAT.Strings.String_Access;
      --  When several lines are selected in a file. The selection starts
      --  at Line. Text is the current selection.

      Entity_Name   : GNAT.Strings.String_Access;
      Entity_Column : Basic_Types.Visible_Column_Type := 0;

      Expression    : GNAT.Strings.String_Access := null;

      Activities   : String_List_Utils.String_List.List :=
                       String_List_Utils.String_List.Null_List;
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

   type Selection_Context_Data is access all Selection_Context_Data_Record;
   type Selection_Context_Controlled is new Ada.Finalization.Controlled
      with record
         Data : Selection_Context_Data;
      end record;
   type Selection_Context is record
      Data : Selection_Context_Controlled;
   end record;
   --  Selection_Context should not be visibly tagged, otherwise we would have
   --  operations dispatching on multiple types above

   overriding
   procedure Adjust   (Context : in out Selection_Context_Controlled);

   overriding
   procedure Finalize (Context : in out Selection_Context_Controlled);
   --  See inherited documentation

   No_Context : constant Selection_Context :=
                  (Data => (Ada.Finalization.Controlled with null));

   No_Tool : constant Tool_Properties_Record :=
               (null, null, null, null, null, False, null, null);

   package Tools_List is new Ada.Containers.Doubly_Linked_Lists
     (Tool_Properties_Record);
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

   procedure Do_Nothing (Filter : in out Action_Filter) is null;
   package Action_Filters_Htable is new String_Hash
     (Action_Filter, Do_Nothing, null);
   --  We never free the filter from this hash-table. This is done in the list
   --  that stores all actions

   procedure Free (Filter : in out Action_Filter);
   package Action_Filters_List is new Generic_List (Action_Filter, Free);

   type Action_Filter_Iterator is record
      Iterator : Action_Filters_Htable.String_Hash_Table.Cursor;
   end record;

   ----------
   -- Hook --
   ----------

   type Hook_Function_Record is abstract tagged record
      Ref_Count    : Natural := 0;
      Watch_Object : Glib.Object.GObject;
      Watch_Data   : System.Address;
      --  Watch_Object and Watch_Data are filled when hook is configured to
      --  watching for Glib object.
   end record;

   type Hook_Description_Base is abstract tagged null record;
   type Hook_Description_Base_Access is access all Hook_Description_Base'Class;

   type Hook_Htable_Num is new Natural range 0 .. 6150;

   function Hash (Hook : Hook_Name) return Hook_Htable_Num;

   procedure Free (Hook : in out Hook_Description_Base);
   --  Free the memory occupied by Hook

   procedure Free (L : in out Hook_Description_Base_Access);
   package Hooks_Hash is new HTables.Simple_HTable
     (Header_Num   => Hook_Htable_Num,
      Element      => Hook_Description_Base_Access,
      No_Element   => null,
      Key          => Hook_Name,
      Hash         => Hash,
      Equal        => "=",
      Free_Element => Free);

   type Location_Marker_Record is abstract tagged null record;

   type Custom_Load_State is (None, System_Level, User_Level);
   --  None         : loading not started
   --  System_Level : system custom files loaded
   --  User_Level   : system and user custom files loaded

   type Pattern_Matcher_Access is access GNAT.Regpat.Pattern_Matcher;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, Pattern_Matcher_Access);

   type Kernel_Handle_Record is new GPS.Core_Kernels.Core_Kernel_Record with
   record
      Tools   : Tools_List.List;
      --  The tools registered in the kernel

      Actions : Root_Table_Access;
      --  The actions registered in the kernel

      Styles : Root_Table_Access;
      --  The styles registered in the kernel

      Startup_Scripts : Root_Table_Access;
      --  The list of startup scripts and whether they should be loaded

      Hooks : Hooks_Hash.Instance;
      --  The hooks registered in the kernel

      Action_Filters : Action_Filters_Htable.String_Hash_Table.Instance;
      All_Action_Filters : Action_Filters_List.List;
      --  The action contexts registered in the kernel

      Application : access Gtk_Application_Record'Class;
      Main_Window : access Gtk.Window.Gtk_Window_Record'Class;
      --  The main GPS window
      --  We used to store the id of the main window (from the application),
      --  but this gets invalid early when GPS exists, and we no longer have
      --  access to the main window while destroying its children

      GNAT_Version : GNAT.Strings.String_Access;
      --  Full GNAT Version, if relevant

      Preferences : Default_Preferences.Preferences_Manager;
      --  The current setting for the preferences

      Perma_Commands : Commands.Command_Lists.List;
      --  The list of global commands associated with menus, actions or
      --  contextual menus, so that they can be freed on exit. These commands
      --  are automatically added to the list when the menu or action is
      --  created

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

      Open_Files : GNATCOLL.VFS.File_Array_Access;
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

      Messages_Container : System.Address := System.Null_Address;
      --  The message container for this instance of kernel

      Key_Setter_Function : Key_Setter;
      Key_Getter_Function : Key_Getter;
      Key_Getter_Simple_Function : Key_Getter_Simple;
      --  The function to set or retrieve default keys

      Refactoring : Standard.Refactoring.Factory_Context;

      Messages : Abstract_Messages_Window_Access;

      Launcher : aliased GPS_Process_Launcher_Record;
      --  External process launcher

      Pending_Messages : Ada.Strings.Unbounded.Unbounded_String
         := Ada.Strings.Unbounded.Null_Unbounded_String;
      --  Messages that should be inserted in the Messages window.
      --  We use this to store messages until the Messages window is
      --  created.

      Contextual_Menu_Open : Boolean := False;

      Check_Monitored_Files_Dialog : Gtk.Dialog.Gtk_Dialog := null;
      --  The dialog that let's users synchronize edited files with the disk.

      Check_Monitored_Files_Id : Glib.Main.G_Source_Id :=
         Glib.Main.No_Source_Id;
      --  An idle callback  used to check whether any file currently edited
      --  has been changed on disk.

      Env : Environment;
      --  List of environment variables overwritten by GPS
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
