-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2008, AdaCore                  --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with GNATCOLL.Scripts;   use GNATCOLL.Scripts;
with GNAT.Strings;
with Basic_Types;

with GPS.Kernel.Hooks;   use GPS.Kernel.Hooks;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with Gdk.Pixbuf;
with Commands;
with Entities;
with Projects;

package GPS.Kernel.Standard_Hooks is

   -----------------
   -- GPS Started --
   -----------------

   GPS_Started_Hook : constant Hook_Name := "gps_started";

   -----------
   -- Hooks --
   -----------

   File_Hook_Type : constant Hook_Type := "file_hooks";
   type File_Hooks_Args is new Hooks_Data with record
      File : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
   end record;
   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access File_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  Base type for hooks that take a single file in parameter
   --  See inherited doc

   String_Hook_Type : constant Hook_Type := "string_hooks";
   type String_Hooks_Args (Length : Natural) is new Hooks_Data with record
      Value : String (1 .. Length);
   end record;
   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access String_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;

   procedure Run_String_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Hook   : Hook_Name;
      Data   : String);
   --  Hooks that take a single string as a parameter.
   --  To create such hooks, use GPS.Kernel.Hooks.Register_Hook with a
   --  Type_Name parameter of String_Hook_Type
   --  See inherited doc

   String_Boolean_Hook_Type : constant Hook_Type := "string_boolean_hooks";
   type String_Boolean_Hooks_Args (Length : Natural) is new
     String_Hooks_Args (Length) with
      record
         Bool : Boolean;
      end record;
   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access String_Boolean_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  Hooks that take a string and a boolean as a parameter.
   --  To create such hooks, use GPS.Kernel.Hooks.Register_Hook with a
   --  Type_Name parameter of String_Boolean_Hook_Type
   --  See inherited doc

   Compilation_Hook_Type : constant Hook_Type := "compilation_hook";
   type Compilation_Hooks_Args (Length : Natural) is new
     String_Hooks_Args (Length) with
      record
         Quiet  : Boolean;
         Shadow : Boolean;
      end record;
   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Compilation_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  Hooks that take a string and two booleans as a parameter.
   --  To create such hooks, use GPS.Kernel.Hooks.Register_Hook with a
   --  Type_Name parameter of String_Boolean_Hook_Type
   --  See inherited doc

   Project_Hook_Type : constant Hook_Type := "project_hooks";
   type Project_Hooks_Args is new Hooks_Data with record
      Project : Projects.Project_Type;
   end record;
   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Project_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  Hooks that take a single project as a parameter.
   --  To create such hooks, use GPS.Kernel.Hooks.Register_Hook with a
   --  Type_Name parameter of Project_Hook_Type.
   --  See inherited documentation

   Context_Hook_Type : constant Hook_Type := "context_hooks";
   type Context_Hooks_Args is new Hooks_Data with record
      Context : GPS.Kernel.Selection_Context;
   end record;
   --  Base type for hooks that take a single context in parameter

   ------------------
   -- Marker_Hooks --
   ------------------

   Marker_Hook_Type : constant Hook_Type := "marker_hooks";
   type Marker_Hooks_Args is new Hooks_Data with record
      Marker : Location_Marker;
   end record;
   type Marker_Hooks_Args_Access is access all Marker_Hooks_Args'Class;
   --  These hooks contain a marker

   --------------------------
   --  File_Location_Hooks --
   --------------------------

   File_Location_Hook_Type : constant Hook_Type := "file_location_hooks";

   type File_Location_Hooks_Args is new File_Hooks_Args with record
      Line   : Natural;
      Column : Natural;
      --  ??? What is the meaning of Column here, user column or char offset?
   end record;
   type File_Location_Hooks_Args_Access is access all
     File_Location_Hooks_Args'Class;
   --  These hooks contains a location inside a source editor

   function Compute_Parent_Entity
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access File_Location_Hooks_Args)
      return Entities.Entity_Information;
   --  Return the name of the entity enclosing the location. This is either
   --  a subprogram, a package, ...
   --  The result of this call will generally be cached in the arguments, so
   --  that multiple calls are not more costly than one call.

   Location_Changed_Hook : constant Hook_Name := "location_changed";
   --  Hook called when the location in the current editor has changed. Its
   --  arguments are of type File_Location_Hooks_Args'Class

   ------------------------
   -- Files_2_Hooks --
   ------------------------

   Files_2_Hook_Type : constant Hook_Type := "file_renamed_hooks";
   type Files_2_Hooks_Args is new File_Hooks_Args with record
      Renamed : GNATCOLL.VFS.Virtual_File;
   end record;
   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Files_2_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  Base type for hooks that take two files in parameter
   --  See inherited doc

   ------------------
   -- Action hooks --
   ------------------
   --  The following are like standard hooks. However, they should be used
   --  through Run_Hook_Until_Success, so that only one module at a time
   --  reacts to them.
   --  Modules that can handle some of these action hook requests should
   --  call Add_Hook appropriately.
   --  Modules that wish to emit such requests should use
   --  Run_Hook_Until_Success. They all have a simple function associated
   --  to ease calls.

   procedure Register_Action_Hooks
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register all hooks defined in this package

   -----------------
   -- Exiting GPS --
   -----------------

   type Exit_Before_Action_Hooks_Args is new Hooks_Data with null record;
   Before_Exit_Action_Hook      : constant Hook_Name :=
                                    "before_exit_action_hook";
   Before_Exit_Action_Hook_Type : constant Hook_Type :=
                                    Hook_Type (Before_Exit_Action_Hook);
   --  Hook functions return a boolean

   procedure Exit_GPS
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Status : Integer := 0);
   --  Run the hook Exit_Before_Action_Hook, and exit GPS if the hook
   --  terminates.
   --  Status is the exit status

   ------------
   -- Macros --
   ------------

   Stop_Macro_Action_Hook : constant Hook_Name := "stop_macro_action_hook";
   --  Requests that the macro currently being replayed be stopped.

   procedure Stop_Macro
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Requests that the macro currently being replayed be stopped.

   -----------------------------
   -- Source_File_Action_Hook --
   -----------------------------

   type Source_File_Hooks_Args is new Hooks_Data with record
      File              : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Line              : Integer := 1;
      Column            : Basic_Types.Visible_Column_Type := 1;
      Column_End        : Basic_Types.Visible_Column_Type := 0;
      Enable_Navigation : Boolean := True;
      New_File          : Boolean := True;
      Force_Reload      : Boolean := False;
      Focus             : Boolean := True;
      Group             : Gtkada.MDI.Child_Group := Gtkada.MDI.Group_Default;
      Initial_Position  : Gtkada.MDI.Child_Position :=
                            Gtkada.MDI.Position_Automatic;
   end record;
   --  Line and column indicate the location to display initially, and are
   --  ignored if left to 0 (in which case any existing editor will be left
   --  at its current location). If Line is set to -1, this closes all file
   --  editors that correspond to this file.
   --  If Column_End is not 0, the area between Column and Column_End will be
   --  highlighted.
   --  if Enable_Navigation is True, the location will be stored for
   --  navigation with Back/Forward.
   --  If New_File is true, new files will be created if needed.
   --  If Force_Reload is True, this forces a reload of the file if there is
   --  already an open editor for it.
   --  Focus indicates whether the MDI child containing the editor should be
   --  given the focus.

   Open_File_Action_Hook : constant Hook_Name := "open_file_action_hook";
   --  This hook requests the opening of an editor. This could be either an
   --  internal editor or an external editor.

   procedure Open_File_Editor
     (Kernel            : access Kernel_Handle_Record'Class;
      Filename          : GNATCOLL.VFS.Virtual_File;
      Line              : Natural := 1;
      Column            : Basic_Types.Visible_Column_Type := 1;
      Column_End        : Basic_Types.Visible_Column_Type := 0;
      Enable_Navigation : Boolean := True;
      New_File          : Boolean := True;
      Force_Reload      : Boolean := False;
      Focus             : Boolean := True;
      Group             : Gtkada.MDI.Child_Group := Gtkada.MDI.Group_Default;
      Initial_Position  : Gtkada.MDI.Child_Position :=
        Gtkada.MDI.Position_Automatic);
   --  Call Open_File_Action_Hook

   procedure Clear_Highlighting
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename : GNATCOLL.VFS.Virtual_File);
   --  If Filename is currently open, clear all highlighting currently
   --  associated to it.
   --  See Open_File_Action_Hook

   procedure Close_File_Editors
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename : GNATCOLL.VFS.Virtual_File);
   --  Close all file editors that edit Filename.
   --  Filename must be an absolute file name.
   --  See Open_File_Action_Hook

   ----------------------------
   --  File_Line_Action_Hook --
   ----------------------------

   type Line_Information_Record is record
      Text               : GNAT.Strings.String_Access := null;
      Tooltip_Text       : GNAT.Strings.String_Access := null;
      --  A text to be displayed in a tooltip
      Image              : Gdk.Pixbuf.Gdk_Pixbuf := Gdk.Pixbuf.Null_Pixbuf;
      Associated_Command : Commands.Command_Access := null;
   end record;
   --  Text must be a valid UTF8 string, which may contain markups in the pango
   --  markup format.

   Empty_Line_Information : constant Line_Information_Record;

   type Line_Information_Array is array (Integer range <>)
     of Line_Information_Record;

   type Line_Information_Data is access Line_Information_Array;
   for Line_Information_Data'Size use Standard'Address_Size;

   type File_Line_Hooks_Args (Identifier_Length : Natural)
     is new Hooks_Data with
   record
      File       : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Info       : Line_Information_Data;
      Every_Line : Boolean := True;
      Normalize  : Boolean := True;
      Identifier : String (1 .. Identifier_Length);
   end record;
   --  Identifier is the identity of the emitted
   --  If Every_Line is set to True, then the editor will emit a line_revealed
   --  signal until all lines for this column are filled.
   --  If File is No_File, then the column will be created for all open files.
   --  If Normalize is True, the file name will be normalized.

   File_Line_Action_Hook : constant Hook_Name := "file_line_action_hook";
   --  Requests dealing with the column on the side of the editors

   procedure Create_Line_Information_Column
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Identifier : String;
      Every_Line : Boolean := True;
      Normalize  : Boolean := True);
   --  Request the creation of a column on the side of some editors.
   --  See File_Line_Action_Hook

   procedure Remove_Line_Information_Column
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Identifier : String);
   --  Remove the column identified by Identifier for the editors of File.
   --  If File is empty, then the column will be removed for all open files.
   --  See File_Line_Action_Hook

   procedure Add_Line_Information
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Identifier : String;
      Info       : Line_Information_Data;
      Normalize  : Boolean := True);
   --  Add line information to File.
   --  The range of Info must correspond to the range of line numbers
   --  that are to be modified.
   --  If Normalize is True, the file name will be normalized.
   --  See File_Line_Action_Hook
   --  Infos must be freed by caller using Unchecked_Free. The actual contents
   --  will be freed by the editor.

   procedure Add_Editor_Label
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Identifier : String;
      Label      : String);
   --  Add a label in the editors for File.
   --  See File_Line_Action_Hook

   procedure Free (X : in out Line_Information_Record);
   --  Free memory associated with X

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Line_Information_Array, Line_Information_Data);

   function To_Line_Information is new Ada.Unchecked_Conversion
     (System.Address, Line_Information_Data);
   function To_Address is new Ada.Unchecked_Conversion
     (Line_Information_Data, System.Address);

   --------------------------
   -- Location_Action_Hook --
   --------------------------

   type Action_Item is access Line_Information_Record;

   function To_Action_Item is new Ada.Unchecked_Conversion
     (System.Address, Action_Item);
   function To_Address is new Ada.Unchecked_Conversion
     (Action_Item, System.Address);

   procedure Free (X : in out Action_Item);
   --  Free memory associated to X

   type Location_Hooks_Args (Ident_Length, Cat_Length, Mes_Length : Natural) is
     new Hooks_Data with
      record
         File       : GNATCOLL.VFS.Virtual_File;
         Line       : Integer;
         Column     : Integer;
         Action     : Action_Item;
         Identifier : String (1 .. Ident_Length);
         Category   : String (1 .. Cat_Length);
         Message    : UTF8_String (1 .. Mes_Length);
      end record;
   --  Identifier is the identity of the emitted

   Location_Action_Hook : constant Hook_Name := "location_action_hook";

   procedure Add_Location_Action
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Identifier : String;
      Category   : String;
      File       : GNATCOLL.VFS.Virtual_File;
      Line       : Integer;
      Column     : Integer;
      Message    : UTF8_String;
      Action     : Action_Item);
   --  Add an action to the location specified. This will show up on the left
   --  side of the result view.
   --  See Location_Action_Hook

   procedure Remove_Location_Action
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Identifier : String;
      Category   : String;
      File       : GNATCOLL.VFS.Virtual_File;
      Line       : Integer;
      Column     : Integer;
      Message    : UTF8_String);
   --  Remove action corresponding to Identifier at specified location.
   --  See Location_Action_Hook.

   ----------------------
   -- Html_Action_Hook --
   ----------------------

   type Html_Hooks_Args (URL_Length, Anchor_Length : Natural) is new Hooks_Data
   with record
      Enable_Navigation : Boolean := True;
      URL_Or_File       : String (1 .. URL_Length);
      Anchor            : String (1 .. Anchor_Length);
   end record;

   Html_Action_Hook : constant Hook_Name := "html_action_hook";

   procedure Open_Html
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      URL_Or_File       : String;
      Enable_Navigation : Boolean := True);
   --  Open, or create, an html viewer for URL or file (Mime_Html_File type)
   --  If Enable_Navigation is True, then the location visited will be
   --  stored in the history for Back/Forward navigation.
   --  Filename can contain only a base name, and will be fully resolved by
   --  listeners.
   --  See Html_Action_Hook

   ----------------------
   -- Diff_Action_Hook --
   ----------------------

   type Diff_Hooks_Args is new Hooks_Data with record
      Orig_File : GNATCOLL.VFS.Virtual_File;
      New_File  : GNATCOLL.VFS.Virtual_File;
      Diff_File : GNATCOLL.VFS.Virtual_File;
   end record;

   Diff_Action_Hook : constant Hook_Name := "diff_action_hook";

   procedure Display_Differences
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Orig_File : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      New_File  : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Diff_File : GNATCOLL.VFS.Virtual_File);
   --  Display differences between Orig_File and New_File (Mime_Diff_File type)
   --  Either Orig_File or New_File can be null (but not both), in which
   --  case, the contents of the file is computed from the other file and the
   --  diff file.

   -------------------------
   -- Status_Changed_Hook --
   -------------------------

   type File_Status is (Modified, Unmodified, Unsaved, Saved);

   type File_Status_Changed_Hooks_Args is new File_Hooks_Args with record
      Status : File_Status;
   end record;

   File_Status_Changed_Action_Hook : constant Hook_Name :=
                                       "file_status_changed_action_hook";

   procedure File_Status_Changed
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Status : File_Status);
   --  Call the file_status_changed hook

private

   Empty_Line_Information : constant Line_Information_Record :=
                              (null, null, Gdk.Pixbuf.Null_Pixbuf, null);

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Context_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited doc

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access File_Line_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited doc

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Source_File_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited doc

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Location_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited doc

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Diff_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited doc

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Html_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited doc

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Exit_Before_Action_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited doc

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access File_Location_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited doc

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Marker_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited subprograms

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access File_Status_Changed_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited subprograms

end GPS.Kernel.Standard_Hooks;
