-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
--                            ACT-Europe                             --
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

with Glide_Kernel.Hooks;   use Glide_Kernel.Hooks;
with Glide_Kernel.Scripts; use Glide_Kernel.Scripts;
with VFS;                use VFS;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;
with System;
with Gdk.Pixbuf;
with Commands;

package Glide_Kernel.Standard_Hooks is

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
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register all hooks defined in this package

   -----------------------------
   -- Source_File_Action_Hook --
   -----------------------------

   type Source_File_Hooks_Args is new Hooks_Data with record
      File              : VFS.Virtual_File := VFS.No_File;
      Line              : Integer := 1;
      Column            : Natural := 1;
      Column_End        : Natural := 0;
      Enable_Navigation : Boolean := True;
      New_File          : Boolean := True;
      Force_Reload      : Boolean := False;
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

   function Get_Name (Data : Source_File_Hooks_Args) return String;
   function Execute_Shell
     (Script    : access Glide_Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : String;
      Hook_Name : String;
      Data      : Source_File_Hooks_Args) return Boolean;
   --  See inherited doc

   Open_File_Action_Hook : constant String := "open_file_action_hook";
   --  This hook requests the opening of an editor. This could be either an
   --  internal editor or an external editor.

   procedure Open_File_Editor
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      Filename          : VFS.Virtual_File;
      Line              : Natural := 1;
      Column            : Natural := 1;
      Column_End        : Natural := 0;
      Enable_Navigation : Boolean := True;
      New_File          : Boolean := True;
      Force_Reload      : Boolean := False);
   --  Calls Open_File_Action_Hook.

   procedure Clear_Highlighting
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Filename : VFS.Virtual_File);
   --  If Filename is currently open, clear all highlighting currently
   --  associated to it.
   --  See Open_File_Action_Hook

   procedure Close_File_Editors
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Filename : VFS.Virtual_File);
   --  Close all file editors that edit Filename.
   --  Filename must be an absolute file name.
   --  See Open_File_Action_Hook

   ----------------------------
   --  File_Line_Action_Hook --
   ----------------------------

   type Line_Information_Record is record
      Text               : GNAT.OS_Lib.String_Access := null;
      Image              : Gdk.Pixbuf.Gdk_Pixbuf := Gdk.Pixbuf.Null_Pixbuf;
      Associated_Command : Commands.Command_Access := null;
   end record;
   --  Text must be a valid UTF8 string, which may contain markups in the pango
   --  markup format.

   type Line_Information_Array is array (Integer range <>)
     of Line_Information_Record;

   type Line_Information_Data is access Line_Information_Array;
   for Line_Information_Data'Size use Standard'Address_Size;

   type File_Line_Hooks_Args (Identifier_Length : Natural)
     is new Hooks_Data with
   record
      Identifier     : String (1 .. Identifier_Length);
      File           : VFS.Virtual_File := VFS.No_File;
      Info           : Line_Information_Data;
      Stick_To_Data  : Boolean := True;
      Every_Line     : Boolean := True;
      Normalize      : Boolean := True;
   end record;
   --  Identifier is the identity of the emitted
   --  If Stick_To_Data is set to True, then the line information is relative
   --  to the original data in the file, otherwise it is relative to the lines
   --  in the view.
   --  If Every_Line is set to True, then the editor will emit a line_revealed
   --  signal until all lines for this column are filled.
   --  If File is No_File, then the column will be created for all open files.
   --  If Normalize is True, the file name will be normalized.

   function Get_Name (Data : File_Line_Hooks_Args) return String;
   function Execute_Shell
     (Script    : access Glide_Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : String;
      Hook_Name : String;
      Data      : File_Line_Hooks_Args) return Boolean;
   --  See inherited doc

   File_Line_Action_Hook : constant String := "file_line_action_hook";
   --  Requests dealing with the column on the side of the editors

   procedure Create_Line_Information_Column
     (Kernel         : access Glide_Kernel.Kernel_Handle_Record'Class;
      File           : VFS.Virtual_File;
      Identifier     : String;
      Stick_To_Data  : Boolean := True;
      Every_Line     : Boolean := True;
      Normalize      : Boolean := True);
   --  Request the creation of a column on the side of some editors.
   --  See File_Line_Action_Hook

   procedure Remove_Line_Information_Column
     (Kernel         : access Glide_Kernel.Kernel_Handle_Record'Class;
      File           : VFS.Virtual_File;
      Identifier     : String);
   --  Remove the column identified by Identifier for the editors of File.
   --  If File is empty, then the column will be removed for all open files.
   --  See File_Line_Action_Hook

   procedure Add_Line_Information
     (Kernel         : access Glide_Kernel.Kernel_Handle_Record'Class;
      File           : VFS.Virtual_File;
      Identifier     : String;
      Info           : Line_Information_Data;
      Normalize      : Boolean := True);
   --  Add line information to File.
   --  The range of Info must correspond to the range of line numbers
   --  that are to be modified.
   --  If Normalize is True, the file name will be normalized.
   --  See File_Line_Action_Hook

   procedure Add_Editor_Label
     (Kernel     : access Glide_Kernel.Kernel_Handle_Record'Class;
      File       : VFS.Virtual_File;
      Identifier : String;
      Label      : String);
   --  Add a label in the editors for File.
   --  See File_Line_Action_Hook

   procedure Free (X : in out Line_Information_Record);
   --  Free memory associated with X.

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
   --  Free memory associated to X.

   type Location_Hooks_Args (Ident_Length, Cat_Length, Mes_Length : Natural)
      is new Hooks_Data with record
         Identifier    : String (1 .. Ident_Length);
         Category      : String (1 .. Cat_Length);
         File          : VFS.Virtual_File;
         Line          : Integer;
         Column        : Integer;
         Message       : String (1 .. Mes_Length);
         Action        : Action_Item;
      end record;
   --  Identifier is the identity of the emitted.

   function Get_Name (Data : Location_Hooks_Args) return String;
   function Execute_Shell
     (Script    : access Glide_Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : String;
      Hook_Name : String;
      Data      : Location_Hooks_Args) return Boolean;
   --  See inherited doc

   Location_Action_Hook : constant String := "location_action_hook";

   procedure Add_Location_Action
     (Kernel        : access Glide_Kernel.Kernel_Handle_Record'Class;
      Identifier    : String;
      Category      : String;
      File          : VFS.Virtual_File;
      Line          : Integer;
      Column        : Integer;
      Message       : String;
      Action        : Action_Item);
   --  Add an action to the location specified. This will show up on the left
   --  side of the result view.
   --  See Location_Action_Hook

   procedure Remove_Location_Action
     (Kernel        : access Glide_Kernel.Kernel_Handle_Record'Class;
      Identifier    : String;
      Category      : String;
      File          : VFS.Virtual_File;
      Line          : Integer;
      Column        : Integer;
      Message       : String);
   --  Remove action corresponding to Identifier at specified location.
   --  See Location_Action_Hook.

   ----------------------
   -- Html_Action_Hook --
   ----------------------

   type Html_Hooks_Args (Anchor_Length : Natural) is new Hooks_Data with record
      File              : VFS.Virtual_File;
      Enable_Navigation : Boolean := True;
      Anchor            : String (1 .. Anchor_Length);
   end record;

   function Get_Name (Data : Html_Hooks_Args) return String;
   function Execute_Shell
     (Script    : access Glide_Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : String;
      Hook_Name : String;
      Data      : Html_Hooks_Args) return Boolean;
   --  See inherited doc

   Html_Action_Hook : constant String := "html_action_hook";

   procedure Open_Html
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      Filename          : VFS.Virtual_File;
      Enable_Navigation : Boolean := True);
   --  Open, or create, an html viewer for Filename (Mime_Html_File type)
   --  If Enable_Navigation is True, then the location visited will be
   --  stored in the history for Back/Forward navigation.
   --  Filename can contain only a base name, and will be fully resolved by
   --  listeners.
   --  See Html_Action_Hook

   ----------------------
   -- Diff_Action_Hook --
   ----------------------

   type Diff_Hooks_Args is new Hooks_Data with record
      Orig_File : VFS.Virtual_File;
      New_File  : VFS.Virtual_File;
      Diff_File : VFS.Virtual_File;
   end record;

   function Get_Name (Data : Diff_Hooks_Args) return String;
   function Execute_Shell
     (Script    : access Glide_Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : String;
      Hook_Name : String;
      Data      : Diff_Hooks_Args) return Boolean;
   --  See inherited doc

   Diff_Action_Hook : constant String := "diff_action_hook";

   procedure Display_Differences
     (Kernel         : access Glide_Kernel.Kernel_Handle_Record'Class;
      Orig_File      : VFS.Virtual_File := VFS.No_File;
      New_File       : VFS.Virtual_File := VFS.No_File;
      Diff_File      : VFS.Virtual_File);
   --  Display differences between Orig_File and New_File (Mime_Diff_File type)
   --  Either Orig_File or New_File can be null (but not both), in which
   --  case, the contents of the file is computed from the other file and the
   --  diff file.

end Glide_Kernel.Standard_Hooks;
