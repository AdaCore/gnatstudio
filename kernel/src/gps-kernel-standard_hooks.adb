-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2005                       --
--                              AdaCore                              --
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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;       use GNAT.OS_Lib;
with System;            use System;

with Gdk.Pixbuf;        use Gdk.Pixbuf;

with Commands;          use Commands;
with GPS.Kernel.Scripts;
with GPS.Kernel;        use GPS.Kernel;
with Traces;            use Traces;
with VFS;

package body GPS.Kernel.Standard_Hooks is

   Me : constant Debug_Handle := Create ("Standard_Hooks");

   Open_File_Hook_Type     : constant String := "open_file_action_hooks";
   Before_Exit_Hook_Type   : constant String := "before_exit_action_hooks";
   File_Line_Hook_Type     : constant String := "location_action_hooks";
   Location_Hook_Type      : constant String := "location_action_hooks";
   Html_Hook_Type          : constant String := "html_action_hooks";
   Diff_Hook_Type          : constant String := "diff_hooks";
   File_Hook_Type          : constant String := "file_hooks";
   Context_Hook_Type       : constant String := "context_hooks";
   Compilation_Hook_Type   : constant String := "compilation_hooks";
   File_Location_Hook_Type : constant String := "file_location_hooks";
   --  The various names to describe the hook types defined in this package

   procedure General_Line_Information
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : Virtual_File;
      Identifier     : String;
      Info           : Line_Information_Data;
      Every_Line     : Boolean := True;
      Normalize      : Boolean := True);
   --  Create the Mime info for adding/creating/removing line information,
   --  and send it.
   --  If File is an empty string, send the Mime for all open buffers.

   procedure Before_Exit_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Open_File_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure String_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Project_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Line_Information_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Location_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Html_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Diff_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure File_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Context_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Location_Changed_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles calls to run_hook from the shell for the various hooks

   ---------------------------
   -- Compute_Parent_Entity --
   ---------------------------

   function Compute_Parent_Entity
     (Data : access File_Location_Hooks_Args)
      return Entities.Entity_Information
   is
      pragma Unreferenced (Data);
   begin
      return null;
   end Compute_Parent_Entity;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Action_Item) is
      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (Line_Information_Record, Action_Item);
   begin
      if X /= null then
         Free (X.all);
         Unchecked_Free (X);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Line_Information_Record) is
   begin
      Free (X.Text);

      if X.Associated_Command /= null then
         Destroy (X.Associated_Command);
      end if;
   end Free;

   ------------------------------
   -- General_Line_Information --
   ------------------------------

   procedure General_Line_Information
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : Virtual_File;
      Identifier     : String;
      Info           : Line_Information_Data;
      Every_Line     : Boolean := True;
      Normalize      : Boolean := True)
   is
      Data : aliased File_Line_Hooks_Args :=
        (Kernel            => Kernel_Handle (Kernel),
         Identifier_Length => Identifier'Length,
         Identifier        => Identifier,
         File              => File,
         Info              => Info,
         Every_Line        => Every_Line,
         Normalize         => Normalize);
   begin
      if File /= VFS.No_File then
         if not Run_Hook_Until_Success
           (Kernel, File_Line_Action_Hook, Data'Unchecked_Access,
            Set_Busy => False)
         then
            Trace (Me, "No file editor with line info display "
                   & "capability was registered");
         end if;

      else
         declare
            Files : constant VFS.File_Array := Open_Files (Kernel);
         begin
            for Node in Files'Range loop
               Data.File := Files (Node);

               if not Run_Hook_Until_Success
                 (Kernel, File_Line_Action_Hook, Data'Unchecked_Access,
                  Set_Busy => False)
               then
                  Trace (Me, "No file editor with line info display "
                         & "capability was registered");
               end if;
            end loop;
         end;
      end if;
   end General_Line_Information;

   ----------------------
   -- Add_Editor_Label --
   ----------------------

   procedure Add_Editor_Label
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : Virtual_File;
      Identifier : String;
      Label      : String)
   is
      Infos  : Line_Information_Data;

   begin
      Infos := new Line_Information_Array (-1 .. -1);
      Infos (-1).Text := new String'(Label);

      Add_Line_Information
        (Kernel,
         File,
         Identifier,
         Infos);
   end Add_Editor_Label;

   ------------------------------------
   -- Create_Line_Information_Column --
   ------------------------------------

   procedure Create_Line_Information_Column
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : Virtual_File;
      Identifier     : String;
      Every_Line     : Boolean := True;
      Normalize      : Boolean := True)
   is
      A_Access : Line_Information_Data;
   begin
      A_Access := new Line_Information_Array (0 .. 0);

      General_Line_Information
        (Kernel,
         File,
         Identifier,
         A_Access,
         Every_Line,
         Normalize);
      Unchecked_Free (A_Access);
   end Create_Line_Information_Column;

   ------------------------------------
   -- Remove_Line_Information_Column --
   ------------------------------------

   procedure Remove_Line_Information_Column
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : Virtual_File;
      Identifier     : String)
   is
      A : Line_Information_Array (1 .. 0);
   begin
      General_Line_Information (Kernel, File, Identifier,
                                new Line_Information_Array'(A));
   end Remove_Line_Information_Column;

   --------------------------
   -- Add_Line_Information --
   --------------------------

   procedure Add_Line_Information
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : Virtual_File;
      Identifier     : String;
      Info           : Line_Information_Data;
      Normalize      : Boolean := True) is
   begin
      General_Line_Information
        (Kernel, File, Identifier, Info, Normalize => Normalize);
   end Add_Line_Information;

   -------------------------
   -- Add_Location_Action --
   -------------------------

   procedure Add_Location_Action
     (Kernel        : access Kernel_Handle_Record'Class;
      Identifier    : String;
      Category      : String;
      File          : Virtual_File;
      Line          : Integer;
      Column        : Integer;
      Message       : String;
      Action        : Action_Item)
   is
      Data : aliased Location_Hooks_Args :=
        (Kernel       => Kernel_Handle (Kernel),
         Ident_Length => Identifier'Length,
         Identifier   => Identifier,
         Cat_Length   => Category'Length,
         Category     => Category,
         File         => File,
         Line         => Line,
         Column       => Column,
         Mes_Length   => Message'Length,
         Message      => Message,
         Action       => Action);
   begin
      if not Run_Hook_Until_Success
        (Kernel, Location_Action_Hook, Data'Unchecked_Access,
         Set_Busy => False)
      then
         Trace (Me, "No location viewer registered.");
      end if;
   end Add_Location_Action;

   ----------------------------
   -- Remove_Location_Action --
   ----------------------------

   procedure Remove_Location_Action
     (Kernel        : access Kernel_Handle_Record'Class;
      Identifier    : String;
      Category      : String;
      File          : Virtual_File;
      Line          : Integer;
      Column        : Integer;
      Message       : String) is
   begin
      Add_Location_Action
        (Kernel, Identifier,
         Category, File, Line, Column, Message, null);
   end Remove_Location_Action;

   ------------------------
   -- Clear_Highlighting --
   ------------------------

   procedure Clear_Highlighting
     (Kernel   : access Kernel_Handle_Record'Class;
      Filename : Virtual_File) is
   begin
      if Is_Open (Kernel, Filename) then
         Open_File_Editor
           (Kernel,
            Filename,
            0, 0,
            Enable_Navigation => False);
      end if;
   end Clear_Highlighting;

   ----------------------
   -- Open_File_Editor --
   ----------------------

   procedure Open_File_Editor
     (Kernel            : access Kernel_Handle_Record'Class;
      Filename          : VFS.Virtual_File;
      Line              : Natural := 1;
      Column            : Natural := 1;
      Column_End        : Natural := 0;
      Enable_Navigation : Boolean := True;
      New_File          : Boolean := True;
      Force_Reload      : Boolean := False;
      Focus             : Boolean := True;
      Group             : Gtkada.MDI.Child_Group := Gtkada.MDI.Group_Default;
      Initial_Position  : Gtkada.MDI.Child_Position :=
        Gtkada.MDI.Position_Automatic)
   is
      Data : aliased Source_File_Hooks_Args :=
        (Kernel            => Kernel_Handle (Kernel),
         File              => Filename,
         Line              => Line,
         Column            => Column,
         Column_End        => Column_End,
         Enable_Navigation => Enable_Navigation,
         New_File          => New_File,
         Force_Reload      => Force_Reload,
         Focus             => Focus,
         Group             => Group,
         Initial_Position  => Initial_Position);
   begin
      if not Run_Hook_Until_Success
        (Kernel, Open_File_Action_Hook, Data'Unchecked_Access)
      then
         Trace (Me, "No file editor was registered");
      end if;
   end Open_File_Editor;

   ------------------------
   -- Close_File_Editors --
   ------------------------

   procedure Close_File_Editors
     (Kernel   : access Kernel_Handle_Record'Class;
      Filename : Virtual_File)
   is
      Data : aliased Source_File_Hooks_Args :=
        (Kernel            => Kernel_Handle (Kernel),
         File              => Filename,
         Line              => -1,
         Column            => 0,
         Column_End        => 0,
         Enable_Navigation => False,
         New_File          => False,
         Force_Reload      => False,
         Focus             => False,
         Group             => Gtkada.MDI.Group_Default,
         Initial_Position  => Gtkada.MDI.Position_Automatic);
   begin
      if not Run_Hook_Until_Success
        (Kernel, Open_File_Action_Hook, Data'Unchecked_Access)
      then
         Trace (Me, "No file editor was registered");
      end if;
   end Close_File_Editors;

   ---------------
   -- Open_Html --
   ---------------

   procedure Open_Html
     (Kernel            : access Kernel_Handle_Record'Class;
      Filename          : Virtual_File;
      Enable_Navigation : Boolean := True)
   is
      Full   : constant String := Full_Name (Filename).all;
      Anchor : Natural := Index (Full, "#");
   begin
      if Anchor = 0 then
         Anchor := Full'Last + 1;
      end if;

      declare
         Data : aliased Html_Hooks_Args :=
           (Kernel            => Kernel_Handle (Kernel),
            Anchor_Length     => Integer'Max (0, Full'Last - Anchor),
            File              => Create (Full (Full'First .. Anchor - 1)),
            Enable_Navigation => Enable_Navigation,
            Anchor            => Full (Anchor + 1 .. Full'Last));
      begin
         if not Run_Hook_Until_Success
           (Kernel, Html_Action_Hook, Data'Unchecked_Access, False)
         then
            Trace (Me, "No html viewer was registered");
         end if;
      end;
   end Open_Html;

   -------------------------
   -- Display_Differences --
   -------------------------

   procedure Display_Differences
     (Kernel    : access Kernel_Handle_Record'Class;
      Orig_File : Virtual_File := VFS.No_File;
      New_File  : Virtual_File := VFS.No_File;
      Diff_File : Virtual_File)
   is
      Data : aliased Diff_Hooks_Args :=
        (Kernel_Handle (Kernel), Orig_File, New_File, Diff_File);
   begin
      if not Run_Hook_Until_Success
        (Kernel, Diff_Action_Hook, Data'Unchecked_Access)
      then
         Trace (Me, "No diff viewer registered");
      end if;
   end Display_Differences;

   --------------------------------
   -- Open_File_Run_Hook_Handler --
   --------------------------------

   procedure Open_File_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Name   : constant String := Get_Hook_Name (Data, 1);
      Args   : aliased Source_File_Hooks_Args;
      pragma Unreferenced (Command);
   begin
      Args := (Kernel            => Kernel,
               File              => Get_File
                 (Get_Data (Nth_Arg (Data, 2, Get_File_Class (Kernel)))),
               Line              => Nth_Arg (Data, 3),
               Column            => Nth_Arg (Data, 4),
               Column_End        => Nth_Arg (Data, 5),
               Enable_Navigation => Nth_Arg (Data, 6),
               New_File          => Nth_Arg (Data, 7),
               Force_Reload      => Nth_Arg (Data, 8),
               Focus             => Nth_Arg (Data, 9, True),
               Group             => Gtkada.MDI.Child_Group
                  (Nth_Arg (Data, 11, Natural (Gtkada.MDI.Group_Default))),
               Initial_Position   => Gtkada.MDI.Child_Position'Val
                  (Nth_Arg (Data, 10,
                   Gtkada.MDI.Child_Position'Pos
                     (Gtkada.MDI.Position_Automatic))));
      Set_Return_Value
        (Data, Run_Hook_Until_Success (Kernel, Name, Args'Unchecked_Access));
   end Open_File_Run_Hook_Handler;

   ----------------------------------
   -- Before_Exit_Run_Hook_Handler --
   ----------------------------------

   procedure Before_Exit_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Name   : constant String := Get_Hook_Name (Data, 1);
      Args   : aliased Exit_Before_Action_Hooks_Args := (Kernel => Kernel);
      pragma Unreferenced (Command);
   begin
      Set_Return_Value
        (Data, Run_Hook_Until_Failure (Kernel, Name, Args'Unchecked_Access));
   end Before_Exit_Run_Hook_Handler;

   --------------
   -- Exit_GPS --
   --------------

   procedure Exit_GPS
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Data : aliased Exit_Before_Action_Hooks_Args :=
         (Kernel => Kernel_Handle (Kernel));
   begin
      if Run_Hook_Until_Failure
        (Kernel, Before_Exit_Action_Hook, Data'Unchecked_Access)
      then
         Gtk.Main.Main_Quit;
      end if;
   end Exit_GPS;

   ---------------------------------------
   -- Line_Information_Run_Hook_Handler --
   ---------------------------------------

   procedure Line_Information_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Name       : constant String := Get_Hook_Name (Data, 1);
      Kernel     : constant Kernel_Handle := Get_Kernel (Data);
      Identifier : constant String := Nth_Arg (Data, 2);
      Args       : aliased  File_Line_Hooks_Args :=
                     (Kernel            => Kernel,
                      Identifier_Length => Identifier'Length,
                      Identifier        => Identifier,
                      File              => Get_File
                        (Get_Data
                           (Nth_Arg (Data, 3, Get_File_Class (Kernel)))),
                      Info              => null,
                      Every_Line        => Nth_Arg (Data, 4),
                      Normalize         => Nth_Arg (Data, 5));
      pragma Unreferenced (Command);
   begin
      Set_Return_Value
        (Data, Run_Hook_Until_Success (Kernel, Name, Args'Unchecked_Access));
   end Line_Information_Run_Hook_Handler;

   -------------------------------
   -- Location_Run_Hook_Handler --
   -------------------------------

   procedure Location_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Name       : constant String := Get_Hook_Name (Data, 1);
      Kernel     : constant Kernel_Handle := Get_Kernel (Data);
      Identifier : constant String := Nth_Arg (Data, 2);
      Category   : constant String := Nth_Arg (Data, 3);
      Message    : constant String := Nth_Arg (Data, 7);
      Args       : aliased Location_Hooks_Args :=
                     (Kernel            => Kernel,
                      Ident_Length      => Identifier'Length,
                      Identifier        => Identifier,
                      Cat_Length        => Category'Length,
                      Category          => Category,
                      File              => Get_File
                        (Get_Data
                           (Nth_Arg (Data, 4, Get_File_Class (Kernel)))),
                      Line              => Nth_Arg (Data, 5),
                      Column            => Nth_Arg (Data, 6),
                      Mes_Length        => Message'Length,
                      Message           => Message,
                      Action            => null);
      pragma Unreferenced (Command);
   begin
      Set_Return_Value
        (Data, Run_Hook_Until_Success (Kernel, Name, Args'Unchecked_Access));
   end Location_Run_Hook_Handler;

   ---------------------------
   -- Html_Run_Hook_Handler --
   ---------------------------

   procedure Html_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Name   : constant String := Get_Hook_Name (Data, 1);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Anchor : constant String := Nth_Arg (Data, 4);
      Args   : aliased Html_Hooks_Args :=
                 (Kernel            => Kernel,
                  File              => Get_File
                    (Get_Data (Nth_Arg (Data, 2, Get_File_Class (Kernel)))),
                  Enable_Navigation => Nth_Arg (Data, 3),
                  Anchor_Length     => Anchor'Length,
                  Anchor            => Anchor);
      pragma Unreferenced (Command);
   begin
      Set_Return_Value
        (Data, Run_Hook_Until_Success (Kernel, Name, Args'Unchecked_Access));
   end Html_Run_Hook_Handler;

   ---------------------------
   -- Diff_Run_Hook_Handler --
   ---------------------------

   procedure Diff_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Name   : constant String := Get_Hook_Name (Data, 1);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Args   : aliased Diff_Hooks_Args;
      pragma Unreferenced (Command);
   begin
      Args :=
        (Kernel    => Kernel,
         Orig_File => Get_File
           (Get_Data (Nth_Arg (Data, 2, Get_File_Class (Kernel), True))),
         New_File  => Get_File
           (Get_Data (Nth_Arg (Data, 3, Get_File_Class (Kernel), True))),
         Diff_File => Get_File
           (Get_Data (Nth_Arg (Data, 4, Get_File_Class (Kernel), True))));
      Set_Return_Value
        (Data, Run_Hook_Until_Success (Kernel, Name, Args'Unchecked_Access));
   end Diff_Run_Hook_Handler;

   ---------------------------
   -- File_Run_Hook_Handler --
   ---------------------------

   procedure File_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Name   : constant String := Get_Hook_Name (Data, 1);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Args   : aliased File_Hooks_Args :=
                 (Kernel => Kernel,
                  File   => Get_File
                    (Get_Data
                       (Nth_Arg (Data, 2, Get_File_Class (Kernel), True))));
      pragma Unreferenced (Command);
   begin
      Run_Hook (Kernel, Name, Args'Unchecked_Access);
   end File_Run_Hook_Handler;

   --------------------------
   -- Project_Hook_Handler --
   --------------------------

   procedure Project_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Name   : constant String := Get_Hook_Name (Data, 1);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Args   : aliased Project_Hooks_Args :=
                 (Kernel  => Kernel,
                  Project => Get_Data (Data, 2));
      pragma Unreferenced (Command);
   begin
      Run_Hook (Kernel, Name, Args'Unchecked_Access);
   end Project_Hook_Handler;

   -------------------------
   -- String_Hook_Handler --
   -------------------------

   procedure String_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Name   : constant String := Get_Hook_Name (Data, 1);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Value  : constant String := Nth_Arg (Data, 2);
      Args   : aliased String_Hooks_Args :=
                 (Kernel => Kernel,
                  Length => Value'Length,
                  Value  => Value);
      pragma Unreferenced (Command);
   begin
      Run_Hook (Kernel, Name, Args'Unchecked_Access);
   end String_Hook_Handler;

   ------------------------------
   -- Context_Run_Hook_Handler --
   ------------------------------

   procedure Context_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Name   : constant String := Get_Hook_Name (Data, 1);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Args   : aliased Context_Hooks_Args :=
                 (Kernel, Context =>  Get_Data (Data, 2));
      pragma Unreferenced (Command);
   begin
      Run_Hook (Kernel, Name, Args'Unchecked_Access);
   end Context_Run_Hook_Handler;

   -----------------------------------
   -- Location_Changed_Hook_Handler --
   -----------------------------------

   procedure Location_Changed_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Name   : constant String := Get_Hook_Name (Data, 1);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Args   : aliased File_Location_Hooks_Args :=
                 (Kernel => Kernel,
                  File   => Get_File
                    (Get_Data
                       (Nth_Arg (Data, 2, Get_File_Class (Kernel), True))),
                  Line   => Nth_Arg (Data, 3),
                  Column => Nth_Arg (Data, 4));
      pragma Unreferenced (Command);
   begin
      Run_Hook (Kernel, Name, Args'Unchecked_Access);
   end Location_Changed_Hook_Handler;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Data : Context_Hooks_Args) return String is
      pragma Unreferenced (Data);
   begin
      return Context_Hook_Type;
   end Get_Name;

   -------------------
   -- Execute_Shell --
   -------------------

   function Execute_Shell
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : GPS.Kernel.Scripts.Subprogram_Type;
      Hook_Name : String;
      Data      : access Context_Hooks_Args) return Boolean
   is
      C   : constant Class_Instance := Create_Context (Script, Data.Context);
      D   : Callback_Data'Class := Create (Script, 2);
      Tmp : Boolean;
   begin
      Set_Nth_Arg (D, 1, Hook_Name);
      Set_Nth_Arg (D, 2, C);
      Tmp := Execute (Command, D);
      Free (C);
      Free (D);
      return Tmp;
   end Execute_Shell;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Data : String_Hooks_Args) return String is
      pragma Unreferenced (Data);
   begin
      return String_Hook_Type;
   end Get_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Data : Project_Hooks_Args) return String is
      pragma Unreferenced (Data);
   begin
      return Project_Hook_Type;
   end Get_Name;

   -------------------
   -- Execute_Shell --
   -------------------

   function Execute_Shell
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : GPS.Kernel.Scripts.Subprogram_Type;
      Hook_Name : String;
      Data      : access String_Hooks_Args) return Boolean
   is
      D   : Callback_Data'Class := Create (Script, 2);
      Tmp : Boolean;
   begin
      Set_Nth_Arg (D, 1, Hook_Name);
      Set_Nth_Arg (D, 2, Data.Value);
      Tmp := Execute (Command, D);
      Free (D);
      return Tmp;
   end Execute_Shell;

   -------------------
   -- Execute_Shell --
   -------------------

   function Execute_Shell
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : GPS.Kernel.Scripts.Subprogram_Type;
      Hook_Name : String;
      Data      : access Project_Hooks_Args) return Boolean
   is
      P   : constant Class_Instance := Create_Project (Script, Data.Project);
      D   : Callback_Data'Class := Create (Script, 2);
      Tmp : Boolean;
   begin
      Set_Nth_Arg (D, 1, Hook_Name);
      Set_Nth_Arg (D, 2, P);
      Tmp := Execute (Command, D);
      Free (P);
      Free (D);
      return Tmp;
   end Execute_Shell;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Data : File_Hooks_Args) return String is
      pragma Unreferenced (Data);
   begin
      return File_Hook_Type;
   end Get_Name;

   -------------------
   -- Execute_Shell --
   -------------------

   function Execute_Shell
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : GPS.Kernel.Scripts.Subprogram_Type;
      Hook_Name : String;
      Data      : access File_Hooks_Args) return Boolean
   is
      F   : constant Class_Instance := Create_File (Script, Data.File);
      D   : Callback_Data'Class := Create (Script, 2);
      Tmp : Boolean;
   begin
      Set_Nth_Arg (D, 1, Hook_Name);
      Set_Nth_Arg (D, 2, F);
      Tmp := Execute (Command, D);
      Free (F);
      Free (D);
      return Tmp;
   end Execute_Shell;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Data : Source_File_Hooks_Args) return String is
      pragma Unreferenced (Data);
   begin
      return Open_File_Hook_Type;
   end Get_Name;

   -------------------
   -- Execute_Shell --
   -------------------

   function Execute_Shell
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : GPS.Kernel.Scripts.Subprogram_Type;
      Hook_Name : String;
      Data      : access Source_File_Hooks_Args) return Boolean
   is
      F   : constant Class_Instance := Create_File (Script, Data.File);
      D   : Callback_Data'Class := Create (Script, 8);
      Tmp : Boolean;
   begin
      Set_Nth_Arg (D, 1, Hook_Name);
      Set_Nth_Arg (D, 2, F);
      Set_Nth_Arg (D, 3, Data.Line);
      Set_Nth_Arg (D, 4, Data.Column);
      Set_Nth_Arg (D, 5, Data.Column_End);
      Set_Nth_Arg (D, 6, Data.Enable_Navigation);
      Set_Nth_Arg (D, 7, Data.New_File);
      Set_Nth_Arg (D, 8, Data.Force_Reload);

      Tmp := Execute (Command, D);
      Free (F);
      Free (D);
      return Tmp;
   end Execute_Shell;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Data : File_Line_Hooks_Args) return String is
      pragma Unreferenced (Data);
   begin
      return File_Line_Hook_Type;
   end Get_Name;

   -------------------
   -- Execute_Shell --
   -------------------

   function Execute_Shell
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : GPS.Kernel.Scripts.Subprogram_Type;
      Hook_Name : String;
      Data      : access File_Line_Hooks_Args) return Boolean
   is
      F   : constant Class_Instance := Create_File (Script, Data.File);
      D   : Callback_Data'Class := Create (Script, 5);
      Tmp : Boolean;
   begin
      Set_Nth_Arg (D, 1, Hook_Name);
      Set_Nth_Arg (D, 2, Data.Identifier);
      Set_Nth_Arg (D, 3, F);
      Set_Nth_Arg (D, 4, Data.Every_Line);
      Set_Nth_Arg (D, 5, Data.Normalize);

      Tmp := Execute (Command, D);
      Free (F);
      Free (D);
      return Tmp;
   end Execute_Shell;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Data : Location_Hooks_Args) return String is
      pragma Unreferenced (Data);
   begin
      return Location_Hook_Type;
   end Get_Name;

   -------------------
   -- Execute_Shell --
   -------------------

   function Execute_Shell
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : GPS.Kernel.Scripts.Subprogram_Type;
      Hook_Name : String;
      Data      : access Location_Hooks_Args) return Boolean
   is
      F   : constant Class_Instance := Create_File (Script, Data.File);
      D   : Callback_Data'Class := Create (Script, 7);
      Tmp : Boolean;
   begin
      Set_Nth_Arg (D, 1, Hook_Name);
      Set_Nth_Arg (D, 2, Data.Identifier);
      Set_Nth_Arg (D, 3, Data.Category);
      Set_Nth_Arg (D, 4, F);
      Set_Nth_Arg (D, 5, Data.Line);
      Set_Nth_Arg (D, 6, Data.Column);
      Set_Nth_Arg (D, 7, Data.Message);

      Tmp := Execute (Command, D);
      Free (F);
      Free (D);
      return Tmp;
   end Execute_Shell;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Data : Html_Hooks_Args) return String is
      pragma Unreferenced (Data);
   begin
      return Html_Hook_Type;
   end Get_Name;

   -------------------
   -- Execute_Shell --
   -------------------

   function Execute_Shell
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : GPS.Kernel.Scripts.Subprogram_Type;
      Hook_Name : String;
      Data      : access Html_Hooks_Args) return Boolean
   is
      F   : constant Class_Instance := Create_File (Script, Data.File);
      D   : Callback_Data'Class := Create (Script, 4);
      Tmp : Boolean;
   begin
      Set_Nth_Arg (D, 1, Hook_Name);
      Set_Nth_Arg (D, 2, F);
      Set_Nth_Arg (D, 3, Data.Enable_Navigation);
      Set_Nth_Arg (D, 4, Data.Anchor);

      Tmp := Execute (Command, D);
      Free (F);
      Free (D);
      return Tmp;
   end Execute_Shell;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Data : File_Location_Hooks_Args) return String is
      pragma Unreferenced (Data);
   begin
      return File_Location_Hook_Type;
   end Get_Name;

   -------------------
   -- Execute_Shell --
   -------------------

   function Execute_Shell
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : GPS.Kernel.Scripts.Subprogram_Type;
      Hook_Name : String;
      Data      : access File_Location_Hooks_Args) return Boolean
   is
      F   : constant Class_Instance := Create_File (Script, Data.File);
      D   : Callback_Data'Class := Create (Script, 4);
      Tmp : Boolean;
   begin
      Set_Nth_Arg (D, 1, Hook_Name);
      Set_Nth_Arg (D, 2, F);
      Set_Nth_Arg (D, 3, Data.Line);
      Set_Nth_Arg (D, 4, Data.Column);

      Tmp := Execute (Command, D);
      Free (F);
      Free (D);
      return Tmp;
   end Execute_Shell;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Data : Diff_Hooks_Args) return String is
      pragma Unreferenced (Data);
   begin
      return Diff_Hook_Type;
   end Get_Name;

   -------------------
   -- Execute_Shell --
   -------------------

   function Execute_Shell
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : GPS.Kernel.Scripts.Subprogram_Type;
      Hook_Name : String;
      Data      : access Diff_Hooks_Args) return Boolean
   is
      F1  : constant Class_Instance := Create_File (Script, Data.Orig_File);
      F2  : constant Class_Instance := Create_File (Script, Data.New_File);
      F3  : constant Class_Instance := Create_File (Script, Data.Diff_File);
      D   : Callback_Data'Class := Create (Script, 4);
      Tmp : Boolean;
   begin
      Set_Nth_Arg (D, 1, Hook_Name);
      Set_Nth_Arg (D, 2, F1);
      Set_Nth_Arg (D, 3, F2);
      Set_Nth_Arg (D, 4, F3);

      Tmp := Execute (Command, D);
      Free (F1);
      Free (F2);
      Free (F3);
      Free (D);
      return Tmp;
   end Execute_Shell;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Data : Compilation_Hooks_Args) return String is
      pragma Unreferenced (Data);
   begin
      return Compilation_Hook_Type;
   end Get_Name;

   -------------------
   -- Execute_Shell --
   -------------------

   function Execute_Shell
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : GPS.Kernel.Scripts.Subprogram_Type;
      Hook_Name : String;
      Data      : access Compilation_Hooks_Args) return Boolean
   is
      F   : constant Class_Instance := Create_File (Script, Data.File);
      D   : Callback_Data'Class := Create (Script, 3);
      Tmp : Boolean;
   begin
      Set_Nth_Arg (D, 1, Hook_Name);
      Set_Nth_Arg (D, 2, F);
      Set_Nth_Arg (D, 3, Data.Category);

      Tmp := Execute (Command, D);
      Free (D);
      return Tmp;
   end Execute_Shell;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Data : Exit_Before_Action_Hooks_Args) return String is
      pragma Unreferenced (Data);
   begin
      return Before_Exit_Hook_Type;
   end Get_Name;

   -------------------
   -- Execute_Shell --
   -------------------

   function Execute_Shell
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : GPS.Kernel.Scripts.Subprogram_Type;
      Hook_Name : String;
      Data      : access Exit_Before_Action_Hooks_Args) return Boolean
   is
      D   : Callback_Data'Class := Create (Script, 1);
      Tmp : Boolean;
      pragma Unreferenced (Data);
   begin
      Set_Nth_Arg (D, 1, Hook_Name);
      Tmp := Execute (Command, D);
      Free (D);
      return Tmp;
   end Execute_Shell;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Data : Marker_Hooks_Args) return String is
      pragma Unreferenced (Data);
   begin
      return Marker_Hook_Type;
   end Get_Name;

   -------------------
   -- Execute_Shell --
   -------------------

   function Execute_Shell
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : GPS.Kernel.Scripts.Subprogram_Type;
      Hook_Name : String;
      Data      : access Marker_Hooks_Args) return Boolean
   is
      D   : Callback_Data'Class := Create (Script, 2);
      Tmp : Boolean;
   begin
      Set_Nth_Arg (D, 1, Hook_Name);
      Set_Nth_Arg (D, 2, To_String (Data.Marker));
      Tmp := Execute (Command, D);
      Free (D);
      return Tmp;
   end Execute_Shell;

   ---------------------------
   -- Register_Action_Hooks --
   ---------------------------

   procedure Register_Action_Hooks
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Create_Hook_Type
        (Kernel, Open_File_Hook_Type,
         Hook_With_Args_And_Return, Open_File_Run_Hook_Handler'Access);
      Register_Hook (Kernel, Open_File_Action_Hook,
                     Type_Name => Open_File_Hook_Type);

      Create_Hook_Type
        (Kernel, String_Hook_Type,
         Hook_With_Args, String_Hook_Handler'Access);

      Create_Hook_Type
        (Kernel, Project_Hook_Type,
         Hook_With_Args, Project_Hook_Handler'Access);

      Create_Hook_Type
        (Kernel, Before_Exit_Hook_Type,
         Hook_With_Args_And_Return, Before_Exit_Run_Hook_Handler'Access);
      Register_Hook (Kernel, Before_Exit_Action_Hook,
                     Type_Name => Before_Exit_Hook_Type);

      Create_Hook_Type
        (Kernel, File_Line_Hook_Type,
         Hook_With_Args_And_Return, Line_Information_Run_Hook_Handler'Access);
      Register_Hook (Kernel, File_Line_Action_Hook,
                     Type_Name => File_Line_Hook_Type);

      Create_Hook_Type
        (Kernel, Location_Hook_Type,
         Hook_With_Args_And_Return, Location_Run_Hook_Handler'Access);
      Register_Hook (Kernel, Location_Action_Hook,
                     Type_Name => Location_Hook_Type);

      Create_Hook_Type
        (Kernel, Html_Hook_Type,
         Hook_With_Args_And_Return, Html_Run_Hook_Handler'Access);
      Register_Hook (Kernel, Html_Action_Hook,
                     Type_Name => Html_Hook_Type);

      Create_Hook_Type
        (Kernel, Diff_Hook_Type,
         Hook_With_Args_And_Return, Diff_Run_Hook_Handler'Access);
      Register_Hook (Kernel, Diff_Action_Hook,
                     Type_Name => Diff_Hook_Type);

      Create_Hook_Type
        (Kernel, File_Hook_Type,
         Hook_With_Args, File_Run_Hook_Handler'Access);
      Create_Hook_Type
        (Kernel, Context_Hook_Type,
         Hook_With_Args, Context_Run_Hook_Handler'Access);
      Create_Hook_Type
        (Kernel, File_Location_Hook_Type,
         Hook_With_Args, Location_Changed_Hook_Handler'Access);
   end Register_Action_Hooks;

end GPS.Kernel.Standard_Hooks;
