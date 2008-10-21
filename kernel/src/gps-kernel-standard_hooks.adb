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

with Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.OS_Lib;       use GNAT.OS_Lib;
with System;            use System;

with Gdk.Pixbuf;        use Gdk.Pixbuf;

with Commands;          use Commands;
with GPS.Kernel;        use GPS.Kernel;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;
with Traces;            use Traces;

package body GPS.Kernel.Standard_Hooks is

   Me : constant Debug_Handle := Create ("Standard_Hooks");

   Open_File_Hook_Type           : constant Hook_Type :=
                                     "open_file_action_hooks";
   File_Line_Hook_Type           : constant Hook_Type :=
                                     "file_line_hooks";
   Location_Hook_Type            : constant Hook_Type :=
                                     "location_action_hooks";
   Html_Hook_Type                : constant Hook_Type :=
                                     "html_action_hooks";
   Diff_Hook_Type                : constant Hook_Type :=
                                     "diff_hooks";
   File_Status_Changed_Hook_Type : constant Hook_Type :=
                                     "file_status_changed_hooks";

   --  The various names to describe the hook types defined in this package

   procedure General_Line_Information
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : Virtual_File;
      Identifier : String;
      Info       : Line_Information_Data;
      Every_Line : Boolean := True;
      Normalize  : Boolean := True);
   --  Create the Mime info for adding/creating/removing line information,
   --  and send it.
   --  If File is an empty string, send the Mime for all open buffers.

   function From_Callback_Data_Open_File
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   function From_Callback_Data_String
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   function From_Callback_Data_String_Boolean
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   function From_Callback_Data_Project
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   function From_Callback_Data_Line_Info
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   function From_Callback_Data_Before_Exit
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   function From_Callback_Data_Location
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   function From_Callback_Data_Html
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   function From_Callback_Data_Diff
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   function From_Callback_Data_File
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   function From_Callback_Data_Context
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   function From_Callback_Data_File_Location
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   function From_Callback_Data_File_Status_Changed
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   --  Convert some shell arguments into suitable hooks_data

   ---------------------------
   -- Compute_Parent_Entity --
   ---------------------------

   function Compute_Parent_Entity
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access File_Location_Hooks_Args)
      return Entities.Entity_Information
   is
      pragma Unreferenced (Kernel, Data);
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
      Free (X.Tooltip_Text);

      if X.Associated_Command /= null then
         Destroy (X.Associated_Command);
      end if;
   end Free;

   ------------------------------
   -- General_Line_Information --
   ------------------------------

   procedure General_Line_Information
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : Virtual_File;
      Identifier : String;
      Info       : Line_Information_Data;
      Every_Line : Boolean := True;
      Normalize  : Boolean := True)
   is
      Data : aliased File_Line_Hooks_Args :=
        (Hooks_Data with
         Identifier_Length => Identifier'Length,
         Identifier        => Identifier,
         File              => File,
         Info              => Info,
         Every_Line        => Every_Line,
         Normalize         => Normalize);
   begin
      if File /= GNATCOLL.VFS.No_File then
         if not Run_Hook_Until_Success
           (Kernel, File_Line_Action_Hook, Data'Unchecked_Access,
            Set_Busy => False)
         then
            Trace (Me, "No file editor with line info display "
                   & "capability was registered");
         end if;

      else
         declare
            Files : constant GNATCOLL.VFS.File_Array := Open_Files (Kernel);
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
      Infos : Line_Information_Data;

   begin
      Infos := new Line_Information_Array (-1 .. -1);
      Infos (-1).Text := new String'(Label);

      Add_Line_Information (Kernel, File, Identifier, Infos);
   end Add_Editor_Label;

   ------------------------------------
   -- Create_Line_Information_Column --
   ------------------------------------

   procedure Create_Line_Information_Column
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : Virtual_File;
      Identifier : String;
      Every_Line : Boolean := True;
      Normalize  : Boolean := True)
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
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : Virtual_File;
      Identifier : String)
   is
      A : Line_Information_Array (1 .. 0);
   begin
      General_Line_Information
        (Kernel, File, Identifier, new Line_Information_Array'(A));
   end Remove_Line_Information_Column;

   --------------------------
   -- Add_Line_Information --
   --------------------------

   procedure Add_Line_Information
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : Virtual_File;
      Identifier : String;
      Info       : Line_Information_Data;
      Normalize  : Boolean := True) is
   begin
      General_Line_Information
        (Kernel, File, Identifier, Info, Normalize => Normalize);
   end Add_Line_Information;

   -------------------------
   -- Add_Location_Action --
   -------------------------

   procedure Add_Location_Action
     (Kernel     : access Kernel_Handle_Record'Class;
      Identifier : String;
      Category   : String;
      File       : Virtual_File;
      Line       : Integer;
      Column     : Integer;
      Message    : UTF8_String;
      Action     : Action_Item)
   is
      Data : aliased Location_Hooks_Args :=
        (Hooks_Data with
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
     (Kernel     : access Kernel_Handle_Record'Class;
      Identifier : String;
      Category   : String;
      File       : Virtual_File;
      Line       : Integer;
      Column     : Integer;
      Message    : UTF8_String) is
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
        Gtkada.MDI.Position_Automatic)
   is
      Data : aliased Source_File_Hooks_Args :=
        (Hooks_Data with
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
        (Hooks_Data with
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
      URL_Or_File       : String;
      Enable_Navigation : Boolean := True)
   is
      Anchor : Natural := Index (URL_Or_File, "#");
   begin
      if Anchor = 0 then
         Anchor := URL_Or_File'Last + 1;
      end if;

      declare
         Data : aliased Html_Hooks_Args :=
           (Hooks_Data with
            URL_Length        => Anchor - URL_Or_File'First,
            Anchor_Length     => Integer'Max (0, URL_Or_File'Last - Anchor),
            Enable_Navigation => Enable_Navigation,
            URL_Or_File       => URL_Or_File (URL_Or_File'First .. Anchor - 1),
            Anchor            => URL_Or_File (Anchor + 1 .. URL_Or_File'Last));
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
      Orig_File : Virtual_File := GNATCOLL.VFS.No_File;
      New_File  : Virtual_File := GNATCOLL.VFS.No_File;
      Diff_File : Virtual_File)
   is
      Data : aliased Diff_Hooks_Args :=
               (Hooks_Data with Orig_File, New_File, Diff_File);
   begin
      if not Run_Hook_Until_Success
        (Kernel, Diff_Action_Hook, Data'Unchecked_Access)
      then
         Trace (Me, "No diff viewer registered");
      end if;
   end Display_Differences;

   -------------------------
   -- File_Status_Changed --
   -------------------------

   procedure File_Status_Changed
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Status : File_Status)
   is
      Data : aliased File_Status_Changed_Hooks_Args :=
               (Hooks_Data with File, Status);
   begin
      Run_Hook
        (Kernel, File_Status_Changed_Action_Hook, Data'Unchecked_Access);
   end File_Status_Changed;

   ------------------------------------
   -- From_Callback_Data_Before_Exit --
   ------------------------------------

   function From_Callback_Data_Before_Exit
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      pragma Unreferenced (Data);
   begin
      return Exit_Before_Action_Hooks_Args'(Hooks_Data with null record);
   end From_Callback_Data_Before_Exit;

   --------------
   -- Exit_GPS --
   --------------

   procedure Exit_GPS
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Status : Integer := 0)
   is
      Data : aliased Exit_Before_Action_Hooks_Args :=
               (Hooks_Data with null record);
   begin
      if Run_Hook_Until_Failure
        (Kernel, Before_Exit_Action_Hook, Data'Unchecked_Access)
      then
         Ada.Command_Line.Set_Exit_Status
           (Ada.Command_Line.Exit_Status (Status));
         Gtk.Main.Main_Quit;
      end if;
   end Exit_GPS;

   ----------------------------------
   -- From_Callback_Data_Line_Info --
   ----------------------------------

   function From_Callback_Data_Line_Info
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Kernel     : constant Kernel_Handle := Get_Kernel (Data);
      Identifier : constant String := Nth_Arg (Data, 2);
   begin
      return File_Line_Hooks_Args'
        (Hooks_Data with
         Identifier_Length => Identifier'Length,
         Identifier        => Identifier,
         File         => Get_Data (Nth_Arg (Data, 3, Get_File_Class (Kernel))),
         Info              => null,
         Every_Line        => Nth_Arg (Data, 4),
         Normalize         => Nth_Arg (Data, 5));
   end From_Callback_Data_Line_Info;

   ---------------------------------
   -- From_Callback_Data_Location --
   ---------------------------------

   function From_Callback_Data_Location
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Kernel     : constant Kernel_Handle := Get_Kernel (Data);
      Identifier : constant String := Nth_Arg (Data, 2);
      Category   : constant String := Nth_Arg (Data, 3);
      Message    : constant String := Nth_Arg (Data, 7);
   begin
      return Location_Hooks_Args'
        (Hooks_Data with
         Ident_Length      => Identifier'Length,
         Identifier        => Identifier,
         Cat_Length        => Category'Length,
         Category          => Category,
         File         => Get_Data (Nth_Arg (Data, 4, Get_File_Class (Kernel))),
         Line              => Nth_Arg (Data, 5),
         Column            => Nth_Arg (Data, 6),
         Mes_Length        => Message'Length,
         Message           => Message,
         Action            => null);
   end From_Callback_Data_Location;

   -----------------------------
   -- From_Callback_Data_Html --
   -----------------------------

   function From_Callback_Data_Html
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      URL    : constant String := Nth_Arg (Data, 2);
      Anchor : constant String := Nth_Arg (Data, 4);
   begin
      return Html_Hooks_Args'
        (Hooks_Data with
         Enable_Navigation => Nth_Arg (Data, 3),
         URL_Length        => URL'Length,
         URL_Or_File       => URL,
         Anchor_Length     => Anchor'Length,
         Anchor            => Anchor);
   end From_Callback_Data_Html;

   -----------------------------
   -- From_Callback_Data_Diff --
   -----------------------------

   function From_Callback_Data_Diff
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
   begin
      return Diff_Hooks_Args'
        (Hooks_Data with
         Orig_File =>
           Get_Data (Nth_Arg (Data, 2, Get_File_Class (Kernel), True)),
         New_File  =>
           Get_Data (Nth_Arg (Data, 3, Get_File_Class (Kernel), True)),
         Diff_File =>
           Get_Data (Nth_Arg (Data, 4, Get_File_Class (Kernel), True)));
   end From_Callback_Data_Diff;

   -----------------------------
   -- From_Callback_Data_File --
   -----------------------------

   function From_Callback_Data_File
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
   begin
      return File_Hooks_Args'
        (Hooks_Data with
         File => Get_Data (Nth_Arg (Data, 2, Get_File_Class (Kernel), True)));
   end From_Callback_Data_File;

   --------------------------------
   -- From_Callback_Data_Project --
   --------------------------------

   function From_Callback_Data_Project
     (Data : Callback_Data'Class) return Hooks_Data'Class is
   begin
      return Project_Hooks_Args'
        (Hooks_Data with Project => Get_Data (Data, 2));
   end From_Callback_Data_Project;

   -------------------------------
   -- From_Callback_Data_String --
   -------------------------------

   function From_Callback_Data_String
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Value : constant String := Nth_Arg (Data, 2);
   begin
      return String_Hooks_Args'
        (Hooks_Data with Length => Value'Length, Value => Value);
   end From_Callback_Data_String;

   ---------------------------------------
   -- From_Callback_Data_String_Boolean --
   ---------------------------------------

   function From_Callback_Data_String_Boolean
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Value : constant String := Nth_Arg (Data, 2);
   begin
      return String_Boolean_Hooks_Args'
        (Hooks_Data with Length => Value'Length, Value => Value,
         Bool => Nth_Arg (Data, 3));
   end From_Callback_Data_String_Boolean;

   --------------------------------
   -- From_Callback_Data_Context --
   --------------------------------

   function From_Callback_Data_Context
     (Data : Callback_Data'Class) return Hooks_Data'Class is
   begin
      return Context_Hooks_Args'
        (Hooks_Data with Context => Get_Data (Data, 2));
   end From_Callback_Data_Context;

   --------------------------------------
   -- From_Callback_Data_File_Location --
   --------------------------------------

   function From_Callback_Data_File_Location
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
   begin
      return File_Location_Hooks_Args'
        (Hooks_Data with
         File   => Get_Data (Nth_Arg (Data, 2, Get_File_Class (Kernel), True)),
         Line   => Nth_Arg (Data, 3),
         Column => Nth_Arg (Data, 4));
   end From_Callback_Data_File_Location;

   --------------------------------------------
   -- From_Callback_Data_File_Changed_Status --
   --------------------------------------------

   function From_Callback_Data_File_Status_Changed
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
   begin
      return File_Status_Changed_Hooks_Args'
        (Hooks_Data with
         Get_Data (Nth_Arg (Data, 2, Get_File_Class (Kernel), True)),
         Status => File_Status'Value (Nth_Arg (Data, 3)));
   end From_Callback_Data_File_Status_Changed;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Context_Hooks_Args) return Callback_Data_Access
   is
      C : constant Class_Instance := Create_Context (Script, Data.Context);
      D : constant Callback_Data_Access :=
            new Callback_Data'Class'(Create (Script, 2));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, C);
      return D;
   end Create_Callback_Data;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access String_Hooks_Args) return Callback_Data_Access
   is
      D : constant Callback_Data_Access :=
            new Callback_Data'Class'(Create (Script, 2));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, Data.Value);
      return D;
   end Create_Callback_Data;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access String_Boolean_Hooks_Args) return Callback_Data_Access
   is
      D : constant Callback_Data_Access :=
        new Callback_Data'Class'(Create (Script, 3));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, Data.Value);
      Set_Nth_Arg (D.all, 3, Data.Bool);
      return D;
   end Create_Callback_Data;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Project_Hooks_Args) return Callback_Data_Access
   is
      P : constant Class_Instance := Create_Project (Script, Data.Project);
      D : constant Callback_Data_Access :=
            new Callback_Data'Class'(Create (Script, 2));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, P);
      return D;
   end Create_Callback_Data;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Compilation_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access
   is
      D : constant Callback_Data_Access :=
        new Callback_Data'Class'(Create (Script, 4));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, Data.Value);
      Set_Nth_Arg (D.all, 3, Data.Quiet);
      Set_Nth_Arg (D.all, 4, Data.Shadow);
      return D;
   end Create_Callback_Data;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access File_Hooks_Args) return Callback_Data_Access
   is
      F : constant Class_Instance := Create_File (Script, Data.File);
      D : constant Callback_Data_Access :=
            new Callback_Data'Class'(Create (Script, 2));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, F);
      return D;
   end Create_Callback_Data;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Files_2_Hooks_Args) return Callback_Data_Access
   is
      F  : constant Class_Instance := Create_File (Script, Data.File);
      F2 : constant Class_Instance := Create_File (Script, Data.Renamed);
      D  : constant Callback_Data_Access :=
             new Callback_Data'Class'(Create (Script, 3));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, F);
      Set_Nth_Arg (D.all, 3, F2);
      return D;
   end Create_Callback_Data;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Source_File_Hooks_Args) return Callback_Data_Access
   is
      F : constant Class_Instance := Create_File (Script, Data.File);
      D : constant Callback_Data_Access :=
            new Callback_Data'Class'(Create (Script, 8));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, F);
      Set_Nth_Arg (D.all, 3, Data.Line);
      Set_Nth_Arg (D.all, 4, Natural (Data.Column));
      Set_Nth_Arg (D.all, 5, Natural (Data.Column_End));
      Set_Nth_Arg (D.all, 6, Data.Enable_Navigation);
      Set_Nth_Arg (D.all, 7, Data.New_File);
      Set_Nth_Arg (D.all, 8, Data.Force_Reload);
      return D;
   end Create_Callback_Data;

   ----------------------------------
   -- From_Callback_Data_Open_File --
   ----------------------------------

   function From_Callback_Data_Open_File
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
   begin
      return Source_File_Hooks_Args'
        (Hooks_Data with
         File         => Get_Data (Nth_Arg (Data, 2, Get_File_Class (Kernel))),
         Line              => Nth_Arg (Data, 3),
         Column            => Basic_Types.Visible_Column_Type
           (Nth_Arg (Data, 4, Default => 1)),
         Column_End        => Basic_Types.Visible_Column_Type
           (Nth_Arg (Data, 5, Default => 1)),
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
   end From_Callback_Data_Open_File;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access File_Line_Hooks_Args) return Callback_Data_Access
   is
      F : constant Class_Instance := Create_File (Script, Data.File);
      D : constant Callback_Data_Access :=
            new Callback_Data'Class'(Create (Script, 5));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, Data.Identifier);
      Set_Nth_Arg (D.all, 3, F);
      Set_Nth_Arg (D.all, 4, Data.Every_Line);
      Set_Nth_Arg (D.all, 5, Data.Normalize);
      return D;
   end Create_Callback_Data;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Location_Hooks_Args) return Callback_Data_Access
   is
      F : constant Class_Instance := Create_File (Script, Data.File);
      D : constant Callback_Data_Access :=
            new Callback_Data'Class'(Create (Script, 7));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, Data.Identifier);
      Set_Nth_Arg (D.all, 3, Data.Category);
      Set_Nth_Arg (D.all, 4, F);
      Set_Nth_Arg (D.all, 5, Data.Line);
      Set_Nth_Arg (D.all, 6, Data.Column);
      Set_Nth_Arg (D.all, 7, Data.Message);
      return D;
   end Create_Callback_Data;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Html_Hooks_Args) return Callback_Data_Access
   is
      D : constant Callback_Data_Access :=
            new Callback_Data'Class'(Create (Script, 4));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, Data.URL_Or_File);
      Set_Nth_Arg (D.all, 3, Data.Enable_Navigation);
      Set_Nth_Arg (D.all, 4, Data.Anchor);
      return D;
   end Create_Callback_Data;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access File_Location_Hooks_Args) return Callback_Data_Access
   is
      F : constant Class_Instance := Create_File (Script, Data.File);
      D : constant Callback_Data_Access :=
            new Callback_Data'Class'(Create (Script, 4));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, F);
      Set_Nth_Arg (D.all, 3, Data.Line);
      Set_Nth_Arg (D.all, 4, Data.Column);
      return D;
   end Create_Callback_Data;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Diff_Hooks_Args) return Callback_Data_Access
   is
      F1 : constant Class_Instance := Create_File (Script, Data.Orig_File);
      F2 : constant Class_Instance := Create_File (Script, Data.New_File);
      F3 : constant Class_Instance := Create_File (Script, Data.Diff_File);
      D  : constant Callback_Data_Access :=
             new Callback_Data'Class'(Create (Script, 4));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, F1);
      Set_Nth_Arg (D.all, 3, F2);
      Set_Nth_Arg (D.all, 4, F3);
      return D;
   end Create_Callback_Data;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Exit_Before_Action_Hooks_Args)
      return Callback_Data_Access
   is
      pragma Unreferenced (Data);
      D : constant Callback_Data_Access :=
            new Callback_Data'Class'(Create (Script, 1));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      return D;
   end Create_Callback_Data;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Marker_Hooks_Args) return Callback_Data_Access
   is
      D : constant Callback_Data_Access :=
            new Callback_Data'Class'(Create (Script, 2));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, To_String (Data.Marker));
      return D;
   end Create_Callback_Data;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access File_Status_Changed_Hooks_Args)
      return Callback_Data_Access
   is
      F : constant Class_Instance := Create_File (Script, Data.File);
      D : constant Callback_Data_Access :=
            new Callback_Data'Class'(Create (Script, 3));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, F);
      Set_Nth_Arg (D.all, 3, File_Status'Image (Data.Status));
      return D;
   end Create_Callback_Data;

   ---------------------
   -- Run_String_Hook --
   ---------------------

   procedure Run_String_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Hook   : Hook_Name;
      Data   : String)
   is
      Args : aliased String_Hooks_Args :=
        (Hooks_Data with
         Length => Data'Length,
         Value  => Data);
   begin
      Run_Hook (Kernel, Hook, Args'Unchecked_Access);
   end Run_String_Hook;

   ----------------
   -- Stop_Macro --
   ----------------

   procedure Stop_Macro
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Run_Hook (Kernel, Stop_Macro_Action_Hook);
   end Stop_Macro;

   ---------------------------
   -- Register_Action_Hooks --
   ---------------------------

   procedure Register_Action_Hooks
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Register_Hook_Data_Type
        (Kernel, Open_File_Hook_Type,
         Args_Creator => From_Callback_Data_Open_File'Access);
      Register_Hook_No_Return
        (Kernel, Open_File_Action_Hook, Open_File_Hook_Type);

      Register_Hook_Data_Type
        (Kernel, File_Status_Changed_Hook_Type,
         Args_Creator => From_Callback_Data_File_Status_Changed'Access);
      Register_Hook_No_Return
        (Kernel,
         File_Status_Changed_Action_Hook, File_Status_Changed_Hook_Type);

      Register_Hook_Data_Type
        (Kernel, String_Hook_Type,
         Args_Creator => From_Callback_Data_String'Access);

      Register_Hook_Data_Type
        (Kernel, String_Boolean_Hook_Type,
         Args_Creator => From_Callback_Data_String_Boolean'Access);

      Register_Hook_Data_Type
        (Kernel, Project_Hook_Type,
         Args_Creator => From_Callback_Data_Project'Access);

      Register_Hook_Data_Type
        (Kernel, Before_Exit_Action_Hook_Type,
         Args_Creator => From_Callback_Data_Before_Exit'Access);
      Register_Hook_Return_Boolean
        (Kernel, Before_Exit_Action_Hook, Before_Exit_Action_Hook_Type);

      Register_Hook_Data_Type
        (Kernel, File_Line_Hook_Type,
         Args_Creator => From_Callback_Data_Line_Info'Access);
      Register_Hook_No_Return
        (Kernel, File_Line_Action_Hook, File_Line_Hook_Type);

      Register_Hook_Data_Type
        (Kernel, Location_Hook_Type,
         Args_Creator => From_Callback_Data_Location'Access);
      Register_Hook_Return_Boolean
        (Kernel, Location_Action_Hook, Location_Hook_Type);

      Register_Hook_Data_Type
        (Kernel, Html_Hook_Type,
         Args_Creator => From_Callback_Data_Html'Access);
      Register_Hook_Return_Boolean (Kernel, Html_Action_Hook, Html_Hook_Type);

      Register_Hook_Data_Type
        (Kernel, Diff_Hook_Type,
         Args_Creator => From_Callback_Data_Diff'Access);
      Register_Hook_Return_Boolean (Kernel, Diff_Action_Hook, Diff_Hook_Type);

      Register_Hook_Data_Type
        (Kernel, File_Hook_Type,
         Args_Creator => From_Callback_Data_File'Access);
      Register_Hook_Data_Type
        (Kernel, Context_Hook_Type,
         Args_Creator => From_Callback_Data_Context'Access);
      Register_Hook_Data_Type
        (Kernel, File_Location_Hook_Type,
         Args_Creator => From_Callback_Data_File_Location'Access);

      Register_Hook_No_Args (Kernel, Stop_Macro_Action_Hook);
   end Register_Action_Hooks;

end GPS.Kernel.Standard_Hooks;
