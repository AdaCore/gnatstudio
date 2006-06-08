-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                             AdaCore                               --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Tags;                  use Ada.Tags;
with Ada.Strings;               use Ada.Strings;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with System;

with GNAT.Expect;               use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Case_Util;            use GNAT.Case_Util;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Gtk.Accel_Group;           use Gtk.Accel_Group;
with Gtk.Accel_Map;             use Gtk.Accel_Map;
with Gtk.Enums;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Widget;                use Gtk.Widget;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Remote;         use GPS.Kernel.Remote;
with GPS.Kernel.Timeout;        use GPS.Kernel.Timeout;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Location_View;         use GPS.Location_View;

with VFS;                       use VFS;
with Projects;                  use Projects;
with Interactive_Consoles;      use Interactive_Consoles;
with Language_Handlers;         use Language_Handlers;
with Projects.Registry;         use Projects.Registry;
with Entities;                  use Entities;
with Histories;                 use Histories;
with Remote_Servers;            use Remote_Servers;

with Basic_Types;
with Std_Dialogs;               use Std_Dialogs;
with String_Utils;              use String_Utils;
with GUI_Utils;                 use GUI_Utils;
with OS_Utils;                  use OS_Utils;
with Traces;                    use Traces;
with Commands;                  use Commands;
with Commands.Builder;          use Commands.Builder;
with Task_Manager;              use Task_Manager;

with Commands.Generic_Asynchronous;

package body Builder_Module is

   Me : constant Debug_Handle := Create ("Builder");

   Cst_Run_Arguments_History : constant History_Key := "gvd_run_arguments";
   --  The key in the history for the arguments to the run command.
   --  WARNING: this constant is shared with gvd-menu.adb, since we want to
   --  have the same history for the debugger arguments.

   Run_External_Key : constant History_Key := "run_external_terminal";
   --  The key in the history for the check button "run in external terminal"

   Make_Menu_Prefix : constant String := "<gps>/Build/Make/";
   Run_Menu_Prefix : constant String := "<gps>/Build/Run/";
   --  Prefixes used in the accel path for the various menus

   Custom_Make_Suffix  : constant String := "Custom...";
   Current_Make_Suffix : constant String := "<current file>";
   Project_Make_Suffix : constant String := "Compile all sources";
   All_Make_Suffix     : constant String := "All";
   --  Name for various menus (need to be translated through)
   --      -"Custom..."
   --      -"<current file>"
   --      -"Compile all sources"
   --      -"All"

   Quiet_Opt      : aliased String := "-q";
   Unique_Compile : aliased constant String := "-u";
   Syntax_Check   : aliased String := "-gnats";
   --  ??? Shouldn't have hard-coded options.

   Sources_Load_Chunk : constant Integer := 20;
   --  The size of the chunk of files loaded by the xrefs loader.
   --  ??? This should be configurable

   Xrefs_Loading_Queue_0 : constant String := "xrefs_loading_0";
   Xrefs_Loading_Queue_1 : constant String := "xrefs_loading_1";

   type Files_Callback_Data is new Callback_Data_Record with record
      Files  : File_Array_Access;

      Buffer : Ada.Strings.Unbounded.Unbounded_String;
      --  This field is used to store uncomplete lines got during the
      --  processing of the compilation output.
   end record;
   type Files_Callback_Data_Access is access all Files_Callback_Data'Class;
   procedure Destroy (Data : in out Files_Callback_Data);
   --  Callback data used for the list of files that need to be freed at the
   --  end of a compilation.

   type LI_Handler_Iterator_Access_Access is access LI_Handler_Iterator_Access;

   type Dynamic_Menu_Item_Record is new Gtk_Menu_Item_Record with null record;
   type Dynamic_Menu_Item is access all Dynamic_Menu_Item_Record'Class;
   --  So that items created for the dynamic Make and Run menus have a special
   --  type, and we only remove these when refreshing the menu

   function Is_Dynamic_Menu_Item
     (W : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;
   --  Return True if W is a Dynamic menu item

   function Kill_Xrefs_Queue
     (Kernel : access Kernel_Handle_Record'Class; Queue_Name : String)
      return Boolean;
   --  Kills a given xrefs queue, provided that the command contained in this
   --  queue is a xref loading command.

   --------------------------------
   -- Computing cross-references --
   --------------------------------

   type Compute_Xref_Data is record
      Kernel : Kernel_Handle;
      Iter   : LI_Handler_Iterator_Access_Access;
      LI     : Natural;
   end record;
   type Compute_Xref_Data_Access is access Compute_Xref_Data;

   procedure Deep_Free (D : in out Compute_Xref_Data_Access);
   --  Free memory associated to D;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (LI_Handler_Iterator_Access, LI_Handler_Iterator_Access_Access);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Compute_Xref_Data, Compute_Xref_Data_Access);

   procedure Xref_Iterate
     (Xref_Data : in out Compute_Xref_Data_Access;
      Command   : Command_Access;
      Result    : out Command_Return_Type);
   --  Query the Xref information for the next files in the project.

   package Xref_Commands is new Commands.Generic_Asynchronous
     (Data_Type => Compute_Xref_Data_Access,
      Free      => Deep_Free);

   ------------------------------
   -- Loading cross-references --
   ------------------------------

   type Load_Xref_Data is record
      Kernel           : Kernel_Handle;
      Current_Progress : Natural;
      Total_Progress   : Natural;
      Std_Files        : File_Array_Access;
      Project_Files    : File_Array_Access;
      Index_In_Std     : Natural;
      Index_In_Project : Natural;
      Stop             : Boolean := False;
   end record;
   type Load_Xref_Data_Access is access Load_Xref_Data;

   procedure Free (D : in out Load_Xref_Data_Access);
   --  Free memory associated to D;

   procedure Load_Xref_Iterate
     (Xref_Data : in out Load_Xref_Data_Access;
      Command   : Command_Access;
      Result    : out Command_Return_Type);
   --  Load all the Xref information in memory

   package Load_Xref_Commands is new Commands.Generic_Asynchronous
     (Data_Type => Load_Xref_Data_Access,
      Free      => Free);

   ----------
   -- Misc --
   ----------

   procedure Free (Ar : in out String_List);
   procedure Free (Ar : in out String_List_Access);
   --  Free the memory associate with Ar.

   type Command_Syntax is (GNAT_Syntax, Make_Syntax);
   --  Type used in Scenario_Variables_Cmd_Line to determine the command line
   --  syntax used when setting variables.
   --  GNAT_Syntax means use the GNAT project file syntax (-XVAR=value)
   --  Make_Syntax means use the gprmake syntax.

   function Compute_Arguments
     (Kernel         : Kernel_Handle;
      Project        : Project_Type;
      Path           : String;
      File           : Virtual_File;
      Compile_Only   : Boolean := False;
      Unique_Project : Boolean := False;
      Extra_Args     : Argument_List_Access := null)
      return Argument_List_Access;
   --  Compute the make arguments compatible with gnatmake/gprmake
   --  given a Project and File name.
   --  It is the responsibility of the caller to free the returned object.
   --
   --  If File is No_File, then all files of the project will be recompiled.
   --  If Compile_Only is True, then use compile rather than build syntax.
   --
   --  If Path is not empty, use Path as project path.
   --
   --  If Extra_Args is not null, then arguments will be copied just before the
   --  file name in the argument list.

   procedure Add_Build_Menu
     (Menu         : in out Gtk_Menu;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Set_Shortcut : Boolean;
      Mains        : Argument_List);
   --  Add new entries for all the main subprograms of Project.
   --  If Menu is null, a new one is created if there are any entries
   --  If Set_Shortcut is true, the F4 shortcut is set for the first entry.

   procedure Add_Root_Project_Build_Menu
     (Menu         : in out Gtk_Menu;
      Kernel       : access Kernel_Handle_Record'Class;
      Set_Shortcut : Boolean);
   --  Add default entries in the Build menu
   --  (build "all", "compile all sources")

   procedure Add_Run_Menu
     (Menu         : in out Gtk_Menu;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Set_Shortcut : Boolean;
      Mains        : Argument_List);
   --  Same as Add_Build_Menu, but for the Run menu

   type Builder_Contextual is new Submenu_Factory_Record with null record;
   procedure Append_To_Menu
     (Builder : access Builder_Contextual;
      Object  : access GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);

   type Run_Contextual is new Submenu_Factory_Record with null record;
   procedure Append_To_Menu
     (Factory : access Run_Contextual;
      Object  : access GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add entries to the contextual menu for Build/ or Run/

   procedure Cleanup_Accel_Map (Kernel : access Kernel_Handle_Record'Class);
   --  Remove from the accel_map the key bindings set for previous projects

   procedure Parse_Compiler_Output
     (Data : Process_Data; Output : String);
   --  Called whenever new output from the compiler is available

   procedure Free_Temporary_Files
     (Data : Process_Data; Status : Integer);
   --  Free the temporary files that were created for the compilation. The
   --  list is stored in Data.Callback_Data.

   --------------------
   -- Menu Callbacks --
   --------------------

   procedure On_Check_Syntax
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Check Syntax menu

   procedure On_Compile
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Compile menu

   procedure On_Build
     (Kernel : access GObject_Record'Class; Data : File_Project_Record);
   --  Build->Make menu.
   --  If Data contains a null file name, then the current file is compiled.

   procedure On_Build_Project
     (Kernel : access GObject_Record'Class; Data : File_Project_Record);
   --  Build->Make->All/Compile all sources menus.

   procedure On_Build
     (Kernel      : Kernel_Handle;
      File        : Virtual_File;
      Project     : Project_Type;
      Main_Units  : Boolean := False;
      Synchronous : Boolean := False;
      Extra_Args  : Argument_List_Access := null);
   --  Same as On_Build.
   --  If Synchronous is True, this subprogram will block GPS until the
   --  compilation is finished executing.
   --  If Project is No_Project and File is VFS.No_File, the current file is
   --  build.
   --  If Project is not No_Project and File is VFS.No_File, either
   --  the main units of Project are built (Main_Units = True), or all the
   --  files in the project are built.
   --  Extra_Args will be added at the end of the argument list.

   procedure On_Custom
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Custom... menu

   procedure On_Compute_Xref
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Compute Xref information menu

   procedure On_Load_Xref_In_Memory
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Load Xref info

   procedure On_Compilation_Finished
     (Kernel : access Kernel_Handle_Record'Class;
      Data : access Hooks_Data'Class);
   --  Called when the compilation is finished.

   procedure Load_Xref_In_Memory (Kernel : access Kernel_Handle_Record'Class);
   --  Load the Xref info in memory, in a background task.

   procedure On_Run
     (Kernel : access GObject_Record'Class; Data : File_Project_Record);
   --  Build->Run menu

   procedure On_Tools_Interrupt
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Tools->Interrupt menu

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called every time the project view has changed, ie potentially the list
   --  of main units.

   procedure Compile_Command
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Command handler for the "compile" command.

   procedure Compile_File
     (Kernel      : Kernel_Handle;
      File        : Virtual_File;
      Synchronous : Boolean := False;
      Syntax_Only : Boolean := False;
      Quiet       : Boolean := False;
      Shadow      : Boolean := False;
      Extra_Args  : Argument_List_Access := null);
   --  Launch a compilation command for File.
   --  If Synchronous is true, then this procedure will not return until the
   --  file is fully compiled; all other GPS operations are blocked while the
   --  compilation takes place in this case.
   --  If Syntax_Only is True, perform only syntax checks.
   --  If Quiet is True, compile only in the background, without showing the
   --  compilation command.
   --  If Shadow is True, then a new file and an extending project will be
   --  created, and those files will be deleted after the compilation command
   --  ends.
   --  The Extra_Args passed will be added at the end of the argument list.

   procedure Clear_Compilation_Output
     (Kernel : Kernel_Handle;
      Shadow : Boolean);
   --  Clear the compiler output, the console, and the result view.

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Data : in out Files_Callback_Data) is
   begin
      Unchecked_Free (Data.Files);
   end Destroy;

   --------------------------
   -- Is_Dynamic_Menu_Item --
   --------------------------

   function Is_Dynamic_Menu_Item
     (W : access Gtk_Widget_Record'Class) return Boolean is
   begin
      return W'Tag = Dynamic_Menu_Item_Record'Tag;
   end Is_Dynamic_Menu_Item;

   ------------------------------
   -- Clear_Compilation_Output --
   ------------------------------

   procedure Clear_Compilation_Output
     (Kernel : Kernel_Handle;
      Shadow : Boolean) is
   begin
      if Shadow then
         Remove_Location_Category (Kernel, -Shadow_Category);
      else
         Console.Clear (Kernel);
         Remove_Location_Category (Kernel, -Error_Category);

         --  We do not need to remove Warning/Style_Category since these
         --  are located under the Error_Category hierarchy in the locations
         --  window.
      end if;

      String_List_Utils.String_List.Free (Builder_Module_ID.Output);
   end Clear_Compilation_Output;

   -----------------------
   -- Compute_Arguments --
   -----------------------

   function Compute_Arguments
     (Kernel         : Kernel_Handle;
      Project        : Project_Type;
      Path           : String;
      File           : Virtual_File;
      Compile_Only   : Boolean := False;
      Unique_Project : Boolean := False;
      Extra_Args     : Argument_List_Access := null)
      return Argument_List_Access
   is
      Project_Str    : GNAT.OS_Lib.String_Access;
      Result         : Argument_List_Access;
      Vars           : Argument_List_Access;

   begin
      --  Convert path to Build_Server style

      if Path = "" then
         Project_Str := new String'
           (To_Remote (Full_Name (Project_Path (Project)).all,
                       Build_Server));
      else
         Project_Str := new String'(To_Remote (Path, Build_Server));
      end if;

      --  -XVAR1=value1 [-c] -Pproject [-u] main...

      Vars := Argument_String_To_List
        (Scenario_Variables_Cmd_Line (Kernel, "-X"));

      declare
         R_Tmp : Argument_List (1 .. 5);
         K     : Natural := 0;
      begin
         if Compile_Only then
            K := K + 1;
            R_Tmp (K) := new String'("-c");
            K := K + 1;
            R_Tmp (K) := new String'(Unique_Compile);
         end if;

         K := K + 1;
         R_Tmp (K) := new String'("-P" & Project_Str.all);

         if Extra_Args /= null then
            for J in Extra_Args'Range loop
               K := K + 1;
               R_Tmp (K) := new String'(Extra_Args (J).all);
            end loop;
         end if;

         if File = VFS.No_File then
            if Unique_Project then
               K := K + 1;
               R_Tmp (K) := new String'(Unique_Compile);
            end if;
         else
            K := K + 1;
            R_Tmp (K) := new String'(Base_Name (File));
         end if;

         K := K + 1;
         R_Tmp (K) := new String'("-d");

         Result := new Argument_List'(R_Tmp (1 .. K) & Vars.all);
      end;

      Basic_Types.Unchecked_Free (Vars);
      Free (Project_Str);
      return Result;
   end Compute_Arguments;

   ----------
   -- Free --
   ----------

   procedure Free (Ar : in out String_List) is
   begin
      for A in Ar'Range loop
         Free (Ar (A));
      end loop;
   end Free;

   procedure Free (Ar : in out String_List_Access) is
      procedure Free is new
        Ada.Unchecked_Deallocation (String_List, String_List_Access);

   begin
      if Ar /= null then
         Free (Ar.all);
         Free (Ar);
      end if;
   end Free;

   ---------------------------
   -- Parse_Compiler_Output --
   ---------------------------

   procedure Parse_Compiler_Output
     (Data : Process_Data; Output : String)
   is
      Last_EOL : Natural := 1;
   begin
      if not Data.Process_Died then
         Last_EOL := Index (Output, (1 => ASCII.LF), Backward);

         --  In case we did not find any LF in the output, we'll just append it
         --  in the current buffer.

         if Last_EOL = 0 then
            Ada.Strings.Unbounded.Append
              (Files_Callback_Data (Data.Callback_Data.all).Buffer,
               Output);

            return;
         end if;
      else
         if Output'Length > 0 then
            Last_EOL := Output'Length + 1;
         end if;
      end if;

      if Output'Length > 0 then
         Process_Builder_Output
           (Kernel  => Data.Kernel,
            Command => Data.Command,
            Output  => Ada.Strings.Unbounded.To_String
              (Files_Callback_Data (Data.Callback_Data.all).Buffer)
               & Output (Output'First .. Last_EOL - 1) & ASCII.LF,
            Quiet   => False);

         Files_Callback_Data (Data.Callback_Data.all).Buffer :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Output (Last_EOL + 1 .. Output'Last));
      elsif Data.Process_Died then
         Process_Builder_Output
           (Kernel  => Data.Kernel,
            Command => Data.Command,
            Output  => Ada.Strings.Unbounded.To_String
              (Files_Callback_Data (Data.Callback_Data.all).Buffer) & ASCII.LF,
            Quiet   => False);

         Files_Callback_Data (Data.Callback_Data.all).Buffer :=
           Ada.Strings.Unbounded.Null_Unbounded_String;
      end if;
   end Parse_Compiler_Output;

   --------------------------
   -- Free_Temporary_Files --
   --------------------------

   procedure Free_Temporary_Files
     (Data : Process_Data; Status : Integer)
   is
      pragma Unreferenced (Status);
      Files   : constant Files_Callback_Data_Access :=
                  Files_Callback_Data_Access (Data.Callback_Data);
      Success : Boolean;
   begin
      if Files /= null and then Files.Files /= null then
         for F in Files.Files'Range loop
            if Is_Regular_File (Files.Files (F)) then
               Trace (Me, "Deleting temporary file "
                      & Full_Name (Files.Files (F)).all);
               Delete (Files.Files (F), Success);
            end if;
         end loop;
      end if;

      Builder_Module_ID.Build_Count := Builder_Module_ID.Build_Count - 1;

      if Builder_Module_ID.Build_Count = 0 then
         Compilation_Finished (Data.Kernel, Error_Category);
      end if;
   end Free_Temporary_Files;

   --------------
   -- On_Build --
   --------------

   procedure On_Build
     (Kernel      : Kernel_Handle;
      File        : Virtual_File;
      Project     : Project_Type;
      Main_Units  : Boolean := False;
      Synchronous : Boolean := False;
      Extra_Args  : Argument_List_Access := null)
   is
      Old_Dir : constant Dir_Name_Str := Get_Current_Dir;
      Cmd     : String_Access;
      Args    : Argument_List_Access;

      Context : Selection_Context;
      Prj     : Project_Type;
      Langs   : Argument_List := Get_Languages
        (Get_Project (Kernel), Recursive => True);
      Syntax  : Command_Syntax;
      Success : Boolean;
      Common_Args : Argument_List_Access;

   begin
      if Langs'Length = 0 then
         return;
      end if;

      if Extra_Args /= null then
         Common_Args := new Argument_List'(Extra_Args.all);
      else
         Common_Args := new Argument_List (1 .. 0);
      end if;

      for F in Langs'Range loop
         To_Lower (Langs (F).all);
      end loop;

      if Langs'Length = 1 and then Langs (Langs'First).all = "ada" then
         Syntax := GNAT_Syntax;
      elsif Get_Pref (Multi_Language_Build) then
         Syntax := Make_Syntax;
      else
         Syntax := Make_Syntax;

         for J in Langs'Range loop
            if Langs (J).all = "ada" then
               Syntax := GNAT_Syntax;

               exit;
            end if;
         end loop;
      end if;

      Free (Langs);

      --  Ask for saving sources/projects before building.
      --  Do this before checking the project, in case we have a default
      --  project whose name is changed when saving

      if not Save_MDI_Children (Kernel, Force => Get_Pref (Auto_Save)) then
         Free (Args);
         return;
      end if;

      Prj := Project;

      --  If no file was specified in data, simply compile the current file

      if File = VFS.No_File and then Project = No_Project then
         Context := Get_Current_Context (Kernel);

         if Has_File_Information (Context) then
            declare
               F : constant Virtual_File := File_Information (Context);

            begin
               if Has_Directory_Information (Context) then
                  Change_Dir (Directory_Information (Context));
               end if;

               Prj := Get_Project_From_File (Get_Registry (Kernel).all, F);
               Args := Compute_Arguments (Kernel, Prj, "", F);
            end;

         --  There is no current file, so we can't compile anything

         else
            Console.Insert
              (Kernel, -"No file selected, cannot build", Mode => Error);
            return;
         end if;

      else
         Args := Compute_Arguments
           (Kernel, Project, "", File,
            Unique_Project => not Main_Units,
            Extra_Args     => Common_Args);
         Change_Dir (Dir_Name (Project_Path (Project)).all);
      end if;

      if Builder_Module_ID.Build_Count = 0 then
         Clear_Compilation_Output (Kernel, False);
      end if;

      case Syntax is
         when GNAT_Syntax =>
            Cmd := new String'(Get_Attribute_Value
              (Prj, Compiler_Command_Attribute,
               Default => "gnatmake", Index => "ada"));

         when Make_Syntax =>
            Cmd := new String'("gprmake");
      end case;

      Builder_Module_ID.Build_Count := Builder_Module_ID.Build_Count + 1;

      Console.Raise_Console (Kernel);
      Launch_Process
        (Kernel,
         Command              => Cmd.all,
         Arguments            => Args.all,
         Server               => Build_Server,
         Console              => Get_Console (Kernel),
         Show_Command         => True,
         Show_Output          => False,
         Callback_Data        => new Files_Callback_Data,
         Success              => Success,
         Line_By_Line         => False,
         Callback             => Parse_Compiler_Output'Access,
         Exit_Cb              => Free_Temporary_Files'Access,
         Show_In_Task_Manager => True,
         Synchronous          => Synchronous,
         Show_Exit_Status     => True);

      Free (Cmd);
      Free (Args);
      Basic_Types.Unchecked_Free (Common_Args);

      Change_Dir (Old_Dir);

   exception
      when Invalid_Process =>
         Console.Insert (Kernel, -"Invalid command", Mode => Error);
         Change_Dir (Old_Dir);
         Free (Cmd);
         Free (Args);

      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         Change_Dir (Old_Dir);
   end On_Build;

   procedure On_Build
     (Kernel : access GObject_Record'Class; Data : File_Project_Record) is
   begin
      On_Build
        (Kernel_Handle (Kernel), Data.File,
         Data.Project, Synchronous => False);
   end On_Build;

   ----------------------
   -- On_Build_Project --
   ----------------------

   procedure On_Build_Project
     (Kernel : access GObject_Record'Class; Data : File_Project_Record) is
   begin
      On_Build
        (Kernel_Handle (Kernel), Data.File,
         Data.Project, Main_Units => True);
   end On_Build_Project;

   ---------------------
   -- On_Check_Syntax --
   ---------------------

   procedure On_Check_Syntax
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Context : constant Selection_Context := Get_Current_Context (Kernel);

   begin
      if Has_File_Information (Context) then
         Compile_File
           (Kernel,
            File_Information (Context),
            Syntax_Only => True);
      else
         Console.Insert
           (Kernel, -"No file selected, cannot check syntax",
            Mode => Error);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Check_Syntax;

   ------------------
   -- Compile_File --
   ------------------

   procedure Compile_File
     (Kernel      : Kernel_Handle;
      File        : Virtual_File;
      Synchronous : Boolean := False;
      Syntax_Only : Boolean := False;
      Quiet       : Boolean := False;
      Shadow      : Boolean := False;
      Extra_Args  : Argument_List_Access := null)
   is
      Prj             : constant Project_Type :=
                          Get_Project_From_File
                            (Get_Registry (Kernel).all, File);
      Old_Dir         : constant Dir_Name_Str := Get_Current_Dir;
      Cmd             : String_Access;
      Fd              : Process_Descriptor_Access;
      Local_File      : String_Access;
      Lang            : String := Get_Language_From_File
        (Get_Language_Handler (Kernel), File);
      Common_Args     : Argument_List_Access;
      Args            : Argument_List_Access;
      Shadow_Path     : String_Access;
      Compilable_File : Virtual_File := File;
      Success         : Boolean;
      Cb_Data         : Files_Callback_Data_Access;

   begin
      --  Is there a file to compile ?

      if File = VFS.No_File then
         if not Quiet then
            Console.Insert
              (Kernel, -"No file name, cannot compile",
               Mode => Error);
         end if;

         return;
      end if;

      To_Lower (Lang);

      --  Save the corresponding files if needed

      if not Shadow then
         if not Save_MDI_Children
           (Kernel, Force => Get_Pref (Auto_Save))
         then
            return;
         end if;
      end if;

      --  Determine the project for the file

      if Prj = No_Project then
         if not Quiet then
            Console.Insert
              (Kernel, -"Could not determine the project for file: "
               & Full_Name (File).all,
               Mode => Error);
         end if;

         return;
      end if;

      --  Determine the language for the file

      if Lang = "" then
         if not Quiet then
            Console.Insert
              (Kernel, -"Could not determine the language for file: "
               & Full_Name (File).all,
               Mode => Error);
         end if;

         return;

      elsif Lang = "ada" then
         Cmd := new String'
           (Get_Attribute_Value
              (Prj, Compiler_Command_Attribute,
               Default => "gnatmake", Index => "ada"));

         if Syntax_Only then
            Common_Args := new Argument_List'
              (Quiet_Opt'Access, Syntax_Check'Access);
         else
            Common_Args := new Argument_List'(1 .. 0 => null);
         end if;

      else
         Cmd         := new String'("gprmake");
         Common_Args := new Argument_List'(1 .. 0 => null);
      end if;

      if Builder_Module_ID.Build_Count = 0 then
         Clear_Compilation_Output (Kernel, Shadow);
      end if;

      Cb_Data := new Files_Callback_Data;

      if Shadow then
         --  Create a temporary project, and a temporary file containing the
         --  buffer data, so as to be able to compile the file without saving
         --  the buffer to disk.

         declare
            Tmp_Dir      : constant String := Get_Tmp_Dir;
            Temp_Project : constant Virtual_File :=
                             Create (Tmp_Dir & "ext.gpr");
            Temp_File    : constant Virtual_File :=
                             Create (Get_Tmp_Dir & Base_Name (File));
            Writable     : Writable_File;
         begin
            --  Do nothing if one of the files already exists.

            if not Is_Regular_File (Temp_Project) then
               --  Write the temporary project file
               Writable := Write_File (Temp_Project);
               Write
                 (Writable,
                  "project ext extends """
                  & Full_Name (Project_Path (Prj)).all & """ is"
                  & ASCII.LF & "end ext;",
                  False);
               Close (Writable);
            end if;

            if not Is_Regular_File (Temp_File) then
               --  Write the temporary buffer file
               Execute_GPS_Shell_Command
                 (Kernel, "Editor.save_buffer " & Full_Name (File).all & " " &
                  Full_Name (Temp_File).all);
            end if;

            Local_File := new String'(Locale_Full_Name (Temp_File));
            Shadow_Path := new String'(Full_Name (Temp_Project).all);
            Compilable_File := Temp_File;
            Change_Dir (Tmp_Dir);

            Cb_Data.Files :=
              new File_Array'(1 => Temp_File, 2 => Temp_Project);
         end;

      else
         Local_File := new String'(Locale_Full_Name (File));
         Change_Dir (Dir_Name (Project_Path (Prj)).all);
         Shadow_Path := new String'("");
      end if;

      if Extra_Args /= null then
         declare
            Garbage_Args : Argument_List_Access := Common_Args;
         begin
            Common_Args :=
              new Argument_List'(Common_Args.all & Extra_Args.all);
            Basic_Types.Unchecked_Free (Garbage_Args);
         end;
      end if;

      Args := Compute_Arguments
        (Kernel, Prj, Shadow_Path.all, Compilable_File, Compile_Only => True);
      Builder_Module_ID.Build_Count := Builder_Module_ID.Build_Count + 1;

      if not Quiet then
         Console.Raise_Console (Kernel);
      end if;

      Launch_Process
        (Kernel,
         Command              => Cmd.all,
         Arguments            => Common_Args.all & Args.all,
         Server               => Build_Server,
         Console              => Get_Console (Kernel),
         Show_Command         => not Quiet,
         Show_Output          => False,
         Callback_Data        => Cb_Data.all'Access,
         Success              => Success,
         Line_By_Line         => False,
         Callback             => Parse_Compiler_Output'Access,
         Exit_Cb              => Free_Temporary_Files'Access,
         Show_In_Task_Manager => True,
         Synchronous          => Synchronous,
         Show_Exit_Status     => True);
      Free (Args);
      Basic_Types.Unchecked_Free (Common_Args);
      Free (Cmd);
      Free (Local_File);
      Free (Shadow_Path);
      Change_Dir (Old_Dir);

   exception
      when Invalid_Process =>
         Console.Insert (Kernel, -"Invalid command", Mode => Error);
         Change_Dir (Old_Dir);
         Free (Fd);
   end Compile_File;

   ---------------------
   -- Compile_Command --
   ---------------------

   procedure Compile_Command
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      use String_List_Utils.String_List;
      Node       : List_Node;
      Info       : Virtual_File;
      Kernel     : constant Kernel_Handle := Get_Kernel (Data);
      C          : Xref_Commands.Generic_Asynchronous_Command_Access;
      Extra_Args : Argument_List_Access;
   begin
      if Command = "compile" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));
         Extra_Args := Argument_String_To_List (Nth_Arg (Data, 2, ""));
         Compile_File
           (Get_Kernel (Data),
            Info, Synchronous => True,
            Extra_Args => Extra_Args);
         Free (Extra_Args);

      elsif Command = "check_syntax" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));
         Compile_File (Get_Kernel (Data), Info,
                       Synchronous => True,
                       Syntax_Only => True);

      elsif Command = "shadow_check_syntax" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));
         Compile_File (Get_Kernel (Data), Info,
                       Synchronous => False,
                       Syntax_Only => True,
                       Quiet       => True,
                       Shadow      => True);

      elsif Command = "make" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));
         Extra_Args := Argument_String_To_List (Nth_Arg (Data, 2, ""));

         declare
            Project : constant Project_Type := Get_Project_From_File
              (Registry => Project_Registry
                 (Get_Registry (Get_Kernel (Data)).all),
               Source_Filename   => Info,
               Root_If_Not_Found => True);
         begin
            On_Build
              (Get_Kernel (Data),
               File        => Info,
               Project     => Project,
               Synchronous => True,
               Extra_Args  => Extra_Args);
         end;

         Free (Extra_Args);

      elsif Command = "get_build_output" then
         Node := First (Builder_Module_ID.Output);

         Set_Return_Value_As_List (Data);
         while Node /= Null_Node loop
            Set_Return_Value
              (Data, String_List_Utils.String_List.Data (Node));

            Node := Next (Node);
         end loop;

      elsif Command = "compute_xref" then
         Xref_Commands.Create
           (C, -"Computing C/C++ xref info",
            new Compute_Xref_Data'(Kernel, new LI_Handler_Iterator_Access, 0),
            Xref_Iterate'Access);

         Launch_Synchronous (Command_Access (C), 0.01);
         Destroy (Command_Access (C));
      end if;
   end Compile_Command;

   ----------------
   -- On_Compile --
   ----------------

   procedure On_Compile
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Context : constant Selection_Context := Get_Current_Context (Kernel);
   begin
      if Has_File_Information (Context) then
         Compile_File (Kernel, File_Information (Context));
      else
         Console.Insert
           (Kernel, -"No file selected, cannot compile", Mode => Error);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Compile;

   ---------------
   -- On_Custom --
   ---------------

   procedure On_Custom
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Cmd : constant String := Simple_Entry_Dialog
        (Parent   => Get_Current_Window (Kernel),
         Title    => -"Custom Execution",
         Message  => -"Enter the command to execute:",
         Position => Gtk.Enums.Win_Pos_Mouse,
         History  => Get_History (Kernel),
         Key      => "gps_custom_command");
      Success : Boolean;

   begin
      if Cmd = "" or else Cmd (Cmd'First) = ASCII.NUL then
         return;
      end if;

      if not Save_MDI_Children
        (Kernel, Force => Get_Pref (Auto_Save))
      then
         return;
      end if;

      if Builder_Module_ID.Build_Count = 0 then
         Clear_Compilation_Output (Kernel, False);
      end if;

      declare
         Args : Argument_List_Access := Argument_String_To_List (Cmd);
      begin
         Builder_Module_ID.Build_Count := Builder_Module_ID.Build_Count + 1;
         Console.Raise_Console (Kernel);
         --  ??? Is this always the Build_Server ? Should ask the user ?
         Launch_Process
           (Kernel,
            Command              => Args (Args'First).all,
            Arguments            => Args (Args'First + 1 .. Args'Last),
            Server               => Build_Server,
            Console              => Get_Console (Kernel),
            Show_Command         => True,
            Show_Output          => False,
            Callback_Data        => new Files_Callback_Data,
            Success              => Success,
            Line_By_Line         => False,
            Callback             => Parse_Compiler_Output'Access,
            Exit_Cb              => Free_Temporary_Files'Access,
            Show_In_Task_Manager => True,
            Synchronous          => False,
            Show_Exit_Status     => True);
         Free (Args);
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Custom;

   ------------------
   -- Xref_Iterate --
   ------------------

   procedure Xref_Iterate
     (Xref_Data : in out Compute_Xref_Data_Access;
      Command   : Command_Access;
      Result    : out Command_Return_Type)
   is
      D            : Compute_Xref_Data_Access renames Xref_Data;
      Handler      : constant Language_Handler :=
                       Language_Handler (Get_Language_Handler (D.Kernel));
      Num_Handlers : constant Natural := LI_Handlers_Count (Handler);
      Not_Finished : Boolean;
      LI           : LI_Handler;
      New_Handler  : Boolean := False;

      procedure Errors (Msg : String);
      --  Print the Msg as an error in the GPS console

      ------------
      -- Errors --
      ------------

      procedure Errors (Msg : String) is
      begin
         Insert (D.Kernel, Msg, Mode => Error);
      end Errors;

   begin
      if D.LI /= 0 and then D.Iter.all /= null then
         Continue (D.Iter.all.all, Errors'Unrestricted_Access, Not_Finished);
      else
         Not_Finished := True;
      end if;

      while Not_Finished loop
         D.LI := D.LI + 1;

         if D.LI > Num_Handlers then
            Insert (D.Kernel, -"Finished parsing all source files");

            Result := Success;
            return;
         end if;

         Free (D.Iter.all);

         LI := Get_Nth_Handler (Handler, D.LI);
         if LI /= null then
            New_Handler := True;
            D.Iter.all := new LI_Handler_Iterator'Class'
              (Generate_LI_For_Project
                 (Handler      => LI,
                  Lang_Handler => Get_Language_Handler (D.Kernel),
                  Project      => Get_Project (D.Kernel),
                  Errors       => Errors'Unrestricted_Access,
                  Recursive    => True));
            Continue (D.Iter.all.all,
                      Errors'Unrestricted_Access,
                      Not_Finished);
         end if;
      end loop;

      if New_Handler then
         Insert (D.Kernel, -"Parsing source files for "
                 & Get_Name (Get_Nth_Handler (Handler, D.LI)));
      end if;

      Set_Progress (Command, (Running, D.LI, Num_Handlers));

      Result := Execute_Again;
      return;
   end Xref_Iterate;

   ---------------------
   -- On_Compute_Xref --
   ---------------------

   procedure On_Compute_Xref
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Object);

      C : Xref_Commands.Generic_Asynchronous_Command_Access;
   begin
      Xref_Commands.Create
        (C, -"Computing C/C++ xref info",
         new Compute_Xref_Data'(Kernel, new LI_Handler_Iterator_Access, 0),
         Xref_Iterate'Access);

      Launch_Background_Command
        (Kernel, Command_Access (C), True, True, "");

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Compute_Xref;

   ----------
   -- Free --
   ----------

   procedure Free (D : in out Load_Xref_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Load_Xref_Data, Load_Xref_Data_Access);
   begin
      Unchecked_Free (D.Std_Files);
      Unchecked_Free (D.Project_Files);
      Unchecked_Free (D);
   end Free;

   -----------------------
   -- Load_Xref_Iterate --
   -----------------------

   procedure Load_Xref_Iterate
     (Xref_Data : in out Load_Xref_Data_Access;
      Command   : Command_Access;
      Result    : out Command_Return_Type)
   is
      D            : Load_Xref_Data_Access renames Xref_Data;

      procedure Load_From_File_Array
        (Files : File_Array_Access; Index : in out Natural);

      procedure Load_From_File_Array
        (Files : File_Array_Access; Index : in out Natural)
      is
         Start, Stop : Integer;
      begin
         Start := Index;
         Stop := Index + Sources_Load_Chunk;

         if Stop > Files'Last then
            Stop := Files'Last;
         end if;

         for J in Start .. Stop loop
            Update_Xref
              (Get_Or_Create
                 (Get_Database (D.Kernel), Files (J)));
         end loop;

         Index := Stop;
         D.Current_Progress := D.Current_Progress + 1;
         Set_Progress
           (Command,
            (Running,
             D.Current_Progress,
             D.Total_Progress));
      end Load_From_File_Array;

   begin
      if D.Stop then
         Result := Success;
         return;
      end if;

      if D.Std_Files /= null
        and then D.Index_In_Std < D.Std_Files'Last
      then
         Load_From_File_Array (D.Std_Files, D.Index_In_Std);
         Result := Execute_Again;
      elsif D.Project_Files /= null
        and then D.Index_In_Project < D.Project_Files'Last
      then
         Load_From_File_Array (D.Project_Files, D.Index_In_Project);
         Result := Execute_Again;
      else
         Result := Success;
      end if;
   end Load_Xref_Iterate;

   ----------------------------
   -- On_Load_Xref_In_Memory --
   ----------------------------

   procedure On_Load_Xref_In_Memory
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Object);
   begin
      Load_Xref_In_Memory (Kernel);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Load_Xref_In_Memory;

   -----------------------------
   -- On_Compilation_Finished --
   -----------------------------

   procedure On_Compilation_Finished
     (Kernel : access Kernel_Handle_Record'Class;
      Data : access Hooks_Data'Class)
   is
      pragma Unreferenced (Data);
   begin
      if Get_Pref (Automatic_Xrefs_Load) then
         Load_Xref_In_Memory (Kernel);
      end if;
   end On_Compilation_Finished;

   ----------------------
   -- Kill_Xrefs_Queue --
   ----------------------

   function Kill_Xrefs_Queue
     (Kernel : access Kernel_Handle_Record'Class; Queue_Name : String)
      return Boolean
   is
      use Load_Xref_Commands;

      Old_Command : Scheduled_Command_Access;
      Old_Data    : Load_Xref_Data_Access;
   begin
      Old_Command := Scheduled_Command_Access
        (Head (Get_Task_Manager (Kernel), Queue_Name));

      if Old_Command /= null then
         --  If there is already something in the queue, then interrupt it.

         Old_Data := Get_Data
           (Load_Xref_Commands.Generic_Asynchronous_Command_Access
              (Get_Command (Old_Command)));
         Old_Data.Stop := True;
         Set_Data (Load_Xref_Commands.Generic_Asynchronous_Command_Access
                   (Get_Command (Old_Command)), Old_Data);

         return True;
      end if;

      return False;
   end Kill_Xrefs_Queue;

   -----------------------------
   -- Interrupt_Xrefs_Loading --
   -----------------------------

   procedure Interrupt_Xrefs_Loading
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
   begin
      Dummy := Kill_Xrefs_Queue (Kernel, Xrefs_Loading_Queue_0);
      Dummy := Kill_Xrefs_Queue (Kernel, Xrefs_Loading_Queue_1);
   end Interrupt_Xrefs_Loading;

   -------------------------
   -- Load_Xref_In_Memory --
   -------------------------

   procedure Load_Xref_In_Memory (Kernel : access Kernel_Handle_Record'Class)
   is
      use Load_Xref_Commands;

      C              : Load_Xref_Commands.Generic_Asynchronous_Command_Access;
      Projects_Count : Natural := 0;
      Iter           : Imported_Project_Iterator :=
        Start (Get_Project (Kernel));

      Queue_Name  : String := Xrefs_Loading_Queue_0;

      Std_Files      : File_Array_Access;
      Project_Files  : File_Array_Access;
      Total_Progress : Natural;
   begin
      if Kill_Xrefs_Queue (Kernel, Queue_Name) then
         --  If there is already something on queue 0, then kill it and load
         --  queue 1.
         Queue_Name := Xrefs_Loading_Queue_1;
      else
         declare
            Dummy : constant Boolean :=
              Kill_Xrefs_Queue (Kernel, Xrefs_Loading_Queue_1);
            pragma Unreferenced (Dummy);
            --  Just in case there is something on queue 1
         begin
            null;
         end;
      end if;

      while Current (Iter) /= No_Project loop
         Projects_Count := Projects_Count + 1;
         Next (Iter);
      end loop;

      Std_Files := Get_Predefined_Source_Files (Get_Registry (Kernel).all);
      Project_Files := Get_Source_Files
        (Get_Root_Project (Get_Registry (Kernel).all), True);

      Total_Progress := Std_Files'Length / Sources_Load_Chunk +
        Project_Files'Length / Sources_Load_Chunk;

      if Std_Files'Length mod Sources_Load_Chunk /= 0 then
         Total_Progress := Total_Progress + 1;
      end if;

      if Project_Files'Length mod Sources_Load_Chunk /= 0 then
         Total_Progress := Total_Progress + 1;
      end if;

      Load_Xref_Commands.Create
        (C, -"Load xref info",
         new Load_Xref_Data'
           (Kernel_Handle (Kernel),
            Current_Progress => 0,
            Total_Progress   => Total_Progress,
            Std_Files        => Std_Files,
            Project_Files    => Project_Files,
            Index_In_Std     => 1,
            Index_In_Project => 1,
            Stop             => False),
         Load_Xref_Iterate'Access);

      Launch_Background_Command
        (Kernel,
         Command_Access (C),
         False,
         True,
         Queue_Name,
         Block_Exit => False);
   end Load_Xref_In_Memory;

   ------------
   -- On_Run --
   ------------

   procedure On_Run
     (Kernel : access GObject_Record'Class; Data : File_Project_Record)
   is
      K       : constant Kernel_Handle := Kernel_Handle (Kernel);
      Active  : aliased Boolean := False;
      Args    : Argument_List_Access;
      Success : Boolean;

      procedure Launch
        (Command      : String;
         Arguments    : GNAT.OS_Lib.Argument_List;
         Ext_Terminal : Boolean;
         Title        : String);
      --  Launch Command (with Args) locally, or remotely if necessary.

      ------------
      -- Launch --
      ------------

      procedure Launch
        (Command      : String;
         Arguments    : GNAT.OS_Lib.Argument_List;
         Ext_Terminal : Boolean;
         Title        : String)
      is
         Console : Interactive_Console;

      begin
         Console := Create_Interactive_Console (K, Title);
         Launch_Process
           (K,
            Command              => Command,
            Arguments            => Arguments,
            Server               => Execution_Server,
            Console              => Console,
            Success              => Success,
            Use_Ext_Terminal     => Ext_Terminal,
            Show_Exit_Status     => True);
      end Launch;

   begin
      if Data.File = VFS.No_File then
         declare
            Command : constant String := Display_Entry_Dialog
              (Parent        => Get_Current_Window (K),
               Title         => -"Run Command",
               Message       => -"Enter the command to run:",
               Check_Msg     => -"Use external terminal",
               Key           => Cst_Run_Arguments_History,
               History       => Get_History (K),
               Button_Active => Active'Unchecked_Access,
               Key_Check     => Run_External_Key);

         begin
            if Command = ""
              or else Command (Command'First) = ASCII.NUL
            then
               return;
            else
               Args := Argument_String_To_List (Command);

               if not Is_Local (Execution_Server) then
                  Launch
                    (Args (Args'First).all,
                     Args (Args'First + 1 .. Args'Last), Active,
                     -"Run on " & Get_Nickname (Execution_Server) & ": " &
                     Command);
               else
                  Launch
                    (Args (Args'First).all,
                     Args (Args'First + 1 .. Args'Last), Active,
                     -"Run: " & Command);
               end if;
               Free (Args);
            end if;
         end;

      else
         declare
            Arguments : constant String := Display_Entry_Dialog
              (Parent        => Get_Current_Window (K),
               Title         => -"Arguments Selection",
               Message       => -"Enter the arguments to your application:",
               Check_Msg     => -"Use external terminal",
               Key           => Cst_Run_Arguments_History,
               History       => Get_History (K),
               Key_Check     => Run_External_Key,
               Button_Active => Active'Unchecked_Access);

         begin
            if Arguments = ""
              or else Arguments (Arguments'First) /= ASCII.NUL
            then
               Args := Argument_String_To_List (Arguments);
               if Is_Local (Execution_Server) then
                  Launch
                    (To_Remote (Full_Name (Data.File).all, Execution_Server),
                     Args.all, Active,
                     -"Run: " &
                     Base_Name (Data.File) & ' ' & Krunch (Arguments, 12));
               else
                  Launch
                    (To_Remote (Full_Name (Data.File).all, Execution_Server),
                     Args.all, Active,
                     -"Run on " & Get_Nickname (Execution_Server) & ": " &
                     Base_Name (Data.File) & ' ' & Krunch (Arguments, 12));
               end if;

               Free (Args);
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Run;

   ------------------------
   -- On_Tools_Interrupt --
   ------------------------

   procedure On_Tools_Interrupt
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Interrupt_Latest_Task (Kernel);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Tools_Interrupt;

   --------------------
   -- Add_Build_Menu --
   --------------------

   procedure Add_Build_Menu
     (Menu         : in out Gtk_Menu;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Set_Shortcut : Boolean;
      Mains        : Argument_List)
   is
      Mitem          : Dynamic_Menu_Item;
      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);
      Main  : Virtual_File;

   begin
      if Menu = null then
         Gtk_New (Menu);
      end if;

      --  Accelerators were removed when the menu items were destroyed (just
      --  before the update)
      Builder_Module_ID.Build_Item := null;

      for M in Mains'Range loop
         Mitem := new Dynamic_Menu_Item_Record;
         Gtk.Menu_Item.Initialize (Mitem, Mains (M).all);
         Append (Menu, Mitem);

         --  If the name of the main is not a source file, we might not be
         --  able to resolve it.

         Main := Create (Mains (M).all, Project);
         if Main = VFS.No_File then
            Main := Create_From_Base
              (Executables_Directory (Project) & Mains (M).all);
         end if;

         File_Project_Cb.Object_Connect
           (Mitem, "activate", On_Build'Access,
            Slot_Object => Kernel,
            User_Data   => File_Project_Record'
              (Project => Project,
               File    => Main));

         declare
            Accel_Path : constant String := Make_Menu_Prefix &
              "item" & Image (M);
            Key : Gtk_Accel_Key;
            Found : Boolean;

         begin
            Set_Accel_Path (Mitem, Accel_Path, Group);

            --  The first item in the make menu should have a key binding

            if Set_Shortcut and then M = Mains'First then
               Lookup_Entry
                 (Accel_Path => Accel_Path,
                  Key        => Key,
                  Found      => Found);

               if not Found or else Key.Accel_Key = 0 then
                  Change_Entry
                    (Accel_Path => Accel_Path,
                     Accel_Key  => GDK_F4,
                     Accel_Mods => 0,
                     Replace    => False);
               end if;

               Builder_Module_ID.Build_Item := Gtk_Menu_Item (Mitem);
            end if;
         end;
      end loop;
   end Add_Build_Menu;

   ---------------------------------
   -- Add_Root_Project_Build_Menu --
   ---------------------------------

   procedure Add_Root_Project_Build_Menu
     (Menu         : in out Gtk_Menu;
      Kernel       : access Kernel_Handle_Record'Class;
      Set_Shortcut : Boolean)
   is
      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);
      Mitem : Dynamic_Menu_Item;
   begin
      Mitem := new Dynamic_Menu_Item_Record;
      Gtk.Menu_Item.Initialize (Mitem, -Project_Make_Suffix);
      Append (Menu, Mitem);

      if Set_Shortcut then
         Set_Accel_Path
           (Mitem, Make_Menu_Prefix & (Project_Make_Suffix), Group);
      end if;

      File_Project_Cb.Object_Connect
        (Mitem, "activate", On_Build'Access,
         Slot_Object => Kernel,
         User_Data => File_Project_Record'
           (Project => Get_Project (Kernel),
            File    => VFS.No_File));

      Mitem := new Dynamic_Menu_Item_Record;
      Gtk.Menu_Item.Initialize (Mitem, -All_Make_Suffix);
      Append (Menu, Mitem);

      if Set_Shortcut then
         Set_Accel_Path
           (Mitem, Make_Menu_Prefix & All_Make_Suffix, Group);
      end if;

      File_Project_Cb.Object_Connect
        (Mitem, "activate", On_Build_Project'Access,
         Slot_Object => Kernel,
         User_Data => File_Project_Record'
           (Project => Get_Project (Kernel),
            File    => VFS.No_File));
   end Add_Root_Project_Build_Menu;

   ------------------
   -- Add_Run_Menu --
   ------------------

   procedure Add_Run_Menu
     (Menu         : in out Gtk_Menu;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Set_Shortcut : Boolean;
      Mains        : Argument_List)
   is
      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);
      Mitem : Dynamic_Menu_Item;
   begin
      if Menu = null then
         if Mains'Length = 0 then
            return;
         end if;

         Gtk_New (Menu);
      end if;

      for M in Mains'Range loop
         declare
            Exec : constant String :=
              Get_Executable_Name (Project, Mains (M).all);
         begin
            Mitem := new Dynamic_Menu_Item_Record;
            Gtk.Menu_Item.Initialize (Mitem, Exec);
            Append (Menu, Mitem);
            File_Project_Cb.Object_Connect
              (Mitem, "activate", On_Run'Access,
               Slot_Object => Kernel,
               User_Data => File_Project_Record'
                 (Project => Project,
                  File    => Create
                    (Executables_Directory (Project) & Exec)));

            declare
               Accel_Path : constant String := Run_Menu_Prefix &
                 "item" & Image (M);
               Key        : Gtk_Accel_Key;
               Found      : Boolean;

            begin
               Set_Accel_Path (Mitem, Accel_Path, Group);

               --  The first item in the run menu should have a key binding

               if Set_Shortcut and then M = Mains'First then
                  Lookup_Entry
                    (Accel_Path => Accel_Path,
                     Key        => Key,
                     Found      => Found);

                  if not Found or else Key.Accel_Key = 0 then
                     Change_Entry
                       (Accel_Path => Accel_Path,
                        Accel_Key  => GDK_F2,
                        Accel_Mods => Shift_Mask,
                        Replace    => False);
                  end if;

                  Builder_Module_ID.Build_Item := Gtk_Menu_Item (Mitem);
               end if;
            end;
         end;
      end loop;
   end Add_Run_Menu;

   -----------------------
   -- Cleanup_Accel_Map --
   -----------------------

   procedure Cleanup_Accel_Map (Kernel : access Kernel_Handle_Record'Class) is
      pragma Unreferenced (Kernel);

      procedure Cleanup_Binding
        (Data       : System.Address;
         Accel_Path : String;
         Accel_Key  : Gdk.Types.Gdk_Key_Type;
         Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
         Changed    : Boolean);
      --  Remove one specific binding if necessary

      ---------------------
      -- Cleanup_Binding --
      ---------------------

      procedure Cleanup_Binding
        (Data       : System.Address;
         Accel_Path : String;
         Accel_Key  : Gdk.Types.Gdk_Key_Type;
         Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
         Changed    : Boolean)
      is
         pragma Unreferenced (Data, Accel_Key, Accel_Mods, Changed);
      begin
         --  We reset the entries to "" so that two keybindings with the same
         --  name don't appear in the list.
         if Accel_Path'Length > Make_Menu_Prefix'Length
           and then Accel_Path
             (Accel_Path'First ..
                  Accel_Path'First + Make_Menu_Prefix'Length - 1) =
               Make_Menu_Prefix
           and then Accel_Path /= Make_Menu_Prefix & Custom_Make_Suffix
           and then Accel_Path /= Make_Menu_Prefix & Current_Make_Suffix
           and then Accel_Path /= Make_Menu_Prefix & Project_Make_Suffix
         then
            Change_Entry (Accel_Path, 0, 0, True);

         elsif Accel_Path'Length > Run_Menu_Prefix'Length
           and then Accel_Path
             (Accel_Path'First ..
                  Accel_Path'First + Run_Menu_Prefix'Length - 1) =
               Run_Menu_Prefix
           and then Accel_Path /= Run_Menu_Prefix & Custom_Make_Suffix
         then
            Change_Entry (Accel_Path, 0, 0, True);
         end if;
      end Cleanup_Binding;

   begin
      Gtk.Accel_Map.Foreach
        (System.Null_Address, Cleanup_Binding'Unrestricted_Access);
   end Cleanup_Accel_Map;

   ---------------------
   -- On_View_Changed --
   ---------------------

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class) is
      Mitem : Gtk_Menu_Item;
      Menu1 : Gtk_Menu renames Builder_Module_ID.Make_Menu;
      Menu2 : Gtk_Menu renames Builder_Module_ID.Run_Menu;
      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);
   begin
      --  Free the previous shortcuts if needed. We only keep the ones from
      --  the current project, to avoid an ever expending custom_keys file,
      --  and to limit the change of a duplicate key binding appearing in the
      --  menu.
      if Builder_Module_ID.Last_Project_For_Menu /= Get_Project (Kernel) then
         Cleanup_Accel_Map (Kernel);
      end if;

      --  Only add the shortcuts for the root project
      --  Special case: if the root project is an extending project (which is
      --  the case as soon as one of the other projects in the hierarchy is
      --  also an extending, for instance when files where modified locally),
      --  we want to add the main units of the parent as well, otherwise the
      --  build menu becomes useless as soon as we are using extending
      --  projects.

      declare
         P : Project_Type := Get_Project (Kernel);
      begin
         if Builder_Module_ID.Make_Menu /= null then
            Remove_All_Children (Builder_Module_ID.Make_Menu,
                                 Is_Dynamic_Menu_Item'Access);
         end if;

         if Builder_Module_ID.Run_Menu /= null then
            Remove_All_Children (Builder_Module_ID.Run_Menu,
                                 Is_Dynamic_Menu_Item'Access);
         end if;

         while P /= No_Project loop
            declare
               Mains : Argument_List := Get_Attribute_Value
                 (P, Attribute => Main_Attribute);
            begin
               if Mains'Length /= 0 then
                  --  Use the main units of the extending project, but we
                  --  should still compile in the context of the root project
                  Add_Build_Menu
                    (Menu         => Builder_Module_ID.Make_Menu,
                     Project      => Get_Project (Kernel),
                     Kernel       => Kernel,
                     Set_Shortcut => True,
                     Mains        => Mains);
                  Add_Run_Menu
                    (Menu         => Builder_Module_ID.Run_Menu,
                     Project      => Get_Project (Kernel),
                     Kernel       => Kernel,
                     Set_Shortcut => True,
                     Mains        => Mains);
                  Free (Mains);
                  exit;
               end if;

               P := Parent_Project (P);
               Free (Mains);
            end;
         end loop;
      end;

      Add_Root_Project_Build_Menu (Builder_Module_ID.Make_Menu, Kernel, True);

      --  No main program ?

      Mitem := new Dynamic_Menu_Item_Record;
      Gtk.Menu_Item.Initialize (Mitem, -Current_Make_Suffix);
      Append (Menu1, Mitem);
      Set_Accel_Path
        (Mitem, Make_Menu_Prefix & Current_Make_Suffix, Group);
      File_Project_Cb.Object_Connect
        (Mitem, "activate", On_Build'Access,
         Slot_Object => Kernel,
         User_Data => File_Project_Record'
           (Project => No_Project,
            File    => VFS.No_File));

      if Builder_Module_ID.Build_Item = null then
         Add_Accelerator
           (Mitem, "activate", Group, GDK_F4, 0,
            Gtk.Accel_Group.Accel_Visible);
         Builder_Module_ID.Build_Item := Mitem;
      end if;

      Mitem := new Dynamic_Menu_Item_Record;
      Gtk.Menu_Item.Initialize (Mitem, -Custom_Make_Suffix);
      Append (Menu1, Mitem);
      Kernel_Callback.Connect
        (Mitem, "activate", On_Custom'Access,
         User_Data => Kernel_Handle (Kernel));
      Add_Accelerator
        (Mitem, "activate", Group, GDK_F9, 0, Gtk.Accel_Group.Accel_Visible);
      Set_Accel_Path (Mitem, "<gps>/Build/Make/Custom", Group);

      --  Should be able to run any program

      Mitem := new Dynamic_Menu_Item_Record;
      Gtk.Menu_Item.Initialize (Mitem, -Custom_Make_Suffix);
      Append (Menu2, Mitem);
               Set_Accel_Path
                 (Mitem, -(Run_Menu_Prefix) & (-Custom_Make_Suffix), Group);
      File_Project_Cb.Object_Connect
        (Mitem, "activate", On_Run'Access,
         Slot_Object => Kernel,
         User_Data   => File_Project_Record'
           (Project => Get_Project (Kernel), File => VFS.No_File));
      Show_All (Menu1);
      Show_All (Menu2);

      if Get_Pref (Automatic_Xrefs_Load) then
         Load_Xref_In_Memory (Kernel_Handle (Kernel));
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_View_Changed;

   --------------------
   -- Append_To_Menu --
   --------------------

   procedure Append_To_Menu
     (Builder : access Builder_Contextual;
      Object  : access GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object, Builder);
      --  The filter garantees we are on a File_Selection_Context

      Mains : Argument_List := Get_Attribute_Value
        (Project_Information (Context),
         Attribute => Main_Attribute);
      M     : Gtk_Menu := Gtk_Menu (Menu);

   begin
      if Mains'Length /= 0 then
         Add_Build_Menu
           (Menu         => M,
            Project      => Project_Information (Context),
            Kernel       => Get_Kernel (Context),
            Set_Shortcut => False,
            Mains        => Mains);
      end if;
      Free (Mains);
   end Append_To_Menu;

   --------------------
   -- Append_To_Menu --
   --------------------

   procedure Append_To_Menu
     (Factory : access Run_Contextual;
      Object  : access GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Factory, Object);
      --  The filter garantees we are on a File_Selection_Context

      Mains : Argument_List := Get_Attribute_Value
        (Project_Information (Context),
         Attribute => Main_Attribute);
      M     : Gtk_Menu := Gtk_Menu (Menu);

   begin
      Add_Run_Menu
        (Menu         => M,
         Project      => Project_Information (Context),
         Kernel       => Get_Kernel (Context),
         Set_Shortcut => False,
         Mains        => Mains);
      Free (Mains);
   end Append_To_Menu;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Build : constant String := '/' & (-"Build") & '/';
      Tools : constant String := '/' & (-"Tools") & '/';
      Mitem : Gtk_Menu_Item;
      Menu  : Gtk_Menu;
   begin
      --  This memory is allocated once, and lives as long as the application.

      Builder_Module_ID := new Builder_Module_ID_Record;
      Register_Module
        (Module      => Builder_Module_ID,
         Kernel      => Kernel,
         Module_Name => Builder_Module_Name,
         Priority    => Default_Priority);

      Register_Menu (Kernel, "/_" & (-"Build"), Ref_Item => -"Tools");
      Register_Menu (Kernel, Build, -"Check _Syntax", "",
                     On_Check_Syntax'Access);
      Register_Menu (Kernel, Build, -"_Compile File", "",
                     On_Compile'Access, null, GDK_F4, Shift_Mask);

      Register_Contextual_Submenu
        (Kernel,
         Name    => "Build",
         Filter  => Lookup_Filter (Kernel, "Project only"),
         Submenu => new Builder_Contextual);
      Register_Contextual_Submenu
        (Kernel,
         Name    => "Run",
         Filter  => Lookup_Filter (Kernel, "Project only"),
         Submenu => new Run_Contextual);

      --  Dynamic make menu

      Mitem := Register_Menu (Kernel, Build, -"_Make", "", null);
      Gtk_New (Menu);
      Builder_Module_ID_Record (Builder_Module_ID.all).Make_Menu := Menu;
      Set_Submenu (Mitem, Menu);

      --  Dynamic run menu
      Mitem := Register_Menu
        (Kernel, Build, -"_Run", Stock_Execute, null);
      Gtk_New (Menu);
      Builder_Module_ID_Record (Builder_Module_ID.all).Run_Menu := Menu;
      Set_Submenu (Mitem, Menu);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Build, Mitem);

      Register_Menu
        (Kernel, Build, -"Recompute C/C++ _Xref info", "",
         On_Compute_Xref'Access);
      Register_Menu
        (Kernel, Build, -"Load Xref info in memory", "",
         On_Load_Xref_In_Memory'Access);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Tools, Mitem);
      Register_Menu
        (Kernel, Tools, -"_Interrupt", Stock_Stop, On_Tools_Interrupt'Access,
         null, GDK_C, Control_Mask + Shift_Mask);

      Add_Hook (Kernel, Project_View_Changed_Hook,
                Wrapper (On_View_Changed'Access),
                Name => "builder_module.on_view_changed");
      Add_Hook
        (Kernel => Kernel,
         Hook   => Compilation_Finished_Hook,
         Func   => Wrapper (On_Compilation_Finished'Access),
         Name   => "load_xrefs");

      Register_Command
        (Kernel, "compile",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class   => Get_File_Class (Kernel),
         Handler => Compile_Command'Access);
      Register_Command
        (Kernel, "check_syntax",
         Class   => Get_File_Class (Kernel),
         Handler => Compile_Command'Access);
      Register_Command
        (Kernel, "shadow_check_syntax",
         Class   => Get_File_Class (Kernel),
         Handler => Compile_Command'Access);
      Register_Command
        (Kernel, "make",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class   => Get_File_Class (Kernel),
         Handler => Compile_Command'Access);
      Register_Command
        (Kernel, "compute_xref",
         Handler => Compile_Command'Access);
      Register_Command
        (Kernel, "get_build_output",
         Handler => Compile_Command'Access);
   end Register_Module;

   ---------------
   -- Deep_Free --
   ---------------

   procedure Deep_Free (D : in out Compute_Xref_Data_Access) is
   begin
      if D /= null then
         Unchecked_Free (D.Iter);
         Unchecked_Free (D);
      end if;
   end Deep_Free;

end Builder_Module;
