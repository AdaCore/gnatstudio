-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                 Copyright (C) 2001-2007, AdaCore                  --
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

with Ada.Unchecked_Deallocation;
with Ada.Tags;                  use Ada.Tags;
with Ada.Strings;               use Ada.Strings;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with System;

with Interfaces.C.Strings;      use Interfaces.C.Strings;

with GNAT.Expect;               use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT; use GNAT.OS_Lib;
with GNAT.Scripts;              use GNAT.Scripts;
with GNAT.Strings;
with GNAT.Case_Util;            use GNAT.Case_Util;

with Glib;                      use Glib;
with Glib.Error;                use Glib.Error;
with Glib.Convert;              use Glib.Convert;
with Glib.Unicode;              use Glib.Unicode;
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
with Gtkada.MDI;                use Gtkada.MDI;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Commands;       use GPS.Kernel.Commands;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Custom;         use GPS.Kernel.Custom;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
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
with Remote.Path.Translator;    use Remote, Remote.Path.Translator;

with Basic_Types;
with Std_Dialogs;               use Std_Dialogs;
with String_Utils;              use String_Utils;
with GUI_Utils;                 use GUI_Utils;
with OS_Utils;                  use OS_Utils;
with Traces;                    use Traces;
with Commands;                  use Commands;
with Commands.Builder;          use Commands.Builder;

with Commands.Generic_Asynchronous;

package body Builder_Module is

   Me : constant Debug_Handle := Create ("Builder");

   Shell_Env : constant String := Getenv ("SHELL").all;

   Cst_Run_Arguments_History : constant History_Key := "gvd_run_arguments";
   --  The key in the history for the arguments to the run command.
   --  WARNING: this constant is shared with gvd-menu.adb, since we want to
   --  have the same history for the debugger arguments.

   Run_External_Key : constant History_Key := "run_external_terminal";
   --  The key in the history for the check button "run in external terminal"

   Make_Menu_Prefix : constant String := "<gps>/Build/Make/";
   Run_Menu_Prefix : constant String := "<gps>/Build/Run/";
   --  Prefixes used in the accel path for the various menus

   Item_Accel_Path : constant String := "item";
   --  Prefix used in accel path for items defined in this module

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

   Sources_Load_Chunk : constant Integer := 1;
   --  The size of the chunk of files loaded by the xrefs loader.
   --  ??? This should be configurable

   Xrefs_Loading_Queue : constant String := "xrefs_loading";

   type Files_Callback_Data is new Callback_Data_Record with record
      Files  : File_Array_Access;

      Buffer : Unbounded_String;
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

   procedure Interrupt_Xrefs_Loading
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Interrupts all xrefs loading

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
   --  Free memory associated to D

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (LI_Handler_Iterator_Access, LI_Handler_Iterator_Access_Access);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Compute_Xref_Data, Compute_Xref_Data_Access);

   procedure Xref_Iterate
     (Xref_Data : in out Compute_Xref_Data_Access;
      Command   : Command_Access;
      Result    : out Command_Return_Type);
   --  Query the Xref information for the next files in the project

   package Xref_Commands is new Commands.Generic_Asynchronous
     (Data_Type => Compute_Xref_Data_Access,
      Free      => Deep_Free);

   ------------------------------
   -- Loading cross-references --
   ------------------------------

   procedure Loads_Xrefs_From_File
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File);

   ----------
   -- Misc --
   ----------

   procedure Free (Ar : in out String_List);
   procedure Free (Ar : in out String_List_Access);
   --  Free the memory associate with Ar

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
      Mains        : String_List;
      Set_Shortcut : Boolean);
   --  Add new entries for all the main subprograms of Project.
   --  If Menu is null, a new one is created if there are any entries
   --  If Set_Shortcut is true, the F4 shortcut is set for the first entry.

   procedure Add_Build_Menu
     (Menu    : in out Gtk_Menu;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class;
      Library : String);
   --  Add new entry for a library project

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
      Mains        : Argument_List;
      Set_Shortcut : Boolean);
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

   procedure Parse_Compiler_Output (Data : Process_Data; Output : String);
   --  Called whenever new output from the compiler is available

   procedure Free_Temporary_Files (Data : Process_Data; Status : Integer);
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
   --  Build->Make->All/Compile all sources menus

   procedure On_Build_Library
     (Kernel : access GObject_Record'Class; Data : File_Project_Record);
   --  To build a library project

   procedure On_Build
     (Kernel      : Kernel_Handle;
      File        : Virtual_File;
      Project     : Project_Type;
      Main_Units  : Boolean := False;
      Library     : Boolean := False;
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

   function On_Compilation_Starting
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   --  Called when the compilation is starting

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

   procedure On_Project_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called every time a new project is loaded

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
         Remove_Location_Category (Kernel, Shadow_Category);
      else
         Console.Clear (Kernel);
         Remove_Location_Category (Kernel, Error_Category);

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
      Project_Str : GNAT.Strings.String_Access;
      Result      : Argument_List_Access;
      Vars        : Argument_List_Access;

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

   procedure Parse_Compiler_Output (Data : Process_Data; Output : String) is
      Last_EOL : Natural := 1;
      Str      : GNAT.OS_Lib.String_Access;
      Valid    : Boolean;
      Invalid_Pos : Natural;
   begin
      if not Data.Process_Died then
         Last_EOL := Index (Output, (1 => ASCII.LF), Backward);

         --  In case we did not find any LF in the output, we'll just append it
         --  in the current buffer.

         if Last_EOL = 0 then
            Append
              (Files_Callback_Data (Data.Callback_Data.all).Buffer, Output);
            return;
         end if;

      else
         if Output'Length > 0 then
            Last_EOL := Output'Length + 1;
         end if;
      end if;

      --  Collect the relevant portion of the output

      if Output'Length > 0 then
         Str := new String'
           (To_String (Files_Callback_Data (Data.Callback_Data.all).Buffer)
            & Output (Output'First .. Last_EOL - 1) & ASCII.LF);

         Files_Callback_Data (Data.Callback_Data.all).Buffer :=
           To_Unbounded_String (Output (Last_EOL + 1 .. Output'Last));

      elsif Data.Process_Died then
         Str := new String'
           (To_String (Files_Callback_Data (Data.Callback_Data.all).Buffer)
            & ASCII.LF);
         Files_Callback_Data (Data.Callback_Data.all).Buffer :=
           Null_Unbounded_String;
      else
         return;
      end if;

      --  If we reach this point, this means we have collected some output to
      --  parse. In this case, verify that it is proper UTF-8 before
      --  transmitting it to the rest of GPS.

      --  It is hard to determine which encoding the compiler result is,
      --  especially given that we are supporting third-party compilers, build
      --  scripts, etc. Therefore, we run the output through UTF8_Validate. If
      --  a string validates, there is a great chance that it is indeed UTF8.

      UTF8_Validate (Str.all, Valid, Invalid_Pos);

      if Valid then
         Process_Builder_Output
           (Kernel  => Data.Kernel,
            Command => Data.Command,
            Output  => Str.all,
            Quiet   => False);
         Free (Str);
      else
         --  If the compiler output is not valid UTF-8, the most likely option
         --  is that it is encoded using the locale.

         declare
            Tentative     : chars_ptr;
            Read, Written : aliased Natural;
            Error         : GError_Access := new GError'(null);
            procedure Unchecked_Free is new Ada.Unchecked_Deallocation
              (GError, GError_Access);
         begin
            Tentative := Locale_To_UTF8
              (Str.all, Read'Access, Written'Access, Error);
            Free (Str);

            if Error.all = null then
               --  We could convert, transmit the message.
               Process_Builder_Output
                 (Kernel  => Data.Kernel,
                  Command => Data.Command,
                  Output  => Value (Tentative),
                  Quiet   => False);
            else
               --  We could not convert, insert a message in the console and
               --  skip.
               Console.Insert
                 (Data.Kernel,
                  -"Could not convert compiler output to UTF8: " &
                  Get_Message (Error.all), Mode => Console.Error);
               Error_Free (Error.all);
            end if;

            Free (Tentative);
            Unchecked_Free (Error);
         end;
      end if;
   end Parse_Compiler_Output;

   --------------------------
   -- Free_Temporary_Files --
   --------------------------

   procedure Free_Temporary_Files
     (Data : Process_Data; Status : Integer)
   is
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

      --  Raise the messages window is compilation was unsuccessful
      --  and no error was parsed. See D914-005

      if Category_Count (Data.Kernel, Error_Category) = 0
        and then Status /= 0
      then
         Console.Raise_Console (Data.Kernel);
      end if;

      --  ??? should also pass the Status value to Compilation_Finished
      --  and to the corresponding hook

      Compilation_Finished (Data.Kernel, Error_Category);
   end Free_Temporary_Files;

   --------------
   -- On_Build --
   --------------

   procedure On_Build
     (Kernel      : Kernel_Handle;
      File        : Virtual_File;
      Project     : Project_Type;
      Main_Units  : Boolean := False;
      Library     : Boolean := False;
      Synchronous : Boolean := False;
      Extra_Args  : Argument_List_Access := null)
   is
      Old_Dir     : constant Dir_Name_Str := Get_Current_Dir;
      Cmd         : OS_Lib.String_Access;
      Args        : Argument_List_Access;

      Context     : Selection_Context;
      Prj         : Project_Type;
      Langs       : Argument_List :=
                      Get_Languages (Get_Project (Kernel), Recursive => True);
      Syntax      : Command_Syntax;
      Success     : Boolean;
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

               Prj := Extending_Project
                 (Get_Project_From_File (Get_Registry (Kernel).all, F),
                  Recurse => True);
               Args := Compute_Arguments (Kernel, Prj, "", F);
            end;

         --  There is no current file, so we can't compile anything

         else
            Console.Insert
              (Kernel, -"No file selected, cannot build",
               Mode => Console.Error);
            return;
         end if;

      else
         Prj := Extending_Project (Project, Recurse => True);
         Args := Compute_Arguments
           (Kernel, Prj, "", File,
            Unique_Project => not Main_Units and not Library,
            Extra_Args     => Common_Args);
         Change_Dir (Dir_Name (Project_Path (Project)).all);
      end if;

      case Syntax is
         when GNAT_Syntax =>
            Cmd := new String'(Get_Attribute_Value
              (Prj, Compiler_Command_Attribute,
               Default => "gnatmake", Index => "ada"));

         when Make_Syntax =>
            Cmd := new String'("gprmake");
      end case;

      if Compilation_Starting (Kernel, Error_Category, Quiet => False) then
         String_List_Utils.String_List.Append
           (Builder_Module_ID.Output,
            Cmd.all & " " & Argument_List_To_Quoted_String (Args.all));
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
      end if;

      Free (Cmd);
      Free (Args);
      Basic_Types.Unchecked_Free (Common_Args);

      Change_Dir (Old_Dir);

   exception
      when Invalid_Process =>
         Console.Insert (Kernel, -"Invalid command", Mode => Console.Error);
         Change_Dir (Old_Dir);
         Free (Cmd);
         Free (Args);

      when E : others =>
         Trace (Exception_Handle, E);
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
        (Kernel_Handle (Kernel), Data.File, Data.Project, Main_Units => True);
   end On_Build_Project;

   ----------------------
   -- On_Build_Library --
   ----------------------

   procedure On_Build_Library
     (Kernel : access GObject_Record'Class; Data : File_Project_Record) is
   begin
      On_Build
        (Kernel_Handle (Kernel), No_File, Data.Project, Library => True);
   end On_Build_Library;

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
            Mode => Console.Error);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
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
      Prj             : constant Project_Type := Get_Project (Kernel);
      Old_Dir         : constant Dir_Name_Str := Get_Current_Dir;
      Cmd             : OS_Lib.String_Access;
      Fd              : Process_Descriptor_Access;
      Local_File      : OS_Lib.String_Access;
      Lang            : String :=
                          Get_Language_From_File
                            (Get_Language_Handler (Kernel), File);
      Common_Args     : Argument_List_Access;
      Args            : Argument_List_Access;
      Shadow_Path     : OS_Lib.String_Access;
      Compilable_File : Virtual_File := File;
      Success         : Boolean;
      Cb_Data         : Files_Callback_Data_Access;

   begin
      --  Is there a file to compile ?

      if File = VFS.No_File then
         if not Quiet then
            Console.Insert
              (Kernel, -"No file name, cannot compile",
               Mode => Console.Error);
         end if;

         return;
      end if;

      To_Lower (Lang);

      --  Determine the project for the file

      if Prj = No_Project then
         if not Quiet then
            Console.Insert
              (Kernel, -"Could not determine the project for file: "
               & Full_Name (File).all,
               Mode => Console.Error);
         end if;

         return;
      end if;

      --  Determine the language for the file

      if Lang = "" then
         if not Quiet then
            Console.Insert
              (Kernel, -"Could not determine the language for file: "
               & Full_Name (File).all,
               Mode => Console.Error);
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
            --  Do nothing if one of the files already exists

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

      --  Remove the entry corresponding to file in the location view.
      --  This is needed otherwise the location view is not cleared in case
      --  there is another compilation running.

      Remove_Location_Category (Kernel, Error_Category, File);

      if Compilation_Starting (Kernel, Error_Category, Quiet => Quiet) then
         String_List_Utils.String_List.Append
           (Builder_Module_ID.Output,
            Cmd.all & " " & Argument_List_To_Quoted_String
              (Common_Args.all & Args.all));

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
      end if;

      Free (Args);
      Basic_Types.Unchecked_Free (Common_Args);
      Free (Cmd);
      Free (Local_File);
      Free (Shadow_Path);
      Change_Dir (Old_Dir);

   exception
      when Invalid_Process =>
         Console.Insert (Kernel, -"Invalid command", Mode => Console.Error);
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
      Kernel     : constant Kernel_Handle := Get_Kernel (Data);
      Node       : List_Node;
      Info       : Virtual_File;
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
           (Kernel, -"No file selected, cannot compile",
            Mode => Console.Error);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
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
      Args    : Argument_List_Access;

   begin
      if Cmd = "" or else Cmd (Cmd'First) = ASCII.NUL then
         return;
      end if;

      if Compilation_Starting (Kernel, Error_Category, Quiet => False) then
         if Shell_Env /= "" and then Is_Local (Build_Server) then
            --  Launch "$SHELL -c cmd" if $SHELL is set and the build server
            --  is local.

            String_List_Utils.String_List.Append
              (Builder_Module_ID.Output, Shell_Env & " -c " & Cmd);
            Args := new Argument_List'(new String'("-c"), new String'(Cmd));
            Launch_Process
              (Kernel,
               Command              => Shell_Env,
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
               Synchronous          => False,
               Show_Exit_Status     => True);

         else
            Args := Argument_String_To_List (Cmd);

            --  ??? Is this always the Build_Server ? Should ask the user ?
            String_List_Utils.String_List.Append
              (Builder_Module_ID.Output,
               Args (Args'First).all & " "
               & Argument_List_To_Quoted_String
                 (Args (Args'First + 1 .. Args'Last)));

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
         end if;

         Free (Args);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Custom;

   ------------------
   -- Xref_Iterate --
   ------------------

   procedure Xref_Iterate
     (Xref_Data : in out Compute_Xref_Data_Access;
      Command   : Command_Access;
      Result    : out Command_Return_Type)
   is
      pragma Warnings (Off, Xref_Data);
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
         Insert (D.Kernel, Msg, Mode => Console.Error);
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
      when E : others => Trace (Exception_Handle, E);
   end On_Compute_Xref;

   -------------------------
   -- Load_Xref_From_File --
   -------------------------

   procedure Loads_Xrefs_From_File
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File) is
   begin
      Update_Xref
        (Get_Or_Create
           (Get_Database (Kernel), File));
   end Loads_Xrefs_From_File;

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
      when E : others => Trace (Exception_Handle, E);
   end On_Load_Xref_In_Memory;

   -----------------------------
   -- On_Compilation_Starting --
   -----------------------------

   function On_Compilation_Starting
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean
   is
      D : constant String_Boolean_Hooks_Args :=
            String_Boolean_Hooks_Args (Data.all);
   begin
      --  Small issue here: if the user cancels the compilation in one of the
      --  custom hooks the user might have connected, then all changes done
      --  here (increase Build_Count) will not be undone since
      --  On_Compilation_Finished is not called.

      --  Ask for saving sources/projects before building.
      --  Do this before checking the project, in case we have a default
      --  project whose name is changed when saving

      if not D.Bool
        and then not Save_MDI_Children (Kernel, Force => Get_Pref (Auto_Save))
      then
         return False;
      end if;

      if not D.Bool and then Builder_Module_ID.Build_Count = 0 then
         Clear_Compilation_Output (Kernel_Handle (Kernel), False);
      end if;

      Interrupt_Xrefs_Loading (Kernel);

      Builder_Module_ID.Build_Count := Builder_Module_ID.Build_Count + 1;

      if not D.Bool then
         Console.Raise_Console (Kernel);
      end if;

      return True;
   end On_Compilation_Starting;

   -----------------------------
   -- On_Compilation_Finished --
   -----------------------------

   procedure On_Compilation_Finished
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Data);
   begin
      if Builder_Module_ID.Build_Count > 0 then
         Builder_Module_ID.Build_Count := Builder_Module_ID.Build_Count - 1;
      end if;

      if Builder_Module_ID.Build_Count = 0 then
         if Get_Pref (Automatic_Xrefs_Load) then
            Load_Xref_In_Memory (Kernel);
         end if;
      end if;
   end On_Compilation_Finished;

   -----------------------------
   -- Interrupt_Xrefs_Loading --
   -----------------------------

   procedure Interrupt_Xrefs_Loading
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Kill_File_Iteration (Kernel, Xrefs_Loading_Queue);
   end Interrupt_Xrefs_Loading;

   -------------------------
   -- Load_Xref_In_Memory --
   -------------------------

   procedure Load_Xref_In_Memory
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Do_On_Each_File
        (Kernel,
         Loads_Xrefs_From_File'Access,
         Sources_Load_Chunk,
         Xrefs_Loading_Queue,
         True,
         "load xrefs info");
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
      --  Launch Command (with Args) locally, or remotely if necessary

      procedure Launch
        (Command      : String;
         Ext_Terminal : Boolean;
         Title        : String);
      --  Ditto, with command being a simple string (may contain parameters)

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
         Child   : MDI_Child;
      begin
         Console := Create_Interactive_Console (K, Title);
         Clear (Console);
         Child := Find_MDI_Child (Get_MDI (K), Console);

         if Child /= null then
            Raise_Child (Child);
         end if;

         Launch_Process
           (K,
            Command          => Command,
            Arguments        => Arguments,
            Server           => Execution_Server,
            Console          => Console,
            Success          => Success,
            Use_Ext_Terminal => Ext_Terminal,
            Show_Exit_Status => True);
      end Launch;

      procedure Launch
        (Command      : String;
         Ext_Terminal : Boolean;
         Title        : String)
      is
         Console    : Interactive_Console;
         Child      : MDI_Child;
         Local_Args : Argument_List_Access;

      begin
         Console := Create_Interactive_Console (K, Title);
         Clear (Console);
         Child := Find_MDI_Child (Get_MDI (K), Console);

         if Child /= null then
            Raise_Child (Child);
         end if;

         if not Ext_Terminal
           and then Shell_Env /= ""
           and then Is_Local (Build_Server)
         then
            --  Launch "$SHELL -c cmd" if $SHELL is set and the build server
            --  is local.

            Local_Args :=
              new Argument_List'(new String'("-c"), new String'(Command));
            Launch_Process
              (K,
               Command              => Shell_Env,
               Arguments            => Local_Args.all,
               Server               => Execution_Server,
               Console              => Console,
               Success              => Success,
               Use_Ext_Terminal     => Ext_Terminal,
               Show_Exit_Status     => True);

         else
            Local_Args := Argument_String_To_List (Command);
            Launch_Process
              (K,
               Command          => Local_Args (Local_Args'First).all,
               Arguments        =>
                 Local_Args (Local_Args'First + 1 .. Local_Args'Last),
               Server           => Execution_Server,
               Console          => Console,
               Success          => Success,
               Use_Ext_Terminal => Ext_Terminal,
               Show_Exit_Status => True);
         end if;

         Free (Local_Args);
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
               if Is_Local (Execution_Server) then
                  Launch (Command, Active, -"Run: " & Command);
               else
                  Launch
                    (Command, Active,
                     -"Run on " & Get_Nickname (Execution_Server) & ": " &
                     Command);
               end if;
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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
   end On_Tools_Interrupt;

   --------------------
   -- Add_Build_Menu --
   --------------------

   procedure Add_Build_Menu
     (Menu         : in out Gtk_Menu;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Mains        : Argument_List;
      Set_Shortcut : Boolean)
   is
      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);
      Mitem : Dynamic_Menu_Item;
      Main  : Virtual_File;
      Tmp   : Boolean;
      pragma Unreferenced (Tmp);

   begin
      if Menu = null then
         Gtk_New (Menu);
      end if;

      --  Accelerators were removed when the menu items were destroyed (just
      --  before the update)

      for M in reverse Mains'Range loop
         Mitem := new Dynamic_Menu_Item_Record;
         Gtk.Menu_Item.Initialize (Mitem, Mains (M).all);
         Prepend (Menu, Mitem);

         --  If the name of the main is not a source file, we might not be
         --  able to resolve it.

         Main := Create (Mains (M).all, Project);
         if Main = VFS.No_File then
            Main := Create_From_Base
              (Executables_Directory (Project) & Mains (M).all);
         end if;

         File_Project_Cb.Object_Connect
           (Mitem, Signal_Activate, On_Build'Access,
            Slot_Object => Kernel,
            User_Data   => File_Project_Record'
              (Project => Project,
               File    => Main));

         if Set_Shortcut and then M = Mains'First then
            Set_Accel_Path
              (Mitem, Make_Menu_Prefix & Item_Accel_Path & Image (M), Group);
         end if;
      end loop;
   end Add_Build_Menu;

   --------------------
   -- Add_Build_Menu --
   --------------------

   procedure Add_Build_Menu
     (Menu    : in out Gtk_Menu;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class;
      Library : String)
   is
      Mitem : Dynamic_Menu_Item;
   begin
      if Menu = null then
         Gtk_New (Menu);
      end if;

      Mitem := new Dynamic_Menu_Item_Record;
      Gtk.Menu_Item.Initialize (Mitem, Library);
      Append (Menu, Mitem);

      File_Project_Cb.Object_Connect
        (Mitem, Signal_Activate, On_Build_Library'Access,
         Slot_Object => Kernel,
         User_Data   => File_Project_Record'
           (Project => Project,
            File    => No_File));
   end Add_Build_Menu;

   ---------------------------------
   -- Add_Root_Project_Build_Menu --
   ---------------------------------

   procedure Add_Root_Project_Build_Menu
     (Menu         : in out Gtk_Menu;
      Kernel       : access Kernel_Handle_Record'Class;
      Set_Shortcut : Boolean)
   is
      pragma Warnings (Off, Menu);
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
        (Mitem, Signal_Activate, On_Build'Access,
         Slot_Object => Kernel,
         User_Data   => File_Project_Record'
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
        (Mitem, Signal_Activate, On_Build_Project'Access,
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
      Mains        : Argument_List;
      Set_Shortcut : Boolean)
   is
      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);
      Mitem : Dynamic_Menu_Item;
      Tmp   : Boolean;
      pragma Unreferenced (Tmp);
   begin
      if Menu = null then
         if Mains'Length = 0 then
            return;
         end if;

         Gtk_New (Menu);
      end if;

      for M in reverse Mains'Range loop
         declare
            Exec : constant String :=
                     Get_Executable_Name (Project, Mains (M).all);
         begin
            Mitem := new Dynamic_Menu_Item_Record;
            Gtk.Menu_Item.Initialize (Mitem, Exec);
            Prepend (Menu, Mitem);
            File_Project_Cb.Object_Connect
              (Mitem, Signal_Activate, On_Run'Access,
               Slot_Object => Kernel,
               User_Data => File_Project_Record'
                 (Project => Project,
                  File    => Create
                    (Executables_Directory (Project) & Exec)));

            if Set_Shortcut and then M = Mains'First then
               Set_Accel_Path
                 (Mitem, Run_Menu_Prefix & Item_Accel_Path & Image (M), Group);
            end if;
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
         Tmp : Boolean;
         pragma Unreferenced (Tmp);

      begin
         --  We reset the entries to "" so that two keybindings with the same
         --  name don't appear in the list.
         if Accel_Path'Length >
             Make_Menu_Prefix'Length + Item_Accel_Path'Length
           and then Accel_Path
             (Accel_Path'First ..
                  Accel_Path'First + Make_Menu_Prefix'Length
                  + Item_Accel_Path'Length - 1) =
               Make_Menu_Prefix & Item_Accel_Path
           and then Accel_Path /= Make_Menu_Prefix & Custom_Make_Suffix
           and then Accel_Path /= Make_Menu_Prefix & Current_Make_Suffix
           and then Accel_Path /= Make_Menu_Prefix & Project_Make_Suffix
         then
            Tmp := Change_Entry (Accel_Path, 0, 0, True);

         elsif Accel_Path'Length > Run_Menu_Prefix'Length
           and then Accel_Path
             (Accel_Path'First ..
                  Accel_Path'First + Run_Menu_Prefix'Length - 1) =
               Run_Menu_Prefix
           and then Accel_Path /= Run_Menu_Prefix & Custom_Make_Suffix
         then
            Tmp := Change_Entry (Accel_Path, 0, 0, True);
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
         Loaded_Project   : constant Project_Type := Get_Project (Kernel);
         Loaded_Mains     : Argument_List :=
                              Get_Attribute_Value
                                (Loaded_Project,
                                 Attribute => Main_Attribute);
         Loaded_Has_Mains : constant Boolean := Loaded_Mains'Length > 0;
         Extended_Project : constant Project_Type :=
                              Parent_Project (Loaded_Project);
         Iter             : Imported_Project_Iterator :=
                              Start (Loaded_Project);
         Current_Project  : Project_Type := Current (Iter);
         Set_Shortcut     : Boolean := True;
      begin
         if Builder_Module_ID.Make_Menu /= null then
            Remove_All_Children (Builder_Module_ID.Make_Menu,
                                 Is_Dynamic_Menu_Item'Access);
         end if;

         if Builder_Module_ID.Run_Menu /= null then
            Remove_All_Children (Builder_Module_ID.Run_Menu,
                                 Is_Dynamic_Menu_Item'Access);
         end if;

         --  ??? Few limitations on what is done here:
         --
         --  1. We iterate over all imported projects. We look at the extended
         --  project main files only if the loaded project has none. This
         --  means that it will work when the current project inherits its Main
         --  attribute from the extended project or when it redefines it except
         --  with an empy list in which case the extended project main files
         --  should be looked at but they are not.
         --
         --  2. We only consider the project extended by the lodaded project as
         --  a special case. Other extended projects are handled as imported
         --  projects whereas they should also be handled separately.
         --
         --  3. This loop is shared with the one in the builder module that is
         --  responsible for filling the /Debug/Initialize menu. Both should
         --  be factorized but this requires maniplulating (main file, project)
         --  association since a main file needs to be build in a specific
         --  context which is not necessarily the one in which is it defined
         --  (see the comment below about main files from extended projects).

         while Current_Project /= No_Project loop
            if not Loaded_Has_Mains
              or else Current_Project /= Extended_Project
            then
               declare
                  Mains : Argument_List :=
                            Get_Attribute_Value
                              (Current_Project, Attribute => Main_Attribute);
                  Context_Project : Project_Type;
               begin
                  --  We should compile main files from the extended project
                  --  in the context of the loaded project.

                  if Current_Project = Extended_Project then
                     Context_Project := Loaded_Project;
                  else
                     Context_Project := Current_Project;
                  end if;

                  if Mains'Length /= 0 then
                     Set_Shortcut := Current_Project = Loaded_Project;

                     Add_Build_Menu
                       (Menu         => Builder_Module_ID.Make_Menu,
                        Project      => Context_Project,
                        Kernel       => Kernel,
                        Mains        => Mains,
                        Set_Shortcut => Set_Shortcut);

                     Add_Run_Menu
                       (Menu         => Builder_Module_ID.Run_Menu,
                        Project      => Context_Project,
                        Kernel       => Kernel,
                        Mains        => Mains,
                        Set_Shortcut => Set_Shortcut);
                     --  The provided Project Argument is not relevant here
                  end if;

                  Free (Mains);
               end;
            end if;

            Next (Iter);
            Current_Project := Current (Iter);
         end loop;

         Free (Loaded_Mains);
      end;

      Add_Root_Project_Build_Menu (Builder_Module_ID.Make_Menu, Kernel, True);

      --  No main program?

      Mitem := new Dynamic_Menu_Item_Record;
      Gtk.Menu_Item.Initialize (Mitem, -Current_Make_Suffix);
      Append (Menu1, Mitem);
      Set_Accel_Path
        (Mitem, Make_Menu_Prefix & Current_Make_Suffix, Group);
      File_Project_Cb.Object_Connect
        (Mitem, Signal_Activate, On_Build'Access,
         Slot_Object => Kernel,
         User_Data => File_Project_Record'
           (Project => No_Project,
            File    => VFS.No_File));

      Mitem := new Dynamic_Menu_Item_Record;
      Gtk.Menu_Item.Initialize (Mitem, -Custom_Make_Suffix);
      Append (Menu1, Mitem);
      Kernel_Callback.Connect
        (Mitem, Signal_Activate, On_Custom'Access,
         User_Data => Kernel_Handle (Kernel));
      Set_Accel_Path (Mitem, "<gps>/Build/Make/Custom...", Group);

      --  Should be able to run any program

      Mitem := new Dynamic_Menu_Item_Record;
      Gtk.Menu_Item.Initialize (Mitem, -Custom_Make_Suffix);
      Append (Menu2, Mitem);
               Set_Accel_Path
                 (Mitem, -(Run_Menu_Prefix) & (-Custom_Make_Suffix), Group);
      File_Project_Cb.Object_Connect
        (Mitem, Signal_Activate, On_Run'Access,
         Slot_Object => Kernel,
         User_Data   => File_Project_Record'
           (Project => Get_Project (Kernel), File => VFS.No_File));
      Show_All (Menu1);
      Show_All (Menu2);

      if Get_Pref (Automatic_Xrefs_Load) then
         Load_Xref_In_Memory (Kernel_Handle (Kernel));
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
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

      Library_Name : constant String :=
                       Get_Attribute_Value
                         (Project_Information (Context),
                          Attribute => Library_Name_Attribute);
      Mains        : Argument_List :=
                       Get_Attribute_Value
                         (Project_Information (Context),
                          Attribute => Main_Attribute);
      M            : Gtk_Menu := Gtk_Menu (Menu);

   begin
      if Mains'Length /= 0 then
         Add_Build_Menu
           (Menu         => M,
            Project      => Project_Information (Context),
            Kernel       => Get_Kernel (Context),
            Mains        => Mains,
            Set_Shortcut => False);
      end if;
      Free (Mains);

      --  Check for library

      if Library_Name /= "" then
         Add_Build_Menu
           (Menu    => M,
            Project => Project_Information (Context),
            Kernel  => Get_Kernel (Context),
            Library => Library_Name);
      end if;
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

      Mains : Argument_List :=
                Get_Attribute_Value
                  (Project_Information (Context),
                   Attribute => Main_Attribute);
      M     : Gtk_Menu := Gtk_Menu (Menu);

   begin
      Add_Run_Menu
        (Menu         => M,
         Project      => Project_Information (Context),
         Kernel       => Get_Kernel (Context),
         Mains        => Mains,
         Set_Shortcut => False);
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
      --  This memory is allocated once, and lives as long as the application

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

      declare
         Result : constant String := Add_Customization_String
           (Kernel        => Kernel,
            Customization => "<key action='/Build/Make/item1'>F4</key>"
            & "<key action='/Build/Run/item1'>shift-F2</key>"
            & "<key action='/Build/Make/Custom...'>F9</key>",
            From_File     => "builder module");
      begin
         if Result /= "" then
            Insert (Kernel, Result, Mode => Console.Error);
         end if;
      end;

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

      Add_Hook
        (Kernel => Kernel,
         Hook   => Project_View_Changed_Hook,
         Func   => Wrapper (On_View_Changed'Access),
         Name   => "builder_module.on_view_changed");
      Add_Hook
        (Kernel => Kernel,
         Hook   => Compilation_Finished_Hook,
         Func   => Wrapper (On_Compilation_Finished'Access),
         Name   => "load_xrefs");
      Add_Hook
        (Kernel => Kernel,
         Hook   => Compilation_Starting_Hook,
         Func   => Wrapper (On_Compilation_Starting'Access),
         Name   => "stop_load_xrefs");
      Add_Hook
        (Kernel => Kernel,
         Hook   => Project_Changed_Hook,
         Func   => Wrapper (On_Project_Changed'Access),
         Name   => "interrupt_xrefs_loading");

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

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changed (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Interrupt_Xrefs_Loading (Kernel);
   end On_Project_Changed;

end Builder_Module;
