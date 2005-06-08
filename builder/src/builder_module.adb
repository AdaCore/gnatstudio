-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
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

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Gtk.Accel_Group;           use Gtk.Accel_Group;
with Gdk.Color;                 use Gdk.Color;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Gtk.Enums;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Accel_Map;             use Gtk.Accel_Map;
with Gtk.Widget;                use Gtk.Widget;

with GPS.Intl;                  use GPS.Intl;

with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;

with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Timeout;        use GPS.Kernel.Timeout;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Location_View;         use GPS.Location_View;
with VFS;                       use VFS;
with Projects;                  use Projects;

with Language_Handlers;         use Language_Handlers;
with Language_Handlers.GPS;     use Language_Handlers.GPS;
with Projects.Registry;         use Projects.Registry;
with Entities;                  use Entities;
with Histories;                 use Histories;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;

with Basic_Types;
with Std_Dialogs;               use Std_Dialogs;
with File_Utils;                use File_Utils;
with String_Utils;              use String_Utils;
with String_List_Utils;
with GUI_Utils;                 use GUI_Utils;
with OS_Utils;                  use OS_Utils;
with System;

with GNAT.Expect;               use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Case_Util;            use GNAT.Case_Util;

with Traces;                    use Traces;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Tags;                  use Ada.Tags;

with Commands;                  use Commands;
with Commands.Builder;          use Commands.Builder;

with Commands.Generic_Asynchronous;

package body Builder_Module is

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
   Unique_Compile : aliased String := "-u";
   Syntax_Check   : aliased String := "-gnats";

   type LI_Handler_Iterator_Access_Access is access LI_Handler_Iterator_Access;

   type Dynamic_Menu_Item_Record is new Gtk_Menu_Item_Record with null record;
   type Dynamic_Menu_Item is access all Dynamic_Menu_Item_Record'Class;
   --  So that items created for the dynamic Make and Run menus have a special
   --  type, and we only remove these when refreshing the menu

   function Is_Dynamic_Menu_Item
     (W : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;
   --  Return True if W is a Dynamic menu item

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
      Unique_Project : Boolean := False) return Argument_List_Access;
   --  Compute the make arguments following the right Syntax
   --  (gnatmake / make), given a Project and File name.
   --  It is the responsibility of the caller to free the returned object.
   --
   --  If File is No_File, then all files of the project will be recompiled.
   --  If Compile_Only is True, then use compile rather than build syntax.
   --
   --  If Path is not empty, use Path as project path.

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
     (Menu    : in out Gtk_Menu;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class;
      Mains   : Argument_List);
   --  Same as Add_Build_Menu, but for the Run menu

   procedure Builder_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   procedure Run_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add entries to the contextual menu for Build/ or Run/

   procedure Insert_And_Launch
     (Kernel          : Kernel_Handle;
      Remote_Host     : String;
      Remote_Protocol : String;
      Command         : String;
      Arguments       : Argument_List;
      Fd              : Process_Descriptor_Access;
      Insert          : Boolean := True);
   --  Compute the command to be executed from Command, Remote_Host
   --  and Remote_Protocol (execution is local when
   --  Remote_Host = "" or Remote_Protocol = ""

   procedure Insert_And_Launch
     (Kernel          : Kernel_Handle;
      Remote_Host     : String;
      Remote_Protocol : String;
      Cmd_Line        : String;
      Fd              : Process_Descriptor_Access);
   --  Similar to procedure above, except that the command and its argument
   --  are provided as a string.

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed.

   procedure Cleanup_Accel_Map (Kernel : access Kernel_Handle_Record'Class);
   --  Remove from the accel_map the key bindings set for previous projects

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
      Synchronous : Boolean := False);
   --  Same as On_Build.
   --  If Synchronous is True, this subprogram will block GPS until the
   --  compilation is finished executing.
   --  If Project is No_Project and File is VFS.No_File, the current file is
   --  build.
   --  If Project is not No_Project and File is VFS.No_File, either
   --  the main units of Project are built (Main_Units = True), or all the
   --  files in the project are built.

   procedure On_Custom
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Custom... menu

   procedure On_Compute_Xref
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Compute Xref information menu

   procedure On_Run
     (Kernel : access GObject_Record'Class; Data : File_Project_Record);
   --  Build->Run menu

   procedure On_Stop_Build
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Stop Build menu

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
      Shadow      : Boolean := False);
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

   procedure Clear_Compilation_Output
     (Kernel : Kernel_Handle;
      Shadow : Boolean);
   --  Clear the compiler output, the console, and the result view.

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

      String_List_Utils.String_List.Free
        (Builder_Module_ID_Access (Builder_Module_ID).Output);
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
      Unique_Project : Boolean := False) return Argument_List_Access
   is
      Project_Str    : String_Access;
      Result         : Argument_List_Access;
      Vars           : Argument_List_Access;

   begin
      --  Convert project path to unix pathname, since this is now supported
      --  by Windows, and is needed when using a remote unix host from Windows.

      if Path = "" then
         Project_Str := new String'(To_Unix_Pathname (Project_Path (Project)));
      else
         Project_Str := new String'(To_Unix_Pathname (Path));
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

   -----------------------
   -- Insert_And_Launch --
   -----------------------

   procedure Insert_And_Launch
     (Kernel          : Kernel_Handle;
      Remote_Host     : String;
      Remote_Protocol : String;
      Command         : String;
      Arguments       : Argument_List;
      Fd              : Process_Descriptor_Access;
      Insert          : Boolean := True)
   is
      Remote_Args : Argument_List_Access;
      Cmd         : constant String :=
        Command & " " & Argument_List_To_String (Arguments);

   begin
      if Insert then
         Console.Raise_Console (Kernel);
      end if;

      if Remote_Host /= ""
        and then Remote_Protocol /= ""
      then
         Remote_Args := Argument_String_To_List
           (Remote_Protocol & " " & Remote_Host);

         if Insert then
            Console.Insert
              (Kernel, Remote_Protocol & " " & Remote_Host & " " & Cmd);
         end if;

         Non_Blocking_Spawn
           (Fd.all,
            Remote_Args (Remote_Args'First).all,
            Remote_Args (Remote_Args'First + 1 .. Remote_Args'Last) &
              Command'Unrestricted_Access & Arguments,
            Buffer_Size => 0, Err_To_Out => True);
         Free (Remote_Args);

      else
         if Insert then
            Console.Insert (Kernel, Cmd);
         end if;

         Non_Blocking_Spawn
           (Fd.all, Command, Arguments, Buffer_Size => 0, Err_To_Out => True);
      end if;
   end Insert_And_Launch;

   procedure Insert_And_Launch
     (Kernel          : Kernel_Handle;
      Remote_Host     : String;
      Remote_Protocol : String;
      Cmd_Line        : String;
      Fd              : Process_Descriptor_Access)
   is
      Args : Argument_List_Access := Argument_String_To_List (Cmd_Line);
   begin
      Insert_And_Launch
        (Kernel,
         Remote_Host,
         Remote_Protocol,
         Args (Args'First).all,
         Args (Args'First + 1 .. Args'Last),
         Fd);
      Free (Args);
   end Insert_And_Launch;

   --------------
   -- On_Build --
   --------------

   procedure On_Build
     (Kernel      : Kernel_Handle;
      File        : Virtual_File;
      Project     : Project_Type;
      Main_Units  : Boolean := False;
      Synchronous : Boolean := False)
   is
      Fd      : Process_Descriptor_Access;
      Cmd     : String_Access;
      Args    : Argument_List_Access;

      Context : Selection_Context_Access;
      Prj     : Project_Type;
      Langs   : Argument_List := Get_Languages
        (Get_Project (Kernel), Recursive => True);
      Syntax  : Command_Syntax;
      Old_Dir : constant Dir_Name_Str := Get_Current_Dir;

      C       : Build_Command_Access;

   begin
      if Langs'Length = 0 then
         return;
      end if;

      for F in Langs'Range loop
         To_Lower (Langs (F).all);
      end loop;

      if Langs'Length = 1 and then Langs (Langs'First).all = "ada" then
         Syntax := GNAT_Syntax;
      elsif Get_Pref (Kernel, Multi_Language_Build) then
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

      if not Save_MDI_Children
        (Kernel, Force => Get_Pref (Kernel, Auto_Save))
      then
         Free (Args);
         return;
      end if;

      Prj := Project;

      --  If no file was specified in data, simply compile the current file.

      if File = VFS.No_File and then Project = No_Project then
         Context := Get_Current_Context (Kernel);

         if Context /= null
           and then Context.all in File_Selection_Context'Class
           and then Has_File_Information
             (File_Selection_Context_Access (Context))
         then
            declare
               File_Context : constant File_Selection_Context_Access :=
                 File_Selection_Context_Access (Context);
               F : constant Virtual_File := File_Information (File_Context);

            begin
               if Has_Directory_Information (File_Context) then
                  Change_Dir (Directory_Information (File_Context));
               end if;

               Prj := Get_Project_From_File (Get_Registry (Kernel).all, F);

               if Prj = No_Project
                 or else Status (Get_Project (Kernel)) /= From_File
               then
                  --  Use the default switches for that tool
                  Args := new Argument_List'
                    (Get_Switches
                       (Handle   => Kernel,
                        Project  => No_Project,
                        In_Pkg   => Builder_Package,
                        Index    => Ada_String,
                        Use_Initial_Value => True)
                       & new String'(Base_Name (F)));
               else
                  Args := Compute_Arguments (Kernel, Prj, "", F);
               end if;
            end;

         --  There is no current file, so we can't compile anything

         else
            Console.Insert
              (Kernel, -"No file selected, cannot build", Mode => Error);
            return;
         end if;

      else
         --  Are we using the default internal project ?

         if Status (Get_Project (Kernel)) /= From_File then
            case Syntax is
               when GNAT_Syntax =>
                  if File = VFS.No_File then
                     Console.Insert
                       (Kernel, -"Default project has no main unit",
                        Mode => Error);
                     return;
                  else
                     --  Use the default switches for that tool
                     Args := new Argument_List'
                       (Get_Switches
                          (Handle   => Kernel,
                           Project  => No_Project,
                           In_Pkg   => Builder_Package,
                           Index    => Ada_String,
                           Use_Initial_Value => True)
                        & new String'(Full_Name (File).all));
                  end if;

               when Make_Syntax =>
                  Console.Insert
                    (Kernel, -"You must save the project file before building",
                     Mode => Error);
                  return;
            end case;

         else
            Args := Compute_Arguments
              (Kernel, Project, "", File,
               Unique_Project => not Main_Units);
         end if;

         Change_Dir (Dir_Name (Project_Path (Project)));
      end if;

      if Builder_Module_ID_Access (Builder_Module_ID).Build_Count = 0 then
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

      Fd := new TTY_Process_Descriptor;
      Insert_And_Launch
        (Kernel,
         Remote_Protocol  => Get_Pref (Kernel, Remote_Protocol),
         Remote_Host      =>
           Get_Attribute_Value (Prj, Remote_Host_Attribute),
         Command          => Cmd.all,
         Arguments        => Args.all,
         Fd               => Fd);
      Free (Cmd);
      Free (Args);

      Create (C, (Kernel, Fd, null, null, System.Null_Address));
      Builder_Module_ID_Access (Builder_Module_ID).Build_Count :=
        Builder_Module_ID_Access (Builder_Module_ID).Build_Count + 1;
      Builder_Module_ID_Access (Builder_Module_ID).Last_Command :=
        Command_Access (C);

      if Synchronous then
         Launch_Synchronous (Command_Access (C), 0.1);
         Destroy (Command_Access (C));

      else
         Launch_Background_Command
           (Kernel,
            Command_Access (C),
            Active   => False,
            Show_Bar => True,
            Queue_Id => "");
      end if;

      Change_Dir (Old_Dir);

   exception
      when Invalid_Process =>
         Console.Insert (Kernel, -"Invalid command", Mode => Error);
         Change_Dir (Old_Dir);
         Free (Cmd);
         Free (Args);
         Free (Fd);

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
      Context : Selection_Context_Access :=
        Get_Current_Context (Kernel);

   begin
      if Context = null
        or else not (Context.all in File_Selection_Context'Class)
      then
         Console.Insert
           (Kernel, -"No file selected, cannot check syntax",
            Mode => Error);
         return;
      end if;

      Ref (Context);
      Compile_File
        (Kernel,
         File_Information (File_Selection_Context_Access (Context)),
         Syntax_Only => True);
      Unref (Context);

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
      Shadow      : Boolean := False)
   is
      Prj             : constant Project_Type :=
        Get_Project_From_File (Get_Registry (Kernel).all, File);
      Cmd             : String_Access;
      Fd              : Process_Descriptor_Access;
      Local_File      : String_Access;
      Lang            : String := Get_Language_From_File
        (Get_Language_Handler (Kernel), File);
      C               : Build_Command_Access;
      Common_Args     : Argument_List_Access;
      Args            : Argument_List_Access;
      Old_Dir         : constant Dir_Name_Str := Get_Current_Dir;
      Shadow_Path     : String_Access;
      Compilable_File : Virtual_File := File;

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
           (Kernel, Force => Get_Pref (Kernel, Auto_Save))
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
         Cmd    := new String'
           (Get_Attribute_Value
              (Prj, Compiler_Command_Attribute,
               Default => "gnatmake", Index => "ada"));

         if Syntax_Only then
            Common_Args := new Argument_List'
              (Unique_Compile'Access, Quiet_Opt'Access, Syntax_Check'Access);
         else
            Common_Args := new Argument_List'(1 => Unique_Compile'Access);
         end if;

      else
         if Status (Prj) /= From_File then
            if not Quiet then
               Console.Insert
                 (Kernel, -"You must save the project before compiling",
                  Mode => Error);
            end if;

            Free (Local_File);
            return;
         end if;

         Cmd         := new String'("gprmake");
         Common_Args := new Argument_List'(1 .. 0 => null);
      end if;

      if Builder_Module_ID_Access (Builder_Module_ID).Build_Count = 0 then
         Clear_Compilation_Output (Kernel, Shadow);
      end if;

      Fd := new TTY_Process_Descriptor;

      if Shadow then
         --  Create a temporary project, and a temporary file containing the
         --  buffer data, so as to be able to compile the file without saving
         --  the buffer to disk.

         declare
            Tmp_Dir      : constant String := Get_Tmp_Dir;
            Temp_Project : Virtual_File := Create (Tmp_Dir & "ext.gpr");
            Temp_File    : Virtual_File := Create
              (Get_Tmp_Dir & Base_Name (File));
            Writable     : Writable_File;
         begin
            --  Do nothing if one of the files already exists.

            if not Is_Regular_File (Temp_Project) then
               --  Write the temporary project file
               Writable := Write_File (Temp_Project);
               Write
                 (Writable,
                  "project ext extends """ & Project_Path (Prj) & """ is"
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
            Create
              (Item  => C,
               Data  => (Kernel, Fd, null, null, System.Null_Address),
               Quiet => Quiet,
               Files => new File_Array'
                 (1 => Temp_File,
                  2 => Temp_Project));
         end;
      else
         Local_File := new String'(Locale_Full_Name (File));
         Change_Dir (Dir_Name (Project_Path (Prj)));
         Shadow_Path := new String'("");
         Create (C, (Kernel, Fd, null, null, System.Null_Address), Quiet);
      end if;

      if not Shadow
        and then (Status (Prj) /= From_File
                  or else Syntax_Only)
      then
         --  Use the default switches for that tool
         declare
            Default_Builder_Switches : Argument_List := Get_Switches
              (Handle   => Kernel,
               Project  => No_Project,
               In_Pkg   => Builder_Package,
               Index    => Ada_String,
               Use_Initial_Value => True);
         begin
            Insert_And_Launch
              (Kernel,
               Remote_Protocol => Get_Pref (Kernel, Remote_Protocol),
               Remote_Host => Get_Attribute_Value (Prj, Remote_Host_Attribute),
               Command         => Cmd.all,
               Arguments       => Default_Builder_Switches &
               Common_Args.all & (1 => Local_File),
               Fd              => Fd,
               Insert          => not Quiet);
            Free (Default_Builder_Switches);
         end;

      else
         Args := Compute_Arguments
           (Kernel,
            Prj,
            Shadow_Path.all,
            Compilable_File,
            Compile_Only => True);

         Insert_And_Launch
           (Kernel,
            Remote_Protocol => Get_Pref (Kernel, Remote_Protocol),
            Remote_Host     =>
              Get_Attribute_Value (Prj, Remote_Host_Attribute),
            Command         => Cmd.all,
            Arguments       => Common_Args.all & Args.all,
            Fd              => Fd,
            Insert          => not Quiet);
         Free (Args);
      end if;

      Basic_Types.Unchecked_Free (Common_Args);
      Free (Cmd);

      Builder_Module_ID_Access (Builder_Module_ID).Build_Count :=
        Builder_Module_ID_Access (Builder_Module_ID).Build_Count + 1;
      Builder_Module_ID_Access (Builder_Module_ID).Last_Command :=
        Command_Access (C);

      if Synchronous then
         Launch_Synchronous (Command_Access (C), 0.1);
         Destroy (Command_Access (C));

      else
         Launch_Background_Command
           (Kernel,
            Command_Access (C),
            Active   => False,
            Show_Bar => not Shadow,
            Queue_Id => "");
      end if;

      Change_Dir (Old_Dir);
      Free (Local_File);
      Free (Shadow_Path);

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
      Node   : List_Node;
      Info   : File_Info;
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      C      : Xref_Commands.Generic_Asynchronous_Command_Access;
   begin
      if Command = "compile" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));
         Compile_File (Get_Kernel (Data), Get_File (Info),
                       Synchronous => True);

      elsif Command = "check_syntax" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));
         Compile_File (Get_Kernel (Data), Get_File (Info),
                       Synchronous => True,
                       Syntax_Only => True);

      elsif Command = "shadow_check_syntax" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));
         Compile_File (Get_Kernel (Data), Get_File (Info),
                       Synchronous => False,
                       Syntax_Only => True,
                       Quiet       => True,
                       Shadow      => True);

      elsif Command = "make" then
         Info := Get_Data (Nth_Arg (Data, 1, Get_File_Class (Kernel)));

         declare
            Main    : constant Virtual_File := Get_File (Info);
            Project : constant Project_Type := Get_Project_From_File
              (Registry => Project_Registry
                 (Get_Registry (Get_Kernel (Data)).all),
               Source_Filename   => Main,
               Root_If_Not_Found => True);
         begin
            On_Build
              (Get_Kernel (Data),
               File        => Main,
               Project     => Project,
               Synchronous => True);
         end;

      elsif Command = "get_build_output" then
         Node := First
           (Builder_Module_ID_Access (Builder_Module_ID).Output);

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
      Context : Selection_Context_Access :=
        Get_Current_Context (Kernel);

   begin
      if Context = null
        or else not (Context.all in File_Selection_Context'Class)
      then
         Console.Insert
           (Kernel, -"No file selected, cannot compile", Mode => Error);
         return;
      end if;

      Ref (Context);
      Compile_File
        (Kernel,
         File_Information (File_Selection_Context_Access (Context)));
      Unref (Context);

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

      C   : Build_Command_Access;
      Fd  : Process_Descriptor_Access;

   begin
      if Cmd = "" or else Cmd (Cmd'First) = ASCII.NUL then
         return;
      end if;

      if not Save_MDI_Children
        (Kernel, Force => Get_Pref (Kernel, Auto_Save))
      then
         return;
      end if;

      if Builder_Module_ID_Access (Builder_Module_ID).Build_Count = 0 then
         Clear_Compilation_Output (Kernel, False);
      end if;

      begin
         Fd := new TTY_Process_Descriptor;
         Insert_And_Launch
           (Kernel,
            Remote_Protocol  => Get_Pref (Kernel, Remote_Protocol),
            Remote_Host      =>
              Get_Attribute_Value
                (Get_Project (Kernel), Remote_Host_Attribute),
            Cmd_Line         => Cmd,
            Fd               => Fd);

         Create (C, (Kernel, Fd, null, null, System.Null_Address));

         Builder_Module_ID_Access (Builder_Module_ID).Build_Count :=
           Builder_Module_ID_Access (Builder_Module_ID).Build_Count + 1;
         Builder_Module_ID_Access (Builder_Module_ID).Last_Command :=
           Command_Access (C);

         Launch_Background_Command
           (Kernel, Command_Access (C),
            Active   => False,
            Show_Bar => True,
            Queue_Id => "");

      exception
         when Invalid_Process =>
            Console.Insert (Kernel, -"Invalid command", Mode => Error);
            Free (Fd);
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
      Handler      : constant GPS_Language_Handler :=
        GPS_Language_Handler (Get_Language_Handler (D.Kernel));
      Num_Handlers : constant Natural := LI_Handlers_Count (Handler);
      Not_Finished : Boolean;
      LI           : LI_Handler;
      New_Handler  : Boolean := False;

   begin
      if D.LI /= 0 and then D.Iter.all /= null then
         Continue (D.Iter.all.all, Not_Finished);
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
                  Project      => Get_Project (D.Kernel),
                  Recursive    => True));
            Continue (D.Iter.all.all, Not_Finished);
         end if;
      end loop;

      if New_Handler then
         Insert (D.Kernel, -"Parsing source files for "
                 & Get_LI_Name (Handler, D.LI));
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

   ------------
   -- On_Run --
   ------------

   procedure On_Run
     (Kernel : access GObject_Record'Class; Data : File_Project_Record)
   is
      K       : constant Kernel_Handle := Kernel_Handle (Kernel);
      Active  : aliased Boolean := False;
      Args    : Argument_List_Access;
      Exec    : String_Access;
      Success : Boolean;

      procedure Launch
        (Command   : String;
         Arguments : GNAT.OS_Lib.Argument_List;
         Title     : String);
      --  Launch Command (with Args) locally, or remotely if necessary.

      procedure Launch
        (Command   : String;
         Arguments : GNAT.OS_Lib.Argument_List;
         Title     : String)
      is
         Remote_Cmd  : constant String := Get_Pref (K, Remote_Protocol);
         Remote_Host : constant String :=
           Get_Attribute_Value (Data.Project, Remote_Host_Attribute);
         Exec        : String_Access;

      begin
         if Remote_Host = ""
           or else Remote_Cmd = ""
         then
            Exec := Locate_Exec_On_Path (Command);

            if Exec = null then
               Insert (K, -"Could not locate executable on path: " & Command);
            else
               Launch_Process
                 (K,
                  Command   => Exec.all,
                  Arguments => Arguments,
                  Console   => Create_Interactive_Console (K, Title),
                  Success   => Success);
            end if;

            Free (Exec);

         else
            declare
               Full_Command : constant String :=
                 To_Unix_Pathname (Command) & " "
                 & Argument_List_To_String (Arguments);
               New_Args     : Argument_List_Access :=
                 Argument_String_To_List (Remote_Cmd & " " & Remote_Host);
               Last_Arg     : String_Access := new String'(Full_Command);

            begin
               Launch_Process
                 (K,
                  Command   => New_Args (New_Args'First).all,
                  Arguments =>
                    New_Args (New_Args'First + 1 .. New_Args'Last) & Last_Arg,
                  Console   => Create_Interactive_Console (K, Title),
                  Success   => Success);

               Free (Last_Arg);
               Free (New_Args);
            end;
         end if;
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
               if Active then
                  Args := Argument_String_To_List
                    (Get_Pref (K, GPS.Kernel.Preferences.Execute_Command) &
                     ' ' & Command);
               else
                  Args := Argument_String_To_List (Command);
               end if;

               Launch
                 (Args (Args'First).all,
                  Args (Args'First + 1 .. Args'Last), -"Run: " & Command);
               Free (Exec);
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
               if Active then
                  Args := Argument_String_To_List
                    (Get_Pref (K, GPS.Kernel.Preferences.Execute_Command) &
                     ' ' & Full_Name (Data.File).all & ' ' & Arguments);
                  Launch
                    (Args (Args'First).all, Args (Args'First + 1 .. Args'Last),
                     -"Run: " & Base_Name (Data.File) & ' ' &
                     Krunch (Arguments, 12));

               else
                  Args := Argument_String_To_List (Arguments);
                  Launch
                    (Full_Name (Data.File).all,
                     Args.all, -"Run: " & Base_Name (Data.File)
                     & ' ' & Krunch (Arguments, 12));
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

   -------------------
   -- On_Stop_Build --
   -------------------

   procedure On_Stop_Build
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      if Builder_Module_ID_Access (Builder_Module_ID).Last_Command /= null then
         Interrupt_Queue
           (Kernel,
            Builder_Module_ID_Access (Builder_Module_ID).Last_Command);
      else
         Console.Insert (Kernel, -"No build to interrupt", Mode => Info);
      end if;
   end On_Stop_Build;

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
      Builder_Module : constant Builder_Module_ID_Access :=
        Builder_Module_ID_Access (Builder_Module_ID);
      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);
      Main  : Virtual_File;

   begin
      if Menu = null then
         Gtk_New (Menu);
      end if;

      --  Accelerators were removed when the menu items were destroyed (just
      --  before the update)
      Builder_Module.Build_Item := null;

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
            Accel_Path : constant String := Make_Menu_Prefix
              & Executables_Directory (Project) & Mains (M).all;
            Key : Gtk_Accel_Key;
            Found : Boolean;
         begin
            Set_Accel_Path (Mitem, Accel_Path, Group);

            --  The first item in the make menu should have a key binding
            if Set_Shortcut
              and then M = Mains'First
            then
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

               Builder_Module.Build_Item := Gtk_Menu_Item (Mitem);
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
      Mitem : Dynamic_Menu_Item;
      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);
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
     (Menu    : in out Gtk_Menu;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class;
      Mains   : Argument_List)
   is
      Mitem : Dynamic_Menu_Item;
      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);
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
            Set_Accel_Path (Mitem, -Run_Menu_Prefix & Exec, Group);
            File_Project_Cb.Object_Connect
              (Mitem, "activate", On_Run'Access,
               Slot_Object => Kernel,
               User_Data => File_Project_Record'
                 (Project => Project,
                  File    => Create
                    (Executables_Directory (Project) & Exec)));
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
      Builder_Module : constant Builder_Module_ID_Access :=
        Builder_Module_ID_Access (Builder_Module_ID);
      Mitem : Gtk_Menu_Item;
      Menu1 : Gtk_Menu renames Builder_Module.Make_Menu;
      Menu2 : Gtk_Menu renames Builder_Module.Run_Menu;
      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);
   begin
      --  Free the previous shortcuts if needed. We only keep the ones from
      --  the current project, to avoid an ever expending custom_keys file,
      --  and to limit the change of a duplicate key binding appearing in the
      --  menu.
      if Builder_Module.Last_Project_For_Menu /= Get_Project (Kernel) then
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
         if Builder_Module.Make_Menu /= null then
            Remove_All_Children (Builder_Module.Make_Menu,
                                 Is_Dynamic_Menu_Item'Access);
         end if;

         if Builder_Module.Run_Menu /= null then
            Remove_All_Children (Builder_Module.Run_Menu,
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
                    (Menu         => Builder_Module.Make_Menu,
                     Project      => Get_Project (Kernel),
                     Kernel       => Kernel,
                     Set_Shortcut => True,
                     Mains        => Mains);
                  Add_Run_Menu
                    (Menu         => Builder_Module.Run_Menu,
                     Project      => Get_Project (Kernel),
                     Kernel       => Kernel,
                     Mains        => Mains);
                  Free (Mains);
                  exit;
               end if;

               P := Parent_Project (P);
               Free (Mains);
            end;
         end loop;
      end;

      Add_Root_Project_Build_Menu (Builder_Module.Make_Menu, Kernel, True);

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

      if Builder_Module.Build_Item = null then
         Add_Accelerator
           (Mitem, "activate", Group, GDK_F4, 0,
            Gtk.Accel_Group.Accel_Visible);
         Builder_Module.Build_Item := Mitem;
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

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_View_Changed;

   ------------------------
   -- Builder_Contextual --
   ------------------------

   procedure Builder_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);
      File_Context : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      --  The filter garantees we are on a File_Selection_Context

      Mains : Argument_List := Get_Attribute_Value
        (Project_Information (File_Context),
         Attribute => Main_Attribute);
      M : Gtk_Menu := Gtk_Menu (Menu);
   begin
      if Mains'Length /= 0 then
         Add_Build_Menu
           (Menu         => M,
            Project      => Project_Information (File_Context),
            Kernel       => Get_Kernel (Context),
            Set_Shortcut => False,
            Mains        => Mains);
      end if;
      Free (Mains);
   end Builder_Contextual;

   --------------------
   -- Run_Contextual --
   --------------------

   procedure Run_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);
      File_Context : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      --  The filter garantees we are on a File_Selection_Context

      Mains : Argument_List := Get_Attribute_Value
        (Project_Information (File_Context),
         Attribute => Main_Attribute);
      M : Gtk_Menu := Gtk_Menu (Menu);
   begin
      Add_Run_Menu
        (Menu         => M,
         Project      => Project_Information (File_Context),
         Kernel       => Get_Kernel (Context),
         Mains        => Mains);
      Free (Mains);
   end Run_Contextual;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Build : constant String := '/' & (-"Build") & '/';
      Mitem : Gtk_Menu_Item;
      Menu  : Gtk_Menu;
   begin
      --  This memory is allocated once, and lives as long as the application.

      Builder_Module_ID := new Builder_Module_ID_Record;
      Register_Module
        (Module       => Builder_Module_ID,
         Kernel       => Kernel,
         Module_Name  => Builder_Module_Name,
         Priority     => Default_Priority);

      Register_Menu (Kernel, "/_" & (-"Build"), Ref_Item => -"Tools");
      Register_Menu (Kernel, Build, -"Check _Syntax", "",
                     On_Check_Syntax'Access);
      Register_Menu (Kernel, Build, -"_Compile File", "",
                     On_Compile'Access, null, GDK_F4, Shift_Mask);

      Register_Contextual_Submenu
        (Kernel,
         Name    => "Build",
         Filter  => Lookup_Filter (Kernel, "Project only"),
         Submenu => Builder_Contextual'Access);
      Register_Contextual_Submenu
        (Kernel,
         Name    => "Run",
         Filter  => Lookup_Filter (Kernel, "Project only"),
         Submenu => Run_Contextual'Access);

      --  Dynamic make menu

      Mitem := Register_Menu (Kernel, Build, -"_Make", "", null);
      Gtk_New (Menu);
      Builder_Module_ID_Record (Builder_Module_ID.all).Make_Menu := Menu;
      Set_Submenu (Mitem, Menu);

      Register_Menu
        (Kernel, Build, -"Recompute C/C++ _Xref info", "",
         On_Compute_Xref'Access);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Build, Mitem);

      --  Dynamic run menu
      Mitem := Register_Menu
        (Kernel, Build, -"_Run", Stock_Execute, null);
      Gtk_New (Menu);
      Builder_Module_ID_Record (Builder_Module_ID.all).Run_Menu := Menu;
      Set_Submenu (Mitem, Menu);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Build, Mitem);

      Register_Menu
        (Kernel, Build, -"_Interrupt", Stock_Stop, On_Stop_Build'Access,
         null, GDK_C, Control_Mask + Shift_Mask);

      Add_Hook (Kernel, Project_View_Changed_Hook, On_View_Changed'Access);

      Register_Command
        (Kernel, "compile",
         Class        => Get_File_Class (Kernel),
         Handler      => Compile_Command'Access);
      Register_Command
        (Kernel, "check_syntax",
         Class        => Get_File_Class (Kernel),
         Handler      => Compile_Command'Access);
      Register_Command
        (Kernel, "shadow_check_syntax",
         Class        => Get_File_Class (Kernel),
         Handler      => Compile_Command'Access);
      Register_Command
        (Kernel, "make",
         Class        => Get_File_Class (Kernel),
         Handler      => Compile_Command'Access);
      Register_Command
        (Kernel, "compute_xref",
         Handler      => Compile_Command'Access);
      Register_Command
        (Kernel, "get_build_output",
         Handler      => Compile_Command'Access);

      Add_Hook
        (Kernel, Preferences_Changed_Hook, Preferences_Changed'Access);
   end Register_Module;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Args : Argument_List (1 .. 2);
   begin
      Args :=
        (1 => new String'(-Error_Category),
         2 => new String'
           (To_String (Get_Pref (Kernel, Error_Src_Highlight))));
      Execute_GPS_Shell_Command
        (Kernel, "Editor.register_highlighting", Args);
      Free (Args);

      Args :=
        (1 => new String'(-Style_Category),
         2 => new String'
           (To_String (Get_Pref (Kernel, Style_Src_Highlight))));
      Execute_GPS_Shell_Command
        (Kernel, "Editor.register_highlighting", Args);
      Free (Args);

      Args :=
        (1 => new String'(-Warning_Category),
         2 => new String'
           (To_String (Get_Pref (Kernel, Warning_Src_Highlight))));
      Execute_GPS_Shell_Command
        (Kernel, "Editor.register_highlighting", Args);
      Free (Args);
   end Preferences_Changed;

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
