-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

with Glide_Intl;                use Glide_Intl;

with GVD.Preferences;           use GVD.Preferences;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Timeout;      use Glide_Kernel.Timeout;
with Glide_Kernel.Task_Manager; use Glide_Kernel.Task_Manager;
with VFS;                       use VFS;

with Language_Handlers;         use Language_Handlers;
with Language_Handlers.Glide;   use Language_Handlers.Glide;
with Projects.Editor;           use Projects, Projects.Editor;
with Projects.Registry;         use Projects.Registry;
with Src_Info;                  use Src_Info;
with Histories;                 use Histories;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;

with Glide_Main_Window;         use Glide_Main_Window;

with Basic_Types;
with GVD.Dialogs;               use GVD.Dialogs;
with String_Utils;              use String_Utils;
with String_List_Utils;
with GUI_Utils;                 use GUI_Utils;

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

   Builder_Category : constant String := "Builder Results";

   Me : constant Debug_Handle := Create (Builder_Module_Name);

   type LI_Handler_Iterator_Access_Access is access LI_Handler_Iterator_Access;

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
     (Data_Type   => Compute_Xref_Data_Access,
      Free        => Deep_Free);

   procedure Free (Ar : in out String_List);
   procedure Free (Ar : in out String_List_Access);
   --  Free the memory associate with Ar.

   function Compute_Arguments
     (Kernel  : Kernel_Handle;
      Syntax  : Command_Syntax;
      Project : String;
      File    : Virtual_File) return Argument_List_Access;
   --  Compute the make arguments following the right Syntax
   --  (gnatmake / make), given a Project and File name.
   --  It is the responsibility of the caller to free the returned object.
   --
   --  If File is No_File, then all files of the project will be recompiled.

   procedure Add_Build_Menu
     (Menu         : in out Gtk_Menu;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Set_Shortcut : Boolean);
   --  Remove all entries in Menu, and add new entries for all the main
   --  subprograms of Project.
   --  If Menu is null, a new one is created if there are any entries
   --  If Set_Shortcut is true, the F4 shortcut is set for the first entry.

   procedure Add_Run_Menu
     (Menu         : in out Gtk_Menu;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class);
   --  Same as Add_Build_Menu, but for the Run menu

   procedure Builder_Contextual
     (Object    : access GObject_Record'Class;
      Context   : access Selection_Context'Class;
      Menu      : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add entries to the contextual menu

   procedure Insert_And_Launch
     (Kernel          : Kernel_Handle;
      Remote_Host     : String;
      Remote_Protocol : String;
      Command         : String;
      Arguments       : Argument_List;
      Fd              : Process_Descriptor_Access);
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
     (K : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when the preferences have changed.

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

   procedure On_Build
     (Kernel      : Kernel_Handle;
      File        : Virtual_File;
      Project     : Project_Type;
      Synchronous : Boolean := False);
   --  Same as On_Build.
   --  If Synchronous is True, this subprogram will block GPS until the
   --  compilation is finished executing.
   --  If Project is No_Project and File is VFS.No_File, the current file is
   --  build.
   --  If Project is not No_Project and File is VFS.No_File, the main units
   --  of Project are built.

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

   procedure On_View_Changed
     (K : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called every time the project view has changed, ie potentially the list
   --  of main units.

   procedure Compile_Command
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Command handler for the "compile" command.

   procedure Compile_File
     (Kernel      : Kernel_Handle;
      File        : Virtual_File;
      Synchronous : Boolean := False);
   --  Launch a compilation command for File.
   --  If Synchronous is true, then this procedure will not return until the
   --  file is fully compiled; all other GPS operations are blocked while the
   --  compilation takes place in this case.

   procedure Clear_Compilation_Output (Kernel : Kernel_Handle);
   --  Clear the compiler output, the console, and the result view.

   ------------------------------
   -- Clear_Compilation_Output --
   ------------------------------

   procedure Clear_Compilation_Output (Kernel : Kernel_Handle) is
   begin
      Console.Clear (Kernel);
      Remove_Result_Category (Kernel, Builder_Category);
      String_List_Utils.String_List.Free
        (Builder_Module_ID_Access (Builder_Module_ID).Output);
   end Clear_Compilation_Output;

   -----------------------
   -- Compute_Arguments --
   -----------------------

   function Compute_Arguments
     (Kernel  : Kernel_Handle;
      Syntax  : Command_Syntax;
      Project : String;
      File    : Virtual_File) return Argument_List_Access
   is
      Result         : Argument_List_Access;
      Vars           : Argument_List_Access :=
        Argument_String_To_List
          (Scenario_Variables_Cmd_Line (Kernel, Syntax));
      Build_Progress : constant Boolean :=
        Get_Pref (Kernel, Show_Build_Progress);
      File_Arg       : String_Access;

   begin
      case Syntax is
         when GNAT_Syntax =>
            --  gnatmake -d -Pproject main -XVAR1=value1 ...

            declare
               R_Tmp : Argument_List (1 .. 3);
               K     : Natural := 0;
            begin
               if Build_Progress then
                  K := K + 1;
                  R_Tmp (K) := new String'("-d");
               end if;

               K := K + 1;
               R_Tmp (K) := new String'("-P" & Project);

               if File /= VFS.No_File then
                  K := K + 1;
                  R_Tmp (K) := new String'(Full_Name (File).all);
               end if;

               Result := new Argument_List'(R_Tmp (1 .. K) & Vars.all);
            end;

         when Make_Syntax =>
            --  make -s -C dir -f Makefile.project build VAR1=value1 ...

            declare
               Lang         : String := Get_Language_From_File
                 (Get_Language_Handler (Kernel), File);
               List         : constant Argument_List :=
                 ((new String'("-s"),
                   new String'("-C"),
                   new String'(Dir_Name (Project)),
                   new String'("-f"),
                   new String'("Makefile." & Base_Name (Project, ".gpr")),
                   new String'("build")) & Vars.all);

            begin
               To_Lower (Lang);

               if Lang = "ada" then
                  --  ??? Should set these values also if Ada is part of the
                  --  supported languages.

                  if File = VFS.No_File then
                     File_Arg := new String'("");
                  else
                     File_Arg :=
                       new String'("ADA_SOURCES=" & Base_Name (File).all);
                  end if;

                  if Build_Progress then
                     Result := new Argument_List'
                       (List &
                        File_Arg &
                        new String'("ADAFLAGS=-d"));

                  else
                     Result := new Argument_List'(List & File_Arg);
                  end if;

               else
                  Result := new Argument_List'(List);
               end if;
            end;
      end case;

      Basic_Types.Unchecked_Free (Vars);
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

   -------------------------
   -- Set_Sensitive_Menus --
   -------------------------

   procedure Set_Sensitive_Menus
     (Kernel    : Kernel_Handle;
      Sensitive : Boolean)
   is
      Build : constant String := '/' & (-"Build") & '/';
   begin
      Set_Sensitive (Find_Menu_Item
        (Kernel, Build & (-"Check Syntax")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Build & (-"Compile File")), Sensitive);
      Set_Sensitive (Find_Menu_Item (Kernel, Build & (-"Make")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Build & (-"Interrupt")), not Sensitive);
   end Set_Sensitive_Menus;

   -----------------------
   -- Insert_And_Launch --
   -----------------------

   procedure Insert_And_Launch
     (Kernel          : Kernel_Handle;
      Remote_Host     : String;
      Remote_Protocol : String;
      Command         : String;
      Arguments       : Argument_List;
      Fd              : Process_Descriptor_Access)
   is
      Remote_Args : Argument_List_Access;
      Cmd         : constant String :=
        Command & " " & Argument_List_To_String (Arguments);

   begin
      Console.Raise_Console (Kernel);

      if Remote_Host /= ""
        and then Remote_Protocol /= ""
      then
         Remote_Args := Argument_String_To_List
           (Remote_Protocol & " " & Remote_Host);

         Console.Insert
           (Kernel, Remote_Protocol & " " & Remote_Host & " " & Cmd);

         Non_Blocking_Spawn
           (Fd.all,
            Remote_Args (Remote_Args'First).all,
            Remote_Args (Remote_Args'First + 1 .. Remote_Args'Last) &
              Command'Unrestricted_Access & Arguments,
            Buffer_Size => 0, Err_To_Out => True);
         Free (Remote_Args);

      else
         Console.Insert (Kernel, Cmd);
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
      Synchronous : Boolean := False)
   is
      Top          : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Fd           : Process_Descriptor_Access;
      Cmd          : String_Access;
      Args         : Argument_List_Access;

      Context      : Selection_Context_Access;
      Prj          : Project_Type;
      Langs        : Argument_List := Get_Languages
        (Get_Project (Kernel), Recursive => True);
      Syntax       : Command_Syntax;
      State_Pushed : Boolean := False;
      Old_Dir      : constant Dir_Name_Str := Get_Current_Dir;

      C            : Build_Command_Access;

   begin
      if Langs'Length = 0 then
         return;
      end if;

      To_Lower (Langs (Langs'First).all);

      if Langs'Length = 1 and then Langs (Langs'First).all = "ada" then
         Syntax := GNAT_Syntax;
      else
         Syntax := Make_Syntax;
      end if;

      Free (Langs);

      --  Ask for saving sources/projects before building.
      --  Do this before checking the project, in case we have a default
      --  project whose name is changed when saving

      if not Save_All_MDI_Children
        (Kernel, Force => Get_Pref (Kernel, Auto_Save))
      then
         Free (Args);
         return;
      end if;

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

               Prj := Get_Project_From_File (Get_Registry (Kernel), F);

               if Prj = No_Project
                 or else Status (Get_Project (Kernel)) /= From_File
               then
                  Args := new Argument_List'
                    (Clone (Default_Builder_Switches)
                     & new String'(Full_Name (F).all));
               else
                  Args := Compute_Arguments
                    (Kernel, Syntax, Project_Path (Prj), F);
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
                     Args := new Argument_List'
                       (Clone (Default_Builder_Switches)
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
              (Kernel, Syntax, Project_Path (Project), File);
         end if;

         Change_Dir (Dir_Name (Project_Path (Project)));
      end if;

      Clear_Compilation_Output (Kernel);

      Push_State (Kernel, Processing);
      State_Pushed := True;

      case Syntax is
         when GNAT_Syntax =>
            Cmd := new String'(Get_Attribute_Value
              (Project, Compiler_Command_Attribute,
               Default => "gnatmake", Index => "ada"));

         when Make_Syntax =>
            Cmd := new String'("make");
      end case;

      Set_Sensitive_Menus (Kernel, False);

      Top.Interrupted := False;
      Fd := new TTY_Process_Descriptor;

      Insert_And_Launch
        (Kernel,
         Remote_Protocol  => Get_Pref (GVD_Prefs, Remote_Protocol),
         Remote_Host      =>
           Get_Attribute_Value (Project, Remote_Host_Attribute),
         Command          => Cmd.all,
         Arguments        => Args.all,
         Fd               => Fd);

      Free (Cmd);
      Free (Args);

      Create (C, (Kernel, Fd, null, null, null));

      if Synchronous then
         Launch_Synchronous (Command_Access (C), 0.1);
         Destroy (Command_Access (C));

      else
         Launch_Background_Command
           (Kernel, Command_Access (C), Active => False, Queue_Id => "");
      end if;

      Change_Dir (Old_Dir);

   exception
      when Invalid_Process =>
         Console.Insert (Kernel, -"Invalid command", Mode => Error);
         Pop_State (Kernel);
         Set_Sensitive_Menus (Kernel, True);
         Change_Dir (Old_Dir);
         Free (Cmd);
         Free (Args);
         Free (Fd);

      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Change_Dir (Old_Dir);

         if State_Pushed then
            Pop_State (Kernel);
         end if;
   end On_Build;

   --------------
   -- On_Build --
   --------------

   procedure On_Build
     (Kernel : access GObject_Record'Class; Data : File_Project_Record) is
   begin
      On_Build
        (Kernel_Handle (Kernel), Data.File,
         Data.Project, Synchronous => False);
   end On_Build;

   ---------------------
   -- On_Check_Syntax --
   ---------------------

   procedure On_Check_Syntax
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Context : constant Selection_Context_Access :=
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

      declare
         Top  : constant Glide_Window :=
           Glide_Window (Get_Main_Window (Kernel));
         File_Context : constant File_Selection_Context_Access :=
           File_Selection_Context_Access (Context);
         File : constant VFS.Virtual_File := File_Information (File_Context);
         Cmd  : constant String :=
           Get_Attribute_Value
             (Get_Project (Kernel), Compiler_Command_Attribute,
              Default => "gnatmake", Index => "ada")
           & " -q -u -gnats " & Full_Name (File).all;
         Fd   : Process_Descriptor_Access;
         Args : Argument_List_Access;
         Lang : String := Get_Language_From_File
           (Get_Language_Handler (Kernel), File);
         C    : Build_Command_Access;

      begin
         if File = VFS.No_File then
            Console.Insert
              (Kernel, -"No file name, cannot check syntax",
               Mode => Error);
            return;
         end if;

         To_Lower (Lang);

         if Lang /= "ada" then
            Console.Insert
              (Kernel, -"Syntax check of non Ada file not yet supported",
               Mode => Error);
            return;
         end if;

         if Save_Child (Kernel, Get_File_Editor (Kernel, File), Force => False)
           = Cancel
         then
            return;
         end if;

         Trace (Me, "On_Check_Syntax: " & Cmd);
         Push_State (Kernel, Processing);

         Clear_Compilation_Output (Kernel);

         Set_Sensitive_Menus (Kernel, False);
         Args := Argument_String_To_List (Cmd);
         Console.Insert (Kernel, Cmd);
         Console.Raise_Console (Kernel);

         Top.Interrupted := False;
         Fd := new TTY_Process_Descriptor;
         Non_Blocking_Spawn
           (Fd.all, Args (Args'First).all, Args (Args'First + 1 .. Args'Last),
            Err_To_Out  => True);
         Free (Args);

         Create (C, (Kernel, Fd, null, null, null));
         Launch_Background_Command
           (Kernel, Command_Access (C), Active => False, Queue_Id => "");

      exception
         when Invalid_Process =>
            Console.Insert (Kernel, -"Invalid command", Mode => Error);
            Pop_State (Kernel);
            Set_Sensitive_Menus (Kernel, True);
            Free (Args);
            Free (Fd);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Check_Syntax;

   ------------------
   -- Compile_File --
   ------------------

   procedure Compile_File
     (Kernel : Kernel_Handle;
      File   : Virtual_File;
      Synchronous : Boolean := False)
   is
      Arg1         : aliased String := "-u";
      Top          : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Prj : constant Project_Type :=
        Get_Project_From_File (Get_Registry (Kernel), File);
      Cmd          : String_Access;
      Fd           : Process_Descriptor_Access;
      Local_File   : aliased String := Locale_Full_Name (File);
      Lang         : String := Get_Language_From_File
        (Get_Language_Handler (Kernel), File);
      C            : Build_Command_Access;
      Old_Dir      : constant Dir_Name_Str := Get_Current_Dir;

   begin
      if File = VFS.No_File then
         Console.Insert
           (Kernel, -"No file name, cannot compile",
            Mode => Error);
      end if;

      To_Lower (Lang);

      if Lang /= "ada" then
         Console.Insert
           (Kernel, -"Compilation of non Ada file not supported yet",
            Mode => Error);
         return;
      end if;

      if not Save_All_MDI_Children
        (Kernel, Force => Get_Pref (Kernel, Auto_Save))
      then
         return;
      end if;

      if Prj = No_Project then
         Console.Insert
           (Kernel, -"Could not determine the project for file: "
            & Full_Name (File).all,
            Mode => Error);
         return;

      else
         Cmd := new String'
           (Get_Attribute_Value
              (Prj, Compiler_Command_Attribute,
               Default => "gnatmake", Index => "ada"));
      end if;

      Change_Dir (Dir_Name (Project_Path (Prj)));
      Push_State (Kernel, Processing);
      Clear_Compilation_Output (Kernel);
      Set_Sensitive_Menus (Kernel, False);
      Top.Interrupted := False;
      Fd := new TTY_Process_Descriptor;

      if Status (Prj) /= From_File then
         Insert_And_Launch
           (Kernel,
            Remote_Protocol => Get_Pref (GVD_Prefs, Remote_Protocol),
            Remote_Host    => Get_Attribute_Value (Prj, Remote_Host_Attribute),
            Command         => Cmd.all,
            Arguments       => Default_Builder_Switches &
              (Arg1'Unchecked_Access, Local_File'Unchecked_Access),
            Fd              => Fd);

      else
         declare
            Args    : Argument_List_Access := Argument_String_To_List
              ("-u " & Scenario_Variables_Cmd_Line (Kernel, GNAT_Syntax));
            Prj_Arg : aliased String := "-P" & Project_Path (Prj);

         begin
            Insert_And_Launch
              (Kernel,
               Remote_Protocol => Get_Pref (GVD_Prefs, Remote_Protocol),
               Remote_Host     =>
                 Get_Attribute_Value (Prj, Remote_Host_Attribute),
               Command         => Cmd.all,
               Arguments       => Args.all &
                 (Prj_Arg'Unchecked_Access, Local_File'Unchecked_Access),
               Fd              => Fd);
            Free (Args);
         end;
      end if;

      Free (Cmd);
      Create (C, (Kernel, Fd, null, null, null));

      if Synchronous then
         Launch_Synchronous (Command_Access (C), 0.1);
         Destroy (Command_Access (C));

      else
         Launch_Background_Command
           (Kernel, Command_Access (C), Active => False, Queue_Id => "");
      end if;

      Change_Dir (Old_Dir);

   exception
      when Invalid_Process =>
         Console.Insert (Kernel, -"Invalid command", Mode => Error);
         Pop_State (Kernel);
         Change_Dir (Old_Dir);
         Set_Sensitive_Menus (Kernel, True);
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
      Node : List_Node;
      Instance : Class_Instance;
      Info     : File_Info;
      Kernel   : constant Kernel_Handle := Get_Kernel (Data);
      C        : Xref_Commands.Generic_Asynchronous_Command_Access;
   begin
      if Command = "compile" then
         Instance := Nth_Arg (Data, 1, Get_File_Class (Kernel));
         Info := Get_Data (Instance);
         Compile_File (Get_Kernel (Data), Get_File (Info),
                       Synchronous => True);

      elsif Command = "make" then
         Instance := Nth_Arg (Data, 1, Get_File_Class (Kernel));
         Info := Get_Data (Instance);

         declare
            Main    : constant Virtual_File := Get_File (Info);
            Project : constant Project_Type := Get_Project_From_File
              (Registry => Project_Registry (Get_Registry (Get_Kernel (Data))),
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
           (C,
            -"Computing cross-references information",
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Compile;

   ---------------
   -- On_Custom --
   ---------------

   procedure On_Custom
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Cmd : constant String := Simple_Entry_Dialog
        (Parent   => Get_Main_Window (Kernel),
         Title    => -"Custom Execution",
         Message  => -"Enter the command to execute:",
         Position => Gtk.Enums.Win_Pos_Mouse,
         History  => Get_History (Kernel),
         Key      => "gps_custom_command");

      C   : Build_Command_Access;

   begin
      if Cmd = "" or else Cmd (Cmd'First) = ASCII.NUL then
         return;
      end if;

      declare
         Top     : constant Glide_Window :=
           Glide_Window (Get_Main_Window (Kernel));
         Fd      : Process_Descriptor_Access;

      begin
         if not Save_All_MDI_Children
           (Kernel, Force => Get_Pref (Kernel, Auto_Save))
         then
            return;
         end if;

         Push_State (Kernel, Processing);
         Clear_Compilation_Output (Kernel);
         Set_Sensitive_Menus (Kernel, False);

         Top.Interrupted := False;
         Fd := new TTY_Process_Descriptor;

         Insert_And_Launch
           (Kernel,
            Remote_Protocol  => Get_Pref (GVD_Prefs, Remote_Protocol),
            Remote_Host      =>
              Get_Attribute_Value
                (Get_Project (Kernel), Remote_Host_Attribute),
            Cmd_Line         => Cmd,
            Fd               => Fd);

         Create (C, (Kernel, Fd, null, null, null));

         Launch_Background_Command
           (Kernel, Command_Access (C), Active => False, Queue_Id => "");

      exception
         when Invalid_Process =>
            Console.Insert (Kernel, -"Invalid command", Mode => Error);
            Pop_State (Kernel);
            Set_Sensitive_Menus (Kernel, True);
            Free (Fd);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
      Handler      : constant Glide_Language_Handler :=
        Glide_Language_Handler (Get_Language_Handler (D.Kernel));
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
                  Root_Project => Get_Project (D.Kernel),
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
        (C, -"Computing cross-references information",
         new Compute_Xref_Data'(Kernel, new LI_Handler_Iterator_Access, 0),
         Xref_Iterate'Access);

      Launch_Background_Command
        (Kernel, Command_Access (C), True, "");

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Remote_Cmd : constant String :=
           Get_Pref (GVD_Prefs, Remote_Protocol);
         Remote_Host     : constant String :=
           Get_Attribute_Value (Data.Project, Remote_Host_Attribute);
         Exec : String_Access;
      begin
         if Remote_Host = ""
           or else Remote_Cmd = ""
         then
            Exec := Locate_Exec_On_Path (Command);

            if Exec = null then
               Insert (K, -"Could not locate executable on path: " & Command);
            else
               Launch_Process
                 (K, Exec.all, Arguments,
                  Title, null, null, "", Success, True);
            end if;

            Free (Exec);

         else
            declare
               Full_Command : constant String :=
                 Command & " " & Argument_List_To_String (Arguments);
               New_Args     : Argument_List_Access
                 := Argument_String_To_List (Remote_Cmd & " " & Remote_Host);
               Last_Arg     : String_Access := new String'(Full_Command);
            begin
               Launch_Process
                 (K, New_Args (New_Args'First).all,
                  New_Args (New_Args'First + 1 .. New_Args'Last) & Last_Arg,
                  Title, null, null, "", Success, True);

               Free (Last_Arg);
               Free (New_Args);
            end;
         end if;
      end Launch;

   begin
      if Data.File = VFS.No_File then
         declare
            Command : constant String := Display_Entry_Dialog
              (Parent        => Get_Main_Window (K),
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
                    (Get_Pref (K, GVD.Preferences.Execute_Command)
                     & ' ' & Command);
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
              (Parent        => Get_Main_Window (K),
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
                    (Get_Pref (K, GVD.Preferences.Execute_Command) & ' ' &
                     Full_Name (Data.File).all & ' ' & Arguments);
                  Launch
                    (Args (Args'First).all, Args (Args'First + 1 .. Args'Last),
                     -"Run: " & Base_Name (Data.File).all & ' ' &
                     Krunch (Arguments, 12));

               else
                  Args := Argument_String_To_List (Arguments);
                  Launch
                    (Full_Name (Data.File).all,
                     Args.all, -"Run: " & Base_Name (Data.File).all
                     & ' ' & Krunch (Arguments, 12));
               end if;

               Free (Args);
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Run;

   -------------------
   -- On_Stop_Build --
   -------------------

   procedure On_Stop_Build
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top : constant Glide_Window := Glide_Window (Get_Main_Window (Kernel));
   begin
      Top.Interrupted := True;
      Console.Raise_Console (Kernel);
   end On_Stop_Build;

   --------------------
   -- Add_Build_Menu --
   --------------------

   procedure Add_Build_Menu
     (Menu         : in out Gtk_Menu;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Set_Shortcut : Boolean)
   is
      Mains : Argument_List := Get_Attribute_Value
        (Project, Attribute => Main_Attribute);
      Mitem        : Gtk_Menu_Item;
      Has_Child    : Boolean := False;
      Builder_Module : constant Builder_Module_ID_Access :=
        Builder_Module_ID_Access (Builder_Module_ID);
      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);

   begin
      if Menu = null then
         Gtk_New (Menu);
      end if;

      --  Accelerators were removed when the menu items were destroyed (just
      --  before the update)
      Builder_Module.Build_Item := null;

      for M in Mains'Range loop
         Gtk_New (Mitem, Mains (M).all);
         Append (Menu, Mitem);
         File_Project_Cb.Object_Connect
           (Mitem, "activate",
            File_Project_Cb.To_Marshaller (On_Build'Access),
            Slot_Object => Kernel,
            User_Data => File_Project_Record'
              (Project => Project,
               File    => Create (Mains (M).all, Project)));

         --  The first item in the make menu should have a key binding
         if Set_Shortcut
           and then not Has_Child
         then
            Add_Accelerator
              (Mitem, "activate", Get_Default_Accelerators (Kernel),
               GDK_F4, 0, Gtk.Accel_Group.Accel_Visible);
            Builder_Module.Build_Item := Mitem;
            Has_Child := True;

         else
            --  ??? F4 key binding is no longer taken into account if the
            --  following line is called systematically:
            Set_Accel_Path (Mitem, "<gps>/Build/Make/" & Mains (M).all, Group);
         end if;
      end loop;

      --  ??? gtk+ bug: mitem is created with a refcount of 2, and therefore
      --  never destroyed later on. As a result, the keybinding will remaining
      --  active for the whole life of GPS.

      Gtk_New (Mitem, -"Project");
      Append (Menu, Mitem);

      if Set_Shortcut then
         Set_Accel_Path (Mitem, "<gps>/Build/Make/Project", Group);
      end if;

      File_Project_Cb.Object_Connect
        (Mitem, "activate",
         File_Project_Cb.To_Marshaller (On_Build'Access),
         Slot_Object => Kernel,
         User_Data => File_Project_Record'
           (Project => Get_Project (Kernel),
            File    => VFS.No_File));

      Free (Mains);
   end Add_Build_Menu;

   ------------------
   -- Add_Run_Menu --
   ------------------

   procedure Add_Run_Menu
     (Menu         : in out Gtk_Menu;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class)
   is
      Mains : constant Argument_List := Get_Attribute_Value
        (Project, Attribute => Main_Attribute);
      Mitem : Gtk_Menu_Item;
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
            Gtk_New (Mitem, Exec);
            Append (Menu, Mitem);
            Set_Accel_Path (Mitem, "<gps>/Build/Run/" & Exec, Group);
            File_Project_Cb.Object_Connect
              (Mitem, "activate",
               File_Project_Cb.To_Marshaller (On_Run'Access),
               Slot_Object => Kernel,
               User_Data => File_Project_Record'
                 (Project => Project,
                  File    => Create
                    (Executables_Directory (Project) & Exec)));
         end;
      end loop;
   end Add_Run_Menu;

   ---------------------
   -- On_View_Changed --
   ---------------------

   procedure On_View_Changed
     (K : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (K);

      Builder_Module : constant Builder_Module_ID_Access :=
        Builder_Module_ID_Access (Builder_Module_ID);
      Mitem : Gtk_Menu_Item;
      Menu1 : Gtk_Menu renames Builder_Module.Make_Menu;
      Menu2 : Gtk_Menu renames Builder_Module.Run_Menu;
      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);
   begin
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
            Remove_All_Children (Builder_Module.Make_Menu);
         end if;

         if Builder_Module.Run_Menu /= null then
            Remove_All_Children (Builder_Module.Run_Menu);
         end if;

         while P /= No_Project loop
            Add_Build_Menu
              (Menu         => Builder_Module.Make_Menu,
               Project      => P,
               Kernel       => Kernel,
               Set_Shortcut => True);
            Add_Run_Menu
              (Menu         => Builder_Module.Run_Menu,
               Project      => P,
               Kernel       => Kernel);
            P := Parent_Project (P);
         end loop;
      end;

      --  No main program ?

      Gtk_New (Mitem, -"<current file>");
      Append (Menu1, Mitem);
      Set_Accel_Path (Mitem, "<gps>/Build/Make/<current file>", Group);
      File_Project_Cb.Object_Connect
        (Mitem, "activate",
         File_Project_Cb.To_Marshaller (On_Build'Access),
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

      Gtk_New (Mitem, -"Custom...");
      Append (Menu1, Mitem);
      --  ??? F9 key binding is no longer taken into account if the following
      --  line is commented out:
      --  Set_Accel_Path (Mitem, "<gps>/Build/Make/Custom...", Group);
      Kernel_Callback.Connect
        (Mitem, "activate",
         Kernel_Callback.To_Marshaller (On_Custom'Access),
         User_Data => Kernel);
      Add_Accelerator
        (Mitem, "activate", Group, GDK_F9, 0, Gtk.Accel_Group.Accel_Visible);

      --  Should be able to run any program

      Gtk_New (Mitem, -"Custom...");
      Append (Menu2, Mitem);
      Set_Accel_Path (Mitem, "<gps>/Build/Run/Custom...", Group);
      File_Project_Cb.Object_Connect
        (Mitem, "activate",
         File_Project_Cb.To_Marshaller (On_Run'Access),
         Slot_Object => Kernel,
         User_Data   => File_Project_Record'
           (Project => Get_Project (Kernel), File => VFS.No_File));
      Show_All (Menu1);
      Show_All (Menu2);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_View_Changed;

   ------------------------
   -- Builder_Contextual --
   ------------------------

   procedure Builder_Contextual
     (Object    : access GObject_Record'Class;
      Context   : access Selection_Context'Class;
      Menu      : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);
      Item         : Gtk_Menu_Item;
      File_Context : File_Selection_Context_Access;
      Submenu      : Gtk_Menu;
   begin
      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_Project_Information (File_Context)
           and then not Has_Directory_Information (File_Context)
           and then not Has_File_Information (File_Context)
         then
            Add_Build_Menu
              (Menu         => Submenu,
               Project      => Project_Information (File_Context),
               Kernel       => Get_Kernel (Context),
               Set_Shortcut => False);

            if Submenu /= null then
               Gtk_New (Item, -"Build");
               Set_Submenu (Item, Submenu);
               Append (Menu, Item);
            end if;

            Submenu := null;
            Add_Run_Menu
              (Menu         => Submenu,
               Project      => Project_Information (File_Context),
               Kernel       => Get_Kernel (Context));

            if Submenu /= null then
               Gtk_New (Item, -"Run");
               Set_Submenu (Item, Submenu);
               Append (Menu, Item);
            end if;
         end if;
      end if;
   end Builder_Contextual;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
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
         Contextual_Menu_Handler => Builder_Contextual'Access,
         Priority     => Default_Priority);

      Register_Menu (Kernel, "/_" & (-"Build"), Ref_Item => -"Debug");
      Register_Menu (Kernel, Build, -"Check _Syntax", "",
                     On_Check_Syntax'Access);
      Register_Menu (Kernel, Build, -"_Compile File", "",
                     On_Compile'Access, null, GDK_F4, Shift_Mask);

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
      Set_Sensitive
        (Register_Menu
           (Kernel, Build, -"_Interrupt", Stock_Stop, On_Stop_Build'Access,
           null, GDK_C, Control_Mask + Shift_Mask),
         False);

      Kernel_Callback.Connect
        (Kernel, "project_view_changed",
         Kernel_Callback.To_Marshaller (On_View_Changed'Access),
         User_Data => Kernel_Handle (Kernel));

      Register_Command
        (Kernel,
         Command      => "compile",
         Description  =>
           -("Compile the file. This call will return only once the"
             & " compilation is completed"),
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Get_File_Class (Kernel),
         Handler      => Compile_Command'Access);

      Register_Command
        (Kernel,
         Command      => "make",
         Description  =>
           -("Compile and link the file and all its dependencies. This call"
             & " will return only once the compilation is completed"),
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Get_File_Class (Kernel),
         Handler      => Compile_Command'Access);

      Register_Command
        (Kernel,
         Command      => "compute_xref",
         Description  =>
           -("Update the cross-reference information stored in GPS. This"
             & " needs to be called after major changes to the sources"
             & " only, since GPS itself is able to work with partially"
             & " up-to-date information"),
         Minimum_Args => 0,
         Maximum_Args => 0,
         Handler      => Compile_Command'Access);

      Register_Command
        (Kernel,
         Command      => "get_build_output",
         Return_Value => "list of lines",
         Description  => -"Return the last compilation results.",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Handler      => Compile_Command'Access);

      --  Create the highlighting category corresponding to the module.

      Kernel_Callback.Connect
        (Kernel, Preferences_Changed_Signal,
         Kernel_Callback.To_Marshaller (Preferences_Changed'Access),
         User_Data   => Kernel_Handle (Kernel));

      Preferences_Changed (Kernel, Kernel_Handle (Kernel));
   end Register_Module;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (K : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (K);
      Args : Argument_List :=
        (1 => new String'(Builder_Category),
         2 => new String'
           (To_String (Get_Pref (Kernel, Message_Src_Highlight))));

   begin
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
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
