-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Gtk.Enums;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Stock;                 use Gtk.Stock;

with Glide_Intl;                use Glide_Intl;

with GVD.Preferences;           use GVD.Preferences;
with GVD.Status_Bar;            use GVD.Status_Bar;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Timeout;      use Glide_Kernel.Timeout;
with Language_Handlers;         use Language_Handlers;
with Language_Handlers.Glide;   use Language_Handlers.Glide;
with Projects.Editor;           use Projects, Projects.Editor;
with Projects.Registry;         use Projects.Registry;
with Src_Info;                  use Src_Info;
with Histories;                 use Histories;

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
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Case_Util;            use GNAT.Case_Util;

with Traces;                    use Traces;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package body Builder_Module is

   Cst_Run_Arguments_History : constant History_Key := "gvd_run_arguments";
   --  The key in the history for the arguments to the run command.
   --  WARNING: this constant is shared with gvd-menu.adb, since we want to
   --  have the same history for the debugger arguments.

   Run_External_Key : constant History_Key := "run_external_terminal";
   --  The key in the history for the check button "run in external terminal"

   Timeout : constant Guint32 := 50;
   --  Timeout in milliseconds to check the build process

   Timeout_Xref : constant Guint32 := 50;
   --  Timeout in milliseconds to generate the xref information

   Me : constant Debug_Handle := Create (Builder_Module_Name);

   All_Files : constant String := "<all>";
   --  String id used to represent all files.

   type Builder_Module_ID_Record is new Module_ID_Record with record
      Make_Menu  : Gtk_Menu;
      Run_Menu   : Gtk_Menu;
      Build_Item : Gtk_Menu_Item;
      --  The build menu, updated automatically every time the list of main
      --  units changes.

      Output     : String_List_Utils.String_List.List;
      --  The last build output.
   end record;
   --  Data stored with the module id.

   type Builder_Module_ID_Access is access all Builder_Module_ID_Record;

   function Idle_Build (Data : Process_Data) return Boolean;
   --  Called by the Gtk main loop when idle.
   --  Handle on going build.

   type LI_Handler_Iterator_Access_Access is access LI_Handler_Iterator_Access;

   type Compute_Xref_Data is record
      Kernel : Kernel_Handle;
      Iter   : LI_Handler_Iterator_Access_Access;
      LI     : Natural;
   end record;
   type Compute_Xref_Data_Access is access Compute_Xref_Data;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (LI_Handler_Iterator_Access, LI_Handler_Iterator_Access_Access);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Compute_Xref_Data, Compute_Xref_Data_Access);

   package Xref_Timeout is new Gtk.Main.Timeout (Compute_Xref_Data_Access);

   procedure Timeout_Xref_Destroy (D : in out Compute_Xref_Data_Access);
   --  Destroy the memory associated with an Xref_Timeout.

   function Timeout_Compute_Xref
     (D : Compute_Xref_Data_Access) return Boolean;
   --  Compute the cross-references for the next files in the project.

   procedure Set_Sensitive_Menus
     (Kernel    : Kernel_Handle;
      Sensitive : Boolean);
   --  Change the sensitive aspect of the build menu items.

   procedure Free (Ar : in out String_List);
   procedure Free (Ar : in out String_List_Access);
   --  Free the memory associate with Ar.

   function Compute_Arguments
     (Kernel  : Kernel_Handle;
      Syntax  : Command_Syntax;
      Project : String;
      File    : String) return Argument_List_Access;
   --  Compute the make arguments following the right Syntax
   --  (gnatmake / make), given a Project and File name.
   --  It is the responsibility of the caller to free the returned object.

   procedure Parse_Compiler_Output
     (Kernel : Kernel_Handle;
      Output : String);
   --  Parse the output of build engine and insert the result
   --    - in the GPS results view if it corresponds to a file location
   --    - in the GPS console if it is a general message.

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

   function Compile_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : Argument_List) return String;
   --  Command handler for the "compile" command.

   procedure Compile_File
     (Kernel : Kernel_Handle;
      File   : String);
   --  Launch a compilation command for File.

   procedure Clear_Compilation_Output (Kernel : Kernel_Handle);
   --  Clear the compiler output, the console, and the result view.

   ------------------------------
   -- Clear_Compilation_Output --
   ------------------------------

   procedure Clear_Compilation_Output (Kernel : Kernel_Handle) is
   begin
      Console.Clear (Kernel);
      Remove_Result_Category (Kernel, -"Builder Results");
      String_List_Utils.String_List.Free
        (Builder_Module_ID_Access (Builder_Module_ID).Output);
   end Clear_Compilation_Output;

   ---------------------------
   -- Parse_Compiler_Output --
   ---------------------------

   procedure Parse_Compiler_Output
     (Kernel : Kernel_Handle;
      Output : String) is
   begin
      Insert (Kernel, Output, Add_LF => False);
      String_List_Utils.String_List.Append
        (Builder_Module_ID_Access (Builder_Module_ID).Output,
         Output);
      Parse_File_Locations (Kernel, Output, -"Builder Results");

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Parse_Compiler_Output;

   -----------------------
   -- Compute_Arguments --
   -----------------------

   function Compute_Arguments
     (Kernel  : Kernel_Handle;
      Syntax  : Command_Syntax;
      Project : String;
      File    : String) return Argument_List_Access
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

            if File = All_Files then
               File_Arg := new String'("");
            else
               File_Arg := new String'(File);
            end if;

            if Build_Progress then
               Result := new Argument_List'
                 ((new String'("-d"),
                   new String'("-P" & Project),
                   File_Arg) & Vars.all);
            else
               Result := new Argument_List'
                 ((new String'("-P" & Project),
                   File_Arg) & Vars.all);
            end if;

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

                  if File = All_Files then
                     File_Arg := new String'("");
                  else
                     File_Arg :=
                       new String'("ADA_SOURCES=" & Base_Name (File));
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
              Arguments,
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
     (Kernel : access GObject_Record'Class; Data : File_Project_Record)
   is
      K            : constant Kernel_Handle := Kernel_Handle (Kernel);
      Top          : constant Glide_Window :=
        Glide_Window (Get_Main_Window (K));
      Fd           : Process_Descriptor_Access;
      Cmd          : String_Access;
      Args         : Argument_List_Access;
      Id           : Timeout_Handler_Id;
      pragma Unreferenced (Id);

      Context      : Selection_Context_Access;
      Prj          : Project_Type;
      Project      : constant Project_Type := Get_Project (K);
      Langs        : Argument_List := Get_Languages
        (Project, Recursive => True);
      Syntax       : Command_Syntax;
      State_Pushed : Boolean := False;

   begin
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

      if Save_All_MDI_Children (K, Force => False) = False then
         Free (Args);
         return;
      end if;

      --  If no file was specified in data, simply compile the current file.

      if Data.Length = 0 then
         Context := Get_Current_Context (K);

         if Context /= null
           and then Context.all in File_Selection_Context'Class
           and then Has_File_Information
             (File_Selection_Context_Access (Context))
         then
            declare
               File : constant String :=
                 File_Information (File_Selection_Context_Access (Context));
            begin
               Prj := Get_Project_From_File (Get_Registry (K), File);

               if Prj = No_Project or else Is_Default (Project) then
                  Args := new Argument_List'
                    (Clone (Default_Builder_Switches) & new String'(File));
               else
                  Args := Compute_Arguments
                    (K, Syntax, Project_Path (Prj), File);
               end if;
            end;

         --  There is no current file, so we can't compile anything

         else
            Console.Insert
              (K, -"No file selected, cannot build", Mode => Error);
            return;
         end if;

      else
         --  Are we using the default internal project ?

         if Is_Default (Get_Project (K)) then
            case Syntax is
               when GNAT_Syntax =>
                  if Data.File = All_Files then
                     Console.Insert
                       (K, -"Default project has no main unit", Mode => Error);
                     return;
                  else
                     Args := new Argument_List'
                       (Clone (Default_Builder_Switches)
                        & new String'(Data.File));
                  end if;

               when Make_Syntax =>
                  Console.Insert
                    (K, -"You must save the project file before building",
                     Mode => Error);
                  return;
            end case;
         else
            Args := Compute_Arguments
              (K, Syntax, Project_Path (Data.Project), Data.File);
         end if;
      end if;

      Clear_Compilation_Output (K);

      Push_State (K, Processing);
      State_Pushed := True;

      case Syntax is
         when GNAT_Syntax =>
            Cmd := new String'(Get_Attribute_Value
              (Project, Compiler_Command_Attribute,
               Ide_Package, Default => "gnatmake", Index => "Ada"));

         when Make_Syntax =>
            Cmd := new String'("make");
      end case;

      Set_Sensitive_Menus (K, False);

      Top.Interrupted := False;
      Fd := new TTY_Process_Descriptor;

      Insert_And_Launch
        (K,
         Remote_Protocol  =>
           Get_Pref (GVD_Prefs, Remote_Protocol),
         Remote_Host      =>
           Get_Attribute_Value
             (Project, Remote_Host_Attribute, Ide_Package),
         Command          => Cmd.all,
         Arguments        => Args.all,
         Fd               => Fd);

      Free (Cmd);
      Free (Args);
      Id := Process_Timeout.Add
        (Timeout, Idle_Build'Access, (K, Fd, null, null, null));

   exception
      when Invalid_Process =>
         Console.Insert (K, -"Invalid command", Mode => Error);
         Pop_State (K);
         Set_Sensitive_Menus (K, True);
         Free (Cmd);
         Free (Args);
         Free (Fd);

      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));

         if State_Pushed then
            Pop_State (K);
         end if;
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
         File : constant String := Directory_Information (File_Context) &
           File_Information (File_Context);
         Cmd  : constant String :=
           Get_Attribute_Value
             (Get_Project (Kernel), Compiler_Command_Attribute,
              Ide_Package, Default => "gnatmake", Index => "Ada")
           & " -q -u -gnats " & File;
         Fd   : Process_Descriptor_Access;
         Args : Argument_List_Access;
         Id   : Timeout_Handler_Id;
         pragma Unreferenced (Id);

         Lang : String := Get_Language_From_File
           (Get_Language_Handler (Kernel), File);

      begin
         if File = "" then
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
         Id := Process_Timeout.Add
           (Timeout, Idle_Build'Access, (Kernel, Fd, null, null, null));

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
      File   : String)
   is
      Arg1         : aliased String := "-u";
      Top          : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Prj : constant Project_Type :=
        Get_Project_From_File (Get_Registry (Kernel), File);
      Project      : constant String := Project_Name (Prj);
      Cmd          : String_Access;
      Fd           : Process_Descriptor_Access;
      Local_File   : aliased String := File;
      Id           : Timeout_Handler_Id;
      pragma Unreferenced (Id);

      Lang         : String := Get_Language_From_File
        (Get_Language_Handler (Kernel), File);

   begin
      if File = "" then
         Console.Insert
           (Kernel, -"No file name, cannot compile",
            Mode => Error);
      end if;

      To_Lower (Lang);

      if Lang /= "ada" then
         Console.Insert
           (Kernel, -"Compilation of non Ada file not yet supported",
            Mode => Error);
         return;
      end if;

      if Save_All_MDI_Children (Kernel, Force => False) = False then
         return;
      end if;

      if Prj = No_Project then
         Console.Insert
           (Kernel, -"Could not determine the project for file: " & File,
            Mode => Error);
         return;

      else
         Cmd := new String'
           (Get_Attribute_Value
              (Prj, Compiler_Command_Attribute,
               Ide_Package,
               Default => "gnatmake", Index => "Ada"));
      end if;

      Push_State (Kernel, Processing);
      Clear_Compilation_Output (Kernel);
      Set_Sensitive_Menus (Kernel, False);
      Top.Interrupted := False;
      Fd := new TTY_Process_Descriptor;

      if Project = "" then
         Insert_And_Launch
           (Kernel,
            Remote_Protocol =>
              Get_Pref (GVD_Prefs, Remote_Protocol),
            Remote_Host     =>
              Get_Attribute_Value
              (Prj, Remote_Host_Attribute, Ide_Package),
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
               Remote_Protocol =>
                 Get_Pref (GVD_Prefs, Remote_Protocol),
               Remote_Host     =>
                 Get_Attribute_Value
                 (Prj, Remote_Host_Attribute, Ide_Package),
               Command         => Cmd.all,
               Arguments       => Args.all &
                 (Prj_Arg'Unchecked_Access, Local_File'Unchecked_Access),
               Fd              => Fd);
            Free (Args);
         end;
      end if;

      Free (Cmd);
      Id := Process_Timeout.Add
        (Timeout, Idle_Build'Access, (Kernel, Fd, null, null, null));

   exception
      when Invalid_Process =>
         Console.Insert (Kernel, -"Invalid command", Mode => Error);
         Pop_State (Kernel);
         Set_Sensitive_Menus (Kernel, True);
         Free (Fd);
   end Compile_File;

   ---------------------
   -- Compile_Command --
   ---------------------

   function Compile_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String
   is
      use String_List_Utils.String_List;
      Node : List_Node;
      L    : Integer := 0;

   begin
      if Command = "compile" then
         for Index in Args'Range loop
            Compile_File (Kernel_Handle (Kernel), Args (Index).all);
         end loop;

      elsif Command = "get_build_output" then
         Node := First
           (Builder_Module_ID_Access (Builder_Module_ID).Output);

         while Node /= Null_Node loop
            L := L + Data (Node)'Length;
            Node := Next (Node);
         end loop;

         if L /= 0 then
            declare
               S      : String (1 .. L);
               Length : Natural;
            begin
               L := 1;
               Node := First
                 (Builder_Module_ID_Access (Builder_Module_ID).Output);

               while Node /= Null_Node loop
                  Length := Data (Node)'Length;
                  S (L .. L + Length - 1) := Data (Node);
                  L := L + Length;

                  Node := Next (Node);
               end loop;

               return S;
            end;
         end if;
      end if;

      return "";
   end Compile_Command;

   ----------------
   -- On_Compile --
   ----------------

   procedure On_Compile
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
           (Kernel, -"No file selected, cannot compile", Mode => Error);
         return;
      end if;

      Compile_File
        (Kernel,
         File_Information (File_Selection_Context_Access (Context)));

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

      Cmd          : constant String := Simple_Entry_Dialog
        (Parent   => Get_Main_Window (Kernel),
         Title    => -"Custom Execution",
         Message  => -"Enter the command to execute:",
         Position => Gtk.Enums.Win_Pos_Mouse,
         History  => Get_History (Kernel),
         Key      => "gps_custom_command");

   begin
      if Cmd = "" or else Cmd (Cmd'First) = ASCII.NUL then
         return;
      end if;

      declare
         Top     : constant Glide_Window :=
           Glide_Window (Get_Main_Window (Kernel));
         Fd      : Process_Descriptor_Access;
         Id      : Timeout_Handler_Id;
         pragma Unreferenced (Id);

      begin
         if Save_All_MDI_Children (Kernel, Force => False) = False then
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
                (Get_Project (Kernel), Remote_Host_Attribute, Ide_Package),
            Cmd_Line         => Cmd,
            Fd               => Fd);
         Id := Process_Timeout.Add
           (Timeout, Idle_Build'Access, (Kernel, Fd, null, null, null));

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

   --------------------------
   -- Timeout_Xref_Destroy --
   --------------------------

   procedure Timeout_Xref_Destroy (D : in out Compute_Xref_Data_Access) is
   begin
      Pop_State (D.Kernel);
      Free (D.Iter.all);
      Unchecked_Free (D.Iter);
      Unchecked_Free (D);
   end Timeout_Xref_Destroy;

   --------------------------
   -- Timeout_Compute_Xref --
   --------------------------

   function Timeout_Compute_Xref
     (D : Compute_Xref_Data_Access) return Boolean
   is
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
            return False;
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

      return True;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Insert (D.Kernel, "Finished parsing all source files");
         return False;
   end Timeout_Compute_Xref;

   ---------------------
   -- On_Compute_Xref --
   ---------------------

   procedure On_Compute_Xref
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Object);
      Id      : Timeout_Handler_Id;
      pragma Unreferenced (Id);

   begin
      Push_State (Kernel, Processing);
      Id := Xref_Timeout.Add
        (Timeout_Xref, Timeout_Compute_Xref'Access,
         new Compute_Xref_Data'(Kernel, new LI_Handler_Iterator_Access, 0),
         Timeout_Xref_Destroy'Access);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Compute_Xref;

   ----------------
   -- Idle_Build --
   ----------------

   function Idle_Build (Data : Process_Data) return Boolean is
      Kernel  : Kernel_Handle renames Data.Kernel;
      Fd      : Process_Descriptor_Access := Data.Descriptor;

      Top          : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Matched      : Match_Array (0 .. 3);
      Result       : Expect_Match;
      Matcher      : constant Pattern_Matcher := Compile
        ("completed ([0-9]+) out of ([0-9]+) \((.*)%\)\.\.\.");
      Timeout      : Integer := 1;
      Line_Matcher : constant Pattern_Matcher :=
        Compile ("^.*?\n", Multiple_Lines);
      Buffer       : String_Access := new String (1 .. 1024);
      Buffer_Pos   : Natural := Buffer'First;
      Min_Size     : Natural;
      New_Size     : Natural;
      Tmp          : String_Access;
      Status       : Integer;

   begin
      if Top.Interrupted then
         Interrupt (Fd.all);
         Console.Insert (Kernel, "<^C>");
         Top.Interrupted := False;
         Print_Message
           (Top.Statusbar, GVD.Status_Bar.Help, -"Interrupting build...");
         Timeout := 10;
      end if;

      loop
         Expect (Fd.all, Result, Line_Matcher, Timeout => Timeout);

         exit when Result = Expect_Timeout;

         declare
            S : constant String := Strip_CR (Expect_Out (Fd.all));
         begin
            Match (Matcher, S, Matched);

            if Matched (0) = No_Match then
               --  Coalesce all the output into one single chunck, which is
               --  much faster to display in the console.

               Min_Size := Buffer_Pos + S'Length;

               if Buffer'Last < Min_Size then
                  New_Size := Buffer'Length * 2;

                  while New_Size < Min_Size loop
                     New_Size := New_Size * 2;
                  end loop;

                  Tmp := new String (1 .. New_Size);
                  Tmp (1 .. Buffer_Pos - 1) := Buffer (1 .. Buffer_Pos - 1);
                  Free (Buffer);
                  Buffer := Tmp;
               end if;

               Buffer (Buffer_Pos .. Buffer_Pos + S'Length - 1) := S;
               Buffer_Pos := Buffer_Pos + S'Length;

            else
               Set_Fraction
                 (Top.Statusbar,
                  Gdouble'Value
                    (S (Matched (3).First .. Matched (3).Last)) / 100.0);
               Set_Progress_Text
                 (Top.Statusbar, S (S'First .. Matched (2).Last));
            end if;
         end;
      end loop;

      if Buffer_Pos /= Buffer'First then
         Parse_Compiler_Output
           (Kernel, Buffer (Buffer'First .. Buffer_Pos - 1));
      end if;

      Free (Buffer);

      return True;

   exception
      when Process_Died =>
         if Buffer_Pos /= Buffer'First then
            Parse_Compiler_Output
              (Kernel,
               Buffer (Buffer'First .. Buffer_Pos - 1) & Expect_Out (Fd.all));
         end if;

         Free (Buffer);
         Set_Fraction (Top.Statusbar, 0.0);
         Set_Progress_Text (Top.Statusbar, "");
         Parse_Compiler_Output (Kernel, Expect_Out (Fd.all));
         Close (Fd.all, Status);

         if Status = 0 then
            Console.Insert
              (Kernel, ASCII.LF & (-"successful compilation/build"));
         else
            Console.Insert
              (Kernel,
               ASCII.LF & (-"process exited with status ") & Image (Status));
         end if;

         Pop_State (Kernel);
         Set_Sensitive_Menus (Kernel, True);
         Free (Fd);

         Compilation_Finished (Kernel, "");
         return False;

      when E : others =>
         Free (Buffer);
         Pop_State (Kernel);
         Set_Sensitive_Menus (Kernel, True);
         Close (Fd.all);
         Free (Fd);
         Set_Fraction (Top.Statusbar, 0.0);
         Set_Progress_Text (Top.Statusbar, "");
         Trace (Me, "Unexpected exception: " & Exception_Information (E));

         return False;
   end Idle_Build;

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
           Get_Attribute_Value
             (Data.Project, Remote_Host_Attribute, Ide_Package);
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
      if Data.Length = 0 then
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
                    (Get_Pref (K, Execute_Command) & ' ' & Command);
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
                    (Get_Pref (K, Execute_Command) & ' ' &
                     Executables_Directory (Data.Project) & Data.File & ' ' &
                     Arguments);
                  Launch
                    (Args (Args'First).all, Args (Args'First + 1 .. Args'Last),
                     -"Run: " & Data.File & ' ' & Arguments);

               else
                  Args := Argument_String_To_List (Arguments);
                  Launch
                    (Executables_Directory (Data.Project) & Data.File,
                     Args.all, -"Run: " & Data.File & ' ' & Arguments);
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
        (Project, Attribute_Name => Main_Attribute);
      Mitem        : Gtk_Menu_Item;
      Has_Child    : Boolean := False;
      Builder_Module : constant Builder_Module_ID_Access :=
        Builder_Module_ID_Access (Builder_Module_ID);
      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);

   begin
      if Menu = null then
         if Mains'Length = 0 then
            return;
         end if;
         Gtk_New (Menu);
      end if;

      --  Remove all existing menus and dynamic accelerators

      if Set_Shortcut
        and then Builder_Module.Build_Item /= null
      then
         Remove_Accelerator (Builder_Module.Build_Item, Group, GDK_F4, 0);
         Builder_Module.Build_Item := null;
      end if;

      Remove_All_Children (Menu);

      for M in Mains'Range loop
         Gtk_New (Mitem, Mains (M).all);
         Append (Menu, Mitem);
         File_Project_Cb.Object_Connect
           (Mitem, "activate",
            File_Project_Cb.To_Marshaller (On_Build'Access),
            Slot_Object => Kernel,
            User_Data => File_Project_Record'
            (Length  => Mains (M)'Length,
             Project => Project,
             File    => Mains (M).all));

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
        (Project, Attribute_Name => Main_Attribute);
      Mitem : Gtk_Menu_Item;
      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);

   begin
      if Menu = null then
         if Mains'Length = 0 then
            return;
         end if;

         Gtk_New (Menu);
      end if;

      Remove_All_Children (Menu);

      for M in Mains'Range loop
         declare
            Full : constant String := Base_Name (Mains (M).all);
            Exec : constant String := Full
              (Full'First .. Delete_File_Suffix (Full, Project));
         begin
            Gtk_New (Mitem, Exec);
            Append (Menu, Mitem);
            Set_Accel_Path (Mitem, "<gps>/Build/Run/" & Exec, Group);
            File_Project_Cb.Object_Connect
              (Mitem, "activate",
               File_Project_Cb.To_Marshaller (On_Run'Access),
               Slot_Object => Kernel,
               User_Data => File_Project_Record'
               (Length  => Exec'Length,
                Project => Project,
                File    => Exec));
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
      Add_Build_Menu
        (Menu         => Builder_Module.Make_Menu,
         Project      => Get_Project (Kernel),
         Kernel       => Kernel,
         Set_Shortcut => True);
      Add_Run_Menu
        (Menu         => Builder_Module.Run_Menu,
         Project      => Get_Project (Kernel),
         Kernel       => Kernel);

      --  No main program ?

      Gtk_New (Mitem, -"<current file>");
      Append (Menu1, Mitem);
      Set_Accel_Path (Mitem, "<gps>/Build/Make/<current file>", Group);
      File_Project_Cb.Object_Connect
        (Mitem, "activate",
         File_Project_Cb.To_Marshaller (On_Build'Access),
         Slot_Object => Kernel,
         User_Data => File_Project_Record'
           (Length  => 0,
            Project => No_Project,
            File    => ""));

      if Builder_Module.Build_Item = null then
         Add_Accelerator
           (Mitem, "activate", Group, GDK_F4, 0,
            Gtk.Accel_Group.Accel_Visible);
         Builder_Module.Build_Item := Mitem;
      end if;

      Gtk_New (Mitem, -"All main subprograms");
      Append (Menu1, Mitem);
      Set_Accel_Path (Mitem, "<gps>/Build/Make/<All main subprograms>", Group);
      File_Project_Cb.Object_Connect
        (Mitem, "activate",
         File_Project_Cb.To_Marshaller (On_Build'Access),
         Slot_Object => Kernel,
         User_Data => File_Project_Record'
           (Length  => All_Files'Length,
            Project => Get_Project (Kernel),
            File    => All_Files));

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
           (Length => 0, Project => Get_Project (Kernel), File => ""));
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
          (Kernel, Build, -"_Interrupt", Stock_Stop, On_Stop_Build'Access),
         False);

      Kernel_Callback.Connect
        (Kernel, "project_view_changed",
         Kernel_Callback.To_Marshaller (On_View_Changed'Access),
         User_Data => Kernel_Handle (Kernel));

      Register_Command
        (Kernel,
         Command      => "compile",
         Usage        => "compile file1 [file2] ...",
         Description  => -"Compile a list of files from the project.",
         Minimum_Args => 1,
         Maximum_Args => Natural'Last,
         Handler      => Compile_Command'Access);

      Register_Command
        (Kernel,
         Command      => "get_build_output",
         Usage        => "get_build_output",
         Description  => -"Return the last compilation results.",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Handler      => Compile_Command'Access);
   end Register_Module;

end Builder_Module;
