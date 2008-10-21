-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                 Copyright (C) 2001-2008, AdaCore                  --
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

with GNAT.Expect;               use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.OS_Lib;               use GNAT; use GNAT.OS_Lib;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNAT.Strings;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Gtk.Accel_Group;           use Gtk.Accel_Group;
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
with GPS.Kernel.Timeout;        use GPS.Kernel.Timeout;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;

with Projects;                  use Projects;
with Interactive_Consoles;      use Interactive_Consoles;
with Language_Handlers;         use Language_Handlers;
with Entities;                  use Entities;
with Histories;                 use Histories;
with Remote.Path.Translator;    use Remote, Remote.Path.Translator;
with Build_Command_Manager;
with Builder_Facility_Module;
with Basic_Types;
with Std_Dialogs;               use Std_Dialogs;
with String_Utils;              use String_Utils;
with GUI_Utils;                 use GUI_Utils;
with Traces;                    use Traces;
with Commands;                  use Commands;

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

   Run_Exec_Dir_Key : constant History_Key := "run_in_executable_directory";
   --  The key in the history for the check button
   --  "run in executable directory"

   Run_Menu_Prefix : constant String := "<gps>/Build/Run/";
   Item_Accel_Path : constant String := "item";
   --  Prefix used in accel path for items defined in this module

   Custom_Make_Suffix  : constant String := "Custom...";

   Sources_Load_Chunk : constant Integer := 1;
   --  The size of the chunk of files loaded by the xrefs loader.
   --  ??? This should be configurable

   Xrefs_Loading_Queue : constant String := "xrefs_loading";

   type Run_Description is record
      Command      : GNAT.Strings.String_Access;
      Arguments    : GNAT.OS_Lib.Argument_List_Access;
      Ext_Terminal : Boolean;
      Directory    : GNAT.Strings.String_Access;
      Title        : GNAT.Strings.String_Access;
   end record;
   --  The arguments used to run an executable from the "Run" menu

   procedure Free (Run : in out Run_Description);
   --  Free the memory associated with Run

   procedure Launch
     (Kernel       : access Kernel_Handle_Record'Class;
      Run          : Run_Description);
   --  Spawn the command described in Run on the Execution_Server.
   --  Caller must not free Run.

   procedure Set_Command
     (Run          : in out Run_Description;
      Ext_Terminal : Boolean;
      Command      : String);
   --  Set the command to be run. Gets both exec name and args from Command.

   type Builder_Module_ID_Record is
     new GPS.Kernel.Modules.Module_ID_Record
   with record
      Run_Menu   : Gtk.Menu.Gtk_Menu;
      --  The build menu, updated automatically every time the list of main
      --  units changes.

      Last_Run_Cmd  : Run_Description;
      --  The last command spawned from the run menu

      Build_Count : Natural := 0;
      --  Number of on-going builds
   end record;
   type Builder_Module_ID_Access is access all Builder_Module_ID_Record;
   --  Data stored with the module id

   Builder_Module_ID : Builder_Module_ID_Access;

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

   procedure Add_Run_Menu
     (Menu         : in out Gtk_Menu;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Mains        : Argument_List;
      Set_Shortcut : Boolean);
   --  Same as Add_Build_Menu, but for the Run menu

   type Run_Contextual is new Submenu_Factory_Record with null record;
   overriding procedure Append_To_Menu
     (Factory : access Run_Contextual;
      Object  : access GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add entries to the contextual menu for Build/ or Run/

   --------------------
   -- Menu Callbacks --
   --------------------

   procedure On_Compute_Xref
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Compute Xref information menu

   procedure On_Load_Xref_In_Memory
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Load Xref info

   procedure On_Compilation_Finished
     (Kernel : access Kernel_Handle_Record'Class;
      Data : access Hooks_Data'Class);
   --  Called when the compilation is finished

   function On_Compilation_Starting
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   --  Called when the compilation is starting

   procedure Load_Xref_In_Memory (Kernel : access Kernel_Handle_Record'Class);
   --  Load the Xref info in memory, in a background task

   procedure On_Run
     (Kernel : access GObject_Record'Class; Data : File_Project_Record);
   --  Build->Run menu

   procedure On_Run_Last_Launched
     (Menu   : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Build->Run->Last Launched
   --  Rererun the last run command

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
   --  Command handler for the "compile" command

   --------------------------
   -- Is_Dynamic_Menu_Item --
   --------------------------

   function Is_Dynamic_Menu_Item
     (W : access Gtk_Widget_Record'Class) return Boolean is
   begin
      return W'Tag = Dynamic_Menu_Item_Record'Tag;
   end Is_Dynamic_Menu_Item;

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

   ---------------------
   -- Compile_Command --
   ---------------------

   procedure Compile_Command
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      C      : Xref_Commands.Generic_Asynchronous_Command_Access;
   begin
      if Command = "compute_xref" then
         Xref_Commands.Create
           (C, -"Computing C/C++ xref info",
            new Compute_Xref_Data'(Kernel, new LI_Handler_Iterator_Access, 0),
            Xref_Iterate'Access);

         Launch_Synchronous (Command_Access (C), 0.01);
         Unref (Command_Access (C));

      elsif Command = "compute_xref_bg" then
         Xref_Commands.Create
           (C, -"Computing C/C++ xref info",
            new Compute_Xref_Data'(Kernel, new LI_Handler_Iterator_Access, 0),
            Xref_Iterate'Access);

         Launch_Background_Command
           (Kernel, Command_Access (C), True, True, "");
      end if;
   end Compile_Command;

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
      Build_Command_Manager.Launch_Target
        (Kernel,
         Builder_Facility_Module.Registry,
         "Build All", "xref",
         GNATCOLL.VFS.No_File,
         Extra_Args  => null,
         Quiet       => True,
         Synchronous => False,
         Dialog      => Build_Command_Manager.Force_No_Dialog,
         Main        => "");
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
      pragma Unreferenced (Data);
   begin
      Interrupt_Xrefs_Loading (Kernel);
      Builder_Module_ID.Build_Count := Builder_Module_ID.Build_Count + 1;

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
         if Automatic_Xrefs_Load.Get_Pref then
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

   ----------
   -- Free --
   ----------

   procedure Free (Run : in out Run_Description) is
   begin
      Free (Run.Command);
      Free (Run.Arguments);
      Free (Run.Directory);
      Free (Run.Title);
   end Free;

   ------------
   -- Launch --
   ------------

   procedure Launch
     (Kernel : access Kernel_Handle_Record'Class;
      Run : Run_Description)
   is
      Console : Interactive_Console;
      Child   : MDI_Child;
      Success : Boolean;
   begin
      if Run.Command = null then
         return;
      end if;

      Console := Create_Interactive_Console (Kernel, Run.Title.all);
      Clear (Console);
      Child := Find_MDI_Child (Get_MDI (Kernel), Console);

      if Child /= null then
         Raise_Child (Child);
      end if;

      --  Save command for the future

      if Builder_Module_ID.Last_Run_Cmd /= Run then
         Free (Builder_Module_ID.Last_Run_Cmd);
         Builder_Module_ID.Last_Run_Cmd := Run;
      end if;

      --  Spawn

      Launch_Process
        (Kernel_Handle (Kernel),
         Command          => Run.Command.all,
         Arguments        => Run.Arguments.all,
         Server           => Execution_Server,
         Console          => Console,
         Success          => Success,
         Directory        => Run.Directory.all,
         Use_Ext_Terminal => Run.Ext_Terminal,
         Show_Exit_Status => True);

      if not Success then
         Trace (Me, "Problem spawning command");
      end if;
   end Launch;

   -----------------
   -- Set_Command --
   -----------------

   procedure Set_Command
     (Run          : in out Run_Description;
      Ext_Terminal : Boolean;
      Command      : String)
   is
      Local_Args : Argument_List_Access;
   begin
      Run.Ext_Terminal := Ext_Terminal;

      if not Ext_Terminal
        and then Shell_Env /= ""
        and then Is_Local (Build_Server)
      then
         --  Launch "$SHELL -c cmd" if $SHELL is set and the build server
         --  is local.

         Run.Command := new String'(Shell_Env);
         Run.Arguments := new Argument_List'
           (new String'("-c"),
            new String'(Command));
      else
         Local_Args := Argument_String_To_List (Command);
         Run.Command := Local_Args (Local_Args'First);
         Run.Arguments := new Argument_List'
           (Local_Args (Local_Args'First + 1 .. Local_Args'Last));
         Basic_Types.Unchecked_Free (Local_Args);  --  Not the actual strings
      end if;
   end Set_Command;

   --------------------------
   -- On_Run_Last_Launched --
   --------------------------

   procedure On_Run_Last_Launched
     (Menu   : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Menu);
   begin
      Launch (Kernel, Builder_Module_ID.Last_Run_Cmd);
   end On_Run_Last_Launched;

   ------------
   -- On_Run --
   ------------

   procedure On_Run
     (Kernel : access GObject_Record'Class; Data : File_Project_Record)
   is
      K            : constant Kernel_Handle := Kernel_Handle (Kernel);
      Active       : aliased Boolean := False;
      Use_Exec_Dir : aliased Boolean := False;
      Run          : Run_Description;
   begin
      if Data.File = GNATCOLL.VFS.No_File then
         declare
            Command : constant String := Display_Entry_Dialog
              (Parent         => Get_Current_Window (K),
               Title          => -"Run Command",
               Message        => -"Enter the command to run:",
               Check_Msg      => -"Use external terminal",
               Key            => Cst_Run_Arguments_History,
               History        => Get_History (K),
               Button_Active  => Active'Unchecked_Access,
               Key_Check      => Run_External_Key);

         begin
            if Command /= ""
              and then Command (Command'First) /= ASCII.NUL
            then
               Set_Command (Run, Active, Command);
               Run.Directory    := new String'("");

               if Is_Local (Execution_Server) then
                  Run.Title := new String'(-"Run: " & Command);
               else
                  Run.Title := new String'
                    (-"Run on " & Get_Nickname (Execution_Server) & ": " &
                     Command);
               end if;
            end if;
         end;

      else
         declare
            Arguments : constant String := Display_Entry_Dialog
              (Parent         => Get_Current_Window (K),
               Title          => -"Arguments Selection",
               Message        => -"Enter the arguments to your application:",
               Key            => Cst_Run_Arguments_History,
               History        => Get_History (K),
               Check_Msg      => -"Use external terminal",
               Key_Check      => Run_External_Key,
               Button_Active  => Active'Unchecked_Access,
               Check_Msg2     => -"Use exec dir instead of current dir",
               Key_Check2     => Run_Exec_Dir_Key,
               Button2_Active => Use_Exec_Dir'Unchecked_Access);

         begin
            if Arguments = ""
              or else Arguments (Arguments'First) /= ASCII.NUL
            then
               Run.Command := new String'
                 (To_Remote (Full_Name (Data.File).all, Execution_Server));
               Run.Arguments := Argument_String_To_List (Arguments);
               Run.Ext_Terminal := Active;

               if Use_Exec_Dir then
                  Run.Directory := new String'
                    (Executables_Directory (Data.Project));
               else
                  Run.Directory := new String'("");
               end if;

               if Is_Local (Execution_Server) then
                  Run.Title := new String'
                    (-"Run: " &
                     Base_Name (Data.File) & ' ' & Krunch (Arguments, 12));
               else
                  Run.Title := new String'
                    (-"Run on " & Get_Nickname (Execution_Server) & ": " &
                     Base_Name (Data.File) & ' ' & Krunch (Arguments, 12));
               end if;

            end if;
         end;
      end if;

      Launch (K, Run);
      --  Run must not be freed

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
      Child : constant MDI_Child := Get_Focus_Child (Get_MDI (Kernel));
   begin
      --  Check whether the current MDI child can handle interrupt on its own

      if Child = null
        or else Child.all not in GPS_MDI_Child_Record'Class
        or else not Interrupt (GPS_MDI_Child (Child))
      then
         --  Else default is to kill the last process we started
         Interrupt_Latest_Task (Kernel);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Tools_Interrupt;

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
         if Mains (M).all /= "" then
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
                  User_Data   => File_Project_Record'
                    (Project => Project,
                     File    => Create
                       (Executables_Directory (Project) & Exec)));

               if Set_Shortcut and then M = Mains'First then
                  Set_Accel_Path
                    (Mitem, Run_Menu_Prefix & Item_Accel_Path & Image (M),
                     Group);
               end if;
            end;
         end if;
      end loop;
   end Add_Run_Menu;

   ---------------------
   -- On_View_Changed --
   ---------------------

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class) is
      Mitem : Gtk_Menu_Item;
      Menu2 : Gtk_Menu renames Builder_Module_ID.Run_Menu;
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
         --  with an empty list in which case the extended project main files
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
           (Project => Get_Project (Kernel), File => GNATCOLL.VFS.No_File));

      --  Rerunning the previous command

      Mitem := new Dynamic_Menu_Item_Record;
      Gtk.Menu_Item.Initialize (Mitem, -"Last Launched");
      Append (Menu2, Mitem);
      Set_Accel_Path (Mitem, -(Run_Menu_Prefix) & (-"Last Launched"), Group);
      Kernel_Callback.Connect
        (Mitem, Signal_Activate, On_Run_Last_Launched'Access,
         User_Data => Kernel_Handle (Kernel));

      Show_All (Menu2);

      if Automatic_Xrefs_Load.Get_Pref then
         Load_Xref_In_Memory (Kernel_Handle (Kernel));
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_View_Changed;

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
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
      Build_Menu : constant String := '/' & (-"_Build") & '/';
      Tools : constant String := '/' & (-"Tools") & '/';
      Mitem : Gtk_Menu_Item;
      Menu  : Gtk_Menu;
   begin
      --  This memory is allocated once, and lives as long as the application

      Builder_Module_ID := new Builder_Module_ID_Record;
      Register_Module
        (Module      => Builder_Module_ID,
         Kernel      => Kernel,
         Module_Name => "Builder",
         Priority    => Default_Priority);

      Register_Contextual_Submenu
        (Kernel,
         Name    => "Run",
         Filter  => Lookup_Filter (Kernel, "Project only"),
         Submenu => new Run_Contextual);

      --  Dynamic run menu

      Mitem := Register_Menu
        (Kernel, Build_Menu, -"_Run", Stock_Execute,
         null, Ref_Item => "Settings");
      Gtk_New (Menu);
      Builder_Module_ID_Record (Builder_Module_ID.all).Run_Menu := Menu;
      Set_Submenu (Mitem, Menu);

      declare
         Result : constant String := Add_Customization_String
           (Kernel        => Kernel,
            Customization =>
              "<key action='/Build/Run/item1'>shift-F2</key>",
            From_File     => "builder module");
      begin
         if Result /= "" then
            Insert (Kernel, Result, Mode => Console.Error);
         end if;
      end;

      Gtk_New (Mitem);
      Register_Menu (Kernel, Build_Menu, Mitem, Ref_Item => -"Settings");

      Register_Menu
        (Kernel, Build_Menu, -"Recompute _Xref info", "",
         On_Compute_Xref'Access, Ref_Item => "Settings");
      Register_Menu
        (Kernel, Build_Menu, -"Load Xref info in memory", "",
         On_Load_Xref_In_Memory'Access, Ref_Item => "Settings");

      Gtk_New (Mitem);
      Register_Menu (Kernel, Build_Menu, Mitem, Ref_Item => -"Settings");

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
        (Kernel, "compute_xref",
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
