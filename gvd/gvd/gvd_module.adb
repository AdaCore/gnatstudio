-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.Directory_Operations;      use GNAT.Directory_Operations;
with GNAT.OS_Lib;

with Glib;                           use Glib;
with Glib.Object;                    use Glib.Object;
with Gdk.Color;                      use Gdk.Color;
with Gdk.Pixbuf;                     use Gdk.Pixbuf;
with Gdk.Types;                      use Gdk.Types;
with Gdk.Types.Keysyms;              use Gdk.Types.Keysyms;
with Gdk.Window;                     use Gdk.Window;
with Gtk.Accel_Group;                use Gtk.Accel_Group;
with Gtk.Box;                        use Gtk.Box;
with Gtk.Container;                  use Gtk.Container;
with Gtk.Dialog;                     use Gtk.Dialog;
with Gtk.Enums;                      use Gtk.Enums;
with Gtk.GEntry;                     use Gtk.GEntry;
with Gtk.Handlers;                   use Gtk.Handlers;
with Gtk.Image;                      use Gtk.Image;
with Gtk.Label;                      use Gtk.Label;
with Gtk.Menu;                       use Gtk.Menu;
with Gtk.Menu_Item;                  use Gtk.Menu_Item;
with Gtk.Scrolled_Window;            use Gtk.Scrolled_Window;
with Gtk.Stock;                      use Gtk.Stock;
with Gtk.Table;                      use Gtk.Table;
with Gtk.Toolbar;                    use Gtk.Toolbar;
with Gtk.Widget;                     use Gtk.Widget;
with Gtk.Window;                     use Gtk.Window;
with Gtkada.Canvas;                  use Gtkada.Canvas;
with Gtkada.Dialogs;                 use Gtkada.Dialogs;
with Gtkada.File_Selector;           use Gtkada.File_Selector;
with Gtkada.Handlers;                use Gtkada.Handlers;
with Gtkada.MDI;                     use Gtkada.MDI;

with Basic_Types;                    use Basic_Types;
with Histories;                      use Histories;
with Display_Items;                  use Display_Items;
with Breakpoints_Editor;             use Breakpoints_Editor;
with Debugger;                       use Debugger;
with GVD.Call_Stack;                 use GVD.Call_Stack;
with GVD.Canvas;                     use GVD.Canvas;
with GVD.Code_Editors;               use GVD.Code_Editors;
with GVD.Dialogs;                    use GVD.Dialogs;
with GPS.Main_Window;                use GPS.Main_Window;
with GPS.Main_Window.Debug;          use GPS.Main_Window.Debug;
with GVD.Memory_View;                use GVD.Memory_View;
with GVD.Menu;                       use GVD.Menu;
with GVD.Proc_Utils;                 use GVD.Proc_Utils;
with GVD.Preferences;                use GVD.Preferences;
with GVD.Assembly_View;              use GVD.Assembly_View;
with GVD.Types;                      use GVD.Types;
with GVD.Process;                    use GVD.Process;
with GVD.Source_Editor;              use GVD.Source_Editor;
with GVD.Source_Editor.GPS;          use GVD.Source_Editor.GPS;
with List_Select_Pkg;                use List_Select_Pkg;
with Language;                       use Language;
with Language_Handlers;              use Language_Handlers;
with Process_Proxies;                use Process_Proxies;
with Projects;                       use Projects;
with Std_Dialogs;                    use Std_Dialogs;

with GPS.Main_Window;                use GPS.Main_Window;
with GPS.Kernel;                     use GPS.Kernel;
with GPS.Kernel.Console;             use GPS.Kernel.Console;
with GPS.Kernel.Contexts;            use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;               use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                 use GPS.Kernel.MDI;
with GPS.Kernel.Modules;             use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;         use GPS.Kernel.Preferences;
with GPS.Kernel.Project;             use GPS.Kernel.Project;
with GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks;      use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                       use GPS.Intl;
with Pixmaps_IDE;                    use Pixmaps_IDE;
with Traces;                         use Traces;
with GUI_Utils;                      use GUI_Utils;
with VFS;                            use VFS;
with Projects.Registry;              use Projects.Registry;
with Projects.Editor;                use Projects.Editor;
with Entities;                       use Entities;

with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Strings.Fixed;              use Ada.Strings.Fixed;

with Debugger_Pixmaps;               use Debugger_Pixmaps;

with Commands;                       use Commands;
with Commands.Interactive;           use Commands.Interactive;
with Commands.Debugger;              use Commands.Debugger;

with Interactive_Consoles;           use Interactive_Consoles;

package body GVD_Module is

   Me : constant Debug_Handle := Create ("Debugger");

   Cst_Run_Arguments_History : constant History_Key := "gvd_run_arguments";
   --  The key in the history for the arguments to the run command.
   --  WARNING: this constant is shared with builder_module.adb, since we want
   --  to have the same history for the run command in GPS.

   Debugger_Started : constant String := "debugger_started";
   Debugger_Terminated : constant String := "debugger_terminated";

   Debug_Menu_Prefix : constant String := "<gps>/Debug/Initialize/";

   Max_Tooltip_Width : constant := 400;
   --  Maximum size to use for the tooltip windows

   type GPS_Debugger_Record is new
     GVD.Process.Visual_Debugger_Record with null record;
   type GPS_Debugger is access all GPS_Debugger_Record'Class;

   type Bp_Array is array (Integer range <>) of Breakpoint_Identifier;

   procedure Gtk_New
     (Debugger : out GPS_Debugger;
      Window   : access GPS_Window_Record'Class);

   procedure Set_Busy
     (Debugger      : access GPS_Debugger_Record;
      Busy          : Boolean := True;
      Force_Refresh : Boolean := False);

   type File_Edited_Hook_Record is new Hook_Args_Record with record
      Top : GPS_Window;
   end record;
   type File_Edited_Hook is access File_Edited_Hook_Record'Class;
   procedure Execute
     (Hook   : File_Edited_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_edited" hook.

   type Lines_Revealed_Hook_Record is new Hook_Args_Record with null record;
   type Lines_Revealed_Hook is access Lines_Revealed_Hook_Record'Class;
   procedure Execute
     (Hook   : Lines_Revealed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "source_lines_revealed_hook" hook.

   type GPS_Proxy is new Process_Proxy with record
      Kernel : Kernel_Handle;
   end record;
   --  GPS specific proxy, used to redefine Set_Command_In_Process

   procedure Set_Command_In_Process
     (Proxy      : access GPS_Proxy;
      In_Process : Boolean := True);
   --  Set the appropriate debugger menu items to the corresponding state.

   type GVD_Module_Record is new Module_ID_Record with record
      Initialized                    : Boolean := False;
      --  Whether the debugger is running;

      Show_Lines_With_Code           : Boolean;
      --  Whether the lines with code should be explicitly queried.

      Initialize_Menu                : Gtk_Menu;

      Current_Executable_For_Project : Virtual_File :=
        Create_From_Base ("</\unknown>");
      --  This is the last executable that was used to compute the automatic
      --  project. The default value is to make sure that it won't match an
      --  actual executable name, and thus make sure we will at least
      --  compute the project once.
      --  It might be left to No_File in case we don't know the
      --  executable because the module was already loaded on cross targets.

      Delete_Id                      : Handler_Id := (Null_Signal_Id, null);
      File_Hook                      : File_Edited_Hook;
      Lines_Hook                     : Lines_Revealed_Hook;

      Cont_Button,
      Step_Button,
      Next_Button,
      Finish_Button,
      Up_Button,
      Down_Button                    : Gtk.Widget.Gtk_Widget;

      First_Debugger                 : Debugger_List_Link;
      --  Points to the list of debuggers

      Current_Debugger               : Glib.Object.GObject;
      --  The current visual debugger

      Breakpoints_Editor             : Breakpoint_Editor_Access;
      Thread_Dialog                  : Thread_Dialog_Access;
      Task_Dialog                    : Task_Dialog_Access;
      PD_Dialog                      : PD_Dialog_Access;
   end record;
   type GVD_Module is access all GVD_Module_Record'Class;

   procedure Destroy (Id : in out GVD_Module_Record);
   --  Terminate the debugger module, and kill the underlying debugger.

   function Tooltip_Handler
     (Module  : access GVD_Module_Record;
      Context : access Selection_Context'Class) return Gdk.Gdk_Pixmap;
   --  See inherited documentation

   GVD_Module_Name : constant String := "Debugger";
   GVD_Module_ID   : GVD_Module;

   procedure Add_Debug_Buttons (Kernel : access Kernel_Handle_Record'Class);
   --  Add debugger related buttons to the main toolbar.

   procedure Remove_Debug_Buttons
     (Kernel : access Kernel_Handle_Record'Class);
   --  Remove debugger related buttons from the main toolbar.

   type Debugger_State is (Debug_None, Debug_Busy, Debug_Available);
   --  Possible states of a debugger:
   --  - Debug_None: debugger is not running
   --  - Debug_Busy: debugger is busy processing a command
   --  - Debug_Available: debugger is available

   procedure Set_Sensitive
     (Kernel : Kernel_Handle;
      State  : Debugger_State);
   --  Change the sensitive state of the debugger menu items and toolbar
   --  buttons

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called every time the project view changes, to recompute the dynamic
   --  menus.

   procedure Create_Debugger_Columns
     (Kernel : Kernel_Handle;
      File   : Virtual_File);
   --  Create the side information columns corresponding to the debugger
   --  in the editors for file.
   --  If File is empty, create them for all files.

   procedure Remove_Debugger_Columns
     (Kernel : Kernel_Handle;
      File   : Virtual_File);
   --  Remove the side information columns corresponding to the debugger
   --  in the editors for file.
   --  If File is empty, remove them for all files.

   procedure Preferences_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences are changed in the GPS kernel

   procedure Debugger_Command_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String);
   --  Interactive command handler for the debugger module.

   function Delete_Asm
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;
   --  Callback for the "delete_event" signal of the assembly view.
   --  Widget is a Code_Editor.

   procedure Debug_Init
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : File_Project_Record;
      Args   : String);
   --  Initialize the debugger

   procedure Debug_Terminate (Kernel : Kernel_Handle);
   --  Terminate the debugging session, and closes all debuggers

   procedure Debug_Terminate
     (Kernel   : Kernel_Handle;
      Debugger : access Visual_Debugger_Record'Class);
   --  Close the given debugger and terminate the debugging session if this
   --  is the last one.

   function Get_Variable_Name
     (Context     : Selection_Context_Access;
      Dereference : Boolean) return String;
   --  If Context contains an entity, get the entity name.
   --  Dereference the entity if Dereference is True.
   --  Return "" if entity name could not be found in Context.

   procedure Load_Project_From_Executable
     (Kernel   : access Kernel_Handle_Record'Class;
      Debugger : access Visual_Debugger_Record'Class);
   --  Create and load a default project from the executable loaded in
   --  Debugger.

   procedure On_Debug_Terminate_Single (Widget : access GObject_Record'Class);
   --  Callback for the "debugger_closed" singla.

   procedure On_Assembly
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Display the assembly view.
   --  Used e.g. for implementing menu Debug->Data->Assembly
   --  Widget parameter is ignored.

   --------------------
   -- Menu Callbacks --
   --------------------

   procedure On_Debug_Init
     (Kernel : access GObject_Record'Class; Data : File_Project_Record);
   --  Debug->Initialize

   procedure On_Connect_To_Board
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Connect to Board

   procedure On_Debug_Executable
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Load File

   procedure On_Add_Symbols
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Add Symbols

   procedure On_Load_Core
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Debug Core File

   procedure On_Attach
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Attach

   procedure On_Detach
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Detach

   procedure On_Kill
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Kill

   procedure On_Call_Stack
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Call Stack

   procedure On_Threads
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Threads

   procedure On_Tasks
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Tasks

   procedure On_PD
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Protection Domains

   procedure On_Edit_Breakpoints
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Edit Breakpoints

   procedure On_Examine_Memory
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Examine Memory

   procedure On_Display_Locals
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Display Local Variables

   procedure On_Display_Args
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Display Arguments

   procedure On_Display_Regs
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Display Registers

   procedure On_Display_Expression
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Display Any Expression

   procedure On_Data_Refresh
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Refresh

   procedure On_Start
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Start menu

   procedure On_Step
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Step menu

   procedure On_Step_Instruction
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Step Instruction menu

   procedure On_Next
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Next menu

   procedure On_Next_Instruction
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Next Instruction menu

   procedure On_Finish
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Finish menu

   procedure On_Continue
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Continue menu

   procedure On_Interrupt
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Interrupt

   procedure On_Debug_Terminate_Current
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Terminate Current

   procedure On_Debug_Terminate
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Terminate

   -----------------------
   -- Toolbar Callbacks --
   -----------------------

   procedure On_Start_Continue (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "start/continue" button

   procedure On_Step (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "step" button

   procedure On_Next (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "next" button

   procedure On_Finish (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "finish" button

   procedure On_Up (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "up" button

   procedure On_Down (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "down" button

   --------------------
   -- Misc Callbacks --
   --------------------

   procedure On_Executable_Changed (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "executable_changed" signal on the debugger process.

   --------------------
   -- GVD_Contextual --
   --------------------

   Is_Printable_Entity : constant array (E_Kinds) of Boolean :=
     (Overloaded_Entity     => True,
      Unresolved_Entity     => True,
      Access_Kind           => True,
      Array_Kind            => True,
      Boolean_Kind          => True,
      Class_Wide            => True,
      Class                 => True,
      Decimal_Fixed_Point   => True,
      Enumeration_Literal   => True,
      Enumeration_Kind      => True,
      Exception_Entity      => True,
      Floating_Point        => True,
      Modular_Integer       => True,
      Named_Number          => True,
      Ordinary_Fixed_Point  => True,
      Record_Kind           => True,
      Signed_Integer        => True,
      String_Kind           => True,
      others                => False);
   --  Set of printable entities

   Is_Access_Entity : constant array (E_Kinds) of Boolean :=
     (Overloaded_Entity => True,
      Unresolved_Entity => True,
      Access_Kind       => True,
      Array_Kind        => True,
      String_Kind       => True,
      others            => False);
   --  Set of potentially dereferenceable entities

   ----------------
   -- Contextual --
   ----------------

   type Debugger_Active_Filter is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Debugger_Active_Filter;
      Context : access Selection_Context'Class) return Boolean;

   type Printable_Variable_Filter is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Printable_Variable_Filter;
      Context : access Selection_Context'Class) return Boolean;

   type Access_Variable_Filter is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Access_Variable_Filter;
      Context : access Selection_Context'Class) return Boolean;

   type Subprogram_Variable_Filter
     is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Subprogram_Variable_Filter;
      Context : access Selection_Context'Class) return Boolean;

   type Print_Variable_Command is new Interactive_Command with record
      Display     : Boolean := False;
      Dereference : Boolean := False;
   end record;
   function Execute
     (Command : access Print_Variable_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Set_Value_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Set_Value_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type View_Memory_Command is new Interactive_Command with null record;
   function Execute
     (Command : access View_Memory_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Show_Location_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Show_Location_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Set_Breakpoint_Command is new Interactive_Command with record
      On_Line       : Boolean := False;  --  If False, on entity
      Continue_Till : Boolean := False;  --  Continue until given line ?
   end record;
   function Execute
     (Command : access Set_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   function Custom_Label_Expansion
     (Context : access Selection_Context'Class) return String;
   --  Provide expansion for "$!" in the labels for contextual menus

   ----------------------------------
   -- Load_Project_From_Executable --
   ----------------------------------

   procedure Load_Project_From_Executable
     (Kernel   : access Kernel_Handle_Record'Class;
      Debugger : access Visual_Debugger_Record'Class)
   is
      Project : Project_Type := Get_Project (Kernel);
      Exec    : Virtual_File;

   begin
      --  Do nothing unless the current project was already generated from an
      --  executable

      if Status (Project) /= From_Executable then
         return;
      end if;

      Exec := Get_Executable (Debugger.Debugger);

      if Exec = GVD_Module_ID.Current_Executable_For_Project then
         return;
      end if;

      GVD_Module_ID.Current_Executable_For_Project := Exec;

      --  No handling of desktop is done here, we want to leave all windows
      --  as-is.

      declare
         Debugger_Name : constant String :=
           (Get_Attribute_Value
             (Project, Debugger_Command_Attribute, Default => ""));
         Target        : constant String :=
           (Get_Attribute_Value
             (Project, Program_Host_Attribute, Default => ""));
         Protocol      : constant String :=
           (Get_Attribute_Value
             (Project, Protocol_Attribute, Default => ""));

      begin
         Unload_Project (Project_Registry (Get_Registry (Kernel).all));

         if Exec /= VFS.No_File then
            Project := Create_Project
              (Project_Registry (Get_Registry (Kernel).all),
               "debugger_" & Base_Name (Exec),
               GNAT.Directory_Operations.Get_Current_Dir);
         else
            Project := Create_Project
              (Project_Registry (Get_Registry (Kernel).all),
               "debugger_no_file",
               GNAT.Directory_Operations.Get_Current_Dir);
         end if;

         if Debugger_Name /= "" then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Debugger_Command_Attribute,
               Value              => Debugger_Name);
         end if;

         if Target /= "" then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Program_Host_Attribute,
               Value              => Target);
         end if;

         if Protocol /= "" then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Protocol_Attribute,
               Value              => Protocol);
         end if;
      end;

      declare
         List       : String_Array := Source_Files_List (Debugger.Debugger);
         Bases      : GNAT.OS_Lib.Argument_List (List'Range);
         Dirs       : GNAT.OS_Lib.Argument_List (List'Range);
         Dirs_Index : Natural := Dirs'First;
         Main       : GNAT.OS_Lib.Argument_List (1 .. 1);
         Langs      : GNAT.OS_Lib.Argument_List (List'Range);
         Lang_Index : Natural := Langs'First;

      begin
         for L in List'Range loop
            Bases (L) := new String'(Base_Name (List (L).all));
         end loop;

         Update_Attribute_Value_In_Scenario
           (Project,
            Scenario_Variables => No_Scenario,
            Attribute          => Source_Files_Attribute,
            Values             => Bases);
         Free (Bases);

         for L in List'Range loop
            declare
               Dir   : constant String := GNAT.OS_Lib.Normalize_Pathname
                 (Dir_Name (List (L).all),
                  Dir_Name (Exec).all,
                  Resolve_Links => False);
               Found : Boolean := False;
            begin
               for D in Dirs'First .. Dirs_Index - 1 loop
                  if Dirs (D).all = Dir then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Dirs (Dirs_Index) := new String'(Dir);
                  Dirs_Index := Dirs_Index + 1;
               end if;
            end;
         end loop;

         Update_Attribute_Value_In_Scenario
           (Project,
            Scenario_Variables => No_Scenario,
            Attribute          => Source_Dirs_Attribute,
            Values             => Dirs (Dirs'First .. Dirs_Index - 1));
         Free (Dirs);

         for L in List'Range loop
            declare
               Lang : constant String := Get_Language_From_File
                 (Get_Language_Handler (Kernel),
                  Create (Full_Filename => List (L).all));
               Found : Boolean := False;
            begin
               if Lang /= "" then
                  for La in Langs'First .. Lang_Index - 1 loop
                     if Langs (La).all = Lang then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Langs (Lang_Index) := new String'(Lang);
                     Lang_Index := Lang_Index + 1;
                  end if;
               end if;
            end;
         end loop;

         Update_Attribute_Value_In_Scenario
           (Project,
            Scenario_Variables => No_Scenario,
            Attribute          => Languages_Attribute,
            Values             => Langs (Langs'First .. Lang_Index - 1));
         Free (Langs);

         if Exec /= VFS.No_File then
            Update_Attribute_Value_In_Scenario
              (Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Obj_Dir_Attribute,
               Value              => Dir_Name (Exec).all);
            Update_Attribute_Value_In_Scenario
              (Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Exec_Dir_Attribute,
               Value              => Dir_Name (Exec).all);

            Main (Main'First) := new String'(Full_Name (Exec).all);
            Update_Attribute_Value_In_Scenario
              (Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Main_Attribute,
               Values             => Main);
            Free (Main);
         end if;
         Free (List);
      end;

      --  Is the information for this executable already cached ? If yes,
      --  we simply reuse it to avoid the need to interact with the debugger.

      Load_Custom_Project
        (Project_Registry (Get_Registry (Kernel).all), Project);
      Set_Status (Get_Project (Kernel), From_Executable);
      Run_Hook (Kernel, Project_Changed_Hook);
      Recompute_View (Kernel);
   end Load_Project_From_Executable;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Debugger : out GPS_Debugger;
      Window   : access GPS_Window_Record'Class)
   is
      Edit : GVD.Source_Editor.GPS.GEdit;
   begin
      Debugger := new GPS_Debugger_Record;

      GVD.Source_Editor.GPS.Gtk_New (Edit, Window);
      GVD.Process.Initialize (Debugger, Window, Source_Editor (Edit));
   end Gtk_New;

   --------------
   -- Set_Busy --
   --------------

   procedure Set_Busy
     (Debugger      : access GPS_Debugger_Record;
      Busy          : Boolean := True;
      Force_Refresh : Boolean := False)
   is
      pragma Unreferenced (Force_Refresh);
   begin
      if Busy then
         Push_State (GPS_Window (Debugger.Window).Kernel, Processing);
      else
         Pop_State (GPS_Window (Debugger.Window).Kernel);
      end if;
   end Set_Busy;

   ----------------------------
   -- Get_Breakpoints_Editor --
   ----------------------------

   function Get_Breakpoints_Editor
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Gtk.Window.Gtk_Window
   is
      pragma Unreferenced (Kernel);
   begin
      return Gtk_Window (GVD_Module_ID.Breakpoints_Editor);
   end Get_Breakpoints_Editor;

   -----------------------
   -- Get_Thread_Dialog --
   -----------------------

   function Get_Thread_Dialog
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Gtk.Dialog.Gtk_Dialog
   is
      pragma Unreferenced (Kernel);
   begin
      return Gtk_Dialog (GVD_Module_ID.Thread_Dialog);
   end Get_Thread_Dialog;

   ---------------------
   -- Get_Task_Dialog --
   ---------------------

   function Get_Task_Dialog
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Gtk.Dialog.Gtk_Dialog
   is
      pragma Unreferenced (Kernel);
   begin
      return Gtk_Dialog (GVD_Module_ID.Task_Dialog);
   end Get_Task_Dialog;

   -------------------
   -- Get_PD_Dialog --
   -------------------

   function Get_PD_Dialog
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Gtk.Dialog.Gtk_Dialog
   is
      pragma Unreferenced (Kernel);
   begin
      return Gtk_Dialog (GVD_Module_ID.PD_Dialog);
   end Get_PD_Dialog;

   -----------------------
   -- Get_Debugger_List --
   -----------------------

   function Get_Debugger_List
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Debugger_List_Link
   is
      pragma Unreferenced (Kernel);
   begin
      return GVD_Module_ID.First_Debugger;
   end Get_Debugger_List;

   --------------------------
   -- Get_Current_Debugger --
   --------------------------

   function Get_Current_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Glib.Object.GObject
   is
      pragma Unreferenced (Kernel);
   begin
      return GVD_Module_ID.Current_Debugger;
   end Get_Current_Debugger;

   ------------------------
   -- Set_First_Debugger --
   ------------------------

   procedure Set_First_Debugger
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : Debugger_List_Link)
   is
      pragma Unreferenced (Kernel);
   begin
      GVD_Module_ID.First_Debugger := Debugger;
   end Set_First_Debugger;

   --------------------------
   -- Set_Current_Debugger --
   --------------------------

   procedure Set_Current_Debugger
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Current : Glib.Object.GObject)
   is
      pragma Unreferenced (Kernel);
   begin
      GVD_Module_ID.Current_Debugger := Current;
   end Set_Current_Debugger;

   -----------------------
   -- Add_Debug_Buttons --
   -----------------------

   procedure Add_Debug_Buttons (Kernel : access Kernel_Handle_Record'Class) is
      Toolbar  : constant Gtk_Toolbar  := Get_Toolbar (Kernel);
      Window   : constant Gtk_Window := Get_Main_Window (Kernel);
      Image    : Gtk_Image;

   begin
      if GVD_Module_ID.Cont_Button /= null then
         return;
      end if;

      Gtk_New (Image, Gdk_New_From_Xpm_Data (run_xpm));
      GVD_Module_ID.Cont_Button := Append_Element
        (Toolbar      => Toolbar,
         The_Type     => Toolbar_Child_Button,
         Text         => -"Go",
         Tooltip_Text => -"Start/Continue the debugged program",
         Icon         => Gtk_Widget (Image));
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Cont_Button, "clicked",
         On_Start_Continue'Access, Window);

      Gtk_New (Image, Gdk_New_From_Xpm_Data (step_xpm));
      GVD_Module_ID.Step_Button := Append_Element
        (Toolbar      => Toolbar,
         The_Type     => Toolbar_Child_Button,
         Text         => -"Step",
         Tooltip_Text => -"Step",
         Icon         => Gtk_Widget (Image));
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Step_Button, "clicked", On_Step'Access, Window);

      Gtk_New (Image, Gdk_New_From_Xpm_Data (next_xpm));
      GVD_Module_ID.Next_Button := Append_Element
        (Toolbar      => Toolbar,
         The_Type     => Toolbar_Child_Button,
         Text         => -"Next",
         Tooltip_Text => -"Next",
         Icon         => Gtk_Widget (Image));
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Next_Button, "clicked", On_Next'Access, Window);

      Gtk_New (Image, Gdk_New_From_Xpm_Data (finish_xpm));
      GVD_Module_ID.Finish_Button := Append_Element
        (Toolbar      => Toolbar,
         The_Type     => Toolbar_Child_Button,
         Text         => -"Finish",
         Tooltip_Text => -"Execute until selected stack frame returns",
         Icon         => Gtk_Widget (Image));
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Finish_Button, "clicked", On_Finish'Access, Window);

      Gtk_New (Image, Gdk_New_From_Xpm_Data (up_xpm));
      GVD_Module_ID.Up_Button := Append_Element
        (Toolbar      => Toolbar,
         The_Type     => Toolbar_Child_Button,
         Text         => -"Up",
         Tooltip_Text =>
         -"Select and print stack frame that called this one",
         Icon         => Gtk_Widget (Image));
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Up_Button, "clicked", On_Up'Access, Window);

      Gtk_New (Image, Gdk_New_From_Xpm_Data (down_xpm));
      GVD_Module_ID.Down_Button := Append_Element
        (Toolbar      => Toolbar,
         The_Type     => Toolbar_Child_Button,
         Text         => -"Down",
         Tooltip_Text => -"Select and print stack frame called by this one",
         Icon         => Gtk_Widget (Image));
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Down_Button, "clicked", On_Down'Access, Window);
   end Add_Debug_Buttons;

   --------------------------
   -- Remove_Debug_Buttons --
   --------------------------

   procedure Remove_Debug_Buttons
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Toolbar : constant Gtk_Toolbar := Get_Toolbar (Kernel);

   begin
      if GVD_Module_ID.Cont_Button /= null then
         Remove (Toolbar, GVD_Module_ID.Cont_Button);
         Remove (Toolbar, GVD_Module_ID.Step_Button);
         Remove (Toolbar, GVD_Module_ID.Next_Button);
         Remove (Toolbar, GVD_Module_ID.Finish_Button);
         Remove (Toolbar, GVD_Module_ID.Up_Button);
         Remove (Toolbar, GVD_Module_ID.Down_Button);
         GVD_Module_ID.Cont_Button := null;
      end if;
   end Remove_Debug_Buttons;

   -------------------------
   -- Initialize_Debugger --
   -------------------------

   procedure Initialize_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Args   : String) is
   begin
      Debug_Init
        (Kernel,
         File_Project_Record'(Get_Project (Kernel), VFS.No_File), Args);
   end Initialize_Debugger;

   -----------------------------
   -- Create_Debugger_Columns --
   -----------------------------

   procedure Create_Debugger_Columns
     (Kernel : Kernel_Handle;
      File   : Virtual_File) is
   begin
      --  Create the information column for the current line.
      Create_Line_Information_Column
        (Kernel,
         File,
         "Current Line",
         --  ??? That should be centralized somewhere !!!
         Every_Line    => False);

      --  Create the information column for the breakpoints
      Create_Line_Information_Column
        (Kernel,
         File,
         Breakpoints_Column_Id,
         Every_Line    => True);
   end Create_Debugger_Columns;

   -----------------------------
   -- Remove_Debugger_Columns --
   -----------------------------

   procedure Remove_Debugger_Columns
     (Kernel : Kernel_Handle;
      File   : Virtual_File) is
   begin
      Remove_Line_Information_Column (Kernel, File, "Current Line");
      Remove_Line_Information_Column
        (Kernel, File, Breakpoints_Column_Id);
   end Remove_Debugger_Columns;

   ----------------------------
   -- Set_Command_In_Process --
   ----------------------------

   procedure Set_Command_In_Process
     (Proxy      : access GPS_Proxy;
      In_Process : Boolean := True) is
   begin
      Set_Command_In_Process (Process_Proxy (Proxy.all)'Access, In_Process);

      if In_Process then
         Set_Sensitive (Proxy.Kernel, Debug_Busy);
      else
         Set_Sensitive (Proxy.Kernel, Debug_Available);
      end if;
   end Set_Command_In_Process;

   --------------------
   -- On_Add_Symbols --
   --------------------

   procedure On_Add_Symbols
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger :=
        Get_Current_Process (Top);
      use Debugger;

   begin
      declare
         S : constant Virtual_File :=
           Select_File
             (Title             => -"Select Module",
              Parent            => Gtk_Window (Top),
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Open_File,
              History           => Get_History (Kernel));
      begin
         if S = VFS.No_File then
            return;
         end if;

         if Process.Descriptor.Remote_Host /= null
           or else Is_Regular_File (S)
         then
            Add_Symbols
              (Process.Debugger, Full_Name (S).all,
               Mode => GVD.Types.Visible);
         else
            Console.Insert
              (Kernel, (-"Could not find file: ") & Full_Name (S).all,
               Mode => Error);
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Add_Symbols;

   ---------------
   -- On_Attach --
   ---------------

   procedure On_Attach
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top           : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process       : constant Visual_Debugger := Get_Current_Process (Top);
      Process_List  : List_Select_Access;
      Success       : Boolean;
      Info          : Process_Info;
      Button        : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Button := Message_Dialog
           ((-"Cannot attach to a task/process while the") & ASCII.LF &
            (-"underlying debugger is busy.") & ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
         return;
      end if;

      Gtk_New (Process_List, Title => -"Process Selection");

      Open_Processes (Process.Debugger);

      loop
         Next_Process (Process.Debugger, Info, Success);

         exit when not Success;

         Add_Item (Process_List, Info.Id, Info.Info);
      end loop;

      Close_Processes (Process.Debugger);

      declare
         Argument : constant String := Show (Process_List);
      begin
         if Argument /= "" then
            Attach_Process
              (Process.Debugger, Argument, Mode => GVD.Types.Visible);
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Attach;

   ---------------
   -- On_Detach --
   ---------------

   procedure On_Detach
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Button := Message_Dialog
           ((-"Cannot detach the task/process while the") & ASCII.LF &
            (-"underlying debugger is busy.") & ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);

      else
         Detach_Process (Process.Debugger, Mode => GVD.Types.Visible);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Detach;

   -------------
   -- On_Step --
   -------------

   procedure On_Step
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Step_Into (Process.Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Step;

   -------------------------
   -- On_Step_Instruction --
   -------------------------

   procedure On_Step_Instruction
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Step_Into_Instruction (Process.Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Step_Instruction;

   -------------
   -- On_Next --
   -------------

   procedure On_Next
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Step_Over (Process.Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Next;

   -------------------------
   -- On_Next_Instruction --
   -------------------------

   procedure On_Next_Instruction
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Step_Over_Instruction (Process.Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Next_Instruction;

   ---------------
   -- On_Finish --
   ---------------

   procedure On_Finish
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Finish (Process.Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Finish;

   -----------------
   -- On_Continue --
   -----------------

   procedure On_Continue
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Continue (Process.Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Continue;

   -------------
   -- On_Kill --
   -------------

   procedure On_Kill
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Kill_Process (Process.Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Kill;

   ------------------
   -- On_Interrupt --
   ------------------

   procedure On_Interrupt
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      --  Give some visual feedback to the user

      Output_Text (Process, "<^C>" & ASCII.LF, Is_Command => True);
      Unregister_Dialog (Process);

      --  Need to flush the queue of commands
      Clear_Queue (Process.Debugger);

      Interrupt (Process.Debugger);

      if not Command_In_Process (Get_Process (Process.Debugger)) then
         Display_Prompt (Process.Debugger);
      end if;

      Set_Busy (Process, False);

      --  We used to flush the output here, so that if the program was
      --  outputting a lot of things, we just stop there.
      --  However, this is not doable, since it in fact also flushes the
      --  prompt that the debugger prints after interruption. Calling
      --  Display_Prompt is also not acceptable, since we might be busy
      --  processing another command.

      --  Note that doing anything at this point is very unsafe, since we got
      --  called while handling a command, and this command has not been fully
      --  handled yet, so we cannot reliably send new commands to the debugger
      --  without creating a synchronization problem. Also, we should be able
      --  to clean up properly the current command, which is particularly
      --  tricky when handling an internal command.

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Interrupt;

   -------------------
   -- On_Call_Stack --
   -------------------

   procedure On_Call_Stack
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : Visual_Debugger;
      Child   : MDI_Child;
      Button  : Message_Dialog_Buttons;
      List    : Debugger_List_Link := Get_Debugger_List (Kernel);
      pragma Unreferenced (Button);

   begin
      while List /= null loop
         Process := Visual_Debugger (List.Debugger);

         if Process.Debugger /= null then
            if Process.Stack = null then
               Create_Call_Stack (Process);

               if Command_In_Process (Get_Process (Process.Debugger)) then
                  Button := Message_Dialog
                    ((-"Cannot show call stack while the debugger is busy.") &
                     ASCII.LF &
                     (-"Interrupt the debugger or wait for its availability."),
                     Dialog_Type => Warning,
                     Buttons => Button_OK);

               else
                  Update_Call_Stack (Process);
               end if;

            else
               Child := Find_MDI_Child (Top.MDI, Process.Stack);

               if Child /= null then
                  Raise_Child (Child);
               else
                  --  Something really bad happened: the stack window is not
                  --  part of the MDI, reset it.

                  Destroy (Process.Stack);
                  Process.Stack := null;
                  Create_Call_Stack (Process);
               end if;
            end if;
         end if;

         List := List.Next;
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Call_Stack;

   ----------------
   -- On_Threads --
   ----------------

   procedure On_Threads
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Button := Message_Dialog
           ((-"Cannot display threads list while the debugger is busy.") &
            ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
         return;
      end if;

      if GVD_Module_ID.Thread_Dialog = null then
         Gtk_New (GVD_Module_ID.Thread_Dialog, Gtk_Window (Top));
      end if;

      Show_All (GVD_Module_ID.Thread_Dialog);
      Gdk_Raise (Get_Window (GVD_Module_ID.Thread_Dialog));
      Update (GVD_Module_ID.Thread_Dialog, Process);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Threads;

   --------------
   -- On_Tasks --
   --------------

   procedure On_Tasks
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Button := Message_Dialog
           ((-"Cannot display tasks list while the debugger is busy.") &
            ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
         return;
      end if;

      if GVD_Module_ID.Task_Dialog = null then
         Gtk_New (GVD_Module_ID.Task_Dialog, Gtk_Window (Top));
      end if;

      Show_All (GVD_Module_ID.Task_Dialog);
      Gdk_Raise (Get_Window (GVD_Module_ID.Task_Dialog));
      Update (GVD_Module_ID.Task_Dialog, Process);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Tasks;

   -----------
   -- On_PD --
   -----------

   procedure On_PD
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Button := Message_Dialog
           ((-"Cannot display protection domain list while the " &
             "debugger is busy.") &
            ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
         return;
      end if;

      if GVD_Module_ID.PD_Dialog = null then
         Gtk_New (GVD_Module_ID.PD_Dialog, Gtk_Window (Top));
      end if;

      Show_All (GVD_Module_ID.PD_Dialog);
      Gdk_Raise (Get_Window (GVD_Module_ID.PD_Dialog));
      Update (GVD_Module_ID.PD_Dialog, Process);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_PD;

   -------------------------
   -- On_Edit_Breakpoints --
   -------------------------

   procedure On_Edit_Breakpoints
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);
      Button  : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Button := Message_Dialog
           ((-"Cannot edit breakpoints while the debugger is busy.") &
            ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
         return;
      end if;

      Breakpoint_Editor (GVD_Module_ID.Breakpoints_Editor, Process);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Edit_Breakpoints;

   -----------------------
   -- On_Examine_Memory --
   -----------------------

   procedure On_Examine_Memory
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top         : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process     : constant Visual_Debugger := Get_Current_Process (Top);
      Memory_View : GVD_Memory_View;

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Gtk_New (Memory_View, Gtk_Widget (Top));
      Show_All (Memory_View);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Examine_Memory;

   -----------------------
   -- On_Display_Locals --
   -----------------------

   procedure On_Display_Locals
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Process_User_Command
        (Process,
         "graph display `" & Info_Locals (Process.Debugger) & '`',
         Output_Command => True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Display_Locals;

   ---------------------
   -- On_Display_Args --
   ---------------------

   procedure On_Display_Args
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Process_User_Command
        (Process,
         "graph display `" & Info_Args (Process.Debugger) & '`',
         Output_Command => True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Display_Args;

   ---------------------
   -- On_Display_Regs --
   ---------------------

   procedure On_Display_Regs
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Process_User_Command
        (Process,
         "graph display `" & Info_Registers (Process.Debugger) & '`',
         Output_Command => True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Display_Regs;

   ---------------------------
   -- On_Display_Expression --
   ---------------------------

   procedure On_Display_Expression
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Display_Expression (Process);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Display_Expression;

   ---------------------
   -- On_Data_Refresh --
   ---------------------

   procedure On_Data_Refresh
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);
      Iter    : Item_Iterator;
      Item    : Canvas_Item;

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Iter := Start (Process.Data_Canvas);
      loop
         Item := Get (Iter);
         exit when Item = null;

         Display_Items.Update
           (GVD_Canvas (Process.Data_Canvas),
            Display_Item (Item),
            Redisplay_Canvas => False);

         Next (Iter);
      end loop;

      Refresh_Canvas (Process.Data_Canvas);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Data_Refresh;

   --------------
   -- On_Start --
   --------------

   procedure On_Start
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top         : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process     : constant Visual_Debugger := Get_Current_Process (Top);
      Button      : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

      Button2     : Boolean_Access := null;
      Multitasks  : aliased Boolean := False;
      Multi_Msg   : aliased String := -"Enable VxWorks multi-tasks mode";
      No_Msg      : aliased String := "";
      Msg         : Basic_Types.String_Access := No_Msg'Unchecked_Access;
      WTX_Version : Natural;

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Button := Message_Dialog
           ((-"Cannot rerun while the underlying debugger is busy.") &
            ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
         return;
      end if;

      --  If we are debugging against VxWorks enable the multi-tasks
      --  mode checkbox

      Info_WTX (Process.Debugger, WTX_Version);

      if WTX_Version = 2 then
         Button2 := Multitasks'Unchecked_Access;
         Msg := Multi_Msg'Unchecked_Access;
      end if;

      declare
         Is_Start  : aliased Boolean;
         Arguments : constant String := Display_Entry_Dialog
           (Parent         => Process.Window,
            Title          => -"Run/Start",
            Message        => -"Run arguments:",
            Key            => Cst_Run_Arguments_History,
            History        => Get_History (GPS_Window (Process.Window).Kernel),
            Check_Msg      => -"Stop at beginning of main subprogram",
            Check_Msg2     => Msg.all,
            Button_Active  => Is_Start'Access,
            Button2_Active => Button2,
            Key_Check      => "stop_beginning_debugger",
            Key_Check2     => "multitask_mode_debugger");
      begin
         if Arguments = ""
           or else Arguments (Arguments'First) /= ASCII.NUL
         then
            if Button2 /= null then
               if Multitasks then
                  Process_User_Command
                    (Process,
                     "set multi-tasks-mode on",
                     Output_Command => False);
               else
                  Process_User_Command
                    (Process,
                     "set multi-tasks-mode off",
                     Output_Command => False);
               end if;
            end if;

            if Is_Start then
               Start (Process.Debugger, Arguments, Mode => GVD.Types.Visible);
            else
               Run (Process.Debugger, Arguments, Mode => GVD.Types.Visible);
            end if;
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Start;

   ----------------
   -- Delete_Asm --
   ----------------

   function Delete_Asm
     (Widget : access Gtk_Widget_Record'Class) return Boolean
   is
      Data_Sub : constant String := '/' & (-"Debug") & '/' & (-"Data") & '/';
      Editor   : constant Code_Editor := Code_Editor (Widget);
      Asm      : constant GVD_Assembly_View  := Get_Asm (Editor);

   begin
      Set_Sensitive
        (Find_Menu_Item (Get_Kernel (GVD_Module_ID.all),
         Data_Sub & (-"Assembly")), True);
      Ref (Asm);
      Remove (Gtk_Container (Get_Parent (Asm)), Asm);
      Set_Mode (Editor, Source);
      Disconnect (Asm, GVD_Module_ID.Delete_Id);

      return False;
   end Delete_Asm;

   -----------------
   -- On_Assembly --
   -----------------

   procedure On_Assembly
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Data_Sub : constant String := '/' & (-"Debug") & '/' & (-"Data") & '/';
      Top      : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process  : constant Visual_Debugger := Get_Current_Process (Top);
      Editor   : constant Code_Editor  := Process.Editor_Text;
      Address  : constant Address_Type := Get_Asm_Address (Editor);
      Assembly : constant GVD_Assembly_View := Get_Asm (Editor);
      Child    : MDI_Child;

   begin
      Trace (Me, "*** [On_Assembly] *** Address = " &
             Address_To_String (Address));
      if Get_Mode (Editor) = Source_Asm then
         return;
      end if;

      Set_Sensitive (Find_Menu_Item (Kernel, Data_Sub & (-"Assembly")), False);

      GVD_Module_ID.Delete_Id :=
        Gtkada.Handlers.Return_Callback.Object_Connect
          (Assembly, "delete_event",
           Gtkada.Handlers.Return_Callback.To_Marshaller (Delete_Asm'Access),
           Editor);

      Set_Mode (Editor, Source_Asm);
      Child := Put (Kernel, Assembly, Module => GVD_Module_ID,
                    Position => Position_Right);
      Unref (Assembly);
      Set_Focus_Child (Child);
      Raise_Child (Child);
      Set_Title (Child, -"Assembly View");

      if not Command_In_Process (Get_Process (Process.Debugger)) then
         Set_Source_Line (Assembly, Get_Line (Editor));

         if Address /= Invalid_Address then
            Set_Address (Assembly, Address);
            Set_Address (Editor, Address);
         end if;

         Update_Display (Assembly);

         if Process.Breakpoints /= null then
            Update_Breakpoints (Assembly, Process.Breakpoints.all);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Assembly;

   -------------------------
   -- On_Connect_To_Board --
   -------------------------

   procedure On_Connect_To_Board
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Top          : constant GPS_Window :=
                       GPS_Window (Get_Main_Window (Kernel));
      Process      : constant Visual_Debugger :=
                       Get_Current_Process (Top);
      Dialog       : Gtk_Dialog;
      Table        : Gtk_Table;
      Ent_Protocol : Gtk_Entry;
      Ent_Target   : Gtk_Entry;
      Label        : Gtk_Label;
      Button       : Gtk_Widget;
      pragma Unreferenced (Widget, Button);

      use Debugger;

   begin
      Gtk_New
        (Dialog,
         Title  => -"Connect to board",
         Parent => Get_Current_Window (Kernel),
         Flags  => Modal or Destroy_With_Parent);
      Set_Position (Dialog, Win_Pos_Mouse);
      Set_Default_Size (Dialog, 300, 100);

      Gtk_New (Table, 2, 2, False);
      Pack_Start (Get_Vbox (Dialog), Table, Expand => False);

      Gtk_New (Label, -"Target name:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 0, 1, Xpadding => 3, Ypadding => 3);

      Gtk_New (Ent_Target);
      Set_Width_Chars (Ent_Target, 20);
      Attach (Table, Ent_Target, 1, 2, 0, 1);
      Grab_Focus (Ent_Target);
      Set_Activates_Default (Ent_Target, True);

      Gtk_New (Label, -"Protocol:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 1, 2, Xpadding => 3, Ypadding => 3);

      Gtk_New (Ent_Protocol);
      Set_Width_Chars (Ent_Protocol, 20);
      Attach (Table, Ent_Protocol, 1, 2, 1, 2);
      Set_Activates_Default (Ent_Protocol, True);

      Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
      Set_Default_Response (Dialog, Gtk_Response_OK);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         Process.Descriptor.Remote_Target :=
           new String'(Get_Text (Ent_Target));
         Process.Descriptor.Protocol :=
           new String'(Get_Text (Ent_Protocol));
         Connect_To_Target
           (Process.Debugger,
            Process.Descriptor.Remote_Target.all,
            Process.Descriptor.Protocol.all,
            GVD.Types.Visible);
      end if;

      Destroy (Dialog);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Connect_To_Board;

   -------------------------
   -- On_Debug_Executable --
   -------------------------

   procedure On_Debug_Executable
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top         : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process     : constant Visual_Debugger :=
        Get_Current_Process (Top);
      Exec        : GNAT.OS_Lib.String_Access;
      Ptr         : GNAT.OS_Lib.String_Access :=
        GNAT.OS_Lib.Get_Executable_Suffix;
      Exec_Suffix : constant String := Ptr.all;

      use Debugger, GNAT.OS_Lib;

   begin
      Free (Ptr);

      declare
         S : Virtual_File :=
           Select_File
             (Title             => -"Select File to Debug",
              File_Pattern      => "*" & Exec_Suffix,
              Pattern_Name      => -"Executable files",
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Open_File,
              History           => Get_History (Kernel));
      begin
         if S = VFS.No_File then
            return;
         end if;

         if not Is_Regular_File (S) then
            Exec := Locate_Exec_On_Path (Base_Name (S));

            if Exec /= null then
               S := Create (Full_Filename => Exec.all);

               if not Is_Regular_File (S) then
                  S := Create (Full_Filename => Exec.all & Exec_Suffix);
               end if;

               Free (Exec);
            else
               Console.Insert
                 (Kernel, (-"Could not find file: ") & Base_Name (S),
                  Mode => Error);
               S := VFS.No_File;
            end if;
         end if;

         if S /= No_File then
            Set_Executable
              (Process.Debugger, Full_Name (S).all, Mode => Hidden);
            Change_Dir (Dir_Name (S).all);
         end if;

      exception
         when Executable_Not_Found =>
            Console.Insert
              (Kernel, (-"Could not find file: ") & Full_Name (S).all,
               Mode => Error);
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Debug_Executable;

   ------------------
   -- On_Load_Core --
   ------------------

   procedure On_Load_Core
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger :=
        Get_Current_Process (Top);
      use Debugger;

   begin
      declare
         S : constant Virtual_File :=
           Select_File
             (Title             => -"Select Core File",
              File_Pattern      => "core*",
              Pattern_Name      => -"Core files",
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Open_File,
              History           => Get_History (Kernel));

      begin
         if S = VFS.No_File then
            return;
         end if;

         if Process.Descriptor.Remote_Host /= null
           or else Is_Regular_File (S)
         then
            Load_Core_File (Process.Debugger, Full_Name (S).all,
                            Mode => GVD.Types.Visible);
         else
            Console.Insert
              (Kernel, (-"Could not find core file: ") & Full_Name (S).all,
               Mode => Error);
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Load_Core;

   ----------------------------
   -- Custom_Label_Expansion --
   ----------------------------

   function Custom_Label_Expansion
     (Context : access Selection_Context'Class) return String is
   begin
      return Get_Variable_Name (Selection_Context_Access (Context), True);
   end Custom_Label_Expansion;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Show_Location_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Top  : constant GPS_Window :=
               GPS_Window (Get_Main_Window (Get_Kernel (Context.Context)));
      Edit : constant GEdit :=
               GEdit
                 (Get_Source
                    (Visual_Debugger (Get_Current_Process (Top)).Editor_Text));
      Name : constant Virtual_File := Get_Current_File (Edit);
   begin
      if Name /= VFS.No_File then
         Highlight_Current_Line (Edit);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Print_Variable_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Process : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context.Context)));
      Debugger : constant Debugger_Access := Process.Debugger;
      Name     : constant String :=
        Get_Variable_Name (Context.Context, Command.Dereference);
   begin
      if Name /= "" then
         if Command.Display then
            Process_User_Command
              (Process, "graph display " & Name, Output_Command => True);
         else
            Send (Debugger, Print_Value_Cmd (Debugger, Name),
                  Mode => GVD.Types.Visible);
         end if;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Set_Value_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Process : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context.Context)));
      Entity  : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context.Context);
      Variable : constant String := Entity_Name_Information (Entity);
      S        : constant String :=
        Simple_Entry_Dialog
          (Parent   => Process.Window,
           Title    => -"Setting value of " & Variable,
           Message  => -"Setting value of " & Variable & ':',
           Position => Win_Pos_Mouse,
           Key      => "gvd_set_value_dialog");
   begin
      if S /= "" and then S (S'First) /= ASCII.NUL then
         Set_Variable (Process.Debugger, Variable, S);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access View_Memory_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Entity      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context.Context);
      Memory_View : GVD_Memory_View;

   begin
      Gtk_New
        (Memory_View, Gtk_Widget (Get_Main_Window (Get_Kernel (Entity))));
      Show_All (Memory_View);
      Display_Memory (Memory_View, Entity_Name_Information (Entity));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Set_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Entity   : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context.Context);
      Debugger : constant Debugger_Access :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Entity))).Debugger;
   begin
      if not Command.On_Line then
         Break_Subprogram
           (Debugger,
            Entity_Name_Information (Entity),
            Mode => GVD.Types.Visible);
      elsif Command.Continue_Till then
         Break_Source
           (Debugger,
            File_Information (Entity),
            Line_Information (Entity),
            Temporary => True);
         Continue (Debugger, Mode => GVD.Types.Visible);
      else
         Break_Source
           (Debugger,
            File_Information (Entity),
            Line_Information (Entity),
            Mode => GVD.Types.Visible);
      end if;

      return Commands.Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Debugger_Active_Filter;
      Context : access Selection_Context'Class) return Boolean
   is
      pragma Unreferenced (Filter);
      Process  : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context)));
   begin
      return Process /= null
        and then Process.Debugger /= null
        and then Context.all in Entity_Selection_Context'Class
        and then not Command_In_Process (Get_Process (Process.Debugger));
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Subprogram_Variable_Filter;
      Context : access Selection_Context'Class) return Boolean
   is
      pragma Unreferenced (Filter);
      Entity : Entity_Information;
   begin
      if Context.all in Entity_Selection_Context'Class then
         Entity := Get_Entity (Entity_Selection_Context_Access (Context));
         return Entity = null or else Is_Subprogram (Entity);
      end if;
      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Printable_Variable_Filter;
      Context : access Selection_Context'Class) return Boolean
   is
      pragma Unreferenced (Filter);
      Entity : Entity_Information;
   begin
      if Context.all in Entity_Selection_Context'Class then
         Entity := Get_Entity (Entity_Selection_Context_Access (Context));
         return Entity = null
           or else (not Get_Kind (Entity).Is_Type
                    and then Is_Printable_Entity (Get_Kind (Entity).Kind));
      end if;
      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Access_Variable_Filter;
      Context : access Selection_Context'Class) return Boolean
   is
      pragma Unreferenced (Filter);
      Entity : Entity_Information;
   begin
      if Context.all in Entity_Selection_Context'Class then
         Entity := Get_Entity (Entity_Selection_Context_Access (Context));
         return Entity = null
           or else (not Get_Kind (Entity).Is_Type
                    and then Is_Access_Entity (Get_Kind (Entity).Kind));
      end if;
      return False;
   end Filter_Matches_Primitive;

   ---------------------
   -- Tooltip_Handler --
   ---------------------

   function Tooltip_Handler
     (Module  : access GVD_Module_Record;
      Context : access Selection_Context'Class) return Gdk.Gdk_Pixmap
   is
      pragma Unreferenced (Module);
      Pixmap    : Gdk.Gdk_Pixmap;
      Selection : Entity_Selection_Context_Access;
      Debugger  : Visual_Debugger;
      Kernel    : Kernel_Handle;
      Value     : Basic_Types.String_Access;

   begin
      if Context.all not in Entity_Selection_Context'Class then
         return null;
      end if;

      Kernel    := Get_Kernel (Context);
      Selection := Entity_Selection_Context_Access (Context);
      Debugger  := Get_Current_Process (Get_Main_Window (Kernel));

      if Debugger = null
        or else Debugger.Debugger = null
        or else not Has_Entity_Name_Information (Selection)
        or else Command_In_Process (Get_Process (Debugger.Debugger))
      then
         return null;
      end if;

      Push_State (Kernel, Busy);

      declare
         Variable_Name : constant String :=
           Entity_Name_Information (Selection);
      begin
         if Variable_Name = ""
           or else not Can_Tooltip_On_Entity
             (Get_Language (Debugger.Debugger), Variable_Name)
         then
            Pop_State (Kernel);
            return null;

         else
            Value := new String'(Value_Of (Debugger.Debugger, Variable_Name));
         end if;

         if Value.all /= "" then
            Create_Pixmap_From_Text
              (Text       => Value.all,
               Font       =>
                 Get_Pref (GPS.Kernel.Preferences.Default_Font),
               Bg_Color   => White (Get_Default_Colormap),
               Widget     => Get_Main_Window (Kernel),
               Pixmap     => Pixmap,
               Wrap_Width => Max_Tooltip_Width);
         end if;
      end;

      Free (Value);
      Pop_State (Kernel);
      return Pixmap;

   exception
      when Language.Unexpected_Type | Constraint_Error =>
         Pop_State (Kernel);
         return null;
      when E : others =>
         Pop_State (Kernel);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return null;
   end Tooltip_Handler;

   -------------------
   -- Set_Sensitive --
   -------------------

   procedure Set_Sensitive
     (Kernel : Kernel_Handle;
      State  : Debugger_State)
   is
      Debug     : constant String := '/' & (-"Debug") & '/';
      Available : constant Boolean := State = Debug_Available;
      Sensitive : constant Boolean := State /= Debug_None;

   begin
      if State = Debug_Available then
         Add_Debug_Buttons (Kernel);
      elsif State = Debug_None then
         Remove_Debug_Buttons (Kernel);
      end if;

      Set_Sensitive
        (Find_Menu_Item (Kernel, Debug & (-"Debug")), Available);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Debug & (-"Data")), Available);

      Set_Sensitive (Find_Menu_Item (Kernel, Debug & (-"Run...")), Available);
      Set_Sensitive (Find_Menu_Item (Kernel, Debug & (-"Step")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Step Instruction")), Sensitive);
      Set_Sensitive (Find_Menu_Item (Kernel, Debug & (-"Next")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Next Instruction")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Finish")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Continue")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Interrupt")), State = Debug_Busy);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Terminate Current")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Terminate")), Sensitive);
   end Set_Sensitive;

   ----------------
   -- Debug_Init --
   ----------------

   procedure Debug_Init
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : File_Project_Record;
      Args   : String)
   is
      K              : constant Kernel_Handle := Kernel_Handle (Kernel);
      Top            : constant GPS_Window :=
        GPS_Window (Get_Main_Window (K));
      Page           : GPS_Debugger;
      Module         : String_Access;
      Program_Args   : String_Access;
      Blank_Pos      : Natural;
      Proxy          : Process_Proxy_Access;
      Success        : Boolean;
      First_Debugger : Boolean;

      use Debugger;
      use type GNAT.OS_Lib.String_Access;

   begin
      Push_State (K, Busy);

      First_Debugger := Get_Current_Debugger (Kernel) = null;
      Gtk_New (Page, Top);
      Object_Callback.Connect
        (Page, "debugger_closed", On_Debug_Terminate_Single'Access);

      Program_Args := new String'("");

      if Data.File /= No_File then
         Module := new String'(Full_Name (Data.File).all);

      elsif Args /= "" then
         Blank_Pos := Ada.Strings.Fixed.Index (Args, " ");

         if Blank_Pos = 0 then
            Module := new String'(Args);
         else
            Module := new String'(Args (Args'First .. Blank_Pos - 1));
            Free (Program_Args);
            Program_Args := new String'(Args (Blank_Pos + 1 .. Args'Last));
         end if;

      else
         Module := new String'("");
      end if;

      declare
         Ptr         : GNAT.OS_Lib.String_Access :=
           GNAT.OS_Lib.Get_Executable_Suffix;
         Module_Exec : constant String := Module.all & Ptr.all;

      begin
         GNAT.OS_Lib.Free (Ptr);

         if Module'Length > 0
           and then not GNAT.OS_Lib.Is_Regular_File (Module.all)
           and then GNAT.OS_Lib.Is_Regular_File (Module_Exec)
         then
            Free (Module);
            Module := new String'(Module_Exec);
         end if;
      end;

      declare
         Args : GNAT.OS_Lib.Argument_List_Access :=
           GNAT.OS_Lib.Argument_String_To_List
             (Get_Attribute_Value
               (Get_Project (K), Debugger_Command_Attribute,
                Default => "gdb"));

      begin
         Proxy := new GPS_Proxy;
         GPS_Proxy (Proxy.all).Kernel := K;
         Configure
           (Process         => Page,
            Kind            => Gdb_Type,
            Proxy           => Proxy,
            Executable      => Module.all,
            Debugger_Args   => Args (2 .. Args'Last),
            Executable_Args => Program_Args.all,
            Remote_Host     =>
              Get_Attribute_Value (Get_Project (K), Remote_Host_Attribute),
            Remote_Target   =>
              Get_Attribute_Value (Get_Project (K), Program_Host_Attribute),
            Remote_Protocol =>
              Get_Attribute_Value (Get_Project (K), Protocol_Attribute),
            Debugger_Name   => Args (1).all,
            Success         => Success);
         GNAT.OS_Lib.Free (Args);
         Free (Module);

         if not Success then
            if Get_Current_Debugger (Kernel) = null then
               Debug_Terminate (K);
            end if;

            Pop_State (K);
            return;
         end if;
      end;

      Set_Sensitive (K, Debug_Available);

      if First_Debugger then
         --  Add columns information for not currently opened files.

         GVD_Module_ID.Lines_Hook := new Lines_Revealed_Hook_Record;
         Add_Hook
           (K, Source_Lines_Revealed_Hook, GVD_Module_ID.Lines_Hook,
            Watch => GObject (Top));

         GVD_Module_ID.File_Hook := new File_Edited_Hook_Record;
         GVD_Module_ID.File_Hook.Top := Top;
         Add_Hook
           (K, GPS.Kernel.File_Edited_Hook, GVD_Module_ID.File_Hook,
            Watch => GObject (Top));

         --  Add columns for debugging information to all the files that
         --  are currently open.

         Create_Debugger_Columns (K, VFS.No_File);
      end if;

      --  Force the creation of the project if needed
      Load_Project_From_Executable
        (K, Get_Current_Process (Get_Main_Window (K)));

      --  Connect only once the debugger has started, to avoid recomputing the
      --  side information twice.
      Widget_Callback.Object_Connect
        (Page, "executable_changed", On_Executable_Changed'Access, Top);

      GVD_Module_ID.Initialized := True;

      Run_Hook (K, Debugger_Started);

      Pop_State (K);

   exception
      when E : others =>
         Pop_State (K);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Debug_Init;

   -------------------
   -- On_Debug_Init --
   -------------------

   procedure On_Debug_Init
     (Kernel : access GObject_Record'Class; Data : File_Project_Record) is
   begin
      Debug_Init (Kernel_Handle (Kernel), Data, "");
   end On_Debug_Init;

   ---------------------
   -- Debug_Terminate --
   ---------------------

   procedure Debug_Terminate
     (Kernel   : Kernel_Handle;
      Debugger : access Visual_Debugger_Record'Class)
   is
      Top              : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Debugger_List    : Debugger_List_Link;
      Prev             : Debugger_List_Link;
      Editor           : Code_Editor;

   begin
      Debugger_List := Get_Debugger_List (Kernel);

      while Debugger_List /= null loop
         exit when Debugger_List.Debugger = GObject (Debugger);

         Prev := Debugger_List;
         Debugger_List := Debugger_List.Next;
      end loop;

      if Debugger_List = null then
         --  Should never happen

         Trace (Me, "debugger not found");
         return;
      end if;

      Push_State (Kernel, Busy);
      Debugger.Exiting := True;
      Editor := Debugger.Editor_Text;

      if Debugger.Debugger /= null
        and then Get_Process (Debugger.Debugger) /= null
      then
         Close (Debugger.Debugger);
      end if;

      Debugger.Debugger := null;

      --  This might have been closed by the user

      if Debugger.Debugger_Text /= null then
         Close (Top.MDI, Debugger.Debugger_Text);
      end if;

      if Debugger.Debuggee_Console /= null then
         Close (Top.MDI, Debugger.Debuggee_Console);
      end if;

      if Debugger.Data_Scrolledwindow /= null then
         Close (Top.MDI, Debugger.Data_Scrolledwindow);
      end if;

      if Debugger.Stack /= null then
         Close (Top.MDI, Debugger.Stack);
      end if;

      if Debugger.Breakpoints /= null then
         Free (Debugger.Breakpoints);
      end if;

      if Get_Mode (Editor) /= Source then
         Gtkada.MDI.Close (Get_MDI (Kernel), Get_Asm (Editor));
      end if;

      Free_Debug_Info (GEdit (Get_Source (Debugger.Editor_Text)));
      Debugger.Exiting := False;
      Unref (Debugger);

      if Prev = null then
         Set_First_Debugger (Kernel, Debugger_List.Next);

         if Debugger_List.Next = null then
            Set_Current_Debugger (Kernel, null);
         else
            Set_Current_Debugger (Kernel, Debugger_List.Next.Debugger);
         end if;
      else
         Prev.Next := Debugger_List.Next;
         Set_Current_Debugger (Kernel, Prev.Debugger);
      end if;

      Free (Debugger_List);

      if Get_Debugger_List (Kernel) = null then
         GVD_Module_ID.Initialized := False;

         if GVD_Module_ID.Lines_Hook /= null then
            Remove_Hook
              (Kernel, Source_Lines_Revealed_Hook, GVD_Module_ID.Lines_Hook);
            GVD_Module_ID.Lines_Hook := null;
         end if;

         if GVD_Module_ID.File_Hook /= null then
            Remove_Hook
              (Kernel, GPS.Kernel.File_Edited_Hook, GVD_Module_ID.File_Hook);
            GVD_Module_ID.File_Hook := null;
         end if;

         Set_Pref (Kernel, Show_Call_Stack, Debugger.Stack /= null);
         Remove_Debugger_Columns (Kernel, VFS.No_File);

         if GVD_Module_ID.Thread_Dialog /= null then
            Hide (GVD_Module_ID.Thread_Dialog);
         end if;

         if GVD_Module_ID.Task_Dialog /= null then
            Hide (GVD_Module_ID.Task_Dialog);
         end if;

         if GVD_Module_ID.PD_Dialog /= null then
            Hide (GVD_Module_ID.PD_Dialog);
         end if;

         if GVD_Module_ID.Breakpoints_Editor /= null then
            Hide (GVD_Module_ID.Breakpoints_Editor);
         end if;

         Set_Sensitive (Kernel, Debug_None);
      end if;

      Pop_State (Kernel);
   end Debug_Terminate;

   ---------------------
   -- Debug_Terminate --
   ---------------------

   procedure Debug_Terminate (Kernel : Kernel_Handle) is
      Debugger_List    : Debugger_List_Link;
      Current_Debugger : GPS_Debugger;
   begin
      Push_State (Kernel, Busy);
      Debugger_List := Get_Debugger_List (Kernel);

      while Debugger_List /= null loop
         Current_Debugger := GPS_Debugger (Debugger_List.Debugger);
         Debugger_List := Debugger_List.Next;
         Debug_Terminate (Kernel, Current_Debugger);
      end loop;

      Run_Hook (Kernel, Debugger_Terminated);
      Pop_State (Kernel);
   end Debug_Terminate;

   ------------------------
   -- On_Debug_Terminate --
   ------------------------

   procedure On_Debug_Terminate
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

   begin
      Debug_Terminate (Kernel);

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Debug_Terminate;

   --------------------------------
   -- On_Debug_Terminate_Current --
   --------------------------------

   procedure On_Debug_Terminate_Current
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

   begin
      Debug_Terminate (Kernel, Get_Current_Process (Get_Main_Window (Kernel)));

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Debug_Terminate_Current;

   -------------------------------
   -- On_Debug_Terminate_Single --
   -------------------------------

   procedure On_Debug_Terminate_Single
     (Widget : access GObject_Record'Class) is
   begin
      Debug_Terminate
        (Get_Kernel (GVD_Module_ID.all), Visual_Debugger (Widget));
   end On_Debug_Terminate_Single;

   -----------------------
   -- Get_Variable_Name --
   -----------------------

   function Get_Variable_Name
     (Context     : Selection_Context_Access;
      Dereference : Boolean) return String
   is
      Entity : Entity_Selection_Context_Access;
      Lang   : Language_Access;
   begin
      if Context = null
        or else Context.all not in Entity_Selection_Context'Class
      then
         return "";
      end if;

      Entity := Entity_Selection_Context_Access (Context);

      if Has_Entity_Name_Information (Entity) then
         if Dereference then
            if Has_File_Information (Entity) then
               Lang := Get_Language_From_File
                 (Get_Language_Handler (Get_Kernel (Context)),
                  File_Information (Entity));

               if Lang /= null then
                  return Dereference_Name
                    (Lang, Entity_Name_Information (Entity));
               end if;
            end if;
         else
            return Entity_Name_Information (Entity);
         end if;
      end if;

      return "";
   end Get_Variable_Name;

   -----------------------
   -- On_Start_Continue --
   -----------------------

   procedure On_Start_Continue (Object : access Gtk_Widget_Record'Class) is
      Tab : constant Visual_Debugger := Get_Current_Process (Object);
   begin
      if Tab /= null and then Tab.Debugger /= null then
         if Is_Started (Tab.Debugger) then
            Continue (Tab.Debugger, Mode => GVD.Types.Visible);
         else
            Start (Tab.Debugger, Mode => GVD.Types.Visible);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Start_Continue;

   -------------
   -- On_Step --
   -------------

   procedure On_Step (Object : access Gtk_Widget_Record'Class) is
      Tab : constant Visual_Debugger := Get_Current_Process (Object);
   begin
      if Tab /= null and then Tab.Debugger /= null then
         Step_Into (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Step;

   -------------
   -- On_Next --
   -------------

   procedure On_Next (Object : access Gtk_Widget_Record'Class) is
      Tab : constant Visual_Debugger := Get_Current_Process (Object);
   begin
      if Tab /= null and then Tab.Debugger /= null then
         Step_Over (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Next;

   ---------------
   -- On_Finish --
   ---------------

   procedure On_Finish (Object : access Gtk_Widget_Record'Class) is
      Tab : constant Visual_Debugger := Get_Current_Process (Object);
   begin
      if Tab /= null and then Tab.Debugger /= null then
         Finish (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Finish;

   -----------
   -- On_Up --
   -----------

   procedure On_Up (Object : access Gtk_Widget_Record'Class) is
      Tab : constant Visual_Debugger := Get_Current_Process (Object);
   begin
      if Tab /= null and then Tab.Debugger /= null then
         Stack_Up (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Up;

   -------------
   -- On_Down --
   -------------

   procedure On_Down (Object : access Gtk_Widget_Record'Class) is
      Tab : constant Visual_Debugger := Get_Current_Process (Object);
   begin
      if Tab /= null and then Tab.Debugger /= null then
         Stack_Down (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Down;

   ---------------------------
   -- On_Executable_Changed --
   ---------------------------

   procedure On_Executable_Changed (Object : access Gtk_Widget_Record'Class) is
      Top : constant GPS_Window := GPS_Window (Object);
      Process  : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Top.Kernel));
   begin
      --  Change the project to match the executable

      Load_Project_From_Executable (Top.Kernel, Process);

      --  Re-create all debugger columns.

      Remove_Debugger_Columns (Top.Kernel, VFS.No_File);
      Create_Debugger_Columns (Top.Kernel, VFS.No_File);
   end On_Executable_Changed;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook   : File_Edited_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D : constant File_Hooks_Args := File_Hooks_Args (Data.all);
   begin
      Create_Debugger_Columns (Kernel_Handle (Kernel), D.File);

   exception
      when E : others =>
         Debug_Terminate
           (Kernel_Handle (Kernel), Get_Current_Process (Hook.Top));
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook   : Lines_Revealed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Hook);
      D : constant Context_Hooks_Args := Context_Hooks_Args (Data.all);
      Area_Context : File_Area_Context_Access;

      Process      : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (D.Context)));

   begin
      if Process = null
        or else Process.Debugger = null
        or else D.Context.all not in File_Area_Context'Class
      then
         return;
      end if;

      Area_Context := File_Area_Context_Access (D.Context);

      declare
         Line1, Line2 : Integer;
         File : constant Virtual_File := File_Information (Area_Context);

      begin
         Get_Area (Area_Context, Line1, Line2);

         if GVD_Module_ID.Show_Lines_With_Code
           and then Command_In_Process (Get_Process (Process.Debugger))
         then
            return;
         end if;

         declare
            Lines       : Line_Array (Line1 .. Line2);
            A           : Line_Information_Array (Line1 .. Line2);
            C           : Set_Breakpoint_Command_Access;
            Mode        : Breakpoint_Command_Mode := Set;
            Tab         : constant Visual_Debugger :=
              Get_Current_Process (Get_Main_Window (Kernel));
            Bps         : Bp_Array (Line1 .. Line2) := (others => 0);
            Lines_Valid : Boolean := False;

         begin
            if GVD_Module_ID.Show_Lines_With_Code then
               Lines_With_Code (Process.Debugger, File, Lines_Valid, Lines);
            end if;

            --  Build an array of breakpoints in the current range, more
            --  efficient than re-browsing the whole array of breakpoints
            --  for each line.

            if Tab.Breakpoints /= null then
               for J in Tab.Breakpoints'Range loop
                  if Tab.Breakpoints (J).Line in Bps'Range
                    and then Tab.Breakpoints (J).File /= VFS.No_File
                    and then Tab.Breakpoints (J).File = File
                  then
                     Bps (Tab.Breakpoints (J).Line) :=
                       Tab.Breakpoints (J).Num;
                  end if;
               end loop;
            end if;

            for J in A'Range loop
               if Lines_Valid then
                  if Lines (J) then
                     A (J).Image := Line_Has_Code_Pixbuf;
                  end if;
               else
                  A (J).Image := Line_Might_Have_Code_Pixbuf;
               end if;

               if Bps (J) /= 0 then
                  Mode        := Unset;
                  A (J).Image := Line_Has_Breakpoint_Pixbuf;
               else
                  Mode := Set;
               end if;

               if (not Lines_Valid)
                 or else Lines (J)
               then
                  Create (C, Kernel_Handle (Kernel), Tab, Mode, File, J);
                  A (J).Associated_Command := Command_Access (C);
               end if;
            end loop;

            Add_Line_Information
              (Kernel, File, Breakpoints_Column_Id,
               new Line_Information_Array'(A));
         end;
      end;

   exception
      when E : others =>
         Debug_Terminate (Kernel_Handle (Kernel), Process);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Execute;

   ---------------------
   -- On_View_Changed --
   ---------------------

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class) is
      use GNAT.OS_Lib;
      Mitem : Gtk_Menu_Item;
      Menu  : Gtk_Menu renames GVD_Module_ID.Initialize_Menu;
      Iter  : Imported_Project_Iterator := Start (Get_Project (Kernel));
      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);
      Debuggable_Suffix : GNAT.OS_Lib.String_Access := Get_Debuggable_Suffix;

   begin
      --  Remove all existing menus
      Remove_All_Children (Menu);

      if Debuggable_Suffix = null then
         Debuggable_Suffix := new String'("");
      end if;

      --  Add all the main units from all the imported projects.
      while Current (Iter) /= No_Project loop
         declare
            Mains : GNAT.OS_Lib.Argument_List := Get_Attribute_Value
              (Current (Iter), Attribute => Main_Attribute);
         begin
            for M in Mains'Range loop
               declare
                  Exec : constant String := Get_Executable_Name
                    (Current (Iter), Mains (M).all);
               begin
                  Gtk_New (Mitem, Exec);
                  Append (Menu, Mitem);
                  File_Project_Cb.Object_Connect
                    (Mitem, "activate",
                     On_Debug_Init'Access,
                     Slot_Object => Kernel,
                     User_Data => File_Project_Record'
                       (Project => Current (Iter),
                        File    => Create
                          (Executables_Directory (Current (Iter)) & Exec)));
                  Set_Accel_Path (Mitem, Debug_Menu_Prefix & Exec, Group);
               end;
            end loop;

            Free (Mains);
         end;

         Next (Iter);
      end loop;

      Free (Debuggable_Suffix);

      --  Specific entry to start the debugger without any main program
      Gtk_New (Mitem, -"<no main file>");
      Append (Menu, Mitem);
      File_Project_Cb.Object_Connect
        (Mitem, "activate", On_Debug_Init'Access,
         Slot_Object => Kernel,
         User_Data => File_Project_Record'
           (Project => Get_Project (Kernel),
            File    => VFS.No_File));
      Set_Accel_Path (Mitem, Debug_Menu_Prefix & "<no main>", Group);
      Show_All (Menu);

   exception
      when E : others =>
         Debug_Terminate (Kernel_Handle (Kernel));
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_View_Changed;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Window : constant Gtk_Window := Get_Main_Window (Kernel);
      Top    : constant GPS_Window := GPS_Window (Window);
      Prev   : Boolean;

   begin
      GPS.Main_Window.Debug.Preferences_Changed (Top);

      if GVD_Module_ID.Initialized then
         Prev   := GVD_Module_ID.Show_Lines_With_Code;
         GVD_Module_ID.Show_Lines_With_Code :=
           Get_Pref (Editor_Show_Line_With_Code);

         if Prev /= GVD_Module_ID.Show_Lines_With_Code then
            Remove_Debugger_Columns (Kernel_Handle (Kernel), VFS.No_File);
            Create_Debugger_Columns (Kernel_Handle (Kernel), VFS.No_File);
         end if;
      end if;
   end Preferences_Changed;

   ------------------------------
   -- Debugger_Command_Handler --
   ------------------------------

   procedure Debugger_Command_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := GPS.Kernel.Scripts.Get_Kernel (Data);
   begin
      if GVD_Module_ID.Initialized then
         if Command = "send" then
            declare
               Process : constant Visual_Debugger :=
                 Get_Current_Process (Get_Main_Window (Kernel));
            begin
               Process_User_Command
                 (Process,
                  GPS.Kernel.Scripts.Nth_Arg (Data, 2),
                  Output_Command => True);
            end;

         elsif Command = GPS.Kernel.Scripts.Constructor_Method then
            --  Nothing to do for now. The plan, ultimately, is to be able to
            --  control which debugger the commands will apply to by selecting
            --  a debugger from the constructor, for instance by passing a name
            --  to the constructor
            null;
         end if;
      end if;
   end Debugger_Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Debug          : constant String := '/' & (-"_Debug") & '/';
      Debug_Sub      : constant String := Debug & (-"_Debug") & '/';
      Data_Sub       : constant String := Debug & (-"D_ata") & '/';
      Mitem          : Gtk_Menu_Item;
      Menu           : Gtk_Menu;
      Debugger_Class : constant GPS.Kernel.Scripts.Class_Type :=
        GPS.Kernel.Scripts.New_Class (Kernel, "Debugger");
      Command        : Interactive_Command_Access;
      Filter         : Action_Filter;
      Debugger_Filter   : Action_Filter;
      Printable_Filter  : Action_Filter;
      Access_Filter     : Action_Filter;
      Subprogram_Filter : Action_Filter;

   begin
      GVD_Module_ID := new GVD_Module_Record;
      Debugger_Module_ID := Module_ID (GVD_Module_ID);
      GVD.Preferences.Register_Default_Preferences (Get_Preferences (Kernel));
      GVD_Module_ID.Show_Lines_With_Code :=
        Get_Pref (Editor_Show_Line_With_Code);

      Register_Module
        (Module          => Module_ID (GVD_Module_ID),
         Kernel          => Kernel,
         Module_Name     => GVD_Module_Name,
         Priority        => Default_Priority + 20);

      Debugger_Filter := new Debugger_Active_Filter;
      Register_Filter (Kernel, Debugger_Filter, "Debugger active");

      Printable_Filter  := new Printable_Variable_Filter;
      Access_Filter     := new Access_Variable_Filter;
      Subprogram_Filter := new Subprogram_Variable_Filter;

      Register_Contextual_Submenu (Kernel, "Debug", Ref_Item => "References");

      Filter := Action_Filter (Debugger_Filter and Printable_Filter);
      Command := new Print_Variable_Command;
      Register_Contextual_Menu
        (Kernel, "Debug print variable",
         Label  => "Debug/Print %e",
         Action => Command,
         Filter => Filter);

      Command := new Print_Variable_Command;
      Print_Variable_Command (Command.all).Display := True;
      Register_Contextual_Menu
        (Kernel, "Debug display variable",
         Label  => "Debug/Display %e",
         Action => Command,
         Filter => Filter);

      Filter := Action_Filter
        (Debugger_Filter and Printable_Filter and Access_Filter);
      Command := new Print_Variable_Command;
      Print_Variable_Command (Command.all).Display := False;
      Print_Variable_Command (Command.all).Dereference := True;
      Register_Contextual_Menu
        (Kernel, "Debug print dereferenced variable",
         Label  => "Debug/Print %C",
         Action => Command,
         Custom => Custom_Label_Expansion'Access,
         Filter => Filter);

      Command := new Print_Variable_Command;
      Print_Variable_Command (Command.all).Display := True;
      Print_Variable_Command (Command.all).Dereference := True;
      Register_Contextual_Menu
        (Kernel, "Debug display dereferenced variable",
         Label  => "Debug/Display %C",
         Action => Command,
         Custom => Custom_Label_Expansion'Access,
         Filter => Filter);

      Command := new Set_Value_Command;
      Register_Contextual_Menu
        (Kernel, "Debug set value",
         Label  => -"Debug/Set value of %e",
         Action => Command,
         Filter => Action_Filter (Debugger_Filter and Printable_Filter));

      Command := new View_Memory_Command;
      Register_Contextual_Menu
        (Kernel, "Debug view memory",
         Label  => -"Debug/View memory at address of %e",
         Action => Command,
         Filter => Action_Filter (Debugger_Filter and Printable_Filter));

      Command := new Set_Breakpoint_Command;
      Register_Contextual_Menu
        (Kernel, "Debug set subprogram breakpoint",
         Label  => -"Debug/Set breakpoint on %e",
         Action => Command,
         Filter => Action_Filter (Debugger_Filter and Subprogram_Filter));

      Command := new Set_Breakpoint_Command;
      Set_Breakpoint_Command (Command.all).On_Line := True;
      Register_Contextual_Menu
        (Kernel, "Debug set line breakpoint",
         Label  => -"Debug/Set breakpoint on line %l",
         Action => Command,
         Filter => Debugger_Filter);

      Command := new Set_Breakpoint_Command;
      Set_Breakpoint_Command (Command.all).On_Line := True;
      Set_Breakpoint_Command (Command.all).Continue_Till := True;
      Register_Contextual_Menu
        (Kernel, "Debug continue until",
         Label  => -"Debug/Continue until line %l",
         Action => Command,
         Filter => Debugger_Filter);

      Command := new Show_Location_Command;
      Register_Contextual_Menu
        (Kernel, "Debug show current location",
         Label  => -"Debug/Show current location",
         Action => Command,
         Filter => Debugger_Filter);

      Register_Menu (Kernel, Debug, Ref_Item => -"Tools");

      --  Dynamic Initialize menu
      Mitem := Register_Menu (Kernel, Debug, -"Initialize", "", null,
                              Ref_Item => -"Data");
      Gtk_New (Menu);
      Set_Submenu (Mitem, Menu);
      GVD_Module_ID.Initialize_Menu := Menu;

      Add_Hook (Kernel, Project_View_Changed_Hook, On_View_Changed'Access);

      --  Add debugger menus

      Mitem := Register_Menu (Kernel, Data_Sub, -"_Protection Domains", "",
                              On_PD'Access);
      Set_Sensitive (Mitem, False);
      Register_Menu (Kernel, Debug_Sub, Ref_Item => -"Data");
      Register_Menu (Kernel, Debug_Sub, -"_Connect to Board...", "",
                     On_Connect_To_Board'Access);
      Register_Menu (Kernel, Debug_Sub, -"_Load File...", "",
                     On_Debug_Executable'Access);
      Register_Menu (Kernel, Debug_Sub, -"Add _Symbols...", "",
                     On_Add_Symbols'Access);
      Register_Menu (Kernel, Debug_Sub, -"_Attach...", "",
                     On_Attach'Access);
      Register_Menu (Kernel, Debug_Sub, -"_Detach", "",
                     On_Detach'Access);
      Register_Menu (Kernel, Debug_Sub, -"Debug C_ore File...", "",
                     On_Load_Core'Access);
      Register_Menu (Kernel, Debug_Sub, -"_Kill", "",
                     On_Kill'Access);

      Register_Menu (Kernel, Data_Sub, -"_Call Stack", "",
                     On_Call_Stack'Access, Ref_Item => -"Protection Domains");
      Register_Menu (Kernel, Data_Sub, -"_Threads", "",
                     On_Threads'Access, Ref_Item => -"Protection Domains");
      Register_Menu (Kernel, Data_Sub, -"Ta_sks", "",
                     On_Tasks'Access, Ref_Item => -"Protection Domains");
      Register_Menu (Kernel, Data_Sub, -"A_ssembly", "", On_Assembly'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Data_Sub, Mitem);
      Register_Menu (Kernel, Data_Sub, -"Edit _Breakpoints", "",
                     On_Edit_Breakpoints'Access);
      Register_Menu (Kernel, Data_Sub, -"Examine _Memory", "",
                     On_Examine_Memory'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Data_Sub, Mitem);
      Register_Menu (Kernel, Data_Sub, -"Display _Local Variables", "",
                     On_Display_Locals'Access, null,
                     GDK_L, Mod1_Mask);
      Register_Menu (Kernel, Data_Sub, -"Display _Arguments", "",
                     On_Display_Args'Access, null,
                     GDK_U, Mod1_Mask);
      Register_Menu (Kernel, Data_Sub, -"Display _Registers", "",
                     On_Display_Regs'Access);
      Register_Menu (Kernel, Data_Sub, -"Display Any _Expression...", "",
                     On_Display_Expression'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Data_Sub, Mitem);
      Register_Menu (Kernel, Data_Sub, -"Re_fresh", Stock_Refresh,
                     On_Data_Refresh'Access, null,
                     GDK_L, Control_Mask);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Debug, Mitem);

      Register_Menu (Kernel, Debug, -"_Run...", "",
                     On_Start'Access, null, GDK_F2, Sensitive => False);
      Register_Menu (Kernel, Debug, -"S_tep", "",
                     On_Step'Access, null,  GDK_F5, Sensitive => False);
      Register_Menu (Kernel, Debug, -"Step _Instruction", "",
                     On_Step_Instruction'Access, null,
                     GDK_F5, Shift_Mask, Sensitive => False);
      Register_Menu (Kernel, Debug, -"_Next", "",
                     On_Next'Access, null, GDK_F6, Sensitive => False);
      Register_Menu (Kernel, Debug, -"N_ext Instruction", "",
                     On_Next_Instruction'Access, null,
                     GDK_F6, Shift_Mask, Sensitive => False);
      Register_Menu (Kernel, Debug, -"_Finish", "",
                     On_Finish'Access, null, GDK_F7, Sensitive => False);
      Register_Menu (Kernel, Debug, -"_Continue", "",
                     On_Continue'Access, null, GDK_F8, Sensitive => False);
      Register_Menu (Kernel, Debug, -"_Interrupt", Stock_Stop,
                     On_Interrupt'Access, null,
                     GDK_backslash, Control_Mask, Sensitive => False);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Debug, Mitem);

      Register_Menu (Kernel, Debug, -"Te_rminate Current", "",
                     On_Debug_Terminate_Current'Access, Sensitive => False);
      Register_Menu (Kernel, Debug, -"Ter_minate", "",
                     On_Debug_Terminate'Access, Sensitive => False);

      Set_Sensitive (Kernel_Handle (Kernel), Debug_None);

      Add_Hook (Kernel, Preferences_Changed_Hook,
                Preferences_Changed'Access);

      --  Commands

      GPS.Kernel.Scripts.Register_Command
        (Kernel, GPS.Kernel.Scripts.Constructor_Method,
         Class         => Debugger_Class,
         Handler       => Debugger_Command_Handler'Access);

      GPS.Kernel.Scripts.Register_Command
        (Kernel, "send",
         Class         => Debugger_Class,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Debugger_Command_Handler'Access);

      --  Hooks

      Register_Hook (Kernel, Debugger_Started);
      Register_Hook (Kernel, Debugger_Terminated);

      Init_Graphics;
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out GVD_Module_Record) is
   begin
      Debug_Terminate (Get_Kernel (Id));

      if Id.Task_Dialog /= null then
         Destroy (Id.Task_Dialog);
      end if;

      if Id.Thread_Dialog /= null then
         Destroy (Id.Thread_Dialog);
      end if;

      if Id.PD_Dialog /= null then
         Destroy (Id.PD_Dialog);
      end if;
   end Destroy;

end GVD_Module;
