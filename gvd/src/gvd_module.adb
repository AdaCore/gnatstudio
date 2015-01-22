------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.Traces;           use GNATCOLL.Traces;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Table;                 use Gtk.Table;
with Gtk.Tool_Button;           use Gtk.Tool_Button;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.File_Selector;      use Gtkada.File_Selector;

with Breakpoints_Editor;        use Breakpoints_Editor;
with Commands.Debugger;         use Commands.Debugger;
with Commands.Interactive;      use Commands.Interactive;
with Commands;                  use Commands;
with Debugger;                  use Debugger;
with Debugger_Pixmaps;          use Debugger_Pixmaps;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Main_Window.Debug;     use GPS.Main_Window.Debug;
with GPS.Main_Window;           use GPS.Main_Window;

with GUI_Utils;                 use GUI_Utils;
with GVD.Assembly_View;         use GVD.Assembly_View;
with GVD.Call_Stack;            use GVD.Call_Stack;
with GVD.Canvas;                use GVD.Canvas;
with GVD.Consoles;              use GVD.Consoles;
with GVD.Code_Editors;          use GVD.Code_Editors;
with GVD.Dialogs;               use GVD.Dialogs;
with GVD.Memory_View;           use GVD.Memory_View;
with GVD.Menu;                  use GVD.Menu;
with GVD.Preferences;           use GVD.Preferences;
with GVD.Proc_Utils;            use GVD.Proc_Utils;
with GVD.Process;               use GVD.Process;
with GVD.Scripts;               use GVD.Scripts;
with GPS.Stock_Icons;           use GPS.Stock_Icons;
with GVD.Source_Editor.GPS;     use GVD.Source_Editor.GPS;
with GVD.Source_Editor;         use GVD.Source_Editor;
with GVD.Types;                 use GVD.Types;
with Histories;                 use Histories;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with List_Select_Pkg;           use List_Select_Pkg;
with Process_Proxies;           use Process_Proxies;
with Std_Dialogs;               use Std_Dialogs;
with String_Utils;              use String_Utils;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GPS.Editors;               use GPS.Editors;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with Xref;                      use Xref;

package body GVD_Module is
   Me : constant Trace_Handle := Create ("GVD_MODULE");

   Cst_Run_Arguments_History : constant History_Key := "gvd_run_arguments";
   --  The key in the history for the arguments to the run command.
   --  WARNING: this constant is shared with builder_module.adb, since we want
   --  to have the same history for the run command in GPS.

   Debug_Menu_Prefix : constant String := "<gps>/Debug/Initialize/";

   History_Target_Name : constant History_Key := "gvd-target-name";
   History_Protocol : constant History_Key := "gvd-protocol";

   type Bp_Array is array (Integer range <>) of Breakpoint_Identifier;

   type File_Edited_Hook_Record is new Function_With_Args with record
      Top : GPS_Window;
   end record;
   type File_Edited_Hook is access File_Edited_Hook_Record'Class;
   overriding procedure Execute
     (Hook   : File_Edited_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_edited" hook

   type Lines_Revealed_Hook_Record is new Function_With_Args with null record;
   type Lines_Revealed_Hook is access Lines_Revealed_Hook_Record'Class;
   overriding procedure Execute
     (Hook   : Lines_Revealed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "source_lines_revealed_hook" hook

   type GVD_Module_Record is new Module_ID_Record with record
      Initialized                    : Boolean := False;
      --  Whether the debugger is running

      Show_Lines_With_Code           : Boolean;
      --  Whether the lines with code should be explicitly queried

      Initialize_Menu                : Gtk_Menu;

      Delete_Id                      : Handler_Id := (Null_Handler_Id, null);
      File_Hook                      : File_Edited_Hook;
      Lines_Hook                     : Lines_Revealed_Hook;

      Cont_Button,
      Step_Button,
      Next_Button,
      Finish_Button,
      Up_Button,
      Down_Button                    : Gtk.Tool_Button.Gtk_Tool_Button;

      First_Debugger                 : Debugger_List_Link;
      --  Points to the list of debuggers

      Current_Debugger               : Glib.Object.GObject;
      --  The current visual debugger
   end record;
   type GVD_Module is access all GVD_Module_Record'Class;

   overriding procedure Destroy (Id : in out GVD_Module_Record);
   --  Terminate the debugger module, and kill the underlying debugger

   overriding function Tooltip_Handler
     (Module  : access GVD_Module_Record;
      Context : Selection_Context) return Gtk_Widget;
   --  See inherited documentation

   GVD_Module_Name : constant String := "Debugger";
   GVD_Module_ID   : GVD_Module;

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called every time the project view changes, to recompute the dynamic
   --  menus.

   procedure Remove_Debugger_Columns
     (Kernel : Kernel_Handle;
      File   : Virtual_File);
   --  Remove the side information columns corresponding to the debugger
   --  in the editors for file.
   --  If File is empty, remove them for all files.

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when the preferences are changed in the GPS kernel

   procedure Create_Debugger_Columns
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Create the side information columns corresponding to the debugger
   --  in the editors for file.
   --  If File is empty, create them for all files.

   procedure Debug_Init
     (Kernel  : GPS.Kernel.Kernel_Handle;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : String);
   --  Initialize the debugger

   procedure Start_Program
     (Process : Visual_Debugger; Start_Cmd : Boolean := False);
   --  Start the execution of the main program.
   --  A dialog is pop up for setting the argument list to give to the program
   --  being debugged when it is started. If Start_Cmd is True then the
   --  debugger must stop at the beginning of the main procedure. Otherwise,
   --  the dialog will include a checkbox so that the user will be able to
   --  select whether he/she wants to stop at the beginning of the program.
   --  For VxWorks systems, the entry point to be executed must also be
   --  specified together with the arguments; in addition, the multi-task-mode
   --  can be set as desired.
   --  For non VxWorks systems, a check box to allow the user to run under the
   --  exec dir of the program is also provided, similar to the Build->Run
   --  dialog.

   -----------------
   -- Dbg_Command --
   -----------------

   --  This subpackage defines a new Debugger_Command type, that is used as a
   --  basis for every debugger command type

   package Dbg_Command is
      type Debugger_Command is abstract new Interactive_Command
      with null record;
      --  Abstract type that is the basis for debugger commands. Will take care
      --  of some boilerplate code, like checking that the debugger is active,
      --  and checking wether the command has been issued from the debugger
      --  console Debugger commands don't need to override Execute, as is
      --  usual with Interactive_Commands, but Execute_Dbg

      type Debugger_Command_Access is access all Debugger_Command'Class;

      overriding function Execute
        (Command : access Debugger_Command;
         Context : Interactive_Command_Context) return Command_Return_Type;
      --  Overriden Execute primitive to take care of Debugger_Command
      --  boilerplate, do not override

      function Execute_Dbg
        (Command : access Debugger_Command;
         Process : Visual_Debugger) return Command_Return_Type is abstract;
      --  Types derived from Debugger_Command need to override this primitive
      --  Process is the process of the active debugger for the command.

   end Dbg_Command;

   package body Dbg_Command is
      overriding function Execute
        (Command : access Debugger_Command;
         Context : Interactive_Command_Context) return Command_Return_Type
      is
         Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
         Top     : GPS_Window;
         Process : Visual_Debugger;
      begin
         Top := GPS_Window (Kernel.Get_Main_Window);
         if Top = null then
            return Commands.Failure;
         end if;

         Process := Get_Current_Process (Top);

         if Process = null or else Process.Debugger = null then
            return Commands.Failure;
         end if;

         if Process.Debugger_Text.Get_View.Has_Focus then
            Process.Is_From_Dbg_Console := True;
         end if;

         return Debugger_Command_Access (Command).Execute_Dbg (Process);
      end Execute;
   end Dbg_Command;

   --------------------
   -- Menu Callbacks --
   --------------------

   type Initialize_Debugger_Command is new Interactive_Command with record
      Exec   : Virtual_File;
   end record;
   overriding function Execute
     (Command : access Initialize_Debugger_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Initialize

   type Connect_To_Board_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Connect_To_Board_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Debug->Connect to Board

   type Load_File_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Load_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Debug->Load File

   type Add_Symbols_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Add_Symbols_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Debug->Add Symbols

   type Load_Core_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Load_Core_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Debug->Debug Core File

   type Attach_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Attach_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Debug->Attach

   type Detach_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Detach_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Debug->Detach

   type Kill_Command is new Dbg_Command.Debugger_Command with null record;
   overriding function Execute_Dbg
     (Command : access Kill_Command;
      Process : Visual_Debugger) return Command_Return_Type;
   --  Debug->Debug->Kill

   type Local_Vars_Command is new Dbg_Command.Debugger_Command
   with null record;

   overriding function Execute_Dbg
     (Command : access Local_Vars_Command;
      Process : Visual_Debugger) return Command_Return_Type;
   --  Debug->Data->Display Local Variables

   type Arguments_Command is new Dbg_Command.Debugger_Command with null record;
   overriding function Execute_Dbg
     (Command : access Arguments_Command;
      Process : Visual_Debugger) return Command_Return_Type;
   --  Debug->Data->Display Arguments

   type Registers_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Data->Display Registers

   type Expression_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Expression_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Data->Display Any Expression

   type Start_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Start_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Run... menu

   type Step_Command is new Dbg_Command.Debugger_Command with null record;
   overriding function Execute_Dbg
     (Command : access Step_Command;
      Process : Visual_Debugger) return Command_Return_Type;
   --  Debug->Step menu

   type Stepi_Command is new Dbg_Command.Debugger_Command with null record;
   overriding function Execute_Dbg
     (Command : access Stepi_Command;
      Process : Visual_Debugger) return Command_Return_Type;
   --  Debug->Step Instruction menu

   type Next_Command is new Dbg_Command.Debugger_Command with null record;
   overriding function Execute_Dbg
     (Command : access Next_Command;
      Process : Visual_Debugger) return Command_Return_Type;
   --  Debug->Next menu

   type Nexti_Command is new Dbg_Command.Debugger_Command with null record;
   overriding function Execute_Dbg
     (Command : access Nexti_Command;
      Process : Visual_Debugger) return Command_Return_Type;
   --  Debug->Next Instruction menu

   type Finish_Command is new Dbg_Command.Debugger_Command with null record;
   overriding function Execute_Dbg
     (Command : access Finish_Command;
      Process : Visual_Debugger) return Command_Return_Type;
   --  Debug->Finish menu

   type Continue_Command is new Dbg_Command.Debugger_Command with null record;
   overriding function Execute_Dbg
     (Command : access Continue_Command;
      Process : Visual_Debugger) return Command_Return_Type;
   --  Debug->Continue menu

   type Interrupt_Command is new Dbg_Command.Debugger_Command with null record;
   overriding function Execute_Dbg
     (Command : access Interrupt_Command;
      Process : Visual_Debugger) return Command_Return_Type;
   --  Debug->Interrupt

   type Terminate_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Terminate_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Terminate Current

   type Terminate_All_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Terminate_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Terminate

   type Up_Command is new Dbg_Command.Debugger_Command with null record;
   overriding function Execute_Dbg
     (Command : access Up_Command;
      Process : Visual_Debugger) return Command_Return_Type;

   type Down_Command is new Dbg_Command.Debugger_Command with null record;
   overriding function Execute_Dbg
     (Command : access Down_Command;
      Process : Visual_Debugger) return Command_Return_Type;

   --------------------
   -- Misc Callbacks --
   --------------------

   procedure On_Executable_Changed
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Hook for "debugger_executable_changed"

   ----------------
   -- Contextual --
   ----------------

   type Debugger_Stopped_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Stopped_Filter;
      Context : Selection_Context) return Boolean;
   --  True if the debugger has been started but is now idle waiting for new
   --  commands.

   type Debugger_Active_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Active_Filter;
      Context : Selection_Context) return Boolean;

   type Printable_Variable_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Printable_Variable_Filter;
      Context : Selection_Context) return Boolean;

   type Access_Variable_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Access_Variable_Filter;
      Context : Selection_Context) return Boolean;

   type Subprogram_Variable_Filter
     is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Subprogram_Variable_Filter;
      Context : Selection_Context) return Boolean;

   type Print_Variable_Command is new Interactive_Command with record
      Display     : Boolean := False;
      Dereference : Boolean := False;
   end record;
   overriding function Execute
     (Command : access Print_Variable_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Set_Value_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Set_Value_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Show_Location_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Show_Location_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Set_Breakpoint_Command is new Interactive_Command with record
      On_Line       : Boolean := False;  --  If False, on entity
      Continue_Till : Boolean := False;  --  Continue until given line ?
   end record;
   overriding function Execute
     (Command : access Set_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   function Custom_Label_Expansion
     (Context : Selection_Context) return String;
   --  Provide expansion for "$!" in the labels for contextual menus

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
      Trace (Me, "Set_Current_Debugger");
      GVD_Module_ID.Current_Debugger := Current;
   end Set_Current_Debugger;

   -------------------------
   -- Initialize_Debugger --
   -------------------------

   procedure Initialize_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Args   : String) is
   begin
      Debug_Init
        (GPS.Kernel.Kernel_Handle (Kernel),
         No_File,
         Args);
   end Initialize_Debugger;

   -----------------------------
   -- Create_Debugger_Columns --
   -----------------------------

   procedure Create_Debugger_Columns
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) is
   begin
      --  Create the information column for the current line
      Create_Line_Information_Column
        (Kernel,
         File,
         "Current Line",
         Empty_Line_Information,
         --  ??? That should be centralized somewhere !!!
         Every_Line    => False);

      --  Create the information column for the breakpoints
      Create_Line_Information_Column
        (Kernel,
         File,
         Breakpoints_Column_Id,
         Empty_Line_Information,
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

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Add_Symbols_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Top     : constant GPS_Window := GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

      use GNAT.Strings;
   begin
      declare
         S : constant Virtual_File :=
               Select_File
                 (Title             => -"Select Module",
                  Parent            => Gtk_Window (Top),
                  Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                  Kind              => Open_File,
                  File_Pattern      => "*",
                  Pattern_Name      => -"All files",
                  History           => Get_History (Kernel));
      begin
         if S = GNATCOLL.VFS.No_File then
            return Commands.Failure;
         end if;

         if Process.Descriptor.Remote_Host /= null
           or else Is_Regular_File (S)
         then
            declare
               Addr : constant String :=
                        Query_User (Gtk_Window (Top),
                                    -"Enter starting address of module's text"
                                    & ASCII.LF
                                    & (-"Optionally leave empty on VxWorks"),
                                    False, False, "0");

            begin
               Add_Symbols
                 (Process.Debugger,
                  Module  => S,
                  Address => Addr,
                  Mode    => GVD.Types.Visible);
            end;

         else
            Kernel.Insert
              ((-"Could not find file: ") & S.Display_Full_Name,
               Mode => Error);
            return Commands.Failure;
         end if;
      end;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Attach_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Top          : constant GPS_Window :=
                       GPS_Window (Get_Main_Window (Kernel));
      Process      : constant Visual_Debugger := Get_Current_Process (Top);
      Process_List : List_Select_Access;
      Success      : Boolean;
      Info         : Process_Info;
      Ignore       : Message_Dialog_Buttons;
      pragma Unreferenced (Command, Ignore);

   begin
      if Process = null or else Process.Debugger = null then
         return Commands.Failure;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Ignore := Message_Dialog
           ((-"Cannot attach to a task/process while the") & ASCII.LF &
            (-"underlying debugger is busy.") & ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
         return Commands.Failure;
      end if;

      Gtk_New
        (Process_List,
         Title         => -"Process Selection",
         Item_Label    => -"Pid",
         Comment_Label => -"Command");

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

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Detach_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Top     : constant GPS_Window :=
                  GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);
      Ignore  : Message_Dialog_Buttons;
      pragma Unreferenced (Command, Ignore);
   begin
      if Process = null or else Process.Debugger = null then
         return Commands.Failure;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Ignore := Message_Dialog
           ((-"Cannot detach the task/process while the") & ASCII.LF &
            (-"underlying debugger is busy.") & ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);

      else
         Detach_Process (Process.Debugger, Mode => GVD.Types.Visible);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute_Dbg
     (Command : access Step_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Step_Into (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   -------------
   -- Execute --
   -------------

   overriding function Execute_Dbg
     (Command : access Stepi_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Step_Into_Instruction (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   -------------
   -- Execute --
   -------------

   overriding function Execute_Dbg
     (Command : access Next_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Step_Over (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   -------------
   -- Execute --
   -------------

   overriding function Execute_Dbg
     (Command : access Nexti_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Step_Over_Instruction (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   -------------
   -- Execute --
   -------------

   overriding function Execute_Dbg
     (Command : access Finish_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin

      Finish (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   -------------
   -- Execute --
   -------------

   overriding function Execute_Dbg
     (Command : access Continue_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin

      if Is_Started (Process.Debugger) then
         Continue (Process.Debugger, Mode => GVD.Types.Visible);
      else
         --  Launch the dialog for starting the application
         Start_Program (Process, Start_Cmd => True);
      end if;

      return Commands.Success;
   end Execute_Dbg;

   -------------
   -- Execute --
   -------------

   overriding function Execute_Dbg
     (Command : access Kill_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Kill_Process (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   -------------
   -- Execute --
   -------------

   overriding function Execute_Dbg
     (Command : access Interrupt_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      --  Give some visual feedback to the user

      Output_Text (Process, "<^C>" & ASCII.LF, Is_Command => True);
      Unregister_Dialog (Process);

      --  Need to flush the queue of commands
      Clear_Queue (Process.Debugger);

      Interrupt (Process.Debugger);

      if not Command_In_Process (Get_Process (Process.Debugger)) then
         Display_Prompt (Process.Debugger);
      end if;

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

      return Commands.Success;
   end Execute_Dbg;

   -------------
   -- Execute --
   -------------

   overriding function Execute_Dbg
     (Command : access Local_Vars_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      --  ???? won't work with GDB/MI
      Process_User_Command
        (Process,
         "graph display `" & Info_Locals (Process.Debugger) & '`',
         Output_Command => True);
      return Commands.Success;
   end Execute_Dbg;

   -------------
   -- Execute --
   -------------

   overriding function Execute_Dbg
     (Command : access Arguments_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      --  ???? won't work with GDB/MI
      Process_User_Command
        (Process,
         "graph display `" & Info_Args (Process.Debugger) & '`',
         Output_Command => True);
      return Commands.Success;
   end Execute_Dbg;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Top     : constant GPS_Window :=
                  GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return Commands.Failure;
      end if;

      --  ???? won't work with GDB/MI
      Process_User_Command
        (Process,
         "graph display `" & Info_Registers (Process.Debugger) & '`',
         Output_Command => True);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Expression_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Top     : constant GPS_Window :=
                  GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);
   begin
      if Process = null or else Process.Debugger = null then
         return Commands.Failure;
      end if;

      Display_Expression (Process);
      return Commands.Success;
   end Execute;

   -------------------
   -- Start_Program --
   -------------------

   procedure Start_Program
     (Process : Visual_Debugger; Start_Cmd : Boolean := False)
   is
      Is_Multitask : aliased Boolean := False;
      Is_Start     : aliased Boolean := False;
      Use_Exec_Dir : aliased Boolean := False;
      Arg_Msg      : aliased String := -"Run arguments:";
      Entry_Msg    : aliased String := -"Entry point and arguments:";
      Start_Msg    : aliased String := -"Stop at beginning of main subprogram";
      Dir_Msg      : aliased String := -"Use exec dir instead of current dir";
      Multi_Msg    : aliased String := -"Enable VxWorks multi-tasks mode";
      Start_Key    : aliased String := "stop_beginning_debugger";
      Dir_Key      : aliased String := "run_in_executable_directory";
      Multi_Key    : aliased String := "multitask_mode_debugger";
      No_Msg       : aliased String := "";
      Button1      : Boolean_Access := null;
      Button2      : Boolean_Access := null;
      Cmd_Msg      : GNAT.Strings.String_Access := Arg_Msg'Unchecked_Access;
      Msg1         : GNAT.Strings.String_Access := No_Msg'Unchecked_Access;
      Msg2         : GNAT.Strings.String_Access;
      Key1         : GNAT.Strings.String_Access := No_Msg'Unchecked_Access;
      Key2         : GNAT.Strings.String_Access;

   begin
      --  If the user has already requested to stop at the beginning (Start
      --  command) do not ask the same question again. Otherwise, we enable
      --  a checkbox so that the user can select whether he/she wants to
      --  stop at the beginning of the main program.

      if Start_Cmd then
         Is_Start := True;
      else
         Button1 := Is_Start'Unchecked_Access;
         Msg1    := Start_Msg'Unchecked_Access;
         Key1    := Start_Key'Unchecked_Access;
      end if;

      --  If we are debugging against VxWorks we as for the entry point to be
      --  executed, and we enable the multi-tasks-mode checkbox.

      if VxWorks_Version (Process.Debugger) in Vx5 .. Vx6 then
         --  Change the message in the dialog window indicating that the entry
         --  point needs to be specified together with the arguments.

         Cmd_Msg := Entry_Msg'Unchecked_Access;

         Button2 := Is_Multitask'Unchecked_Access;
         Msg2    := Multi_Msg'Unchecked_Access;
         Key2    := Multi_Key'Unchecked_Access;

      else
         Button2 := Use_Exec_Dir'Unchecked_Access;
         Msg2    := Dir_Msg'Unchecked_Access;
         Key2    := Dir_Key'Unchecked_Access;
      end if;

      declare
         Arguments : constant String :=
           Strip_Ending_Linebreaks
             (Display_Entry_Dialog
                (Parent         => Process.Kernel.Get_Main_Window,
                 Title          => -"Run/Start",
                 Message        => Cmd_Msg.all,
                 Key            => Cst_Run_Arguments_History,
                 History        => Get_History (Process.Kernel),
                 Check_Msg      => Msg1.all,
                 Check_Msg2     => Msg2.all,
                 Button_Active  => Button1,
                 Button2_Active => Button2,
                 Key_Check      => History_Key (Key1.all),
                 Key_Check2     => History_Key (Key2.all)));

      begin
         if Arguments = ""
           or else Arguments (Arguments'First) /= ASCII.NUL
         then
            --  For VxWorks we need to set the desired multi-tasks-mode mode

            if VxWorks_Version (Process.Debugger) in Vx5 .. Vx6 then
               if Is_Multitask then
                  Send
                    (Process.Debugger,
                     "set multi-tasks-mode on",
                     Mode => GVD.Types.Hidden);
               else
                  Send
                    (Process.Debugger,
                     "set multi-tasks-mode off",
                     Mode => GVD.Types.Hidden);
               end if;
            end if;

            if Use_Exec_Dir then
               Change_Directory
                 (Process.Debugger,
                  Process.Descriptor.Program.Dir);
            end if;

            if Is_Start then
               Start (Process.Debugger, Arguments, Mode => GVD.Types.Visible);
            else
               Run (Process.Debugger, Arguments, Mode => GVD.Types.Visible);
            end if;
         end if;
      end;
   end Start_Program;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Start_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Process : constant Visual_Debugger :=
                  Get_Current_Process (GPS_Window (Get_Main_Window (Kernel)));
      Ignore  : Message_Dialog_Buttons;
      pragma Unreferenced (Ignore);
   begin
      if Process = null or else Process.Debugger = null then
         return Commands.Failure;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Ignore := Message_Dialog
           ((-"Cannot rerun while the underlying debugger is busy.") &
            ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
         return Commands.Failure;
      end if;

      --  Launch the dialog for starting the application

      Start_Program (Process);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Connect_To_Board_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Top          : constant GPS_Window :=
                       GPS_Window (Get_Main_Window (Kernel));
      Process      : constant Visual_Debugger :=
                       Get_Current_Process (Top);
      Dialog       : Gtk_Dialog;
      Table        : Gtk_Table;
      Ent_Protocol : Gtk_Entry;
      Ent_Target   : Gtk_Entry;
      Label        : Gtk_Label;
      Ignore       : Gtk_Widget;
      pragma Unreferenced (Command, Ignore);
   begin
      Gtk_New
        (Dialog,
         Title  => -"Connect to board",
         Parent => Get_Current_Window (Kernel),
         Flags  => Modal or Destroy_With_Parent);
      Set_Position (Dialog, Win_Pos_Mouse);
      Set_Default_Size (Dialog, 300, 100);

      Gtk_New (Table, 2, 2, False);
      Pack_Start (Get_Content_Area (Dialog), Table, Expand => False);

      Gtk_New (Label, -"Target name:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 0, 1, Xpadding => 3, Ypadding => 3);

      Gtk_New (Ent_Target);
      Set_Width_Chars (Ent_Target, 20);
      Ent_Target.Set_Text
        (Most_Recent (Get_History (Kernel), History_Target_Name));
      Attach (Table, Ent_Target, 1, 2, 0, 1);
      Grab_Focus (Ent_Target);
      Set_Activates_Default (Ent_Target, True);

      Gtk_New (Label, -"Protocol:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 1, 2, Xpadding => 3, Ypadding => 3);

      Gtk_New (Ent_Protocol);
      Ent_Protocol.Set_Text
        (Most_Recent (Get_History (Kernel), History_Protocol));
      Set_Width_Chars (Ent_Protocol, 20);
      Attach (Table, Ent_Protocol, 1, 2, 1, 2);
      Set_Activates_Default (Ent_Protocol, True);

      Ignore := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Ignore := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
      Set_Default_Response (Dialog, Gtk_Response_OK);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         Process.Descriptor.Remote_Target :=
           new String'(Get_Text (Ent_Target));
         Process.Descriptor.Protocol :=
           new String'(Get_Text (Ent_Protocol));

         Add_To_History
           (Get_History (Kernel).all, History_Target_Name,
            Get_Text (Ent_Target));
         Add_To_History
           (Get_History (Kernel).all, History_Protocol,
            Get_Text (Ent_Protocol));

         Connect_To_Target
           (Process.Debugger,
            Process.Descriptor.Remote_Target.all,
            Process.Descriptor.Protocol.all,
            GVD.Types.Visible);
      end if;

      Destroy (Dialog);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Load_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Top         : constant GPS_Window :=
                      GPS_Window (Get_Main_Window (Kernel));
      Process     : constant Visual_Debugger :=
                      Get_Current_Process (Top);
      Exec        : Virtual_File;
      Ptr         : GNAT.Strings.String_Access :=
                      GNAT.OS_Lib.Get_Executable_Suffix;
      Exec_Suffix : constant String := Ptr.all;

      use GNAT.OS_Lib;

   begin
      Free (Ptr);

      declare
         S : Virtual_File :=
               Select_File
                 (Title             => -"Select File to Debug",
                  File_Pattern      => +("*" & Exec_Suffix & ";*"),
                  Pattern_Name      => -"Executable files;All files",
                  Parent            => Get_Current_Window (Kernel),
                  Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                  Kind              => Open_File,
                  History           => Get_History (Kernel));
      begin
         if S = GNATCOLL.VFS.No_File then
            return Commands.Failure;
         end if;

         if not Is_Regular_File (S) then
            Exec := GNATCOLL.VFS.Locate_On_Path (Base_Name (S));

            if not Is_Regular_File (Exec) then
               Kernel.Insert
                 ((-"Could not find file: ") & Display_Base_Name (S),
                  Mode => Error);
               S := GNATCOLL.VFS.No_File;
            else
               S := Exec;
            end if;
         end if;

         if S /= No_File then
            Set_Executable (Process.Debugger, S, Mode => Hidden);
         end if;

      exception
         when Executable_Not_Found =>
            Kernel.Insert
              ((-"Could not find file: ") & Display_Full_Name (S),
               Mode => Error);
            return Commands.Failure;
      end;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Load_Core_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Top     : constant GPS_Window := GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

      use GNAT.Strings;

   begin
      declare
         S : constant Virtual_File :=
               Select_File
                 (Title             => -"Select Core File",
                  File_Pattern      => "core*;*",
                  Pattern_Name      => -"Core files;All files",
                  Parent            => Get_Current_Window (Kernel),
                  Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                  Kind              => Open_File,
                  History           => Get_History (Kernel));
      begin
         if S = GNATCOLL.VFS.No_File then
            return Commands.Failure;
         end if;

         if Process.Descriptor.Remote_Host /= null
           or else Is_Regular_File (S)
         then
            Load_Core_File
              (Process.Debugger, S,
               Mode => GVD.Types.Visible);

         else
            Kernel.Insert
              ((-"Could not find core file: ") &
               Display_Full_Name (S),
               Mode => Error);
         end if;
      end;
      return Commands.Success;
   end Execute;

   ----------------------------
   -- Custom_Label_Expansion --
   ----------------------------

   function Custom_Label_Expansion
     (Context : Selection_Context) return String is
   begin
      return Emphasize (Get_Variable_Name (Context, True));
   end Custom_Label_Expansion;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Show_Location_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Top  : constant GPS_Window :=
               GPS_Window (Get_Main_Window (Get_Kernel (Context.Context)));
      Edit : constant GEdit :=
               GEdit (Get_Source (Get_Current_Process (Top).Editor_Text));
      Name : constant Virtual_File := Get_Current_File (Edit);
   begin
      if Name /= GNATCOLL.VFS.No_File then
         Highlight_Current_Line (Edit);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Print_Variable_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Process  : constant Visual_Debugger :=
                   Get_Current_Process
                     (Get_Main_Window (Get_Kernel (Context.Context)));
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

   overriding function Execute
     (Command : access Set_Value_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Process  : constant Visual_Debugger :=
                   Get_Current_Process
                     (Get_Main_Window (Get_Kernel (Context.Context)));
      Variable : constant String :=
                   Get_Variable_Name (Context.Context, False);
      S        : constant String :=
                   Simple_Entry_Dialog
                     (Parent   => Process.Kernel.Get_Main_Window,
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

   overriding function Execute
     (Command : access Set_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Debugger : constant Debugger_Access := Get_Current_Process
        (Get_Main_Window (Get_Kernel (Context.Context))).Debugger;
   begin
      if not Command.On_Line then
         Break_Subprogram
           (Debugger,
            Entity_Name_Information (Context.Context),
            Mode => GVD.Types.Visible);
      elsif Command.Continue_Till then
         Break_Source
           (Debugger,
            File_Information (Context.Context),
            Contexts.Line_Information (Context.Context),
            Temporary => True);
         Continue (Debugger, Mode => GVD.Types.Visible);
      else
         Break_Source
           (Debugger,
            File_Information (Context.Context),
            Contexts.Line_Information (Context.Context),
            Mode => GVD.Types.Visible);
      end if;

      return Commands.Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Stopped_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Process : constant Visual_Debugger :=
                  Get_Current_Process (Get_Main_Window (Get_Kernel (Context)));
   begin
      return Process /= null
        and then Process.Debugger /= null
        and then not Command_In_Process (Get_Process (Process.Debugger));
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Active_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Process : constant Visual_Debugger :=
                  Get_Current_Process (Get_Main_Window (Get_Kernel (Context)));
   begin
      return Process /= null and then Process.Debugger /= null;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Subprogram_Variable_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if Has_Entity_Name_Information (Context) then
         declare
            Entity : constant Root_Entity'Class := Get_Entity (Context);
         begin
            return Is_Fuzzy (Entity)
              or else Is_Subprogram (Entity);
         end;
      end if;
      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Printable_Variable_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if Has_Entity_Name_Information (Context) then
         declare
            Entity : constant Root_Entity'Class := Get_Entity (Context);
         begin
            return Is_Fuzzy (Entity)
              or else Is_Printable_In_Debugger (Entity);
         end;

      elsif Has_Area_Information (Context) then
         --  We assume the user knows best
         return True;
      end if;
      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Access_Variable_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if Has_Entity_Name_Information (Context) then
         declare
            Entity : constant Root_Entity'Class := Get_Entity (Context);
         begin
            return Is_Fuzzy (Entity)

            --  ??? Should also include array variables
              or else (not Is_Type (Entity)
                       and then Is_Access (Entity));
         end;

      elsif Has_Area_Information (Context) then
         return True;
      end if;
      return False;
   end Filter_Matches_Primitive;

   ---------------------
   -- Tooltip_Handler --
   ---------------------

   overriding function Tooltip_Handler
     (Module  : access GVD_Module_Record;
      Context : Selection_Context) return Gtk_Widget
   is
      pragma Unreferenced (Module);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Debugger : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Kernel));
      Value    : GNAT.Strings.String_Access;
      W        : Gtk_Widget;
      Label    : Gtk_Label;

   begin
      if Debugger = null
        or else Debugger.Debugger = null
        or else not Has_Entity_Name_Information (Context)
        or else Command_In_Process (Get_Process (Debugger.Debugger))
      then
         return null;
      end if;

      declare
         Variable_Name : constant String := Get_Variable_Name
           (Context, Dereference => False);

      begin
         if Variable_Name = ""
           or else not Can_Tooltip_On_Entity
             (Get_Language (Debugger.Debugger), Variable_Name)
         then
            return null;

         else
            Value := new String'(Value_Of (Debugger.Debugger, Variable_Name));
         end if;

         if Value.all /= "" then
            Gtk_New (Label, Value.all);
            W := Gtk_Widget (Label);
         else
            --  Note: if Value.all is "", we will return Pixmap below, hence
            --  the assignment.

            W := null;
         end if;

         GNAT.Strings.Free (Value);
         return W;
      end;

   exception
      when Language.Unexpected_Type | Constraint_Error =>
         return null;
      when E : others =>
         Trace (Me, E);
         return null;
   end Tooltip_Handler;

   ----------------
   -- Debug_Init --
   ----------------

   procedure Debug_Init
     (Kernel  : GPS.Kernel.Kernel_Handle;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : String)
   is
      Ignore : Visual_Debugger;
      pragma Unreferenced (Ignore);
   begin
      Ignore := Spawn
        (Kernel, Debugger_Kind.Get_Pref, File,
         Get_Project (Kernel), Args);
   end Debug_Init;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Initialize_Debugger_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
   begin
      Debug_Init (Get_Kernel (Context.Context), Command.Exec, "");
      return Commands.Success;
   end Execute;

   ------------------------
   -- Setup_Side_Columns --
   ------------------------

   procedure Setup_Side_Columns
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Top : constant GPS_Window := GPS_Window (Get_Main_Window (Kernel));
   begin
      if GVD_Module_ID.Lines_Hook = null then
         --  Add columns information for not currently opened files

         GVD_Module_ID.Lines_Hook := new Lines_Revealed_Hook_Record;
         Add_Hook
           (Kernel, Source_Lines_Revealed_Hook, GVD_Module_ID.Lines_Hook,
            Name  => "gvd.lines_revealed",
            Watch => GObject (Top));

         GVD_Module_ID.File_Hook := new File_Edited_Hook_Record;
         GVD_Module_ID.File_Hook.Top := Top;
         Add_Hook
           (Kernel, GPS.Kernel.File_Edited_Hook, GVD_Module_ID.File_Hook,
            Name  => "gvd.file_edited",
            Watch => GObject (Top));

         --  Add columns for debugging information to all the files that
         --  are currently open.

         Create_Debugger_Columns (Kernel, GNATCOLL.VFS.No_File);
      end if;
      GVD_Module_ID.Initialized := True;
   end Setup_Side_Columns;

   ---------------------
   -- Debug_Terminate --
   ---------------------

   procedure Debug_Terminate (Kernel : Kernel_Handle) is
      Debugger_List    : Debugger_List_Link := Get_Debugger_List (Kernel);
      Current_Debugger : Visual_Debugger;
   begin
      while Debugger_List /= null loop
         Current_Debugger := Visual_Debugger (Debugger_List.Debugger);
         Debugger_List := Debugger_List.Next;
         Close_Debugger (Current_Debugger);
      end loop;

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

      Remove_Debugger_Columns (Kernel, GNATCOLL.VFS.No_File);
   end Debug_Terminate;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Terminate_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Debug_Terminate (Kernel);
      Update_Menus_And_Buttons (Kernel);
      return Commands.Success;
   exception
      when E : others =>
         Trace (Me, E);
         return Commands.Failure;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Terminate_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Close_Debugger (Get_Current_Process (Get_Main_Window (Kernel)));
      Update_Menus_And_Buttons (Kernel);
      return Commands.Success;

   exception
      when E : others =>
         Trace (Me, E);
         return Commands.Failure;
   end Execute;

   -----------------------
   -- Get_Variable_Name --
   -----------------------

   function Get_Variable_Name
     (Context     : Selection_Context;
      Dereference : Boolean) return String
   is
      Lang  : Language_Access;
   begin
      if Context = No_Context then
         return "";
      end if;

      if Has_File_Information (Context) then
         Lang := Get_Language_From_File
           (Get_Language_Handler (Get_Kernel (Context)),
            File_Information (Context));
      end if;

      if Has_Area_Information (Context) then
         if Dereference and then Lang /= null then
            return Dereference_Name (Lang, Text_Information (Context));
         end if;

         return Text_Information (Context);
      end if;

      if Has_Expression_Information (Context) then
         if Dereference and then Lang /= null then
            return Dereference_Name (Lang, Expression_Information (Context));
         end if;

         return Expression_Information (Context);
      end if;

      if Has_Entity_Name_Information (Context) then
         if Dereference and then Lang /= null then
            return Dereference_Name
              (Lang, Entity_Name_Information (Context));
         end if;

         return Entity_Name_Information (Context);
      end if;

      return "";
   end Get_Variable_Name;

   -------------
   -- Execute --
   -------------

   overriding function Execute_Dbg
     (Command : access Up_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Stack_Up (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   -------------
   -- Execute --
   -------------

   overriding function Execute_Dbg
     (Command : access Down_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Stack_Down (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   ---------------------------
   -- On_Executable_Changed --
   ---------------------------

   procedure On_Executable_Changed
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      Process : constant Visual_Debugger :=
                  Get_Process (Debugger_Hooks_Data_Access (Data));
   begin
      --  Change the project to match the executable

      Load_Project_From_Executable (Kernel, Process);

      --  Verify the language used in the executable

      Detect_Language (Process.Debugger);

      --  Re-create all debugger columns

      Remove_Debugger_Columns (Kernel_Handle (Kernel), GNATCOLL.VFS.No_File);
      Create_Debugger_Columns (Kernel_Handle (Kernel), GNATCOLL.VFS.No_File);
   end On_Executable_Changed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : File_Edited_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D : constant File_Hooks_Args := File_Hooks_Args (Data.all);
   begin
      Create_Debugger_Columns (Kernel_Handle (Kernel), D.File);

   exception
      when E : others =>  Trace (Me, E);
         Close_Debugger (Get_Current_Process (Hook.Top));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : Lines_Revealed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Hook);
      D       : constant Context_Hooks_Args := Context_Hooks_Args (Data.all);

      Process : constant Visual_Debugger :=
                  Get_Current_Process
                    (Get_Main_Window (Get_Kernel (D.Context)));

   begin
      if Process = null
        or else Process.Debugger = null
        or else not Has_Area_Information (D.Context)
      then
         return;
      end if;

      declare
         File         : constant Virtual_File := File_Information (D.Context);
         Line1, Line2 : Integer;

      begin
         Get_Area (D.Context, Line1, Line2);

         if GVD_Module_ID.Show_Lines_With_Code
           and then Command_In_Process (Get_Process (Process.Debugger))
         then
            return;
         end if;

         declare
            Tab         : constant Visual_Debugger :=
                            Get_Current_Process (Get_Main_Window (Kernel));
            Lines       : Line_Array (Line1 .. Line2);
            A           : Line_Information_Array (Line1 .. Line2);
            C           : Set_Breakpoint_Command_Access;
            Mode        : Breakpoint_Command_Mode := Set;
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
                    and then Tab.Breakpoints (J).File /= GNATCOLL.VFS.No_File
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
      when E : others => Trace (Me, E);
         Close_Debugger (Process);
   end Execute;

   ---------------------
   -- On_View_Changed --
   ---------------------

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class) is
      use GNAT.OS_Lib;
      Menu              : Gtk_Menu renames GVD_Module_ID.Initialize_Menu;
      Loaded_Project    : constant Project_Type := Get_Project (Kernel);
      Iter              : Project_Iterator := Loaded_Project.Start;
      Current_Project   : Project_Type := Current (Iter);
      Tmp               : Project_Type;

      procedure Create_Action_And_Menu
        (Prj : Project_Type; Main : Virtual_File; M : Integer);
      --  Create the action and menu to initialize a specific executable

      procedure Add_Entries (Mains : in out Argument_List; Prj : Project_Type);
      --  Add menu entries for all executables in Main. Main is freed on exit

      ----------------------------
      -- Create_Action_And_Menu --
      ----------------------------

      procedure Create_Action_And_Menu
        (Prj : Project_Type; Main : Virtual_File; M : Integer)
      is
         Main_Name : constant String :=
           (if Main = No_File then
               -"<no main file>"
            else
               Escape_Underscore (Escape_Menu_Name (Main.Display_Base_Name)));

         Action  : constant String := "debug initialize " & Main_Name;
         Menu    : constant String :=
           "/Debug/Initialize/"
           & (if Prj = No_Project
              then "" else Escape_Underscore (Prj.Name) & '/')
           & Main_Name;
         Command : Interactive_Command_Access;
         Item    : Gtk_Menu_Item;
      begin
         Command := new Initialize_Debugger_Command'
           (Interactive_Command with Exec => Main);
         Unregister_Action (Kernel, Action);
         Register_Action
           (Kernel, Action, Command,
            -"Initialize the debugger on the file "
            & Main.Display_Full_Name,
            Category => -"Debug");
         Item := Register_Menu (Kernel, Menu, Action => Action);

         --  Only set accelerators for main units of the root project
         if Prj = No_Project or else Prj = Loaded_Project then

            if Main = No_File then
               Set_Accel_Path (Item, Debug_Menu_Prefix & "<no main>",
                               Get_Default_Accelerators (Kernel));
            else
               Set_Accel_Path
                 (Item, Debug_Menu_Prefix & "item" & Image (M),
                  Get_Default_Accelerators (Kernel));
            end if;
         end if;
      end Create_Action_And_Menu;

      -----------------
      -- Add_Entries --
      -----------------

      procedure Add_Entries
        (Mains : in out Argument_List; Prj : Project_Type) is
      begin
         for M in Mains'Range loop
            declare
               Exec : constant Filesystem_String :=
                 Prj.Executable_Name (+Mains (M).all);
               Dir  : constant Virtual_File := Executables_Directory (Prj);
            begin
               if Dir = No_File then
                  Create_Action_And_Menu (Prj, Create_From_Base (Exec), M);
               else
                  Create_Action_And_Menu (Prj, Create_From_Dir (Dir, Exec), M);
               end if;
            end;
         end loop;
      end Add_Entries;

   begin
      --  Remove all existing menus

      Remove_All_Children (Menu);

      --  The following loop should be factorized with the one in the builder
      --  module (see Builder_Module.On_View_Changed).

      while Current_Project /= No_Project loop
         --  Never show the main units from an extended project, since we only
         --  look at the actual executables in the extending project

         if Extending_Project (Current_Project) /= No_Project then
            null;

         --  If we have an extending project, the list of main units could
         --  come from the project itself, or be inherited from extended
         --  projects.

         else
            Tmp := Current_Project;
            while Tmp /= No_Project loop
               declare
                  Mains : String_List_Access :=
                    Tmp.Attribute_Value (Main_Attribute);
               begin
                  if Mains /= null and then Mains'Length /= 0 then
                     --  Basenames inherited, but exec_dir is current project
                     Add_Entries (Mains.all, Current_Project);

                     --  Stop looking in inherited project, since the attribute
                     --  has been overridden.
                     Tmp := No_Project;
                  else
                     Tmp := Extended_Project (Tmp);
                  end if;

                  Free (Mains);
               end;
            end loop;
         end if;

         Next (Iter);
         Current_Project := Current (Iter);
      end loop;

      --  Specific entry to start the debugger without any main program

      Create_Action_And_Menu (No_Project, No_File, 0);
      Show_All (Menu);

   exception
      when E : others =>
         Trace (Me, E);
         Debug_Terminate (Kernel_Handle (Kernel));
   end On_View_Changed;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Data);
      Window : constant Gtk_Window := Get_Main_Window (Kernel);
      Top    : constant GPS_Window := GPS_Window (Window);
      Prev   : Boolean;

   begin
      GPS.Main_Window.Debug.Preferences_Changed (Top);

      Prev   := GVD_Module_ID.Show_Lines_With_Code;
      GVD_Module_ID.Show_Lines_With_Code :=
        Editor_Show_Line_With_Code.Get_Pref;

      if GVD_Module_ID.Initialized
        and then Prev /= GVD_Module_ID.Show_Lines_With_Code
      then
         Remove_Debugger_Columns (Kernel_Handle (Kernel), No_File);
         Create_Debugger_Columns (Kernel_Handle (Kernel), No_File);
      end if;

      Init_Graphics (Gtk_Widget (Get_Main_Window (Kernel)));
   end Preferences_Changed;

   -----------------------
   -- Create_GVD_Module --
   -----------------------

   procedure Create_GVD_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      GVD_Module_ID := new GVD_Module_Record;
      Debugger_Module_ID := Module_ID (GVD_Module_ID);
      if Kernel /= null then
         Register_Module
           (Module          => Module_ID (GVD_Module_ID),
            Kernel          => Kernel,
            Module_Name     => GVD_Module_Name,
            Priority        => Default_Priority + 20);
      end if;
   end Create_GVD_Module;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Mitem             : Gtk_Menu_Item;
      Menu              : Gtk_Menu;
      Command           : Interactive_Command_Access;
      Filter            : Action_Filter;
      Debugger_Filter   : Action_Filter;
      Stopped_And_In_File : Action_Filter;
      Debugger_Active   : Action_Filter;
      Printable_Filter  : Action_Filter;
      Access_Filter     : Action_Filter;
      Subprogram_Filter : Action_Filter;

   begin
      Create_GVD_Module (Kernel);
      GVD.Preferences.Register_Default_Preferences (Get_Preferences (Kernel));
      GVD.Scripts.Create_Hooks (Kernel);
      GVD_Module_ID.Show_Lines_With_Code :=
        Editor_Show_Line_With_Code.Get_Pref;

      Debugger_Filter := new Debugger_Stopped_Filter;
      Register_Filter (Kernel, Debugger_Filter, "Debugger stopped");

      Debugger_Active := new Debugger_Active_Filter;
      Register_Filter (Kernel, Debugger_Active, "Debugger active");

      Stopped_And_In_File :=
        Debugger_Filter and Lookup_Filter (Kernel, "Source editor");

      Printable_Filter  := new Printable_Variable_Filter;
      Register_Filter
        (Kernel, Printable_Filter, "Debugger printable variable");

      Access_Filter     := new Access_Variable_Filter;
      Subprogram_Filter := new Subprogram_Variable_Filter;

      Register_Contextual_Submenu (Kernel, "Debug", Ref_Item => "References");

      Filter := Debugger_Filter and Printable_Filter;
      Command := new Print_Variable_Command;
      Register_Contextual_Menu
        (Kernel, "Debug print variable",
         Label  => "Debug/Print %S",
         Action => Command,
         Filter => Filter);

      Command := new Print_Variable_Command;
      Print_Variable_Command (Command.all).Display := True;
      Register_Contextual_Menu
        (Kernel, "Debug display variable",
         Label  => "Debug/Display %S",
         Action => Command,
         Filter => Filter);

      Filter := Debugger_Filter and Printable_Filter and Access_Filter;
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
         Label  => -"Debug/Set value of %S",
         Action => Command,
         Filter => Debugger_Filter and Printable_Filter);

      Command := new Set_Breakpoint_Command;
      Register_Contextual_Menu
        (Kernel, "Debug set subprogram breakpoint",
         Label  => -"Debug/Set breakpoint on %e",
         Action => Command,
         Filter => Debugger_Filter and Subprogram_Filter);

      Command := new Set_Breakpoint_Command;
      Set_Breakpoint_Command (Command.all).On_Line := True;
      Register_Contextual_Menu
        (Kernel, "Debug set line breakpoint",
         Label  => -"Debug/Set breakpoint on line %l",
         Action => Command,
         Filter => Stopped_And_In_File);

      Command := new Set_Breakpoint_Command;
      Set_Breakpoint_Command (Command.all).On_Line := True;
      Set_Breakpoint_Command (Command.all).Continue_Till := True;
      Register_Contextual_Menu
        (Kernel, "Debug continue until",
         Label  => -"Debug/Continue until line %l",
         Action => Command,
         Filter => Stopped_And_In_File);

      Command := new Show_Location_Command;
      Register_Contextual_Menu
        (Kernel, "Debug show current location",
         Label  => -"Debug/Show current location",
         Action => Command,
         Filter => Debugger_Filter);

      --  Dynamic Initialize menu
      Mitem := Find_Menu_Item (Kernel, -"/Debug/Initialize");
      Gtk_New (Menu);
      Set_Submenu (Mitem, Menu);
      GVD_Module_ID.Initialize_Menu := Menu;
      Add_Hook (Kernel, Project_View_Changed_Hook,
                Wrapper (On_View_Changed'Access),
                Name => "gvd.project_view_changed");

      --  Add debugger menus

      Register_Action
        (Kernel, "debug connect to board", new Connect_To_Board_Command,
         Description =>
           -("Opens a simple dialog to connect to a remote board. This option"
           & " is only relevant to cross debuggers."),
         Filter   => Debugger_Filter,
         Category => -"Debug");

      Register_Action
        (Kernel, "debug load file", new Load_File_Command,
         Description =>
           -("Opens a file selection dialog that allows you to choose a"
           & " program to debug. The program to debug is either an executable"
           & " for native debugging, or a partially linked module for cross"
           & " environments (e.g VxWorks)."),
         Filter   => Debugger_Filter,
         Category => -"Debug");

      Register_Action
        (Kernel, "debug add symbols", new Add_Symbols_Command,
         Description =>
           -("Add the symbols from a given file/module. This corresponds to"
           & " the gdb command add-symbol-file. This menu is particularly"
           & " useful under VxWorks targets, where the modules can be loaded"
           & " independently of the debugger.  For instance, if a module is"
           & " independently loaded on the target (e.g. using windshell), it"
           & " is absolutely required to use this functionality, otherwise"
           & " the debugger won't work properly."),
         Filter   => Debugger_Filter,
         Category => -"Debug");

      Register_Action
        (Kernel, "debug attach", new Attach_Command,
         Description => -"Attach to a running process",
         Filter   => Debugger_Filter,
         Category => -"Debug");

      Register_Action
        (Kernel, "debug detach", new Detach_Command,
         Description => -"Detach the application from the debugger",
         Filter   => Debugger_Filter,
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug core file", new Load_Core_Command,
         Description => -"Debug a core file instead of a running process",
         Filter   => Debugger_Filter,
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug kill", new Kill_Command,
         Description => -"Kill the debuggee process",
         Filter   => Debugger_Filter,
         Category    => -"Debug");

      GVD.Canvas.Register_Module (Kernel);
      GVD.Call_Stack.Register_Module (Kernel);
      GVD.Consoles.Register_Module (Kernel);
      GVD.Dialogs.Register_Module (Kernel);
      GVD.Assembly_View.Register_Module (Kernel);
      Breakpoints_Editor.Register_Module (Kernel);
      GVD.Memory_View.Register_Module (Kernel);

      Register_Action
        (Kernel, "debug display local variables", new Local_Vars_Command,
         Filter      => Debugger_Filter,
         Description => -"Display local variables in the data window",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug display arguments", new Arguments_Command,
         Filter      => Debugger_Filter,
         Description => -"Display arguments to the current subprogram",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug display registers", new Registers_Command,
         Description => -"Display the contents of registers in data window",
         Filter      => Debugger_Filter,
         Category    => -"Debug");

      Register_Action
        (Kernel, "Debug display any expression", new Expression_Command,
         Description => -"Opens a dialog to choose an expression to display",
         Filter      => Debugger_Filter,
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug run dialog", new Start_Command,
         Filter      => Debugger_Active,
         Description =>
           -"Choose the arguments to the program, and start running it",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug step", new Step_Command,
         Stock_Id    => "gps-debugger-step",
         Filter      => Debugger_Active,
         Description =>
           -"Execute until program reaches a new line of source code",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug stepi", new Stepi_Command,
         Filter      => Debugger_Active,
         Description =>
           -"Execute the program for one machine instruction only",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug next", new Next_Command,
         Stock_Id    => "gps-debugger-next",
         Filter      => Debugger_Active,
         Description =>
           -("Execute the program until the next source line, stepping over"
             & " subprogram calls"),
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug nexti", new Nexti_Command,
         Filter      => Debugger_Active,
         Description =>
           -("Execute the program until the next machine instruction, stepping"
             & " over subprogram calls"),
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug finish", new Finish_Command,
         Stock_Id    => "gps-debugger-finish",
         Filter      => Debugger_Active,
         Description =>
           -("Continue execution until selected stack frame returns"),
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug continue", new Continue_Command,
         Stock_Id    => "gps-debugger-run",
         Filter      => Debugger_Active,
         Description =>
           -("Continue execution until next breakpoint." & ASCII.LF
           & "Start the debugger if not started yet"),
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug up", new Up_Command,
         Stock_Id    => "gps-debugger-up",
         Filter      => Debugger_Active,
         Description => "Move up one frame",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug down", new Down_Command,
         Stock_Id    => "gps-debugger-down",
         Filter      => Debugger_Active,
         Description => "Move down one frame",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug interrupt", new Interrupt_Command,
         Stock_Id    => GPS_Stop_Task,
         Filter      => Debugger_Active,
         Description => -"Asynchronously interrupt the debuggee program",
         Category    => -"Debug");

      Register_Action
        (Kernel, "terminate debugger", new Terminate_Command,
         Description => -"Terminate the current debugger",
         Filter      => Debugger_Active);

      Register_Action
        (Kernel, "terminate all debuggers", new Terminate_All_Command,
         Description => -"Terminate all running debugger",
         Filter      => Debugger_Active);

      Add_Hook (Kernel, Preference_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "gvd.preferences_changed");
      Add_Hook (Kernel, Debugger_Executable_Changed_Hook,
                Wrapper (On_Executable_Changed'Access),
                Name => "gvd.debugger_ext_changed");
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Id : in out GVD_Module_Record) is
   begin
      Debug_Terminate (Get_Kernel (Id));
   end Destroy;

   ----------------
   -- Get_Module --
   ----------------

   function Get_Module return GPS.Kernel.Modules.Module_ID is
   begin
      return Debugger_Module_ID;
   end Get_Module;

end GVD_Module;
