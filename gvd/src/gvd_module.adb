------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;
with GNAT.Strings;                 use GNAT.Strings;

with GNATCOLL.Any_Types;           use GNATCOLL.Any_Types;
with GNATCOLL.Projects;            use GNATCOLL.Projects;
with GNATCOLL.Traces;              use GNATCOLL.Traces;
with GNATCOLL.VFS;                 use GNATCOLL.VFS;

with Glib;                         use Glib;

with Gtk.Check_Button;             use Gtk.Check_Button;
with Gtk.Dialog;                   use Gtk.Dialog;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Label;                    use Gtk.Label;
with Gtk.Text_Iter;                use Gtk.Text_Iter;
with Gtk.Widget;                   use Gtk.Widget;
with Gtk.Window;                   use Gtk.Window;

with Gtkada.Dialogs;               use Gtkada.Dialogs;
with Gtkada.File_Selector;         use Gtkada.File_Selector;

with Basic_Types;                  use Basic_Types;
with Commands.Interactive;         use Commands.Interactive;
with Commands;                     use Commands;
with Debugger;                     use Debugger;
with Default_Preferences;          use Default_Preferences;
with GPS.Editors;                  use GPS.Editors;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with GPS.Intl;                     use GPS.Intl;
with GPS.Kernel.Actions;           use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;          use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;               use GPS.Kernel.MDI;
with GPS.Kernel.Messages;          use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Simple;   use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules.UI;        use GPS.Kernel.Modules.UI;
with GPS.Kernel.Modules;           use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;       use GPS.Kernel.Preferences;
with GPS.Kernel.Project;           use GPS.Kernel.Project;
with GPS.Kernel;                   use GPS.Kernel;
with GPS.Main_Window;              use GPS.Main_Window;
with GUI_Utils;                    use GUI_Utils;
with GVD.Code_Editors;             use GVD.Code_Editors;
with GVD.Consoles;
with GVD.Contexts;                 use GVD.Contexts;
with GVD.Preferences;              use GVD.Preferences;
with GVD.Process;                  use GVD.Process;
with GVD.Process_Lists;            use GVD.Process_Lists;
with GVD.Scripts;
with GVD.Types;                    use GVD.Types;
with GVD.Variables.Items;          use GVD.Variables.Items;
with Histories;                    use Histories;
with Language;                     use Language;
with Language_Handlers;            use Language_Handlers;
with Process_Proxies;              use Process_Proxies;
with GPS.Dialogs;                  use GPS.Dialogs;
with Remote;                       use Remote;
with String_Utils;                 use String_Utils;
with Xref;                         use Xref;

package body GVD_Module is
   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.GVD_MODULE");

   Run_Arguments_History_Key : constant History_Key := "gvd_run_arguments";
   --  The key in the history for the arguments to the run command.
   --  WARNING: this constant is shared with builder_module.adb, since we want
   --  to have the same history for the run command in GPS.

   package Debugger_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Base_Visual_Debugger_Access);

   type GVD_Module_Record is new Module_ID_Record with record
      Actions : Action_Lists.List;
      --  Actions that have been registered dynamically by this module,
      --  for the dynamic menus

      Debugger_List                  : Debugger_Lists.List;
      --  Points to the list of debuggers

      Current_Debugger               : access Base_Visual_Debugger'Class;
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

   Messages_Category_Continue_To_Line : constant String :=
                                         "debugger-run-to-line";
   Continue_To_Line_Messages_Flags    : constant Message_Flags :=
                                         (Editor_Line => True,
                                          Locations   => False,
                                          Editor_Side => False);

   type On_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
      (Self   : On_View_Changed;
       Kernel : not null access Kernel_Handle_Record'Class);
   --  Called every time the project view changes, to recompute the dynamic
   --  menus.

   type On_Location_Changed is new File_Location_Hooks_Function with
     null record;
   overriding procedure Execute
     (Self         : On_Location_Changed;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Column : Integer;
      Project      : GNATCOLL.Projects.Project_Type);
   --  Called when the current editor's location changes.
   --  Used to display a clickable icon on the gutter to continue the
   --  execution until we reach the given location.

   type On_Debugger_Location_Changed is new Debugger_Hooks_Function with
     null record;
   overriding procedure Execute
     (Self     : On_Debugger_Location_Changed;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class);
   --  Called when the current debugger's location changes.
   --  Used to remove the "Continue to line" clickable icon on the debugger's
   --  current location.

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);

   type On_Debugger_Started is new Debugger_Hooks_Function with
     null record;
   overriding procedure Execute
     (Self     : On_Debugger_Started;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class);
   --  Called when a debugger starts.
   --  Used to add the "Continue to line" editor columns.

   type On_Debugger_Terminated is new Debugger_Hooks_Function with null record;
   overriding procedure Execute
     (Self     : On_Debugger_Terminated;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class);
   --  Called when a debugger terminates.
   --  Used to remove the "Continue to line" clickable icon, if any.

   procedure Create_Continue_To_Line_Columns
     (Kernel : not null access Kernel_Handle_Record'Class;
      Buffer : Editor_Buffer'Class);
   --  Create the column where the "Continue to line" clcikable icons will
   --  be displayed.

   procedure Remove_Continue_To_Line_Messages
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Remove the message associated with the "Continue to line" action

   procedure Disable_Continue_To_Line_On_Editors
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Disable the "Continue to line" clickable icons on editors, removing the
   --  associated messages and the extra column if necessary.

   procedure Debug_Init
     (Kernel  : GPS.Kernel.Kernel_Handle;
      Project : Project_Type;
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

   --------------------
   -- Menu Callbacks --
   --------------------

   type Initialize_Debugger_Command is new Interactive_Command with record
      Project : Project_Type;
      Exec    : Virtual_File;
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

   type Continue_Until_Line_Command is new Interactive_Command
   with record
      File : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Line : Integer := -1;
   end record;
   overriding function Execute
     (Command : access Continue_Until_Line_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Continue until current line

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

   type On_Executable_Changed is new Debugger_Hooks_Function with null record;
   overriding procedure Execute
     (Self     : On_Executable_Changed;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class);
   --  Hook for "debugger_executable_changed"

   ----------------
   -- Contextual --
   ----------------

   type Debugger_Inactive_Or_Stopped_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Inactive_Or_Stopped_Filter;
      Context : Selection_Context) return Boolean;
   --  True if the debugger has not been started or now idle waiting for new
   --  commands.

   type Debugger_Stopped_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Stopped_Filter;
      Context : Selection_Context) return Boolean;
   --  True if the debugger has been started but is now idle waiting for new
   --  commands.

   type Debuggee_Started_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Debuggee_Started_Filter;
      Context : Selection_Context) return Boolean;
   --  True if the debuggee has been started

   type Debugger_Active_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Active_Filter;
      Context : Selection_Context) return Boolean;

   type Printable_Variable_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Printable_Variable_Filter;
      Context : Selection_Context) return Boolean;

   type Subprogram_Variable_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Subprogram_Variable_Filter;
      Context : Selection_Context) return Boolean;

   type In_Debugger_Frame_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access In_Debugger_Frame_Filter;
      Context : Selection_Context) return Boolean;

   type Not_Command_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Not_Command_Filter;
      Context : Selection_Context) return Boolean;

   type Set_Value_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Set_Value_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Set_Watchpoint_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Set_Watchpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Show_Location_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Show_Location_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   ------------------
   -- Add_Debugger --
   ------------------

   procedure Add_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Object : not null access Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Kernel);

      Process : Visual_Debugger_Record'Class renames
        Visual_Debugger_Record'Class (Object.all);
   begin
      Process.Debugger_Num :=
        Natural (GVD_Module_ID.Debugger_List.Length) + 1;

      GVD_Module_ID.Debugger_List.Prepend (Object);
   end Add_Debugger;

   ---------------------
   -- Remove_Debugger --
   ---------------------

   procedure Remove_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Object : not null access Base_Visual_Debugger'Class)
   is
      Cursor : Debugger_Lists.Cursor :=
        GVD_Module_ID.Debugger_List.Find (Object);
      Prev   : constant Debugger_Lists.Cursor :=
        Debugger_Lists.Previous (Cursor);
      Next   : constant Debugger_Lists.Cursor :=
        Debugger_Lists.Next (Cursor);
   begin
      if not Debugger_Lists.Has_Element (Cursor) then
         --  Should never happen
         return;
      end if;

      if Debugger_Lists.Has_Element (Prev) then
         Set_Current_Debugger (Kernel, Debugger_Lists.Element (Prev));
      elsif Debugger_Lists.Has_Element (Next) then
         Set_Current_Debugger (Kernel, Debugger_Lists.Element (Next));
      else
         Set_Current_Debugger (Kernel, null);
      end if;

      GVD_Module_ID.Debugger_List.Delete (Cursor);
   end Remove_Debugger;

   -----------------------
   -- For_Each_Debugger --
   -----------------------

   procedure For_Each_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Action : access procedure
        (Object : not null access Base_Visual_Debugger'Class))
   is
      pragma Unreferenced (Kernel);
   begin
      for J of GVD_Module_ID.Debugger_List loop
         Action (J);
      end loop;
   end For_Each_Debugger;

   -----------------------------
   -- Count_Running_Debuggers --
   -----------------------------

   function Count_Running_Debuggers
     (Kernel : not null access Kernel_Handle_Record'Class)
     return Natural
   is
      pragma Unreferenced (Kernel);
   begin
      return Natural (GVD_Module_ID.Debugger_List.Length);
   end Count_Running_Debuggers;

   --------------------------
   -- Get_Current_Debugger --
   --------------------------

   function Get_Current_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return access Base_Visual_Debugger'Class
   is
      pragma Unreferenced (Kernel);
   begin
      return GVD_Module_ID.Current_Debugger;
   end Get_Current_Debugger;

   --------------------------
   -- Set_Current_Debugger --
   --------------------------

   procedure Set_Current_Debugger
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Current : access Base_Visual_Debugger'Class)
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
         Get_Project (Kernel),
         No_File,
         Args);
   end Initialize_Debugger;

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
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));

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
      Process      : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
      List         : Process_List;
      Dummy       : Message_Dialog_Buttons;
      pragma Unreferenced (Command);

   begin
      if Process = null or else Process.Debugger = null then
         return Commands.Failure;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Dummy := Message_Dialog
           ((-"Cannot attach to a task/process while the") & ASCII.LF &
            (-"underlying debugger is busy.") & ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
            Dialog_Type => Warning,
            Buttons     => Button_OK,
            Parent      => Kernel.Get_Main_Window);
         return Commands.Failure;
      end if;

      Gtk_New (List, Process);

      declare
         Argument : constant String := List.Get_Selection;
      begin
         List.Destroy;

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
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
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
            Buttons     => Button_OK,
            Parent      => Kernel.Get_Main_Window);

      else
         Detach_Process (Process.Debugger, Mode => GVD.Types.Visible);
      end if;
      return Commands.Success;
   end Execute;

   -----------------
   -- Execute_Dbg --
   -----------------

   overriding function Execute_Dbg
     (Command : access Step_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Step_Into (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   -----------------
   -- Execute_Dbg --
   -----------------

   overriding function Execute_Dbg
     (Command : access Stepi_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Step_Into_Instruction (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   -----------------
   -- Execute_Dbg --
   -----------------

   overriding function Execute_Dbg
     (Command : access Next_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Step_Over (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   -----------------
   -- Execute_Dbg --
   -----------------

   overriding function Execute_Dbg
     (Command : access Nexti_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Step_Over_Instruction (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   -----------------
   -- Execute_Dbg --
   -----------------

   overriding function Execute_Dbg
     (Command : access Finish_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin

      Finish (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   -----------------
   -- Execute_Dbg --
   -----------------

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

   overriding function Execute
     (Command : access Continue_Until_Line_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Process : constant Visual_Debugger :=
                  Visual_Debugger (Get_Current_Debugger (Kernel));
      File    : constant Virtual_File :=
                  (if Command.File /= GNATCOLL.VFS.No_File then
                      Command.File
                   else
                      File_Information (Context.Context));
      Line    : constant Editable_Line_Type := Editable_Line_Type
        ((if Command.Line > 0 then
            Command.Line
         elsif Has_File_Line_Information (Context.Context) then
            File_Line_Information (Context.Context)
         else
            GPS.Kernel.Contexts.Line_Information (Context.Context)));
   begin
      Continue_Until_Location
        (Process.Debugger,
         File => File,
         Line => Line,
         Mode => GVD.Types.Visible);

      return Commands.Success;
   end Execute;

   -----------------
   -- Execute_Dbg --
   -----------------

   overriding function Execute_Dbg
     (Command : access Kill_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Kill_Process (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   -----------------
   -- Execute_Dbg --
   -----------------

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

   -------------------
   -- Start_Program --
   -------------------

   procedure Start_Program
     (Process : Visual_Debugger; Start_Cmd : Boolean := False)
   is
      On_Vx_56 : constant Boolean :=
        VxWorks_Version (Process.Debugger) in Vx5 .. Vx6;

      Dialog       : GPS_Dialog;
      Args         : Combo_Box;
      Is_Start     : Gtk_Check_Button;
      Is_Multitask : Gtk_Check_Button;
      Use_Exec_Dir : Gtk_Check_Button;
   begin
      Gtk_New
        (Dialog,
         Title  => -"Run/Start",
         Flags  => Destroy_With_Parent or Modal,
         Kernel => Process.Kernel);
      Dialog.Add_OK_Cancel;

      Args := Dialog.Add_Combo
        (Message => (if On_Vx_56
                     then -"Entry point and arguments:"
                     else -"Run arguments:"),
         Key     => Run_Arguments_History_Key);

      --  If the user has already requested to stop at the beginning (Start
      --  command) do not ask the same question again. Otherwise, we enable
      --  a checkbox so that the user can select whether he/she wants to
      --  stop at the beginning of the main program.

      if not Start_Cmd then
         Is_Start := Dialog.Add_Check_Button
           (Message => -"Stop at beginning of main subprogram",
            Key     => "stop_beginning_debugger");
      end if;

      --  If we are debugging on VxWorks we ask for the entry point to be
      --  executed, and we enable the multi-tasks-mode checkbox.

      if On_Vx_56 then
         Is_Multitask := Dialog.Add_Check_Button
           (Message => -"Enable VxWorks multi-tasks mode",
            Key     => "multitask_mode_debugger");
      else
         Use_Exec_Dir := Dialog.Add_Check_Button
           (Message => -"Use exec dir instead of current dir",
            Key     => "run_in_executable_directory");
      end if;

      Dialog.Show_All;

      if Dialog.Run = Gtk_Response_OK then
         if Is_Multitask /= null then
            Process.Debugger.Send
              ("set multi-tasks-mode "
               & (if Is_Multitask.Get_Active then "on" else "off"),
               Mode => GVD.Types.Hidden);
         end if;

         if Use_Exec_Dir /= null and then Use_Exec_Dir.Get_Active then
            Process.Debugger.Change_Directory (Process.Descriptor.Program.Dir);
         end if;

         declare
            A : constant String := Strip_Ending_Linebreaks (Args.Get_Text);
         begin
            if Is_Start = null or else Is_Start.Get_Active then
               Process.Debugger.Start (A, Mode => GVD.Types.Visible);
            else
               Process.Debugger.Run (A, Mode => GVD.Types.Visible);
            end if;
         end;
      end if;

      Dialog.Destroy;
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
        Visual_Debugger (Get_Current_Debugger (Kernel));
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
            Buttons     => Button_OK,
            Parent      => Kernel.Get_Main_Window);
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
      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      Top      : constant GPS_Window :=
                       GPS_Window (Get_Main_Window (Kernel));
      Process  : constant Visual_Debugger :=
                       Get_Current_Debugger (Kernel);
      Continue : Boolean := True;
      pragma Unreferenced (Command);

      function Display_Confirmation_Dialog return Boolean;
      --  Display a confirmation dialog if the debugger is already connected to
      --  a target.
      --  Return True if the user wants to continue, False otherwise.

      ---------------------------------
      -- Display_Confirmation_Dialog --
      ---------------------------------

      function Display_Confirmation_Dialog return Boolean
      is
         Response : Message_Dialog_Buttons;
      begin
         Response := Message_Dialog
           (Msg         =>
              "The debugger is already connected to a target."
            & ASCII.LF
            & ASCII.LF
            & "Do you want to disconnect it and start a new connection?",
            Buttons     => Button_Yes or Button_No,
            Dialog_Type => Confirmation,
            Parent      => Gtk_Window (Top));

         return Response /= Button_No;
      end Display_Confirmation_Dialog;

   begin
      --  Display a confirmation dialog if the debugger is already connected
      --  to a target.
      if Process.Debugger.Is_Connected_To_Target then
         Continue := Display_Confirmation_Dialog;
      end if;

      if Continue then
         declare
            Remote_Target   : constant String :=
                                Process.Debugger.Get_Remote_Target;
            Remote_Protocol : constant String :=
                                Process.Debugger.Get_Remote_Protocol;
         begin
            --  Try to connect only if the remote target and a protocol have
            --  been specified.

            if Remote_Target /= "" and then Remote_Protocol /= "" then
               Connect_To_Target
                 (Process.Debugger,
                  Target   => Remote_Target,
                  Protocol => Remote_Protocol,
                  Force    => True,
                  Mode     => GVD.Types.Visible);
            end if;
         end;
      end if;

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

      Kernel      : constant Kernel_Handle := Get_Kernel (Context.Context);
      Process     : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
      Exec        : Virtual_File;
      Ptr         : GNAT.Strings.String_Access :=
        GNAT.OS_Lib.Get_Executable_Suffix;
      Exec_Suffix : constant String := Ptr.all;

   begin
      GNAT.Strings.Free (Ptr);

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
            Set_Executable (Process.Debugger, S);

            --  Load the executable to the remote target if we are connected
            --  remotely.
            Load_Executable
              (Process.Debugger,
               Executable => S.To_Remote (Get_Nickname (Debug_Server)),
               Mode       => Visible);
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
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));

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

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Show_Location_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
      Name : constant Virtual_File := Process.Current_File;
   begin
      if Name /= GNATCOLL.VFS.No_File then
         Goto_Current_Line (Kernel, Process);
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
        Visual_Debugger (Get_Current_Debugger (Get_Kernel (Context.Context)));
      Variable : constant String :=
                   Get_Variable_Name (Context.Context, False);
      S        : constant String :=
                   Display_Text_Input_Dialog
                     (Kernel   => Process.Kernel,
                      Title    => -"Setting value of " & Variable,
                      Message  => -"Setting value of " & Variable & ':',
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
     (Command : access Set_Watchpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Process  : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Get_Kernel (Context.Context)));
      Variable : constant String := Get_Variable_Name (Context.Context, False);
      Id : Breakpoint_Identifier;
      pragma Unreferenced (Command, Id);
   begin
      Id := Process.Debugger.Watch
         (Name    => Variable,
          Trigger => Write,
          Mode    => GVD.Types.Visible);
      return Commands.Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Inactive_Or_Stopped_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Get_Kernel (Context)));
   begin
      return Process = null
        or else Process.Debugger = null
        or else not Command_In_Process (Process);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Stopped_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Get_Kernel (Context)));
   begin
      return Process /= null and then not Command_In_Process (Process);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Debuggee_Started_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Get_Kernel (Context)));
   begin
      return Process /= null
        and then Process.Debugger /= null
        and then Process.Debugger.Is_Started;
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
        Visual_Debugger (Get_Current_Debugger (Get_Kernel (Context)));
   begin
      return Process /= null and then Process.Debugger /= null;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Printable_Variable_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);

      Start_Iter, End_Iter     : Gtk_Text_Iter;
      Entity_Start, Entity_End : Gtk_Text_Iter;
   begin
      if Has_Entity_Name_Information (Context) then
         declare
            Entity : constant Root_Entity'Class := Get_Entity (Context);
         begin
            return Is_Fuzzy (Entity)
              or else Is_Printable_In_Debugger (Entity);
         end;

      elsif Has_Debugging_Variable (Context) then
         return True;

      elsif Has_Area_Information (Context) then
         --  check whether the selection contains only a set
         --  of printable entities

         Get_Area (Context, Start_Iter, End_Iter);
         if Get_Line (Start_Iter) /= Get_Line (End_Iter) then
            return False;
         end if;

         Copy (Source => End_Iter, Dest => Entity_Start);
         declare
            Db     : constant General_Xref_Database :=
              Get_Kernel (Context).Databases;
         begin
            Search_Entity_Bounds
              (Entity_Start, Entity_End, Maybe_File => False);

            if Get_Offset (Entity_End) /= Get_Offset (End_Iter)
              or else Get_Offset (Entity_Start) < Get_Offset (Start_Iter)
            then
               --  the selection contains only a part of an entity
               return False;
            end if;

            declare
               Entity      : Xref.Root_Entity_Ref;
               Closest_Ref : Root_Entity_Reference_Ref;
            begin
               --  getting an entity
               Entity.Replace_Element
                 (Db.Get_Entity
                    (Loc  =>
                         (File         => File_Information (Context),
                          Project_Path =>
                            Project_Information (Context).Project_Path,
                          Line         => Integer
                            (Get_Line (Entity_Start)) + 1,
                          Column       => Visible_Column_Type
                            (Get_Line_Offset (Entity_Start) + 1)),
                     Name => Get_Text (Entity_Start, Entity_End),
                     Closest_Ref => Closest_Ref,
                     Approximate_Search_Fallback => True));

               if Entity.Is_Empty then
                  --  corresponding entity is not found
                  return False;
               else
                  if not Is_Fuzzy (Entity.Element)
                    and then not Is_Printable_In_Debugger (Entity.Element)
                  then
                     --  inappropriate entity
                     return False;
                  end if;
               end if;
            end;

            --  check whether an entity is selected from its beginning
            declare
               Lang : constant Language.Language_Access :=
                 Get_Language_From_File
                   (Get_Language_Handler (Get_Kernel (Context)),
                    File_Information (Context));
               Begin_Of_Line, End_Of_Line : Gtk_Text_Iter;
               Success                    : Boolean;
            begin
               Copy (Source => End_Iter, Dest => End_Of_Line);
               Copy (Source => Start_Iter, Dest => Begin_Of_Line);
               Forward_To_Line_End (End_Of_Line, Success);
               Set_Line (Begin_Of_Line, Get_Line (Start_Iter));

               return Text_Information (Context) = Parse_Reference_Backwards
                 (Lang,
                  Buffer       => Get_Text (Begin_Of_Line, End_Of_Line),
                  Start_Offset =>
                    String_Index_Type (Get_Line_Index (Entity_End)));
            end;
         end;
      end if;

      return False;
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
            return Is_Fuzzy (Entity) or else Is_Subprogram (Entity);
         end;
      end if;
      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access In_Debugger_Frame_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context);
      Process : constant Visual_Debugger := Visual_Debugger
        (Get_Current_Debugger (Kernel));

      function In_Debugger_Frame
        (Buffer : Editor_Buffer'Class;
         Line   : Natural)
         return Boolean;
      --  Return True if the specified location is in the same frame as
      --  the debugger's current location.

      -----------------------
      -- In_Debugger_Frame --
      -----------------------

      function In_Debugger_Frame
        (Buffer : Editor_Buffer'Class;
         Line   : Natural)
         return Boolean
      is
         Debugger_Subprogram : constant String := Block_Name
           (This        => Buffer.New_Location (Process.Current_Line, 0),
            Subprogram  => True);
         New_Loc_Subprogram : constant String := Block_Name
           (This        => Buffer.New_Location (Line, 0),
            Subprogram  => True);
      begin
         return Debugger_Subprogram = New_Loc_Subprogram;
      end In_Debugger_Frame;

   begin
      if Process = null then
         return False;
      end if;

      if Has_File_Information (Context)
        and then Has_Line_Information (Context)
      then
         declare
            File   : constant GNATCOLL.VFS.Virtual_File :=
                       File_Information (Context);
            Line   : constant Natural := Contexts.Line_Information (Context);
            Buffer : constant Editor_Buffer'Class :=
                       Kernel.Get_Buffer_Factory.Get
                       (File        => File,
                        Open_View   => False);
         begin
            if Process.Current_File = File
              and then Process.Current_Line /= Line
              and then Buffer /= Nil_Editor_Buffer
            then
               return In_Debugger_Frame (Buffer, Line);
            end if;
         end;
      end if;

      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Not_Command_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      use Ada.Strings.Unbounded;

   begin
      if GPS.Kernel.Contexts.Has_Debugging_Variable (Context)
        and then Get_Variable (Context).Cmd /= ""
      then
         return False;
      else
         return True;
      end if;
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
        Visual_Debugger (Get_Current_Debugger (Kernel));
      Value    : GNAT.Strings.String_Access;
      Pretty   : GNAT.Strings.String_Access;
      Output   : GNAT.Strings.String_Access;
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
         Variable      : Item_Info       := Wrap_Variable (Variable_Name);
      begin
         if Variable_Name = ""
           or else not Can_Tooltip_On_Entity
             (Get_Language (Debugger.Debugger), Variable_Name)
         then
            return null;

         else
            --  Retrieve the debugger output
            Value := new String'(Value_Of (Debugger.Debugger, Variable_Name));
         end if;

         if Value.all /= "" then
            Update (Variable, Debugger);
            --  Compute the output pretty printed by the variables view
            Pretty := new String'(Variable.Entity.Get_Type.Get_Advanced_Value);

            --  Choose the appropriate output
            if Pretty.all /= "" then
               Output := new String'(Pretty.all);
            else
               Output := new String'(Value.all);
            end if;
            GNAT.Strings.Free (Pretty);

            Gtk_New (Label, "<b>Debugger value:</b> " & Output.all);
            GNAT.Strings.Free (Output);
            --  If the tooltips is too long wrap it
            Label.Set_Line_Wrap (True);
            Label.Set_Max_Width_Chars (80);
            Label.Set_Use_Markup (True);
            Label.Modify_Font (View_Fixed_Font.Get_Pref);
            Label.Set_Alignment (0.0, 0.5);
            W := Gtk_Widget (Label);
         else
            --  Note: if Value.all is "", we will return Pixmap below, hence
            --  the assignment.

            W := null;
         end if;

         GNAT.Strings.Free (Value);
         Free (Variable);
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
      Project : Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : String)
   is
      Ignore : Visual_Debugger;
      pragma Unreferenced (Ignore);
   begin
      Ignore := Spawn
         (Kernel          => Kernel,
          Prefered_Kind   => Debugger_Kind.Get_Pref,
          File            => File,
          Project         => Project,
          Args            => Args,
          Load_Executable => Load_Executable_On_Init.Get_Pref);
      Kernel.Refresh_Context;
   end Debug_Init;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Initialize_Debugger_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      Debug_Init
         (Get_Kernel (Context.Context), Command.Project, Command.Exec, "");
      return Commands.Success;
   end Execute;

   ---------------------
   -- Debug_Terminate --
   ---------------------

   procedure Debug_Terminate (Kernel : Kernel_Handle) is
      List : array (1 .. Natural (GVD_Module_ID.Debugger_List.Length)) of
        Base_Visual_Debugger_Access;
      Index : Positive := 1;
   begin
      for J of GVD_Module_ID.Debugger_List loop
         List (Index) := J;
         Index := Index + 1;
      end loop;

      for J of List loop
         Close_Debugger (Visual_Debugger (J));
      end loop;

      Kernel.Refresh_Context;
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
      Close_Debugger (Visual_Debugger (Get_Current_Debugger (Kernel)));
      Update_Menus_And_Buttons (Kernel);
      return Commands.Success;

   exception
      when E : others =>
         Trace (Me, E);
         return Commands.Failure;
   end Execute;

   -----------------
   -- Execute_Dbg --
   -----------------

   overriding function Execute_Dbg
     (Command : access Up_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Stack_Up (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   -----------------
   -- Execute_Dbg --
   -----------------

   overriding function Execute_Dbg
     (Command : access Down_Command;
      Process : Visual_Debugger) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Stack_Down (Process.Debugger, Mode => GVD.Types.Visible);
      return Commands.Success;
   end Execute_Dbg;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Executable_Changed;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self);
      Process : constant Visual_Debugger := Visual_Debugger (Debugger);
   begin
      --  Change the project to match the executable

      Load_Project_From_Executable (Kernel, Process);

      --  Verify the language used in the executable

      Detect_Language (Process.Debugger);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self   : On_View_Changed;
       Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);

      procedure Create_Action_And_Menu
        (Prj : Project_Type; Main : Virtual_File);
      --  Create the action and menu to initialize a specific executable

      Mains : Any_Type :=
        Compute_Build_Targets_Hook.Run (Kernel, "executable");

      Show_Project_In_Menu : constant Boolean :=
        Group_Mains_Into_Projects (Kernel, Mains.Length);

      ----------------------------
      -- Create_Action_And_Menu --
      ----------------------------

      procedure Create_Action_And_Menu
        (Prj : Project_Type; Main : Virtual_File)
      is
         Main_Name : constant String :=
           (if Main = No_File then
               -"no main file"
            else
               Escape_Underscore (Escape_Menu_Name (Main.Display_Base_Name)));

         Action  : constant String :=
            "debug initialize " & Prj.Name & ":" & Main_Name;
         Menu    : constant String :=
           "/Debug/Initialize/"
           & (if not Show_Project_In_Menu or else Main = No_File
              then "" else Escape_Underscore (Prj.Name) & '/')
           & Main_Name;
         Command : Interactive_Command_Access;
      begin
         Command := new Initialize_Debugger_Command'
           (Interactive_Command with
            Project => Prj,
            Exec    => Main);
         GVD_Module_ID.Actions.Append (Action);

         Register_Action
           (Kernel, Action, Command,
            (if Main /= No_File
             then (-"Initialize the debugger on the file "
               & Main.Display_Full_Name)
             else -"Initialize the debugger, no file specified"),
            Category => -"Debug");
         Register_Menu (Kernel, Menu, Action => Action);
      end Create_Action_And_Menu;

   begin
      for A of GVD_Module_ID.Actions loop
         Unregister_Action (Kernel, A, Remove_Menus_And_Toolbars => True);
      end loop;
      GVD_Module_ID.Actions.Clear;

      for J in 1 .. Mains.Length loop
         if Mains.List (J).Length /= 0 then
            declare
               Main : constant Virtual_File :=
                  Create (+Mains.List (J).Tuple (2).Str);
               Prj  : constant Virtual_File :=
                  Create (+Mains.List (J).Tuple (3).Str);
               P    : constant Project_Type :=
                  Kernel.Registry.Tree.Project_From_Path (Prj);
            begin
               Create_Action_And_Menu (P, Main);
            end;
         end if;
      end loop;

      Free (Mains);

      --  Specific entry to start the debugger without any main program.
      --  We need to pass the root project so that Ide'debugger_command is
      --  found.

      Create_Action_And_Menu (Get_Project (Kernel), No_File);

   exception
      when E : others =>
         Trace (Me, E);
         Debug_Terminate (Kernel_Handle (Kernel));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self         : On_Location_Changed;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Column : Integer;
      Project      : GNATCOLL.Projects.Project_Type)
   is
      pragma Unreferenced (Self, Column, Project);
      Process                 : constant Visual_Debugger := Visual_Debugger
        (Get_Current_Debugger (Kernel));
      Buffer                  : constant Editor_Buffer'Class :=
                                  Kernel.Get_Buffer_Factory.Get
                                    (File        => File,
                                     Open_View   => False);
      Continue_To_Line_Filter : constant Action_Filter :=
                                  Lookup_Filter
                                    (Kernel,
                                     Name => "Can continue until");
   begin
      --  Do nothing if there is no active debugger or if the debugger's
      --  current file is different from the new location's one.
      if Process = null or else Process.Current_File /= File then
         return;
      end if;

      --  Create the side area colum that will display the "Continue to line"
      --  clickable icons if needed.
      Create_Continue_To_Line_Columns (Kernel, Buffer);

      --  Remove the previous message
      Remove_Continue_To_Line_Messages (Kernel);

      --  Add a "Continue to line" clickable icon if the context allows it
      if Filter_Matches_Primitive
        (Continue_To_Line_Filter, Context => Kernel.Get_Current_Context)
      then
         declare
            Msg       : Simple_Message_Access;
            Help_Text : constant String :=
                          "Continue to line " & Integer'Image (Line);
            use Ada.Strings.Unbounded;
         begin
            Msg := Create_Simple_Message
              (Get_Messages_Container (Kernel),
               Category                 =>
                 Messages_Category_Continue_To_Line,
               File                     => File,
               Line                     => Line,
               Column                   => 1,
               Text                     => Help_Text,
               Importance               => Unspecified,
               Flags                    => Continue_To_Line_Messages_Flags,
               Allow_Auto_Jump_To_First => False);

            Msg.Set_Action
              (new Line_Information_Record'
                 (Text               => Null_Unbounded_String,
                  Tooltip_Text       => To_Unbounded_String (Help_Text),
                  Image              => To_Unbounded_String
                    ("gps-debugger-continue-until"),
                  Message            => <>,
                  Associated_Command => new Continue_Until_Line_Command'
                    (Root_Command with
                     File => File,
                     Line => Line)));
         end;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference) is
      pragma Unreferenced (Self);
   begin
      if Pref = Preference (Continue_To_Line_Buttons) then
         declare
            Process : constant Visual_Debugger := Visual_Debugger
              (Get_Current_Debugger (Kernel));
         begin
            if Process = null then
               return;
            end if;

            Disable_Continue_To_Line_On_Editors (Kernel);

            if Continue_To_Line_Buttons.Get_Pref then
               declare
                  Func    : constant access File_Location_Hooks_Function'Class
                    := new On_Location_Changed;
                  Context : constant Selection_Context :=
                           Kernel.Get_Current_Context;
               begin
                  --  Add the hook function that will monitor the debugging
                  --  context to check if we can add the "Continue to line"
                  --  clickable icon.
                  Location_Changed_Hook.Add (Func);

                  --  Execute it with the current editor's location, if any.
                  if Has_File_Information (Context)
                    and then Has_Line_Information (Context)
                  then
                     Func.Execute
                       (Kernel  => Kernel,
                        File    => File_Information (Context),
                        Line    => Contexts.Line_Information (Context),
                        Column  => 0);
                  end if;
               end;
            end if;
         end;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Debugger_Started;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self, Debugger, Kernel);
   begin
      if Continue_To_Line_Buttons.Get_Pref then

         --  Add the hook function that will monitor the debugging context
         --  to check if we can add the "Continue to line" clickable icon.
         Location_Changed_Hook.Add (new On_Location_Changed);

      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Debugger_Location_Changed;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class) is
      pragma Unreferenced (Self, Debugger);
   begin
      Remove_Continue_To_Line_Messages (Kernel);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Debugger_Terminated;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self, Debugger);
   begin
      Disable_Continue_To_Line_On_Editors (Kernel);
   end Execute;

   -------------------------------------
   -- Create_Continue_To_Line_Columns --
   -------------------------------------

   procedure Create_Continue_To_Line_Columns
     (Kernel : not null access Kernel_Handle_Record'Class;
      Buffer : Editor_Buffer'Class) is
   begin
      if Buffer /= Nil_Editor_Buffer
        and then not  Buffer.Has_Information_Column
          (Messages_Category_Continue_To_Line)
      then
         Create_Line_Information_Column
           (Kernel     => Kernel,
            File       => Buffer.File,
            Identifier => Messages_Category_Continue_To_Line);
      end if;
   end Create_Continue_To_Line_Columns;

   --------------------------------------
   -- Remove_Continue_To_Line_Messages --
   --------------------------------------

   procedure Remove_Continue_To_Line_Messages
     (Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      Get_Messages_Container (Kernel).Remove_Category
        (Messages_Category_Continue_To_Line,
         Continue_To_Line_Messages_Flags);
   end Remove_Continue_To_Line_Messages;

   -----------------------------------------
   -- Disable_Continue_To_Line_On_Editors --
   -----------------------------------------

   procedure Disable_Continue_To_Line_On_Editors
     (Kernel : not null access Kernel_Handle_Record'Class) is
      Buffers : constant Buffer_Lists.List :=
                  Kernel.Get_Buffer_Factory.Buffers;

      function Is_Location_Changed_Function
        (F : not null access Hook_Function'Class) return Boolean
      is (F.all in On_Location_Changed'Class);
   begin
      --  We don't need to monitor the debugging context anymore so remove
      --  the Location_Changed hook function.
      Location_Changed_Hook.Remove
        (Is_Location_Changed_Function'Access);

      --  Remove the extra column we may have added on editors
      for Buffer of Buffers loop
         if Buffer /= Nil_Editor_Buffer
           and Buffer.Has_Information_Column
             (Messages_Category_Continue_To_Line)
         then
            Remove_Line_Information_Column
              (Kernel     => Kernel,
               File       => Buffer.File,
               Identifier => Messages_Category_Continue_To_Line);
         end if;
      end loop;
   end Disable_Continue_To_Line_On_Editors;

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
      Debugger_Filter           : Action_Filter;
      Debuggee_Started          : Action_Filter;
      Debugger_Active           : Action_Filter;
      Printable_Filter          : Action_Filter;
      Subprogram_Filter         : Action_Filter;
      Is_Not_Command_Filter     : Action_Filter;
      Continue_Until_Filter     : Action_Filter;
      Set_Value_Filter          : Action_Filter;
   begin
      Create_GVD_Module (Kernel);
      GVD.Preferences.Register_Default_Preferences (Get_Preferences (Kernel));
      GVD.Scripts.Create_Hooks (Kernel);

      Debugger_Filter := new Debugger_Inactive_Or_Stopped_Filter;
      Register_Filter
        (Kernel, Debugger_Filter, "Debugger inactive or stopped");

      Debugger_Filter := new Debugger_Stopped_Filter;
      Register_Filter (Kernel, Debugger_Filter, "Debugger stopped");

      Debuggee_Started := new Debuggee_Started_Filter;
      Register_Filter (Kernel, Debuggee_Started, "Debuggee started");

      Debugger_Active := new Debugger_Active_Filter;
      Register_Filter (Kernel, Debugger_Active, "Debugger active");

      Printable_Filter := new Printable_Variable_Filter;
      Register_Filter
        (Kernel, Printable_Filter, "Debugger printable variable");

      Subprogram_Filter := new Subprogram_Variable_Filter;
      Register_Filter
        (Kernel, Subprogram_Filter, "Debugger subprogram");

      Is_Not_Command_Filter := new Not_Command_Filter;
      Register_Filter
        (Kernel, Is_Not_Command_Filter, "Debugger not command variable");

      Continue_Until_Filter :=
        Debuggee_Started and new In_Debugger_Frame_Filter;
      Register_Filter
        (Kernel, Continue_Until_Filter, "Can continue until");

      Set_Value_Filter := Debugger_Filter and Is_Not_Command_Filter and
        Printable_Filter;
      Register_Filter
        (Kernel, Set_Value_Filter, "Debugger set value");

      Register_Contextual_Submenu
        (Kernel, "Debug", Ref_Item => "References");

      Register_Action
        (Kernel, "debug set value",
         Command     => new Set_Value_Command,
         Description => "Modify the value of the variable",
         Filter      => Set_Value_Filter,
         Category    => -"Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Debug/Set value of %S",
         Action => "debug set value");

      Register_Action
        (Kernel, "debug set watchpoint",
         Command     => new Set_Watchpoint_Command,
         Description =>
            -("Set a watchpoint on the variable. The debugger will stop every"
              & " time the variable's value is changed"),
         Filter      => Debugger_Filter and Is_Not_Command_Filter and
           Printable_Filter,
         Category    => -"Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Debug/Set watchpoint on %S",
         Action => "debug set watchpoint");

      Register_Action
        (Kernel, "debug show current location",
         Command     => new Show_Location_Command,
         Description => "Display the current debugger location in an editor",
         Filter      => Debugger_Filter,
         Category    => -"Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Debug/Show current location",
         Action => "debug show current location");

      --  Add hooks' callbacks

      Project_View_Changed_Hook.Add (new On_View_Changed);
      Preferences_Changed_Hook.Add (new On_Pref_Changed);
      Debugger_Started_Hook.Add (new On_Debugger_Started);
      Debugger_Location_Changed_Hook.Add (new On_Debugger_Location_Changed);
      Debugger_Terminated_Hook.Add (new On_Debugger_Terminated, Last => False);
      Debugger_Executable_Changed_Hook.Add (new On_Executable_Changed);

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

      GVD.Consoles.Register_Module (Kernel);

      Register_Action
        (Kernel, "debug run dialog", new Start_Command,
         Filter      => Debugger_Active,
         Description =>
           -"Choose the arguments to the program, and start running it",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug step", new Step_Command,
         Icon_Name    => "gps-debugger-step-symbolic",
         Filter       => Debugger_Active,
         Description  =>
           -"Execute until program reaches a new line of source code",
         Category     => -"Debug",
         For_Learning => True);

      Register_Action
        (Kernel, "debug stepi", new Stepi_Command,
         Filter      => Debugger_Active,
         Description =>
           -"Execute the program for one machine instruction only",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug next", new Next_Command,
         Icon_Name    => "gps-debugger-next-symbolic",
         Filter       => Debugger_Active,
         Description  =>
           -("Execute the program until the next source line, stepping over"
             & " subprogram calls"),
         Category     => -"Debug",
         For_Learning => True);

      Register_Action
        (Kernel, "debug nexti", new Nexti_Command,
         Filter      => Debugger_Active,
         Description =>
           -("Execute the program until the next machine instruction, stepping"
             & " over subprogram calls"),
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug finish", new Finish_Command,
         Icon_Name    => "gps-debugger-finish-symbolic",
         Filter       => Debugger_Active,
         Description  =>
           -("Continue execution until selected stack frame returns"),
         Category     => -"Debug",
         For_Learning => True);

      Register_Action
        (Kernel, "debug continue", new Continue_Command,
         Icon_Name    => "gps-debugger-run-symbolic",
         Filter       => Debugger_Active,
         Description  =>
           -("Continue execution until next breakpoint." & ASCII.LF
           & "Start the debugger if not started yet"),
         Category     => -"Debug",
         For_Learning => True);

      Register_Action
        (Kernel, "debug continue until",
         new Continue_Until_Line_Command,
         Filter       => Continue_Until_Filter,
         Description  =>
           -("Continue execution until the given line."),
         Category     => -"Debug",
         For_Learning => True);
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Debug/Continue until line %l",
         Action => "debug continue until line");

      Register_Action
        (Kernel, "debug up", new Up_Command,
         Icon_Name   => "gps-debugger-up-symbolic",
         Filter      => Debugger_Active,
         Description => "Move up one frame",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug down", new Down_Command,
         Icon_Name   => "gps-debugger-down-symbolic",
         Filter      => Debugger_Active,
         Description => "Move down one frame",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug interrupt", new Interrupt_Command,
         Icon_Name    => "gps-debugger-pause-symbolic",
         Filter       => Debugger_Active,
         Description  => -"Asynchronously interrupt the debuggee program",
         Category     => -"Debug",
         For_Learning => True);

      Register_Action
        (Kernel, "terminate debugger", new Terminate_Command,
         Icon_Name   => "gps-debugger-terminate-symbolic",
         Description => -"Terminate the current debugger",
         Filter      => Debugger_Active);

      Register_Action
        (Kernel, "terminate all debuggers", new Terminate_All_Command,
         Description => -"Terminate all running debugger",
         Filter      => Debugger_Active);
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
