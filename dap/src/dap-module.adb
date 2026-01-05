------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2022-2026, AdaCore                     --
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

with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

with GNAT.Strings;
with GNAT.OS_Lib;

with GNATCOLL.Any_Types;           use GNATCOLL.Any_Types;
with GNATCOLL.Traces;              use GNATCOLL.Traces;

with VSS.Characters.Latin;
with VSS.Strings.Conversions;

with Glib;                         use Glib;
with Glib.Main;                    use Glib.Main;
with Glib.Object;                  use Glib.Object;

with Gtk.Check_Button;             use Gtk.Check_Button;
with Gtk.Dialog;                   use Gtk.Dialog;
with Gtk.Label;                    use Gtk.Label;
with Gtk.Widget;                   use Gtk.Widget;

with Gtkada.Dialogs;
with Gtkada.File_Selector;         use Gtkada.File_Selector;

with Default_Preferences;
with Commands;                     use Commands;
with Commands.Interactive;         use Commands.Interactive;
with Histories;
with Language;                     use Language;

with GPS.Dialogs;                  use GPS.Dialogs;
with GPS.Editors;                  use GPS.Editors;
with GPS.Core_Kernels;
with GPS.Kernel.Actions;
with GPS.Kernel.Contexts;          use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;               use GPS.Kernel.MDI;
with GPS.Kernel.Modules;           use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;
with GPS.Kernel.Project;
with GUI_Utils;

with Basic_Types;

with DAP.Contexts;
with DAP.Module.Breakpoints;
with DAP.Modules.Preferences;
with DAP.Modules.Scripts;
with DAP.Clients.Attach;
with DAP.Clients.Evaluate;
with DAP.Clients.Disconnect;
with DAP.Clients.Next;
with DAP.Clients.StepIn;
with DAP.Clients.Stack_Trace;
with DAP.Clients.Variables;
with DAP.Views.Assembly;
with DAP.Views.Call_Stack;
with DAP.Views.Consoles;
with DAP.Views.Threads;
with DAP.Views.Memory;
with DAP.Views.Registers;
with DAP.Views.Variables;
with DAP.Types;
with DAP.Utils;
with String_Utils;
with Xref;

package body DAP.Module is

   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.DAP_MODULE", Off);
   --  Enable/disable DAP support

   Run_Arguments_History_Key : constant Histories.History_Key :=
      "dap_run_arguments";
   --  The key in the history for the arguments to the run command.

   package DAP_Client_Vectors is new Ada.Containers.Vectors
     (Positive, DAP.Clients.DAP_Client_Access, "=" => DAP.Clients."=");
   subtype DAP_Client_Vector is DAP_Client_Vectors.Vector;

   procedure Free is new Ada.Unchecked_Deallocation
     (DAP.Clients.DAP_Client'Class, DAP.Clients.DAP_Client_Access);

   type DAP_Module_Record
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is new
     Module_ID_Record with record
      Dynamic_Actions : Action_Lists.List;
      --  Actions that have been registered dynamically by this module,
      --  for the dynamic menus

      Clients               : DAP_Client_Vector;
      --  Clients that handles DAP requests

      Clients_To_Deallocate : DAP_Client_Vector;
      --  Clients that should be deallocated
      Deallocate_Id          : G_Source_Id := No_Source_Id;
      --  Signal to deallocate clients

      Current_Debuger_ID    : Client_Id_Type := No_Client;
      --  Client that is used as a current active debugger

      Client_ID             : Valid_Client_Id_Type := 1;
      --  Counter to numerate started debuggers
   end record;

   overriding procedure Destroy (Id : in out DAP_Module_Record);

   type DAP_Module is access all DAP_Module_Record'Class;

   function Get_Current_Debugger
     (Id : DAP_Module)
      return DAP.Clients.DAP_Client_Access;

   function Get_Debugger
     (Id  : DAP_Module;
      Num : Client_Id_Type)
      return DAP.Clients.DAP_Client_Access;

   overriding function Tooltip_Handler
     (Module  : access DAP_Module_Record;
      Context : Selection_Context) return Gtk_Widget;
   --  See inherited documentation

   function On_Idle return Boolean;
   --  Called from Gtk on Idle to deallocate clients

   procedure Enable_Continue_To_Line_On_Editors
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Enable the "Continue to line" clickable icons on editors, adding the
   --  necessary hooks to monitor when we should add an additional column
   --  for them and when they should be displayed.

   procedure Disable_Continue_To_Line_On_Editors
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Disable the "Continue to line" clickable icons on editors, removing the
   --  associated messages and the extra column if necessary.

   procedure Create_Continue_To_Line_Columns
     (Kernel : not null access Kernel_Handle_Record'Class;
      Buffer : Editor_Buffer'Class);
   --  Create the column where the "Continue to line" clcikable icons will
   --  be displayed.
   --  Do nothing if the buffer is null or if there is already a column for
   --  these icons.

   -- Hooks callbacks --

   type On_Project_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called every time the project view changes, to recompute the dynamic
   --  menus.

   type On_Before_Exit is new Return_Boolean_Hooks_Function with null record;
   overriding function Execute
     (Self   : On_Before_Exit;
      Kernel : not null access Kernel_Handle_Record'Class) return Boolean;
   --  Called before GNAT Studio exist. This is a good time to store
   --  persistent data.

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);

   -- Filters --

   type Has_Debuggers_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Debuggers_Filter;
      Context : Selection_Context) return Boolean;

   type Debugger_Initialized_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Initialized_Filter;
      Context : Selection_Context) return Boolean;

   type Debugger_Available_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Available_Filter;
      Context : Selection_Context) return Boolean;

   type Debugger_Stopped_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Stopped_Filter;
      Context : Selection_Context) return Boolean;

   type Debuggee_Running_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Debuggee_Running_Filter;
      Context : Selection_Context) return Boolean;

   type No_Debugger_Or_Available_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access No_Debugger_Or_Available_Filter;
      Context : Selection_Context) return Boolean;

   type No_Debugger_Or_Initialized_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access No_Debugger_Or_Initialized_Filter;
      Context : Selection_Context) return Boolean;

   type Breakable_Source_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Breakable_Source_Filter;
      Context : Selection_Context) return Boolean;

   type Entity_Name_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Entity_Name_Filter;
      Context : Selection_Context) return Boolean;

   type Not_Command_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Not_Command_Filter;
      Context : Selection_Context) return Boolean;

   type Attached_Debuggee_Filter  is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Attached_Debuggee_Filter;
      Context : Selection_Context) return Boolean;
   --  Return True when the debugger is attached to a debuggee process.

   type In_Debugger_Frame_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access In_Debugger_Frame_Filter;
      Context : Selection_Context) return Boolean;

   -- Commands --

   type Initialize_Debugger_Command is new Interactive_Command with record
      Project : Project_Type;
      Exec    : Virtual_File;
   end record;
   overriding function Execute
     (Command : access Initialize_Debugger_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Initialize

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

   type Start_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Start_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Run... menu

   type Continue_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Continue_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type;
   --  Debug->Continue menu

   type Next_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Next_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Next menu

   type Nexti_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Nexti_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Next Instruction menu

   type Step_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Step_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Step menu

   type Stepi_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Stepi_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Step Instruction menu

   type Interrupt_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Interrupt_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Interrupt

   type Connect_To_Board_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Connect_To_Board_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Debug->Connect to Board

   type Attach_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Attach_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Debug->Attach

   type Detach_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Detach_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Detach command

   type Load_File_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Load_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Debug->Load File

   -- Hooks --

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

   type On_File_Edited is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when an editor is opened.
   --  Used to create a column in the editor's left-side bar for the
   --  "Continue to line" clickable icons.

   -----------
   -- Utils --
   -----------

   function Debug_Init
     (Kernel          : GPS.Kernel.Kernel_Handle;
      Project         : Project_Type;
      File            : GNATCOLL.VFS.Virtual_File;
      Executable_Args : String;
      Remote_Target   : String)
      return DAP.Clients.DAP_Client_Access;
   --  Initialize the debugger with the executable refered by File/Project.
   --  Executable_Args contain the extra arguments that will be passed to the
   --  debugged executable.

   DAP_Module_Name  : constant String := "DAP";
   DAP_Module_ID    : DAP_Module;
   Client_Started   : Integer := 0;

   ---------------------------------------
   -- Get_Started_Per_Session_Debuggers --
   ---------------------------------------

   function Get_Started_Per_Session_Debuggers return Integer is
   begin
      return Client_Started;
   end Get_Started_Per_Session_Debuggers;

   ----------------
   -- Debug_Init --
   ----------------

   function Debug_Init
     (Kernel          : GPS.Kernel.Kernel_Handle;
      Project         : Project_Type;
      File            : GNATCOLL.VFS.Virtual_File;
      Executable_Args : String;
      Remote_Target   : String)
      return DAP.Clients.DAP_Client_Access
   is
      Client : DAP.Clients.DAP_Client_Access;
   begin
      if DAP_Module_ID = null then
         return null;
      end if;

      Client := new DAP.Clients.DAP_Client (Kernel, DAP_Module_ID.Client_ID);
      DAP.Clients.Initialize_Client (Client);

      DAP_Module_ID.Current_Debuger_ID := DAP_Module_ID.Client_ID;

      if DAP_Module_ID.Client_ID < Valid_Client_Id_Type'Last then
         DAP_Module_ID.Client_ID := DAP_Module_ID.Client_ID + 1;
      else
         DAP_Module_ID.Client_ID := Valid_Client_Id_Type'First;
      end if;

      if DAP_Module_ID.Clients.Is_Empty then
         Client_Started := 1;
         --  Start the first debugger

         --  Switch to the "Debug" perspective if available
         GPS.Kernel.MDI.Load_Perspective
           (DAP_Module_ID.Get_Kernel, "Debug");

         if DAP.Modules.Preferences.Continue_To_Line_Buttons.Get_Pref then
            Enable_Continue_To_Line_On_Editors (DAP_Module_ID.Kernel);
         end if;

      else
         Client_Started := Client_Started + 1;
      end if;

      DAP_Module_ID.Clients.Append (Client);

      Client.Start
        (Project         => Project,
         Executable      => File,
         Executable_Args => Executable_Args,
         Remote_Target   => Remote_Target);

      Set_Current_Debugger (Client);

      return Client;
   end Debug_Init;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Id : in out DAP_Module_Record) is
      Local : constant DAP_Client_Vector := Id.Clients;
   begin
      for Client of Local loop
         begin
            Client.On_Destroy;
         exception
            when E : others =>
               Trace (Me, E);
         end;
      end loop;

      DAP.Module.Breakpoints.On_Destroy;

      if Id.Deallocate_Id /= No_Source_Id then
         Remove (Id.Deallocate_Id);
      end if;

      for Client of Id.Clients_To_Deallocate loop
         Free (Client);
      end loop;
      Id.Clients_To_Deallocate.Clear;

      DAP_Module_ID := null;
   end Destroy;

   -------------
   -- On_Idle --
   -------------

   function On_Idle return Boolean is
      Id : Valid_Client_Id_Type;
   begin
      DAP_Module_ID.Deallocate_Id := No_Source_Id;

      for Client of DAP_Module_ID.Clients_To_Deallocate loop
         Id := Client.Id;
         Free (Client);
         Dap_Debugger_Unloaded_Hook.Run (DAP_Module_ID.Kernel, Integer (Id));
      end loop;
      DAP_Module_ID.Clients_To_Deallocate.Clear;

      return False;
   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end On_Idle;

   ---------------------
   -- Tooltip_Handler --
   ---------------------

   overriding function Tooltip_Handler
     (Module  : access DAP_Module_Record;
      Context : Selection_Context) return Gtk_Widget
   is
      use type DAP.Clients.DAP_Client_Access;
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Client : constant DAP.Clients.DAP_Client_Access :=
        Get_Current_Debugger;
      W      : Gtk_Widget;
      Label  : Gtk_Label;
   begin
      if Module = null
        or else Client = null
        or else not Client.Is_Stopped
      then
         return null;
      end if;

      --  Display the "Continue to line" clickable icons if the dedicated
      --  preference is enabled.

      if DAP.Modules.Preferences.Continue_To_Line_Buttons.Get_Pref then
         Client.Display_Continue_To_Line_Icons (Context);
      end if;

      --  Return immediately if we are not hovering on an entity
      if not Has_Entity_Name_Information (Context)
        or else Xref.Is_Subprogram (Get_Entity (Context))
      then
         return null;
      end if;

      --  Get the name of the entity and try to display a tooltip with it's
      --  current value if possible.
      declare
         Variable_Name : constant String := DAP.Contexts.Get_Variable_Name
           (Context, Dereference => False);
      begin
         if Variable_Name = ""
           or else not Can_Tooltip_On_Entity
             (Kernel.Get_Language_Handler.Get_Language_From_File
                (File_Information (Context)), Variable_Name)
         then
            return null;
         end if;
         Gtk_New (Label, "<b>Debugger value :</b> ...");

         --  Retrieve the variable's value via the appropriate DAP evaluate
         --  request.
         DAP.Clients.Evaluate.Send_Get_Value_Of_Request
           (Client => Client.all,
            Label  => Label,
            Entity => Variable_Name);

         --  Wrap the tooltip's text according to the the user's right margin
         --  preference.
         Label.Set_Line_Wrap (True);
         Label.Set_Max_Width_Chars
           (Gint
              (Integer'(GPS.Kernel.Preferences.Gutter_Right_Margin.Get_Pref)));
         Label.Set_Use_Markup (True);
         Label.Modify_Font
           (GPS.Kernel.Preferences.View_Fixed_Font.Get_Pref);
         Label.Set_Alignment (0.0, 0.5);
         W := Gtk_Widget (Label);

         return W;
      end;

   exception
      when Language.Unexpected_Type | Constraint_Error =>
         return null;

      when E : others =>
         Trace (Me, E);
         return null;
   end Tooltip_Handler;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self   : On_Before_Exit;
      Kernel : not null access Kernel_Handle_Record'Class) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      --  Notify any running DAP server that we are exiting.
      if DAP_Module_ID /= null
        and then not DAP_Module_ID.Clients.Is_Empty
      then
         for D of DAP_Module_ID.Clients loop
            begin
               D.On_Before_Exit;
            exception
               when E : others =>
                  Trace (Me, E);
            end;
         end loop;
      end if;

      --  Save persistent breakpoints before exiting.
      DAP.Module.Breakpoints.
         Save_Persistent_Breakpoints (Kernel);

      return True;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);

      procedure Create_Action_And_Menu
        (Prj : Project_Type; Main : Virtual_File);
      --  Create the action and menu to initialize a specific executable

      Mains : Any_Type :=
        Compute_Build_Targets_Hook.Run (Kernel, "executable");

      Show_Project_In_Menu : constant Boolean :=
        GPS.Kernel.Modules.UI.Group_Mains_Into_Projects
          (Kernel, Mains.Length);

      ----------------------------
      -- Create_Action_And_Menu --
      ----------------------------

      procedure Create_Action_And_Menu
        (Prj : Project_Type; Main : Virtual_File)
      is
         Main_Name         : constant String :=
           (if Main = No_File
            then "no main file"
            else Main.Display_Base_Name (Suffix => Main.File_Extension));
         Escaped_Main_Name : constant String :=
           GUI_Utils.Escape_Underscore
             (GUI_Utils.Escape_Menu_Name (Main_Name));

         Action  : constant String :=
           "debug initialize " & Prj.Name & ":" & Main_Name;
         Menu    : constant String :=
           "/Debug/Initialize/"
           & (if not Show_Project_In_Menu or else Main = No_File
              then ""
              else GUI_Utils.Escape_Underscore (Prj.Name) & '/')
           & Escaped_Main_Name;
         Command : Interactive_Command_Access;
      begin
         Command := new Initialize_Debugger_Command'
           (Interactive_Command with
            Project => Prj,
            Exec    => Main);
         DAP_Module_ID.Dynamic_Actions.Append (Action);

         GPS.Kernel.Actions.Register_Action
           (Kernel, Action, Command,
            (if Main /= No_File
             then ("Initialize the debugger on the file "
               & Main.Display_Full_Name)
             else "Initialize the debugger, no file specified"),
            Category => "Debug");
         GPS.Kernel.Modules.UI.Register_Menu (Kernel, Menu, Action => Action);
      end Create_Action_And_Menu;

   begin
      if DAP_Module_ID = null then
         return;
      end if;

      for A of DAP_Module_ID.Dynamic_Actions loop
         GPS.Kernel.Actions.Unregister_Action
           (Kernel, A, Remove_Menus_And_Toolbars => True);
      end loop;
      DAP_Module_ID.Dynamic_Actions.Clear;

      for J in 1 .. Mains.Length loop
         if Mains.List (J).Length /= 0 then
            declare
               Main : constant Virtual_File := GPS.Core_Kernels.To_File
                 (Kernel, Mains.List (J).Tuple (2).Str,
                  --  Here we obtain the file name not from the debugger but
                  --  from the project itself: we don't need to check if the
                  --  main's file actually exists on the disk.
                  Check_Exist => False);

               Prj  : constant Virtual_File := GPS.Core_Kernels.To_File
                 (Kernel, Mains.List (J).Tuple (3).Str);
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

      Create_Action_And_Menu
        (GPS.Kernel.Project.Get_Project (Kernel), No_File);

   exception
      when E : others =>
         Trace (Me, E);
         --  Debug_Terminate (Kernel_Handle (Kernel));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference)
   is
      use Default_Preferences;
      use DAP.Clients;

      pragma Unreferenced (Self);
   begin
      if DAP_Module_ID = null then
         return;
      end if;

      if Pref = Preference
        (DAP.Modules.Preferences.Continue_To_Line_Buttons)
      then
         declare
            Client : constant DAP.Clients.DAP_Client_Access :=
              DAP.Module.Get_Current_Debugger;
         begin
            if Client /= null then
               if DAP.Modules.Preferences.
                 Continue_To_Line_Buttons.Get_Pref
               then
                  Enable_Continue_To_Line_On_Editors (DAP_Module_ID.Kernel);
               else
                  Disable_Continue_To_Line_On_Editors (DAP_Module_ID.Kernel);
               end if;
            end if;
         end;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Initialize_Debugger_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      Initialize_Debugger
        (Kernel  => Get_Kernel (Context.Context),
         Project => Command.Project,
         File    => Command.Exec);

      return Success;
   exception
      when E : others =>
         Trace (Me, E);
         return Failure;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Terminate_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      if DAP_Module_ID /= null
        and then DAP_Module_ID.Current_Debuger_ID /= 0
      then
         for Client of DAP_Module_ID.Clients loop
            if Client.Id = DAP_Module_ID.Current_Debuger_ID then
               Client.Quit;
               exit;
            end if;
         end loop;
      end if;

      return Success;
   exception
      when E : others =>
         Trace (Me, E);
         return Failure;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Terminate_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      Terminate_Debuggers;
      return Success;
   exception
      when E : others =>
         Trace (Me, E);
         return Failure;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Start_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      use type DAP.Clients.DAP_Client_Access;
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Client : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
   begin
      if Client = null then
         return Commands.Failure;
      else
         Start_Executable
           (Kernel               => Kernel,
            Client               => Client,
            Display_Start_Dialog => True);
         return Commands.Success;
      end if;
   end Execute;

   ----------------------
   -- Start_Executable --
   ----------------------

   procedure Start_Executable
     (Kernel               : not null Kernel_Handle;
      Client               : not null DAP.Clients.DAP_Client_Access;
      Display_Start_Dialog : Boolean := False;
      Stop_At_Beginning    : Boolean := True)

   is
      use type DAP.Types.Debugger_Status_Kind;
      Ignore  : Gtkada.Dialogs.Message_Dialog_Buttons;
      pragma Unreferenced (Ignore);

      procedure Display_Dialog;
      --  Display a modal dialog asking for arguments to be passed to
      --  the launched executable, and other options (e.g: stop at the
      --  beginning of the program).

      --------------------
      -- Display_Dialog --
      --------------------

      procedure Display_Dialog is
         Dialog                   : GPS_Dialog;
         Args_Combo               : Combo_Box;
         Stop_At_Beginning_Button : Gtk_Check_Button;
         Response                 : Gtk_Response_Type;
      begin
         Gtk_New
         (Dialog,
            Title  => "Run/Start",
            Flags  => Destroy_With_Parent or Modal,
            Kernel => Kernel);
         Dialog.Add_OK_Cancel;

         --  Add a combo box for the arguments that should be passed to the
         --  debuggee.

         Args_Combo := Dialog.Add_Combo
            (Message => "Run arguments:",
             Key     => Run_Arguments_History_Key);

         --  Add a checkbox so that the user can select whether he/she wants to
         --  stop at the beginning of the main program.

         Stop_At_Beginning_Button :=
           Dialog.Add_Check_Button
             (Message => "Stop at beginning of main subprogram",
              Key     => "stop_beginning_debugger");

         Dialog.Show_All;
         Response := Dialog.Run;

         declare
            Stop : constant Boolean :=
               Stop_At_Beginning_Button.Get_Active;
            Args : constant String :=
               String_Utils.Strip_Ending_Linebreaks
                 (Args_Combo.Get_Text);
         begin
            Dialog.Destroy;
            if Response = Gtk_Response_OK then
               Client.Launch_Executable
                  (Executable        => Client.Get_Executable,
                   Executable_Args   =>
                     VSS.Strings.Conversions.To_Virtual_String
                       (Args).Split
                       (Separator           => VSS.Characters.Latin.Space,
                        Keep_Empty_Segments => False),
                   Stop_At_Beginning => Stop);
            end if;
         end;
      end Display_Dialog;

   begin
      if Client.Get_Status /= DAP.Types.Initialized then
         Ignore := GUI_Utils.GPS_Message_Dialog
           ("Cannot rerun while the underlying debugger is busy." &
              ASCII.LF &
              "Interrupt the debugger or wait for its availability.",
            Dialog_Type => Gtkada.Dialogs.Warning,
            Buttons     => Gtkada.Dialogs.Button_OK,
            Parent      => Kernel.Get_Main_Window);
      elsif Display_Start_Dialog then
         Display_Dialog;
      else
         Client.Launch_Executable
           (Executable        => Client.Get_Executable,
            Executable_Args   => Client.Get_Executable_Args,
            Stop_At_Beginning => Stop_At_Beginning);
      end if;
   end Start_Executable;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Continue_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      use type DAP.Clients.DAP_Client_Access;
      use type DAP.Types.Debugger_Status_Kind;

      Client : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;

   begin
      if Client = null then
         return Commands.Failure;
      end if;

      if Client.Get_Status = DAP.Types.Initialized then
         Start_Executable
           (Kernel               => GPS.Kernel.Get_Kernel (Context.Context),
            Client               => Client,
            Display_Start_Dialog => True);

      else
         Client.Continue_Execution;
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Next_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type is
   begin
      DAP.Clients.Next.Send_Next (Get_Current_Debugger);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Nexti_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type is
   begin
      DAP.Clients.Next.Send_Next_Instruction (Get_Current_Debugger);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Step_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type is
   begin
      DAP.Clients.StepIn.Send_Step_In (Get_Current_Debugger);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Stepi_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type is
   begin
      DAP.Clients.StepIn.Send_Step_In_Instruction (Get_Current_Debugger);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Interrupt_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command, Context);
   begin
      Get_Current_Debugger.Interrupt;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Connect_To_Board_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command, Context);
   begin
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Attach_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      use DAP.Clients;

      Kernel : constant Kernel_Handle :=
        GPS.Kernel.Get_Kernel (Context.Context);
      PID    : constant String :=
        GUI_Utils.Query_User
          (Parent        => Kernel.Get_Main_Window,
           Prompt        => "Enter the PID of the process to attach to",
           Password_Mode => False);
      Client : constant DAP.Clients.DAP_Client_Access :=
        Get_Current_Debugger;
   begin
      if Client /= null then
         DAP.Clients.Attach.Send_Attach_Request
           (Client => Client.all,
            PID    => Integer'Value (PID));

         return Commands.Success;
      else
         return Commands.Failure;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Detach_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      use DAP.Clients;

      Client : constant DAP.Clients.DAP_Client_Access :=
        Get_Current_Debugger;
   begin
      if Client /= null then
         --  Send a DAP 'disconnect' request to detach the debugger from
         --  the debuggee, specifying that it should not terminate the
         --  debuggee since the user might want to re-attach to it later.
         DAP.Clients.Disconnect.Send_Disconnect_Request
           (Client             => Client.all,
            Terminate_Debuggee => False);

         return Commands.Success;
      else
         return Commands.Failure;
      end if;
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
      Client      : constant DAP.Clients.DAP_Client_Access :=
        Get_Current_Debugger;
      Exec        : Virtual_File;
      Ptr         : GNAT.Strings.String_Access :=
        GNAT.OS_Lib.Get_Executable_Suffix;
      Exec_Suffix : constant String := Ptr.all;

   begin
      GNAT.Strings.Free (Ptr);

      declare
         Selected_File : Virtual_File :=
               Select_File
                 (Title             => "Select File to Debug",
                  File_Pattern      => +("*" & Exec_Suffix & ";*"),
                  Pattern_Name      => "Executable files;All files",
                  Parent            => Get_Current_Window (Kernel),
                  Use_Native_Dialog =>
                    GPS.Kernel.Preferences.Use_Native_Dialogs.Get_Pref,
                  Kind              => Open_File,
                  History           => Get_History (Kernel));
      begin
         if Selected_File = GNATCOLL.VFS.No_File then
            return Commands.Failure;
         end if;

         if not Is_Regular_File (Selected_File) then
            Exec := GNATCOLL.VFS.Locate_On_Path (Base_Name (Selected_File));

            if not Is_Regular_File (Exec) then
               Kernel.Insert
                 (("Could not find file: ") &
                    Display_Base_Name (Selected_File),
                  Mode => Error);
               Selected_File := GNATCOLL.VFS.No_File;
            else
               Selected_File := Exec;
            end if;
         end if;

         if Selected_File /= No_File then
            Client.Set_Executable (Selected_File);

            Client.Launch_Executable
              (Executable        => Selected_File,
               Stop_At_Beginning => True);
         end if;

      exception
         when others =>
            Kernel.Insert
              (("Could not find file: ") & Display_Full_Name (Selected_File),
               Mode => Error);
            return Commands.Failure;
      end;

      return Commands.Success;
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
      use DAP.Clients;

      Client : constant DAP.Clients.DAP_Client_Access :=
        Get_Current_Debugger;
      Context : Selection_Context := New_Context (Kernel);
   begin
      if Client = null then
         Remove_Continue_To_Line_Messages (Kernel);
         return;
      end if;

      Set_File_Information
        (Context,
         Files   => (1 => File),
         Project => Project);
      Set_Entity_Information
        (Context         => Context,
         Entity_Name     => "",
         Entity_Line     => Basic_Types.Editable_Line_Type (Line),
         Entity_Column   => Basic_Types.Visible_Column_Type (Column),
         From_Expression => "");
      Display_Continue_To_Line_Icons (Client, Context);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      Buffer : constant Editor_Buffer'Class :=
                 Kernel.Get_Buffer_Factory.Get
                   (File        => File,
                    Open_View   => False);
   begin
      Create_Continue_To_Line_Columns
        (Kernel => Kernel,
         Buffer => Buffer);
   end Execute;

   ----------------------------------------
   -- Enable_Continue_To_Line_On_Editors --
   ----------------------------------------

   procedure Enable_Continue_To_Line_On_Editors
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      Buffers : constant Buffer_Lists.List :=
                  Kernel.Get_Buffer_Factory.Buffers;
   begin
      Location_Changed_Hook.Add (new On_Location_Changed);
      File_Edited_Hook.Add (new On_File_Edited);

      --  Add the extra column do display the clickable icons
      for Buffer of Buffers loop
         Create_Continue_To_Line_Columns
           (Kernel,
            Buffer => Buffer);
      end loop;
   end Enable_Continue_To_Line_On_Editors;

   -----------------------------------------
   -- Disable_Continue_To_Line_On_Editors --
   -----------------------------------------

   procedure Disable_Continue_To_Line_On_Editors
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      Buffers : constant Buffer_Lists.List :=
                  Kernel.Get_Buffer_Factory.Buffers;

      function Is_Location_Changed_Function
        (F : not null access Hook_Function'Class) return Boolean
      is (F.all in On_Location_Changed'Class);

      function Is_File_Edited_Function
        (F : not null access Hook_Function'Class) return Boolean
      is (F.all in On_File_Edited'Class);

   begin
      --  We don't need to monitor the debugging context anymore so remove
      --  the Location_Changed and File_Edited hook function.
      Location_Changed_Hook.Remove
        (Is_Location_Changed_Function'Access);
      File_Edited_Hook.Remove
        (Is_File_Edited_Function'Access);

      --  Remove the extra column we may have added on editors
      for Buffer of Buffers loop
         if Buffer /= Nil_Editor_Buffer
           and Buffer.Has_Information_Column
             (VSS.Strings.Conversions.To_UTF_8_String
                (DAP.Types.Messages_Category_Continue_To_Line))
         then
            Remove_Line_Information_Column
              (Kernel     => Kernel,
               File       => Buffer.File,
               Identifier =>
                 VSS.Strings.Conversions.To_UTF_8_String
                   (DAP.Types.Messages_Category_Continue_To_Line));
         end if;
      end loop;
   end Disable_Continue_To_Line_On_Editors;

   -------------------------------------
   -- Create_Continue_To_Line_Columns --
   -------------------------------------

   procedure Create_Continue_To_Line_Columns
     (Kernel : not null access Kernel_Handle_Record'Class;
      Buffer : Editor_Buffer'Class) is
   begin
      if Buffer /= Nil_Editor_Buffer
        and then not Buffer.Has_Information_Column
          (VSS.Strings.Conversions.To_UTF_8_String
             (DAP.Types.Messages_Category_Continue_To_Line))
      then
         Create_Line_Information_Column
           (Kernel     => Kernel,
            File       => Buffer.File,
            Identifier =>
              VSS.Strings.Conversions.To_UTF_8_String
                (DAP.Types.Messages_Category_Continue_To_Line));
      end if;
   end Create_Continue_To_Line_Columns;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Debuggers_Filter;
      Context : Selection_Context) return Boolean is
   begin
      return DAP_Module_ID /= null
        and then not DAP_Module_ID.Clients.Is_Empty;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Initialized_Filter;
      Context : Selection_Context) return Boolean
   is
      use DAP.Types;
      use type DAP.Clients.DAP_Client_Access;
   begin
      return DAP.Module.Get_Current_Debugger /= null
        and then DAP.Module.Get_Current_Debugger.Get_Status = Initialized;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Breakable_Source_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_File_Information (Context)
        and then not Has_Suffix (File_Information (Context), ".gpr");
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access No_Debugger_Or_Available_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return DAP_Module_ID = null
        or else DAP_Module_ID.Clients.Is_Empty
        or else Get_Current_Debugger.Is_Available;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access No_Debugger_Or_Initialized_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      use DAP.Types;
      use DAP.Clients;
   begin
      return DAP_Module_ID = null
        or else Get_Current_Debugger = null
        or else
          (Get_Current_Debugger.Get_Status = Initialized
           or else Get_Current_Debugger.Is_Available);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Available_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return DAP_Module_ID /= null
        and then not DAP_Module_ID.Clients.Is_Empty
        and then Get_Current_Debugger.Is_Available;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Stopped_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return DAP_Module_ID /= null
        and then not DAP_Module_ID.Clients.Is_Empty
        and then Get_Current_Debugger.Is_Stopped;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Debuggee_Running_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      use DAP.Types;
   begin
      return DAP_Module_ID /= null
        and then not DAP_Module_ID.Clients.Is_Empty
        and then Get_Current_Debugger.Get_Status = DAP.Types.Running;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Entity_Name_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Entity_Name_Information (Context);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Not_Command_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);

   begin
      if GPS.Kernel.Contexts.Has_Debugging_Variable (Context)
        and then not DAP.Contexts.Get_Variable (Context).Is_Command
      then
         return False;
      else
         return True;
      end if;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Attached_Debuggee_Filter;
      Context : Selection_Context) return Boolean
   is
      use DAP.Clients;
      use DAP.Types;

      Client : constant DAP.Clients.DAP_Client_Access :=
        Get_Current_Debugger;
   begin
      return Client /= null
        and then Client.Get_Debuggee_Start_Method = Attached;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access In_Debugger_Frame_Filter;
      Context : Selection_Context) return Boolean
   is
      use DAP.Clients;
      pragma Unreferenced (Filter);

      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Client : constant DAP.Clients.DAP_Client_Access :=
        Get_Current_Debugger;

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
           (This        => Buffer.New_Location
              (Client.Get_Stack_Trace.Get_Current_Line, 0),
            Subprogram  => True);
         New_Loc_Subprogram : constant String := Block_Name
           (This        => Buffer.New_Location (Line, 0),
            Subprogram  => True);
      begin
         return Debugger_Subprogram = New_Loc_Subprogram;
      end In_Debugger_Frame;

   begin
      if Client = null then
         return False;
      end if;

      if Has_File_Information (Context)
        and then Has_Entity_Line_Information (Context)
      then
         declare
            File   : constant GNATCOLL.VFS.Virtual_File :=
                       File_Information (Context);
            Line   : constant Natural := Natural
              (GPS.Kernel.Contexts.Entity_Line_Information (Context));
            Buffer : constant Editor_Buffer'Class :=
                       Kernel.Get_Buffer_Factory.Get
                       (File        => File,
                        Open_View   => False);
         begin
            if Client.Get_Stack_Trace.Get_Current_File = File
              and then Client.Get_Stack_Trace.Get_Current_Line /= Line
              and then Buffer /= Nil_Editor_Buffer
            then
               return In_Debugger_Frame (Buffer, Line);
            end if;
         end;
      end if;

      return False;
   end Filter_Matches_Primitive;

   -----------------------
   -- For_Each_Debugger --
   -----------------------

   procedure For_Each_Debugger
     (Callback : access procedure (Debugger : DAP.Clients.DAP_Client_Access))
   is
   begin
      if DAP_Module_ID = null then
         return;
      end if;

      for C of DAP_Module_ID.Clients loop
         Callback (C);
      end loop;
   end For_Each_Debugger;

   -----------------------------
   -- Count_Running_Debuggers --
   -----------------------------

   function Count_Running_Debuggers return Natural is
   begin
      if DAP_Module_ID = null then
         return 0;
      else
         return Natural (DAP_Module_ID.Clients.Length);
      end if;
   end Count_Running_Debuggers;

   --------------------------
   -- Get_Current_Debugger --
   --------------------------

   function Get_Current_Debugger return DAP.Clients.DAP_Client_Access is
   begin
      if DAP_Module_ID = null then
         return null;
      else
         return Get_Current_Debugger (DAP_Module_ID);
      end if;
   end Get_Current_Debugger;

   ------------------
   -- Get_Debugger --
   ------------------

   function Get_Debugger
     (Id : Client_Id_Type)
      return DAP.Clients.DAP_Client_Access is
   begin
      return Get_Debugger (DAP_Module_ID, Id);
   end Get_Debugger;

   --------------------------
   -- Get_Current_Debugger --
   --------------------------

   function Get_Current_Debugger
     (Id : DAP_Module)
      return DAP.Clients.DAP_Client_Access is
   begin
      if Id = null then
         return null;
      end if;

      for C of Id.Clients loop
         if C.Id = Id.Current_Debuger_ID then
            return C;
         end if;
      end loop;

      return null;
   end Get_Current_Debugger;

   ------------------
   -- Get_Debugger --
   ------------------

   function Get_Debugger
     (Id  : DAP_Module;
      Num : Client_Id_Type)
      return DAP.Clients.DAP_Client_Access is
   begin
      if Id = null or else Num = No_Client then
         return null;
      end if;

      for C of Id.Clients loop
         if C.Id = Num then
            return C;
         end if;
      end loop;

      return null;
   end Get_Debugger;

   -------------------------
   -- Initialize_Debugger --
   -------------------------

   procedure Initialize_Debugger
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      File            : GNATCOLL.VFS.Virtual_File := No_File;
      Project         : Project_Type := No_Project;
      Executable_Args : String := "";
      Remote_Target   : String := "")
   is
      Dummy : DAP.Clients.DAP_Client_Access;
   begin
      Dummy := Initialize_Debugger
        (Kernel          => GPS.Kernel.Kernel_Handle (Kernel),
         File            => File,
         Project         => Project,
         Executable_Args => Executable_Args,
         Remote_Target   => Remote_Target);
   end Initialize_Debugger;

   -------------------------
   -- Initialize_Debugger --
   -------------------------

   function Initialize_Debugger
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      File            : GNATCOLL.VFS.Virtual_File := No_File;
      Project         : Project_Type := No_Project;
      Executable_Args : String := "";
      Remote_Target   : String := "")
      return DAP.Clients.DAP_Client_Access is
   begin
      return Debug_Init
        (Kernel          => GPS.Kernel.Kernel_Handle (Kernel),
         Project         => Project,
         File            => File,
         Executable_Args => Executable_Args,
         Remote_Target   => Remote_Target);
   end Initialize_Debugger;

   --------------
   -- Finished --
   --------------

   procedure Finished (Id : Valid_Client_Id_Type) is
      use DAP_Client_Vectors;
      use type DAP.Clients.DAP_Client_Access;

      C      : DAP_Client_Vectors.Cursor;
      Client : DAP.Clients.DAP_Client_Access := null;
   begin
      if DAP_Module_ID = null
        or else DAP_Module_ID.Clients.Is_Empty
      then
         return;
      end if;

      C := DAP_Module_ID.Clients.First;
      while Has_Element (C) loop
         if Element (C).Id = Id then
            Client := Element (C);
            exit;
         end if;
         Next (C);
      end loop;

      --  if debugger is found
      if Client /= null then
         DAP_Module_ID.Clients.Delete (C);
         DAP_Module_ID.Clients_To_Deallocate.Append (Client);

         --  Reregister callback for future dealocation
         if DAP_Module_ID.Deallocate_Id /= No_Source_Id then
            Remove (DAP_Module_ID.Deallocate_Id);
         end if;
         DAP_Module_ID.Deallocate_Id := Idle_Add (On_Idle'Access);

         --  Set current "active" debugger if any
         if DAP_Module_ID.Current_Debuger_ID = Id then
            if DAP_Module_ID.Clients.Is_Empty then
               Set_Current_Debugger (null);
            else
               Set_Current_Debugger (DAP_Module_ID.Clients.First_Element);
            end if;
         end if;
      end if;

      --  if it was the last debugger
      if DAP_Module_ID.Clients.Is_Empty then
         declare
            Kernel : constant Kernel_Handle := DAP_Module_ID.Get_Kernel;
         begin
            --  The last debugger has been finished
            DAP_Module_ID.Client_ID := 1;

            --  Save persistent breakpoints
            DAP.Module.Breakpoints.On_Debugging_Terminated
              (Kernel);

            --  Unhighlight the current debugger line
            DAP.Utils.Unhighlight_Current_Line (Kernel);

            GPS.Kernel.MDI.Load_Perspective (Kernel, "Default");

            Kernel.Refresh_Context;
         end;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Finished;

   --------------------------
   -- Set_Current_Debugger --
   --------------------------

   procedure Set_Current_Debugger
     (Current : DAP.Clients.DAP_Client_Access)
   is
      use type DAP.Clients.DAP_Client_Access;

      function Set_Current_Debugger
        (Id : DAP_Module; Current : DAP.Clients.DAP_Client_Access)
      return Boolean;

      --------------------------
      -- Set_Current_Debugger --
      --------------------------

      function Set_Current_Debugger
        (Id : DAP_Module; Current : DAP.Clients.DAP_Client_Access)
      return Boolean is
      begin
         if Id = null
           or else (Current /= null
                    and then Id.Current_Debuger_ID = Current.Id)
         then
            return False;
         end if;

         if Current /= null then
            Id.Current_Debuger_ID := Current.Id;
         else
            Id.Current_Debuger_ID := 0;
         end if;

         return True;
      end Set_Current_Debugger;

      Old : constant Client_Id_Type := DAP_Module_ID.Current_Debuger_ID;

   begin
      if Set_Current_Debugger (DAP_Module_ID, Current)
        and then Current /= null
      then
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Current.Kernel, Current.Get_Visual);
      end if;

      if DAP.Modules.Preferences.Continue_To_Line_Buttons.Get_Pref then
         if Old = No_Client
           and then DAP_Module_ID.Current_Debuger_ID /= No_Client
         then
            Enable_Continue_To_Line_On_Editors (DAP_Module_ID.Kernel);

         elsif Old /= No_Client
           and then DAP_Module_ID.Current_Debuger_ID = No_Client
         then
            Disable_Continue_To_Line_On_Editors (DAP_Module_ID.Kernel);
         end if;
      end if;
   end Set_Current_Debugger;

   -------------------------
   -- Terminate_Debuggers --
   -------------------------

   procedure Terminate_Debuggers is
   begin
      if DAP_Module_ID = null then
         return;
      end if;

      for D of DAP_Module_ID.Clients loop
         begin
            D.Quit;
         exception
            when E : others =>
               Trace (Me, E);
         end;
      end loop;
   end Terminate_Debuggers;

   --------------------------------------
   -- Remove_Continue_To_Line_Messages --
   --------------------------------------

   procedure Remove_Continue_To_Line_Messages
     (Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      Get_Messages_Container (Kernel).Remove_Category
        (DAP.Types.Messages_Category_Continue_To_Line,
         DAP.Types.Continue_To_Line_Messages_Flags);
   end Remove_Continue_To_Line_Messages;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Has_Debugger               : Action_Filter;
      No_Debugger_Or_Available   : Action_Filter;
      No_Debugger_Or_Initialized : Action_Filter;
      Debugger_Available         : Action_Filter;
      Debugger_Initialized       : Action_Filter;
      Debugger_Stopped           : Action_Filter;
      Debuggee_Running           : Action_Filter;
      Breakable_Filter           : Action_Filter;
      Entity_Filter              : Action_Filter;
      Is_Not_Command_Filter      : Action_Filter;
      Attached_Debuggee          : Action_Filter;
      Continue_Until_Filter      : Action_Filter;
   begin
      DAP.Modules.Preferences.Register_Default_Preferences
        (Kernel.Get_Preferences);

      DAP_Module_ID := new DAP_Module_Record (Kernel);
      if Kernel /= null then
         Register_Module
           (Module          => Module_ID (DAP_Module_ID),
            Kernel          => Kernel,
            Module_Name     => DAP_Module_Name,
            Priority        => Default_Priority + 20);
      end if;

      -- Filters --

      Has_Debugger := new Has_Debuggers_Filter;
      Register_Filter (Kernel, Has_Debugger, "Has debuggers");

      No_Debugger_Or_Available := new No_Debugger_Or_Available_Filter;
      Register_Filter
        (Kernel, No_Debugger_Or_Available, "No debugger or available");

      No_Debugger_Or_Initialized := new No_Debugger_Or_Initialized_Filter;
      Register_Filter
        (Kernel, No_Debugger_Or_Initialized, "No debugger or initialized");

      Debugger_Available := new Debugger_Available_Filter;
      Register_Filter (Kernel, Debugger_Available, "Debugger available");

      Debugger_Stopped := new Debugger_Stopped_Filter;
      Register_Filter (Kernel, Debugger_Stopped, "Debugger stopped");

      Debuggee_Running := new Debuggee_Running_Filter;
      Register_Filter (Kernel, Debuggee_Running, "Debuggee running");

      Breakable_Filter := new Breakable_Source_Filter;
      Register_Filter
        (Kernel, Breakable_Filter, "Debugger breakable source");

      Entity_Filter := new Entity_Name_Filter;
      Register_Filter (Kernel, Entity_Filter, "Debugger entity name");

      Debugger_Initialized := new Debugger_Initialized_Filter;
      Register_Filter (Kernel, Debugger_Initialized, "Debugger initialized");

      Is_Not_Command_Filter := new Not_Command_Filter;
      Register_Filter
        (Kernel, Is_Not_Command_Filter, "Debugger not command variable");

      Attached_Debuggee := new Attached_Debuggee_Filter;
      Register_Filter
        (Kernel, Attached_Debuggee, "Debuggee attached");

      Continue_Until_Filter :=
        Debugger_Available and new In_Debugger_Frame_Filter;
      Register_Filter
        (Kernel, Continue_Until_Filter, "Can continue until");

      -- Hooks --
      Before_Exit_Action_Hook.Add (new On_Before_Exit);
      Preferences_Changed_Hook.Add (new On_Pref_Changed);
      Project_View_Changed_Hook.Add (new On_Project_View_Changed);

      --  Actions --
      GPS.Kernel.Modules.UI.Register_Contextual_Submenu
        (Kernel, "Debug",
         Group => GPS.Kernel.Modules.UI.Debug_Contextual_Group);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "terminate debugger", new Terminate_Command,
         Icon_Name   => "gps-debugger-terminate-symbolic",
         Description => "Terminate the current debugger",
         Filter      => Has_Debugger and not Attached_Debuggee);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "terminate all debuggers", new Terminate_All_Command,
         Description => "Terminate all running debugger",
         Filter      => Has_Debugger and not Attached_Debuggee);

      GPS.Kernel.Modules.UI.Register_Contextual_Submenu
        (Kernel, "Debug",
         Group => GPS.Kernel.Modules.UI.Debug_Contextual_Group);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug run dialog", new Start_Command,
         Filter      => Debugger_Initialized,
         Description =>
           "Choose the arguments to the program, and start running it",
         Category    => "Debug");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug continue", new Continue_Command,
         Icon_Name    => "gps-debugger-run-symbolic",
         Filter       => Debugger_Available,
         Description  =>
           "Continue execution until next breakpoint." & ASCII.LF
         & "Start the debugger if not started yet",
         Category     => "Debug",
         For_Learning => True);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug next", new Next_Command,
         Icon_Name    => "gps-debugger-next-symbolic",
         Filter       => Debugger_Stopped,
         Description  =>
           "Execute the program until the next source line, stepping over"
         & " subprogram calls",
         Category     => "Debug",
         For_Learning => True);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug nexti", new Nexti_Command,
         Filter      => Debugger_Stopped,
         Description =>
           "Execute the program until the next machine instruction, stepping"
         & " over subprogram calls",
         Category    => "Debug");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug step", new Step_Command,
         Icon_Name    => "gps-debugger-step-symbolic",
         Filter       => Debugger_Stopped,
         Description  =>
           "Execute until program reaches a new line of source code",
         Category     => "Debug",
         For_Learning => True);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug stepi", new Stepi_Command,
         Filter      => Debugger_Stopped,
         Description =>
           "Execute the program for one machine instruction only",
         Category    => "Debug");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug interrupt", new Interrupt_Command,
         Icon_Name    => "gps-debugger-pause-symbolic",
         Filter       => Has_Debugger and Debuggee_Running,
         Description  => "Asynchronously interrupt the debuggee program",
         Category     => "Debug",
         For_Learning => True);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug connect to board", new Connect_To_Board_Command,
         Description =>
           "Opens a simple dialog to connect to a remote board. This option"
           & " is only relevant to cross debuggers.",
         Filter   => No_Debugger_Or_Available,
         Category => "Debug");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug attach", new Attach_Command,
         Description => "Attach to a running process",
         Filter      => Has_Debugger,
         Category    => "Debug");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug detach", new Detach_Command,
         Icon_Name   => "gps-debugger-detach-symbolic",
         Description => "Detach the running process from the debugger",
         Filter      => Has_Debugger and Attached_Debuggee,
         Category    => "Debug");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug load file", new Load_File_Command,
         Description =>
           "Opens a file selection dialog that allows you to choose a"
           & " program to debug. The program to debug is either an executable"
           & " for native debugging, or a partially linked module for cross"
           & " environments (e.g VxWorks).",
         Filter   => Debugger_Available,
         Category => "Debug");

      DAP.Module.Breakpoints.Register_Module (Kernel);
      DAP.Modules.Scripts.Register_Module (Kernel);

      DAP.Clients.Stack_Trace.Register_Module;
      DAP.Clients.Variables.Register_Module;

      DAP.Views.Call_Stack.Register_Module (Kernel);
      DAP.Views.Threads.Register_Module (Kernel);
      DAP.Views.Assembly.Register_Module (Kernel);
      DAP.Views.Consoles.Register_Module (Kernel);
      DAP.Views.Memory.Register_Module (Kernel);
      DAP.Views.Variables.Register_Module (Kernel);
      DAP.Views.Registers.Register_Module (Kernel);
   end Register_Module;

end DAP.Module;
