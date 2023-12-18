------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with GNATCOLL.Any_Types;           use GNATCOLL.Any_Types;
with GNATCOLL.Traces;              use GNATCOLL.Traces;

with Glib.Main;                    use Glib.Main;
with Glib.Object;                  use Glib.Object;

with Gtk.Handlers;
with Gtk.Label;                    use Gtk.Label;
with Gtk.Widget;                   use Gtk.Widget;

with Gtkada.Dialogs;
with Gtkada.MDI;                   use Gtkada.MDI;

with Commands;                     use Commands;
with Commands.Interactive;         use Commands.Interactive;
with Language;                     use Language;

with GPS.Debuggers;
with GPS.Editors;                  use GPS.Editors;
with GPS.Kernel.Actions;
with GPS.Kernel.Contexts;          use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;               use GPS.Kernel.MDI;
with GPS.Kernel.Modules;           use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;
with GPS.Kernel.Project;
with GUI_Utils;

with Remote;

with DAP.Contexts;
with DAP.Modules.Contexts;
with DAP.Modules.Persistent_Breakpoints;
with DAP.Modules.Preferences;
with DAP.Modules.Scripts;
with DAP.Clients.Attach;
with DAP.Clients.ConfigurationDone;
with DAP.Clients.Next;
with DAP.Clients.StepIn;
with DAP.Views.Assembly;
with DAP.Views.Call_Stack;
with DAP.Views.Consoles;
with DAP.Views.Threads;
with DAP.Views.Memory;
with DAP.Views.Registers;
with DAP.Views.Variables;
with DAP.Requests;

package body DAP.Module is

   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.DAP_MODULE", Off);
   --  Enable/disable DAP support

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

      Current_Debuger_ID    : Integer := 0;
      --  Client that is used as a current active debugger

      Client_ID             : Positive := 1;
      --  Counter to numerate started debuggers
   end record;

   overriding procedure Destroy (Id : in out DAP_Module_Record);

   type DAP_Module is access all DAP_Module_Record'Class;

   function Get_Current_Debugger
     (Id : DAP_Module)
      return DAP.Clients.DAP_Client_Access;

   function Get_Debugger
     (Id  : DAP_Module;
      Num : Integer)
      return DAP.Clients.DAP_Client_Access;

   function Set_Current_Debugger
     (Id : DAP_Module; Current : DAP.Clients.DAP_Client_Access)
      return Boolean;
   --  Returns True when current debugger is changed

   overriding function Tooltip_Handler
     (Module  : access DAP_Module_Record;
      Context : Selection_Context) return Gtk_Widget;
   --  See inherited documentation

   package Client_ID_Callback is new Gtk.Handlers.User_Callback
     (Generic_Views.View_Record, Integer);

   procedure On_Console_Destroy
     (Console : access Generic_Views.View_Record'Class;
      Id      : Integer);
   --  Called when the debugger console is destroyed, which also terminates the
   --  debugger itself

   procedure Start
     (Kernel : Kernel_Handle;
      Client : DAP.Clients.DAP_Client_Access);

   function On_Idle return Boolean;
   --  Called from Gtk on Idle to deallocate clients

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

   -- Filters --

   type Has_Debuggers_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Debuggers_Filter;
      Context : Selection_Context) return Boolean;

   type Debugger_Ready_State_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Ready_State_Filter;
      Context : Selection_Context) return Boolean;

   type Debugger_Ready_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Ready_Filter;
      Context : Selection_Context) return Boolean;

   type Debugger_Stopped_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Stopped_Filter;
      Context : Selection_Context) return Boolean;

   type No_Debugger_Or_Ready_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access No_Debugger_Or_Ready_Filter;
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

   -- Utils --

   function Debug_Init
     (Kernel          : GPS.Kernel.Kernel_Handle;
      Project         : Project_Type;
      File            : GNATCOLL.VFS.Virtual_File;
      Executable_Args : String)
      return DAP.Clients.DAP_Client_Access;
   --  Initialize the debugger with the executable refered by File/Project.
   --  Executable_Args contain the extra arguments that will be passed to the
   --  debugged executable.

   function To_File
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Name    : String)
      return GNATCOLL.VFS.Virtual_File;

   DAP_Module_Name  : constant String := "DAP";
   DAP_Module_ID    : DAP_Module;
   Client_Started   : Integer := 0;
   Breakpoints_View : Generic_Views.Abstract_View_Access := null;

   --------------------------
   -- Get_Breakpoints_View --
   --------------------------

   function Get_Breakpoints_View return Generic_Views.Abstract_View_Access is
   begin
      return Breakpoints_View;
   end Get_Breakpoints_View;

   --------------------------
   -- Set_Breakpoints_View --
   --------------------------

   procedure Set_Breakpoints_View
     (View : Generic_Views.Abstract_View_Access) is
   begin
      Breakpoints_View := View;
   end Set_Breakpoints_View;

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
      Executable_Args : String)
      return DAP.Clients.DAP_Client_Access
   is
      use type Generic_Views.Abstract_View_Access;

      Client        : DAP.Clients.DAP_Client_Access;
      Console_Child : MDI_Child;
   begin
      if DAP_Module_ID = null then
         return null;
      end if;

      Client := new DAP.Clients.DAP_Client (Kernel, DAP_Module_ID.Client_ID);
      DAP.Clients.Initialize_Client (Client);

      DAP_Module_ID.Current_Debuger_ID := DAP_Module_ID.Client_ID;

      if DAP_Module_ID.Client_ID < Positive'Last then
         DAP_Module_ID.Client_ID := DAP_Module_ID.Client_ID + 1;
      else
         DAP_Module_ID.Client_ID := 1;
      end if;

      if DAP_Module_ID.Clients.Is_Empty then
         Client_Started := 1;
         --  Start first debugger

         --  Switch to the "Debug" perspective if available
         GPS.Kernel.MDI.Load_Perspective
           (DAP_Module_ID.Get_Kernel, "Debug");

         --  hide persistent breakpoints
         DAP.Modules.Persistent_Breakpoints.Hide_Breakpoints (Kernel);
      else
         Client_Started := Client_Started + 1;
      end if;

      DAP_Module_ID.Clients.Append (Client);

      --  Create console
      DAP.Views.Consoles.Attach_To_Debugger_Console
        (Client              => Client,
         Kernel              => Kernel,
         Create_If_Necessary => True,
         Name                => " " & (+Base_Name (File)));

      if Client.Get_Debugger_Console /= null then
         Client_ID_Callback.Connect
           (Client.Get_Debugger_Console,
            Signal_Destroy,
            On_Console_Destroy'Access,
            After       => True,
            User_Data   => Client.Id);

         DAP.Views.Consoles.Get_Debugger_Interactive_Console
           (DAP.Clients.DAP_Client (Client.all)).Display_Prompt;
      end if;

      Client.Start (Project, File, Executable_Args);

      DAP.Modules.Persistent_Breakpoints.Hide_Breakpoints (Kernel);
      Set_Current_Debugger (Client);

      --  Give the focus to the Debugger Console
      Console_Child := Find_MDI_Child
        (Get_MDI (Kernel), Client.Get_Debugger_Console);

      if Console_Child /= null then
         Raise_Child (Console_Child);
      end if;

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

      DAP.Modules.Persistent_Breakpoints.On_Destroy;

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
      Id : Integer;
   begin
      DAP_Module_ID.Deallocate_Id := No_Source_Id;

      for Client of DAP_Module_ID.Clients_To_Deallocate loop
         Id := Client.Id;
         Free (Client);
         Dap_Debugger_Unloaded_Hook.Run (DAP_Module_ID.Kernel, Id);
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

      --  Return immediately if we are not hovering on an entity
      if not Has_Entity_Name_Information (Context) then
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
         --  Retrieve the debugger output
         Client.Value_Of
           (Entity => Variable_Name,
            Label  => Label);
         --  If the tooltips is too long wrap it
         Label.Set_Line_Wrap (True);
         Label.Set_Max_Width_Chars (80);
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

         DAP.Modules.Persistent_Breakpoints.
           Save_Persistent_Breakpoints (Kernel);
      end if;

      return True;  --  allow exit
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
               Main : constant Virtual_File :=
                 To_File (Kernel, Mains.List (J).Tuple (2).Str);
               Prj  : constant Virtual_File :=
                 To_File (Kernel, Mains.List (J).Tuple (3).Str);
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

   overriding function Execute
     (Command : access Initialize_Debugger_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      Initialize_Debugger
        (Kernel          => Get_Kernel (Context.Context),
         Project         => Command.Project,
         File            => Command.Exec,
         Executable_Args => "");

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
         Start (Kernel, Client);
         return Commands.Success;
      end if;
   end Execute;

   -----------
   -- Start --
   -----------

   procedure Start
     (Kernel : Kernel_Handle;
      Client : DAP.Clients.DAP_Client_Access)
   is
      use type DAP.Types.Debugger_Status_Kind;
      Ignore  : Gtkada.Dialogs.Message_Dialog_Buttons;
      pragma Unreferenced (Ignore);
   begin
      if Client.Get_Status /= DAP.Types.Ready then
         Ignore := GUI_Utils.GPS_Message_Dialog
           ("Cannot rerun while the underlying debugger is busy." &
              ASCII.LF &
              "Interrupt the debugger or wait for its availability.",
            Dialog_Type => Gtkada.Dialogs.Warning,
            Buttons     => Gtkada.Dialogs.Button_OK,
            Parent      => Kernel.Get_Main_Window);
      else
         --  Launch the application
         Start_Program (Client);
      end if;
   end Start;

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

      if Client.Get_Status = DAP.Types.Ready then
         Start (GPS.Kernel.Get_Kernel (Context.Context), Client);

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
      Kernel : constant Kernel_Handle :=
        GPS.Kernel.Get_Kernel (Context.Context);
      PID    : constant String :=
        GUI_Utils.Query_User
          (Parent        => Kernel.Get_Main_Window,
           Prompt        => "Enter the PID of the process to attach to",
           Password_Mode => False);
      Client : constant DAP.Clients.DAP_Client_Access :=
        Get_Current_Debugger;
      Attach_Req : DAP.Clients.Attach.Attach_Request_Access :=
        DAP.Clients.Attach.Create
          (Kernel => Kernel,
           PID    => Integer'Value (PID));
   begin
      Client.Enqueue
        (DAP.Requests.DAP_Request_Access (Attach_Req));

      return Commands.Success;
   end Execute;

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
     (Filter  : access Debugger_Ready_State_Filter;
      Context : Selection_Context) return Boolean
   is
      use DAP.Types;
      use type DAP.Clients.DAP_Client_Access;
   begin
      return DAP.Module.Get_Current_Debugger /= null
        and then DAP.Module.Get_Current_Debugger.Get_Status = Ready;
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
     (Filter  : access No_Debugger_Or_Ready_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return DAP_Module_ID = null
        or else DAP_Module_ID.Clients.Is_Empty
        or else Get_Current_Debugger.Is_Ready_For_Command;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Ready_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return DAP_Module_ID /= null
        and then not DAP_Module_ID.Clients.Is_Empty
        and then Get_Current_Debugger.Is_Ready_For_Command;
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
        and then not DAP.Modules.Contexts.Get_Variable (Context).Cmd.Is_Empty
      then
         return False;
      else
         return True;
      end if;
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

   function Get_Debugger (Id : Integer) return DAP.Clients.DAP_Client_Access is
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
      Num : Integer)
      return DAP.Clients.DAP_Client_Access is
   begin
      if Id = null then
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
      Executable_Args : String := "")
   is
      Dummy : DAP.Clients.DAP_Client_Access;
   begin
      Dummy := Initialize_Debugger
        (Kernel          => GPS.Kernel.Kernel_Handle (Kernel),
         File            => File,
         Project         => Project,
         Executable_Args => Executable_Args);
   end Initialize_Debugger;

   -------------------------
   -- Initialize_Debugger --
   -------------------------

   function Initialize_Debugger
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      File            : GNATCOLL.VFS.Virtual_File := No_File;
      Project         : Project_Type := No_Project;
      Executable_Args : String := "")
      return DAP.Clients.DAP_Client_Access is
   begin
      return Debug_Init
        (Kernel          => GPS.Kernel.Kernel_Handle (Kernel),
         Project         => Project,
         File            => File,
         Executable_Args => Executable_Args);
   end Initialize_Debugger;

   ------------------------
   -- On_Console_Destroy --
   ------------------------

   procedure On_Console_Destroy
     (Console : access Generic_Views.View_Record'Class;
      Id      : Integer)
   is
      pragma Unreferenced (Console);
      use type DAP.Clients.DAP_Client_Access;

      Client : constant DAP.Clients.DAP_Client_Access := Get_Debugger (Id);
   begin
      if Client /= null then
         Client.Set_Debugger_Console (null);
         Client.Quit;
      end if;
   end On_Console_Destroy;

   --------------
   -- Finished --
   --------------

   procedure Finished (Id : Positive) is
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
               DAP_Module_ID.Current_Debuger_ID := 0;
            else
               Set_Current_Debugger (DAP_Module_ID.Clients.First_Element);
            end if;
         end if;
      end if;

      --  if it was the last debugger
      if DAP_Module_ID.Clients.Is_Empty then
         --  The last debugger has been finished
         DAP_Module_ID.Client_ID := 1;

         --  save persistent breakpoints
         DAP.Modules.Persistent_Breakpoints.On_Debugging_Terminated
           (DAP_Module_ID.Get_Kernel);

         GPS.Kernel.MDI.Load_Perspective (DAP_Module_ID.Get_Kernel, "Default");

         DAP_Module_ID.Get_Kernel.Refresh_Context;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Finished;

   --------------------------
   -- Set_Current_Debugger --
   --------------------------

   procedure Set_Current_Debugger (Current : DAP.Clients.DAP_Client_Access)
   is
      use DAP.Types;
      use type DAP.Clients.DAP_Client_Access;
      use type Generic_Views.Abstract_View_Access;

      Set : Boolean;

   begin
      Set := Set_Current_Debugger (DAP_Module_ID, Current);

      if Set
        and then Current /= null
      then
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Current.Kernel, Current.Get_Visual);

         Current.Show_Breakpoints;

         if Breakpoints_View /= null then
            DAP.Views.View_Access (Breakpoints_View).On_Status_Changed
              (if Current.Get_Status /= Stopped
               and then Current.Get_Status /= Ready
               then GPS.Debuggers.Debug_Busy
               else GPS.Debuggers.Debug_Available);
         end if;
      end if;
   end Set_Current_Debugger;

   --------------------------
   -- Set_Current_Debugger --
   --------------------------

   function Set_Current_Debugger
     (Id : DAP_Module; Current : DAP.Clients.DAP_Client_Access)
      return Boolean
   is
      use type DAP.Clients.DAP_Client_Access;
   begin
      if Id = null
        or else (Current /= null
                 and then Id.Current_Debuger_ID = Current.Id)
      then
         return False;
      end if;

      if DAP.Modules.Preferences.Continue_To_Line_Buttons.Get_Pref then

         --  If we are creating a debugger, enable the 'Continue to line' icons
         --  on editors.
         --  If we are setting the current debugger to null (i.e: when all the
         --  debuggers are closed), make sure to disable them

         --  To_Do: Implement Continue_To_Line
         null;

         --  if Id.Current_Debuger_ID = 0
         --    and then Current /= null
         --  then
         --     Enable_Continue_To_Line_On_Editors (Id.Kernel);
         --  elsif Current = null then
         --     Disable_Continue_To_Line_On_Editors (Id.Kernel);
         --  end if;
      end if;

      if Current /= null then
         Id.Current_Debuger_ID := Current.Id;
      else
         Id.Current_Debuger_ID := 0;
      end if;

      return True;
   end Set_Current_Debugger;

   -------------------
   -- Start_Program --
   -------------------

   procedure Start_Program
     (Client : DAP.Clients.DAP_Client_Access) is
   begin
      DAP.Clients.ConfigurationDone.Send_Configuration_Done (Client);
   end Start_Program;

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

   ----------------------------
   -- Get_Breakpoint_From_Id --
   ----------------------------

   function Get_Breakpoint_From_Id
     (Id : DAP.Types.Breakpoint_Identifier)
      return DAP.Modules.Breakpoints.Breakpoint_Data
   is
      use type DAP.Clients.DAP_Client_Access;
      use DAP.Modules.Breakpoints;
   begin
      if Get_Current_Debugger = null then
         for Data of DAP.Modules.Persistent_Breakpoints.
           Get_Persistent_Breakpoints
         loop
            if Data = Id then
               return Data;
            end if;
         end loop;

      else
         for Data of Get_Current_Debugger.Get_Breakpoints loop
            if Data = Id then
               return Data;
            end if;
         end loop;
      end if;

      return DAP.Modules.Breakpoints.Empty_Breakpoint_Data;
   end Get_Breakpoint_From_Id;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Has_Debugger          : Action_Filter;
      No_Debugger_Or_Ready  : Action_Filter;
      Debugger_Ready        : Action_Filter;
      Debugger_Ready_State  : Action_Filter;
      Debugger_Stopped      : Action_Filter;
      Breakable_Filter      : Action_Filter;
      Entity_Filter         : Action_Filter;
      Is_Not_Command_Filter : Action_Filter;

   begin
      Before_Exit_Action_Hook.Add (new On_Before_Exit);

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

      No_Debugger_Or_Ready := new No_Debugger_Or_Ready_Filter;
      Register_Filter
        (Kernel, No_Debugger_Or_Ready, "No debugger or ready");

      Debugger_Ready := new Debugger_Ready_Filter;
      Register_Filter (Kernel, Debugger_Ready, "Debugger ready");

      Debugger_Stopped := new Debugger_Stopped_Filter;
      Register_Filter (Kernel, Debugger_Stopped, "Debugger stopped");

      Breakable_Filter := new Breakable_Source_Filter;
      Register_Filter
        (Kernel, Breakable_Filter, "Debugger breakable source");

      Entity_Filter := new Entity_Name_Filter;
      Register_Filter (Kernel, Entity_Filter, "Debugger entity name");

      Debugger_Ready_State := new Debugger_Ready_State_Filter;
      Register_Filter (Kernel, Debugger_Ready_State, "Debugger ready state");

      Is_Not_Command_Filter := new Not_Command_Filter;
      Register_Filter
        (Kernel, Is_Not_Command_Filter, "Debugger not command variable");

      --  Actions --

      Project_View_Changed_Hook.Add (new On_Project_View_Changed);

      GPS.Kernel.Modules.UI.Register_Contextual_Submenu
        (Kernel, "Debug",
         Group => GPS.Kernel.Modules.UI.Debug_Contextual_Group);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "terminate debugger", new Terminate_Command,
         Icon_Name   => "gps-debugger-terminate-symbolic",
         Description => "Terminate the current debugger",
         Filter      => Has_Debugger);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "terminate all debuggers", new Terminate_All_Command,
         Description => "Terminate all running debugger",
         Filter      => Has_Debugger);

      GPS.Kernel.Modules.UI.Register_Contextual_Submenu
        (Kernel, "Debug",
         Group => GPS.Kernel.Modules.UI.Debug_Contextual_Group);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug run dialog", new Start_Command,
         Filter      => Debugger_Ready_State,
         Description =>
           "Choose the arguments to the program, and start running it",
         Category    => "Debug");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug continue", new Continue_Command,
         Icon_Name    => "gps-debugger-run-symbolic",
         Filter       => Debugger_Ready,
         Description  =>
           "Continue execution until next breakpoint." & ASCII.LF
         & "Start the debugger if not started yet",
         Category     => "Debug",
         For_Learning => True);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug next", new Next_Command,
         Icon_Name    => "gps-debugger-next-symbolic",
         Filter       => Debugger_Ready,
         Description  =>
           "Execute the program until the next source line, stepping over"
         & " subprogram calls",
         Category     => "Debug",
         For_Learning => True);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug nexti", new Nexti_Command,
         Filter      => Debugger_Ready,
         Description =>
           "Execute the program until the next machine instruction, stepping"
         & " over subprogram calls",
         Category    => "Debug");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug step", new Step_Command,
         Icon_Name    => "gps-debugger-step-symbolic",
         Filter       => Debugger_Ready,
         Description  =>
           "Execute until program reaches a new line of source code",
         Category     => "Debug",
         For_Learning => True);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug stepi", new Stepi_Command,
         Filter      => Debugger_Ready,
         Description =>
           "Execute the program for one machine instruction only",
         Category    => "Debug");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug interrupt", new Interrupt_Command,
         Icon_Name    => "gps-debugger-pause-symbolic",
         Filter       => Has_Debugger,
         Description  => "Asynchronously interrupt the debuggee program",
         Category     => "Debug",
         For_Learning => True);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug connect to board", new Connect_To_Board_Command,
         Description =>
           "Opens a simple dialog to connect to a remote board. This option"
           & " is only relevant to cross debuggers.",
         Filter   => No_Debugger_Or_Ready,
         Category => "Debug");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug attach", new Attach_Command,
         Description => "Attach to a running process",
         Filter      => Has_Debugger,
         Category    => "Debug");

      DAP.Modules.Persistent_Breakpoints.Register_Module (Kernel);
      DAP.Views.Call_Stack.Register_Module (Kernel);
      DAP.Views.Threads.Register_Module (Kernel);
      DAP.Views.Assembly.Register_Module (Kernel);
      DAP.Modules.Scripts.Register_Module (Kernel);
      DAP.Views.Consoles.Register_Module (Kernel);
      DAP.Views.Memory.Register_Module (Kernel);
      DAP.Views.Variables.Register_Module (Kernel);
      DAP.Views.Registers.Register_Module (Kernel);
   end Register_Module;

   -------------
   -- To_File --
   -------------

   function To_File
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Name    : String)
      return GNATCOLL.VFS.Virtual_File
   is
      F : Virtual_File;
   begin
      --  Translate filename into local file if needed
      F := To_Local
        (Create (+Name, Remote.Get_Nickname (Remote.Debug_Server)));

      --  Convert from a patch returned by the debugger to the actual
      --  path in the project, in case sources have changed
      if not F.Is_Absolute_Path or else not F.Is_Regular_File then
         F := Kernel.Create_From_Base (F.Full_Name);
      end if;

      return F;
   end To_File;

end DAP.Module;
