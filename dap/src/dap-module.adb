------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Any_Types;           use GNATCOLL.Any_Types;
with GNATCOLL.Projects;            use GNATCOLL.Projects;
with GNATCOLL.Traces;              use GNATCOLL.Traces;

with Gtk.Label;                    use Gtk.Label;
with Gtk.Widget;                   use Gtk.Widget;

with Gtkada.Dialogs;

with Commands;                     use Commands;
with Commands.Interactive;         use Commands.Interactive;
with Language;                     use Language;
with GPS.Editors;                  use GPS.Editors;
with GPS.Kernel.Actions;
with GPS.Kernel.Contexts;          use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;
with GPS.Kernel.Modules;           use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;
with GPS.Kernel.Project;
with GUI_Utils;
with Remote;

with DAP.Contexts;
with DAP.Persistent_Breakpoints;
with DAP.Preferences;
with DAP.Requests.ConfigurationDone;
with DAP.Requests.Continue;
with DAP.Requests.Next;
with DAP.Requests.Step_In_Request;
with DAP.Scripts;
with DAP.Tools;                    use DAP.Tools;
with DAP.Types;
with DAP.Views.Call_Stack;
with DAP.Views.Threads;

package body DAP.Module is

   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.DAP_MODULE", Off);
   --  Enable/disable DAP support

   package DAP_Client_Vectors is new Ada.Containers.Vectors
     (Positive, DAP.Clients.DAP_Client_Access, "=" => DAP.Clients."=");
   subtype DAP_Client_Vector is DAP_Client_Vectors.Vector;

   procedure Free is new Ada.Unchecked_Deallocation
     (DAP.Clients.DAP_Client'Class, DAP.Clients.DAP_Client_Access);

   type DAP_Module_Record is new Module_ID_Record with record
      Dynamic_Actions : Action_Lists.List;
      --  Actions that have been registered dynamically by this module,
      --  for the dynamic menus

      Clients            : DAP_Client_Vector;
      --  Clients that handles DAP requests

      Current_Debuger_ID : Integer := 0;
      --  Client that is used as a current active debugger

      Client_ID          : Positive := 1;
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

   overriding function Tooltip_Handler
     (Module  : access DAP_Module_Record;
      Context : Selection_Context) return Gtk_Widget;
   --  See inherited documentation

   -- Hooks callbacks --

   type On_Project_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called every time the project view changes, to recompute the dynamic
   --  menus.

   -- Filters --

   type Has_Debuggers_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Debuggers_Filter;
      Context : Selection_Context) return Boolean;

   type Debugger_Ready_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Ready_Filter;
      Context : Selection_Context) return Boolean;

   type Debuggers_Stopped_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Debuggers_Stopped_Filter;
      Context : Selection_Context) return Boolean;

   type No_Debugger_Or_Stopped_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access No_Debugger_Or_Stopped_Filter;
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

   -- Utils --

   procedure Debug_Init
     (Kernel  : GPS.Kernel.Kernel_Handle;
      Project : Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : String);
   --  Initialize the debugger

   procedure Start_Program
     (Kernel : Kernel_Handle;
      Client : DAP.Clients.DAP_Client_Access);

   function To_File
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Name    : String)
      return GNATCOLL.VFS.Virtual_File;

   DAP_Module_Name : constant String := "DAP";
   Debug_Adapter   : Ada.Strings.Unbounded.Unbounded_String;

   DAP_Module_ID   : DAP_Module;

   ----------------
   -- Debug_Init --
   ----------------

   procedure Debug_Init
     (Kernel  : GPS.Kernel.Kernel_Handle;
      Project : Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : String)
   is
      Client : constant DAP.Clients.DAP_Client_Access :=
        new DAP.Clients.DAP_Client (Kernel, DAP_Module_ID.Client_ID);
   begin
      DAP_Module_ID.Current_Debuger_ID := DAP_Module_ID.Client_ID;

      if DAP_Module_ID.Client_ID < Positive'Last then
         DAP_Module_ID.Client_ID := DAP_Module_ID.Client_ID + 1;
      else
         DAP_Module_ID.Client_ID := 1;
      end if;

      if DAP_Module_ID.Clients.Is_Empty then
         --  Start first debugger

         --  Switch to the "Debug" perspective if available
         GPS.Kernel.MDI.Load_Perspective
           (DAP_Module_ID.Get_Kernel, "DAP_Debug");

         --  hide persistent breakpoints
         DAP.Persistent_Breakpoints.Hide_Breakpoints (Kernel);
      end if;

      DAP_Module_ID.Clients.Append (Client);
      Client.Start
        (Ada.Strings.Unbounded.To_String (Debug_Adapter),
         Project,
         File,
         Args);
   end Debug_Init;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Id : in out DAP_Module_Record) is
   begin
      for D of Id.Clients loop
         begin
            D.Quit;
         exception
            when E : others =>
               Trace (Me, E);
         end;
      end loop;
   end Destroy;

   ---------------------
   -- Tooltip_Handler --
   ---------------------

   overriding function Tooltip_Handler
     (Module  : access DAP_Module_Record;
      Context : Selection_Context) return Gtk_Widget
   is
      pragma Unreferenced (Module);
      use type DAP.Clients.DAP_Client_Access;
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Client : constant DAP.Clients.DAP_Client_Access :=
        Get_Current_Debugger;
      W      : Gtk_Widget;
      Label  : Gtk_Label;
   begin
      if Client = null then
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
      Debug_Init
         (Get_Kernel (Context.Context), Command.Project, Command.Exec, "");

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
      if DAP_Module_ID.Current_Debuger_ID /= 0 then
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
      use type DAP.Types.Debugger_Status_Kind;
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Client : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
      Ignore  : Gtkada.Dialogs.Message_Dialog_Buttons;
      pragma Unreferenced (Ignore);
   begin

      if Client = null then
         return Commands.Failure;
      end if;

      if Client.Get_Status /= DAP.Types.Ready then
         Ignore := GUI_Utils.GPS_Message_Dialog
           ("Cannot rerun while the underlying debugger is busy." &
            ASCII.LF &
            "Interrupt the debugger or wait for its availability.",
            Dialog_Type => Gtkada.Dialogs.Warning,
            Buttons     => Gtkada.Dialogs.Button_OK,
            Parent      => Kernel.Get_Main_Window);
         return Commands.Failure;
      end if;

      --  Launch the application

      Start_Program (Kernel, Client);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Continue_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      Req : DAP.Requests.Continue.Continue_DAP_Request_Access :=
        new DAP.Requests.Continue.Continue_DAP_Request
          (GPS.Kernel.Get_Kernel (Context.Context));
   begin
      Get_Current_Debugger.Enqueue (DAP.Requests.DAP_Request_Access (Req));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Next_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      Req : DAP.Requests.Next.Next_DAP_Request_Access :=
        new DAP.Requests.Next.Next_DAP_Request
          (GPS.Kernel.Get_Kernel (Context.Context));
   begin
      Get_Current_Debugger.Enqueue (DAP.Requests.DAP_Request_Access (Req));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Nexti_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      Req : DAP.Requests.Next.Next_DAP_Request_Access :=
        new DAP.Requests.Next.Next_DAP_Request
          (GPS.Kernel.Get_Kernel (Context.Context));
   begin
      Req.Parameters.arguments.granularity :=
        (Is_Set => True, Value => Enum.instruction);
      Get_Current_Debugger.Enqueue (DAP.Requests.DAP_Request_Access (Req));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Step_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      Req : DAP.Requests.Step_In_Request.Step_In_DAP_Request_Access :=
        new DAP.Requests.Step_In_Request.Step_In_DAP_Request
          (GPS.Kernel.Get_Kernel (Context.Context));
   begin
      Get_Current_Debugger.Enqueue (DAP.Requests.DAP_Request_Access (Req));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Stepi_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      Req : DAP.Requests.Step_In_Request.Step_In_DAP_Request_Access :=
        new DAP.Requests.Step_In_Request.Step_In_DAP_Request
          (GPS.Kernel.Get_Kernel (Context.Context));
   begin
      Req.Parameters.arguments.granularity :=
        (Is_Set => True, Value => Enum.instruction);
      Get_Current_Debugger.Enqueue (DAP.Requests.DAP_Request_Access (Req));
      return Commands.Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Debuggers_Filter;
      Context : Selection_Context) return Boolean is
   begin
      return not DAP_Module_ID.Clients.Is_Empty;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Ready_Filter;
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
     (Filter  : access Debuggers_Stopped_Filter;
      Context : Selection_Context) return Boolean is
   begin
      return not DAP_Module_ID.Clients.Is_Empty
        and then Get_Current_Debugger.Is_Stopped;
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
     (Filter  : access No_Debugger_Or_Stopped_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return DAP_Module_ID.Clients.Is_Empty
        or else Get_Current_Debugger.Is_Stopped;
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

   -----------------------
   -- For_Each_Debugger --
   -----------------------

   procedure For_Each_Debugger
     (Callback : access procedure (Debugger : DAP.Clients.DAP_Client_Access))
   is
   begin
      for C of DAP_Module_ID.Clients loop
         Callback (C);
      end loop;
   end For_Each_Debugger;

   -----------------------------
   -- Count_Running_Debuggers --
   -----------------------------

   function Count_Running_Debuggers return Natural is
   begin
      return Natural (DAP_Module_ID.Clients.Length);
   end Count_Running_Debuggers;

   --------------------------
   -- Get_Current_Debugger --
   --------------------------

   function Get_Current_Debugger return DAP.Clients.DAP_Client_Access is
   begin
      return Get_Current_Debugger (DAP_Module_ID);
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
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Args   : String) is
   begin
      Debug_Init
        (GPS.Kernel.Kernel_Handle (Kernel),
         GPS.Kernel.Project.Get_Project (Kernel),
         No_File,
         Args);
   end Initialize_Debugger;

   --------------
   -- Finished --
   --------------

   procedure Finished (Id : Positive) is
      use DAP_Client_Vectors;
      use type DAP.Clients.DAP_Client_Access;

      C      : DAP_Client_Vectors.Cursor := DAP_Module_ID.Clients.First;
      Client : DAP.Clients.DAP_Client_Access := null;
   begin
      while Has_Element (C) loop
         if Element (C).Id = Id then
            Client := Element (C);
            exit;
         end if;
         Next (C);
      end loop;

      if Client /= null then
         DAP_Module_ID.Clients.Delete (C);
         Free (Client);

         if DAP_Module_ID.Current_Debuger_ID = Id then
            if DAP_Module_ID.Clients.Is_Empty then
               DAP_Module_ID.Current_Debuger_ID := 0;
            else
               DAP_Module_ID.Current_Debuger_ID :=
                 DAP_Module_ID.Clients.First_Element.Id;
            end if;
         end if;
      end if;

      if DAP_Module_ID.Clients.Is_Empty then
         --  The last debugger has been finished
         DAP_Module_ID.Client_ID := 1;
         GPS.Kernel.MDI.Load_Perspective (DAP_Module_ID.Get_Kernel, "Default");

         --  Show persistent breakpoints
         DAP.Persistent_Breakpoints.Show_Breakpoints_In_All_Editors
           (DAP_Module_ID.Get_Kernel);

         DAP_Module_ID.Get_Kernel.Refresh_Context;
      end if;
   end Finished;

   -------------------
   -- Start_Program --
   -------------------

   procedure Start_Program
     (Kernel : Kernel_Handle;
      Client : DAP.Clients.DAP_Client_Access)
   is
      Done : DAP.Requests.ConfigurationDone.
        ConfigurationDone_DAP_Request_Access :=
          new DAP.Requests.ConfigurationDone.
            ConfigurationDone_DAP_Request (Kernel);
   begin
      Client.Enqueue (DAP.Requests.DAP_Request_Access (Done));
   end Start_Program;

   -------------------------
   -- Terminate_Debuggers --
   -------------------------

   procedure Terminate_Debuggers is
   begin
      if DAP_Module_ID /= null then
         DAP_Module_ID.Destroy;
      end if;
   end Terminate_Debuggers;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Prefix_Dir : Virtual_File)
   is
      Debugger_Active        : Action_Filter;
      Debugger_Ready         : Action_Filter;
      Breakable_Filter       : Action_Filter;
      No_Debugger_Or_Stopped : Action_Filter;
      Entity_Filter          : Action_Filter;
      Debugger_Stopped       : Action_Filter;

   begin
      DAP.Preferences.Register_Default_Preferences (Kernel.Get_Preferences);

      DAP_Module_ID := new DAP_Module_Record;
      if Kernel /= null then
         Register_Module
           (Module          => Module_ID (DAP_Module_ID),
            Kernel          => Kernel,
            Module_Name     => DAP_Module_Name,
            Priority        => Default_Priority + 20);
      end if;

      Debug_Adapter := Ada.Strings.Unbounded.To_Unbounded_String
        (+Prefix_Dir.Create_From_Dir
           ("share/gnatstudio/cdt-gdb-adapter/debugAdapter.js").Full_Name);

      -- Filters --

      Debugger_Active := new Has_Debuggers_Filter;
      Register_Filter (Kernel, Debugger_Active, "Has debuggers");

      Debugger_Stopped := new Debuggers_Stopped_Filter;
      Register_Filter (Kernel, Debugger_Stopped, "Debugger stopped");

      No_Debugger_Or_Stopped := new No_Debugger_Or_Stopped_Filter;
      Register_Filter
        (Kernel, No_Debugger_Or_Stopped, "No debugger or stopped");

      Breakable_Filter := new Breakable_Source_Filter;
      Register_Filter
        (Kernel, Breakable_Filter, "Debugger breakable source");

      Entity_Filter := new Entity_Name_Filter;
      Register_Filter (Kernel, Entity_Filter, "Debugger entity name");

      Debugger_Ready := new Debugger_Ready_Filter;
      Register_Filter (Kernel, Debugger_Ready, "Debugger ready");
      --  Actions --

      Project_View_Changed_Hook.Add (new On_Project_View_Changed);

      GPS.Kernel.Modules.UI.Register_Contextual_Submenu
        (Kernel, "Debug",
         Group => GPS.Kernel.Modules.UI.Debug_Contextual_Group);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "terminate debugger", new Terminate_Command,
         Icon_Name   => "gps-debugger-terminate-symbolic",
         Description => "Terminate the current debugger",
         Filter      => Debugger_Active);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "terminate all debuggers", new Terminate_All_Command,
         Description => "Terminate all running debugger",
         Filter      => Debugger_Active);

      GPS.Kernel.Modules.UI.Register_Contextual_Submenu
        (Kernel, "Debug",
         Group => GPS.Kernel.Modules.UI.Debug_Contextual_Group);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug run dialog", new Start_Command,
         Filter      => Debugger_Ready,
         Description =>
           "Choose the arguments to the program, and start running it",
         Category    => "Debug");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug continue", new Continue_Command,
         Icon_Name    => "gps-debugger-run-symbolic",
         Filter       => Debugger_Stopped,
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

      DAP.Persistent_Breakpoints.Register_Module (Kernel);
      DAP.Views.Call_Stack.Register_Module (Kernel);
      DAP.Views.Threads.Register_Module (Kernel);
      DAP.Scripts.Register_Module (Kernel);
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
