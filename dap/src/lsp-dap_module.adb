------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

with GNATCOLL.VFS;       use GNATCOLL.VFS;
with GNATCOLL.Traces;    use GNATCOLL.Traces;
with GNATCOLL.Projects;  use GNATCOLL.Projects;
with GNATCOLL.Any_Types; use GNATCOLL.Any_Types;

with GUI_Utils;             use GUI_Utils;
with GPS.Editors;           use GPS.Editors;
with GPS.Kernel.MDI;        use GPS.Kernel.MDI;
with GPS.Kernel.Hooks;      use GPS.Kernel.Hooks;
with GPS.Kernel.Actions;    use GPS.Kernel.Actions;
with GPS.Kernel.Modules;    use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI; use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;    use GPS.Kernel.Project;

with Commands;             use Commands;
with Commands.Interactive; use Commands.Interactive;

with LSP.Types; use LSP.Types;
with LSP.JSON_Streams;

with VSS.Strings; use VSS.Strings;
with VSS.Strings.Conversions;
with VSS.Text_Streams.Memory_UTF8_Output;

with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Spawn.String_Vectors;
with GVD.Breakpoints_List;    use GVD.Breakpoints_List;
with GVD.Code_Editors;
with LSP.DAP_Clients;
with DAP.Tools;               use DAP.Tools;
with DAP.Breakpoint_Map;
with Debugger;

package body LSP.DAP_Module is
   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.DAP_MODULE");
   DAP_Module_Name : constant String       := "DAP";

   node_binary   : constant String := "/usr/bin/node";
   debug_Adapter : constant String :=
     "/home/dotty/cdt-gdb-adapter/dist/debugAdapter.js";

   program : constant Virtual_String := "/home/dotty/simple/obj/main";
   --  debuggee

   args : Spawn.String_Vectors.UTF_8_String_Vector;

   type Module_Id_Record is new GPS.Kernel.Modules.Module_ID_Record with record
      Actions : Action_Lists.List;
      --  Actions that have been registered dynamically by this module,
      --  for the dynamic menus

      Client : LSP.DAP_Clients.Client;
      --  Clients that handles DAP requests

      Step : LSP_Number := 0;
      --  to be removed, only used for development phase
   end record;

   type DAP_Module_Id is access all Module_Id_Record'Class;

   Module : DAP_Module_Id;

   --  Contextual Filters

   type DAP_Started_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter : access DAP_Started_Filter; Context : Selection_Context)
      return Boolean;

   overriding function Filter_Matches_Primitive
     (Filter : access DAP_Started_Filter; Context : Selection_Context)
      return Boolean
   is
      pragma Unreferenced (Filter, Context);
   begin
      return Module.Client.Started;
   end Filter_Matches_Primitive;

   type DAP_Running_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter : access DAP_Running_Filter; Context : Selection_Context)
      return Boolean;

   overriding function Filter_Matches_Primitive
     (Filter : access DAP_Running_Filter; Context : Selection_Context)
      return Boolean
   is
      pragma Unreferenced (Filter, Context);
   begin
      return Module.Client.Running;
   end Filter_Matches_Primitive;

   type Terminate_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Terminate_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  DAP->Terminate Current

   overriding function Execute
     (Command : access Terminate_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      DAP_Terminate (Kernel => Kernel);
      Update_Menus_And_Buttons (Kernel);
      return Commands.Success;

   exception
      when E : others =>
         Trace (Me, E);
         return Commands.Failure;
   end Execute;

   type Config_Done_Or_Continue_Command is new Interactive_Command with
   null record;
   overriding function Execute
     (Command : access Config_Done_Or_Continue_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   overriding function Execute
     (Command : access Config_Done_Or_Continue_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      pragma Unreferenced (Command, Kernel);
   begin
      if Module.Client.Configured then
         declare
            JS : aliased LSP.JSON_Streams.JSON_Stream
              (Is_Server_Side => False, R => null);
            Output : aliased VSS.Text_Streams.Memory_UTF8_Output
              .Memory_UTF8_Output_Stream;
            cont_rq : aliased ContinueRequest;
         begin
            Trace (Me, "Sent ContinueRequest");

            cont_rq.seq                := Module.Client.Get_Request_ID;
            cont_rq.a_type             := "request";
            cont_rq.command            := "continue";
            cont_rq.arguments.threadId := 1;

            JS.Set_Stream (Output'Unchecked_Access);
            Write_ContinueRequest (JS'Access, cont_rq'Unchecked_Access);
            JS.End_Document;
            Module.Client.Send_Buffer (Output.Buffer);
         end;
      else
         Send_Breakpoint_Request;
         delay 0.3;
         declare
            JS : aliased LSP.JSON_Streams.JSON_Stream
              (Is_Server_Side => False, R => null);
            Output : aliased VSS.Text_Streams.Memory_UTF8_Output
              .Memory_UTF8_Output_Stream;
            confdone_rq : aliased ConfigurationDoneRequest;
         begin
            Trace (Me, "Send ConfigurationDone request");

            confdone_rq.a_type  := "request";
            confdone_rq.seq     := Module.Client.Get_Request_ID;
            confdone_rq.command := "configurationDone";

            JS.Set_Stream (Output'Unchecked_Access);
            Write_ConfigurationDoneRequest
              (JS'Access, confdone_rq'Unchecked_Access);
            JS.End_Document;
            Module.Client.Send_Buffer (Output.Buffer);
         end;
      end if;
      return Commands.Success;

   exception
      when E : others =>
         Trace (Me, E);
         return Commands.Failure;
   end Execute;

   type Next_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Next_Command; Context : Interactive_Command_Context)
      return Command_Return_Type;

   overriding function Execute
     (Command : access Next_Command; Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      JS : aliased LSP.JSON_Streams.JSON_Stream
        (Is_Server_Side => False, R => null);
      Output : aliased VSS.Text_Streams.Memory_UTF8_Output
        .Memory_UTF8_Output_Stream;
      next_rq : aliased NextRequest;
   begin
      next_rq.seq     := Module.Client.Get_Request_ID;
      next_rq.a_type  := "request";
      next_rq.command := "next";

      next_rq.arguments.granularity := Enums.statement;
      next_rq.arguments.threadId    := 1;

      JS.Set_Stream (Output'Unchecked_Access);
      Write_NextRequest (JS'Access, next_rq'Unchecked_Access);
      JS.End_Document;
      Module.Client.Send_Buffer (Output.Buffer);

      return Commands.Success;
   end Execute;

   type Step_In_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Step_In_Command; Context : Interactive_Command_Context)
      return Command_Return_Type;

   overriding function Execute
     (Command : access Step_In_Command; Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      JS : aliased LSP.JSON_Streams.JSON_Stream
        (Is_Server_Side => False, R => null);
      Output : aliased VSS.Text_Streams.Memory_UTF8_Output
        .Memory_UTF8_Output_Stream;
      stepin_rq : aliased StepInRequest;
   begin
      stepin_rq.seq     := Module.Client.Get_Request_ID;
      stepin_rq.a_type  := "request";
      stepin_rq.command := "stepIn";

      stepin_rq.arguments.granularity := Enums.instruction;
      stepin_rq.arguments.targetId    := 0;
      stepin_rq.arguments.threadId    := 1;

      JS.Set_Stream (Output'Unchecked_Access);
      Write_StepInRequest (JS'Access, stepin_rq'Unchecked_Access);
      JS.End_Document;
      Module.Client.Send_Buffer (Output.Buffer);

      return Commands.Success;
   end Execute;

   type Step_Out_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Step_Out_Command; Context : Interactive_Command_Context)
      return Command_Return_Type;

   overriding function Execute
     (Command : access Step_Out_Command; Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      JS : aliased LSP.JSON_Streams.JSON_Stream
        (Is_Server_Side => False, R => null);
      Output : aliased VSS.Text_Streams.Memory_UTF8_Output
        .Memory_UTF8_Output_Stream;
      stepout_rq : aliased StepOutRequest;
   begin
      stepout_rq.seq     := Module.Client.Get_Request_ID;
      stepout_rq.a_type  := "request";
      stepout_rq.command := "stepOut";

      stepout_rq.arguments.granularity := Enums.statement;
      stepout_rq.arguments.threadId    := 1;

      JS.Set_Stream (Output'Unchecked_Access);
      Write_StepOutRequest (JS'Access, stepout_rq'Unchecked_Access);
      JS.End_Document;
      Module.Client.Send_Buffer (Output.Buffer);

      return Commands.Success;
   end Execute;

   type Test_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Test_Command; Context : Interactive_Command_Context)
      return Command_Return_Type;

   overriding function Execute
     (Command : access Test_Command; Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      case Module.Step is
         when 0 =>
            declare
               JS : aliased LSP.JSON_Streams.JSON_Stream
                 (Is_Server_Side => False, R => null);
               Output : aliased VSS.Text_Streams.Memory_UTF8_Output
                 .Memory_UTF8_Output_Stream;
               init_rq : aliased InitializeRequest;
            begin

               Module.Client.Set_Program (node_binary);
               args.Append (debug_Adapter);
               Module.Client.Set_Arguments (args);
               Module.Client.Start;

               Trace
                 (Me,
                  "Launching the debug adapter: " & node_binary & " " &
                  debug_Adapter);

               Trace (Me, "Sending ""initialize"" request");

               init_rq.seq     := Module.Client.Get_Request_ID;
               init_rq.a_type  := "request";
               init_rq.command := "initialize";

               init_rq.arguments.adapterID  := "0";
               init_rq.arguments.clientID   := "0";
               init_rq.arguments.clientName := "GNATSTUDIO";
               init_rq.arguments.locale     := "fr-FR";
               init_rq.arguments.pathFormat := "path";

               init_rq.arguments.columnsStartAt1              := True;
               init_rq.arguments.linesStartAt1                := True;
               init_rq.arguments.supportsInvalidatedEvent     := False;
               init_rq.arguments.supportsMemoryReferences     := False;
               init_rq.arguments.supportsProgressReporting    := True;
               init_rq.arguments.supportsRunInTerminalRequest := False;
               init_rq.arguments.supportsVariablePaging       := False;
               init_rq.arguments.supportsVariableType         := False;

               JS.Set_Stream (Output'Unchecked_Access);
               Write_InitializeRequest (JS'Access, init_rq'Unchecked_Access);
               JS.End_Document;
               Module.Client.Send_Buffer (Output.Buffer);
            end;
         when 1 =>
            declare
               JS : aliased LSP.JSON_Streams.JSON_Stream
                 (Is_Server_Side => False, R => null);
               Output : aliased VSS.Text_Streams.Memory_UTF8_Output
                 .Memory_UTF8_Output_Stream;
               launch_rq : aliased LaunchRequest;
            begin
               Trace (Me, "Sending launch request");

               launch_rq.seq                 := Module.Client.Get_Request_ID;
               launch_rq.a_type              := "request";
               launch_rq.command             := "launch";
               launch_rq.arguments.noDebug   := False;
               launch_rq.arguments.a_restart := LSP.Types.Empty;
               launch_rq.arguments.program   := program;

               JS.Set_Stream (Output'Unchecked_Access);
               Write_LaunchRequest (JS'Access, launch_rq'Unchecked_Access);
               JS.End_Document;
               Module.Client.Send_Buffer (Output.Buffer);
            end;
         when 2 =>
            Send_Breakpoint_Request;
         when 3 =>
            declare
               JS : aliased LSP.JSON_Streams.JSON_Stream
                 (Is_Server_Side => False, R => null);
               Output : aliased VSS.Text_Streams.Memory_UTF8_Output
                 .Memory_UTF8_Output_Stream;
               confdone_rq : aliased ConfigurationDoneRequest;
            begin
               Trace (Me, "Send ConfigurationDone request");

               confdone_rq.seq     := Module.Client.Get_Request_ID;
               confdone_rq.a_type  := "request";
               confdone_rq.command := "configurationDone";

               JS.Set_Stream (Output'Unchecked_Access);
               Write_ConfigurationDoneRequest
                 (JS'Access, confdone_rq'Unchecked_Access);
               JS.End_Document;
               Module.Client.Send_Buffer (Output.Buffer);
            end;
         when 4 =>
            declare
               JS : aliased LSP.JSON_Streams.JSON_Stream
                 (Is_Server_Side => False, R => null);
               Output : aliased VSS.Text_Streams.Memory_UTF8_Output
                 .Memory_UTF8_Output_Stream;
               thrq : aliased ThreadsRequest;
            begin
               Trace (Me, "Send ThreadRequest");

               thrq.seq     := Module.Client.Get_Request_ID;
               thrq.a_type  := "request";
               thrq.command := "threads";

               JS.Set_Stream (Output'Unchecked_Access);
               Write_ThreadsRequest (JS'Access, thrq'Unchecked_Access);
               JS.End_Document;
               Module.Client.Send_Buffer (Output.Buffer);
            end;
         when 5 =>
            declare
               JS : aliased LSP.JSON_Streams.JSON_Stream
                 (Is_Server_Side => False, R => null);
               Output : aliased VSS.Text_Streams.Memory_UTF8_Output
                 .Memory_UTF8_Output_Stream;
               strq : aliased StackTraceRequest;
            begin
               Trace (Me, "Send stackTraceRequest");

               strq.seq                  := Module.Client.Get_Request_ID;
               strq.a_type               := "request";
               strq.command              := "stackTrace";
               strq.arguments.threadId   := 1;
               strq.arguments.startFrame := 0;
               strq.arguments.levels     := 0;

               JS.Set_Stream (Output'Unchecked_Access);
               Write_StackTraceRequest (JS'Access, strq'Unchecked_Access);
               JS.End_Document;
               Module.Client.Send_Buffer (Output.Buffer);
            end;
         when 6 =>
            declare
               JS : aliased LSP.JSON_Streams.JSON_Stream
                 (Is_Server_Side => False, R => null);
               Output : aliased VSS.Text_Streams.Memory_UTF8_Output
                 .Memory_UTF8_Output_Stream;
               scope_rq : aliased ScopesRequest;
            begin
               Trace (Me, "Send ScopeRequest");

               scope_rq.seq     := Module.Client.Get_Request_ID;
               scope_rq.a_type  := "request";
               scope_rq.command := "scopes";

               scope_rq.arguments.frameId := 1_000;

               JS.Set_Stream (Output'Unchecked_Access);
               Write_ScopesRequest (JS'Access, scope_rq'Unchecked_Access);
               JS.End_Document;
               Module.Client.Send_Buffer (Output.Buffer);
            end;
         when 7 =>
            declare
               JS : aliased LSP.JSON_Streams.JSON_Stream
                 (Is_Server_Side => False, R => null);
               Output : aliased VSS.Text_Streams.Memory_UTF8_Output
                 .Memory_UTF8_Output_Stream;
               var_rq : aliased VariablesRequest;
            begin
               Trace (Me, "Send VariablesRequest");

               var_rq.seq     := Module.Client.Get_Request_ID;
               var_rq.a_type  := "request";
               var_rq.command := "variables";

               var_rq.arguments.count              := 100;
               var_rq.arguments.filter             := "indexed";
               var_rq.arguments.format.hex         := False;
               var_rq.arguments.variablesReference := 1_000;
               var_rq.arguments.start              := 0;

               JS.Set_Stream (Output'Unchecked_Access);
               Write_VariablesRequest (JS'Access, var_rq'Unchecked_Access);
               JS.End_Document;
               Module.Client.Send_Buffer (Output.Buffer);
            end;
         when 8 =>
            declare
               JS : aliased LSP.JSON_Streams.JSON_Stream
                 (Is_Server_Side => False, R => null);
               Output : aliased VSS.Text_Streams.Memory_UTF8_Output
                 .Memory_UTF8_Output_Stream;
               cont_rq : aliased ContinueRequest;
            begin
               Trace (Me, "Sent ContinueRequest");

               cont_rq.seq                := Module.Client.Get_Request_ID;
               cont_rq.a_type             := "request";
               cont_rq.command            := "continue";
               cont_rq.arguments.threadId := 1;

               JS.Set_Stream (Output'Unchecked_Access);
               Write_ContinueRequest (JS'Access, cont_rq'Unchecked_Access);
               JS.End_Document;
               Module.Client.Send_Buffer (Output.Buffer);
            end;
         when others =>
            declare
               JS : aliased LSP.JSON_Streams.JSON_Stream
                 (Is_Server_Side => False, R => null);
               Output : aliased VSS.Text_Streams.Memory_UTF8_Output
                 .Memory_UTF8_Output_Stream;
               disco_rq : aliased DisconnectRequest;
            begin
               Trace (Me, "Send disconnect request and stop client");

               disco_rq.seq     := Module.Client.Get_Request_ID;
               disco_rq.a_type  := "request";
               disco_rq.command := "disconnect";

               disco_rq.arguments.restart           := False;
               disco_rq.arguments.restart           := False;
               disco_rq.arguments.terminateDebuggee := True;

               JS.Set_Stream (Output'Unchecked_Access);
               Write_DisconnectRequest (JS'Access, disco_rq'Unchecked_Access);
               JS.End_Document;
               Module.Client.Send_Buffer (Output.Buffer);

               Module.Step := -1;
            end;
      end case;
      Module.Step := Module.Step + 1;

      return Commands.Success;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (ASCII.LF & Exception_Name (E) & " - " & Exception_Message (E));
         Ada.Text_IO.Put_Line (Symbolic_Traceback (E));

         return Commands.Success;
   end Execute;

   type Initialize_DAP_Command is new Interactive_Command with record
      Project : Project_Type;
      Exec    : Virtual_File;
   end record;
   overriding function Execute
     (Command : access Initialize_DAP_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  DAP->Initialize

   overriding function Execute
     (Command : access Initialize_DAP_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Module.Client.Initialize (Kernel => Kernel);
      declare -- initialize dap
         JS : aliased LSP.JSON_Streams.JSON_Stream
           (Is_Server_Side => False, R => null);
         Output : aliased VSS.Text_Streams.Memory_UTF8_Output
           .Memory_UTF8_Output_Stream;
         init_rq : aliased InitializeRequest;
      begin

         Module.Client.Set_Program (node_binary);
         args.Append (debug_Adapter);
         Module.Client.Set_Arguments (args);
         Module.Client.Start;

         Trace
           (Me,
            "Launching the debug adapter: " & node_binary & " " &
            debug_Adapter);

         Trace (Me, "Sending ""initialize"" request");

         init_rq.seq     := Module.Client.Get_Request_ID;
         init_rq.a_type  := "request";
         init_rq.command := "initialize";

         init_rq.arguments.adapterID  := "0";
         init_rq.arguments.clientID   := "0";
         init_rq.arguments.clientName := "GNATSTUDIO";
         init_rq.arguments.locale     := "fr-FR";
         init_rq.arguments.pathFormat := "path";

         init_rq.arguments.columnsStartAt1              := True;
         init_rq.arguments.linesStartAt1                := True;
         init_rq.arguments.supportsInvalidatedEvent     := False;
         init_rq.arguments.supportsMemoryReferences     := False;
         init_rq.arguments.supportsProgressReporting    := True;
         init_rq.arguments.supportsRunInTerminalRequest := False;
         init_rq.arguments.supportsVariablePaging       := False;
         init_rq.arguments.supportsVariableType         := False;

         JS.Set_Stream (Output'Unchecked_Access);
         Write_InitializeRequest (JS'Access, init_rq'Unchecked_Access);
         JS.End_Document;
         Module.Client.Send_Buffer (Output.Buffer);
      end;
      declare -- launch dap
         JS : aliased LSP.JSON_Streams.JSON_Stream
           (Is_Server_Side => False, R => null);
         Output : aliased VSS.Text_Streams.Memory_UTF8_Output
           .Memory_UTF8_Output_Stream;
         launch_rq : aliased LaunchRequest;
      begin
         Trace (Me, "Sending launch request");

         launch_rq.seq                 := Module.Client.Get_Request_ID;
         launch_rq.a_type              := "request";
         launch_rq.command             := "launch";
         launch_rq.arguments.noDebug   := False;
         launch_rq.arguments.a_restart := LSP.Types.Empty;
         launch_rq.arguments.program   :=
           VSS.Strings.Conversions.To_Virtual_String
             (Command.Exec.Display_Full_Name);

         JS.Set_Stream (Output'Unchecked_Access);
         Write_LaunchRequest (JS'Access, launch_rq'Unchecked_Access);
         JS.End_Document;
         Module.Client.Send_Buffer (Output.Buffer);
         Load_Perspective (Kernel, "Debug");
      end;
      Kernel.Refresh_Context;
      return Commands.Success;
   end Execute;

   type On_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called every time the project view changes, to recompute the dynamic
   --  menus.

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

      procedure Create_Action_And_Menu
        (Prj : Project_Type; Main : Virtual_File)
      is
         Main_Name : constant String :=
           (if Main = No_File then "no main file" else Main.Display_Base_Name);
         Escaped_Main_Name : constant String :=
           Escape_Underscore (Escape_Menu_Name (Main_Name));

         Action : constant String :=
           "debug dap initialize " & Prj.Name & ":" & Main_Name;
         Menu : constant String :=
           "DAP/Initialize/" &
           (if not Show_Project_In_Menu or else Main = No_File then ""
            else Escape_Underscore (Prj.Name) & '/') &
           Escaped_Main_Name;
         Command : Interactive_Command_Access;
      begin
         Command :=
           new Initialize_DAP_Command'
             (Interactive_Command with Project => Prj, Exec => Main);
         Module.Actions.Append (Action);

         Register_Action
           (Kernel      => Kernel, Name => Action, Command => Command,
            Description =>
              (if Main /= No_File then
                 ("Initialize the debugger on the file " &
                  Main.Display_Full_Name)
               else "Initialize the debugger, no file specified"),
            Category => "DAP");
         Register_Menu (Kernel => Kernel, Path => Menu, Action => Action);
      end Create_Action_And_Menu;

   begin
      for A of Module.Actions loop
         Unregister_Action (Kernel, A, Remove_Menus_And_Toolbars => True);
      end loop;
      Module.Actions.Clear;

      for J in 1 .. Mains.Length loop
         if Mains.List (J).Length /= 0 then
            declare
               Main : constant Virtual_File :=
                 Debugger.To_File (Kernel, Mains.List (J).Tuple (2).Str);
               Prj : constant Virtual_File :=
                 Debugger.To_File (Kernel, Mains.List (J).Tuple (3).Str);
               P : constant Project_Type :=
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
         DAP_Terminate (Kernel_Handle (Kernel));
   end Execute;

   procedure Send_Breakpoint_Request is
      map : DAP.Breakpoint_Map.Breakpoint_Map;
   begin
      Trace (Me, "Sending SetBreakpointsRequest");

      --  build a multimap for sorting breakpoints by source path
      for B of GVD.Breakpoints_List.Get_Stored_List_Of_Breakpoints.List loop
         map.Add (B);
      end loop;

      for C in map.Iterate loop
         declare
            JS : aliased LSP.JSON_Streams.JSON_Stream
              (Is_Server_Side => False, R => null);
            Output : aliased VSS.Text_Streams.Memory_UTF8_Output
              .Memory_UTF8_Output_Stream;
            setbp_rq : aliased SetBreakpointsRequest;
         begin
            setbp_rq.seq     := Module.Client.Get_Request_ID;
            setbp_rq.a_type  := "request";
            setbp_rq.command := "setBreakpoints";

            setbp_rq.arguments.a_source.name :=
              VSS.Strings.Conversions.To_Virtual_String
                (GNATCOLL.VFS.Display_Base_Name
                   (GPS.Editors.Get_File (map (C).First_Element.Location)));

            setbp_rq.arguments.a_source.path :=
              VSS.Strings.Conversions.To_Virtual_String
                (GNATCOLL.VFS.Display_Full_Name
                   (GPS.Editors.Get_File (map (C).First_Element.Location)));

            setbp_rq.arguments.sourceModified := False;

            for E in map (C).Iterate loop
               declare
                  sb : constant Access_SourceBreakpoint :=
                    new SourceBreakpoint;
               begin
                  sb.line :=
                    LSP_Number (GPS.Editors.Get_Line (map (C) (E).Location));
                  sb.column := 0;
                  setbp_rq.arguments.breakpoints.Append (sb);
               end;
            end loop;

            JS.Set_Stream (Output'Unchecked_Access);
            Write_SetBreakpointsRequest (JS'Access, setbp_rq'Unchecked_Access);
            JS.End_Document;
            Module.Client.Send_Buffer (Output.Buffer);
         end;
      end loop;
   end Send_Breakpoint_Request;

   procedure Update_Breakpoints is
   begin
      if Module.Client.Started then
         if not Module.Client.Running then
            Send_Breakpoint_Request;
         end if;
      end if;
   end Update_Breakpoints;

   overriding procedure Execute
     (Self     : On_Breakpoint_Deleted;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class; Id : Integer)
   is
      pragma Unreferenced (Self, Kernel, Debugger);
   begin
      Update_Breakpoints;
   end Execute;

   overriding procedure Execute
     (Self     : On_Breakpoint_Added;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class; Id : Integer)
   is
      pragma Unreferenced (Self, Kernel, Debugger);
   begin
      Update_Breakpoints;
   end Execute;

   -------------------
   -- DAP_Terminate --
   -------------------

   procedure DAP_Terminate (Kernel : GPS.Kernel.Kernel_Handle) is
   begin
      declare
         JS : aliased LSP.JSON_Streams.JSON_Stream
           (Is_Server_Side => False, R => null);
         Output : aliased VSS.Text_Streams.Memory_UTF8_Output
           .Memory_UTF8_Output_Stream;
         disco_rq : aliased DisconnectRequest;
      begin
         Trace (Me, "Send disconnect request and stop client");

         disco_rq.seq     := Module.Client.Get_Request_ID;
         disco_rq.a_type  := "request";
         disco_rq.command := "disconnect";

         disco_rq.arguments.restart           := False;
         disco_rq.arguments.terminateDebuggee := True;

         JS.Set_Stream (Output'Unchecked_Access);
         Write_DisconnectRequest (JS'Access, disco_rq'Unchecked_Access);
         JS.End_Document;
         Module.Client.Send_Buffer (Output.Buffer);
      end;
      Load_Perspective (Kernel, "Default");
      GVD.Code_Editors.Unhighlight_Current_Line (Kernel => Kernel);
   end DAP_Terminate;

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      DAP_Started : Action_Filter;
   begin
      Module := new Module_Id_Record;
      Module.Client.Initialize (Kernel => Kernel);
      Register_Module (Module_ID (Module), Kernel, "DAP_Client");
      Trace (Me, "Register " & DAP_Module_Name & " Module");

      --  Register Filters
      DAP_Started := new DAP_Started_Filter;
      Register_Filter (Kernel, DAP_Started, "DAP started");

      --  Add hooks
      Project_View_Changed_Hook.Add (new On_View_Changed);
      Debugger_Breakpoint_Added_Hook.Add (new On_Breakpoint_Added);
      Debugger_Breakpoint_Deleted_Hook.Add (new On_Breakpoint_Deleted);

      --  Add actions
      Register_Action
        (Kernel   => Kernel, Name => "debug dap action",
         Command  => new Test_Command, Description => "DAP Action",
         Category => "DAP", Icon_Name => "gps-debugger-run-symbolic");

      Register_Action
        (Kernel      => Kernel, Name => "debug dap conf done or continue",
         Command     => new Config_Done_Or_Continue_Command,
         Icon_Name   => "gps-debugger-run-symbolic", Filter => DAP_Started,
         Description =>
           "Continue execution until next breakpoint." & ASCII.LF &
           "Start the debugger if not started yet",
         Category => "DAP");

      Register_Action
        (Kernel      => Kernel, Name => "debug dap next",
         Command     => new Next_Command,
         Icon_Name   => "gps-debugger-next-symbolic", Filter => DAP_Started,
         Description =>
           "Execute the program until the next source line, stepping over" &
           " subprogram calls",
         Category => "DAP");

      Register_Action
        (Kernel      => Kernel, Name => "debug dap step",
         Command     => new Step_In_Command,
         Icon_Name   => "gps-debugger-step-symbolic", Filter => DAP_Started,
         Description => "Execute the program for one machine instruction only",
         Category    => "DAP");

      Register_Action
        (Kernel      => Kernel, Name => "debug dap finish",
         Command     => new Step_Out_Command,
         Icon_Name   => "gps-debugger-finish-symbolic", Filter => DAP_Started,
         Description =>
           "Continue execution until selected stack frame returns",
         Category => "DAP");

      Register_Action
        (Kernel      => Kernel, Name => "dap terminate debugger",
         Command     => new Terminate_Command,
         Description => "Terminate the current debugger",
         Icon_Name   => "gps-debugger-terminate-symbolic",
         Filter      => DAP_Started);

   end Register_Module;

end LSP.DAP_Module;
