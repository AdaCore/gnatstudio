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

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.Strings;

with GNATCOLL.Any_Types;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Utils;
with Spawn.String_Vectors;

with Glib.Convert;
with Glib.Object;                use Glib.Object;

with Gtkada.Dialogs;
with Gtkada.MDI;                 use Gtkada.MDI;

with VSS.Characters;             use VSS.Characters;
with VSS.Characters.Latin;
with VSS.JSON.Pull_Readers.Simple;
with VSS.JSON.Push_Writers;
with VSS.Regular_Expressions;
with VSS.Stream_Element_Vectors.Conversions;
with VSS.Strings;                use VSS.Strings;
with VSS.Strings.Conversions;
with VSS.Strings.Cursors.Iterators.Characters;
with VSS.Text_Streams.Memory_UTF8_Input;
with VSS.Text_Streams.Memory_UTF8_Output;

with GPS.Editors;                use GPS.Editors;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Hooks;
with GPS.Kernel.Project;

with LSP.Types;
with LSP.JSON_Streams;

with DAP.Module;
with DAP.Modules.Preferences;
with DAP.Requests.Evaluate;
with DAP.Requests.Initialize;
with DAP.Requests.Disconnects;
with DAP.Requests.StackTraces;
with DAP.Views.Consoles;
with DAP.Views.Memory;
with DAP.Tools.Inputs;
with DAP.Utils;

with Interactive_Consoles;       use Interactive_Consoles;
with GUI_Utils;
with Language_Handlers;          use Language_Handlers;
with String_Utils;
with Toolchains;                 use Toolchains;
with Remote;

package body DAP.Clients is

   Me      : constant Trace_Handle := Create ("GPS.DAP.Clients", On);
   DAP_Log : constant GNATCOLL.Traces.Trace_Handle :=
     Create ("GPS.DAP.IN_OUT", Off);

   procedure Free is new Ada.Unchecked_Deallocation
     (DAP.Modules.Breakpoint_Managers.DAP_Client_Breakpoint_Manager'Class,
      DAP.Modules.Breakpoint_Managers.DAP_Client_Breakpoint_Manager_Access);

   -- StackTrace_Request --

   type StackTrace_Request is
     new DAP.Requests.StackTraces.StackTrace_DAP_Request
   with record
      Client : DAP_Client_Access;
   end record;

   type StackTrace_Request_Access is access all StackTrace_Request;

   overriding procedure On_Result_Message
     (Self        : in out StackTrace_Request;
      Result      : in out DAP.Tools.StackTraceResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);

   overriding procedure On_Error_Message
     (Self    : in out StackTrace_Request;
      Message : VSS.Strings.Virtual_String);

   overriding procedure On_Rejected (Self : in out StackTrace_Request);

   -- Evaluate_Request --

   type Evaluate_Request is
     new DAP.Requests.Evaluate.Evaluate_DAP_Request
   with record
      Kind   : Evaluate_Kind := Hover;
      Client : DAP_Client_Access;
      Label  : Gtk.Label.Gtk_Label;
      Output : Boolean := False;

      -- Callbacks --
      On_Result_Message : GNATCOLL.Scripts.Subprogram_Type := null;
      On_Error_Message  : GNATCOLL.Scripts.Subprogram_Type := null;
      On_Rejected       : GNATCOLL.Scripts.Subprogram_Type := null;
   end record;
   type Evaluate_Request_Access is access all Evaluate_Request;
   overriding procedure Finalize (Self : in out Evaluate_Request);
   overriding procedure On_Result_Message
     (Self        : in out Evaluate_Request;
      Result      : in out DAP.Tools.EvaluateResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);
   overriding procedure On_Rejected (Self : in out Evaluate_Request);
   overriding procedure On_Error_Message
     (Self    : in out Evaluate_Request;
      Message : VSS.Strings.Virtual_String);

   GNAT_Binder_File_Pattern  : constant VSS.Regular_Expressions.
     Regular_Expression := VSS.Regular_Expressions.To_Regular_Expression
       ("b[~_]_?([^.]+\.(?:adb|c))");

   File_Name_Pattern         : constant VSS.Regular_Expressions.
     Regular_Expression := VSS.Regular_Expressions.To_Regular_Expression
       ("^Line ([0-9]+) of (?:""([^""]+)"")? (?:starts|is)"
        & " at address (0x[0-9a-f]+)");
   --  Matches a file name/line indication in gdb's output

   Language_Pattern          : constant VSS.Regular_Expressions.
     Regular_Expression := VSS.Regular_Expressions.To_Regular_Expression
       ("^(?:The current source language is|Current language:) +"
        & """?(?:auto; currently )?([^""\t ]+)(?:""\.)?");
   --  Pattern used to detect language changes in the debugger

   Endian_Pattern            : constant VSS.Regular_Expressions.
     Regular_Expression := VSS.Regular_Expressions.To_Regular_Expression
       ("little endian");
   --  Pattern used to detect endian

   Is_Quit_Pattern : constant VSS.Regular_Expressions.
     Regular_Expression := VSS.Regular_Expressions.To_Regular_Expression
       ("^\s*(quit|qui|q|-gdb-exit)\s*$");
   --  'qu' can be quit or queue-signal

   Is_Frame_Up_Pattern : constant VSS.Regular_Expressions.
     Regular_Expression := VSS.Regular_Expressions.To_Regular_Expression
       ("^\s*(up)\s*$");

   Is_Frame_Down_Pattern : constant VSS.Regular_Expressions.
     Regular_Expression := VSS.Regular_Expressions.To_Regular_Expression
       ("^\s*(down)\s*$");

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error
     (Self  : in out DAP_Client;
      Error : String) is
   begin
      Trace (Me, "On_Error:" & Error);
      Self.Reject_All_Requests;
   end On_Error;

   -------------------------------
   -- On_Standard_Error_Message --
   -------------------------------

   overriding procedure On_Standard_Error_Message
     (Self : in out DAP_Client;
      Text : String)
   is
      pragma Unreferenced (Self);
   begin
      Trace (Me, "On_Standard_Error_Message:" & Text);
   end On_Standard_Error_Message;

   ------------------
   -- On_Exception --
   ------------------

   overriding procedure On_Exception
     (Self       : in out DAP_Client;
      Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (Self);
   begin
      Trace (Me, Occurrence);
   end On_Exception;

   ---------------------
   -- Break_Exception --
   ---------------------

   procedure Break_Exception
     (Self      : in out DAP_Client;
      Name      : String;
      Unhandled : Boolean := False;
      Temporary : Boolean := False)
   is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Break_Exception (Self.Breakpoints, Name, Unhandled, Temporary);
      end if;
   end Break_Exception;

   -----------
   -- Break --
   -----------

   procedure Break
     (Self : in out DAP_Client;
      Data : DAP.Modules.Breakpoints.Breakpoint_Data)
   is
      use DAP.Modules.Breakpoint_Managers;
      D : DAP.Modules.Breakpoints.Breakpoint_Data := Data;
   begin
      if Self.Breakpoints /= null then
         D.Executable := Self.Get_Executable;
         Break (Self.Breakpoints, D);
      end if;
   end Break;

   ------------------
   -- Break_Source --
   ------------------

   procedure Break_Source
     (Self      : in out DAP_Client;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False)
   is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Break_Source (Self.Breakpoints, File, Line, Temporary);
      end if;
   end Break_Source;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Self       : in out DAP_Client;
      Subprogram : String;
      Temporary  : Boolean := False)
   is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Break_Subprogram (Self.Breakpoints, Subprogram, Temporary);
      end if;
   end Break_Subprogram;

   -----------------------------------
   -- Toggle_Instruction_Breakpoint --
   -----------------------------------

   procedure Toggle_Instruction_Breakpoint
     (Self    : in out DAP_Client;
      Address : Address_Type)
   is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Toggle_Instruction_Breakpoint (Self.Breakpoints, Address);
      end if;
   end Toggle_Instruction_Breakpoint;

   ------------------
   -- Current_File --
   ------------------

   function Current_File
     (Self : in out DAP_Client) return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.Selected_File;
   end Current_File;

   ------------------
   -- Current_Line --
   ------------------

   function Current_Line
     (Self : in out DAP_Client) return Integer is
   begin
      return Self.Selected_Line;
   end Current_Line;

   ---------------------
   -- Current_Address --
   ---------------------

   function Current_Address
     (Self : in out DAP_Client) return Address_Type is
   begin
      return Self.Selected_Address;
   end Current_Address;

   --------------------------
   -- Remove_Breakpoint_At --
   --------------------------

   procedure Remove_Breakpoint_At
     (Self      : in out DAP_Client;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type)
   is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Remove_Breakpoint_At (Self.Breakpoints, File, Line);
      end if;
   end Remove_Breakpoint_At;

   ------------------------
   -- Remove_Breakpoints --
   ------------------------

   procedure Remove_Breakpoints
     (Self : in out DAP_Client;
      List : DAP.Types.Breakpoint_Identifier_Lists.List)
   is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Remove_Breakpoints (Self.Breakpoints, List);
      end if;
   end Remove_Breakpoints;

   ----------------------------
   -- Remove_All_Breakpoints --
   ----------------------------

   procedure Remove_All_Breakpoints (Self : in out DAP_Client) is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Remove_All_Breakpoints (Self.Breakpoints);
      end if;
   end Remove_All_Breakpoints;

   --------------------
   -- Has_Breakpoint --
   --------------------

   function Has_Breakpoint
     (Self      : DAP_Client;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type)
      return Boolean is
   begin
      return False;
   end Has_Breakpoint;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access DAP_Client) is
   begin
      Self.Visual := new DAP_Visual_Debugger'
        (Glib.Object.GObject_Record with Client => Self.This);
      Glib.Object.Initialize (Self.Visual);
      Ref (Self.Visual);
   end Initialize;

   ----------------
   -- Is_Stopped --
   ----------------

   function Is_Stopped (Self : DAP_Client) return Boolean is
   begin
      return Self.Status = Stopped;
   end Is_Stopped;

   --------------
   -- Is_Ready --
   --------------

   function Is_Ready (Self : DAP_Client) return Boolean is
   begin
      return Self.Status = Ready;
   end Is_Ready;

   --------------------------
   -- Is_Ready_For_Command --
   --------------------------

   function Is_Ready_For_Command (Self : DAP_Client) return Boolean is
   begin
      return (Self.Status = Stopped or else Self.Status = Ready)
        and then Self.Sent.Is_Empty;
   end Is_Ready_For_Command;

   ---------------------
   -- Is_Quit_Command --
   ---------------------

   function Is_Quit_Command
     (Self : DAP_Client;
      Cmd  : String)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return Is_Quit_Pattern.Match
        (VSS.Strings.Conversions.To_Virtual_String (Cmd)).Has_Match;
   end Is_Quit_Command;

   -------------------------
   -- Is_Frame_Up_Command --
   -------------------------

   function Is_Frame_Up_Command
     (Self : DAP_Client;
      Cmd  : String)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return Is_Frame_Up_Pattern.Match
        (VSS.Strings.Conversions.To_Virtual_String (Cmd)).Has_Match;
   end Is_Frame_Up_Command;

   ---------------------------
   -- Is_Frame_Down_Command --
   ---------------------------

   function Is_Frame_Down_Command
     (Self : DAP_Client;
      Cmd  : String)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return Is_Frame_Down_Pattern.Match
        (VSS.Strings.Conversions.To_Virtual_String (Cmd)).Has_Match;
   end Is_Frame_Down_Command;

   ----------------------
   -- Set_Capabilities --
   ----------------------

   procedure Set_Capabilities
     (Self         : in out DAP_Client;
      Capabilities : DAP.Tools.Optional_Capabilities) is
   begin
      Self.Capabilities := Capabilities;
   end Set_Capabilities;

   ----------------------
   -- Get_Capabilities --
   ----------------------

   function Get_Capabilities
     (Self : in out DAP_Client)
      return DAP.Tools.Optional_Capabilities is
   begin
      return Self.Capabilities;
   end Get_Capabilities;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status
     (Self   : in out DAP_Client;
      Status : Debugger_Status_Kind)
   is
      use GNATCOLL.VFS;
      Old : constant Debugger_Status_Kind := Self.Status;
   begin
      if Self.Status = Status
        or else Self.Status = Terminating
      then
         return;
      end if;

      Self.Status := Status;

      if Self.Status not in Ready .. Stopped then
         Self.Selected_File    := No_File;
         Self.Selected_Line    := 0;
         Self.Selected_Address := Invalid_Address;
         Self.Selected_Frame   := 0;
         Self.Selected_Thread  := 0;
         Self.Frames.Clear;

         DAP.Utils.Unhighlight_Current_Line (Self.Kernel);
      end if;

      case Self.Status is
         when Ready =>
            --  Trigger this hook when we did all preparations
            --   (for example set breakpoints). In ither case we will
            --   have the mess with debugging
            GPS.Kernel.Hooks.Debugger_Started_Hook.Run
              (Self.Kernel, Self.Visual);

         when Stopped =>
            GPS.Kernel.Hooks.Debugger_Process_Stopped_Hook.Run
              (Self.Kernel, Self.Visual);

         when Terminating =>
            if Debugger_Status_Kind'Pos (Old) >=
              Debugger_Status_Kind'Pos (Initialized)
            then
               GPS.Kernel.Hooks.Debugger_Terminated_Hook.Run
                 (Self.Kernel, Self.Get_Visual);
            end if;

         when others =>
            null;
      end case;

      GPS.Kernel.Hooks.Debugger_State_Changed_Hook.Run
        (Self.Kernel, Self.Visual,
         (if Self.Status = Terminating
          then GPS.Debuggers.Debug_None
          elsif Self.Status in Ready .. Stopped
          then GPS.Debuggers.Debug_Available
          else GPS.Debuggers.Debug_Busy));

      Self.Kernel.Refresh_Context;
   end Set_Status;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Self    : in out DAP_Client;
      Request : in out DAP.Requests.DAP_Request_Access) is
   begin
      if Self.Status in Initialization .. Stopped then
         Self.Process (Request);

      else
         Request.On_Rejected;
         DAP.Requests.Destroy (Request);
      end if;

      Request := null;
   end Enqueue;

   -----------------------
   -- Get_Assembly_View --
   -----------------------

   function Get_Assembly_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access is
   begin
      return Self.Assembly_View;
   end Get_Assembly_View;

   ---------------------
   -- Get_Memory_View --
   ---------------------

   function Get_Memory_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access is
   begin
      return Self.Memory_View;
   end Get_Memory_View;

   ------------------------
   -- Get_Variables_View --
   ------------------------

   function Get_Variables_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access is
   begin
      return Self.Variables_View;
   end Get_Variables_View;

   ---------------------
   -- Get_Breakpoints --
   ---------------------

   function Get_Breakpoints
     (Self : DAP_Client)
      return DAP.Modules.Breakpoints.Breakpoint_Vectors.Vector
   is
      use type DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager_Access;

      Empty : DAP.Modules.Breakpoints.Breakpoint_Vectors.Vector;
   begin
      if Self.Breakpoints /= null then
         return Self.Breakpoints.Get_Breakpoints;
      else
         return Empty;
      end if;
   end Get_Breakpoints;

   -------------------------
   -- Get_Call_Stack_View --
   -------------------------

   function Get_Call_Stack_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access is
   begin
      return Self.Call_Stack_View;
   end Get_Call_Stack_View;

   -------------------------
   -- Get_Command_History --
   -------------------------

   function Get_Command_History
     (Self : in out DAP_Client)
      return History_List_Access is
   begin
      return Self.Command_History'Unchecked_Access;
   end Get_Command_History;

   --------------------------
   -- Get_Debugger_Console --
   --------------------------

   function Get_Debugger_Console
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access is
   begin
      return Self.Debugger_Console;
   end Get_Debugger_Console;

   --------------------------
   -- Set_Debugger_Console --
   --------------------------

   procedure Set_Debugger_Console
     (Self    : in out DAP_Client;
      Console : Generic_Views.Abstract_View_Access) is
   begin
      Self.Debugger_Console := Console;
   end Set_Debugger_Console;

   ------------------------
   -- Get_Current_Thread --
   ------------------------

   function Get_Current_Thread (Self  : in out DAP_Client) return Integer
   is
      use type Generic_Views.Abstract_View_Access;
   begin
      if Self.Stopped_Threads.Is_Empty then
         return 0;
      end if;

      if Self.Thread_View /= null
        and then Self.Selected_Thread /= 0
      then
         return Self.Selected_Thread;
      else
         return Self.Stopped_Threads.Element (Self.Stopped_Threads.First);
      end if;
   end Get_Current_Thread;

   ---------------------
   -- Get_Thread_View --
   ---------------------

   function Get_Thread_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access is
   begin
      return Self.Thread_View;
   end Get_Thread_View;

   --------------------
   -- Get_Executable --
   --------------------

   function Get_Executable
     (Self : in out DAP_Client) return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.Executable;
   end Get_Executable;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project
     (Self : in out DAP_Client) return GNATCOLL.Projects.Project_Type is
   begin
      return Self.Project;
   end Get_Project;

   -------------------------
   -- Get_Executable_Args --
   -------------------------

   function Get_Executable_Args
     (Self : in out DAP_Client) return Ada.Strings.Unbounded.Unbounded_String
   is
   begin
      return Self.Args;
   end Get_Executable_Args;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
     (Self : in out DAP_Client) return Language_Access is
   begin
      --  To-Do: Check if language is detected
      return Get_Language_By_Name
        (GPS.Kernel.Get_Language_Handler (Self.Kernel),
         VSS.Strings.Conversions.To_UTF_8_String (Self.Stored_Lang));
   end Get_Language;

   ---------------------
   -- Get_Endian_Type --
   ---------------------

   function Get_Endian_Type (Self : in out DAP_Client) return Endian_Type is
   begin
      return Self.Endian;
   end Get_Endian_Type;

   -----------------------
   -- Set_Assembly_View --
   -----------------------

   procedure Set_Assembly_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access) is
   begin
      Self.Assembly_View := View;
   end Set_Assembly_View;

   --------------------
   -- Set_Executable --
   --------------------

   procedure Set_Executable
     (Self : in out DAP_Client;
      File : GNATCOLL.VFS.Virtual_File) is
   begin
      Self.Executable := File;
   end Set_Executable;

   ---------------------
   -- Set_Memory_View --
   ---------------------

   procedure Set_Memory_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access) is
   begin
      Self.Memory_View := View;
   end Set_Memory_View;

   ------------------------
   -- Set_Variables_View --
   ------------------------

   procedure Set_Variables_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access) is
   begin
      Self.Variables_View := View;
   end Set_Variables_View;

   ---------------------------
   -- Set_Breakpoints_State --
   ---------------------------

   procedure Set_Breakpoints_State
     (Self  : in out DAP_Client;
      List  : Breakpoint_Identifier_Lists.List;
      State : Boolean)
   is
      use type DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager_Access;
   begin
      if Self.Breakpoints /= null then
         DAP.Modules.Breakpoint_Managers.Set_Breakpoints_State
           (Self.Breakpoints, List, State);
      end if;
   end Set_Breakpoints_State;

   -------------------------
   -- Set_Call_Stack_View --
   -------------------------

   procedure Set_Call_Stack_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access) is
   begin
      Self.Call_Stack_View := View;
   end Set_Call_Stack_View;

   ---------------------
   -- Set_Thread_View --
   ---------------------

   procedure Set_Thread_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access) is
   begin
      Self.Thread_View := View;
   end Set_Thread_View;

   ------------------------
   -- Set_Selected_Frame --
   ------------------------

   procedure Set_Selected_Frame
     (Self                      : in out DAP_Client;
      Id                        : Integer;
      File                      : GNATCOLL.VFS.Virtual_File;
      Line                      : Integer;
      Address                   : Address_Type;
      Run_Location_Changed_Hook : Boolean := True)
   is
      use GNATCOLL.VFS;
   begin
      Self.Selected_Frame   := Id;
      Self.Selected_File    := File;
      Self.Selected_Line    := Line;
      Self.Selected_Address := Address;

      --  highlight selected location
      if Self.Selected_File /= No_File then
         DAP.Utils.Highlight_Current_File_And_Line
           (Self.Kernel, Self.Selected_File, Self.Selected_Line);
      end if;

      if Run_Location_Changed_Hook then
         --  Triggering the hook to inform views. Not needed when we set
         --  location for the first time after the debugging stop.
         GPS.Kernel.Hooks.Debugger_Location_Changed_Hook.Run
           (Self.Kernel, Self.Visual);
      end if;
   end Set_Selected_Frame;

   ------------------------
   -- Get_Selected_Frame --
   ------------------------

   function Get_Selected_Frame
     (Self : DAP_Client) return Integer is
   begin
      return Self.Selected_Frame;
   end Get_Selected_Frame;

   -------------------------
   -- Set_Selected_Thread --
   -------------------------

   procedure Set_Selected_Thread (Self : in out DAP_Client; Id : Integer) is
   begin
      Self.Selected_Thread := Id;
      GPS.Kernel.Hooks.Debugger_Frame_Changed_Hook.Run
        (Self.Kernel, Self.Visual);
   end Set_Selected_Thread;

   ----------------------
   -- Set_Source_Files --
   ----------------------

   procedure Set_Source_Files
     (Self         : in out DAP_Client;
      Source_Files : VSS.String_Vectors.Virtual_String_Vector) is
   begin
      Self.Source_Files := Source_Files;
   end Set_Source_Files;

   --------------------
   -- Get_Request_ID --
   --------------------

   function Get_Request_ID
     (Self : in out DAP_Client) return Integer
   is
      ID : constant Integer := Self.Request_Id;
   begin
      if Self.Request_Id < Integer'Last then
         Self.Request_Id := Self.Request_Id + 1;
      else
         Self.Request_Id := 1;
      end if;

      return ID;
   end Get_Request_ID;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Self : in out DAP_Client) return Debugger_Status_Kind is
   begin
      return Self.Status;
   end Get_Status;

   ----------------
   -- Get_Visual --
   ----------------

   function Get_Visual
     (Self : in out DAP_Client)
      return DAP_Visual_Debugger_Access is
   begin
      return Self.Visual;
   end Get_Visual;

   ------------------
   -- Current_File --
   ------------------

   function Current_File
     (Visual : not null access DAP_Visual_Debugger)
      return GNATCOLL.VFS.Virtual_File is
   begin
      return Visual.Client.Current_File;
   end Current_File;

   ------------------
   -- Current_Line --
   ------------------

   function Current_Line
     (Visual : not null access DAP_Visual_Debugger)
      return Natural is
   begin
      return Visual.Client.Current_Line;
   end Current_Line;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : DAP_Client) return VSS.Strings.Virtual_String is
   begin
      return Self.Error_Msg;
   end Error_Message;

   -------------------
   -- On_Configured --
   -------------------

   procedure On_Configured (Self : in out DAP_Client) is
   begin
      Self.Set_Status (Running);
   end On_Configured;

   ------------------------
   -- On_Breakpoints_Set --
   ------------------------

   procedure On_Breakpoints_Set (Self : in out DAP_Client) is
      Req   : Evaluate_Request_Access := new Evaluate_Request (Self.Kernel);
      Frame : constant Integer := Self.Get_Selected_Frame;
   begin
      Req.Kind   := Show_Lang;
      Req.Client := Self.This;
      Req.Parameters.arguments.expression :=
        VSS.Strings.Conversions.To_Virtual_String ("show lang");
      if Frame /= 0 then
         Req.Parameters.arguments.frameId := (Is_Set => True, Value => Frame);
      end if;
      Req.Parameters.arguments.context :=
        (Is_Set => True, Value => DAP.Tools.Enum.repl);
      Self.Enqueue (DAP.Requests.DAP_Request_Access (Req));
   end On_Breakpoints_Set;

   -----------------
   -- On_Continue --
   -----------------

   procedure On_Continue (Self : in out DAP_Client) is
   begin
      Self.Set_Status (Running);
   end On_Continue;

   -----------------
   -- On_Finished --
   -----------------

   overriding procedure On_Finished (Self : in out DAP_Client)
   is
      use type DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager_Access;

   begin
      if Self.Visual = null then
         return;
      end if;

      if Self.Breakpoints /= null then
         Self.Breakpoints.Finalize;
         Free (Self.Breakpoints);
      end if;

      Unref (Self.Visual);
      Self.Visual := null;

      DAP.Module.Finished (Self.Id);
   exception
      when E : others =>
         Trace (Me, E);
   end On_Finished;

   ----------------------------------
   -- Load_Project_From_Executable --
   ----------------------------------

   procedure Load_Project_From_Executable (Self : in out DAP_Client) is
      use GNAT.Strings;
      use GNATCOLL.Projects;
      use GNATCOLL.VFS;
      use Gtkada.Dialogs;
      use GPS.Kernel.Project;

      Project : GNATCOLL.Projects.Project_Type := Get_Project (Self.Kernel);
   begin
      --  Do nothing unless the current project was already generated from an
      --  executable.

      if Get_Registry (Self.Kernel).Tree.Status /= From_Executable
      then
         return;
      end if;

      if Self.Executable /= No_File
        and then not Is_Regular_File (Self.Get_Executable)
      then
         declare
            Buttons : Message_Dialog_Buttons;
            pragma Unreferenced (Buttons);
         begin
            Buttons := GUI_Utils.GPS_Message_Dialog
              (Msg         =>
                 "The executable specified with" &
                   " --debug does not exist on disk",
               Dialog_Type => Error,
               Buttons     => Button_OK,
               Title       => "Executable not found",
               Parent      => GPS.Kernel.Get_Main_Window (Self.Kernel));
         end;
      end if;

      declare
         List : GNAT.Strings.String_List_Access :=
           Project.Attribute_Value (Main_Attribute);
      begin
         if List /= null then
            for L in List'Range loop
               if Equal (+List (L).all, Full_Name (Self.Executable)) then
                  Free (List);
                  return;
               end if;
            end loop;
            Free (List);
         end if;
      end;

      --  No handling of desktop is done here, we want to leave all windows
      --  as-is.

      Get_Registry (Self.Kernel).Tree.Unload;

      --  Create an empty project, and we'll add properties to it

      if Self.Executable /= No_File then
         Get_Registry (Self.Kernel).Tree.Load_Empty_Project
           (Get_Registry (Self.Kernel).Environment,
            Name           => "debugger_" & (+Base_Name (Self.Get_Executable)),
            Recompute_View => False);
      else
         Get_Registry (Self.Kernel).Tree.Load_Empty_Project
           (Get_Registry (Self.Kernel).Environment,
            Name           => "debugger_no_file",
            Recompute_View => False);
      end if;

      Project := Get_Registry (Self.Kernel).Tree.Root_Project;

      declare
         Bases       : GNAT.OS_Lib.Argument_List
           (1 .. Self.Source_Files.Length);
         Bases_Index : Natural := Bases'First;
         Dirs        : GNAT.OS_Lib.Argument_List
           (1 .. Self.Source_Files.Length);
         Dirs_Index  : Natural := Dirs'First;
         Main        : GNAT.OS_Lib.Argument_List (1 .. 1);
         Langs       : GNAT.OS_Lib.Argument_List
           (1 .. Self.Source_Files.Length);
         Lang_Index  : Natural := Langs'First;

      begin
         --  Source_Files, Source_Dirs & Languages

         for L in 1 .. Self.Source_Files.Length loop
            declare
               Remote_File : constant Virtual_File :=
                 Create_From_Base
                   (+VSS.Strings.Conversions.To_UTF_8_String
                      (Self.Source_Files.Element (L)),
                    Dir_Name (Self.Get_Executable),
                    Remote.Get_Nickname (Remote.Debug_Server));
               Local_File  : constant Virtual_File := To_Local (Remote_File);
               Dir         : constant Virtual_File := Local_File.Dir;
               Base        : constant Filesystem_String :=
                 Base_Name (Local_File);
               Lang        : constant String :=
                 Get_Language_From_File
                   (GPS.Kernel.Get_Language_Handler (Self.Kernel),
                    Local_File);
               Found       : Boolean;

            begin
               Found := False;

               if Is_Directory (Dir) then
                  for D in Dirs'First .. Dirs_Index - 1 loop
                     if Equal (+Dirs (D).all, Dir.Full_Name) then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Dirs (Dirs_Index) := new String'(+Dir.Full_Name);
                     Dirs_Index := Dirs_Index + 1;
                  end if;

                  Found := False;
                  for J in Bases'First .. Bases_Index - 1 loop
                     if Equal (+Bases (J).all, Base) then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Bases (Bases_Index) := new String'
                       (Base_Name
                          (VSS.Strings.Conversions.To_UTF_8_String
                               (Self.Source_Files.Element (L))));
                     Bases_Index := Bases_Index + 1;
                  end if;

                  Found := False;
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
               end if;
            end;
         end loop;

         GNATCOLL.Traces.Trace (Me, "Setting Source_Dirs:");
         for D in Dirs'First .. Dirs_Index - 1 loop
            GNATCOLL.Traces.Trace (Me, "   " & Dirs (D).all);
         end loop;

         Project.Set_Attribute
           (Attribute          => Source_Dirs_Attribute,
            Values             => Dirs (Dirs'First .. Dirs_Index - 1));
         GNATCOLL.Utils.Free (Dirs);

         GNATCOLL.Traces.Trace (Me, "Setting Source_Files:");
         for B in Bases'First .. Bases_Index - 1 loop
            GNATCOLL.Traces.Trace (Me, "   " & Bases (B).all);
         end loop;

         Project.Set_Attribute
           (Attribute          => Source_Files_Attribute,
            Values             => Bases (Bases'First .. Bases_Index - 1));
         GNATCOLL.Utils.Free (Bases);

         GNATCOLL.Traces.Trace (Me, "Setting Languages:");
         for L in Langs'First .. Lang_Index - 1 loop
            GNATCOLL.Traces.Trace (Me, "   " & Langs (L).all);
         end loop;

         if Lang_Index = Langs'First then
            Project.Set_Attribute
              (Scenario  => All_Scenarios,
               Attribute => Languages_Attribute,
               Values    =>
                 (new String'("ada"), new String'("c"), new String'("c++")));
         else
            Project.Set_Attribute
              (Attribute          => Languages_Attribute,
               Values             => Langs (Langs'First .. Lang_Index - 1));
         end if;

         GNATCOLL.Utils.Free (Langs);

         --  Object_Dir, Exec_Dir, Main

         if Self.Executable /= No_File then
            Project.Set_Attribute
              (Attribute          => Obj_Dir_Attribute,
               Value              => +Dir_Name (Self.Get_Executable));
            Project.Set_Attribute
              (Attribute          => Exec_Dir_Attribute,
               Value              => +Dir_Name (Self.Get_Executable));

            Main (Main'First) := new String'(+Full_Name (Self.Executable));
            Project.Set_Attribute
              (Attribute          => Main_Attribute,
               Values             => Main);
            GNATCOLL.Utils.Free (Main);
         end if;
      end;

      --  Is the information for this executable already cached? If yes,
      --  we simply reuse it to avoid the need to interact with the debugger.

      Project.Set_Modified (False);
      Get_Registry (Self.Kernel).Tree.Set_Status (From_Executable);
      GPS.Kernel.Hooks.Project_Changed_Hook.Run (Self.Kernel);
      Recompute_View (Self.Kernel);
   end Load_Project_From_Executable;

   -----------------
   -- On_Launched --
   -----------------

   procedure On_Launched (Self : in out DAP_Client) is
   begin
      if not Self.Source_Files.Is_Empty then
         Self.Load_Project_From_Executable;
      end if;

      --  Show Main file when 'info line' command is in the protocol

      Self.Breakpoints := new DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager (Self.Kernel, Self.This);
      DAP.Modules.Breakpoint_Managers.Initialize (Self.Breakpoints);
   end On_Launched;

   --------------------
   -- On_Raw_Message --
   --------------------

   overriding procedure On_Raw_Message
     (Self    : in out DAP_Client;
      Data    : Ada.Strings.Unbounded.Unbounded_String;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);
      use type DAP.Requests.DAP_Request_Access;

      procedure Look_Ahead
        (Seq         : out LSP.Types.LSP_Number;
         Request_Seq : out Integer;
         A_Type      : out VSS.Strings.Virtual_String;
         Success     : out LSP.Types.Optional_Boolean;
         Message     : in out VSS.Strings.Virtual_String;
         Event       : in out VSS.Strings.Virtual_String);

      Memory : aliased
        VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;

      ----------------
      -- Look_Ahead --
      ----------------

      procedure Look_Ahead
        (Seq         : out LSP.Types.LSP_Number;
         Request_Seq : out Integer;
         A_Type      : out VSS.Strings.Virtual_String;
         Success     : out LSP.Types.Optional_Boolean;
         Message     : in out VSS.Strings.Virtual_String;
         Event       : in out VSS.Strings.Virtual_String)
      is

         Reader : aliased
           VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
         JS     : aliased LSP.JSON_Streams.JSON_Stream
           (False, Reader'Access);

      begin
         Seq         := 0;
         Request_Seq := 0;
         A_Type      := VSS.Strings.Empty_Virtual_String;
         Success     := (Is_Set => False);

         Reader.Set_Stream (Memory'Unchecked_Access);
         JS.R.Read_Next;
         pragma Assert (JS.R.Is_Start_Document);
         JS.R.Read_Next;
         pragma Assert (JS.R.Is_Start_Object);
         JS.R.Read_Next;

         while not JS.R.Is_End_Object loop
            pragma Assert (JS.R.Is_Key_Name);
            declare
               Key : constant String :=
                 VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);

            begin
               JS.R.Read_Next;

               if Key = "seq" then
                  pragma Assert (JS.R.Is_Number_Value);
                  Seq := LSP.Types.LSP_Number
                    (JS.R.Number_Value.Integer_Value);

                  JS.R.Read_Next;

               elsif Key = "request_seq" then
                  pragma Assert (JS.R.Is_Number_Value);
                  Request_Seq := Integer (JS.R.Number_Value.Integer_Value);

                  JS.R.Read_Next;

               elsif Key = "type" then
                  pragma Assert (JS.R.Is_String_Value);

                  A_Type := JS.R.String_Value;

                  JS.R.Read_Next;

               elsif Key = "success" then
                  pragma Assert (JS.R.Is_Boolean_Value);

                  Success := (True, JS.R.Boolean_Value);

                  JS.R.Read_Next;

               elsif Key = "message" then
                  pragma Assert (JS.R.Is_String_Value);

                  Message := JS.R.String_Value;

                  JS.R.Read_Next;

               elsif Key = "event" then
                  pragma Assert (JS.R.Is_String_Value);

                  Event := JS.R.String_Value;

                  JS.R.Read_Next;

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;

         Memory.Rewind;
      end Look_Ahead;

      Reader : aliased VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
      Stream : aliased LSP.JSON_Streams.JSON_Stream
        (Is_Server_Side => False, R => Reader'Access);

      Seq         : LSP.Types.LSP_Number;
      A_Type      : VSS.Strings.Virtual_String;
      Request_Seq : Integer;
      R_Success   : LSP.Types.Optional_Boolean;
      Message     : VSS.Strings.Virtual_String;
      Event       : VSS.Strings.Virtual_String;

      Position    : Requests_Maps.Cursor;
      Request     : DAP.Requests.DAP_Request_Access;
      New_Request : DAP.Requests.DAP_Request_Access := null;

   begin
      if DAP_Log.Is_Active then
         Trace (DAP_Log, "[" & Self.Id'Img & "<-]" & To_String (Data));
      end if;

      Memory.Set_Data
        (VSS.Stream_Element_Vectors.Conversions.Unchecked_From_Unbounded_String
           (Data));

      Look_Ahead (Seq, Request_Seq, A_Type, R_Success, Message, Event);

      Reader.Set_Stream (Memory'Unchecked_Access);
      Stream.R.Read_Next;
      pragma Assert (Stream.R.Is_Start_Document);
      Stream.R.Read_Next;

      if A_Type = "response" then
         if Request_Seq /= 0 then
            Position := Self.Sent.Find (Request_Seq);

            if Requests_Maps.Has_Element (Position) then
               Request := Requests_Maps.Element (Position);
               Self.Sent.Delete (Position);

               if Self.Status /= Terminating
                 or else Request.all in
                   DAP.Requests.Disconnects.Disconnect_DAP_Request'Class
               then
                  if R_Success.Is_Set
                    and then not R_Success.Value
                  then
                     begin
                        Request.On_Error_Message (Message);
                     exception
                        when E : others =>
                           Trace (Me, E);
                     end;

                  else
                     begin
                        if Request.Kernel = null
                          or else not Request.Kernel.Is_In_Destruction
                        then
                           Request.On_Result_Message (Reader, New_Request);

                           GPS.Kernel.Hooks.Dap_Response_Processed_Hook.Run
                             (Kernel   => Request.Kernel,
                              Method   => Request.Method);
                        end if;

                     exception
                        when E : others =>
                           Trace (Me, E);
                     end;
                  end if;
               end if;

               DAP.Requests.Destroy (Request);
            end if;
         end if;

         if New_Request /= null then
            Self.Process (New_Request);
         end if;

      elsif A_Type = "event" then
         if Self.Status /= Terminating then
            Self.Process_Event (Reader, Event);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Raw_Message;

   --------------------
   -- Get_StackTrace --
   --------------------

   procedure Get_StackTrace
     (Self      : in out DAP_Client;
      Thread_Id : Integer)
   is
      Limit : constant Integer :=
        DAP.Modules.Preferences.Frames_Limit.Get_Pref;
      Req   : StackTrace_Request_Access :=
        new StackTrace_Request (Self.Kernel);

   begin
      Req.Client := Self.This;
      Req.Parameters.arguments.threadId := Thread_Id;
      if Limit /= 0 then
         Req.Parameters.arguments.startFrame := (True, 0);
         Req.Parameters.arguments.levels     := (True, Limit);
      end if;

      Self.Process (DAP.Requests.DAP_Request_Access (Req));
   end Get_StackTrace;

   -------------------
   -- Process_Event --
   -------------------

   procedure Process_Event
     (Self   : in out DAP_Client;
      Stream : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Event  : VSS.Strings.Virtual_String)
   is
      use VSS.Strings;
      use DAP.Tools.Enum;
      use GNATCOLL.VFS;

      Success : Boolean := True;
   begin
      if Event = "output" then
         declare
            output : DAP.Tools.OutputEvent;
         begin
            DAP.Tools.Inputs.Input_OutputEvent (Stream, output, Success);
            if not Success then
               return;
            end if;

            if output.a_body.category.Is_Set
              and then
                ((output.a_body.category.Value = console
                  and then DAP.Modules.Preferences.
                    Debugger_Console_Console.Get_Pref)
                 or else
                   (output.a_body.category.Value = stdout
                    and then DAP.Modules.Preferences.
                      Debugger_Console_Stdout.Get_Pref)
                 or else output.a_body.category.Value = stderr)
            then
               declare
                  Console : constant access Interactive_Console_Record'Class :=
                    DAP.Views.Consoles.Get_Debugger_Interactive_Console (Self);
                  S       : constant String :=
                    VSS.Strings.Conversions.To_UTF_8_String
                      (output.a_body.output);
               begin
                  if Console /= null then
                     Console.Insert
                       ((if output.a_body.category.Value = stderr
                        then "[ERROR] "
                        else "") & S,
                        Add_LF => False);
                  end if;

                  if output.a_body.category.Value = stderr then
                     Trace (Me, "Debugger" & Self.Id'Img & " [stderr]:" & S);
                  end if;
               end;
            end if;
         end;

      elsif Event = "initialized" then
         Self.Set_Status (Initialized);

      elsif Event = "stopped" then
         declare
            stop : DAP.Tools.StoppedEvent;
         begin
            DAP.Tools.Inputs.Input_StoppedEvent (Stream, stop, Success);
            if not Success then
               Self.Set_Status (Stopped);
               return;
            end if;

            if stop.a_body.threadId.Is_Set then
               Integer_Sets.Include
                 (Self.Stopped_Threads, stop.a_body.threadId.Value);
               Self.Selected_Thread := stop.a_body.threadId.Value;
            end if;
            Self.All_Threads_Stopped := stop.a_body.allThreadsStopped;

            if stop.a_body.reason = breakpoint then
               Self.Breakpoints.Stopped
                 (stop, Self.Selected_File,
                  Self.Selected_Line, Self.Selected_Address);

            elsif stop.a_body.reason = step
              and then stop.a_body.threadId.Is_Set
            then
               null;

            else
               Trace (Me, "Debugger" & Self.Id'Img & " stopped:" &
                        stop.a_body.reason'Img);
            end if;

            --  Get stopped frameId/file/line/address
            if stop.a_body.threadId.Is_Set then
               Self.Get_StackTrace (stop.a_body.threadId.Value);

            elsif Self.Selected_File /= No_File then
               Self.Set_Status (Stopped);
            end if;

            --  Trigger Debuggee_Started_Hook if we did not trigger it before
            if not Self.Is_Debuggee_Started_Called then
               Self.Is_Debuggee_Started_Called := True;
               GPS.Kernel.Hooks.Debuggee_Started_Hook.Run
                 (Self.Kernel, Self.Visual);
            end if;

         end;

      elsif Event = "continued" then
         declare
            Continued : DAP.Tools.ContinuedEvent;
         begin
            DAP.Tools.Inputs.Input_ContinuedEvent (Stream, Continued, Success);
            if Continued.a_body.allThreadsContinued then
               Self.All_Threads_Stopped := False;
               Integer_Sets.Clear (Self.Stopped_Threads);
            else
               Integer_Sets.Exclude
                 (Self.Stopped_Threads, Continued.a_body.threadId);
            end if;

            Self.Set_Status (Running);
         end;

      elsif Event = "breakpoint" then
         declare
            use type DAP.Modules.Breakpoint_Managers.
              DAP_Client_Breakpoint_Manager_Access;

            Event : DAP.Tools.BreakpointEvent;
         begin
            DAP.Tools.Inputs.Input_BreakpointEvent (Stream, Event, Success);
            if Success
              and then Self.Breakpoints /= null
            then
               Self.Breakpoints.On_Notification (Event.a_body);
            end if;
         end;

      elsif Event = "thread" then
         --  The tread view uses requests to get the list of threads.
         null;

      elsif Event = "exited" then
         --  Trigger the hook to inform that debuggee process is finished
         if not Self.Is_Debuggee_Started_Called then
            Self.Is_Debuggee_Started_Called := False;
            GPS.Kernel.Hooks.Debugger_Process_Terminated_Hook.Run
              (Self.Kernel, Self.Visual);
         end if;

      elsif Event = "module" then
         --  Do not handle, at least for now.
         null;

      else
         Self.Kernel.Get_Messages_Window.Insert_Error
           ("Event:" & VSS.Strings.Conversions.To_UTF_8_String (Event));

         Trace (Me, "Event:" &
                  VSS.Strings.Conversions.To_UTF_8_String (Event));
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Process_Event;

   --------------------------
   -- Process_User_Command --
   --------------------------

   procedure Process_User_Command
     (Self              : in out DAP_Client;
      Cmd               : String;
      Output_Command    : Boolean := False;
      Result_In_Console : Boolean := False;
      On_Result_Message : GNATCOLL.Scripts.Subprogram_Type := null;
      On_Error_Message  : GNATCOLL.Scripts.Subprogram_Type := null;
      On_Rejected       : GNATCOLL.Scripts.Subprogram_Type := null)
   is
      use GNATCOLL.Scripts;

      Request : DAP.Requests.DAP_Request_Access;

      Tmp     : constant String := GPS.Kernel.Hooks.
        Debugger_Command_Action_Hook.Run
          (Kernel   => Self.Kernel,
           Debugger => Self.Get_Visual,
           Str      => Cmd);

      Result_Message : GNATCOLL.Scripts.Subprogram_Type := On_Result_Message;
      Error_Message  : GNATCOLL.Scripts.Subprogram_Type := On_Error_Message;
      Rejected       : GNATCOLL.Scripts.Subprogram_Type := On_Rejected;

   begin
      if Tmp /= "" then
         if Self.Is_Quit_Command (Cmd) then
            Self.Quit;

         else
            if Output_Command then
               Self.Display_In_Debugger_Console (Cmd, Is_Command => True);
            end if;

            if Self.Is_Frame_Up_Command (Cmd) then
               Self.Frame_Up;

            elsif Self.Is_Frame_Down_Command (Cmd) then
               Self.Frame_Down;
            end if;

            Request := Self.Create_Evaluate_Command
              (Command,
               VSS.Strings.Conversions.To_Virtual_String (Cmd),
               Result_In_Console,
               Result_Message, Error_Message, Rejected);

            Self.Enqueue (Request);
            return;
         end if;
      end if;

      if Result_Message /= null then
         declare

            Arguments : Callback_Data'Class :=
              Result_Message.Get_Script.Create (1);
         begin
            Set_Nth_Arg (Arguments, 1, String'(""));

            declare
               Dummy : GNATCOLL.Any_Types.Any_Type :=
                 Result_Message.Execute (Arguments);

            begin
               null;
            end;

            Free (Arguments);
         end;

         GNATCOLL.Scripts.Free (Result_Message);
      end if;

      GNATCOLL.Scripts.Free (Error_Message);
      GNATCOLL.Scripts.Free (Rejected);

   exception
      when E : others =>
         Trace (Me, E);
   end Process_User_Command;

   ---------------------------------
   -- Display_In_Debugger_Console --
   ---------------------------------

   procedure Display_In_Debugger_Console
     (Self       : in out DAP_Client;
      Msg        : String;
      Is_Command : Boolean := False)
   is
      Console : constant access Interactive_Console_Record'Class :=
        DAP.Views.Consoles.Get_Debugger_Interactive_Console (Self);
      Console_Child : MDI_Child;

   begin
      if Console /= null then
         if Is_Command then
            Console.Insert
              (Msg, Add_LF => True, Highlight => True, Add_To_History => True);
         else
            Console.Insert (Msg, Add_LF => True);
         end if;

         Console_Child := Find_MDI_Child
           (Get_MDI (Self.Kernel), Self.Debugger_Console);

         if Console_Child /= null then
            Highlight_Child (Console_Child);
         end if;
      end if;
   end Display_In_Debugger_Console;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out StackTrace_Request;
      Result      : in out DAP.Tools.StackTraceResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      use GNATCOLL.VFS;
      use DAP.Tools;

   begin
      New_Request := null;

      for Index in 1 .. Length (Result.a_body.stackFrames) loop
         declare
            Frame : constant StackFrame_Variable_Reference :=
              Get_StackFrame_Variable_Reference
                (Result.a_body.stackFrames, Index);
            Bt : Backtrace_Record;
         begin
            Bt.Frame_Id := Frame.id;
            Bt.Name     := To_Unbounded_String
              (VSS.Strings.Conversions.To_UTF_8_String (Frame.name));

            if not Frame.instructionPointerReference.Is_Empty then
               Bt.Address := String_To_Address
                 (VSS.Strings.Conversions.To_UTF_8_String
                    (Frame.instructionPointerReference));
            end if;

            if Frame.source.Is_Set then
               Bt.File := Create
                 (+(VSS.Strings.Conversions.To_UTF_8_String
                  (Frame.source.Value.path)));
               Bt.Line := Frame.line;
            end if;

            if Index = 1 then
               Self.Client.Set_Selected_Frame
                 (Id                        => Bt.Frame_Id,
                  File                      => Bt.File,
                  Line                      => Bt.Line,
                  Address                   => Bt.Address,
                  Run_Location_Changed_Hook => False);
            end if;

            Self.Client.Frames.Append (Bt);
         end;
      end loop;

      Self.Client.Set_Status (Stopped);
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out StackTrace_Request;
      Message : VSS.Strings.Virtual_String) is
   begin
      DAP.Requests.StackTraces.StackTrace_DAP_Request
        (Self).On_Error_Message (Message);

      Self.Client.Set_Status (Stopped);
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out StackTrace_Request) is
   begin
      Self.Client.Set_Status (Stopped);
   end On_Rejected;

   ---------------
   -- Backtrace --
   ---------------

   procedure Backtrace
     (Self : DAP_Client;
      Bt   : out Backtrace_Vectors.Vector) is
   begin
      Bt := Self.Frames;
   end Backtrace;

   -------------------
   -- Set_Backtrace --
   -------------------

   procedure Set_Backtrace
     (Self : in out DAP_Client;
      Bt   : Backtrace_Vectors.Vector) is
   begin
      Self.Frames := Bt;
   end Set_Backtrace;

   --------------
   -- Frame_Up --
   --------------

   procedure Frame_Up (Self : in out DAP_Client)
   is
      Next : Boolean := False;
   begin
      for Bt of Self.Frames loop
         if Next then
            Self.Set_Selected_Frame
              (Bt.Frame_Id, Bt.File, Bt.Line, Bt.Address);
            exit;

         elsif Bt.Frame_Id = Self.Selected_Frame then
            Next := True;
         end if;
      end loop;
   end Frame_Up;

   ----------------
   -- Frame_Down --
   ----------------

   procedure Frame_Down (Self : in out DAP_Client)
   is
      Next : Boolean := False;
   begin
      for Bt of reverse Self.Frames loop
         if Next then
            Self.Set_Selected_Frame
              (Bt.Frame_Id, Bt.File, Bt.Line, Bt.Address);
            exit;

         elsif Bt.Frame_Id = Self.Selected_Frame then
            Next := True;
         end if;
      end loop;
   end Frame_Down;

   ------------------
   -- Select_Frame --
   ------------------

   procedure Select_Frame (Self : in out DAP_Client; Id : Integer) is
   begin
      for Bt of Self.Frames loop
         if Bt.Frame_Id = Id then
            Self.Set_Selected_Frame
              (Bt.Frame_Id, Bt.File, Bt.Line, Bt.Address);
            exit;
         end if;
      end loop;
   end Select_Frame;

   ----------------
   -- On_Started --
   ----------------

   overriding procedure On_Started (Self : in out DAP_Client) is
      Init : DAP.Requests.Initialize.Initialize_DAP_Request_Access :=
        new DAP.Requests.Initialize.Initialize_DAP_Request (Self.Kernel);
   begin
      Self.Process (DAP.Requests.DAP_Request_Access (Init));
   end On_Started;

   ---------------------
   -- On_Disconnected --
   ---------------------

   procedure On_Disconnected (Self : in out DAP_Client) is
   begin
      Self.Stop;
   exception
      when E : others =>
         Trace (Me, E);
   end On_Disconnected;

   -------------
   -- Process --
   -------------

   procedure Process
     (Self    : in out DAP_Client;
      Request : in out DAP.Requests.DAP_Request_Access)
   is
      Id     : constant Integer :=
        Self.Get_Request_ID;
      Writer : VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
      Stream : aliased VSS.Text_Streams.Memory_UTF8_Output.
        Memory_UTF8_Output_Stream;

   begin
      if Self.Status /= Terminating
        or else Request.all in
          DAP.Requests.Disconnects.Disconnect_DAP_Request'Class
      then
         Request.Set_Seq (Id);
         Request.Set_Client (Self.This);
         Writer.Set_Stream (Stream'Unchecked_Access);
         Writer.Start_Document;
         Request.Write (Writer);
         Writer.End_Document;

         --  Send request's message

         Self.Send_Buffer (Stream.Buffer);

         if DAP_Log.Is_Active then
            Trace (DAP_Log,
                   "[" & Self.Id'Img & "->]"
                   & VSS.Stream_Element_Vectors.Conversions.Unchecked_To_String
                     (Stream.Buffer));
         end if;

         --  Add request to the map

         Self.Sent.Insert (Id, Request);

      else
         Request.On_Rejected;
         DAP.Requests.Destroy (Request);
      end if;
   end Process;

   -------------------------
   -- Reject_All_Requests --
   -------------------------

   procedure Reject_All_Requests (Self : in out DAP_Client) is
   begin
      --  Reject all queued requests. Clean commands queue.

      for Request of Self.Sent loop
         Request.On_Rejected;
         DAP.Requests.Destroy (Request);
      end loop;

      Self.Sent.Clear;
   end Reject_All_Requests;

   -----------
   -- Start --
   -----------

   procedure Start
     (Self    : in out DAP_Client;
      Project : GNATCOLL.Projects.Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : String)
   is
      Node_Args : Spawn.String_Vectors.UTF_8_String_Vector;

      function Get_Debugger_Executable return String;
      --  Returns name of debugger and parameters for it

      -----------------------------
      -- Get_Debugger_Executable --
      -----------------------------

      function Get_Debugger_Executable return String is
      begin
         if DAP.Modules.Preferences.DAP_Adapter.Get_Pref /= "" then
            return DAP.Modules.Preferences.DAP_Adapter.Get_Pref;
         end if;

         if Project.Has_Attribute
           (GNATCOLL.Projects.Debugger_Command_Attribute)
         then
            --  return debuger from project
            declare
               Name : constant String := Project.Attribute_Value
                 (GNATCOLL.Projects.Debugger_Command_Attribute);
            begin

               return Name;
            end;
         end if;

         declare
            Tc      : constant Toolchain :=
              Self.Kernel.Get_Toolchains_Manager.Get_Toolchain (Project);
            Command : constant String := Get_Command
              (Tc, Toolchains.Debugger);
         begin
            --  return debugger from toolchain

            return Command;
         end;
      end Get_Debugger_Executable;

      Adapter : constant String := Get_Debugger_Executable;

   begin

      Self.Project    := Project;
      Self.Executable := File;
      Self.Args       := Ada.Strings.Unbounded.To_Unbounded_String (Args);

      declare
         use GNATCOLL.VFS;
         Vals : GNAT.Strings.String_List_Access := GNATCOLL.Utils.Split
           (Adapter, On => ' ');
         Exec : constant Virtual_File :=
           Locate_On_Path (+Vals (Vals'First).all);
      begin
         Trace (Me, "Launching the debug adapter: " & (+Exec.Full_Name)
                & " for file:" & (+Full_Name (Self.Executable)));
         Self.Set_Program (+Exec.Full_Name);
         Node_Args.Append ("-i=dap");
         for Index in Vals'First + 1 .. Vals'Last loop
            Node_Args.Append (Vals (Index).all);
         end loop;
         GNAT.Strings.Free (Vals);
      end;

      Self.Set_Arguments (Node_Args);
      Self.Start;
   end Start;

   --------------------
   -- On_Before_Exit --
   --------------------

   procedure On_Before_Exit (Self : in out DAP_Client) is
      use type DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager_Access;

   begin
      if Self.Status = Initialization
        or else Self.Status = Terminating
      then
         return;
      end if;

      if Self.Breakpoints /= null then
         Self.Breakpoints.Finalize;
         Free (Self.Breakpoints);
      end if;
   exception
      when E : others =>
         Trace (Me, E);
   end On_Before_Exit;

   ----------
   -- Quit --
   ----------

   procedure Quit (Self : in out DAP_Client)
   is
      use type DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager_Access;

      Disconnect : DAP.Requests.Disconnects.Disconnect_DAP_Request_Access;
      Old        : constant Debugger_Status_Kind := Self.Status;
   begin
      if Old = Terminating then
         return;
      end if;

      Self.Set_Status (Terminating);
      Self.Reject_All_Requests;

      if Old /= Initialization then
         Disconnect := new DAP.Requests.Disconnects.
           Disconnect_DAP_Request (Self.Kernel);

         Self.Process (DAP.Requests.DAP_Request_Access (Disconnect));

         if Self.Breakpoints /= null then
            Self.Breakpoints.Finalize;
            Free (Self.Breakpoints);
         end if;

      else
         Self.Stop;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Quit;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Self : in out DAP_Client) is
   begin
      if Self.Status /= Terminating then
         Self.Set_Status (Terminating);
         Self.Reject_All_Requests;
         Self.Stop;
      end if;
   end On_Destroy;

   --------------
   -- Value_Of --
   --------------

   procedure Value_Of
     (Self   : in out DAP_Client;
      Entity : String;
      Label  : Gtk.Label.Gtk_Label)
   is
      Req   : Evaluate_Request_Access := new Evaluate_Request (Self.Kernel);
      Frame : constant Integer := Self.Get_Selected_Frame;
   begin
      Req.Label := Label;
      Ref (GObject (Label));
      Req.Parameters.arguments.expression :=
        VSS.Strings.Conversions.To_Virtual_String (Entity);
      if Frame /= 0 then
         Req.Parameters.arguments.frameId := (Is_Set => True, Value => Frame);
      end if;
      Req.Parameters.arguments.context :=
        (Is_Set => True, Value => DAP.Tools.Enum.repl);
      Self.Enqueue (DAP.Requests.DAP_Request_Access (Req));
   end Value_Of;

   ----------------------------
   -- Set_Breakpoint_Command --
   ----------------------------

   procedure Set_Breakpoint_Command
     (Self    : in out DAP_Client;
      Id      : Breakpoint_Identifier;
      Command : VSS.Strings.Virtual_String)
   is
      Req   : Evaluate_Request_Access := new Evaluate_Request (Self.Kernel);
      Frame : constant Integer := Self.Get_Selected_Frame;
   begin
      Req.Kind := DAP.Clients.Command;
      Req.Parameters.arguments.expression :=
        VSS.Strings.Conversions.To_Virtual_String
          ("command" & Breakpoint_Identifier'Image (Id)
           & ASCII.LF) & Command;

      if not Command.Is_Empty
        and then VSS.Strings.Cursors.Iterators.Characters.Element
          (Command.At_Last_Character) /= VSS.Characters.Latin.Line_Feed
      then
         Req.Parameters.arguments.expression.Append
           (VSS.Characters.Latin.Line_Feed);
      end if;
      Req.Parameters.arguments.expression.Append
        (VSS.Strings.Conversions.To_Virtual_String ("end"));

      if Frame /= 0 then
         Req.Parameters.arguments.frameId := (Is_Set => True, Value => Frame);
      end if;
      Req.Parameters.arguments.context :=
        (Is_Set => True, Value => DAP.Tools.Enum.repl);
      Self.Enqueue (DAP.Requests.DAP_Request_Access (Req));
   end Set_Breakpoint_Command;

   -----------------------------
   -- Create_Evaluate_Command --
   -----------------------------

   function Create_Evaluate_Command
     (Self              : DAP_Client;
      Kind              : Evaluate_Kind;
      Cmd               : Virtual_String;
      Output            : Boolean := False;
      On_Result_Message : GNATCOLL.Scripts.Subprogram_Type := null;
      On_Error_Message  : GNATCOLL.Scripts.Subprogram_Type := null;
      On_Rejected       : GNATCOLL.Scripts.Subprogram_Type := null)
      return DAP.Requests.DAP_Request_Access
   is
      Req   : constant Evaluate_Request_Access :=
        new Evaluate_Request (Self.Kernel);
      Frame : constant Integer := Self.Get_Selected_Frame;
   begin
      Req.Kind   := Kind;
      Req.Client := Self.This;
      Req.Output := Output;

      Req.On_Result_Message := On_Result_Message;
      Req.On_Error_Message  := On_Error_Message;
      Req.On_Rejected       := On_Rejected;

      Req.Parameters.arguments.expression := Cmd;
      if Frame /= 0 then
         Req.Parameters.arguments.frameId :=
           (Is_Set => True, Value => Frame);
      end if;
      Req.Parameters.arguments.context :=
        (Is_Set => True, Value => DAP.Tools.Enum.repl);
      return DAP.Requests.DAP_Request_Access (Req);
   end Create_Evaluate_Command;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Evaluate_Request) is
   begin
      GNATCOLL.Scripts.Free (Self.On_Result_Message);
      GNATCOLL.Scripts.Free (Self.On_Error_Message);
      GNATCOLL.Scripts.Free (Self.On_Rejected);

      DAP.Requests.Evaluate.Evaluate_DAP_Request (Self).Finalize;
   end Finalize;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Evaluate_Request;
      Result      : in out DAP.Tools.EvaluateResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      use GNATCOLL.Scripts;

      procedure Show_File
        (File : Virtual_String;
         Line : Natural;
         Addr : Address_Type);

      ---------------
      -- Show_File --
      ---------------

      procedure Show_File
        (File : Virtual_String;
         Line : Natural;
         Addr : Address_Type) is
      begin
         if not File.Is_Empty
           and then Line /= 0
         then
            DAP.Utils.Highlight_Current_File_And_Line
              (Self.Client.Kernel,
               GNATCOLL.VFS.Create_From_UTF8
                 (VSS.Strings.Conversions.To_UTF_8_String (File)),
               Line);

         elsif Addr /= Invalid_Address then
            --  the address without debugging information
            DAP.Utils.Unhighlight_Current_Line (Self.Client.Kernel);
         end if;
      end Show_File;

   begin
      New_Request := null;

      case Self.Kind is
         when Show_Lang =>
            declare
               Match : VSS.Regular_Expressions.Regular_Expression_Match;
            begin
               Match := Language_Pattern.Match (Result.a_body.result);

               Self.Client.Stored_Lang := (if Match.Has_Match
                                             and then Match.Has_Capture (1)
                                           then Match.Captured (1)
                                           else "auto");
            end;
            New_Request := Self.Client.Create_Evaluate_Command
              (Endian, "show endian");

         when Endian =>
            declare
               Match : VSS.Regular_Expressions.Regular_Expression_Match;
            begin
               Self.Client.Endian := (if Match.Has_Match
                                      then Little_Endian
                                      else Big_Endian);

               Match := Endian_Pattern.Match (Result.a_body.result);
            end;

            New_Request := Self.Client.Create_Evaluate_Command
              (Set_Lang, "set lang c");

         when Set_Lang =>
            New_Request := Self.Client.Create_Evaluate_Command
              (List_Adainit, "list adainit");

         when List_Adainit =>
            New_Request := Self.Client.Create_Evaluate_Command
              (Restore_Lang, "set lang " & Self.Client.Stored_Lang);

         when Restore_Lang =>
            if DAP.Modules.Preferences.Open_Main_Unit.Get_Pref then
               New_Request := Self.Client.Create_Evaluate_Command
                 (Info_Line, "info line");
            else
               Self.Client.Set_Status (Ready);
            end if;

         when Info_Line =>
            declare
               File : Virtual_String;
               Line : Natural;
               Addr : Address_Type;

            begin
               Self.Client.Found_File_Name
                 (Result.a_body.result, File, Line, Addr);

               if not File.Is_Empty then
                  declare
                     Match : VSS.Regular_Expressions.Regular_Expression_Match;
                  begin
                     Match := GNAT_Binder_File_Pattern.Match (File);

                     --  If we find a file that looks like a GNAT binder file,
                     --  load the corresponding main file.

                     if Match.Has_Match
                       and then Match.Has_Capture (1)
                     then
                        New_Request := Self.Client.Create_Evaluate_Command
                          (Info_First_Line, "info line "
                           & Match.Captured (1) & ":1");
                     else
                        Show_File (File, Line, Addr);
                        Self.Client.Set_Status (Ready);
                     end if;
                  end;
               else
                  Self.Client.Set_Status (Ready);
               end if;
            end;

         when Info_First_Line =>
            declare
               File : Virtual_String;
               Line : Natural;
               Addr : Address_Type;

            begin
               Self.Client.Found_File_Name
                 (Result.a_body.result, File, Line, Addr);
               Show_File (File, Line, Addr);
               Self.Client.Set_Status (Ready);
            end;

         when Hover =>
            Self.Label.Set_Markup
              ("<b>Debugger value :</b> " & Glib.Convert.Escape_Text
                 (VSS.Strings.Conversions.To_UTF_8_String
                      (Result.a_body.result)));
            Unref (GObject (Self.Label));

         when Variable_Address =>
            declare
               S     : constant String := VSS.Strings.Conversions.
                 To_UTF_8_String (Result.a_body.result);
               Index : Integer := S'Last;
            begin
               String_Utils.Skip_To_Char (S, Index, 'x', Step => -1);
               if Index >= S'First then
                  DAP.Views.Memory.Display_Memory
                    (Self.Kernel, "0" & S (Index .. S'Last));
               end if;
            end;

         when Command =>
            if Self.Output then
               Self.Client.Display_In_Debugger_Console
                 (VSS.Strings.Conversions.To_UTF_8_String
                    (Result.a_body.result), False);
            end if;

            if Self.On_Result_Message /= null then
               declare

                  Arguments : Callback_Data'Class :=
                    Self.On_Result_Message.Get_Script.Create (1);
               begin
                  Set_Nth_Arg
                    (Arguments, 1,
                     VSS.Strings.Conversions.To_UTF_8_String
                       (Result.a_body.result));

                  declare
                     Dummy : GNATCOLL.Any_Types.Any_Type :=
                       Self.On_Result_Message.Execute (Arguments);

                  begin
                     null;
                  end;

                  Free (Arguments);
               end;
            end if;
      end case;
   end On_Result_Message;

   ---------------------
   -- Found_File_Name --
   ---------------------

   procedure Found_File_Name
     (Self : DAP_Client;
      Str  : VSS.Strings.Virtual_String;
      Name : out VSS.Strings.Virtual_String;
      Line : out Natural;
      Addr : out DAP.Types.Address_Type)
   is
      pragma Unreferenced (Self);

      Match : VSS.Regular_Expressions.Regular_Expression_Match;
   begin
      --  Default values if nothing better is found
      Name := Empty_Virtual_String;
      Line := 0;
      Addr := Invalid_Address;

      Match := File_Name_Pattern.Match (Str);

      if Match.Has_Match then
         if Match.Has_Capture (2) then
            Name := Match.Captured (2);
         end if;

         if Match.Has_Capture (3) then
            Addr := String_To_Address
              (VSS.Strings.Conversions.To_UTF_8_String
                 (Match.Captured (3)));
         end if;

         if Match.Has_Capture (1) then
            Line := Natural'Value
              (VSS.Strings.Conversions.To_UTF_8_String
                 (Match.Captured (1)));
         end if;
      end if;

   exception
      when E : others =>
         Me.Trace (E);

         Name := Empty_Virtual_String;
         Line := 0;
         Addr := Invalid_Address;
   end Found_File_Name;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out Evaluate_Request)
   is
      use GNATCOLL.Scripts;
   begin
      case Self.Kind is
         when Hover =>
            Self.Label.Set_Markup ("<b>Debugger value :</b> (rejected)");
            Unref (GObject (Self.Label));

         when Variable_Address | Endian =>
            null;

         when Command =>
            if Self.Output then
               Self.Client.Display_In_Debugger_Console ("Rejected", False);
            end if;

            if Self.On_Rejected /= null then
               declare
                  Arguments : Callback_Data'Class :=
                    Self.On_Rejected.Get_Script.Create (0);
                  Dummy     : GNATCOLL.Any_Types.Any_Type :=
                    Self.On_Rejected.Execute (Arguments);

               begin
                  Free (Arguments);
               end;
            end if;

         when Show_Lang | Set_Lang | Restore_Lang | List_Adainit |
              Info_Line | Info_First_Line =>
               Self.Client.Set_Status (Ready);
      end case;
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Evaluate_Request;
      Message : VSS.Strings.Virtual_String)
   is
      use GNATCOLL.Scripts;
   begin
      DAP.Requests.Evaluate.Evaluate_DAP_Request
        (Self).On_Error_Message (Message);

      case Self.Kind is
         when Hover =>
            Self.Label.Set_Markup ("<b>Debugger value :</b> (error)");
            Unref (GObject (Self.Label));

         when Variable_Address | Endian =>
            null;

         when Command =>
            if Self.Output then
               Self.Client.Display_In_Debugger_Console
                 (VSS.Strings.Conversions.To_UTF_8_String (Message), False);
            end if;

            if Self.On_Error_Message /= null then
               declare
                  Arguments : Callback_Data'Class :=
                    Self.On_Error_Message.Get_Script.Create (1);

               begin
                  Set_Nth_Arg
                    (Arguments, 1,
                     VSS.Strings.Conversions.To_UTF_8_String (Message));

                  declare
                     Dummy : GNATCOLL.Any_Types.Any_Type :=
                       Self.On_Error_Message.Execute (Arguments);

                  begin
                     null;
                  end;

                  Free (Arguments);
               end;
            end if;

         when Show_Lang | Set_Lang | Restore_Lang | List_Adainit |
              Info_Line | Info_First_Line =>
            Self.Client.Set_Status (Ready);
      end case;
   end On_Error_Message;

   ------------------------
   -- Command_In_Process --
   ------------------------

   overriding function Command_In_Process
     (Visual : not null access DAP_Visual_Debugger) return Boolean is
   begin
      return Visual.Client /= null
        and then not Visual.Client.Is_Ready_For_Command;
   end Command_In_Process;

   ----------------------
   -- Show_Breakpoints --
   ----------------------

   procedure Show_Breakpoints (Self : DAP_Client) is
      use type DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager_Access;
   begin
      if Self.Breakpoints /= null then
         Self.Breakpoints.Show_Breakpoints;
      end if;
   end Show_Breakpoints;

   --------------------------
   -- Get_Variable_Address --
   --------------------------

   procedure Get_Variable_Address
     (Self     : in out DAP_Client;
      Variable : String) is
   begin
      if Variable /= "" then
         declare
            Req   : Evaluate_Request_Access :=
              new Evaluate_Request (Self.Kernel);
            Frame : constant Integer := Self.Get_Selected_Frame;
         begin
            Req.Kind   := Variable_Address;
            Req.Client := Self.This;
            Req.Parameters.arguments.expression := VSS.Strings.Conversions.
              To_Virtual_String ("print &(" & Variable & ")");
            if Frame /= 0 then
               Req.Parameters.arguments.frameId :=
                 (Is_Set => True, Value => Frame);
            end if;
            Req.Parameters.arguments.context :=
              (Is_Set => True, Value => DAP.Tools.Enum.repl);
            Self.Enqueue (DAP.Requests.DAP_Request_Access (Req));
         end;
      end if;
   end Get_Variable_Address;

end DAP.Clients;
