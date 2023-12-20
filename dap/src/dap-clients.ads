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

with Ada.Exceptions;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;
with GNAT.TTY;

with GNATCOLL.Projects;
with GNATCOLL.Scripts;
with GNATCOLL.VFS;

with VSS.JSON;
with VSS.JSON.Pull_Readers;
with VSS.Strings;
with VSS.String_Vectors;

with Glib;

with GPS.Kernel;
with GPS.Debuggers;
with GPS.Markers;

with LSP.Raw_Clients;

with DAP.Modules.Breakpoints;
with DAP.Modules.Histories;
with DAP.Requests;
with DAP.Types;                  use DAP.Types;
with DAP.Tools;
limited with DAP.Clients.Stack_Trace;

with Basic_Types;                use Basic_Types;
with Generic_Views;

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Hashed_Sets;
private with DAP.Modules.Breakpoint_Managers;

package DAP.Clients is

   -- String_History --

   type History_Data is record
      Mode    : DAP.Types.Command_Type;
      Command : GNAT.OS_Lib.String_Access;
   end record;

   package String_History is new DAP.Modules.Histories (History_Data);
   use String_History;
   --  Used for holding commands history in the debugging console

   ----------------
   -- DAP_Client --
   ----------------

   type DAP_Client
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Id     : Positive) is new LSP.Raw_Clients.Raw_Client with private;
   --  The client to communicate with DAP adapter

   type DAP_Client_Access is access all DAP_Client'Class;

   -------------------------
   -- DAP_Visual_Debugger --
   -------------------------

   type DAP_Visual_Debugger is
     new GPS.Debuggers.Base_Visual_Debugger with record
      Client : DAP_Client_Access;
   end record;
   type DAP_Visual_Debugger_Access is access all DAP_Visual_Debugger'Class;

   -----------------
   --  DAP_Client --
   -----------------

   procedure Initialize_Client (Self : not null access DAP_Client);

   procedure Start
     (Self               : in out DAP_Client;
      Project            : GNATCOLL.Projects.Project_Type;
      File               : GNATCOLL.VFS.Virtual_File;
      Executable_Args    : String);
   --  Start DAP adapter

   function Is_Stopped (Self : DAP_Client) return Boolean;
   --  Debugging program is stopped and new command can be accepted
   function Is_Ready (Self : DAP_Client) return Boolean;
   --  Debugging program is ready for start
   function Is_Ready_For_Command (Self : DAP_Client) return Boolean;
   --  Debugger can accept new command. Debugging can be not started yet.

   procedure Enqueue
     (Self    : in out DAP_Client;
      Request : in out DAP.Requests.DAP_Request_Access;
      Force   : Boolean := False);
   --  Queue the given request to send it to the DAP adapter
   --  Does not check whether the debugger is stopped when Force is True

   procedure Quit (Self : in out DAP_Client);

   --  Notification about debugging process' status changing --

   procedure On_Launched (Self : in out DAP_Client);
   --  Inform that the debugger is ready for debugging

   procedure On_Configured (Self : in out DAP_Client);
   --  Debugger starts executing debugree program

   procedure On_Breakpoints_Set (Self : in out DAP_Client);
   --  Called when all the initial breakpoints have been set on the server
   --  side. Will set the debugger's status to Ready.

   procedure On_Continue (Self : in out DAP_Client);
   --  Called on continue requests. Will set the debugger's status to Running.

   procedure On_Before_Exit (Self : in out DAP_Client);
   --  Called when GNAT Studio is exiting.

   procedure On_Destroy (Self : in out DAP_Client);
   --  Called when DAP module is being destroyed, to terminate the current
   --  debugging session.

   function Get_Status (Self : in out DAP_Client) return Debugger_Status_Kind;
   --  Return the debugger's status.

   function Get_Debuggee_Start_Method
     (Self : DAP_Client) return Debuggee_Start_Method_Kind;
   --  Return the method used to start the debuggee by the given DAP client.

   function Has_Breakpoint
     (Self   : DAP_Client;
      Marker : GPS.Markers.Location_Marker)
      return Boolean;
   --  Return True if some breakpoint is set for the location

   procedure Break
     (Self : in out DAP_Client;
      Data : DAP.Modules.Breakpoints.Breakpoint_Data);

   procedure Break_Source
     (Self      : in out DAP_Client;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False;
      Condition : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);
   --  Add a breakpoint for the file/line

   procedure Break_Subprogram
     (Self       : in out DAP_Client;
      Subprogram : String;
      Temporary  : Boolean := False;
      Condition  : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);
   --  Add a breakpoint for the subprogram

   procedure Break_Exception
     (Self      : in out DAP_Client;
      Name      : String;
      Unhandled : Boolean := False;
      Temporary : Boolean := False);
   --  Add a breakpoint for the exception

   procedure Break_Address
     (Self      : in out DAP_Client;
      Address   : Address_Type;
      Temporary : Boolean := False;
      Condition : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);
   --  Add a breakpoint for the address

   procedure Toggle_Instruction_Breakpoint
     (Self    : in out DAP_Client;
      Address : Address_Type);
   --  Add/delete a breakpoint for the address

   procedure Remove_Breakpoints
     (Self : in out DAP_Client;
      List : DAP.Types.Breakpoint_Identifier_Lists.List);
   --  Remove breakpoints included in the given list

   procedure Remove_All_Breakpoints (Self : in out DAP_Client);
   --  Remove all breakpoints set for this DAP client

   procedure Remove_Breakpoint_At
     (Self : in out DAP_Client;
      File : GNATCOLL.VFS.Virtual_File;
      Line : Editable_Line_Type);
   --  Remove breakpoint from the file/line

   procedure Set_Breakpoints_State
     (Self  : in out DAP_Client;
      List  : Breakpoint_Identifier_Lists.List;
      State : Boolean);
   --  Enable/disable breakpoints

   function Get_Breakpoints
     (Self : DAP_Client)
      return DAP.Modules.Breakpoints.Breakpoint_Vectors.Vector;
   --  Returns the list of breakpoints

   function Get_Debugger_Console
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access;
   --  Returns the debugger console, if any.

   procedure Set_Debugger_Console
     (Self    : in out DAP_Client;
      Console : Generic_Views.Abstract_View_Access);
   --  Set the debugger console

   function Get_Debuggee_Console
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access;
   --  Returns the debuggee view, if any.

   procedure Set_Debuggee_Console
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access);
   --  Attach the debuggee view

   procedure Set_Selected_Frame
     (Self : in out DAP_Client;
      Id   : Integer);
   --  Set the current Frame.

   function Get_Executable
     (Self : in out DAP_Client) return GNATCOLL.VFS.Virtual_File;
   --  Return the name of the executable currently debugged.

   function Get_Project
     (Self : in out DAP_Client) return GNATCOLL.Projects.Project_Type;
   --  Return the project currently debugged.

   function Get_Executable_Args
     (Self : in out DAP_Client) return Ada.Strings.Unbounded.Unbounded_String;
   --  Return the command line arguments for the executable currently debugged.

   procedure Set_Source_Files
     (Self         : in out DAP_Client;
      Source_Files : VSS.String_Vectors.Virtual_String_Vector);

   procedure Set_Selected_Thread (Self : in out DAP_Client; Id : Integer);
   --  Set the Thread ID that has been selected in the thread view

   function Get_Current_Thread (Self  : in out DAP_Client) return Integer;
   --  Returns current selected stopped thread Id or
   --  the first stopped thread or 0

   function Get_Command_History
     (Self : in out DAP_Client)
      return History_List_Access;
   --  Returns debugging console commands history

   function Get_Visual
     (Self : in out DAP_Client)
      return DAP_Visual_Debugger_Access;
   --  Return the visual debugger associated with the given DAP client

   procedure Set_Breakpoint_Command
     (Self    : in out DAP_Client;
      Id      : Breakpoint_Identifier;
      Command : VSS.Strings.Virtual_String);
   --  Set a command that will be executed when reaching the breakpoint
   --  designated by Id.

   procedure Set_Capabilities
     (Self         : in out DAP_Client;
      Capabilities : DAP.Tools.Optional_Capabilities);
   --  Set the DAP client's capabilities.

   function Get_Capabilities
     (Self : in out DAP_Client)
      return DAP.Tools.Optional_Capabilities;
   --  Return the DAP client's capabilities.

   procedure Display_In_Debugger_Console
     (Self       : in out DAP_Client;
      Msg        : String;
      Is_Command : Boolean := False);
   --  Displays the message in the console. Highlight it and add to the
   --  history if Is_Command is True.

   procedure Show_Breakpoints (Self : DAP_Client);

   function Get_Endian_Type (Self : in out DAP_Client) return Endian_Type;
   --  Reurn the debugee's endianness, if a debuggee has been
   --  launched/attached.
   --  Little endian is returned by default if not.

   function Get_Stack_Trace
     (Self : DAP_Client)
      return DAP.Clients.Stack_Trace.Stack_Trace_Access;
   --  Returns the Stack_Trace module

   procedure Process_User_Command
     (Self              : in out DAP_Client;
      Cmd               : String;
      Output_Command    : Boolean := False;
      Result_In_Console : Boolean := False;
      On_Result_Message : GNATCOLL.Scripts.Subprogram_Type := null;
      On_Error_Message  : GNATCOLL.Scripts.Subprogram_Type := null;
      On_Rejected       : GNATCOLL.Scripts.Subprogram_Type := null);
   --  Execute the debugger command.
   --  Print the command in the console if Output_Command is True
   --  Print the result in the console if Result_In_Console is True
   --  On_Result_Message, On_Error_Message, On_Rejected callbacks used in the
   --  Python API.

   function Is_Quit_Command
     (Self : DAP_Client;
      Cmd  : VSS.Strings.Virtual_String)
      return Boolean;
   --  Is `quit` command. Used to intercept console input.

   function Is_Frame_Up_Command
     (Self : DAP_Client;
      Cmd  : VSS.Strings.Virtual_String)
      return Boolean;
   --  Is `up` command. Used to intercept console input.

   function Is_Frame_Down_Command
     (Self : DAP_Client;
      Cmd  : VSS.Strings.Virtual_String)
      return Boolean;
   --  Is `down` command. Used to intercept console input.

   function Is_Frame_Command
     (Self  : DAP_Client;
      Cmd   : VSS.Strings.Virtual_String;
      Level : out Integer)
      return Boolean;
   --  Is `frame` command. Used to intercept console input.

   procedure Process_Event
     (Self   : in out DAP_Client;
      Stream : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Event  : VSS.Strings.Virtual_String);
   --  Process the event from the DAP adapter

   -------------------------
   -- DAP_Visual_Debugger --
   -------------------------

   overriding function Get_Num
     (Self : not null access DAP_Visual_Debugger) return Glib.Gint
      is (Glib.Gint (Self.Client.Id));

   overriding function Command_In_Process
     (Visual : not null access DAP_Visual_Debugger) return Boolean;

   function Current_File
     (Visual : not null access DAP_Visual_Debugger)
      return GNATCOLL.VFS.Virtual_File;

   function Current_Line
     (Visual : not null access DAP_Visual_Debugger)
      return Natural;

   procedure Set_Executable
     (Self : in out DAP_Client;
      File : GNATCOLL.VFS.Virtual_File);
   --  Set the main file

   function Get_Debuggee_TTY
     (Self : DAP_Client)
      return GNAT.TTY.TTY_Handle;
   --  Returns TTY allocated for debuggee

   procedure Allocate_TTY (Self : in out DAP_Client);
   --  Open TTY that will be used for debuggee

   procedure Close_TTY (Self : in out DAP_Client);
   --  Close TTY that is used for debuggee

   procedure Continue_Execution (Self : in out DAP_Client);
   --  Sends the corresponding request to continue debuggee execution.

private

   function Hash
     (Item : Integer)
      return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Val (Item));

   package Requests_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Integer,
      Element_Type    => DAP.Requests.DAP_Request_Access,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => DAP.Requests."=");

   package Integer_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Integer,
      Hash                => Hash,
      Equivalent_Elements => "=");

   type DAP_Client
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Id     : Positive) is new LSP.Raw_Clients.Raw_Client
   with record
      This            : DAP_Client_Access := DAP_Client'Unchecked_Access;
      Visual          : DAP_Visual_Debugger_Access;
      Project         : GNATCOLL.Projects.Project_Type;
      Executable      : GNATCOLL.VFS.Virtual_File;
      Executable_Args : Ada.Strings.Unbounded.Unbounded_String;
      Source_Files    : VSS.String_Vectors.Virtual_String_Vector;

      Is_Attached                : Boolean := False;
      Is_Debuggee_Started_Called : Boolean := False;

      Capabilities   : DAP.Tools.Optional_Capabilities;
      Status         : Debugger_Status_Kind := Initialization;
      Endian         : Endian_Type := Unknown_Endian;

      Sent           : Requests_Maps.Map;

      Request_Id     : Integer := 1;
      Error_Msg      : VSS.Strings.Virtual_String;

      --  to monitoring stoped threads
      Stopped_Threads     : Integer_Sets.Set;
      All_Threads_Stopped : Boolean := False;
      Selected_Thread     : Integer := 0;

      Start_Method        : Debuggee_Start_Method_Kind := None;
      --  The method that was used to start the current debugging session

      --  Modules --
      Breakpoints      : DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager_Access;
      Stack_Trace      : access DAP.Clients.Stack_Trace.Stack_Trace'Class;

      Command_History  : aliased String_History.History_List;

      Debugger_Console : Generic_Views.Abstract_View_Access := null;
      Debuggee_Console : Generic_Views.Abstract_View_Access := null;
      Debuggee_TTY     : GNAT.TTY.TTY_Handle;
   end record;

   overriding function Error_Message
     (Self : DAP_Client) return VSS.Strings.Virtual_String;
   --  Error message for the last detected "internal error".

   overriding procedure On_Raw_Message
     (Self    : in out DAP_Client;
      Data    : Ada.Strings.Unbounded.Unbounded_String;
      Success : in out Boolean);
   --  Called to parse RAW response from the DAP adapter

   overriding procedure On_Started (Self : in out DAP_Client);
   --  Send initialization request on successful startup of the language
   --  server process.

   overriding procedure On_Finished (Self : in out DAP_Client);
   --  Handle termination of the language server process. If this wasn't
   --  expected and we're within the acceptable throttling limits, relaunch.

   overriding procedure On_Error
     (Self  : in out DAP_Client;
      Error : String);

   overriding procedure On_Standard_Error_Message
     (Self : in out DAP_Client;
      Text : String);

   overriding procedure On_Exception
     (Self       : in out DAP_Client;
      Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Set_Status
     (Self   : in out DAP_Client'Class;
      Status : Debugger_Status_Kind);
   --  Set the current debugging status.
   --  Will run the debugger hook appropriate to the new status.

   procedure Process
     (Self    : in out DAP_Client;
      Request : in out DAP.Requests.DAP_Request_Access);
   --  Process (send) the request to the DAP adapter

   function Get_Request_ID
     (Self : in out DAP_Client) return Integer;
   --  Returns the unique ID for the request

   procedure Reject_All_Requests (Self : in out DAP_Client);
   --  Discard all requests included in the internal queue

   procedure Load_Project_From_Executable (Self : in out DAP_Client);
   --  Creates a project based on the debugger's response about sources. Used
   --  when the debugger is started via the --debug switch.

end DAP.Clients;
