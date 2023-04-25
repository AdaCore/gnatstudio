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

with GNAT.OS_Lib;

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with VSS.Strings;
with VSS.String_Vectors;

with Glib;
with Gtk.Label;

with GPS.Kernel;
with GPS.Debuggers;

with LSP.Raw_Clients;

with DAP.Modules.Breakpoints;
with DAP.Modules.Histories;
with DAP.Requests;
with DAP.Types;                  use DAP.Types;
with DAP.Tools;

with Basic_Types;                use Basic_Types;
with Generic_Views;
with Language;                   use Language;

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Hashed_Sets;
private with Ada.Strings.Unbounded;
private with VSS.JSON.Pull_Readers;
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

   -- DAP_Client --

   type DAP_Client
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Id     : Positive) is new LSP.Raw_Clients.Raw_Client with private;
   --  The client to communicate with DAP adapter

   type DAP_Client_Access is access all DAP_Client'Class;

   -- DAP_Visual_Debugger --

   type DAP_Visual_Debugger is
     new GPS.Debuggers.Base_Visual_Debugger with record
      Client : DAP_Client_Access;
   end record;
   type DAP_Visual_Debugger_Access is access all DAP_Visual_Debugger'Class;

   --  DAP_Client --

   procedure Initialize (Self : not null access DAP_Client);

   procedure Start
     (Self    : in out DAP_Client;
      Project : GNATCOLL.Projects.Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : String);
   --  Start DAP adapter

   function Is_Stopped (Self : DAP_Client) return Boolean;
   --  Debugging program is stopped and new command can be accepted
   function Is_Ready (Self : DAP_Client) return Boolean;
   --  Debugging program is ready for start
   function Is_Ready_For_Command (Self : DAP_Client) return Boolean;
   --  Debugger can accept new command. Debugging can be not started yet.

   procedure Enqueue
     (Self    : in out DAP_Client;
      Request : in out DAP.Requests.DAP_Request_Access);
   --  Add the commant to a queue to send it to the adapter

   procedure Quit (Self : in out DAP_Client);

   --  Notification about debugging process' status changing --

   procedure On_Launched (Self : in out DAP_Client);
   --  Inform that the debugger is ready for debugging
   procedure On_Ready (Self : in out DAP_Client);
   procedure On_Configured (Self : in out DAP_Client);
   --  Debugger starts executing debugree program
   procedure On_Continue (Self : in out DAP_Client);
   procedure On_Terminated (Self : in out DAP_Client);
   procedure On_Breakpoints_Set (Self : in out DAP_Client);

   function Get_Status (Self : in out DAP_Client) return Debugger_Status_Kind;

   function Has_Breakpoint
     (Self      : DAP_Client;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type)
      return Boolean;
   --  Return True if some breakpoint is set for the file/line

   procedure Break_Source
     (Self      : in out DAP_Client;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False);
   --  Add a breakpoint for the file/line

   procedure Break_Subprogram
     (Self       : in out DAP_Client;
      Subprogram : String;
      Temporary  : Boolean := False);
   --  Add a breakpoint for the subprogram

   procedure Toggle_Instruction_Breakpoint
     (Self    : in out DAP_Client;
      Address : Address_Type);
   --  Add/delete a breakpoint for the address

   procedure Remove_Breakpoints
     (Self : in out DAP_Client;
      List : DAP.Types.Breakpoint_Identifier_Lists.List);
   --  Remove breakpoints included in the list

   procedure Remove_All_Breakpoints (Self : in out DAP_Client);
   --  Remove all breakpoints

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

   function Get_Assembly_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access;
   --  Returns the assembly view, if any.

   procedure Set_Assembly_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access);
   --  Attach the assembly view to the client

   function Get_Breakpoints
     (Self : DAP_Client)
      return DAP.Modules.Breakpoints.Breakpoint_Vectors.Vector;
   --  Returns the list of breakpoints

   function Get_Call_Stack_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access;
   --  Returns the callstack view, if any.

   procedure Set_Call_Stack_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access);
   --  Attach the callstack view to the client

   function Get_Thread_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access;
   --  Returns the thread view, if any.

   procedure Set_Thread_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access);
   --  Attach the thread view to the client

   function Get_Debugger_Console
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access;
   --  Returns the debugger console, if any.

   procedure Set_Debugger_Console
     (Self    : in out DAP_Client;
      Console : Generic_Views.Abstract_View_Access);
   --  Set the debugger console

   function Get_Memory_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access;
   --  Returns the assembly view, if any.

   procedure Set_Memory_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access);
   --  Attach the assembly view to the client

   function Get_Variables_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access;
   --  Returns the variables view, if any.

   procedure Set_Variables_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access);
   --  Attach the variables view to the client

   procedure Set_Selected_Frame
     (Self    : in out DAP_Client;
      Id      : Integer;
      File    : GNATCOLL.VFS.Virtual_File;
      Line    : Integer;
      Address : Address_Type);

   function Get_Selected_Frame
     (Self : DAP_Client) return Integer;

   function Current_File
     (Self : in out DAP_Client) return GNATCOLL.VFS.Virtual_File;
   --  Returns the file where the debugging is stopped

   function Current_Line
     (Self : in out DAP_Client) return Integer;
   --  Returns the line where the debugging is stopped

   function Current_Address
     (Self : in out DAP_Client) return Address_Type;
   --  Returns the address where the debugging is stopped

   function Get_Executable
     (Self : in out DAP_Client) return GNATCOLL.VFS.Virtual_File;
   --  Return the name of the executable currently debugged.

   function Get_Language
     (Self : in out DAP_Client) return Language_Access;

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

   procedure Value_Of
     (Self   : in out DAP_Client;
      Entity : String;
      Label  : Gtk.Label.Gtk_Label);

   procedure Set_Capabilities
     (Self         : in out DAP_Client;
      Capabilities : DAP.Tools.Optional_Capabilities);

   function Get_Capabilities
     (Self : in out DAP_Client)
      return DAP.Tools.Optional_Capabilities;

   procedure Display_In_Debugger_Console
     (Self       : in out DAP_Client;
      Msg        : String;
      Is_Command : Boolean := False);
   --  Displays the message in the console. Highlight it and add to the
   --  history if Is_Command is True

   procedure Show_Breakpoints (Self : DAP_Client);

   procedure Get_Variable_Address
     (Self     : in out DAP_Client;
      Variable : String);

   function Get_Endian_Type (Self : in out DAP_Client) return Endian_Type;

   procedure Backtrace
     (Self : in out DAP_Client;
      Bt   : out Backtrace_Vectors.Vector);
   --  Returns backtrace

   procedure Stack_Up (Self : in out DAP_Client);
   procedure Stack_Down (Self : in out DAP_Client);
   procedure Stack_Frame (Self : in out DAP_Client; Id : Integer);
   --  Select frame

   procedure Set_Backtrace
     (Self : in out DAP_Client;
      Bt   : Backtrace_Vectors.Vector);
   --  Update backtrace information

   procedure Process_User_Command
     (Self           : in out DAP_Client;
      Cmd            : String;
      Output_Command : Boolean := False);
   --  Execute the debugger command. Print the command in the console
   --  if Output_Command is True

   -- DAP_Visual_Debugger --

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
      This           : DAP_Client_Access := DAP_Client'Unchecked_Access;
      Visual         : DAP_Visual_Debugger_Access;
      Project        : GNATCOLL.Projects.Project_Type;
      File           : Ada.Strings.Unbounded.Unbounded_String;
      Args           : Ada.Strings.Unbounded.Unbounded_String;
      Source_Files   : VSS.String_Vectors.Virtual_String_Vector;

      Is_Attached    : Boolean := False;
      Capabilities   : DAP.Tools.Optional_Capabilities;
      Status         : Debugger_Status_Kind := Initialization;
      Endian         : Endian_Type := Little_Endian;

      Sent           : Requests_Maps.Map;

      Request_Id     : Integer := 1;
      Error_Msg      : VSS.Strings.Virtual_String;

      Selected_File       : GNATCOLL.VFS.Virtual_File;
      Selected_Line       : Integer;
      Selected_Address    : Address_Type := Invalid_Address;
      Selected_Frame      : Integer;
      Frames              : Backtrace_Vectors.Vector;

      --  to monitoring stoped threads
      Stopped_Threads     : Integer_Sets.Set;
      All_Threads_Stopped : Boolean := False;
      Selected_Thread     : Integer := 0;

      --  internal data
      Stored_Lang         : VSS.Strings.Virtual_String;

      --  Modules --
      Breakpoints      : DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager_Access;

      Command_History  : aliased String_History.History_List;

      --  Views --
      Call_Stack_View  : Generic_Views.Abstract_View_Access := null;
      Thread_View      : Generic_Views.Abstract_View_Access := null;
      Assembly_View    : Generic_Views.Abstract_View_Access := null;
      Memory_View      : Generic_Views.Abstract_View_Access := null;
      Variables_View   : Generic_Views.Abstract_View_Access := null;
      Debugger_Console : Generic_Views.Abstract_View_Access := null;
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

   procedure Process
     (Self    : in out DAP_Client;
      Request : in out DAP.Requests.DAP_Request_Access);
   --  Process (send) the request to the DAP adapter

   function Get_Request_ID
     (Self : in out DAP_Client) return Integer;
   --  Returns the unique ID for the request

   procedure Reject_All_Requests (Self : in out DAP_Client);
   --  Discard all requests included in the internal queue

   procedure Process_Event
     (Self   : in out DAP_Client;
      Stream : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Event  : VSS.Strings.Virtual_String);
   --  Process the event from the DAP adapter

   procedure Set_Status
     (Self   : in out DAP_Client;
      Status : Debugger_Status_Kind);
   --  Set the current debugging status

   procedure Load_Project_From_Executable (Self : in out DAP_Client);

   procedure On_Location_Changed
     (Self : in out DAP_Client);

   procedure Get_StackTrace
     (Self      : in out DAP_Client;
      Thread_Id : Integer);

   procedure Found_File_Name
     (Self : DAP_Client;
      Str  : VSS.Strings.Virtual_String;
      Name : out VSS.Strings.Virtual_String;
      Line : out Natural;
      Addr : out DAP.Types.Address_Type);

   type Evaluate_Kind is
     (Show_Lang, Set_Lang, Restore_Lang, List_Adainit,
      Info_Line, Info_First_Line, Hover, Variable_Address, Endian, Command);

   function Create_Evaluate_Command
     (Self : DAP_Client;
      Kind : Evaluate_Kind;
      Cmd  : VSS.Strings.Virtual_String)
      return DAP.Requests.DAP_Request_Access;

end DAP.Clients;
