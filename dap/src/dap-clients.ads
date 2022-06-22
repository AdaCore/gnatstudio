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

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with VSS.Strings;
with VSS.String_Vectors;

with Glib.Object;

with Gtk.Label;

with GPS.Debuggers;
with GPS.Kernel;

with LSP.Raw_Clients;

with DAP.Requests;
with DAP.Types;            use DAP.Types;
with DAP.Breakpoint_Maps;

with Basic_Types;          use Basic_Types;
with Generic_Views;

private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded;
private with VSS.JSON.Pull_Readers;
private with DAP.Modules.Breakpoint_Managers;

package DAP.Clients is

   type DAP_Client
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Id     : Positive) is new LSP.Raw_Clients.Raw_Client with private;
   --  The client to communicate with DAP adapter

   type DAP_Client_Access is access all DAP_Client'Class;

   procedure Start
     (Self    : in out DAP_Client;
      Adapter : String;
      Project : GNATCOLL.Projects.Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : String);
   --  Start DAP adapter

   function Is_Stopped (Self : DAP_Client) return Boolean;
   --  Debugging program is stopped and new command can be accepted

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

   function Get_Breakpoints
     (Self : DAP_Client)
      return DAP.Breakpoint_Maps.All_Breakpoints;
   --  Returns the list of breakpoints

   function Get_Breakpoints_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access;
   --  Returns the breakpoints view, if any.

   procedure Set_Breakpoints_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access);
   --  Attach the breakpoints view to the client

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

   procedure Set_Selected_Frame
     (Self : in out DAP_Client;
      Id   : Integer);

   function Get_Selected_Frame
     (Self : in out DAP_Client) return Integer;

   function Current_File
     (Self : in out DAP_Client) return GNATCOLL.VFS.Virtual_File;
   --  Returns the file where the debugging is stopped

   function Current_Line
     (Self : in out DAP_Client) return Integer;
   --  Returns the line where the debugging is stopped

   function Get_Executable
     (Self : in out DAP_Client) return GNATCOLL.VFS.Virtual_File;
   --  Return the name of the executable currently debugged.

   procedure Set_Source_Files
     (Self         : in out DAP_Client;
      Source_Files : VSS.String_Vectors.Virtual_String_Vector);

   procedure Highlight_Current_File_And_Line
     (Self  : in out DAP_Client;
      File  : GNATCOLL.VFS.Virtual_File;
      Line  : Integer);

   -- Visual_Debugger --

   type Visual_Debugger is new GPS.Debuggers.Base_Visual_Debugger with record
      Client : DAP_Client_Access;
   end record;

   type Visual_Debugger_Access is access all Visual_Debugger;

   overriding function Get_Num
     (Self : not null access Visual_Debugger) return Glib.Gint is
     (Glib.Gint (Self.Client.Id));

   overriding function Command_In_Process
     (Self : not null access Visual_Debugger) return Boolean is
      (Self.Client.Get_Status /= Stopped);

   function Get_Visual
     (Self : in out DAP_Client) return Visual_Debugger_Access;

   procedure Value_Of
     (Self   : in out DAP_Client;
      Entity : String;
      Label  : Gtk.Label.Gtk_Label);

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

   type DAP_Client
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Id     : Positive) is new LSP.Raw_Clients.Raw_Client
   with record
      This         : DAP_Client_Access := DAP_Client'Unchecked_Access;
      Visual       : aliased Visual_Debugger := Visual_Debugger'
        (Glib.Object.GObject_Record with
           Client => DAP_Client'Unchecked_Access);
      Project        : GNATCOLL.Projects.Project_Type;
      File           : GNATCOLL.VFS.Virtual_File;
      Args           : Ada.Strings.Unbounded.Unbounded_String;
      Source_Files   : VSS.String_Vectors.Virtual_String_Vector;

      Is_Attached    : Boolean := False;
      Status         : Debugger_Status_Kind := Initialization;

      Sent           : Requests_Maps.Map;

      Request_Id     : Integer := 1;
      Error_Msg      : VSS.Strings.Virtual_String;

      Stopped_File   : GNATCOLL.VFS.Virtual_File;
      Stopped_Line   : Integer;
      Selected_Frame : Integer;

      --  Modules --
      Breakpoints    : DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager_Access;

      --  Views --
      Breakpoints_View : Generic_Views.Abstract_View_Access;
      Call_Stack_View  : Generic_Views.Abstract_View_Access;
      Thread_View      : Generic_Views.Abstract_View_Access;
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

end DAP.Clients;
