------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with Ada.Exceptions;

with GNATCOLL.VFS; use GNATCOLL.VFS;

private with VSS.Strings;

with Glib.Main;

with GPS.Kernel;
with GPS.LSP_Client.Requests;
with GPS.LSP_Client.Text_Documents;

with LSP.Clients.Response_Handlers;
with LSP.Clients.Request_Handlers;
with LSP.Clients;
with LSP.Messages.Server_Responses;
with LSP.Types;
with Spawn.String_Vectors;
with Language; use Language;

package GPS.LSP_Clients is

   type LSP_Client_Listener is limited interface;

   type LSP_Client_Listener_Access is access all LSP_Client_Listener'Class;

   -------------------------
   -- LSP_Client_Listener --
   -------------------------

   procedure On_Server_Started (Self : in out LSP_Client_Listener) is null;
   --  Called when server has been started, initialized and configured, and
   --  ready to process requests/notifications.

   procedure On_Server_Stopped (Self : in out LSP_Client_Listener) is null;
   --  Called when server stopped for any reason.

   procedure On_Response_Processed
     (Self   : in out LSP_Client_Listener;
      Data   : Ada.Strings.Unbounded.Unbounded_String;
      Method : Ada.Strings.Unbounded.Unbounded_String) is null;
   --  Called when response messages has been processed.

   procedure On_Response_Sent
     (Self : in out LSP_Client_Listener;
      Data : Ada.Strings.Unbounded.Unbounded_String) is null;
   --  Called when response messages has been sent.

   procedure On_Send_Request
     (Self    : in out LSP_Client_Listener;
      Request : GPS.LSP_Client.Requests.Request_Access) is null;

   procedure On_Send_Cancel
     (Self    : in out LSP_Client_Listener;
      Request : GPS.LSP_Client.Requests.Request_Access) is null;

   procedure On_Receive_Reply
     (Self    : in out LSP_Client_Listener;
      Request : GPS.LSP_Client.Requests.Request_Access) is null;

   procedure On_Reject_Request
     (Self    : in out LSP_Client_Listener;
      Request : GPS.LSP_Client.Requests.Request_Access) is null;

   ----------------
   -- LSP_Client --
   ----------------

   type LSP_Client
     (Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Listener : not null access LSP_Client_Listener'Class;
      Language : not null access Language_Root'Class)
   is limited new LSP.Clients.Client
     and GPS.LSP_Client.Text_Documents.Text_Document_Server_Proxy
   with private;
   --  Client represents a connect to LSP server for some language

   type LSP_Client_Access is access all LSP_Client'Class;

   procedure Start
     (Self       : aliased in out LSP_Client;
      Executable : String;
      Arguments  : Spawn.String_Vectors.UTF_8_String_Vector);
   --  Use given command line to start LSP server

   procedure Stop
     (Self               : in out LSP_Client'Class;
      Reject_Immediately : Boolean);
   --  Shutdown the language server. When Reject_Immediately is True all
   --  ongoing and queued requests will be rejected immediately. It is
   --  necessary to avoid possible crashes due to dangling cursors at
   --  GNAT Studio exit.

   procedure Restart (Self : in out LSP_Client'Class);
   --  Restart the language server executable

   function Is_Ready (Self : LSP_Client'Class) return Boolean;
   --  Return True when language server is running.

   procedure Enqueue
     (Self    : in out LSP_Client'Class;
      Request : in out GPS.LSP_Client.Requests.Request_Access);
   --  Adds request to the queue. If server has not been started yet,
   --  On_Reject will be called immediately and request will be not enqueued.

   procedure Cancel
     (Self    : in out LSP_Client'Class;
      Request : in out GPS.LSP_Client.Requests.Request_Access);
   --  Cancel given request.

   function Get_Running_Request
     (Self       : LSP_Client'Class;
      Request_Id : LSP.Types.LSP_Number_Or_String)
      return GPS.LSP_Client.Requests.Request_Access;
   --  If a request with the given Id is currently running, return it.
   --  Return null otherwise.
   --  Clients must NOT free the result: the request remains owned by
   --  the server.

   function Capabilities
     (Self : LSP_Client'Class) return LSP.Messages.ServerCapabilities;

   type On_Server_Capabilities_Proc is
     access procedure (Capabilities : in out LSP.Messages.ServerCapabilities);

   procedure Set_On_Server_Capabilities
     (Self : in out LSP_Client'Class;
      Proc : On_Server_Capabilities_Proc);
   --  Set callback which will be called when server has returned
   --  its capabilities. Used for adjusting capabilities, for example when
   --  server has limitations which does not allow us to use some requests.

   procedure Set_Standard_Errors_File
     (Self : in out LSP_Client'Class;
      File : Virtual_File);
   --  Set file for redirecting standard errors to it.

private

   package Request_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LSP.Types.LSP_Number_Or_String,
      Element_Type    => GPS.LSP_Client.Requests.Request_Access,
      Hash            => LSP.Types.Hash,
      Equivalent_Keys => LSP.Types."=",
      "="             => GPS.LSP_Client.Requests."=");

   package Request_Id_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => LSP.Types.LSP_Number_Or_String,
      Hash                => LSP.Types.Hash,
      Equivalent_Elements => LSP.Types."=",
      "="                 => LSP.Types."=");

   type Response_Handler (Client : access LSP_Client) is
     new LSP.Clients.Response_Handlers.Response_Handler with null record;

   overriding procedure Initialize_Response
     (Self     : not null access Response_Handler;
      Request  : LSP.Types.LSP_Number_Or_String;
      Response : LSP.Messages.Server_Responses.Initialize_Response);

   type Request_Handler (Client : access LSP_Client) is
     new LSP.Clients.Request_Handlers.Request_Handler with null record;

   overriding procedure Workspace_Apply_Edit
     (Self    : not null access Request_Handler;
      Request : LSP.Types.LSP_Number_Or_String;
      Params  : LSP.Messages.ApplyWorkspaceEditParams);

   overriding procedure Window_Work_Done_Progress_Create
     (Self    : not null access Request_Handler;
      Request : LSP.Types.LSP_Number_Or_String;
      Params  : LSP.Messages.WorkDoneProgressCreateParams);

   type Command_Kinds is
     (Open_File,
      Changed_File,
      Close_File,
      GPS_Request,
      Cancel_GPS_Request);

   type Command (Kind : Command_Kinds := Command_Kinds'First) is record
      case Kind is
         when Changed_File =>
            Handler :
              GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access;

         when Close_File | Open_File =>
            File : GNATCOLL.VFS.Virtual_File;

         when GPS_Request =>
            Request : not null GPS.LSP_Client.Requests.Request_Access;

         when Cancel_GPS_Request =>
            Id : LSP.Types.LSP_Number_Or_String;
      end case;
   end record;

   package Command_Lists is new Ada.Containers.Doubly_Linked_Lists (Command);
   --  Until the server has responded to the initialize request, the client
   --  must not send any additional requests or notifications to the server.

   package Time_List is new Ada.Containers.Doubly_Linked_Lists
     (Ada.Calendar.Time);

   type LSP_Client
     (Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Listener : not null access LSP_Client_Listener'Class;
      Language : not null access Language_Root'Class) is
   limited new LSP.Clients.Client
     and GPS.LSP_Client.Text_Documents.Text_Document_Server_Proxy
   with record
      Is_Ready                      : Boolean := False;
      --  If server is initialized

      Exiting                       : Boolean := False;
      --  Set to True after send of "exit" notification to prevent processing
      --  of potential subsequent error notification due to later close of
      --  file descriptors on Windows

      Shutdown_Intentionally_Requested : Boolean := False;
      --  This flag indicates that the shutdown of the subprocess was
      --  required by the application - we use this to prevent automatic
      --  relaunches of the server.

      Launches : Time_List.List;
      --  The times at which the server was launched or relaunched,
      --  ordered from most recent to oldest.

      Response_Handler              : aliased LSP_Clients.Response_Handler
        (LSP_Client'Unchecked_Access);
      Request_Handler              : aliased LSP_Clients.Request_Handler
        (LSP_Client'Unchecked_Access);

      Commands                      : Command_Lists.List;
      --  Command Queue
      Server_Capabilities           : LSP.Messages.ServerCapabilities;
      On_Server_Capabilities        : On_Server_Capabilities_Proc := null;

      Requests                      : Request_Maps.Map;
      --  Map from sent request's ids to request objects to handle response.

      Canceled_Requests             : Request_Id_Sets.Set;
      --  Set of canceled requests for which reply message is still expected.

      Text_Document_Synchronization :
        GPS.LSP_Client.Text_Documents.Text_Document_Sync_Kind_Type;
      --  Current mode of text synchronization.

      Standard_Errors_File : Virtual_File;
      Errors_Writable_File : GNATCOLL.VFS.Writable_File;
      --  For redirecting standard errors to the file

      Restart_Timer                  : Glib.Main.G_Source_Id :=
                                         Glib.Main.No_Source_Id;
      --  Timer to postpone restart of the language server to allow to process
      --  all notifications for currently shutting down language server
      --  process.
   end record;

   procedure Process_Command
     (Self : in out LSP_Client'Class;
      Item : Command);
   --  Executes given command.

   procedure Enqueue
     (Self : in out LSP_Client'Class;
      Item : Command);
   --  Put given command into the queue.

   overriding procedure On_Error (Self : in out LSP_Client; Error : String);

   overriding procedure On_Standard_Error_Message
     (Self : in out LSP_Client; Text : String);

   overriding procedure On_Started (Self : in out LSP_Client);
   --  Send initialization request on successful startup of the language
   --  server process.

   overriding procedure On_Finished (Self : in out LSP_Client);
   --  Handle termination of the language server process. If this wasn't
   --  expected and we're within the acceptable throttling limits, relaunch.

   overriding procedure On_Raw_Message
     (Self    : in out LSP_Client;
      Data    : Ada.Strings.Unbounded.Unbounded_String;
      Success : in out Boolean);

   overriding procedure On_Exception
     (Self       : in out LSP_Client;
      Occurrence : Ada.Exceptions.Exception_Occurrence);

   overriding procedure On_Exit_Notification (Self : access LSP_Client);

   overriding function Request_Id_Prefix
     (Self : LSP_Client) return VSS.Strings.Virtual_String;
   --  Return unique prefix to generate request id.

   -------------------------------------------
   -- Methods of Text_Document_Server_Proxy --
   -------------------------------------------

   overriding procedure Send_Text_Document_Did_Open
     (Self : in out LSP_Client;
      File : GNATCOLL.VFS.Virtual_File);
   --  Request to send a DidOpenTextDocument notification to LSP server.

   overriding procedure Send_Text_Document_Did_Change
     (Self     : in out LSP_Client;
      Document : not null
        GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access);
   --  Request to send a DidChangeTextDocument notification. Note, given text
   --  document handler is used to obtain content of DidChangeTextDocument
   --  notification message to be sent asynchronously when server proxy is
   --  ready to sent it, or on request to sent DidCloseTextDocument
   --  notification message (in this case returned message will be stored
   --  internally).

   overriding procedure Send_Text_Document_Did_Close
     (Self : in out LSP_Client;
      File : GNATCOLL.VFS.Virtual_File);
   --  Request to send a DidCloseTextDocument notification. When request to
   --  send DidChangeTextDocument notification is in queue for given document
   --  content of DidChangeTextDocument notification message has been
   --  requested, stored and sent before DidCloseTextDocument notification.

end GPS.LSP_Clients;
