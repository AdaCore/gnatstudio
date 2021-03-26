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

with Ada.Containers.Hashed_Maps;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.UTF_Encoding;

private with VSS.Strings;

with LSP.Messages;
with LSP.Server_Notification_Receivers;
with LSP.Raw_Clients;
with LSP.Types; use LSP.Types;

limited with LSP.Clients.Request_Handlers;
limited with LSP.Clients.Response_Handlers;
limited with LSP.Client_Notification_Receivers;

package LSP.DAP_Clients is

   type Client is
     new LSP.Raw_Clients.Raw_Client and
     Server_Notification_Receivers.Server_Notification_Receiver with private;
   --  Client object to send/recieve request and notification to/from
   --  the LSP server

   type Response_Decoder is access procedure
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Request : LSP.Types.LSP_Number_Or_String; Is_Error : Boolean;
      Handler : access LSP.Clients.Response_Handlers.Response_Handler'Class);

   procedure Insert (Self : in out Client;
                    Key : LSP.Types.LSP_Number_Or_String;
                    Elt : Response_Decoder);

   package Decoders is

      --  Responses

      procedure Initialize_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number_Or_String; Is_Error : Boolean;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'
           Class);

      procedure Shutdown_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number_Or_String; Is_Error : Boolean;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'
           Class);

      procedure Text_Document_Code_Action_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number_Or_String; Is_Error : Boolean;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'
           Class);

      procedure Text_Document_Completion_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number_Or_String; Is_Error : Boolean;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'
           Class);

      procedure Text_Document_Definition_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number_Or_String; Is_Error : Boolean;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'
           Class);

      procedure Text_Document_Type_Definition_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number_Or_String; Is_Error : Boolean;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'
           Class);

      procedure Text_Document_Hover_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number_Or_String; Is_Error : Boolean;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'
           Class);

      procedure Text_Document_Folding_Range_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number_Or_String; Is_Error : Boolean;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'
           Class);

      procedure Text_Document_Highlight_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number_Or_String; Is_Error : Boolean;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'
           Class);

      procedure Text_Document_References_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number_Or_String; Is_Error : Boolean;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'
           Class);

      procedure Text_Document_Signature_Help_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number_Or_String; Is_Error : Boolean;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'
           Class);

      procedure Text_Document_Symbol_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number_Or_String; Is_Error : Boolean;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'
           Class);

      procedure Workspace_Execute_Command_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number_Or_String; Is_Error : Boolean;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'
           Class);

      procedure Workspace_Symbol_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number_Or_String; Is_Error : Boolean;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'
           Class);

      --  Notifications

      procedure Show_Message
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Handler : access LSP.Client_Notification_Receivers
           .Client_Notification_Receiver'
           Class);

      procedure Log_Message
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Handler : access LSP.Client_Notification_Receivers
           .Client_Notification_Receiver'
           Class);

      procedure Publish_Diagnostics
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Handler : access LSP.Client_Notification_Receivers
           .Client_Notification_Receiver'
           Class);

      procedure Progress
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Handler : access LSP.Client_Notification_Receivers
           .Client_Notification_Receiver'
           Class);

   end Decoders;

   procedure Initialize (Self : in out Client'Class);
   --  Initialize Client to correct state

   overriding procedure On_Initialized_Notification (Self : access Client);

   overriding procedure On_Exit_Notification (Self : access Client);

   overriding procedure On_DidChangeConfiguration_Notification
     (Self : access Client; Value : LSP.Messages.DidChangeConfigurationParams);

   overriding procedure On_DidChangeWorkspaceFolders_Notification
     (Self  : access Client;
      Value : LSP.Messages.DidChangeWorkspaceFoldersParams);

   overriding procedure On_DidChangeWatchedFiles_Notification
     (Self : access Client; Value : LSP.Messages.DidChangeWatchedFilesParams);

   overriding procedure On_DidOpenTextDocument_Notification
     (Self : access Client; Value : LSP.Messages.DidOpenTextDocumentParams);

   overriding procedure On_DidChangeTextDocument_Notification
     (Self : access Client; Value : LSP.Messages.DidChangeTextDocumentParams);

   overriding procedure On_DidSaveTextDocument_Notification
     (Self : access Client; Value : LSP.Messages.DidSaveTextDocumentParams);

   overriding procedure On_DidCloseTextDocument_Notification
     (Self : access Client; Value : LSP.Messages.DidCloseTextDocumentParams);

   overriding procedure On_Cancel_Notification
     (Self : access Client; Value : LSP.Messages.CancelParams);

   procedure Set_Response_Handler
     (Self  : in out Client'Class;
      Value :    access LSP.Clients.Response_Handlers.Response_Handler'Class);
   --  Set response handler

   procedure Set_Request_Handler
     (Self  : in out Client'Class;
      Value :    access LSP.Clients.Request_Handlers.Request_Handler'Class);
   --  Set request handler

   procedure Set_Notification_Handler
     (Self  : in out Client'Class;
      Value :        access Client_Notification_Receivers
      .Client_Notification_Receiver'
        Class);
   --  Set notification handler

   --  Routines to send request to the LSP server

   procedure Initialize_Request
     (Self : in out Client'Class; Request : out LSP.Types.LSP_Number_Or_String;
      Value :        LSP.Messages.InitializeParams);

   procedure Shutdown_Request
     (Self    : in out Client'Class;
      Request :    out LSP.Types.LSP_Number_Or_String);

   procedure Text_Document_Code_Action_Request
     (Self : in out Client'Class; Request : out LSP.Types.LSP_Number_Or_String;
      Value :        LSP.Messages.CodeActionParams);

   procedure Text_Document_Completion_Request
     (Self : in out Client'Class; Request : out LSP.Types.LSP_Number_Or_String;
      Value :        LSP.Messages.TextDocumentPositionParams);

   procedure Text_Document_Definition_Request
     (Self : in out Client'Class; Request : out LSP.Types.LSP_Number_Or_String;
      Value :        LSP.Messages.DefinitionParams);

   procedure Text_Document_Type_Definition_Request
     (Self : in out Client'Class; Request : out LSP.Types.LSP_Number_Or_String;
      Value :        LSP.Messages.TextDocumentPositionParams);

   procedure Text_Document_Hover_Request
     (Self : in out Client'Class; Request : out LSP.Types.LSP_Number_Or_String;
      Value :        LSP.Messages.TextDocumentPositionParams);

   procedure Text_Document_Folding_Range_Request
     (Self : in out Client'Class; Request : out LSP.Types.LSP_Number_Or_String;
      Value :        LSP.Messages.FoldingRangeParams);

   procedure Text_Document_Highlight_Request
     (Self : in out Client'Class; Request : out LSP.Types.LSP_Number_Or_String;
      Value :        LSP.Messages.TextDocumentPositionParams);

   procedure Text_Document_References_Request
     (Self : in out Client'Class; Request : out LSP.Types.LSP_Number_Or_String;
      Value :        LSP.Messages.ReferenceParams);

   procedure Text_Document_Signature_Help_Request
     (Self : in out Client'Class; Request : out LSP.Types.LSP_Number_Or_String;
      Value :        LSP.Messages.TextDocumentPositionParams);

   procedure Text_Document_Symbol_Request
     (Self : in out Client'Class; Request : out LSP.Types.LSP_Number_Or_String;
      Value :        LSP.Messages.DocumentSymbolParams);

   procedure Workspace_Execute_Command_Request
     (Self : in out Client'Class; Request : out LSP.Types.LSP_Number_Or_String;
      Value :        LSP.Messages.ExecuteCommandParams);

   procedure Workspace_Symbol_Request
     (Self : in out Client'Class; Request : out LSP.Types.LSP_Number_Or_String;
      Value :        LSP.Messages.WorkspaceSymbolParams);

   --  Send response to the LSP server

   procedure Workspace_Apply_Edit
     (Self    : in out Client'Class; Request : LSP.Types.LSP_Number_Or_String;
      Failure :        LSP.Types.Optional_String);

   procedure Void_Response
     (Self : in out Client'Class; Request : LSP.Types.LSP_Number_Or_String);
   --  Return a void response to server->client request

   function Allocate_Request_Id
     (Self : in out Client'Class) return LSP.Types.LSP_Number_Or_String;
   --  Allocates request id.

   function Request_Id_Prefix (Self : Client) return LSP.Types.LSP_String;
   --  Prefix to generate request id in form "prefix-id".

private

   package Request_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LSP.Types.LSP_Number_Or_String,
      Element_Type    => Response_Decoder, Hash => LSP.Types.Hash,
      Equivalent_Keys => "=");

   type Notification_Decoder is access procedure
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : access LSP.Client_Notification_Receivers
      .Client_Notification_Receiver'
        Class);

   package Notification_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type => Notification_Decoder, Hash => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

   type Client is new LSP.Raw_Clients.Raw_Client and
     Server_Notification_Receivers.Server_Notification_Receiver with record
      Request_Id       : LSP.Types.LSP_Number := 0;  --  Id of prev request
      Request_Map      : Request_Maps.Map;  --  issued requests
      Notif_Decoders   : Notification_Maps.Map;  --  notification decoders
      Response_Handler : access LSP.Clients.Response_Handlers.Response_Handler'
        Class;
      Request_Handler : access LSP.Clients.Request_Handlers.Request_Handler'
        Class;
      Notification : access LSP.Client_Notification_Receivers
        .Client_Notification_Receiver'
          Class;
      Error_Message : VSS.Strings.Virtual_String;
   end record;

   overriding procedure On_Raw_Message
     (Self    : in out Client;
      Data : Ada.Strings.Unbounded.Unbounded_String;
      Success : in out Boolean);

   overriding function Error_Message
     (Self : Client) return VSS.Strings.Virtual_String;

   procedure Send_Notification
     (Self   : in out Client'Class;
      Method :        Ada.Strings.UTF_Encoding.UTF_8_String;
      Value  : in out LSP.Messages.NotificationMessage'Class);

   procedure Send_Request
     (Self : in out Client'Class;
      Request : out LSP.Types.LSP_Number_Or_String;
      Method  :        Ada.Strings.UTF_Encoding.UTF_8_String;
      Decoder :        Response_Decoder;
      Value   : in out LSP.Messages.RequestMessage'Class);

   procedure Send_Response
     (Self  : in out Client'Class;
      Request : LSP.Types.LSP_Number_Or_String;
      Value : in out LSP.Messages.ResponseMessage'Class);

end LSP.DAP_Clients;
