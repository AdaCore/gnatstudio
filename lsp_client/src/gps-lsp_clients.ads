------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;

with GNATCOLL.Arg_Lists;
with GNATCOLL.VFS;

with GPS.Kernel;
with GPS.LSP_Client.Text_Documents;

with LSP.Clients.Response_Handlers;
with LSP.Clients;
with LSP.Messages;
with LSP.Types;

package GPS.LSP_Clients is

   type LSP_Client
     (Kernel  : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Manager : not null access
        GPS.LSP_Client.Text_Documents.Text_Document_Manager'Class)
   is limited new LSP.Clients.Client
     and GPS.LSP_Client.Text_Documents.Text_Document_Server_Proxy
   with private;
   --  Client represents a connect to LSP server for some language

   type LSP_Client_Access is access all LSP_Client;

   procedure Start
     (Self : aliased in out LSP_Client;
      Cmd  : GNATCOLL.Arg_Lists.Arg_List);
   --  Use given command line to start LSP server

   procedure Associate
     (Self     : in out LSP_Client'Class;
      Document :
        not null GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access);
   --  Associate text document with language server proxy. Set_Server
   --  supbrogram of the text document will be called immediately if the
   --  server is up. Otherwise, it will be called later when server will be
   --  up.

   procedure Dissociate
     (Self     : in out LSP_Client'Class;
      Document :
        not null GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access);
   --  Dissociate association of text document and server proxy. Set_Server
   --  with null value will be called on text document before dissociation if
   --  server was set for text document.

   procedure Dissociate_All (Self : in out LSP_Client'Class);
   --  Dissociate all associated text documents.

   function Text_Document
     (Self : LSP_Client'Class;
      File : GNATCOLL.VFS.Virtual_File)
      return GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access;
   --  Return text document for given file if it is associated with the
   --  client and null overwise.

private

   type Response_Handler (Client : access LSP_Client) is
     new LSP.Clients.Response_Handlers.Response_Handler with null record;

   overriding procedure Initialize_Response
     (Self     : not null access Response_Handler;
      Request  : LSP.Types.LSP_Number;
      Response : LSP.Messages.Initialize_Response);

   type Command_Kinds is (Open_File, Changed_File, Close_File);

   type Command (Kind : Command_Kinds := Command_Kinds'First) is record
      case Kind is
         when Open_File | Changed_File =>
            Handler :
              GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access;

         when Close_File =>
            File : GNATCOLL.VFS.Virtual_File;
      end case;
   end record;

   package Command_Lists is new Ada.Containers.Doubly_Linked_Lists (Command);
   --  Until the server has responded to the initialize request, the client
   --  must not send any additional requests or notifications to the server.

   type LSP_Client
     (Kernel  : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Manager : not null access
        GPS.LSP_Client.Text_Documents.Text_Document_Manager'Class) is
   limited new LSP.Clients.Client
     and GPS.LSP_Client.Text_Documents.Text_Document_Server_Proxy
   with record
      Is_Ready                      : Boolean := False;
      --  If server is initialized
      Response_Handler              : aliased LSP_Clients.Response_Handler
        (LSP_Client'Unchecked_Access);
      Commands                      : Command_Lists.List;
      --  Command Queue
      Server_Capabilities           : LSP.Messages.ServerCapabilities;

      Text_Document_Handlers        :
        GPS.LSP_Client.Text_Documents.Text_Document_Handler_Maps.Map;
      --  Handlers of currently opened text documents.

      Text_Document_Synchronization :
        GPS.LSP_Client.Text_Documents.Text_Document_Sync_Kind_Type;
      --  Current mode of text synchronization.
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

   overriding procedure On_Started (Self : in out LSP_Client);
   --  Send initialization request on successful startup of the language
   --  server process.

   -------------------------------------------
   -- Methods of Text_Document_Server_Proxy --
   -------------------------------------------

   overriding procedure Send_Text_Document_Did_Open
     (Self     : in out LSP_Client;
      Document : not null
        GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access);
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
     (Self     : in out LSP_Client;
      Document : not null
        GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access);
   --  Request to send a DidCloseTextDocument notification. When request to
   --  send DidChangeTextDocument notification is in queue for given document
   --  content of DidChangeTextDocument notification message has been
   --  requested, stored and sent before DidCloseTextDocument notification.

end GPS.LSP_Clients;
