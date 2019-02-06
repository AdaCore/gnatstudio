------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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
--  Interface of the text document handler and server proxy to integrate text
--  editing capabilities.

with GNATCOLL.VFS;

with LSP.Messages;

package GPS.LSP_Client.Text_Document_Handlers is

   type Text_Document_Sync_Kinds is (Full, Incremental);

   type Text_Document_Handler is limited interface;

   type Text_Document_Handler_Access is access all Text_Document_Handler'Class;

   type Text_Document_Server_Proxy is limited interface;

   type Text_Document_Server_Proxy_Access is
     access all Text_Document_Server_Proxy'Class;

   ---------------------------
   -- Text_Document_Handler --
   ---------------------------

   procedure Finalize (Self : in out Text_Document_Handler) is null;
   --  Called before deallocation of text document handler to allow to release
   --  all used resources.

   function File
     (Self : Text_Document_Handler)
      return GNATCOLL.VFS.Virtual_File is abstract;
   --  Returns name of the file processed by this handler.

   function Get_Did_Change_Message
     (Self : in out Text_Document_Handler)
      return LSP.Messages.DidChangeTextDocumentParams is abstract;
   --  Returns message to be send to the server. Called by server manager
   --  when it is ready to send update to the server.

   procedure Set_Sync_Kind
     (Self : in out Text_Document_Handler;
      To   : Text_Document_Sync_Kinds) is abstract;
   --  Set text synchronization mode requested by the server. May be changed
   --  dynamically by server. Implementation must be ready to such change.

   procedure Set_Server
     (Self   : in out Text_Document_Handler;
      Server : Text_Document_Server_Proxy_Access) is abstract;
   --  Set server proxy object to be used to send requests/notifictions to the
   --  server. When Server is null it means that where is no server right now.
   --  Server may be changed dynamically, in this case handler is responsible
   --  send all necessary modification notifications, document close
   --  notification and to send document open notification to the new server
   --  as well as other necessary notifications.

   --------------------------------
   -- Text_Document_Server_Proxy --
   --------------------------------

   procedure Text_Document_Did_Open
     (Self     : in out Text_Document_Server_Proxy;
      Document : not null Text_Document_Handler_Access) is abstract;
   --  Send text document did open notification. Implementation can save
   --  reference to Document for internal use.

   procedure Text_Document_Did_Close
     (Self     : in out Text_Document_Server_Proxy;
      Document : not null Text_Document_Handler_Access) is abstract;
   --  Send text document did close notification. Implementation can call
   --  Get_Did_Change_Message on Document if necessary during execution of
   --  this subprogram.

end GPS.LSP_Client.Text_Document_Handlers;
