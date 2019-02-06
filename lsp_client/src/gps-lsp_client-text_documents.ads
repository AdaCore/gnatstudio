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

with Ada.Containers.Hashed_Maps;

with GNATCOLL.VFS;

with LSP.Messages;

package GPS.LSP_Client.Text_Documents is

   type Text_Document_Sync_Kind_Type is (Full, Incremental);

   type Text_Document_Manager is limited interface;

   type Text_Document_Handler is limited interface;

   type Text_Document_Handler_Access is access all Text_Document_Handler'Class;

   type Text_Document_Server_Proxy is limited interface;

   type Text_Document_Server_Proxy_Access is
     access all Text_Document_Server_Proxy'Class;

   ---------------------------
   -- Text_Document_Handler --
   ---------------------------

   procedure Destroy (Self : in out Text_Document_Handler) is null;
   --  Called during destruction of LSP module to allow to release all used
   --  resources.

   function File
     (Self : Text_Document_Handler)
      return GNATCOLL.VFS.Virtual_File is abstract;
   --  Returns name of the file processed by this handler.

   function Get_Did_Change_Message
     (Self : in out Text_Document_Handler;
      Mode : Text_Document_Sync_Kind_Type)
      return LSP.Messages.DidChangeTextDocumentParams is abstract;
   --  Returns message to be send to the server. Called by server manager
   --  when it is ready to send update to the server. Mode is active text
   --  synchronization mode.

   procedure Set_Server
     (Self   : in out Text_Document_Handler;
      Server : Text_Document_Server_Proxy_Access) is abstract;
   --  Set server proxy object to be used to send requests/notifictions to the
   --  server. When Server is null it means that there is no server right now.
   --  Server may be changed dynamically, in this case handler is responsible
   --  send all necessary modification notifications, DidCloseTextDocument
   --  notification and to send DidOpenTextDocument notification to the new
   --  server as well as other necessary notifications.

   --------------------------------
   -- Text_Document_Server_Proxy --
   --------------------------------

   procedure Send_Text_Document_Did_Open
     (Self     : in out Text_Document_Server_Proxy;
      Document : not null Text_Document_Handler_Access) is abstract;
   --  Send DidOpenTextDocument notification. Implementation can save
   --  reference to Document for internal use.

   procedure Send_Text_Document_Did_Change
     (Self     : in out Text_Document_Server_Proxy;
      Document : not null Text_Document_Handler_Access) is abstract;
   --  Send DidChangeTextDocument notification. Implementation should
   --  get actual modification changes with Get_Did_Change_Message on
   --  Document when ready to deliver information to server.

   procedure Send_Text_Document_Did_Close
     (Self     : in out Text_Document_Server_Proxy;
      Document : not null Text_Document_Handler_Access) is abstract;
   --  Send text DidCloseDocument notification. Implementation can call
   --  Get_Did_Change_Message on Document if necessary during execution of
   --  this subprogram.

   ---------------------------
   -- Text_Document_Manager --
   ---------------------------

   procedure Register
     (Self     : in out Text_Document_Manager;
      Document : not null Text_Document_Handler_Access) is abstract;
   --  Register new text document handler.

   procedure Unregister
     (Self     : in out Text_Document_Manager;
      Document : not null Text_Document_Handler_Access) is abstract;
   --  Unregister text document handler.

   package Text_Document_Handler_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => GNATCOLL.VFS.Virtual_File,
        Element_Type    => Text_Document_Handler_Access,
        Hash            => GNATCOLL.VFS.Full_Name_Hash,
        Equivalent_Keys => GNATCOLL.VFS."=");

end GPS.LSP_Client.Text_Documents;
