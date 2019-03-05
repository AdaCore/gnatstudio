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
--  Interface of the text document handler to integrate text editing
--  capabilities.

with GNATCOLL.VFS;

with LSP.Messages;

package GPS.LSP_Client.Text_Document_Handlers is

   type Text_Document_Sync_Kinds is (Full, Incremental);

   type Text_Document_Handler is limited interface;

   type Text_Document_Handler_Access is access all Text_Document_Handler'Class;

   not overriding procedure Finalize
     (Self : in out Text_Document_Handler) is null;
   --  Called before deallocation of text document handler to allow to release
   --  all used resources.

   not overriding function File
     (Self : Text_Document_Handler)
      return GNATCOLL.VFS.Virtual_File is abstract;
   --  Returns name of the file processed by this handler.

   not overriding function Get_Did_Change_Message
     (Self : in out Text_Document_Handler)
      return LSP.Messages.DidChangeTextDocumentParams is abstract;
   --  Returns message to be send to the server. Called by server manager
   --  when it is ready to send update to the server.

   not overriding procedure Set_Sync_Kind
     (Self : in out Text_Document_Handler;
      To   : Text_Document_Sync_Kinds) is abstract;
   --  Set text synchronization mode requested by the server. May be changed
   --  dynamically by server. Implementation must be ready to such change.

end GPS.LSP_Client.Text_Document_Handlers;
