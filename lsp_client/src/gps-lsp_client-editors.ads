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
--  Integration with GPS's source editor

private with Ada.Containers.Indefinite_Holders;

with GNATCOLL.VFS;

with LSP.Messages;

with GPS.Editors;
with GPS.Kernel;
with GPS.LSP_Client.Text_Document_Handlers;

package GPS.LSP_Client.Editors is

   type Src_Editor_Handler
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   limited new GPS.LSP_Client.Text_Document_Handlers.Text_Document_Handler
     and GPS.Editors.Editor_Listener with private;

   type Src_Editor_Handler_Access is access all Src_Editor_Handler'Class;

   procedure Initialize
     (Self   : in out Src_Editor_Handler'Class;
      Buffer : GPS.Editors.Editor_Buffer'Class);
   --  Initialize handler and register it in the module.

private

   package Editor_Buffer_Holders is
     new Ada.Containers.Indefinite_Holders
       (GPS.Editors.Editor_Buffer'Class, GPS.Editors."=");

   type Src_Editor_Handler
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   limited new GPS.LSP_Client.Text_Document_Handlers.Text_Document_Handler
     and GPS.Editors.Editor_Listener
   with record
      Server :
        GPS.LSP_Client.Text_Document_Handlers
          .Text_Document_Server_Proxy_Access;
      Buffer : Editor_Buffer_Holders.Holder;
   end record;

   overriding function File
     (Self : Src_Editor_Handler) return GNATCOLL.VFS.Virtual_File;
   --  Returns name of the file processed by this handler.

   overriding function Get_Did_Change_Message
     (Self : in out Src_Editor_Handler)
      return LSP.Messages.DidChangeTextDocumentParams;
   --  Returns message to be send to the server. Called by server manager
   --  when it is ready to send update to the server.

   overriding procedure Set_Sync_Kind
     (Self : in out Src_Editor_Handler;
      To   :
        GPS.LSP_Client.Text_Document_Handlers.Text_Document_Sync_Kinds);
   --  Set text synchronization mode requested by the server. May be changed
   --  dynamically by server. Implementation must be ready to such change.

   overriding procedure Set_Server
     (Self   : in out Src_Editor_Handler;
      Server :
      GPS.LSP_Client.Text_Document_Handlers.Text_Document_Server_Proxy_Access);
   --  Sets new server proxy to be used

   overriding procedure Before_Insert_Text
     (Self      : in out Src_Editor_Handler;
      Location  : GPS.Editors.Editor_Location'Class;
      Text      : String := "";
      From_User : Boolean);

   overriding procedure Before_Delete_Range
     (Self           : in out Src_Editor_Handler;
      Start_Location : GPS.Editors.Editor_Location'Class;
      End_Location   : GPS.Editors.Editor_Location'Class;
      Offset         : Integer;
      From_User      : Boolean);

   overriding procedure Finalize (Self : in out Src_Editor_Handler);

end GPS.LSP_Client.Editors;
