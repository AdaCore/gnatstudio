------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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
--  Abstract language server to manage language server process and associated
--  text documents.

private with Ada.Unchecked_Deallocation;

with GNATCOLL.VFS;

with GPS.LSP_Client.Requests;
with GPS.LSP_Client.Text_Documents;

package GPS.LSP_Client.Language_Servers is

   type Abstract_Language_Server
     (Manager : not null access
        GPS.LSP_Client.Text_Documents.Text_Document_Manager'Class)
   is tagged limited private;

   type Language_Server_Access is access all Abstract_Language_Server'Class;

   procedure Associate
     (Self     : in out Abstract_Language_Server;
      Document :
        not null GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access);
   --  Associate text document with language server.

   procedure Dissociate
     (Self     : in out Abstract_Language_Server;
      Document :
        not null GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access);
   --  Dissociate association of text document and the language server.

   procedure Dissociate_All (Self : in out Abstract_Language_Server'Class);
   --  Dissociate all associated text documents.

   function Text_Document
     (Self : Abstract_Language_Server'Class;
      File : GNATCOLL.VFS.Virtual_File)
      return GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access;
   --  Return text document for given file if it is associated with the
   --  language server and null overwise.

   procedure Execute
     (Self    : in out Abstract_Language_Server;
      Request : in out GPS.LSP_Client.Requests.Request_Access);
   --  Executes request. Memory will be deallocated after execution.

private

   type Abstract_Language_Server
     (Manager : not null access
        GPS.LSP_Client.Text_Documents.Text_Document_Manager'Class) is
   tagged limited record
      Text_Documents :
        GPS.LSP_Client.Text_Documents.Text_Document_Handler_Maps.Map;
      --  Handlers of currently opened text documents.
   end record;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (GPS.LSP_Client.Requests.LSP_Request'Class,
        GPS.LSP_Client.Requests.Request_Access);

end GPS.LSP_Client.Language_Servers;
