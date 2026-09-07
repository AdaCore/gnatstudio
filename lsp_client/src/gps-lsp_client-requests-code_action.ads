------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020-2026, AdaCore                   --
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

with GNATCOLL.VFS;

with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;

package GPS.LSP_Client.Requests.Code_Action is

   type Abstract_Code_Action_Request is abstract new LSP_Request with record
      Text_Document    : GNATCOLL.VFS.Virtual_File;
      Start_Position   : LSP.Structures.Position;
      End_Position     : LSP.Structures.Position;
      Document_Version : Integer;
   end record;

   function Params
     (Self : Abstract_Code_Action_Request)
      return LSP.Structures.CodeActionParams;
   --  Return parameters of the request to be sent to the server.

   procedure On_Result_Message
     (Self   : in out Abstract_Code_Action_Request;
      Result : LSP.Structures.Command_Or_CodeAction_Vector)
   is abstract;
   --  Called when a result response is received from the server. The
   --  response may contain either Command or CodeAction entries; passed
   --  through unnormalized.

   overriding
   function Method
     (Self : Abstract_Code_Action_Request) return VSS.Strings.Virtual_String;

   overriding
   procedure Params
     (Self    : Abstract_Code_Action_Request;
      Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding
   procedure On_Result_Message
     (Self    : in out Abstract_Code_Action_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class);

end GPS.LSP_Client.Requests.Code_Action;
