------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2021-2023, AdaCore                  --
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

--  This package implements supports for code action requests in the editors

with VSS.Strings.Conversions;

with GNATCOLL.VFS;  use GNATCOLL.VFS;
with GPS.Kernel;    use GPS.Kernel;

with LSP.Messages;
with GPS.LSP_Client.Requests; use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Requests.Execute_Command;

package GPS.LSP_Client.Editors.Code_Actions is

   procedure Request_Code_Action
     (Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Initiate a code action request at the location marked by the current
   --  context.

   -----------------------------
   -- Execute_Command_Request --
   -----------------------------

   --  This part handles the emission of the textDocument/codeAction request
   --  and the processing of its results.

   type Execute_Command_Request is new
     GPS.LSP_Client.Requests.Execute_Command.Abstract_Execute_Command_Request
   with record
      Params : LSP.Messages.ExecuteCommandParams (True);
   end record;
   type Execute_Command_Request_Access is
     access all Execute_Command_Request'Class;

   overriding function Params
     (Self : Execute_Command_Request)
      return LSP.Messages.ExecuteCommandParams is (Self.Params);

   overriding procedure On_Result_Message
     (Self : in out Execute_Command_Request) is null;

   overriding function Get_Task_Label
     (Self : Execute_Command_Request) return String
   is
     (VSS.Strings.Conversions.To_UTF_8_String (Self.Params.command));

   overriding function Command_Name
     (Self : Execute_Command_Request)
        return VSS.Strings.Virtual_String is (Self.Params.command);

end GPS.LSP_Client.Editors.Code_Actions;
