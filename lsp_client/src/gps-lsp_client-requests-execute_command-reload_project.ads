------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020-2023, AdaCore                   --
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

package GPS.LSP_Client.Requests.Execute_Command.Reload_Project is

   type Reload_Project_Command_Request is new Abstract_Execute_Command_Request
     with null record;

   overriding procedure On_Result_Message
     (Self : in out Reload_Project_Command_Request) is null;

   overriding function Command_Name
     (Self : Reload_Project_Command_Request)
        return VSS.Strings.Virtual_String is ("als-reload-project");

   overriding function Params
     (Self : Reload_Project_Command_Request)
      return LSP.Messages.ExecuteCommandParams is
        (Is_Unknown => True,
         command    => Self.Command_Name,
         arguments  => (Is_Set => True, Value => <>),
         others => <>);
   --  Return parameters of the request to be sent to the server.

end GPS.LSP_Client.Requests.Execute_Command.Reload_Project;
