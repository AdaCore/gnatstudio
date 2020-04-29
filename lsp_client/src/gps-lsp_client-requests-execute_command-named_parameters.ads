------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with GNATCOLL.Projects;

with Basic_Types;

package GPS.LSP_Client.Requests.Execute_Command.Named_Parameters is

   type Abstract_Named_Parameters_Command_Request is
     abstract new Abstract_Execute_Command_Request with record
      Project : GNATCOLL.Projects.Project_Type;
      Line    : Positive;
      Column  : Basic_Types.Visible_Column_Type;
   end record;

   overriding
   function Params
     (Self : Abstract_Named_Parameters_Command_Request)
      return LSP.Messages.ExecuteCommandParams;
   --  Return parameters of the request to be sent to the server.

end GPS.LSP_Client.Requests.Execute_Command.Named_Parameters;
