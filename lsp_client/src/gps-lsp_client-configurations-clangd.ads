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

with LSP.Messages;

package GPS.LSP_Client.Configurations.Clangd is

   type Clangd_Configuration is new Server_Configuration with private;

   overriding procedure Prepare_Configuration_Settings
     (Self : in out Clangd_Configuration);

   procedure On_Server_Capabilities
     (Capabilities : in out LSP.Messages.ServerCapabilities);

private

   type Clangd_Configuration is new Server_Configuration with null record;

end GPS.LSP_Client.Configurations.Clangd;
