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

with Ada.Unchecked_Deallocation;

with GPS.LSP_Client.Language_Servers;
with GPS.LSP_Module;

package body GPS.LSP_Client.Requests is

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out Request_Access) is
      procedure Free is
        new Ada.Unchecked_Deallocation (LSP_Request'Class, Request_Access);

   begin
      if Item /= null then
         Item.Finalize;
         Free (Item);
      end if;
   end Destroy;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Language : not null Standard.Language.Language_Access;
      Request  : in out Request_Access)
   is
      use type GPS.LSP_Client.Language_Servers.Language_Server_Access;

      Server : constant
        GPS.LSP_Client.Language_Servers.Language_Server_Access :=
          GPS.LSP_Module.Get_Language_Server (Language);

   begin
      if Server = null then
         --  Reject the request when there is no language server configured

         Request.On_Rejected;
         Destroy (Request);

         return;

      else
         Server.Execute (Request);
      end if;
   end Execute;

end GPS.LSP_Client.Requests;
