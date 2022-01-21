------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

package body DAP.Requests is

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out DAP_Request_Access) is
      procedure Free is
        new Ada.Unchecked_Deallocation (DAP_Request'Class, DAP_Request_Access);

   begin
      if Item /= null then
         Item.Finalize;
         Free (Item);
         Item := null;
      end if;
   end Destroy;

   ----------------
   -- Set_Client --
   ----------------

   procedure Set_Client
     (Self   : in out DAP_Request;
      Client : access DAP.Clients.DAP_Client'Class) is
   begin
      Self.Client := Client;
   end Set_Client;

end DAP.Requests;
