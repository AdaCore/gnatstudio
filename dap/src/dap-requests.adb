------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with GPS.DAP_Client.Requests;

package body DAP.Requests is

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out DAP_Request_Access) is
   begin
      GPS.DAP_Client.Requests.Destroy
        (GPS.DAP_Client.Requests.Request_Access (Item));
      Item := null;
   end Destroy;

   -----------------
   -- On_Rejected --
   -----------------

   procedure On_Rejected
     (Self   : in out DAP_Request;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      GPS.DAP_Client.Requests.On_Rejected
        (GPS.DAP_Client.Requests.Request'Class (Self));
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   procedure On_Error_Message
     (Self    : in out DAP_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      GPS.DAP_Client.Requests.On_Error_Message
        (GPS.DAP_Client.Requests.Request'Class (Self), Message);
   end On_Error_Message;

end DAP.Requests;
