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

--  "disconnect" request

with DAP.Tools;

package DAP.Requests.Disconnect is

   type Disconnect_DAP_Request is abstract new DAP_Request with record
      Parameters : aliased DAP.Tools.DisconnectRequest :=
        DAP.Tools.DisconnectRequest'
          (seq       => 0,
           arguments =>
             (Is_Set => True,
              Value  =>
                (restart           => False,
                 terminateDebuggee => True,
                 suspendDebuggee   => False)));
   end record;

   type Disconnect_DAP_Request_Access is
     access all Disconnect_DAP_Request;

   overriding procedure Write
     (Self   : Disconnect_DAP_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding procedure On_Result_Message
     (Self        : in out Disconnect_DAP_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out DAP_Request_Access);

   procedure On_Result_Message
     (Self        : in out Disconnect_DAP_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.DisconnectResponse;
      New_Request : in out DAP_Request_Access) is abstract;

   overriding procedure Set_Seq
     (Self : in out Disconnect_DAP_Request;
      Id   : Integer);

   overriding function Method
     (Self : in out Disconnect_DAP_Request)
      return String is ("disconnect");

end DAP.Requests.Disconnect;
