------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

package body DAP.Clients.Disconnect is

   ------------
   -- Create --
   ------------

   function Create
     (Kernel             : not null Kernel_Handle;
      Terminate_Debuggee : Boolean)
      return Disconnect_Request_Access
   is
      Self : constant Disconnect_Request_Access :=
        new Disconnect_Request (Kernel);
   begin
      Self.Parameters.arguments :=
        (Is_Set => True,
         Value  =>
           (restart           => False,
            terminateDebuggee => Terminate_Debuggee,
            suspendDebuggee   => False));
      return Self;
   end Create;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Disconnect_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.DisconnectResponse;
      New_Request : in out DAP_Request_Access)
   is
      pragma Unreferenced (New_Request);
   begin
      Client.On_Disconnected;
   end On_Result_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Disconnect_Request;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      DAP.Requests.Disconnect.On_Rejected
        (DAP.Requests.Disconnect.Disconnect_DAP_Request (Self), Client);

      Client.On_Disconnected;
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Disconnect_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      DAP.Requests.Disconnect.On_Error_Message
        (DAP.Requests.Disconnect.Disconnect_DAP_Request (Self),
         Client, Message);

      Client.On_Disconnected;
   end On_Error_Message;

end DAP.Clients.Disconnect;
