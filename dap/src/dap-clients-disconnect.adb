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

with GPS.Kernel;          use GPS.Kernel;
with DAP.Requests;        use DAP.Requests;
with DAP.Requests.Disconnect;

package body DAP.Clients.Disconnect is

   type Disconnect_Request is
     new DAP.Requests.Disconnect.Disconnect_DAP_Request with null record;
   type Disconnect_Request_Access is access all Disconnect_Request'Class;

   function Create
     (Kernel             : not null Kernel_Handle;
      Terminate_Debuggee : Boolean)
      return Disconnect_Request_Access;
   --  Create a new DAP 'disconnect' request.
   --  Terminate_Debuggee indicates whether the debuggee should be terminated
   --  when the debugger is disconnected if supported. If unspecified, the
   --  debug adapter is free to do whatever it thinks is best.

   overriding procedure On_Rejected
     (Self   : in out Disconnect_Request;
      Client : not null access DAP.Clients.DAP_Client'Class);

   overriding procedure On_Error_Message
     (Self    : in out Disconnect_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String);

   overriding procedure On_Result_Message
     (Self        : in out Disconnect_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.DisconnectResponse;
      New_Request : in out DAP_Request_Access);

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
      --  Stop the debugging session only if we launched the debuggee through
      --  the debugger itself, not if we just attached to it.
      if Client.Start_Method = DAP.Types.Attached then
         Client.Set_Status (Initialized);
      else
         Client.Stop;
      end if;
   end On_Result_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Disconnect_Request;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      DAP.Requests.Disconnect.Disconnect_DAP_Request
        (Self).On_Rejected (Client);

      --  Something is wrong, but we still have to stop the client
      Client.Stop;
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Disconnect_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      DAP.Requests.Disconnect.Disconnect_DAP_Request
        (Self).On_Error_Message (Client, Message);

      --  Stop client even on error, because the debugger should be shot down
      Client.Stop;
   end On_Error_Message;

   -----------------------------
   -- Send_Disconnect_Request --
   -----------------------------

   procedure Send_Disconnect_Request
     (Client             : in out DAP.Clients.DAP_Client'Class;
      Terminate_Debuggee : Boolean)
   is
      Disconnect_Req : Disconnect_Request_Access :=
         DAP.Clients.Disconnect.Create
          (Kernel             => Client.Kernel,
           Terminate_Debuggee => Terminate_Debuggee);
   begin
      --  Set the DAP client's status to Terminating
      Client.Set_Status (Terminating);

      --  Clear the queue
      Client.Reject_All_Requests;

      --  Send the 'disconnect' request
      Client.Enqueue
        (DAP.Requests.DAP_Request_Access (Disconnect_Req), Force => True);
   end Send_Disconnect_Request;

end DAP.Clients.Disconnect;
