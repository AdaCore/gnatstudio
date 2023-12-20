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
with VSS.Strings.Conversions;
with GPS.Kernel;          use GPS.Kernel;
with DAP.Requests;        use DAP.Requests;
with DAP.Requests.Attach;
with DAP.Types;

package body DAP.Clients.Attach is

   type Attach_Request is new DAP.Requests.Attach.Attach_DAP_Request
   with null record;
   type Attach_Request_Access is access all Attach_Request'Class;

   function Create
     (Kernel : not null Kernel_Handle;
      PID    : Integer := -1;
      Target : String := "") return Attach_Request_Access;
   --  Create a new DAP 'attach' request.
   --  PID refers to the process we want to attach to.
   --  Target refers to the remote target we want to connect.
   --  Note that PID and Target are mutually exclusive: specifying one
   --  parameter will make the underlying DAP adapter ignore the other.

   overriding procedure On_Result_Message
     (Self        : in out Attach_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.AttachResponse;
      New_Request : in out DAP_Request_Access);
   --  Called when the 'attach' request has succeed.

   ------------
   -- Create --
   ------------

   function Create
     (Kernel : not null Kernel_Handle;
      PID    : Integer := -1;
      Target : String := "") return Attach_Request_Access
   is
      Self : constant Attach_Request_Access := new Attach_Request (Kernel);
   begin
      Self.Parameters.arguments.target :=
        VSS.Strings.Conversions.To_Virtual_String (Target);

      if PID /= -1 then
         Self.Parameters.arguments.pid := (Is_Set => True, Value => PID);
      end if;

      return Self;
   end Create;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Attach_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.AttachResponse;
      New_Request : in out DAP_Request_Access) is
   begin
      New_Request := null;
      Client.Start_Method := DAP.Types.Attached;
      Client.On_Launched;
   end On_Result_Message;

   -------------------------
   -- Send_Attach_Request --
   -------------------------

   procedure Send_Attach_Request
     (Client : in out DAP.Clients.DAP_Client'Class;
      PID    : Integer := -1;
      Target : String := "")
   is
      Attach_Req : DAP.Clients.Attach.Attach_Request_Access :=
        DAP.Clients.Attach.Create
          (Kernel => Client.Kernel,
           PID    => PID,
           Target => Target);
   begin
      Client.Enqueue
        (DAP.Requests.DAP_Request_Access (Attach_Req));
   end Send_Attach_Request;

end DAP.Clients.Attach;
