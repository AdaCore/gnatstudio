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
with GPS.Kernel;              use GPS.Kernel;

with DAP.Clients.Breakpoint_Managers;
with DAP.Requests;            use DAP.Requests;
with DAP.Requests.Initialize;
with VSS.Strings.Conversions;

package body DAP.Clients.Initialize is

   type Initialize_Request is
     new DAP.Requests.Initialize.Initialize_DAP_Request with null record;
   type Initialize_Request_Access is access all Initialize_Request'Class;

   function Create
     (Kernel : not null Kernel_Handle) return Initialize_Request_Access;
   --  Create a new DAP 'initialize' request.

   overriding procedure On_Result_Message
     (Self        : in out Initialize_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.InitializeResponse;
      New_Request : in out DAP_Request_Access);

   overriding procedure On_Error_Message
     (Self    : in out Initialize_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String);

   ------------
   -- Create --
   ------------

   function Create
     (Kernel : not null Kernel_Handle) return Initialize_Request_Access
   is
      Self : constant Initialize_Request_Access :=
        new Initialize_Request (Kernel);
   begin
      return Self;
   end Create;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Initialize_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.InitializeResponse;
      New_Request : in out DAP_Request_Access)
   is
      pragma Unreferenced (New_Request);
   begin
      Client.Set_Capabilities (Result.a_body);

      --  Initialize the Breakpoints' manager: it will send the needed requests
      --  to set all the initial breakpoints on the server's side and set the
      --  debugger's status to Ready when done.
      Client.Breakpoints :=
        DAP.Clients.Breakpoint_Managers.Breakpoint_Manager_Access'
          (new DAP.Clients.Breakpoint_Managers.Breakpoint_Manager_Type
             (Kernel_Handle (Client.Kernel),
              Client.This));
      Client.Breakpoints.Initialize;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Initialize_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      Self.Kernel.Get_Messages_Window.Insert_Error
        ("[Debug]:" &
           VSS.Strings.Conversions.To_UTF_8_String (Message));

      DAP.Requests.Initialize.On_Error_Message
        (DAP.Requests.Initialize.Initialize_DAP_Request (Self),
         Client, Message);
   end On_Error_Message;

   -----------------------------
   -- Send_Initialize_Request --
   -----------------------------

   procedure Send_Initialize_Request
     (Client : in out DAP.Clients.DAP_Client'Class)
   is
      Initialize_Req : Initialize_Request_Access :=
        DAP.Clients.Initialize.Create
          (Client.Kernel);
   begin
      Client.Process (DAP.Requests.DAP_Request_Access (Initialize_Req));
   end Send_Initialize_Request;

end DAP.Clients.Initialize;
