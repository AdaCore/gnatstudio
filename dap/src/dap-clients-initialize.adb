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

with VSS.Strings.Conversions;
with DAP.Clients.Launch;

package body DAP.Clients.Initialize is

   ------------
   -- Create --
   ------------

   function Create
     (Kernel : not null Kernel_Handle)
      return Initialize_Request_Access
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
      New_Request : in out DAP_Request_Access) is
   begin
      Client.Set_Capabilities (Result.a_body);
      New_Request := DAP_Request_Access
        (DAP.Clients.Launch.Create
           (Self.Kernel, DAP.Clients.DAP_Client_Access (Client)));
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

end DAP.Clients.Initialize;
