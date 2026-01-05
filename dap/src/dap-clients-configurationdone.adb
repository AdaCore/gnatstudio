------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2023-2026, AdaCore                  --
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

with DAP.Requests;                   use DAP.Requests;
with DAP.Requests.ConfigurationDone;
with GPS.Kernel;                     use GPS.Kernel;

package body DAP.Clients.ConfigurationDone is

   type ConfigurationDone_Request is
     new DAP.Requests.ConfigurationDone.ConfigurationDone_DAP_Request
   with null record;
   type ConfigurationDone_Request_Access is
     access all ConfigurationDone_Request'Class;

   function Create
     (Kernel : not null Kernel_Handle)
      return ConfigurationDone_Request_Access;
   --  Create a new DAP 'configurationDone' request.

   overriding procedure On_Result_Message
     (Self        : in out ConfigurationDone_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.ConfigurationDoneResponse;
      New_Request : in out DAP_Request_Access);

   ------------
   -- Create --
   ------------

   function Create
     (Kernel : not null Kernel_Handle)
      return ConfigurationDone_Request_Access
   is
      Self : constant ConfigurationDone_Request_Access :=
        new ConfigurationDone_Request (Kernel);
   begin
      return Self;
   end Create;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out ConfigurationDone_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.ConfigurationDoneResponse;
      New_Request : in out DAP_Request_Access) is
   begin
      New_Request := null;

      Client.On_Configured;
   end On_Result_Message;

   -----------------------------
   -- Send_Configuration_Done --
   -----------------------------

   procedure Send_Configuration_Done
     (Client : in out DAP.Clients.DAP_Client'Class)
   is
      Request : ConfigurationDone_Request_Access := Create (Client.Kernel);
   begin
      Client.Enqueue (DAP.Requests.DAP_Request_Access (Request));
   end Send_Configuration_Done;

end DAP.Clients.ConfigurationDone;
