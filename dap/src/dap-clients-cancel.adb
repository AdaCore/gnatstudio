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

with DAP.Tools;

package body DAP.Clients.Cancel is

   ------------
   -- Create --
   ------------

   function Create
     (Kernel : not null Kernel_Handle)
      return Cancel_Request_Access
   is
      Self : constant Cancel_Request_Access := new Cancel_Request (Kernel);
   begin
      return Self;
   end Create;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Cancel_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.CancelResponse;
      New_Request : in out DAP_Request_Access)
   is
      pragma Unreferenced (New_Request);
   begin
      null;
   end On_Result_Message;

   -------------------------
   -- Send_Cancel_Request --
   -------------------------

   procedure Send_Cancel_Request (Client : in out DAP.Clients.DAP_Client'Class)
   is
      R : DAP_Request_Access := DAP_Request_Access (Create (Client.Kernel));
   begin
      Client.Process (R);
   end Send_Cancel_Request;

end DAP.Clients.Cancel;
