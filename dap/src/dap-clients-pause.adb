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

package body DAP.Clients.Pause is

   ------------
   -- Create --
   ------------

   function Create
     (Kernel      : not null Kernel_Handle;
      Thread_Id   : Integer)
      return Pause_Request_Access
   is
      Self : constant Pause_Request_Access := new Pause_Request (Kernel);
   begin
      Self.Parameters.arguments.threadId := Thread_Id;
      return Self;
   end Create;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Pause_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.PauseResponse;
      New_Request : in out DAP_Request_Access)
   is
      pragma Unreferenced (New_Request);
   begin
      null;
   end On_Result_Message;

   ------------------------
   -- Send_Pause_Request --
   ------------------------

   procedure Send_Pause_Request
     (Client    : in out DAP.Clients.DAP_Client'Class;
      Thread_Id : Integer)
   is
      R : DAP_Request_Access :=
        DAP_Request_Access (Create (Client.Kernel, Thread_Id));
   begin
      Client.Process (R);
   end Send_Pause_Request;

end DAP.Clients.Pause;
