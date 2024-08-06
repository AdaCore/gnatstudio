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

--  Concrete implementation of the DAP 'pause' request

with GPS.Kernel;          use GPS.Kernel;

with DAP.Requests;        use DAP.Requests;
with DAP.Requests.Pause;

package DAP.Clients.Pause is

   type Pause_Request (<>) is
     new DAP.Requests.Pause.Pause_DAP_Request
   with private;
   type Pause_Request_Access is access all Pause_Request'Class;

   procedure Send_Pause_Request
     (Client    : in out DAP.Clients.DAP_Client'Class;
      Thread_Id : Integer);
   --  Send a new DAP 'pause' request.

   overriding procedure On_Result_Message
     (Self        : in out Pause_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.PauseResponse;
      New_Request : in out DAP_Request_Access);

private

   type Pause_Request is
     new DAP.Requests.Pause.Pause_DAP_Request with null record;

   function Create
     (Kernel    : not null Kernel_Handle;
      Thread_Id : Integer)
      return Pause_Request_Access;
   --  Create a new DAP 'pause' request.

end DAP.Clients.Pause;
