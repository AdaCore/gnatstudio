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

--  Concrete implementation of the DAP 'continue' request

with DAP.Requests;        use DAP.Requests;
with DAP.Requests.Continue;
with GPS.Kernel;          use GPS.Kernel;

package DAP.Clients.Continues is

   type Continue_Request (<>) is
     new DAP.Requests.Continue.Continue_DAP_Request
   with private;
   type Continue_Request_Access is access all Continue_Request'Class;

   function Create
     (Kernel    : not null Kernel_Handle;
      Thread_Id : Integer)
      return Continue_Request_Access;
   --  Create a new DAP 'continue' request.
   --  Thread_Id specifies the active thread. Only the thread with this ID is
   --  resumed if the debug adapter supports single thread execution.

   overriding procedure On_Result_Message
     (Self        : in out Continue_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.ContinueResponse;
      New_Request : in out DAP_Request_Access);

private

   type Continue_Request is
     new DAP.Requests.Continue.Continue_DAP_Request with
       null record;

end DAP.Clients.Continues;
