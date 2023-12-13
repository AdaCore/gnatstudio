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

--  Concrete implementation of the DAP 'initialize' request

with DAP.Requests;        use DAP.Requests;
with DAP.Requests.Initialize;
with GPS.Kernel;          use GPS.Kernel;

package DAP.Clients.Initialize is

   type Initialize_Request (<>) is
     new DAP.Requests.Initialize.Initialize_DAP_Request
   with private;
   type Initialize_Request_Access is access all Initialize_Request'Class;

   function Create
     (Kernel : not null Kernel_Handle)
      return Initialize_Request_Access;
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

private

   type Initialize_Request is
     new DAP.Requests.Initialize.Initialize_DAP_Request with null record;

end DAP.Clients.Initialize;
