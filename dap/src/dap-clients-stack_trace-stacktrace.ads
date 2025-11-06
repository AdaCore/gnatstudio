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

--  Concrete implementation of the DAP 'stackTrace' request

with GPS.Kernel;
with GPS.DAP_Client.Requests.Stacktrace;
with DAP.Clients;

package DAP.Clients.Stack_Trace.StackTrace is

   type StackTrace_Request (<>) is
     new GPS.DAP_Client.Requests.Stacktrace.Stacktrace_Request with private;
   type StackTrace_Request_Access is access all StackTrace_Request'Class;

   function Create
     (Client : access DAP.Clients.DAP_Client'Class;
      From   : Integer := 0;
      Limit  : Integer := 0)
      return StackTrace_Request_Access;
   --  Create a new DAP 'stackTrace' request.

private

   type StackTrace_Request is
     new GPS.DAP_Client.Requests.Stacktrace.Stacktrace_Request with null record;

end DAP.Clients.Stack_Trace.StackTrace;
