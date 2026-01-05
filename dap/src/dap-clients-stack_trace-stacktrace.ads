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

--  Concrete implementation of the DAP 'stackTrace' request

with VSS.Strings;         use VSS.Strings;
with DAP.Requests;        use DAP.Requests;
with DAP.Requests.StackTrace;

package DAP.Clients.Stack_Trace.StackTrace is

   type StackTrace_Request (<>) is
     new DAP.Requests.StackTrace.StackTrace_DAP_Request with private;
   type StackTrace_Request_Access is access all StackTrace_Request'Class;

   function Create
     (Client : access DAP.Clients.DAP_Client'Class;
      From   : Integer := 0;
      Limit  : Integer := 0)
      return StackTrace_Request_Access;
   --  Create a new DAP 'stackTrace' request.
   --  Get traces `From` the Id and up to `Limit` count if not 0

   overriding procedure On_Result_Message
     (Self        : in out StackTrace_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.StackTraceResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);

   overriding procedure On_Error_Message
     (Self    : in out StackTrace_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String);

   overriding procedure On_Rejected
     (Self   : in out StackTrace_Request;
      Client : not null access DAP.Clients.DAP_Client'Class);

private

   type StackTrace_Request is
     new DAP.Requests.StackTrace.StackTrace_DAP_Request with null record;

   procedure On_Response
     (Self       : in out StackTrace_Request;
      Client     : not null access DAP.Clients.DAP_Client'Class;
      Has_Result : Boolean);

end DAP.Clients.Stack_Trace.StackTrace;
