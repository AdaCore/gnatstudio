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

with DAP.Clients;

package body DAP.Modules.Breakpoint_Managers.SetExceptionBreakpoints is

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Exception_Breakpoint_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      DAP.Requests.SetExceptionBreakpoints.On_Error_Message
        (DAP.Requests.SetExceptionBreakpoints.
           Exception_Breakpoints_DAP_Request (Self), Client, Message);
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Exception_Breakpoint_Request;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      DAP.Requests.SetExceptionBreakpoints.On_Rejected
        (DAP.Requests.SetExceptionBreakpoints.
           Exception_Breakpoints_DAP_Request (Self), Client);
   end On_Rejected;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Exception_Breakpoint_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.SetExceptionBreakpointsResponse;
      New_Request : in out DAP_Request_Access)
   is
      pragma Unreferenced (New_Request);
      Empty_Breakpoints : DAP.Tools.Breakpoint_Vector;
   begin
      Self.Manager.On_Breakpoint_Request_Response
        (Client          => Client,
         New_Breakpoints =>
           (if Result.a_body.Is_Set then Result.a_body.Value.breakpoints
            else Empty_Breakpoints),
         Old_Breakpoints => Self.Breakpoints);
   end On_Result_Message;

end DAP.Modules.Breakpoint_Managers.SetExceptionBreakpoints;
