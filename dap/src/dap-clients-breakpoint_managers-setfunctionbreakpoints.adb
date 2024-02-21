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

package body DAP.Clients.Breakpoint_Managers.SetFunctionBreakpoints is

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Function_Breakpoint_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      DAP.Requests.SetFunctionBreakpoints.On_Error_Message
        (DAP.Requests.SetFunctionBreakpoints.
           Function_Breakpoint_DAP_Request (Self), Client, Message);
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Function_Breakpoint_Request;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      DAP.Requests.SetFunctionBreakpoints.On_Rejected
        (DAP.Requests.SetFunctionBreakpoints.
           Function_Breakpoint_DAP_Request (Self), Client);
   end On_Rejected;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Function_Breakpoint_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.SetFunctionBreakpointsResponse;
      New_Request : in out DAP_Request_Access)
   is
      pragma Unreferenced (New_Request);
   begin
      Self.Manager.On_Breakpoint_Request_Response
        (Client          => Client,
         New_Breakpoints => Result.a_body.breakpoints,
         Old_Breakpoints => Self.Breakpoints);
   end On_Result_Message;

end DAP.Clients.Breakpoint_Managers.SetFunctionBreakpoints;
