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

with VSS.Strings;
with DAP.Requests.Function_Breakpoints;

private package DAP.Modules.Breakpoint_Managers.Functions is

   type Function_Breakpoint_Request is
     new DAP.Requests.Function_Breakpoints.Function_Breakpoint_DAP_Request
   with record
      Manager : DAP_Client_Breakpoint_Manager_Access;
      Action  : Action_Kind;
      Sent    : Breakpoint_Vectors.Vector;
      Last    : Boolean := False;
   end record;

   type Function_Breakpoint_Request_Access is
     access all Function_Breakpoint_Request;

   overriding procedure On_Result_Message
     (Self        : in out Function_Breakpoint_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.SetFunctionBreakpointsResponse;
      New_Request : in out DAP_Request_Access);

   overriding procedure On_Rejected
     (Self   : in out Function_Breakpoint_Request;
      Client : not null access DAP.Clients.DAP_Client'Class);

   overriding procedure On_Error_Message
     (Self    : in out Function_Breakpoint_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String);

end DAP.Modules.Breakpoint_Managers.Functions;
