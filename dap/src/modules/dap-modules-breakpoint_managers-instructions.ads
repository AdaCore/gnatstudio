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
with DAP.Requests.Instruction_Breakpoints;

private package DAP.Modules.Breakpoint_Managers.Instructions is

   type Instruction_Breakpoint_Request is
     new DAP.Requests.Instruction_Breakpoints.
       Instruction_Breakpoint_DAP_Request
   with record
      Manager : DAP_Client_Breakpoint_Manager_Access;
      Action  : Action_Kind;
      Sent    : Breakpoint_Vectors.Vector;
   end record;

   type Instruction_Breakpoint_Request_Access is
     access all Instruction_Breakpoint_Request;

   overriding procedure On_Result_Message
     (Self        : in out Instruction_Breakpoint_Request;
      Result      : in out DAP.Tools.SetInstructionBreakpointsResponse;
      New_Request : in out DAP_Request_Access);

   overriding procedure On_Rejected
     (Self : in out Instruction_Breakpoint_Request);

   overriding procedure On_Error_Message
     (Self    : in out Instruction_Breakpoint_Request;
      Message : VSS.Strings.Virtual_String);

end DAP.Modules.Breakpoint_Managers.Instructions;
