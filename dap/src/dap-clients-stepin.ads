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

--  Concrete implementation of the DAP 'stepIn' request

with DAP.Requests;        use DAP.Requests;
with DAP.Requests.StepIn;
with GPS.Kernel;          use GPS.Kernel;

package DAP.Clients.StepIn is

   type Step_In_Request (<>) is
     new DAP.Requests.StepIn.Step_In_DAP_Request
   with private;
   type Step_In_Request_Access is access all Step_In_Request'Class;

   function Create
     (Kernel      : not null Kernel_Handle;
      Thread_Id   : Integer;
      Instruction : Boolean)
      return Step_In_Request_Access;
   --  Create a new DAP 'stepIn' request.
   --  Thread_Id specifies the thread for which to resume execution for one
   --  step-into (of the given granularity).
   --  If Instruction is True than step over one instruction only.

   overriding procedure On_Result_Message
     (Self        : in out Step_In_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.StepInResponse;
      New_Request : in out DAP_Request_Access);

   procedure Send_Step_In
     (Client : not null access DAP.Clients.DAP_Client'Class);
   --  Sends the corresponding request to step into.

   procedure Send_Step_In_Instruction
     (Client : not null access DAP.Clients.DAP_Client'Class);
   --  Sends the corresponding request to step into for one instruction.

private

   type Step_In_Request is
     new DAP.Requests.StepIn.Step_In_DAP_Request with null record;

end DAP.Clients.StepIn;
