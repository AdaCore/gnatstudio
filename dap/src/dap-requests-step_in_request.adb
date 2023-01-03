------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with DAP.Module;
with DAP.Tools.Inputs;
with DAP.Tools.Outputs;

package body DAP.Requests.Step_In_Request is

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self   : Step_In_DAP_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      DAP.Tools.Outputs.Output_StepInRequest (Stream, Self.Parameters);
   end Write;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Step_In_DAP_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      New_Request : in out DAP_Request_Access)
   is
      Response : DAP.Tools.StepInResponse;
      Success  : Boolean := True;
   begin
      DAP.Tools.Inputs.Input_StepInResponse (Stream, Response, Success);
      if Success then
         Step_In_DAP_Request'Class
           (Self).On_Result_Message (Response, New_Request);
      end if;
   end On_Result_Message;

   -------------
   -- Set_Seq --
   -------------

   overriding procedure Set_Seq
     (Self : in out Step_In_DAP_Request;
      Id   : Integer) is
   begin
      Self.Parameters.seq := Id;
   end Set_Seq;

   -----------------------
   -- On_Result_Message --
   -----------------------

   procedure On_Result_Message
     (Self        : in out Step_In_DAP_Request;
      Result      : DAP.Tools.StepInResponse;
      New_Request : in out DAP_Request_Access) is
   begin
      New_Request := null;
      DAP.Module.Get_Current_Debugger.On_Continue;
   end On_Result_Message;

end DAP.Requests.Step_In_Request;
