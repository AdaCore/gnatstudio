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

with GNATCOLL.Traces;         use GNATCOLL.Traces;
with VSS.Strings.Conversions;

with DAP.Tools.Inputs;
with DAP.Tools.Outputs;

package body DAP.Requests.SetFunctionBreakpoints is

   Me : constant Trace_Handle := Create
     ("GPS.DAP.Requests_Function_Breakpoints", On);

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self   : Function_Breakpoint_DAP_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      DAP.Tools.Outputs.Output_SetFunctionBreakpointsRequest
        (Stream, Self.Parameters);
   end Write;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Function_Breakpoint_DAP_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out DAP_Request_Access)
   is
      Response : DAP.Tools.SetFunctionBreakpointsResponse;
   begin
      DAP.Tools.Inputs.Input_SetFunctionBreakpointsResponse
        (Stream, Response, Success);

      if Success then
         Function_Breakpoint_DAP_Request'Class
           (Self).On_Result_Message (Client, Response, New_Request);
      end if;
   end On_Result_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Function_Breakpoint_DAP_Request;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      Trace (Me, "Rejected");
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Function_Breakpoint_DAP_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      Trace (Me, VSS.Strings.Conversions.To_UTF_8_String (Message));
   end On_Error_Message;

   -------------
   -- Set_Seq --
   -------------

   overriding procedure Set_Seq
     (Self : in out Function_Breakpoint_DAP_Request;
      Id   : Integer) is
   begin
      Self.Parameters.seq := Id;
   end Set_Seq;

end DAP.Requests.SetFunctionBreakpoints;
