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

with GNATCOLL.Traces;       use GNATCOLL.Traces;
with VSS.Strings.Conversions;
with DAP.Clients;
with DAP.Tools.Inputs;
with DAP.Tools.Outputs;

package body DAP.Requests.Disconnects is

   Me : constant Trace_Handle := Create ("GPS.DAP.Requests_Disconnects", On);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self               : in out Disconnect_DAP_Request;
      Terminate_Debuggee : Boolean) is
   begin
      Self.Parameters.arguments :=
        (Is_Set => True,
         Value  =>
           (restart           => False,
            terminateDebuggee => Terminate_Debuggee));
   end Initialize;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self   : Disconnect_DAP_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      DAP.Tools.Outputs.Output_DisconnectRequest (Stream, Self.Parameters);
   end Write;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Disconnect_DAP_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      New_Request : in out DAP_Request_Access)
   is
      Response : DAP.Tools.DisconnectResponse;
      Success  : Boolean := True;
   begin
      DAP.Tools.Inputs.Input_DisconnectResponse (Stream, Response, Success);
      if Success then
         Disconnect_DAP_Request'Class
           (Self).On_Result_Message (Response, New_Request);
      end if;
   end On_Result_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   procedure On_Result_Message
     (Self        : in out Disconnect_DAP_Request;
      Result      : DAP.Tools.DisconnectResponse;
      New_Request : in out DAP_Request_Access)
   is
      pragma Unreferenced (New_Request);
   begin
      Self.Client.On_Terminated;
   end On_Result_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out Disconnect_DAP_Request) is
   begin
      Trace (Me, "Rejected");
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Disconnect_DAP_Request;
      Message : VSS.Strings.Virtual_String) is
   begin
      Trace (Me, VSS.Strings.Conversions.To_UTF_8_String (Message));
   end On_Error_Message;

   -------------
   -- Set_Seq --
   -------------

   overriding procedure Set_Seq
     (Self : in out Disconnect_DAP_Request;
      Id   : Integer) is
   begin
      Self.Parameters.seq := Id;
   end Set_Seq;

end DAP.Requests.Disconnects;
