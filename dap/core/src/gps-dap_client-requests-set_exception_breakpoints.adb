with DAP.Tools.Inputs;
with DAP.Tools.Outputs;

package body GPS.DAP_Client.Requests.Set_Exception_Breakpoints is

   overriding procedure Write
     (Self   : Set_Exception_Breakpoints_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      DAP.Tools.Outputs.Output_SetExceptionBreakpointsRequest
        (Stream, Self.Parameters);
   end Write;

   overriding procedure On_Result_Message
     (Self        : in out Set_Exception_Breakpoints_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access) is
      Response : DAP.Tools.SetExceptionBreakpointsResponse;
   begin
      New_Request := null;

      DAP.Tools.Inputs.Input_SetExceptionBreakpointsResponse
        (Stream, Response, Success);

      if Success then
         Set_Exception_Breakpoints_Request'Class (Self).On_Result_Message
           (Response, New_Request);
      end if;
   end On_Result_Message;

   overriding procedure Set_Seq
     (Self : in out Set_Exception_Breakpoints_Request;
      Id   : Integer) is
   begin
      Self.Parameters.seq := Id;
   end Set_Seq;

end GPS.DAP_Client.Requests.Set_Exception_Breakpoints;
