with DAP.Tools.Inputs;
with DAP.Tools.Outputs;

package body GPS.DAP_Client.Requests.Next is

   overriding procedure Write
     (Self   : Next_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      DAP.Tools.Outputs.Output_NextRequest (Stream, Self.Parameters);
   end Write;

   overriding procedure On_Result_Message
     (Self        : in out Next_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access) is
      Response  : DAP.Tools.NextResponse;
      Thread_Id : constant Integer := Self.Parameters.arguments.threadId;
   begin
      New_Request := null;

      DAP.Tools.Inputs.Input_NextResponse (Stream, Response, Success);

      if Success then
         Self.Callbacks.On_Debugger_Resumed (Thread_Id);
      end if;
   end On_Result_Message;

   overriding procedure Set_Seq
     (Self : in out Next_Request;
      Id   : Integer) is
   begin
      Self.Parameters.seq := Id;
   end Set_Seq;

end GPS.DAP_Client.Requests.Next;
