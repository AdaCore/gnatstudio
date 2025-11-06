with DAP.Tools.Inputs;
with DAP.Tools.Outputs;

package body GPS.DAP_Client.Requests.Stacktrace is

   overriding procedure Write
     (Self   : Stacktrace_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      DAP.Tools.Outputs.Output_StackTraceRequest (Stream, Self.Parameters);
   end Write;

   overriding procedure On_Result_Message
     (Self        : in out Stacktrace_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access)
   is
      Response   : DAP.Tools.StackTraceResponse;
      Append     : Boolean := False;
      Start_Frame : constant Natural :=
        (if Self.Parameters.arguments.startFrame.Is_Set then
            Self.Parameters.arguments.startFrame.Value
         else 0);
      Thread_Id  : constant Integer := Self.Parameters.arguments.threadId;
   begin
      New_Request := null;

      DAP.Tools.Inputs.Input_StackTraceResponse (Stream, Response, Success);

      if Success then
         Self.Callbacks.On_Stacktrace_Frames
           (Thread_Id   => Thread_Id,
            Start_Frame => Start_Frame,
            Response    => Response,
            Append_More => Append);

         Self.Callbacks.On_Stacktrace_Fetch_Complete
           (Thread_Id     => Thread_Id,
            Success       => True,
            Initial_Fetch => Start_Frame = 0);
      else
         Self.Callbacks.On_Stacktrace_Fetch_Complete
           (Thread_Id     => Thread_Id,
            Success       => False,
            Initial_Fetch => Start_Frame = 0);
      end if;
   end On_Result_Message;

   overriding procedure Set_Seq
     (Self : in out Stacktrace_Request;
      Id   : Integer) is
   begin
      Self.Parameters.seq := Id;
   end Set_Seq;

end GPS.DAP_Client.Requests.Stacktrace;
