with DAP.Tools;

package GPS.DAP_Client.Requests.Stacktrace is

   type StackTrace_Request is abstract new Request with record
      Parameters : aliased DAP.Tools.StackTraceRequest :=
        DAP.Tools.StackTraceRequest'
          (seq       => 0,
           arguments => (threadId => 0, others => <>));
   end record;

   overriding procedure Write
     (Self   : StackTrace_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding procedure On_Result_Message
     (Self        : in out StackTrace_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access);

   procedure On_Result_Message
     (Self        : in out StackTrace_Request;
      Result      : in out DAP.Tools.StackTraceResponse;
      New_Request : in out Request_Access) is abstract;

   overriding procedure Set_Seq
     (Self : in out StackTrace_Request;
      Id   : Integer);

   overriding function Method
     (Self : in out StackTrace_Request) return String is ("stackTrace");

end GPS.DAP_Client.Requests.Stacktrace;
