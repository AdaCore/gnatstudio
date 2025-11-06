with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;
with DAP.Tools;

package GPS.DAP_Client.Requests.Continue is

   type Continue_Request is abstract new Request with record
      Parameters : aliased DAP.Tools.ContinueRequest :=
        DAP.Tools.ContinueRequest'
          (seq       => 0,
           arguments => (threadId => 0,
                         singleThread => False));
   end record;

   overriding procedure Write
     (Self   : Continue_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding procedure On_Result_Message
     (Self        : in out Continue_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access);

   overriding procedure Set_Seq
     (Self : in out Continue_Request;
      Id   : Integer);

   overriding function Method
     (Self : in out Continue_Request) return String is ("continue");

end GPS.DAP_Client.Requests.Continue;
