with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;
with DAP.Tools;

package GPS.DAP_Client.Requests.Disconnect is

   type Disconnect_Request is abstract new Request with record
      Parameters : aliased DAP.Tools.DisconnectRequest :=
        DAP.Tools.DisconnectRequest'
          (seq       => 0,
           arguments =>
             (Is_Set => True,
              Value  =>
                (restart           => False,
                 terminateDebuggee => True,
                 suspendDebuggee   => False)));
   end record;

   overriding procedure Write
     (Self   : Disconnect_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding procedure On_Result_Message
     (Self        : in out Disconnect_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access);

   procedure On_Result_Message
     (Self        : in out Disconnect_Request;
      Response    : DAP.Tools.DisconnectResponse;
      New_Request : in out Request_Access) is abstract;

   overriding procedure Set_Seq
     (Self : in out Disconnect_Request;
      Id   : Integer);

   overriding function Method
     (Self : in out Disconnect_Request) return String is ("disconnect");

end GPS.DAP_Client.Requests.Disconnect;
