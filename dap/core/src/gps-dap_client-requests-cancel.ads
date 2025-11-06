with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;
with DAP.Tools;

package GPS.DAP_Client.Requests.Cancel is

   type Cancel_Request is abstract new Request with record
      Parameters : aliased DAP.Tools.CancelRequest :=
        DAP.Tools.CancelRequest'
          (seq       => 0,
           arguments => (Is_Set => False));
   end record;

   overriding procedure Write
     (Self   : Cancel_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding procedure On_Result_Message
     (Self        : in out Cancel_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access);

   procedure On_Result_Message
     (Self        : in out Cancel_Request;
      Response    : DAP.Tools.CancelResponse;
      New_Request : in out Request_Access) is abstract;

   overriding procedure Set_Seq
     (Self : in out Cancel_Request;
      Id   : Integer);

   overriding function Method
     (Self : in out Cancel_Request) return String is ("cancel");

end GPS.DAP_Client.Requests.Cancel;
