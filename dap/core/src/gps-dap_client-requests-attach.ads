with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;
with DAP.Tools;

package GPS.DAP_Client.Requests.Attach is

   type Attach_Request is abstract new Request with record
      Parameters : aliased DAP.Tools.AttachRequest :=
        DAP.Tools.AttachRequest'
          (seq       => 0,
           arguments => (others => <>));
   end record;

   overriding procedure Write
     (Self   : Attach_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding procedure On_Result_Message
     (Self        : in out Attach_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access);

   procedure On_Result_Message
     (Self        : in out Attach_Request;
      Response    : DAP.Tools.AttachResponse;
      New_Request : in out Request_Access) is abstract;

   overriding procedure Set_Seq
     (Self : in out Attach_Request;
      Id   : Integer);

   overriding function Method
     (Self : in out Attach_Request) return String is ("attach");

end GPS.DAP_Client.Requests.Attach;
