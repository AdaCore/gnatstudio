with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;
with DAP.Tools;

package GPS.DAP_Client.Requests.Configuration_Done is

   type Configuration_Done_Request is abstract new Request with record
      Parameters : aliased DAP.Tools.ConfigurationDoneRequest :=
        DAP.Tools.ConfigurationDoneRequest'
          (seq       => 0,
           arguments => <>);
   end record;

   overriding procedure Write
     (Self   : Configuration_Done_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding procedure On_Result_Message
     (Self        : in out Configuration_Done_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access);

   procedure On_Result_Message
     (Self        : in out Configuration_Done_Request;
      Response    : DAP.Tools.ConfigurationDoneResponse;
      New_Request : in out Request_Access) is abstract;

   overriding procedure Set_Seq
     (Self : in out Configuration_Done_Request;
      Id   : Integer);

   overriding function Method
     (Self : in out Configuration_Done_Request)
      return String is ("configurationDone");

end GPS.DAP_Client.Requests.Configuration_Done;
