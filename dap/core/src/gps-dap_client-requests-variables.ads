with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;
with DAP.Tools;

package GPS.DAP_Client.Requests.Variables is

   type Variables_Request is abstract new Request with record
      Parameters : aliased DAP.Tools.VariablesRequest :=
        DAP.Tools.VariablesRequest'
          (seq       => 0,
           arguments => <>);
   end record;

   overriding procedure Write
     (Self   : Variables_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding procedure On_Result_Message
     (Self        : in out Variables_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access);

   procedure On_Result_Message
     (Self        : in out Variables_Request;
      Response    : in out DAP.Tools.VariablesResponse;
      New_Request : in out Request_Access) is abstract;

   overriding procedure Set_Seq
     (Self : in out Variables_Request;
      Id   : Integer);

   overriding function Method
     (Self : in out Variables_Request) return String is ("variables");

end GPS.DAP_Client.Requests.Variables;
