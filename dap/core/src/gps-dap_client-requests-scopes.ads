with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;
with DAP.Tools;

package GPS.DAP_Client.Requests.Scopes is

   type Scopes_Request is abstract new Request with record
      Parameters : aliased DAP.Tools.ScopesRequest :=
        DAP.Tools.ScopesRequest'
          (seq       => 0,
           arguments => <>);
   end record;

   overriding procedure Write
     (Self   : Scopes_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding procedure On_Result_Message
     (Self        : in out Scopes_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access);

   procedure On_Result_Message
     (Self        : in out Scopes_Request;
      Response    : in out DAP.Tools.ScopesResponse;
      New_Request : in out Request_Access) is abstract;

   overriding procedure Set_Seq
     (Self : in out Scopes_Request;
      Id   : Integer);

   overriding function Method
     (Self : in out Scopes_Request) return String is ("scopes");

end GPS.DAP_Client.Requests.Scopes;
