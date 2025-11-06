with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;
with DAP.Tools;

package GPS.DAP_Client.Requests.Loaded_Sources is

   type Loaded_Sources_Request is abstract new Request with record
      Parameters : aliased DAP.Tools.LoadedSourcesRequest :=
        DAP.Tools.LoadedSourcesRequest'
          (seq       => 0,
           arguments => <>);
   end record;

   overriding procedure Write
     (Self   : Loaded_Sources_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding procedure On_Result_Message
     (Self        : in out Loaded_Sources_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access);

   procedure On_Result_Message
     (Self        : in out Loaded_Sources_Request;
      Response    : DAP.Tools.LoadedSourcesResponse;
      New_Request : in out Request_Access) is abstract;

   overriding procedure Set_Seq
     (Self : in out Loaded_Sources_Request;
      Id   : Integer);

   overriding function Method
     (Self : in out Loaded_Sources_Request) return String is ("loadedSources");

end GPS.DAP_Client.Requests.Loaded_Sources;
