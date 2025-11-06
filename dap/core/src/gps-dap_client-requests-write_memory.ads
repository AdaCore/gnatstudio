with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;
with VSS.Strings;
with DAP.Tools;

package GPS.DAP_Client.Requests.Write_Memory is

   type Write_Memory_Request is abstract new Request with record
      Parameters : aliased DAP.Tools.WriteMemoryRequest :=
        DAP.Tools.WriteMemoryRequest'
          (seq       => 0,
           arguments => <>);
   end record;

   overriding procedure Write
     (Self   : Write_Memory_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding procedure On_Result_Message
     (Self        : in out Write_Memory_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access);

   procedure On_Result_Message
     (Self        : in out Write_Memory_Request;
      Response    : DAP.Tools.WriteMemoryResponse;
      New_Request : in out Request_Access) is abstract;

   overriding procedure On_Error_Message
     (Self    : in out Write_Memory_Request;
      Message : VSS.Strings.Virtual_String);

   overriding procedure Set_Seq
     (Self : in out Write_Memory_Request;
      Id   : Integer);

   overriding function Method
     (Self : in out Write_Memory_Request) return String is ("writeMemory");

end GPS.DAP_Client.Requests.Write_Memory;
