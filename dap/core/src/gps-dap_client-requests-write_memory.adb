with DAP.Tools.Inputs;
with DAP.Tools.Outputs;
with VSS.Strings.Conversions;

package body GPS.DAP_Client.Requests.Write_Memory is

   overriding procedure Write
     (Self   : Write_Memory_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      DAP.Tools.Outputs.Output_WriteMemoryRequest (Stream, Self.Parameters);
   end Write;

   overriding procedure On_Result_Message
     (Self        : in out Write_Memory_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access) is
      Response : DAP.Tools.WriteMemoryResponse;
   begin
      New_Request := null;

      DAP.Tools.Inputs.Input_WriteMemoryResponse (Stream, Response, Success);

      if Success then
         Write_Memory_Request'Class (Self).On_Result_Message
           (Response, New_Request);
      end if;
   end On_Result_Message;

   overriding procedure On_Error_Message
     (Self    : in out Write_Memory_Request;
      Message : VSS.Strings.Virtual_String) is
      UTF8 : constant String :=
        VSS.Strings.Conversions.To_UTF_8_String (Message);
   begin
      Self.Callbacks.Trace
        ("Request error: writeMemory -> " & UTF8,
         GPS.DAP_Client.Callbacks.Trace_Error);
      Self.Callbacks.On_Request_Error
        (Method  => "writeMemory",
         Message => UTF8);
   end On_Error_Message;

   overriding procedure Set_Seq
     (Self : in out Write_Memory_Request;
      Id   : Integer) is
   begin
      Self.Parameters.seq := Id;
   end Set_Seq;

end GPS.DAP_Client.Requests.Write_Memory;
