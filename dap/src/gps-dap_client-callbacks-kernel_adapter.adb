with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GPS.Kernel.Hooks;

package body GPS.DAP_Client.Callbacks.Kernel_Adapter is

   Me : constant Trace_Handle := Create ("GPS.DAP.Callbacks", Off);

   function Create
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Kernel_Callback_Access is
   begin
      return new Kernel_Callback'(Kernel => Kernel);
   end Create;

   overriding procedure Trace
     (Self    : Kernel_Callback;
      Message : String;
      Mode    : GPS.DAP_Client.Callbacks.Trace_Mode :=
        GPS.DAP_Client.Callbacks.Trace_Info)
   is
      pragma Unreferenced (Self, Mode);
   begin
      if Me.Is_Active then
         Trace (Me, Message);
      end if;
   end Trace;

   overriding function Allow_Request_Processing
     (Self : Kernel_Callback) return Boolean is
   begin
      return Self.Kernel = null
        or else not Self.Kernel.Is_In_Destruction;
   end Allow_Request_Processing;

   overriding procedure On_Response_Processed
     (Self   : Kernel_Callback;
      Method : String) is
   begin
      if Self.Kernel /= null then
         GPS.Kernel.Hooks.Dap_Response_Processed_Hook.Run
           (Kernel => GPS.Kernel.Kernel_Handle (Self.Kernel),
            Method => Method);
      end if;
   end On_Response_Processed;

   overriding procedure On_Request_Error
     (Self    : Kernel_Callback;
      Method  : String;
      Message : String) is
   begin
      Trace
        (Self,
         "Request error: " & Method & " -> " & Message,
         GPS.DAP_Client.Callbacks.Trace_Error);
   end On_Request_Error;

end GPS.DAP_Client.Callbacks.Kernel_Adapter;
