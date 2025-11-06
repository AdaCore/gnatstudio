with GPS.DAP_Client.Callbacks;
with GPS.Kernel;

package GPS.DAP_Client.Callbacks.Kernel_Adapter is

   type Kernel_Callback
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is new GPS.DAP_Client.Callbacks.DAP_Callback_Interface with null record;

   type Kernel_Callback_Access is access all Kernel_Callback;

   function Create
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Kernel_Callback_Access;

   overriding procedure Trace
     (Self    : Kernel_Callback;
      Message : String;
      Mode    : GPS.DAP_Client.Callbacks.Trace_Mode :=
        GPS.DAP_Client.Callbacks.Trace_Info);

   overriding function Allow_Request_Processing
     (Self : Kernel_Callback) return Boolean;

   overriding procedure On_Response_Processed
     (Self   : Kernel_Callback;
      Method : String);

   overriding procedure On_Request_Error
     (Self    : Kernel_Callback;
      Method  : String;
      Message : String);

end GPS.DAP_Client.Callbacks.Kernel_Adapter;
