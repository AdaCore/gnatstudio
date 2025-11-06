with GPS.DAP_Client.Callbacks;
with GPS.Kernel;
with DAP.Tools;
limited with DAP.Clients;

package GPS.DAP_Client.Callbacks.Kernel_Adapter is

   type Kernel_Callback
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Client : DAP.Clients.DAP_Client_Access)
   is new GPS.DAP_Client.Callbacks.DAP_Callback_Interface with null record;

   type Kernel_Callback_Access is access all Kernel_Callback;

   function Create
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Client : DAP.Clients.DAP_Client_Access)
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

   overriding procedure On_Debugger_Resumed
     (Self      : Kernel_Callback;
      Thread_Id : Integer);

   overriding procedure On_Request_Error
     (Self    : Kernel_Callback;
      Method  : String;
      Message : String);

   overriding procedure On_Stacktrace_Frames
     (Self        : Kernel_Callback;
      Thread_Id   : Integer;
      Start_Frame : Natural;
      Response    : DAP.Tools.StackTraceResponse;
      Append_More : out Boolean);

   overriding procedure On_Stacktrace_Selected
     (Self      : Kernel_Callback;
      Thread_Id : Integer;
      Frame_Id  : Integer);

   overriding procedure On_Stacktrace_Fetch_Complete
     (Self          : Kernel_Callback;
      Thread_Id     : Integer;
      Success       : Boolean;
      Initial_Fetch : Boolean);

end GPS.DAP_Client.Callbacks.Kernel_Adapter;
