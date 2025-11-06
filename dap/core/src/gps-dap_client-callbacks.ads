------------------------------------------------------------------------------
--  GPS.DAP_Client.Callbacks                                                --
--                                                                          --
--  Headless callback interface for the Debug Adapter Protocol (DAP) core.  --
--  Mirrors the strategy used by GPS.LSP_Client.Callbacks so the protocol   --
--  layer stays decoupled from GPS.Kernel/GtkAda.                           --
------------------------------------------------------------------------------

with DAP.Tools;

package GPS.DAP_Client.Callbacks is

   type Trace_Mode is (Trace_Error, Trace_Warning, Trace_Info, Trace_Debug);

   type DAP_Callback_Interface is limited interface;
   type DAP_Callback_Access is access all DAP_Callback_Interface'Class;

   procedure Trace
     (Self    : DAP_Callback_Interface;
      Message : String;
      Mode    : Trace_Mode := Trace_Info) is abstract;
   --  Route debug/diagnostic messages to the host.

   function Allow_Request_Processing
     (Self : DAP_Callback_Interface) return Boolean is abstract;
   --  Return False when the host is shutting down and the core should stop
   --  processing responses for outstanding requests.

   procedure On_Response_Processed
     (Self   : DAP_Callback_Interface;
      Method : String) is abstract;
   --  Hook invoked after a DAP response has been parsed and dispatched.

   procedure On_Debugger_Resumed
     (Self      : DAP_Callback_Interface;
      Thread_Id : Integer) is abstract;
   --  Execution resumed (continue/step/next acknowledged).

   procedure On_Request_Error
     (Self    : DAP_Callback_Interface;
      Method  : String;
      Message : String) is abstract;
   --  Hook invoked when a DAP response is flagged as an error.

   procedure On_Stacktrace_Frames
     (Self        : DAP_Callback_Interface;
      Thread_Id   : Integer;
      Start_Frame : Natural;
      Response    : DAP.Tools.StackTraceResponse;
      Append_More : out Boolean) is abstract;
   --  Merge frames into the host cache. Set Append_More=False to stop paging.

   procedure On_Stacktrace_Selected
     (Self      : DAP_Callback_Interface;
      Thread_Id : Integer;
      Frame_Id  : Integer) is abstract;
   --  Notify host that a frame has been selected for display.

   procedure On_Stacktrace_Fetch_Complete
     (Self          : DAP_Callback_Interface;
      Thread_Id     : Integer;
      Success       : Boolean;
      Initial_Fetch : Boolean) is abstract;
   --  Inform host that the fetch cycle finished (initial vs incremental).
   --  Hook invoked when a DAP response is flagged as an error.

   type Null_Callback is new DAP_Callback_Interface with null record;
   --  No-op implementation used by guard rails and tests.

   overriding procedure Trace
     (Self    : Null_Callback;
      Message : String;
      Mode    : Trace_Mode := Trace_Info);

   overriding function Allow_Request_Processing
     (Self : Null_Callback) return Boolean;

   overriding procedure On_Response_Processed
     (Self   : Null_Callback;
      Method : String);

   overriding procedure On_Debugger_Resumed
     (Self      : Null_Callback;
      Thread_Id : Integer);

   overriding procedure On_Request_Error
     (Self    : Null_Callback;
      Method  : String;
      Message : String);

   overriding procedure On_Stacktrace_Frames
     (Self        : Null_Callback;
      Thread_Id   : Integer;
      Start_Frame : Natural;
      Response    : DAP.Tools.StackTraceResponse;
      Append_More : out Boolean);

   overriding procedure On_Stacktrace_Selected
     (Self      : Null_Callback;
      Thread_Id : Integer;
      Frame_Id  : Integer);

   overriding procedure On_Stacktrace_Fetch_Complete
     (Self          : Null_Callback;
      Thread_Id     : Integer;
      Success       : Boolean;
      Initial_Fetch : Boolean);

end GPS.DAP_Client.Callbacks;
