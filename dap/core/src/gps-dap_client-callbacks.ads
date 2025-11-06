------------------------------------------------------------------------------
--  GPS.DAP_Client.Callbacks                                                --
--                                                                          --
--  Headless callback interface for the Debug Adapter Protocol (DAP) core.  --
--  Mirrors the strategy used by GPS.LSP_Client.Callbacks so the protocol   --
--  layer stays decoupled from GPS.Kernel/GtkAda.                           --
------------------------------------------------------------------------------

package GPS.DAP_Client.Callbacks is

   pragma Preelaborate;

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

   procedure On_Request_Error
     (Self    : DAP_Callback_Interface;
      Method  : String;
      Message : String) is abstract;
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

   overriding procedure On_Request_Error
     (Self    : Null_Callback;
      Method  : String;
      Message : String);

end GPS.DAP_Client.Callbacks;
