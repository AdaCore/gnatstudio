------------------------------------------------------------------------------
--  GPS.DAP_Client.Callbacks                                                --
------------------------------------------------------------------------------

package body GPS.DAP_Client.Callbacks is

   overriding procedure Trace
     (Self    : Null_Callback;
      Message : String;
      Mode    : Trace_Mode := Trace_Info) is
      pragma Unreferenced (Self, Message, Mode);
   begin
      null;
   end Trace;

   overriding function Allow_Request_Processing
     (Self : Null_Callback) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Allow_Request_Processing;

   overriding procedure On_Response_Processed
     (Self   : Null_Callback;
      Method : String) is
      pragma Unreferenced (Self, Method);
   begin
      null;
   end On_Response_Processed;

   overriding procedure On_Request_Error
     (Self    : Null_Callback;
      Method  : String;
      Message : String) is
      pragma Unreferenced (Self, Method, Message);
   begin
      null;
   end On_Request_Error;

end GPS.DAP_Client.Callbacks;
