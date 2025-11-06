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

   overriding procedure On_Stacktrace_Frames
     (Self        : Null_Callback;
      Thread_Id   : Integer;
      Start_Frame : Natural;
      Response    : DAP.Tools.StackTraceResponse;
      Append_More : out Boolean) is
      pragma Unreferenced (Self, Thread_Id, Start_Frame, Response);
   begin
      Append_More := False;
   end On_Stacktrace_Frames;

   overriding procedure On_Stacktrace_Selected
     (Self      : Null_Callback;
      Thread_Id : Integer;
      Frame_Id  : Integer) is
      pragma Unreferenced (Self, Thread_Id, Frame_Id);
   begin
      null;
   end On_Stacktrace_Selected;

   overriding procedure On_Stacktrace_Fetch_Complete
     (Self          : Null_Callback;
      Thread_Id     : Integer;
      Success       : Boolean;
      Initial_Fetch : Boolean) is
      pragma Unreferenced (Self, Thread_Id, Success, Initial_Fetch);
   begin
      null;
   end On_Stacktrace_Fetch_Complete;

end GPS.DAP_Client.Callbacks;
