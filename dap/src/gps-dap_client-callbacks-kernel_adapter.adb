with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.VFS;
with GPS.Kernel;
with GPS.Kernel.Hooks;
with DAP.Tools;
with DAP.Types;                use DAP.Types;
with DAP.Clients;
with DAP.Clients.Stack_Trace;
with DAP.Views.Call_Stack;
with VSS.Strings.Conversions;

package body GPS.DAP_Client.Callbacks.Kernel_Adapter is

   package Stack_Traces renames DAP.Clients.Stack_Trace;

   Me : constant Trace_Handle := Create ("GPS.DAP.Callbacks", Off);

   function Create
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Client : DAP.Clients.DAP_Client_Access)
      return Kernel_Callback_Access is
   begin
      return new Kernel_Callback'
        (Kernel => Kernel,
         Client => Client);
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

   overriding procedure On_Debugger_Resumed
     (Self      : Kernel_Callback;
      Thread_Id : Integer) is
      pragma Unreferenced (Thread_Id);
   begin
      if Self.Client /= null then
         Self.Client.Set_Status (Running);
      end if;
   end On_Debugger_Resumed;

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

   overriding procedure On_Stacktrace_Frames
     (Self        : Kernel_Callback;
      Thread_Id   : Integer;
      Start_Frame : Natural;
      Response    : DAP.Tools.StackTraceResponse;
      Append_More : out Boolean) is
      pragma Unreferenced (Thread_Id);
      Trace     : constant Stack_Traces.Stack_Trace_Access :=
        (if Self.Client /= null then Self.Client.Get_Stack_Trace else null);
      Local_Append : Boolean := False;
      Selected     : Natural := 0;
   begin
      if Trace = null then
         Append_More := False;
         return;
      end if;

      Trace.Merge_Response
        (Client         => Self.Client,
         Start_Frame    => Start_Frame,
         Response       => Response,
         Append_More    => Local_Append,
         Selected_Index => Selected);

      Append_More := Local_Append;

      if Selected /= 0 then
         Trace.Select_Frame (Id => Selected, Client => Self.Client);
      end if;
   end On_Stacktrace_Frames;

   overriding procedure On_Stacktrace_Selected
     (Self      : Kernel_Callback;
      Thread_Id : Integer;
      Frame_Id  : Integer) is
      pragma Unreferenced (Thread_Id);
      Trace : constant Stack_Traces.Stack_Trace_Access :=
        (if Self.Client /= null then Self.Client.Get_Stack_Trace else null);
   begin
      if Trace /= null then
         Trace.Select_Frame (Id => Frame_Id, Client => Self.Client);
      end if;
   end On_Stacktrace_Selected;

   overriding procedure On_Stacktrace_Fetch_Complete
     (Self          : Kernel_Callback;
      Thread_Id     : Integer;
      Success       : Boolean;
      Initial_Fetch : Boolean) is
      pragma Unreferenced (Thread_Id);
   begin
      if Self.Client = null then
         return;
      end if;

      if Success then
         if Initial_Fetch then
            Self.Client.Set_Status (Stopped);
         end if;

         DAP.Views.Call_Stack.Update (Self.Client.Kernel, Self.Client);
         Self.Client.Kernel.Context_Changed (GPS.Kernel.No_Context);
      end if;
   end On_Stacktrace_Fetch_Complete;

end GPS.DAP_Client.Callbacks.Kernel_Adapter;
