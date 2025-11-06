# Stack Trace Callback Surface Design

This note captures the callback extensions required to migrate the
`stackTrace` request family into the headless DAP core.

## Current Behaviour (GUI implementation)

`DAP.Clients.Stack_Trace.StackTrace` presently relies on
`GPS.Kernel` and assorted GUI helpers for three responsibilities:

1. **State merge** – Append frames from the DAP response into the client-side
   stack cache, update the total frame count, and select a frame that maps to a
   readable location.
2. **View refresh** – When the first fetch completes, mark the debugger as
   `Stopped`; on incremental fetches, refresh the call-stack view and notify the
   kernel so contextual actions update.
3. **Error handling** – On rejection or parse failure, propagate the failure to
   the cache/view layer so the UI can remain coherent.

All of this work is performed directly inside the request object using
`DAP_Client'Class` and `GPS.Kernel` APIs, which is why the unit cannot move into
`dap/core/src` yet.

## Proposed Callback Extensions

Extend `GPS.DAP_Client.Callbacks` with a small stack-trace focused surface so
that protocol requests can delegate platform-specific behaviour to the host.

| Callback | Purpose |
| --- | --- |
| `procedure On_Stacktrace_Frames (Self; Thread_Id : Integer; Start_Frame : Natural; Frames : in DAP.Tools.StackTraceResponse; Append_Frames : out Boolean)` | Merge the decoded response into the host's stack cache. Host decides whether the frames were accepted (and can optionally request another fetch by returning `Append_Frames = False`). |
| `procedure On_Stacktrace_Selected (Self; Thread_Id : Integer; Frame_Id : Integer)` | Allow the host to select/update the active frame once new data is available. |
| `procedure On_Stacktrace_Fetch_Complete (Self; Thread_Id : Integer; Success : Boolean; Initial_Fetch : Boolean)` | Notify the host that the fetch cycle has finished so it can update debugger status, refresh views, etc. |

Design considerations:

- All callbacks operate on primitive data (thread id, frame ids) and the raw
  `DAP.Tools.StackTraceResponse`. No GUI types are referenced.
- The request body remains responsible for JSON encode/decode and for invoking
  these callbacks in the correct order.
- Existing behaviour (auto-select first reachable frame, update status) moves
  into the host implementation that wires the callbacks to the current cache and
  view modules.

## Migration Plan

1. Extend `GPS.DAP_Client.Callbacks` with the three callbacks above and provide
   no-op implementations in `Null_Callback`. *(Done — commit 00979012bf)*
2. Implement a concrete adapter in `DAP.Clients` that forwards these callbacks
   to the existing stack-cache/view helpers (`Client.Get_Stack_Trace`,
   `Call_Stack.Update`, `Set_Status`, etc.). *(Done — commit 14426bec1a)*
3. Refactor the stack-trace request family to call the new callbacks instead of
   touching `GPS.Kernel` directly and host it under `dap/core/src`. *(Done — this
   session)*

Once the adapter and callbacks are in place, the stack-trace request family can
be migrated without any GUI dependencies.
