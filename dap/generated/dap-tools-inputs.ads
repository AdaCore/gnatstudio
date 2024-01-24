--  Copyright <YEAR> <COPYRIGHT HOLDER>
--
--  Permission is hereby granted, free of charge, to any person obtaining a
--  copy of this software and associated documentation files (the "Software"),
--  to deal in the Software without restriction, including without limitation
--  the rights to use, copy, modify, merge, publish, distribute, sublicense,
--  and/or sell copies of the Software, and to permit persons to whom the
--  Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE.

with VSS.JSON.Pull_Readers;

package DAP.Tools.Inputs is

   procedure Input_ModuleEvent_reason
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.ModuleEvent_reason;
      Success : in out Boolean);

   procedure Input_ColumnDescriptor_type
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.ColumnDescriptor_type;
      Success : in out Boolean);

   procedure Input_StackFrame_presentationHint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.StackFrame_presentationHint;
      Success : in out Boolean);

   procedure Input_ExceptionBreakMode
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.ExceptionBreakMode;
      Success : in out Boolean);

   procedure Input_StoppedEvent_reason
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.StoppedEvent_reason;
      Success : in out Boolean);

   procedure Input_StartDebuggingRequestArguments_request
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.StartDebuggingRequestArguments_request;
      Success : in out Boolean);

   procedure Input_OutputEvent_category
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.OutputEvent_category;
      Success : in out Boolean);

   procedure Input_OutputEvent_group
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.OutputEvent_group;
      Success : in out Boolean);

   procedure Input_ChecksumAlgorithm
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.ChecksumAlgorithm;
      Success : in out Boolean);

   procedure Input_ProcessEvent_startMethod
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.ProcessEvent_startMethod;
      Success : in out Boolean);

   procedure Input_Scope_presentationHint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.Scope_presentationHint;
      Success : in out Boolean);

   procedure Input_Response_message
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.Response_message;
      Success : in out Boolean);

   procedure Input_CompletionItemType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.CompletionItemType;
      Success : in out Boolean);

   procedure Input_InvalidatedAreas
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.InvalidatedAreas;
      Success : in out Boolean);

   procedure Input_Source_presentationHint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.Source_presentationHint;
      Success : in out Boolean);

   procedure Input_LoadedSourceEvent_reason
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.LoadedSourceEvent_reason;
      Success : in out Boolean);

   procedure Input_ProtocolMessage_type
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.ProtocolMessage_type;
      Success : in out Boolean);

   procedure Input_RunInTerminalRequestArguments_kind
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.RunInTerminalRequestArguments_kind;
      Success : in out Boolean);

   procedure Input_VariablesArguments_filter
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.VariablesArguments_filter;
      Success : in out Boolean);

   procedure Input_VariablePresentationHint_kind
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.VariablePresentationHint_kind;
      Success : in out Boolean);

   procedure Input_VariablePresentationHint_attributes
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.VariablePresentationHint_attributes;
      Success : in out Boolean);

   procedure Input_VariablePresentationHint_visibility
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.VariablePresentationHint_visibility;
      Success : in out Boolean);

   procedure Input_InitializeRequestArguments_pathFormat
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.InitializeRequestArguments_pathFormat;
      Success : in out Boolean);

   procedure Input_ThreadEvent_reason
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.ThreadEvent_reason;
      Success : in out Boolean);

   procedure Input_DataBreakpointAccessType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.DataBreakpointAccessType;
      Success : in out Boolean);

   procedure Input_BreakpointEvent_reason
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.BreakpointEvent_reason;
      Success : in out Boolean);

   procedure Input_EvaluateArguments_context
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.EvaluateArguments_context;
      Success : in out Boolean);

   procedure Input_SteppingGranularity
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.SteppingGranularity;
      Success : in out Boolean);

   procedure Input_GotoResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out GotoResponse;
      Success : in out Boolean);

   procedure Input_ExceptionDetails
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionDetails;
      Success : in out Boolean);

   procedure Input_StepInTargetsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepInTargetsRequest;
      Success : in out Boolean);

   procedure Input_ModulesResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ModulesResponse;
      Success : in out Boolean);

   procedure Input_NextArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out NextArguments;
      Success : in out Boolean);

   procedure Input_ExceptionInfoRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionInfoRequest;
      Success : in out Boolean);

   procedure Input_TerminateThreadsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TerminateThreadsArguments;
      Success : in out Boolean);

   procedure Input_DataBreakpointInfoRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DataBreakpointInfoRequest;
      Success : in out Boolean);

   procedure Input_TerminateThreadsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TerminateThreadsRequest;
      Success : in out Boolean);

   procedure Input_SetBreakpointsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetBreakpointsArguments;
      Success : in out Boolean);

   procedure Input_ModuleEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ModuleEvent;
      Success : in out Boolean);

   procedure Input_ContinuedEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ContinuedEvent;
      Success : in out Boolean);

   procedure Input_RestartArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RestartArguments;
      Success : in out Boolean);

   procedure Input_SetVariableArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetVariableArguments;
      Success : in out Boolean);

   procedure Input_Checksum
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Checksum;
      Success : in out Boolean);

   procedure Input_BreakpointLocationsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out BreakpointLocationsRequest;
      Success : in out Boolean);

   procedure Input_ModulesViewDescriptor
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ModulesViewDescriptor;
      Success : in out Boolean);

   procedure Input_ColumnDescriptor
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ColumnDescriptor;
      Success : in out Boolean);

   procedure Input_Capabilities
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Capabilities;
      Success : in out Boolean);

   procedure Input_StackTraceResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StackTraceResponse;
      Success : in out Boolean);

   procedure Input_LoadedSourcesArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LoadedSourcesArguments;
      Success : in out Boolean);

   procedure Input_ConfigurationDoneRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ConfigurationDoneRequest;
      Success : in out Boolean);

   procedure Input_StepInTargetsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepInTargetsResponse;
      Success : in out Boolean);

   procedure Input_StackFrame
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StackFrame;
      Success : in out Boolean);

   procedure Input_SetExpressionRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetExpressionRequest;
      Success : in out Boolean);

   procedure Input_SourceArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SourceArguments;
      Success : in out Boolean);

   procedure Input_ExceptionFilterOptions
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionFilterOptions;
      Success : in out Boolean);

   procedure Input_ExceptionBreakpointsFilter
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionBreakpointsFilter;
      Success : in out Boolean);

   procedure Input_SetVariableRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetVariableRequest;
      Success : in out Boolean);

   procedure Input_AttachRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out AttachRequest;
      Success : in out Boolean);

   procedure Input_MemoryEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out MemoryEvent;
      Success : in out Boolean);

   procedure Input_ReadMemoryArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ReadMemoryArguments;
      Success : in out Boolean);

   procedure Input_LoadedSourcesRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LoadedSourcesRequest;
      Success : in out Boolean);

   procedure Input_LaunchRequestArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LaunchRequestArguments;
      Success : in out Boolean);

   procedure Input_ExitedEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExitedEvent;
      Success : in out Boolean);

   procedure Input_SetBreakpointsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetBreakpointsRequest;
      Success : in out Boolean);

   procedure Input_TerminateResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TerminateResponse;
      Success : in out Boolean);

   procedure Input_Variable
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Variable;
      Success : in out Boolean);

   procedure Input_StoppedEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StoppedEvent;
      Success : in out Boolean);

   procedure Input_RestartFrameRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RestartFrameRequest;
      Success : in out Boolean);

   procedure Input_AttachRequestArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out AttachRequestArguments;
      Success : in out Boolean);

   procedure Input_ScopesResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ScopesResponse;
      Success : in out Boolean);

   procedure Input_StepOutArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepOutArguments;
      Success : in out Boolean);

   procedure Input_CompletionsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CompletionsRequest;
      Success : in out Boolean);

   procedure Input_StartDebuggingRequestArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StartDebuggingRequestArguments;
      Success : in out Boolean);

   procedure Input_ProgressUpdateEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ProgressUpdateEvent;
      Success : in out Boolean);

   procedure Input_ExceptionInfoResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionInfoResponse;
      Success : in out Boolean);

   procedure Input_InitializedEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out InitializedEvent;
      Success : in out Boolean);

   procedure Input_SetExpressionResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetExpressionResponse;
      Success : in out Boolean);

   procedure Input_StepInTarget
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepInTarget;
      Success : in out Boolean);

   procedure Input_ReverseContinueResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ReverseContinueResponse;
      Success : in out Boolean);

   procedure Input_OutputEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out OutputEvent;
      Success : in out Boolean);

   procedure Input_RestartRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RestartRequest;
      Success : in out Boolean);

   procedure Input_StackTraceArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StackTraceArguments;
      Success : in out Boolean);

   procedure Input_Thread
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Thread;
      Success : in out Boolean);

   procedure Input_SetDataBreakpointsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetDataBreakpointsRequest;
      Success : in out Boolean);

   procedure Input_SourceRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SourceRequest;
      Success : in out Boolean);

   procedure Input_PauseResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out PauseResponse;
      Success : in out Boolean);

   procedure Input_SetFunctionBreakpointsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetFunctionBreakpointsRequest;
      Success : in out Boolean);

   procedure Input_ProcessEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ProcessEvent;
      Success : in out Boolean);

   procedure Input_NextResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out NextResponse;
      Success : in out Boolean);

   procedure Input_AttachResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out AttachResponse;
      Success : in out Boolean);

   procedure Input_RestartResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RestartResponse;
      Success : in out Boolean);

   procedure Input_CapabilitiesEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CapabilitiesEvent;
      Success : in out Boolean);

   procedure Input_Scope
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Scope;
      Success : in out Boolean);

   procedure Input_DisassembleArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DisassembleArguments;
      Success : in out Boolean);

   procedure Input_SetInstructionBreakpointsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetInstructionBreakpointsRequest;
      Success : in out Boolean);

   procedure Input_Response
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Response;
      Success : in out Boolean);

   procedure Input_DataBreakpointInfoResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DataBreakpointInfoResponse;
      Success : in out Boolean);

   procedure Input_SourceBreakpoint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SourceBreakpoint;
      Success : in out Boolean);

   procedure Input_PauseRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out PauseRequest;
      Success : in out Boolean);

   procedure Input_FunctionBreakpoint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out FunctionBreakpoint;
      Success : in out Boolean);

   procedure Input_SetExpressionArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetExpressionArguments;
      Success : in out Boolean);

   procedure Input_SetExceptionBreakpointsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetExceptionBreakpointsArguments;
      Success : in out Boolean);

   procedure Input_ValueFormat
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ValueFormat;
      Success : in out Boolean);

   procedure Input_RunInTerminalRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RunInTerminalRequest;
      Success : in out Boolean);

   procedure Input_CompletionsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CompletionsArguments;
      Success : in out Boolean);

   procedure Input_WriteMemoryResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out WriteMemoryResponse;
      Success : in out Boolean);

   procedure Input_ReverseContinueArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ReverseContinueArguments;
      Success : in out Boolean);

   procedure Input_RunInTerminalResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RunInTerminalResponse;
      Success : in out Boolean);

   procedure Input_DisconnectArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DisconnectArguments;
      Success : in out Boolean);

   procedure Input_Module
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Module;
      Success : in out Boolean);

   procedure Input_GotoTargetsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out GotoTargetsRequest;
      Success : in out Boolean);

   procedure Input_ThreadsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ThreadsResponse;
      Success : in out Boolean);

   procedure Input_SetDataBreakpointsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetDataBreakpointsResponse;
      Success : in out Boolean);

   procedure Input_DataBreakpoint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DataBreakpoint;
      Success : in out Boolean);

   procedure Input_SetDataBreakpointsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetDataBreakpointsArguments;
      Success : in out Boolean);

   procedure Input_ExceptionPathSegment
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionPathSegment;
      Success : in out Boolean);

   procedure Input_Message
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Message;
      Success : in out Boolean);

   procedure Input_SourceResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SourceResponse;
      Success : in out Boolean);

   procedure Input_ContinueResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ContinueResponse;
      Success : in out Boolean);

   procedure Input_RestartFrameResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RestartFrameResponse;
      Success : in out Boolean);

   procedure Input_StepInArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepInArguments;
      Success : in out Boolean);

   procedure Input_LaunchResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LaunchResponse;
      Success : in out Boolean);

   procedure Input_StepInResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepInResponse;
      Success : in out Boolean);

   procedure Input_TerminateArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TerminateArguments;
      Success : in out Boolean);

   procedure Input_LaunchRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LaunchRequest;
      Success : in out Boolean);

   procedure Input_StepOutResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepOutResponse;
      Success : in out Boolean);

   procedure Input_EvaluateRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out EvaluateRequest;
      Success : in out Boolean);

   procedure Input_ContinueArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ContinueArguments;
      Success : in out Boolean);

   procedure Input_StepBackArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepBackArguments;
      Success : in out Boolean);

   procedure Input_BreakpointLocationsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out BreakpointLocationsArguments;
      Success : in out Boolean);

   procedure Input_CancelArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CancelArguments;
      Success : in out Boolean);

   procedure Input_CompletionsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CompletionsResponse;
      Success : in out Boolean);

   procedure Input_Breakpoint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Breakpoint;
      Success : in out Boolean);

   procedure Input_Source
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Source;
      Success : in out Boolean);

   procedure Input_WriteMemoryArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out WriteMemoryArguments;
      Success : in out Boolean);

   procedure Input_ConfigurationDoneArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ConfigurationDoneArguments;
      Success : in out Boolean);

   procedure Input_StepOutRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepOutRequest;
      Success : in out Boolean);

   procedure Input_Request
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Request;
      Success : in out Boolean);

   procedure Input_LoadedSourceEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LoadedSourceEvent;
      Success : in out Boolean);

   procedure Input_StackFrameFormat
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StackFrameFormat;
      Success : in out Boolean);

   procedure Input_DisassembleRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DisassembleRequest;
      Success : in out Boolean);

   procedure Input_ReadMemoryResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ReadMemoryResponse;
      Success : in out Boolean);

   procedure Input_StepBackRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepBackRequest;
      Success : in out Boolean);

   procedure Input_ProtocolMessage
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ProtocolMessage;
      Success : in out Boolean);

   procedure Input_ThreadsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ThreadsRequest;
      Success : in out Boolean);

   procedure Input_VariablesResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out VariablesResponse;
      Success : in out Boolean);

   procedure Input_RunInTerminalRequestArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RunInTerminalRequestArguments;
      Success : in out Boolean);

   procedure Input_TerminateRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TerminateRequest;
      Success : in out Boolean);

   procedure Input_VariablesArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out VariablesArguments;
      Success : in out Boolean);

   procedure Input_InitializeRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out InitializeRequest;
      Success : in out Boolean);

   procedure Input_BreakpointLocationsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out BreakpointLocationsResponse;
      Success : in out Boolean);

   procedure Input_VariablePresentationHint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out VariablePresentationHint;
      Success : in out Boolean);

   procedure Input_DisassembledInstruction
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DisassembledInstruction;
      Success : in out Boolean);

   procedure Input_PauseArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out PauseArguments;
      Success : in out Boolean);

   procedure Input_CancelResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CancelResponse;
      Success : in out Boolean);

   procedure Input_InitializeRequestArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out InitializeRequestArguments;
      Success : in out Boolean);

   procedure Input_SetInstructionBreakpointsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetInstructionBreakpointsResponse;
      Success : in out Boolean);

   procedure Input_CancelRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CancelRequest;
      Success : in out Boolean);

   procedure Input_ProgressEndEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ProgressEndEvent;
      Success : in out Boolean);

   procedure Input_Event
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Event;
      Success : in out Boolean);

   procedure Input_VariablesRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out VariablesRequest;
      Success : in out Boolean);

   procedure Input_ExceptionOptions
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionOptions;
      Success : in out Boolean);

   procedure Input_TerminatedEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TerminatedEvent;
      Success : in out Boolean);

   procedure Input_StartDebuggingRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StartDebuggingRequest;
      Success : in out Boolean);

   procedure Input_ThreadEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ThreadEvent;
      Success : in out Boolean);

   procedure Input_GotoTargetsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out GotoTargetsResponse;
      Success : in out Boolean);

   procedure Input_CompletionItem
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CompletionItem;
      Success : in out Boolean);

   procedure Input_ScopesArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ScopesArguments;
      Success : in out Boolean);

   procedure Input_ErrorResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ErrorResponse;
      Success : in out Boolean);

   procedure Input_SetInstructionBreakpointsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetInstructionBreakpointsArguments;
      Success : in out Boolean);

   procedure Input_GotoArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out GotoArguments;
      Success : in out Boolean);

   procedure Input_BreakpointEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out BreakpointEvent;
      Success : in out Boolean);

   procedure Input_GotoTarget
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out GotoTarget;
      Success : in out Boolean);

   procedure Input_ReadMemoryRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ReadMemoryRequest;
      Success : in out Boolean);

   procedure Input_ModulesArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ModulesArguments;
      Success : in out Boolean);

   procedure Input_NextRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out NextRequest;
      Success : in out Boolean);

   procedure Input_ProgressStartEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ProgressStartEvent;
      Success : in out Boolean);

   procedure Input_SetVariableResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetVariableResponse;
      Success : in out Boolean);

   procedure Input_BreakpointLocation
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out BreakpointLocation;
      Success : in out Boolean);

   procedure Input_RestartFrameArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RestartFrameArguments;
      Success : in out Boolean);

   procedure Input_DisconnectResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DisconnectResponse;
      Success : in out Boolean);

   procedure Input_SetExceptionBreakpointsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetExceptionBreakpointsRequest;
      Success : in out Boolean);

   procedure Input_WriteMemoryRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out WriteMemoryRequest;
      Success : in out Boolean);

   procedure Input_DataBreakpointInfoArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DataBreakpointInfoArguments;
      Success : in out Boolean);

   procedure Input_InitializeResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out InitializeResponse;
      Success : in out Boolean);

   procedure Input_ConfigurationDoneResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ConfigurationDoneResponse;
      Success : in out Boolean);

   procedure Input_StepInTargetsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepInTargetsArguments;
      Success : in out Boolean);

   procedure Input_EvaluateArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out EvaluateArguments;
      Success : in out Boolean);

   procedure Input_SetFunctionBreakpointsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetFunctionBreakpointsArguments;
      Success : in out Boolean);

   procedure Input_GotoRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out GotoRequest;
      Success : in out Boolean);

   procedure Input_SetFunctionBreakpointsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetFunctionBreakpointsResponse;
      Success : in out Boolean);

   procedure Input_ExceptionInfoArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionInfoArguments;
      Success : in out Boolean);

   procedure Input_StackTraceRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StackTraceRequest;
      Success : in out Boolean);

   procedure Input_EvaluateResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out EvaluateResponse;
      Success : in out Boolean);

   procedure Input_ReverseContinueRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ReverseContinueRequest;
      Success : in out Boolean);

   procedure Input_ModulesRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ModulesRequest;
      Success : in out Boolean);

   procedure Input_SetBreakpointsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetBreakpointsResponse;
      Success : in out Boolean);

   procedure Input_InstructionBreakpoint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out InstructionBreakpoint;
      Success : in out Boolean);

   procedure Input_DisconnectRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DisconnectRequest;
      Success : in out Boolean);

   procedure Input_TerminateThreadsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TerminateThreadsResponse;
      Success : in out Boolean);

   procedure Input_ScopesRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ScopesRequest;
      Success : in out Boolean);

   procedure Input_SetExceptionBreakpointsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetExceptionBreakpointsResponse;
      Success : in out Boolean);

   procedure Input_StepBackResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepBackResponse;
      Success : in out Boolean);

   procedure Input_DisassembleResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DisassembleResponse;
      Success : in out Boolean);

   procedure Input_InvalidatedEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out InvalidatedEvent;
      Success : in out Boolean);

   procedure Input_GotoTargetsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out GotoTargetsArguments;
      Success : in out Boolean);

   procedure Input_StepInRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepInRequest;
      Success : in out Boolean);

   procedure Input_ContinueRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ContinueRequest;
      Success : in out Boolean);

   procedure Input_LoadedSourcesResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LoadedSourcesResponse;
      Success : in out Boolean);

   procedure Input_StartDebuggingResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StartDebuggingResponse;
      Success : in out Boolean);

end DAP.Tools.Inputs;
