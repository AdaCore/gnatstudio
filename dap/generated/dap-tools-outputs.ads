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

with VSS.JSON.Content_Handlers;

package DAP.Tools.Outputs is

   procedure Output_ModuleEvent_reason
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.ModuleEvent_reason);

   procedure Output_ColumnDescriptor_type
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.ColumnDescriptor_type);

   procedure Output_StackFrame_presentationHint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.StackFrame_presentationHint);

   procedure Output_ExceptionBreakMode
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.ExceptionBreakMode);

   procedure Output_StoppedEvent_reason
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.StoppedEvent_reason);

   procedure Output_StartDebuggingRequestArguments_request
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.StartDebuggingRequestArguments_request);

   procedure Output_OutputEvent_category
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.OutputEvent_category);

   procedure Output_OutputEvent_group
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.OutputEvent_group);

   procedure Output_ChecksumAlgorithm
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.ChecksumAlgorithm);

   procedure Output_ProcessEvent_startMethod
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.ProcessEvent_startMethod);

   procedure Output_Scope_presentationHint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.Scope_presentationHint);

   procedure Output_Response_message
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.Response_message);

   procedure Output_CompletionItemType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.CompletionItemType);

   procedure Output_InvalidatedAreas
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.InvalidatedAreas);

   procedure Output_Source_presentationHint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.Source_presentationHint);

   procedure Output_LoadedSourceEvent_reason
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.LoadedSourceEvent_reason);

   procedure Output_ProtocolMessage_type
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.ProtocolMessage_type);

   procedure Output_RunInTerminalRequestArguments_kind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.RunInTerminalRequestArguments_kind);

   procedure Output_VariablesArguments_filter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.VariablesArguments_filter);

   procedure Output_VariablePresentationHint_kind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.VariablePresentationHint_kind);

   procedure Output_VariablePresentationHint_attributes
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.VariablePresentationHint_attributes);

   procedure Output_VariablePresentationHint_visibility
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.VariablePresentationHint_visibility);

   procedure Output_InitializeRequestArguments_pathFormat
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.InitializeRequestArguments_pathFormat);

   procedure Output_ThreadEvent_reason
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.ThreadEvent_reason);

   procedure Output_DataBreakpointAccessType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.DataBreakpointAccessType);

   procedure Output_BreakpointEvent_reason
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.BreakpointEvent_reason);

   procedure Output_EvaluateArguments_context
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.EvaluateArguments_context);

   procedure Output_SteppingGranularity
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.SteppingGranularity);

   procedure Output_GotoResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : GotoResponse);

   procedure Output_ExceptionDetails
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionDetails);

   procedure Output_StepInTargetsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepInTargetsRequest);

   procedure Output_ModulesResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ModulesResponse);

   procedure Output_NextArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : NextArguments);

   procedure Output_ExceptionInfoRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionInfoRequest);

   procedure Output_TerminateThreadsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TerminateThreadsArguments);

   procedure Output_DataBreakpointInfoRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DataBreakpointInfoRequest);

   procedure Output_TerminateThreadsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TerminateThreadsRequest);

   procedure Output_SetBreakpointsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetBreakpointsArguments);

   procedure Output_ModuleEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ModuleEvent);

   procedure Output_ContinuedEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ContinuedEvent);

   procedure Output_RestartArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RestartArguments);

   procedure Output_SetVariableArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetVariableArguments);

   procedure Output_Checksum
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Checksum);

   procedure Output_BreakpointLocationsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : BreakpointLocationsRequest);

   procedure Output_ModulesViewDescriptor
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ModulesViewDescriptor);

   procedure Output_ColumnDescriptor
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ColumnDescriptor);

   procedure Output_Capabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Capabilities);

   procedure Output_StackTraceResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StackTraceResponse);

   procedure Output_LoadedSourcesArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LoadedSourcesArguments);

   procedure Output_ConfigurationDoneRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ConfigurationDoneRequest);

   procedure Output_StepInTargetsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepInTargetsResponse);

   procedure Output_StackFrame
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StackFrame);

   procedure Output_SetExpressionRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetExpressionRequest);

   procedure Output_SourceArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SourceArguments);

   procedure Output_ExceptionFilterOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionFilterOptions);

   procedure Output_ExceptionBreakpointsFilter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionBreakpointsFilter);

   procedure Output_SetVariableRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetVariableRequest);

   procedure Output_AttachRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : AttachRequest);

   procedure Output_MemoryEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : MemoryEvent);

   procedure Output_ReadMemoryArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ReadMemoryArguments);

   procedure Output_LoadedSourcesRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LoadedSourcesRequest);

   procedure Output_LaunchRequestArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LaunchRequestArguments);

   procedure Output_ExitedEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExitedEvent);

   procedure Output_SetBreakpointsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetBreakpointsRequest);

   procedure Output_TerminateResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TerminateResponse);

   procedure Output_Variable
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Variable);

   procedure Output_StoppedEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StoppedEvent);

   procedure Output_RestartFrameRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RestartFrameRequest);

   procedure Output_AttachRequestArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : AttachRequestArguments);

   procedure Output_ScopesResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ScopesResponse);

   procedure Output_StepOutArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepOutArguments);

   procedure Output_CompletionsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CompletionsRequest);

   procedure Output_StartDebuggingRequestArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StartDebuggingRequestArguments);

   procedure Output_ProgressUpdateEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ProgressUpdateEvent);

   procedure Output_ExceptionInfoResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionInfoResponse);

   procedure Output_InitializedEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : InitializedEvent);

   procedure Output_SetExpressionResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetExpressionResponse);

   procedure Output_StepInTarget
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepInTarget);

   procedure Output_ReverseContinueResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ReverseContinueResponse);

   procedure Output_OutputEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : OutputEvent);

   procedure Output_RestartRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RestartRequest);

   procedure Output_StackTraceArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StackTraceArguments);

   procedure Output_Thread
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Thread);

   procedure Output_SetDataBreakpointsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetDataBreakpointsRequest);

   procedure Output_SourceRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SourceRequest);

   procedure Output_PauseResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : PauseResponse);

   procedure Output_SetFunctionBreakpointsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetFunctionBreakpointsRequest);

   procedure Output_ProcessEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ProcessEvent);

   procedure Output_NextResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : NextResponse);

   procedure Output_AttachResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : AttachResponse);

   procedure Output_RestartResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RestartResponse);

   procedure Output_CapabilitiesEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CapabilitiesEvent);

   procedure Output_Scope
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Scope);

   procedure Output_DisassembleArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DisassembleArguments);

   procedure Output_SetInstructionBreakpointsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetInstructionBreakpointsRequest);

   procedure Output_Response
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Response);

   procedure Output_DataBreakpointInfoResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DataBreakpointInfoResponse);

   procedure Output_SourceBreakpoint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SourceBreakpoint);

   procedure Output_PauseRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : PauseRequest);

   procedure Output_FunctionBreakpoint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : FunctionBreakpoint);

   procedure Output_SetExpressionArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetExpressionArguments);

   procedure Output_SetExceptionBreakpointsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetExceptionBreakpointsArguments);

   procedure Output_ValueFormat
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ValueFormat);

   procedure Output_RunInTerminalRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RunInTerminalRequest);

   procedure Output_CompletionsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CompletionsArguments);

   procedure Output_WriteMemoryResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : WriteMemoryResponse);

   procedure Output_ReverseContinueArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ReverseContinueArguments);

   procedure Output_RunInTerminalResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RunInTerminalResponse);

   procedure Output_DisconnectArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DisconnectArguments);

   procedure Output_Module
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Module);

   procedure Output_GotoTargetsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : GotoTargetsRequest);

   procedure Output_ThreadsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ThreadsResponse);

   procedure Output_SetDataBreakpointsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetDataBreakpointsResponse);

   procedure Output_DataBreakpoint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DataBreakpoint);

   procedure Output_SetDataBreakpointsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetDataBreakpointsArguments);

   procedure Output_ExceptionPathSegment
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionPathSegment);

   procedure Output_Message
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Message);

   procedure Output_SourceResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SourceResponse);

   procedure Output_ContinueResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ContinueResponse);

   procedure Output_RestartFrameResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RestartFrameResponse);

   procedure Output_StepInArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepInArguments);

   procedure Output_LaunchResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LaunchResponse);

   procedure Output_StepInResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepInResponse);

   procedure Output_TerminateArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TerminateArguments);

   procedure Output_LaunchRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LaunchRequest);

   procedure Output_StepOutResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepOutResponse);

   procedure Output_EvaluateRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : EvaluateRequest);

   procedure Output_ContinueArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ContinueArguments);

   procedure Output_StepBackArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepBackArguments);

   procedure Output_BreakpointLocationsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : BreakpointLocationsArguments);

   procedure Output_CancelArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CancelArguments);

   procedure Output_CompletionsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CompletionsResponse);

   procedure Output_Breakpoint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Breakpoint);

   procedure Output_Source
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Source);

   procedure Output_WriteMemoryArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : WriteMemoryArguments);

   procedure Output_ConfigurationDoneArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ConfigurationDoneArguments);

   procedure Output_StepOutRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepOutRequest);

   procedure Output_Request
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Request);

   procedure Output_LoadedSourceEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LoadedSourceEvent);

   procedure Output_StackFrameFormat
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StackFrameFormat);

   procedure Output_DisassembleRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DisassembleRequest);

   procedure Output_ReadMemoryResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ReadMemoryResponse);

   procedure Output_StepBackRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepBackRequest);

   procedure Output_ProtocolMessage
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ProtocolMessage);

   procedure Output_ThreadsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ThreadsRequest);

   procedure Output_VariablesResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : VariablesResponse);

   procedure Output_RunInTerminalRequestArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RunInTerminalRequestArguments);

   procedure Output_TerminateRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TerminateRequest);

   procedure Output_VariablesArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : VariablesArguments);

   procedure Output_InitializeRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : InitializeRequest);

   procedure Output_BreakpointLocationsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : BreakpointLocationsResponse);

   procedure Output_VariablePresentationHint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : VariablePresentationHint);

   procedure Output_DisassembledInstruction
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DisassembledInstruction);

   procedure Output_PauseArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : PauseArguments);

   procedure Output_CancelResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CancelResponse);

   procedure Output_InitializeRequestArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : InitializeRequestArguments);

   procedure Output_SetInstructionBreakpointsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetInstructionBreakpointsResponse);

   procedure Output_CancelRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CancelRequest);

   procedure Output_ProgressEndEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ProgressEndEvent);

   procedure Output_Event
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Event);

   procedure Output_VariablesRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : VariablesRequest);

   procedure Output_ExceptionOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionOptions);

   procedure Output_TerminatedEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TerminatedEvent);

   procedure Output_StartDebuggingRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StartDebuggingRequest);

   procedure Output_ThreadEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ThreadEvent);

   procedure Output_GotoTargetsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : GotoTargetsResponse);

   procedure Output_CompletionItem
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CompletionItem);

   procedure Output_ScopesArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ScopesArguments);

   procedure Output_ErrorResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ErrorResponse);

   procedure Output_SetInstructionBreakpointsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetInstructionBreakpointsArguments);

   procedure Output_GotoArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : GotoArguments);

   procedure Output_BreakpointEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : BreakpointEvent);

   procedure Output_GotoTarget
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : GotoTarget);

   procedure Output_ReadMemoryRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ReadMemoryRequest);

   procedure Output_ModulesArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ModulesArguments);

   procedure Output_NextRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : NextRequest);

   procedure Output_ProgressStartEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ProgressStartEvent);

   procedure Output_SetVariableResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetVariableResponse);

   procedure Output_BreakpointLocation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : BreakpointLocation);

   procedure Output_RestartFrameArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RestartFrameArguments);

   procedure Output_DisconnectResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DisconnectResponse);

   procedure Output_SetExceptionBreakpointsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetExceptionBreakpointsRequest);

   procedure Output_WriteMemoryRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : WriteMemoryRequest);

   procedure Output_DataBreakpointInfoArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DataBreakpointInfoArguments);

   procedure Output_InitializeResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : InitializeResponse);

   procedure Output_ConfigurationDoneResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ConfigurationDoneResponse);

   procedure Output_StepInTargetsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepInTargetsArguments);

   procedure Output_EvaluateArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : EvaluateArguments);

   procedure Output_SetFunctionBreakpointsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetFunctionBreakpointsArguments);

   procedure Output_GotoRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : GotoRequest);

   procedure Output_SetFunctionBreakpointsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetFunctionBreakpointsResponse);

   procedure Output_ExceptionInfoArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionInfoArguments);

   procedure Output_StackTraceRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StackTraceRequest);

   procedure Output_EvaluateResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : EvaluateResponse);

   procedure Output_ReverseContinueRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ReverseContinueRequest);

   procedure Output_ModulesRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ModulesRequest);

   procedure Output_SetBreakpointsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetBreakpointsResponse);

   procedure Output_InstructionBreakpoint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : InstructionBreakpoint);

   procedure Output_DisconnectRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DisconnectRequest);

   procedure Output_TerminateThreadsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TerminateThreadsResponse);

   procedure Output_ScopesRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ScopesRequest);

   procedure Output_SetExceptionBreakpointsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetExceptionBreakpointsResponse);

   procedure Output_StepBackResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepBackResponse);

   procedure Output_DisassembleResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DisassembleResponse);

   procedure Output_InvalidatedEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : InvalidatedEvent);

   procedure Output_GotoTargetsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : GotoTargetsArguments);

   procedure Output_StepInRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepInRequest);

   procedure Output_ContinueRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ContinueRequest);

   procedure Output_LoadedSourcesResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LoadedSourcesResponse);

   procedure Output_StartDebuggingResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StartDebuggingResponse);

end DAP.Tools.Outputs;
