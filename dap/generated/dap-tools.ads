------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------
--  generated do not edit
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Finalization;
with Ada.Streams;
with Ada.Unchecked_Deallocation;
with LSP.Types;   use LSP.Types;
with VSS.Strings; use VSS.Strings;

package DAP.Tools is

   package Enums is
      type ChecksumAlgorithm is (MD5, SHA1, SHA256, timestamp);
      type CompletionItemType is
        (method, a_function, constructor, field, variable, class, a_interface,
         module, property, unit, value, enum, keyword, snippet, text, color,
         file, reference, customcolor);
      type DataBreakpointAccessType is (read, write, readWrite);
      type ExceptionBreakMode is (never, always, unhandled, userUnhandled);
      type InvalidatedAreas is (a_all, stacks, threads, variables);
      type SteppingGranularity is (statement, line, instruction);
   end Enums;

   package DAP_String_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Virtual_String, Element_Type => Virtual_String,
      Hash     => LSP.Types.Hash, Equivalent_Keys => VSS.Strings."=");
   type Access_DAP_String_Map is access all DAP_String_Maps.Map;

   package DAP_String_Vectors is new Ada.Containers.Vectors
     (Positive, Virtual_String, "=");
   type DAP_String_Vector is new DAP_String_Vectors.Vector with null record;
   type Access_DAP_String_Vector is access all DAP_String_Vector;

   package DAP_Integer_Vectors is new Ada.Containers.Vectors
     (Positive, Integer, "=");
   type DAP_Integer_Vector is new DAP_Integer_Vectors.Vector with null record;
   type Access_DAP_Integer_Vector is access all DAP_Integer_Vector;

   type Access_ChecksumAlgorithm is access all Enums.ChecksumAlgorithm;
   package DAP_ChecksumAlgorithm_Vectors is new Ada.Containers.Vectors
     (Positive, Access_ChecksumAlgorithm);
   type DAP_ChecksumAlgorithm_Vector is new DAP_ChecksumAlgorithm_Vectors
     .Vector with
   null record;
   type Access_DAP_ChecksumAlgorithm_Vector is
     access all DAP_ChecksumAlgorithm_Vector;

   type Checksum;
   type Access_Checksum is access all Checksum;
   package DAP_Checksum_Vectors is new Ada.Containers.Vectors
     (Positive, Access_Checksum);
   type DAP_Checksum_Vector is new DAP_Checksum_Vectors.Vector with
   null record;
   type Access_DAP_Checksum_Vector is access all DAP_Checksum_Vector;

   type ColumnDescriptor;
   type Access_ColumnDescriptor is access all ColumnDescriptor;
   package DAP_ColumnDescriptor_Vectors is new Ada.Containers.Vectors
     (Positive, Access_ColumnDescriptor);
   type DAP_ColumnDescriptor_Vector is new DAP_ColumnDescriptor_Vectors
     .Vector with
   null record;
   type Access_DAP_ColumnDescriptor_Vector is
     access all DAP_ColumnDescriptor_Vector;

   type DataBreakpoint;
   type Access_DataBreakpoint is access all DataBreakpoint;
   package DAP_DataBreakpoint_Vectors is new Ada.Containers.Vectors
     (Positive, Access_DataBreakpoint);
   type DAP_DataBreakpoint_Vector is new DAP_DataBreakpoint_Vectors.Vector with
   null record;
   type Access_DAP_DataBreakpoint_Vector is
     access all DAP_DataBreakpoint_Vector;

   type ExceptionBreakpointsFilter;
   type Access_ExceptionBreakpointsFilter is
     access all ExceptionBreakpointsFilter;
   package DAP_ExceptionBreakpointsFilter_Vectors is new Ada.Containers.Vectors
     (Positive, Access_ExceptionBreakpointsFilter);
   type DAP_ExceptionBreakpointsFilter_Vector is new
     DAP_ExceptionBreakpointsFilter_Vectors
     .Vector with
   null record;
   type Access_DAP_ExceptionBreakpointsFilter_Vector is
     access all DAP_ExceptionBreakpointsFilter_Vector;

   type ExceptionDetails;
   type Access_ExceptionDetails is access all ExceptionDetails;
   package DAP_ExceptionDetails_Vectors is new Ada.Containers.Vectors
     (Positive, Access_ExceptionDetails);
   type DAP_ExceptionDetails_Vector is new DAP_ExceptionDetails_Vectors
     .Vector with
   null record;
   type Access_DAP_ExceptionDetails_Vector is
     access all DAP_ExceptionDetails_Vector;

   type ExceptionFilterOptions;
   type Access_ExceptionFilterOptions is access all ExceptionFilterOptions;
   package DAP_ExceptionFilterOptions_Vectors is new Ada.Containers.Vectors
     (Positive, Access_ExceptionFilterOptions);
   type DAP_ExceptionFilterOptions_Vector is new
     DAP_ExceptionFilterOptions_Vectors
     .Vector with
   null record;
   type Access_DAP_ExceptionFilterOptions_Vector is
     access all DAP_ExceptionFilterOptions_Vector;

   type ExceptionOptions;
   type Access_ExceptionOptions is access all ExceptionOptions;
   package DAP_ExceptionOptions_Vectors is new Ada.Containers.Vectors
     (Positive, Access_ExceptionOptions);
   type DAP_ExceptionOptions_Vector is new DAP_ExceptionOptions_Vectors
     .Vector with
   null record;
   type Access_DAP_ExceptionOptions_Vector is
     access all DAP_ExceptionOptions_Vector;

   type ExceptionPathSegment;
   type Access_ExceptionPathSegment is access all ExceptionPathSegment;
   package DAP_ExceptionPathSegment_Vectors is new Ada.Containers.Vectors
     (Positive, Access_ExceptionPathSegment);
   type DAP_ExceptionPathSegment_Vector is new DAP_ExceptionPathSegment_Vectors
     .Vector with
   null record;
   type Access_DAP_ExceptionPathSegment_Vector is
     access all DAP_ExceptionPathSegment_Vector;

   type FunctionBreakpoint;
   type Access_FunctionBreakpoint is access all FunctionBreakpoint;
   package DAP_FunctionBreakpoint_Vectors is new Ada.Containers.Vectors
     (Positive, Access_FunctionBreakpoint);
   type DAP_FunctionBreakpoint_Vector is new DAP_FunctionBreakpoint_Vectors
     .Vector with
   null record;
   type Access_DAP_FunctionBreakpoint_Vector is
     access all DAP_FunctionBreakpoint_Vector;

   type InstructionBreakpoint;
   type Access_InstructionBreakpoint is access all InstructionBreakpoint;
   package DAP_InstructionBreakpoint_Vectors is new Ada.Containers.Vectors
     (Positive, Access_InstructionBreakpoint);
   type DAP_InstructionBreakpoint_Vector is new
     DAP_InstructionBreakpoint_Vectors
     .Vector with
   null record;
   type Access_DAP_InstructionBreakpoint_Vector is
     access all DAP_InstructionBreakpoint_Vector;

   type SourceBreakpoint;
   type Access_SourceBreakpoint is access all SourceBreakpoint;
   package DAP_SourceBreakpoint_Vectors is new Ada.Containers.Vectors
     (Positive, Access_SourceBreakpoint);
   type DAP_SourceBreakpoint_Vector is new DAP_SourceBreakpoint_Vectors
     .Vector with
   null record;
   type Access_DAP_SourceBreakpoint_Vector is
     access all DAP_SourceBreakpoint_Vector;

   type Source;
   type Access_Source is access all Source;
   package DAP_Source_Vectors is new Ada.Containers.Vectors
     (Positive, Access_Source);
   type DAP_Source_Vector is new DAP_Source_Vectors.Vector with null record;
   type Access_DAP_Source_Vector is access all DAP_Source_Vector;

   type Access_CompletionItemType is access all Enums.CompletionItemType;

   type Access_DataBreakpointAccessType is
     access all Enums.DataBreakpointAccessType;

   type Access_ExceptionBreakMode is access all Enums.ExceptionBreakMode;

   type Access_InvalidatedAreas is access all Enums.InvalidatedAreas;

   type Access_SteppingGranularity is access all Enums.SteppingGranularity;

   type AttachRequestArguments is new Ada.Finalization.Controlled with record
      a_restart : aliased LSP_Any;
   end record;
   type Access_AttachRequestArguments is access all AttachRequestArguments;

   type BreakpointLocation is new Ada.Finalization.Controlled with record
      column    : aliased LSP_Number;
      endColumn : aliased LSP_Number;
      endLine   : aliased LSP_Number;
      line      : aliased LSP_Number;
   end record;
   type Access_BreakpointLocation is access all BreakpointLocation;

   type CancelArguments is new Ada.Finalization.Controlled with record
      progressId : aliased Virtual_String;
      requestId  : aliased LSP_Number;
   end record;
   type Access_CancelArguments is access all CancelArguments;

   type ColumnDescriptor is new Ada.Finalization.Controlled with record
      attributeName : aliased Virtual_String;
      format        : aliased Virtual_String;
      label         : aliased Virtual_String;
      a_type        : aliased Virtual_String;
      width         : aliased LSP_Number;
   end record;

   type CompletionsArguments is new Ada.Finalization.Controlled with record
      column  : aliased LSP_Number;
      frameId : aliased LSP_Number;
      line    : aliased LSP_Number;
      text    : aliased Virtual_String;
   end record;
   type Access_CompletionsArguments is access all CompletionsArguments;

   type ConfigurationDoneArguments is new Ada.Finalization.Controlled with
   null record;

   type Access_ConfigurationDoneArguments is
     access all ConfigurationDoneArguments;

   type ContinueArguments is new Ada.Finalization.Controlled with record
      threadId : aliased LSP_Number;
   end record;
   type Access_ContinueArguments is access all ContinueArguments;

   type DataBreakpointInfoArguments is new Ada.Finalization.Controlled with
   record
      name               : aliased Virtual_String;
      variablesReference : aliased LSP_Number;
   end record;
   type Access_DataBreakpointInfoArguments is
     access all DataBreakpointInfoArguments;

   type DisassembleArguments is new Ada.Finalization.Controlled with record
      instructionCount  : aliased LSP_Number;
      instructionOffset : aliased LSP_Number;
      memoryReference   : aliased Virtual_String;
      offset            : aliased LSP_Number;
      resolveSymbols    : aliased Boolean;
   end record;
   type Access_DisassembleArguments is access all DisassembleArguments;

   type DisconnectArguments is new Ada.Finalization.Controlled with record
      restart           : aliased Boolean;
      terminateDebuggee : aliased Boolean;
   end record;
   type Access_DisconnectArguments is access all DisconnectArguments;

   type ExceptionBreakpointsFilter is new Ada.Finalization.Controlled with
   record
      conditionDescription : aliased Virtual_String;
      default              : aliased Boolean;
      filter               : aliased Virtual_String;
      label                : aliased Virtual_String;
      supportsCondition    : aliased Boolean;
   end record;

   type ExceptionFilterOptions is new Ada.Finalization.Controlled with record
      condition : aliased Virtual_String;
      filterId  : aliased Virtual_String;
   end record;

   type ExceptionInfoArguments is new Ada.Finalization.Controlled with record
      threadId : aliased LSP_Number;
   end record;
   type Access_ExceptionInfoArguments is access all ExceptionInfoArguments;

   type FunctionBreakpoint is new Ada.Finalization.Controlled with record
      condition    : aliased Virtual_String;
      hitCondition : aliased Virtual_String;
      name         : aliased Virtual_String;
   end record;

   type GotoArguments is new Ada.Finalization.Controlled with record
      targetId : aliased LSP_Number;
      threadId : aliased LSP_Number;
   end record;
   type Access_GotoArguments is access all GotoArguments;

   type GotoTarget is new Ada.Finalization.Controlled with record
      column                      : aliased LSP_Number;
      endColumn                   : aliased LSP_Number;
      endLine                     : aliased LSP_Number;
      id                          : aliased LSP_Number;
      instructionPointerReference : aliased Virtual_String;
      label                       : aliased Virtual_String;
      line                        : aliased LSP_Number;
   end record;
   type Access_GotoTarget is access all GotoTarget;

   type InitializeRequestArguments is new Ada.Finalization.Controlled with
   record
      adapterID                    : aliased Virtual_String;
      clientID                     : aliased Virtual_String;
      clientName                   : aliased Virtual_String;
      columnsStartAt1              : aliased Boolean;
      linesStartAt1                : aliased Boolean;
      locale                       : aliased Virtual_String;
      pathFormat                   : aliased Virtual_String;
      supportsInvalidatedEvent     : aliased Boolean;
      supportsMemoryReferences     : aliased Boolean;
      supportsProgressReporting    : aliased Boolean;
      supportsRunInTerminalRequest : aliased Boolean;
      supportsVariablePaging       : aliased Boolean;
      supportsVariableType         : aliased Boolean;
   end record;
   type Access_InitializeRequestArguments is
     access all InitializeRequestArguments;

   type InstructionBreakpoint is new Ada.Finalization.Controlled with record
      condition            : aliased Virtual_String;
      hitCondition         : aliased Virtual_String;
      instructionReference : aliased Virtual_String;
      offset               : aliased LSP_Number;
   end record;

   type LaunchRequestArguments is new Ada.Finalization.Controlled with record
      a_restart : aliased LSP_Any;
      noDebug   : aliased Boolean;
      program   : aliased Virtual_String;  --  cdt-gdb-adapter specific
   end record;
   type Access_LaunchRequestArguments is access all LaunchRequestArguments;

   type LoadedSourcesArguments is new Ada.Finalization.Controlled with
   null record;

   type Access_LoadedSourcesArguments is access all LoadedSourcesArguments;

   type Module is new Ada.Finalization.Controlled with record
      addressRange   : aliased Virtual_String;
      dateTimeStamp  : aliased Virtual_String;
      id             : aliased LSP_Number_Or_String;
      isOptimized    : aliased Boolean;
      isUserCode     : aliased Boolean;
      name           : aliased Virtual_String;
      path           : aliased Virtual_String;
      symbolFilePath : aliased Virtual_String;
      symbolStatus   : aliased Virtual_String;
      version        : aliased Virtual_String;
   end record;
   type Access_Module is access all Module;

   type ModulesArguments is new Ada.Finalization.Controlled with record
      moduleCount : aliased LSP_Number;
      startModule : aliased LSP_Number;
   end record;
   type Access_ModulesArguments is access all ModulesArguments;

   type PauseArguments is new Ada.Finalization.Controlled with record
      threadId : aliased LSP_Number;
   end record;
   type Access_PauseArguments is access all PauseArguments;

   type ProtocolMessage is new Ada.Finalization.Controlled with record
      seq    : aliased LSP_Number;
      a_type : aliased Virtual_String;
   end record;
   type Access_ProtocolMessage is access all ProtocolMessage;

   type ReadMemoryArguments is new Ada.Finalization.Controlled with record
      count           : aliased LSP_Number;
      memoryReference : aliased Virtual_String;
      offset          : aliased LSP_Number;
   end record;
   type Access_ReadMemoryArguments is access all ReadMemoryArguments;

   type RestartArguments is new Ada.Finalization.Controlled with null record;

   type Access_RestartArguments is access all RestartArguments;

   type RestartFrameArguments is new Ada.Finalization.Controlled with record
      frameId : aliased LSP_Number;
   end record;
   type Access_RestartFrameArguments is access all RestartFrameArguments;

   type ReverseContinueArguments is new Ada.Finalization.Controlled with record
      threadId : aliased LSP_Number;
   end record;
   type Access_ReverseContinueArguments is access all ReverseContinueArguments;

   type ScopesArguments is new Ada.Finalization.Controlled with record
      frameId : aliased LSP_Number;
   end record;
   type Access_ScopesArguments is access all ScopesArguments;

   type SourceBreakpoint is new Ada.Finalization.Controlled with record
      column       : aliased LSP_Number;
      condition    : aliased Virtual_String;
      hitCondition : aliased Virtual_String;
      line         : aliased LSP_Number;
      logMessage   : aliased Virtual_String;
   end record;

   type StepInTarget is new Ada.Finalization.Controlled with record
      id    : aliased LSP_Number;
      label : aliased Virtual_String;
   end record;
   type Access_StepInTarget is access all StepInTarget;

   type StepInTargetsArguments is new Ada.Finalization.Controlled with record
      frameId : aliased LSP_Number;
   end record;
   type Access_StepInTargetsArguments is access all StepInTargetsArguments;

   type TerminateArguments is new Ada.Finalization.Controlled with record
      restart : aliased Boolean;
   end record;
   type Access_TerminateArguments is access all TerminateArguments;

   type Thread is new Ada.Finalization.Controlled with record
      id   : aliased LSP_Number;
      name : aliased Virtual_String;
   end record;
   type Access_Thread is access all Thread;

   type ValueFormat is new Ada.Finalization.Controlled with record
      hex : aliased Boolean;
   end record;
   type Access_ValueFormat is access all ValueFormat;

   type Request is new Ada.Finalization.Controlled with record
      arguments : aliased LSP_Any;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_Request is access all Request;

   type AttachRequest is new Ada.Finalization.Controlled with record
      arguments : aliased AttachRequestArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_AttachRequest is access all AttachRequest;

   type Response is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      success     : aliased Boolean;
      a_type      : aliased Virtual_String;
      seq         : aliased LSP_Number;
   end record;
   type Access_Response is access all Response;

   type AttachResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_AttachResponse is access all AttachResponse;

   type Source is new Ada.Finalization.Controlled with record
      adapterData      : aliased LSP_Any;
      checksums        : aliased DAP_Checksum_Vector;
      name             : aliased Virtual_String;
      origin           : aliased Virtual_String;
      path             : aliased Virtual_String;
      presentationHint : aliased Virtual_String;
      sourceReference  : aliased LSP_Number;
      sources          : aliased DAP_Source_Vector;
   end record;

   type Breakpoint is new Ada.Finalization.Controlled with record
      column               : aliased LSP_Number;
      endColumn            : aliased LSP_Number;
      endLine              : aliased LSP_Number;
      id                   : aliased LSP_Number;
      instructionReference : aliased Virtual_String;
      line                 : aliased LSP_Number;
      message              : aliased Virtual_String;
      offset               : aliased LSP_Number;
      a_source             : aliased Source;
      verified             : aliased Boolean;
   end record;
   type Access_Breakpoint is access all Breakpoint;

   type Event is new Ada.Finalization.Controlled with record
      a_body : aliased LSP_Any;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_Event is access all Event;

   type BreakpointEvent is new Ada.Finalization.Controlled with record
      a_body : aliased DAP_String_Maps.Map;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_BreakpointEvent is access all BreakpointEvent;

   type BreakpointLocationsArguments is new Ada.Finalization.Controlled with
   record
      column    : aliased LSP_Number;
      endColumn : aliased LSP_Number;
      endLine   : aliased LSP_Number;
      line      : aliased LSP_Number;
      a_source  : aliased Source;
   end record;
   type Access_BreakpointLocationsArguments is
     access all BreakpointLocationsArguments;

   type BreakpointLocationsRequest is new Ada.Finalization.Controlled with
   record
      arguments : aliased BreakpointLocationsArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_BreakpointLocationsRequest is
     access all BreakpointLocationsRequest;

   type BreakpointLocationsResponse is new Ada.Finalization.Controlled with
   record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_BreakpointLocationsResponse is
     access all BreakpointLocationsResponse;

   type CancelRequest is new Ada.Finalization.Controlled with record
      arguments : aliased CancelArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_CancelRequest is access all CancelRequest;

   type CancelResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_CancelResponse is access all CancelResponse;

   type Capabilities is new Ada.Finalization.Controlled with record
      additionalModuleColumns            : aliased DAP_ColumnDescriptor_Vector;
      completionTriggerCharacters        : aliased DAP_String_Vector;
      exceptionBreakpointFilters : aliased
        DAP_ExceptionBreakpointsFilter_Vector;
      supportTerminateDebuggee           : aliased Boolean;
      supportedChecksumAlgorithms : aliased DAP_ChecksumAlgorithm_Vector;
      supportsBreakpointLocationsRequest : aliased Boolean;
      supportsCancelRequest              : aliased Boolean;
      supportsClipboardContext           : aliased Boolean;
      supportsCompletionsRequest         : aliased Boolean;
      supportsConditionalBreakpoints     : aliased Boolean;
      supportsConfigurationDoneRequest   : aliased Boolean;
      supportsDataBreakpoints            : aliased Boolean;
      supportsDelayedStackTraceLoading   : aliased Boolean;
      supportsDisassembleRequest         : aliased Boolean;
      supportsEvaluateForHovers          : aliased Boolean;
      supportsExceptionFilterOptions     : aliased Boolean;
      supportsExceptionInfoRequest       : aliased Boolean;
      supportsExceptionOptions           : aliased Boolean;
      supportsFunctionBreakpoints        : aliased Boolean;
      supportsGotoTargetsRequest         : aliased Boolean;
      supportsHitConditionalBreakpoints  : aliased Boolean;
      supportsInstructionBreakpoints     : aliased Boolean;
      supportsLoadedSourcesRequest       : aliased Boolean;
      supportsLogPoints                  : aliased Boolean;
      supportsModulesRequest             : aliased Boolean;
      supportsReadMemoryRequest          : aliased Boolean;
      supportsRestartFrame               : aliased Boolean;
      supportsRestartRequest             : aliased Boolean;
      supportsSetExpression              : aliased Boolean;
      supportsSetVariable                : aliased Boolean;
      supportsStepBack                   : aliased Boolean;
      supportsStepInTargetsRequest       : aliased Boolean;
      supportsSteppingGranularity        : aliased Boolean;
      supportsTerminateRequest           : aliased Boolean;
      supportsTerminateThreadsRequest    : aliased Boolean;
      supportsValueFormattingOptions     : aliased Boolean;
   end record;
   type Access_Capabilities is access all Capabilities;

   type CapabilitiesEvent is new Ada.Finalization.Controlled with record
      a_body : aliased DAP_String_Maps.Map;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_CapabilitiesEvent is access all CapabilitiesEvent;

   type Checksum is new Ada.Finalization.Controlled with record
      algorithm : aliased Enums.ChecksumAlgorithm;
      checksum  : aliased Virtual_String;
   end record;

   type CompletionItem is new Ada.Finalization.Controlled with record
      label           : aliased Virtual_String;
      length          : aliased LSP_Number;
      selectionLength : aliased LSP_Number;
      selectionStart  : aliased LSP_Number;
      sortText        : aliased Virtual_String;
      start           : aliased LSP_Number;
      text            : aliased Virtual_String;
      a_type          : aliased Enums.CompletionItemType;
   end record;
   type Access_CompletionItem is access all CompletionItem;

   type CompletionsRequest is new Ada.Finalization.Controlled with record
      arguments : aliased CompletionsArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_CompletionsRequest is access all CompletionsRequest;

   type CompletionsResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_CompletionsResponse is access all CompletionsResponse;

   type ConfigurationDoneRequest is new Ada.Finalization.Controlled with record
      arguments : aliased ConfigurationDoneArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_ConfigurationDoneRequest is access all ConfigurationDoneRequest;

   type ConfigurationDoneResponse is new Ada.Finalization.Controlled with
   record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_ConfigurationDoneResponse is
     access all ConfigurationDoneResponse;

   type ContinueRequest is new Ada.Finalization.Controlled with record
      arguments : aliased ContinueArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_ContinueRequest is access all ContinueRequest;

   type ContinueResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_ContinueResponse is access all ContinueResponse;

   type ContinuedEvent is new Ada.Finalization.Controlled with record
      a_body : aliased DAP_String_Maps.Map;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_ContinuedEvent is access all ContinuedEvent;

   type DataBreakpoint is new Ada.Finalization.Controlled with record
      accessType   : aliased Enums.DataBreakpointAccessType;
      condition    : aliased Virtual_String;
      dataId       : aliased Virtual_String;
      hitCondition : aliased Virtual_String;
   end record;

   type DataBreakpointInfoRequest is new Ada.Finalization.Controlled with
   record
      arguments : aliased DataBreakpointInfoArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_DataBreakpointInfoRequest is
     access all DataBreakpointInfoRequest;

   type DataBreakpointInfoResponse is new Ada.Finalization.Controlled with
   record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_DataBreakpointInfoResponse is
     access all DataBreakpointInfoResponse;

   type DisassembleRequest is new Ada.Finalization.Controlled with record
      arguments : aliased DisassembleArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_DisassembleRequest is access all DisassembleRequest;

   type DisassembleResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_DisassembleResponse is access all DisassembleResponse;

   type DisassembledInstruction is new Ada.Finalization.Controlled with record
      address          : aliased Virtual_String;
      column           : aliased LSP_Number;
      endColumn        : aliased LSP_Number;
      endLine          : aliased LSP_Number;
      instruction      : aliased Virtual_String;
      instructionBytes : aliased Virtual_String;
      line             : aliased LSP_Number;
      location         : aliased Source;
      symbol           : aliased Virtual_String;
   end record;
   type Access_DisassembledInstruction is access all DisassembledInstruction;

   type DisconnectRequest is new Ada.Finalization.Controlled with record
      arguments : aliased DisconnectArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_DisconnectRequest is access all DisconnectRequest;

   type DisconnectResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_DisconnectResponse is access all DisconnectResponse;

   type ErrorResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_ErrorResponse is access all ErrorResponse;

   type EvaluateArguments is new Ada.Finalization.Controlled with record
      context    : aliased Virtual_String;
      expression : aliased Virtual_String;
      format     : aliased ValueFormat;
      frameId    : aliased LSP_Number;
   end record;
   type Access_EvaluateArguments is access all EvaluateArguments;

   type EvaluateRequest is new Ada.Finalization.Controlled with record
      arguments : aliased EvaluateArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_EvaluateRequest is access all EvaluateRequest;

   type EvaluateResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_EvaluateResponse is access all EvaluateResponse;

   type ExceptionDetails is new Ada.Finalization.Controlled with record
      evaluateName   : aliased Virtual_String;
      fullTypeName   : aliased Virtual_String;
      innerException : aliased DAP_ExceptionDetails_Vector;
      message        : aliased Virtual_String;
      stackTrace     : aliased Virtual_String;
      typeName       : aliased Virtual_String;
   end record;

   type ExceptionInfoRequest is new Ada.Finalization.Controlled with record
      arguments : aliased ExceptionInfoArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_ExceptionInfoRequest is access all ExceptionInfoRequest;

   type ExceptionInfoResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_ExceptionInfoResponse is access all ExceptionInfoResponse;

   type ExceptionOptions is new Ada.Finalization.Controlled with record
      breakMode : aliased Enums.ExceptionBreakMode;
      path      : aliased DAP_ExceptionPathSegment_Vector;
   end record;

   type ExceptionPathSegment is new Ada.Finalization.Controlled with record
      names  : aliased DAP_String_Vector;
      negate : aliased Boolean;
   end record;

   type ExitedEvent is new Ada.Finalization.Controlled with record
      a_body : aliased DAP_String_Maps.Map;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_ExitedEvent is access all ExitedEvent;

   type GotoRequest is new Ada.Finalization.Controlled with record
      arguments : aliased GotoArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_GotoRequest is access all GotoRequest;

   type GotoResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_GotoResponse is access all GotoResponse;

   type GotoTargetsArguments is new Ada.Finalization.Controlled with record
      column   : aliased LSP_Number;
      line     : aliased LSP_Number;
      a_source : aliased Source;
   end record;
   type Access_GotoTargetsArguments is access all GotoTargetsArguments;

   type GotoTargetsRequest is new Ada.Finalization.Controlled with record
      arguments : aliased GotoTargetsArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_GotoTargetsRequest is access all GotoTargetsRequest;

   type GotoTargetsResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_GotoTargetsResponse is access all GotoTargetsResponse;

   type InitializeRequest is new Ada.Finalization.Controlled with record
      arguments : aliased InitializeRequestArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_InitializeRequest is access all InitializeRequest;

   type InitializeResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased Capabilities;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_InitializeResponse is access all InitializeResponse;

   type InitializedEvent is new Ada.Finalization.Controlled with record
      event  : aliased Virtual_String;
      a_body : aliased LSP_Any;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_InitializedEvent is access all InitializedEvent;

   type InvalidatedEvent is new Ada.Finalization.Controlled with record
      a_body : aliased DAP_String_Maps.Map;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_InvalidatedEvent is access all InvalidatedEvent;

   type LaunchRequest is new Ada.Finalization.Controlled with record
      arguments : aliased LaunchRequestArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_LaunchRequest is access all LaunchRequest;

   type LaunchResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_LaunchResponse is access all LaunchResponse;

   type LoadedSourceEvent is new Ada.Finalization.Controlled with record
      a_body : aliased DAP_String_Maps.Map;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_LoadedSourceEvent is access all LoadedSourceEvent;

   type LoadedSourcesRequest is new Ada.Finalization.Controlled with record
      arguments : aliased LoadedSourcesArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_LoadedSourcesRequest is access all LoadedSourcesRequest;

   type LoadedSourcesResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_LoadedSourcesResponse is access all LoadedSourcesResponse;

   type Message is new Ada.Finalization.Controlled with record
      format        : aliased Virtual_String;
      id            : aliased LSP_Number;
      sendTelemetry : aliased Boolean;
      showUser      : aliased Boolean;
      url           : aliased Virtual_String;
      urlLabel      : aliased Virtual_String;
      variables     : aliased DAP_String_Maps.Map;
   end record;
   type Access_Message is access all Message;

   type ModuleEvent is new Ada.Finalization.Controlled with record
      a_body : aliased DAP_String_Maps.Map;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_ModuleEvent is access all ModuleEvent;

   type ModulesRequest is new Ada.Finalization.Controlled with record
      arguments : aliased ModulesArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_ModulesRequest is access all ModulesRequest;

   type ModulesResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_ModulesResponse is access all ModulesResponse;

   type ModulesViewDescriptor is new Ada.Finalization.Controlled with record
      columns : aliased DAP_ColumnDescriptor_Vector;
   end record;
   type Access_ModulesViewDescriptor is access all ModulesViewDescriptor;

   type NextArguments is new Ada.Finalization.Controlled with record
      granularity : aliased Enums.SteppingGranularity;
      threadId    : aliased LSP_Number;
   end record;
   type Access_NextArguments is access all NextArguments;

   type NextRequest is new Ada.Finalization.Controlled with record
      arguments : aliased NextArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_NextRequest is access all NextRequest;

   type NextResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_NextResponse is access all NextResponse;

   type OutputEvent is new Ada.Finalization.Controlled with record
      a_body : aliased DAP_String_Maps.Map;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_OutputEvent is access all OutputEvent;

   type PauseRequest is new Ada.Finalization.Controlled with record
      arguments : aliased PauseArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_PauseRequest is access all PauseRequest;

   type PauseResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_PauseResponse is access all PauseResponse;

   type ProcessEvent is new Ada.Finalization.Controlled with record
      a_body : aliased DAP_String_Maps.Map;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_ProcessEvent is access all ProcessEvent;

   type ProgressEndEvent is new Ada.Finalization.Controlled with record
      a_body : aliased DAP_String_Maps.Map;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_ProgressEndEvent is access all ProgressEndEvent;

   type ProgressStartEvent is new Ada.Finalization.Controlled with record
      a_body : aliased DAP_String_Maps.Map;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_ProgressStartEvent is access all ProgressStartEvent;

   type ProgressUpdateEvent is new Ada.Finalization.Controlled with record
      a_body : aliased DAP_String_Maps.Map;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_ProgressUpdateEvent is access all ProgressUpdateEvent;

   type ReadMemoryRequest is new Ada.Finalization.Controlled with record
      arguments : aliased ReadMemoryArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_ReadMemoryRequest is access all ReadMemoryRequest;

   type ReadMemoryResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_ReadMemoryResponse is access all ReadMemoryResponse;

   type RestartFrameRequest is new Ada.Finalization.Controlled with record
      arguments : aliased RestartFrameArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_RestartFrameRequest is access all RestartFrameRequest;

   type RestartFrameResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_RestartFrameResponse is access all RestartFrameResponse;

   type RestartRequest is new Ada.Finalization.Controlled with record
      arguments : aliased RestartArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_RestartRequest is access all RestartRequest;

   type RestartResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_RestartResponse is access all RestartResponse;

   type ReverseContinueRequest is new Ada.Finalization.Controlled with record
      arguments : aliased ReverseContinueArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_ReverseContinueRequest is access all ReverseContinueRequest;

   type ReverseContinueResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_ReverseContinueResponse is access all ReverseContinueResponse;

   type RunInTerminalRequestArguments is new Ada.Finalization.Controlled with
   record
      args : aliased DAP_String_Vector;
      cwd  : aliased Virtual_String;
      env  : aliased DAP_String_Maps.Map;
      kind : aliased Virtual_String;
   end record;
   type Access_RunInTerminalRequestArguments is
     access all RunInTerminalRequestArguments;

   type RunInTerminalRequest is new Ada.Finalization.Controlled with record
      arguments : aliased RunInTerminalRequestArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_RunInTerminalRequest is access all RunInTerminalRequest;

   type RunInTerminalResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_RunInTerminalResponse is access all RunInTerminalResponse;

   type Scope is new Ada.Finalization.Controlled with record
      column             : aliased LSP_Number;
      endColumn          : aliased LSP_Number;
      endLine            : aliased LSP_Number;
      expensive          : aliased Boolean;
      indexedVariables   : aliased LSP_Number;
      line               : aliased LSP_Number;
      name               : aliased Virtual_String;
      namedVariables     : aliased LSP_Number;
      presentationHint   : aliased Virtual_String;
      a_source           : aliased Source;
      variablesReference : aliased LSP_Number;
   end record;
   type Access_Scope is access all Scope;

   type ScopesRequest is new Ada.Finalization.Controlled with record
      arguments : aliased ScopesArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_ScopesRequest is access all ScopesRequest;

   type ScopesResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_ScopesResponse is access all ScopesResponse;

   type SetBreakpointsArguments is new Ada.Finalization.Controlled with record
      breakpoints    : aliased DAP_SourceBreakpoint_Vector;
      lines          : aliased DAP_Integer_Vector;
      a_source       : aliased Source;
      sourceModified : aliased Boolean;
   end record;
   type Access_SetBreakpointsArguments is access all SetBreakpointsArguments;

   type SetBreakpointsRequest is new Ada.Finalization.Controlled with record
      arguments : aliased SetBreakpointsArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_SetBreakpointsRequest is access all SetBreakpointsRequest;

   type SetBreakpointsResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_SetBreakpointsResponse is access all SetBreakpointsResponse;

   type SetDataBreakpointsArguments is new Ada.Finalization.Controlled with
   record
      breakpoints : aliased DAP_DataBreakpoint_Vector;
   end record;
   type Access_SetDataBreakpointsArguments is
     access all SetDataBreakpointsArguments;

   type SetDataBreakpointsRequest is new Ada.Finalization.Controlled with
   record
      arguments : aliased SetDataBreakpointsArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_SetDataBreakpointsRequest is
     access all SetDataBreakpointsRequest;

   type SetDataBreakpointsResponse is new Ada.Finalization.Controlled with
   record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_SetDataBreakpointsResponse is
     access all SetDataBreakpointsResponse;

   type SetExceptionBreakpointsArguments is new Ada.Finalization
     .Controlled with
   record
      exceptionOptions : aliased DAP_ExceptionOptions_Vector;
      filterOptions    : aliased DAP_ExceptionFilterOptions_Vector;
      filters          : aliased DAP_String_Vector;
   end record;
   type Access_SetExceptionBreakpointsArguments is
     access all SetExceptionBreakpointsArguments;

   type SetExceptionBreakpointsRequest is new Ada.Finalization.Controlled with
   record
      arguments : aliased SetExceptionBreakpointsArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_SetExceptionBreakpointsRequest is
     access all SetExceptionBreakpointsRequest;

   type SetExceptionBreakpointsResponse is new Ada.Finalization.Controlled with
   record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_SetExceptionBreakpointsResponse is
     access all SetExceptionBreakpointsResponse;

   type SetExpressionArguments is new Ada.Finalization.Controlled with record
      expression : aliased Virtual_String;
      format     : aliased ValueFormat;
      frameId    : aliased LSP_Number;
      value      : aliased Virtual_String;
   end record;
   type Access_SetExpressionArguments is access all SetExpressionArguments;

   type SetExpressionRequest is new Ada.Finalization.Controlled with record
      arguments : aliased SetExpressionArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_SetExpressionRequest is access all SetExpressionRequest;

   type SetExpressionResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_SetExpressionResponse is access all SetExpressionResponse;

   type SetFunctionBreakpointsArguments is new Ada.Finalization.Controlled with
   record
      breakpoints : aliased DAP_FunctionBreakpoint_Vector;
   end record;
   type Access_SetFunctionBreakpointsArguments is
     access all SetFunctionBreakpointsArguments;

   type SetFunctionBreakpointsRequest is new Ada.Finalization.Controlled with
   record
      arguments : aliased SetFunctionBreakpointsArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_SetFunctionBreakpointsRequest is
     access all SetFunctionBreakpointsRequest;

   type SetFunctionBreakpointsResponse is new Ada.Finalization.Controlled with
   record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_SetFunctionBreakpointsResponse is
     access all SetFunctionBreakpointsResponse;

   type SetInstructionBreakpointsArguments is new Ada.Finalization
     .Controlled with
   record
      breakpoints : aliased DAP_InstructionBreakpoint_Vector;
   end record;
   type Access_SetInstructionBreakpointsArguments is
     access all SetInstructionBreakpointsArguments;

   type SetInstructionBreakpointsRequest is new Ada.Finalization
     .Controlled with
   record
      arguments : aliased SetInstructionBreakpointsArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_SetInstructionBreakpointsRequest is
     access all SetInstructionBreakpointsRequest;

   type SetInstructionBreakpointsResponse is new Ada.Finalization
     .Controlled with
   record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_SetInstructionBreakpointsResponse is
     access all SetInstructionBreakpointsResponse;

   type SetVariableArguments is new Ada.Finalization.Controlled with record
      format             : aliased ValueFormat;
      name               : aliased Virtual_String;
      value              : aliased Virtual_String;
      variablesReference : aliased LSP_Number;
   end record;
   type Access_SetVariableArguments is access all SetVariableArguments;

   type SetVariableRequest is new Ada.Finalization.Controlled with record
      arguments : aliased SetVariableArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_SetVariableRequest is access all SetVariableRequest;

   type SetVariableResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_SetVariableResponse is access all SetVariableResponse;

   type SourceArguments is new Ada.Finalization.Controlled with record
      a_source        : aliased Source;
      sourceReference : aliased LSP_Number;
   end record;
   type Access_SourceArguments is access all SourceArguments;

   type SourceRequest is new Ada.Finalization.Controlled with record
      arguments : aliased SourceArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_SourceRequest is access all SourceRequest;

   type SourceResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_SourceResponse is access all SourceResponse;

   type StackFrame is new Ada.Finalization.Controlled with record
      canRestart                  : aliased Boolean;
      column                      : aliased LSP_Number;
      endColumn                   : aliased LSP_Number;
      endLine                     : aliased LSP_Number;
      id                          : aliased LSP_Number;
      instructionPointerReference : aliased Virtual_String;
      line                        : aliased LSP_Number;
      moduleId                    : aliased LSP_Number_Or_String;
      name                        : aliased Virtual_String;
      presentationHint            : aliased Virtual_String;
      a_source                    : aliased Source;
   end record;
   type Access_StackFrame is access all StackFrame;

   type StackFrameFormat is new Ada.Finalization.Controlled with record
      includeAll      : aliased Boolean;
      line            : aliased Boolean;
      module          : aliased Boolean;
      parameterNames  : aliased Boolean;
      parameterTypes  : aliased Boolean;
      parameterValues : aliased Boolean;
      parameters      : aliased Boolean;
      hex             : aliased Boolean;
   end record;
   type Access_StackFrameFormat is access all StackFrameFormat;

   type StackTraceArguments is new Ada.Finalization.Controlled with record
      format     : aliased StackFrameFormat;
      levels     : aliased LSP_Number;
      startFrame : aliased LSP_Number;
      threadId   : aliased LSP_Number;
   end record;
   type Access_StackTraceArguments is access all StackTraceArguments;

   type StackTraceRequest is new Ada.Finalization.Controlled with record
      arguments : aliased StackTraceArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_StackTraceRequest is access all StackTraceRequest;

   type StackTraceResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_StackTraceResponse is access all StackTraceResponse;

   type StepBackArguments is new Ada.Finalization.Controlled with record
      granularity : aliased Enums.SteppingGranularity;
      threadId    : aliased LSP_Number;
   end record;
   type Access_StepBackArguments is access all StepBackArguments;

   type StepBackRequest is new Ada.Finalization.Controlled with record
      arguments : aliased StepBackArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_StepBackRequest is access all StepBackRequest;

   type StepBackResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_StepBackResponse is access all StepBackResponse;

   type StepInArguments is new Ada.Finalization.Controlled with record
      granularity : aliased Enums.SteppingGranularity;
      targetId    : aliased LSP_Number;
      threadId    : aliased LSP_Number;
   end record;
   type Access_StepInArguments is access all StepInArguments;

   type StepInRequest is new Ada.Finalization.Controlled with record
      arguments : aliased StepInArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_StepInRequest is access all StepInRequest;

   type StepInResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_StepInResponse is access all StepInResponse;

   type StepInTargetsRequest is new Ada.Finalization.Controlled with record
      arguments : aliased StepInTargetsArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_StepInTargetsRequest is access all StepInTargetsRequest;

   type StepInTargetsResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_StepInTargetsResponse is access all StepInTargetsResponse;

   type StepOutArguments is new Ada.Finalization.Controlled with record
      granularity : aliased Enums.SteppingGranularity;
      threadId    : aliased LSP_Number;
   end record;
   type Access_StepOutArguments is access all StepOutArguments;

   type StepOutRequest is new Ada.Finalization.Controlled with record
      arguments : aliased StepOutArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_StepOutRequest is access all StepOutRequest;

   type StepOutResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_StepOutResponse is access all StepOutResponse;

   type StoppedEvent is new Ada.Finalization.Controlled with record
      a_body : aliased DAP_String_Maps.Map;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_StoppedEvent is access all StoppedEvent;

   type TerminateRequest is new Ada.Finalization.Controlled with record
      arguments : aliased TerminateArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_TerminateRequest is access all TerminateRequest;

   type TerminateResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_TerminateResponse is access all TerminateResponse;

   type TerminateThreadsArguments is new Ada.Finalization.Controlled with
   record
      threadIds : aliased DAP_Integer_Vector;
   end record;
   type Access_TerminateThreadsArguments is
     access all TerminateThreadsArguments;

   type TerminateThreadsRequest is new Ada.Finalization.Controlled with record
      arguments : aliased TerminateThreadsArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_TerminateThreadsRequest is access all TerminateThreadsRequest;

   type TerminateThreadsResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased LSP_Any;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_TerminateThreadsResponse is access all TerminateThreadsResponse;

   type TerminatedEvent is new Ada.Finalization.Controlled with record
      a_body : aliased DAP_String_Maps.Map;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_TerminatedEvent is access all TerminatedEvent;

   type ThreadEvent is new Ada.Finalization.Controlled with record
      a_body : aliased DAP_String_Maps.Map;
      event  : aliased Virtual_String;
      a_type : aliased Virtual_String;
      seq    : aliased LSP_Number;
   end record;
   type Access_ThreadEvent is access all ThreadEvent;

   type ThreadsRequest is new Ada.Finalization.Controlled with record
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      arguments : aliased LSP_Any;
      seq       : aliased LSP_Number;
   end record;
   type Access_ThreadsRequest is access all ThreadsRequest;

   type ThreadsResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_ThreadsResponse is access all ThreadsResponse;

   type VariablePresentationHint is new Ada.Finalization.Controlled with record
      attributes : aliased DAP_String_Vector;
      kind       : aliased Virtual_String;
      visibility : aliased Virtual_String;
   end record;
   type Access_VariablePresentationHint is access all VariablePresentationHint;

   type Variable is new Ada.Finalization.Controlled with record
      evaluateName       : aliased Virtual_String;
      indexedVariables   : aliased LSP_Number;
      memoryReference    : aliased Virtual_String;
      name               : aliased Virtual_String;
      namedVariables     : aliased LSP_Number;
      presentationHint   : aliased VariablePresentationHint;
      a_type             : aliased Virtual_String;
      value              : aliased Virtual_String;
      variablesReference : aliased LSP_Number;
   end record;
   type Access_Variable is access all Variable;

   type VariablesArguments is new Ada.Finalization.Controlled with record
      count              : aliased LSP_Number;
      filter             : aliased Virtual_String;
      format             : aliased ValueFormat;
      start              : aliased LSP_Number;
      variablesReference : aliased LSP_Number;
   end record;
   type Access_VariablesArguments is access all VariablesArguments;

   type VariablesRequest is new Ada.Finalization.Controlled with record
      arguments : aliased VariablesArguments;
      command   : aliased Virtual_String;
      a_type    : aliased Virtual_String;
      seq       : aliased LSP_Number;
   end record;
   type Access_VariablesRequest is access all VariablesRequest;

   type VariablesResponse is new Ada.Finalization.Controlled with record
      a_body      : aliased DAP_String_Maps.Map;
      a_type      : aliased Virtual_String;
      command     : aliased Virtual_String;
      message     : aliased Virtual_String;
      request_seq : aliased LSP_Number;
      seq         : aliased LSP_Number;
      success     : aliased Boolean;
   end record;
   type Access_VariablesResponse is access all VariablesResponse;

   overriding procedure Finalize (Self : in out Capabilities);

   overriding procedure Finalize (Self : in out ExceptionDetails);

   overriding procedure Finalize (Self : in out ExceptionOptions);

   overriding procedure Finalize (Self : in out ExceptionPathSegment);

   overriding procedure Finalize (Self : in out ModulesViewDescriptor);

   overriding procedure Finalize (Self : in out RunInTerminalRequestArguments);

   overriding procedure Finalize (Self : in out SetBreakpointsArguments);

   overriding procedure Finalize (Self : in out SetDataBreakpointsArguments);

   overriding procedure Finalize
     (Self : in out SetExceptionBreakpointsArguments);

   overriding procedure Finalize
     (Self : in out SetFunctionBreakpointsArguments);

   overriding procedure Finalize
     (Self : in out SetInstructionBreakpointsArguments);

   overriding procedure Finalize (Self : in out Source);

   overriding procedure Finalize (Self : in out TerminateThreadsArguments);

   overriding procedure Finalize (Self : in out VariablePresentationHint);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Enums.ChecksumAlgorithm, Name => Access_ChecksumAlgorithm);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Checksum, Name => Access_Checksum);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ColumnDescriptor, Name => Access_ColumnDescriptor);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => DataBreakpoint, Name => Access_DataBreakpoint);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ExceptionBreakpointsFilter,
      Name   => Access_ExceptionBreakpointsFilter);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ExceptionDetails, Name => Access_ExceptionDetails);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ExceptionFilterOptions, Name => Access_ExceptionFilterOptions);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ExceptionOptions, Name => Access_ExceptionOptions);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ExceptionPathSegment, Name => Access_ExceptionPathSegment);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => FunctionBreakpoint, Name => Access_FunctionBreakpoint);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => InstructionBreakpoint, Name => Access_InstructionBreakpoint);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => SourceBreakpoint, Name => Access_SourceBreakpoint);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Source, Name => Access_Source);

   procedure Read_AttachRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_AttachRequest);

   procedure Read_AttachRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_AttachRequestArguments);

   procedure Read_AttachResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_AttachResponse);

   procedure Read_Breakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Breakpoint);

   procedure Read_BreakpointEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointEvent);

   procedure Read_BreakpointLocation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocation);

   procedure Read_BreakpointLocationsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocationsArguments);

   procedure Read_BreakpointLocationsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocationsRequest);

   procedure Read_BreakpointLocationsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocationsResponse);

   procedure Read_CancelArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CancelArguments);

   procedure Read_CancelRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_CancelRequest);

   procedure Read_CancelResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CancelResponse);

   procedure Read_Capabilities
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Capabilities);

   procedure Read_CapabilitiesEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CapabilitiesEvent);

   procedure Read_Checksum
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Checksum);

   procedure Read_ColumnDescriptor
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ColumnDescriptor);

   procedure Read_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionItem);

   procedure Read_CompletionsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionsArguments);

   procedure Read_CompletionsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionsRequest);

   procedure Read_CompletionsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionsResponse);

   procedure Read_ConfigurationDoneArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ConfigurationDoneArguments);

   procedure Read_ConfigurationDoneRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ConfigurationDoneRequest);

   procedure Read_ConfigurationDoneResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ConfigurationDoneResponse);

   procedure Read_ContinueArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ContinueArguments);

   procedure Read_ContinueRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ContinueRequest);

   procedure Read_ContinueResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ContinueResponse);

   procedure Read_ContinuedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ContinuedEvent);

   procedure Read_DAP_ChecksumAlgorithm_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ChecksumAlgorithm_Vector);

   procedure Read_DAP_Checksum_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_Checksum_Vector);

   procedure Read_DAP_ColumnDescriptor_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ColumnDescriptor_Vector);

   procedure Read_DAP_DataBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_DataBreakpoint_Vector);

   procedure Read_DAP_ExceptionBreakpointsFilter_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionBreakpointsFilter_Vector);

   procedure Read_DAP_ExceptionDetails_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionDetails_Vector);

   procedure Read_DAP_ExceptionFilterOptions_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionFilterOptions_Vector);

   procedure Read_DAP_ExceptionOptions_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionOptions_Vector);

   procedure Read_DAP_ExceptionPathSegment_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionPathSegment_Vector);

   procedure Read_DAP_FunctionBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_FunctionBreakpoint_Vector);

   procedure Read_DAP_InstructionBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_InstructionBreakpoint_Vector);

   procedure Read_DAP_Integer_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_Integer_Vector);

   procedure Read_DAP_SourceBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_SourceBreakpoint_Vector);

   procedure Read_DAP_Source_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_Source_Vector);

   procedure Read_DAP_String_Map
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_String_Map);

   procedure Read_DAP_String_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_String_Vector);

   procedure Read_DataBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpoint);

   procedure Read_DataBreakpointInfoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointInfoArguments);

   procedure Read_DataBreakpointInfoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointInfoRequest);

   procedure Read_DataBreakpointInfoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointInfoResponse);

   procedure Read_DisassembleArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembleArguments);

   procedure Read_DisassembleRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembleRequest);

   procedure Read_DisassembleResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembleResponse);

   procedure Read_DisassembledInstruction
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembledInstruction);

   procedure Read_DisconnectArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisconnectArguments);

   procedure Read_DisconnectRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisconnectRequest);

   procedure Read_DisconnectResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisconnectResponse);

   procedure Read_ChecksumAlgorithm
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ChecksumAlgorithm);

   procedure Read_CompletionItemType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionItemType);

   procedure Read_DataBreakpointAccessType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointAccessType);

   procedure Read_ExceptionBreakMode
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionBreakMode);

   procedure Read_InvalidatedAreas
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InvalidatedAreas);

   procedure Read_SteppingGranularity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SteppingGranularity);

   procedure Read_ErrorResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ErrorResponse);

   procedure Read_EvaluateArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_EvaluateArguments);

   procedure Read_EvaluateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_EvaluateRequest);

   procedure Read_EvaluateResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_EvaluateResponse);

   procedure Read_Event
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Event);

   procedure Read_ExceptionBreakpointsFilter
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionBreakpointsFilter);

   procedure Read_ExceptionDetails
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionDetails);

   procedure Read_ExceptionFilterOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionFilterOptions);

   procedure Read_ExceptionInfoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionInfoArguments);

   procedure Read_ExceptionInfoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionInfoRequest);

   procedure Read_ExceptionInfoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionInfoResponse);

   procedure Read_ExceptionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionOptions);

   procedure Read_ExceptionPathSegment
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionPathSegment);

   procedure Read_ExitedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ExitedEvent);

   procedure Read_FunctionBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_FunctionBreakpoint);

   procedure Read_GotoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoArguments);

   procedure Read_GotoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoRequest);

   procedure Read_GotoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoResponse);

   procedure Read_GotoTarget
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoTarget);

   procedure Read_GotoTargetsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_GotoTargetsArguments);

   procedure Read_GotoTargetsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_GotoTargetsRequest);

   procedure Read_GotoTargetsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_GotoTargetsResponse);

   procedure Read_InitializeRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializeRequest);

   procedure Read_InitializeRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializeRequestArguments);

   procedure Read_InitializeResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializeResponse);

   procedure Read_InitializedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializedEvent);

   procedure Read_InstructionBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InstructionBreakpoint);

   procedure Read_InvalidatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InvalidatedEvent);

   procedure Read_LaunchRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_LaunchRequest);

   procedure Read_LaunchRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LaunchRequestArguments);

   procedure Read_LaunchResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LaunchResponse);

   procedure Read_LoadedSourceEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourceEvent);

   procedure Read_LoadedSourcesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourcesArguments);

   procedure Read_LoadedSourcesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourcesRequest);

   procedure Read_LoadedSourcesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourcesResponse);

   procedure Read_Message
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Message);

   procedure Read_Module
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Module);

   procedure Read_ModuleEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ModuleEvent);

   procedure Read_ModulesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ModulesArguments);

   procedure Read_ModulesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ModulesRequest);

   procedure Read_ModulesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ModulesResponse);

   procedure Read_ModulesViewDescriptor
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ModulesViewDescriptor);

   procedure Read_NextArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_NextArguments);

   procedure Read_NextRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_NextRequest);

   procedure Read_NextResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_NextResponse);

   procedure Read_OutputEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_OutputEvent);

   procedure Read_PauseArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_PauseArguments);

   procedure Read_PauseRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_PauseRequest);

   procedure Read_PauseResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_PauseResponse);

   procedure Read_ProcessEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ProcessEvent);

   procedure Read_ProgressEndEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProgressEndEvent);

   procedure Read_ProgressStartEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProgressStartEvent);

   procedure Read_ProgressUpdateEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProgressUpdateEvent);

   procedure Read_ProtocolMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProtocolMessage);

   procedure Read_ReadMemoryArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReadMemoryArguments);

   procedure Read_ReadMemoryRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReadMemoryRequest);

   procedure Read_ReadMemoryResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReadMemoryResponse);

   procedure Read_Request
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Request);

   procedure Read_Response
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Response);

   procedure Read_RestartArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartArguments);

   procedure Read_RestartFrameArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartFrameArguments);

   procedure Read_RestartFrameRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartFrameRequest);

   procedure Read_RestartFrameResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartFrameResponse);

   procedure Read_RestartRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartRequest);

   procedure Read_RestartResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartResponse);

   procedure Read_ReverseContinueArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReverseContinueArguments);

   procedure Read_ReverseContinueRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReverseContinueRequest);

   procedure Read_ReverseContinueResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReverseContinueResponse);

   procedure Read_RunInTerminalRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RunInTerminalRequest);

   procedure Read_RunInTerminalRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RunInTerminalRequestArguments);

   procedure Read_RunInTerminalResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RunInTerminalResponse);

   procedure Read_Scope
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Scope);

   procedure Read_ScopesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ScopesArguments);

   procedure Read_ScopesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ScopesRequest);

   procedure Read_ScopesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ScopesResponse);

   procedure Read_SetBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetBreakpointsArguments);

   procedure Read_SetBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetBreakpointsRequest);

   procedure Read_SetBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetBreakpointsResponse);

   procedure Read_SetDataBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetDataBreakpointsArguments);

   procedure Read_SetDataBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetDataBreakpointsRequest);

   procedure Read_SetDataBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetDataBreakpointsResponse);

   procedure Read_SetExceptionBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExceptionBreakpointsArguments);

   procedure Read_SetExceptionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExceptionBreakpointsRequest);

   procedure Read_SetExceptionBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExceptionBreakpointsResponse);

   procedure Read_SetExpressionArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExpressionArguments);

   procedure Read_SetExpressionRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExpressionRequest);

   procedure Read_SetExpressionResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExpressionResponse);

   procedure Read_SetFunctionBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetFunctionBreakpointsArguments);

   procedure Read_SetFunctionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetFunctionBreakpointsRequest);

   procedure Read_SetFunctionBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetFunctionBreakpointsResponse);

   procedure Read_SetInstructionBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetInstructionBreakpointsArguments);

   procedure Read_SetInstructionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetInstructionBreakpointsRequest);

   procedure Read_SetInstructionBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetInstructionBreakpointsResponse);

   procedure Read_SetVariableArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetVariableArguments);

   procedure Read_SetVariableRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetVariableRequest);

   procedure Read_SetVariableResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetVariableResponse);

   procedure Read_Source
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Source);

   procedure Read_SourceArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SourceArguments);

   procedure Read_SourceBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SourceBreakpoint);

   procedure Read_SourceRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_SourceRequest);

   procedure Read_SourceResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SourceResponse);

   procedure Read_StackFrame
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StackFrame);

   procedure Read_StackFrameFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackFrameFormat);

   procedure Read_StackTraceArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackTraceArguments);

   procedure Read_StackTraceRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackTraceRequest);

   procedure Read_StackTraceResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackTraceResponse);

   procedure Read_StepBackArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepBackArguments);

   procedure Read_StepBackRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepBackRequest);

   procedure Read_StepBackResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepBackResponse);

   procedure Read_StepInArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInArguments);

   procedure Read_StepInRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StepInRequest);

   procedure Read_StepInResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInResponse);

   procedure Read_StepInTarget
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StepInTarget);

   procedure Read_StepInTargetsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInTargetsArguments);

   procedure Read_StepInTargetsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInTargetsRequest);

   procedure Read_StepInTargetsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInTargetsResponse);

   procedure Read_StepOutArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepOutArguments);

   procedure Read_StepOutRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepOutRequest);

   procedure Read_StepOutResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepOutResponse);

   procedure Read_StoppedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StoppedEvent);

   procedure Read_TerminateArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateArguments);

   procedure Read_TerminateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateRequest);

   procedure Read_TerminateResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateResponse);

   procedure Read_TerminateThreadsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateThreadsArguments);

   procedure Read_TerminateThreadsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateThreadsRequest);

   procedure Read_TerminateThreadsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateThreadsResponse);

   procedure Read_TerminatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminatedEvent);

   procedure Read_Thread
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Thread);

   procedure Read_ThreadEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ThreadEvent);

   procedure Read_ThreadsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ThreadsRequest);

   procedure Read_ThreadsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ThreadsResponse);

   procedure Read_ValueFormat
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ValueFormat);

   procedure Read_Variable
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Variable);

   procedure Read_VariablePresentationHint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablePresentationHint);

   procedure Read_VariablesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablesArguments);

   procedure Read_VariablesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablesRequest);

   procedure Read_VariablesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablesResponse);

   procedure Write_AttachRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_AttachRequest);

   procedure Write_AttachRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_AttachRequestArguments);

   procedure Write_AttachResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_AttachResponse);

   procedure Write_Breakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Breakpoint);

   procedure Write_BreakpointEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointEvent);

   procedure Write_BreakpointLocation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocation);

   procedure Write_BreakpointLocationsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocationsArguments);

   procedure Write_BreakpointLocationsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocationsRequest);

   procedure Write_BreakpointLocationsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocationsResponse);

   procedure Write_CancelArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CancelArguments);

   procedure Write_CancelRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_CancelRequest);

   procedure Write_CancelResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CancelResponse);

   procedure Write_Capabilities
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Capabilities);

   procedure Write_CapabilitiesEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CapabilitiesEvent);

   procedure Write_Checksum
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Checksum);

   procedure Write_ColumnDescriptor
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ColumnDescriptor);

   procedure Write_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionItem);

   procedure Write_CompletionsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionsArguments);

   procedure Write_CompletionsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionsRequest);

   procedure Write_CompletionsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionsResponse);

   procedure Write_ConfigurationDoneArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ConfigurationDoneArguments);

   procedure Write_ConfigurationDoneRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ConfigurationDoneRequest);

   procedure Write_ConfigurationDoneResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ConfigurationDoneResponse);

   procedure Write_ContinueArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ContinueArguments);

   procedure Write_ContinueRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ContinueRequest);

   procedure Write_ContinueResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ContinueResponse);

   procedure Write_ContinuedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ContinuedEvent);

   procedure Write_DAP_ChecksumAlgorithm_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ChecksumAlgorithm_Vector);

   procedure Write_DAP_Checksum_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_Checksum_Vector);

   procedure Write_DAP_ColumnDescriptor_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ColumnDescriptor_Vector);

   procedure Write_DAP_DataBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_DataBreakpoint_Vector);

   procedure Write_DAP_ExceptionBreakpointsFilter_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionBreakpointsFilter_Vector);

   procedure Write_DAP_ExceptionDetails_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionDetails_Vector);

   procedure Write_DAP_ExceptionFilterOptions_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionFilterOptions_Vector);

   procedure Write_DAP_ExceptionOptions_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionOptions_Vector);

   procedure Write_DAP_ExceptionPathSegment_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionPathSegment_Vector);

   procedure Write_DAP_FunctionBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_FunctionBreakpoint_Vector);

   procedure Write_DAP_InstructionBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_InstructionBreakpoint_Vector);

   procedure Write_DAP_Integer_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_Integer_Vector);

   procedure Write_DAP_SourceBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_SourceBreakpoint_Vector);

   procedure Write_DAP_Source_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_Source_Vector);

   procedure Write_DAP_String_Map
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_String_Map);

   procedure Write_DAP_String_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_String_Vector);

   procedure Write_DataBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpoint);

   procedure Write_DataBreakpointInfoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointInfoArguments);

   procedure Write_DataBreakpointInfoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointInfoRequest);

   procedure Write_DataBreakpointInfoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointInfoResponse);

   procedure Write_DisassembleArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembleArguments);

   procedure Write_DisassembleRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembleRequest);

   procedure Write_DisassembleResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembleResponse);

   procedure Write_DisassembledInstruction
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembledInstruction);

   procedure Write_DisconnectArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisconnectArguments);

   procedure Write_DisconnectRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisconnectRequest);

   procedure Write_DisconnectResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisconnectResponse);

   procedure Write_ChecksumAlgorithm
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ChecksumAlgorithm);

   procedure Write_CompletionItemType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionItemType);

   procedure Write_DataBreakpointAccessType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointAccessType);

   procedure Write_ExceptionBreakMode
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionBreakMode);

   procedure Write_InvalidatedAreas
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InvalidatedAreas);

   procedure Write_SteppingGranularity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SteppingGranularity);

   procedure Write_ErrorResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ErrorResponse);

   procedure Write_EvaluateArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_EvaluateArguments);

   procedure Write_EvaluateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_EvaluateRequest);

   procedure Write_EvaluateResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_EvaluateResponse);

   procedure Write_Event
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Event);

   procedure Write_ExceptionBreakpointsFilter
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionBreakpointsFilter);

   procedure Write_ExceptionDetails
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionDetails);

   procedure Write_ExceptionFilterOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionFilterOptions);

   procedure Write_ExceptionInfoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionInfoArguments);

   procedure Write_ExceptionInfoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionInfoRequest);

   procedure Write_ExceptionInfoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionInfoResponse);

   procedure Write_ExceptionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionOptions);

   procedure Write_ExceptionPathSegment
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionPathSegment);

   procedure Write_ExitedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ExitedEvent);

   procedure Write_FunctionBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_FunctionBreakpoint);

   procedure Write_GotoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoArguments);

   procedure Write_GotoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoRequest);

   procedure Write_GotoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoResponse);

   procedure Write_GotoTarget
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoTarget);

   procedure Write_GotoTargetsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_GotoTargetsArguments);

   procedure Write_GotoTargetsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_GotoTargetsRequest);

   procedure Write_GotoTargetsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_GotoTargetsResponse);

   procedure Write_InitializeRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializeRequest);

   procedure Write_InitializeRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializeRequestArguments);

   procedure Write_InitializeResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializeResponse);

   procedure Write_InitializedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializedEvent);

   procedure Write_InstructionBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InstructionBreakpoint);

   procedure Write_InvalidatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InvalidatedEvent);

   procedure Write_LaunchRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_LaunchRequest);

   procedure Write_LaunchRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LaunchRequestArguments);

   procedure Write_LaunchResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LaunchResponse);

   procedure Write_LoadedSourceEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourceEvent);

   procedure Write_LoadedSourcesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourcesArguments);

   procedure Write_LoadedSourcesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourcesRequest);

   procedure Write_LoadedSourcesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourcesResponse);

   procedure Write_Message
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Message);

   procedure Write_Module
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Module);

   procedure Write_ModuleEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ModuleEvent);

   procedure Write_ModulesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ModulesArguments);

   procedure Write_ModulesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ModulesRequest);

   procedure Write_ModulesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ModulesResponse);

   procedure Write_ModulesViewDescriptor
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ModulesViewDescriptor);

   procedure Write_NextArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_NextArguments);

   procedure Write_NextRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_NextRequest);

   procedure Write_NextResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_NextResponse);

   procedure Write_OutputEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_OutputEvent);

   procedure Write_PauseArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_PauseArguments);

   procedure Write_PauseRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_PauseRequest);

   procedure Write_PauseResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_PauseResponse);

   procedure Write_ProcessEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ProcessEvent);

   procedure Write_ProgressEndEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProgressEndEvent);

   procedure Write_ProgressStartEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProgressStartEvent);

   procedure Write_ProgressUpdateEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProgressUpdateEvent);

   procedure Write_ProtocolMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProtocolMessage);

   procedure Write_ReadMemoryArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReadMemoryArguments);

   procedure Write_ReadMemoryRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReadMemoryRequest);

   procedure Write_ReadMemoryResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReadMemoryResponse);

   procedure Write_Request
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Request);

   procedure Write_Response
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Response);

   procedure Write_RestartArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartArguments);

   procedure Write_RestartFrameArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartFrameArguments);

   procedure Write_RestartFrameRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartFrameRequest);

   procedure Write_RestartFrameResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartFrameResponse);

   procedure Write_RestartRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartRequest);

   procedure Write_RestartResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartResponse);

   procedure Write_ReverseContinueArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReverseContinueArguments);

   procedure Write_ReverseContinueRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReverseContinueRequest);

   procedure Write_ReverseContinueResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReverseContinueResponse);

   procedure Write_RunInTerminalRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RunInTerminalRequest);

   procedure Write_RunInTerminalRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RunInTerminalRequestArguments);

   procedure Write_RunInTerminalResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RunInTerminalResponse);

   procedure Write_Scope
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Scope);

   procedure Write_ScopesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ScopesArguments);

   procedure Write_ScopesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ScopesRequest);

   procedure Write_ScopesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ScopesResponse);

   procedure Write_SetBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetBreakpointsArguments);

   procedure Write_SetBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetBreakpointsRequest);

   procedure Write_SetBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetBreakpointsResponse);

   procedure Write_SetDataBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetDataBreakpointsArguments);

   procedure Write_SetDataBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetDataBreakpointsRequest);

   procedure Write_SetDataBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetDataBreakpointsResponse);

   procedure Write_SetExceptionBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExceptionBreakpointsArguments);

   procedure Write_SetExceptionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExceptionBreakpointsRequest);

   procedure Write_SetExceptionBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExceptionBreakpointsResponse);

   procedure Write_SetExpressionArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExpressionArguments);

   procedure Write_SetExpressionRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExpressionRequest);

   procedure Write_SetExpressionResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExpressionResponse);

   procedure Write_SetFunctionBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetFunctionBreakpointsArguments);

   procedure Write_SetFunctionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetFunctionBreakpointsRequest);

   procedure Write_SetFunctionBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetFunctionBreakpointsResponse);

   procedure Write_SetInstructionBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetInstructionBreakpointsArguments);

   procedure Write_SetInstructionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetInstructionBreakpointsRequest);

   procedure Write_SetInstructionBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetInstructionBreakpointsResponse);

   procedure Write_SetVariableArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetVariableArguments);

   procedure Write_SetVariableRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetVariableRequest);

   procedure Write_SetVariableResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetVariableResponse);

   procedure Write_Source
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Source);

   procedure Write_SourceArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SourceArguments);

   procedure Write_SourceBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SourceBreakpoint);

   procedure Write_SourceRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_SourceRequest);

   procedure Write_SourceResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SourceResponse);

   procedure Write_StackFrame
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StackFrame);

   procedure Write_StackFrameFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackFrameFormat);

   procedure Write_StackTraceArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackTraceArguments);

   procedure Write_StackTraceRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackTraceRequest);

   procedure Write_StackTraceResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackTraceResponse);

   procedure Write_StepBackArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepBackArguments);

   procedure Write_StepBackRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepBackRequest);

   procedure Write_StepBackResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepBackResponse);

   procedure Write_StepInArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInArguments);

   procedure Write_StepInRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StepInRequest);

   procedure Write_StepInResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInResponse);

   procedure Write_StepInTarget
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StepInTarget);

   procedure Write_StepInTargetsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInTargetsArguments);

   procedure Write_StepInTargetsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInTargetsRequest);

   procedure Write_StepInTargetsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInTargetsResponse);

   procedure Write_StepOutArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepOutArguments);

   procedure Write_StepOutRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepOutRequest);

   procedure Write_StepOutResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepOutResponse);

   procedure Write_StoppedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StoppedEvent);

   procedure Write_TerminateArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateArguments);

   procedure Write_TerminateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateRequest);

   procedure Write_TerminateResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateResponse);

   procedure Write_TerminateThreadsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateThreadsArguments);

   procedure Write_TerminateThreadsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateThreadsRequest);

   procedure Write_TerminateThreadsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateThreadsResponse);

   procedure Write_TerminatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminatedEvent);

   procedure Write_Thread
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Thread);

   procedure Write_ThreadEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ThreadEvent);

   procedure Write_ThreadsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ThreadsRequest);

   procedure Write_ThreadsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ThreadsResponse);

   procedure Write_ValueFormat
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ValueFormat);

   procedure Write_Variable
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Variable);

   procedure Write_VariablePresentationHint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablePresentationHint);

   procedure Write_VariablesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablesArguments);

   procedure Write_VariablesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablesRequest);

   procedure Write_VariablesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablesResponse);

end DAP.Tools;
