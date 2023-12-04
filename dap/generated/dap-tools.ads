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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with VSS.JSON.Streams;
with VSS.Strings;
with VSS.String_Vectors;

package DAP.Tools is
   package JSON_Event_Lists is new Ada.Containers.Doubly_Linked_Lists
     (VSS.JSON.Streams.JSON_Stream_Element, VSS.JSON.Streams."=");

   type Any_Value is new JSON_Event_Lists.List with null record;
   type Any_Object is new Any_Value with null record;

   type Optional_Integer (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Integer;
         when False =>
            null;
      end case;
   end record;

   type Optional_Float (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Float;
         when False =>
            null;
      end case;
   end record;

   type Integer_Or_String (Is_String : Boolean := False) is record
      case Is_String is
         when False =>
            Integer : Standard.Integer;
         when True =>
            String : VSS.Strings.Virtual_String;
      end case;
   end record;

   type Optional_Integer_Or_String (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Integer_Or_String;
         when False =>
            null;
      end case;
   end record;

   type Thread_Vector is tagged private with
     Variable_Indexing => Get_Thread_Variable_Reference,
     Constant_Indexing => Get_Thread_Constant_Reference;

   type Checksum_Vector is tagged private with
     Variable_Indexing => Get_Checksum_Variable_Reference,
     Constant_Indexing => Get_Checksum_Constant_Reference;

   type Breakpoint_Vector is tagged private with
     Variable_Indexing => Get_Breakpoint_Variable_Reference,
     Constant_Indexing => Get_Breakpoint_Constant_Reference;

   type StepInTarget_Vector is tagged private with
     Variable_Indexing => Get_StepInTarget_Variable_Reference,
     Constant_Indexing => Get_StepInTarget_Constant_Reference;

   type FunctionBreakpoint_Vector is tagged private with
     Variable_Indexing => Get_FunctionBreakpoint_Variable_Reference,
     Constant_Indexing => Get_FunctionBreakpoint_Constant_Reference;

   type DataBreakpoint_Vector is tagged private with
     Variable_Indexing => Get_DataBreakpoint_Variable_Reference,
     Constant_Indexing => Get_DataBreakpoint_Constant_Reference;

   type ExceptionOptions_Vector is tagged private with
     Variable_Indexing => Get_ExceptionOptions_Variable_Reference,
     Constant_Indexing => Get_ExceptionOptions_Constant_Reference;

   type Integer_Vector is tagged private with
     Variable_Indexing => Get_Integer_Variable_Reference,
     Constant_Indexing => Get_Integer_Constant_Reference;

   type GotoTarget_Vector is tagged private with
     Variable_Indexing => Get_GotoTarget_Variable_Reference,
     Constant_Indexing => Get_GotoTarget_Constant_Reference;

   type InvalidatedAreas_Vector is tagged private with
     Variable_Indexing => Get_InvalidatedAreas_Variable_Reference,
     Constant_Indexing => Get_InvalidatedAreas_Constant_Reference;

   type BreakpointLocation_Vector is tagged private with
     Variable_Indexing => Get_BreakpointLocation_Variable_Reference,
     Constant_Indexing => Get_BreakpointLocation_Constant_Reference;

   type InstructionBreakpoint_Vector is tagged private with
     Variable_Indexing => Get_InstructionBreakpoint_Variable_Reference,
     Constant_Indexing => Get_InstructionBreakpoint_Constant_Reference;

   type StackFrame_Vector is tagged private with
     Variable_Indexing => Get_StackFrame_Variable_Reference,
     Constant_Indexing => Get_StackFrame_Constant_Reference;

   type Scope_Vector is tagged private with
     Variable_Indexing => Get_Scope_Variable_Reference,
     Constant_Indexing => Get_Scope_Constant_Reference;

   type Variable_Vector is tagged private with
     Variable_Indexing => Get_Variable_Variable_Reference,
     Constant_Indexing => Get_Variable_Constant_Reference;

   type Source_Vector is tagged private with
     Variable_Indexing => Get_Source_Variable_Reference,
     Constant_Indexing => Get_Source_Constant_Reference;

   type SourceBreakpoint_Vector is tagged private with
     Variable_Indexing => Get_SourceBreakpoint_Variable_Reference,
     Constant_Indexing => Get_SourceBreakpoint_Constant_Reference;

   type ChecksumAlgorithm_Vector is tagged private with
     Variable_Indexing => Get_ChecksumAlgorithm_Variable_Reference,
     Constant_Indexing => Get_ChecksumAlgorithm_Constant_Reference;

   type ExceptionBreakpointsFilter_Vector is tagged private with
     Variable_Indexing => Get_ExceptionBreakpointsFilter_Variable_Reference,
     Constant_Indexing => Get_ExceptionBreakpointsFilter_Constant_Reference;

   type CompletionItem_Vector is tagged private with
     Variable_Indexing => Get_CompletionItem_Variable_Reference,
     Constant_Indexing => Get_CompletionItem_Constant_Reference;

   type ExceptionPathSegment_Vector is tagged private with
     Variable_Indexing => Get_ExceptionPathSegment_Variable_Reference,
     Constant_Indexing => Get_ExceptionPathSegment_Constant_Reference;

   type DataBreakpointAccessType_Vector is tagged private with
     Variable_Indexing => Get_DataBreakpointAccessType_Variable_Reference,
     Constant_Indexing => Get_DataBreakpointAccessType_Constant_Reference;

   type DisassembledInstruction_Vector is tagged private with
     Variable_Indexing => Get_DisassembledInstruction_Variable_Reference,
     Constant_Indexing => Get_DisassembledInstruction_Constant_Reference;

   type Module_Vector is tagged private with
     Variable_Indexing => Get_Module_Variable_Reference,
     Constant_Indexing => Get_Module_Constant_Reference;

   type ExceptionFilterOptions_Vector is tagged private with
     Variable_Indexing => Get_ExceptionFilterOptions_Variable_Reference,
     Constant_Indexing => Get_ExceptionFilterOptions_Constant_Reference;

   type ColumnDescriptor_Vector is tagged private with
     Variable_Indexing => Get_ColumnDescriptor_Variable_Reference,
     Constant_Indexing => Get_ColumnDescriptor_Constant_Reference;

   type ExceptionDetails_Vector is tagged private with
     Variable_Indexing => Get_ExceptionDetails_Variable_Reference,
     Constant_Indexing => Get_ExceptionDetails_Constant_Reference;

   package Enum is

      type ModuleEvent_reason is (a_new, changed, removed);

      type ColumnDescriptor_type is
        (string, number, a_boolean, unixTimestampUTC);

      type Optional_ColumnDescriptor_type (Is_Set : Boolean := False) is record
         case Is_Set is
            when True =>
               Value : ColumnDescriptor_type;
            when False =>
               null;
         end case;
      end record;

      type StackFrame_presentationHint is (normal, label, subtle);

      type Optional_StackFrame_presentationHint (Is_Set : Boolean := False) is
      record
         case Is_Set is
            when True =>
               Value : StackFrame_presentationHint;
            when False =>
               null;
         end case;
      end record;

      type ExceptionBreakMode is (never, always, unhandled, userUnhandled);

      type StoppedEvent_reason is
        (step, breakpoint, a_exception, pause, a_entry, a_goto,
         function_breakpoint, data_breakpoint, instruction_breakpoint);

      type StartDebuggingRequestArguments_request is (launch, attach);

      type OutputEvent_category is
        (console, important, stdout, stderr, telemetry);

      type Optional_OutputEvent_category (Is_Set : Boolean := False) is record
         case Is_Set is
            when True =>
               Value : OutputEvent_category;
            when False =>
               null;
         end case;
      end record;

      type OutputEvent_group is (start, startCollapsed, a_end);

      type Optional_OutputEvent_group (Is_Set : Boolean := False) is record
         case Is_Set is
            when True =>
               Value : OutputEvent_group;
            when False =>
               null;
         end case;
      end record;

      type ChecksumAlgorithm is (MD5, SHA1, SHA256, timestamp);

      type ProcessEvent_startMethod is
        (launch, attach, attachForSuspendedLaunch);

      type Optional_ProcessEvent_startMethod (Is_Set : Boolean := False) is
      record
         case Is_Set is
            when True =>
               Value : ProcessEvent_startMethod;
            when False =>
               null;
         end case;
      end record;

      type Scope_presentationHint is (arguments, locals, registers);

      type Optional_Scope_presentationHint (Is_Set : Boolean := False) is
      record
         case Is_Set is
            when True =>
               Value : Scope_presentationHint;
            when False =>
               null;
         end case;
      end record;

      type Response_message is (cancelled, notStopped);

      type Optional_Response_message (Is_Set : Boolean := False) is record
         case Is_Set is
            when True =>
               Value : Response_message;
            when False =>
               null;
         end case;
      end record;

      type CompletionItemType is
        (method, a_function, constructor, field, variable, class, an_interface,
         module, property, unit, value, enum, keyword, snippet, text, color,
         file, reference, customcolor);

      type Optional_CompletionItemType (Is_Set : Boolean := False) is record
         case Is_Set is
            when True =>
               Value : CompletionItemType;
            when False =>
               null;
         end case;
      end record;

      type InvalidatedAreas is (an_all, stacks, threads, variables);

      type Source_presentationHint is (normal, emphasize, deemphasize);

      type Optional_Source_presentationHint (Is_Set : Boolean := False) is
      record
         case Is_Set is
            when True =>
               Value : Source_presentationHint;
            when False =>
               null;
         end case;
      end record;

      type LoadedSourceEvent_reason is (a_new, changed, removed);

      type ProtocolMessage_type is (request, response, event);

      type RunInTerminalRequestArguments_kind is (integrated, external);

      type Optional_RunInTerminalRequestArguments_kind
        (Is_Set : Boolean := False) is
      record
         case Is_Set is
            when True =>
               Value : RunInTerminalRequestArguments_kind;
            when False =>
               null;
         end case;
      end record;

      type VariablesArguments_filter is (indexed, named);

      type Optional_VariablesArguments_filter (Is_Set : Boolean := False) is
      record
         case Is_Set is
            when True =>
               Value : VariablesArguments_filter;
            when False =>
               null;
         end case;
      end record;

      type VariablePresentationHint_kind is
        (property, method, class, data, event, baseClass, innerClass,
         an_interface, mostDerivedClass, virtual, dataBreakpoint);

      type Optional_VariablePresentationHint_kind (Is_Set : Boolean := False)
      is
      record
         case Is_Set is
            when True =>
               Value : VariablePresentationHint_kind;
            when False =>
               null;
         end case;
      end record;

      type VariablePresentationHint_visibility is
        (public, a_private, a_protected, internal, final);

      type Optional_VariablePresentationHint_visibility
        (Is_Set : Boolean := False) is
      record
         case Is_Set is
            when True =>
               Value : VariablePresentationHint_visibility;
            when False =>
               null;
         end case;
      end record;

      type InitializeRequestArguments_pathFormat is (path, uri);

      type Optional_InitializeRequestArguments_pathFormat
        (Is_Set : Boolean := False) is
      record
         case Is_Set is
            when True =>
               Value : InitializeRequestArguments_pathFormat;
            when False =>
               null;
         end case;
      end record;

      type ThreadEvent_reason is (started, exited);

      type DataBreakpointAccessType is (read, write, readWrite);

      type Optional_DataBreakpointAccessType (Is_Set : Boolean := False) is
      record
         case Is_Set is
            when True =>
               Value : DataBreakpointAccessType;
            when False =>
               null;
         end case;
      end record;

      type BreakpointEvent_reason is (changed, a_new, removed);

      type EvaluateArguments_context is
        (watch, repl, hover, clipboard, variables);

      type Optional_EvaluateArguments_context (Is_Set : Boolean := False) is
      record
         case Is_Set is
            when True =>
               Value : EvaluateArguments_context;
            when False =>
               null;
         end case;
      end record;

      type SteppingGranularity is (statement, line, instruction);

      type Optional_SteppingGranularity (Is_Set : Boolean := False) is record
         case Is_Set is
            when True =>
               Value : SteppingGranularity;
            when False =>
               null;
         end case;
      end record;

   end Enum;

   type ProtocolMessage is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_type : Enum.ProtocolMessage_type;
      --  Message type.
   end record;

   type Response is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type GotoResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type ExceptionDetails is record
      message        : VSS.Strings.Virtual_String;
      --  Message contained in the exception.
      typeName       : VSS.Strings.Virtual_String;
      --  Short type name of the exception object.
      fullTypeName   : VSS.Strings.Virtual_String;
      --  Fully-qualified type name of the exception object.
      evaluateName   : VSS.Strings.Virtual_String;
      --  An expression that can be evaluated in the current scope to obtain
      --  the exception object.
      stackTrace     : VSS.Strings.Virtual_String;
      --  Stack trace at the time the exception was thrown.
      innerException : ExceptionDetails_Vector;
      --  Details of the exception contained by this exception, if any.
   end record;

   type Optional_ExceptionDetails (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ExceptionDetails;
         when False =>
            null;
      end case;
   end record;

   type Request is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      command   : VSS.Strings.Virtual_String;
      --  The command to execute.
      arguments : Any_Value;
      --  Object containing arguments for the command.
   end record;

   type StepInTargetsArguments is record
      frameId : Integer;
      --  The stack frame for which to retrieve the possible step-in targets.
   end record;

   type StepInTargetsRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : StepInTargetsArguments;
   end record;

   type ModulesResponse_body is record
      modules      : Module_Vector;
      --  All modules or range of modules.
      totalModules : Optional_Integer;
      --  The total number of modules available.
   end record;

   type Optional_ModulesResponse_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ModulesResponse_body;
         when False =>
            null;
      end case;
   end record;

   type ModulesResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : ModulesResponse_body;
   end record;

   type NextArguments is record
      threadId     : Integer;
      --  Specifies the thread for which to resume execution for one step (of
      --  the given granularity).
      singleThread : Boolean := Boolean'First;
      --  If this flag is true, all other suspended threads are not resumed.
      granularity  : Enum.Optional_SteppingGranularity;
      --  Stepping granularity. If no granularity is specified, a granularity
      --  of `statement` is assumed.
   end record;

   type ExceptionInfoArguments is record
      threadId : Integer;
      --  Thread for which exception information should be retrieved.
   end record;

   type ExceptionInfoRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : ExceptionInfoArguments;
   end record;

   type TerminateThreadsArguments is record
      threadIds : Integer_Vector;
      --  Ids of threads to be terminated.
   end record;

   type DataBreakpointInfoArguments is record
      variablesReference : Optional_Integer;
      --  Reference to the variable container if the data breakpoint is
      --  requested for a child of the container. The `variablesReference` must
      --  have been obtained in the current suspended state. See 'Lifetime of
      --  Object References' in the Overview section for details.
      name               : VSS.Strings.Virtual_String;
      --  The name of the variable's child to obtain data breakpoint
      --  information for. If `variablesReference` isn't specified, this can
      --  be an expression.
      frameId            : Optional_Integer;
      --  When `name` is an expression, evaluate it in the scope of this stack
      --  frame. If not specified, the expression is evaluated in the global
      --  scope. When `variablesReference` is specified, this property has no
      --  effect.
   end record;

   type DataBreakpointInfoRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : DataBreakpointInfoArguments;
   end record;

   type TerminateThreadsRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : TerminateThreadsArguments;
   end record;

   type Source is record
      name             : VSS.Strings.Virtual_String;
      --  The short name of the source. Every source returned from the debug
      --  adapter has a name. When sending a source to the debug adapter this
      --  name is optional.
      path             : VSS.Strings.Virtual_String;
      --  The path of the source to be shown in the UI. It is only used to
      --  locate and load the content of the source if no `sourceReference`
      --  is specified (or its value is 0).
      sourceReference  : Optional_Integer;
      --  If the value > 0 the contents of the source must be retrieved
      --  through the `source` request (even if a path is specified). Since
      --  a `sourceReference` is only valid for a session, it can not be
      --  used to persist a source. The value should be less than or equal
      --  to 2147483647 (2^31-1).
      presentationHint : Enum.Optional_Source_presentationHint;
      --  A hint for how to present the source in the UI. A value of
      --  `deemphasize` can be used to indicate that the source is not
      --  available or that it is skipped on stepping.
      origin           : VSS.Strings.Virtual_String;
      --  The origin of this source. For example, 'internal module', 'inlined
      --  content from source map', etc.
      sources          : Source_Vector;
      --  A list of sources that are related to this source. These may be the
      --  source that generated this source.
      adapterData      : Any_Value;
      --  Additional data that a debug adapter might want to loop through the
      --  client. The client should leave the data intact and persist it across
      --  sessions. The client should not interpret the data.
      checksums        : Checksum_Vector;
      --  The checksums associated with this file.
   end record;

   type Optional_Source (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Source;
         when False =>
            null;
      end case;
   end record;

   type SetBreakpointsArguments is record
      source         : DAP.Tools.Source;
      --  The source location of the breakpoints; either `source.path` or
      --  `source.sourceReference` must be specified.
      breakpoints    : SourceBreakpoint_Vector;
      --  The code locations of the breakpoints.
      lines          : Integer_Vector;
      --  Deprecated: The code locations of the breakpoints.
      sourceModified : Boolean := Boolean'First;
      --  A value of true indicates that the underlying source has been
      --  modified which results in new breakpoint locations.
   end record;

   type Event is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      event  : VSS.Strings.Virtual_String;
      --  Type of event.
      a_body : Any_Value;
      --  Event-specific information.
   end record;

   type Module is record
      id             : Integer_Or_String;
      --  Unique identifier for the module.
      name           : VSS.Strings.Virtual_String;
      --  A name of the module.
      path           : VSS.Strings.Virtual_String;
      --  Logical full path to the module. The exact definition is
      --  implementation defined, but usually this would be a full path to
      --  the on-disk file for the module.
      isOptimized    : Boolean := Boolean'First;
      --  True if the module is optimized.
      isUserCode     : Boolean := Boolean'First;
      --  True if the module is considered 'user code' by a debugger that
      --  supports 'Just My Code'.
      version        : VSS.Strings.Virtual_String;
      --  Version of Module.
      symbolStatus   : VSS.Strings.Virtual_String;
      --  User-understandable description of if symbols were found for the
      --  module (ex: 'Symbols Loaded', 'Symbols not found', etc.)
      symbolFilePath : VSS.Strings.Virtual_String;
      --  Logical full path to the symbol file. The exact definition is
      --  implementation defined.
      dateTimeStamp  : VSS.Strings.Virtual_String;
      --  Module created or modified, encoded as a RFC 3339 timestamp.
      addressRange   : VSS.Strings.Virtual_String;
      --  Address range covered by this module.
   end record;

   type ModuleEvent_body is record
      reason : Enum.ModuleEvent_reason;
      --  The reason for the event.
      module : DAP.Tools.Module;
      --  The new, changed, or removed module. In case of `removed` only the
      --  module id is used.
   end record;

   type Optional_ModuleEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ModuleEvent_body;
         when False =>
            null;
      end case;
   end record;

   type ModuleEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : ModuleEvent_body;
   end record;

   type ContinuedEvent_body is record
      threadId            : Integer;
      --  The thread which was continued.
      allThreadsContinued : Boolean := Boolean'First;
      --  If `allThreadsContinued` is true, a debug adapter can announce that
      --  all threads have continued.
   end record;

   type Optional_ContinuedEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ContinuedEvent_body;
         when False =>
            null;
      end case;
   end record;

   type ContinuedEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : ContinuedEvent_body;
   end record;

   type AttachRequestArguments is record
      restart : Any_Value;
      --  Arbitrary data from the previous, restarted session. The data is
      --  sent as the `restart` attribute of the `terminated` event. The
      --  client should leave the data intact.
      pid     : Optional_Integer;
      --  Extension. The process ID to which gdb should attach. See Attach.
      target  : VSS.Strings.Virtual_String;
      --  Extension. The target to which gdb should connect. This is a string
      --  and is passed to the target remote command. See Connecting.
   end record;

   type Optional_AttachRequestArguments (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : AttachRequestArguments;
         when False =>
            null;
      end case;
   end record;

   type RestartArguments is record
      arguments : Optional_AttachRequestArguments;
      --  The latest version of the `launch` or `attach` configuration.
   end record;

   type Optional_RestartArguments (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : RestartArguments;
         when False =>
            null;
      end case;
   end record;

   type ValueFormat is record
      hex : Boolean := Boolean'First;
      --  Display the value in hex.
   end record;

   type Optional_ValueFormat (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ValueFormat;
         when False =>
            null;
      end case;
   end record;

   type SetVariableArguments is record
      variablesReference : Integer;
      --  The reference of the variable container. The `variablesReference`
      --  must have been obtained in the current suspended state. See 'Lifetime
      --  of Object References' in the Overview section for details.
      name               : VSS.Strings.Virtual_String;
      --  The name of the variable in the container.
      value              : VSS.Strings.Virtual_String;
      --  The value of the variable.
      format             : Optional_ValueFormat;
      --  Specifies details on how to format the response value.
   end record;

   type Checksum is record
      algorithm : Enum.ChecksumAlgorithm;
      --  The algorithm used to calculate this checksum.
      checksum  : VSS.Strings.Virtual_String;
      --  Value of the checksum, encoded as a hexadecimal value.
   end record;

   type BreakpointLocationsArguments is record
      source    : DAP.Tools.Source;
      --  The source location of the breakpoints; either `source.path` or
      --  `source.reference` must be specified.
      line      : Integer;
      --  Start line of range to search possible breakpoint locations in.
      --  If only the line is specified, the request returns all possible
      --  locations in that line.
      column    : Optional_Integer;
      --  Start position within `line` to search possible breakpoint locations
      --  in. It is measured in UTF-16 code units and the client capability
      --  `columnsStartAt1` determines whether it is 0- or 1-based. If no
      --  column is given, the first position in the start line is assumed.
      endLine   : Optional_Integer;
      --  End line of range to search possible breakpoint locations in. If no
      --  end line is given, then the end line is assumed to be the start line.
      endColumn : Optional_Integer;
      --  End position within `endLine` to search possible breakpoint locations
      --  in. It is measured in UTF-16 code units and the client capability
      --  `columnsStartAt1` determines whether it is 0- or 1-based. If no end
      --  column is given, the last position in the end line is assumed.
   end record;

   type Optional_BreakpointLocationsArguments (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : BreakpointLocationsArguments;
         when False =>
            null;
      end case;
   end record;

   type BreakpointLocationsRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : Optional_BreakpointLocationsArguments;
   end record;

   type ModulesViewDescriptor is record
      columns : ColumnDescriptor_Vector;
   end record;

   type ColumnDescriptor is record
      attributeName : VSS.Strings.Virtual_String;
      --  Name of the attribute rendered in this column.
      label         : VSS.Strings.Virtual_String;
      --  Header UI label of column.
      format        : VSS.Strings.Virtual_String;
      --  Format to use for the rendered values in this column. TBD how the
      --  format strings looks like.
      a_type        : Enum.Optional_ColumnDescriptor_type;
      --  Datatype of values in this column. Defaults to `string` if not
      --  specified.
      width         : Optional_Integer;
      --  Width of this column in characters (hint only).
   end record;

   type Capabilities is record
      supportsConfigurationDoneRequest      : Boolean := Boolean'First;
      --  The debug adapter supports the `configurationDone` request.
      supportsFunctionBreakpoints           : Boolean := Boolean'First;
      --  The debug adapter supports function breakpoints.
      supportsConditionalBreakpoints        : Boolean := Boolean'First;
      --  The debug adapter supports conditional breakpoints.
      supportsHitConditionalBreakpoints     : Boolean := Boolean'First;
      --  The debug adapter supports breakpoints that break execution after a
      --  specified number of hits.
      supportsEvaluateForHovers             : Boolean := Boolean'First;
      --  The debug adapter supports a (side effect free) `evaluate` request
      --  for data hovers.
      exceptionBreakpointFilters : ExceptionBreakpointsFilter_Vector;
      --  Available exception filter options for the `setExceptionBreakpoints`
      --  request.
      supportsStepBack                      : Boolean := Boolean'First;
      --  The debug adapter supports stepping back via the `stepBack` and
      --  `reverseContinue` requests.
      supportsSetVariable                   : Boolean := Boolean'First;
      --  The debug adapter supports setting a variable to a value.
      supportsRestartFrame                  : Boolean := Boolean'First;
      --  The debug adapter supports restarting a frame.
      supportsGotoTargetsRequest            : Boolean := Boolean'First;
      --  The debug adapter supports the `gotoTargets` request.
      supportsStepInTargetsRequest          : Boolean := Boolean'First;
      --  The debug adapter supports the `stepInTargets` request.
      supportsCompletionsRequest            : Boolean := Boolean'First;
      --  The debug adapter supports the `completions` request.
      completionTriggerCharacters : VSS.String_Vectors.Virtual_String_Vector;
      --  The set of characters that should trigger completion in a REPL. If
      --  not specified, the UI should assume the `.` character.
      supportsModulesRequest                : Boolean := Boolean'First;
      --  The debug adapter supports the `modules` request.
      additionalModuleColumns               : ColumnDescriptor_Vector;
      --  The set of additional module information exposed by the debug
      --  adapter.
      supportedChecksumAlgorithms           : ChecksumAlgorithm_Vector;
      --  Checksum algorithms supported by the debug adapter.
      supportsRestartRequest                : Boolean := Boolean'First;
      --  The debug adapter supports the `restart` request. In this case a
      --  client should not implement `restart` by terminating and relaunching
      --  the adapter but by calling the `restart` request.
      supportsExceptionOptions              : Boolean := Boolean'First;
      --  The debug adapter supports `exceptionOptions` on the
      --  `setExceptionBreakpoints` request.
      supportsValueFormattingOptions        : Boolean := Boolean'First;
      --  The debug adapter supports a `format` attribute on the `stackTrace`,
      --  `variables`, and `evaluate` requests.
      supportsExceptionInfoRequest          : Boolean := Boolean'First;
      --  The debug adapter supports the `exceptionInfo` request.
      supportTerminateDebuggee              : Boolean := Boolean'First;
      --  The debug adapter supports the `terminateDebuggee` attribute on the
      --  `disconnect` request.
      supportSuspendDebuggee                : Boolean := Boolean'First;
      --  The debug adapter supports the `suspendDebuggee` attribute on the
      --  `disconnect` request.
      supportsDelayedStackTraceLoading      : Boolean := Boolean'First;
      --  The debug adapter supports the delayed loading of parts of the stack,
      --  which requires that both the `startFrame` and `levels` arguments and
      --  the `totalFrames` result of the `stackTrace` request are supported.
      supportsLoadedSourcesRequest          : Boolean := Boolean'First;
      --  The debug adapter supports the `loadedSources` request.
      supportsLogPoints                     : Boolean := Boolean'First;
      --  The debug adapter supports log points by interpreting the
      --  `logMessage` attribute of the `SourceBreakpoint`.
      supportsTerminateThreadsRequest       : Boolean := Boolean'First;
      --  The debug adapter supports the `terminateThreads` request.
      supportsSetExpression                 : Boolean := Boolean'First;
      --  The debug adapter supports the `setExpression` request.
      supportsTerminateRequest              : Boolean := Boolean'First;
      --  The debug adapter supports the `terminate` request.
      supportsDataBreakpoints               : Boolean := Boolean'First;
      --  The debug adapter supports data breakpoints.
      supportsReadMemoryRequest             : Boolean := Boolean'First;
      --  The debug adapter supports the `readMemory` request.
      supportsWriteMemoryRequest            : Boolean := Boolean'First;
      --  The debug adapter supports the `writeMemory` request.
      supportsDisassembleRequest            : Boolean := Boolean'First;
      --  The debug adapter supports the `disassemble` request.
      supportsCancelRequest                 : Boolean := Boolean'First;
      --  The debug adapter supports the `cancel` request.
      supportsBreakpointLocationsRequest    : Boolean := Boolean'First;
      --  The debug adapter supports the `breakpointLocations` request.
      supportsClipboardContext              : Boolean := Boolean'First;
      --  The debug adapter supports the `clipboard` context value in the
      --  `evaluate` request.
      supportsSteppingGranularity           : Boolean := Boolean'First;
      --  The debug adapter supports stepping granularities (argument
      --  `granularity`) for the stepping requests.
      supportsInstructionBreakpoints        : Boolean := Boolean'First;
      --  The debug adapter supports adding breakpoints based on instruction
      --  references.
      supportsExceptionFilterOptions        : Boolean := Boolean'First;
      --  The debug adapter supports `filterOptions` as an argument on the
      --  `setExceptionBreakpoints` request.
      supportsSingleThreadExecutionRequests : Boolean := Boolean'First;
      --  The debug adapter supports the `singleThread` property on the
      --  execution requests (`continue`, `next`, `stepIn`, `stepOut`,
      --  `reverseContinue`, `stepBack`).
   end record;

   type Optional_Capabilities (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Capabilities;
         when False =>
            null;
      end case;
   end record;

   type StackTraceResponse_body is record
      stackFrames : StackFrame_Vector;
      --  The frames of the stack frame. If the array has length zero, there
      --  are no stack frames available. This means that there is no location
      --  information available.
      totalFrames : Optional_Integer;
      --  The total number of frames available in the stack. If omitted or
      --  if `totalFrames` is larger than the available frames, a client
      --  is expected to request frames until a request returns less frames
      --  than requested (which indicates the end of the stack). Returning
      --  monotonically increasing `totalFrames` values for subsequent
      --  requests can be used to enforce paging in the client.
   end record;

   type Optional_StackTraceResponse_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : StackTraceResponse_body;
         when False =>
            null;
      end case;
   end record;

   type StackTraceResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : StackTraceResponse_body;
   end record;

   type LoadedSourcesArguments is new Any_Object with null record;
   type Optional_LoadedSourcesArguments (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : LoadedSourcesArguments;
         when False =>
            null;
      end case;
   end record;

   type ConfigurationDoneArguments is new Any_Object with null record;
   type Optional_ConfigurationDoneArguments (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : ConfigurationDoneArguments;
         when False =>
            null;
      end case;
   end record;

   type ConfigurationDoneRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : Optional_ConfigurationDoneArguments;
   end record;

   type StepInTargetsResponse_body is record
      targets : StepInTarget_Vector;
      --  The possible step-in targets of the specified source location.
   end record;

   type Optional_StepInTargetsResponse_body (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : StepInTargetsResponse_body;
         when False =>
            null;
      end case;
   end record;

   type StepInTargetsResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : StepInTargetsResponse_body;
   end record;

   type StackFrame is record
      id                          : Integer;
      --  An identifier for the stack frame. It must be unique across all
      --  threads. This id can be used to retrieve the scopes of the frame with
      --  the `scopes` request or to restart the execution of a stack frame.
      name                        : VSS.Strings.Virtual_String;
      --  The name of the stack frame, typically a method name.
      source                      : Optional_Source;
      --  The source of the frame.
      line                        : Integer;
      --  The line within the source of the frame. If the source attribute is
      --  missing or doesn't exist, `line` is 0 and should be ignored by the
      --  client.
      column                      : Integer;
      --  Start position of the range covered by the stack frame. It
      --  is measured in UTF-16 code units and the client capability
      --  `columnsStartAt1` determines whether it is 0- or 1-based. If
      --  attribute `source` is missing or doesn't exist, `column` is 0
      --  and should be ignored by the client.
      endLine                     : Optional_Integer;
      --  The end line of the range covered by the stack frame.
      endColumn                   : Optional_Integer;
      --  End position of the range covered by the stack frame. It is measured
      --  in UTF-16 code units and the client capability `columnsStartAt1`
      --  determines whether it is 0- or 1-based.
      canRestart                  : Boolean := Boolean'First;
      --  Indicates whether this frame can be restarted with the `restart`
      --  request. Clients should only use this if the debug adapter
      --  supports the `restart` request and the corresponding capability
      --  `supportsRestartRequest` is true. If a debug adapter has this
      --  capability, then `canRestart` defaults to `true` if the property
      --  is absent.
      instructionPointerReference : VSS.Strings.Virtual_String;
      --  A memory reference for the current instruction pointer in this frame.
      moduleId                    : Optional_Integer_Or_String;
      --  The module associated with this frame, if any.
      presentationHint            : Enum.Optional_StackFrame_presentationHint;
      --  A hint for how to present this frame in the UI. A value of `label`
      --  can be used to indicate that the frame is an artificial frame that is
      --  used as a visual label or separator. A value of `subtle` can be used
      --  to change the appearance of a frame in a 'subtle' way.
   end record;

   type SetExpressionArguments is record
      expression : VSS.Strings.Virtual_String;
      --  The l-value expression to assign to.
      value      : VSS.Strings.Virtual_String;
      --  The value expression to assign to the l-value expression.
      frameId    : Optional_Integer;
      --  Evaluate the expressions in the scope of this stack frame. If not
      --  specified, the expressions are evaluated in the global scope.
      format     : Optional_ValueFormat;
      --  Specifies how the resulting value should be formatted.
   end record;

   type SetExpressionRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : SetExpressionArguments;
   end record;

   type SourceArguments is record
      source          : Optional_Source;
      --  Specifies the source content to load. Either `source.path` or
      --  `source.sourceReference` must be specified.
      sourceReference : Integer;
      --  The reference to the source. This is the same as
      --  `source.sourceReference`. This is provided for backward compatibility
      --  since old clients do not understand the `source` attribute.
   end record;

   type ExceptionFilterOptions is record
      filterId  : VSS.Strings.Virtual_String;
      --  ID of an exception filter returned by the
      --  `exceptionBreakpointFilters` capability.
      condition : VSS.Strings.Virtual_String;
      --  An expression for conditional exceptions. The exception breaks into
      --  the debugger if the result of the condition is true.
   end record;

   type ExceptionBreakpointsFilter is record
      filter               : VSS.Strings.Virtual_String;
      --  The internal ID of the filter option. This value is passed to the
      --  `setExceptionBreakpoints` request.
      label                : VSS.Strings.Virtual_String;
      --  The name of the filter option. This is shown in the UI.
      description          : VSS.Strings.Virtual_String;
      --  A help text providing additional information about the exception
      --  filter. This string is typically shown as a hover and can be
      --  translated.
      default              : Boolean := Boolean'First;
      --  Initial value of the filter option. If not specified a value false is
      --  assumed.
      supportsCondition    : Boolean := Boolean'First;
      --  Controls whether a condition can be specified for this filter option.
      --  If false or missing, a condition can not be set.
      conditionDescription : VSS.Strings.Virtual_String;
      --  A help text providing information about the condition. This string is
      --  shown as the placeholder text for a text box and can be translated.
   end record;

   type SetVariableRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : SetVariableArguments;
   end record;

   type AttachRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : AttachRequestArguments;
   end record;

   type MemoryEvent_body is record
      memoryReference : VSS.Strings.Virtual_String;
      --  Memory reference of a memory range that has been updated.
      offset          : Integer;
      --  Starting offset in bytes where memory has been updated. Can be
      --  negative.
      count           : Integer;
      --  Number of bytes updated.
   end record;

   type Optional_MemoryEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : MemoryEvent_body;
         when False =>
            null;
      end case;
   end record;

   type MemoryEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : MemoryEvent_body;
   end record;

   type ReadMemoryArguments is record
      memoryReference : VSS.Strings.Virtual_String;
      --  Memory reference to the base location from which data should be read.
      offset          : Optional_Integer;
      --  Offset (in bytes) to be applied to the reference location before
      --  reading data. Can be negative.
      count           : Integer;
      --  Number of bytes to read at the specified location and offset.
   end record;

   type LoadedSourcesRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : Optional_LoadedSourcesArguments;
   end record;

   type LaunchRequestArguments is record
      noDebug                         : Boolean := Boolean'First;
      --  If true, the launch request should launch the program without
      --  enabling debugging.
      restart                         : Any_Value;
      --  Arbitrary data from the previous, restarted session. The data is
      --  sent as the `restart` attribute of the `terminated` event. The
      --  client should leave the data intact.
      program                         : VSS.Strings.Virtual_String;
      --  Extension. If provided, this is a string that specifies the program
      --  to use. This corresponds to the file command. See Files.
      args : VSS.String_Vectors.Virtual_String_Vector;
      --  Extension. If provided, this should be an array of strings. These
      --  strings are provided as command-line arguments to the inferior, as
      --  if by set args.
      cwd                             : VSS.Strings.Virtual_String;
      --  Extension. If provided, this should be a string. gdb will change its
      --  working directory to this directory, as if by the cd command (see
      --  Working Directory). The launched program will inherit this as its
      --  working directory. Note that change of directory happens before
      --  the program parameter is processed. This will affect the result
      --  if program is a relative filename.
      stopAtBeginningOfMainSubprogram : Boolean := Boolean'First;
      --  Extension. If provided, this must be a boolean. When ‘True’,
      --  gdb will set a temporary breakpoint at the program's main procedure,
      --  using the same approach as the start command. See Starting.
   end record;

   type ExitedEvent_body is record
      exitCode : Integer;
      --  The exit code returned from the debuggee.
   end record;

   type Optional_ExitedEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ExitedEvent_body;
         when False =>
            null;
      end case;
   end record;

   type ExitedEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : ExitedEvent_body;
   end record;

   type SetBreakpointsRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : SetBreakpointsArguments;
   end record;

   type TerminateResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type VariablePresentationHint is record
      kind       : Enum.Optional_VariablePresentationHint_kind;
      --  The kind of variable. Before introducing additional values, try to
      --  use the listed values.
      attributes : VSS.String_Vectors.Virtual_String_Vector;
      --  Set of attributes represented as an array of strings. Before
      --  introducing additional values, try to use the listed values.
      visibility : Enum.Optional_VariablePresentationHint_visibility;
      --  Visibility of variable. Before introducing additional values, try to
      --  use the listed values.
      lazy       : Boolean := Boolean'First;
      --  If true, clients can present the variable with a UI that supports
      --  a specific gesture to trigger its evaluation. This mechanism can be
      --  used for properties that require executing code when retrieving their
      --  value and where the code execution can be expensive and/or produce
      --  side-effects. A typical example are properties based on a getter
      --  function. Please note that in addition to the `lazy` flag, the
      --  variable's `variablesReference` is expected to refer to a variable
      --  that will provide the value through another `variable` request.
   end record;

   type Optional_VariablePresentationHint (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : VariablePresentationHint;
         when False =>
            null;
      end case;
   end record;

   type Variable is record
      name               : VSS.Strings.Virtual_String;
      --  The variable's name.
      value              : VSS.Strings.Virtual_String;
      --  The variable's value.
      --  This can be a multi-line text, e.g. for a function the body of
      --  a function. For structured variables (which do not have a simple
      --  value), it is recommended to provide a one-line representation of the
      --  structured object. This helps to identify the structured object in
      --  the collapsed state when its children are not yet visible. An empty
      --  string can be used if no value should be shown in the UI.
      a_type             : VSS.Strings.Virtual_String;
      --  The type of the variable's value. Typically shown in the UI when
      --  hovering over the value. This attribute should only be returned by a
      --  debug adapter if the corresponding capability `supportsVariableType`
      --  is true.
      presentationHint   : Optional_VariablePresentationHint;
      --  Properties of a variable that can be used to determine how to render
      --  the variable in the UI.
      evaluateName       : VSS.Strings.Virtual_String;
      --  The evaluatable name of this variable which can be passed to the
      --  `evaluate` request to fetch the variable's value.
      variablesReference : Integer;
      --  If `variablesReference` is > 0, the variable is structured and its
      --  children can be retrieved by passing `variablesReference` to the
      --  `variables` request as long as execution remains suspended. See
      --  'Lifetime of Object References' in the Overview section for details.
      namedVariables     : Optional_Integer;
      --  The number of named child variables. The client can use this
      --  information to present the children in a paged UI and fetch them
      --  in chunks.
      indexedVariables   : Optional_Integer;
      --  The number of indexed child variables. The client can use this
      --  information to present the children in a paged UI and fetch them
      --  in chunks.
      memoryReference    : VSS.Strings.Virtual_String;
      --  The memory reference for the variable if the variable represents
      --  executable code, such as a function pointer. This attribute is only
      --  required if the corresponding capability `supportsMemoryReferences`
      --  is true.
   end record;

   type StoppedEvent_body is record
      reason            : Enum.StoppedEvent_reason;
      --  The reason for the event.
      --  For backward compatibility this string is shown in the UI if the
      --  `description` attribute is missing (but it must not be translated).
      description       : VSS.Strings.Virtual_String;
      --  The full reason for the event, e.g. 'Paused on exception'. This
      --  string is shown in the UI as is and can be translated.
      threadId          : Optional_Integer;
      --  The thread which was stopped.
      preserveFocusHint : Boolean := Boolean'First;
      --  A value of true hints to the client that this event should not change
      --  the focus.
      text              : VSS.Strings.Virtual_String;
      --  Additional information. E.g. if reason is `exception`, text contains
      --  the exception name. This string is shown in the UI.
      allThreadsStopped : Boolean := Boolean'First;
      --  If `allThreadsStopped` is true, a debug adapter can announce that
      --  all threads have stopped. - The client should use this information to
      --  enable that all threads can be expanded to access their stacktraces.
      --  - If the attribute is missing or false, only the thread with the
      --  given `threadId` can be expanded.
      hitBreakpointIds  : Integer_Vector;
      --  Ids of the breakpoints that triggered the event. In most cases there
      --  is only a single breakpoint but here are some examples for multiple
      --  breakpoints: - Different types of breakpoints map to the same
      --  location. - Multiple source breakpoints get collapsed to the same
      --  instruction by the compiler/runtime. - Multiple function breakpoints
      --  with different function names map to the same location.
   end record;

   type Optional_StoppedEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : StoppedEvent_body;
         when False =>
            null;
      end case;
   end record;

   type StoppedEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : StoppedEvent_body;
   end record;

   type RestartFrameArguments is record
      frameId : Integer;
      --  Restart the stack frame identified by `frameId`. The `frameId` must
      --  have been obtained in the current suspended state. See 'Lifetime of
      --  Object References' in the Overview section for details.
   end record;

   type RestartFrameRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : RestartFrameArguments;
   end record;

   type ScopesResponse_body is record
      scopes : Scope_Vector;
      --  The scopes of the stack frame. If the array has length zero, there
      --  are no scopes available.
   end record;

   type Optional_ScopesResponse_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ScopesResponse_body;
         when False =>
            null;
      end case;
   end record;

   type ScopesResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : ScopesResponse_body;
   end record;

   type StepOutArguments is record
      threadId     : Integer;
      --  Specifies the thread for which to resume execution for one step-out
      --  (of the given granularity).
      singleThread : Boolean := Boolean'First;
      --  If this flag is true, all other suspended threads are not resumed.
      granularity  : Enum.Optional_SteppingGranularity;
      --  Stepping granularity. If no granularity is specified, a granularity
      --  of `statement` is assumed.
   end record;

   type CompletionsArguments is record
      frameId : Optional_Integer;
      --  Returns completions in the scope of this stack frame. If not
      --  specified, the completions are returned for the global scope.
      text    : VSS.Strings.Virtual_String;
      --  One or more source lines. Typically this is the text users have typed
      --  into the debug console before they asked for completion.
      column  : Integer;
      --  The position within `text` for which to determine the completion
      --  proposals. It is measured in UTF-16 code units and the client
      --  capability `columnsStartAt1` determines whether it is 0- or 1-based.
      line    : Optional_Integer;
      --  A line for which to determine the completion proposals. If missing
      --  the first line of the text is assumed.
   end record;

   type CompletionsRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : CompletionsArguments;
   end record;

   type StartDebuggingRequestArguments is record
      configuration : Any_Object;
      --  Arguments passed to the new debug session. The arguments must only
      --  contain properties understood by the `launch` or `attach` requests
      --  of the debug adapter and they must not contain any client-specific
      --  properties (e.g. `type`) or client-specific features (e.g.
      --  substitutable 'variables').
      request       : Enum.StartDebuggingRequestArguments_request;
      --  Indicates whether the new debug session should be started with a
      --  `launch` or `attach` request.
   end record;

   type ProgressUpdateEvent_body is record
      progressId : VSS.Strings.Virtual_String;
      --  The ID that was introduced in the initial `progressStart` event.
      message    : VSS.Strings.Virtual_String;
      --  More detailed progress message. If omitted, the previous message (if
      --  any) is used.
      percentage : Optional_Float;
      --  Progress percentage to display (value range: 0 to 100). If omitted no
      --  percentage is shown.
   end record;

   type Optional_ProgressUpdateEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ProgressUpdateEvent_body;
         when False =>
            null;
      end case;
   end record;

   type ProgressUpdateEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : ProgressUpdateEvent_body;
   end record;

   type ExceptionInfoResponse_body is record
      exceptionId : VSS.Strings.Virtual_String;
      --  ID of the exception that was thrown.
      description : VSS.Strings.Virtual_String;
      --  Descriptive text for the exception.
      breakMode   : Enum.ExceptionBreakMode;
      --  Mode that caused the exception notification to be raised.
      details     : Optional_ExceptionDetails;
      --  Detailed information about the exception.
   end record;

   type Optional_ExceptionInfoResponse_body (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : ExceptionInfoResponse_body;
         when False =>
            null;
      end case;
   end record;

   type ExceptionInfoResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : ExceptionInfoResponse_body;
   end record;

   type InitializedEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : Any_Value;
      --  Event-specific information.
   end record;

   type SetExpressionResponse_body is record
      value              : VSS.Strings.Virtual_String;
      --  The new value of the expression.
      a_type             : VSS.Strings.Virtual_String;
      --  The type of the value.
      --  This attribute should only be returned by a debug adapter if the
      --  corresponding capability `supportsVariableType` is true.
      presentationHint   : Optional_VariablePresentationHint;
      --  Properties of a value that can be used to determine how to render the
      --  result in the UI.
      variablesReference : Optional_Integer;
      --  If `variablesReference` is > 0, the evaluate result is structured
      --  and its children can be retrieved by passing `variablesReference` to
      --  the `variables` request as long as execution remains suspended. See
      --  'Lifetime of Object References' in the Overview section for details.
      namedVariables     : Optional_Integer;
      --  The number of named child variables. The client can use this
      --  information to present the variables in a paged UI and fetch them
      --  in chunks. The value should be less than or equal to 2147483647
      --  (2^31-1).
      indexedVariables   : Optional_Integer;
      --  The number of indexed child variables. The client can use this
      --  information to present the variables in a paged UI and fetch them
      --  in chunks. The value should be less than or equal to 2147483647
      --  (2^31-1).
   end record;

   type Optional_SetExpressionResponse_body (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : SetExpressionResponse_body;
         when False =>
            null;
      end case;
   end record;

   type SetExpressionResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : SetExpressionResponse_body;
   end record;

   type StepInTarget is record
      id        : Integer;
      --  Unique identifier for a step-in target.
      label     : VSS.Strings.Virtual_String;
      --  The name of the step-in target (shown in the UI).
      line      : Optional_Integer;
      --  The line of the step-in target.
      column    : Optional_Integer;
      --  Start position of the range covered by the step in target.
      --  It is measured in UTF-16 code units and the client capability
      --  `columnsStartAt1` determines whether it is 0- or 1-based.
      endLine   : Optional_Integer;
      --  The end line of the range covered by the step-in target.
      endColumn : Optional_Integer;
      --  End position of the range covered by the step in target. It
      --  is measured in UTF-16 code units and the client capability
      --  `columnsStartAt1` determines whether it is 0- or 1-based.
   end record;

   type ReverseContinueResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type OutputEvent_body is record
      category           : Enum.Optional_OutputEvent_category;
      --  The output category. If not specified or if the category is not
      --  understood by the client, `console` is assumed.
      output             : VSS.Strings.Virtual_String;
      --  The output to report.
      group              : Enum.Optional_OutputEvent_group;
      --  Support for keeping an output log organized by grouping related
      --  messages.
      variablesReference : Optional_Integer;
      --  If an attribute `variablesReference` exists and its value is >
      --  0, the output contains objects which can be retrieved by passing
      --  `variablesReference` to the `variables` request as long as execution
      --  remains suspended. See 'Lifetime of Object References' in the
      --  Overview section for details.
      source             : Optional_Source;
      --  The source location where the output was produced.
      line               : Optional_Integer;
      --  The source location's line where the output was produced.
      column             : Optional_Integer;
      --  The position in `line` where the output was produced. It is measured
      --  in UTF-16 code units and the client capability `columnsStartAt1`
      --  determines whether it is 0- or 1-based.
      data               : Any_Value;
      --  Additional data to report. For the `telemetry` category the data is
      --  sent to telemetry, for the other categories the data is shown in JSON
      --  format.
   end record;

   type Optional_OutputEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : OutputEvent_body;
         when False =>
            null;
      end case;
   end record;

   type OutputEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : OutputEvent_body;
   end record;

   type RestartRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : Optional_RestartArguments;
   end record;

   type StackFrameFormat is record
      hex             : Boolean := Boolean'First;
      --  Display the value in hex.
      parameters      : Boolean := Boolean'First;
      --  Displays parameters for the stack frame.
      parameterTypes  : Boolean := Boolean'First;
      --  Displays the types of parameters for the stack frame.
      parameterNames  : Boolean := Boolean'First;
      --  Displays the names of parameters for the stack frame.
      parameterValues : Boolean := Boolean'First;
      --  Displays the values of parameters for the stack frame.
      line            : Boolean := Boolean'First;
      --  Displays the line number of the stack frame.
      module          : Boolean := Boolean'First;
      --  Displays the module of the stack frame.
      includeAll      : Boolean := Boolean'First;
      --  Includes all stack frames, including those the debug adapter might
      --  otherwise hide.
   end record;

   type Optional_StackFrameFormat (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : StackFrameFormat;
         when False =>
            null;
      end case;
   end record;

   type StackTraceArguments is record
      threadId   : Integer;
      --  Retrieve the stacktrace for this thread.
      startFrame : Optional_Integer;
      --  The index of the first frame to return; if omitted frames start at 0.
      levels     : Optional_Integer;
      --  The maximum number of frames to return. If levels is not specified or
      --  0, all frames are returned.
      format     : Optional_StackFrameFormat;
      --  Specifies details on how to format the stack frames. The attribute
      --  is only honored by a debug adapter if the corresponding capability
      --  `supportsValueFormattingOptions` is true.
   end record;

   type Thread is record
      id   : Integer;
      --  Unique identifier for the thread.
      name : VSS.Strings.Virtual_String;
      --  The name of the thread.
   end record;

   type SetDataBreakpointsArguments is record
      breakpoints : DataBreakpoint_Vector;
      --  The contents of this array replaces all existing data breakpoints. An
      --  empty array clears all data breakpoints.
   end record;

   type SetDataBreakpointsRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : SetDataBreakpointsArguments;
   end record;

   type SourceRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : SourceArguments;
   end record;

   type PauseResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type SetFunctionBreakpointsArguments is record
      breakpoints : FunctionBreakpoint_Vector;
      --  The function names of the breakpoints.
   end record;

   type SetFunctionBreakpointsRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : SetFunctionBreakpointsArguments;
   end record;

   type ProcessEvent_body is record
      name            : VSS.Strings.Virtual_String;
      --  The logical name of the process. This is usually the full path to
      --  process's executable file. Example: /home/example/myproj/program.js.
      systemProcessId : Optional_Integer;
      --  The system process id of the debugged process. This property is
      --  missing for non-system processes.
      isLocalProcess  : Boolean := Boolean'First;
      --  If true, the process is running on the same computer as the debug
      --  adapter.
      startMethod     : Enum.Optional_ProcessEvent_startMethod;
      --  Describes how the debug engine started debugging this process.
      pointerSize     : Optional_Integer;
      --  The size of a pointer or address for this process, in bits. This
      --  value may be used by clients when formatting addresses for display.
   end record;

   type Optional_ProcessEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ProcessEvent_body;
         when False =>
            null;
      end case;
   end record;

   type ProcessEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : ProcessEvent_body;
   end record;

   type NextResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type AttachResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type RestartResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type CapabilitiesEvent_body is record
      capabilities : DAP.Tools.Capabilities;
      --  The set of updated capabilities.
   end record;

   type Optional_CapabilitiesEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : CapabilitiesEvent_body;
         when False =>
            null;
      end case;
   end record;

   type CapabilitiesEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : CapabilitiesEvent_body;
   end record;

   type Scope is record
      name               : VSS.Strings.Virtual_String;
      --  Name of the scope such as 'Arguments', 'Locals', or 'Registers'. This
      --  string is shown in the UI as is and can be translated.
      presentationHint   : Enum.Optional_Scope_presentationHint;
      --  A hint for how to present this scope in the UI. If this attribute is
      --  missing, the scope is shown with a generic UI.
      variablesReference : Integer;
      --  The variables of this scope can be retrieved by passing the value of
      --  `variablesReference` to the `variables` request as long as execution
      --  remains suspended. See 'Lifetime of Object References' in the
      --  Overview section for details.
      namedVariables     : Optional_Integer;
      --  The number of named variables in this scope. The client can use this
      --  information to present the variables in a paged UI and fetch them in
      --  chunks.
      indexedVariables   : Optional_Integer;
      --  The number of indexed variables in this scope. The client can use
      --  this information to present the variables in a paged UI and fetch
      --  them in chunks.
      expensive          : Boolean;
      --  If true, the number of variables in this scope is large or expensive
      --  to retrieve.
      source             : Optional_Source;
      --  The source for this scope.
      line               : Optional_Integer;
      --  The start line of the range covered by this scope.
      column             : Optional_Integer;
      --  Start position of the range covered by the scope. It is measured
      --  in UTF-16 code units and the client capability `columnsStartAt1`
      --  determines whether it is 0- or 1-based.
      endLine            : Optional_Integer;
      --  The end line of the range covered by this scope.
      endColumn          : Optional_Integer;
      --  End position of the range covered by the scope. It is measured
      --  in UTF-16 code units and the client capability `columnsStartAt1`
      --  determines whether it is 0- or 1-based.
   end record;

   type DisassembleArguments is record
      memoryReference   : VSS.Strings.Virtual_String;
      --  Memory reference to the base location containing the instructions to
      --  disassemble.
      offset            : Optional_Integer;
      --  Offset (in bytes) to be applied to the reference location before
      --  disassembling. Can be negative.
      instructionOffset : Optional_Integer;
      --  Offset (in instructions) to be applied after the byte offset (if any)
      --  before disassembling. Can be negative.
      instructionCount  : Integer;
      --  Number of instructions to disassemble starting at the specified
      --  location and offset. An adapter must return exactly this number of
      --  instructions - any unavailable instructions should be replaced with
      --  an implementation-defined 'invalid instruction' value.
      resolveSymbols    : Boolean := Boolean'First;
      --  If true, the adapter should attempt to resolve memory addresses and
      --  other values to symbolic names.
   end record;

   type SetInstructionBreakpointsArguments is record
      breakpoints : InstructionBreakpoint_Vector;
      --  The instruction references of the breakpoints
   end record;

   type SetInstructionBreakpointsRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : SetInstructionBreakpointsArguments;
   end record;

   type DataBreakpointInfoResponse_body is record
      dataId      : VSS.Strings.Virtual_String;
      --  An identifier for the data on which a data breakpoint can be
      --  registered with the `setDataBreakpoints` request or null if no
      --  data breakpoint is available.
      description : VSS.Strings.Virtual_String;
      --  UI string that describes on what data the breakpoint is set on or why
      --  a data breakpoint is not available.
      accessTypes : DataBreakpointAccessType_Vector;
      --  Attribute lists the available access types for a potential data
      --  breakpoint. A UI client could surface this information.
      canPersist  : Boolean := Boolean'First;
      --  Attribute indicates that a potential data breakpoint could be
      --  persisted across sessions.
   end record;

   type Optional_DataBreakpointInfoResponse_body (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : DataBreakpointInfoResponse_body;
         when False =>
            null;
      end case;
   end record;

   type DataBreakpointInfoResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : DataBreakpointInfoResponse_body;
   end record;

   type SourceBreakpoint is record
      line         : Integer;
      --  The source line of the breakpoint or logpoint.
      column       : Optional_Integer;
      --  Start position within source line of the breakpoint or logpoint.
      --  It is measured in UTF-16 code units and the client capability
      --  `columnsStartAt1` determines whether it is 0- or 1-based.
      condition    : VSS.Strings.Virtual_String;
      --  The expression for conditional breakpoints. It is only
      --  honored by a debug adapter if the corresponding capability
      --  `supportsConditionalBreakpoints` is true.
      hitCondition : VSS.Strings.Virtual_String;
      --  The expression that controls how many hits of the breakpoint are
      --  ignored. The debug adapter is expected to interpret the expression
      --  as needed. The attribute is only honored by a debug adapter if the
      --  corresponding capability `supportsHitConditionalBreakpoints` is true.
      --  If both this property and `condition` are specified, `hitCondition`
      --  should be evaluated only if the `condition` is met, and the debug
      --  adapter should stop only if both conditions are met.
      logMessage   : VSS.Strings.Virtual_String;
      --  If this attribute exists and is non-empty, the debug adapter must not
      --  'break' (stop) but log the message instead. Expressions within `{}`
      --  are interpolated. The attribute is only honored by a debug adapter if
      --  the corresponding capability `supportsLogPoints` is true. If either
      --  `hitCondition` or `condition` is specified, then the message should
      --  only be logged if those conditions are met.
   end record;

   type PauseArguments is record
      threadId : Integer;
      --  Pause execution for this thread.
   end record;

   type PauseRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : PauseArguments;
   end record;

   type FunctionBreakpoint is record
      name         : VSS.Strings.Virtual_String;
      --  The name of the function.
      condition    : VSS.Strings.Virtual_String;
      --  An expression for conditional breakpoints. It is only
      --  honored by a debug adapter if the corresponding capability
      --  `supportsConditionalBreakpoints` is true.
      hitCondition : VSS.Strings.Virtual_String;
      --  An expression that controls how many hits of the breakpoint are
      --  ignored. The debug adapter is expected to interpret the expression
      --  as needed. The attribute is only honored by a debug adapter if the
      --  corresponding capability `supportsHitConditionalBreakpoints` is true.
   end record;

   type SetExceptionBreakpointsArguments is record
      filters          : VSS.String_Vectors.Virtual_String_Vector;
      --  Set of exception filters specified by their ID. The
      --  set of all possible exception filters is defined by the
      --  `exceptionBreakpointFilters` capability. The `filter` and
      --  `filterOptions` sets are additive.
      filterOptions    : ExceptionFilterOptions_Vector;
      --  Set of exception filters and their options. The set of all possible
      --  exception filters is defined by the `exceptionBreakpointFilters`
      --  capability. This attribute is only honored by a debug adapter if the
      --  corresponding capability `supportsExceptionFilterOptions` is true.
      --  The `filter` and `filterOptions` sets are additive.
      exceptionOptions : ExceptionOptions_Vector;
      --  Configuration options for selected exceptions. The attribute is
      --  only honored by a debug adapter if the corresponding capability
      --  `supportsExceptionOptions` is true.
   end record;

   type RunInTerminalRequestArguments is record
      kind : Enum.Optional_RunInTerminalRequestArguments_kind;
      --  What kind of terminal to launch. Defaults to `integrated` if not
      --  specified.
      title                       : VSS.Strings.Virtual_String;
      --  Title of the terminal.
      cwd                         : VSS.Strings.Virtual_String;
      --  Working directory for the command. For non-empty, valid paths this
      --  typically results in execution of a change directory command.
      args                        : VSS.String_Vectors.Virtual_String_Vector;
      --  List of arguments. The first argument is the command to run.
      env                         : Any_Object;
      --  Environment key-value pairs that are added to or removed from the
      --  default environment.
      argsCanBeInterpretedByShell : Boolean := Boolean'First;
      --  This property should only be set if the corresponding capability
      --  `supportsArgsCanBeInterpretedByShell` is true. If the client uses an
      --  intermediary shell to launch the application, then the client must
      --  not attempt to escape characters with special meanings for the
      --  shell. The user is fully responsible for escaping as needed and that
      --  arguments using special characters may not be portable across shells.
   end record;

   type RunInTerminalRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : RunInTerminalRequestArguments;
   end record;

   type WriteMemoryResponse_body is record
      offset       : Optional_Integer;
      --  Property that should be returned when `allowPartial` is true to
      --  indicate the offset of the first byte of data successfully written.
      --  Can be negative.
      bytesWritten : Optional_Integer;
      --  Property that should be returned when `allowPartial` is true
      --  to indicate the number of bytes starting from address that
      --  were successfully written.
   end record;

   type Optional_WriteMemoryResponse_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : WriteMemoryResponse_body;
         when False =>
            null;
      end case;
   end record;

   type WriteMemoryResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Optional_WriteMemoryResponse_body;
   end record;

   type ReverseContinueArguments is record
      threadId     : Integer;
      --  Specifies the active thread. If the debug adapter supports single
      --  thread execution (see `supportsSingleThreadExecutionRequests`) and
      --  the `singleThread` argument is true, only the thread with this ID
      --  is resumed.
      singleThread : Boolean := Boolean'First;
      --  If this flag is true, backward execution is resumed only for the
      --  thread with given `threadId`.
   end record;

   type RunInTerminalResponse_body is record
      processId      : Optional_Integer;
      --  The process ID. The value should be less than or equal to 2147483647
      --  (2^31-1).
      shellProcessId : Optional_Integer;
      --  The process ID of the terminal shell. The value should be less than
      --  or equal to 2147483647 (2^31-1).
   end record;

   type Optional_RunInTerminalResponse_body (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : RunInTerminalResponse_body;
         when False =>
            null;
      end case;
   end record;

   type RunInTerminalResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : RunInTerminalResponse_body;
   end record;

   type DisconnectArguments is record
      restart           : Boolean := Boolean'First;
      --  A value of true indicates that this `disconnect` request is part of a
      --  restart sequence.
      terminateDebuggee : Boolean := Boolean'First;
      --  Indicates whether the debuggee should be terminated when the debugger
      --  is disconnected. If unspecified, the debug adapter is free to do
      --  whatever it thinks is best. The attribute is only honored by a debug
      --  adapter if the corresponding capability `supportTerminateDebuggee` is
      --  true.
      suspendDebuggee   : Boolean := Boolean'First;
      --  Indicates whether the debuggee should stay suspended when the
      --  debugger is disconnected. If unspecified, the debuggee should resume
      --  execution. The attribute is only honored by a debug adapter if the
      --  corresponding capability `supportSuspendDebuggee` is true.
   end record;

   type Optional_DisconnectArguments (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : DisconnectArguments;
         when False =>
            null;
      end case;
   end record;

   type GotoTargetsArguments is record
      source : DAP.Tools.Source;
      --  The source location for which the goto targets are determined.
      line   : Integer;
      --  The line location for which the goto targets are determined.
      column : Optional_Integer;
      --  The position within `line` for which the goto targets are determined.
      --  It is measured in UTF-16 code units and the client capability
      --  `columnsStartAt1` determines whether it is 0- or 1-based.
   end record;

   type GotoTargetsRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : GotoTargetsArguments;
   end record;

   type ThreadsResponse_body is record
      threads : Thread_Vector;
      --  All threads.
   end record;

   type Optional_ThreadsResponse_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ThreadsResponse_body;
         when False =>
            null;
      end case;
   end record;

   type ThreadsResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : ThreadsResponse_body;
   end record;

   type SetDataBreakpointsResponse_body is record
      breakpoints : Breakpoint_Vector;
      --  Information about the data breakpoints. The array elements correspond
      --  to the elements of the input argument `breakpoints` array.
   end record;

   type Optional_SetDataBreakpointsResponse_body (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : SetDataBreakpointsResponse_body;
         when False =>
            null;
      end case;
   end record;

   type SetDataBreakpointsResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : SetDataBreakpointsResponse_body;
   end record;

   type DataBreakpoint is record
      dataId       : VSS.Strings.Virtual_String;
      --  An id representing the data. This id is returned from the
      --  `dataBreakpointInfo` request.
      accessType   : Enum.Optional_DataBreakpointAccessType;
      --  The access type of the data.
      condition    : VSS.Strings.Virtual_String;
      --  An expression for conditional breakpoints.
      hitCondition : VSS.Strings.Virtual_String;
      --  An expression that controls how many hits of the breakpoint are
      --  ignored. The debug adapter is expected to interpret the expression
      --  as needed.
   end record;

   type ExceptionPathSegment is record
      negate : Boolean := Boolean'First;
      --  If false or missing this segment matches the names provided,
      --  otherwise it matches anything except the names provided.
      names  : VSS.String_Vectors.Virtual_String_Vector;
      --  Depending on the value of `negate` the names that should match or not
      --  match.
   end record;

   type Message is record
      id            : Integer;
      --  Unique (within a debug adapter implementation) identifier for the
      --  message. The purpose of these error IDs is to help extension authors
      --  that have the requirement that every user visible error message needs
      --  a corresponding error number, so that users or customer support can
      --  find information about the specific error more easily.
      format        : VSS.Strings.Virtual_String;
      --  A format string for the message. Embedded variables have the form
      --  `{name}`. If variable name starts with an underscore character, the
      --  variable does not contain user data (PII) and can be safely used for
      --  telemetry purposes.
      variables     : Any_Object;
      --  An object used as a dictionary for looking up the variables in the
      --  format string.
      sendTelemetry : Boolean := Boolean'First;
      --  If true send to telemetry.
      showUser      : Boolean := Boolean'First;
      --  If true show user.
      url           : VSS.Strings.Virtual_String;
      --  A url where additional information about this message can be found.
      urlLabel      : VSS.Strings.Virtual_String;
      --  A label that is presented to the user as the UI for opening the url.
   end record;

   type Optional_Message (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Message;
         when False =>
            null;
      end case;
   end record;

   type SourceResponse_body is record
      content  : VSS.Strings.Virtual_String;
      --  Content of the source reference.
      mimeType : VSS.Strings.Virtual_String;
      --  Content type (MIME type) of the source.
   end record;

   type Optional_SourceResponse_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : SourceResponse_body;
         when False =>
            null;
      end case;
   end record;

   type SourceResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : SourceResponse_body;
   end record;

   type ContinueResponse_body is record
      allThreadsContinued : Boolean := Boolean'First;
      --  The value true (or a missing property) signals to the client that
      --  all threads have been resumed. The value false indicates that not
      --  all threads were resumed.
   end record;

   type Optional_ContinueResponse_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ContinueResponse_body;
         when False =>
            null;
      end case;
   end record;

   type ContinueResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : ContinueResponse_body;
   end record;

   type RestartFrameResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type StepInArguments is record
      threadId     : Integer;
      --  Specifies the thread for which to resume execution for one step-into
      --  (of the given granularity).
      singleThread : Boolean := Boolean'First;
      --  If this flag is true, all other suspended threads are not resumed.
      targetId     : Optional_Integer;
      --  Id of the target to step into.
      granularity  : Enum.Optional_SteppingGranularity;
      --  Stepping granularity. If no granularity is specified, a granularity
      --  of `statement` is assumed.
   end record;

   type LaunchResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type StepInResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type TerminateArguments is record
      restart : Boolean := Boolean'First;
      --  A value of true indicates that this `terminate` request is part of a
      --  restart sequence.
   end record;

   type Optional_TerminateArguments (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : TerminateArguments;
         when False =>
            null;
      end case;
   end record;

   type LaunchRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : LaunchRequestArguments;
   end record;

   type StepOutResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type EvaluateArguments is record
      expression : VSS.Strings.Virtual_String;
      --  The expression to evaluate.
      frameId    : Optional_Integer;
      --  Evaluate the expression in the scope of this stack frame. If not
      --  specified, the expression is evaluated in the global scope.
      context    : Enum.Optional_EvaluateArguments_context;
      --  The context in which the evaluate request is used.
      format     : Optional_ValueFormat;
      --  Specifies details on how to format the result. The attribute is
      --  only honored by a debug adapter if the corresponding capability
      --  `supportsValueFormattingOptions` is true.
   end record;

   type EvaluateRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : EvaluateArguments;
   end record;

   type ContinueArguments is record
      threadId     : Integer;
      --  Specifies the active thread. If the debug adapter supports single
      --  thread execution (see `supportsSingleThreadExecutionRequests`) and
      --  the argument `singleThread` is true, only the thread with this ID
      --  is resumed.
      singleThread : Boolean := Boolean'First;
      --  If this flag is true, execution is resumed only for the thread with
      --  given `threadId`.
   end record;

   type StepBackArguments is record
      threadId     : Integer;
      --  Specifies the thread for which to resume execution for one step
      --  backwards (of the given granularity).
      singleThread : Boolean := Boolean'First;
      --  If this flag is true, all other suspended threads are not resumed.
      granularity  : Enum.Optional_SteppingGranularity;
      --  Stepping granularity to step. If no granularity is specified, a
      --  granularity of `statement` is assumed.
   end record;

   type CancelArguments is record
      requestId  : Optional_Integer;
      --  The ID (attribute `seq`) of the request to cancel. If missing no
      --  request is cancelled. Both a `requestId` and a `progressId` can
      --  be specified in one request.
      progressId : VSS.Strings.Virtual_String;
      --  The ID (attribute `progressId`) of the progress to cancel. If missing
      --  no progress is cancelled. Both a `requestId` and a `progressId` can
      --  be specified in one request.
   end record;

   type Optional_CancelArguments (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : CancelArguments;
         when False =>
            null;
      end case;
   end record;

   type CompletionsResponse_body is record
      targets : CompletionItem_Vector;
      --  The possible completions for .
   end record;

   type Optional_CompletionsResponse_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : CompletionsResponse_body;
         when False =>
            null;
      end case;
   end record;

   type CompletionsResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : CompletionsResponse_body;
   end record;

   type Breakpoint is record
      id                   : Optional_Integer;
      --  The identifier for the breakpoint. It is needed if breakpoint events
      --  are used to update or remove breakpoints.
      verified             : Boolean;
      --  If true, the breakpoint could be set (but not necessarily at the
      --  desired location).
      message              : VSS.Strings.Virtual_String;
      --  A message about the state of the breakpoint. This is shown to
      --  the user and can be used to explain why a breakpoint could not
      --  be verified.
      source               : Optional_Source;
      --  The source where the breakpoint is located.
      line                 : Optional_Integer;
      --  The start line of the actual range covered by the breakpoint.
      column               : Optional_Integer;
      --  Start position of the source range covered by the breakpoint.
      --  It is measured in UTF-16 code units and the client capability
      --  `columnsStartAt1` determines whether it is 0- or 1-based.
      endLine              : Optional_Integer;
      --  The end line of the actual range covered by the breakpoint.
      endColumn            : Optional_Integer;
      --  End position of the source range covered by the breakpoint.
      --  It is measured in UTF-16 code units and the client capability
      --  `columnsStartAt1` determines whether it is 0- or 1-based. If no end
      --  line is given, then the end column is assumed to be in the start
      --  line.
      instructionReference : VSS.Strings.Virtual_String;
      --  A memory reference to where the breakpoint is set.
      offset               : Optional_Integer;
      --  The offset from the instruction reference. This can be negative.
   end record;

   type WriteMemoryArguments is record
      memoryReference : VSS.Strings.Virtual_String;
      --  Memory reference to the base location to which data should be
      --  written.
      offset          : Optional_Integer;
      --  Offset (in bytes) to be applied to the reference location before
      --  writing data. Can be negative.
      allowPartial    : Boolean := Boolean'First;
      --  Property to control partial writes. If true, the debug adapter should
      --  attempt to write memory even if the entire memory region is not
      --  writable. In such a case the debug adapter should stop after hitting
      --  the first byte of memory that cannot be written and return the number
      --  of bytes written in the response via the `offset` and `bytesWritten`
      --  properties. If false or missing, a debug adapter should attempt to
      --  verify the region is writable before writing, and fail the response
      --  if it is not.
      data            : VSS.Strings.Virtual_String;
      --  Bytes to write, encoded using base64.
   end record;

   type StepOutRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : StepOutArguments;
   end record;

   type LoadedSourceEvent_body is record
      reason : Enum.LoadedSourceEvent_reason;
      --  The reason for the event.
      source : DAP.Tools.Source;
      --  The new, changed, or removed source.
   end record;

   type Optional_LoadedSourceEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : LoadedSourceEvent_body;
         when False =>
            null;
      end case;
   end record;

   type LoadedSourceEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : LoadedSourceEvent_body;
   end record;

   type DisassembleRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : DisassembleArguments;
   end record;

   type ReadMemoryResponse_body is record
      address         : VSS.Strings.Virtual_String;
      --  The address of the first byte of data returned. Treated as a hex
      --  value if prefixed with `0x`, or as a decimal value otherwise.
      unreadableBytes : Optional_Integer;
      --  The number of unreadable bytes encountered after the last
      --  successfully read byte. This can be used to determine the number of
      --  bytes that should be skipped before a subsequent `readMemory` request
      --  succeeds.
      data            : VSS.Strings.Virtual_String;
      --  The bytes read from memory, encoded using base64. If the decoded
      --  length of `data` is less than the requested `count` in the original
      --  `readMemory` request, and `unreadableBytes` is zero or omitted, then
      --  the client should assume it's reached the end of readable memory.
   end record;

   type Optional_ReadMemoryResponse_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ReadMemoryResponse_body;
         when False =>
            null;
      end case;
   end record;

   type ReadMemoryResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Optional_ReadMemoryResponse_body;
   end record;

   type StepBackRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : StepBackArguments;
   end record;

   type ThreadsRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : Any_Value;
      --  Object containing arguments for the command.
   end record;

   type VariablesResponse_body is record
      variables : Variable_Vector;
      --  All (or a range) of variables for the given variable reference.
   end record;

   type Optional_VariablesResponse_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : VariablesResponse_body;
         when False =>
            null;
      end case;
   end record;

   type VariablesResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : VariablesResponse_body;
   end record;

   type TerminateRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : Optional_TerminateArguments;
   end record;

   type VariablesArguments is record
      variablesReference : Integer;
      --  The variable for which to retrieve its children. The
      --  `variablesReference` must have been obtained in the current suspended
      --  state. See 'Lifetime of Object References' in the Overview section
      --  for details.
      filter             : Enum.Optional_VariablesArguments_filter;
      --  Filter to limit the child variables to either named or indexed. If
      --  omitted, both types are fetched.
      start              : Optional_Integer;
      --  The index of the first variable to return; if omitted children start
      --  at 0.
      count              : Optional_Integer;
      --  The number of variables to return. If count is missing or 0, all
      --  variables are returned.
      format             : Optional_ValueFormat;
      --  Specifies details on how to format the Variable values. The attribute
      --  is only honored by a debug adapter if the corresponding capability
      --  `supportsValueFormattingOptions` is true.
   end record;

   type InitializeRequestArguments is record
      clientID                            : VSS.Strings.Virtual_String;
      --  The ID of the client using this adapter.
      clientName                          : VSS.Strings.Virtual_String;
      --  The human-readable name of the client using this adapter.
      adapterID                           : VSS.Strings.Virtual_String;
      --  The ID of the debug adapter.
      locale                              : VSS.Strings.Virtual_String;
      --  The ISO-639 locale of the client using this adapter, e.g. en-US or
      --  de-CH.
      linesStartAt1                       : Boolean := Boolean'First;
      --  If true all line numbers are 1-based (default).
      columnsStartAt1                     : Boolean := Boolean'First;
      --  If true all column numbers are 1-based (default).
      pathFormat : Enum.Optional_InitializeRequestArguments_pathFormat;
      --  Determines in what format paths are specified. The default is `path`,
      --  which is the native format.
      supportsVariableType                : Boolean := Boolean'First;
      --  Client supports the `type` attribute for variables.
      supportsVariablePaging              : Boolean := Boolean'First;
      --  Client supports the paging of variables.
      supportsRunInTerminalRequest        : Boolean := Boolean'First;
      --  Client supports the `runInTerminal` request.
      supportsMemoryReferences            : Boolean := Boolean'First;
      --  Client supports memory references.
      supportsProgressReporting           : Boolean := Boolean'First;
      --  Client supports progress reporting.
      supportsInvalidatedEvent            : Boolean := Boolean'First;
      --  Client supports the `invalidated` event.
      supportsMemoryEvent                 : Boolean := Boolean'First;
      --  Client supports the `memory` event.
      supportsArgsCanBeInterpretedByShell : Boolean := Boolean'First;
      --  Client supports the `argsCanBeInterpretedByShell` attribute on the
      --  `runInTerminal` request.
      supportsStartDebuggingRequest       : Boolean := Boolean'First;
      --  Client supports the `startDebugging` request.
   end record;

   type InitializeRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : InitializeRequestArguments;
   end record;

   type BreakpointLocationsResponse_body is record
      breakpoints : BreakpointLocation_Vector;
      --  Sorted set of possible breakpoint locations.
   end record;

   type Optional_BreakpointLocationsResponse_body (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when True =>
            Value : BreakpointLocationsResponse_body;
         when False =>
            null;
      end case;
   end record;

   type BreakpointLocationsResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : BreakpointLocationsResponse_body;
   end record;

   type DisassembledInstruction is record
      address          : VSS.Strings.Virtual_String;
      --  The address of the instruction. Treated as a hex value if prefixed
      --  with `0x`, or as a decimal value otherwise.
      instructionBytes : VSS.Strings.Virtual_String;
      --  Raw bytes representing the instruction and its operands, in an
      --  implementation-defined format.
      instruction      : VSS.Strings.Virtual_String;
      --  Text representing the instruction and its operands, in an
      --  implementation-defined format.
      symbol           : VSS.Strings.Virtual_String;
      --  Name of the symbol that corresponds with the location of this
      --  instruction, if any.
      location         : Optional_Source;
      --  Source location that corresponds to this instruction, if any. Should
      --  always be set (if available) on the first instruction returned, but
      --  can be omitted afterwards if this instruction maps to the same source
      --  file as the previous instruction.
      line             : Optional_Integer;
      --  The line within the source location that corresponds to this
      --  instruction, if any.
      column           : Optional_Integer;
      --  The column within the line that corresponds to this instruction, if
      --  any.
      endLine          : Optional_Integer;
      --  The end line of the range that corresponds to this instruction, if
      --  any.
      endColumn        : Optional_Integer;
      --  The end column of the range that corresponds to this instruction, if
      --  any.
   end record;

   type CancelResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type SetInstructionBreakpointsResponse_body is record
      breakpoints : Breakpoint_Vector;
      --  Information about the breakpoints. The array elements correspond to
      --  the elements of the `breakpoints` array.
   end record;

   type Optional_SetInstructionBreakpointsResponse_body
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : SetInstructionBreakpointsResponse_body;
         when False =>
            null;
      end case;
   end record;

   type SetInstructionBreakpointsResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : SetInstructionBreakpointsResponse_body;
   end record;

   type CancelRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : Optional_CancelArguments;
   end record;

   type ProgressEndEvent_body is record
      progressId : VSS.Strings.Virtual_String;
      --  The ID that was introduced in the initial `ProgressStartEvent`.
      message    : VSS.Strings.Virtual_String;
      --  More detailed progress message. If omitted, the previous message (if
      --  any) is used.
   end record;

   type Optional_ProgressEndEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ProgressEndEvent_body;
         when False =>
            null;
      end case;
   end record;

   type ProgressEndEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : ProgressEndEvent_body;
   end record;

   type VariablesRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : VariablesArguments;
   end record;

   type ExceptionOptions is record
      path      : ExceptionPathSegment_Vector;
      --  A path that selects a single or multiple exceptions in a tree. If
      --  `path` is missing, the whole tree is selected. By convention the
      --  first segment of the path is a category that is used to group
      --  exceptions in the UI.
      breakMode : Enum.ExceptionBreakMode;
      --  Condition when a thrown exception should result in a break.
   end record;

   type TerminatedEvent_body is record
      restart : Any_Value;
      --  A debug adapter may set `restart` to true (or to an arbitrary object)
      --  to request that the client restarts the session. The value is not
      --  interpreted by the client and passed unmodified as an attribute
      --  `__restart` to the `launch` and `attach` requests.
   end record;

   type Optional_TerminatedEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : TerminatedEvent_body;
         when False =>
            null;
      end case;
   end record;

   type TerminatedEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : Optional_TerminatedEvent_body;
   end record;

   type StartDebuggingRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : StartDebuggingRequestArguments;
   end record;

   type ThreadEvent_body is record
      reason   : Enum.ThreadEvent_reason;
      --  The reason for the event.
      threadId : Integer;
      --  The identifier of the thread.
   end record;

   type Optional_ThreadEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ThreadEvent_body;
         when False =>
            null;
      end case;
   end record;

   type ThreadEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : ThreadEvent_body;
   end record;

   type GotoTargetsResponse_body is record
      targets : GotoTarget_Vector;
      --  The possible goto targets of the specified location.
   end record;

   type Optional_GotoTargetsResponse_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : GotoTargetsResponse_body;
         when False =>
            null;
      end case;
   end record;

   type GotoTargetsResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : GotoTargetsResponse_body;
   end record;

   type CompletionItem is record
      label           : VSS.Strings.Virtual_String;
      --  The label of this completion item. By default this is also the text
      --  that is inserted when selecting this completion.
      text            : VSS.Strings.Virtual_String;
      --  If text is returned and not an empty string, then it is inserted
      --  instead of the label.
      sortText        : VSS.Strings.Virtual_String;
      --  A string that should be used when comparing this item with other
      --  items. If not returned or an empty string, the `label` is used
      --  instead.
      detail          : VSS.Strings.Virtual_String;
      --  A human-readable string with additional information about this item,
      --  like type or symbol information.
      a_type          : Enum.Optional_CompletionItemType;
      --  The item's type. Typically the client uses this information to render
      --  the item in the UI with an icon.
      start           : Optional_Integer;
      --  Start position (within the `text` attribute of the `completions`
      --  request) where the completion text is added. The position is measured
      --  in UTF-16 code units and the client capability `columnsStartAt1`
      --  determines whether it is 0- or 1-based. If the start position is
      --  omitted the text is added at the location specified by the `column`
      --  attribute of the `completions` request.
      length          : Optional_Integer;
      --  Length determines how many characters are overwritten by the
      --  completion text and it is measured in UTF-16 code units. If missing
      --  the value 0 is assumed which results in the completion text being
      --  inserted.
      selectionStart  : Optional_Integer;
      --  Determines the start of the new selection after the text has been
      --  inserted (or replaced). `selectionStart` is measured in UTF-16 code
      --  units and must be in the range 0 and length of the completion text.
      --  If omitted the selection starts at the end of the completion text.
      selectionLength : Optional_Integer;
      --  Determines the length of the new selection after the text has been
      --  inserted (or replaced) and it is measured in UTF-16 code units. The
      --  selection can not extend beyond the bounds of the completion text.
      --  If omitted the length is assumed to be 0.
   end record;

   type ScopesArguments is record
      frameId : Integer;
      --  Retrieve the scopes for the stack frame identified by `frameId`. The
      --  `frameId` must have been obtained in the current suspended state. See
      --  'Lifetime of Object References' in the Overview section for details.
   end record;

   type ErrorResponse_body is record
      error : Optional_Message;
      --  A structured error message.
   end record;

   type Optional_ErrorResponse_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ErrorResponse_body;
         when False =>
            null;
      end case;
   end record;

   type ErrorResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : ErrorResponse_body;
   end record;

   type GotoArguments is record
      threadId : Integer;
      --  Set the goto target for this thread.
      targetId : Integer;
      --  The location where the debuggee will continue to run.
   end record;

   type BreakpointEvent_body is record
      reason     : Enum.BreakpointEvent_reason;
      --  The reason for the event.
      breakpoint : DAP.Tools.Breakpoint;
      --  The `id` attribute is used to find the target breakpoint, the other
      --  attributes are used as the new values.
   end record;

   type Optional_BreakpointEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : BreakpointEvent_body;
         when False =>
            null;
      end case;
   end record;

   type BreakpointEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : BreakpointEvent_body;
   end record;

   type GotoTarget is record
      id                          : Integer;
      --  Unique identifier for a goto target. This is used in the `goto`
      --  request.
      label                       : VSS.Strings.Virtual_String;
      --  The name of the goto target (shown in the UI).
      line                        : Integer;
      --  The line of the goto target.
      column                      : Optional_Integer;
      --  The column of the goto target.
      endLine                     : Optional_Integer;
      --  The end line of the range covered by the goto target.
      endColumn                   : Optional_Integer;
      --  The end column of the range covered by the goto target.
      instructionPointerReference : VSS.Strings.Virtual_String;
      --  A memory reference for the instruction pointer value represented by
      --  this target.
   end record;

   type ReadMemoryRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : ReadMemoryArguments;
   end record;

   type ModulesArguments is record
      startModule : Optional_Integer;
      --  The index of the first module to return; if omitted modules start at
      --  0.
      moduleCount : Optional_Integer;
      --  The number of modules to return. If `moduleCount` is not specified or
      --  0, all modules are returned.
   end record;

   type NextRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : NextArguments;
   end record;

   type ProgressStartEvent_body is record
      progressId  : VSS.Strings.Virtual_String;
      --  An ID that can be used in subsequent `progressUpdate` and
      --  `progressEnd` events to make them refer to the same progress
      --  reporting. IDs must be unique within a debug session.
      title       : VSS.Strings.Virtual_String;
      --  Short title of the progress reporting. Shown in the UI to describe
      --  the long running operation.
      requestId   : Optional_Integer;
      --  The request ID that this progress report is related to. If specified
      --  a debug adapter is expected to emit progress events for the long
      --  running request until the request has been either completed or
      --  cancelled. If the request ID is omitted, the progress report is
      --  assumed to be related to some general activity of the debug adapter.
      cancellable : Boolean := Boolean'First;
      --  If true, the request that reports progress may be cancelled with a
      --  `cancel` request. So this property basically controls whether the
      --  client should use UX that supports cancellation. Clients that don't
      --  support cancellation are allowed to ignore the setting.
      message     : VSS.Strings.Virtual_String;
      --  More detailed progress message.
      percentage  : Optional_Float;
      --  Progress percentage to display (value range: 0 to 100). If omitted no
      --  percentage is shown.
   end record;

   type Optional_ProgressStartEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : ProgressStartEvent_body;
         when False =>
            null;
      end case;
   end record;

   type ProgressStartEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : ProgressStartEvent_body;
   end record;

   type SetVariableResponse_body is record
      value              : VSS.Strings.Virtual_String;
      --  The new value of the variable.
      a_type             : VSS.Strings.Virtual_String;
      --  The type of the new value. Typically shown in the UI when hovering
      --  over the value.
      variablesReference : Optional_Integer;
      --  If `variablesReference` is > 0, the new value is structured and its
      --  children can be retrieved by passing `variablesReference` to the
      --  `variables` request as long as execution remains suspended. See
      --  'Lifetime of Object References' in the Overview section for details.
      namedVariables     : Optional_Integer;
      --  The number of named child variables. The client can use this
      --  information to present the variables in a paged UI and fetch them
      --  in chunks. The value should be less than or equal to 2147483647
      --  (2^31-1).
      indexedVariables   : Optional_Integer;
      --  The number of indexed child variables. The client can use this
      --  information to present the variables in a paged UI and fetch them
      --  in chunks. The value should be less than or equal to 2147483647
      --  (2^31-1).
   end record;

   type Optional_SetVariableResponse_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : SetVariableResponse_body;
         when False =>
            null;
      end case;
   end record;

   type SetVariableResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : SetVariableResponse_body;
   end record;

   type BreakpointLocation is record
      line      : Integer;
      --  Start line of breakpoint location.
      column    : Optional_Integer;
      --  The start position of a breakpoint location. Position is measured
      --  in UTF-16 code units and the client capability `columnsStartAt1`
      --  determines whether it is 0- or 1-based.
      endLine   : Optional_Integer;
      --  The end line of breakpoint location if the location covers a range.
      endColumn : Optional_Integer;
      --  The end position of a breakpoint location (if the location covers
      --  a range). Position is measured in UTF-16 code units and the client
      --  capability `columnsStartAt1` determines whether it is 0- or 1-based.
   end record;

   type DisconnectResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type SetExceptionBreakpointsRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : SetExceptionBreakpointsArguments;
   end record;

   type WriteMemoryRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : WriteMemoryArguments;
   end record;

   type InitializeResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Optional_Capabilities;
      --  The capabilities of this debug adapter.
   end record;

   type ConfigurationDoneResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type GotoRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : GotoArguments;
   end record;

   type SetFunctionBreakpointsResponse_body is record
      breakpoints : Breakpoint_Vector;
      --  Information about the breakpoints. The array elements correspond to
      --  the elements of the `breakpoints` array.
   end record;

   type Optional_SetFunctionBreakpointsResponse_body
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : SetFunctionBreakpointsResponse_body;
         when False =>
            null;
      end case;
   end record;

   type SetFunctionBreakpointsResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : SetFunctionBreakpointsResponse_body;
   end record;

   type StackTraceRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : StackTraceArguments;
   end record;

   type EvaluateResponse_body is record
      result             : VSS.Strings.Virtual_String;
      --  The result of the evaluate request.
      a_type             : VSS.Strings.Virtual_String;
      --  The type of the evaluate result. This attribute should only
      --  be returned by a debug adapter if the corresponding capability
      --  `supportsVariableType` is true.
      presentationHint   : Optional_VariablePresentationHint;
      --  Properties of an evaluate result that can be used to determine how to
      --  render the result in the UI.
      variablesReference : Integer;
      --  If `variablesReference` is > 0, the evaluate result is structured
      --  and its children can be retrieved by passing `variablesReference` to
      --  the `variables` request as long as execution remains suspended. See
      --  'Lifetime of Object References' in the Overview section for details.
      namedVariables     : Optional_Integer;
      --  The number of named child variables. The client can use this
      --  information to present the variables in a paged UI and fetch them
      --  in chunks. The value should be less than or equal to 2147483647
      --  (2^31-1).
      indexedVariables   : Optional_Integer;
      --  The number of indexed child variables. The client can use this
      --  information to present the variables in a paged UI and fetch them
      --  in chunks. The value should be less than or equal to 2147483647
      --  (2^31-1).
      memoryReference    : VSS.Strings.Virtual_String;
      --  A memory reference to a location appropriate for this result. For
      --  pointer type eval results, this is generally a reference to the
      --  memory address contained in the pointer. This attribute should
      --  be returned by a debug adapter if corresponding capability
      --  `supportsMemoryReferences` is true.
   end record;

   type Optional_EvaluateResponse_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : EvaluateResponse_body;
         when False =>
            null;
      end case;
   end record;

   type EvaluateResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : EvaluateResponse_body;
   end record;

   type ReverseContinueRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : ReverseContinueArguments;
   end record;

   type ModulesRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : ModulesArguments;
   end record;

   type SetBreakpointsResponse_body is record
      breakpoints : Breakpoint_Vector;
      --  Information about the breakpoints. The array elements are in the
      --  same order as the elements of the `breakpoints` (or the deprecated
      --  `lines`) array in the arguments.
   end record;

   type Optional_SetBreakpointsResponse_body (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : SetBreakpointsResponse_body;
         when False =>
            null;
      end case;
   end record;

   type SetBreakpointsResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : SetBreakpointsResponse_body;
   end record;

   type InstructionBreakpoint is record
      instructionReference : VSS.Strings.Virtual_String;
      --  The instruction reference of the breakpoint. This should be a
      --  memory or instruction pointer reference from an `EvaluateResponse`,
      --  `Variable`, `StackFrame`, `GotoTarget`, or `Breakpoint`.
      offset               : Optional_Integer;
      --  The offset from the instruction reference. This can be negative.
      condition            : VSS.Strings.Virtual_String;
      --  An expression for conditional breakpoints. It is only
      --  honored by a debug adapter if the corresponding capability
      --  `supportsConditionalBreakpoints` is true.
      hitCondition         : VSS.Strings.Virtual_String;
      --  An expression that controls how many hits of the breakpoint are
      --  ignored. The debug adapter is expected to interpret the expression
      --  as needed. The attribute is only honored by a debug adapter if the
      --  corresponding capability `supportsHitConditionalBreakpoints` is true.
   end record;

   type DisconnectRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : Optional_DisconnectArguments;
   end record;

   type TerminateThreadsResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type ScopesRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : ScopesArguments;
   end record;

   type SetExceptionBreakpointsResponse_body is record
      breakpoints : Breakpoint_Vector;
      --  Information about the exception breakpoints or filters. The
      --  breakpoints returned are in the same order as the elements of
      --  the `filters`, `filterOptions`, `exceptionOptions` arrays in the
      --  arguments. If both `filters` and `filterOptions` are given, the
      --  returned array must start with `filters` information first,
      --  followed by `filterOptions` information.
   end record;

   type Optional_SetExceptionBreakpointsResponse_body
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : SetExceptionBreakpointsResponse_body;
         when False =>
            null;
      end case;
   end record;

   type SetExceptionBreakpointsResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Optional_SetExceptionBreakpointsResponse_body;
   end record;

   type StepBackResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   type DisassembleResponse_body is record
      instructions : DisassembledInstruction_Vector;
      --  The list of disassembled instructions.
   end record;

   type Optional_DisassembleResponse_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : DisassembleResponse_body;
         when False =>
            null;
      end case;
   end record;

   type DisassembleResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Optional_DisassembleResponse_body;
   end record;

   type InvalidatedEvent_body is record
      areas        : InvalidatedAreas_Vector;
      --  Set of logical areas that got invalidated. This property has a hint
      --  characteristic: a client can only be expected to make a 'best effort'
      --  in honoring the areas but there are no guarantees. If this property
      --  is missing, empty, or if values are not understood, the client should
      --  assume a single value `all`.
      threadId     : Optional_Integer;
      --  If specified, the client only needs to refetch data related to this
      --  thread.
      stackFrameId : Optional_Integer;
      --  If specified, the client only needs to refetch data related to this
      --  stack frame (and the `threadId` is ignored).
   end record;

   type Optional_InvalidatedEvent_body (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : InvalidatedEvent_body;
         when False =>
            null;
      end case;
   end record;

   type InvalidatedEvent is record
      seq    : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      a_body : InvalidatedEvent_body;
   end record;

   type StepInRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : StepInArguments;
   end record;

   type ContinueRequest is record
      seq       : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      arguments : ContinueArguments;
   end record;

   type LoadedSourcesResponse_body is record
      sources : Source_Vector;
      --  Set of loaded sources.
   end record;

   type Optional_LoadedSourcesResponse_body (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : LoadedSourcesResponse_body;
         when False =>
            null;
      end case;
   end record;

   type LoadedSourcesResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : LoadedSourcesResponse_body;
   end record;

   type StartDebuggingResponse is record
      seq         : Integer;
      --  Sequence number of the message (also known as message ID). The `seq`
      --  for the first message sent by a client or debug adapter is 1, and for
      --  each subsequent message is 1 greater than the previous message sent
      --  by that actor. `seq` can be used to order requests, responses, and
      --  events, and to associate requests with their corresponding responses.
      --  For protocol messages of type `request` the sequence number can be
      --  used to cancel the request.
      request_seq : Integer;
      --  Sequence number of the corresponding request.
      success     : Boolean;
      --  Outcome of the request.
      --  If true, the request was successful and the `body` attribute may
      --  contain the result of the request. If the value is false, the
      --  attribute `message` contains the error in short form and the `body`
      --  may contain additional information (see `ErrorResponse.body.error`).
      command     : VSS.Strings.Virtual_String;
      --  The command requested.
      message     : Enum.Optional_Response_message;
      --  Contains the raw error in short form if `success` is false. This raw
      --  error might be interpreted by the client and is not shown in the UI.
      --  Some predefined values exist.
      a_body      : Any_Value;
      --  Contains request result if success is true and error details if
      --  success is false.
   end record;

   function Length (Self : Thread_Vector) return Natural;

   procedure Clear (Self : in out Thread_Vector);

   procedure Append (Self : in out Thread_Vector; Value : Thread);

   type Thread_Variable_Reference (Element : not null access Thread) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Thread_Variable_Reference
     (Self  : aliased in out Thread_Vector;
      Index : Positive)
      return Thread_Variable_Reference with
     Inline;

   type Thread_Constant_Reference (Element : not null access constant Thread)
   is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Thread_Constant_Reference
     (Self  : aliased Thread_Vector;
      Index : Positive)
      return Thread_Constant_Reference with
     Inline;

   function Length (Self : Checksum_Vector) return Natural;

   procedure Clear (Self : in out Checksum_Vector);

   procedure Append (Self : in out Checksum_Vector; Value : Checksum);

   type Checksum_Variable_Reference (Element : not null access Checksum) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Checksum_Variable_Reference
     (Self  : aliased in out Checksum_Vector;
      Index : Positive)
      return Checksum_Variable_Reference with
     Inline;

   type Checksum_Constant_Reference
     (Element : not null access constant Checksum) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Checksum_Constant_Reference
     (Self  : aliased Checksum_Vector;
      Index : Positive)
      return Checksum_Constant_Reference with
     Inline;

   function Length (Self : Breakpoint_Vector) return Natural;

   procedure Clear (Self : in out Breakpoint_Vector);

   procedure Append (Self : in out Breakpoint_Vector; Value : Breakpoint);

   type Breakpoint_Variable_Reference (Element : not null access Breakpoint) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Breakpoint_Variable_Reference
     (Self  : aliased in out Breakpoint_Vector;
      Index : Positive)
      return Breakpoint_Variable_Reference with
     Inline;

   type Breakpoint_Constant_Reference
     (Element : not null access constant Breakpoint) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Breakpoint_Constant_Reference
     (Self  : aliased Breakpoint_Vector;
      Index : Positive)
      return Breakpoint_Constant_Reference with
     Inline;

   function Length (Self : StepInTarget_Vector) return Natural;

   procedure Clear (Self : in out StepInTarget_Vector);

   procedure Append (Self : in out StepInTarget_Vector; Value : StepInTarget);

   type StepInTarget_Variable_Reference
     (Element : not null access StepInTarget) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_StepInTarget_Variable_Reference
     (Self  : aliased in out StepInTarget_Vector;
      Index : Positive)
      return StepInTarget_Variable_Reference with
     Inline;

   type StepInTarget_Constant_Reference
     (Element : not null access constant StepInTarget) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_StepInTarget_Constant_Reference
     (Self  : aliased StepInTarget_Vector;
      Index : Positive)
      return StepInTarget_Constant_Reference with
     Inline;

   function Length (Self : FunctionBreakpoint_Vector) return Natural;

   procedure Clear (Self : in out FunctionBreakpoint_Vector);

   procedure Append
     (Self : in out FunctionBreakpoint_Vector; Value : FunctionBreakpoint);

   type FunctionBreakpoint_Variable_Reference
     (Element : not null access FunctionBreakpoint) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_FunctionBreakpoint_Variable_Reference
     (Self  : aliased in out FunctionBreakpoint_Vector;
      Index : Positive)
      return FunctionBreakpoint_Variable_Reference with
     Inline;

   type FunctionBreakpoint_Constant_Reference
     (Element : not null access constant FunctionBreakpoint) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_FunctionBreakpoint_Constant_Reference
     (Self  : aliased FunctionBreakpoint_Vector;
      Index : Positive)
      return FunctionBreakpoint_Constant_Reference with
     Inline;

   function Length (Self : DataBreakpoint_Vector) return Natural;

   procedure Clear (Self : in out DataBreakpoint_Vector);

   procedure Append
     (Self : in out DataBreakpoint_Vector; Value : DataBreakpoint);

   type DataBreakpoint_Variable_Reference
     (Element : not null access DataBreakpoint) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_DataBreakpoint_Variable_Reference
     (Self  : aliased in out DataBreakpoint_Vector;
      Index : Positive)
      return DataBreakpoint_Variable_Reference with
     Inline;

   type DataBreakpoint_Constant_Reference
     (Element : not null access constant DataBreakpoint) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_DataBreakpoint_Constant_Reference
     (Self  : aliased DataBreakpoint_Vector;
      Index : Positive)
      return DataBreakpoint_Constant_Reference with
     Inline;

   function Length (Self : ExceptionOptions_Vector) return Natural;

   procedure Clear (Self : in out ExceptionOptions_Vector);

   procedure Append
     (Self : in out ExceptionOptions_Vector; Value : ExceptionOptions);

   type ExceptionOptions_Variable_Reference
     (Element : not null access ExceptionOptions) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_ExceptionOptions_Variable_Reference
     (Self  : aliased in out ExceptionOptions_Vector;
      Index : Positive)
      return ExceptionOptions_Variable_Reference with
     Inline;

   type ExceptionOptions_Constant_Reference
     (Element : not null access constant ExceptionOptions) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_ExceptionOptions_Constant_Reference
     (Self  : aliased ExceptionOptions_Vector;
      Index : Positive)
      return ExceptionOptions_Constant_Reference with
     Inline;

   function Length (Self : Integer_Vector) return Natural;

   procedure Clear (Self : in out Integer_Vector);

   procedure Append (Self : in out Integer_Vector; Value : Integer);

   type Integer_Variable_Reference (Element : not null access Integer) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Integer_Variable_Reference
     (Self  : aliased in out Integer_Vector;
      Index : Positive)
      return Integer_Variable_Reference with
     Inline;

   type Integer_Constant_Reference (Element : not null access constant Integer)
   is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Integer_Constant_Reference
     (Self  : aliased Integer_Vector;
      Index : Positive)
      return Integer_Constant_Reference with
     Inline;

   function Length (Self : GotoTarget_Vector) return Natural;

   procedure Clear (Self : in out GotoTarget_Vector);

   procedure Append (Self : in out GotoTarget_Vector; Value : GotoTarget);

   type GotoTarget_Variable_Reference (Element : not null access GotoTarget) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_GotoTarget_Variable_Reference
     (Self  : aliased in out GotoTarget_Vector;
      Index : Positive)
      return GotoTarget_Variable_Reference with
     Inline;

   type GotoTarget_Constant_Reference
     (Element : not null access constant GotoTarget) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_GotoTarget_Constant_Reference
     (Self  : aliased GotoTarget_Vector;
      Index : Positive)
      return GotoTarget_Constant_Reference with
     Inline;

   function Length (Self : InvalidatedAreas_Vector) return Natural;

   procedure Clear (Self : in out InvalidatedAreas_Vector);

   procedure Append
     (Self : in out InvalidatedAreas_Vector; Value : Enum.InvalidatedAreas);

   type InvalidatedAreas_Variable_Reference
     (Element : not null access Enum.InvalidatedAreas) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_InvalidatedAreas_Variable_Reference
     (Self  : aliased in out InvalidatedAreas_Vector;
      Index : Positive)
      return InvalidatedAreas_Variable_Reference with
     Inline;

   type InvalidatedAreas_Constant_Reference
     (Element : not null access constant Enum.InvalidatedAreas) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_InvalidatedAreas_Constant_Reference
     (Self  : aliased InvalidatedAreas_Vector;
      Index : Positive)
      return InvalidatedAreas_Constant_Reference with
     Inline;

   function Length (Self : BreakpointLocation_Vector) return Natural;

   procedure Clear (Self : in out BreakpointLocation_Vector);

   procedure Append
     (Self : in out BreakpointLocation_Vector; Value : BreakpointLocation);

   type BreakpointLocation_Variable_Reference
     (Element : not null access BreakpointLocation) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_BreakpointLocation_Variable_Reference
     (Self  : aliased in out BreakpointLocation_Vector;
      Index : Positive)
      return BreakpointLocation_Variable_Reference with
     Inline;

   type BreakpointLocation_Constant_Reference
     (Element : not null access constant BreakpointLocation) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_BreakpointLocation_Constant_Reference
     (Self  : aliased BreakpointLocation_Vector;
      Index : Positive)
      return BreakpointLocation_Constant_Reference with
     Inline;

   function Length (Self : InstructionBreakpoint_Vector) return Natural;

   procedure Clear (Self : in out InstructionBreakpoint_Vector);

   procedure Append
     (Self  : in out InstructionBreakpoint_Vector;
      Value : InstructionBreakpoint);

   type InstructionBreakpoint_Variable_Reference
     (Element : not null access InstructionBreakpoint) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_InstructionBreakpoint_Variable_Reference
     (Self  : aliased in out InstructionBreakpoint_Vector;
      Index : Positive)
      return InstructionBreakpoint_Variable_Reference with
     Inline;

   type InstructionBreakpoint_Constant_Reference
     (Element : not null access constant InstructionBreakpoint) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_InstructionBreakpoint_Constant_Reference
     (Self  : aliased InstructionBreakpoint_Vector;
      Index : Positive)
      return InstructionBreakpoint_Constant_Reference with
     Inline;

   function Length (Self : StackFrame_Vector) return Natural;

   procedure Clear (Self : in out StackFrame_Vector);

   procedure Append (Self : in out StackFrame_Vector; Value : StackFrame);

   type StackFrame_Variable_Reference (Element : not null access StackFrame) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_StackFrame_Variable_Reference
     (Self  : aliased in out StackFrame_Vector;
      Index : Positive)
      return StackFrame_Variable_Reference with
     Inline;

   type StackFrame_Constant_Reference
     (Element : not null access constant StackFrame) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_StackFrame_Constant_Reference
     (Self  : aliased StackFrame_Vector;
      Index : Positive)
      return StackFrame_Constant_Reference with
     Inline;

   function Length (Self : Scope_Vector) return Natural;

   procedure Clear (Self : in out Scope_Vector);

   procedure Append (Self : in out Scope_Vector; Value : Scope);

   type Scope_Variable_Reference (Element : not null access Scope) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Scope_Variable_Reference
     (Self  : aliased in out Scope_Vector;
      Index : Positive)
      return Scope_Variable_Reference with
     Inline;

   type Scope_Constant_Reference (Element : not null access constant Scope) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Scope_Constant_Reference
     (Self  : aliased Scope_Vector;
      Index : Positive)
      return Scope_Constant_Reference with
     Inline;

   function Length (Self : Variable_Vector) return Natural;

   procedure Clear (Self : in out Variable_Vector);

   procedure Append (Self : in out Variable_Vector; Value : Variable);

   type Variable_Variable_Reference (Element : not null access Variable) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Variable_Variable_Reference
     (Self  : aliased in out Variable_Vector;
      Index : Positive)
      return Variable_Variable_Reference with
     Inline;

   type Variable_Constant_Reference
     (Element : not null access constant Variable) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Variable_Constant_Reference
     (Self  : aliased Variable_Vector;
      Index : Positive)
      return Variable_Constant_Reference with
     Inline;

   function Length (Self : Source_Vector) return Natural;

   procedure Clear (Self : in out Source_Vector);

   procedure Append (Self : in out Source_Vector; Value : Source);

   type Source_Variable_Reference (Element : not null access Source) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Source_Variable_Reference
     (Self  : aliased in out Source_Vector;
      Index : Positive)
      return Source_Variable_Reference with
     Inline;

   type Source_Constant_Reference (Element : not null access constant Source)
   is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Source_Constant_Reference
     (Self  : aliased Source_Vector;
      Index : Positive)
      return Source_Constant_Reference with
     Inline;

   function Length (Self : SourceBreakpoint_Vector) return Natural;

   procedure Clear (Self : in out SourceBreakpoint_Vector);

   procedure Append
     (Self : in out SourceBreakpoint_Vector; Value : SourceBreakpoint);

   type SourceBreakpoint_Variable_Reference
     (Element : not null access SourceBreakpoint) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_SourceBreakpoint_Variable_Reference
     (Self  : aliased in out SourceBreakpoint_Vector;
      Index : Positive)
      return SourceBreakpoint_Variable_Reference with
     Inline;

   type SourceBreakpoint_Constant_Reference
     (Element : not null access constant SourceBreakpoint) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_SourceBreakpoint_Constant_Reference
     (Self  : aliased SourceBreakpoint_Vector;
      Index : Positive)
      return SourceBreakpoint_Constant_Reference with
     Inline;

   function Length (Self : ChecksumAlgorithm_Vector) return Natural;

   procedure Clear (Self : in out ChecksumAlgorithm_Vector);

   procedure Append
     (Self : in out ChecksumAlgorithm_Vector; Value : Enum.ChecksumAlgorithm);

   type ChecksumAlgorithm_Variable_Reference
     (Element : not null access Enum.ChecksumAlgorithm) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_ChecksumAlgorithm_Variable_Reference
     (Self  : aliased in out ChecksumAlgorithm_Vector;
      Index : Positive)
      return ChecksumAlgorithm_Variable_Reference with
     Inline;

   type ChecksumAlgorithm_Constant_Reference
     (Element : not null access constant Enum.ChecksumAlgorithm) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_ChecksumAlgorithm_Constant_Reference
     (Self  : aliased ChecksumAlgorithm_Vector;
      Index : Positive)
      return ChecksumAlgorithm_Constant_Reference with
     Inline;

   function Length (Self : ExceptionBreakpointsFilter_Vector) return Natural;

   procedure Clear (Self : in out ExceptionBreakpointsFilter_Vector);

   procedure Append
     (Self  : in out ExceptionBreakpointsFilter_Vector;
      Value : ExceptionBreakpointsFilter);

   type ExceptionBreakpointsFilter_Variable_Reference
     (Element : not null access ExceptionBreakpointsFilter) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_ExceptionBreakpointsFilter_Variable_Reference
     (Self  : aliased in out ExceptionBreakpointsFilter_Vector;
      Index : Positive)
      return ExceptionBreakpointsFilter_Variable_Reference with
     Inline;

   type ExceptionBreakpointsFilter_Constant_Reference
     (Element : not null access constant ExceptionBreakpointsFilter) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_ExceptionBreakpointsFilter_Constant_Reference
     (Self  : aliased ExceptionBreakpointsFilter_Vector;
      Index : Positive)
      return ExceptionBreakpointsFilter_Constant_Reference with
     Inline;

   function Length (Self : CompletionItem_Vector) return Natural;

   procedure Clear (Self : in out CompletionItem_Vector);

   procedure Append
     (Self : in out CompletionItem_Vector; Value : CompletionItem);

   type CompletionItem_Variable_Reference
     (Element : not null access CompletionItem) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_CompletionItem_Variable_Reference
     (Self  : aliased in out CompletionItem_Vector;
      Index : Positive)
      return CompletionItem_Variable_Reference with
     Inline;

   type CompletionItem_Constant_Reference
     (Element : not null access constant CompletionItem) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_CompletionItem_Constant_Reference
     (Self  : aliased CompletionItem_Vector;
      Index : Positive)
      return CompletionItem_Constant_Reference with
     Inline;

   function Length (Self : ExceptionPathSegment_Vector) return Natural;

   procedure Clear (Self : in out ExceptionPathSegment_Vector);

   procedure Append
     (Self : in out ExceptionPathSegment_Vector; Value : ExceptionPathSegment);

   type ExceptionPathSegment_Variable_Reference
     (Element : not null access ExceptionPathSegment) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_ExceptionPathSegment_Variable_Reference
     (Self  : aliased in out ExceptionPathSegment_Vector;
      Index : Positive)
      return ExceptionPathSegment_Variable_Reference with
     Inline;

   type ExceptionPathSegment_Constant_Reference
     (Element : not null access constant ExceptionPathSegment) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_ExceptionPathSegment_Constant_Reference
     (Self  : aliased ExceptionPathSegment_Vector;
      Index : Positive)
      return ExceptionPathSegment_Constant_Reference with
     Inline;

   function Length (Self : DataBreakpointAccessType_Vector) return Natural;

   procedure Clear (Self : in out DataBreakpointAccessType_Vector);

   procedure Append
     (Self  : in out DataBreakpointAccessType_Vector;
      Value : Enum.DataBreakpointAccessType);

   type DataBreakpointAccessType_Variable_Reference
     (Element : not null access Enum.DataBreakpointAccessType) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_DataBreakpointAccessType_Variable_Reference
     (Self  : aliased in out DataBreakpointAccessType_Vector;
      Index : Positive)
      return DataBreakpointAccessType_Variable_Reference with
     Inline;

   type DataBreakpointAccessType_Constant_Reference
     (Element : not null access constant Enum.DataBreakpointAccessType) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_DataBreakpointAccessType_Constant_Reference
     (Self  : aliased DataBreakpointAccessType_Vector;
      Index : Positive)
      return DataBreakpointAccessType_Constant_Reference with
     Inline;

   function Length (Self : DisassembledInstruction_Vector) return Natural;

   procedure Clear (Self : in out DisassembledInstruction_Vector);

   procedure Append
     (Self  : in out DisassembledInstruction_Vector;
      Value : DisassembledInstruction);

   type DisassembledInstruction_Variable_Reference
     (Element : not null access DisassembledInstruction) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_DisassembledInstruction_Variable_Reference
     (Self  : aliased in out DisassembledInstruction_Vector;
      Index : Positive)
      return DisassembledInstruction_Variable_Reference with
     Inline;

   type DisassembledInstruction_Constant_Reference
     (Element : not null access constant DisassembledInstruction) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_DisassembledInstruction_Constant_Reference
     (Self  : aliased DisassembledInstruction_Vector;
      Index : Positive)
      return DisassembledInstruction_Constant_Reference with
     Inline;

   function Length (Self : Module_Vector) return Natural;

   procedure Clear (Self : in out Module_Vector);

   procedure Append (Self : in out Module_Vector; Value : Module);

   type Module_Variable_Reference (Element : not null access Module) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Module_Variable_Reference
     (Self  : aliased in out Module_Vector;
      Index : Positive)
      return Module_Variable_Reference with
     Inline;

   type Module_Constant_Reference (Element : not null access constant Module)
   is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Module_Constant_Reference
     (Self  : aliased Module_Vector;
      Index : Positive)
      return Module_Constant_Reference with
     Inline;

   function Length (Self : ExceptionFilterOptions_Vector) return Natural;

   procedure Clear (Self : in out ExceptionFilterOptions_Vector);

   procedure Append
     (Self  : in out ExceptionFilterOptions_Vector;
      Value : ExceptionFilterOptions);

   type ExceptionFilterOptions_Variable_Reference
     (Element : not null access ExceptionFilterOptions) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_ExceptionFilterOptions_Variable_Reference
     (Self  : aliased in out ExceptionFilterOptions_Vector;
      Index : Positive)
      return ExceptionFilterOptions_Variable_Reference with
     Inline;

   type ExceptionFilterOptions_Constant_Reference
     (Element : not null access constant ExceptionFilterOptions) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_ExceptionFilterOptions_Constant_Reference
     (Self  : aliased ExceptionFilterOptions_Vector;
      Index : Positive)
      return ExceptionFilterOptions_Constant_Reference with
     Inline;

   function Length (Self : ColumnDescriptor_Vector) return Natural;

   procedure Clear (Self : in out ColumnDescriptor_Vector);

   procedure Append
     (Self : in out ColumnDescriptor_Vector; Value : ColumnDescriptor);

   type ColumnDescriptor_Variable_Reference
     (Element : not null access ColumnDescriptor) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_ColumnDescriptor_Variable_Reference
     (Self  : aliased in out ColumnDescriptor_Vector;
      Index : Positive)
      return ColumnDescriptor_Variable_Reference with
     Inline;

   type ColumnDescriptor_Constant_Reference
     (Element : not null access constant ColumnDescriptor) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_ColumnDescriptor_Constant_Reference
     (Self  : aliased ColumnDescriptor_Vector;
      Index : Positive)
      return ColumnDescriptor_Constant_Reference with
     Inline;

   function Length (Self : ExceptionDetails_Vector) return Natural;

   procedure Clear (Self : in out ExceptionDetails_Vector);

   procedure Append
     (Self : in out ExceptionDetails_Vector; Value : ExceptionDetails);

   type ExceptionDetails_Variable_Reference
     (Element : not null access ExceptionDetails) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_ExceptionDetails_Variable_Reference
     (Self  : aliased in out ExceptionDetails_Vector;
      Index : Positive)
      return ExceptionDetails_Variable_Reference with
     Inline;

   type ExceptionDetails_Constant_Reference
     (Element : not null access constant ExceptionDetails) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_ExceptionDetails_Constant_Reference
     (Self  : aliased ExceptionDetails_Vector;
      Index : Positive)
      return ExceptionDetails_Constant_Reference with
     Inline;

private
   type Thread_Array is array (Positive range <>) of aliased Thread;
   type Thread_Array_Access is access Thread_Array;
   type Thread_Vector is new Ada.Finalization.Controlled with record
      Data   : Thread_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Thread_Vector);

   overriding procedure Finalize (Self : in out Thread_Vector);

   type Checksum_Array is array (Positive range <>) of aliased Checksum;
   type Checksum_Array_Access is access Checksum_Array;
   type Checksum_Vector is new Ada.Finalization.Controlled with record
      Data   : Checksum_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Checksum_Vector);

   overriding procedure Finalize (Self : in out Checksum_Vector);

   type Breakpoint_Array is array (Positive range <>) of aliased Breakpoint;
   type Breakpoint_Array_Access is access Breakpoint_Array;
   type Breakpoint_Vector is new Ada.Finalization.Controlled with record
      Data   : Breakpoint_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Breakpoint_Vector);

   overriding procedure Finalize (Self : in out Breakpoint_Vector);

   type StepInTarget_Array is
     array (Positive range <>) of aliased StepInTarget;
   type StepInTarget_Array_Access is access StepInTarget_Array;
   type StepInTarget_Vector is new Ada.Finalization.Controlled with record
      Data   : StepInTarget_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out StepInTarget_Vector);

   overriding procedure Finalize (Self : in out StepInTarget_Vector);

   type FunctionBreakpoint_Array is
     array (Positive range <>) of aliased FunctionBreakpoint;
   type FunctionBreakpoint_Array_Access is access FunctionBreakpoint_Array;
   type FunctionBreakpoint_Vector is
   new Ada.Finalization.Controlled with record
      Data   : FunctionBreakpoint_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out FunctionBreakpoint_Vector);

   overriding procedure Finalize (Self : in out FunctionBreakpoint_Vector);

   type DataBreakpoint_Array is
     array (Positive range <>) of aliased DataBreakpoint;
   type DataBreakpoint_Array_Access is access DataBreakpoint_Array;
   type DataBreakpoint_Vector is new Ada.Finalization.Controlled with record
      Data   : DataBreakpoint_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out DataBreakpoint_Vector);

   overriding procedure Finalize (Self : in out DataBreakpoint_Vector);

   type ExceptionOptions_Array is
     array (Positive range <>) of aliased ExceptionOptions;
   type ExceptionOptions_Array_Access is access ExceptionOptions_Array;
   type ExceptionOptions_Vector is new Ada.Finalization.Controlled with record
      Data   : ExceptionOptions_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out ExceptionOptions_Vector);

   overriding procedure Finalize (Self : in out ExceptionOptions_Vector);

   type Integer_Array is array (Positive range <>) of aliased Integer;
   type Integer_Array_Access is access Integer_Array;
   type Integer_Vector is new Ada.Finalization.Controlled with record
      Data   : Integer_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Integer_Vector);

   overriding procedure Finalize (Self : in out Integer_Vector);

   type GotoTarget_Array is array (Positive range <>) of aliased GotoTarget;
   type GotoTarget_Array_Access is access GotoTarget_Array;
   type GotoTarget_Vector is new Ada.Finalization.Controlled with record
      Data   : GotoTarget_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out GotoTarget_Vector);

   overriding procedure Finalize (Self : in out GotoTarget_Vector);

   type InvalidatedAreas_Array is
     array (Positive range <>) of aliased Enum.InvalidatedAreas;
   type InvalidatedAreas_Array_Access is access InvalidatedAreas_Array;
   type InvalidatedAreas_Vector is new Ada.Finalization.Controlled with record
      Data   : InvalidatedAreas_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out InvalidatedAreas_Vector);

   overriding procedure Finalize (Self : in out InvalidatedAreas_Vector);

   type BreakpointLocation_Array is
     array (Positive range <>) of aliased BreakpointLocation;
   type BreakpointLocation_Array_Access is access BreakpointLocation_Array;
   type BreakpointLocation_Vector is
   new Ada.Finalization.Controlled with record
      Data   : BreakpointLocation_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out BreakpointLocation_Vector);

   overriding procedure Finalize (Self : in out BreakpointLocation_Vector);

   type InstructionBreakpoint_Array is
     array (Positive range <>) of aliased InstructionBreakpoint;
   type InstructionBreakpoint_Array_Access is
     access InstructionBreakpoint_Array;
   type InstructionBreakpoint_Vector is
   new Ada.Finalization.Controlled with record
      Data   : InstructionBreakpoint_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out InstructionBreakpoint_Vector);

   overriding procedure Finalize (Self : in out InstructionBreakpoint_Vector);

   type StackFrame_Array is array (Positive range <>) of aliased StackFrame;
   type StackFrame_Array_Access is access StackFrame_Array;
   type StackFrame_Vector is new Ada.Finalization.Controlled with record
      Data   : StackFrame_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out StackFrame_Vector);

   overriding procedure Finalize (Self : in out StackFrame_Vector);

   type Scope_Array is array (Positive range <>) of aliased Scope;
   type Scope_Array_Access is access Scope_Array;
   type Scope_Vector is new Ada.Finalization.Controlled with record
      Data   : Scope_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Scope_Vector);

   overriding procedure Finalize (Self : in out Scope_Vector);

   type Variable_Array is array (Positive range <>) of aliased Variable;
   type Variable_Array_Access is access Variable_Array;
   type Variable_Vector is new Ada.Finalization.Controlled with record
      Data   : Variable_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Variable_Vector);

   overriding procedure Finalize (Self : in out Variable_Vector);

   type Source_Array is array (Positive range <>) of aliased Source;
   type Source_Array_Access is access Source_Array;
   type Source_Vector is new Ada.Finalization.Controlled with record
      Data   : Source_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Source_Vector);

   overriding procedure Finalize (Self : in out Source_Vector);

   type SourceBreakpoint_Array is
     array (Positive range <>) of aliased SourceBreakpoint;
   type SourceBreakpoint_Array_Access is access SourceBreakpoint_Array;
   type SourceBreakpoint_Vector is new Ada.Finalization.Controlled with record
      Data   : SourceBreakpoint_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out SourceBreakpoint_Vector);

   overriding procedure Finalize (Self : in out SourceBreakpoint_Vector);

   type ChecksumAlgorithm_Array is
     array (Positive range <>) of aliased Enum.ChecksumAlgorithm;
   type ChecksumAlgorithm_Array_Access is access ChecksumAlgorithm_Array;
   type ChecksumAlgorithm_Vector is new Ada.Finalization.Controlled with record
      Data   : ChecksumAlgorithm_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out ChecksumAlgorithm_Vector);

   overriding procedure Finalize (Self : in out ChecksumAlgorithm_Vector);

   type ExceptionBreakpointsFilter_Array is
     array (Positive range <>) of aliased ExceptionBreakpointsFilter;
   type ExceptionBreakpointsFilter_Array_Access is
     access ExceptionBreakpointsFilter_Array;
   type ExceptionBreakpointsFilter_Vector is
   new Ada.Finalization.Controlled with record
      Data   : ExceptionBreakpointsFilter_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust
     (Self : in out ExceptionBreakpointsFilter_Vector);

   overriding procedure Finalize
     (Self : in out ExceptionBreakpointsFilter_Vector);

   type CompletionItem_Array is
     array (Positive range <>) of aliased CompletionItem;
   type CompletionItem_Array_Access is access CompletionItem_Array;
   type CompletionItem_Vector is new Ada.Finalization.Controlled with record
      Data   : CompletionItem_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out CompletionItem_Vector);

   overriding procedure Finalize (Self : in out CompletionItem_Vector);

   type ExceptionPathSegment_Array is
     array (Positive range <>) of aliased ExceptionPathSegment;
   type ExceptionPathSegment_Array_Access is access ExceptionPathSegment_Array;
   type ExceptionPathSegment_Vector is
   new Ada.Finalization.Controlled with record
      Data   : ExceptionPathSegment_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out ExceptionPathSegment_Vector);

   overriding procedure Finalize (Self : in out ExceptionPathSegment_Vector);

   type DataBreakpointAccessType_Array is
     array (Positive range <>) of aliased Enum.DataBreakpointAccessType;
   type DataBreakpointAccessType_Array_Access is
     access DataBreakpointAccessType_Array;
   type DataBreakpointAccessType_Vector is
   new Ada.Finalization.Controlled with record
      Data   : DataBreakpointAccessType_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out DataBreakpointAccessType_Vector);

   overriding procedure Finalize
     (Self : in out DataBreakpointAccessType_Vector);

   type DisassembledInstruction_Array is
     array (Positive range <>) of aliased DisassembledInstruction;
   type DisassembledInstruction_Array_Access is
     access DisassembledInstruction_Array;
   type DisassembledInstruction_Vector is
   new Ada.Finalization.Controlled with record
      Data   : DisassembledInstruction_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out DisassembledInstruction_Vector);

   overriding procedure Finalize
     (Self : in out DisassembledInstruction_Vector);

   type Module_Array is array (Positive range <>) of aliased Module;
   type Module_Array_Access is access Module_Array;
   type Module_Vector is new Ada.Finalization.Controlled with record
      Data   : Module_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Module_Vector);

   overriding procedure Finalize (Self : in out Module_Vector);

   type ExceptionFilterOptions_Array is
     array (Positive range <>) of aliased ExceptionFilterOptions;
   type ExceptionFilterOptions_Array_Access is
     access ExceptionFilterOptions_Array;
   type ExceptionFilterOptions_Vector is
   new Ada.Finalization.Controlled with record
      Data   : ExceptionFilterOptions_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out ExceptionFilterOptions_Vector);

   overriding procedure Finalize (Self : in out ExceptionFilterOptions_Vector);

   type ColumnDescriptor_Array is
     array (Positive range <>) of aliased ColumnDescriptor;
   type ColumnDescriptor_Array_Access is access ColumnDescriptor_Array;
   type ColumnDescriptor_Vector is new Ada.Finalization.Controlled with record
      Data   : ColumnDescriptor_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out ColumnDescriptor_Vector);

   overriding procedure Finalize (Self : in out ColumnDescriptor_Vector);

   type ExceptionDetails_Array is
     array (Positive range <>) of aliased ExceptionDetails;
   type ExceptionDetails_Array_Access is access ExceptionDetails_Array;
   type ExceptionDetails_Vector is new Ada.Finalization.Controlled with record
      Data   : ExceptionDetails_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out ExceptionDetails_Vector);

   overriding procedure Finalize (Self : in out ExceptionDetails_Vector);

end DAP.Tools;
