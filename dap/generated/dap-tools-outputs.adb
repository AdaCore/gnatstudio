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

with Interfaces;
package body DAP.Tools.Outputs is
   pragma Style_Checks (Off);
   procedure Output_Any_Value
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Any_Value'Class) is
   begin
      for Item of Value loop
         case Item.Kind is
            when VSS.JSON.Events.Start_Array =>
               Handler.Start_Array;
            when VSS.JSON.Events.End_Array =>
               Handler.End_Array;
            when VSS.JSON.Events.Start_Object =>
               Handler.Start_Object;
            when VSS.JSON.Events.End_Object =>
               Handler.End_Object;
            when VSS.JSON.Events.Key_Name =>
               Handler.Key_Name (Item.Key);
            when VSS.JSON.Events.String_Value =>
               Handler.String_Value (Item.String_Value);
            when VSS.JSON.Events.Number_Value =>
               Handler.Number_Value (Item.Number_Value);
            when VSS.JSON.Events.Boolean_Value =>
               Handler.Boolean_Value (Item.Boolean_Value);
            when VSS.JSON.Events.Null_Value =>
               Handler.Null_Value;
            when VSS.JSON.Events.None =>
               null;
         end case;
      end loop;
   end Output_Any_Value;

   procedure Output_ModuleEvent_reason
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.ModuleEvent_reason) is
   begin
      case Value is
         when Enum.a_new =>
            Handler.String_Value ("new");
         when Enum.changed =>
            Handler.String_Value ("changed");
         when Enum.removed =>
            Handler.String_Value ("removed");
      end case;
   end Output_ModuleEvent_reason;

   procedure Output_ColumnDescriptor_type
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.ColumnDescriptor_type) is
   begin
      case Value is
         when Enum.string =>
            Handler.String_Value ("string");
         when Enum.number =>
            Handler.String_Value ("number");
         when Enum.a_boolean =>
            Handler.String_Value ("boolean");
         when Enum.unixTimestampUTC =>
            Handler.String_Value ("unixTimestampUTC");
      end case;
   end Output_ColumnDescriptor_type;

   procedure Output_StackFrame_presentationHint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.StackFrame_presentationHint) is
   begin
      case Value is
         when Enum.normal =>
            Handler.String_Value ("normal");
         when Enum.label =>
            Handler.String_Value ("label");
         when Enum.subtle =>
            Handler.String_Value ("subtle");
      end case;
   end Output_StackFrame_presentationHint;

   procedure Output_ExceptionBreakMode
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.ExceptionBreakMode) is
   begin
      case Value is
         when Enum.never =>
            Handler.String_Value ("never");
         when Enum.always =>
            Handler.String_Value ("always");
         when Enum.unhandled =>
            Handler.String_Value ("unhandled");
         when Enum.userUnhandled =>
            Handler.String_Value ("userUnhandled");
      end case;
   end Output_ExceptionBreakMode;

   procedure Output_StoppedEvent_reason
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.StoppedEvent_reason) is
   begin
      case Value is
         when Enum.step =>
            Handler.String_Value ("step");
         when Enum.breakpoint =>
            Handler.String_Value ("breakpoint");
         when Enum.a_exception =>
            Handler.String_Value ("exception");
         when Enum.pause =>
            Handler.String_Value ("pause");
         when Enum.a_entry =>
            Handler.String_Value ("entry");
         when Enum.a_goto =>
            Handler.String_Value ("goto");
         when Enum.function_breakpoint =>
            Handler.String_Value ("function breakpoint");
         when Enum.data_breakpoint =>
            Handler.String_Value ("data breakpoint");
         when Enum.instruction_breakpoint =>
            Handler.String_Value ("instruction breakpoint");
      end case;
   end Output_StoppedEvent_reason;

   procedure Output_StartDebuggingRequestArguments_request
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.StartDebuggingRequestArguments_request) is
   begin
      case Value is
         when Enum.launch =>
            Handler.String_Value ("launch");
         when Enum.attach =>
            Handler.String_Value ("attach");
      end case;
   end Output_StartDebuggingRequestArguments_request;

   procedure Output_OutputEvent_category
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.OutputEvent_category) is
   begin
      case Value is
         when Enum.console =>
            Handler.String_Value ("console");
         when Enum.important =>
            Handler.String_Value ("important");
         when Enum.stdout =>
            Handler.String_Value ("stdout");
         when Enum.stderr =>
            Handler.String_Value ("stderr");
         when Enum.telemetry =>
            Handler.String_Value ("telemetry");
      end case;
   end Output_OutputEvent_category;

   procedure Output_OutputEvent_group
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.OutputEvent_group) is
   begin
      case Value is
         when Enum.start =>
            Handler.String_Value ("start");
         when Enum.startCollapsed =>
            Handler.String_Value ("startCollapsed");
         when Enum.a_end =>
            Handler.String_Value ("end");
      end case;
   end Output_OutputEvent_group;

   procedure Output_ChecksumAlgorithm
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.ChecksumAlgorithm) is
   begin
      case Value is
         when Enum.MD5 =>
            Handler.String_Value ("MD5");
         when Enum.SHA1 =>
            Handler.String_Value ("SHA1");
         when Enum.SHA256 =>
            Handler.String_Value ("SHA256");
         when Enum.timestamp =>
            Handler.String_Value ("timestamp");
      end case;
   end Output_ChecksumAlgorithm;

   procedure Output_ProcessEvent_startMethod
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.ProcessEvent_startMethod) is
   begin
      case Value is
         when Enum.launch =>
            Handler.String_Value ("launch");
         when Enum.attach =>
            Handler.String_Value ("attach");
         when Enum.attachForSuspendedLaunch =>
            Handler.String_Value ("attachForSuspendedLaunch");
      end case;
   end Output_ProcessEvent_startMethod;

   procedure Output_Scope_presentationHint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.Scope_presentationHint) is
   begin
      case Value is
         when Enum.arguments =>
            Handler.String_Value ("arguments");
         when Enum.locals =>
            Handler.String_Value ("locals");
         when Enum.registers =>
            Handler.String_Value ("registers");
      end case;
   end Output_Scope_presentationHint;

   procedure Output_Response_message
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.Response_message) is
   begin
      case Value is
         when Enum.cancelled =>
            Handler.String_Value ("cancelled");
         when Enum.notStopped =>
            Handler.String_Value ("notStopped");
      end case;
   end Output_Response_message;

   procedure Output_CompletionItemType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.CompletionItemType) is
   begin
      case Value is
         when Enum.method =>
            Handler.String_Value ("method");
         when Enum.a_function =>
            Handler.String_Value ("function");
         when Enum.constructor =>
            Handler.String_Value ("constructor");
         when Enum.field =>
            Handler.String_Value ("field");
         when Enum.variable =>
            Handler.String_Value ("variable");
         when Enum.class =>
            Handler.String_Value ("class");
         when Enum.an_interface =>
            Handler.String_Value ("interface");
         when Enum.module =>
            Handler.String_Value ("module");
         when Enum.property =>
            Handler.String_Value ("property");
         when Enum.unit =>
            Handler.String_Value ("unit");
         when Enum.value =>
            Handler.String_Value ("value");
         when Enum.enum =>
            Handler.String_Value ("enum");
         when Enum.keyword =>
            Handler.String_Value ("keyword");
         when Enum.snippet =>
            Handler.String_Value ("snippet");
         when Enum.text =>
            Handler.String_Value ("text");
         when Enum.color =>
            Handler.String_Value ("color");
         when Enum.file =>
            Handler.String_Value ("file");
         when Enum.reference =>
            Handler.String_Value ("reference");
         when Enum.customcolor =>
            Handler.String_Value ("customcolor");
      end case;
   end Output_CompletionItemType;

   procedure Output_InvalidatedAreas
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.InvalidatedAreas) is
   begin
      case Value is
         when Enum.an_all =>
            Handler.String_Value ("all");
         when Enum.stacks =>
            Handler.String_Value ("stacks");
         when Enum.threads =>
            Handler.String_Value ("threads");
         when Enum.variables =>
            Handler.String_Value ("variables");
      end case;
   end Output_InvalidatedAreas;

   procedure Output_Source_presentationHint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.Source_presentationHint) is
   begin
      case Value is
         when Enum.normal =>
            Handler.String_Value ("normal");
         when Enum.emphasize =>
            Handler.String_Value ("emphasize");
         when Enum.deemphasize =>
            Handler.String_Value ("deemphasize");
      end case;
   end Output_Source_presentationHint;

   procedure Output_LoadedSourceEvent_reason
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.LoadedSourceEvent_reason) is
   begin
      case Value is
         when Enum.a_new =>
            Handler.String_Value ("new");
         when Enum.changed =>
            Handler.String_Value ("changed");
         when Enum.removed =>
            Handler.String_Value ("removed");
      end case;
   end Output_LoadedSourceEvent_reason;

   procedure Output_ProtocolMessage_type
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.ProtocolMessage_type) is
   begin
      case Value is
         when Enum.request =>
            Handler.String_Value ("request");
         when Enum.response =>
            Handler.String_Value ("response");
         when Enum.event =>
            Handler.String_Value ("event");
      end case;
   end Output_ProtocolMessage_type;

   procedure Output_RunInTerminalRequestArguments_kind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.RunInTerminalRequestArguments_kind) is
   begin
      case Value is
         when Enum.integrated =>
            Handler.String_Value ("integrated");
         when Enum.external =>
            Handler.String_Value ("external");
      end case;
   end Output_RunInTerminalRequestArguments_kind;

   procedure Output_VariablesArguments_filter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.VariablesArguments_filter) is
   begin
      case Value is
         when Enum.indexed =>
            Handler.String_Value ("indexed");
         when Enum.named =>
            Handler.String_Value ("named");
      end case;
   end Output_VariablesArguments_filter;

   procedure Output_VariablePresentationHint_kind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.VariablePresentationHint_kind) is
   begin
      case Value is
         when Enum.property =>
            Handler.String_Value ("property");
         when Enum.method =>
            Handler.String_Value ("method");
         when Enum.class =>
            Handler.String_Value ("class");
         when Enum.data =>
            Handler.String_Value ("data");
         when Enum.event =>
            Handler.String_Value ("event");
         when Enum.baseClass =>
            Handler.String_Value ("baseClass");
         when Enum.innerClass =>
            Handler.String_Value ("innerClass");
         when Enum.an_interface =>
            Handler.String_Value ("interface");
         when Enum.mostDerivedClass =>
            Handler.String_Value ("mostDerivedClass");
         when Enum.virtual =>
            Handler.String_Value ("virtual");
         when Enum.dataBreakpoint =>
            Handler.String_Value ("dataBreakpoint");
      end case;
   end Output_VariablePresentationHint_kind;

   procedure Output_VariablePresentationHint_visibility
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.VariablePresentationHint_visibility) is
   begin
      case Value is
         when Enum.public =>
            Handler.String_Value ("public");
         when Enum.a_private =>
            Handler.String_Value ("private");
         when Enum.a_protected =>
            Handler.String_Value ("protected");
         when Enum.internal =>
            Handler.String_Value ("internal");
         when Enum.final =>
            Handler.String_Value ("final");
      end case;
   end Output_VariablePresentationHint_visibility;

   procedure Output_InitializeRequestArguments_pathFormat
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.InitializeRequestArguments_pathFormat) is
   begin
      case Value is
         when Enum.path =>
            Handler.String_Value ("path");
         when Enum.uri =>
            Handler.String_Value ("uri");
      end case;
   end Output_InitializeRequestArguments_pathFormat;

   procedure Output_ThreadEvent_reason
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.ThreadEvent_reason) is
   begin
      case Value is
         when Enum.started =>
            Handler.String_Value ("started");
         when Enum.exited =>
            Handler.String_Value ("exited");
      end case;
   end Output_ThreadEvent_reason;

   procedure Output_DataBreakpointAccessType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.DataBreakpointAccessType) is
   begin
      case Value is
         when Enum.read =>
            Handler.String_Value ("read");
         when Enum.write =>
            Handler.String_Value ("write");
         when Enum.readWrite =>
            Handler.String_Value ("readWrite");
      end case;
   end Output_DataBreakpointAccessType;

   procedure Output_BreakpointEvent_reason
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.BreakpointEvent_reason) is
   begin
      case Value is
         when Enum.changed =>
            Handler.String_Value ("changed");
         when Enum.a_new =>
            Handler.String_Value ("new");
         when Enum.removed =>
            Handler.String_Value ("removed");
      end case;
   end Output_BreakpointEvent_reason;

   procedure Output_EvaluateArguments_context
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.EvaluateArguments_context) is
   begin
      case Value is
         when Enum.watch =>
            Handler.String_Value ("watch");
         when Enum.repl =>
            Handler.String_Value ("repl");
         when Enum.hover =>
            Handler.String_Value ("hover");
         when Enum.clipboard =>
            Handler.String_Value ("clipboard");
         when Enum.variables =>
            Handler.String_Value ("variables");
      end case;
   end Output_EvaluateArguments_context;

   procedure Output_SteppingGranularity
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.SteppingGranularity) is
   begin
      case Value is
         when Enum.statement =>
            Handler.String_Value ("statement");
         when Enum.line =>
            Handler.String_Value ("line");
         when Enum.instruction =>
            Handler.String_Value ("instruction");
      end case;
   end Output_SteppingGranularity;

   procedure Output_GotoResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : GotoResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_GotoResponse;

   procedure Output_ExceptionDetails
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionDetails) is
   begin
      Handler.Start_Object;
      if not Value.message.Is_Null then
         Handler.Key_Name ("message");
         Handler.String_Value (Value.message);
      end if;
      if not Value.typeName.Is_Null then
         Handler.Key_Name ("typeName");
         Handler.String_Value (Value.typeName);
      end if;
      if not Value.fullTypeName.Is_Null then
         Handler.Key_Name ("fullTypeName");
         Handler.String_Value (Value.fullTypeName);
      end if;
      if not Value.evaluateName.Is_Null then
         Handler.Key_Name ("evaluateName");
         Handler.String_Value (Value.evaluateName);
      end if;
      if not Value.stackTrace.Is_Null then
         Handler.Key_Name ("stackTrace");
         Handler.String_Value (Value.stackTrace);
      end if;
      if Value.innerException.Length > 0 then
         Handler.Key_Name ("innerException");
         Handler.Start_Array;
         for J in 1 .. Value.innerException.Length loop
            Output_ExceptionDetails (Handler, Value.innerException (J));
         end loop;
         Handler.End_Array;
      end if;
      Handler.End_Object;
   end Output_ExceptionDetails;

   procedure Output_StepInTargetsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepInTargetsRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("stepInTargets");
      Handler.Key_Name ("arguments");
      Output_StepInTargetsArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_StepInTargetsRequest;

   procedure Output_ModulesResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ModulesResponse) is
      procedure Output_ModulesResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : ModulesResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("modules");
         Handler.Start_Array;
         for J in 1 .. Value.modules.Length loop
            Output_Module (Handler, Value.modules (J));
         end loop;
         Handler.End_Array;
         if Value.totalModules.Is_Set then
            Handler.Key_Name ("totalModules");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.totalModules.Value)));
         end if;
         Handler.End_Object;
      end Output_ModulesResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_ModulesResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_ModulesResponse;

   procedure Output_NextArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : NextArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("threadId");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.threadId)));
      if Value.singleThread then
         Handler.Key_Name ("singleThread");
         Handler.Boolean_Value (Value.singleThread);
      end if;
      if Value.granularity.Is_Set then
         Handler.Key_Name ("granularity");
         Output_SteppingGranularity (Handler, Value.granularity.Value);
      end if;
      Handler.End_Object;
   end Output_NextArguments;

   procedure Output_ExceptionInfoRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionInfoRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("exceptionInfo");
      Handler.Key_Name ("arguments");
      Output_ExceptionInfoArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_ExceptionInfoRequest;

   procedure Output_TerminateThreadsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TerminateThreadsArguments) is
   begin
      Handler.Start_Object;
      if Value.threadIds.Length > 0 then
         Handler.Key_Name ("threadIds");
         Handler.Start_Array;
         for J in 1 .. Value.threadIds.Length loop
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.threadIds (J))));
         end loop;
         Handler.End_Array;
      end if;
      Handler.End_Object;
   end Output_TerminateThreadsArguments;

   procedure Output_DataBreakpointInfoRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DataBreakpointInfoRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("dataBreakpointInfo");
      Handler.Key_Name ("arguments");
      Output_DataBreakpointInfoArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_DataBreakpointInfoRequest;

   procedure Output_TerminateThreadsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TerminateThreadsRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("terminateThreads");
      Handler.Key_Name ("arguments");
      Output_TerminateThreadsArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_TerminateThreadsRequest;

   procedure Output_SetBreakpointsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetBreakpointsArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("source");
      Output_Source (Handler, Value.source);
      if Value.breakpoints.Length > 0 then
         Handler.Key_Name ("breakpoints");
         Handler.Start_Array;
         for J in 1 .. Value.breakpoints.Length loop
            Output_SourceBreakpoint (Handler, Value.breakpoints (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.lines.Length > 0 then
         Handler.Key_Name ("lines");
         Handler.Start_Array;
         for J in 1 .. Value.lines.Length loop
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.lines (J))));
         end loop;
         Handler.End_Array;
      end if;
      if Value.sourceModified then
         Handler.Key_Name ("sourceModified");
         Handler.Boolean_Value (Value.sourceModified);
      end if;
      Handler.End_Object;
   end Output_SetBreakpointsArguments;

   procedure Output_ModuleEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ModuleEvent) is
      procedure Output_ModuleEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : ModuleEvent_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("reason");
         Output_ModuleEvent_reason (Handler, Value.reason);
         Handler.Key_Name ("module");
         Output_Module (Handler, Value.module);
         Handler.End_Object;
      end Output_ModuleEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("module");
      Handler.Key_Name ("body");
      Output_ModuleEvent_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_ModuleEvent;

   procedure Output_ContinuedEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ContinuedEvent) is
      procedure Output_ContinuedEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : ContinuedEvent_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("threadId");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.threadId)));
         if Value.allThreadsContinued then
            Handler.Key_Name ("allThreadsContinued");
            Handler.Boolean_Value (Value.allThreadsContinued);
         end if;
         Handler.End_Object;
      end Output_ContinuedEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("continued");
      Handler.Key_Name ("body");
      Output_ContinuedEvent_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_ContinuedEvent;

   procedure Output_RestartArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RestartArguments) is
   begin
      Handler.Start_Object;
      if Value.arguments.Is_Set then
         Handler.Key_Name ("arguments");
         Output_AttachRequestArguments (Handler, Value.arguments.Value);
      end if;
      Handler.End_Object;
   end Output_RestartArguments;

   procedure Output_SetVariableArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetVariableArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("variablesReference");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.variablesReference)));
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      Handler.Key_Name ("value");
      Handler.String_Value (Value.value);
      if Value.format.Is_Set then
         Handler.Key_Name ("format");
         Output_ValueFormat (Handler, Value.format.Value);
      end if;
      Handler.End_Object;
   end Output_SetVariableArguments;

   procedure Output_Checksum
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Checksum) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("algorithm");
      Output_ChecksumAlgorithm (Handler, Value.algorithm);
      Handler.Key_Name ("checksum");
      Handler.String_Value (Value.checksum);
      Handler.End_Object;
   end Output_Checksum;

   procedure Output_BreakpointLocationsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : BreakpointLocationsRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("breakpointLocations");
      if Value.arguments.Is_Set then
         Handler.Key_Name ("arguments");
         Output_BreakpointLocationsArguments (Handler, Value.arguments.Value);
      end if;
      Handler.End_Object;
   end Output_BreakpointLocationsRequest;

   procedure Output_ModulesViewDescriptor
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ModulesViewDescriptor) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("columns");
      Handler.Start_Array;
      for J in 1 .. Value.columns.Length loop
         Output_ColumnDescriptor (Handler, Value.columns (J));
      end loop;
      Handler.End_Array;
      Handler.End_Object;
   end Output_ModulesViewDescriptor;

   procedure Output_ColumnDescriptor
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ColumnDescriptor) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("attributeName");
      Handler.String_Value (Value.attributeName);
      Handler.Key_Name ("label");
      Handler.String_Value (Value.label);
      if not Value.format.Is_Null then
         Handler.Key_Name ("format");
         Handler.String_Value (Value.format);
      end if;
      if Value.a_type.Is_Set then
         Handler.Key_Name ("type");
         Output_ColumnDescriptor_type (Handler, Value.a_type.Value);
      end if;
      if Value.width.Is_Set then
         Handler.Key_Name ("width");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.width.Value)));
      end if;
      Handler.End_Object;
   end Output_ColumnDescriptor;

   procedure Output_Capabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Capabilities) is
   begin
      Handler.Start_Object;
      if Value.supportsConfigurationDoneRequest then
         Handler.Key_Name ("supportsConfigurationDoneRequest");
         Handler.Boolean_Value (Value.supportsConfigurationDoneRequest);
      end if;
      if Value.supportsFunctionBreakpoints then
         Handler.Key_Name ("supportsFunctionBreakpoints");
         Handler.Boolean_Value (Value.supportsFunctionBreakpoints);
      end if;
      if Value.supportsConditionalBreakpoints then
         Handler.Key_Name ("supportsConditionalBreakpoints");
         Handler.Boolean_Value (Value.supportsConditionalBreakpoints);
      end if;
      if Value.supportsHitConditionalBreakpoints then
         Handler.Key_Name ("supportsHitConditionalBreakpoints");
         Handler.Boolean_Value (Value.supportsHitConditionalBreakpoints);
      end if;
      if Value.supportsEvaluateForHovers then
         Handler.Key_Name ("supportsEvaluateForHovers");
         Handler.Boolean_Value (Value.supportsEvaluateForHovers);
      end if;
      if Value.exceptionBreakpointFilters.Length > 0 then
         Handler.Key_Name ("exceptionBreakpointFilters");
         Handler.Start_Array;
         for J in 1 .. Value.exceptionBreakpointFilters.Length loop
            Output_ExceptionBreakpointsFilter
              (Handler, Value.exceptionBreakpointFilters (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.supportsStepBack then
         Handler.Key_Name ("supportsStepBack");
         Handler.Boolean_Value (Value.supportsStepBack);
      end if;
      if Value.supportsSetVariable then
         Handler.Key_Name ("supportsSetVariable");
         Handler.Boolean_Value (Value.supportsSetVariable);
      end if;
      if Value.supportsRestartFrame then
         Handler.Key_Name ("supportsRestartFrame");
         Handler.Boolean_Value (Value.supportsRestartFrame);
      end if;
      if Value.supportsGotoTargetsRequest then
         Handler.Key_Name ("supportsGotoTargetsRequest");
         Handler.Boolean_Value (Value.supportsGotoTargetsRequest);
      end if;
      if Value.supportsStepInTargetsRequest then
         Handler.Key_Name ("supportsStepInTargetsRequest");
         Handler.Boolean_Value (Value.supportsStepInTargetsRequest);
      end if;
      if Value.supportsCompletionsRequest then
         Handler.Key_Name ("supportsCompletionsRequest");
         Handler.Boolean_Value (Value.supportsCompletionsRequest);
      end if;
      if not Value.completionTriggerCharacters.Is_Empty then
         Handler.Key_Name ("completionTriggerCharacters");
         Handler.Start_Array;
         for J in 1 .. Value.completionTriggerCharacters.Length loop
            Handler.String_Value (Value.completionTriggerCharacters (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.supportsModulesRequest then
         Handler.Key_Name ("supportsModulesRequest");
         Handler.Boolean_Value (Value.supportsModulesRequest);
      end if;
      if Value.additionalModuleColumns.Length > 0 then
         Handler.Key_Name ("additionalModuleColumns");
         Handler.Start_Array;
         for J in 1 .. Value.additionalModuleColumns.Length loop
            Output_ColumnDescriptor
              (Handler, Value.additionalModuleColumns (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.supportedChecksumAlgorithms.Length > 0 then
         Handler.Key_Name ("supportedChecksumAlgorithms");
         Handler.Start_Array;
         for J in 1 .. Value.supportedChecksumAlgorithms.Length loop
            Output_ChecksumAlgorithm
              (Handler, Value.supportedChecksumAlgorithms (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.supportsRestartRequest then
         Handler.Key_Name ("supportsRestartRequest");
         Handler.Boolean_Value (Value.supportsRestartRequest);
      end if;
      if Value.supportsExceptionOptions then
         Handler.Key_Name ("supportsExceptionOptions");
         Handler.Boolean_Value (Value.supportsExceptionOptions);
      end if;
      if Value.supportsValueFormattingOptions then
         Handler.Key_Name ("supportsValueFormattingOptions");
         Handler.Boolean_Value (Value.supportsValueFormattingOptions);
      end if;
      if Value.supportsExceptionInfoRequest then
         Handler.Key_Name ("supportsExceptionInfoRequest");
         Handler.Boolean_Value (Value.supportsExceptionInfoRequest);
      end if;
      if Value.supportTerminateDebuggee then
         Handler.Key_Name ("supportTerminateDebuggee");
         Handler.Boolean_Value (Value.supportTerminateDebuggee);
      end if;
      if Value.supportSuspendDebuggee then
         Handler.Key_Name ("supportSuspendDebuggee");
         Handler.Boolean_Value (Value.supportSuspendDebuggee);
      end if;
      if Value.supportsDelayedStackTraceLoading then
         Handler.Key_Name ("supportsDelayedStackTraceLoading");
         Handler.Boolean_Value (Value.supportsDelayedStackTraceLoading);
      end if;
      if Value.supportsLoadedSourcesRequest then
         Handler.Key_Name ("supportsLoadedSourcesRequest");
         Handler.Boolean_Value (Value.supportsLoadedSourcesRequest);
      end if;
      if Value.supportsLogPoints then
         Handler.Key_Name ("supportsLogPoints");
         Handler.Boolean_Value (Value.supportsLogPoints);
      end if;
      if Value.supportsTerminateThreadsRequest then
         Handler.Key_Name ("supportsTerminateThreadsRequest");
         Handler.Boolean_Value (Value.supportsTerminateThreadsRequest);
      end if;
      if Value.supportsSetExpression then
         Handler.Key_Name ("supportsSetExpression");
         Handler.Boolean_Value (Value.supportsSetExpression);
      end if;
      if Value.supportsTerminateRequest then
         Handler.Key_Name ("supportsTerminateRequest");
         Handler.Boolean_Value (Value.supportsTerminateRequest);
      end if;
      if Value.supportsDataBreakpoints then
         Handler.Key_Name ("supportsDataBreakpoints");
         Handler.Boolean_Value (Value.supportsDataBreakpoints);
      end if;
      if Value.supportsReadMemoryRequest then
         Handler.Key_Name ("supportsReadMemoryRequest");
         Handler.Boolean_Value (Value.supportsReadMemoryRequest);
      end if;
      if Value.supportsWriteMemoryRequest then
         Handler.Key_Name ("supportsWriteMemoryRequest");
         Handler.Boolean_Value (Value.supportsWriteMemoryRequest);
      end if;
      if Value.supportsDisassembleRequest then
         Handler.Key_Name ("supportsDisassembleRequest");
         Handler.Boolean_Value (Value.supportsDisassembleRequest);
      end if;
      if Value.supportsCancelRequest then
         Handler.Key_Name ("supportsCancelRequest");
         Handler.Boolean_Value (Value.supportsCancelRequest);
      end if;
      if Value.supportsBreakpointLocationsRequest then
         Handler.Key_Name ("supportsBreakpointLocationsRequest");
         Handler.Boolean_Value (Value.supportsBreakpointLocationsRequest);
      end if;
      if Value.supportsClipboardContext then
         Handler.Key_Name ("supportsClipboardContext");
         Handler.Boolean_Value (Value.supportsClipboardContext);
      end if;
      if Value.supportsSteppingGranularity then
         Handler.Key_Name ("supportsSteppingGranularity");
         Handler.Boolean_Value (Value.supportsSteppingGranularity);
      end if;
      if Value.supportsInstructionBreakpoints then
         Handler.Key_Name ("supportsInstructionBreakpoints");
         Handler.Boolean_Value (Value.supportsInstructionBreakpoints);
      end if;
      if Value.supportsExceptionFilterOptions then
         Handler.Key_Name ("supportsExceptionFilterOptions");
         Handler.Boolean_Value (Value.supportsExceptionFilterOptions);
      end if;
      if Value.supportsSingleThreadExecutionRequests then
         Handler.Key_Name ("supportsSingleThreadExecutionRequests");
         Handler.Boolean_Value (Value.supportsSingleThreadExecutionRequests);
      end if;
      Handler.End_Object;
   end Output_Capabilities;

   procedure Output_StackTraceResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StackTraceResponse) is
      procedure Output_StackTraceResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : StackTraceResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("stackFrames");
         Handler.Start_Array;
         for J in 1 .. Value.stackFrames.Length loop
            Output_StackFrame (Handler, Value.stackFrames (J));
         end loop;
         Handler.End_Array;
         if Value.totalFrames.Is_Set then
            Handler.Key_Name ("totalFrames");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.totalFrames.Value)));
         end if;
         Handler.End_Object;
      end Output_StackTraceResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_StackTraceResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_StackTraceResponse;

   procedure Output_LoadedSourcesArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LoadedSourcesArguments) is
   begin
      Output_Any_Value (Handler, Value);
   end Output_LoadedSourcesArguments;

   procedure Output_ConfigurationDoneRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ConfigurationDoneRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("configurationDone");
      if Value.arguments.Is_Set then
         Handler.Key_Name ("arguments");
         Output_ConfigurationDoneArguments (Handler, Value.arguments.Value);
      end if;
      Handler.End_Object;
   end Output_ConfigurationDoneRequest;

   procedure Output_StepInTargetsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepInTargetsResponse) is
      procedure Output_StepInTargetsResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : StepInTargetsResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("targets");
         Handler.Start_Array;
         for J in 1 .. Value.targets.Length loop
            Output_StepInTarget (Handler, Value.targets (J));
         end loop;
         Handler.End_Array;
         Handler.End_Object;
      end Output_StepInTargetsResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_StepInTargetsResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_StepInTargetsResponse;

   procedure Output_StackFrame
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StackFrame) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("id");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.id)));
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      if Value.source.Is_Set then
         Handler.Key_Name ("source");
         Output_Source (Handler, Value.source.Value);
      end if;
      Handler.Key_Name ("line");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.line)));
      Handler.Key_Name ("column");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.column)));
      if Value.endLine.Is_Set then
         Handler.Key_Name ("endLine");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endLine.Value)));
      end if;
      if Value.endColumn.Is_Set then
         Handler.Key_Name ("endColumn");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endColumn.Value)));
      end if;
      if Value.canRestart then
         Handler.Key_Name ("canRestart");
         Handler.Boolean_Value (Value.canRestart);
      end if;
      if not Value.instructionPointerReference.Is_Null then
         Handler.Key_Name ("instructionPointerReference");
         Handler.String_Value (Value.instructionPointerReference);
      end if;
      if Value.moduleId.Is_Set then
         Handler.Key_Name ("moduleId");
         if Value.moduleId.Value.Is_String then
            Handler.String_Value (Value.moduleId.Value.String);
         else
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.moduleId.Value.Integer)));
         end if;
      end if;
      if Value.presentationHint.Is_Set then
         Handler.Key_Name ("presentationHint");
         Output_StackFrame_presentationHint
           (Handler, Value.presentationHint.Value);
      end if;
      Handler.End_Object;
   end Output_StackFrame;

   procedure Output_SetExpressionRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetExpressionRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("setExpression");
      Handler.Key_Name ("arguments");
      Output_SetExpressionArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_SetExpressionRequest;

   procedure Output_SourceArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SourceArguments) is
   begin
      Handler.Start_Object;
      if Value.source.Is_Set then
         Handler.Key_Name ("source");
         Output_Source (Handler, Value.source.Value);
      end if;
      Handler.Key_Name ("sourceReference");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.sourceReference)));
      Handler.End_Object;
   end Output_SourceArguments;

   procedure Output_ExceptionFilterOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionFilterOptions) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("filterId");
      Handler.String_Value (Value.filterId);
      if not Value.condition.Is_Null then
         Handler.Key_Name ("condition");
         Handler.String_Value (Value.condition);
      end if;
      Handler.End_Object;
   end Output_ExceptionFilterOptions;

   procedure Output_ExceptionBreakpointsFilter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionBreakpointsFilter) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("filter");
      Handler.String_Value (Value.filter);
      Handler.Key_Name ("label");
      Handler.String_Value (Value.label);
      if not Value.description.Is_Null then
         Handler.Key_Name ("description");
         Handler.String_Value (Value.description);
      end if;
      if Value.default then
         Handler.Key_Name ("default");
         Handler.Boolean_Value (Value.default);
      end if;
      if Value.supportsCondition then
         Handler.Key_Name ("supportsCondition");
         Handler.Boolean_Value (Value.supportsCondition);
      end if;
      if not Value.conditionDescription.Is_Null then
         Handler.Key_Name ("conditionDescription");
         Handler.String_Value (Value.conditionDescription);
      end if;
      Handler.End_Object;
   end Output_ExceptionBreakpointsFilter;

   procedure Output_SetVariableRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetVariableRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("setVariable");
      Handler.Key_Name ("arguments");
      Output_SetVariableArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_SetVariableRequest;

   procedure Output_AttachRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : AttachRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("attach");
      Handler.Key_Name ("arguments");
      Output_AttachRequestArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_AttachRequest;

   procedure Output_MemoryEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : MemoryEvent) is
      procedure Output_MemoryEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : MemoryEvent_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("memoryReference");
         Handler.String_Value (Value.memoryReference);
         Handler.Key_Name ("offset");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.offset)));
         Handler.Key_Name ("count");
         Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.count)));
         Handler.End_Object;
      end Output_MemoryEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("memory");
      Handler.Key_Name ("body");
      Output_MemoryEvent_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_MemoryEvent;

   procedure Output_ReadMemoryArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ReadMemoryArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("memoryReference");
      Handler.String_Value (Value.memoryReference);
      if Value.offset.Is_Set then
         Handler.Key_Name ("offset");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.offset.Value)));
      end if;
      Handler.Key_Name ("count");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.count)));
      Handler.End_Object;
   end Output_ReadMemoryArguments;

   procedure Output_LoadedSourcesRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LoadedSourcesRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("loadedSources");
      if Value.arguments.Is_Set then
         Handler.Key_Name ("arguments");
         Output_LoadedSourcesArguments (Handler, Value.arguments.Value);
      end if;
      Handler.End_Object;
   end Output_LoadedSourcesRequest;

   procedure Output_LaunchRequestArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LaunchRequestArguments) is
   begin
      Handler.Start_Object;
      if Value.noDebug then
         Handler.Key_Name ("noDebug");
         Handler.Boolean_Value (Value.noDebug);
      end if;
      if not Value.restart.Is_Empty then
         Handler.Key_Name ("__restart");
         Output_Any_Value (Handler, Value.restart);
      end if;
      if not Value.program.Is_Null then
         Handler.Key_Name ("program");
         Handler.String_Value (Value.program);
      end if;
      Handler.End_Object;
   end Output_LaunchRequestArguments;

   procedure Output_ExitedEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExitedEvent) is
      procedure Output_ExitedEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : ExitedEvent_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("exitCode");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.exitCode)));
         Handler.End_Object;
      end Output_ExitedEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("exited");
      Handler.Key_Name ("body");
      Output_ExitedEvent_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_ExitedEvent;

   procedure Output_SetBreakpointsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetBreakpointsRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("setBreakpoints");
      Handler.Key_Name ("arguments");
      Output_SetBreakpointsArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_SetBreakpointsRequest;

   procedure Output_TerminateResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TerminateResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_TerminateResponse;

   procedure Output_Variable
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Variable) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      Handler.Key_Name ("value");
      Handler.String_Value (Value.value);
      if not Value.a_type.Is_Null then
         Handler.Key_Name ("type");
         Handler.String_Value (Value.a_type);
      end if;
      if Value.presentationHint.Is_Set then
         Handler.Key_Name ("presentationHint");
         Output_VariablePresentationHint
           (Handler, Value.presentationHint.Value);
      end if;
      if not Value.evaluateName.Is_Null then
         Handler.Key_Name ("evaluateName");
         Handler.String_Value (Value.evaluateName);
      end if;
      Handler.Key_Name ("variablesReference");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.variablesReference)));
      if Value.namedVariables.Is_Set then
         Handler.Key_Name ("namedVariables");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.namedVariables.Value)));
      end if;
      if Value.indexedVariables.Is_Set then
         Handler.Key_Name ("indexedVariables");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.indexedVariables.Value)));
      end if;
      if not Value.memoryReference.Is_Null then
         Handler.Key_Name ("memoryReference");
         Handler.String_Value (Value.memoryReference);
      end if;
      Handler.End_Object;
   end Output_Variable;

   procedure Output_StoppedEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StoppedEvent) is
      procedure Output_StoppedEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : StoppedEvent_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("reason");
         Output_StoppedEvent_reason (Handler, Value.reason);
         if not Value.description.Is_Null then
            Handler.Key_Name ("description");
            Handler.String_Value (Value.description);
         end if;
         if Value.threadId.Is_Set then
            Handler.Key_Name ("threadId");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.threadId.Value)));
         end if;
         if Value.preserveFocusHint then
            Handler.Key_Name ("preserveFocusHint");
            Handler.Boolean_Value (Value.preserveFocusHint);
         end if;
         if not Value.text.Is_Null then
            Handler.Key_Name ("text");
            Handler.String_Value (Value.text);
         end if;
         if Value.allThreadsStopped then
            Handler.Key_Name ("allThreadsStopped");
            Handler.Boolean_Value (Value.allThreadsStopped);
         end if;
         if Value.hitBreakpointIds.Length > 0 then
            Handler.Key_Name ("hitBreakpointIds");
            Handler.Start_Array;
            for J in 1 .. Value.hitBreakpointIds.Length loop
               Handler.Integer_Value
                 (Interfaces.Integer_64
                    (Integer'(Value.hitBreakpointIds (J))));
            end loop;
            Handler.End_Array;
         end if;
         Handler.End_Object;
      end Output_StoppedEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("stopped");
      Handler.Key_Name ("body");
      Output_StoppedEvent_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_StoppedEvent;

   procedure Output_RestartFrameRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RestartFrameRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("restartFrame");
      Handler.Key_Name ("arguments");
      Output_RestartFrameArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_RestartFrameRequest;

   procedure Output_AttachRequestArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : AttachRequestArguments) is
   begin
      Handler.Start_Object;
      if not Value.restart.Is_Empty then
         Handler.Key_Name ("__restart");
         Output_Any_Value (Handler, Value.restart);
      end if;
      Handler.End_Object;
   end Output_AttachRequestArguments;

   procedure Output_ScopesResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ScopesResponse) is
      procedure Output_ScopesResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : ScopesResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("scopes");
         Handler.Start_Array;
         for J in 1 .. Value.scopes.Length loop
            Output_Scope (Handler, Value.scopes (J));
         end loop;
         Handler.End_Array;
         Handler.End_Object;
      end Output_ScopesResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_ScopesResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_ScopesResponse;

   procedure Output_StepOutArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepOutArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("threadId");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.threadId)));
      if Value.singleThread then
         Handler.Key_Name ("singleThread");
         Handler.Boolean_Value (Value.singleThread);
      end if;
      if Value.granularity.Is_Set then
         Handler.Key_Name ("granularity");
         Output_SteppingGranularity (Handler, Value.granularity.Value);
      end if;
      Handler.End_Object;
   end Output_StepOutArguments;

   procedure Output_CompletionsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CompletionsRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("completions");
      Handler.Key_Name ("arguments");
      Output_CompletionsArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_CompletionsRequest;

   procedure Output_StartDebuggingRequestArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StartDebuggingRequestArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("configuration");
      Output_Any_Value (Handler, Value.configuration);
      Handler.Key_Name ("request");
      Output_StartDebuggingRequestArguments_request (Handler, Value.request);
      Handler.End_Object;
   end Output_StartDebuggingRequestArguments;

   procedure Output_ProgressUpdateEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ProgressUpdateEvent) is
      procedure Output_ProgressUpdateEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : ProgressUpdateEvent_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("progressId");
         Handler.String_Value (Value.progressId);
         if not Value.message.Is_Null then
            Handler.Key_Name ("message");
            Handler.String_Value (Value.message);
         end if;
         if Value.percentage.Is_Set then
            Handler.Key_Name ("percentage");
            Handler.Float_Value
              (Interfaces.IEEE_Float_64 (Value.percentage.Value));
         end if;
         Handler.End_Object;
      end Output_ProgressUpdateEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("progressUpdate");
      Handler.Key_Name ("body");
      Output_ProgressUpdateEvent_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_ProgressUpdateEvent;

   procedure Output_ExceptionInfoResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionInfoResponse) is
      procedure Output_ExceptionInfoResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : ExceptionInfoResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("exceptionId");
         Handler.String_Value (Value.exceptionId);
         if not Value.description.Is_Null then
            Handler.Key_Name ("description");
            Handler.String_Value (Value.description);
         end if;
         Handler.Key_Name ("breakMode");
         Output_ExceptionBreakMode (Handler, Value.breakMode);
         if Value.details.Is_Set then
            Handler.Key_Name ("details");
            Output_ExceptionDetails (Handler, Value.details.Value);
         end if;
         Handler.End_Object;
      end Output_ExceptionInfoResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_ExceptionInfoResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_ExceptionInfoResponse;

   procedure Output_InitializedEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : InitializedEvent) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("initialized");
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_InitializedEvent;

   procedure Output_SetExpressionResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetExpressionResponse) is
      procedure Output_SetExpressionResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : SetExpressionResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("value");
         Handler.String_Value (Value.value);
         if not Value.a_type.Is_Null then
            Handler.Key_Name ("type");
            Handler.String_Value (Value.a_type);
         end if;
         if Value.presentationHint.Is_Set then
            Handler.Key_Name ("presentationHint");
            Output_VariablePresentationHint
              (Handler, Value.presentationHint.Value);
         end if;
         if Value.variablesReference.Is_Set then
            Handler.Key_Name ("variablesReference");
            Handler.Integer_Value
              (Interfaces.Integer_64
                 (Integer'(Value.variablesReference.Value)));
         end if;
         if Value.namedVariables.Is_Set then
            Handler.Key_Name ("namedVariables");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.namedVariables.Value)));
         end if;
         if Value.indexedVariables.Is_Set then
            Handler.Key_Name ("indexedVariables");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.indexedVariables.Value)));
         end if;
         Handler.End_Object;
      end Output_SetExpressionResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_SetExpressionResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_SetExpressionResponse;

   procedure Output_StepInTarget
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepInTarget) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("id");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.id)));
      Handler.Key_Name ("label");
      Handler.String_Value (Value.label);
      if Value.line.Is_Set then
         Handler.Key_Name ("line");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.line.Value)));
      end if;
      if Value.column.Is_Set then
         Handler.Key_Name ("column");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.column.Value)));
      end if;
      if Value.endLine.Is_Set then
         Handler.Key_Name ("endLine");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endLine.Value)));
      end if;
      if Value.endColumn.Is_Set then
         Handler.Key_Name ("endColumn");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endColumn.Value)));
      end if;
      Handler.End_Object;
   end Output_StepInTarget;

   procedure Output_ReverseContinueResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ReverseContinueResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_ReverseContinueResponse;

   procedure Output_OutputEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : OutputEvent) is
      procedure Output_OutputEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : OutputEvent_body) is
      begin
         Handler.Start_Object;
         if Value.category.Is_Set then
            Handler.Key_Name ("category");
            Output_OutputEvent_category (Handler, Value.category.Value);
         end if;
         Handler.Key_Name ("output");
         Handler.String_Value (Value.output);
         if Value.group.Is_Set then
            Handler.Key_Name ("group");
            Output_OutputEvent_group (Handler, Value.group.Value);
         end if;
         if Value.variablesReference.Is_Set then
            Handler.Key_Name ("variablesReference");
            Handler.Integer_Value
              (Interfaces.Integer_64
                 (Integer'(Value.variablesReference.Value)));
         end if;
         if Value.source.Is_Set then
            Handler.Key_Name ("source");
            Output_Source (Handler, Value.source.Value);
         end if;
         if Value.line.Is_Set then
            Handler.Key_Name ("line");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.line.Value)));
         end if;
         if Value.column.Is_Set then
            Handler.Key_Name ("column");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.column.Value)));
         end if;
         if not Value.data.Is_Empty then
            Handler.Key_Name ("data");
            Output_Any_Value (Handler, Value.data);
         end if;
         Handler.End_Object;
      end Output_OutputEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("output");
      Handler.Key_Name ("body");
      Output_OutputEvent_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_OutputEvent;

   procedure Output_RestartRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RestartRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("restart");
      if Value.arguments.Is_Set then
         Handler.Key_Name ("arguments");
         Output_RestartArguments (Handler, Value.arguments.Value);
      end if;
      Handler.End_Object;
   end Output_RestartRequest;

   procedure Output_StackTraceArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StackTraceArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("threadId");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.threadId)));
      if Value.startFrame.Is_Set then
         Handler.Key_Name ("startFrame");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.startFrame.Value)));
      end if;
      if Value.levels.Is_Set then
         Handler.Key_Name ("levels");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.levels.Value)));
      end if;
      if Value.format.Is_Set then
         Handler.Key_Name ("format");
         Output_StackFrameFormat (Handler, Value.format.Value);
      end if;
      Handler.End_Object;
   end Output_StackTraceArguments;

   procedure Output_Thread
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Thread) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("id");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.id)));
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      Handler.End_Object;
   end Output_Thread;

   procedure Output_SetDataBreakpointsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetDataBreakpointsRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("setDataBreakpoints");
      Handler.Key_Name ("arguments");
      Output_SetDataBreakpointsArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_SetDataBreakpointsRequest;

   procedure Output_SourceRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SourceRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("source");
      Handler.Key_Name ("arguments");
      Output_SourceArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_SourceRequest;

   procedure Output_PauseResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : PauseResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_PauseResponse;

   procedure Output_SetFunctionBreakpointsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetFunctionBreakpointsRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("setFunctionBreakpoints");
      Handler.Key_Name ("arguments");
      Output_SetFunctionBreakpointsArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_SetFunctionBreakpointsRequest;

   procedure Output_ProcessEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ProcessEvent) is
      procedure Output_ProcessEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : ProcessEvent_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("name");
         Handler.String_Value (Value.name);
         if Value.systemProcessId.Is_Set then
            Handler.Key_Name ("systemProcessId");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.systemProcessId.Value)));
         end if;
         if Value.isLocalProcess then
            Handler.Key_Name ("isLocalProcess");
            Handler.Boolean_Value (Value.isLocalProcess);
         end if;
         if Value.startMethod.Is_Set then
            Handler.Key_Name ("startMethod");
            Output_ProcessEvent_startMethod (Handler, Value.startMethod.Value);
         end if;
         if Value.pointerSize.Is_Set then
            Handler.Key_Name ("pointerSize");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.pointerSize.Value)));
         end if;
         Handler.End_Object;
      end Output_ProcessEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("process");
      Handler.Key_Name ("body");
      Output_ProcessEvent_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_ProcessEvent;

   procedure Output_NextResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : NextResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_NextResponse;

   procedure Output_AttachResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : AttachResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_AttachResponse;

   procedure Output_RestartResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RestartResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_RestartResponse;

   procedure Output_CapabilitiesEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CapabilitiesEvent) is
      procedure Output_CapabilitiesEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : CapabilitiesEvent_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("capabilities");
         Output_Capabilities (Handler, Value.capabilities);
         Handler.End_Object;
      end Output_CapabilitiesEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("capabilities");
      Handler.Key_Name ("body");
      Output_CapabilitiesEvent_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_CapabilitiesEvent;

   procedure Output_Scope
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Scope) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      if Value.presentationHint.Is_Set then
         Handler.Key_Name ("presentationHint");
         Output_Scope_presentationHint (Handler, Value.presentationHint.Value);
      end if;
      Handler.Key_Name ("variablesReference");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.variablesReference)));
      if Value.namedVariables.Is_Set then
         Handler.Key_Name ("namedVariables");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.namedVariables.Value)));
      end if;
      if Value.indexedVariables.Is_Set then
         Handler.Key_Name ("indexedVariables");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.indexedVariables.Value)));
      end if;
      Handler.Key_Name ("expensive");
      Handler.Boolean_Value (Value.expensive);
      if Value.source.Is_Set then
         Handler.Key_Name ("source");
         Output_Source (Handler, Value.source.Value);
      end if;
      if Value.line.Is_Set then
         Handler.Key_Name ("line");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.line.Value)));
      end if;
      if Value.column.Is_Set then
         Handler.Key_Name ("column");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.column.Value)));
      end if;
      if Value.endLine.Is_Set then
         Handler.Key_Name ("endLine");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endLine.Value)));
      end if;
      if Value.endColumn.Is_Set then
         Handler.Key_Name ("endColumn");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endColumn.Value)));
      end if;
      Handler.End_Object;
   end Output_Scope;

   procedure Output_DisassembleArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DisassembleArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("memoryReference");
      Handler.String_Value (Value.memoryReference);
      if Value.offset.Is_Set then
         Handler.Key_Name ("offset");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.offset.Value)));
      end if;
      if Value.instructionOffset.Is_Set then
         Handler.Key_Name ("instructionOffset");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.instructionOffset.Value)));
      end if;
      Handler.Key_Name ("instructionCount");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.instructionCount)));
      if Value.resolveSymbols then
         Handler.Key_Name ("resolveSymbols");
         Handler.Boolean_Value (Value.resolveSymbols);
      end if;
      Handler.End_Object;
   end Output_DisassembleArguments;

   procedure Output_SetInstructionBreakpointsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetInstructionBreakpointsRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("setInstructionBreakpoints");
      Handler.Key_Name ("arguments");
      Output_SetInstructionBreakpointsArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_SetInstructionBreakpointsRequest;

   procedure Output_Response
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Response) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_Response;

   procedure Output_DataBreakpointInfoResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DataBreakpointInfoResponse) is
      procedure Output_DataBreakpointInfoResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : DataBreakpointInfoResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("dataId");
         Handler.String_Value (Value.dataId);
         Handler.Key_Name ("description");
         Handler.String_Value (Value.description);
         if Value.accessTypes.Length > 0 then
            Handler.Key_Name ("accessTypes");
            Handler.Start_Array;
            for J in 1 .. Value.accessTypes.Length loop
               Output_DataBreakpointAccessType
                 (Handler, Value.accessTypes (J));
            end loop;
            Handler.End_Array;
         end if;
         if Value.canPersist then
            Handler.Key_Name ("canPersist");
            Handler.Boolean_Value (Value.canPersist);
         end if;
         Handler.End_Object;
      end Output_DataBreakpointInfoResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_DataBreakpointInfoResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_DataBreakpointInfoResponse;

   procedure Output_SourceBreakpoint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SourceBreakpoint) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("line");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.line)));
      if Value.column.Is_Set then
         Handler.Key_Name ("column");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.column.Value)));
      end if;
      if not Value.condition.Is_Null then
         Handler.Key_Name ("condition");
         Handler.String_Value (Value.condition);
      end if;
      if not Value.hitCondition.Is_Null then
         Handler.Key_Name ("hitCondition");
         Handler.String_Value (Value.hitCondition);
      end if;
      if not Value.logMessage.Is_Null then
         Handler.Key_Name ("logMessage");
         Handler.String_Value (Value.logMessage);
      end if;
      Handler.End_Object;
   end Output_SourceBreakpoint;

   procedure Output_PauseRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : PauseRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("pause");
      Handler.Key_Name ("arguments");
      Output_PauseArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_PauseRequest;

   procedure Output_FunctionBreakpoint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : FunctionBreakpoint) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      if not Value.condition.Is_Null then
         Handler.Key_Name ("condition");
         Handler.String_Value (Value.condition);
      end if;
      if not Value.hitCondition.Is_Null then
         Handler.Key_Name ("hitCondition");
         Handler.String_Value (Value.hitCondition);
      end if;
      Handler.End_Object;
   end Output_FunctionBreakpoint;

   procedure Output_SetExpressionArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetExpressionArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("expression");
      Handler.String_Value (Value.expression);
      Handler.Key_Name ("value");
      Handler.String_Value (Value.value);
      if Value.frameId.Is_Set then
         Handler.Key_Name ("frameId");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.frameId.Value)));
      end if;
      if Value.format.Is_Set then
         Handler.Key_Name ("format");
         Output_ValueFormat (Handler, Value.format.Value);
      end if;
      Handler.End_Object;
   end Output_SetExpressionArguments;

   procedure Output_SetExceptionBreakpointsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetExceptionBreakpointsArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("filters");
      Handler.Start_Array;
      for J in 1 .. Value.filters.Length loop
         Handler.String_Value (Value.filters (J));
      end loop;
      Handler.End_Array;
      if Value.filterOptions.Length > 0 then
         Handler.Key_Name ("filterOptions");
         Handler.Start_Array;
         for J in 1 .. Value.filterOptions.Length loop
            Output_ExceptionFilterOptions (Handler, Value.filterOptions (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.exceptionOptions.Length > 0 then
         Handler.Key_Name ("exceptionOptions");
         Handler.Start_Array;
         for J in 1 .. Value.exceptionOptions.Length loop
            Output_ExceptionOptions (Handler, Value.exceptionOptions (J));
         end loop;
         Handler.End_Array;
      end if;
      Handler.End_Object;
   end Output_SetExceptionBreakpointsArguments;

   procedure Output_ValueFormat
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ValueFormat) is
   begin
      Handler.Start_Object;
      if Value.hex then
         Handler.Key_Name ("hex");
         Handler.Boolean_Value (Value.hex);
      end if;
      Handler.End_Object;
   end Output_ValueFormat;

   procedure Output_RunInTerminalRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RunInTerminalRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("runInTerminal");
      Handler.Key_Name ("arguments");
      Output_RunInTerminalRequestArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_RunInTerminalRequest;

   procedure Output_CompletionsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CompletionsArguments) is
   begin
      Handler.Start_Object;
      if Value.frameId.Is_Set then
         Handler.Key_Name ("frameId");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.frameId.Value)));
      end if;
      Handler.Key_Name ("text");
      Handler.String_Value (Value.text);
      Handler.Key_Name ("column");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.column)));
      if Value.line.Is_Set then
         Handler.Key_Name ("line");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.line.Value)));
      end if;
      Handler.End_Object;
   end Output_CompletionsArguments;

   procedure Output_WriteMemoryResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : WriteMemoryResponse) is
      procedure Output_WriteMemoryResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : WriteMemoryResponse_body) is
      begin
         Handler.Start_Object;
         if Value.offset.Is_Set then
            Handler.Key_Name ("offset");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.offset.Value)));
         end if;
         if Value.bytesWritten.Is_Set then
            Handler.Key_Name ("bytesWritten");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.bytesWritten.Value)));
         end if;
         Handler.End_Object;
      end Output_WriteMemoryResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if Value.a_body.Is_Set then
         Handler.Key_Name ("body");
         Output_WriteMemoryResponse_body (Handler, Value.a_body.Value);
      end if;
      Handler.End_Object;
   end Output_WriteMemoryResponse;

   procedure Output_ReverseContinueArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ReverseContinueArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("threadId");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.threadId)));
      if Value.singleThread then
         Handler.Key_Name ("singleThread");
         Handler.Boolean_Value (Value.singleThread);
      end if;
      Handler.End_Object;
   end Output_ReverseContinueArguments;

   procedure Output_RunInTerminalResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RunInTerminalResponse) is
      procedure Output_RunInTerminalResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : RunInTerminalResponse_body) is
      begin
         Handler.Start_Object;
         if Value.processId.Is_Set then
            Handler.Key_Name ("processId");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.processId.Value)));
         end if;
         if Value.shellProcessId.Is_Set then
            Handler.Key_Name ("shellProcessId");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.shellProcessId.Value)));
         end if;
         Handler.End_Object;
      end Output_RunInTerminalResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_RunInTerminalResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_RunInTerminalResponse;

   procedure Output_DisconnectArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DisconnectArguments) is
   begin
      Handler.Start_Object;
      if Value.restart then
         Handler.Key_Name ("restart");
         Handler.Boolean_Value (Value.restart);
      end if;
      if Value.terminateDebuggee then
         Handler.Key_Name ("terminateDebuggee");
         Handler.Boolean_Value (Value.terminateDebuggee);
      end if;
      if Value.suspendDebuggee then
         Handler.Key_Name ("suspendDebuggee");
         Handler.Boolean_Value (Value.suspendDebuggee);
      end if;
      Handler.End_Object;
   end Output_DisconnectArguments;

   procedure Output_Module
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Module) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("id");
      if Value.id.Is_String then
         Handler.String_Value (Value.id.String);
      else
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.id.Integer)));
      end if;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      if not Value.path.Is_Null then
         Handler.Key_Name ("path");
         Handler.String_Value (Value.path);
      end if;
      if Value.isOptimized then
         Handler.Key_Name ("isOptimized");
         Handler.Boolean_Value (Value.isOptimized);
      end if;
      if Value.isUserCode then
         Handler.Key_Name ("isUserCode");
         Handler.Boolean_Value (Value.isUserCode);
      end if;
      if not Value.version.Is_Null then
         Handler.Key_Name ("version");
         Handler.String_Value (Value.version);
      end if;
      if not Value.symbolStatus.Is_Null then
         Handler.Key_Name ("symbolStatus");
         Handler.String_Value (Value.symbolStatus);
      end if;
      if not Value.symbolFilePath.Is_Null then
         Handler.Key_Name ("symbolFilePath");
         Handler.String_Value (Value.symbolFilePath);
      end if;
      if not Value.dateTimeStamp.Is_Null then
         Handler.Key_Name ("dateTimeStamp");
         Handler.String_Value (Value.dateTimeStamp);
      end if;
      if not Value.addressRange.Is_Null then
         Handler.Key_Name ("addressRange");
         Handler.String_Value (Value.addressRange);
      end if;
      Handler.End_Object;
   end Output_Module;

   procedure Output_GotoTargetsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : GotoTargetsRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("gotoTargets");
      Handler.Key_Name ("arguments");
      Output_GotoTargetsArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_GotoTargetsRequest;

   procedure Output_ThreadsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ThreadsResponse) is
      procedure Output_ThreadsResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : ThreadsResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("threads");
         Handler.Start_Array;
         for J in 1 .. Value.threads.Length loop
            Output_Thread (Handler, Value.threads (J));
         end loop;
         Handler.End_Array;
         Handler.End_Object;
      end Output_ThreadsResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_ThreadsResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_ThreadsResponse;

   procedure Output_SetDataBreakpointsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetDataBreakpointsResponse) is
      procedure Output_SetDataBreakpointsResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : SetDataBreakpointsResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("breakpoints");
         Handler.Start_Array;
         for J in 1 .. Value.breakpoints.Length loop
            Output_Breakpoint (Handler, Value.breakpoints (J));
         end loop;
         Handler.End_Array;
         Handler.End_Object;
      end Output_SetDataBreakpointsResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_SetDataBreakpointsResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_SetDataBreakpointsResponse;

   procedure Output_DataBreakpoint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DataBreakpoint) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("dataId");
      Handler.String_Value (Value.dataId);
      if Value.accessType.Is_Set then
         Handler.Key_Name ("accessType");
         Output_DataBreakpointAccessType (Handler, Value.accessType.Value);
      end if;
      if not Value.condition.Is_Null then
         Handler.Key_Name ("condition");
         Handler.String_Value (Value.condition);
      end if;
      if not Value.hitCondition.Is_Null then
         Handler.Key_Name ("hitCondition");
         Handler.String_Value (Value.hitCondition);
      end if;
      Handler.End_Object;
   end Output_DataBreakpoint;

   procedure Output_SetDataBreakpointsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetDataBreakpointsArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("breakpoints");
      Handler.Start_Array;
      for J in 1 .. Value.breakpoints.Length loop
         Output_DataBreakpoint (Handler, Value.breakpoints (J));
      end loop;
      Handler.End_Array;
      Handler.End_Object;
   end Output_SetDataBreakpointsArguments;

   procedure Output_ExceptionPathSegment
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionPathSegment) is
   begin
      Handler.Start_Object;
      if Value.negate then
         Handler.Key_Name ("negate");
         Handler.Boolean_Value (Value.negate);
      end if;
      Handler.Key_Name ("names");
      Handler.Start_Array;
      for J in 1 .. Value.names.Length loop
         Handler.String_Value (Value.names (J));
      end loop;
      Handler.End_Array;
      Handler.End_Object;
   end Output_ExceptionPathSegment;

   procedure Output_Message
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Message) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("id");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.id)));
      Handler.Key_Name ("format");
      Handler.String_Value (Value.format);
      if not Value.variables.Is_Empty then
         Handler.Key_Name ("variables");
         Output_Any_Value (Handler, Value.variables);
      end if;
      if Value.sendTelemetry then
         Handler.Key_Name ("sendTelemetry");
         Handler.Boolean_Value (Value.sendTelemetry);
      end if;
      if Value.showUser then
         Handler.Key_Name ("showUser");
         Handler.Boolean_Value (Value.showUser);
      end if;
      if not Value.url.Is_Null then
         Handler.Key_Name ("url");
         Handler.String_Value (Value.url);
      end if;
      if not Value.urlLabel.Is_Null then
         Handler.Key_Name ("urlLabel");
         Handler.String_Value (Value.urlLabel);
      end if;
      Handler.End_Object;
   end Output_Message;

   procedure Output_SourceResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SourceResponse) is
      procedure Output_SourceResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : SourceResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("content");
         Handler.String_Value (Value.content);
         if not Value.mimeType.Is_Null then
            Handler.Key_Name ("mimeType");
            Handler.String_Value (Value.mimeType);
         end if;
         Handler.End_Object;
      end Output_SourceResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_SourceResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_SourceResponse;

   procedure Output_ContinueResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ContinueResponse) is
      procedure Output_ContinueResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : ContinueResponse_body) is
      begin
         Handler.Start_Object;
         if Value.allThreadsContinued then
            Handler.Key_Name ("allThreadsContinued");
            Handler.Boolean_Value (Value.allThreadsContinued);
         end if;
         Handler.End_Object;
      end Output_ContinueResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_ContinueResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_ContinueResponse;

   procedure Output_RestartFrameResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RestartFrameResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_RestartFrameResponse;

   procedure Output_StepInArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepInArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("threadId");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.threadId)));
      if Value.singleThread then
         Handler.Key_Name ("singleThread");
         Handler.Boolean_Value (Value.singleThread);
      end if;
      if Value.targetId.Is_Set then
         Handler.Key_Name ("targetId");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.targetId.Value)));
      end if;
      if Value.granularity.Is_Set then
         Handler.Key_Name ("granularity");
         Output_SteppingGranularity (Handler, Value.granularity.Value);
      end if;
      Handler.End_Object;
   end Output_StepInArguments;

   procedure Output_LaunchResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LaunchResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_LaunchResponse;

   procedure Output_StepInResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepInResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_StepInResponse;

   procedure Output_TerminateArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TerminateArguments) is
   begin
      Handler.Start_Object;
      if Value.restart then
         Handler.Key_Name ("restart");
         Handler.Boolean_Value (Value.restart);
      end if;
      Handler.End_Object;
   end Output_TerminateArguments;

   procedure Output_LaunchRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LaunchRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("launch");
      Handler.Key_Name ("arguments");
      Output_LaunchRequestArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_LaunchRequest;

   procedure Output_StepOutResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepOutResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_StepOutResponse;

   procedure Output_EvaluateRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : EvaluateRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("evaluate");
      Handler.Key_Name ("arguments");
      Output_EvaluateArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_EvaluateRequest;

   procedure Output_ContinueArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ContinueArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("threadId");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.threadId)));
      if Value.singleThread then
         Handler.Key_Name ("singleThread");
         Handler.Boolean_Value (Value.singleThread);
      end if;
      Handler.End_Object;
   end Output_ContinueArguments;

   procedure Output_StepBackArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepBackArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("threadId");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.threadId)));
      if Value.singleThread then
         Handler.Key_Name ("singleThread");
         Handler.Boolean_Value (Value.singleThread);
      end if;
      if Value.granularity.Is_Set then
         Handler.Key_Name ("granularity");
         Output_SteppingGranularity (Handler, Value.granularity.Value);
      end if;
      Handler.End_Object;
   end Output_StepBackArguments;

   procedure Output_BreakpointLocationsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : BreakpointLocationsArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("source");
      Output_Source (Handler, Value.source);
      Handler.Key_Name ("line");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.line)));
      if Value.column.Is_Set then
         Handler.Key_Name ("column");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.column.Value)));
      end if;
      if Value.endLine.Is_Set then
         Handler.Key_Name ("endLine");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endLine.Value)));
      end if;
      if Value.endColumn.Is_Set then
         Handler.Key_Name ("endColumn");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endColumn.Value)));
      end if;
      Handler.End_Object;
   end Output_BreakpointLocationsArguments;

   procedure Output_CancelArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CancelArguments) is
   begin
      Handler.Start_Object;
      if Value.requestId.Is_Set then
         Handler.Key_Name ("requestId");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.requestId.Value)));
      end if;
      if not Value.progressId.Is_Null then
         Handler.Key_Name ("progressId");
         Handler.String_Value (Value.progressId);
      end if;
      Handler.End_Object;
   end Output_CancelArguments;

   procedure Output_CompletionsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CompletionsResponse) is
      procedure Output_CompletionsResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : CompletionsResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("targets");
         Handler.Start_Array;
         for J in 1 .. Value.targets.Length loop
            Output_CompletionItem (Handler, Value.targets (J));
         end loop;
         Handler.End_Array;
         Handler.End_Object;
      end Output_CompletionsResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_CompletionsResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_CompletionsResponse;

   procedure Output_Breakpoint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Breakpoint) is
   begin
      Handler.Start_Object;
      if Value.id.Is_Set then
         Handler.Key_Name ("id");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.id.Value)));
      end if;
      Handler.Key_Name ("verified");
      Handler.Boolean_Value (Value.verified);
      if not Value.message.Is_Null then
         Handler.Key_Name ("message");
         Handler.String_Value (Value.message);
      end if;
      if Value.source.Is_Set then
         Handler.Key_Name ("source");
         Output_Source (Handler, Value.source.Value);
      end if;
      if Value.line.Is_Set then
         Handler.Key_Name ("line");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.line.Value)));
      end if;
      if Value.column.Is_Set then
         Handler.Key_Name ("column");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.column.Value)));
      end if;
      if Value.endLine.Is_Set then
         Handler.Key_Name ("endLine");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endLine.Value)));
      end if;
      if Value.endColumn.Is_Set then
         Handler.Key_Name ("endColumn");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endColumn.Value)));
      end if;
      if not Value.instructionReference.Is_Null then
         Handler.Key_Name ("instructionReference");
         Handler.String_Value (Value.instructionReference);
      end if;
      if Value.offset.Is_Set then
         Handler.Key_Name ("offset");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.offset.Value)));
      end if;
      Handler.End_Object;
   end Output_Breakpoint;

   procedure Output_Source
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Source) is
   begin
      Handler.Start_Object;
      if not Value.name.Is_Null then
         Handler.Key_Name ("name");
         Handler.String_Value (Value.name);
      end if;
      if not Value.path.Is_Null then
         Handler.Key_Name ("path");
         Handler.String_Value (Value.path);
      end if;
      if Value.sourceReference.Is_Set then
         Handler.Key_Name ("sourceReference");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.sourceReference.Value)));
      end if;
      if Value.presentationHint.Is_Set then
         Handler.Key_Name ("presentationHint");
         Output_Source_presentationHint
           (Handler, Value.presentationHint.Value);
      end if;
      if not Value.origin.Is_Null then
         Handler.Key_Name ("origin");
         Handler.String_Value (Value.origin);
      end if;
      if Value.sources.Length > 0 then
         Handler.Key_Name ("sources");
         Handler.Start_Array;
         for J in 1 .. Value.sources.Length loop
            Output_Source (Handler, Value.sources (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.adapterData.Is_Empty then
         Handler.Key_Name ("adapterData");
         Output_Any_Value (Handler, Value.adapterData);
      end if;
      if Value.checksums.Length > 0 then
         Handler.Key_Name ("checksums");
         Handler.Start_Array;
         for J in 1 .. Value.checksums.Length loop
            Output_Checksum (Handler, Value.checksums (J));
         end loop;
         Handler.End_Array;
      end if;
      Handler.End_Object;
   end Output_Source;

   procedure Output_WriteMemoryArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : WriteMemoryArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("memoryReference");
      Handler.String_Value (Value.memoryReference);
      if Value.offset.Is_Set then
         Handler.Key_Name ("offset");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.offset.Value)));
      end if;
      if Value.allowPartial then
         Handler.Key_Name ("allowPartial");
         Handler.Boolean_Value (Value.allowPartial);
      end if;
      Handler.Key_Name ("data");
      Handler.String_Value (Value.data);
      Handler.End_Object;
   end Output_WriteMemoryArguments;

   procedure Output_ConfigurationDoneArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ConfigurationDoneArguments) is
   begin
      Output_Any_Value (Handler, Value);
   end Output_ConfigurationDoneArguments;

   procedure Output_StepOutRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepOutRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("stepOut");
      Handler.Key_Name ("arguments");
      Output_StepOutArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_StepOutRequest;

   procedure Output_Request
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Request) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if not Value.arguments.Is_Empty then
         Handler.Key_Name ("arguments");
         Output_Any_Value (Handler, Value.arguments);
      end if;
      Handler.End_Object;
   end Output_Request;

   procedure Output_LoadedSourceEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LoadedSourceEvent) is
      procedure Output_LoadedSourceEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LoadedSourceEvent_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("reason");
         Output_LoadedSourceEvent_reason (Handler, Value.reason);
         Handler.Key_Name ("source");
         Output_Source (Handler, Value.source);
         Handler.End_Object;
      end Output_LoadedSourceEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("loadedSource");
      Handler.Key_Name ("body");
      Output_LoadedSourceEvent_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_LoadedSourceEvent;

   procedure Output_StackFrameFormat
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StackFrameFormat) is
   begin
      Handler.Start_Object;
      if Value.hex then
         Handler.Key_Name ("hex");
         Handler.Boolean_Value (Value.hex);
      end if;
      if Value.parameters then
         Handler.Key_Name ("parameters");
         Handler.Boolean_Value (Value.parameters);
      end if;
      if Value.parameterTypes then
         Handler.Key_Name ("parameterTypes");
         Handler.Boolean_Value (Value.parameterTypes);
      end if;
      if Value.parameterNames then
         Handler.Key_Name ("parameterNames");
         Handler.Boolean_Value (Value.parameterNames);
      end if;
      if Value.parameterValues then
         Handler.Key_Name ("parameterValues");
         Handler.Boolean_Value (Value.parameterValues);
      end if;
      if Value.line then
         Handler.Key_Name ("line");
         Handler.Boolean_Value (Value.line);
      end if;
      if Value.module then
         Handler.Key_Name ("module");
         Handler.Boolean_Value (Value.module);
      end if;
      if Value.includeAll then
         Handler.Key_Name ("includeAll");
         Handler.Boolean_Value (Value.includeAll);
      end if;
      Handler.End_Object;
   end Output_StackFrameFormat;

   procedure Output_DisassembleRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DisassembleRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("disassemble");
      Handler.Key_Name ("arguments");
      Output_DisassembleArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_DisassembleRequest;

   procedure Output_ReadMemoryResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ReadMemoryResponse) is
      procedure Output_ReadMemoryResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : ReadMemoryResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("address");
         Handler.String_Value (Value.address);
         if Value.unreadableBytes.Is_Set then
            Handler.Key_Name ("unreadableBytes");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.unreadableBytes.Value)));
         end if;
         if not Value.data.Is_Null then
            Handler.Key_Name ("data");
            Handler.String_Value (Value.data);
         end if;
         Handler.End_Object;
      end Output_ReadMemoryResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if Value.a_body.Is_Set then
         Handler.Key_Name ("body");
         Output_ReadMemoryResponse_body (Handler, Value.a_body.Value);
      end if;
      Handler.End_Object;
   end Output_ReadMemoryResponse;

   procedure Output_StepBackRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepBackRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("stepBack");
      Handler.Key_Name ("arguments");
      Output_StepBackArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_StepBackRequest;

   procedure Output_ProtocolMessage
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ProtocolMessage) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Output_ProtocolMessage_type (Handler, Value.a_type);
      Handler.End_Object;
   end Output_ProtocolMessage;

   procedure Output_ThreadsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ThreadsRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("threads");
      if not Value.arguments.Is_Empty then
         Handler.Key_Name ("arguments");
         Output_Any_Value (Handler, Value.arguments);
      end if;
      Handler.End_Object;
   end Output_ThreadsRequest;

   procedure Output_VariablesResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : VariablesResponse) is
      procedure Output_VariablesResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : VariablesResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("variables");
         Handler.Start_Array;
         for J in 1 .. Value.variables.Length loop
            Output_Variable (Handler, Value.variables (J));
         end loop;
         Handler.End_Array;
         Handler.End_Object;
      end Output_VariablesResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_VariablesResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_VariablesResponse;

   procedure Output_RunInTerminalRequestArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RunInTerminalRequestArguments) is
   begin
      Handler.Start_Object;
      if Value.kind.Is_Set then
         Handler.Key_Name ("kind");
         Output_RunInTerminalRequestArguments_kind (Handler, Value.kind.Value);
      end if;
      if not Value.title.Is_Null then
         Handler.Key_Name ("title");
         Handler.String_Value (Value.title);
      end if;
      Handler.Key_Name ("cwd");
      Handler.String_Value (Value.cwd);
      Handler.Key_Name ("args");
      Handler.Start_Array;
      for J in 1 .. Value.args.Length loop
         Handler.String_Value (Value.args (J));
      end loop;
      Handler.End_Array;
      if not Value.env.Is_Empty then
         Handler.Key_Name ("env");
         Output_Any_Value (Handler, Value.env);
      end if;
      if Value.argsCanBeInterpretedByShell then
         Handler.Key_Name ("argsCanBeInterpretedByShell");
         Handler.Boolean_Value (Value.argsCanBeInterpretedByShell);
      end if;
      Handler.End_Object;
   end Output_RunInTerminalRequestArguments;

   procedure Output_TerminateRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TerminateRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("terminate");
      if Value.arguments.Is_Set then
         Handler.Key_Name ("arguments");
         Output_TerminateArguments (Handler, Value.arguments.Value);
      end if;
      Handler.End_Object;
   end Output_TerminateRequest;

   procedure Output_VariablesArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : VariablesArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("variablesReference");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.variablesReference)));
      if Value.filter.Is_Set then
         Handler.Key_Name ("filter");
         Output_VariablesArguments_filter (Handler, Value.filter.Value);
      end if;
      if Value.start.Is_Set then
         Handler.Key_Name ("start");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.start.Value)));
      end if;
      if Value.count.Is_Set then
         Handler.Key_Name ("count");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.count.Value)));
      end if;
      if Value.format.Is_Set then
         Handler.Key_Name ("format");
         Output_ValueFormat (Handler, Value.format.Value);
      end if;
      Handler.End_Object;
   end Output_VariablesArguments;

   procedure Output_InitializeRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : InitializeRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("initialize");
      Handler.Key_Name ("arguments");
      Output_InitializeRequestArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_InitializeRequest;

   procedure Output_BreakpointLocationsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : BreakpointLocationsResponse) is
      procedure Output_BreakpointLocationsResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : BreakpointLocationsResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("breakpoints");
         Handler.Start_Array;
         for J in 1 .. Value.breakpoints.Length loop
            Output_BreakpointLocation (Handler, Value.breakpoints (J));
         end loop;
         Handler.End_Array;
         Handler.End_Object;
      end Output_BreakpointLocationsResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_BreakpointLocationsResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_BreakpointLocationsResponse;

   procedure Output_VariablePresentationHint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : VariablePresentationHint) is
   begin
      Handler.Start_Object;
      if Value.kind.Is_Set then
         Handler.Key_Name ("kind");
         Output_VariablePresentationHint_kind (Handler, Value.kind.Value);
      end if;
      if not Value.attributes.Is_Empty then
         Handler.Key_Name ("attributes");
         Handler.Start_Array;
         for J in 1 .. Value.attributes.Length loop
            Handler.String_Value (Value.attributes (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.visibility.Is_Set then
         Handler.Key_Name ("visibility");
         Output_VariablePresentationHint_visibility
           (Handler, Value.visibility.Value);
      end if;
      if Value.lazy then
         Handler.Key_Name ("lazy");
         Handler.Boolean_Value (Value.lazy);
      end if;
      Handler.End_Object;
   end Output_VariablePresentationHint;

   procedure Output_DisassembledInstruction
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DisassembledInstruction) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("address");
      Handler.String_Value (Value.address);
      if not Value.instructionBytes.Is_Null then
         Handler.Key_Name ("instructionBytes");
         Handler.String_Value (Value.instructionBytes);
      end if;
      Handler.Key_Name ("instruction");
      Handler.String_Value (Value.instruction);
      if not Value.symbol.Is_Null then
         Handler.Key_Name ("symbol");
         Handler.String_Value (Value.symbol);
      end if;
      if Value.location.Is_Set then
         Handler.Key_Name ("location");
         Output_Source (Handler, Value.location.Value);
      end if;
      if Value.line.Is_Set then
         Handler.Key_Name ("line");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.line.Value)));
      end if;
      if Value.column.Is_Set then
         Handler.Key_Name ("column");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.column.Value)));
      end if;
      if Value.endLine.Is_Set then
         Handler.Key_Name ("endLine");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endLine.Value)));
      end if;
      if Value.endColumn.Is_Set then
         Handler.Key_Name ("endColumn");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endColumn.Value)));
      end if;
      Handler.End_Object;
   end Output_DisassembledInstruction;

   procedure Output_PauseArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : PauseArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("threadId");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.threadId)));
      Handler.End_Object;
   end Output_PauseArguments;

   procedure Output_CancelResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CancelResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_CancelResponse;

   procedure Output_InitializeRequestArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : InitializeRequestArguments) is
   begin
      Handler.Start_Object;
      if not Value.clientID.Is_Null then
         Handler.Key_Name ("clientID");
         Handler.String_Value (Value.clientID);
      end if;
      if not Value.clientName.Is_Null then
         Handler.Key_Name ("clientName");
         Handler.String_Value (Value.clientName);
      end if;
      Handler.Key_Name ("adapterID");
      Handler.String_Value (Value.adapterID);
      if not Value.locale.Is_Null then
         Handler.Key_Name ("locale");
         Handler.String_Value (Value.locale);
      end if;
      if Value.linesStartAt1 then
         Handler.Key_Name ("linesStartAt1");
         Handler.Boolean_Value (Value.linesStartAt1);
      end if;
      if Value.columnsStartAt1 then
         Handler.Key_Name ("columnsStartAt1");
         Handler.Boolean_Value (Value.columnsStartAt1);
      end if;
      if Value.pathFormat.Is_Set then
         Handler.Key_Name ("pathFormat");
         Output_InitializeRequestArguments_pathFormat
           (Handler, Value.pathFormat.Value);
      end if;
      if Value.supportsVariableType then
         Handler.Key_Name ("supportsVariableType");
         Handler.Boolean_Value (Value.supportsVariableType);
      end if;
      if Value.supportsVariablePaging then
         Handler.Key_Name ("supportsVariablePaging");
         Handler.Boolean_Value (Value.supportsVariablePaging);
      end if;
      if Value.supportsRunInTerminalRequest then
         Handler.Key_Name ("supportsRunInTerminalRequest");
         Handler.Boolean_Value (Value.supportsRunInTerminalRequest);
      end if;
      if Value.supportsMemoryReferences then
         Handler.Key_Name ("supportsMemoryReferences");
         Handler.Boolean_Value (Value.supportsMemoryReferences);
      end if;
      if Value.supportsProgressReporting then
         Handler.Key_Name ("supportsProgressReporting");
         Handler.Boolean_Value (Value.supportsProgressReporting);
      end if;
      if Value.supportsInvalidatedEvent then
         Handler.Key_Name ("supportsInvalidatedEvent");
         Handler.Boolean_Value (Value.supportsInvalidatedEvent);
      end if;
      if Value.supportsMemoryEvent then
         Handler.Key_Name ("supportsMemoryEvent");
         Handler.Boolean_Value (Value.supportsMemoryEvent);
      end if;
      if Value.supportsArgsCanBeInterpretedByShell then
         Handler.Key_Name ("supportsArgsCanBeInterpretedByShell");
         Handler.Boolean_Value (Value.supportsArgsCanBeInterpretedByShell);
      end if;
      if Value.supportsStartDebuggingRequest then
         Handler.Key_Name ("supportsStartDebuggingRequest");
         Handler.Boolean_Value (Value.supportsStartDebuggingRequest);
      end if;
      Handler.End_Object;
   end Output_InitializeRequestArguments;

   procedure Output_SetInstructionBreakpointsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetInstructionBreakpointsResponse) is
      procedure Output_SetInstructionBreakpointsResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : SetInstructionBreakpointsResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("breakpoints");
         Handler.Start_Array;
         for J in 1 .. Value.breakpoints.Length loop
            Output_Breakpoint (Handler, Value.breakpoints (J));
         end loop;
         Handler.End_Array;
         Handler.End_Object;
      end Output_SetInstructionBreakpointsResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_SetInstructionBreakpointsResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_SetInstructionBreakpointsResponse;

   procedure Output_CancelRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CancelRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("cancel");
      if Value.arguments.Is_Set then
         Handler.Key_Name ("arguments");
         Output_CancelArguments (Handler, Value.arguments.Value);
      end if;
      Handler.End_Object;
   end Output_CancelRequest;

   procedure Output_ProgressEndEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ProgressEndEvent) is
      procedure Output_ProgressEndEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : ProgressEndEvent_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("progressId");
         Handler.String_Value (Value.progressId);
         if not Value.message.Is_Null then
            Handler.Key_Name ("message");
            Handler.String_Value (Value.message);
         end if;
         Handler.End_Object;
      end Output_ProgressEndEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("progressEnd");
      Handler.Key_Name ("body");
      Output_ProgressEndEvent_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_ProgressEndEvent;

   procedure Output_Event
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Event) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value (Value.event);
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_Event;

   procedure Output_VariablesRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : VariablesRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("variables");
      Handler.Key_Name ("arguments");
      Output_VariablesArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_VariablesRequest;

   procedure Output_ExceptionOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionOptions) is
   begin
      Handler.Start_Object;
      if Value.path.Length > 0 then
         Handler.Key_Name ("path");
         Handler.Start_Array;
         for J in 1 .. Value.path.Length loop
            Output_ExceptionPathSegment (Handler, Value.path (J));
         end loop;
         Handler.End_Array;
      end if;
      Handler.Key_Name ("breakMode");
      Output_ExceptionBreakMode (Handler, Value.breakMode);
      Handler.End_Object;
   end Output_ExceptionOptions;

   procedure Output_TerminatedEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TerminatedEvent) is
      procedure Output_TerminatedEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : TerminatedEvent_body) is
      begin
         Handler.Start_Object;
         if not Value.restart.Is_Empty then
            Handler.Key_Name ("restart");
            Output_Any_Value (Handler, Value.restart);
         end if;
         Handler.End_Object;
      end Output_TerminatedEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("terminated");
      if Value.a_body.Is_Set then
         Handler.Key_Name ("body");
         Output_TerminatedEvent_body (Handler, Value.a_body.Value);
      end if;
      Handler.End_Object;
   end Output_TerminatedEvent;

   procedure Output_StartDebuggingRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StartDebuggingRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("startDebugging");
      Handler.Key_Name ("arguments");
      Output_StartDebuggingRequestArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_StartDebuggingRequest;

   procedure Output_ThreadEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ThreadEvent) is
      procedure Output_ThreadEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : ThreadEvent_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("reason");
         Output_ThreadEvent_reason (Handler, Value.reason);
         Handler.Key_Name ("threadId");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.threadId)));
         Handler.End_Object;
      end Output_ThreadEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("thread");
      Handler.Key_Name ("body");
      Output_ThreadEvent_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_ThreadEvent;

   procedure Output_GotoTargetsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : GotoTargetsResponse) is
      procedure Output_GotoTargetsResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : GotoTargetsResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("targets");
         Handler.Start_Array;
         for J in 1 .. Value.targets.Length loop
            Output_GotoTarget (Handler, Value.targets (J));
         end loop;
         Handler.End_Array;
         Handler.End_Object;
      end Output_GotoTargetsResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_GotoTargetsResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_GotoTargetsResponse;

   procedure Output_CompletionItem
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : CompletionItem) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("label");
      Handler.String_Value (Value.label);
      if not Value.text.Is_Null then
         Handler.Key_Name ("text");
         Handler.String_Value (Value.text);
      end if;
      if not Value.sortText.Is_Null then
         Handler.Key_Name ("sortText");
         Handler.String_Value (Value.sortText);
      end if;
      if not Value.detail.Is_Null then
         Handler.Key_Name ("detail");
         Handler.String_Value (Value.detail);
      end if;
      if Value.a_type.Is_Set then
         Handler.Key_Name ("type");
         Output_CompletionItemType (Handler, Value.a_type.Value);
      end if;
      if Value.start.Is_Set then
         Handler.Key_Name ("start");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.start.Value)));
      end if;
      if Value.length.Is_Set then
         Handler.Key_Name ("length");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.length.Value)));
      end if;
      if Value.selectionStart.Is_Set then
         Handler.Key_Name ("selectionStart");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.selectionStart.Value)));
      end if;
      if Value.selectionLength.Is_Set then
         Handler.Key_Name ("selectionLength");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.selectionLength.Value)));
      end if;
      Handler.End_Object;
   end Output_CompletionItem;

   procedure Output_ScopesArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ScopesArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("frameId");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.frameId)));
      Handler.End_Object;
   end Output_ScopesArguments;

   procedure Output_ErrorResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ErrorResponse) is
      procedure Output_ErrorResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : ErrorResponse_body) is
      begin
         Handler.Start_Object;
         if Value.error.Is_Set then
            Handler.Key_Name ("error");
            Output_Message (Handler, Value.error.Value);
         end if;
         Handler.End_Object;
      end Output_ErrorResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_ErrorResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_ErrorResponse;

   procedure Output_SetInstructionBreakpointsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetInstructionBreakpointsArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("breakpoints");
      Handler.Start_Array;
      for J in 1 .. Value.breakpoints.Length loop
         Output_InstructionBreakpoint (Handler, Value.breakpoints (J));
      end loop;
      Handler.End_Array;
      Handler.End_Object;
   end Output_SetInstructionBreakpointsArguments;

   procedure Output_GotoArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : GotoArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("threadId");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.threadId)));
      Handler.Key_Name ("targetId");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.targetId)));
      Handler.End_Object;
   end Output_GotoArguments;

   procedure Output_BreakpointEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : BreakpointEvent) is
      procedure Output_BreakpointEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : BreakpointEvent_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("reason");
         Output_BreakpointEvent_reason (Handler, Value.reason);
         Handler.Key_Name ("breakpoint");
         Output_Breakpoint (Handler, Value.breakpoint);
         Handler.End_Object;
      end Output_BreakpointEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("breakpoint");
      Handler.Key_Name ("body");
      Output_BreakpointEvent_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_BreakpointEvent;

   procedure Output_GotoTarget
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : GotoTarget) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("id");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.id)));
      Handler.Key_Name ("label");
      Handler.String_Value (Value.label);
      Handler.Key_Name ("line");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.line)));
      if Value.column.Is_Set then
         Handler.Key_Name ("column");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.column.Value)));
      end if;
      if Value.endLine.Is_Set then
         Handler.Key_Name ("endLine");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endLine.Value)));
      end if;
      if Value.endColumn.Is_Set then
         Handler.Key_Name ("endColumn");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endColumn.Value)));
      end if;
      if not Value.instructionPointerReference.Is_Null then
         Handler.Key_Name ("instructionPointerReference");
         Handler.String_Value (Value.instructionPointerReference);
      end if;
      Handler.End_Object;
   end Output_GotoTarget;

   procedure Output_ReadMemoryRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ReadMemoryRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("readMemory");
      Handler.Key_Name ("arguments");
      Output_ReadMemoryArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_ReadMemoryRequest;

   procedure Output_ModulesArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ModulesArguments) is
   begin
      Handler.Start_Object;
      if Value.startModule.Is_Set then
         Handler.Key_Name ("startModule");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.startModule.Value)));
      end if;
      if Value.moduleCount.Is_Set then
         Handler.Key_Name ("moduleCount");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.moduleCount.Value)));
      end if;
      Handler.End_Object;
   end Output_ModulesArguments;

   procedure Output_NextRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : NextRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("next");
      Handler.Key_Name ("arguments");
      Output_NextArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_NextRequest;

   procedure Output_ProgressStartEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ProgressStartEvent) is
      procedure Output_ProgressStartEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : ProgressStartEvent_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("progressId");
         Handler.String_Value (Value.progressId);
         Handler.Key_Name ("title");
         Handler.String_Value (Value.title);
         if Value.requestId.Is_Set then
            Handler.Key_Name ("requestId");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.requestId.Value)));
         end if;
         if Value.cancellable then
            Handler.Key_Name ("cancellable");
            Handler.Boolean_Value (Value.cancellable);
         end if;
         if not Value.message.Is_Null then
            Handler.Key_Name ("message");
            Handler.String_Value (Value.message);
         end if;
         if Value.percentage.Is_Set then
            Handler.Key_Name ("percentage");
            Handler.Float_Value
              (Interfaces.IEEE_Float_64 (Value.percentage.Value));
         end if;
         Handler.End_Object;
      end Output_ProgressStartEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("progressStart");
      Handler.Key_Name ("body");
      Output_ProgressStartEvent_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_ProgressStartEvent;

   procedure Output_SetVariableResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetVariableResponse) is
      procedure Output_SetVariableResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : SetVariableResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("value");
         Handler.String_Value (Value.value);
         if not Value.a_type.Is_Null then
            Handler.Key_Name ("type");
            Handler.String_Value (Value.a_type);
         end if;
         if Value.variablesReference.Is_Set then
            Handler.Key_Name ("variablesReference");
            Handler.Integer_Value
              (Interfaces.Integer_64
                 (Integer'(Value.variablesReference.Value)));
         end if;
         if Value.namedVariables.Is_Set then
            Handler.Key_Name ("namedVariables");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.namedVariables.Value)));
         end if;
         if Value.indexedVariables.Is_Set then
            Handler.Key_Name ("indexedVariables");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.indexedVariables.Value)));
         end if;
         Handler.End_Object;
      end Output_SetVariableResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_SetVariableResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_SetVariableResponse;

   procedure Output_BreakpointLocation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : BreakpointLocation) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("line");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.line)));
      if Value.column.Is_Set then
         Handler.Key_Name ("column");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.column.Value)));
      end if;
      if Value.endLine.Is_Set then
         Handler.Key_Name ("endLine");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endLine.Value)));
      end if;
      if Value.endColumn.Is_Set then
         Handler.Key_Name ("endColumn");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endColumn.Value)));
      end if;
      Handler.End_Object;
   end Output_BreakpointLocation;

   procedure Output_RestartFrameArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : RestartFrameArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("frameId");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.frameId)));
      Handler.End_Object;
   end Output_RestartFrameArguments;

   procedure Output_DisconnectResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DisconnectResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_DisconnectResponse;

   procedure Output_SetExceptionBreakpointsRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetExceptionBreakpointsRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("setExceptionBreakpoints");
      Handler.Key_Name ("arguments");
      Output_SetExceptionBreakpointsArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_SetExceptionBreakpointsRequest;

   procedure Output_WriteMemoryRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : WriteMemoryRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("writeMemory");
      Handler.Key_Name ("arguments");
      Output_WriteMemoryArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_WriteMemoryRequest;

   procedure Output_DataBreakpointInfoArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DataBreakpointInfoArguments) is
   begin
      Handler.Start_Object;
      if Value.variablesReference.Is_Set then
         Handler.Key_Name ("variablesReference");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.variablesReference.Value)));
      end if;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      if Value.frameId.Is_Set then
         Handler.Key_Name ("frameId");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.frameId.Value)));
      end if;
      Handler.End_Object;
   end Output_DataBreakpointInfoArguments;

   procedure Output_InitializeResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : InitializeResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if Value.a_body.Is_Set then
         Handler.Key_Name ("body");
         Output_Capabilities (Handler, Value.a_body.Value);
      end if;
      Handler.End_Object;
   end Output_InitializeResponse;

   procedure Output_ConfigurationDoneResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ConfigurationDoneResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_ConfigurationDoneResponse;

   procedure Output_StepInTargetsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepInTargetsArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("frameId");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.frameId)));
      Handler.End_Object;
   end Output_StepInTargetsArguments;

   procedure Output_EvaluateArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : EvaluateArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("expression");
      Handler.String_Value (Value.expression);
      if Value.frameId.Is_Set then
         Handler.Key_Name ("frameId");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.frameId.Value)));
      end if;
      if Value.context.Is_Set then
         Handler.Key_Name ("context");
         Output_EvaluateArguments_context (Handler, Value.context.Value);
      end if;
      if Value.format.Is_Set then
         Handler.Key_Name ("format");
         Output_ValueFormat (Handler, Value.format.Value);
      end if;
      Handler.End_Object;
   end Output_EvaluateArguments;

   procedure Output_SetFunctionBreakpointsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetFunctionBreakpointsArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("breakpoints");
      Handler.Start_Array;
      for J in 1 .. Value.breakpoints.Length loop
         Output_FunctionBreakpoint (Handler, Value.breakpoints (J));
      end loop;
      Handler.End_Array;
      Handler.End_Object;
   end Output_SetFunctionBreakpointsArguments;

   procedure Output_GotoRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : GotoRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("goto");
      Handler.Key_Name ("arguments");
      Output_GotoArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_GotoRequest;

   procedure Output_SetFunctionBreakpointsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetFunctionBreakpointsResponse) is
      procedure Output_SetFunctionBreakpointsResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : SetFunctionBreakpointsResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("breakpoints");
         Handler.Start_Array;
         for J in 1 .. Value.breakpoints.Length loop
            Output_Breakpoint (Handler, Value.breakpoints (J));
         end loop;
         Handler.End_Array;
         Handler.End_Object;
      end Output_SetFunctionBreakpointsResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_SetFunctionBreakpointsResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_SetFunctionBreakpointsResponse;

   procedure Output_ExceptionInfoArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ExceptionInfoArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("threadId");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.threadId)));
      Handler.End_Object;
   end Output_ExceptionInfoArguments;

   procedure Output_StackTraceRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StackTraceRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("stackTrace");
      Handler.Key_Name ("arguments");
      Output_StackTraceArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_StackTraceRequest;

   procedure Output_EvaluateResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : EvaluateResponse) is
      procedure Output_EvaluateResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : EvaluateResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("result");
         Handler.String_Value (Value.result);
         if not Value.a_type.Is_Null then
            Handler.Key_Name ("type");
            Handler.String_Value (Value.a_type);
         end if;
         if Value.presentationHint.Is_Set then
            Handler.Key_Name ("presentationHint");
            Output_VariablePresentationHint
              (Handler, Value.presentationHint.Value);
         end if;
         Handler.Key_Name ("variablesReference");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.variablesReference)));
         if Value.namedVariables.Is_Set then
            Handler.Key_Name ("namedVariables");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.namedVariables.Value)));
         end if;
         if Value.indexedVariables.Is_Set then
            Handler.Key_Name ("indexedVariables");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.indexedVariables.Value)));
         end if;
         if not Value.memoryReference.Is_Null then
            Handler.Key_Name ("memoryReference");
            Handler.String_Value (Value.memoryReference);
         end if;
         Handler.End_Object;
      end Output_EvaluateResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_EvaluateResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_EvaluateResponse;

   procedure Output_ReverseContinueRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ReverseContinueRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("reverseContinue");
      Handler.Key_Name ("arguments");
      Output_ReverseContinueArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_ReverseContinueRequest;

   procedure Output_ModulesRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ModulesRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("modules");
      Handler.Key_Name ("arguments");
      Output_ModulesArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_ModulesRequest;

   procedure Output_SetBreakpointsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetBreakpointsResponse) is
      procedure Output_SetBreakpointsResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : SetBreakpointsResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("breakpoints");
         Handler.Start_Array;
         for J in 1 .. Value.breakpoints.Length loop
            Output_Breakpoint (Handler, Value.breakpoints (J));
         end loop;
         Handler.End_Array;
         Handler.End_Object;
      end Output_SetBreakpointsResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_SetBreakpointsResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_SetBreakpointsResponse;

   procedure Output_InstructionBreakpoint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : InstructionBreakpoint) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("instructionReference");
      Handler.String_Value (Value.instructionReference);
      if Value.offset.Is_Set then
         Handler.Key_Name ("offset");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.offset.Value)));
      end if;
      if not Value.condition.Is_Null then
         Handler.Key_Name ("condition");
         Handler.String_Value (Value.condition);
      end if;
      if not Value.hitCondition.Is_Null then
         Handler.Key_Name ("hitCondition");
         Handler.String_Value (Value.hitCondition);
      end if;
      Handler.End_Object;
   end Output_InstructionBreakpoint;

   procedure Output_DisconnectRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DisconnectRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("disconnect");
      if Value.arguments.Is_Set then
         Handler.Key_Name ("arguments");
         Output_DisconnectArguments (Handler, Value.arguments.Value);
      end if;
      Handler.End_Object;
   end Output_DisconnectRequest;

   procedure Output_TerminateThreadsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : TerminateThreadsResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_TerminateThreadsResponse;

   procedure Output_ScopesRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ScopesRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("scopes");
      Handler.Key_Name ("arguments");
      Output_ScopesArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_ScopesRequest;

   procedure Output_SetExceptionBreakpointsResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : SetExceptionBreakpointsResponse) is
      procedure Output_SetExceptionBreakpointsResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : SetExceptionBreakpointsResponse_body) is
      begin
         Handler.Start_Object;
         if Value.breakpoints.Length > 0 then
            Handler.Key_Name ("breakpoints");
            Handler.Start_Array;
            for J in 1 .. Value.breakpoints.Length loop
               Output_Breakpoint (Handler, Value.breakpoints (J));
            end loop;
            Handler.End_Array;
         end if;
         Handler.End_Object;
      end Output_SetExceptionBreakpointsResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if Value.a_body.Is_Set then
         Handler.Key_Name ("body");
         Output_SetExceptionBreakpointsResponse_body
           (Handler, Value.a_body.Value);
      end if;
      Handler.End_Object;
   end Output_SetExceptionBreakpointsResponse;

   procedure Output_StepBackResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepBackResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_StepBackResponse;

   procedure Output_DisassembleResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : DisassembleResponse) is
      procedure Output_DisassembleResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : DisassembleResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("instructions");
         Handler.Start_Array;
         for J in 1 .. Value.instructions.Length loop
            Output_DisassembledInstruction (Handler, Value.instructions (J));
         end loop;
         Handler.End_Array;
         Handler.End_Object;
      end Output_DisassembleResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if Value.a_body.Is_Set then
         Handler.Key_Name ("body");
         Output_DisassembleResponse_body (Handler, Value.a_body.Value);
      end if;
      Handler.End_Object;
   end Output_DisassembleResponse;

   procedure Output_InvalidatedEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : InvalidatedEvent) is
      procedure Output_InvalidatedEvent_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : InvalidatedEvent_body) is
      begin
         Handler.Start_Object;
         if Value.areas.Length > 0 then
            Handler.Key_Name ("areas");
            Handler.Start_Array;
            for J in 1 .. Value.areas.Length loop
               Output_InvalidatedAreas (Handler, Value.areas (J));
            end loop;
            Handler.End_Array;
         end if;
         if Value.threadId.Is_Set then
            Handler.Key_Name ("threadId");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.threadId.Value)));
         end if;
         if Value.stackFrameId.Is_Set then
            Handler.Key_Name ("stackFrameId");
            Handler.Integer_Value
              (Interfaces.Integer_64 (Integer'(Value.stackFrameId.Value)));
         end if;
         Handler.End_Object;
      end Output_InvalidatedEvent_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("event");
      Handler.Key_Name ("event");
      Handler.String_Value ("invalidated");
      Handler.Key_Name ("body");
      Output_InvalidatedEvent_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_InvalidatedEvent;

   procedure Output_GotoTargetsArguments
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : GotoTargetsArguments) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("source");
      Output_Source (Handler, Value.source);
      Handler.Key_Name ("line");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.line)));
      if Value.column.Is_Set then
         Handler.Key_Name ("column");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.column.Value)));
      end if;
      Handler.End_Object;
   end Output_GotoTargetsArguments;

   procedure Output_StepInRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StepInRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("stepIn");
      Handler.Key_Name ("arguments");
      Output_StepInArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_StepInRequest;

   procedure Output_ContinueRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : ContinueRequest) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("request");
      Handler.Key_Name ("command");
      Handler.String_Value ("continue");
      Handler.Key_Name ("arguments");
      Output_ContinueArguments (Handler, Value.arguments);
      Handler.End_Object;
   end Output_ContinueRequest;

   procedure Output_LoadedSourcesResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LoadedSourcesResponse) is
      procedure Output_LoadedSourcesResponse_body
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LoadedSourcesResponse_body) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("sources");
         Handler.Start_Array;
         for J in 1 .. Value.sources.Length loop
            Output_Source (Handler, Value.sources (J));
         end loop;
         Handler.End_Array;
         Handler.End_Object;
      end Output_LoadedSourcesResponse_body;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("body");
      Output_LoadedSourcesResponse_body (Handler, Value.a_body);
      Handler.End_Object;
   end Output_LoadedSourcesResponse;

   procedure Output_StartDebuggingResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : StartDebuggingResponse) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("seq");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.seq)));
      Handler.Key_Name ("type");
      Handler.String_Value ("response");
      Handler.Key_Name ("request_seq");
      Handler.Integer_Value
        (Interfaces.Integer_64 (Integer'(Value.request_seq)));
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_Response_message (Handler, Value.message.Value);
      end if;
      if not Value.a_body.Is_Empty then
         Handler.Key_Name ("body");
         Output_Any_Value (Handler, Value.a_body);
      end if;
      Handler.End_Object;
   end Output_StartDebuggingResponse;

end DAP.Tools.Outputs;
