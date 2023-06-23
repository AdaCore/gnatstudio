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

pragma Ada_2022;
with Minimal_Perfect_Hash;

package body DAP.Tools.Inputs is
   pragma Style_Checks (Off);
   use type VSS.JSON.JSON_Number_Kind;
   use type VSS.Strings.Virtual_String;

   procedure Input_Any_Value
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Any_Value'Class;
      Success : in out Boolean) is
      use type VSS.JSON.Streams.JSON_Stream_Element_Kind;
   begin
      case Reader.Element_Kind is
         when VSS.JSON.Streams.Start_Array =>
            Value.Append ((Kind => VSS.JSON.Streams.Start_Array));
            Reader.Read_Next;
            while Success and Reader.Element_Kind /= VSS.JSON.Streams.End_Array
            loop
               Input_Any_Value (Reader, Value, Success);
            end loop;
            Value.Append ((Kind => VSS.JSON.Streams.End_Array));
         when VSS.JSON.Streams.Start_Object =>
            Value.Append ((Kind => VSS.JSON.Streams.Start_Object));
            Reader.Read_Next;
            while Success and Reader.Element_Kind = VSS.JSON.Streams.Key_Name
            loop
               Value.Append (Reader.Element);
               Reader.Read_Next;
               Input_Any_Value (Reader, Value, Success);
            end loop;
            Value.Append ((Kind => VSS.JSON.Streams.End_Object));
         when VSS.JSON.Streams.String_Value | VSS.JSON.Streams.Number_Value
           | VSS.JSON.Streams.Boolean_Value | VSS.JSON.Streams.Null_Value =>
            Value.Append (Reader.Element);
         when others =>
            Success := False;
      end case;
      if Success then
         Reader.Read_Next;
      end if;
   end Input_Any_Value;

   package ModuleEvent_reason_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["new", "changed", "removed"]);

   procedure Input_ModuleEvent_reason
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.ModuleEvent_reason;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           ModuleEvent_reason_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.ModuleEvent_reason'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_ModuleEvent_reason;

   package ColumnDescriptor_type_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["string", "number", "boolean", "unixTimestampUTC"]);

   procedure Input_ColumnDescriptor_type
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.ColumnDescriptor_type;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           ColumnDescriptor_type_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.ColumnDescriptor_type'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_ColumnDescriptor_type;

   package StackFrame_presentationHint_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["normal", "label", "subtle"]);

   procedure Input_StackFrame_presentationHint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.StackFrame_presentationHint;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           StackFrame_presentationHint_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.StackFrame_presentationHint'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_StackFrame_presentationHint;

   package ExceptionBreakMode_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["never", "always", "unhandled", "userUnhandled"]);

   procedure Input_ExceptionBreakMode
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.ExceptionBreakMode;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           ExceptionBreakMode_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.ExceptionBreakMode'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_ExceptionBreakMode;

   package StoppedEvent_reason_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["step", "breakpoint", "exception", "pause", "entry", "goto",
       "function breakpoint", "data breakpoint", "instruction breakpoint"]);

   procedure Input_StoppedEvent_reason
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.StoppedEvent_reason;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           StoppedEvent_reason_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.StoppedEvent_reason'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_StoppedEvent_reason;

   package StartDebuggingRequestArguments_request_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["launch", "attach"]);

   procedure Input_StartDebuggingRequestArguments_request
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.StartDebuggingRequestArguments_request;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           StartDebuggingRequestArguments_request_Minimal_Perfect_Hash
             .Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.StartDebuggingRequestArguments_request'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_StartDebuggingRequestArguments_request;

   package OutputEvent_category_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["console", "important", "stdout", "stderr", "telemetry"]);

   procedure Input_OutputEvent_category
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.OutputEvent_category;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           OutputEvent_category_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.OutputEvent_category'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_OutputEvent_category;

   package OutputEvent_group_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["start", "startCollapsed", "end"]);

   procedure Input_OutputEvent_group
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.OutputEvent_group;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           OutputEvent_group_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.OutputEvent_group'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_OutputEvent_group;

   package ChecksumAlgorithm_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["MD5", "SHA1", "SHA256", "timestamp"]);

   procedure Input_ChecksumAlgorithm
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.ChecksumAlgorithm;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           ChecksumAlgorithm_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.ChecksumAlgorithm'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_ChecksumAlgorithm;

   package ProcessEvent_startMethod_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["launch", "attach", "attachForSuspendedLaunch"]);

   procedure Input_ProcessEvent_startMethod
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.ProcessEvent_startMethod;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           ProcessEvent_startMethod_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.ProcessEvent_startMethod'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_ProcessEvent_startMethod;

   package Scope_presentationHint_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["arguments", "locals", "registers"]);

   procedure Input_Scope_presentationHint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.Scope_presentationHint;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           Scope_presentationHint_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.Scope_presentationHint'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_Scope_presentationHint;

   package Response_message_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["cancelled", "notStopped"]);

   procedure Input_Response_message
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.Response_message;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           Response_message_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.Response_message'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_Response_message;

   package CompletionItemType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["method", "function", "constructor", "field", "variable", "class",
       "interface", "module", "property", "unit", "value", "enum", "keyword",
       "snippet", "text", "color", "file", "reference", "customcolor"]);

   procedure Input_CompletionItemType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.CompletionItemType;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           CompletionItemType_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.CompletionItemType'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_CompletionItemType;

   package InvalidatedAreas_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["all", "stacks", "threads", "variables"]);

   procedure Input_InvalidatedAreas
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.InvalidatedAreas;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           InvalidatedAreas_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.InvalidatedAreas'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_InvalidatedAreas;

   package Source_presentationHint_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["normal", "emphasize", "deemphasize"]);

   procedure Input_Source_presentationHint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.Source_presentationHint;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           Source_presentationHint_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.Source_presentationHint'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_Source_presentationHint;

   package LoadedSourceEvent_reason_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["new", "changed", "removed"]);

   procedure Input_LoadedSourceEvent_reason
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.LoadedSourceEvent_reason;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           LoadedSourceEvent_reason_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.LoadedSourceEvent_reason'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_LoadedSourceEvent_reason;

   package ProtocolMessage_type_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["request", "response", "event"]);

   procedure Input_ProtocolMessage_type
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.ProtocolMessage_type;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           ProtocolMessage_type_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.ProtocolMessage_type'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_ProtocolMessage_type;

   package RunInTerminalRequestArguments_kind_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["integrated", "external"]);

   procedure Input_RunInTerminalRequestArguments_kind
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.RunInTerminalRequestArguments_kind;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           RunInTerminalRequestArguments_kind_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.RunInTerminalRequestArguments_kind'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_RunInTerminalRequestArguments_kind;

   package VariablesArguments_filter_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["indexed", "named"]);

   procedure Input_VariablesArguments_filter
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.VariablesArguments_filter;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           VariablesArguments_filter_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.VariablesArguments_filter'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_VariablesArguments_filter;

   package VariablePresentationHint_kind_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["property", "method", "class", "data", "event", "baseClass",
       "innerClass", "interface", "mostDerivedClass", "virtual",
       "dataBreakpoint"]);

   procedure Input_VariablePresentationHint_kind
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.VariablePresentationHint_kind;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           VariablePresentationHint_kind_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.VariablePresentationHint_kind'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_VariablePresentationHint_kind;

   package VariablePresentationHint_visibility_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["public", "private", "protected", "internal", "final"]);

   procedure Input_VariablePresentationHint_visibility
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.VariablePresentationHint_visibility;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           VariablePresentationHint_visibility_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.VariablePresentationHint_visibility'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_VariablePresentationHint_visibility;

   package InitializeRequestArguments_pathFormat_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["path", "uri"]);

   procedure Input_InitializeRequestArguments_pathFormat
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.InitializeRequestArguments_pathFormat;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           InitializeRequestArguments_pathFormat_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.InitializeRequestArguments_pathFormat'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_InitializeRequestArguments_pathFormat;

   package ThreadEvent_reason_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["started", "exited"]);

   procedure Input_ThreadEvent_reason
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.ThreadEvent_reason;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           ThreadEvent_reason_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.ThreadEvent_reason'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_ThreadEvent_reason;

   package DataBreakpointAccessType_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["read", "write", "readWrite"]);

   procedure Input_DataBreakpointAccessType
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.DataBreakpointAccessType;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           DataBreakpointAccessType_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.DataBreakpointAccessType'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_DataBreakpointAccessType;

   package BreakpointEvent_reason_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["changed", "new", "removed"]);

   procedure Input_BreakpointEvent_reason
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.BreakpointEvent_reason;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           BreakpointEvent_reason_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.BreakpointEvent_reason'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_BreakpointEvent_reason;

   package EvaluateArguments_context_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["watch", "repl", "hover", "clipboard", "variables"]);

   procedure Input_EvaluateArguments_context
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.EvaluateArguments_context;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           EvaluateArguments_context_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.EvaluateArguments_context'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_EvaluateArguments_context;

   package SteppingGranularity_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["statement", "line", "instruction"]);

   procedure Input_SteppingGranularity
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.SteppingGranularity;
      Success : in out Boolean) is
      Index : constant Natural :=
        (if Reader.Is_String_Value then
           SteppingGranularity_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else 0);
   begin
      if Index > 0 then
         Value := Enum.SteppingGranularity'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_SteppingGranularity;

   package GotoResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_GotoResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out GotoResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 GotoResponse_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_GotoResponse;

   package ExceptionDetails_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["message", "typeName", "fullTypeName", "evaluateName", "stackTrace",
       "innerException"]);

   procedure Input_ExceptionDetails
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionDetails;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ExceptionDetails_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  message
                     if Reader.Is_String_Value then
                        Value.message := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  typeName
                     if Reader.Is_String_Value then
                        Value.typeName := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  fullTypeName
                     if Reader.Is_String_Value then
                        Value.fullTypeName := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  evaluateName
                     if Reader.Is_String_Value then
                        Value.evaluateName := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  stackTrace
                     if Reader.Is_String_Value then
                        Value.stackTrace := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  innerException
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : ExceptionDetails;
                           begin
                              Input_ExceptionDetails (Reader, Item, Success);
                              Value.innerException.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ExceptionDetails;

   package StepInTargetsRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_StepInTargetsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepInTargetsRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StepInTargetsRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "stepInTargets"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_StepInTargetsArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StepInTargetsRequest;

   package ModulesResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package ModulesResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["modules", "totalModules"]);

   procedure Input_ModulesResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ModulesResponse;
      Success : in out Boolean) is
      procedure Input_ModulesResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out ModulesResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    ModulesResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  modules
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : Module;
                              begin
                                 Input_Module (Reader, Item, Success);
                                 Value.modules.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  totalModules
                        Value.totalModules := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.totalModules.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_ModulesResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ModulesResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_ModulesResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ModulesResponse;

   package NextArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["threadId", "singleThread", "granularity"]);

   procedure Input_NextArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out NextArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 NextArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  threadId
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.threadId :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  singleThread
                     if Reader.Is_Boolean_Value then
                        Value.singleThread := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  granularity
                     Value.granularity := (Is_Set => True, Value => <>);
                     Input_SteppingGranularity
                       (Reader, Value.granularity.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_NextArguments;

   package ExceptionInfoRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_ExceptionInfoRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionInfoRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ExceptionInfoRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "exceptionInfo"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_ExceptionInfoArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ExceptionInfoRequest;

   package TerminateThreadsArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["threadIds"]);

   procedure Input_TerminateThreadsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TerminateThreadsArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 TerminateThreadsArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  threadIds
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : Integer;
                           begin
                              if Reader.Is_Number_Value
                                and then Reader.Number_Value.Kind =
                                  VSS.JSON.JSON_Integer
                              then
                                 Item :=
                                   Integer (Reader.Number_Value.Integer_Value);
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.threadIds.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_TerminateThreadsArguments;

   package DataBreakpointInfoRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_DataBreakpointInfoRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DataBreakpointInfoRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 DataBreakpointInfoRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "dataBreakpointInfo"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_DataBreakpointInfoArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_DataBreakpointInfoRequest;

   package TerminateThreadsRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_TerminateThreadsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TerminateThreadsRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 TerminateThreadsRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "terminateThreads"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_TerminateThreadsArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_TerminateThreadsRequest;

   package SetBreakpointsArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["source", "breakpoints", "lines", "sourceModified"]);

   procedure Input_SetBreakpointsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetBreakpointsArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetBreakpointsArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  source
                     Input_Source (Reader, Value.source, Success);
                  when 2 =>  --  breakpoints
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : SourceBreakpoint;
                           begin
                              Input_SourceBreakpoint (Reader, Item, Success);
                              Value.breakpoints.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  lines
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : Integer;
                           begin
                              if Reader.Is_Number_Value
                                and then Reader.Number_Value.Kind =
                                  VSS.JSON.JSON_Integer
                              then
                                 Item :=
                                   Integer (Reader.Number_Value.Integer_Value);
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.lines.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  sourceModified
                     if Reader.Is_Boolean_Value then
                        Value.sourceModified := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetBreakpointsArguments;

   package ModuleEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package ModuleEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["reason", "module"]);

   procedure Input_ModuleEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ModuleEvent;
      Success : in out Boolean) is
      procedure Input_ModuleEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out ModuleEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    ModuleEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  reason
                        Input_ModuleEvent_reason
                          (Reader, Value.reason, Success);
                     when 2 =>  --  module
                        Input_Module (Reader, Value.module, Success);
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_ModuleEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ModuleEvent_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "module"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_ModuleEvent_body (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ModuleEvent;

   package ContinuedEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package ContinuedEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["threadId", "allThreadsContinued"]);

   procedure Input_ContinuedEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ContinuedEvent;
      Success : in out Boolean) is
      procedure Input_ContinuedEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out ContinuedEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    ContinuedEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  threadId
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.threadId :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  allThreadsContinued
                        if Reader.Is_Boolean_Value then
                           Value.allThreadsContinued := Reader.Boolean_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_ContinuedEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ContinuedEvent_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "continued"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_ContinuedEvent_body (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ContinuedEvent;

   package RestartArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["arguments"]);

   procedure Input_RestartArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RestartArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 RestartArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  arguments
                     Value.arguments := (Is_Set => True, Value => <>);
                     Input_AttachRequestArguments
                       (Reader, Value.arguments.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_RestartArguments;

   package SetVariableArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["variablesReference", "name", "value", "format"]);

   procedure Input_SetVariableArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetVariableArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetVariableArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  variablesReference
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.variablesReference :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  name
                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  value
                     if Reader.Is_String_Value then
                        Value.value := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  format
                     Value.format := (Is_Set => True, Value => <>);
                     Input_ValueFormat (Reader, Value.format.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetVariableArguments;

   package Checksum_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["algorithm", "checksum"]);

   procedure Input_Checksum
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Checksum;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Checksum_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  algorithm
                     Input_ChecksumAlgorithm
                       (Reader, Value.algorithm, Success);
                  when 2 =>  --  checksum
                     if Reader.Is_String_Value then
                        Value.checksum := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Checksum;

   package BreakpointLocationsRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_BreakpointLocationsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out BreakpointLocationsRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 BreakpointLocationsRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "breakpointLocations"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Value.arguments := (Is_Set => True, Value => <>);
                     Input_BreakpointLocationsArguments
                       (Reader, Value.arguments.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_BreakpointLocationsRequest;

   package ModulesViewDescriptor_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["columns"]);

   procedure Input_ModulesViewDescriptor
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ModulesViewDescriptor;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ModulesViewDescriptor_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  columns
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : ColumnDescriptor;
                           begin
                              Input_ColumnDescriptor (Reader, Item, Success);
                              Value.columns.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ModulesViewDescriptor;

   package ColumnDescriptor_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["attributeName", "label", "format", "type", "width"]);

   procedure Input_ColumnDescriptor
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ColumnDescriptor;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ColumnDescriptor_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  attributeName
                     if Reader.Is_String_Value then
                        Value.attributeName := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  label
                     if Reader.Is_String_Value then
                        Value.label := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  format
                     if Reader.Is_String_Value then
                        Value.format := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  type
                     Value.a_type := (Is_Set => True, Value => <>);
                     Input_ColumnDescriptor_type
                       (Reader, Value.a_type.Value, Success);
                  when 5 =>  --  width
                     Value.width := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.width.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ColumnDescriptor;

   package Capabilities_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["supportsConfigurationDoneRequest", "supportsFunctionBreakpoints",
       "supportsConditionalBreakpoints", "supportsHitConditionalBreakpoints",
       "supportsEvaluateForHovers", "exceptionBreakpointFilters",
       "supportsStepBack", "supportsSetVariable", "supportsRestartFrame",
       "supportsGotoTargetsRequest", "supportsStepInTargetsRequest",
       "supportsCompletionsRequest", "completionTriggerCharacters",
       "supportsModulesRequest", "additionalModuleColumns",
       "supportedChecksumAlgorithms", "supportsRestartRequest",
       "supportsExceptionOptions", "supportsValueFormattingOptions",
       "supportsExceptionInfoRequest", "supportTerminateDebuggee",
       "supportSuspendDebuggee", "supportsDelayedStackTraceLoading",
       "supportsLoadedSourcesRequest", "supportsLogPoints",
       "supportsTerminateThreadsRequest", "supportsSetExpression",
       "supportsTerminateRequest", "supportsDataBreakpoints",
       "supportsReadMemoryRequest", "supportsWriteMemoryRequest",
       "supportsDisassembleRequest", "supportsCancelRequest",
       "supportsBreakpointLocationsRequest", "supportsClipboardContext",
       "supportsSteppingGranularity", "supportsInstructionBreakpoints",
       "supportsExceptionFilterOptions",
       "supportsSingleThreadExecutionRequests"]);

   procedure Input_Capabilities
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Capabilities;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Capabilities_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  supportsConfigurationDoneRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsConfigurationDoneRequest :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  supportsFunctionBreakpoints
                     if Reader.Is_Boolean_Value then
                        Value.supportsFunctionBreakpoints :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  supportsConditionalBreakpoints
                     if Reader.Is_Boolean_Value then
                        Value.supportsConditionalBreakpoints :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  supportsHitConditionalBreakpoints
                     if Reader.Is_Boolean_Value then
                        Value.supportsHitConditionalBreakpoints :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  supportsEvaluateForHovers
                     if Reader.Is_Boolean_Value then
                        Value.supportsEvaluateForHovers :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  exceptionBreakpointFilters
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : ExceptionBreakpointsFilter;
                           begin
                              Input_ExceptionBreakpointsFilter
                                (Reader, Item, Success);
                              Value.exceptionBreakpointFilters.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  supportsStepBack
                     if Reader.Is_Boolean_Value then
                        Value.supportsStepBack := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 8 =>  --  supportsSetVariable
                     if Reader.Is_Boolean_Value then
                        Value.supportsSetVariable := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  supportsRestartFrame
                     if Reader.Is_Boolean_Value then
                        Value.supportsRestartFrame := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 10 =>  --  supportsGotoTargetsRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsGotoTargetsRequest :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 11 =>  --  supportsStepInTargetsRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsStepInTargetsRequest :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 12 =>  --  supportsCompletionsRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsCompletionsRequest :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 13 =>  --  completionTriggerCharacters
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.completionTriggerCharacters.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 14 =>  --  supportsModulesRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsModulesRequest := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 15 =>  --  additionalModuleColumns
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : ColumnDescriptor;
                           begin
                              Input_ColumnDescriptor (Reader, Item, Success);
                              Value.additionalModuleColumns.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 16 =>  --  supportedChecksumAlgorithms
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : Enum.ChecksumAlgorithm;
                           begin
                              Input_ChecksumAlgorithm (Reader, Item, Success);
                              Value.supportedChecksumAlgorithms.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 17 =>  --  supportsRestartRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsRestartRequest := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 18 =>  --  supportsExceptionOptions
                     if Reader.Is_Boolean_Value then
                        Value.supportsExceptionOptions := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 19 =>  --  supportsValueFormattingOptions
                     if Reader.Is_Boolean_Value then
                        Value.supportsValueFormattingOptions :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 20 =>  --  supportsExceptionInfoRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsExceptionInfoRequest :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 21 =>  --  supportTerminateDebuggee
                     if Reader.Is_Boolean_Value then
                        Value.supportTerminateDebuggee := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 22 =>  --  supportSuspendDebuggee
                     if Reader.Is_Boolean_Value then
                        Value.supportSuspendDebuggee := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 23 =>  --  supportsDelayedStackTraceLoading
                     if Reader.Is_Boolean_Value then
                        Value.supportsDelayedStackTraceLoading :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 24 =>  --  supportsLoadedSourcesRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsLoadedSourcesRequest :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 25 =>  --  supportsLogPoints
                     if Reader.Is_Boolean_Value then
                        Value.supportsLogPoints := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 26 =>  --  supportsTerminateThreadsRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsTerminateThreadsRequest :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 27 =>  --  supportsSetExpression
                     if Reader.Is_Boolean_Value then
                        Value.supportsSetExpression := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 28 =>  --  supportsTerminateRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsTerminateRequest := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 29 =>  --  supportsDataBreakpoints
                     if Reader.Is_Boolean_Value then
                        Value.supportsDataBreakpoints := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 30 =>  --  supportsReadMemoryRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsReadMemoryRequest :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 31 =>  --  supportsWriteMemoryRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsWriteMemoryRequest :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 32 =>  --  supportsDisassembleRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsDisassembleRequest :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 33 =>  --  supportsCancelRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsCancelRequest := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 34 =>  --  supportsBreakpointLocationsRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsBreakpointLocationsRequest :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 35 =>  --  supportsClipboardContext
                     if Reader.Is_Boolean_Value then
                        Value.supportsClipboardContext := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 36 =>  --  supportsSteppingGranularity
                     if Reader.Is_Boolean_Value then
                        Value.supportsSteppingGranularity :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 37 =>  --  supportsInstructionBreakpoints
                     if Reader.Is_Boolean_Value then
                        Value.supportsInstructionBreakpoints :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 38 =>  --  supportsExceptionFilterOptions
                     if Reader.Is_Boolean_Value then
                        Value.supportsExceptionFilterOptions :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 39 =>  --  supportsSingleThreadExecutionRequests
                     if Reader.Is_Boolean_Value then
                        Value.supportsSingleThreadExecutionRequests :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Capabilities;

   package StackTraceResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package StackTraceResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["stackFrames", "totalFrames"]);

   procedure Input_StackTraceResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StackTraceResponse;
      Success : in out Boolean) is
      procedure Input_StackTraceResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out StackTraceResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    StackTraceResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  stackFrames
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : StackFrame;
                              begin
                                 Input_StackFrame (Reader, Item, Success);
                                 Value.stackFrames.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  totalFrames
                        Value.totalFrames := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.totalFrames.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_StackTraceResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StackTraceResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_StackTraceResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StackTraceResponse;

   procedure Input_LoadedSourcesArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LoadedSourcesArguments;
      Success : in out Boolean) is
   begin
      Input_Any_Value (Reader, Value, Success);
   end Input_LoadedSourcesArguments;

   package ConfigurationDoneRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_ConfigurationDoneRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ConfigurationDoneRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ConfigurationDoneRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "configurationDone"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Value.arguments := (Is_Set => True, Value => <>);
                     Input_ConfigurationDoneArguments
                       (Reader, Value.arguments.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ConfigurationDoneRequest;

   package StepInTargetsResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package StepInTargetsResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["targets"]);

   procedure Input_StepInTargetsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepInTargetsResponse;
      Success : in out Boolean) is
      procedure Input_StepInTargetsResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out StepInTargetsResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    StepInTargetsResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  targets
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : StepInTarget;
                              begin
                                 Input_StepInTarget (Reader, Item, Success);
                                 Value.targets.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_StepInTargetsResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StepInTargetsResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_StepInTargetsResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StepInTargetsResponse;

   package StackFrame_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["id", "name", "source", "line", "column", "endLine", "endColumn",
       "canRestart", "instructionPointerReference", "moduleId",
       "presentationHint"]);

   procedure Input_StackFrame
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StackFrame;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StackFrame_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  id
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.id :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  name
                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  source
                     Value.source := (Is_Set => True, Value => <>);
                     Input_Source (Reader, Value.source.Value, Success);
                  when 4 =>  --  line
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.line :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  column
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.column :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  endLine
                     Value.endLine := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endLine.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  endColumn
                     Value.endColumn := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endColumn.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 8 =>  --  canRestart
                     if Reader.Is_Boolean_Value then
                        Value.canRestart := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  instructionPointerReference
                     if Reader.Is_String_Value then
                        Value.instructionPointerReference :=
                          Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 10 =>  --  moduleId
                     Value.moduleId       := (Is_Set => True, Value => <>);
                     Value.moduleId.Value := (False, Integer => <>);
                     if Reader.Is_String_Value then
                        Value.moduleId.Value := (True, Reader.String_Value);
                        Reader.Read_Next;
                     elsif Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.moduleId.Value.Integer :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 11 =>  --  presentationHint
                     Value.presentationHint := (Is_Set => True, Value => <>);
                     Input_StackFrame_presentationHint
                       (Reader, Value.presentationHint.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StackFrame;

   package SetExpressionRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_SetExpressionRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetExpressionRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetExpressionRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "setExpression"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_SetExpressionArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetExpressionRequest;

   package SourceArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["source", "sourceReference"]);

   procedure Input_SourceArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SourceArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SourceArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  source
                     Value.source := (Is_Set => True, Value => <>);
                     Input_Source (Reader, Value.source.Value, Success);
                  when 2 =>  --  sourceReference
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.sourceReference :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SourceArguments;

   package ExceptionFilterOptions_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["filterId", "condition"]);

   procedure Input_ExceptionFilterOptions
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionFilterOptions;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ExceptionFilterOptions_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  filterId
                     if Reader.Is_String_Value then
                        Value.filterId := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  condition
                     if Reader.Is_String_Value then
                        Value.condition := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ExceptionFilterOptions;

   package ExceptionBreakpointsFilter_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["filter", "label", "description", "default", "supportsCondition",
       "conditionDescription"]);

   procedure Input_ExceptionBreakpointsFilter
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionBreakpointsFilter;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ExceptionBreakpointsFilter_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  filter
                     if Reader.Is_String_Value then
                        Value.filter := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  label
                     if Reader.Is_String_Value then
                        Value.label := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  description
                     if Reader.Is_String_Value then
                        Value.description := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  default
                     if Reader.Is_Boolean_Value then
                        Value.default := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  supportsCondition
                     if Reader.Is_Boolean_Value then
                        Value.supportsCondition := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  conditionDescription
                     if Reader.Is_String_Value then
                        Value.conditionDescription := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ExceptionBreakpointsFilter;

   package SetVariableRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_SetVariableRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetVariableRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetVariableRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "setVariable"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_SetVariableArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetVariableRequest;

   package AttachRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_AttachRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out AttachRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 AttachRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "attach"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_AttachRequestArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_AttachRequest;

   package MemoryEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package MemoryEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["memoryReference", "offset", "count"]);

   procedure Input_MemoryEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out MemoryEvent;
      Success : in out Boolean) is
      procedure Input_MemoryEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out MemoryEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    MemoryEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  memoryReference
                        if Reader.Is_String_Value then
                           Value.memoryReference := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  offset
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.offset :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 3 =>  --  count
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.count :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_MemoryEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 MemoryEvent_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "memory"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_MemoryEvent_body (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_MemoryEvent;

   package ReadMemoryArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["memoryReference", "offset", "count"]);

   procedure Input_ReadMemoryArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ReadMemoryArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ReadMemoryArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  memoryReference
                     if Reader.Is_String_Value then
                        Value.memoryReference := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  offset
                     Value.offset := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.offset.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  count
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.count :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ReadMemoryArguments;

   package LoadedSourcesRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_LoadedSourcesRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LoadedSourcesRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 LoadedSourcesRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "loadedSources"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Value.arguments := (Is_Set => True, Value => <>);
                     Input_LoadedSourcesArguments
                       (Reader, Value.arguments.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_LoadedSourcesRequest;

   package LaunchRequestArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["noDebug", "__restart", "program"]);

   procedure Input_LaunchRequestArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LaunchRequestArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 LaunchRequestArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  noDebug
                     if Reader.Is_Boolean_Value then
                        Value.noDebug := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  __restart
                     Input_Any_Value (Reader, Value.restart, Success);
                  when 3 =>  --  program
                     if Reader.Is_String_Value then
                        Value.program := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_LaunchRequestArguments;

   package ExitedEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package ExitedEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["exitCode"]);

   procedure Input_ExitedEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExitedEvent;
      Success : in out Boolean) is
      procedure Input_ExitedEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out ExitedEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    ExitedEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  exitCode
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.exitCode :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_ExitedEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ExitedEvent_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "exited"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_ExitedEvent_body (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ExitedEvent;

   package SetBreakpointsRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_SetBreakpointsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetBreakpointsRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetBreakpointsRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "setBreakpoints"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_SetBreakpointsArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetBreakpointsRequest;

   package TerminateResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_TerminateResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TerminateResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 TerminateResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_TerminateResponse;

   package Variable_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["name", "value", "type", "presentationHint", "evaluateName",
       "variablesReference", "namedVariables", "indexedVariables",
       "memoryReference"]);

   procedure Input_Variable
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Variable;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Variable_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  name
                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  value
                     if Reader.Is_String_Value then
                        Value.value := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  type
                     if Reader.Is_String_Value then
                        Value.a_type := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  presentationHint
                     Value.presentationHint := (Is_Set => True, Value => <>);
                     Input_VariablePresentationHint
                       (Reader, Value.presentationHint.Value, Success);
                  when 5 =>  --  evaluateName
                     if Reader.Is_String_Value then
                        Value.evaluateName := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  variablesReference
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.variablesReference :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  namedVariables
                     Value.namedVariables := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.namedVariables.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 8 =>  --  indexedVariables
                     Value.indexedVariables := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.indexedVariables.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  memoryReference
                     if Reader.Is_String_Value then
                        Value.memoryReference := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Variable;

   package StoppedEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package StoppedEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["reason", "description", "threadId", "preserveFocusHint", "text",
       "allThreadsStopped", "hitBreakpointIds"]);

   procedure Input_StoppedEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StoppedEvent;
      Success : in out Boolean) is
      procedure Input_StoppedEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out StoppedEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    StoppedEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  reason
                        Input_StoppedEvent_reason
                          (Reader, Value.reason, Success);
                     when 2 =>  --  description
                        if Reader.Is_String_Value then
                           Value.description := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 3 =>  --  threadId
                        Value.threadId := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.threadId.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 4 =>  --  preserveFocusHint
                        if Reader.Is_Boolean_Value then
                           Value.preserveFocusHint := Reader.Boolean_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 5 =>  --  text
                        if Reader.Is_String_Value then
                           Value.text := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 6 =>  --  allThreadsStopped
                        if Reader.Is_Boolean_Value then
                           Value.allThreadsStopped := Reader.Boolean_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 7 =>  --  hitBreakpointIds
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : Integer;
                              begin
                                 if Reader.Is_Number_Value
                                   and then Reader.Number_Value.Kind =
                                     VSS.JSON.JSON_Integer
                                 then
                                    Item :=
                                      Integer
                                        (Reader.Number_Value.Integer_Value);
                                    Reader.Read_Next;
                                 else
                                    Success := False;
                                 end if;
                                 Value.hitBreakpointIds.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_StoppedEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StoppedEvent_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "stopped"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_StoppedEvent_body (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StoppedEvent;

   package RestartFrameRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_RestartFrameRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RestartFrameRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 RestartFrameRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "restartFrame"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_RestartFrameArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_RestartFrameRequest;

   package AttachRequestArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["__restart"]);

   procedure Input_AttachRequestArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out AttachRequestArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 AttachRequestArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  __restart
                     Input_Any_Value (Reader, Value.restart, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_AttachRequestArguments;

   package ScopesResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package ScopesResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["scopes"]);

   procedure Input_ScopesResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ScopesResponse;
      Success : in out Boolean) is
      procedure Input_ScopesResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out ScopesResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    ScopesResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  scopes
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : Scope;
                              begin
                                 Input_Scope (Reader, Item, Success);
                                 Value.scopes.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_ScopesResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ScopesResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_ScopesResponse_body (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ScopesResponse;

   package StepOutArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["threadId", "singleThread", "granularity"]);

   procedure Input_StepOutArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepOutArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StepOutArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  threadId
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.threadId :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  singleThread
                     if Reader.Is_Boolean_Value then
                        Value.singleThread := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  granularity
                     Value.granularity := (Is_Set => True, Value => <>);
                     Input_SteppingGranularity
                       (Reader, Value.granularity.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StepOutArguments;

   package CompletionsRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_CompletionsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CompletionsRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 CompletionsRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "completions"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_CompletionsArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_CompletionsRequest;

   package StartDebuggingRequestArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["configuration", "request"]);

   procedure Input_StartDebuggingRequestArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StartDebuggingRequestArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StartDebuggingRequestArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  configuration
                     Input_Any_Value (Reader, Value.configuration, Success);
                  when 2 =>  --  request
                     Input_StartDebuggingRequestArguments_request
                       (Reader, Value.request, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StartDebuggingRequestArguments;

   package ProgressUpdateEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package ProgressUpdateEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["progressId", "message", "percentage"]);

   procedure Input_ProgressUpdateEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ProgressUpdateEvent;
      Success : in out Boolean) is
      procedure Input_ProgressUpdateEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out ProgressUpdateEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    ProgressUpdateEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  progressId
                        if Reader.Is_String_Value then
                           Value.progressId := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  message
                        if Reader.Is_String_Value then
                           Value.message := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 3 =>  --  percentage
                        Value.percentage := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value then
                           if Reader.Number_Value.Kind = VSS.JSON.JSON_Integer
                           then
                              Value.percentage.Value :=
                                Float (Reader.Number_Value.Integer_Value);
                           elsif Reader.Number_Value.Kind = VSS.JSON.JSON_Float
                           then
                              Value.percentage.Value :=
                                Float (Reader.Number_Value.Float_Value);
                           else
                              Success := False;
                           end if;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_ProgressUpdateEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ProgressUpdateEvent_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "progressUpdate"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_ProgressUpdateEvent_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ProgressUpdateEvent;

   package ExceptionInfoResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package ExceptionInfoResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["exceptionId", "description", "breakMode", "details"]);

   procedure Input_ExceptionInfoResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionInfoResponse;
      Success : in out Boolean) is
      procedure Input_ExceptionInfoResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out ExceptionInfoResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    ExceptionInfoResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  exceptionId
                        if Reader.Is_String_Value then
                           Value.exceptionId := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  description
                        if Reader.Is_String_Value then
                           Value.description := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 3 =>  --  breakMode
                        Input_ExceptionBreakMode
                          (Reader, Value.breakMode, Success);
                     when 4 =>  --  details
                        Value.details := (Is_Set => True, Value => <>);
                        Input_ExceptionDetails
                          (Reader, Value.details.Value, Success);
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_ExceptionInfoResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ExceptionInfoResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_ExceptionInfoResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ExceptionInfoResponse;

   package InitializedEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   procedure Input_InitializedEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out InitializedEvent;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 InitializedEvent_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "initialized"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_InitializedEvent;

   package SetExpressionResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package SetExpressionResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["value", "type", "presentationHint", "variablesReference",
       "namedVariables", "indexedVariables"]);

   procedure Input_SetExpressionResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetExpressionResponse;
      Success : in out Boolean) is
      procedure Input_SetExpressionResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out SetExpressionResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    SetExpressionResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  value
                        if Reader.Is_String_Value then
                           Value.value := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  type
                        if Reader.Is_String_Value then
                           Value.a_type := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 3 =>  --  presentationHint
                        Value.presentationHint :=
                          (Is_Set => True, Value => <>);
                        Input_VariablePresentationHint
                          (Reader, Value.presentationHint.Value, Success);
                     when 4 =>  --  variablesReference
                        Value.variablesReference :=
                          (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.variablesReference.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 5 =>  --  namedVariables
                        Value.namedVariables := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.namedVariables.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 6 =>  --  indexedVariables
                        Value.indexedVariables :=
                          (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.indexedVariables.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_SetExpressionResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetExpressionResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_SetExpressionResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetExpressionResponse;

   package StepInTarget_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["id", "label", "line", "column", "endLine", "endColumn"]);

   procedure Input_StepInTarget
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepInTarget;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StepInTarget_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  id
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.id :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  label
                     if Reader.Is_String_Value then
                        Value.label := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  line
                     Value.line := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.line.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  column
                     Value.column := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.column.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  endLine
                     Value.endLine := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endLine.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  endColumn
                     Value.endColumn := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endColumn.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StepInTarget;

   package ReverseContinueResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_ReverseContinueResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ReverseContinueResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ReverseContinueResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ReverseContinueResponse;

   package OutputEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package OutputEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["category", "output", "group", "variablesReference", "source", "line",
       "column", "data"]);

   procedure Input_OutputEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out OutputEvent;
      Success : in out Boolean) is
      procedure Input_OutputEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out OutputEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    OutputEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  category
                        Value.category := (Is_Set => True, Value => <>);
                        Input_OutputEvent_category
                          (Reader, Value.category.Value, Success);
                     when 2 =>  --  output
                        if Reader.Is_String_Value then
                           Value.output := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 3 =>  --  group
                        Value.group := (Is_Set => True, Value => <>);
                        Input_OutputEvent_group
                          (Reader, Value.group.Value, Success);
                     when 4 =>  --  variablesReference
                        Value.variablesReference :=
                          (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.variablesReference.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 5 =>  --  source
                        Value.source := (Is_Set => True, Value => <>);
                        Input_Source (Reader, Value.source.Value, Success);
                     when 6 =>  --  line
                        Value.line := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.line.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 7 =>  --  column
                        Value.column := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.column.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 8 =>  --  data
                        Input_Any_Value (Reader, Value.data, Success);
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_OutputEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 OutputEvent_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "output"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_OutputEvent_body (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_OutputEvent;

   package RestartRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_RestartRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RestartRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 RestartRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "restart"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Value.arguments := (Is_Set => True, Value => <>);
                     Input_RestartArguments
                       (Reader, Value.arguments.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_RestartRequest;

   package StackTraceArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["threadId", "startFrame", "levels", "format"]);

   procedure Input_StackTraceArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StackTraceArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StackTraceArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  threadId
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.threadId :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  startFrame
                     Value.startFrame := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.startFrame.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  levels
                     Value.levels := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.levels.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  format
                     Value.format := (Is_Set => True, Value => <>);
                     Input_StackFrameFormat
                       (Reader, Value.format.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StackTraceArguments;

   package Thread_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["id", "name"]);

   procedure Input_Thread
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Thread;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Thread_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  id
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.id :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  name
                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Thread;

   package SetDataBreakpointsRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_SetDataBreakpointsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetDataBreakpointsRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetDataBreakpointsRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "setDataBreakpoints"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_SetDataBreakpointsArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetDataBreakpointsRequest;

   package SourceRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_SourceRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SourceRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SourceRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "source"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_SourceArguments (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SourceRequest;

   package PauseResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_PauseResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out PauseResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 PauseResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_PauseResponse;

   package SetFunctionBreakpointsRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_SetFunctionBreakpointsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetFunctionBreakpointsRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetFunctionBreakpointsRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "setFunctionBreakpoints"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_SetFunctionBreakpointsArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetFunctionBreakpointsRequest;

   package ProcessEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package ProcessEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["name", "systemProcessId", "isLocalProcess", "startMethod",
       "pointerSize"]);

   procedure Input_ProcessEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ProcessEvent;
      Success : in out Boolean) is
      procedure Input_ProcessEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out ProcessEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    ProcessEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  name
                        if Reader.Is_String_Value then
                           Value.name := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  systemProcessId
                        Value.systemProcessId := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.systemProcessId.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 3 =>  --  isLocalProcess
                        if Reader.Is_Boolean_Value then
                           Value.isLocalProcess := Reader.Boolean_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 4 =>  --  startMethod
                        Value.startMethod := (Is_Set => True, Value => <>);
                        Input_ProcessEvent_startMethod
                          (Reader, Value.startMethod.Value, Success);
                     when 5 =>  --  pointerSize
                        Value.pointerSize := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.pointerSize.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_ProcessEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ProcessEvent_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "process"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_ProcessEvent_body (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ProcessEvent;

   package NextResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_NextResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out NextResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 NextResponse_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_NextResponse;

   package AttachResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_AttachResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out AttachResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 AttachResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_AttachResponse;

   package RestartResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_RestartResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RestartResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 RestartResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_RestartResponse;

   package CapabilitiesEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package CapabilitiesEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["capabilities"]);

   procedure Input_CapabilitiesEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CapabilitiesEvent;
      Success : in out Boolean) is
      procedure Input_CapabilitiesEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out CapabilitiesEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    CapabilitiesEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  capabilities
                        Input_Capabilities
                          (Reader, Value.capabilities, Success);
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_CapabilitiesEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 CapabilitiesEvent_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "capabilities"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_CapabilitiesEvent_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_CapabilitiesEvent;

   package Scope_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["name", "presentationHint", "variablesReference", "namedVariables",
       "indexedVariables", "expensive", "source", "line", "column", "endLine",
       "endColumn"]);

   procedure Input_Scope
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Scope;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Scope_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  name
                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  presentationHint
                     Value.presentationHint := (Is_Set => True, Value => <>);
                     Input_Scope_presentationHint
                       (Reader, Value.presentationHint.Value, Success);
                  when 3 =>  --  variablesReference
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.variablesReference :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  namedVariables
                     Value.namedVariables := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.namedVariables.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  indexedVariables
                     Value.indexedVariables := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.indexedVariables.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  expensive
                     if Reader.Is_Boolean_Value then
                        Value.expensive := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  source
                     Value.source := (Is_Set => True, Value => <>);
                     Input_Source (Reader, Value.source.Value, Success);
                  when 8 =>  --  line
                     Value.line := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.line.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  column
                     Value.column := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.column.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 10 =>  --  endLine
                     Value.endLine := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endLine.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 11 =>  --  endColumn
                     Value.endColumn := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endColumn.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Scope;

   package DisassembleArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["memoryReference", "offset", "instructionOffset", "instructionCount",
       "resolveSymbols"]);

   procedure Input_DisassembleArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DisassembleArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 DisassembleArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  memoryReference
                     if Reader.Is_String_Value then
                        Value.memoryReference := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  offset
                     Value.offset := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.offset.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  instructionOffset
                     Value.instructionOffset := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.instructionOffset.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  instructionCount
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.instructionCount :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  resolveSymbols
                     if Reader.Is_Boolean_Value then
                        Value.resolveSymbols := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_DisassembleArguments;

   package SetInstructionBreakpointsRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_SetInstructionBreakpointsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetInstructionBreakpointsRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetInstructionBreakpointsRequest_Minimal_Perfect_Hash
                   .Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value =
                         "setInstructionBreakpoints"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_SetInstructionBreakpointsArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetInstructionBreakpointsRequest;

   package Response_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_Response
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Response;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Response_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Response;

   package DataBreakpointInfoResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package DataBreakpointInfoResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["dataId", "description", "accessTypes", "canPersist"]);

   procedure Input_DataBreakpointInfoResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DataBreakpointInfoResponse;
      Success : in out Boolean) is
      procedure Input_DataBreakpointInfoResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out DataBreakpointInfoResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    DataBreakpointInfoResponse_body_Minimal_Perfect_Hash
                      .Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  dataId
                        if Reader.Is_String_Value then
                           Value.dataId := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  description
                        if Reader.Is_String_Value then
                           Value.description := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 3 =>  --  accessTypes
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : Enum.DataBreakpointAccessType;
                              begin
                                 Input_DataBreakpointAccessType
                                   (Reader, Item, Success);
                                 Value.accessTypes.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when 4 =>  --  canPersist
                        if Reader.Is_Boolean_Value then
                           Value.canPersist := Reader.Boolean_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_DataBreakpointInfoResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 DataBreakpointInfoResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_DataBreakpointInfoResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_DataBreakpointInfoResponse;

   package SourceBreakpoint_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["line", "column", "condition", "hitCondition", "logMessage"]);

   procedure Input_SourceBreakpoint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SourceBreakpoint;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SourceBreakpoint_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  line
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.line :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  column
                     Value.column := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.column.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  condition
                     if Reader.Is_String_Value then
                        Value.condition := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  hitCondition
                     if Reader.Is_String_Value then
                        Value.hitCondition := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  logMessage
                     if Reader.Is_String_Value then
                        Value.logMessage := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SourceBreakpoint;

   package PauseRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_PauseRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out PauseRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 PauseRequest_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "pause"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_PauseArguments (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_PauseRequest;

   package FunctionBreakpoint_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["name", "condition", "hitCondition"]);

   procedure Input_FunctionBreakpoint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out FunctionBreakpoint;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 FunctionBreakpoint_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  name
                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  condition
                     if Reader.Is_String_Value then
                        Value.condition := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  hitCondition
                     if Reader.Is_String_Value then
                        Value.hitCondition := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_FunctionBreakpoint;

   package SetExpressionArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["expression", "value", "frameId", "format"]);

   procedure Input_SetExpressionArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetExpressionArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetExpressionArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  expression
                     if Reader.Is_String_Value then
                        Value.expression := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  value
                     if Reader.Is_String_Value then
                        Value.value := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  frameId
                     Value.frameId := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.frameId.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  format
                     Value.format := (Is_Set => True, Value => <>);
                     Input_ValueFormat (Reader, Value.format.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetExpressionArguments;

   package SetExceptionBreakpointsArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["filters", "filterOptions", "exceptionOptions"]);

   procedure Input_SetExceptionBreakpointsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetExceptionBreakpointsArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetExceptionBreakpointsArguments_Minimal_Perfect_Hash
                   .Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  filters
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.filters.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  filterOptions
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : ExceptionFilterOptions;
                           begin
                              Input_ExceptionFilterOptions
                                (Reader, Item, Success);
                              Value.filterOptions.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  exceptionOptions
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : ExceptionOptions;
                           begin
                              Input_ExceptionOptions (Reader, Item, Success);
                              Value.exceptionOptions.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetExceptionBreakpointsArguments;

   package ValueFormat_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["hex"]);

   procedure Input_ValueFormat
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ValueFormat;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ValueFormat_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  hex
                     if Reader.Is_Boolean_Value then
                        Value.hex := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ValueFormat;

   package RunInTerminalRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_RunInTerminalRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RunInTerminalRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 RunInTerminalRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "runInTerminal"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_RunInTerminalRequestArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_RunInTerminalRequest;

   package CompletionsArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["frameId", "text", "column", "line"]);

   procedure Input_CompletionsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CompletionsArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 CompletionsArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  frameId
                     Value.frameId := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.frameId.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  text
                     if Reader.Is_String_Value then
                        Value.text := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  column
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.column :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  line
                     Value.line := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.line.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_CompletionsArguments;

   package WriteMemoryResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package WriteMemoryResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["offset", "bytesWritten"]);

   procedure Input_WriteMemoryResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out WriteMemoryResponse;
      Success : in out Boolean) is
      procedure Input_WriteMemoryResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out WriteMemoryResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    WriteMemoryResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  offset
                        Value.offset := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.offset.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  bytesWritten
                        Value.bytesWritten := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.bytesWritten.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_WriteMemoryResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 WriteMemoryResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Value.a_body := (Is_Set => True, Value => <>);
                     Input_WriteMemoryResponse_body
                       (Reader, Value.a_body.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_WriteMemoryResponse;

   package ReverseContinueArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["threadId", "singleThread"]);

   procedure Input_ReverseContinueArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ReverseContinueArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ReverseContinueArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  threadId
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.threadId :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  singleThread
                     if Reader.Is_Boolean_Value then
                        Value.singleThread := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ReverseContinueArguments;

   package RunInTerminalResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package RunInTerminalResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["processId", "shellProcessId"]);

   procedure Input_RunInTerminalResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RunInTerminalResponse;
      Success : in out Boolean) is
      procedure Input_RunInTerminalResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out RunInTerminalResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    RunInTerminalResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  processId
                        Value.processId := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.processId.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  shellProcessId
                        Value.shellProcessId := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.shellProcessId.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_RunInTerminalResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 RunInTerminalResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_RunInTerminalResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_RunInTerminalResponse;

   package DisconnectArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["restart", "terminateDebuggee", "suspendDebuggee"]);

   procedure Input_DisconnectArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DisconnectArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 DisconnectArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  restart
                     if Reader.Is_Boolean_Value then
                        Value.restart := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  terminateDebuggee
                     if Reader.Is_Boolean_Value then
                        Value.terminateDebuggee := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  suspendDebuggee
                     if Reader.Is_Boolean_Value then
                        Value.suspendDebuggee := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_DisconnectArguments;

   package Module_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["id", "name", "path", "isOptimized", "isUserCode", "version",
       "symbolStatus", "symbolFilePath", "dateTimeStamp", "addressRange"]);

   procedure Input_Module
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Module;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Module_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  id
                     Value.id := (False, Integer => <>);
                     if Reader.Is_String_Value then
                        Value.id := (True, Reader.String_Value);
                        Reader.Read_Next;
                     elsif Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.id.Integer :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  name
                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  path
                     if Reader.Is_String_Value then
                        Value.path := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  isOptimized
                     if Reader.Is_Boolean_Value then
                        Value.isOptimized := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  isUserCode
                     if Reader.Is_Boolean_Value then
                        Value.isUserCode := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  version
                     if Reader.Is_String_Value then
                        Value.version := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  symbolStatus
                     if Reader.Is_String_Value then
                        Value.symbolStatus := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 8 =>  --  symbolFilePath
                     if Reader.Is_String_Value then
                        Value.symbolFilePath := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  dateTimeStamp
                     if Reader.Is_String_Value then
                        Value.dateTimeStamp := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 10 =>  --  addressRange
                     if Reader.Is_String_Value then
                        Value.addressRange := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Module;

   package GotoTargetsRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_GotoTargetsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out GotoTargetsRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 GotoTargetsRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "gotoTargets"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_GotoTargetsArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_GotoTargetsRequest;

   package ThreadsResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package ThreadsResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["threads"]);

   procedure Input_ThreadsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ThreadsResponse;
      Success : in out Boolean) is
      procedure Input_ThreadsResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out ThreadsResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    ThreadsResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  threads
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : Thread;
                              begin
                                 Input_Thread (Reader, Item, Success);
                                 Value.threads.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_ThreadsResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ThreadsResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_ThreadsResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ThreadsResponse;

   package SetDataBreakpointsResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package SetDataBreakpointsResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["breakpoints"]);

   procedure Input_SetDataBreakpointsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetDataBreakpointsResponse;
      Success : in out Boolean) is
      procedure Input_SetDataBreakpointsResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out SetDataBreakpointsResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    SetDataBreakpointsResponse_body_Minimal_Perfect_Hash
                      .Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  breakpoints
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : Breakpoint;
                              begin
                                 Input_Breakpoint (Reader, Item, Success);
                                 Value.breakpoints.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_SetDataBreakpointsResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetDataBreakpointsResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_SetDataBreakpointsResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetDataBreakpointsResponse;

   package DataBreakpoint_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["dataId", "accessType", "condition", "hitCondition"]);

   procedure Input_DataBreakpoint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DataBreakpoint;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 DataBreakpoint_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  dataId
                     if Reader.Is_String_Value then
                        Value.dataId := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  accessType
                     Value.accessType := (Is_Set => True, Value => <>);
                     Input_DataBreakpointAccessType
                       (Reader, Value.accessType.Value, Success);
                  when 3 =>  --  condition
                     if Reader.Is_String_Value then
                        Value.condition := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  hitCondition
                     if Reader.Is_String_Value then
                        Value.hitCondition := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_DataBreakpoint;

   package SetDataBreakpointsArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["breakpoints"]);

   procedure Input_SetDataBreakpointsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetDataBreakpointsArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetDataBreakpointsArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  breakpoints
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : DataBreakpoint;
                           begin
                              Input_DataBreakpoint (Reader, Item, Success);
                              Value.breakpoints.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetDataBreakpointsArguments;

   package ExceptionPathSegment_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["negate", "names"]);

   procedure Input_ExceptionPathSegment
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionPathSegment;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ExceptionPathSegment_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  negate
                     if Reader.Is_Boolean_Value then
                        Value.negate := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  names
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.names.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ExceptionPathSegment;

   package Message_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["id", "format", "variables", "sendTelemetry", "showUser", "url",
       "urlLabel"]);

   procedure Input_Message
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Message;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Message_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  id
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.id :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  format
                     if Reader.Is_String_Value then
                        Value.format := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  variables
                     Input_Any_Value (Reader, Value.variables, Success);
                  when 4 =>  --  sendTelemetry
                     if Reader.Is_Boolean_Value then
                        Value.sendTelemetry := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  showUser
                     if Reader.Is_Boolean_Value then
                        Value.showUser := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  url
                     if Reader.Is_String_Value then
                        Value.url := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  urlLabel
                     if Reader.Is_String_Value then
                        Value.urlLabel := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Message;

   package SourceResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package SourceResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["content", "mimeType"]);

   procedure Input_SourceResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SourceResponse;
      Success : in out Boolean) is
      procedure Input_SourceResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out SourceResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    SourceResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  content
                        if Reader.Is_String_Value then
                           Value.content := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  mimeType
                        if Reader.Is_String_Value then
                           Value.mimeType := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_SourceResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SourceResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_SourceResponse_body (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SourceResponse;

   package ContinueResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package ContinueResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["allThreadsContinued"]);

   procedure Input_ContinueResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ContinueResponse;
      Success : in out Boolean) is
      procedure Input_ContinueResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out ContinueResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    ContinueResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  allThreadsContinued
                        if Reader.Is_Boolean_Value then
                           Value.allThreadsContinued := Reader.Boolean_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_ContinueResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ContinueResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_ContinueResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ContinueResponse;

   package RestartFrameResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_RestartFrameResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RestartFrameResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 RestartFrameResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_RestartFrameResponse;

   package StepInArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["threadId", "singleThread", "targetId", "granularity"]);

   procedure Input_StepInArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepInArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StepInArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  threadId
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.threadId :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  singleThread
                     if Reader.Is_Boolean_Value then
                        Value.singleThread := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  targetId
                     Value.targetId := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.targetId.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  granularity
                     Value.granularity := (Is_Set => True, Value => <>);
                     Input_SteppingGranularity
                       (Reader, Value.granularity.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StepInArguments;

   package LaunchResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_LaunchResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LaunchResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 LaunchResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_LaunchResponse;

   package StepInResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_StepInResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepInResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StepInResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StepInResponse;

   package TerminateArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["restart"]);

   procedure Input_TerminateArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TerminateArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 TerminateArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  restart
                     if Reader.Is_Boolean_Value then
                        Value.restart := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_TerminateArguments;

   package LaunchRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_LaunchRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LaunchRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 LaunchRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "launch"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_LaunchRequestArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_LaunchRequest;

   package StepOutResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_StepOutResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepOutResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StepOutResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StepOutResponse;

   package EvaluateRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_EvaluateRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out EvaluateRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 EvaluateRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "evaluate"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_EvaluateArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_EvaluateRequest;

   package ContinueArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["threadId", "singleThread"]);

   procedure Input_ContinueArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ContinueArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ContinueArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  threadId
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.threadId :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  singleThread
                     if Reader.Is_Boolean_Value then
                        Value.singleThread := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ContinueArguments;

   package StepBackArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["threadId", "singleThread", "granularity"]);

   procedure Input_StepBackArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepBackArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StepBackArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  threadId
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.threadId :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  singleThread
                     if Reader.Is_Boolean_Value then
                        Value.singleThread := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  granularity
                     Value.granularity := (Is_Set => True, Value => <>);
                     Input_SteppingGranularity
                       (Reader, Value.granularity.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StepBackArguments;

   package BreakpointLocationsArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["source", "line", "column", "endLine", "endColumn"]);

   procedure Input_BreakpointLocationsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out BreakpointLocationsArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 BreakpointLocationsArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  source
                     Input_Source (Reader, Value.source, Success);
                  when 2 =>  --  line
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.line :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  column
                     Value.column := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.column.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  endLine
                     Value.endLine := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endLine.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  endColumn
                     Value.endColumn := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endColumn.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_BreakpointLocationsArguments;

   package CancelArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["requestId", "progressId"]);

   procedure Input_CancelArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CancelArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 CancelArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  requestId
                     Value.requestId := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.requestId.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  progressId
                     if Reader.Is_String_Value then
                        Value.progressId := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_CancelArguments;

   package CompletionsResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package CompletionsResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["targets"]);

   procedure Input_CompletionsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CompletionsResponse;
      Success : in out Boolean) is
      procedure Input_CompletionsResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out CompletionsResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    CompletionsResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  targets
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : CompletionItem;
                              begin
                                 Input_CompletionItem (Reader, Item, Success);
                                 Value.targets.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_CompletionsResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 CompletionsResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_CompletionsResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_CompletionsResponse;

   package Breakpoint_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["id", "verified", "message", "source", "line", "column", "endLine",
       "endColumn", "instructionReference", "offset"]);

   procedure Input_Breakpoint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Breakpoint;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Breakpoint_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  id
                     Value.id := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.id.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  verified
                     if Reader.Is_Boolean_Value then
                        Value.verified := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  message
                     if Reader.Is_String_Value then
                        Value.message := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  source
                     Value.source := (Is_Set => True, Value => <>);
                     Input_Source (Reader, Value.source.Value, Success);
                  when 5 =>  --  line
                     Value.line := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.line.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  column
                     Value.column := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.column.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  endLine
                     Value.endLine := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endLine.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 8 =>  --  endColumn
                     Value.endColumn := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endColumn.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  instructionReference
                     if Reader.Is_String_Value then
                        Value.instructionReference := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 10 =>  --  offset
                     Value.offset := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.offset.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Breakpoint;

   package Source_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["name", "path", "sourceReference", "presentationHint", "origin",
       "sources", "adapterData", "checksums"]);

   procedure Input_Source
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Source;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Source_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  name
                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  path
                     if Reader.Is_String_Value then
                        Value.path := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  sourceReference
                     Value.sourceReference := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.sourceReference.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  presentationHint
                     Value.presentationHint := (Is_Set => True, Value => <>);
                     Input_Source_presentationHint
                       (Reader, Value.presentationHint.Value, Success);
                  when 5 =>  --  origin
                     if Reader.Is_String_Value then
                        Value.origin := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  sources
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : Source;
                           begin
                              Input_Source (Reader, Item, Success);
                              Value.sources.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  adapterData
                     Input_Any_Value (Reader, Value.adapterData, Success);
                  when 8 =>  --  checksums
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : Checksum;
                           begin
                              Input_Checksum (Reader, Item, Success);
                              Value.checksums.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Source;

   package WriteMemoryArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["memoryReference", "offset", "allowPartial", "data"]);

   procedure Input_WriteMemoryArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out WriteMemoryArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 WriteMemoryArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  memoryReference
                     if Reader.Is_String_Value then
                        Value.memoryReference := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  offset
                     Value.offset := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.offset.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  allowPartial
                     if Reader.Is_Boolean_Value then
                        Value.allowPartial := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  data
                     if Reader.Is_String_Value then
                        Value.data := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_WriteMemoryArguments;

   procedure Input_ConfigurationDoneArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ConfigurationDoneArguments;
      Success : in out Boolean) is
   begin
      Input_Any_Value (Reader, Value, Success);
   end Input_ConfigurationDoneArguments;

   package StepOutRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_StepOutRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepOutRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StepOutRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "stepOut"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_StepOutArguments (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StepOutRequest;

   package Request_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_Request
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Request;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Request_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_Any_Value (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Request;

   package LoadedSourceEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package LoadedSourceEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["reason", "source"]);

   procedure Input_LoadedSourceEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LoadedSourceEvent;
      Success : in out Boolean) is
      procedure Input_LoadedSourceEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LoadedSourceEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    LoadedSourceEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  reason
                        Input_LoadedSourceEvent_reason
                          (Reader, Value.reason, Success);
                     when 2 =>  --  source
                        Input_Source (Reader, Value.source, Success);
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_LoadedSourceEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 LoadedSourceEvent_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "loadedSource"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_LoadedSourceEvent_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_LoadedSourceEvent;

   package StackFrameFormat_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["hex", "parameters", "parameterTypes", "parameterNames",
       "parameterValues", "line", "module", "includeAll"]);

   procedure Input_StackFrameFormat
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StackFrameFormat;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StackFrameFormat_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  hex
                     if Reader.Is_Boolean_Value then
                        Value.hex := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  parameters
                     if Reader.Is_Boolean_Value then
                        Value.parameters := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  parameterTypes
                     if Reader.Is_Boolean_Value then
                        Value.parameterTypes := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  parameterNames
                     if Reader.Is_Boolean_Value then
                        Value.parameterNames := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  parameterValues
                     if Reader.Is_Boolean_Value then
                        Value.parameterValues := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  line
                     if Reader.Is_Boolean_Value then
                        Value.line := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  module
                     if Reader.Is_Boolean_Value then
                        Value.module := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 8 =>  --  includeAll
                     if Reader.Is_Boolean_Value then
                        Value.includeAll := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StackFrameFormat;

   package DisassembleRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_DisassembleRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DisassembleRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 DisassembleRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "disassemble"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_DisassembleArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_DisassembleRequest;

   package ReadMemoryResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package ReadMemoryResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["address", "unreadableBytes", "data"]);

   procedure Input_ReadMemoryResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ReadMemoryResponse;
      Success : in out Boolean) is
      procedure Input_ReadMemoryResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out ReadMemoryResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    ReadMemoryResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  address
                        if Reader.Is_String_Value then
                           Value.address := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  unreadableBytes
                        Value.unreadableBytes := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.unreadableBytes.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 3 =>  --  data
                        if Reader.Is_String_Value then
                           Value.data := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_ReadMemoryResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ReadMemoryResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Value.a_body := (Is_Set => True, Value => <>);
                     Input_ReadMemoryResponse_body
                       (Reader, Value.a_body.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ReadMemoryResponse;

   package StepBackRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_StepBackRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepBackRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StepBackRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "stepBack"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_StepBackArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StepBackRequest;

   package ProtocolMessage_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type"]);

   procedure Input_ProtocolMessage
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ProtocolMessage;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ProtocolMessage_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     Input_ProtocolMessage_type
                       (Reader, Value.a_type, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ProtocolMessage;

   package ThreadsRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_ThreadsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ThreadsRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ThreadsRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "threads"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_Any_Value (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ThreadsRequest;

   package VariablesResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package VariablesResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["variables"]);

   procedure Input_VariablesResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out VariablesResponse;
      Success : in out Boolean) is
      procedure Input_VariablesResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out VariablesResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    VariablesResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  variables
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : Variable;
                              begin
                                 Input_Variable (Reader, Item, Success);
                                 Value.variables.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_VariablesResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 VariablesResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_VariablesResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_VariablesResponse;

   package RunInTerminalRequestArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["kind", "title", "cwd", "args", "env", "argsCanBeInterpretedByShell"]);

   procedure Input_RunInTerminalRequestArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RunInTerminalRequestArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 RunInTerminalRequestArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  kind
                     Value.kind := (Is_Set => True, Value => <>);
                     Input_RunInTerminalRequestArguments_kind
                       (Reader, Value.kind.Value, Success);
                  when 2 =>  --  title
                     if Reader.Is_String_Value then
                        Value.title := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  cwd
                     if Reader.Is_String_Value then
                        Value.cwd := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  args
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.args.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  env
                     Input_Any_Value (Reader, Value.env, Success);
                  when 6 =>  --  argsCanBeInterpretedByShell
                     if Reader.Is_Boolean_Value then
                        Value.argsCanBeInterpretedByShell :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_RunInTerminalRequestArguments;

   package TerminateRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_TerminateRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TerminateRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 TerminateRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "terminate"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Value.arguments := (Is_Set => True, Value => <>);
                     Input_TerminateArguments
                       (Reader, Value.arguments.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_TerminateRequest;

   package VariablesArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["variablesReference", "filter", "start", "count", "format"]);

   procedure Input_VariablesArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out VariablesArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 VariablesArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  variablesReference
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.variablesReference :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  filter
                     Value.filter := (Is_Set => True, Value => <>);
                     Input_VariablesArguments_filter
                       (Reader, Value.filter.Value, Success);
                  when 3 =>  --  start
                     Value.start := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.start.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  count
                     Value.count := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.count.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  format
                     Value.format := (Is_Set => True, Value => <>);
                     Input_ValueFormat (Reader, Value.format.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_VariablesArguments;

   package InitializeRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_InitializeRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out InitializeRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 InitializeRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "initialize"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_InitializeRequestArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_InitializeRequest;

   package BreakpointLocationsResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package BreakpointLocationsResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["breakpoints"]);

   procedure Input_BreakpointLocationsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out BreakpointLocationsResponse;
      Success : in out Boolean) is
      procedure Input_BreakpointLocationsResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out BreakpointLocationsResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    BreakpointLocationsResponse_body_Minimal_Perfect_Hash
                      .Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  breakpoints
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : BreakpointLocation;
                              begin
                                 Input_BreakpointLocation
                                   (Reader, Item, Success);
                                 Value.breakpoints.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_BreakpointLocationsResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 BreakpointLocationsResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_BreakpointLocationsResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_BreakpointLocationsResponse;

   package VariablePresentationHint_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["kind", "attributes", "visibility", "lazy"]);

   procedure Input_VariablePresentationHint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out VariablePresentationHint;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 VariablePresentationHint_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  kind
                     Value.kind := (Is_Set => True, Value => <>);
                     Input_VariablePresentationHint_kind
                       (Reader, Value.kind.Value, Success);
                  when 2 =>  --  attributes
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.attributes.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  visibility
                     Value.visibility := (Is_Set => True, Value => <>);
                     Input_VariablePresentationHint_visibility
                       (Reader, Value.visibility.Value, Success);
                  when 4 =>  --  lazy
                     if Reader.Is_Boolean_Value then
                        Value.lazy := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_VariablePresentationHint;

   package DisassembledInstruction_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["address", "instructionBytes", "instruction", "symbol", "location",
       "line", "column", "endLine", "endColumn"]);

   procedure Input_DisassembledInstruction
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DisassembledInstruction;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 DisassembledInstruction_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  address
                     if Reader.Is_String_Value then
                        Value.address := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  instructionBytes
                     if Reader.Is_String_Value then
                        Value.instructionBytes := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  instruction
                     if Reader.Is_String_Value then
                        Value.instruction := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  symbol
                     if Reader.Is_String_Value then
                        Value.symbol := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  location
                     Value.location := (Is_Set => True, Value => <>);
                     Input_Source (Reader, Value.location.Value, Success);
                  when 6 =>  --  line
                     Value.line := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.line.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  column
                     Value.column := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.column.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 8 =>  --  endLine
                     Value.endLine := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endLine.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  endColumn
                     Value.endColumn := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endColumn.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_DisassembledInstruction;

   package PauseArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["threadId"]);

   procedure Input_PauseArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out PauseArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 PauseArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  threadId
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.threadId :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_PauseArguments;

   package CancelResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_CancelResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CancelResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 CancelResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_CancelResponse;

   package InitializeRequestArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["clientID", "clientName", "adapterID", "locale", "linesStartAt1",
       "columnsStartAt1", "pathFormat", "supportsVariableType",
       "supportsVariablePaging", "supportsRunInTerminalRequest",
       "supportsMemoryReferences", "supportsProgressReporting",
       "supportsInvalidatedEvent", "supportsMemoryEvent",
       "supportsArgsCanBeInterpretedByShell",
       "supportsStartDebuggingRequest"]);

   procedure Input_InitializeRequestArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out InitializeRequestArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 InitializeRequestArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  clientID
                     if Reader.Is_String_Value then
                        Value.clientID := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  clientName
                     if Reader.Is_String_Value then
                        Value.clientName := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  adapterID
                     if Reader.Is_String_Value then
                        Value.adapterID := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  locale
                     if Reader.Is_String_Value then
                        Value.locale := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  linesStartAt1
                     if Reader.Is_Boolean_Value then
                        Value.linesStartAt1 := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  columnsStartAt1
                     if Reader.Is_Boolean_Value then
                        Value.columnsStartAt1 := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  pathFormat
                     Value.pathFormat := (Is_Set => True, Value => <>);
                     Input_InitializeRequestArguments_pathFormat
                       (Reader, Value.pathFormat.Value, Success);
                  when 8 =>  --  supportsVariableType
                     if Reader.Is_Boolean_Value then
                        Value.supportsVariableType := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  supportsVariablePaging
                     if Reader.Is_Boolean_Value then
                        Value.supportsVariablePaging := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 10 =>  --  supportsRunInTerminalRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsRunInTerminalRequest :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 11 =>  --  supportsMemoryReferences
                     if Reader.Is_Boolean_Value then
                        Value.supportsMemoryReferences := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 12 =>  --  supportsProgressReporting
                     if Reader.Is_Boolean_Value then
                        Value.supportsProgressReporting :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 13 =>  --  supportsInvalidatedEvent
                     if Reader.Is_Boolean_Value then
                        Value.supportsInvalidatedEvent := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 14 =>  --  supportsMemoryEvent
                     if Reader.Is_Boolean_Value then
                        Value.supportsMemoryEvent := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 15 =>  --  supportsArgsCanBeInterpretedByShell
                     if Reader.Is_Boolean_Value then
                        Value.supportsArgsCanBeInterpretedByShell :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 16 =>  --  supportsStartDebuggingRequest
                     if Reader.Is_Boolean_Value then
                        Value.supportsStartDebuggingRequest :=
                          Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_InitializeRequestArguments;

   package SetInstructionBreakpointsResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package SetInstructionBreakpointsResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["breakpoints"]);

   procedure Input_SetInstructionBreakpointsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetInstructionBreakpointsResponse;
      Success : in out Boolean) is
      procedure Input_SetInstructionBreakpointsResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out SetInstructionBreakpointsResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    SetInstructionBreakpointsResponse_body_Minimal_Perfect_Hash
                      .Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  breakpoints
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : Breakpoint;
                              begin
                                 Input_Breakpoint (Reader, Item, Success);
                                 Value.breakpoints.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_SetInstructionBreakpointsResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetInstructionBreakpointsResponse_Minimal_Perfect_Hash
                   .Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_SetInstructionBreakpointsResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetInstructionBreakpointsResponse;

   package CancelRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_CancelRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CancelRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 CancelRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "cancel"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Value.arguments := (Is_Set => True, Value => <>);
                     Input_CancelArguments
                       (Reader, Value.arguments.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_CancelRequest;

   package ProgressEndEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package ProgressEndEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["progressId", "message"]);

   procedure Input_ProgressEndEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ProgressEndEvent;
      Success : in out Boolean) is
      procedure Input_ProgressEndEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out ProgressEndEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    ProgressEndEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  progressId
                        if Reader.Is_String_Value then
                           Value.progressId := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  message
                        if Reader.Is_String_Value then
                           Value.message := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_ProgressEndEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ProgressEndEvent_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "progressEnd"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_ProgressEndEvent_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ProgressEndEvent;

   package Event_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   procedure Input_Event
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Event;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Event_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value then
                        Value.event := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Event;

   package VariablesRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_VariablesRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out VariablesRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 VariablesRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "variables"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_VariablesArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_VariablesRequest;

   package ExceptionOptions_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["path", "breakMode"]);

   procedure Input_ExceptionOptions
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionOptions;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ExceptionOptions_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  path
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : ExceptionPathSegment;
                           begin
                              Input_ExceptionPathSegment
                                (Reader, Item, Success);
                              Value.path.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  breakMode
                     Input_ExceptionBreakMode
                       (Reader, Value.breakMode, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ExceptionOptions;

   package TerminatedEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package TerminatedEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["restart"]);

   procedure Input_TerminatedEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TerminatedEvent;
      Success : in out Boolean) is
      procedure Input_TerminatedEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out TerminatedEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    TerminatedEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  restart
                        Input_Any_Value (Reader, Value.restart, Success);
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_TerminatedEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 TerminatedEvent_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "terminated"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Value.a_body := (Is_Set => True, Value => <>);
                     Input_TerminatedEvent_body
                       (Reader, Value.a_body.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_TerminatedEvent;

   package StartDebuggingRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_StartDebuggingRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StartDebuggingRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StartDebuggingRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "startDebugging"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_StartDebuggingRequestArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StartDebuggingRequest;

   package ThreadEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package ThreadEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["reason", "threadId"]);

   procedure Input_ThreadEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ThreadEvent;
      Success : in out Boolean) is
      procedure Input_ThreadEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out ThreadEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    ThreadEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  reason
                        Input_ThreadEvent_reason
                          (Reader, Value.reason, Success);
                     when 2 =>  --  threadId
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.threadId :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_ThreadEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ThreadEvent_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "thread"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_ThreadEvent_body (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ThreadEvent;

   package GotoTargetsResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package GotoTargetsResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["targets"]);

   procedure Input_GotoTargetsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out GotoTargetsResponse;
      Success : in out Boolean) is
      procedure Input_GotoTargetsResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out GotoTargetsResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    GotoTargetsResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  targets
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : GotoTarget;
                              begin
                                 Input_GotoTarget (Reader, Item, Success);
                                 Value.targets.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_GotoTargetsResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 GotoTargetsResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_GotoTargetsResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_GotoTargetsResponse;

   package CompletionItem_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["label", "text", "sortText", "detail", "type", "start", "length",
       "selectionStart", "selectionLength"]);

   procedure Input_CompletionItem
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out CompletionItem;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 CompletionItem_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  label
                     if Reader.Is_String_Value then
                        Value.label := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  text
                     if Reader.Is_String_Value then
                        Value.text := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  sortText
                     if Reader.Is_String_Value then
                        Value.sortText := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  detail
                     if Reader.Is_String_Value then
                        Value.detail := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  type
                     Value.a_type := (Is_Set => True, Value => <>);
                     Input_CompletionItemType
                       (Reader, Value.a_type.Value, Success);
                  when 6 =>  --  start
                     Value.start := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.start.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  length
                     Value.length := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.length.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 8 =>  --  selectionStart
                     Value.selectionStart := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.selectionStart.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  selectionLength
                     Value.selectionLength := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.selectionLength.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_CompletionItem;

   package ScopesArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["frameId"]);

   procedure Input_ScopesArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ScopesArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ScopesArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  frameId
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.frameId :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ScopesArguments;

   package ErrorResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package ErrorResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["error"]);

   procedure Input_ErrorResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ErrorResponse;
      Success : in out Boolean) is
      procedure Input_ErrorResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out ErrorResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    ErrorResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  error
                        Value.error := (Is_Set => True, Value => <>);
                        Input_Message (Reader, Value.error.Value, Success);
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_ErrorResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ErrorResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_ErrorResponse_body (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ErrorResponse;

   package SetInstructionBreakpointsArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["breakpoints"]);

   procedure Input_SetInstructionBreakpointsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetInstructionBreakpointsArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetInstructionBreakpointsArguments_Minimal_Perfect_Hash
                   .Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  breakpoints
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : InstructionBreakpoint;
                           begin
                              Input_InstructionBreakpoint
                                (Reader, Item, Success);
                              Value.breakpoints.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetInstructionBreakpointsArguments;

   package GotoArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["threadId", "targetId"]);

   procedure Input_GotoArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out GotoArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 GotoArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  threadId
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.threadId :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  targetId
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.targetId :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_GotoArguments;

   package BreakpointEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package BreakpointEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["reason", "breakpoint"]);

   procedure Input_BreakpointEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out BreakpointEvent;
      Success : in out Boolean) is
      procedure Input_BreakpointEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out BreakpointEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    BreakpointEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  reason
                        Input_BreakpointEvent_reason
                          (Reader, Value.reason, Success);
                     when 2 =>  --  breakpoint
                        Input_Breakpoint (Reader, Value.breakpoint, Success);
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_BreakpointEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 BreakpointEvent_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "breakpoint"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_BreakpointEvent_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_BreakpointEvent;

   package GotoTarget_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["id", "label", "line", "column", "endLine", "endColumn",
       "instructionPointerReference"]);

   procedure Input_GotoTarget
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out GotoTarget;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 GotoTarget_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  id
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.id :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  label
                     if Reader.Is_String_Value then
                        Value.label := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  line
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.line :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  column
                     Value.column := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.column.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  endLine
                     Value.endLine := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endLine.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  endColumn
                     Value.endColumn := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endColumn.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  instructionPointerReference
                     if Reader.Is_String_Value then
                        Value.instructionPointerReference :=
                          Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_GotoTarget;

   package ReadMemoryRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_ReadMemoryRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ReadMemoryRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ReadMemoryRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "readMemory"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_ReadMemoryArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ReadMemoryRequest;

   package ModulesArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["startModule", "moduleCount"]);

   procedure Input_ModulesArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ModulesArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ModulesArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  startModule
                     Value.startModule := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.startModule.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  moduleCount
                     Value.moduleCount := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.moduleCount.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ModulesArguments;

   package NextRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_NextRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out NextRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 NextRequest_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "next"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_NextArguments (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_NextRequest;

   package ProgressStartEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package ProgressStartEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["progressId", "title", "requestId", "cancellable", "message",
       "percentage"]);

   procedure Input_ProgressStartEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ProgressStartEvent;
      Success : in out Boolean) is
      procedure Input_ProgressStartEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out ProgressStartEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    ProgressStartEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  progressId
                        if Reader.Is_String_Value then
                           Value.progressId := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  title
                        if Reader.Is_String_Value then
                           Value.title := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 3 =>  --  requestId
                        Value.requestId := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.requestId.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 4 =>  --  cancellable
                        if Reader.Is_Boolean_Value then
                           Value.cancellable := Reader.Boolean_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 5 =>  --  message
                        if Reader.Is_String_Value then
                           Value.message := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 6 =>  --  percentage
                        Value.percentage := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value then
                           if Reader.Number_Value.Kind = VSS.JSON.JSON_Integer
                           then
                              Value.percentage.Value :=
                                Float (Reader.Number_Value.Integer_Value);
                           elsif Reader.Number_Value.Kind = VSS.JSON.JSON_Float
                           then
                              Value.percentage.Value :=
                                Float (Reader.Number_Value.Float_Value);
                           else
                              Success := False;
                           end if;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_ProgressStartEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ProgressStartEvent_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "progressStart"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_ProgressStartEvent_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ProgressStartEvent;

   package SetVariableResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package SetVariableResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["value", "type", "variablesReference", "namedVariables",
       "indexedVariables"]);

   procedure Input_SetVariableResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetVariableResponse;
      Success : in out Boolean) is
      procedure Input_SetVariableResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out SetVariableResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    SetVariableResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  value
                        if Reader.Is_String_Value then
                           Value.value := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  type
                        if Reader.Is_String_Value then
                           Value.a_type := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 3 =>  --  variablesReference
                        Value.variablesReference :=
                          (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.variablesReference.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 4 =>  --  namedVariables
                        Value.namedVariables := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.namedVariables.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 5 =>  --  indexedVariables
                        Value.indexedVariables :=
                          (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.indexedVariables.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_SetVariableResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetVariableResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_SetVariableResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetVariableResponse;

   package BreakpointLocation_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["line", "column", "endLine", "endColumn"]);

   procedure Input_BreakpointLocation
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out BreakpointLocation;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 BreakpointLocation_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  line
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.line :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  column
                     Value.column := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.column.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  endLine
                     Value.endLine := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endLine.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  endColumn
                     Value.endColumn := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endColumn.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_BreakpointLocation;

   package RestartFrameArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["frameId"]);

   procedure Input_RestartFrameArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out RestartFrameArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 RestartFrameArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  frameId
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.frameId :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_RestartFrameArguments;

   package DisconnectResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_DisconnectResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DisconnectResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 DisconnectResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_DisconnectResponse;

   package SetExceptionBreakpointsRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_SetExceptionBreakpointsRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetExceptionBreakpointsRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetExceptionBreakpointsRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "setExceptionBreakpoints"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_SetExceptionBreakpointsArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetExceptionBreakpointsRequest;

   package WriteMemoryRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_WriteMemoryRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out WriteMemoryRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 WriteMemoryRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "writeMemory"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_WriteMemoryArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_WriteMemoryRequest;

   package DataBreakpointInfoArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["variablesReference", "name", "frameId"]);

   procedure Input_DataBreakpointInfoArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DataBreakpointInfoArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 DataBreakpointInfoArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  variablesReference
                     Value.variablesReference := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.variablesReference.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  name
                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  frameId
                     Value.frameId := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.frameId.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_DataBreakpointInfoArguments;

   package InitializeResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_InitializeResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out InitializeResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 InitializeResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Value.a_body := (Is_Set => True, Value => <>);
                     Input_Capabilities (Reader, Value.a_body.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_InitializeResponse;

   package ConfigurationDoneResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_ConfigurationDoneResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ConfigurationDoneResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ConfigurationDoneResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ConfigurationDoneResponse;

   package StepInTargetsArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["frameId"]);

   procedure Input_StepInTargetsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepInTargetsArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StepInTargetsArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  frameId
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.frameId :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StepInTargetsArguments;

   package EvaluateArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["expression", "frameId", "context", "format"]);

   procedure Input_EvaluateArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out EvaluateArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 EvaluateArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  expression
                     if Reader.Is_String_Value then
                        Value.expression := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  frameId
                     Value.frameId := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.frameId.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  context
                     Value.context := (Is_Set => True, Value => <>);
                     Input_EvaluateArguments_context
                       (Reader, Value.context.Value, Success);
                  when 4 =>  --  format
                     Value.format := (Is_Set => True, Value => <>);
                     Input_ValueFormat (Reader, Value.format.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_EvaluateArguments;

   package SetFunctionBreakpointsArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["breakpoints"]);

   procedure Input_SetFunctionBreakpointsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetFunctionBreakpointsArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetFunctionBreakpointsArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  breakpoints
                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : FunctionBreakpoint;
                           begin
                              Input_FunctionBreakpoint (Reader, Item, Success);
                              Value.breakpoints.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetFunctionBreakpointsArguments;

   package GotoRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_GotoRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out GotoRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 GotoRequest_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "goto"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_GotoArguments (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_GotoRequest;

   package SetFunctionBreakpointsResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package SetFunctionBreakpointsResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["breakpoints"]);

   procedure Input_SetFunctionBreakpointsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetFunctionBreakpointsResponse;
      Success : in out Boolean) is
      procedure Input_SetFunctionBreakpointsResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out SetFunctionBreakpointsResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    SetFunctionBreakpointsResponse_body_Minimal_Perfect_Hash
                      .Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  breakpoints
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : Breakpoint;
                              begin
                                 Input_Breakpoint (Reader, Item, Success);
                                 Value.breakpoints.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_SetFunctionBreakpointsResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetFunctionBreakpointsResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_SetFunctionBreakpointsResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetFunctionBreakpointsResponse;

   package ExceptionInfoArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["threadId"]);

   procedure Input_ExceptionInfoArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ExceptionInfoArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ExceptionInfoArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  threadId
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.threadId :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ExceptionInfoArguments;

   package StackTraceRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_StackTraceRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StackTraceRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StackTraceRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "stackTrace"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_StackTraceArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StackTraceRequest;

   package EvaluateResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package EvaluateResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["result", "type", "presentationHint", "variablesReference",
       "namedVariables", "indexedVariables", "memoryReference"]);

   procedure Input_EvaluateResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out EvaluateResponse;
      Success : in out Boolean) is
      procedure Input_EvaluateResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out EvaluateResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    EvaluateResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  result
                        if Reader.Is_String_Value then
                           Value.result := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  type
                        if Reader.Is_String_Value then
                           Value.a_type := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 3 =>  --  presentationHint
                        Value.presentationHint :=
                          (Is_Set => True, Value => <>);
                        Input_VariablePresentationHint
                          (Reader, Value.presentationHint.Value, Success);
                     when 4 =>  --  variablesReference
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.variablesReference :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 5 =>  --  namedVariables
                        Value.namedVariables := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.namedVariables.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 6 =>  --  indexedVariables
                        Value.indexedVariables :=
                          (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.indexedVariables.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 7 =>  --  memoryReference
                        if Reader.Is_String_Value then
                           Value.memoryReference := Reader.String_Value;
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_EvaluateResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 EvaluateResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_EvaluateResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_EvaluateResponse;

   package ReverseContinueRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_ReverseContinueRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ReverseContinueRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ReverseContinueRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "reverseContinue"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_ReverseContinueArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ReverseContinueRequest;

   package ModulesRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_ModulesRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ModulesRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ModulesRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "modules"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_ModulesArguments (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ModulesRequest;

   package SetBreakpointsResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package SetBreakpointsResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["breakpoints"]);

   procedure Input_SetBreakpointsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetBreakpointsResponse;
      Success : in out Boolean) is
      procedure Input_SetBreakpointsResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out SetBreakpointsResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    SetBreakpointsResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  breakpoints
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : Breakpoint;
                              begin
                                 Input_Breakpoint (Reader, Item, Success);
                                 Value.breakpoints.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_SetBreakpointsResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetBreakpointsResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_SetBreakpointsResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetBreakpointsResponse;

   package InstructionBreakpoint_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["instructionReference", "offset", "condition", "hitCondition"]);

   procedure Input_InstructionBreakpoint
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out InstructionBreakpoint;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 InstructionBreakpoint_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  instructionReference
                     if Reader.Is_String_Value then
                        Value.instructionReference := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  offset
                     Value.offset := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.offset.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  condition
                     if Reader.Is_String_Value then
                        Value.condition := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  hitCondition
                     if Reader.Is_String_Value then
                        Value.hitCondition := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_InstructionBreakpoint;

   package DisconnectRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_DisconnectRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DisconnectRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 DisconnectRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "disconnect"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Value.arguments := (Is_Set => True, Value => <>);
                     Input_DisconnectArguments
                       (Reader, Value.arguments.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_DisconnectRequest;

   package TerminateThreadsResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_TerminateThreadsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out TerminateThreadsResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 TerminateThreadsResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_TerminateThreadsResponse;

   package ScopesRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_ScopesRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ScopesRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ScopesRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "scopes"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_ScopesArguments (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ScopesRequest;

   package SetExceptionBreakpointsResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package SetExceptionBreakpointsResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["breakpoints"]);

   procedure Input_SetExceptionBreakpointsResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out SetExceptionBreakpointsResponse;
      Success : in out Boolean) is
      procedure Input_SetExceptionBreakpointsResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out SetExceptionBreakpointsResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    SetExceptionBreakpointsResponse_body_Minimal_Perfect_Hash
                      .Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  breakpoints
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : Breakpoint;
                              begin
                                 Input_Breakpoint (Reader, Item, Success);
                                 Value.breakpoints.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_SetExceptionBreakpointsResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 SetExceptionBreakpointsResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Value.a_body := (Is_Set => True, Value => <>);
                     Input_SetExceptionBreakpointsResponse_body
                       (Reader, Value.a_body.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_SetExceptionBreakpointsResponse;

   package StepBackResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_StepBackResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepBackResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StepBackResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StepBackResponse;

   package DisassembleResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package DisassembleResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["instructions"]);

   procedure Input_DisassembleResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out DisassembleResponse;
      Success : in out Boolean) is
      procedure Input_DisassembleResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out DisassembleResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    DisassembleResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  instructions
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : DisassembledInstruction;
                              begin
                                 Input_DisassembledInstruction
                                   (Reader, Item, Success);
                                 Value.instructions.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_DisassembleResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 DisassembleResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Value.a_body := (Is_Set => True, Value => <>);
                     Input_DisassembleResponse_body
                       (Reader, Value.a_body.Value, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_DisassembleResponse;

   package InvalidatedEvent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "event", "body"]);

   package InvalidatedEvent_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["areas", "threadId", "stackFrameId"]);

   procedure Input_InvalidatedEvent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out InvalidatedEvent;
      Success : in out Boolean) is
      procedure Input_InvalidatedEvent_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out InvalidatedEvent_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    InvalidatedEvent_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  areas
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : Enum.InvalidatedAreas;
                              begin
                                 Input_InvalidatedAreas
                                   (Reader, Item, Success);
                                 Value.areas.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when 2 =>  --  threadId
                        Value.threadId := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.threadId.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when 3 =>  --  stackFrameId
                        Value.stackFrameId := (Is_Set => True, Value => <>);
                        if Reader.Is_Number_Value
                          and then Reader.Number_Value.Kind =
                            VSS.JSON.JSON_Integer
                        then
                           Value.stackFrameId.Value :=
                             Integer (Reader.Number_Value.Integer_Value);
                           Reader.Read_Next;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_InvalidatedEvent_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 InvalidatedEvent_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "event"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  event
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "invalidated"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  body
                     Input_InvalidatedEvent_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_InvalidatedEvent;

   package GotoTargetsArguments_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["source", "line", "column"]);

   procedure Input_GotoTargetsArguments
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out GotoTargetsArguments;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 GotoTargetsArguments_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  source
                     Input_Source (Reader, Value.source, Success);
                  when 2 =>  --  line
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.line :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  column
                     Value.column := (Is_Set => True, Value => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.column.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_GotoTargetsArguments;

   package StepInRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_StepInRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StepInRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StepInRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "stepIn"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_StepInArguments (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StepInRequest;

   package ContinueRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "command", "arguments"]);

   procedure Input_ContinueRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out ContinueRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 ContinueRequest_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "request"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  command
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "continue"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Input_ContinueArguments
                       (Reader, Value.arguments, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_ContinueRequest;

   package LoadedSourcesResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   package LoadedSourcesResponse_body_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["sources"]);

   procedure Input_LoadedSourcesResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LoadedSourcesResponse;
      Success : in out Boolean) is
      procedure Input_LoadedSourcesResponse_body
        (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LoadedSourcesResponse_body;
         Success : in out Boolean) is
      begin
         if Success and Reader.Is_Start_Object then
            Reader.Read_Next;
         else
            Success := False;
         end if;

         while Success and not Reader.Is_End_Object loop
            if Reader.Is_Key_Name then
               declare
                  Index : constant Natural :=
                    LoadedSourcesResponse_body_Minimal_Perfect_Hash.Get_Index
                      (Reader.Key_Name);
               begin
                  Reader.Read_Next;

                  case Index is
                     when 1 =>  --  sources
                        if Success and Reader.Is_Start_Array then
                           Reader.Read_Next;
                           while Success and not Reader.Is_End_Array loop
                              declare
                                 Item : Source;
                              begin
                                 Input_Source (Reader, Item, Success);
                                 Value.sources.Append (Item);
                              end;
                           end loop;
                           if Success then
                              Reader.Read_Next;  --  skip End_Array
                           end if;
                        else
                           Success := False;
                        end if;
                     when others =>
                        Reader.Skip_Current_Value;
                  end case;
               end;
            else
               Success := False;
            end if;
         end loop;

         if Success then
            Reader.Read_Next;  --  skip End_Object
         end if;
      end Input_LoadedSourcesResponse_body;

   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 LoadedSourcesResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_LoadedSourcesResponse_body
                       (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_LoadedSourcesResponse;

   package StartDebuggingResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["seq", "type", "request_seq", "success", "command", "message", "body"]);

   procedure Input_StartDebuggingResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out StartDebuggingResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 StartDebuggingResponse_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin
               Reader.Read_Next;

               case Index is
                  when 1 =>  --  seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  type
                     if Reader.Is_String_Value
                       and then Reader.String_Value = "response"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  request_seq
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.request_seq :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  success
                     if Reader.Is_Boolean_Value then
                        Value.success := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  command
                     if Reader.Is_String_Value then
                        Value.command := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  message
                     Value.message := (Is_Set => True, Value => <>);
                     Input_Response_message
                       (Reader, Value.message.Value, Success);
                  when 7 =>  --  body
                     Input_Any_Value (Reader, Value.a_body, Success);
                  when others =>
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_StartDebuggingResponse;

end DAP.Tools.Inputs;
