------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

pragma Style_Checks (Off);

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
      procedure Read_ChecksumAlgorithm
        (S :     access Ada.Streams.Root_Stream_Type'Class;
         V : out ChecksumAlgorithm);
      procedure Write_ChecksumAlgorithm
        (S : access Ada.Streams.Root_Stream_Type'Class; V : ChecksumAlgorithm);
      for ChecksumAlgorithm'Read use Read_ChecksumAlgorithm;
      for ChecksumAlgorithm'Write use Write_ChecksumAlgorithm;

      type CompletionItemType is
        (method, a_function, constructor, field, variable, class, a_interface,
         module, property, unit, value, enum, keyword, snippet, text, color,
         file, reference, customcolor);
      procedure Read_CompletionItemType
        (S :     access Ada.Streams.Root_Stream_Type'Class;
         V : out CompletionItemType);
      procedure Write_CompletionItemType
        (S : access Ada.Streams.Root_Stream_Type'Class;
         V : CompletionItemType);
      for CompletionItemType'Read use Read_CompletionItemType;
      for CompletionItemType'Write use Write_CompletionItemType;

      type DataBreakpointAccessType is (read, write, readWrite);
      procedure Read_DataBreakpointAccessType
        (S :     access Ada.Streams.Root_Stream_Type'Class;
         V : out DataBreakpointAccessType);
      procedure Write_DataBreakpointAccessType
        (S : access Ada.Streams.Root_Stream_Type'Class;
         V : DataBreakpointAccessType);
      for DataBreakpointAccessType'Read use Read_DataBreakpointAccessType;
      for DataBreakpointAccessType'Write use Write_DataBreakpointAccessType;

      type ExceptionBreakMode is (never, always, unhandled, userUnhandled);
      procedure Read_ExceptionBreakMode
        (S :     access Ada.Streams.Root_Stream_Type'Class;
         V : out ExceptionBreakMode);
      procedure Write_ExceptionBreakMode
        (S : access Ada.Streams.Root_Stream_Type'Class;
         V : ExceptionBreakMode);
      for ExceptionBreakMode'Read use Read_ExceptionBreakMode;
      for ExceptionBreakMode'Write use Write_ExceptionBreakMode;

      type InvalidatedAreas is (a_all, stacks, threads, variables);
      procedure Read_InvalidatedAreas
        (S :     access Ada.Streams.Root_Stream_Type'Class;
         V : out InvalidatedAreas);
      procedure Write_InvalidatedAreas
        (S : access Ada.Streams.Root_Stream_Type'Class; V : InvalidatedAreas);
      for InvalidatedAreas'Read use Read_InvalidatedAreas;
      for InvalidatedAreas'Write use Write_InvalidatedAreas;

      type SteppingGranularity is (statement, line, instruction);
      procedure Read_SteppingGranularity
        (S :     access Ada.Streams.Root_Stream_Type'Class;
         V : out SteppingGranularity);
      procedure Write_SteppingGranularity
        (S : access Ada.Streams.Root_Stream_Type'Class;
         V : SteppingGranularity);
      for SteppingGranularity'Read use Read_SteppingGranularity;
      for SteppingGranularity'Write use Write_SteppingGranularity;

   end Enums;

   package DAP_String_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Virtual_String, Element_Type => Virtual_String,
      Hash     => LSP.Types.Hash, Equivalent_Keys => VSS.Strings."=");
   type DAP_String_Map is new DAP_String_Maps.Map with null record;

   package DAP_String_Vectors is new Ada.Containers.Vectors
     (Positive, Virtual_String, "=");
   type DAP_String_Vector is new DAP_String_Vectors.Vector with null record;

   package DAP_Integer_Vectors is new Ada.Containers.Vectors
     (Positive, Integer, "=");
   type DAP_Integer_Vector is new DAP_Integer_Vectors.Vector with null record;

   procedure Read_DAP_String_Map
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out DAP_String_Map);
   procedure Write_DAP_String_Map
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_String_Map);
   for DAP_String_Map'Read use Read_DAP_String_Map;
   for DAP_String_Map'Write use Write_DAP_String_Map;

   procedure Read_DAP_String_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_String_Vector);
   procedure Write_DAP_String_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_String_Vector);
   for DAP_String_Vector'Read use Read_DAP_String_Vector;
   for DAP_String_Vector'Write use Write_DAP_String_Vector;

   procedure Read_DAP_Integer_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_Integer_Vector);
   procedure Write_DAP_Integer_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_Integer_Vector);
   for DAP_Integer_Vector'Read use Read_DAP_Integer_Vector;
   for DAP_Integer_Vector'Write use Write_DAP_Integer_Vector;

   type AttachRequestArguments is record
      a_restart : LSP_Any;
   end record;

   procedure Read_AttachRequestArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out AttachRequestArguments);
   procedure Write_AttachRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AttachRequestArguments);
   for AttachRequestArguments'Read use Read_AttachRequestArguments;
   for AttachRequestArguments'Write use Write_AttachRequestArguments;

   type BreakpointLocation is record
      column    : LSP_Number;
      endColumn : LSP_Number;
      endLine   : LSP_Number;
      line      : LSP_Number;
   end record;

   procedure Read_BreakpointLocation
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out BreakpointLocation);
   procedure Write_BreakpointLocation
     (S : access Ada.Streams.Root_Stream_Type'Class; V : BreakpointLocation);
   for BreakpointLocation'Read use Read_BreakpointLocation;
   for BreakpointLocation'Write use Write_BreakpointLocation;

   type CancelArguments is record
      progressId : Virtual_String;
      requestId  : LSP_Number;
   end record;

   procedure Read_CancelArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out CancelArguments);
   procedure Write_CancelArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CancelArguments);
   for CancelArguments'Read use Read_CancelArguments;
   for CancelArguments'Write use Write_CancelArguments;

   type ColumnDescriptor is record
      attributeName : Virtual_String;
      format        : Virtual_String;
      label         : Virtual_String;
      a_type        : Virtual_String;
      width         : LSP_Number;
   end record;

   procedure Read_ColumnDescriptor
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ColumnDescriptor);
   procedure Write_ColumnDescriptor
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ColumnDescriptor);
   for ColumnDescriptor'Read use Read_ColumnDescriptor;
   for ColumnDescriptor'Write use Write_ColumnDescriptor;

   type CompletionsArguments is record
      column  : LSP_Number;
      frameId : LSP_Number;
      line    : LSP_Number;
      text    : Virtual_String;
   end record;

   procedure Read_CompletionsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionsArguments);
   procedure Write_CompletionsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CompletionsArguments);
   for CompletionsArguments'Read use Read_CompletionsArguments;
   for CompletionsArguments'Write use Write_CompletionsArguments;

   type ConfigurationDoneArguments is null record;

   procedure Read_ConfigurationDoneArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ConfigurationDoneArguments);
   procedure Write_ConfigurationDoneArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ConfigurationDoneArguments);
   for ConfigurationDoneArguments'Read use Read_ConfigurationDoneArguments;
   for ConfigurationDoneArguments'Write use Write_ConfigurationDoneArguments;

   type ContinueArguments is record
      threadId : LSP_Number;
   end record;

   procedure Read_ContinueArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ContinueArguments);
   procedure Write_ContinueArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ContinueArguments);
   for ContinueArguments'Read use Read_ContinueArguments;
   for ContinueArguments'Write use Write_ContinueArguments;

   type DataBreakpointInfoArguments is record
      name               : Virtual_String;
      variablesReference : LSP_Number;
   end record;

   procedure Read_DataBreakpointInfoArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DataBreakpointInfoArguments);
   procedure Write_DataBreakpointInfoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DataBreakpointInfoArguments);
   for DataBreakpointInfoArguments'Read use Read_DataBreakpointInfoArguments;
   for DataBreakpointInfoArguments'Write use Write_DataBreakpointInfoArguments;

   type DisassembleArguments is record
      instructionCount  : LSP_Number;
      instructionOffset : LSP_Number;
      memoryReference   : Virtual_String;
      offset            : LSP_Number;
      resolveSymbols    : Boolean;
   end record;

   procedure Read_DisassembleArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DisassembleArguments);
   procedure Write_DisassembleArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DisassembleArguments);
   for DisassembleArguments'Read use Read_DisassembleArguments;
   for DisassembleArguments'Write use Write_DisassembleArguments;

   type DisconnectArguments is record
      restart           : Boolean;
      terminateDebuggee : Boolean;
   end record;

   procedure Read_DisconnectArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DisconnectArguments);
   procedure Write_DisconnectArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DisconnectArguments);
   for DisconnectArguments'Read use Read_DisconnectArguments;
   for DisconnectArguments'Write use Write_DisconnectArguments;

   type ExceptionBreakpointsFilter is record
      conditionDescription : Virtual_String;
      default              : Boolean;
      filter               : Virtual_String;
      label                : Virtual_String;
      supportsCondition    : Boolean;
   end record;

   procedure Read_ExceptionBreakpointsFilter
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ExceptionBreakpointsFilter);
   procedure Write_ExceptionBreakpointsFilter
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExceptionBreakpointsFilter);
   for ExceptionBreakpointsFilter'Read use Read_ExceptionBreakpointsFilter;
   for ExceptionBreakpointsFilter'Write use Write_ExceptionBreakpointsFilter;

   type ExceptionFilterOptions is record
      condition : Virtual_String;
      filterId  : Virtual_String;
   end record;

   procedure Read_ExceptionFilterOptions
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ExceptionFilterOptions);
   procedure Write_ExceptionFilterOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExceptionFilterOptions);
   for ExceptionFilterOptions'Read use Read_ExceptionFilterOptions;
   for ExceptionFilterOptions'Write use Write_ExceptionFilterOptions;

   type ExceptionInfoArguments is record
      threadId : LSP_Number;
   end record;

   procedure Read_ExceptionInfoArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ExceptionInfoArguments);
   procedure Write_ExceptionInfoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExceptionInfoArguments);
   for ExceptionInfoArguments'Read use Read_ExceptionInfoArguments;
   for ExceptionInfoArguments'Write use Write_ExceptionInfoArguments;

   type FunctionBreakpoint is record
      condition    : Virtual_String;
      hitCondition : Virtual_String;
      name         : Virtual_String;
   end record;

   procedure Read_FunctionBreakpoint
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out FunctionBreakpoint);
   procedure Write_FunctionBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : FunctionBreakpoint);
   for FunctionBreakpoint'Read use Read_FunctionBreakpoint;
   for FunctionBreakpoint'Write use Write_FunctionBreakpoint;

   type GotoArguments is record
      targetId : LSP_Number;
      threadId : LSP_Number;
   end record;

   procedure Read_GotoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out GotoArguments);
   procedure Write_GotoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : GotoArguments);
   for GotoArguments'Read use Read_GotoArguments;
   for GotoArguments'Write use Write_GotoArguments;

   type GotoTarget is record
      column                      : LSP_Number;
      endColumn                   : LSP_Number;
      endLine                     : LSP_Number;
      id                          : LSP_Number;
      instructionPointerReference : Virtual_String;
      label                       : Virtual_String;
      line                        : LSP_Number;
   end record;

   procedure Read_GotoTarget
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out GotoTarget);
   procedure Write_GotoTarget
     (S : access Ada.Streams.Root_Stream_Type'Class; V : GotoTarget);
   for GotoTarget'Read use Read_GotoTarget;
   for GotoTarget'Write use Write_GotoTarget;

   type InitializeRequestArguments is record
      adapterID                    : Virtual_String;
      clientID                     : Virtual_String;
      clientName                   : Virtual_String;
      columnsStartAt1              : Boolean;
      linesStartAt1                : Boolean;
      locale                       : Virtual_String;
      pathFormat                   : Virtual_String;
      supportsInvalidatedEvent     : Boolean;
      supportsMemoryReferences     : Boolean;
      supportsProgressReporting    : Boolean;
      supportsRunInTerminalRequest : Boolean;
      supportsVariablePaging       : Boolean;
      supportsVariableType         : Boolean;
   end record;

   procedure Read_InitializeRequestArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeRequestArguments);
   procedure Write_InitializeRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializeRequestArguments);
   for InitializeRequestArguments'Read use Read_InitializeRequestArguments;
   for InitializeRequestArguments'Write use Write_InitializeRequestArguments;

   type InstructionBreakpoint is record
      condition            : Virtual_String;
      hitCondition         : Virtual_String;
      instructionReference : Virtual_String;
      offset               : LSP_Number;
   end record;

   procedure Read_InstructionBreakpoint
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out InstructionBreakpoint);
   procedure Write_InstructionBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InstructionBreakpoint);
   for InstructionBreakpoint'Read use Read_InstructionBreakpoint;
   for InstructionBreakpoint'Write use Write_InstructionBreakpoint;

   type LaunchRequestArguments is record
      a_restart : LSP_Any;
      noDebug   : Boolean;
      program   : Virtual_String;
   end record;

   procedure Read_LaunchRequestArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out LaunchRequestArguments);
   procedure Write_LaunchRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LaunchRequestArguments);
   for LaunchRequestArguments'Read use Read_LaunchRequestArguments;
   for LaunchRequestArguments'Write use Write_LaunchRequestArguments;

   type LoadedSourcesArguments is null record;

   procedure Read_LoadedSourcesArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out LoadedSourcesArguments);
   procedure Write_LoadedSourcesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LoadedSourcesArguments);
   for LoadedSourcesArguments'Read use Read_LoadedSourcesArguments;
   for LoadedSourcesArguments'Write use Write_LoadedSourcesArguments;

   type Message is record
      format        : Virtual_String;
      id            : LSP_Number;
      sendTelemetry : Boolean;
      showUser      : Boolean;
      url           : Virtual_String;
      urlLabel      : Virtual_String;
      variables     : DAP_String_Map;
   end record;

   procedure Read_Message
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Message);
   procedure Write_Message
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Message);
   for Message'Read use Read_Message;
   for Message'Write use Write_Message;

   type Module is record
      addressRange   : Virtual_String;
      dateTimeStamp  : Virtual_String;
      id             : LSP_Number_Or_String;
      isOptimized    : Boolean;
      isUserCode     : Boolean;
      name           : Virtual_String;
      path           : Virtual_String;
      symbolFilePath : Virtual_String;
      symbolStatus   : Virtual_String;
      version        : Virtual_String;
   end record;

   procedure Read_Module
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Module);
   procedure Write_Module
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Module);
   for Module'Read use Read_Module;
   for Module'Write use Write_Module;

   type ModulesArguments is record
      moduleCount : LSP_Number;
      startModule : LSP_Number;
   end record;

   procedure Read_ModulesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ModulesArguments);
   procedure Write_ModulesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ModulesArguments);
   for ModulesArguments'Read use Read_ModulesArguments;
   for ModulesArguments'Write use Write_ModulesArguments;

   type PauseArguments is record
      threadId : LSP_Number;
   end record;

   procedure Read_PauseArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out PauseArguments);
   procedure Write_PauseArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : PauseArguments);
   for PauseArguments'Read use Read_PauseArguments;
   for PauseArguments'Write use Write_PauseArguments;

   type ProtocolMessage is record
      seq    : LSP_Number;
      a_type : Virtual_String;
   end record;

   procedure Read_ProtocolMessage
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ProtocolMessage);
   procedure Write_ProtocolMessage
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ProtocolMessage);
   for ProtocolMessage'Read use Read_ProtocolMessage;
   for ProtocolMessage'Write use Write_ProtocolMessage;

   type ReadMemoryArguments is record
      count           : LSP_Number;
      memoryReference : Virtual_String;
      offset          : LSP_Number;
   end record;

   procedure Read_ReadMemoryArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ReadMemoryArguments);
   procedure Write_ReadMemoryArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ReadMemoryArguments);
   for ReadMemoryArguments'Read use Read_ReadMemoryArguments;
   for ReadMemoryArguments'Write use Write_ReadMemoryArguments;

   type RestartArguments is null record;

   procedure Read_RestartArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out RestartArguments);
   procedure Write_RestartArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : RestartArguments);
   for RestartArguments'Read use Read_RestartArguments;
   for RestartArguments'Write use Write_RestartArguments;

   type RestartFrameArguments is record
      frameId : LSP_Number;
   end record;

   procedure Read_RestartFrameArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out RestartFrameArguments);
   procedure Write_RestartFrameArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RestartFrameArguments);
   for RestartFrameArguments'Read use Read_RestartFrameArguments;
   for RestartFrameArguments'Write use Write_RestartFrameArguments;

   type ReverseContinueArguments is record
      threadId : LSP_Number;
   end record;

   procedure Read_ReverseContinueArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ReverseContinueArguments);
   procedure Write_ReverseContinueArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ReverseContinueArguments);
   for ReverseContinueArguments'Read use Read_ReverseContinueArguments;
   for ReverseContinueArguments'Write use Write_ReverseContinueArguments;

   type ScopesArguments is record
      frameId : LSP_Number;
   end record;

   procedure Read_ScopesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ScopesArguments);
   procedure Write_ScopesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ScopesArguments);
   for ScopesArguments'Read use Read_ScopesArguments;
   for ScopesArguments'Write use Write_ScopesArguments;

   type SourceBreakpoint is record
      column : LSP_Number;
      line   : LSP_Number;
   end record;

   procedure Read_SourceBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out SourceBreakpoint);
   procedure Write_SourceBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SourceBreakpoint);
   for SourceBreakpoint'Read use Read_SourceBreakpoint;
   for SourceBreakpoint'Write use Write_SourceBreakpoint;

   type StackTraceArguments is record
      levels     : LSP_Number;
      startFrame : LSP_Number;
      threadId   : LSP_Number;
   end record;

   procedure Read_StackTraceArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out StackTraceArguments);
   procedure Write_StackTraceArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StackTraceArguments);
   for StackTraceArguments'Read use Read_StackTraceArguments;
   for StackTraceArguments'Write use Write_StackTraceArguments;

   type StepInTarget is record
      id    : LSP_Number;
      label : Virtual_String;
   end record;

   procedure Read_StepInTarget
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepInTarget);
   procedure Write_StepInTarget
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepInTarget);
   for StepInTarget'Read use Read_StepInTarget;
   for StepInTarget'Write use Write_StepInTarget;

   type StepInTargetsArguments is record
      frameId : LSP_Number;
   end record;

   procedure Read_StepInTargetsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out StepInTargetsArguments);
   procedure Write_StepInTargetsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : StepInTargetsArguments);
   for StepInTargetsArguments'Read use Read_StepInTargetsArguments;
   for StepInTargetsArguments'Write use Write_StepInTargetsArguments;

   type TerminateArguments is record
      restart : Boolean;
   end record;

   procedure Read_TerminateArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out TerminateArguments);
   procedure Write_TerminateArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : TerminateArguments);
   for TerminateArguments'Read use Read_TerminateArguments;
   for TerminateArguments'Write use Write_TerminateArguments;

   type Thread is record
      id   : LSP_Number;
      name : Virtual_String;
   end record;

   procedure Read_Thread
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Thread);
   procedure Write_Thread
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Thread);
   for Thread'Read use Read_Thread;
   for Thread'Write use Write_Thread;

   type ValueFormat is record
      hex : Boolean;
   end record;

   procedure Read_ValueFormat
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ValueFormat);
   procedure Write_ValueFormat
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ValueFormat);
   for ValueFormat'Read use Read_ValueFormat;
   for ValueFormat'Write use Write_ValueFormat;

   type Request is record
      arguments : LSP_Any;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_Request
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Request);
   procedure Write_Request
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Request);
   for Request'Read use Read_Request;
   for Request'Write use Write_Request;

   type AttachRequest is record
      arguments : AttachRequestArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_AttachRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out AttachRequest);
   procedure Write_AttachRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : AttachRequest);
   for AttachRequest'Read use Read_AttachRequest;
   for AttachRequest'Write use Write_AttachRequest;

   type Response is record
      command     : Virtual_String;
      a_message   : Virtual_String;
      request_seq : LSP_Number;
      success     : Boolean;
      a_type      : Virtual_String;
      seq         : LSP_Number;
   end record;

   procedure Read_Response
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Response);
   procedure Write_Response
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Response);
   for Response'Read use Read_Response;
   for Response'Write use Write_Response;

   type AttachResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_AttachResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out AttachResponse);
   procedure Write_AttachResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : AttachResponse);
   for AttachResponse'Read use Read_AttachResponse;
   for AttachResponse'Write use Write_AttachResponse;

   type Checksum is record
      algorithm : Enums.ChecksumAlgorithm;
      checksum  : Virtual_String;
   end record;

   procedure Read_Checksum
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Checksum);
   procedure Write_Checksum
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Checksum);
   for Checksum'Read use Read_Checksum;
   for Checksum'Write use Write_Checksum;

   package DAP_Checksum_Vectors is new Ada.Containers.Vectors
     (Positive, Checksum);
   type DAP_Checksum_Vector is new DAP_Checksum_Vectors.Vector with
   null record;
   procedure Read_DAP_Checksum_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_Checksum_Vector);
   procedure Write_DAP_Checksum_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_Checksum_Vector);
   for DAP_Checksum_Vector'Read use Read_DAP_Checksum_Vector;
   for DAP_Checksum_Vector'Write use Write_DAP_Checksum_Vector;

   type Source;
   type Access_Source is access all Source;

   package DAP_Source_Vectors is new Ada.Containers.Vectors
     (Positive, Access_Source);
   type DAP_Source_Vector is new DAP_Source_Vectors.Vector with null record;
   procedure Read_DAP_Source_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_Source_Vector);
   procedure Write_DAP_Source_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_Source_Vector);
   for DAP_Source_Vector'Read use Read_DAP_Source_Vector;
   for DAP_Source_Vector'Write use Write_DAP_Source_Vector;

   type Source is new Ada.Finalization.Controlled with record
      adapterData      : LSP_Any;
      checksums        : DAP_Checksum_Vector;
      name             : Virtual_String;
      origin           : Virtual_String;
      path             : Virtual_String;
      presentationHint : Virtual_String;
      sourceReference  : LSP_Number;
      sources          : DAP_Source_Vector;
   end record;

   procedure Read_Source
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Source);
   procedure Write_Source
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Source);
   for Source'Read use Read_Source;
   for Source'Write use Write_Source;

   overriding procedure Finalize (Self : in out Source);

   type Breakpoint is record
      column               : LSP_Number;
      endColumn            : LSP_Number;
      endLine              : LSP_Number;
      id                   : LSP_Number;
      instructionReference : Virtual_String;
      line                 : LSP_Number := 0;
      a_message            : Virtual_String;
      offset               : LSP_Number;
      a_source             : Source;
      verified             : Boolean;
   end record;

   procedure Read_Breakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Breakpoint);
   procedure Write_Breakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Breakpoint);
   for Breakpoint'Read use Read_Breakpoint;
   for Breakpoint'Write use Write_Breakpoint;

   type Event is record
      event  : Virtual_String;
      a_type : Virtual_String;
      seq    : LSP_Number;
   end record;

   procedure Read_Event
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Event);
   procedure Write_Event
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Event);
   for Event'Read use Read_Event;
   for Event'Write use Write_Event;

   type BreakpointEvent is record
      event           : Virtual_String;
      a_type          : Virtual_String;
      seq             : LSP_Number;
      body_breakpoint : Breakpoint;
      body_reason     : Virtual_String;
   end record;

   procedure Read_BreakpointEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out BreakpointEvent);
   procedure Write_BreakpointEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : BreakpointEvent);
   for BreakpointEvent'Read use Read_BreakpointEvent;
   for BreakpointEvent'Write use Write_BreakpointEvent;

   type BreakpointLocationsArguments is record
      column    : LSP_Number;
      endColumn : LSP_Number;
      endLine   : LSP_Number;
      line      : LSP_Number;
      a_source  : Source;
   end record;

   procedure Read_BreakpointLocationsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out BreakpointLocationsArguments);
   procedure Write_BreakpointLocationsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : BreakpointLocationsArguments);
   for BreakpointLocationsArguments'Read use Read_BreakpointLocationsArguments;
   for BreakpointLocationsArguments'
     Write use Write_BreakpointLocationsArguments;

   type BreakpointLocationsRequest is record
      arguments : BreakpointLocationsArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_BreakpointLocationsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out BreakpointLocationsRequest);
   procedure Write_BreakpointLocationsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : BreakpointLocationsRequest);
   for BreakpointLocationsRequest'Read use Read_BreakpointLocationsRequest;
   for BreakpointLocationsRequest'Write use Write_BreakpointLocationsRequest;

   package DAP_BreakpointLocation_Vectors is new Ada.Containers.Vectors
     (Positive, BreakpointLocation);
   type DAP_BreakpointLocation_Vector is new DAP_BreakpointLocation_Vectors
     .Vector with
   null record;
   procedure Read_DAP_BreakpointLocation_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_BreakpointLocation_Vector);
   procedure Write_DAP_BreakpointLocation_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_BreakpointLocation_Vector);
   for DAP_BreakpointLocation_Vector'
     Read use Read_DAP_BreakpointLocation_Vector;
   for DAP_BreakpointLocation_Vector'
     Write use Write_DAP_BreakpointLocation_Vector;

   type BreakpointLocationsResponse is record
      a_message        : Virtual_String;
      a_type           : Virtual_String;
      command          : Virtual_String;
      request_seq      : LSP_Number;
      seq              : LSP_Number;
      success          : Boolean;
      body_breakpoints : DAP_BreakpointLocation_Vector;
   end record;

   procedure Read_BreakpointLocationsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out BreakpointLocationsResponse);
   procedure Write_BreakpointLocationsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : BreakpointLocationsResponse);
   for BreakpointLocationsResponse'Read use Read_BreakpointLocationsResponse;
   for BreakpointLocationsResponse'Write use Write_BreakpointLocationsResponse;

   type CancelRequest is record
      arguments : CancelArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_CancelRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out CancelRequest);
   procedure Write_CancelRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CancelRequest);
   for CancelRequest'Read use Read_CancelRequest;
   for CancelRequest'Write use Write_CancelRequest;

   type CancelResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_CancelResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out CancelResponse);
   procedure Write_CancelResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CancelResponse);
   for CancelResponse'Read use Read_CancelResponse;
   for CancelResponse'Write use Write_CancelResponse;

   package DAP_ColumnDescriptor_Vectors is new Ada.Containers.Vectors
     (Positive, ColumnDescriptor);
   type DAP_ColumnDescriptor_Vector is new DAP_ColumnDescriptor_Vectors
     .Vector with
   null record;
   procedure Read_DAP_ColumnDescriptor_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_ColumnDescriptor_Vector);
   procedure Write_DAP_ColumnDescriptor_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_ColumnDescriptor_Vector);
   for DAP_ColumnDescriptor_Vector'Read use Read_DAP_ColumnDescriptor_Vector;
   for DAP_ColumnDescriptor_Vector'Write use Write_DAP_ColumnDescriptor_Vector;

   package DAP_ExceptionBreakpointsFilter_Vectors is new Ada.Containers.Vectors
     (Positive, ExceptionBreakpointsFilter);
   type DAP_ExceptionBreakpointsFilter_Vector is new DAP_ExceptionBreakpointsFilter_Vectors
     .Vector with
   null record;
   procedure Read_DAP_ExceptionBreakpointsFilter_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_ExceptionBreakpointsFilter_Vector);
   procedure Write_DAP_ExceptionBreakpointsFilter_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_ExceptionBreakpointsFilter_Vector);
   for DAP_ExceptionBreakpointsFilter_Vector'
     Read use Read_DAP_ExceptionBreakpointsFilter_Vector;
   for DAP_ExceptionBreakpointsFilter_Vector'
     Write use Write_DAP_ExceptionBreakpointsFilter_Vector;

   package DAP_ChecksumAlgorithm_Vectors is new Ada.Containers.Vectors
     (Positive, Enums.ChecksumAlgorithm, "=" => Enums."=");
   type DAP_ChecksumAlgorithm_Vector is new DAP_ChecksumAlgorithm_Vectors
     .Vector with
   null record;
   procedure Read_DAP_ChecksumAlgorithm_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_ChecksumAlgorithm_Vector);
   procedure Write_DAP_ChecksumAlgorithm_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_ChecksumAlgorithm_Vector);
   for DAP_ChecksumAlgorithm_Vector'Read use Read_DAP_ChecksumAlgorithm_Vector;
   for DAP_ChecksumAlgorithm_Vector'
     Write use Write_DAP_ChecksumAlgorithm_Vector;

   type Capabilities is record
      additionalModuleColumns            : DAP_ColumnDescriptor_Vector;
      completionTriggerCharacters        : DAP_String_Vector;
      exceptionBreakpointFilters : DAP_ExceptionBreakpointsFilter_Vector;
      supportTerminateDebuggee           : Boolean;
      supportedChecksumAlgorithms        : DAP_ChecksumAlgorithm_Vector;
      supportsBreakpointLocationsRequest : Boolean;
      supportsCancelRequest              : Boolean;
      supportsClipboardContext           : Boolean;
      supportsCompletionsRequest         : Boolean;
      supportsConditionalBreakpoints     : Boolean;
      supportsConfigurationDoneRequest   : Boolean;
      supportsDataBreakpoints            : Boolean;
      supportsDelayedStackTraceLoading   : Boolean;
      supportsDisassembleRequest         : Boolean;
      supportsEvaluateForHovers          : Boolean;
      supportsExceptionFilterOptions     : Boolean;
      supportsExceptionInfoRequest       : Boolean;
      supportsExceptionOptions           : Boolean;
      supportsFunctionBreakpoints        : Boolean;
      supportsGotoTargetsRequest         : Boolean;
      supportsHitConditionalBreakpoints  : Boolean;
      supportsInstructionBreakpoints     : Boolean;
      supportsLoadedSourcesRequest       : Boolean;
      supportsLogPoints                  : Boolean;
      supportsModulesRequest             : Boolean;
      supportsReadMemoryRequest          : Boolean;
      supportsRestartFrame               : Boolean;
      supportsRestartRequest             : Boolean;
      supportsSetExpression              : Boolean;
      supportsSetVariable                : Boolean;
      supportsStepBack                   : Boolean;
      supportsStepInTargetsRequest       : Boolean;
      supportsSteppingGranularity        : Boolean;
      supportsTerminateRequest           : Boolean;
      supportsTerminateThreadsRequest    : Boolean;
      supportsValueFormattingOptions     : Boolean;
   end record;

   procedure Read_Capabilities
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Capabilities);
   procedure Write_Capabilities
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Capabilities);
   for Capabilities'Read use Read_Capabilities;
   for Capabilities'Write use Write_Capabilities;

   type CapabilitiesEvent is record
      event             : Virtual_String;
      a_type            : Virtual_String;
      seq               : LSP_Number;
      body_capabilities : Capabilities;
   end record;

   procedure Read_CapabilitiesEvent
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out CapabilitiesEvent);
   procedure Write_CapabilitiesEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CapabilitiesEvent);
   for CapabilitiesEvent'Read use Read_CapabilitiesEvent;
   for CapabilitiesEvent'Write use Write_CapabilitiesEvent;

   type CompletionItem is record
      label           : Virtual_String;
      length          : LSP_Number;
      selectionLength : LSP_Number;
      selectionStart  : LSP_Number;
      sortText        : Virtual_String;
      start           : LSP_Number;
      text            : Virtual_String;
      a_type          : Enums.CompletionItemType;
   end record;

   procedure Read_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out CompletionItem);
   procedure Write_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CompletionItem);
   for CompletionItem'Read use Read_CompletionItem;
   for CompletionItem'Write use Write_CompletionItem;

   type CompletionsRequest is record
      arguments : CompletionsArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_CompletionsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionsRequest);
   procedure Write_CompletionsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CompletionsRequest);
   for CompletionsRequest'Read use Read_CompletionsRequest;
   for CompletionsRequest'Write use Write_CompletionsRequest;

   package DAP_CompletionItem_Vectors is new Ada.Containers.Vectors
     (Positive, CompletionItem);
   type DAP_CompletionItem_Vector is new DAP_CompletionItem_Vectors.Vector with
   null record;
   procedure Read_DAP_CompletionItem_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_CompletionItem_Vector);
   procedure Write_DAP_CompletionItem_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_CompletionItem_Vector);
   for DAP_CompletionItem_Vector'Read use Read_DAP_CompletionItem_Vector;
   for DAP_CompletionItem_Vector'Write use Write_DAP_CompletionItem_Vector;

   type CompletionsResponse is record
      a_message    : Virtual_String;
      a_type       : Virtual_String;
      command      : Virtual_String;
      request_seq  : LSP_Number;
      seq          : LSP_Number;
      success      : Boolean;
      body_targets : DAP_CompletionItem_Vector;
   end record;

   procedure Read_CompletionsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionsResponse);
   procedure Write_CompletionsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CompletionsResponse);
   for CompletionsResponse'Read use Read_CompletionsResponse;
   for CompletionsResponse'Write use Write_CompletionsResponse;

   type ConfigurationDoneRequest is record
      arguments : ConfigurationDoneArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_ConfigurationDoneRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ConfigurationDoneRequest);
   procedure Write_ConfigurationDoneRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ConfigurationDoneRequest);
   for ConfigurationDoneRequest'Read use Read_ConfigurationDoneRequest;
   for ConfigurationDoneRequest'Write use Write_ConfigurationDoneRequest;

   type ConfigurationDoneResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_ConfigurationDoneResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ConfigurationDoneResponse);
   procedure Write_ConfigurationDoneResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ConfigurationDoneResponse);
   for ConfigurationDoneResponse'Read use Read_ConfigurationDoneResponse;
   for ConfigurationDoneResponse'Write use Write_ConfigurationDoneResponse;

   type ContinueRequest is record
      arguments : ContinueArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_ContinueRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ContinueRequest);
   procedure Write_ContinueRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ContinueRequest);
   for ContinueRequest'Read use Read_ContinueRequest;
   for ContinueRequest'Write use Write_ContinueRequest;

   type ContinueResponse is record
      a_message                : Virtual_String;
      a_type                   : Virtual_String;
      command                  : Virtual_String;
      request_seq              : LSP_Number;
      seq                      : LSP_Number;
      success                  : Boolean;
      body_allThreadsContinued : Boolean;
   end record;

   procedure Read_ContinueResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ContinueResponse);
   procedure Write_ContinueResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ContinueResponse);
   for ContinueResponse'Read use Read_ContinueResponse;
   for ContinueResponse'Write use Write_ContinueResponse;

   type ContinuedEvent is record
      event                    : Virtual_String;
      a_type                   : Virtual_String;
      seq                      : LSP_Number;
      body_allThreadsContinued : Boolean;
      body_threadId            : LSP_Number;
   end record;

   procedure Read_ContinuedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ContinuedEvent);
   procedure Write_ContinuedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ContinuedEvent);
   for ContinuedEvent'Read use Read_ContinuedEvent;
   for ContinuedEvent'Write use Write_ContinuedEvent;

   type DataBreakpoint is record
      accessType   : Enums.DataBreakpointAccessType;
      condition    : Virtual_String;
      dataId       : Virtual_String;
      hitCondition : Virtual_String;
   end record;

   procedure Read_DataBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out DataBreakpoint);
   procedure Write_DataBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DataBreakpoint);
   for DataBreakpoint'Read use Read_DataBreakpoint;
   for DataBreakpoint'Write use Write_DataBreakpoint;

   type DataBreakpointInfoRequest is record
      arguments : DataBreakpointInfoArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_DataBreakpointInfoRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DataBreakpointInfoRequest);
   procedure Write_DataBreakpointInfoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DataBreakpointInfoRequest);
   for DataBreakpointInfoRequest'Read use Read_DataBreakpointInfoRequest;
   for DataBreakpointInfoRequest'Write use Write_DataBreakpointInfoRequest;

   package DAP_DataBreakpointAccessType_Vectors is new Ada.Containers.Vectors
     (Positive, Enums.DataBreakpointAccessType, "=" => Enums."=");
   type DAP_DataBreakpointAccessType_Vector is new DAP_DataBreakpointAccessType_Vectors
     .Vector with
   null record;
   procedure Read_DAP_DataBreakpointAccessType_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_DataBreakpointAccessType_Vector);
   procedure Write_DAP_DataBreakpointAccessType_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_DataBreakpointAccessType_Vector);
   for DAP_DataBreakpointAccessType_Vector'
     Read use Read_DAP_DataBreakpointAccessType_Vector;
   for DAP_DataBreakpointAccessType_Vector'
     Write use Write_DAP_DataBreakpointAccessType_Vector;

   type DataBreakpointInfoResponse is record
      a_message        : Virtual_String;
      a_type           : Virtual_String;
      command          : Virtual_String;
      request_seq      : LSP_Number;
      seq              : LSP_Number;
      success          : Boolean;
      body_accessTypes : DAP_DataBreakpointAccessType_Vector;
      body_canPersist  : Boolean;
      body_dataId      : Virtual_String;
      body_description : Virtual_String;
   end record;

   procedure Read_DataBreakpointInfoResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DataBreakpointInfoResponse);
   procedure Write_DataBreakpointInfoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DataBreakpointInfoResponse);
   for DataBreakpointInfoResponse'Read use Read_DataBreakpointInfoResponse;
   for DataBreakpointInfoResponse'Write use Write_DataBreakpointInfoResponse;

   type DisassembleRequest is record
      arguments : DisassembleArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_DisassembleRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DisassembleRequest);
   procedure Write_DisassembleRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DisassembleRequest);
   for DisassembleRequest'Read use Read_DisassembleRequest;
   for DisassembleRequest'Write use Write_DisassembleRequest;

   type DisassembledInstruction is record
      address          : Virtual_String;
      column           : LSP_Number;
      endColumn        : LSP_Number;
      endLine          : LSP_Number;
      instruction      : Virtual_String;
      instructionBytes : Virtual_String;
      line             : LSP_Number;
      location         : Source;
      symbol           : Virtual_String;
   end record;

   procedure Read_DisassembledInstruction
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DisassembledInstruction);
   procedure Write_DisassembledInstruction
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DisassembledInstruction);
   for DisassembledInstruction'Read use Read_DisassembledInstruction;
   for DisassembledInstruction'Write use Write_DisassembledInstruction;

   package DAP_DisassembledInstruction_Vectors is new Ada.Containers.Vectors
     (Positive, DisassembledInstruction);
   type DAP_DisassembledInstruction_Vector is new DAP_DisassembledInstruction_Vectors
     .Vector with
   null record;
   procedure Read_DAP_DisassembledInstruction_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_DisassembledInstruction_Vector);
   procedure Write_DAP_DisassembledInstruction_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_DisassembledInstruction_Vector);
   for DAP_DisassembledInstruction_Vector'
     Read use Read_DAP_DisassembledInstruction_Vector;
   for DAP_DisassembledInstruction_Vector'
     Write use Write_DAP_DisassembledInstruction_Vector;

   type DisassembleResponse is record
      a_message         : Virtual_String;
      a_type            : Virtual_String;
      command           : Virtual_String;
      request_seq       : LSP_Number;
      seq               : LSP_Number;
      success           : Boolean;
      body_instructions : DAP_DisassembledInstruction_Vector;
   end record;

   procedure Read_DisassembleResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DisassembleResponse);
   procedure Write_DisassembleResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DisassembleResponse);
   for DisassembleResponse'Read use Read_DisassembleResponse;
   for DisassembleResponse'Write use Write_DisassembleResponse;

   type DisconnectRequest is record
      arguments : DisconnectArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_DisconnectRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DisconnectRequest);
   procedure Write_DisconnectRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DisconnectRequest);
   for DisconnectRequest'Read use Read_DisconnectRequest;
   for DisconnectRequest'Write use Write_DisconnectRequest;

   type DisconnectResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_DisconnectResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DisconnectResponse);
   procedure Write_DisconnectResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DisconnectResponse);
   for DisconnectResponse'Read use Read_DisconnectResponse;
   for DisconnectResponse'Write use Write_DisconnectResponse;

   type ErrorResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
      body_error  : Message;
   end record;

   procedure Read_ErrorResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ErrorResponse);
   procedure Write_ErrorResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ErrorResponse);
   for ErrorResponse'Read use Read_ErrorResponse;
   for ErrorResponse'Write use Write_ErrorResponse;

   type EvaluateArguments is record
      context    : Virtual_String;
      expression : Virtual_String;
      format     : ValueFormat;
      frameId    : LSP_Number;
   end record;

   procedure Read_EvaluateArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out EvaluateArguments);
   procedure Write_EvaluateArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : EvaluateArguments);
   for EvaluateArguments'Read use Read_EvaluateArguments;
   for EvaluateArguments'Write use Write_EvaluateArguments;

   type EvaluateRequest is record
      arguments : EvaluateArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_EvaluateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out EvaluateRequest);
   procedure Write_EvaluateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : EvaluateRequest);
   for EvaluateRequest'Read use Read_EvaluateRequest;
   for EvaluateRequest'Write use Write_EvaluateRequest;

   type VariablePresentationHint is record
      attributes : DAP_String_Vector;
      kind       : Virtual_String;
      visibility : Virtual_String;
   end record;

   procedure Read_VariablePresentationHint
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out VariablePresentationHint);
   procedure Write_VariablePresentationHint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : VariablePresentationHint);
   for VariablePresentationHint'Read use Read_VariablePresentationHint;
   for VariablePresentationHint'Write use Write_VariablePresentationHint;

   type EvaluateResponse is record
      a_message               : Virtual_String;
      a_type                  : Virtual_String;
      command                 : Virtual_String;
      request_seq             : LSP_Number;
      seq                     : LSP_Number;
      success                 : Boolean;
      body_indexedVariables   : LSP_Number;
      body_memoryReference    : Virtual_String;
      body_namedVariables     : LSP_Number;
      body_presentationHint   : VariablePresentationHint;
      body_result             : Virtual_String;
      body_type               : Virtual_String;
      body_variablesReference : LSP_Number;
   end record;

   procedure Read_EvaluateResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out EvaluateResponse);
   procedure Write_EvaluateResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : EvaluateResponse);
   for EvaluateResponse'Read use Read_EvaluateResponse;
   for EvaluateResponse'Write use Write_EvaluateResponse;

   type ExceptionDetails;
   type Access_ExceptionDetails is access all ExceptionDetails;

   package DAP_ExceptionDetails_Vectors is new Ada.Containers.Vectors
     (Positive, Access_ExceptionDetails);
   type DAP_ExceptionDetails_Vector is new DAP_ExceptionDetails_Vectors
     .Vector with
   null record;
   procedure Read_DAP_ExceptionDetails_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_ExceptionDetails_Vector);
   procedure Write_DAP_ExceptionDetails_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_ExceptionDetails_Vector);
   for DAP_ExceptionDetails_Vector'Read use Read_DAP_ExceptionDetails_Vector;
   for DAP_ExceptionDetails_Vector'Write use Write_DAP_ExceptionDetails_Vector;

   type ExceptionDetails is new Ada.Finalization.Controlled with record
      evaluateName   : Virtual_String;
      fullTypeName   : Virtual_String;
      innerException : DAP_ExceptionDetails_Vector;
      a_message      : Virtual_String;
      stackTrace     : Virtual_String;
      typeName       : Virtual_String;
   end record;

   procedure Read_ExceptionDetails
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ExceptionDetails);
   procedure Write_ExceptionDetails
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ExceptionDetails);
   for ExceptionDetails'Read use Read_ExceptionDetails;
   for ExceptionDetails'Write use Write_ExceptionDetails;

   overriding procedure Finalize (Self : in out ExceptionDetails);

   type ExceptionInfoRequest is record
      arguments : ExceptionInfoArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_ExceptionInfoRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ExceptionInfoRequest);
   procedure Write_ExceptionInfoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ExceptionInfoRequest);
   for ExceptionInfoRequest'Read use Read_ExceptionInfoRequest;
   for ExceptionInfoRequest'Write use Write_ExceptionInfoRequest;

   type ExceptionInfoResponse is record
      a_message        : Virtual_String;
      a_type           : Virtual_String;
      command          : Virtual_String;
      request_seq      : LSP_Number;
      seq              : LSP_Number;
      success          : Boolean;
      body_breakMode   : Enums.ExceptionBreakMode;
      body_description : Virtual_String;
      body_details     : ExceptionDetails;
      body_exceptionId : Virtual_String;
   end record;

   procedure Read_ExceptionInfoResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ExceptionInfoResponse);
   procedure Write_ExceptionInfoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExceptionInfoResponse);
   for ExceptionInfoResponse'Read use Read_ExceptionInfoResponse;
   for ExceptionInfoResponse'Write use Write_ExceptionInfoResponse;

   type ExceptionPathSegment is record
      names  : DAP_String_Vector;
      negate : Boolean;
   end record;

   procedure Read_ExceptionPathSegment
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ExceptionPathSegment);
   procedure Write_ExceptionPathSegment
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ExceptionPathSegment);
   for ExceptionPathSegment'Read use Read_ExceptionPathSegment;
   for ExceptionPathSegment'Write use Write_ExceptionPathSegment;

   package DAP_ExceptionPathSegment_Vectors is new Ada.Containers.Vectors
     (Positive, ExceptionPathSegment);
   type DAP_ExceptionPathSegment_Vector is new DAP_ExceptionPathSegment_Vectors
     .Vector with
   null record;
   procedure Read_DAP_ExceptionPathSegment_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_ExceptionPathSegment_Vector);
   procedure Write_DAP_ExceptionPathSegment_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_ExceptionPathSegment_Vector);
   for DAP_ExceptionPathSegment_Vector'
     Read use Read_DAP_ExceptionPathSegment_Vector;
   for DAP_ExceptionPathSegment_Vector'
     Write use Write_DAP_ExceptionPathSegment_Vector;

   type ExceptionOptions is record
      breakMode : Enums.ExceptionBreakMode;
      path      : DAP_ExceptionPathSegment_Vector;
   end record;

   procedure Read_ExceptionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ExceptionOptions);
   procedure Write_ExceptionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ExceptionOptions);
   for ExceptionOptions'Read use Read_ExceptionOptions;
   for ExceptionOptions'Write use Write_ExceptionOptions;

   type ExitedEvent is record
      event         : Virtual_String;
      a_type        : Virtual_String;
      seq           : LSP_Number;
      body_exitCode : LSP_Number;
   end record;

   procedure Read_ExitedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ExitedEvent);
   procedure Write_ExitedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ExitedEvent);
   for ExitedEvent'Read use Read_ExitedEvent;
   for ExitedEvent'Write use Write_ExitedEvent;

   type GotoRequest is record
      arguments : GotoArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_GotoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out GotoRequest);
   procedure Write_GotoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : GotoRequest);
   for GotoRequest'Read use Read_GotoRequest;
   for GotoRequest'Write use Write_GotoRequest;

   type GotoResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_GotoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out GotoResponse);
   procedure Write_GotoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : GotoResponse);
   for GotoResponse'Read use Read_GotoResponse;
   for GotoResponse'Write use Write_GotoResponse;

   type GotoTargetsArguments is record
      column   : LSP_Number;
      line     : LSP_Number;
      a_source : Source;
   end record;

   procedure Read_GotoTargetsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out GotoTargetsArguments);
   procedure Write_GotoTargetsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : GotoTargetsArguments);
   for GotoTargetsArguments'Read use Read_GotoTargetsArguments;
   for GotoTargetsArguments'Write use Write_GotoTargetsArguments;

   type GotoTargetsRequest is record
      arguments : GotoTargetsArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_GotoTargetsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out GotoTargetsRequest);
   procedure Write_GotoTargetsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : GotoTargetsRequest);
   for GotoTargetsRequest'Read use Read_GotoTargetsRequest;
   for GotoTargetsRequest'Write use Write_GotoTargetsRequest;

   package DAP_GotoTarget_Vectors is new Ada.Containers.Vectors
     (Positive, GotoTarget);
   type DAP_GotoTarget_Vector is new DAP_GotoTarget_Vectors.Vector with
   null record;
   procedure Read_DAP_GotoTarget_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_GotoTarget_Vector);
   procedure Write_DAP_GotoTarget_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_GotoTarget_Vector);
   for DAP_GotoTarget_Vector'Read use Read_DAP_GotoTarget_Vector;
   for DAP_GotoTarget_Vector'Write use Write_DAP_GotoTarget_Vector;

   type GotoTargetsResponse is record
      a_message    : Virtual_String;
      a_type       : Virtual_String;
      command      : Virtual_String;
      request_seq  : LSP_Number;
      seq          : LSP_Number;
      success      : Boolean;
      body_targets : DAP_GotoTarget_Vector;
   end record;

   procedure Read_GotoTargetsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out GotoTargetsResponse);
   procedure Write_GotoTargetsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : GotoTargetsResponse);
   for GotoTargetsResponse'Read use Read_GotoTargetsResponse;
   for GotoTargetsResponse'Write use Write_GotoTargetsResponse;

   type InitializeRequest is record
      arguments : InitializeRequestArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_InitializeRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeRequest);
   procedure Write_InitializeRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : InitializeRequest);
   for InitializeRequest'Read use Read_InitializeRequest;
   for InitializeRequest'Write use Write_InitializeRequest;

   type InitializeResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;

   procedure Read_InitializeResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeResponse);
   procedure Write_InitializeResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : InitializeResponse);
   for InitializeResponse'Read use Read_InitializeResponse;
   for InitializeResponse'Write use Write_InitializeResponse;

   type InitializedEvent is record
      event  : Virtual_String;
      a_type : Virtual_String;
      seq    : LSP_Number;
   end record;

   procedure Read_InitializedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out InitializedEvent);
   procedure Write_InitializedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : InitializedEvent);
   for InitializedEvent'Read use Read_InitializedEvent;
   for InitializedEvent'Write use Write_InitializedEvent;

   package DAP_InvalidatedAreas_Vectors is new Ada.Containers.Vectors
     (Positive, Enums.InvalidatedAreas, "=" => Enums."=");
   type DAP_InvalidatedAreas_Vector is new DAP_InvalidatedAreas_Vectors
     .Vector with
   null record;
   procedure Read_DAP_InvalidatedAreas_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_InvalidatedAreas_Vector);
   procedure Write_DAP_InvalidatedAreas_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_InvalidatedAreas_Vector);
   for DAP_InvalidatedAreas_Vector'Read use Read_DAP_InvalidatedAreas_Vector;
   for DAP_InvalidatedAreas_Vector'Write use Write_DAP_InvalidatedAreas_Vector;

   type InvalidatedEvent is record
      event             : Virtual_String;
      a_type            : Virtual_String;
      seq               : LSP_Number;
      body_areas        : DAP_InvalidatedAreas_Vector;
      body_stackFrameId : LSP_Number;
      body_threadId     : LSP_Number;
   end record;

   procedure Read_InvalidatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out InvalidatedEvent);
   procedure Write_InvalidatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : InvalidatedEvent);
   for InvalidatedEvent'Read use Read_InvalidatedEvent;
   for InvalidatedEvent'Write use Write_InvalidatedEvent;

   type LaunchRequest is record
      arguments : LaunchRequestArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_LaunchRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out LaunchRequest);
   procedure Write_LaunchRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : LaunchRequest);
   for LaunchRequest'Read use Read_LaunchRequest;
   for LaunchRequest'Write use Write_LaunchRequest;

   type LaunchResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_LaunchResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out LaunchResponse);
   procedure Write_LaunchResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : LaunchResponse);
   for LaunchResponse'Read use Read_LaunchResponse;
   for LaunchResponse'Write use Write_LaunchResponse;

   type LoadedSourceEvent is record
      event       : Virtual_String;
      a_type      : Virtual_String;
      seq         : LSP_Number;
      body_reason : Virtual_String;
      body_source : Source;
   end record;

   procedure Read_LoadedSourceEvent
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out LoadedSourceEvent);
   procedure Write_LoadedSourceEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : LoadedSourceEvent);
   for LoadedSourceEvent'Read use Read_LoadedSourceEvent;
   for LoadedSourceEvent'Write use Write_LoadedSourceEvent;

   type LoadedSourcesRequest is record
      arguments : LoadedSourcesArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_LoadedSourcesRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out LoadedSourcesRequest);
   procedure Write_LoadedSourcesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : LoadedSourcesRequest);
   for LoadedSourcesRequest'Read use Read_LoadedSourcesRequest;
   for LoadedSourcesRequest'Write use Write_LoadedSourcesRequest;

   type LoadedSourcesResponse is new Ada.Finalization.Controlled with record
      a_message    : Virtual_String;
      a_type       : Virtual_String;
      command      : Virtual_String;
      request_seq  : LSP_Number;
      seq          : LSP_Number;
      success      : Boolean;
      body_sources : DAP_Source_Vector;
   end record;

   procedure Read_LoadedSourcesResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out LoadedSourcesResponse);
   procedure Write_LoadedSourcesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LoadedSourcesResponse);
   for LoadedSourcesResponse'Read use Read_LoadedSourcesResponse;
   for LoadedSourcesResponse'Write use Write_LoadedSourcesResponse;

   overriding procedure Finalize (Self : in out LoadedSourcesResponse);

   type ModuleEvent is record
      event       : Virtual_String;
      a_type      : Virtual_String;
      seq         : LSP_Number;
      body_module : Module;
      body_reason : Virtual_String;
   end record;

   procedure Read_ModuleEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ModuleEvent);
   procedure Write_ModuleEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ModuleEvent);
   for ModuleEvent'Read use Read_ModuleEvent;
   for ModuleEvent'Write use Write_ModuleEvent;

   type ModulesRequest is record
      arguments : ModulesArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_ModulesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ModulesRequest);
   procedure Write_ModulesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ModulesRequest);
   for ModulesRequest'Read use Read_ModulesRequest;
   for ModulesRequest'Write use Write_ModulesRequest;

   package DAP_Module_Vectors is new Ada.Containers.Vectors (Positive, Module);
   type DAP_Module_Vector is new DAP_Module_Vectors.Vector with null record;
   procedure Read_DAP_Module_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_Module_Vector);
   procedure Write_DAP_Module_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_Module_Vector);
   for DAP_Module_Vector'Read use Read_DAP_Module_Vector;
   for DAP_Module_Vector'Write use Write_DAP_Module_Vector;

   type ModulesResponse is record
      a_message         : Virtual_String;
      a_type            : Virtual_String;
      command           : Virtual_String;
      request_seq       : LSP_Number;
      seq               : LSP_Number;
      success           : Boolean;
      body_modules      : DAP_Module_Vector;
      body_totalModules : LSP_Number;
   end record;

   procedure Read_ModulesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ModulesResponse);
   procedure Write_ModulesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ModulesResponse);
   for ModulesResponse'Read use Read_ModulesResponse;
   for ModulesResponse'Write use Write_ModulesResponse;

   type ModulesViewDescriptor is record
      columns : DAP_ColumnDescriptor_Vector;
   end record;

   procedure Read_ModulesViewDescriptor
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ModulesViewDescriptor);
   procedure Write_ModulesViewDescriptor
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ModulesViewDescriptor);
   for ModulesViewDescriptor'Read use Read_ModulesViewDescriptor;
   for ModulesViewDescriptor'Write use Write_ModulesViewDescriptor;

   type NextArguments is record
      granularity : Enums.SteppingGranularity;
      threadId    : LSP_Number;
   end record;

   procedure Read_NextArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out NextArguments);
   procedure Write_NextArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : NextArguments);
   for NextArguments'Read use Read_NextArguments;
   for NextArguments'Write use Write_NextArguments;

   type NextRequest is record
      arguments : NextArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_NextRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out NextRequest);
   procedure Write_NextRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : NextRequest);
   for NextRequest'Read use Read_NextRequest;
   for NextRequest'Write use Write_NextRequest;

   type NextResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_NextResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out NextResponse);
   procedure Write_NextResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : NextResponse);
   for NextResponse'Read use Read_NextResponse;
   for NextResponse'Write use Write_NextResponse;

   type OutputEvent is record
      event                   : Virtual_String;
      a_type                  : Virtual_String;
      seq                     : LSP_Number;
      body_category           : Virtual_String;
      body_column             : LSP_Number;
      body_data               : LSP_Any;
      body_group              : Virtual_String;
      body_line               : LSP_Number;
      body_output             : Virtual_String;
      body_source             : Source;
      body_variablesReference : LSP_Number;
   end record;

   procedure Read_OutputEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out OutputEvent);
   procedure Write_OutputEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : OutputEvent);
   for OutputEvent'Read use Read_OutputEvent;
   for OutputEvent'Write use Write_OutputEvent;

   type PauseRequest is record
      arguments : PauseArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_PauseRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out PauseRequest);
   procedure Write_PauseRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : PauseRequest);
   for PauseRequest'Read use Read_PauseRequest;
   for PauseRequest'Write use Write_PauseRequest;

   type PauseResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_PauseResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out PauseResponse);
   procedure Write_PauseResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : PauseResponse);
   for PauseResponse'Read use Read_PauseResponse;
   for PauseResponse'Write use Write_PauseResponse;

   type ProcessEvent is record
      event                : Virtual_String;
      a_type               : Virtual_String;
      seq                  : LSP_Number;
      body_isLocalProcess  : Boolean;
      body_name            : Virtual_String;
      body_pointerSize     : LSP_Number;
      body_startMethod     : Virtual_String;
      body_systemProcessId : LSP_Number;
   end record;

   procedure Read_ProcessEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ProcessEvent);
   procedure Write_ProcessEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ProcessEvent);
   for ProcessEvent'Read use Read_ProcessEvent;
   for ProcessEvent'Write use Write_ProcessEvent;

   type ProgressEndEvent is record
      event           : Virtual_String;
      a_type          : Virtual_String;
      seq             : LSP_Number;
      body_message    : Virtual_String;
      body_progressId : Virtual_String;
   end record;

   procedure Read_ProgressEndEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ProgressEndEvent);
   procedure Write_ProgressEndEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ProgressEndEvent);
   for ProgressEndEvent'Read use Read_ProgressEndEvent;
   for ProgressEndEvent'Write use Write_ProgressEndEvent;

   type ProgressStartEvent is record
      event            : Virtual_String;
      a_type           : Virtual_String;
      seq              : LSP_Number;
      body_cancellable : Boolean;
      body_message     : Virtual_String;
      body_percentage  : LSP_Number;
      body_progressId  : Virtual_String;
      body_requestId   : LSP_Number;
      body_title       : Virtual_String;
   end record;

   procedure Read_ProgressStartEvent
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ProgressStartEvent);
   procedure Write_ProgressStartEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ProgressStartEvent);
   for ProgressStartEvent'Read use Read_ProgressStartEvent;
   for ProgressStartEvent'Write use Write_ProgressStartEvent;

   type ProgressUpdateEvent is record
      event           : Virtual_String;
      a_type          : Virtual_String;
      seq             : LSP_Number;
      body_message    : Virtual_String;
      body_percentage : LSP_Number;
      body_progressId : Virtual_String;
   end record;

   procedure Read_ProgressUpdateEvent
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ProgressUpdateEvent);
   procedure Write_ProgressUpdateEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ProgressUpdateEvent);
   for ProgressUpdateEvent'Read use Read_ProgressUpdateEvent;
   for ProgressUpdateEvent'Write use Write_ProgressUpdateEvent;

   type ReadMemoryRequest is record
      arguments : ReadMemoryArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_ReadMemoryRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ReadMemoryRequest);
   procedure Write_ReadMemoryRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ReadMemoryRequest);
   for ReadMemoryRequest'Read use Read_ReadMemoryRequest;
   for ReadMemoryRequest'Write use Write_ReadMemoryRequest;

   type ReadMemoryResponse is record
      a_message            : Virtual_String;
      a_type               : Virtual_String;
      command              : Virtual_String;
      request_seq          : LSP_Number;
      seq                  : LSP_Number;
      success              : Boolean;
      body_address         : Virtual_String;
      body_data            : Virtual_String;
      body_unreadableBytes : LSP_Number;
   end record;

   procedure Read_ReadMemoryResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ReadMemoryResponse);
   procedure Write_ReadMemoryResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ReadMemoryResponse);
   for ReadMemoryResponse'Read use Read_ReadMemoryResponse;
   for ReadMemoryResponse'Write use Write_ReadMemoryResponse;

   type RestartFrameRequest is record
      arguments : RestartFrameArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_RestartFrameRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out RestartFrameRequest);
   procedure Write_RestartFrameRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : RestartFrameRequest);
   for RestartFrameRequest'Read use Read_RestartFrameRequest;
   for RestartFrameRequest'Write use Write_RestartFrameRequest;

   type RestartFrameResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_RestartFrameResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out RestartFrameResponse);
   procedure Write_RestartFrameResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : RestartFrameResponse);
   for RestartFrameResponse'Read use Read_RestartFrameResponse;
   for RestartFrameResponse'Write use Write_RestartFrameResponse;

   type RestartRequest is record
      arguments : RestartArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_RestartRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out RestartRequest);
   procedure Write_RestartRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : RestartRequest);
   for RestartRequest'Read use Read_RestartRequest;
   for RestartRequest'Write use Write_RestartRequest;

   type RestartResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_RestartResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out RestartResponse);
   procedure Write_RestartResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : RestartResponse);
   for RestartResponse'Read use Read_RestartResponse;
   for RestartResponse'Write use Write_RestartResponse;

   type ReverseContinueRequest is record
      arguments : ReverseContinueArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_ReverseContinueRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ReverseContinueRequest);
   procedure Write_ReverseContinueRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ReverseContinueRequest);
   for ReverseContinueRequest'Read use Read_ReverseContinueRequest;
   for ReverseContinueRequest'Write use Write_ReverseContinueRequest;

   type ReverseContinueResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_ReverseContinueResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ReverseContinueResponse);
   procedure Write_ReverseContinueResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ReverseContinueResponse);
   for ReverseContinueResponse'Read use Read_ReverseContinueResponse;
   for ReverseContinueResponse'Write use Write_ReverseContinueResponse;

   type RunInTerminalRequestArguments is record
      args : DAP_String_Vector;
      cwd  : Virtual_String;
      env  : DAP_String_Map;
      kind : Virtual_String;
   end record;

   procedure Read_RunInTerminalRequestArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out RunInTerminalRequestArguments);
   procedure Write_RunInTerminalRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RunInTerminalRequestArguments);
   for RunInTerminalRequestArguments'
     Read use Read_RunInTerminalRequestArguments;
   for RunInTerminalRequestArguments'
     Write use Write_RunInTerminalRequestArguments;

   type RunInTerminalRequest is record
      arguments : RunInTerminalRequestArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_RunInTerminalRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out RunInTerminalRequest);
   procedure Write_RunInTerminalRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : RunInTerminalRequest);
   for RunInTerminalRequest'Read use Read_RunInTerminalRequest;
   for RunInTerminalRequest'Write use Write_RunInTerminalRequest;

   type RunInTerminalResponse is record
      a_message           : Virtual_String;
      a_type              : Virtual_String;
      command             : Virtual_String;
      request_seq         : LSP_Number;
      seq                 : LSP_Number;
      success             : Boolean;
      body_processId      : LSP_Number;
      body_shellProcessId : LSP_Number;
   end record;

   procedure Read_RunInTerminalResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out RunInTerminalResponse);
   procedure Write_RunInTerminalResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RunInTerminalResponse);
   for RunInTerminalResponse'Read use Read_RunInTerminalResponse;
   for RunInTerminalResponse'Write use Write_RunInTerminalResponse;

   type Scope is record
      column             : LSP_Number;
      endColumn          : LSP_Number;
      endLine            : LSP_Number;
      expensive          : Boolean;
      indexedVariables   : LSP_Number;
      line               : LSP_Number;
      name               : Virtual_String;
      namedVariables     : LSP_Number;
      presentationHint   : Virtual_String;
      a_source           : Source;
      variablesReference : LSP_Number;
   end record;

   procedure Read_Scope
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Scope);
   procedure Write_Scope
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Scope);
   for Scope'Read use Read_Scope;
   for Scope'Write use Write_Scope;

   type ScopesRequest is record
      arguments : ScopesArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_ScopesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ScopesRequest);
   procedure Write_ScopesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ScopesRequest);
   for ScopesRequest'Read use Read_ScopesRequest;
   for ScopesRequest'Write use Write_ScopesRequest;

   package DAP_Scope_Vectors is new Ada.Containers.Vectors (Positive, Scope);
   type DAP_Scope_Vector is new DAP_Scope_Vectors.Vector with null record;
   procedure Read_DAP_Scope_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out DAP_Scope_Vector);
   procedure Write_DAP_Scope_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_Scope_Vector);
   for DAP_Scope_Vector'Read use Read_DAP_Scope_Vector;
   for DAP_Scope_Vector'Write use Write_DAP_Scope_Vector;

   type ScopesResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
      body_scopes : DAP_Scope_Vector;
   end record;

   procedure Read_ScopesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ScopesResponse);
   procedure Write_ScopesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ScopesResponse);
   for ScopesResponse'Read use Read_ScopesResponse;
   for ScopesResponse'Write use Write_ScopesResponse;

   package DAP_SourceBreakpoint_Vectors is new Ada.Containers.Vectors
     (Positive, SourceBreakpoint);
   type DAP_SourceBreakpoint_Vector is new DAP_SourceBreakpoint_Vectors
     .Vector with
   null record;
   procedure Read_DAP_SourceBreakpoint_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_SourceBreakpoint_Vector);
   procedure Write_DAP_SourceBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_SourceBreakpoint_Vector);
   for DAP_SourceBreakpoint_Vector'Read use Read_DAP_SourceBreakpoint_Vector;
   for DAP_SourceBreakpoint_Vector'Write use Write_DAP_SourceBreakpoint_Vector;

   type SetBreakpointsArguments is record
      breakpoints    : DAP_SourceBreakpoint_Vector;
      lines          : DAP_Integer_Vector;
      a_source       : Source;
      sourceModified : Boolean;
   end record;

   procedure Read_SetBreakpointsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetBreakpointsArguments);
   procedure Write_SetBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetBreakpointsArguments);
   for SetBreakpointsArguments'Read use Read_SetBreakpointsArguments;
   for SetBreakpointsArguments'Write use Write_SetBreakpointsArguments;

   type SetBreakpointsRequest is record
      arguments : SetBreakpointsArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_SetBreakpointsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetBreakpointsRequest);
   procedure Write_SetBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetBreakpointsRequest);
   for SetBreakpointsRequest'Read use Read_SetBreakpointsRequest;
   for SetBreakpointsRequest'Write use Write_SetBreakpointsRequest;

   package DAP_Breakpoint_Vectors is new Ada.Containers.Vectors
     (Positive, Breakpoint);
   type DAP_Breakpoint_Vector is new DAP_Breakpoint_Vectors.Vector with
   null record;
   procedure Read_DAP_Breakpoint_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_Breakpoint_Vector);
   procedure Write_DAP_Breakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_Breakpoint_Vector);
   for DAP_Breakpoint_Vector'Read use Read_DAP_Breakpoint_Vector;
   for DAP_Breakpoint_Vector'Write use Write_DAP_Breakpoint_Vector;

   type SetBreakpointsResponse is record
      a_message        : Virtual_String;
      a_type           : Virtual_String;
      command          : Virtual_String;
      request_seq      : LSP_Number;
      seq              : LSP_Number;
      success          : Boolean;
      body_breakpoints : DAP_Breakpoint_Vector;
   end record;

   procedure Read_SetBreakpointsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetBreakpointsResponse);
   procedure Write_SetBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetBreakpointsResponse);
   for SetBreakpointsResponse'Read use Read_SetBreakpointsResponse;
   for SetBreakpointsResponse'Write use Write_SetBreakpointsResponse;

   package DAP_DataBreakpoint_Vectors is new Ada.Containers.Vectors
     (Positive, DataBreakpoint);
   type DAP_DataBreakpoint_Vector is new DAP_DataBreakpoint_Vectors.Vector with
   null record;
   procedure Read_DAP_DataBreakpoint_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_DataBreakpoint_Vector);
   procedure Write_DAP_DataBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_DataBreakpoint_Vector);
   for DAP_DataBreakpoint_Vector'Read use Read_DAP_DataBreakpoint_Vector;
   for DAP_DataBreakpoint_Vector'Write use Write_DAP_DataBreakpoint_Vector;

   type SetDataBreakpointsArguments is record
      breakpoints : DAP_DataBreakpoint_Vector;
   end record;

   procedure Read_SetDataBreakpointsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetDataBreakpointsArguments);
   procedure Write_SetDataBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetDataBreakpointsArguments);
   for SetDataBreakpointsArguments'Read use Read_SetDataBreakpointsArguments;
   for SetDataBreakpointsArguments'Write use Write_SetDataBreakpointsArguments;

   type SetDataBreakpointsRequest is record
      arguments : SetDataBreakpointsArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_SetDataBreakpointsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetDataBreakpointsRequest);
   procedure Write_SetDataBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetDataBreakpointsRequest);
   for SetDataBreakpointsRequest'Read use Read_SetDataBreakpointsRequest;
   for SetDataBreakpointsRequest'Write use Write_SetDataBreakpointsRequest;

   type SetDataBreakpointsResponse is record
      a_message        : Virtual_String;
      a_type           : Virtual_String;
      command          : Virtual_String;
      request_seq      : LSP_Number;
      seq              : LSP_Number;
      success          : Boolean;
      body_breakpoints : DAP_Breakpoint_Vector;
   end record;

   procedure Read_SetDataBreakpointsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetDataBreakpointsResponse);
   procedure Write_SetDataBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetDataBreakpointsResponse);
   for SetDataBreakpointsResponse'Read use Read_SetDataBreakpointsResponse;
   for SetDataBreakpointsResponse'Write use Write_SetDataBreakpointsResponse;

   package DAP_ExceptionOptions_Vectors is new Ada.Containers.Vectors
     (Positive, ExceptionOptions);
   type DAP_ExceptionOptions_Vector is new DAP_ExceptionOptions_Vectors
     .Vector with
   null record;
   procedure Read_DAP_ExceptionOptions_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_ExceptionOptions_Vector);
   procedure Write_DAP_ExceptionOptions_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_ExceptionOptions_Vector);
   for DAP_ExceptionOptions_Vector'Read use Read_DAP_ExceptionOptions_Vector;
   for DAP_ExceptionOptions_Vector'Write use Write_DAP_ExceptionOptions_Vector;

   package DAP_ExceptionFilterOptions_Vectors is new Ada.Containers.Vectors
     (Positive, ExceptionFilterOptions);
   type DAP_ExceptionFilterOptions_Vector is new DAP_ExceptionFilterOptions_Vectors
     .Vector with
   null record;
   procedure Read_DAP_ExceptionFilterOptions_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_ExceptionFilterOptions_Vector);
   procedure Write_DAP_ExceptionFilterOptions_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_ExceptionFilterOptions_Vector);
   for DAP_ExceptionFilterOptions_Vector'
     Read use Read_DAP_ExceptionFilterOptions_Vector;
   for DAP_ExceptionFilterOptions_Vector'
     Write use Write_DAP_ExceptionFilterOptions_Vector;

   type SetExceptionBreakpointsArguments is record
      exceptionOptions : DAP_ExceptionOptions_Vector;
      filterOptions    : DAP_ExceptionFilterOptions_Vector;
      filters          : DAP_String_Vector;
   end record;

   procedure Read_SetExceptionBreakpointsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetExceptionBreakpointsArguments);
   procedure Write_SetExceptionBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetExceptionBreakpointsArguments);
   for SetExceptionBreakpointsArguments'
     Read use Read_SetExceptionBreakpointsArguments;
   for SetExceptionBreakpointsArguments'
     Write use Write_SetExceptionBreakpointsArguments;

   type SetExceptionBreakpointsRequest is record
      arguments : SetExceptionBreakpointsArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_SetExceptionBreakpointsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetExceptionBreakpointsRequest);
   procedure Write_SetExceptionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetExceptionBreakpointsRequest);
   for SetExceptionBreakpointsRequest'
     Read use Read_SetExceptionBreakpointsRequest;
   for SetExceptionBreakpointsRequest'
     Write use Write_SetExceptionBreakpointsRequest;

   type SetExceptionBreakpointsResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_SetExceptionBreakpointsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetExceptionBreakpointsResponse);
   procedure Write_SetExceptionBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetExceptionBreakpointsResponse);
   for SetExceptionBreakpointsResponse'
     Read use Read_SetExceptionBreakpointsResponse;
   for SetExceptionBreakpointsResponse'
     Write use Write_SetExceptionBreakpointsResponse;

   type SetExpressionArguments is record
      expression : Virtual_String;
      format     : ValueFormat;
      frameId    : LSP_Number;
      value      : Virtual_String;
   end record;

   procedure Read_SetExpressionArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetExpressionArguments);
   procedure Write_SetExpressionArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetExpressionArguments);
   for SetExpressionArguments'Read use Read_SetExpressionArguments;
   for SetExpressionArguments'Write use Write_SetExpressionArguments;

   type SetExpressionRequest is record
      arguments : SetExpressionArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_SetExpressionRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetExpressionRequest);
   procedure Write_SetExpressionRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SetExpressionRequest);
   for SetExpressionRequest'Read use Read_SetExpressionRequest;
   for SetExpressionRequest'Write use Write_SetExpressionRequest;

   type SetExpressionResponse is record
      a_message               : Virtual_String;
      a_type                  : Virtual_String;
      command                 : Virtual_String;
      request_seq             : LSP_Number;
      seq                     : LSP_Number;
      success                 : Boolean;
      body_indexedVariables   : LSP_Number;
      body_namedVariables     : LSP_Number;
      body_presentationHint   : VariablePresentationHint;
      body_type               : Virtual_String;
      body_value              : Virtual_String;
      body_variablesReference : LSP_Number;
   end record;

   procedure Read_SetExpressionResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetExpressionResponse);
   procedure Write_SetExpressionResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetExpressionResponse);
   for SetExpressionResponse'Read use Read_SetExpressionResponse;
   for SetExpressionResponse'Write use Write_SetExpressionResponse;

   package DAP_FunctionBreakpoint_Vectors is new Ada.Containers.Vectors
     (Positive, FunctionBreakpoint);
   type DAP_FunctionBreakpoint_Vector is new DAP_FunctionBreakpoint_Vectors
     .Vector with
   null record;
   procedure Read_DAP_FunctionBreakpoint_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_FunctionBreakpoint_Vector);
   procedure Write_DAP_FunctionBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_FunctionBreakpoint_Vector);
   for DAP_FunctionBreakpoint_Vector'
     Read use Read_DAP_FunctionBreakpoint_Vector;
   for DAP_FunctionBreakpoint_Vector'
     Write use Write_DAP_FunctionBreakpoint_Vector;

   type SetFunctionBreakpointsArguments is record
      breakpoints : DAP_FunctionBreakpoint_Vector;
   end record;

   procedure Read_SetFunctionBreakpointsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetFunctionBreakpointsArguments);
   procedure Write_SetFunctionBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetFunctionBreakpointsArguments);
   for SetFunctionBreakpointsArguments'
     Read use Read_SetFunctionBreakpointsArguments;
   for SetFunctionBreakpointsArguments'
     Write use Write_SetFunctionBreakpointsArguments;

   type SetFunctionBreakpointsRequest is record
      arguments : SetFunctionBreakpointsArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_SetFunctionBreakpointsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetFunctionBreakpointsRequest);
   procedure Write_SetFunctionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetFunctionBreakpointsRequest);
   for SetFunctionBreakpointsRequest'
     Read use Read_SetFunctionBreakpointsRequest;
   for SetFunctionBreakpointsRequest'
     Write use Write_SetFunctionBreakpointsRequest;

   type SetFunctionBreakpointsResponse is record
      a_message        : Virtual_String;
      a_type           : Virtual_String;
      command          : Virtual_String;
      request_seq      : LSP_Number;
      seq              : LSP_Number;
      success          : Boolean;
      body_breakpoints : DAP_Breakpoint_Vector;
   end record;

   procedure Read_SetFunctionBreakpointsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetFunctionBreakpointsResponse);
   procedure Write_SetFunctionBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetFunctionBreakpointsResponse);
   for SetFunctionBreakpointsResponse'
     Read use Read_SetFunctionBreakpointsResponse;
   for SetFunctionBreakpointsResponse'
     Write use Write_SetFunctionBreakpointsResponse;

   package DAP_InstructionBreakpoint_Vectors is new Ada.Containers.Vectors
     (Positive, InstructionBreakpoint);
   type DAP_InstructionBreakpoint_Vector is new DAP_InstructionBreakpoint_Vectors
     .Vector with
   null record;
   procedure Read_DAP_InstructionBreakpoint_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_InstructionBreakpoint_Vector);
   procedure Write_DAP_InstructionBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_InstructionBreakpoint_Vector);
   for DAP_InstructionBreakpoint_Vector'
     Read use Read_DAP_InstructionBreakpoint_Vector;
   for DAP_InstructionBreakpoint_Vector'
     Write use Write_DAP_InstructionBreakpoint_Vector;

   type SetInstructionBreakpointsArguments is record
      breakpoints : DAP_InstructionBreakpoint_Vector;
   end record;

   procedure Read_SetInstructionBreakpointsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetInstructionBreakpointsArguments);
   procedure Write_SetInstructionBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetInstructionBreakpointsArguments);
   for SetInstructionBreakpointsArguments'
     Read use Read_SetInstructionBreakpointsArguments;
   for SetInstructionBreakpointsArguments'
     Write use Write_SetInstructionBreakpointsArguments;

   type SetInstructionBreakpointsRequest is record
      arguments : SetInstructionBreakpointsArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_SetInstructionBreakpointsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetInstructionBreakpointsRequest);
   procedure Write_SetInstructionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetInstructionBreakpointsRequest);
   for SetInstructionBreakpointsRequest'
     Read use Read_SetInstructionBreakpointsRequest;
   for SetInstructionBreakpointsRequest'
     Write use Write_SetInstructionBreakpointsRequest;

   type SetInstructionBreakpointsResponse is record
      a_message        : Virtual_String;
      a_type           : Virtual_String;
      command          : Virtual_String;
      request_seq      : LSP_Number;
      seq              : LSP_Number;
      success          : Boolean;
      body_breakpoints : DAP_Breakpoint_Vector;
   end record;

   procedure Read_SetInstructionBreakpointsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetInstructionBreakpointsResponse);
   procedure Write_SetInstructionBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetInstructionBreakpointsResponse);
   for SetInstructionBreakpointsResponse'
     Read use Read_SetInstructionBreakpointsResponse;
   for SetInstructionBreakpointsResponse'
     Write use Write_SetInstructionBreakpointsResponse;

   type SetVariableArguments is record
      format             : ValueFormat;
      name               : Virtual_String;
      value              : Virtual_String;
      variablesReference : LSP_Number;
   end record;

   procedure Read_SetVariableArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetVariableArguments);
   procedure Write_SetVariableArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SetVariableArguments);
   for SetVariableArguments'Read use Read_SetVariableArguments;
   for SetVariableArguments'Write use Write_SetVariableArguments;

   type SetVariableRequest is record
      arguments : SetVariableArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_SetVariableRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetVariableRequest);
   procedure Write_SetVariableRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SetVariableRequest);
   for SetVariableRequest'Read use Read_SetVariableRequest;
   for SetVariableRequest'Write use Write_SetVariableRequest;

   type SetVariableResponse is record
      a_message               : Virtual_String;
      a_type                  : Virtual_String;
      command                 : Virtual_String;
      request_seq             : LSP_Number;
      seq                     : LSP_Number;
      success                 : Boolean;
      body_indexedVariables   : LSP_Number;
      body_namedVariables     : LSP_Number;
      body_type               : Virtual_String;
      body_value              : Virtual_String;
      body_variablesReference : LSP_Number;
   end record;

   procedure Read_SetVariableResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetVariableResponse);
   procedure Write_SetVariableResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SetVariableResponse);
   for SetVariableResponse'Read use Read_SetVariableResponse;
   for SetVariableResponse'Write use Write_SetVariableResponse;

   type SourceArguments is record
      a_source        : Source;
      sourceReference : LSP_Number;
   end record;

   procedure Read_SourceArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out SourceArguments);
   procedure Write_SourceArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SourceArguments);
   for SourceArguments'Read use Read_SourceArguments;
   for SourceArguments'Write use Write_SourceArguments;

   type SourceRequest is record
      arguments : SourceArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_SourceRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out SourceRequest);
   procedure Write_SourceRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SourceRequest);
   for SourceRequest'Read use Read_SourceRequest;
   for SourceRequest'Write use Write_SourceRequest;

   type SourceResponse is record
      a_message     : Virtual_String;
      a_type        : Virtual_String;
      command       : Virtual_String;
      request_seq   : LSP_Number;
      seq           : LSP_Number;
      success       : Boolean;
      body_content  : Virtual_String;
      body_mimeType : Virtual_String;
   end record;

   procedure Read_SourceResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out SourceResponse);
   procedure Write_SourceResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SourceResponse);
   for SourceResponse'Read use Read_SourceResponse;
   for SourceResponse'Write use Write_SourceResponse;

   type StackFrame is record
      canRestart                  : Boolean;
      column                      : LSP_Number;
      endColumn                   : LSP_Number;
      endLine                     : LSP_Number;
      id                          : LSP_Number;
      instructionPointerReference : Virtual_String;
      line                        : LSP_Number;
      moduleId                    : LSP_Number_Or_String;
      name                        : Virtual_String;
      presentationHint            : Virtual_String;
      a_source                    : Source;
   end record;

   procedure Read_StackFrame
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StackFrame);
   procedure Write_StackFrame
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StackFrame);
   for StackFrame'Read use Read_StackFrame;
   for StackFrame'Write use Write_StackFrame;

   type StackFrameFormat is record
      includeAll      : Boolean;
      line            : Boolean;
      module          : Boolean;
      parameterNames  : Boolean;
      parameterTypes  : Boolean;
      parameterValues : Boolean;
      parameters      : Boolean;
      hex             : Boolean;
   end record;

   procedure Read_StackFrameFormat
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StackFrameFormat);
   procedure Write_StackFrameFormat
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StackFrameFormat);
   for StackFrameFormat'Read use Read_StackFrameFormat;
   for StackFrameFormat'Write use Write_StackFrameFormat;

   type StackTraceRequest is record
      arguments : StackTraceArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_StackTraceRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out StackTraceRequest);
   procedure Write_StackTraceRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StackTraceRequest);
   for StackTraceRequest'Read use Read_StackTraceRequest;
   for StackTraceRequest'Write use Write_StackTraceRequest;

   package DAP_StackFrame_Vectors is new Ada.Containers.Vectors
     (Positive, StackFrame);
   type DAP_StackFrame_Vector is new DAP_StackFrame_Vectors.Vector with
   null record;
   procedure Read_DAP_StackFrame_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_StackFrame_Vector);
   procedure Write_DAP_StackFrame_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_StackFrame_Vector);
   for DAP_StackFrame_Vector'Read use Read_DAP_StackFrame_Vector;
   for DAP_StackFrame_Vector'Write use Write_DAP_StackFrame_Vector;

   type StackTraceResponse is record
      a_message        : Virtual_String;
      a_type           : Virtual_String;
      command          : Virtual_String;
      request_seq      : LSP_Number;
      seq              : LSP_Number;
      success          : Boolean;
      body_stackFrames : DAP_StackFrame_Vector;
      body_totalFrames : LSP_Number;
   end record;

   procedure Read_StackTraceResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out StackTraceResponse);
   procedure Write_StackTraceResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StackTraceResponse);
   for StackTraceResponse'Read use Read_StackTraceResponse;
   for StackTraceResponse'Write use Write_StackTraceResponse;

   type StepBackArguments is record
      granularity : Enums.SteppingGranularity;
      threadId    : LSP_Number;
   end record;

   procedure Read_StepBackArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out StepBackArguments);
   procedure Write_StepBackArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepBackArguments);
   for StepBackArguments'Read use Read_StepBackArguments;
   for StepBackArguments'Write use Write_StepBackArguments;

   type StepBackRequest is record
      arguments : StepBackArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_StepBackRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepBackRequest);
   procedure Write_StepBackRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepBackRequest);
   for StepBackRequest'Read use Read_StepBackRequest;
   for StepBackRequest'Write use Write_StepBackRequest;

   type StepBackResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_StepBackResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepBackResponse);
   procedure Write_StepBackResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepBackResponse);
   for StepBackResponse'Read use Read_StepBackResponse;
   for StepBackResponse'Write use Write_StepBackResponse;

   type StepInArguments is record
      granularity : Enums.SteppingGranularity;
      targetId    : LSP_Number;
      threadId    : LSP_Number;
   end record;

   procedure Read_StepInArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepInArguments);
   procedure Write_StepInArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepInArguments);
   for StepInArguments'Read use Read_StepInArguments;
   for StepInArguments'Write use Write_StepInArguments;

   type StepInRequest is record
      arguments : StepInArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_StepInRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepInRequest);
   procedure Write_StepInRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepInRequest);
   for StepInRequest'Read use Read_StepInRequest;
   for StepInRequest'Write use Write_StepInRequest;

   type StepInResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_StepInResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepInResponse);
   procedure Write_StepInResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepInResponse);
   for StepInResponse'Read use Read_StepInResponse;
   for StepInResponse'Write use Write_StepInResponse;

   type StepInTargetsRequest is record
      arguments : StepInTargetsArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_StepInTargetsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out StepInTargetsRequest);
   procedure Write_StepInTargetsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepInTargetsRequest);
   for StepInTargetsRequest'Read use Read_StepInTargetsRequest;
   for StepInTargetsRequest'Write use Write_StepInTargetsRequest;

   package DAP_StepInTarget_Vectors is new Ada.Containers.Vectors
     (Positive, StepInTarget);
   type DAP_StepInTarget_Vector is new DAP_StepInTarget_Vectors.Vector with
   null record;
   procedure Read_DAP_StepInTarget_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_StepInTarget_Vector);
   procedure Write_DAP_StepInTarget_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_StepInTarget_Vector);
   for DAP_StepInTarget_Vector'Read use Read_DAP_StepInTarget_Vector;
   for DAP_StepInTarget_Vector'Write use Write_DAP_StepInTarget_Vector;

   type StepInTargetsResponse is record
      a_message    : Virtual_String;
      a_type       : Virtual_String;
      command      : Virtual_String;
      request_seq  : LSP_Number;
      seq          : LSP_Number;
      success      : Boolean;
      body_targets : DAP_StepInTarget_Vector;
   end record;

   procedure Read_StepInTargetsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out StepInTargetsResponse);
   procedure Write_StepInTargetsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : StepInTargetsResponse);
   for StepInTargetsResponse'Read use Read_StepInTargetsResponse;
   for StepInTargetsResponse'Write use Write_StepInTargetsResponse;

   type StepOutArguments is record
      granularity : Enums.SteppingGranularity;
      threadId    : LSP_Number;
   end record;

   procedure Read_StepOutArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepOutArguments);
   procedure Write_StepOutArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepOutArguments);
   for StepOutArguments'Read use Read_StepOutArguments;
   for StepOutArguments'Write use Write_StepOutArguments;

   type StepOutRequest is record
      arguments : StepOutArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_StepOutRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepOutRequest);
   procedure Write_StepOutRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepOutRequest);
   for StepOutRequest'Read use Read_StepOutRequest;
   for StepOutRequest'Write use Write_StepOutRequest;

   type StepOutResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_StepOutResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepOutResponse);
   procedure Write_StepOutResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepOutResponse);
   for StepOutResponse'Read use Read_StepOutResponse;
   for StepOutResponse'Write use Write_StepOutResponse;

   type StoppedEvent is record
      event                  : Virtual_String;
      a_type                 : Virtual_String;
      seq                    : LSP_Number;
      body_allThreadsStopped : Boolean;
      body_description       : Virtual_String;
      body_hitBreakpointIds  : DAP_Integer_Vector;
      body_preserveFocusHint : Boolean;
      body_reason            : Virtual_String;
      body_text              : Virtual_String;
      body_threadId          : LSP_Number;
   end record;

   procedure Read_StoppedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StoppedEvent);
   procedure Write_StoppedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StoppedEvent);
   for StoppedEvent'Read use Read_StoppedEvent;
   for StoppedEvent'Write use Write_StoppedEvent;

   type TerminateRequest is record
      arguments : TerminateArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_TerminateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out TerminateRequest);
   procedure Write_TerminateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : TerminateRequest);
   for TerminateRequest'Read use Read_TerminateRequest;
   for TerminateRequest'Write use Write_TerminateRequest;

   type TerminateResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_TerminateResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out TerminateResponse);
   procedure Write_TerminateResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : TerminateResponse);
   for TerminateResponse'Read use Read_TerminateResponse;
   for TerminateResponse'Write use Write_TerminateResponse;

   type TerminateThreadsArguments is record
      threadIds : DAP_Integer_Vector;
   end record;

   procedure Read_TerminateThreadsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out TerminateThreadsArguments);
   procedure Write_TerminateThreadsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TerminateThreadsArguments);
   for TerminateThreadsArguments'Read use Read_TerminateThreadsArguments;
   for TerminateThreadsArguments'Write use Write_TerminateThreadsArguments;

   type TerminateThreadsRequest is record
      arguments : TerminateThreadsArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_TerminateThreadsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out TerminateThreadsRequest);
   procedure Write_TerminateThreadsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TerminateThreadsRequest);
   for TerminateThreadsRequest'Read use Read_TerminateThreadsRequest;
   for TerminateThreadsRequest'Write use Write_TerminateThreadsRequest;

   type TerminateThreadsResponse is record
      a_message   : Virtual_String;
      a_type      : Virtual_String;
      command     : Virtual_String;
      request_seq : LSP_Number;
      seq         : LSP_Number;
      success     : Boolean;
   end record;
   procedure Read_TerminateThreadsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out TerminateThreadsResponse);
   procedure Write_TerminateThreadsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TerminateThreadsResponse);
   for TerminateThreadsResponse'Read use Read_TerminateThreadsResponse;
   for TerminateThreadsResponse'Write use Write_TerminateThreadsResponse;

   type TerminatedEvent is record
      event        : Virtual_String;
      a_type       : Virtual_String;
      seq          : LSP_Number;
      body_restart : LSP_Any;
   end record;

   procedure Read_TerminatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out TerminatedEvent);
   procedure Write_TerminatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : TerminatedEvent);
   for TerminatedEvent'Read use Read_TerminatedEvent;
   for TerminatedEvent'Write use Write_TerminatedEvent;

   type ThreadEvent is record
      event         : Virtual_String;
      a_type        : Virtual_String;
      seq           : LSP_Number;
      body_reason   : Virtual_String;
      body_threadId : LSP_Number;
   end record;

   procedure Read_ThreadEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ThreadEvent);
   procedure Write_ThreadEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ThreadEvent);
   for ThreadEvent'Read use Read_ThreadEvent;
   for ThreadEvent'Write use Write_ThreadEvent;

   type ThreadsRequest is record
      command   : Virtual_String;
      a_type    : Virtual_String;
      arguments : LSP_Any;
      seq       : LSP_Number;
   end record;

   procedure Read_ThreadsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ThreadsRequest);
   procedure Write_ThreadsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ThreadsRequest);
   for ThreadsRequest'Read use Read_ThreadsRequest;
   for ThreadsRequest'Write use Write_ThreadsRequest;

   package DAP_Thread_Vectors is new Ada.Containers.Vectors (Positive, Thread);
   type DAP_Thread_Vector is new DAP_Thread_Vectors.Vector with null record;
   procedure Read_DAP_Thread_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_Thread_Vector);
   procedure Write_DAP_Thread_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_Thread_Vector);
   for DAP_Thread_Vector'Read use Read_DAP_Thread_Vector;
   for DAP_Thread_Vector'Write use Write_DAP_Thread_Vector;

   type ThreadsResponse is record
      a_message    : Virtual_String;
      a_type       : Virtual_String;
      command      : Virtual_String;
      request_seq  : LSP_Number;
      seq          : LSP_Number;
      success      : Boolean;
      body_threads : DAP_Thread_Vector;
   end record;

   procedure Read_ThreadsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ThreadsResponse);
   procedure Write_ThreadsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ThreadsResponse);
   for ThreadsResponse'Read use Read_ThreadsResponse;
   for ThreadsResponse'Write use Write_ThreadsResponse;

   type Variable is record
      evaluateName       : Virtual_String;
      indexedVariables   : LSP_Number;
      memoryReference    : Virtual_String;
      name               : Virtual_String;
      namedVariables     : LSP_Number;
      presentationHint   : VariablePresentationHint;
      a_type             : Virtual_String;
      value              : Virtual_String;
      variablesReference : LSP_Number;
   end record;

   procedure Read_Variable
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Variable);
   procedure Write_Variable
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Variable);
   for Variable'Read use Read_Variable;
   for Variable'Write use Write_Variable;

   type VariablesArguments is record
      count              : LSP_Number;
      filter             : Virtual_String;
      format             : ValueFormat;
      start              : LSP_Number;
      variablesReference : LSP_Number;
   end record;

   procedure Read_VariablesArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out VariablesArguments);
   procedure Write_VariablesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : VariablesArguments);
   for VariablesArguments'Read use Read_VariablesArguments;
   for VariablesArguments'Write use Write_VariablesArguments;

   type VariablesRequest is record
      arguments : VariablesArguments;
      command   : Virtual_String;
      a_type    : Virtual_String;
      seq       : LSP_Number;
   end record;

   procedure Read_VariablesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out VariablesRequest);
   procedure Write_VariablesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : VariablesRequest);
   for VariablesRequest'Read use Read_VariablesRequest;
   for VariablesRequest'Write use Write_VariablesRequest;

   package DAP_Variable_Vectors is new Ada.Containers.Vectors
     (Positive, Variable);
   type DAP_Variable_Vector is new DAP_Variable_Vectors.Vector with
   null record;
   procedure Read_DAP_Variable_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_Variable_Vector);
   procedure Write_DAP_Variable_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_Variable_Vector);
   for DAP_Variable_Vector'Read use Read_DAP_Variable_Vector;
   for DAP_Variable_Vector'Write use Write_DAP_Variable_Vector;

   type VariablesResponse is record
      a_message      : Virtual_String;
      a_type         : Virtual_String;
      command        : Virtual_String;
      request_seq    : LSP_Number;
      seq            : LSP_Number;
      success        : Boolean;
      body_variables : DAP_Variable_Vector;
   end record;

   procedure Read_VariablesResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out VariablesResponse);
   procedure Write_VariablesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : VariablesResponse);
   for VariablesResponse'Read use Read_VariablesResponse;
   for VariablesResponse'Write use Write_VariablesResponse;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ExceptionDetails, Name => Access_ExceptionDetails);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Source, Name => Access_Source);

end DAP.Tools;
