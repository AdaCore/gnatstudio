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
with Interfaces;
with LSP.JSON_Streams;
with VSS.Strings.Conversions;

package body DAP.Tools is

   overriding procedure Finalize (Self : in out Capabilities) is
   begin
      for E of Self.additionalModuleColumns loop
         Free (E);
      end loop;
      for E of Self.exceptionBreakpointFilters loop
         Free (E);
      end loop;
      for E of Self.supportedChecksumAlgorithms loop
         Free (E);
      end loop;
   end Finalize;

   overriding procedure Finalize (Self : in out ExceptionDetails) is
   begin
      for E of Self.innerException loop
         Free (E);
      end loop;
   end Finalize;

   overriding procedure Finalize (Self : in out ExceptionOptions) is
   begin
      for E of Self.path loop
         Free (E);
      end loop;
   end Finalize;

   overriding procedure Finalize (Self : in out ExceptionPathSegment) is
   begin
      null;
   end Finalize;

   overriding procedure Finalize (Self : in out ModulesViewDescriptor) is
   begin
      for E of Self.columns loop
         Free (E);
      end loop;
   end Finalize;

   overriding procedure Finalize (Self : in out RunInTerminalRequestArguments)
   is
   begin
      null;
   end Finalize;

   overriding procedure Finalize (Self : in out SetBreakpointsArguments) is
   begin
      for E of Self.breakpoints loop
         Free (E);
      end loop;
   end Finalize;

   overriding procedure Finalize (Self : in out SetDataBreakpointsArguments) is
   begin
      for E of Self.breakpoints loop
         Free (E);
      end loop;
   end Finalize;

   overriding procedure Finalize
     (Self : in out SetExceptionBreakpointsArguments)
   is
   begin
      for E of Self.exceptionOptions loop
         Free (E);
      end loop;
      for E of Self.filterOptions loop
         Free (E);
      end loop;
   end Finalize;

   overriding procedure Finalize
     (Self : in out SetFunctionBreakpointsArguments)
   is
   begin
      for E of Self.breakpoints loop
         Free (E);
      end loop;
   end Finalize;

   overriding procedure Finalize
     (Self : in out SetInstructionBreakpointsArguments)
   is
   begin
      for E of Self.breakpoints loop
         Free (E);
      end loop;
   end Finalize;

   overriding procedure Finalize (Self : in out Source) is
   begin
      for E of Self.checksums loop
         Free (E);
      end loop;
      for E of Self.sources loop
         Free (E);
      end loop;
   end Finalize;

   overriding procedure Finalize (Self : in out TerminateThreadsArguments) is
   begin
      null;
   end Finalize;

   overriding procedure Finalize (Self : in out VariablePresentationHint) is
   begin
      null;
   end Finalize;

   procedure Write_ChecksumAlgorithm
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ChecksumAlgorithm)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_String
        (Value : Enums.ChecksumAlgorithm) return Virtual_String;

      function To_String
        (Value : Enums.ChecksumAlgorithm) return Virtual_String
      is
      begin
         case Value is
            when Enums.MD5 =>
               return "MD5";
            when Enums.SHA1 =>
               return "SHA1";
            when Enums.SHA256 =>
               return "SHA256";
            when Enums.timestamp =>
               return "timestamp";
         end case;
      end To_String;
   begin
      if V /= null then
         JS.Write_String (To_String (V.all));
      end if;
   end Write_ChecksumAlgorithm;

   procedure Write_CompletionItemType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionItemType)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_String
        (Value : Enums.CompletionItemType) return Virtual_String;

      function To_String
        (Value : Enums.CompletionItemType) return Virtual_String
      is
      begin
         case Value is
            when Enums.class =>
               return "class";
            when Enums.color =>
               return "color";
            when Enums.constructor =>
               return "constructor";
            when Enums.customcolor =>
               return "customcolor";
            when Enums.enum =>
               return "enum";
            when Enums.field =>
               return "field";
            when Enums.file =>
               return "file";
            when Enums.a_function =>
               return "function";
            when Enums.a_interface =>
               return "interface";
            when Enums.keyword =>
               return "keyword";
            when Enums.method =>
               return "method";
            when Enums.module =>
               return "module";
            when Enums.property =>
               return "property";
            when Enums.reference =>
               return "reference";
            when Enums.snippet =>
               return "snippet";
            when Enums.text =>
               return "text";
            when Enums.unit =>
               return "unit";
            when Enums.value =>
               return "value";
            when Enums.variable =>
               return "variable";
         end case;
      end To_String;
   begin
      if V /= null then
         JS.Write_String (To_String (V.all));
      end if;
   end Write_CompletionItemType;

   procedure Write_DataBreakpointAccessType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointAccessType)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_String
        (Value : Enums.DataBreakpointAccessType) return Virtual_String;

      function To_String
        (Value : Enums.DataBreakpointAccessType) return Virtual_String
      is
      begin
         case Value is
            when Enums.read =>
               return "read";
            when Enums.readWrite =>
               return "readWrite";
            when Enums.write =>
               return "write";
         end case;
      end To_String;
   begin
      if V /= null then
         JS.Write_String (To_String (V.all));
      end if;
   end Write_DataBreakpointAccessType;

   procedure Write_ExceptionBreakMode
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionBreakMode)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_String
        (Value : Enums.ExceptionBreakMode) return Virtual_String;

      function To_String
        (Value : Enums.ExceptionBreakMode) return Virtual_String
      is
      begin
         case Value is
            when Enums.always =>
               return "always";
            when Enums.never =>
               return "never";
            when Enums.unhandled =>
               return "unhandled";
            when Enums.userUnhandled =>
               return "userUnhandled";
         end case;
      end To_String;
   begin
      if V /= null then
         JS.Write_String (To_String (V.all));
      end if;
   end Write_ExceptionBreakMode;

   procedure Write_InvalidatedAreas
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InvalidatedAreas)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_String
        (Value : Enums.InvalidatedAreas) return Virtual_String;

      function To_String (Value : Enums.InvalidatedAreas) return Virtual_String
      is
      begin
         case Value is
            when Enums.a_all =>
               return "all";
            when Enums.stacks =>
               return "stacks";
            when Enums.threads =>
               return "threads";
            when Enums.variables =>
               return "variables";
         end case;
      end To_String;
   begin
      if V /= null then
         JS.Write_String (To_String (V.all));
      end if;
   end Write_InvalidatedAreas;

   procedure Write_SteppingGranularity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SteppingGranularity)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_String
        (Value : Enums.SteppingGranularity) return Virtual_String;

      function To_String
        (Value : Enums.SteppingGranularity) return Virtual_String
      is
      begin
         case Value is
            when Enums.instruction =>
               return "instruction";
            when Enums.line =>
               return "line";
            when Enums.statement =>
               return "statement";
         end case;
      end To_String;
   begin
      if V /= null then
         JS.Write_String (To_String (V.all));
      end if;
   end Write_SteppingGranularity;

   procedure Write_DAP_String_Map
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_DAP_String_Map)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      declare
         procedure WMap_Item (C : DAP_String_Maps.Cursor);
         procedure WMap_Item (C : DAP_String_Maps.Cursor) is
         begin
            LSP.JSON_Streams.Key (JS'Access, (DAP_String_Maps.Key (C)));
            JS.Write_String (Virtual_String'(DAP_String_Maps.Element (C)));
         end WMap_Item;
      begin
         V.Iterate (WMap_Item'Access);
      end;
      JS.End_Object;
   end Write_DAP_String_Map;

   procedure Write_DAP_String_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_String_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         JS.Write_String (V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_String_Vector;

   procedure Write_DAP_Integer_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_Integer_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         JS.Write_Integer (Interfaces.Integer_64 (V.Element (J)));
      end loop;
      JS.End_Array;
   end Write_DAP_Integer_Vector;

   procedure Write_DAP_ChecksumAlgorithm_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ChecksumAlgorithm_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Write_ChecksumAlgorithm (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_ChecksumAlgorithm_Vector;

   procedure Write_DAP_Checksum_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_Checksum_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Write_Checksum (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_Checksum_Vector;

   procedure Write_DAP_ColumnDescriptor_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ColumnDescriptor_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Write_ColumnDescriptor (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_ColumnDescriptor_Vector;

   procedure Write_DAP_DataBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_DataBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Write_DataBreakpoint (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_DataBreakpoint_Vector;

   procedure Write_DAP_ExceptionBreakpointsFilter_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionBreakpointsFilter_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Write_ExceptionBreakpointsFilter (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_ExceptionBreakpointsFilter_Vector;

   procedure Write_DAP_ExceptionDetails_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionDetails_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Write_ExceptionDetails (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_ExceptionDetails_Vector;

   procedure Write_DAP_ExceptionFilterOptions_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionFilterOptions_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Write_ExceptionFilterOptions (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_ExceptionFilterOptions_Vector;

   procedure Write_DAP_ExceptionOptions_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionOptions_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Write_ExceptionOptions (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_ExceptionOptions_Vector;

   procedure Write_DAP_ExceptionPathSegment_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionPathSegment_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Write_ExceptionPathSegment (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_ExceptionPathSegment_Vector;

   procedure Write_DAP_FunctionBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_FunctionBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Write_FunctionBreakpoint (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_FunctionBreakpoint_Vector;

   procedure Write_DAP_InstructionBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_InstructionBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Write_InstructionBreakpoint (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_InstructionBreakpoint_Vector;

   procedure Write_DAP_SourceBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_SourceBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Write_SourceBreakpoint (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_SourceBreakpoint_Vector;

   procedure Write_DAP_Source_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_Source_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Write_Source (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_Source_Vector;

   procedure Write_AttachRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_AttachRequestArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("__restart");
      Write_Any (S, V.a_restart);

      JS.End_Object;
   end Write_AttachRequestArguments;

   procedure Write_BreakpointLocation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("column");
      JS.Write_Integer (Interfaces.Integer_64 (V.column));

      JS.Key ("endColumn");
      JS.Write_Integer (Interfaces.Integer_64 (V.endColumn));

      JS.Key ("endLine");
      JS.Write_Integer (Interfaces.Integer_64 (V.endLine));

      JS.Key ("line");
      JS.Write_Integer (Interfaces.Integer_64 (V.line));

      JS.End_Object;
   end Write_BreakpointLocation;

   procedure Write_CancelArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CancelArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("progressId");
      JS.Write_String (V.progressId);

      JS.Key ("requestId");
      JS.Write_Integer (Interfaces.Integer_64 (V.requestId));

      JS.End_Object;
   end Write_CancelArguments;

   procedure Write_ColumnDescriptor
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ColumnDescriptor)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("attributeName");
      JS.Write_String (V.attributeName);

      JS.Key ("format");
      JS.Write_String (V.format);

      JS.Key ("label");
      JS.Write_String (V.label);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("width");
      JS.Write_Integer (Interfaces.Integer_64 (V.width));

      JS.End_Object;
   end Write_ColumnDescriptor;

   procedure Write_CompletionsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("column");
      JS.Write_Integer (Interfaces.Integer_64 (V.column));

      JS.Key ("frameId");
      JS.Write_Integer (Interfaces.Integer_64 (V.frameId));

      JS.Key ("line");
      JS.Write_Integer (Interfaces.Integer_64 (V.line));

      JS.Key ("text");
      JS.Write_String (V.text);

      JS.End_Object;
   end Write_CompletionsArguments;

   procedure Write_ConfigurationDoneArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ConfigurationDoneArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      --  this record is empty
      pragma Unreferenced (V);
      JS.Start_Object;
      JS.End_Object;
   end Write_ConfigurationDoneArguments;

   procedure Write_ContinueArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ContinueArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.threadId));

      JS.End_Object;
   end Write_ContinueArguments;

   procedure Write_DataBreakpointInfoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointInfoArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("name");
      JS.Write_String (V.name);

      JS.Key ("variablesReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.variablesReference));

      JS.End_Object;
   end Write_DataBreakpointInfoArguments;

   procedure Write_DisassembleArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembleArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("instructionCount");
      JS.Write_Integer (Interfaces.Integer_64 (V.instructionCount));

      JS.Key ("instructionOffset");
      JS.Write_Integer (Interfaces.Integer_64 (V.instructionOffset));

      JS.Key ("memoryReference");
      JS.Write_String (V.memoryReference);

      JS.Key ("offset");
      JS.Write_Integer (Interfaces.Integer_64 (V.offset));

      JS.Key ("resolveSymbols");
      JS.Write_Boolean (V.resolveSymbols);

      JS.End_Object;
   end Write_DisassembleArguments;

   procedure Write_DisconnectArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisconnectArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("restart");
      JS.Write_Boolean (V.restart);

      JS.Key ("terminateDebuggee");
      JS.Write_Boolean (V.terminateDebuggee);

      JS.End_Object;
   end Write_DisconnectArguments;

   procedure Write_ExceptionBreakpointsFilter
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionBreakpointsFilter)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("conditionDescription");
      JS.Write_String (V.conditionDescription);

      JS.Key ("default");
      JS.Write_Boolean (V.default);

      JS.Key ("filter");
      JS.Write_String (V.filter);

      JS.Key ("label");
      JS.Write_String (V.label);

      JS.Key ("supportsCondition");
      JS.Write_Boolean (V.supportsCondition);

      JS.End_Object;
   end Write_ExceptionBreakpointsFilter;

   procedure Write_ExceptionFilterOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionFilterOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("condition");
      JS.Write_String (V.condition);

      JS.Key ("filterId");
      JS.Write_String (V.filterId);

      JS.End_Object;
   end Write_ExceptionFilterOptions;

   procedure Write_ExceptionInfoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionInfoArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.threadId));

      JS.End_Object;
   end Write_ExceptionInfoArguments;

   procedure Write_FunctionBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_FunctionBreakpoint)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("condition");
      JS.Write_String (V.condition);

      JS.Key ("hitCondition");
      JS.Write_String (V.hitCondition);

      JS.Key ("name");
      JS.Write_String (V.name);

      JS.End_Object;
   end Write_FunctionBreakpoint;

   procedure Write_GotoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("targetId");
      JS.Write_Integer (Interfaces.Integer_64 (V.targetId));

      JS.Key ("threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.threadId));

      JS.End_Object;
   end Write_GotoArguments;

   procedure Write_GotoTarget
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoTarget)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("column");
      JS.Write_Integer (Interfaces.Integer_64 (V.column));

      JS.Key ("endColumn");
      JS.Write_Integer (Interfaces.Integer_64 (V.endColumn));

      JS.Key ("endLine");
      JS.Write_Integer (Interfaces.Integer_64 (V.endLine));

      JS.Key ("id");
      JS.Write_Integer (Interfaces.Integer_64 (V.id));

      JS.Key ("instructionPointerReference");
      JS.Write_String (V.instructionPointerReference);

      JS.Key ("label");
      JS.Write_String (V.label);

      JS.Key ("line");
      JS.Write_Integer (Interfaces.Integer_64 (V.line));

      JS.End_Object;
   end Write_GotoTarget;

   procedure Write_InitializeRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializeRequestArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("adapterID");
      JS.Write_String (V.adapterID);

      JS.Key ("clientID");
      JS.Write_String (V.clientID);

      JS.Key ("clientName");
      JS.Write_String (V.clientName);

      JS.Key ("columnsStartAt1");
      JS.Write_Boolean (V.columnsStartAt1);

      JS.Key ("linesStartAt1");
      JS.Write_Boolean (V.linesStartAt1);

      JS.Key ("locale");
      JS.Write_String (V.locale);

      JS.Key ("pathFormat");
      JS.Write_String (V.pathFormat);

      JS.Key ("supportsInvalidatedEvent");
      JS.Write_Boolean (V.supportsInvalidatedEvent);

      JS.Key ("supportsMemoryReferences");
      JS.Write_Boolean (V.supportsMemoryReferences);

      JS.Key ("supportsProgressReporting");
      JS.Write_Boolean (V.supportsProgressReporting);

      JS.Key ("supportsRunInTerminalRequest");
      JS.Write_Boolean (V.supportsRunInTerminalRequest);

      JS.Key ("supportsVariablePaging");
      JS.Write_Boolean (V.supportsVariablePaging);

      JS.Key ("supportsVariableType");
      JS.Write_Boolean (V.supportsVariableType);

      JS.End_Object;
   end Write_InitializeRequestArguments;

   procedure Write_InstructionBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InstructionBreakpoint)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("condition");
      JS.Write_String (V.condition);

      JS.Key ("hitCondition");
      JS.Write_String (V.hitCondition);

      JS.Key ("instructionReference");
      JS.Write_String (V.instructionReference);

      JS.Key ("offset");
      JS.Write_Integer (Interfaces.Integer_64 (V.offset));

      JS.End_Object;
   end Write_InstructionBreakpoint;

   procedure Write_LaunchRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LaunchRequestArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("__restart");
      Write_Any (S, V.a_restart);

      JS.Key ("noDebug");
      JS.Write_Boolean (V.noDebug);

      JS.Key ("program");
      JS.Write_String (V.program);
      --  cdt-gdb-adapter specific

      JS.End_Object;
   end Write_LaunchRequestArguments;

   procedure Write_LoadedSourcesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourcesArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      --  this record is empty
      pragma Unreferenced (V);
      JS.Start_Object;
      JS.End_Object;
   end Write_LoadedSourcesArguments;

   procedure Write_Module
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Module)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("addressRange");
      JS.Write_String (V.addressRange);

      JS.Key ("dateTimeStamp");
      JS.Write_String (V.dateTimeStamp);

      JS.Key ("id");
      if V.id.Is_Number then
         JS.Write_Integer (Interfaces.Integer_64 (V.id.Number));
      elsif not Is_Empty (V.id.String) then
         JS.Write_String (V.id.String);
      end if;

      JS.Key ("isOptimized");
      JS.Write_Boolean (V.isOptimized);

      JS.Key ("isUserCode");
      JS.Write_Boolean (V.isUserCode);

      JS.Key ("name");
      JS.Write_String (V.name);

      JS.Key ("path");
      JS.Write_String (V.path);

      JS.Key ("symbolFilePath");
      JS.Write_String (V.symbolFilePath);

      JS.Key ("symbolStatus");
      JS.Write_String (V.symbolStatus);

      JS.Key ("version");
      JS.Write_String (V.version);

      JS.End_Object;
   end Write_Module;

   procedure Write_ModulesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ModulesArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("moduleCount");
      JS.Write_Integer (Interfaces.Integer_64 (V.moduleCount));

      JS.Key ("startModule");
      JS.Write_Integer (Interfaces.Integer_64 (V.startModule));

      JS.End_Object;
   end Write_ModulesArguments;

   procedure Write_PauseArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_PauseArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.threadId));

      JS.End_Object;
   end Write_PauseArguments;

   procedure Write_ProtocolMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProtocolMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.End_Object;
   end Write_ProtocolMessage;

   procedure Write_ReadMemoryArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReadMemoryArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("count");
      JS.Write_Integer (Interfaces.Integer_64 (V.count));

      JS.Key ("memoryReference");
      JS.Write_String (V.memoryReference);

      JS.Key ("offset");
      JS.Write_Integer (Interfaces.Integer_64 (V.offset));

      JS.End_Object;
   end Write_ReadMemoryArguments;

   procedure Write_RestartArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      --  this record is empty
      pragma Unreferenced (V);
      JS.Start_Object;
      JS.End_Object;
   end Write_RestartArguments;

   procedure Write_RestartFrameArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartFrameArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("frameId");
      JS.Write_Integer (Interfaces.Integer_64 (V.frameId));

      JS.End_Object;
   end Write_RestartFrameArguments;

   procedure Write_ReverseContinueArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReverseContinueArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.threadId));

      JS.End_Object;
   end Write_ReverseContinueArguments;

   procedure Write_ScopesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ScopesArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("frameId");
      JS.Write_Integer (Interfaces.Integer_64 (V.frameId));

      JS.End_Object;
   end Write_ScopesArguments;

   procedure Write_SourceBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SourceBreakpoint)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("column");
      JS.Write_Integer (Interfaces.Integer_64 (V.column));

      --  JS.Key ("condition");
      --  JS.Write_String (V.condition);
      --  prevent cdt-gdb-adapter from breaking

      --  JS.Key ("hitCondition");
      --  JS.Write_String (V.hitCondition);
      --  prevent cdt-gdb-adapter from breaking

      JS.Key ("line");
      JS.Write_Integer (Interfaces.Integer_64 (V.line));

      --  JS.Key ("logMessage");
      --  JS.Write_String (V.logMessage);
      --  prevent cdt-gdb-adapter from breaking

      JS.End_Object;
   end Write_SourceBreakpoint;

   procedure Write_StepInTarget
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StepInTarget)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("id");
      JS.Write_Integer (Interfaces.Integer_64 (V.id));

      JS.Key ("label");
      JS.Write_String (V.label);

      JS.End_Object;
   end Write_StepInTarget;

   procedure Write_StepInTargetsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInTargetsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("frameId");
      JS.Write_Integer (Interfaces.Integer_64 (V.frameId));

      JS.End_Object;
   end Write_StepInTargetsArguments;

   procedure Write_TerminateArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("restart");
      JS.Write_Boolean (V.restart);

      JS.End_Object;
   end Write_TerminateArguments;

   procedure Write_Thread
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Thread)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("id");
      JS.Write_Integer (Interfaces.Integer_64 (V.id));

      JS.Key ("name");
      JS.Write_String (V.name);

      JS.End_Object;
   end Write_Thread;

   procedure Write_ValueFormat
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ValueFormat)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("hex");
      JS.Write_Boolean (V.hex);

      JS.End_Object;
   end Write_ValueFormat;

   procedure Write_Request
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_Any (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_Request;

   procedure Write_AttachRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_AttachRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_AttachRequestArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_AttachRequest;

   procedure Write_Response
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Response)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_Response;

   procedure Write_AttachResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_AttachResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_AttachResponse;

   procedure Write_Source
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Source)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("adapterData");
      Write_Any (S, V.adapterData);

      JS.Key ("checksums");
      Write_DAP_Checksum_Vector (S, V.checksums'Access);

      JS.Key ("name");
      JS.Write_String (V.name);

      JS.Key ("origin");
      JS.Write_String (V.origin);

      JS.Key ("path");
      JS.Write_String (V.path);

      JS.Key ("presentationHint");
      JS.Write_String (V.presentationHint);

      JS.Key ("sourceReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.sourceReference));

      JS.Key ("sources");
      Write_DAP_Source_Vector (S, V.sources'Access);

      JS.End_Object;
   end Write_Source;

   procedure Write_Breakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Breakpoint)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("column");
      JS.Write_Integer (Interfaces.Integer_64 (V.column));

      JS.Key ("endColumn");
      JS.Write_Integer (Interfaces.Integer_64 (V.endColumn));

      JS.Key ("endLine");
      JS.Write_Integer (Interfaces.Integer_64 (V.endLine));

      JS.Key ("id");
      JS.Write_Integer (Interfaces.Integer_64 (V.id));

      JS.Key ("instructionReference");
      JS.Write_String (V.instructionReference);

      JS.Key ("line");
      JS.Write_Integer (Interfaces.Integer_64 (V.line));

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("offset");
      JS.Write_Integer (Interfaces.Integer_64 (V.offset));

      JS.Key ("source");
      Write_Source (S, V.a_source'Access);

      JS.Key ("verified");
      JS.Write_Boolean (V.verified);

      JS.End_Object;
   end Write_Breakpoint;

   procedure Write_Event
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Event)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_Event;

   procedure Write_BreakpointEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_BreakpointEvent;

   procedure Write_BreakpointLocationsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocationsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("column");
      JS.Write_Integer (Interfaces.Integer_64 (V.column));

      JS.Key ("endColumn");
      JS.Write_Integer (Interfaces.Integer_64 (V.endColumn));

      JS.Key ("endLine");
      JS.Write_Integer (Interfaces.Integer_64 (V.endLine));

      JS.Key ("line");
      JS.Write_Integer (Interfaces.Integer_64 (V.line));

      JS.Key ("source");
      Write_Source (S, V.a_source'Access);

      JS.End_Object;
   end Write_BreakpointLocationsArguments;

   procedure Write_BreakpointLocationsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocationsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_BreakpointLocationsArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_BreakpointLocationsRequest;

   procedure Write_BreakpointLocationsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocationsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_BreakpointLocationsResponse;

   procedure Write_CancelRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_CancelRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_CancelArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_CancelRequest;

   procedure Write_CancelResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_CancelResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_CancelResponse;

   procedure Write_Capabilities
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Capabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("additionalModuleColumns");
      Write_DAP_ColumnDescriptor_Vector (S, V.additionalModuleColumns'Access);

      JS.Key ("completionTriggerCharacters");
      Write_DAP_String_Vector (S, V.completionTriggerCharacters'Access);

      JS.Key ("exceptionBreakpointFilters");
      Write_DAP_ExceptionBreakpointsFilter_Vector
        (S, V.exceptionBreakpointFilters'Access);

      JS.Key ("supportTerminateDebuggee");
      JS.Write_Boolean (V.supportTerminateDebuggee);

      JS.Key ("supportedChecksumAlgorithms");
      Write_DAP_ChecksumAlgorithm_Vector
        (S, V.supportedChecksumAlgorithms'Access);

      JS.Key ("supportsBreakpointLocationsRequest");
      JS.Write_Boolean (V.supportsBreakpointLocationsRequest);

      JS.Key ("supportsCancelRequest");
      JS.Write_Boolean (V.supportsCancelRequest);

      JS.Key ("supportsClipboardContext");
      JS.Write_Boolean (V.supportsClipboardContext);

      JS.Key ("supportsCompletionsRequest");
      JS.Write_Boolean (V.supportsCompletionsRequest);

      JS.Key ("supportsConditionalBreakpoints");
      JS.Write_Boolean (V.supportsConditionalBreakpoints);

      JS.Key ("supportsConfigurationDoneRequest");
      JS.Write_Boolean (V.supportsConfigurationDoneRequest);

      JS.Key ("supportsDataBreakpoints");
      JS.Write_Boolean (V.supportsDataBreakpoints);

      JS.Key ("supportsDelayedStackTraceLoading");
      JS.Write_Boolean (V.supportsDelayedStackTraceLoading);

      JS.Key ("supportsDisassembleRequest");
      JS.Write_Boolean (V.supportsDisassembleRequest);

      JS.Key ("supportsEvaluateForHovers");
      JS.Write_Boolean (V.supportsEvaluateForHovers);

      JS.Key ("supportsExceptionFilterOptions");
      JS.Write_Boolean (V.supportsExceptionFilterOptions);

      JS.Key ("supportsExceptionInfoRequest");
      JS.Write_Boolean (V.supportsExceptionInfoRequest);

      JS.Key ("supportsExceptionOptions");
      JS.Write_Boolean (V.supportsExceptionOptions);

      JS.Key ("supportsFunctionBreakpoints");
      JS.Write_Boolean (V.supportsFunctionBreakpoints);

      JS.Key ("supportsGotoTargetsRequest");
      JS.Write_Boolean (V.supportsGotoTargetsRequest);

      JS.Key ("supportsHitConditionalBreakpoints");
      JS.Write_Boolean (V.supportsHitConditionalBreakpoints);

      JS.Key ("supportsInstructionBreakpoints");
      JS.Write_Boolean (V.supportsInstructionBreakpoints);

      JS.Key ("supportsLoadedSourcesRequest");
      JS.Write_Boolean (V.supportsLoadedSourcesRequest);

      JS.Key ("supportsLogPoints");
      JS.Write_Boolean (V.supportsLogPoints);

      JS.Key ("supportsModulesRequest");
      JS.Write_Boolean (V.supportsModulesRequest);

      JS.Key ("supportsReadMemoryRequest");
      JS.Write_Boolean (V.supportsReadMemoryRequest);

      JS.Key ("supportsRestartFrame");
      JS.Write_Boolean (V.supportsRestartFrame);

      JS.Key ("supportsRestartRequest");
      JS.Write_Boolean (V.supportsRestartRequest);

      JS.Key ("supportsSetExpression");
      JS.Write_Boolean (V.supportsSetExpression);

      JS.Key ("supportsSetVariable");
      JS.Write_Boolean (V.supportsSetVariable);

      JS.Key ("supportsStepBack");
      JS.Write_Boolean (V.supportsStepBack);

      JS.Key ("supportsStepInTargetsRequest");
      JS.Write_Boolean (V.supportsStepInTargetsRequest);

      JS.Key ("supportsSteppingGranularity");
      JS.Write_Boolean (V.supportsSteppingGranularity);

      JS.Key ("supportsTerminateRequest");
      JS.Write_Boolean (V.supportsTerminateRequest);

      JS.Key ("supportsTerminateThreadsRequest");
      JS.Write_Boolean (V.supportsTerminateThreadsRequest);

      JS.Key ("supportsValueFormattingOptions");
      JS.Write_Boolean (V.supportsValueFormattingOptions);

      JS.End_Object;
   end Write_Capabilities;

   procedure Write_CapabilitiesEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CapabilitiesEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_CapabilitiesEvent;

   procedure Write_Checksum
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Checksum)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("algorithm");
      Write_ChecksumAlgorithm (S, V.algorithm'Access);

      JS.Key ("checksum");
      JS.Write_String (V.checksum);

      JS.End_Object;
   end Write_Checksum;

   procedure Write_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_CompletionItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("label");
      JS.Write_String (V.label);

      JS.Key ("length");
      JS.Write_Integer (Interfaces.Integer_64 (V.length));

      JS.Key ("selectionLength");
      JS.Write_Integer (Interfaces.Integer_64 (V.selectionLength));

      JS.Key ("selectionStart");
      JS.Write_Integer (Interfaces.Integer_64 (V.selectionStart));

      JS.Key ("sortText");
      JS.Write_String (V.sortText);

      JS.Key ("start");
      JS.Write_Integer (Interfaces.Integer_64 (V.start));

      JS.Key ("text");
      JS.Write_String (V.text);

      JS.Key ("type");
      Write_CompletionItemType (S, V.a_type'Access);

      JS.End_Object;
   end Write_CompletionItem;

   procedure Write_CompletionsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_CompletionsArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_CompletionsRequest;

   procedure Write_CompletionsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_CompletionsResponse;

   procedure Write_ConfigurationDoneRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ConfigurationDoneRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_ConfigurationDoneArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ConfigurationDoneRequest;

   procedure Write_ConfigurationDoneResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ConfigurationDoneResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_ConfigurationDoneResponse;

   procedure Write_ContinueRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ContinueRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_ContinueArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ContinueRequest;

   procedure Write_ContinueResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ContinueResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ContinueResponse;

   procedure Write_ContinuedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ContinuedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ContinuedEvent;

   procedure Write_DataBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_DataBreakpoint)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("accessType");
      Write_DataBreakpointAccessType (S, V.accessType'Access);

      JS.Key ("condition");
      JS.Write_String (V.condition);

      JS.Key ("dataId");
      JS.Write_String (V.dataId);

      JS.Key ("hitCondition");
      JS.Write_String (V.hitCondition);

      JS.End_Object;
   end Write_DataBreakpoint;

   procedure Write_DataBreakpointInfoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointInfoRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_DataBreakpointInfoArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_DataBreakpointInfoRequest;

   procedure Write_DataBreakpointInfoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointInfoResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_DataBreakpointInfoResponse;

   procedure Write_DisassembleRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembleRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_DisassembleArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_DisassembleRequest;

   procedure Write_DisassembleResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembleResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_DisassembleResponse;

   procedure Write_DisassembledInstruction
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembledInstruction)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("address");
      JS.Write_String (V.address);

      JS.Key ("column");
      JS.Write_Integer (Interfaces.Integer_64 (V.column));

      JS.Key ("endColumn");
      JS.Write_Integer (Interfaces.Integer_64 (V.endColumn));

      JS.Key ("endLine");
      JS.Write_Integer (Interfaces.Integer_64 (V.endLine));

      JS.Key ("instruction");
      JS.Write_String (V.instruction);

      JS.Key ("instructionBytes");
      JS.Write_String (V.instructionBytes);

      JS.Key ("line");
      JS.Write_Integer (Interfaces.Integer_64 (V.line));

      JS.Key ("location");
      Write_Source (S, V.location'Access);

      JS.Key ("symbol");
      JS.Write_String (V.symbol);

      JS.End_Object;
   end Write_DisassembledInstruction;

   procedure Write_DisconnectRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisconnectRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_DisconnectArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_DisconnectRequest;

   procedure Write_DisconnectResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisconnectResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_DisconnectResponse;

   procedure Write_ErrorResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ErrorResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ErrorResponse;

   procedure Write_EvaluateArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_EvaluateArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("context");
      JS.Write_String (V.context);

      JS.Key ("expression");
      JS.Write_String (V.expression);

      JS.Key ("format");
      Write_ValueFormat (S, V.format'Access);

      JS.Key ("frameId");
      JS.Write_Integer (Interfaces.Integer_64 (V.frameId));

      JS.End_Object;
   end Write_EvaluateArguments;

   procedure Write_EvaluateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_EvaluateRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_EvaluateArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_EvaluateRequest;

   procedure Write_EvaluateResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_EvaluateResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_EvaluateResponse;

   procedure Write_ExceptionDetails
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionDetails)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("evaluateName");
      JS.Write_String (V.evaluateName);

      JS.Key ("fullTypeName");
      JS.Write_String (V.fullTypeName);

      JS.Key ("innerException");
      Write_DAP_ExceptionDetails_Vector (S, V.innerException'Access);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("stackTrace");
      JS.Write_String (V.stackTrace);

      JS.Key ("typeName");
      JS.Write_String (V.typeName);

      JS.End_Object;
   end Write_ExceptionDetails;

   procedure Write_ExceptionInfoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionInfoRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_ExceptionInfoArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ExceptionInfoRequest;

   procedure Write_ExceptionInfoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionInfoResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ExceptionInfoResponse;

   procedure Write_ExceptionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("breakMode");
      Write_ExceptionBreakMode (S, V.breakMode'Access);

      JS.Key ("path");
      Write_DAP_ExceptionPathSegment_Vector (S, V.path'Access);

      JS.End_Object;
   end Write_ExceptionOptions;

   procedure Write_ExceptionPathSegment
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionPathSegment)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("names");
      Write_DAP_String_Vector (S, V.names'Access);

      JS.Key ("negate");
      JS.Write_Boolean (V.negate);

      JS.End_Object;
   end Write_ExceptionPathSegment;

   procedure Write_ExitedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ExitedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ExitedEvent;

   procedure Write_GotoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_GotoArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_GotoRequest;

   procedure Write_GotoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_GotoResponse;

   procedure Write_GotoTargetsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_GotoTargetsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("column");
      JS.Write_Integer (Interfaces.Integer_64 (V.column));

      JS.Key ("line");
      JS.Write_Integer (Interfaces.Integer_64 (V.line));

      JS.Key ("source");
      Write_Source (S, V.a_source'Access);

      JS.End_Object;
   end Write_GotoTargetsArguments;

   procedure Write_GotoTargetsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_GotoTargetsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_GotoTargetsArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_GotoTargetsRequest;

   procedure Write_GotoTargetsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_GotoTargetsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_GotoTargetsResponse;

   procedure Write_InitializeRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializeRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_InitializeRequestArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_InitializeRequest;

   procedure Write_InitializeResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializeResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Capabilities (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_InitializeResponse;

   procedure Write_InitializedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_InitializedEvent;

   procedure Write_InvalidatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InvalidatedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_InvalidatedEvent;

   procedure Write_LaunchRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_LaunchRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_LaunchRequestArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_LaunchRequest;

   procedure Write_LaunchResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_LaunchResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_LaunchResponse;

   procedure Write_LoadedSourceEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourceEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_LoadedSourceEvent;

   procedure Write_LoadedSourcesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourcesRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_LoadedSourcesArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_LoadedSourcesRequest;

   procedure Write_LoadedSourcesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourcesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_LoadedSourcesResponse;

   procedure Write_Message
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Message)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("format");
      JS.Write_String (V.format);

      JS.Key ("id");
      JS.Write_Integer (Interfaces.Integer_64 (V.id));

      JS.Key ("sendTelemetry");
      JS.Write_Boolean (V.sendTelemetry);

      JS.Key ("showUser");
      JS.Write_Boolean (V.showUser);

      JS.Key ("url");
      JS.Write_String (V.url);

      JS.Key ("urlLabel");
      JS.Write_String (V.urlLabel);

      JS.Key ("variables");
      Write_DAP_String_Map (S, V.variables'Access);

      JS.End_Object;
   end Write_Message;

   procedure Write_ModuleEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ModuleEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ModuleEvent;

   procedure Write_ModulesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ModulesRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_ModulesArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ModulesRequest;

   procedure Write_ModulesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ModulesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ModulesResponse;

   procedure Write_ModulesViewDescriptor
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ModulesViewDescriptor)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("columns");
      Write_DAP_ColumnDescriptor_Vector (S, V.columns'Access);

      JS.End_Object;
   end Write_ModulesViewDescriptor;

   procedure Write_NextArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_NextArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("granularity");
      Write_SteppingGranularity (S, V.granularity'Access);

      JS.Key ("threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.threadId));

      JS.End_Object;
   end Write_NextArguments;

   procedure Write_NextRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_NextRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_NextArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_NextRequest;

   procedure Write_NextResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_NextResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_NextResponse;

   procedure Write_OutputEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_OutputEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_OutputEvent;

   procedure Write_PauseRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_PauseRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_PauseArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_PauseRequest;

   procedure Write_PauseResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_PauseResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_PauseResponse;

   procedure Write_ProcessEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ProcessEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ProcessEvent;

   procedure Write_ProgressEndEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProgressEndEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ProgressEndEvent;

   procedure Write_ProgressStartEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProgressStartEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ProgressStartEvent;

   procedure Write_ProgressUpdateEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProgressUpdateEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ProgressUpdateEvent;

   procedure Write_ReadMemoryRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReadMemoryRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_ReadMemoryArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ReadMemoryRequest;

   procedure Write_ReadMemoryResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReadMemoryResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ReadMemoryResponse;

   procedure Write_RestartFrameRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartFrameRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_RestartFrameArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_RestartFrameRequest;

   procedure Write_RestartFrameResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartFrameResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_RestartFrameResponse;

   procedure Write_RestartRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_RestartRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_RestartArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_RestartRequest;

   procedure Write_RestartResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_RestartResponse;

   procedure Write_ReverseContinueRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReverseContinueRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_ReverseContinueArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ReverseContinueRequest;

   procedure Write_ReverseContinueResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReverseContinueResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_ReverseContinueResponse;

   procedure Write_RunInTerminalRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RunInTerminalRequestArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("args");
      Write_DAP_String_Vector (S, V.args'Access);

      JS.Key ("cwd");
      JS.Write_String (V.cwd);

      JS.Key ("env");
      Write_DAP_String_Map (S, V.env'Access);

      JS.Key ("kind");
      JS.Write_String (V.kind);

      JS.End_Object;
   end Write_RunInTerminalRequestArguments;

   procedure Write_RunInTerminalRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RunInTerminalRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_RunInTerminalRequestArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_RunInTerminalRequest;

   procedure Write_RunInTerminalResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RunInTerminalResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_RunInTerminalResponse;

   procedure Write_Scope
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Scope)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("column");
      JS.Write_Integer (Interfaces.Integer_64 (V.column));

      JS.Key ("endColumn");
      JS.Write_Integer (Interfaces.Integer_64 (V.endColumn));

      JS.Key ("endLine");
      JS.Write_Integer (Interfaces.Integer_64 (V.endLine));

      JS.Key ("expensive");
      JS.Write_Boolean (V.expensive);

      JS.Key ("indexedVariables");
      JS.Write_Integer (Interfaces.Integer_64 (V.indexedVariables));

      JS.Key ("line");
      JS.Write_Integer (Interfaces.Integer_64 (V.line));

      JS.Key ("name");
      JS.Write_String (V.name);

      JS.Key ("namedVariables");
      JS.Write_Integer (Interfaces.Integer_64 (V.namedVariables));

      JS.Key ("presentationHint");
      JS.Write_String (V.presentationHint);

      JS.Key ("source");
      Write_Source (S, V.a_source'Access);

      JS.Key ("variablesReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.variablesReference));

      JS.End_Object;
   end Write_Scope;

   procedure Write_ScopesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ScopesRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_ScopesArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ScopesRequest;

   procedure Write_ScopesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ScopesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ScopesResponse;

   procedure Write_SetBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetBreakpointsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("breakpoints");
      Write_DAP_SourceBreakpoint_Vector (S, V.breakpoints'Access);

      JS.Key ("lines");
      Write_DAP_Integer_Vector (S, V.lines'Access);

      JS.Key ("source");
      Write_Source (S, V.a_source'Access);

      JS.Key ("sourceModified");
      JS.Write_Boolean (V.sourceModified);

      JS.End_Object;
   end Write_SetBreakpointsArguments;

   procedure Write_SetBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetBreakpointsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_SetBreakpointsArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SetBreakpointsRequest;

   procedure Write_SetBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SetBreakpointsResponse;

   procedure Write_SetDataBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetDataBreakpointsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("breakpoints");
      Write_DAP_DataBreakpoint_Vector (S, V.breakpoints'Access);

      JS.End_Object;
   end Write_SetDataBreakpointsArguments;

   procedure Write_SetDataBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetDataBreakpointsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_SetDataBreakpointsArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SetDataBreakpointsRequest;

   procedure Write_SetDataBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetDataBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SetDataBreakpointsResponse;

   procedure Write_SetExceptionBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExceptionBreakpointsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("exceptionOptions");
      Write_DAP_ExceptionOptions_Vector (S, V.exceptionOptions'Access);

      JS.Key ("filterOptions");
      Write_DAP_ExceptionFilterOptions_Vector (S, V.filterOptions'Access);

      JS.Key ("filters");
      Write_DAP_String_Vector (S, V.filters'Access);

      JS.End_Object;
   end Write_SetExceptionBreakpointsArguments;

   procedure Write_SetExceptionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExceptionBreakpointsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_SetExceptionBreakpointsArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SetExceptionBreakpointsRequest;

   procedure Write_SetExceptionBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExceptionBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_SetExceptionBreakpointsResponse;

   procedure Write_SetExpressionArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExpressionArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("expression");
      JS.Write_String (V.expression);

      JS.Key ("format");
      Write_ValueFormat (S, V.format'Access);

      JS.Key ("frameId");
      JS.Write_Integer (Interfaces.Integer_64 (V.frameId));

      JS.Key ("value");
      JS.Write_String (V.value);

      JS.End_Object;
   end Write_SetExpressionArguments;

   procedure Write_SetExpressionRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExpressionRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_SetExpressionArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SetExpressionRequest;

   procedure Write_SetExpressionResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExpressionResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SetExpressionResponse;

   procedure Write_SetFunctionBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetFunctionBreakpointsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("breakpoints");
      Write_DAP_FunctionBreakpoint_Vector (S, V.breakpoints'Access);

      JS.End_Object;
   end Write_SetFunctionBreakpointsArguments;

   procedure Write_SetFunctionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetFunctionBreakpointsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_SetFunctionBreakpointsArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SetFunctionBreakpointsRequest;

   procedure Write_SetFunctionBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetFunctionBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SetFunctionBreakpointsResponse;

   procedure Write_SetInstructionBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetInstructionBreakpointsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("breakpoints");
      Write_DAP_InstructionBreakpoint_Vector (S, V.breakpoints'Access);

      JS.End_Object;
   end Write_SetInstructionBreakpointsArguments;

   procedure Write_SetInstructionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetInstructionBreakpointsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_SetInstructionBreakpointsArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SetInstructionBreakpointsRequest;

   procedure Write_SetInstructionBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetInstructionBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SetInstructionBreakpointsResponse;

   procedure Write_SetVariableArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetVariableArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("format");
      Write_ValueFormat (S, V.format'Access);

      JS.Key ("name");
      JS.Write_String (V.name);

      JS.Key ("value");
      JS.Write_String (V.value);

      JS.Key ("variablesReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.variablesReference));

      JS.End_Object;
   end Write_SetVariableArguments;

   procedure Write_SetVariableRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetVariableRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_SetVariableArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SetVariableRequest;

   procedure Write_SetVariableResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetVariableResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SetVariableResponse;

   procedure Write_SourceArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SourceArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("source");
      Write_Source (S, V.a_source'Access);

      JS.Key ("sourceReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.sourceReference));

      JS.End_Object;
   end Write_SourceArguments;

   procedure Write_SourceRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_SourceRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_SourceArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SourceRequest;

   procedure Write_SourceResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_SourceResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SourceResponse;

   procedure Write_StackFrame
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StackFrame)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("canRestart");
      JS.Write_Boolean (V.canRestart);

      JS.Key ("column");
      JS.Write_Integer (Interfaces.Integer_64 (V.column));

      JS.Key ("endColumn");
      JS.Write_Integer (Interfaces.Integer_64 (V.endColumn));

      JS.Key ("endLine");
      JS.Write_Integer (Interfaces.Integer_64 (V.endLine));

      JS.Key ("id");
      JS.Write_Integer (Interfaces.Integer_64 (V.id));

      JS.Key ("instructionPointerReference");
      JS.Write_String (V.instructionPointerReference);

      JS.Key ("line");
      JS.Write_Integer (Interfaces.Integer_64 (V.line));

      JS.Key ("moduleId");
      if V.moduleId.Is_Number then
         JS.Write_Integer (Interfaces.Integer_64 (V.moduleId.Number));
      elsif not Is_Empty (V.moduleId.String) then
         JS.Write_String (V.moduleId.String);
      end if;

      JS.Key ("name");
      JS.Write_String (V.name);

      JS.Key ("presentationHint");
      JS.Write_String (V.presentationHint);

      JS.Key ("source");
      Write_Source (S, V.a_source'Access);

      JS.End_Object;
   end Write_StackFrame;

   procedure Write_StackFrameFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackFrameFormat)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("includeAll");
      JS.Write_Boolean (V.includeAll);

      JS.Key ("line");
      JS.Write_Boolean (V.line);

      JS.Key ("module");
      JS.Write_Boolean (V.module);

      JS.Key ("parameterNames");
      JS.Write_Boolean (V.parameterNames);

      JS.Key ("parameterTypes");
      JS.Write_Boolean (V.parameterTypes);

      JS.Key ("parameterValues");
      JS.Write_Boolean (V.parameterValues);

      JS.Key ("parameters");
      JS.Write_Boolean (V.parameters);

      JS.Key ("hex");
      JS.Write_Boolean (V.hex);

      JS.End_Object;
   end Write_StackFrameFormat;

   procedure Write_StackTraceArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackTraceArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      --  JS.Key ("format");
      --  Write_StackFrameFormat (S, V.format'Access);
      --  prevent cdt-gdb-adapter from breaking

      JS.Key ("levels");
      JS.Write_Integer (Interfaces.Integer_64 (V.levels));

      JS.Key ("startFrame");
      JS.Write_Integer (Interfaces.Integer_64 (V.startFrame));

      JS.Key ("threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.threadId));

      JS.End_Object;
   end Write_StackTraceArguments;

   procedure Write_StackTraceRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackTraceRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_StackTraceArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_StackTraceRequest;

   procedure Write_StackTraceResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackTraceResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_StackTraceResponse;

   procedure Write_StepBackArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepBackArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("granularity");
      Write_SteppingGranularity (S, V.granularity'Access);

      JS.Key ("threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.threadId));

      JS.End_Object;
   end Write_StepBackArguments;

   procedure Write_StepBackRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepBackRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_StepBackArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_StepBackRequest;

   procedure Write_StepBackResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepBackResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_StepBackResponse;

   procedure Write_StepInArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("granularity");
      Write_SteppingGranularity (S, V.granularity'Access);

      JS.Key ("targetId");
      JS.Write_Integer (Interfaces.Integer_64 (V.targetId));

      JS.Key ("threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.threadId));

      JS.End_Object;
   end Write_StepInArguments;

   procedure Write_StepInRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StepInRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_StepInArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_StepInRequest;

   procedure Write_StepInResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StepInResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_StepInResponse;

   procedure Write_StepInTargetsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInTargetsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_StepInTargetsArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_StepInTargetsRequest;

   procedure Write_StepInTargetsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInTargetsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_StepInTargetsResponse;

   procedure Write_StepOutArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepOutArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("granularity");
      Write_SteppingGranularity (S, V.granularity'Access);

      JS.Key ("threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.threadId));

      JS.End_Object;
   end Write_StepOutArguments;

   procedure Write_StepOutRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StepOutRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_StepOutArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_StepOutRequest;

   procedure Write_StepOutResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepOutResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_StepOutResponse;

   procedure Write_StoppedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StoppedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_StoppedEvent;

   procedure Write_TerminateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_TerminateArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_TerminateRequest;

   procedure Write_TerminateResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_TerminateResponse;

   procedure Write_TerminateThreadsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateThreadsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("threadIds");
      Write_DAP_Integer_Vector (S, V.threadIds'Access);

      JS.End_Object;
   end Write_TerminateThreadsArguments;

   procedure Write_TerminateThreadsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateThreadsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_TerminateThreadsArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_TerminateThreadsRequest;

   procedure Write_TerminateThreadsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateThreadsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_Any (S, V.a_body);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_TerminateThreadsResponse;

   procedure Write_TerminatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminatedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_TerminatedEvent;

   procedure Write_ThreadEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ThreadEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ThreadEvent;

   procedure Write_ThreadsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ThreadsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("arguments");
      Write_Any (S, V.arguments);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ThreadsRequest;

   procedure Write_ThreadsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ThreadsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ThreadsResponse;

   procedure Write_VariablePresentationHint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablePresentationHint)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("attributes");
      Write_DAP_String_Vector (S, V.attributes'Access);

      JS.Key ("kind");
      JS.Write_String (V.kind);

      JS.Key ("visibility");
      JS.Write_String (V.visibility);

      JS.End_Object;
   end Write_VariablePresentationHint;

   procedure Write_Variable
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Variable)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("evaluateName");
      JS.Write_String (V.evaluateName);

      JS.Key ("indexedVariables");
      JS.Write_Integer (Interfaces.Integer_64 (V.indexedVariables));

      JS.Key ("memoryReference");
      JS.Write_String (V.memoryReference);

      JS.Key ("name");
      JS.Write_String (V.name);

      JS.Key ("namedVariables");
      JS.Write_Integer (Interfaces.Integer_64 (V.namedVariables));

      JS.Key ("presentationHint");
      Write_VariablePresentationHint (S, V.presentationHint'Access);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("value");
      JS.Write_String (V.value);

      JS.Key ("variablesReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.variablesReference));

      JS.End_Object;
   end Write_Variable;

   procedure Write_VariablesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablesArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("count");
      JS.Write_Integer (Interfaces.Integer_64 (V.count));

      JS.Key ("filter");
      JS.Write_String (V.filter);

      JS.Key ("format");
      Write_ValueFormat (S, V.format'Access);

      JS.Key ("start");
      JS.Write_Integer (Interfaces.Integer_64 (V.start));

      JS.Key ("variablesReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.variablesReference));

      JS.End_Object;
   end Write_VariablesArguments;

   procedure Write_VariablesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablesRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      Write_VariablesArguments (S, V.arguments'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_VariablesRequest;

   procedure Write_VariablesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body");
      Write_DAP_String_Map (S, V.a_body'Access);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.message);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_VariablesResponse;

   procedure Read_ChecksumAlgorithm
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ChecksumAlgorithm)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if V /= null then
         if Text = "MD5" then
            V.all := Enums.MD5;
         elsif Text = "SHA1" then
            V.all := Enums.SHA1;
         elsif Text = "SHA256" then
            V.all := Enums.SHA256;
         else
            V.all := Enums.ChecksumAlgorithm'First;
         end if;
      end if;
   end Read_ChecksumAlgorithm;

   procedure Read_CompletionItemType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionItemType)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if V /= null then
         if Text = "method" then
            V.all := Enums.method;
         elsif Text = "function" then
            V.all := Enums.a_function;
         elsif Text = "constructor" then
            V.all := Enums.constructor;
         elsif Text = "field" then
            V.all := Enums.field;
         elsif Text = "variable" then
            V.all := Enums.variable;
         elsif Text = "class" then
            V.all := Enums.class;
         elsif Text = "interface" then
            V.all := Enums.a_interface;
         elsif Text = "module" then
            V.all := Enums.module;
         elsif Text = "property" then
            V.all := Enums.property;
         elsif Text = "unit" then
            V.all := Enums.unit;
         elsif Text = "value" then
            V.all := Enums.value;
         elsif Text = "enum" then
            V.all := Enums.enum;
         elsif Text = "keyword" then
            V.all := Enums.keyword;
         elsif Text = "snippet" then
            V.all := Enums.snippet;
         elsif Text = "text" then
            V.all := Enums.text;
         elsif Text = "color" then
            V.all := Enums.color;
         elsif Text = "file" then
            V.all := Enums.file;
         elsif Text = "reference" then
            V.all := Enums.reference;
         else
            V.all := Enums.CompletionItemType'First;
         end if;
      end if;
   end Read_CompletionItemType;

   procedure Read_DataBreakpointAccessType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointAccessType)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if V /= null then
         if Text = "read" then
            V.all := Enums.read;
         elsif Text = "write" then
            V.all := Enums.write;
         else
            V.all := Enums.DataBreakpointAccessType'First;
         end if;
      end if;
   end Read_DataBreakpointAccessType;

   procedure Read_ExceptionBreakMode
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionBreakMode)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if V /= null then
         if Text = "never" then
            V.all := Enums.never;
         elsif Text = "always" then
            V.all := Enums.always;
         elsif Text = "unhandled" then
            V.all := Enums.unhandled;
         else
            V.all := Enums.ExceptionBreakMode'First;
         end if;
      end if;
   end Read_ExceptionBreakMode;

   procedure Read_InvalidatedAreas
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InvalidatedAreas)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if V /= null then
         if Text = "all" then
            V.all := Enums.a_all;
         elsif Text = "stacks" then
            V.all := Enums.stacks;
         elsif Text = "threads" then
            V.all := Enums.threads;
         else
            V.all := Enums.InvalidatedAreas'First;
         end if;
      end if;
   end Read_InvalidatedAreas;

   procedure Read_SteppingGranularity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SteppingGranularity)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if V /= null then
         if Text = "statement" then
            V.all := Enums.statement;
         elsif Text = "line" then
            V.all := Enums.line;
         else
            V.all := Enums.SteppingGranularity'First;
         end if;
      end if;
   end Read_SteppingGranularity;

   procedure Read_DAP_String_Map
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_DAP_String_Map)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key   : Virtual_String;
            Value : Virtual_String;
         begin
            LSP.Types.Read_String (S, Key);
            JS.R.Read_Next;
            LSP.Types.Read_String (S, Value);
            if V /= null then
               V.all.Insert (Key, Value);
            end if;
         end;
      end loop;

      JS.R.Read_Next;
   end Read_DAP_String_Map;

   procedure Read_DAP_String_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_String_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Virtual_String;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         LSP.Types.Read_String (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_String_Vector;

   procedure Read_DAP_Integer_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_Integer_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : LSP.Types.LSP_Number;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;
      while not JS.R.Is_End_Array loop
         LSP.Types.Read (S, T);
         V.Append (Integer (T));
      end loop;
      JS.R.Read_Next;

   end Read_DAP_Integer_Vector;

   procedure Read_DAP_ChecksumAlgorithm_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ChecksumAlgorithm_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Access_ChecksumAlgorithm;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         T := new Enums.ChecksumAlgorithm;
         Read_ChecksumAlgorithm (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_ChecksumAlgorithm_Vector;

   procedure Read_DAP_Checksum_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_Checksum_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Access_Checksum;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         T := new Checksum;
         Read_Checksum (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_Checksum_Vector;

   procedure Read_DAP_ColumnDescriptor_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ColumnDescriptor_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Access_ColumnDescriptor;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         T := new ColumnDescriptor;
         Read_ColumnDescriptor (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_ColumnDescriptor_Vector;

   procedure Read_DAP_DataBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_DataBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Access_DataBreakpoint;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         T := new DataBreakpoint;
         Read_DataBreakpoint (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_DataBreakpoint_Vector;

   procedure Read_DAP_ExceptionBreakpointsFilter_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionBreakpointsFilter_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Access_ExceptionBreakpointsFilter;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         T := new ExceptionBreakpointsFilter;
         Read_ExceptionBreakpointsFilter (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_ExceptionBreakpointsFilter_Vector;

   procedure Read_DAP_ExceptionDetails_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionDetails_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Access_ExceptionDetails;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         T := new ExceptionDetails;
         Read_ExceptionDetails (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_ExceptionDetails_Vector;

   procedure Read_DAP_ExceptionFilterOptions_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionFilterOptions_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Access_ExceptionFilterOptions;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         T := new ExceptionFilterOptions;
         Read_ExceptionFilterOptions (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_ExceptionFilterOptions_Vector;

   procedure Read_DAP_ExceptionOptions_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionOptions_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Access_ExceptionOptions;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         T := new ExceptionOptions;
         Read_ExceptionOptions (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_ExceptionOptions_Vector;

   procedure Read_DAP_ExceptionPathSegment_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_ExceptionPathSegment_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Access_ExceptionPathSegment;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         T := new ExceptionPathSegment;
         Read_ExceptionPathSegment (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_ExceptionPathSegment_Vector;

   procedure Read_DAP_FunctionBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_FunctionBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Access_FunctionBreakpoint;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         T := new FunctionBreakpoint;
         Read_FunctionBreakpoint (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_FunctionBreakpoint_Vector;

   procedure Read_DAP_InstructionBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_InstructionBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Access_InstructionBreakpoint;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         T := new InstructionBreakpoint;
         Read_InstructionBreakpoint (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_InstructionBreakpoint_Vector;

   procedure Read_DAP_SourceBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_SourceBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Access_SourceBreakpoint;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         T := new SourceBreakpoint;
         Read_SourceBreakpoint (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_SourceBreakpoint_Vector;

   procedure Read_DAP_Source_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DAP_Source_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Access_Source;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         T := new Source;
         Read_Source (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_Source_Vector;

   procedure Read_AttachRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_AttachRequestArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "__restart" then
               Read_Any (S, V.all.a_restart);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_AttachRequestArguments;

   procedure Read_BreakpointLocation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "column" then
               Read (S, V.all.column);

            elsif Key = "endColumn" then
               Read (S, V.all.endColumn);

            elsif Key = "endLine" then
               Read (S, V.all.endLine);

            elsif Key = "line" then
               Read (S, V.all.line);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_BreakpointLocation;

   procedure Read_CancelArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CancelArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "progressId" then
               Read_String (S, V.all.progressId);

            elsif Key = "requestId" then
               Read (S, V.all.requestId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CancelArguments;

   procedure Read_ColumnDescriptor
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ColumnDescriptor)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "attributeName" then
               Read_String (S, V.all.attributeName);

            elsif Key = "format" then
               Read_String (S, V.all.format);

            elsif Key = "label" then
               Read_String (S, V.all.label);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "width" then
               Read (S, V.all.width);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ColumnDescriptor;

   procedure Read_CompletionsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "column" then
               Read (S, V.all.column);

            elsif Key = "frameId" then
               Read (S, V.all.frameId);

            elsif Key = "line" then
               Read (S, V.all.line);

            elsif Key = "text" then
               Read_String (S, V.all.text);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionsArguments;

   procedure Read_ConfigurationDoneArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ConfigurationDoneArguments)
   is
   begin
      --  this record is empty
      pragma Unreferenced (S);
      pragma Unreferenced (V);
   end Read_ConfigurationDoneArguments;
   procedure Read_ContinueArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ContinueArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "threadId" then
               Read (S, V.all.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ContinueArguments;

   procedure Read_DataBreakpointInfoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointInfoArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "name" then
               Read_String (S, V.all.name);

            elsif Key = "variablesReference" then
               Read (S, V.all.variablesReference);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DataBreakpointInfoArguments;

   procedure Read_DisassembleArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembleArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "instructionCount" then
               Read (S, V.all.instructionCount);

            elsif Key = "instructionOffset" then
               Read (S, V.all.instructionOffset);

            elsif Key = "memoryReference" then
               Read_String (S, V.all.memoryReference);

            elsif Key = "offset" then
               Read (S, V.all.offset);

            elsif Key = "resolveSymbols" then
               Read_Boolean (JS, V.all.resolveSymbols);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DisassembleArguments;

   procedure Read_DisconnectArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisconnectArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "restart" then
               Read_Boolean (JS, V.all.restart);

            elsif Key = "terminateDebuggee" then
               Read_Boolean (JS, V.all.terminateDebuggee);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DisconnectArguments;

   procedure Read_ExceptionBreakpointsFilter
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionBreakpointsFilter)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "conditionDescription" then
               Read_String (S, V.all.conditionDescription);

            elsif Key = "default" then
               Read_Boolean (JS, V.all.default);

            elsif Key = "filter" then
               Read_String (S, V.all.filter);

            elsif Key = "label" then
               Read_String (S, V.all.label);

            elsif Key = "supportsCondition" then
               Read_Boolean (JS, V.all.supportsCondition);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionBreakpointsFilter;

   procedure Read_ExceptionFilterOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionFilterOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "condition" then
               Read_String (S, V.all.condition);

            elsif Key = "filterId" then
               Read_String (S, V.all.filterId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionFilterOptions;

   procedure Read_ExceptionInfoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionInfoArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "threadId" then
               Read (S, V.all.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionInfoArguments;

   procedure Read_FunctionBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_FunctionBreakpoint)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "condition" then
               Read_String (S, V.all.condition);

            elsif Key = "hitCondition" then
               Read_String (S, V.all.hitCondition);

            elsif Key = "name" then
               Read_String (S, V.all.name);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FunctionBreakpoint;

   procedure Read_GotoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "targetId" then
               Read (S, V.all.targetId);

            elsif Key = "threadId" then
               Read (S, V.all.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_GotoArguments;

   procedure Read_GotoTarget
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoTarget)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "column" then
               Read (S, V.all.column);

            elsif Key = "endColumn" then
               Read (S, V.all.endColumn);

            elsif Key = "endLine" then
               Read (S, V.all.endLine);

            elsif Key = "id" then
               Read (S, V.all.id);

            elsif Key = "instructionPointerReference" then
               Read_String (S, V.all.instructionPointerReference);

            elsif Key = "label" then
               Read_String (S, V.all.label);

            elsif Key = "line" then
               Read (S, V.all.line);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_GotoTarget;

   procedure Read_InitializeRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializeRequestArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "adapterID" then
               Read_String (S, V.all.adapterID);

            elsif Key = "clientID" then
               Read_String (S, V.all.clientID);

            elsif Key = "clientName" then
               Read_String (S, V.all.clientName);

            elsif Key = "columnsStartAt1" then
               Read_Boolean (JS, V.all.columnsStartAt1);

            elsif Key = "linesStartAt1" then
               Read_Boolean (JS, V.all.linesStartAt1);

            elsif Key = "locale" then
               Read_String (S, V.all.locale);

            elsif Key = "pathFormat" then
               Read_String (S, V.all.pathFormat);

            elsif Key = "supportsInvalidatedEvent" then
               Read_Boolean (JS, V.all.supportsInvalidatedEvent);

            elsif Key = "supportsMemoryReferences" then
               Read_Boolean (JS, V.all.supportsMemoryReferences);

            elsif Key = "supportsProgressReporting" then
               Read_Boolean (JS, V.all.supportsProgressReporting);

            elsif Key = "supportsRunInTerminalRequest" then
               Read_Boolean (JS, V.all.supportsRunInTerminalRequest);

            elsif Key = "supportsVariablePaging" then
               Read_Boolean (JS, V.all.supportsVariablePaging);

            elsif Key = "supportsVariableType" then
               Read_Boolean (JS, V.all.supportsVariableType);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InitializeRequestArguments;

   procedure Read_InstructionBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InstructionBreakpoint)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "condition" then
               Read_String (S, V.all.condition);

            elsif Key = "hitCondition" then
               Read_String (S, V.all.hitCondition);

            elsif Key = "instructionReference" then
               Read_String (S, V.all.instructionReference);

            elsif Key = "offset" then
               Read (S, V.all.offset);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InstructionBreakpoint;

   procedure Read_LaunchRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LaunchRequestArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "__restart" then
               Read_Any (S, V.all.a_restart);

            elsif Key = "noDebug" then
               Read_Boolean (JS, V.all.noDebug);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LaunchRequestArguments;

   procedure Read_LoadedSourcesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourcesArguments)
   is
   begin
      --  this record is empty
      pragma Unreferenced (S);
      pragma Unreferenced (V);
   end Read_LoadedSourcesArguments;
   procedure Read_Module
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Module)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "addressRange" then
               Read_String (S, V.all.addressRange);

            elsif Key = "dateTimeStamp" then
               Read_String (S, V.all.dateTimeStamp);

            elsif Key = "id" then
               Read_LSP_Number_Or_String (S, V.all.id);

            elsif Key = "isOptimized" then
               Read_Boolean (JS, V.all.isOptimized);

            elsif Key = "isUserCode" then
               Read_Boolean (JS, V.all.isUserCode);

            elsif Key = "name" then
               Read_String (S, V.all.name);

            elsif Key = "path" then
               Read_String (S, V.all.path);

            elsif Key = "symbolFilePath" then
               Read_String (S, V.all.symbolFilePath);

            elsif Key = "symbolStatus" then
               Read_String (S, V.all.symbolStatus);

            elsif Key = "version" then
               Read_String (S, V.all.version);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Module;

   procedure Read_ModulesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ModulesArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "moduleCount" then
               Read (S, V.all.moduleCount);

            elsif Key = "startModule" then
               Read (S, V.all.startModule);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ModulesArguments;

   procedure Read_PauseArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_PauseArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "threadId" then
               Read (S, V.all.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_PauseArguments;

   procedure Read_ProtocolMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProtocolMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ProtocolMessage;

   procedure Read_ReadMemoryArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReadMemoryArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "count" then
               Read (S, V.all.count);

            elsif Key = "memoryReference" then
               Read_String (S, V.all.memoryReference);

            elsif Key = "offset" then
               Read (S, V.all.offset);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ReadMemoryArguments;

   procedure Read_RestartArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartArguments)
   is
   begin
      --  this record is empty
      pragma Unreferenced (S);
      pragma Unreferenced (V);
   end Read_RestartArguments;
   procedure Read_RestartFrameArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartFrameArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "frameId" then
               Read (S, V.all.frameId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RestartFrameArguments;

   procedure Read_ReverseContinueArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReverseContinueArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "threadId" then
               Read (S, V.all.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ReverseContinueArguments;

   procedure Read_ScopesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ScopesArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "frameId" then
               Read (S, V.all.frameId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ScopesArguments;

   procedure Read_SourceBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SourceBreakpoint)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "column" then
               Read (S, V.all.column);

            elsif Key = "condition" then
               Read_String (S, V.all.condition);

            elsif Key = "hitCondition" then
               Read_String (S, V.all.hitCondition);

            elsif Key = "line" then
               Read (S, V.all.line);

            elsif Key = "logMessage" then
               Read_String (S, V.all.logMessage);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SourceBreakpoint;

   procedure Read_StepInTarget
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StepInTarget)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "id" then
               Read (S, V.all.id);

            elsif Key = "label" then
               Read_String (S, V.all.label);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepInTarget;

   procedure Read_StepInTargetsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInTargetsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "frameId" then
               Read (S, V.all.frameId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepInTargetsArguments;

   procedure Read_TerminateArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "restart" then
               Read_Boolean (JS, V.all.restart);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TerminateArguments;

   procedure Read_Thread
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Thread)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "id" then
               Read (S, V.all.id);

            elsif Key = "name" then
               Read_String (S, V.all.name);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Thread;

   procedure Read_ValueFormat
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ValueFormat)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "hex" then
               Read_Boolean (JS, V.all.hex);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ValueFormat;

   procedure Read_Request
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_Any (S, V.all.arguments);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Request;

   procedure Read_AttachRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_AttachRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_AttachRequestArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_AttachRequest;

   procedure Read_Response
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Response)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Response;

   procedure Read_AttachResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_AttachResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_AttachResponse;

   procedure Read_Source
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Source)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "adapterData" then
               Read_Any (S, V.all.adapterData);

            elsif Key = "checksums" then
               Read_DAP_Checksum_Vector (S, V.all.checksums'Access);

            elsif Key = "name" then
               Read_String (S, V.all.name);

            elsif Key = "origin" then
               Read_String (S, V.all.origin);

            elsif Key = "path" then
               Read_String (S, V.all.path);

            elsif Key = "presentationHint" then
               Read_String (S, V.all.presentationHint);

            elsif Key = "sourceReference" then
               Read (S, V.all.sourceReference);

            elsif Key = "sources" then
               Read_DAP_Source_Vector (S, V.all.sources'Access);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Source;

   procedure Read_Breakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Breakpoint)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "column" then
               Read (S, V.all.column);

            elsif Key = "endColumn" then
               Read (S, V.all.endColumn);

            elsif Key = "endLine" then
               Read (S, V.all.endLine);

            elsif Key = "id" then
               Read (S, V.all.id);

            elsif Key = "instructionReference" then
               Read_String (S, V.all.instructionReference);

            elsif Key = "line" then
               Read (S, V.all.line);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "offset" then
               Read (S, V.all.offset);

            elsif Key = "source" then
               Read_Source (S, V.all.a_source'Access);

            elsif Key = "verified" then
               Read_Boolean (JS, V.all.verified);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Breakpoint;

   procedure Read_Event
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Event)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Event;

   procedure Read_BreakpointEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_BreakpointEvent;

   procedure Read_BreakpointLocationsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocationsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "column" then
               Read (S, V.all.column);

            elsif Key = "endColumn" then
               Read (S, V.all.endColumn);

            elsif Key = "endLine" then
               Read (S, V.all.endLine);

            elsif Key = "line" then
               Read (S, V.all.line);

            elsif Key = "source" then
               Read_Source (S, V.all.a_source'Access);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_BreakpointLocationsArguments;

   procedure Read_BreakpointLocationsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocationsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_BreakpointLocationsArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_BreakpointLocationsRequest;

   procedure Read_BreakpointLocationsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_BreakpointLocationsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_BreakpointLocationsResponse;

   procedure Read_CancelRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_CancelRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_CancelArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CancelRequest;

   procedure Read_CancelResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_CancelResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CancelResponse;

   procedure Read_Capabilities
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Capabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "additionalModuleColumns" then
               Read_DAP_ColumnDescriptor_Vector
                 (S, V.all.additionalModuleColumns'Access);

            elsif Key = "completionTriggerCharacters" then
               Read_DAP_String_Vector
                 (S, V.all.completionTriggerCharacters'Access);

            elsif Key = "exceptionBreakpointFilters" then
               Read_DAP_ExceptionBreakpointsFilter_Vector
                 (S, V.all.exceptionBreakpointFilters'Access);

            elsif Key = "supportTerminateDebuggee" then
               Read_Boolean (JS, V.all.supportTerminateDebuggee);

            elsif Key = "supportedChecksumAlgorithms" then
               Read_DAP_ChecksumAlgorithm_Vector
                 (S, V.all.supportedChecksumAlgorithms'Access);

            elsif Key = "supportsBreakpointLocationsRequest" then
               Read_Boolean (JS, V.all.supportsBreakpointLocationsRequest);

            elsif Key = "supportsCancelRequest" then
               Read_Boolean (JS, V.all.supportsCancelRequest);

            elsif Key = "supportsClipboardContext" then
               Read_Boolean (JS, V.all.supportsClipboardContext);

            elsif Key = "supportsCompletionsRequest" then
               Read_Boolean (JS, V.all.supportsCompletionsRequest);

            elsif Key = "supportsConditionalBreakpoints" then
               Read_Boolean (JS, V.all.supportsConditionalBreakpoints);

            elsif Key = "supportsConfigurationDoneRequest" then
               Read_Boolean (JS, V.all.supportsConfigurationDoneRequest);

            elsif Key = "supportsDataBreakpoints" then
               Read_Boolean (JS, V.all.supportsDataBreakpoints);

            elsif Key = "supportsDelayedStackTraceLoading" then
               Read_Boolean (JS, V.all.supportsDelayedStackTraceLoading);

            elsif Key = "supportsDisassembleRequest" then
               Read_Boolean (JS, V.all.supportsDisassembleRequest);

            elsif Key = "supportsEvaluateForHovers" then
               Read_Boolean (JS, V.all.supportsEvaluateForHovers);

            elsif Key = "supportsExceptionFilterOptions" then
               Read_Boolean (JS, V.all.supportsExceptionFilterOptions);

            elsif Key = "supportsExceptionInfoRequest" then
               Read_Boolean (JS, V.all.supportsExceptionInfoRequest);

            elsif Key = "supportsExceptionOptions" then
               Read_Boolean (JS, V.all.supportsExceptionOptions);

            elsif Key = "supportsFunctionBreakpoints" then
               Read_Boolean (JS, V.all.supportsFunctionBreakpoints);

            elsif Key = "supportsGotoTargetsRequest" then
               Read_Boolean (JS, V.all.supportsGotoTargetsRequest);

            elsif Key = "supportsHitConditionalBreakpoints" then
               Read_Boolean (JS, V.all.supportsHitConditionalBreakpoints);

            elsif Key = "supportsInstructionBreakpoints" then
               Read_Boolean (JS, V.all.supportsInstructionBreakpoints);

            elsif Key = "supportsLoadedSourcesRequest" then
               Read_Boolean (JS, V.all.supportsLoadedSourcesRequest);

            elsif Key = "supportsLogPoints" then
               Read_Boolean (JS, V.all.supportsLogPoints);

            elsif Key = "supportsModulesRequest" then
               Read_Boolean (JS, V.all.supportsModulesRequest);

            elsif Key = "supportsReadMemoryRequest" then
               Read_Boolean (JS, V.all.supportsReadMemoryRequest);

            elsif Key = "supportsRestartFrame" then
               Read_Boolean (JS, V.all.supportsRestartFrame);

            elsif Key = "supportsRestartRequest" then
               Read_Boolean (JS, V.all.supportsRestartRequest);

            elsif Key = "supportsSetExpression" then
               Read_Boolean (JS, V.all.supportsSetExpression);

            elsif Key = "supportsSetVariable" then
               Read_Boolean (JS, V.all.supportsSetVariable);

            elsif Key = "supportsStepBack" then
               Read_Boolean (JS, V.all.supportsStepBack);

            elsif Key = "supportsStepInTargetsRequest" then
               Read_Boolean (JS, V.all.supportsStepInTargetsRequest);

            elsif Key = "supportsSteppingGranularity" then
               Read_Boolean (JS, V.all.supportsSteppingGranularity);

            elsif Key = "supportsTerminateRequest" then
               Read_Boolean (JS, V.all.supportsTerminateRequest);

            elsif Key = "supportsTerminateThreadsRequest" then
               Read_Boolean (JS, V.all.supportsTerminateThreadsRequest);

            elsif Key = "supportsValueFormattingOptions" then
               Read_Boolean (JS, V.all.supportsValueFormattingOptions);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Capabilities;

   procedure Read_CapabilitiesEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CapabilitiesEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CapabilitiesEvent;

   procedure Read_Checksum
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Checksum)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "algorithm" then
               Read_ChecksumAlgorithm (S, V.all.algorithm'Access);

            elsif Key = "checksum" then
               Read_String (S, V.all.checksum);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Checksum;

   procedure Read_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_CompletionItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "label" then
               Read_String (S, V.all.label);

            elsif Key = "length" then
               Read (S, V.all.length);

            elsif Key = "selectionLength" then
               Read (S, V.all.selectionLength);

            elsif Key = "selectionStart" then
               Read (S, V.all.selectionStart);

            elsif Key = "sortText" then
               Read_String (S, V.all.sortText);

            elsif Key = "start" then
               Read (S, V.all.start);

            elsif Key = "text" then
               Read_String (S, V.all.text);

            elsif Key = "type" then
               Read_CompletionItemType (S, V.all.a_type'Access);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionItem;

   procedure Read_CompletionsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_CompletionsArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionsRequest;

   procedure Read_CompletionsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_CompletionsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionsResponse;

   procedure Read_ConfigurationDoneRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ConfigurationDoneRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_ConfigurationDoneArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ConfigurationDoneRequest;

   procedure Read_ConfigurationDoneResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ConfigurationDoneResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ConfigurationDoneResponse;

   procedure Read_ContinueRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ContinueRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_ContinueArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ContinueRequest;

   procedure Read_ContinueResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ContinueResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ContinueResponse;

   procedure Read_ContinuedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ContinuedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ContinuedEvent;

   procedure Read_DataBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_DataBreakpoint)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "accessType" then
               Read_DataBreakpointAccessType (S, V.all.accessType'Access);

            elsif Key = "condition" then
               Read_String (S, V.all.condition);

            elsif Key = "dataId" then
               Read_String (S, V.all.dataId);

            elsif Key = "hitCondition" then
               Read_String (S, V.all.hitCondition);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DataBreakpoint;

   procedure Read_DataBreakpointInfoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointInfoRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_DataBreakpointInfoArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DataBreakpointInfoRequest;

   procedure Read_DataBreakpointInfoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DataBreakpointInfoResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DataBreakpointInfoResponse;

   procedure Read_DisassembleRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembleRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_DisassembleArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DisassembleRequest;

   procedure Read_DisassembleResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembleResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DisassembleResponse;

   procedure Read_DisassembledInstruction
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisassembledInstruction)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "address" then
               Read_String (S, V.all.address);

            elsif Key = "column" then
               Read (S, V.all.column);

            elsif Key = "endColumn" then
               Read (S, V.all.endColumn);

            elsif Key = "endLine" then
               Read (S, V.all.endLine);

            elsif Key = "instruction" then
               Read_String (S, V.all.instruction);

            elsif Key = "instructionBytes" then
               Read_String (S, V.all.instructionBytes);

            elsif Key = "line" then
               Read (S, V.all.line);

            elsif Key = "location" then
               Read_Source (S, V.all.location'Access);

            elsif Key = "symbol" then
               Read_String (S, V.all.symbol);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DisassembledInstruction;

   procedure Read_DisconnectRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisconnectRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_DisconnectArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DisconnectRequest;

   procedure Read_DisconnectResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_DisconnectResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DisconnectResponse;

   procedure Read_ErrorResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ErrorResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ErrorResponse;

   procedure Read_EvaluateArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_EvaluateArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "context" then
               Read_String (S, V.all.context);

            elsif Key = "expression" then
               Read_String (S, V.all.expression);

            elsif Key = "format" then
               Read_ValueFormat (S, V.all.format'Access);

            elsif Key = "frameId" then
               Read (S, V.all.frameId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_EvaluateArguments;

   procedure Read_EvaluateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_EvaluateRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_EvaluateArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_EvaluateRequest;

   procedure Read_EvaluateResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_EvaluateResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_EvaluateResponse;

   procedure Read_ExceptionDetails
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionDetails)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "evaluateName" then
               Read_String (S, V.all.evaluateName);

            elsif Key = "fullTypeName" then
               Read_String (S, V.all.fullTypeName);

            elsif Key = "innerException" then
               Read_DAP_ExceptionDetails_Vector
                 (S, V.all.innerException'Access);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "stackTrace" then
               Read_String (S, V.all.stackTrace);

            elsif Key = "typeName" then
               Read_String (S, V.all.typeName);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionDetails;

   procedure Read_ExceptionInfoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionInfoRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_ExceptionInfoArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionInfoRequest;

   procedure Read_ExceptionInfoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionInfoResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionInfoResponse;

   procedure Read_ExceptionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "breakMode" then
               Read_ExceptionBreakMode (S, V.all.breakMode'Access);

            elsif Key = "path" then
               Read_DAP_ExceptionPathSegment_Vector (S, V.all.path'Access);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionOptions;

   procedure Read_ExceptionPathSegment
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ExceptionPathSegment)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "names" then
               Read_DAP_String_Vector (S, V.all.names'Access);

            elsif Key = "negate" then
               Read_Boolean (JS, V.all.negate);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionPathSegment;

   procedure Read_ExitedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ExitedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExitedEvent;

   procedure Read_GotoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_GotoArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_GotoRequest;

   procedure Read_GotoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_GotoResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_GotoResponse;

   procedure Read_GotoTargetsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_GotoTargetsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "column" then
               Read (S, V.all.column);

            elsif Key = "line" then
               Read (S, V.all.line);

            elsif Key = "source" then
               Read_Source (S, V.all.a_source'Access);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_GotoTargetsArguments;

   procedure Read_GotoTargetsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_GotoTargetsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_GotoTargetsArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_GotoTargetsRequest;

   procedure Read_GotoTargetsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_GotoTargetsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_GotoTargetsResponse;

   procedure Read_InitializeRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializeRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_InitializeRequestArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InitializeRequest;

   procedure Read_InitializeResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializeResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Capabilities (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InitializeResponse;

   procedure Read_InitializedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InitializedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InitializedEvent;

   procedure Read_InvalidatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_InvalidatedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InvalidatedEvent;

   procedure Read_LaunchRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_LaunchRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_LaunchRequestArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LaunchRequest;

   procedure Read_LaunchResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_LaunchResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LaunchResponse;

   procedure Read_LoadedSourceEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourceEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LoadedSourceEvent;

   procedure Read_LoadedSourcesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourcesRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_LoadedSourcesArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LoadedSourcesRequest;

   procedure Read_LoadedSourcesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_LoadedSourcesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LoadedSourcesResponse;

   procedure Read_Message
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Message)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "format" then
               Read_String (S, V.all.format);

            elsif Key = "id" then
               Read (S, V.all.id);

            elsif Key = "sendTelemetry" then
               Read_Boolean (JS, V.all.sendTelemetry);

            elsif Key = "showUser" then
               Read_Boolean (JS, V.all.showUser);

            elsif Key = "url" then
               Read_String (S, V.all.url);

            elsif Key = "urlLabel" then
               Read_String (S, V.all.urlLabel);

            elsif Key = "variables" then
               Read_DAP_String_Map (S, V.all.variables'Access);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Message;

   procedure Read_ModuleEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ModuleEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ModuleEvent;

   procedure Read_ModulesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ModulesRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_ModulesArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ModulesRequest;

   procedure Read_ModulesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ModulesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ModulesResponse;

   procedure Read_ModulesViewDescriptor
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ModulesViewDescriptor)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "columns" then
               Read_DAP_ColumnDescriptor_Vector (S, V.all.columns'Access);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ModulesViewDescriptor;

   procedure Read_NextArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_NextArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "granularity" then
               Read_SteppingGranularity (S, V.all.granularity'Access);

            elsif Key = "threadId" then
               Read (S, V.all.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_NextArguments;

   procedure Read_NextRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_NextRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_NextArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_NextRequest;

   procedure Read_NextResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_NextResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_NextResponse;

   procedure Read_OutputEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_OutputEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_OutputEvent;

   procedure Read_PauseRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_PauseRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_PauseArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_PauseRequest;

   procedure Read_PauseResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_PauseResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_PauseResponse;

   procedure Read_ProcessEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ProcessEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ProcessEvent;

   procedure Read_ProgressEndEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProgressEndEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ProgressEndEvent;

   procedure Read_ProgressStartEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProgressStartEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ProgressStartEvent;

   procedure Read_ProgressUpdateEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ProgressUpdateEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ProgressUpdateEvent;

   procedure Read_ReadMemoryRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReadMemoryRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_ReadMemoryArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ReadMemoryRequest;

   procedure Read_ReadMemoryResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReadMemoryResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ReadMemoryResponse;

   procedure Read_RestartFrameRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartFrameRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_RestartFrameArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RestartFrameRequest;

   procedure Read_RestartFrameResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartFrameResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RestartFrameResponse;

   procedure Read_RestartRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_RestartRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_RestartArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RestartRequest;

   procedure Read_RestartResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RestartResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RestartResponse;

   procedure Read_ReverseContinueRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReverseContinueRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_ReverseContinueArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ReverseContinueRequest;

   procedure Read_ReverseContinueResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ReverseContinueResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ReverseContinueResponse;

   procedure Read_RunInTerminalRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RunInTerminalRequestArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "args" then
               Read_DAP_String_Vector (S, V.all.args'Access);

            elsif Key = "cwd" then
               Read_String (S, V.all.cwd);

            elsif Key = "env" then
               Read_DAP_String_Map (S, V.all.env'Access);

            elsif Key = "kind" then
               Read_String (S, V.all.kind);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RunInTerminalRequestArguments;

   procedure Read_RunInTerminalRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RunInTerminalRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_RunInTerminalRequestArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RunInTerminalRequest;

   procedure Read_RunInTerminalResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_RunInTerminalResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RunInTerminalResponse;

   procedure Read_Scope
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Scope)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "column" then
               Read (S, V.all.column);

            elsif Key = "endColumn" then
               Read (S, V.all.endColumn);

            elsif Key = "endLine" then
               Read (S, V.all.endLine);

            elsif Key = "expensive" then
               Read_Boolean (JS, V.all.expensive);

            elsif Key = "indexedVariables" then
               Read (S, V.all.indexedVariables);

            elsif Key = "line" then
               Read (S, V.all.line);

            elsif Key = "name" then
               Read_String (S, V.all.name);

            elsif Key = "namedVariables" then
               Read (S, V.all.namedVariables);

            elsif Key = "presentationHint" then
               Read_String (S, V.all.presentationHint);

            elsif Key = "source" then
               Read_Source (S, V.all.a_source'Access);

            elsif Key = "variablesReference" then
               Read (S, V.all.variablesReference);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Scope;

   procedure Read_ScopesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ScopesRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_ScopesArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ScopesRequest;

   procedure Read_ScopesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ScopesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ScopesResponse;

   procedure Read_SetBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetBreakpointsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "breakpoints" then
               Read_DAP_SourceBreakpoint_Vector (S, V.all.breakpoints'Access);

            elsif Key = "lines" then
               Read_DAP_Integer_Vector (S, V.all.lines'Access);

            elsif Key = "source" then
               Read_Source (S, V.all.a_source'Access);

            elsif Key = "sourceModified" then
               Read_Boolean (JS, V.all.sourceModified);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetBreakpointsArguments;

   procedure Read_SetBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetBreakpointsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_SetBreakpointsArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetBreakpointsRequest;

   procedure Read_SetBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetBreakpointsResponse;

   procedure Read_SetDataBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetDataBreakpointsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "breakpoints" then
               Read_DAP_DataBreakpoint_Vector (S, V.all.breakpoints'Access);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetDataBreakpointsArguments;

   procedure Read_SetDataBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetDataBreakpointsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_SetDataBreakpointsArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetDataBreakpointsRequest;

   procedure Read_SetDataBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetDataBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetDataBreakpointsResponse;

   procedure Read_SetExceptionBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExceptionBreakpointsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "exceptionOptions" then
               Read_DAP_ExceptionOptions_Vector
                 (S, V.all.exceptionOptions'Access);

            elsif Key = "filterOptions" then
               Read_DAP_ExceptionFilterOptions_Vector
                 (S, V.all.filterOptions'Access);

            elsif Key = "filters" then
               Read_DAP_String_Vector (S, V.all.filters'Access);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetExceptionBreakpointsArguments;

   procedure Read_SetExceptionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExceptionBreakpointsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_SetExceptionBreakpointsArguments
                 (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetExceptionBreakpointsRequest;

   procedure Read_SetExceptionBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExceptionBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetExceptionBreakpointsResponse;

   procedure Read_SetExpressionArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExpressionArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "expression" then
               Read_String (S, V.all.expression);

            elsif Key = "format" then
               Read_ValueFormat (S, V.all.format'Access);

            elsif Key = "frameId" then
               Read (S, V.all.frameId);

            elsif Key = "value" then
               Read_String (S, V.all.value);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetExpressionArguments;

   procedure Read_SetExpressionRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExpressionRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_SetExpressionArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetExpressionRequest;

   procedure Read_SetExpressionResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetExpressionResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetExpressionResponse;

   procedure Read_SetFunctionBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetFunctionBreakpointsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "breakpoints" then
               Read_DAP_FunctionBreakpoint_Vector
                 (S, V.all.breakpoints'Access);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetFunctionBreakpointsArguments;

   procedure Read_SetFunctionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetFunctionBreakpointsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_SetFunctionBreakpointsArguments
                 (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetFunctionBreakpointsRequest;

   procedure Read_SetFunctionBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetFunctionBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetFunctionBreakpointsResponse;

   procedure Read_SetInstructionBreakpointsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetInstructionBreakpointsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "breakpoints" then
               Read_DAP_InstructionBreakpoint_Vector
                 (S, V.all.breakpoints'Access);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetInstructionBreakpointsArguments;

   procedure Read_SetInstructionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetInstructionBreakpointsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_SetInstructionBreakpointsArguments
                 (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetInstructionBreakpointsRequest;

   procedure Read_SetInstructionBreakpointsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetInstructionBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetInstructionBreakpointsResponse;

   procedure Read_SetVariableArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetVariableArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "format" then
               Read_ValueFormat (S, V.all.format'Access);

            elsif Key = "name" then
               Read_String (S, V.all.name);

            elsif Key = "value" then
               Read_String (S, V.all.value);

            elsif Key = "variablesReference" then
               Read (S, V.all.variablesReference);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetVariableArguments;

   procedure Read_SetVariableRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetVariableRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_SetVariableArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetVariableRequest;

   procedure Read_SetVariableResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SetVariableResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetVariableResponse;

   procedure Read_SourceArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_SourceArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "source" then
               Read_Source (S, V.all.a_source'Access);

            elsif Key = "sourceReference" then
               Read (S, V.all.sourceReference);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SourceArguments;

   procedure Read_SourceRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_SourceRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_SourceArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SourceRequest;

   procedure Read_SourceResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_SourceResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SourceResponse;

   procedure Read_StackFrame
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StackFrame)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "canRestart" then
               Read_Boolean (JS, V.all.canRestart);

            elsif Key = "column" then
               Read (S, V.all.column);

            elsif Key = "endColumn" then
               Read (S, V.all.endColumn);

            elsif Key = "endLine" then
               Read (S, V.all.endLine);

            elsif Key = "id" then
               Read (S, V.all.id);

            elsif Key = "instructionPointerReference" then
               Read_String (S, V.all.instructionPointerReference);

            elsif Key = "line" then
               Read (S, V.all.line);

            elsif Key = "moduleId" then
               Read_LSP_Number_Or_String (S, V.all.moduleId);

            elsif Key = "name" then
               Read_String (S, V.all.name);

            elsif Key = "presentationHint" then
               Read_String (S, V.all.presentationHint);

            elsif Key = "source" then
               Read_Source (S, V.all.a_source'Access);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StackFrame;

   procedure Read_StackFrameFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackFrameFormat)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "includeAll" then
               Read_Boolean (JS, V.all.includeAll);

            elsif Key = "line" then
               Read_Boolean (JS, V.all.line);

            elsif Key = "module" then
               Read_Boolean (JS, V.all.module);

            elsif Key = "parameterNames" then
               Read_Boolean (JS, V.all.parameterNames);

            elsif Key = "parameterTypes" then
               Read_Boolean (JS, V.all.parameterTypes);

            elsif Key = "parameterValues" then
               Read_Boolean (JS, V.all.parameterValues);

            elsif Key = "parameters" then
               Read_Boolean (JS, V.all.parameters);

            elsif Key = "hex" then
               Read_Boolean (JS, V.all.hex);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StackFrameFormat;

   procedure Read_StackTraceArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackTraceArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "format" then
               Read_StackFrameFormat (S, V.all.format'Access);

            elsif Key = "levels" then
               Read (S, V.all.levels);

            elsif Key = "startFrame" then
               Read (S, V.all.startFrame);

            elsif Key = "threadId" then
               Read (S, V.all.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StackTraceArguments;

   procedure Read_StackTraceRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackTraceRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_StackTraceArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StackTraceRequest;

   procedure Read_StackTraceResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StackTraceResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StackTraceResponse;

   procedure Read_StepBackArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepBackArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "granularity" then
               Read_SteppingGranularity (S, V.all.granularity'Access);

            elsif Key = "threadId" then
               Read (S, V.all.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepBackArguments;

   procedure Read_StepBackRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepBackRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_StepBackArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepBackRequest;

   procedure Read_StepBackResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepBackResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepBackResponse;

   procedure Read_StepInArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "granularity" then
               Read_SteppingGranularity (S, V.all.granularity'Access);

            elsif Key = "targetId" then
               Read (S, V.all.targetId);

            elsif Key = "threadId" then
               Read (S, V.all.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepInArguments;

   procedure Read_StepInRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StepInRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_StepInArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepInRequest;

   procedure Read_StepInResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StepInResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepInResponse;

   procedure Read_StepInTargetsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInTargetsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_StepInTargetsArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepInTargetsRequest;

   procedure Read_StepInTargetsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepInTargetsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepInTargetsResponse;

   procedure Read_StepOutArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepOutArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "granularity" then
               Read_SteppingGranularity (S, V.all.granularity'Access);

            elsif Key = "threadId" then
               Read (S, V.all.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepOutArguments;

   procedure Read_StepOutRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StepOutRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_StepOutArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepOutRequest;

   procedure Read_StepOutResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_StepOutResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepOutResponse;

   procedure Read_StoppedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_StoppedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StoppedEvent;

   procedure Read_TerminateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_TerminateArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TerminateRequest;

   procedure Read_TerminateResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TerminateResponse;

   procedure Read_TerminateThreadsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateThreadsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "threadIds" then
               Read_DAP_Integer_Vector (S, V.all.threadIds'Access);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TerminateThreadsArguments;

   procedure Read_TerminateThreadsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateThreadsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_TerminateThreadsArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TerminateThreadsRequest;

   procedure Read_TerminateThreadsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminateThreadsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_Any (S, V.all.a_body);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TerminateThreadsResponse;

   procedure Read_TerminatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_TerminatedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TerminatedEvent;

   procedure Read_ThreadEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ThreadEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "event" then
               Read_String (S, V.all.event);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ThreadEvent;

   procedure Read_ThreadsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_ThreadsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "arguments" then
               Read_Any (S, V.all.arguments);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ThreadsRequest;

   procedure Read_ThreadsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_ThreadsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ThreadsResponse;

   procedure Read_VariablePresentationHint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablePresentationHint)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "attributes" then
               Read_DAP_String_Vector (S, V.all.attributes'Access);

            elsif Key = "kind" then
               Read_String (S, V.all.kind);

            elsif Key = "visibility" then
               Read_String (S, V.all.visibility);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_VariablePresentationHint;

   procedure Read_Variable
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Access_Variable)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "evaluateName" then
               Read_String (S, V.all.evaluateName);

            elsif Key = "indexedVariables" then
               Read (S, V.all.indexedVariables);

            elsif Key = "memoryReference" then
               Read_String (S, V.all.memoryReference);

            elsif Key = "name" then
               Read_String (S, V.all.name);

            elsif Key = "namedVariables" then
               Read (S, V.all.namedVariables);

            elsif Key = "presentationHint" then
               Read_VariablePresentationHint
                 (S, V.all.presentationHint'Access);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "value" then
               Read_String (S, V.all.value);

            elsif Key = "variablesReference" then
               Read (S, V.all.variablesReference);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Variable;

   procedure Read_VariablesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablesArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "count" then
               Read (S, V.all.count);

            elsif Key = "filter" then
               Read_String (S, V.all.filter);

            elsif Key = "format" then
               Read_ValueFormat (S, V.all.format'Access);

            elsif Key = "start" then
               Read (S, V.all.start);

            elsif Key = "variablesReference" then
               Read (S, V.all.variablesReference);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_VariablesArguments;

   procedure Read_VariablesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablesRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "arguments" then
               Read_VariablesArguments (S, V.all.arguments'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_VariablesRequest;

   procedure Read_VariablesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_VariablesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "body" then
               Read_DAP_String_Map (S, V.all.a_body'Access);

            elsif Key = "command" then
               Read_String (S, V.all.command);

            elsif Key = "message" then
               Read_String (S, V.all.message);

            elsif Key = "request_seq" then
               Read (S, V.all.request_seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.all.success);

            elsif Key = "type" then
               Read_String (S, V.all.a_type);

            elsif Key = "seq" then
               Read (S, V.all.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_VariablesResponse;

end DAP.Tools;
