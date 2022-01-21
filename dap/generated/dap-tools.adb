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

   overriding procedure Finalize (Self : in out ExceptionDetails) is
   begin
      for E of Self.innerException loop
         Free (E);
      end loop;
   end Finalize;

   overriding procedure Finalize (Self : in out LoadedSourcesResponse) is
   begin
      for E of Self.body_sources loop
         Free (E);
      end loop;
   end Finalize;

   overriding procedure Finalize (Self : in out Source) is
   begin
      for E of Self.sources loop
         Free (E);
      end loop;
   end Finalize;

   package body Enums is
      procedure Read_ChecksumAlgorithm
        (S :     access Ada.Streams.Root_Stream_Type'Class;
         V : out ChecksumAlgorithm)
      is
         JS : LSP.JSON_Streams.JSON_Stream'Class renames
           LSP.JSON_Streams.JSON_Stream'Class (S.all);

         Text : constant Standard.String :=
           VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
      begin
         JS.R.Read_Next;
         if Text = "MD5" then
            V := Enums.MD5;
         elsif Text = "SHA1" then
            V := Enums.SHA1;
         elsif Text = "SHA256" then
            V := Enums.SHA256;
         else
            V := Enums.ChecksumAlgorithm'First;
         end if;
      end Read_ChecksumAlgorithm;

      procedure Write_ChecksumAlgorithm
        (S : access Ada.Streams.Root_Stream_Type'Class; V : ChecksumAlgorithm)
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
         JS.Write_String (To_String (V));
      end Write_ChecksumAlgorithm;

      procedure Read_CompletionItemType
        (S :     access Ada.Streams.Root_Stream_Type'Class;
         V : out CompletionItemType)
      is
         JS : LSP.JSON_Streams.JSON_Stream'Class renames
           LSP.JSON_Streams.JSON_Stream'Class (S.all);

         Text : constant Standard.String :=
           VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
      begin
         JS.R.Read_Next;
         if Text = "method" then
            V := Enums.method;
         elsif Text = "function" then
            V := Enums.a_function;
         elsif Text = "constructor" then
            V := Enums.constructor;
         elsif Text = "field" then
            V := Enums.field;
         elsif Text = "variable" then
            V := Enums.variable;
         elsif Text = "class" then
            V := Enums.class;
         elsif Text = "interface" then
            V := Enums.a_interface;
         elsif Text = "module" then
            V := Enums.module;
         elsif Text = "property" then
            V := Enums.property;
         elsif Text = "unit" then
            V := Enums.unit;
         elsif Text = "value" then
            V := Enums.value;
         elsif Text = "enum" then
            V := Enums.enum;
         elsif Text = "keyword" then
            V := Enums.keyword;
         elsif Text = "snippet" then
            V := Enums.snippet;
         elsif Text = "text" then
            V := Enums.text;
         elsif Text = "color" then
            V := Enums.color;
         elsif Text = "file" then
            V := Enums.file;
         elsif Text = "reference" then
            V := Enums.reference;
         else
            V := Enums.CompletionItemType'First;
         end if;
      end Read_CompletionItemType;

      procedure Write_CompletionItemType
        (S : access Ada.Streams.Root_Stream_Type'Class; V : CompletionItemType)
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
         JS.Write_String (To_String (V));
      end Write_CompletionItemType;

      procedure Read_DataBreakpointAccessType
        (S :     access Ada.Streams.Root_Stream_Type'Class;
         V : out DataBreakpointAccessType)
      is
         JS : LSP.JSON_Streams.JSON_Stream'Class renames
           LSP.JSON_Streams.JSON_Stream'Class (S.all);

         Text : constant Standard.String :=
           VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
      begin
         JS.R.Read_Next;
         if Text = "read" then
            V := Enums.read;
         elsif Text = "write" then
            V := Enums.write;
         else
            V := Enums.DataBreakpointAccessType'First;
         end if;
      end Read_DataBreakpointAccessType;

      procedure Write_DataBreakpointAccessType
        (S : access Ada.Streams.Root_Stream_Type'Class;
         V : DataBreakpointAccessType)
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
         JS.Write_String (To_String (V));
      end Write_DataBreakpointAccessType;

      procedure Read_ExceptionBreakMode
        (S :     access Ada.Streams.Root_Stream_Type'Class;
         V : out ExceptionBreakMode)
      is
         JS : LSP.JSON_Streams.JSON_Stream'Class renames
           LSP.JSON_Streams.JSON_Stream'Class (S.all);

         Text : constant Standard.String :=
           VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
      begin
         JS.R.Read_Next;
         if Text = "never" then
            V := Enums.never;
         elsif Text = "always" then
            V := Enums.always;
         elsif Text = "unhandled" then
            V := Enums.unhandled;
         else
            V := Enums.ExceptionBreakMode'First;
         end if;
      end Read_ExceptionBreakMode;

      procedure Write_ExceptionBreakMode
        (S : access Ada.Streams.Root_Stream_Type'Class; V : ExceptionBreakMode)
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
         JS.Write_String (To_String (V));
      end Write_ExceptionBreakMode;

      procedure Read_InvalidatedAreas
        (S :     access Ada.Streams.Root_Stream_Type'Class;
         V : out InvalidatedAreas)
      is
         JS : LSP.JSON_Streams.JSON_Stream'Class renames
           LSP.JSON_Streams.JSON_Stream'Class (S.all);

         Text : constant Standard.String :=
           VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
      begin
         JS.R.Read_Next;
         if Text = "all" then
            V := Enums.a_all;
         elsif Text = "stacks" then
            V := Enums.stacks;
         elsif Text = "threads" then
            V := Enums.threads;
         else
            V := Enums.InvalidatedAreas'First;
         end if;
      end Read_InvalidatedAreas;

      procedure Write_InvalidatedAreas
        (S : access Ada.Streams.Root_Stream_Type'Class; V : InvalidatedAreas)
      is
         JS : LSP.JSON_Streams.JSON_Stream'Class renames
           LSP.JSON_Streams.JSON_Stream'Class (S.all);

         function To_String
           (Value : Enums.InvalidatedAreas) return Virtual_String;

         function To_String
           (Value : Enums.InvalidatedAreas) return Virtual_String
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
         JS.Write_String (To_String (V));
      end Write_InvalidatedAreas;

      procedure Read_SteppingGranularity
        (S :     access Ada.Streams.Root_Stream_Type'Class;
         V : out SteppingGranularity)
      is
         JS : LSP.JSON_Streams.JSON_Stream'Class renames
           LSP.JSON_Streams.JSON_Stream'Class (S.all);

         Text : constant Standard.String :=
           VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
      begin
         JS.R.Read_Next;
         if Text = "statement" then
            V := Enums.statement;
         elsif Text = "line" then
            V := Enums.line;
         else
            V := Enums.SteppingGranularity'First;
         end if;
      end Read_SteppingGranularity;

      procedure Write_SteppingGranularity
        (S : access Ada.Streams.Root_Stream_Type'Class;
         V : SteppingGranularity)
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
         JS.Write_String (To_String (V));
      end Write_SteppingGranularity;

   end Enums;

   procedure Write_DAP_String_Map
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_String_Map)
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
         Iterate (V, WMap_Item'Access);
      end;
      JS.End_Object;
   end Write_DAP_String_Map;

   procedure Write_DAP_String_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_String_Vector)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_Integer_Vector)
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

   procedure Write_DAP_BreakpointLocation_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_BreakpointLocation_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         BreakpointLocation'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_BreakpointLocation_Vector;

   procedure Write_DAP_Breakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_Breakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Breakpoint'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_Breakpoint_Vector;

   procedure Write_DAP_ChecksumAlgorithm_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_ChecksumAlgorithm_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Enums.ChecksumAlgorithm'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_ChecksumAlgorithm_Vector;

   procedure Write_DAP_Checksum_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_Checksum_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Checksum'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_Checksum_Vector;

   procedure Write_DAP_ColumnDescriptor_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_ColumnDescriptor_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         ColumnDescriptor'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_ColumnDescriptor_Vector;

   procedure Write_DAP_CompletionItem_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_CompletionItem_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         CompletionItem'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_CompletionItem_Vector;

   procedure Write_DAP_DataBreakpointAccessType_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_DataBreakpointAccessType_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Enums.DataBreakpointAccessType'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_DataBreakpointAccessType_Vector;

   procedure Write_DAP_DataBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_DataBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         DataBreakpoint'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_DataBreakpoint_Vector;

   procedure Write_DAP_DisassembledInstruction_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_DisassembledInstruction_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         DisassembledInstruction'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_DisassembledInstruction_Vector;

   procedure Write_DAP_ExceptionBreakpointsFilter_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_ExceptionBreakpointsFilter_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         ExceptionBreakpointsFilter'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_ExceptionBreakpointsFilter_Vector;

   procedure Write_DAP_ExceptionDetails_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_ExceptionDetails_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         ExceptionDetails'Write (S, V.Element (J).all);
      end loop;
      JS.End_Array;
   end Write_DAP_ExceptionDetails_Vector;

   procedure Write_DAP_ExceptionFilterOptions_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_ExceptionFilterOptions_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         ExceptionFilterOptions'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_ExceptionFilterOptions_Vector;

   procedure Write_DAP_ExceptionOptions_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_ExceptionOptions_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         ExceptionOptions'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_ExceptionOptions_Vector;

   procedure Write_DAP_ExceptionPathSegment_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_ExceptionPathSegment_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         ExceptionPathSegment'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_ExceptionPathSegment_Vector;

   procedure Write_DAP_FunctionBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_FunctionBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         FunctionBreakpoint'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_FunctionBreakpoint_Vector;

   procedure Write_DAP_GotoTarget_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_GotoTarget_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         GotoTarget'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_GotoTarget_Vector;

   procedure Write_DAP_InstructionBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_InstructionBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         InstructionBreakpoint'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_InstructionBreakpoint_Vector;

   procedure Write_DAP_InvalidatedAreas_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_InvalidatedAreas_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Enums.InvalidatedAreas'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_InvalidatedAreas_Vector;

   procedure Write_DAP_Module_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_Module_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Module'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_Module_Vector;

   procedure Write_DAP_Scope_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_Scope_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Scope'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_Scope_Vector;

   procedure Write_DAP_SourceBreakpoint_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_SourceBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         SourceBreakpoint'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_SourceBreakpoint_Vector;

   procedure Write_DAP_Source_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_Source_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Source'Write (S, V.Element (J).all);
      end loop;
      JS.End_Array;
   end Write_DAP_Source_Vector;

   procedure Write_DAP_StackFrame_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_StackFrame_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         StackFrame'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_StackFrame_Vector;

   procedure Write_DAP_StepInTarget_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DAP_StepInTarget_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         StepInTarget'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_StepInTarget_Vector;

   procedure Write_DAP_Thread_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_Thread_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Thread'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_Thread_Vector;

   procedure Write_DAP_Variable_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DAP_Variable_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;
      for J in V.First_Index .. V.Last_Index loop
         Variable'Write (S, V.Element (J));
      end loop;
      JS.End_Array;
   end Write_DAP_Variable_Vector;

   procedure Write_AttachRequestArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AttachRequestArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("__restart");
      LSP_Any'Write (S, V.a_restart);

      JS.End_Object;
   end Write_AttachRequestArguments;

   procedure Write_BreakpointLocation
     (S : access Ada.Streams.Root_Stream_Type'Class; V : BreakpointLocation)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CancelArguments)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ColumnDescriptor)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CompletionsArguments)
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
      V : ConfigurationDoneArguments)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ContinueArguments)
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
      V : DataBreakpointInfoArguments)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DisassembleArguments)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DisconnectArguments)
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
      V : ExceptionBreakpointsFilter)
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
      V : ExceptionFilterOptions)
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
      V : ExceptionInfoArguments)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : FunctionBreakpoint)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : GotoArguments)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : GotoTarget)
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
      V : InitializeRequestArguments)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : InstructionBreakpoint)
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
      V : LaunchRequestArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("__restart");
      LSP_Any'Write (S, V.a_restart);

      JS.Key ("noDebug");
      JS.Write_Boolean (V.noDebug);

      JS.Key ("program");
      JS.Write_String (V.program);

      JS.Key ("verbose");
      JS.Write_Boolean (True);

      JS.Key ("logFile");
      JS.Write_String ("/tmp/log1.out");

      JS.End_Object;
   end Write_LaunchRequestArguments;

   procedure Write_LoadedSourcesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LoadedSourcesArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      --  this record is empty
      pragma Unreferenced (V);
      JS.Start_Object;
      JS.End_Object;
   end Write_LoadedSourcesArguments;

   procedure Write_Message
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Message)
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
      DAP_String_Map'Write (S, V.variables);

      JS.End_Object;
   end Write_Message;

   procedure Write_Module
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Module)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ModulesArguments)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : PauseArguments)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ProtocolMessage)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ReadMemoryArguments)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : RestartArguments)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : RestartFrameArguments)
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
      V : ReverseContinueArguments)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ScopesArguments)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SourceBreakpoint)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("column");
      JS.Write_Integer (Interfaces.Integer_64 (V.column));

      JS.Key ("line");
      JS.Write_Integer (Interfaces.Integer_64 (V.line));

      JS.End_Object;
   end Write_SourceBreakpoint;

   procedure Write_StackTraceArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StackTraceArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("levels");
      JS.Write_Integer (Interfaces.Integer_64 (V.levels));

      JS.Key ("startFrame");
      JS.Write_Integer (Interfaces.Integer_64 (V.startFrame));

      JS.Key ("threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.threadId));

      JS.End_Object;
   end Write_StackTraceArguments;

   procedure Write_StepInTarget
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepInTarget)
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
      V : StepInTargetsArguments)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : TerminateArguments)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Thread)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ValueFormat)
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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      LSP_Any'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_Request;

   procedure Write_AttachRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : AttachRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      AttachRequestArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_AttachRequest;

   procedure Write_Response
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Response)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : AttachResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_AttachResponse;

   procedure Write_Checksum
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Checksum)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("algorithm");
      Enums.ChecksumAlgorithm'Write (S, V.algorithm);

      JS.Key ("checksum");
      JS.Write_String (V.checksum);

      JS.End_Object;
   end Write_Checksum;

   procedure Write_Source
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Source)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("adapterData");
      LSP_Any'Write (S, V.adapterData);

      JS.Key ("checksums");
      DAP_Checksum_Vector'Write (S, V.checksums);

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
      DAP_Source_Vector'Write (S, V.sources);

      JS.End_Object;
   end Write_Source;

   procedure Write_Breakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Breakpoint)
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
      JS.Write_String (V.a_message);

      JS.Key ("offset");
      JS.Write_Integer (Interfaces.Integer_64 (V.offset));

      JS.Key ("source");
      Source'Write (S, V.a_source);

      JS.Key ("verified");
      JS.Write_Boolean (V.verified);

      JS.End_Object;
   end Write_Breakpoint;

   procedure Write_Event
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Event)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_Event;

   procedure Write_BreakpointEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : BreakpointEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body_breakpoint");
      Breakpoint'Write (S, V.body_breakpoint);

      JS.Key ("body_reason");
      JS.Write_String (V.body_reason);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_BreakpointEvent;

   procedure Write_BreakpointLocationsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : BreakpointLocationsArguments)
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
      Source'Write (S, V.a_source);

      JS.End_Object;
   end Write_BreakpointLocationsArguments;

   procedure Write_BreakpointLocationsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : BreakpointLocationsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      BreakpointLocationsArguments'Write (S, V.arguments);

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
      V : BreakpointLocationsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_breakpoints");
      DAP_BreakpointLocation_Vector'Write (S, V.body_breakpoints);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CancelRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      CancelArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_CancelRequest;

   procedure Write_CancelResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CancelResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_CancelResponse;

   procedure Write_Capabilities
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Capabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("additionalModuleColumns");
      DAP_ColumnDescriptor_Vector'Write (S, V.additionalModuleColumns);

      JS.Key ("completionTriggerCharacters");
      DAP_String_Vector'Write (S, V.completionTriggerCharacters);

      JS.Key ("exceptionBreakpointFilters");
      DAP_ExceptionBreakpointsFilter_Vector'Write
        (S, V.exceptionBreakpointFilters);

      JS.Key ("supportTerminateDebuggee");
      JS.Write_Boolean (V.supportTerminateDebuggee);

      JS.Key ("supportedChecksumAlgorithms");
      DAP_ChecksumAlgorithm_Vector'Write (S, V.supportedChecksumAlgorithms);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CapabilitiesEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body_capabilities");
      Capabilities'Write (S, V.body_capabilities);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_CapabilitiesEvent;

   procedure Write_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CompletionItem)
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
      Enums.CompletionItemType'Write (S, V.a_type);

      JS.End_Object;
   end Write_CompletionItem;

   procedure Write_CompletionsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CompletionsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      CompletionsArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_CompletionsRequest;

   procedure Write_CompletionsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : CompletionsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_targets");
      DAP_CompletionItem_Vector'Write (S, V.body_targets);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
      V : ConfigurationDoneRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      ConfigurationDoneArguments'Write (S, V.arguments);

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
      V : ConfigurationDoneResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_ConfigurationDoneResponse;

   procedure Write_ContinueRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ContinueRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      ContinueArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ContinueRequest;

   procedure Write_ContinueResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ContinueResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_allThreadsContinued");
      JS.Write_Boolean (V.body_allThreadsContinued);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ContinuedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body_allThreadsContinued");
      JS.Write_Boolean (V.body_allThreadsContinued);

      JS.Key ("body_threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_threadId));

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ContinuedEvent;

   procedure Write_DataBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DataBreakpoint)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("accessType");
      Enums.DataBreakpointAccessType'Write (S, V.accessType);

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
      V : DataBreakpointInfoRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      DataBreakpointInfoArguments'Write (S, V.arguments);

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
      V : DataBreakpointInfoResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_accessTypes");
      DAP_DataBreakpointAccessType_Vector'Write (S, V.body_accessTypes);

      JS.Key ("body_canPersist");
      JS.Write_Boolean (V.body_canPersist);

      JS.Key ("body_dataId");
      JS.Write_String (V.body_dataId);

      JS.Key ("body_description");
      JS.Write_String (V.body_description);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DisassembleRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      DisassembleArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_DisassembleRequest;

   procedure Write_DisassembledInstruction
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DisassembledInstruction)
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
      Source'Write (S, V.location);

      JS.Key ("symbol");
      JS.Write_String (V.symbol);

      JS.End_Object;
   end Write_DisassembledInstruction;

   procedure Write_DisassembleResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DisassembleResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_instructions");
      DAP_DisassembledInstruction_Vector'Write (S, V.body_instructions);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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

   procedure Write_DisconnectRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DisconnectRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      DisconnectArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_DisconnectRequest;

   procedure Write_DisconnectResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : DisconnectResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_DisconnectResponse;

   procedure Write_ErrorResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ErrorResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_error");
      Message'Write (S, V.body_error);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : EvaluateArguments)
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
      ValueFormat'Write (S, V.format);

      JS.Key ("frameId");
      JS.Write_Integer (Interfaces.Integer_64 (V.frameId));

      JS.End_Object;
   end Write_EvaluateArguments;

   procedure Write_EvaluateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : EvaluateRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      EvaluateArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_EvaluateRequest;

   procedure Write_VariablePresentationHint
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : VariablePresentationHint)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("attributes");
      DAP_String_Vector'Write (S, V.attributes);

      JS.Key ("kind");
      JS.Write_String (V.kind);

      JS.Key ("visibility");
      JS.Write_String (V.visibility);

      JS.End_Object;
   end Write_VariablePresentationHint;

   procedure Write_EvaluateResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : EvaluateResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_indexedVariables");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_indexedVariables));

      JS.Key ("body_memoryReference");
      JS.Write_String (V.body_memoryReference);

      JS.Key ("body_namedVariables");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_namedVariables));

      JS.Key ("body_presentationHint");
      VariablePresentationHint'Write (S, V.body_presentationHint);

      JS.Key ("body_result");
      JS.Write_String (V.body_result);

      JS.Key ("body_type");
      JS.Write_String (V.body_type);

      JS.Key ("body_variablesReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_variablesReference));

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ExceptionDetails)
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
      DAP_ExceptionDetails_Vector'Write (S, V.innerException);

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("stackTrace");
      JS.Write_String (V.stackTrace);

      JS.Key ("typeName");
      JS.Write_String (V.typeName);

      JS.End_Object;
   end Write_ExceptionDetails;

   procedure Write_ExceptionInfoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ExceptionInfoRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      ExceptionInfoArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ExceptionInfoRequest;

   procedure Write_ExceptionInfoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ExceptionInfoResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_breakMode");
      Enums.ExceptionBreakMode'Write (S, V.body_breakMode);

      JS.Key ("body_description");
      JS.Write_String (V.body_description);

      JS.Key ("body_details");
      ExceptionDetails'Write (S, V.body_details);

      JS.Key ("body_exceptionId");
      JS.Write_String (V.body_exceptionId);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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

   procedure Write_ExceptionPathSegment
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ExceptionPathSegment)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("names");
      DAP_String_Vector'Write (S, V.names);

      JS.Key ("negate");
      JS.Write_Boolean (V.negate);

      JS.End_Object;
   end Write_ExceptionPathSegment;

   procedure Write_ExceptionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ExceptionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("breakMode");
      Enums.ExceptionBreakMode'Write (S, V.breakMode);

      JS.Key ("path");
      DAP_ExceptionPathSegment_Vector'Write (S, V.path);

      JS.End_Object;
   end Write_ExceptionOptions;

   procedure Write_ExitedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ExitedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body_exitCode");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_exitCode));

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ExitedEvent;

   procedure Write_GotoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : GotoRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      GotoArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_GotoRequest;

   procedure Write_GotoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : GotoResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_GotoResponse;

   procedure Write_GotoTargetsArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : GotoTargetsArguments)
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
      Source'Write (S, V.a_source);

      JS.End_Object;
   end Write_GotoTargetsArguments;

   procedure Write_GotoTargetsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : GotoTargetsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      GotoTargetsArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_GotoTargetsRequest;

   procedure Write_GotoTargetsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : GotoTargetsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_targets");
      DAP_GotoTarget_Vector'Write (S, V.body_targets);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : InitializeRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      InitializeRequestArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_InitializeRequest;

   procedure Write_InitializeResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : InitializeResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : InitializedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_InitializedEvent;

   procedure Write_InvalidatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : InvalidatedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body_areas");
      DAP_InvalidatedAreas_Vector'Write (S, V.body_areas);

      JS.Key ("body_stackFrameId");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_stackFrameId));

      JS.Key ("body_threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_threadId));

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_InvalidatedEvent;

   procedure Write_LaunchRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : LaunchRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      LaunchRequestArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_LaunchRequest;

   procedure Write_LaunchResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : LaunchResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_LaunchResponse;

   procedure Write_LoadedSourceEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : LoadedSourceEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body_reason");
      JS.Write_String (V.body_reason);

      JS.Key ("body_source");
      Source'Write (S, V.body_source);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_LoadedSourceEvent;

   procedure Write_LoadedSourcesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : LoadedSourcesRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      LoadedSourcesArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_LoadedSourcesRequest;

   procedure Write_LoadedSourcesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : LoadedSourcesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_sources");
      DAP_Source_Vector'Write (S, V.body_sources);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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

   procedure Write_ModuleEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ModuleEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body_module");
      Module'Write (S, V.body_module);

      JS.Key ("body_reason");
      JS.Write_String (V.body_reason);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ModuleEvent;

   procedure Write_ModulesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ModulesRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      ModulesArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ModulesRequest;

   procedure Write_ModulesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ModulesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_modules");
      DAP_Module_Vector'Write (S, V.body_modules);

      JS.Key ("body_totalModules");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_totalModules));

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ModulesViewDescriptor)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("columns");
      DAP_ColumnDescriptor_Vector'Write (S, V.columns);

      JS.End_Object;
   end Write_ModulesViewDescriptor;

   procedure Write_NextArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : NextArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("granularity");
      Enums.SteppingGranularity'Write (S, V.granularity);

      JS.Key ("threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.threadId));

      JS.End_Object;
   end Write_NextArguments;

   procedure Write_NextRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : NextRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      NextArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_NextRequest;

   procedure Write_NextResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : NextResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_NextResponse;

   procedure Write_OutputEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : OutputEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body_category");
      JS.Write_String (V.body_category);

      JS.Key ("body_column");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_column));

      JS.Key ("body_data");
      LSP_Any'Write (S, V.body_data);

      JS.Key ("body_group");
      JS.Write_String (V.body_group);

      JS.Key ("body_line");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_line));

      JS.Key ("body_output");
      JS.Write_String (V.body_output);

      JS.Key ("body_source");
      Source'Write (S, V.body_source);

      JS.Key ("body_variablesReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_variablesReference));

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_OutputEvent;

   procedure Write_PauseRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : PauseRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      PauseArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_PauseRequest;

   procedure Write_PauseResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : PauseResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_PauseResponse;

   procedure Write_ProcessEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ProcessEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body_isLocalProcess");
      JS.Write_Boolean (V.body_isLocalProcess);

      JS.Key ("body_name");
      JS.Write_String (V.body_name);

      JS.Key ("body_pointerSize");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_pointerSize));

      JS.Key ("body_startMethod");
      JS.Write_String (V.body_startMethod);

      JS.Key ("body_systemProcessId");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_systemProcessId));

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ProcessEvent;

   procedure Write_ProgressEndEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ProgressEndEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body_message");
      JS.Write_String (V.body_message);

      JS.Key ("body_progressId");
      JS.Write_String (V.body_progressId);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ProgressEndEvent;

   procedure Write_ProgressStartEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ProgressStartEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body_cancellable");
      JS.Write_Boolean (V.body_cancellable);

      JS.Key ("body_message");
      JS.Write_String (V.body_message);

      JS.Key ("body_percentage");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_percentage));

      JS.Key ("body_progressId");
      JS.Write_String (V.body_progressId);

      JS.Key ("body_requestId");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_requestId));

      JS.Key ("body_title");
      JS.Write_String (V.body_title);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ProgressStartEvent;

   procedure Write_ProgressUpdateEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ProgressUpdateEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body_message");
      JS.Write_String (V.body_message);

      JS.Key ("body_percentage");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_percentage));

      JS.Key ("body_progressId");
      JS.Write_String (V.body_progressId);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ProgressUpdateEvent;

   procedure Write_ReadMemoryRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ReadMemoryRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      ReadMemoryArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ReadMemoryRequest;

   procedure Write_ReadMemoryResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ReadMemoryResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_address");
      JS.Write_String (V.body_address);

      JS.Key ("body_data");
      JS.Write_String (V.body_data);

      JS.Key ("body_unreadableBytes");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_unreadableBytes));

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : RestartFrameRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      RestartFrameArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_RestartFrameRequest;

   procedure Write_RestartFrameResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : RestartFrameResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_RestartFrameResponse;

   procedure Write_RestartRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : RestartRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      RestartArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_RestartRequest;

   procedure Write_RestartResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : RestartResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

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
      V : ReverseContinueRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      ReverseContinueArguments'Write (S, V.arguments);

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
      V : ReverseContinueResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

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
      V : RunInTerminalRequestArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("args");
      DAP_String_Vector'Write (S, V.args);

      JS.Key ("cwd");
      JS.Write_String (V.cwd);

      JS.Key ("env");
      DAP_String_Map'Write (S, V.env);

      JS.Key ("kind");
      JS.Write_String (V.kind);

      JS.End_Object;
   end Write_RunInTerminalRequestArguments;

   procedure Write_RunInTerminalRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : RunInTerminalRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      RunInTerminalRequestArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_RunInTerminalRequest;

   procedure Write_RunInTerminalResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : RunInTerminalResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_processId");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_processId));

      JS.Key ("body_shellProcessId");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_shellProcessId));

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Scope)
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
      Source'Write (S, V.a_source);

      JS.Key ("variablesReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.variablesReference));

      JS.End_Object;
   end Write_Scope;

   procedure Write_ScopesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ScopesRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      ScopesArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ScopesRequest;

   procedure Write_ScopesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ScopesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_scopes");
      DAP_Scope_Vector'Write (S, V.body_scopes);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
      V : SetBreakpointsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("breakpoints");
      DAP_SourceBreakpoint_Vector'Write (S, V.breakpoints);

      JS.Key ("lines");
      DAP_Integer_Vector'Write (S, V.lines);

      JS.Key ("source");
      Source'Write (S, V.a_source);

      JS.Key ("sourceModified");
      JS.Write_Boolean (V.sourceModified);

      JS.End_Object;
   end Write_SetBreakpointsArguments;

   procedure Write_SetBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SetBreakpointsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      SetBreakpointsArguments'Write (S, V.arguments);

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
      V : SetBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_breakpoints");
      DAP_Breakpoint_Vector'Write (S, V.body_breakpoints);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
      V : SetDataBreakpointsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("breakpoints");
      DAP_DataBreakpoint_Vector'Write (S, V.breakpoints);

      JS.End_Object;
   end Write_SetDataBreakpointsArguments;

   procedure Write_SetDataBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetDataBreakpointsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      SetDataBreakpointsArguments'Write (S, V.arguments);

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
      V : SetDataBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_breakpoints");
      DAP_Breakpoint_Vector'Write (S, V.body_breakpoints);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
      V : SetExceptionBreakpointsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("exceptionOptions");
      DAP_ExceptionOptions_Vector'Write (S, V.exceptionOptions);

      JS.Key ("filterOptions");
      DAP_ExceptionFilterOptions_Vector'Write (S, V.filterOptions);

      JS.Key ("filters");
      DAP_String_Vector'Write (S, V.filters);

      JS.End_Object;
   end Write_SetExceptionBreakpointsArguments;

   procedure Write_SetExceptionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetExceptionBreakpointsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      SetExceptionBreakpointsArguments'Write (S, V.arguments);

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
      V : SetExceptionBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

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
      V : SetExpressionArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("expression");
      JS.Write_String (V.expression);

      JS.Key ("format");
      ValueFormat'Write (S, V.format);

      JS.Key ("frameId");
      JS.Write_Integer (Interfaces.Integer_64 (V.frameId));

      JS.Key ("value");
      JS.Write_String (V.value);

      JS.End_Object;
   end Write_SetExpressionArguments;

   procedure Write_SetExpressionRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SetExpressionRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      SetExpressionArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SetExpressionRequest;

   procedure Write_SetExpressionResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SetExpressionResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_indexedVariables");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_indexedVariables));

      JS.Key ("body_namedVariables");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_namedVariables));

      JS.Key ("body_presentationHint");
      VariablePresentationHint'Write (S, V.body_presentationHint);

      JS.Key ("body_type");
      JS.Write_String (V.body_type);

      JS.Key ("body_value");
      JS.Write_String (V.body_value);

      JS.Key ("body_variablesReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_variablesReference));

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
      V : SetFunctionBreakpointsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("breakpoints");
      DAP_FunctionBreakpoint_Vector'Write (S, V.breakpoints);

      JS.End_Object;
   end Write_SetFunctionBreakpointsArguments;

   procedure Write_SetFunctionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetFunctionBreakpointsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      SetFunctionBreakpointsArguments'Write (S, V.arguments);

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
      V : SetFunctionBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_breakpoints");
      DAP_Breakpoint_Vector'Write (S, V.body_breakpoints);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
      V : SetInstructionBreakpointsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("breakpoints");
      DAP_InstructionBreakpoint_Vector'Write (S, V.breakpoints);

      JS.End_Object;
   end Write_SetInstructionBreakpointsArguments;

   procedure Write_SetInstructionBreakpointsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetInstructionBreakpointsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      SetInstructionBreakpointsArguments'Write (S, V.arguments);

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
      V : SetInstructionBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_breakpoints");
      DAP_Breakpoint_Vector'Write (S, V.body_breakpoints);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SetVariableArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("format");
      ValueFormat'Write (S, V.format);

      JS.Key ("name");
      JS.Write_String (V.name);

      JS.Key ("value");
      JS.Write_String (V.value);

      JS.Key ("variablesReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.variablesReference));

      JS.End_Object;
   end Write_SetVariableArguments;

   procedure Write_SetVariableRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SetVariableRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      SetVariableArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SetVariableRequest;

   procedure Write_SetVariableResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SetVariableResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_indexedVariables");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_indexedVariables));

      JS.Key ("body_namedVariables");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_namedVariables));

      JS.Key ("body_type");
      JS.Write_String (V.body_type);

      JS.Key ("body_value");
      JS.Write_String (V.body_value);

      JS.Key ("body_variablesReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_variablesReference));

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SourceArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("source");
      Source'Write (S, V.a_source);

      JS.Key ("sourceReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.sourceReference));

      JS.End_Object;
   end Write_SourceArguments;

   procedure Write_SourceRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SourceRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      SourceArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_SourceRequest;

   procedure Write_SourceResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : SourceResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_content");
      JS.Write_String (V.body_content);

      JS.Key ("body_mimeType");
      JS.Write_String (V.body_mimeType);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StackFrame)
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
      Source'Write (S, V.a_source);

      JS.End_Object;
   end Write_StackFrame;

   procedure Write_StackFrameFormat
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StackFrameFormat)
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

   procedure Write_StackTraceRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StackTraceRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      StackTraceArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_StackTraceRequest;

   procedure Write_StackTraceResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StackTraceResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_stackFrames");
      DAP_StackFrame_Vector'Write (S, V.body_stackFrames);

      JS.Key ("body_totalFrames");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_totalFrames));

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepBackArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("granularity");
      Enums.SteppingGranularity'Write (S, V.granularity);

      JS.Key ("threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.threadId));

      JS.End_Object;
   end Write_StepBackArguments;

   procedure Write_StepBackRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepBackRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      StepBackArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_StepBackRequest;

   procedure Write_StepBackResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepBackResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_StepBackResponse;

   procedure Write_StepInArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepInArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("granularity");
      Enums.SteppingGranularity'Write (S, V.granularity);

      JS.Key ("targetId");
      JS.Write_Integer (Interfaces.Integer_64 (V.targetId));

      JS.Key ("threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.threadId));

      JS.End_Object;
   end Write_StepInArguments;

   procedure Write_StepInRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepInRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      StepInArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_StepInRequest;

   procedure Write_StepInResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepInResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_StepInResponse;

   procedure Write_StepInTargetsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepInTargetsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      StepInTargetsArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_StepInTargetsRequest;

   procedure Write_StepInTargetsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepInTargetsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_targets");
      DAP_StepInTarget_Vector'Write (S, V.body_targets);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepOutArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("granularity");
      Enums.SteppingGranularity'Write (S, V.granularity);

      JS.Key ("threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.threadId));

      JS.End_Object;
   end Write_StepOutArguments;

   procedure Write_StepOutRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepOutRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      StepOutArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_StepOutRequest;

   procedure Write_StepOutResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StepOutResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_StepOutResponse;

   procedure Write_StoppedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : StoppedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body_allThreadsStopped");
      JS.Write_Boolean (V.body_allThreadsStopped);

      JS.Key ("body_description");
      JS.Write_String (V.body_description);

      JS.Key ("body_hitBreakpointIds");
      DAP_Integer_Vector'Write (S, V.body_hitBreakpointIds);

      JS.Key ("body_preserveFocusHint");
      JS.Write_Boolean (V.body_preserveFocusHint);

      JS.Key ("body_reason");
      JS.Write_String (V.body_reason);

      JS.Key ("body_text");
      JS.Write_String (V.body_text);

      JS.Key ("body_threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_threadId));

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_StoppedEvent;

   procedure Write_TerminateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : TerminateRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      TerminateArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_TerminateRequest;

   procedure Write_TerminateResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : TerminateResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

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
      V : TerminateThreadsArguments)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("threadIds");
      DAP_Integer_Vector'Write (S, V.threadIds);

      JS.End_Object;
   end Write_TerminateThreadsArguments;

   procedure Write_TerminateThreadsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TerminateThreadsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      TerminateThreadsArguments'Write (S, V.arguments);

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
      V : TerminateThreadsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("message");
      JS.Write_String (V.a_message);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("request_seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.request_seq));

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.Key ("success");
      JS.Write_Boolean (V.success);

      JS.End_Object;
   end Write_TerminateThreadsResponse;

   procedure Write_TerminatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : TerminatedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body_restart");
      LSP_Any'Write (S, V.body_restart);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_TerminatedEvent;

   procedure Write_ThreadEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ThreadEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("event");
      JS.Write_String (V.event);

      JS.Key ("body_reason");
      JS.Write_String (V.body_reason);

      JS.Key ("body_threadId");
      JS.Write_Integer (Interfaces.Integer_64 (V.body_threadId));

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ThreadEvent;

   procedure Write_ThreadsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ThreadsRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("arguments");
      LSP_Any'Write (S, V.arguments);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_ThreadsRequest;

   procedure Write_ThreadsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : ThreadsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_threads");
      DAP_Thread_Vector'Write (S, V.body_threads);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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

   procedure Write_Variable
     (S : access Ada.Streams.Root_Stream_Type'Class; V : Variable)
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
      VariablePresentationHint'Write (S, V.presentationHint);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("value");
      JS.Write_String (V.value);

      JS.Key ("variablesReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.variablesReference));

      JS.End_Object;
   end Write_Variable;

   procedure Write_VariablesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : VariablesArguments)
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
      ValueFormat'Write (S, V.format);

      JS.Key ("start");
      JS.Write_Integer (Interfaces.Integer_64 (V.start));

      JS.Key ("variablesReference");
      JS.Write_Integer (Interfaces.Integer_64 (V.variablesReference));

      JS.End_Object;
   end Write_VariablesArguments;

   procedure Write_VariablesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : VariablesRequest)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("arguments");
      VariablesArguments'Write (S, V.arguments);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("type");
      JS.Write_String (V.a_type);

      JS.Key ("seq");
      JS.Write_Integer (Interfaces.Integer_64 (V.seq));

      JS.End_Object;
   end Write_VariablesRequest;

   procedure Write_VariablesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : VariablesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;

      JS.Key ("body_variables");
      DAP_Variable_Vector'Write (S, V.body_variables);

      JS.Key ("command");
      JS.Write_String (V.command);

      JS.Key ("message");
      JS.Write_String (V.a_message);

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

   procedure Read_DAP_String_Map
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out DAP_String_Map)
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
            V.Insert (Key, Value);
         end;
      end loop;

      JS.R.Read_Next;
   end Read_DAP_String_Map;

   procedure Read_DAP_String_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out DAP_String_Vector)
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
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_Integer_Vector)
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

   procedure Read_DAP_BreakpointLocation_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_BreakpointLocation_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : BreakpointLocation;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         BreakpointLocation'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_BreakpointLocation_Vector;

   procedure Read_DAP_Breakpoint_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_Breakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Breakpoint;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         Breakpoint'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_Breakpoint_Vector;

   procedure Read_DAP_ChecksumAlgorithm_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_ChecksumAlgorithm_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Enums.ChecksumAlgorithm;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         Enums.ChecksumAlgorithm'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_ChecksumAlgorithm_Vector;

   procedure Read_DAP_Checksum_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_Checksum_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Checksum;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         Checksum'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_Checksum_Vector;

   procedure Read_DAP_ColumnDescriptor_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_ColumnDescriptor_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : ColumnDescriptor;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         ColumnDescriptor'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_ColumnDescriptor_Vector;

   procedure Read_DAP_CompletionItem_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_CompletionItem_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : CompletionItem;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         CompletionItem'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_CompletionItem_Vector;

   procedure Read_DAP_DataBreakpointAccessType_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_DataBreakpointAccessType_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Enums.DataBreakpointAccessType;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         Enums.DataBreakpointAccessType'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_DataBreakpointAccessType_Vector;

   procedure Read_DAP_DataBreakpoint_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_DataBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : DataBreakpoint;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         DataBreakpoint'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_DataBreakpoint_Vector;

   procedure Read_DAP_DisassembledInstruction_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_DisassembledInstruction_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : DisassembledInstruction;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         DisassembledInstruction'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_DisassembledInstruction_Vector;

   procedure Read_DAP_ExceptionBreakpointsFilter_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_ExceptionBreakpointsFilter_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : ExceptionBreakpointsFilter;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         ExceptionBreakpointsFilter'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_ExceptionBreakpointsFilter_Vector;

   procedure Read_DAP_ExceptionDetails_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_ExceptionDetails_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : constant Access_ExceptionDetails := new ExceptionDetails;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         ExceptionDetails'Read (S, T.all);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_ExceptionDetails_Vector;

   procedure Read_DAP_ExceptionFilterOptions_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_ExceptionFilterOptions_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : ExceptionFilterOptions;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         ExceptionFilterOptions'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_ExceptionFilterOptions_Vector;

   procedure Read_DAP_ExceptionOptions_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_ExceptionOptions_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : ExceptionOptions;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         ExceptionOptions'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_ExceptionOptions_Vector;

   procedure Read_DAP_ExceptionPathSegment_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_ExceptionPathSegment_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : ExceptionPathSegment;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         ExceptionPathSegment'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_ExceptionPathSegment_Vector;

   procedure Read_DAP_FunctionBreakpoint_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_FunctionBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : FunctionBreakpoint;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         FunctionBreakpoint'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_FunctionBreakpoint_Vector;

   procedure Read_DAP_GotoTarget_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_GotoTarget_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : GotoTarget;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         GotoTarget'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_GotoTarget_Vector;

   procedure Read_DAP_InstructionBreakpoint_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_InstructionBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : InstructionBreakpoint;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         InstructionBreakpoint'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_InstructionBreakpoint_Vector;

   procedure Read_DAP_InvalidatedAreas_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_InvalidatedAreas_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Enums.InvalidatedAreas;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         Enums.InvalidatedAreas'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_InvalidatedAreas_Vector;

   procedure Read_DAP_Module_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out DAP_Module_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Module;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         Module'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_Module_Vector;

   procedure Read_DAP_Scope_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out DAP_Scope_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Scope;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         Scope'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_Scope_Vector;

   procedure Read_DAP_SourceBreakpoint_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_SourceBreakpoint_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : SourceBreakpoint;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         SourceBreakpoint'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_SourceBreakpoint_Vector;

   procedure Read_DAP_Source_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out DAP_Source_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : constant Access_Source := new Source;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         Source'Read (S, T.all);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_Source_Vector;

   procedure Read_DAP_StackFrame_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_StackFrame_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : StackFrame;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         StackFrame'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_StackFrame_Vector;

   procedure Read_DAP_StepInTarget_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_StepInTarget_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : StepInTarget;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         StepInTarget'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_StepInTarget_Vector;

   procedure Read_DAP_Thread_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out DAP_Thread_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Thread;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         Thread'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_Thread_Vector;

   procedure Read_DAP_Variable_Vector
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DAP_Variable_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      T : Variable;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         Variable'Read (S, T);
         V.Append (T);
      end loop;
      JS.R.Read_Next;

   end Read_DAP_Variable_Vector;

   procedure Read_AttachRequestArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out AttachRequestArguments)
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
               LSP_Any'Read (S, V.a_restart);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_AttachRequestArguments;

   procedure Read_BreakpointLocation
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out BreakpointLocation)
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
               LSP_Number'Read (S, V.column);

            elsif Key = "endColumn" then
               LSP_Number'Read (S, V.endColumn);

            elsif Key = "endLine" then
               LSP_Number'Read (S, V.endLine);

            elsif Key = "line" then
               LSP_Number'Read (S, V.line);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_BreakpointLocation;

   procedure Read_CancelArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out CancelArguments)
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
               Read_String (S, V.progressId);

            elsif Key = "requestId" then
               LSP_Number'Read (S, V.requestId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CancelArguments;

   procedure Read_ColumnDescriptor
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ColumnDescriptor)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "attributeName" then
               Read_String (S, V.attributeName);

            elsif Key = "format" then
               Read_String (S, V.format);

            elsif Key = "label" then
               Read_String (S, V.label);

            elsif Key = "width" then
               LSP_Number'Read (S, V.width);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ColumnDescriptor;

   procedure Read_CompletionsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionsArguments)
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
               LSP_Number'Read (S, V.column);

            elsif Key = "frameId" then
               LSP_Number'Read (S, V.frameId);

            elsif Key = "line" then
               LSP_Number'Read (S, V.line);

            elsif Key = "text" then
               Read_String (S, V.text);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionsArguments;

   procedure Read_ConfigurationDoneArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ConfigurationDoneArguments)
   is
   begin
      --  this record is empty
      pragma Unreferenced (S);
      pragma Unreferenced (V);
   end Read_ConfigurationDoneArguments;
   procedure Read_ContinueArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ContinueArguments)
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
               LSP_Number'Read (S, V.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ContinueArguments;

   procedure Read_DataBreakpointInfoArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DataBreakpointInfoArguments)
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
               Read_String (S, V.name);

            elsif Key = "variablesReference" then
               LSP_Number'Read (S, V.variablesReference);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DataBreakpointInfoArguments;

   procedure Read_DisassembleArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DisassembleArguments)
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
               LSP_Number'Read (S, V.instructionCount);

            elsif Key = "instructionOffset" then
               LSP_Number'Read (S, V.instructionOffset);

            elsif Key = "memoryReference" then
               Read_String (S, V.memoryReference);

            elsif Key = "offset" then
               LSP_Number'Read (S, V.offset);

            elsif Key = "resolveSymbols" then
               Read_Boolean (JS, V.resolveSymbols);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DisassembleArguments;

   procedure Read_DisconnectArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DisconnectArguments)
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
               Read_Boolean (JS, V.restart);

            elsif Key = "terminateDebuggee" then
               Read_Boolean (JS, V.terminateDebuggee);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DisconnectArguments;

   procedure Read_ExceptionBreakpointsFilter
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ExceptionBreakpointsFilter)
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
               Read_String (S, V.conditionDescription);

            elsif Key = "default" then
               Read_Boolean (JS, V.default);

            elsif Key = "filter" then
               Read_String (S, V.filter);

            elsif Key = "label" then
               Read_String (S, V.label);

            elsif Key = "supportsCondition" then
               Read_Boolean (JS, V.supportsCondition);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionBreakpointsFilter;

   procedure Read_ExceptionFilterOptions
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ExceptionFilterOptions)
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
               Read_String (S, V.condition);

            elsif Key = "filterId" then
               Read_String (S, V.filterId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionFilterOptions;

   procedure Read_ExceptionInfoArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ExceptionInfoArguments)
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
               LSP_Number'Read (S, V.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionInfoArguments;

   procedure Read_FunctionBreakpoint
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out FunctionBreakpoint)
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
               Read_String (S, V.condition);

            elsif Key = "hitCondition" then
               Read_String (S, V.hitCondition);

            elsif Key = "name" then
               Read_String (S, V.name);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FunctionBreakpoint;

   procedure Read_GotoArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out GotoArguments)
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
               LSP_Number'Read (S, V.targetId);

            elsif Key = "threadId" then
               LSP_Number'Read (S, V.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_GotoArguments;

   procedure Read_GotoTarget
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out GotoTarget)
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
               LSP_Number'Read (S, V.column);

            elsif Key = "endColumn" then
               LSP_Number'Read (S, V.endColumn);

            elsif Key = "endLine" then
               LSP_Number'Read (S, V.endLine);

            elsif Key = "id" then
               LSP_Number'Read (S, V.id);

            elsif Key = "instructionPointerReference" then
               Read_String (S, V.instructionPointerReference);

            elsif Key = "label" then
               Read_String (S, V.label);

            elsif Key = "line" then
               LSP_Number'Read (S, V.line);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_GotoTarget;

   procedure Read_InitializeRequestArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeRequestArguments)
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
               Read_String (S, V.adapterID);

            elsif Key = "clientID" then
               Read_String (S, V.clientID);

            elsif Key = "clientName" then
               Read_String (S, V.clientName);

            elsif Key = "columnsStartAt1" then
               Read_Boolean (JS, V.columnsStartAt1);

            elsif Key = "linesStartAt1" then
               Read_Boolean (JS, V.linesStartAt1);

            elsif Key = "locale" then
               Read_String (S, V.locale);

            elsif Key = "pathFormat" then
               Read_String (S, V.pathFormat);

            elsif Key = "supportsInvalidatedEvent" then
               Read_Boolean (JS, V.supportsInvalidatedEvent);

            elsif Key = "supportsMemoryReferences" then
               Read_Boolean (JS, V.supportsMemoryReferences);

            elsif Key = "supportsProgressReporting" then
               Read_Boolean (JS, V.supportsProgressReporting);

            elsif Key = "supportsRunInTerminalRequest" then
               Read_Boolean (JS, V.supportsRunInTerminalRequest);

            elsif Key = "supportsVariablePaging" then
               Read_Boolean (JS, V.supportsVariablePaging);

            elsif Key = "supportsVariableType" then
               Read_Boolean (JS, V.supportsVariableType);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InitializeRequestArguments;

   procedure Read_InstructionBreakpoint
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out InstructionBreakpoint)
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
               Read_String (S, V.condition);

            elsif Key = "hitCondition" then
               Read_String (S, V.hitCondition);

            elsif Key = "instructionReference" then
               Read_String (S, V.instructionReference);

            elsif Key = "offset" then
               LSP_Number'Read (S, V.offset);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InstructionBreakpoint;

   procedure Read_LaunchRequestArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out LaunchRequestArguments)
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
               LSP_Any'Read (S, V.a_restart);

            elsif Key = "noDebug" then
               Read_Boolean (JS, V.noDebug);

            elsif Key = "program" then
               Read_String (S, V.program);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LaunchRequestArguments;

   procedure Read_LoadedSourcesArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out LoadedSourcesArguments)
   is
   begin
      --  this record is empty
      pragma Unreferenced (S);
      pragma Unreferenced (V);
   end Read_LoadedSourcesArguments;
   procedure Read_Message
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Message)
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
               Read_String (S, V.format);

            elsif Key = "id" then
               LSP_Number'Read (S, V.id);

            elsif Key = "sendTelemetry" then
               Read_Boolean (JS, V.sendTelemetry);

            elsif Key = "showUser" then
               Read_Boolean (JS, V.showUser);

            elsif Key = "url" then
               Read_String (S, V.url);

            elsif Key = "urlLabel" then
               Read_String (S, V.urlLabel);

            elsif Key = "variables" then
               DAP_String_Map'Read (S, V.variables);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Message;

   procedure Read_Module
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Module)
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
               Read_String (S, V.addressRange);

            elsif Key = "dateTimeStamp" then
               Read_String (S, V.dateTimeStamp);

            elsif Key = "id" then
               LSP_Number_Or_String'Read (S, V.id);

            elsif Key = "isOptimized" then
               Read_Boolean (JS, V.isOptimized);

            elsif Key = "isUserCode" then
               Read_Boolean (JS, V.isUserCode);

            elsif Key = "name" then
               Read_String (S, V.name);

            elsif Key = "path" then
               Read_String (S, V.path);

            elsif Key = "symbolFilePath" then
               Read_String (S, V.symbolFilePath);

            elsif Key = "symbolStatus" then
               Read_String (S, V.symbolStatus);

            elsif Key = "version" then
               Read_String (S, V.version);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Module;

   procedure Read_ModulesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ModulesArguments)
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
               LSP_Number'Read (S, V.moduleCount);

            elsif Key = "startModule" then
               LSP_Number'Read (S, V.startModule);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ModulesArguments;

   procedure Read_PauseArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out PauseArguments)
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
               LSP_Number'Read (S, V.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_PauseArguments;

   procedure Read_ProtocolMessage
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ProtocolMessage)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ProtocolMessage;

   procedure Read_ReadMemoryArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ReadMemoryArguments)
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
               LSP_Number'Read (S, V.count);

            elsif Key = "memoryReference" then
               Read_String (S, V.memoryReference);

            elsif Key = "offset" then
               LSP_Number'Read (S, V.offset);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ReadMemoryArguments;

   procedure Read_RestartArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out RestartArguments)
   is
   begin
      --  this record is empty
      pragma Unreferenced (S);
      pragma Unreferenced (V);
   end Read_RestartArguments;
   procedure Read_RestartFrameArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out RestartFrameArguments)
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
               LSP_Number'Read (S, V.frameId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RestartFrameArguments;

   procedure Read_ReverseContinueArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ReverseContinueArguments)
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
               LSP_Number'Read (S, V.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ReverseContinueArguments;

   procedure Read_ScopesArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ScopesArguments)
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
               LSP_Number'Read (S, V.frameId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ScopesArguments;

   procedure Read_SourceBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out SourceBreakpoint)
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
               LSP_Number'Read (S, V.column);

            elsif Key = "line" then
               LSP_Number'Read (S, V.line);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SourceBreakpoint;

   procedure Read_StackTraceArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out StackTraceArguments)
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
            if Key = "levels" then
               LSP_Number'Read (S, V.levels);

            elsif Key = "startFrame" then
               LSP_Number'Read (S, V.startFrame);

            elsif Key = "threadId" then
               LSP_Number'Read (S, V.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StackTraceArguments;

   procedure Read_StepInTarget
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepInTarget)
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
               LSP_Number'Read (S, V.id);

            elsif Key = "label" then
               Read_String (S, V.label);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepInTarget;

   procedure Read_StepInTargetsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out StepInTargetsArguments)
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
               LSP_Number'Read (S, V.frameId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepInTargetsArguments;

   procedure Read_TerminateArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out TerminateArguments)
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
               Read_Boolean (JS, V.restart);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TerminateArguments;

   procedure Read_Thread
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Thread)
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
               LSP_Number'Read (S, V.id);

            elsif Key = "name" then
               Read_String (S, V.name);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Thread;

   procedure Read_ValueFormat
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ValueFormat)
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
               Read_Boolean (JS, V.hex);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ValueFormat;

   procedure Read_Request
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Request)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               LSP_Any'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Request;

   procedure Read_AttachRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out AttachRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               AttachRequestArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_AttachRequest;

   procedure Read_Response
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Response)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Response;

   procedure Read_AttachResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out AttachResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_AttachResponse;

   procedure Read_Checksum
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Checksum)
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
               Enums.ChecksumAlgorithm'Read (S, V.algorithm);

            elsif Key = "checksum" then
               Read_String (S, V.checksum);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Checksum;

   procedure Read_Source
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Source)
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
               LSP_Any'Read (S, V.adapterData);

            elsif Key = "checksums" then
               DAP_Checksum_Vector'Read (S, V.checksums);

            elsif Key = "name" then
               Read_String (S, V.name);

            elsif Key = "origin" then
               Read_String (S, V.origin);

            elsif Key = "path" then
               Read_String (S, V.path);

            elsif Key = "presentationHint" then
               Read_String (S, V.presentationHint);

            elsif Key = "sourceReference" then
               LSP_Number'Read (S, V.sourceReference);

            elsif Key = "sources" then
               DAP_Source_Vector'Read (S, V.sources);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Source;

   procedure Read_Breakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Breakpoint)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "source" then
               Source'Read (S, V.a_source);

            elsif Key = "column" then
               LSP_Number'Read (S, V.column);

            elsif Key = "endColumn" then
               LSP_Number'Read (S, V.endColumn);

            elsif Key = "endLine" then
               LSP_Number'Read (S, V.endLine);

            elsif Key = "id" then
               LSP_Number'Read (S, V.id);

            elsif Key = "instructionReference" then
               Read_String (S, V.instructionReference);

            elsif Key = "line" then
               LSP_Number'Read (S, V.line);

            elsif Key = "offset" then
               LSP_Number'Read (S, V.offset);

            elsif Key = "verified" then
               Read_Boolean (JS, V.verified);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Breakpoint;

   procedure Read_Event
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Event)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Event;

   procedure Read_BreakpointEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out BreakpointEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "breakpoint" then
                  Breakpoint'Read (S, V.body_breakpoint);

               elsif Key = "reason" then
                  Read_String (S, V.body_reason);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_BreakpointEvent;

   procedure Read_BreakpointLocationsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out BreakpointLocationsArguments)
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
               Source'Read (S, V.a_source);

            elsif Key = "column" then
               LSP_Number'Read (S, V.column);

            elsif Key = "endColumn" then
               LSP_Number'Read (S, V.endColumn);

            elsif Key = "endLine" then
               LSP_Number'Read (S, V.endLine);

            elsif Key = "line" then
               LSP_Number'Read (S, V.line);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_BreakpointLocationsArguments;

   procedure Read_BreakpointLocationsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out BreakpointLocationsRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               BreakpointLocationsArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_BreakpointLocationsRequest;

   procedure Read_BreakpointLocationsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out BreakpointLocationsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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
                  DAP_BreakpointLocation_Vector'Read (S, V.body_breakpoints);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_BreakpointLocationsResponse;

   procedure Read_CancelRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out CancelRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               CancelArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CancelRequest;

   procedure Read_CancelResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out CancelResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CancelResponse;

   procedure Read_Capabilities
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Capabilities)
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
               DAP_ColumnDescriptor_Vector'Read (S, V.additionalModuleColumns);

            elsif Key = "completionTriggerCharacters" then
               DAP_String_Vector'Read (S, V.completionTriggerCharacters);

            elsif Key = "exceptionBreakpointFilters" then
               DAP_ExceptionBreakpointsFilter_Vector'Read
                 (S, V.exceptionBreakpointFilters);

            elsif Key = "supportTerminateDebuggee" then
               Read_Boolean (JS, V.supportTerminateDebuggee);

            elsif Key = "supportedChecksumAlgorithms" then
               DAP_ChecksumAlgorithm_Vector'Read
                 (S, V.supportedChecksumAlgorithms);

            elsif Key = "supportsBreakpointLocationsRequest" then
               Read_Boolean (JS, V.supportsBreakpointLocationsRequest);

            elsif Key = "supportsCancelRequest" then
               Read_Boolean (JS, V.supportsCancelRequest);

            elsif Key = "supportsClipboardContext" then
               Read_Boolean (JS, V.supportsClipboardContext);

            elsif Key = "supportsCompletionsRequest" then
               Read_Boolean (JS, V.supportsCompletionsRequest);

            elsif Key = "supportsConditionalBreakpoints" then
               Read_Boolean (JS, V.supportsConditionalBreakpoints);

            elsif Key = "supportsConfigurationDoneRequest" then
               Read_Boolean (JS, V.supportsConfigurationDoneRequest);

            elsif Key = "supportsDataBreakpoints" then
               Read_Boolean (JS, V.supportsDataBreakpoints);

            elsif Key = "supportsDelayedStackTraceLoading" then
               Read_Boolean (JS, V.supportsDelayedStackTraceLoading);

            elsif Key = "supportsDisassembleRequest" then
               Read_Boolean (JS, V.supportsDisassembleRequest);

            elsif Key = "supportsEvaluateForHovers" then
               Read_Boolean (JS, V.supportsEvaluateForHovers);

            elsif Key = "supportsExceptionFilterOptions" then
               Read_Boolean (JS, V.supportsExceptionFilterOptions);

            elsif Key = "supportsExceptionInfoRequest" then
               Read_Boolean (JS, V.supportsExceptionInfoRequest);

            elsif Key = "supportsExceptionOptions" then
               Read_Boolean (JS, V.supportsExceptionOptions);

            elsif Key = "supportsFunctionBreakpoints" then
               Read_Boolean (JS, V.supportsFunctionBreakpoints);

            elsif Key = "supportsGotoTargetsRequest" then
               Read_Boolean (JS, V.supportsGotoTargetsRequest);

            elsif Key = "supportsHitConditionalBreakpoints" then
               Read_Boolean (JS, V.supportsHitConditionalBreakpoints);

            elsif Key = "supportsInstructionBreakpoints" then
               Read_Boolean (JS, V.supportsInstructionBreakpoints);

            elsif Key = "supportsLoadedSourcesRequest" then
               Read_Boolean (JS, V.supportsLoadedSourcesRequest);

            elsif Key = "supportsLogPoints" then
               Read_Boolean (JS, V.supportsLogPoints);

            elsif Key = "supportsModulesRequest" then
               Read_Boolean (JS, V.supportsModulesRequest);

            elsif Key = "supportsReadMemoryRequest" then
               Read_Boolean (JS, V.supportsReadMemoryRequest);

            elsif Key = "supportsRestartFrame" then
               Read_Boolean (JS, V.supportsRestartFrame);

            elsif Key = "supportsRestartRequest" then
               Read_Boolean (JS, V.supportsRestartRequest);

            elsif Key = "supportsSetExpression" then
               Read_Boolean (JS, V.supportsSetExpression);

            elsif Key = "supportsSetVariable" then
               Read_Boolean (JS, V.supportsSetVariable);

            elsif Key = "supportsStepBack" then
               Read_Boolean (JS, V.supportsStepBack);

            elsif Key = "supportsStepInTargetsRequest" then
               Read_Boolean (JS, V.supportsStepInTargetsRequest);

            elsif Key = "supportsSteppingGranularity" then
               Read_Boolean (JS, V.supportsSteppingGranularity);

            elsif Key = "supportsTerminateRequest" then
               Read_Boolean (JS, V.supportsTerminateRequest);

            elsif Key = "supportsTerminateThreadsRequest" then
               Read_Boolean (JS, V.supportsTerminateThreadsRequest);

            elsif Key = "supportsValueFormattingOptions" then
               Read_Boolean (JS, V.supportsValueFormattingOptions);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Capabilities;

   procedure Read_CapabilitiesEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out CapabilitiesEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "capabilities" then
                  Capabilities'Read (S, V.body_capabilities);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CapabilitiesEvent;

   procedure Read_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out CompletionItem)
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
            if Key = "type" then
               Enums.CompletionItemType'Read (S, V.a_type);

            elsif Key = "label" then
               Read_String (S, V.label);

            elsif Key = "length" then
               LSP_Number'Read (S, V.length);

            elsif Key = "selectionLength" then
               LSP_Number'Read (S, V.selectionLength);

            elsif Key = "selectionStart" then
               LSP_Number'Read (S, V.selectionStart);

            elsif Key = "sortText" then
               Read_String (S, V.sortText);

            elsif Key = "start" then
               LSP_Number'Read (S, V.start);

            elsif Key = "text" then
               Read_String (S, V.text);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionItem;

   procedure Read_CompletionsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionsRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               CompletionsArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionsRequest;

   procedure Read_CompletionsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "targets" then
                  DAP_CompletionItem_Vector'Read (S, V.body_targets);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionsResponse;

   procedure Read_ConfigurationDoneRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ConfigurationDoneRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               ConfigurationDoneArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ConfigurationDoneRequest;

   procedure Read_ConfigurationDoneResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ConfigurationDoneResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ConfigurationDoneResponse;

   procedure Read_ContinueRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ContinueRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               ContinueArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ContinueRequest;

   procedure Read_ContinueResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ContinueResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "allThreadsContinued" then
                  Read_Boolean (JS, V.body_allThreadsContinued);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ContinueResponse;

   procedure Read_ContinuedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ContinuedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "allThreadsContinued" then
                  Read_Boolean (JS, V.body_allThreadsContinued);

               elsif Key = "threadId" then
                  LSP_Number'Read (S, V.body_threadId);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ContinuedEvent;

   procedure Read_DataBreakpoint
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out DataBreakpoint)
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
               Enums.DataBreakpointAccessType'Read (S, V.accessType);

            elsif Key = "condition" then
               Read_String (S, V.condition);

            elsif Key = "dataId" then
               Read_String (S, V.dataId);

            elsif Key = "hitCondition" then
               Read_String (S, V.hitCondition);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DataBreakpoint;

   procedure Read_DataBreakpointInfoRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DataBreakpointInfoRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               DataBreakpointInfoArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DataBreakpointInfoRequest;

   procedure Read_DataBreakpointInfoResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DataBreakpointInfoResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "accessTypes" then
                  DAP_DataBreakpointAccessType_Vector'Read
                    (S, V.body_accessTypes);

               elsif Key = "canPersist" then
                  Read_Boolean (JS, V.body_canPersist);

               elsif Key = "dataId" then
                  Read_String (S, V.body_dataId);

               elsif Key = "description" then
                  Read_String (S, V.body_description);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DataBreakpointInfoResponse;

   procedure Read_DisassembleRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DisassembleRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               DisassembleArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DisassembleRequest;

   procedure Read_DisassembledInstruction
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DisassembledInstruction)
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
               Read_String (S, V.address);

            elsif Key = "column" then
               LSP_Number'Read (S, V.column);

            elsif Key = "endColumn" then
               LSP_Number'Read (S, V.endColumn);

            elsif Key = "endLine" then
               LSP_Number'Read (S, V.endLine);

            elsif Key = "instruction" then
               Read_String (S, V.instruction);

            elsif Key = "instructionBytes" then
               Read_String (S, V.instructionBytes);

            elsif Key = "line" then
               LSP_Number'Read (S, V.line);

            elsif Key = "location" then
               Source'Read (S, V.location);

            elsif Key = "symbol" then
               Read_String (S, V.symbol);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DisassembledInstruction;

   procedure Read_DisassembleResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DisassembleResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "instructions" then
                  DAP_DisassembledInstruction_Vector'Read
                    (S, V.body_instructions);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DisassembleResponse;

   procedure Read_DisconnectRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out DisconnectRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               DisconnectArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DisconnectRequest;

   procedure Read_DisconnectResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out DisconnectResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DisconnectResponse;

   procedure Read_ErrorResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ErrorResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "error" then
                  Message'Read (S, V.body_error);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ErrorResponse;

   procedure Read_EvaluateArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out EvaluateArguments)
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
               Read_String (S, V.context);

            elsif Key = "expression" then
               Read_String (S, V.expression);

            elsif Key = "format" then
               ValueFormat'Read (S, V.format);

            elsif Key = "frameId" then
               LSP_Number'Read (S, V.frameId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_EvaluateArguments;

   procedure Read_EvaluateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out EvaluateRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               EvaluateArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_EvaluateRequest;

   procedure Read_VariablePresentationHint
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out VariablePresentationHint)
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
               DAP_String_Vector'Read (S, V.attributes);

            elsif Key = "kind" then
               Read_String (S, V.kind);

            elsif Key = "visibility" then
               Read_String (S, V.visibility);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_VariablePresentationHint;

   procedure Read_EvaluateResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out EvaluateResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "indexedVariables" then
                  LSP_Number'Read (S, V.body_indexedVariables);

               elsif Key = "memoryReference" then
                  Read_String (S, V.body_memoryReference);

               elsif Key = "namedVariables" then
                  LSP_Number'Read (S, V.body_namedVariables);

               elsif Key = "presentationHint" then
                  VariablePresentationHint'Read (S, V.body_presentationHint);

               elsif Key = "result" then
                  Read_String (S, V.body_result);

               elsif Key = "type" then
                  Read_String (S, V.body_type);

               elsif Key = "variablesReference" then
                  LSP_Number'Read (S, V.body_variablesReference);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_EvaluateResponse;

   procedure Read_ExceptionDetails
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ExceptionDetails)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "evaluateName" then
               Read_String (S, V.evaluateName);

            elsif Key = "fullTypeName" then
               Read_String (S, V.fullTypeName);

            elsif Key = "innerException" then
               DAP_ExceptionDetails_Vector'Read (S, V.innerException);

            elsif Key = "stackTrace" then
               Read_String (S, V.stackTrace);

            elsif Key = "typeName" then
               Read_String (S, V.typeName);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionDetails;

   procedure Read_ExceptionInfoRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ExceptionInfoRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               ExceptionInfoArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionInfoRequest;

   procedure Read_ExceptionInfoResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ExceptionInfoResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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
                  Enums.ExceptionBreakMode'Read (S, V.body_breakMode);

               elsif Key = "description" then
                  Read_String (S, V.body_description);

               elsif Key = "details" then
                  ExceptionDetails'Read (S, V.body_details);

               elsif Key = "exceptionId" then
                  Read_String (S, V.body_exceptionId);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionInfoResponse;

   procedure Read_ExceptionPathSegment
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ExceptionPathSegment)
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
               DAP_String_Vector'Read (S, V.names);

            elsif Key = "negate" then
               Read_Boolean (JS, V.negate);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionPathSegment;

   procedure Read_ExceptionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ExceptionOptions)
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
               Enums.ExceptionBreakMode'Read (S, V.breakMode);

            elsif Key = "path" then
               DAP_ExceptionPathSegment_Vector'Read (S, V.path);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExceptionOptions;

   procedure Read_ExitedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ExitedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "exitCode" then
                  LSP_Number'Read (S, V.body_exitCode);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExitedEvent;

   procedure Read_GotoRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out GotoRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               GotoArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_GotoRequest;

   procedure Read_GotoResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out GotoResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_GotoResponse;

   procedure Read_GotoTargetsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out GotoTargetsArguments)
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
               Source'Read (S, V.a_source);

            elsif Key = "column" then
               LSP_Number'Read (S, V.column);

            elsif Key = "line" then
               LSP_Number'Read (S, V.line);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_GotoTargetsArguments;

   procedure Read_GotoTargetsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out GotoTargetsRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               GotoTargetsArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_GotoTargetsRequest;

   procedure Read_GotoTargetsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out GotoTargetsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "targets" then
                  DAP_GotoTarget_Vector'Read (S, V.body_targets);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_GotoTargetsResponse;

   procedure Read_InitializeRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out InitializeRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               InitializeRequestArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InitializeRequest;

   procedure Read_InitializeResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InitializeResponse;

   procedure Read_InitializedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out InitializedEvent)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InitializedEvent;

   procedure Read_InvalidatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out InvalidatedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "areas" then
                  DAP_InvalidatedAreas_Vector'Read (S, V.body_areas);

               elsif Key = "stackFrameId" then
                  LSP_Number'Read (S, V.body_stackFrameId);

               elsif Key = "threadId" then
                  LSP_Number'Read (S, V.body_threadId);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InvalidatedEvent;

   procedure Read_LaunchRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out LaunchRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               LaunchRequestArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LaunchRequest;

   procedure Read_LaunchResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out LaunchResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LaunchResponse;

   procedure Read_LoadedSourceEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out LoadedSourceEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "reason" then
                  Read_String (S, V.body_reason);

               elsif Key = "source" then
                  Source'Read (S, V.body_source);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LoadedSourceEvent;

   procedure Read_LoadedSourcesRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out LoadedSourcesRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               LoadedSourcesArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LoadedSourcesRequest;

   procedure Read_LoadedSourcesResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out LoadedSourcesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "sources" then
                  DAP_Source_Vector'Read (S, V.body_sources);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LoadedSourcesResponse;

   procedure Read_ModuleEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ModuleEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "module" then
                  Module'Read (S, V.body_module);

               elsif Key = "reason" then
                  Read_String (S, V.body_reason);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ModuleEvent;

   procedure Read_ModulesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ModulesRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               ModulesArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ModulesRequest;

   procedure Read_ModulesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ModulesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "modules" then
                  DAP_Module_Vector'Read (S, V.body_modules);

               elsif Key = "totalModules" then
                  LSP_Number'Read (S, V.body_totalModules);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ModulesResponse;

   procedure Read_ModulesViewDescriptor
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ModulesViewDescriptor)
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
               DAP_ColumnDescriptor_Vector'Read (S, V.columns);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ModulesViewDescriptor;

   procedure Read_NextArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out NextArguments)
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
               Enums.SteppingGranularity'Read (S, V.granularity);

            elsif Key = "threadId" then
               LSP_Number'Read (S, V.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_NextArguments;

   procedure Read_NextRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out NextRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               NextArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_NextRequest;

   procedure Read_NextResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out NextResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_NextResponse;

   procedure Read_OutputEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out OutputEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "category" then
                  Read_String (S, V.body_category);

               elsif Key = "column" then
                  LSP_Number'Read (S, V.body_column);

               elsif Key = "data" then
                  LSP_Any'Read (S, V.body_data);

               elsif Key = "group" then
                  Read_String (S, V.body_group);

               elsif Key = "line" then
                  LSP_Number'Read (S, V.body_line);

               elsif Key = "output" then
                  Read_String (S, V.body_output);

               elsif Key = "source" then
                  Source'Read (S, V.body_source);

               elsif Key = "variablesReference" then
                  LSP_Number'Read (S, V.body_variablesReference);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_OutputEvent;

   procedure Read_PauseRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out PauseRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               PauseArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_PauseRequest;

   procedure Read_PauseResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out PauseResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_PauseResponse;

   procedure Read_ProcessEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ProcessEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "isLocalProcess" then
                  Read_Boolean (JS, V.body_isLocalProcess);

               elsif Key = "name" then
                  Read_String (S, V.body_name);

               elsif Key = "pointerSize" then
                  LSP_Number'Read (S, V.body_pointerSize);

               elsif Key = "startMethod" then
                  Read_String (S, V.body_startMethod);

               elsif Key = "systemProcessId" then
                  LSP_Number'Read (S, V.body_systemProcessId);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ProcessEvent;

   procedure Read_ProgressEndEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ProgressEndEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "message" then
                  Read_String (S, V.body_message);

               elsif Key = "progressId" then
                  Read_String (S, V.body_progressId);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ProgressEndEvent;

   procedure Read_ProgressStartEvent
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ProgressStartEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "cancellable" then
                  Read_Boolean (JS, V.body_cancellable);

               elsif Key = "message" then
                  Read_String (S, V.body_message);

               elsif Key = "percentage" then
                  LSP_Number'Read (S, V.body_percentage);

               elsif Key = "progressId" then
                  Read_String (S, V.body_progressId);

               elsif Key = "requestId" then
                  LSP_Number'Read (S, V.body_requestId);

               elsif Key = "title" then
                  Read_String (S, V.body_title);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ProgressStartEvent;

   procedure Read_ProgressUpdateEvent
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ProgressUpdateEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "message" then
                  Read_String (S, V.body_message);

               elsif Key = "percentage" then
                  LSP_Number'Read (S, V.body_percentage);

               elsif Key = "progressId" then
                  Read_String (S, V.body_progressId);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ProgressUpdateEvent;

   procedure Read_ReadMemoryRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ReadMemoryRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               ReadMemoryArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ReadMemoryRequest;

   procedure Read_ReadMemoryResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ReadMemoryResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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
                  Read_String (S, V.body_address);

               elsif Key = "data" then
                  Read_String (S, V.body_data);

               elsif Key = "unreadableBytes" then
                  LSP_Number'Read (S, V.body_unreadableBytes);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ReadMemoryResponse;

   procedure Read_RestartFrameRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out RestartFrameRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               RestartFrameArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RestartFrameRequest;

   procedure Read_RestartFrameResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out RestartFrameResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RestartFrameResponse;

   procedure Read_RestartRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out RestartRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               RestartArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RestartRequest;

   procedure Read_RestartResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out RestartResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RestartResponse;

   procedure Read_ReverseContinueRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ReverseContinueRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               ReverseContinueArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ReverseContinueRequest;

   procedure Read_ReverseContinueResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out ReverseContinueResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ReverseContinueResponse;

   procedure Read_RunInTerminalRequestArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out RunInTerminalRequestArguments)
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
               DAP_String_Vector'Read (S, V.args);

            elsif Key = "cwd" then
               Read_String (S, V.cwd);

            elsif Key = "env" then
               DAP_String_Map'Read (S, V.env);

            elsif Key = "kind" then
               Read_String (S, V.kind);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RunInTerminalRequestArguments;

   procedure Read_RunInTerminalRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out RunInTerminalRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               RunInTerminalRequestArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RunInTerminalRequest;

   procedure Read_RunInTerminalResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out RunInTerminalResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "processId" then
                  LSP_Number'Read (S, V.body_processId);

               elsif Key = "shellProcessId" then
                  LSP_Number'Read (S, V.body_shellProcessId);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RunInTerminalResponse;

   procedure Read_Scope
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Scope)
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
               Source'Read (S, V.a_source);

            elsif Key = "column" then
               LSP_Number'Read (S, V.column);

            elsif Key = "endColumn" then
               LSP_Number'Read (S, V.endColumn);

            elsif Key = "endLine" then
               LSP_Number'Read (S, V.endLine);

            elsif Key = "expensive" then
               Read_Boolean (JS, V.expensive);

            elsif Key = "indexedVariables" then
               LSP_Number'Read (S, V.indexedVariables);

            elsif Key = "line" then
               LSP_Number'Read (S, V.line);

            elsif Key = "name" then
               Read_String (S, V.name);

            elsif Key = "namedVariables" then
               LSP_Number'Read (S, V.namedVariables);

            elsif Key = "presentationHint" then
               Read_String (S, V.presentationHint);

            elsif Key = "variablesReference" then
               LSP_Number'Read (S, V.variablesReference);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Scope;

   procedure Read_ScopesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ScopesRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               ScopesArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ScopesRequest;

   procedure Read_ScopesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ScopesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "scopes" then
                  DAP_Scope_Vector'Read (S, V.body_scopes);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ScopesResponse;

   procedure Read_SetBreakpointsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetBreakpointsArguments)
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
               Source'Read (S, V.a_source);

            elsif Key = "breakpoints" then
               DAP_SourceBreakpoint_Vector'Read (S, V.breakpoints);

            elsif Key = "lines" then
               DAP_Integer_Vector'Read (S, V.lines);

            elsif Key = "sourceModified" then
               Read_Boolean (JS, V.sourceModified);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetBreakpointsArguments;

   procedure Read_SetBreakpointsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetBreakpointsRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               SetBreakpointsArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetBreakpointsRequest;

   procedure Read_SetBreakpointsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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
                  DAP_Breakpoint_Vector'Read (S, V.body_breakpoints);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetBreakpointsResponse;

   procedure Read_SetDataBreakpointsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetDataBreakpointsArguments)
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
               DAP_DataBreakpoint_Vector'Read (S, V.breakpoints);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetDataBreakpointsArguments;

   procedure Read_SetDataBreakpointsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetDataBreakpointsRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               SetDataBreakpointsArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetDataBreakpointsRequest;

   procedure Read_SetDataBreakpointsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetDataBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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
                  DAP_Breakpoint_Vector'Read (S, V.body_breakpoints);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetDataBreakpointsResponse;

   procedure Read_SetExceptionBreakpointsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetExceptionBreakpointsArguments)
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
               DAP_ExceptionOptions_Vector'Read (S, V.exceptionOptions);

            elsif Key = "filterOptions" then
               DAP_ExceptionFilterOptions_Vector'Read (S, V.filterOptions);

            elsif Key = "filters" then
               DAP_String_Vector'Read (S, V.filters);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetExceptionBreakpointsArguments;

   procedure Read_SetExceptionBreakpointsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetExceptionBreakpointsRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               SetExceptionBreakpointsArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetExceptionBreakpointsRequest;

   procedure Read_SetExceptionBreakpointsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetExceptionBreakpointsResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetExceptionBreakpointsResponse;

   procedure Read_SetExpressionArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetExpressionArguments)
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
               Read_String (S, V.expression);

            elsif Key = "format" then
               ValueFormat'Read (S, V.format);

            elsif Key = "frameId" then
               LSP_Number'Read (S, V.frameId);

            elsif Key = "value" then
               Read_String (S, V.value);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetExpressionArguments;

   procedure Read_SetExpressionRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetExpressionRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               SetExpressionArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetExpressionRequest;

   procedure Read_SetExpressionResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetExpressionResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "indexedVariables" then
                  LSP_Number'Read (S, V.body_indexedVariables);

               elsif Key = "namedVariables" then
                  LSP_Number'Read (S, V.body_namedVariables);

               elsif Key = "presentationHint" then
                  VariablePresentationHint'Read (S, V.body_presentationHint);

               elsif Key = "type" then
                  Read_String (S, V.body_type);

               elsif Key = "value" then
                  Read_String (S, V.body_value);

               elsif Key = "variablesReference" then
                  LSP_Number'Read (S, V.body_variablesReference);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetExpressionResponse;

   procedure Read_SetFunctionBreakpointsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetFunctionBreakpointsArguments)
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
               DAP_FunctionBreakpoint_Vector'Read (S, V.breakpoints);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetFunctionBreakpointsArguments;

   procedure Read_SetFunctionBreakpointsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetFunctionBreakpointsRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               SetFunctionBreakpointsArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetFunctionBreakpointsRequest;

   procedure Read_SetFunctionBreakpointsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetFunctionBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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
                  DAP_Breakpoint_Vector'Read (S, V.body_breakpoints);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetFunctionBreakpointsResponse;

   procedure Read_SetInstructionBreakpointsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetInstructionBreakpointsArguments)
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
               DAP_InstructionBreakpoint_Vector'Read (S, V.breakpoints);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetInstructionBreakpointsArguments;

   procedure Read_SetInstructionBreakpointsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetInstructionBreakpointsRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               SetInstructionBreakpointsArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetInstructionBreakpointsRequest;

   procedure Read_SetInstructionBreakpointsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetInstructionBreakpointsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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
                  DAP_Breakpoint_Vector'Read (S, V.body_breakpoints);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetInstructionBreakpointsResponse;

   procedure Read_SetVariableArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetVariableArguments)
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
               ValueFormat'Read (S, V.format);

            elsif Key = "name" then
               Read_String (S, V.name);

            elsif Key = "value" then
               Read_String (S, V.value);

            elsif Key = "variablesReference" then
               LSP_Number'Read (S, V.variablesReference);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetVariableArguments;

   procedure Read_SetVariableRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetVariableRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               SetVariableArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetVariableRequest;

   procedure Read_SetVariableResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out SetVariableResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "indexedVariables" then
                  LSP_Number'Read (S, V.body_indexedVariables);

               elsif Key = "namedVariables" then
                  LSP_Number'Read (S, V.body_namedVariables);

               elsif Key = "type" then
                  Read_String (S, V.body_type);

               elsif Key = "value" then
                  Read_String (S, V.body_value);

               elsif Key = "variablesReference" then
                  LSP_Number'Read (S, V.body_variablesReference);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetVariableResponse;

   procedure Read_SourceArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out SourceArguments)
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
               Source'Read (S, V.a_source);

            elsif Key = "sourceReference" then
               LSP_Number'Read (S, V.sourceReference);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SourceArguments;

   procedure Read_SourceRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out SourceRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               SourceArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SourceRequest;

   procedure Read_SourceResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out SourceResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "content" then
                  Read_String (S, V.body_content);

               elsif Key = "mimeType" then
                  Read_String (S, V.body_mimeType);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SourceResponse;

   procedure Read_StackFrame
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StackFrame)
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
               Source'Read (S, V.a_source);

            elsif Key = "canRestart" then
               Read_Boolean (JS, V.canRestart);

            elsif Key = "column" then
               LSP_Number'Read (S, V.column);

            elsif Key = "endColumn" then
               LSP_Number'Read (S, V.endColumn);

            elsif Key = "endLine" then
               LSP_Number'Read (S, V.endLine);

            elsif Key = "id" then
               LSP_Number'Read (S, V.id);

            elsif Key = "instructionPointerReference" then
               Read_String (S, V.instructionPointerReference);

            elsif Key = "line" then
               LSP_Number'Read (S, V.line);

            elsif Key = "moduleId" then
               LSP_Number_Or_String'Read (S, V.moduleId);

            elsif Key = "name" then
               Read_String (S, V.name);

            elsif Key = "presentationHint" then
               Read_String (S, V.presentationHint);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StackFrame;

   procedure Read_StackFrameFormat
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StackFrameFormat)
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
               Read_Boolean (JS, V.hex);

            elsif Key = "includeAll" then
               Read_Boolean (JS, V.includeAll);

            elsif Key = "line" then
               Read_Boolean (JS, V.line);

            elsif Key = "module" then
               Read_Boolean (JS, V.module);

            elsif Key = "parameterNames" then
               Read_Boolean (JS, V.parameterNames);

            elsif Key = "parameterTypes" then
               Read_Boolean (JS, V.parameterTypes);

            elsif Key = "parameterValues" then
               Read_Boolean (JS, V.parameterValues);

            elsif Key = "parameters" then
               Read_Boolean (JS, V.parameters);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StackFrameFormat;

   procedure Read_StackTraceRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StackTraceRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               StackTraceArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StackTraceRequest;

   procedure Read_StackTraceResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out StackTraceResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "stackFrames" then
                  DAP_StackFrame_Vector'Read (S, V.body_stackFrames);

               elsif Key = "totalFrames" then
                  LSP_Number'Read (S, V.body_totalFrames);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StackTraceResponse;

   procedure Read_StepBackArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepBackArguments)
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
               Enums.SteppingGranularity'Read (S, V.granularity);

            elsif Key = "threadId" then
               LSP_Number'Read (S, V.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepBackArguments;

   procedure Read_StepBackRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepBackRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               StepBackArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepBackRequest;

   procedure Read_StepBackResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepBackResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepBackResponse;

   procedure Read_StepInArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepInArguments)
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
               Enums.SteppingGranularity'Read (S, V.granularity);

            elsif Key = "targetId" then
               LSP_Number'Read (S, V.targetId);

            elsif Key = "threadId" then
               LSP_Number'Read (S, V.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepInArguments;

   procedure Read_StepInRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepInRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               StepInArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepInRequest;

   procedure Read_StepInResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepInResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepInResponse;

   procedure Read_StepInTargetsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out StepInTargetsRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               StepInTargetsArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepInTargetsRequest;

   procedure Read_StepInTargetsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out StepInTargetsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "targets" then
                  DAP_StepInTarget_Vector'Read (S, V.body_targets);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepInTargetsResponse;

   procedure Read_StepOutArguments
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepOutArguments)
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
               Enums.SteppingGranularity'Read (S, V.granularity);

            elsif Key = "threadId" then
               LSP_Number'Read (S, V.threadId);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepOutArguments;

   procedure Read_StepOutRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepOutRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               StepOutArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepOutRequest;

   procedure Read_StepOutResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StepOutResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StepOutResponse;

   procedure Read_StoppedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out StoppedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "allThreadsStopped" then
                  Read_Boolean (JS, V.body_allThreadsStopped);

               elsif Key = "description" then
                  Read_String (S, V.body_description);

               elsif Key = "hitBreakpointIds" then
                  DAP_Integer_Vector'Read (S, V.body_hitBreakpointIds);

               elsif Key = "preserveFocusHint" then
                  Read_Boolean (JS, V.body_preserveFocusHint);

               elsif Key = "reason" then
                  Read_String (S, V.body_reason);

               elsif Key = "text" then
                  Read_String (S, V.body_text);

               elsif Key = "threadId" then
                  LSP_Number'Read (S, V.body_threadId);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_StoppedEvent;

   procedure Read_TerminateRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out TerminateRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               TerminateArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TerminateRequest;

   procedure Read_TerminateResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out TerminateResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TerminateResponse;

   procedure Read_TerminateThreadsArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out TerminateThreadsArguments)
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
               DAP_Integer_Vector'Read (S, V.threadIds);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TerminateThreadsArguments;

   procedure Read_TerminateThreadsRequest
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out TerminateThreadsRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               TerminateThreadsArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TerminateThreadsRequest;

   procedure Read_TerminateThreadsResponse
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out TerminateThreadsResponse)
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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TerminateThreadsResponse;

   procedure Read_TerminatedEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out TerminatedEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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
                  LSP_Any'Read (S, V.body_restart);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TerminatedEvent;

   procedure Read_ThreadEvent
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ThreadEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "reason" then
                  Read_String (S, V.body_reason);

               elsif Key = "threadId" then
                  LSP_Number'Read (S, V.body_threadId);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "event" then
               Read_String (S, V.event);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ThreadEvent;

   procedure Read_ThreadsRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ThreadsRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               LSP_Any'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ThreadsRequest;

   procedure Read_ThreadsResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out ThreadsResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "threads" then
                  DAP_Thread_Vector'Read (S, V.body_threads);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ThreadsResponse;

   procedure Read_Variable
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out Variable)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "evaluateName" then
               Read_String (S, V.evaluateName);

            elsif Key = "indexedVariables" then
               LSP_Number'Read (S, V.indexedVariables);

            elsif Key = "memoryReference" then
               Read_String (S, V.memoryReference);

            elsif Key = "name" then
               Read_String (S, V.name);

            elsif Key = "namedVariables" then
               LSP_Number'Read (S, V.namedVariables);

            elsif Key = "presentationHint" then
               VariablePresentationHint'Read (S, V.presentationHint);

            elsif Key = "value" then
               Read_String (S, V.value);

            elsif Key = "variablesReference" then
               LSP_Number'Read (S, V.variablesReference);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Variable;

   procedure Read_VariablesArguments
     (S :     access Ada.Streams.Root_Stream_Type'Class;
      V : out VariablesArguments)
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
               LSP_Number'Read (S, V.count);

            elsif Key = "filter" then
               Read_String (S, V.filter);

            elsif Key = "format" then
               ValueFormat'Read (S, V.format);

            elsif Key = "start" then
               LSP_Number'Read (S, V.start);

            elsif Key = "variablesReference" then
               LSP_Number'Read (S, V.variablesReference);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_VariablesArguments;

   procedure Read_VariablesRequest
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out VariablesRequest)
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
            if Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "arguments" then
               VariablesArguments'Read (S, V.arguments);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_VariablesRequest;

   procedure Read_VariablesResponse
     (S : access Ada.Streams.Root_Stream_Type'Class; V : out VariablesResponse)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Read_Body;

      procedure Read_Body is
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

               if Key = "variables" then
                  DAP_Variable_Vector'Read (S, V.body_variables);

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Body;

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
            if Key = "message" then
               Read_String (S, V.a_message);

            elsif Key = "type" then
               Read_String (S, V.a_type);

            elsif Key = "command" then
               Read_String (S, V.command);

            elsif Key = "request_seq" then
               LSP_Number'Read (S, V.request_seq);

            elsif Key = "seq" then
               LSP_Number'Read (S, V.seq);

            elsif Key = "success" then
               Read_Boolean (JS, V.success);

            elsif Key = "body" then
               Read_Body;

            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_VariablesResponse;

end DAP.Tools;
