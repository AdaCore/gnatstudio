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

with Ada.Unchecked_Deallocation;

package body DAP.Tools is
   procedure Free is new Ada.Unchecked_Deallocation
     (Thread_Array, Thread_Array_Access);

   overriding procedure Adjust (Self : in out Thread_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Thread_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Thread_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : Thread_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out Thread_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out Thread_Vector; Value : Thread) is
      Init_Length : constant Positive   := Positive'Max (1, 256 / Thread'Size);
      Self_Data_Saved : Thread_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new Thread_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Thread_Array'
             (Self.Data.all & Thread_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_Thread_Variable_Reference
     (Self  : aliased in out Thread_Vector;
      Index : Positive)
      return Thread_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_Thread_Constant_Reference
     (Self  : aliased Thread_Vector;
      Index : Positive)
      return Thread_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Checksum_Array, Checksum_Array_Access);

   overriding procedure Adjust (Self : in out Checksum_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Checksum_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Checksum_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : Checksum_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out Checksum_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out Checksum_Vector; Value : Checksum) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Checksum'Size);
      Self_Data_Saved : Checksum_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new Checksum_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Checksum_Array'
             (Self.Data.all & Checksum_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_Checksum_Variable_Reference
     (Self  : aliased in out Checksum_Vector;
      Index : Positive)
      return Checksum_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_Checksum_Constant_Reference
     (Self  : aliased Checksum_Vector;
      Index : Positive)
      return Checksum_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Breakpoint_Array, Breakpoint_Array_Access);

   overriding procedure Adjust (Self : in out Breakpoint_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Breakpoint_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Breakpoint_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : Breakpoint_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out Breakpoint_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out Breakpoint_Vector; Value : Breakpoint) is
      Init_Length     : constant Positive       :=
        Positive'Max (1, 256 / Breakpoint'Size);
      Self_Data_Saved : Breakpoint_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new Breakpoint_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Breakpoint_Array'
             (Self.Data.all & Breakpoint_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_Breakpoint_Variable_Reference
     (Self  : aliased in out Breakpoint_Vector;
      Index : Positive)
      return Breakpoint_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_Breakpoint_Constant_Reference
     (Self  : aliased Breakpoint_Vector;
      Index : Positive)
      return Breakpoint_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (StepInTarget_Array, StepInTarget_Array_Access);

   overriding procedure Adjust (Self : in out StepInTarget_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new StepInTarget_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out StepInTarget_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : StepInTarget_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out StepInTarget_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out StepInTarget_Vector; Value : StepInTarget) is
      Init_Length     : constant Positive         :=
        Positive'Max (1, 256 / StepInTarget'Size);
      Self_Data_Saved : StepInTarget_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new StepInTarget_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new StepInTarget_Array'
             (Self.Data.all & StepInTarget_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_StepInTarget_Variable_Reference
     (Self  : aliased in out StepInTarget_Vector;
      Index : Positive)
      return StepInTarget_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_StepInTarget_Constant_Reference
     (Self  : aliased StepInTarget_Vector;
      Index : Positive)
      return StepInTarget_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (FunctionBreakpoint_Array, FunctionBreakpoint_Array_Access);

   overriding procedure Adjust (Self : in out FunctionBreakpoint_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new FunctionBreakpoint_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out FunctionBreakpoint_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : FunctionBreakpoint_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out FunctionBreakpoint_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out FunctionBreakpoint_Vector; Value : FunctionBreakpoint) is
      Init_Length     : constant Positive               :=
        Positive'Max (1, 256 / FunctionBreakpoint'Size);
      Self_Data_Saved : FunctionBreakpoint_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new FunctionBreakpoint_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new FunctionBreakpoint_Array'
             (Self.Data.all &
              FunctionBreakpoint_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_FunctionBreakpoint_Variable_Reference
     (Self  : aliased in out FunctionBreakpoint_Vector;
      Index : Positive)
      return FunctionBreakpoint_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_FunctionBreakpoint_Constant_Reference
     (Self  : aliased FunctionBreakpoint_Vector;
      Index : Positive)
      return FunctionBreakpoint_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (DataBreakpoint_Array, DataBreakpoint_Array_Access);

   overriding procedure Adjust (Self : in out DataBreakpoint_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new DataBreakpoint_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out DataBreakpoint_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : DataBreakpoint_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out DataBreakpoint_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out DataBreakpoint_Vector; Value : DataBreakpoint) is
      Init_Length     : constant Positive           :=
        Positive'Max (1, 256 / DataBreakpoint'Size);
      Self_Data_Saved : DataBreakpoint_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new DataBreakpoint_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new DataBreakpoint_Array'
             (Self.Data.all & DataBreakpoint_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_DataBreakpoint_Variable_Reference
     (Self  : aliased in out DataBreakpoint_Vector;
      Index : Positive)
      return DataBreakpoint_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_DataBreakpoint_Constant_Reference
     (Self  : aliased DataBreakpoint_Vector;
      Index : Positive)
      return DataBreakpoint_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (ExceptionOptions_Array, ExceptionOptions_Array_Access);

   overriding procedure Adjust (Self : in out ExceptionOptions_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new ExceptionOptions_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out ExceptionOptions_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : ExceptionOptions_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out ExceptionOptions_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out ExceptionOptions_Vector; Value : ExceptionOptions) is
      Init_Length     : constant Positive             :=
        Positive'Max (1, 256 / ExceptionOptions'Size);
      Self_Data_Saved : ExceptionOptions_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new ExceptionOptions_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new ExceptionOptions_Array'
             (Self.Data.all & ExceptionOptions_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_ExceptionOptions_Variable_Reference
     (Self  : aliased in out ExceptionOptions_Vector;
      Index : Positive)
      return ExceptionOptions_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_ExceptionOptions_Constant_Reference
     (Self  : aliased ExceptionOptions_Vector;
      Index : Positive)
      return ExceptionOptions_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Integer_Array, Integer_Array_Access);

   overriding procedure Adjust (Self : in out Integer_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Integer_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Integer_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : Integer_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out Integer_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out Integer_Vector; Value : Integer) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Integer'Size);
      Self_Data_Saved : Integer_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new Integer_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Integer_Array'
             (Self.Data.all & Integer_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_Integer_Variable_Reference
     (Self  : aliased in out Integer_Vector;
      Index : Positive)
      return Integer_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_Integer_Constant_Reference
     (Self  : aliased Integer_Vector;
      Index : Positive)
      return Integer_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (GotoTarget_Array, GotoTarget_Array_Access);

   overriding procedure Adjust (Self : in out GotoTarget_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new GotoTarget_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out GotoTarget_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : GotoTarget_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out GotoTarget_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out GotoTarget_Vector; Value : GotoTarget) is
      Init_Length     : constant Positive       :=
        Positive'Max (1, 256 / GotoTarget'Size);
      Self_Data_Saved : GotoTarget_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new GotoTarget_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new GotoTarget_Array'
             (Self.Data.all & GotoTarget_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_GotoTarget_Variable_Reference
     (Self  : aliased in out GotoTarget_Vector;
      Index : Positive)
      return GotoTarget_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_GotoTarget_Constant_Reference
     (Self  : aliased GotoTarget_Vector;
      Index : Positive)
      return GotoTarget_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (InvalidatedAreas_Array, InvalidatedAreas_Array_Access);

   overriding procedure Adjust (Self : in out InvalidatedAreas_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new InvalidatedAreas_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out InvalidatedAreas_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : InvalidatedAreas_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out InvalidatedAreas_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out InvalidatedAreas_Vector; Value : Enum.InvalidatedAreas) is
      Init_Length     : constant Positive             :=
        Positive'Max (1, 256 / Enum.InvalidatedAreas'Size);
      Self_Data_Saved : InvalidatedAreas_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new InvalidatedAreas_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new InvalidatedAreas_Array'
             (Self.Data.all & InvalidatedAreas_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_InvalidatedAreas_Variable_Reference
     (Self  : aliased in out InvalidatedAreas_Vector;
      Index : Positive)
      return InvalidatedAreas_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_InvalidatedAreas_Constant_Reference
     (Self  : aliased InvalidatedAreas_Vector;
      Index : Positive)
      return InvalidatedAreas_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (BreakpointLocation_Array, BreakpointLocation_Array_Access);

   overriding procedure Adjust (Self : in out BreakpointLocation_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new BreakpointLocation_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out BreakpointLocation_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : BreakpointLocation_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out BreakpointLocation_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out BreakpointLocation_Vector; Value : BreakpointLocation) is
      Init_Length     : constant Positive               :=
        Positive'Max (1, 256 / BreakpointLocation'Size);
      Self_Data_Saved : BreakpointLocation_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new BreakpointLocation_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new BreakpointLocation_Array'
             (Self.Data.all &
              BreakpointLocation_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_BreakpointLocation_Variable_Reference
     (Self  : aliased in out BreakpointLocation_Vector;
      Index : Positive)
      return BreakpointLocation_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_BreakpointLocation_Constant_Reference
     (Self  : aliased BreakpointLocation_Vector;
      Index : Positive)
      return BreakpointLocation_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (InstructionBreakpoint_Array, InstructionBreakpoint_Array_Access);

   overriding procedure Adjust (Self : in out InstructionBreakpoint_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new InstructionBreakpoint_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
     (Self : in out InstructionBreakpoint_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : InstructionBreakpoint_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out InstructionBreakpoint_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out InstructionBreakpoint_Vector;
      Value : InstructionBreakpoint) is
      Init_Length     : constant Positive                  :=
        Positive'Max (1, 256 / InstructionBreakpoint'Size);
      Self_Data_Saved : InstructionBreakpoint_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new InstructionBreakpoint_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new InstructionBreakpoint_Array'
             (Self.Data.all &
              InstructionBreakpoint_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_InstructionBreakpoint_Variable_Reference
     (Self  : aliased in out InstructionBreakpoint_Vector;
      Index : Positive)
      return InstructionBreakpoint_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_InstructionBreakpoint_Constant_Reference
     (Self  : aliased InstructionBreakpoint_Vector;
      Index : Positive)
      return InstructionBreakpoint_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (StackFrame_Array, StackFrame_Array_Access);

   overriding procedure Adjust (Self : in out StackFrame_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new StackFrame_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out StackFrame_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : StackFrame_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out StackFrame_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out StackFrame_Vector; Value : StackFrame) is
      Init_Length     : constant Positive       :=
        Positive'Max (1, 256 / StackFrame'Size);
      Self_Data_Saved : StackFrame_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new StackFrame_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new StackFrame_Array'
             (Self.Data.all & StackFrame_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_StackFrame_Variable_Reference
     (Self  : aliased in out StackFrame_Vector;
      Index : Positive)
      return StackFrame_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_StackFrame_Constant_Reference
     (Self  : aliased StackFrame_Vector;
      Index : Positive)
      return StackFrame_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Scope_Array, Scope_Array_Access);

   overriding procedure Adjust (Self : in out Scope_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Scope_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Scope_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : Scope_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out Scope_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out Scope_Vector; Value : Scope) is
      Init_Length : constant Positive  := Positive'Max (1, 256 / Scope'Size);
      Self_Data_Saved : Scope_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new Scope_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Scope_Array'
             (Self.Data.all & Scope_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_Scope_Variable_Reference
     (Self  : aliased in out Scope_Vector;
      Index : Positive)
      return Scope_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_Scope_Constant_Reference
     (Self  : aliased Scope_Vector;
      Index : Positive)
      return Scope_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Variable_Array, Variable_Array_Access);

   overriding procedure Adjust (Self : in out Variable_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Variable_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Variable_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : Variable_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out Variable_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out Variable_Vector; Value : Variable) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Variable'Size);
      Self_Data_Saved : Variable_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new Variable_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Variable_Array'
             (Self.Data.all & Variable_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_Variable_Variable_Reference
     (Self  : aliased in out Variable_Vector;
      Index : Positive)
      return Variable_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_Variable_Constant_Reference
     (Self  : aliased Variable_Vector;
      Index : Positive)
      return Variable_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Source_Array, Source_Array_Access);

   overriding procedure Adjust (Self : in out Source_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Source_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Source_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : Source_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out Source_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out Source_Vector; Value : Source) is
      Init_Length : constant Positive   := Positive'Max (1, 256 / Source'Size);
      Self_Data_Saved : Source_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new Source_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Source_Array'
             (Self.Data.all & Source_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_Source_Variable_Reference
     (Self  : aliased in out Source_Vector;
      Index : Positive)
      return Source_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_Source_Constant_Reference
     (Self  : aliased Source_Vector;
      Index : Positive)
      return Source_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (VariablePresentationHint_attributes_Array,
      VariablePresentationHint_attributes_Array_Access);

   overriding procedure Adjust
     (Self : in out VariablePresentationHint_attributes_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new VariablePresentationHint_attributes_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
     (Self : in out VariablePresentationHint_attributes_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length
     (Self : VariablePresentationHint_attributes_Vector) return Natural is
     (Self.Length);

   procedure Clear
     (Self : in out VariablePresentationHint_attributes_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out VariablePresentationHint_attributes_Vector;
      Value : Enum.VariablePresentationHint_attributes) is
      Init_Length     : constant Positive                                :=
        Positive'Max (1, 256 / Enum.VariablePresentationHint_attributes'Size);
      Self_Data_Saved : VariablePresentationHint_attributes_Array_Access :=
        Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data :=
           new VariablePresentationHint_attributes_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new VariablePresentationHint_attributes_Array'
             (Self.Data.all &
              VariablePresentationHint_attributes_Array'
                (1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_VariablePresentationHint_attributes_Variable_Reference
     (Self  : aliased in out VariablePresentationHint_attributes_Vector;
      Index : Positive)
      return VariablePresentationHint_attributes_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_VariablePresentationHint_attributes_Constant_Reference
     (Self  : aliased VariablePresentationHint_attributes_Vector;
      Index : Positive)
      return VariablePresentationHint_attributes_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (SourceBreakpoint_Array, SourceBreakpoint_Array_Access);

   overriding procedure Adjust (Self : in out SourceBreakpoint_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new SourceBreakpoint_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out SourceBreakpoint_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : SourceBreakpoint_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out SourceBreakpoint_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out SourceBreakpoint_Vector; Value : SourceBreakpoint) is
      Init_Length     : constant Positive             :=
        Positive'Max (1, 256 / SourceBreakpoint'Size);
      Self_Data_Saved : SourceBreakpoint_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new SourceBreakpoint_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new SourceBreakpoint_Array'
             (Self.Data.all & SourceBreakpoint_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_SourceBreakpoint_Variable_Reference
     (Self  : aliased in out SourceBreakpoint_Vector;
      Index : Positive)
      return SourceBreakpoint_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_SourceBreakpoint_Constant_Reference
     (Self  : aliased SourceBreakpoint_Vector;
      Index : Positive)
      return SourceBreakpoint_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (ChecksumAlgorithm_Array, ChecksumAlgorithm_Array_Access);

   overriding procedure Adjust (Self : in out ChecksumAlgorithm_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new ChecksumAlgorithm_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out ChecksumAlgorithm_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : ChecksumAlgorithm_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out ChecksumAlgorithm_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out ChecksumAlgorithm_Vector;
      Value : Enum.ChecksumAlgorithm) is
      Init_Length     : constant Positive              :=
        Positive'Max (1, 256 / Enum.ChecksumAlgorithm'Size);
      Self_Data_Saved : ChecksumAlgorithm_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new ChecksumAlgorithm_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new ChecksumAlgorithm_Array'
             (Self.Data.all &
              ChecksumAlgorithm_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_ChecksumAlgorithm_Variable_Reference
     (Self  : aliased in out ChecksumAlgorithm_Vector;
      Index : Positive)
      return ChecksumAlgorithm_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_ChecksumAlgorithm_Constant_Reference
     (Self  : aliased ChecksumAlgorithm_Vector;
      Index : Positive)
      return ChecksumAlgorithm_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (ExceptionBreakpointsFilter_Array,
      ExceptionBreakpointsFilter_Array_Access);

   overriding procedure Adjust
     (Self : in out ExceptionBreakpointsFilter_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new ExceptionBreakpointsFilter_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
     (Self : in out ExceptionBreakpointsFilter_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : ExceptionBreakpointsFilter_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out ExceptionBreakpointsFilter_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out ExceptionBreakpointsFilter_Vector;
      Value : ExceptionBreakpointsFilter) is
      Init_Length     : constant Positive                       :=
        Positive'Max (1, 256 / ExceptionBreakpointsFilter'Size);
      Self_Data_Saved : ExceptionBreakpointsFilter_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new ExceptionBreakpointsFilter_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new ExceptionBreakpointsFilter_Array'
             (Self.Data.all &
              ExceptionBreakpointsFilter_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_ExceptionBreakpointsFilter_Variable_Reference
     (Self  : aliased in out ExceptionBreakpointsFilter_Vector;
      Index : Positive)
      return ExceptionBreakpointsFilter_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_ExceptionBreakpointsFilter_Constant_Reference
     (Self  : aliased ExceptionBreakpointsFilter_Vector;
      Index : Positive)
      return ExceptionBreakpointsFilter_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (CompletionItem_Array, CompletionItem_Array_Access);

   overriding procedure Adjust (Self : in out CompletionItem_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new CompletionItem_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out CompletionItem_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : CompletionItem_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out CompletionItem_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out CompletionItem_Vector; Value : CompletionItem) is
      Init_Length     : constant Positive           :=
        Positive'Max (1, 256 / CompletionItem'Size);
      Self_Data_Saved : CompletionItem_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new CompletionItem_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new CompletionItem_Array'
             (Self.Data.all & CompletionItem_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_CompletionItem_Variable_Reference
     (Self  : aliased in out CompletionItem_Vector;
      Index : Positive)
      return CompletionItem_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_CompletionItem_Constant_Reference
     (Self  : aliased CompletionItem_Vector;
      Index : Positive)
      return CompletionItem_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (ExceptionPathSegment_Array, ExceptionPathSegment_Array_Access);

   overriding procedure Adjust (Self : in out ExceptionPathSegment_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new ExceptionPathSegment_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out ExceptionPathSegment_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : ExceptionPathSegment_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out ExceptionPathSegment_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out ExceptionPathSegment_Vector;
      Value : ExceptionPathSegment) is
      Init_Length     : constant Positive                 :=
        Positive'Max (1, 256 / ExceptionPathSegment'Size);
      Self_Data_Saved : ExceptionPathSegment_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new ExceptionPathSegment_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new ExceptionPathSegment_Array'
             (Self.Data.all &
              ExceptionPathSegment_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_ExceptionPathSegment_Variable_Reference
     (Self  : aliased in out ExceptionPathSegment_Vector;
      Index : Positive)
      return ExceptionPathSegment_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_ExceptionPathSegment_Constant_Reference
     (Self  : aliased ExceptionPathSegment_Vector;
      Index : Positive)
      return ExceptionPathSegment_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (DataBreakpointAccessType_Array, DataBreakpointAccessType_Array_Access);

   overriding procedure Adjust
     (Self : in out DataBreakpointAccessType_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new DataBreakpointAccessType_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
     (Self : in out DataBreakpointAccessType_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : DataBreakpointAccessType_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out DataBreakpointAccessType_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out DataBreakpointAccessType_Vector;
      Value : Enum.DataBreakpointAccessType) is
      Init_Length     : constant Positive                     :=
        Positive'Max (1, 256 / Enum.DataBreakpointAccessType'Size);
      Self_Data_Saved : DataBreakpointAccessType_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new DataBreakpointAccessType_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new DataBreakpointAccessType_Array'
             (Self.Data.all &
              DataBreakpointAccessType_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_DataBreakpointAccessType_Variable_Reference
     (Self  : aliased in out DataBreakpointAccessType_Vector;
      Index : Positive)
      return DataBreakpointAccessType_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_DataBreakpointAccessType_Constant_Reference
     (Self  : aliased DataBreakpointAccessType_Vector;
      Index : Positive)
      return DataBreakpointAccessType_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (DisassembledInstruction_Array, DisassembledInstruction_Array_Access);

   overriding procedure Adjust
     (Self : in out DisassembledInstruction_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new DisassembledInstruction_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
     (Self : in out DisassembledInstruction_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : DisassembledInstruction_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out DisassembledInstruction_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out DisassembledInstruction_Vector;
      Value : DisassembledInstruction) is
      Init_Length     : constant Positive                    :=
        Positive'Max (1, 256 / DisassembledInstruction'Size);
      Self_Data_Saved : DisassembledInstruction_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new DisassembledInstruction_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new DisassembledInstruction_Array'
             (Self.Data.all &
              DisassembledInstruction_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_DisassembledInstruction_Variable_Reference
     (Self  : aliased in out DisassembledInstruction_Vector;
      Index : Positive)
      return DisassembledInstruction_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_DisassembledInstruction_Constant_Reference
     (Self  : aliased DisassembledInstruction_Vector;
      Index : Positive)
      return DisassembledInstruction_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Module_Array, Module_Array_Access);

   overriding procedure Adjust (Self : in out Module_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Module_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Module_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : Module_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out Module_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out Module_Vector; Value : Module) is
      Init_Length : constant Positive   := Positive'Max (1, 256 / Module'Size);
      Self_Data_Saved : Module_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new Module_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Module_Array'
             (Self.Data.all & Module_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_Module_Variable_Reference
     (Self  : aliased in out Module_Vector;
      Index : Positive)
      return Module_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_Module_Constant_Reference
     (Self  : aliased Module_Vector;
      Index : Positive)
      return Module_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (ExceptionFilterOptions_Array, ExceptionFilterOptions_Array_Access);

   overriding procedure Adjust (Self : in out ExceptionFilterOptions_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new ExceptionFilterOptions_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
     (Self : in out ExceptionFilterOptions_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : ExceptionFilterOptions_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out ExceptionFilterOptions_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out ExceptionFilterOptions_Vector;
      Value : ExceptionFilterOptions) is
      Init_Length     : constant Positive                   :=
        Positive'Max (1, 256 / ExceptionFilterOptions'Size);
      Self_Data_Saved : ExceptionFilterOptions_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new ExceptionFilterOptions_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new ExceptionFilterOptions_Array'
             (Self.Data.all &
              ExceptionFilterOptions_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_ExceptionFilterOptions_Variable_Reference
     (Self  : aliased in out ExceptionFilterOptions_Vector;
      Index : Positive)
      return ExceptionFilterOptions_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_ExceptionFilterOptions_Constant_Reference
     (Self  : aliased ExceptionFilterOptions_Vector;
      Index : Positive)
      return ExceptionFilterOptions_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (ColumnDescriptor_Array, ColumnDescriptor_Array_Access);

   overriding procedure Adjust (Self : in out ColumnDescriptor_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new ColumnDescriptor_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out ColumnDescriptor_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : ColumnDescriptor_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out ColumnDescriptor_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out ColumnDescriptor_Vector; Value : ColumnDescriptor) is
      Init_Length     : constant Positive             :=
        Positive'Max (1, 256 / ColumnDescriptor'Size);
      Self_Data_Saved : ColumnDescriptor_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new ColumnDescriptor_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new ColumnDescriptor_Array'
             (Self.Data.all & ColumnDescriptor_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_ColumnDescriptor_Variable_Reference
     (Self  : aliased in out ColumnDescriptor_Vector;
      Index : Positive)
      return ColumnDescriptor_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_ColumnDescriptor_Constant_Reference
     (Self  : aliased ColumnDescriptor_Vector;
      Index : Positive)
      return ColumnDescriptor_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (ExceptionDetails_Array, ExceptionDetails_Array_Access);

   overriding procedure Adjust (Self : in out ExceptionDetails_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new ExceptionDetails_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out ExceptionDetails_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : ExceptionDetails_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out ExceptionDetails_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out ExceptionDetails_Vector; Value : ExceptionDetails) is
      Init_Length     : constant Positive             :=
        Positive'Max (1, 256 / ExceptionDetails'Size);
      Self_Data_Saved : ExceptionDetails_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new ExceptionDetails_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new ExceptionDetails_Array'
             (Self.Data.all & ExceptionDetails_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_ExceptionDetails_Variable_Reference
     (Self  : aliased in out ExceptionDetails_Vector;
      Index : Positive)
      return ExceptionDetails_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_ExceptionDetails_Constant_Reference
     (Self  : aliased ExceptionDetails_Vector;
      Index : Positive)
      return ExceptionDetails_Constant_Reference is
     (Element => Self.Data (Index)'Access);

end DAP.Tools;
