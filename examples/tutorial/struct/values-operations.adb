with Except;
with Screen_Output;
with Stack;

package body Values.Operations is

   ----------
   -- Read --
   ----------

   function Read (Op : String) return Operation is
   begin
      if Op = "+" then
         return Add;

      elsif Op = "-" then
         return Sub;

      elsif Op = "*" then
         return Mul;

      elsif Op = "/" then
         return Div;

      else
         raise Except.User_Error;
      end if;
   end Read;

   -------------
   -- Process --
   -------------

   procedure Process (Op : Operation) is
      V2 : Value := Stack.Pop;
      V1 : Value := Stack.Pop;

      Result : Integer;

   begin
      case Op is
         when Add =>
            Result := V1.E + V2.E;

         when Div =>
            Result := V1.E / V2.E;

         when Mul =>
            Result := V1.E * V2.E;

         when Sub =>
            Result := V1.E - V2.E;
      end case;

      --  Create an integer Value by setting the field "E" of the record
      --  to Result.

      Stack.Push (new Value_Info'(E => Result));

   exception
      --  If we get a Constraint_Error exception, then we had a computation
      --  overflow or a divide by zero.

      when Constraint_Error =>
         Screen_Output.Error_Msg ("Operation error. Values popped.");

   end Process;

end Values.Operations;


