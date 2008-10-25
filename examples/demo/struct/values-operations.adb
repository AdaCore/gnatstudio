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

      elsif Op = "#" then
         return Matrix;
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

      Result   : Integer;
      Result_M : Matrix_Type;

   begin
      case V1.Kind is
         when Int =>
            case Op is
               when Add =>
                  Result := V1.E + V2.E;
                  Stack.Push (new Value_Info'(Kind => Int, E => Result));

               when Div =>
                  Result := V1.E / V2.E;
                  Stack.Push (new Value_Info'(Kind => Int, E => Result));

               when Mul =>
                  Result := V1.E * V2.E;
                  Stack.Push (new Value_Info'(Kind => Int, E => Result));

               when Sub =>
                  Result := V1.E - V2.E;
                  Stack.Push (new Value_Info'(Kind => Int, E => Result));

               when Matrix =>
                  Result_M := Alloc (2, 2);
                  Set (Result_M, 1, 0, V1.E);
                  Set (Result_M, 1, 1, V2.E);
                  Set (Result_M, 0, 1, Stack.Pop.E);
                  Set (Result_M, 0, 0, Stack.Pop.E);
                  Stack.Push (new Value_Info'(Kind => Matrix, M => Result_M));

            end case;

         when Matrix =>
            --  This is a Matrix operation

            case Op is
               when Add =>
                  Result_M := V1.M + V2.M;

               when Div =>
                  raise Except.User_Error;

               when Mul =>
                  Result_M := V1.M * V2.M;

               when Sub =>
                  raise Except.User_Error;

               when Matrix =>
                  raise Except.User_Error;
            end case;

            Stack.Push (new Value_Info'(Kind => Matrix, M => Result_M));
      end case;

   exception
      --  If we get a Constraint_Error exception, then we had a computation
      --  overflow or a divide by zero.

      when Constraint_Error =>
         Screen_Output.Error_Msg ("Operation error. Values popped.");
         raise;

   end Process;

end Values.Operations;


