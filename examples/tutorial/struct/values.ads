--  Values (such as integers, reals, complex numbers, strings, etc.)
--  manipulated by SDC.

with Matrix_Binding; use Matrix_Binding;

package Values is

   type Value is private;
   --  The actual value type.

   function To_String (V : Value) return String;
   --  Returns a String representation of the Value.

   function Read (Word : String) return Value;
   --  If Word contains a value, the value is returned, otherwise
   --  Except.User_Error is raised.

   procedure Process (V : Value);
   --  Processes a Value.

private

   --  For now integer and matrices are the only data type available in SDC.

   type Value_Kind is (Int, Matrix);

   type Value_Info (Kind : Value_Kind) is record
      case Kind is
         when Int =>
            E : Integer;

         when Matrix =>
            M : Matrix_Type;
      end case;
   end record;

   type Value is access Value_Info;

end Values;
