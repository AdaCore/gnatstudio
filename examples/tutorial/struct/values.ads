--  Values (such as integers, reals, complex numbers, strings, etc.)
--  manipulated by SDC.

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

   --  For now integer is the only data type available in SDC.

   type Value_Info is record
      E : Integer;
   end record;

   type Value is access Value_Info;

end Values;
