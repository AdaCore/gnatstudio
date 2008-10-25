--  The operations allowed in SDC (addition, subtraction, multiplication, etc.)
--  It must be a child package of Values, since the operations must have
--  access to the internal structure of a Value.

package Values.Operations is

   type Operation is private;
   --  The actual operation type.

   function Read (Op : String) return Operation;
   --  If Op contains the characters of a valid operation the operation is
   --  returned, otherwise Except.User_Error is raised.

   procedure Process (Op : Operation);
   --  Processes an Operation.

private

   type Operation is (Add, Div, Mul, Sub, Matrix);

end Values.Operations;


