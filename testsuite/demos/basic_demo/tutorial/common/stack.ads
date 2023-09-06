
--  This package implements a stack.

with Values; use Values;

package Stack is

   Overflow  : exception;
   --  Raised if operation Push below is called when the stack is full.

   Underflow : exception;
   --  Raised if operations Pop/Top below are called when the stack is empty.

   procedure Push (V : Value);
   --  Pushes value onto the stack.
   --  If the stack is full Stack.Overflow is raised.

   function Pop return Value;
   --  Pops a value off the stack and returns it. If the pop fails
   --  because the stack is empty Stack.Underflow is raised.

   function Empty return Boolean;
   --  Returns True if the Stack is empty.

   procedure Clear;
   --  Empties the stack.

   function Top return Value;
   --  Returns the value on top of the stack. If the stack is empty
   --  the exception Stack.Underflow is raised.

   procedure View;
   --  Prints the contents of the Stack on the screen.
   --  Values are printed in the order in which they occur in the Stack.

end Stack;
