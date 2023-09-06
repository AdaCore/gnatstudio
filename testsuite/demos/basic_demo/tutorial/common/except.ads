--  Basic exceptions used throughout SDC

package Except is

   type X is limited private;

   Internal_Error : exception;
   --  Raised when an internal error is detected in one of SDC's modules.

   Exit_SDC       : exception;
   --  Should be raised to have a clean exit from SDC.

   User_Error     : exception;
   --  Generic exception raised upon encountering a user input error.
   --  This exception is handled during lexical analysis in package Tokens.

   procedure Foo;

private


   type X is limited record
      Field : Boolean;
   end record;

end Except;
