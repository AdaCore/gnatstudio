--  This package defines the notion of a token. In general terms a token is
--  the smallest lexical entity known to SDC.

with Instructions;
with Values;
with Values.Operations;

package Tokens is

   type Token_Kind is (Val, Op, Instr);

   type Token (Kind : Token_Kind) is private;
   --  The actual token type.

   function Next return Token;
   --  Reads the input characters typed by the user
   --  and converts them into tokens.

   procedure Process (T : Token);
   --  Process token T, ie undertake the actions corresponding to token T.

private

   type Token (Kind  : Token_Kind) is record
      case Kind is
         when Val =>
            Val   : Values.Value;
         when Op =>
            Op    : Values.Operations.Operation;
         when Instr =>
            Instr : Instructions.Instruction;
      end case;
   end record;

end Tokens;


