--  Implements the instructions available in sdc.

package Instructions is

   type Instruction is private;
   --  The actual instruction type.

   function Read (Word : String) return Instruction;
   --  If Word contains the name of a valid instruction the instruction is
   --  returned, otherwise Except.User_Error is raised.

   procedure Process (I : Instruction);
   --  Processes an Instruction.

private

   type Instruction is (Clear, Print, Quit);

end Instructions;


