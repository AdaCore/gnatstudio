with Glide_Kernel;           use Glide_Kernel;

with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix.Text_Manager;   use Codefix.Text_Manager;

package Commands.Codefix is

   type Codefix_Command is new Root_Command with record
      Error : Error_Id;
      Current_Text : Ptr_Text_Navigator;
      Corrector    : Ptr_Correction_Manager;
      Kernel       : Kernel_Handle;
   end record;

   function Execute (Command : access Codefix_Command) return Boolean;
   --  Fix the error recorded in the Codefix_Command.

   procedure Free (Command : in out Codefix_Command);
   --  Do not do anyting, as far as nothing has to be freed in Codefix_Command.

end Commands.Codefix;
