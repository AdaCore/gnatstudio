package body Commands.Codefix is

   -------------
   -- Execute --
   -------------

   function Execute (Command : access Codefix_Command) return Boolean is
   begin
      Command_Finished (Command, True);

      return True;
   end Execute;

   ----------
   -- Free --
   ----------

   procedure Free (Command : in out Codefix_Command) is
      pragma Unreferenced (Command);
   begin
      null;
   end Free;

end Commands.Codefix;
