with Scripts;        use Scripts;
with Scripts.Python; use Scripts.Python;
with Scripts.Shell;  use Scripts.Shell;

package body Common is

   procedure On_Hello (Data : in out Callback_Data'Class; Command : String);

   procedure On_Hello (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value (Data, "Hello " & Nth_Arg (Data, 1, "world") & " !");
   end On_Hello;

   ------------------------------------
   -- Register_Scripts_And_Functions --
   ------------------------------------

   function Register_Scripts_And_Functions return Scripts.Scripts_Repository is
      Repo : Scripts_Repository;
   begin
      --  Register all scripting languages. In practice, you only need to
      --  register those you intend to support

      Initialize (Repo);
      Register_Shell_Scripting  (Repo);
      Register_Python_Scripting (Repo, "Hello");
      Register_Standard_Classes (Repo, "Console");

      --  Now register our custom functions. Note that we do not need to
      --  register them once for every support language, once is enough, they
      --  are automatically exported to all registered languages.

      --  Available as "Hello.hello("world")" in python,
      --  and "hello world" in shell script
      Register_Command
        (Repo, "hello", 0, 1,
         Handler => On_Hello'Unrestricted_Access);

      return Repo;
   end Register_Scripts_And_Functions;

end Common;
