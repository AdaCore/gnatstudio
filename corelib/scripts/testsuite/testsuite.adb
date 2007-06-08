with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Scripts;          use Scripts;
with Scripts.Python;   use Scripts.Python;
with Scripts.Shell;    use Scripts.Shell;
with Testsuite_Export; use Testsuite_Export;
with TestConsole;      use TestConsole;

procedure Testsuite is
   Repo    : Scripts_Repository;
   Errors  : Boolean;
   Console : aliased Test_Console;
   Lang    : constant String := Argument (1);
   File    : constant String := Argument (2);
begin
   Initialize (Repo);

   --  Register a single language, so that we can more easily detect
   --  memory leaks in each case

   if Lang = Shell_Name then
      Register_Shell_Scripting (Repo);
   elsif Lang = Python_Name then
      Register_Python_Scripting (Repo, "M");
   end if;
   Register_Standard_Classes (Repo, "Console");

   Testsuite_Export.Register_Functions (Repo);

   Set_Default_Console
     (Lookup_Scripting_Language (Repo, Lang), Console'Unchecked_Access);

   Execute_File
     (Script       => Lookup_Scripting_Language (Repo, Lang),
      Filename     => File,
      Show_Command => False,
      Errors       => Errors);
   if Errors then
      Put_Line ("Errors were reported by Execute_File");
   end if;

   Free (Console);
   Destroy (Repo);

end Testsuite;
