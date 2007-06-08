with Ada.Text_IO;      use Ada.Text_IO;
with Scripts;          use Scripts;
with Scripts.Python;   use Scripts.Python;
with Scripts.Shell;    use Scripts.Shell;
with Testsuite_Export; use Testsuite_Export;
with TestConsole;      use TestConsole;

procedure TestAPI is
   use String_Lists;
   Repo    : Scripts_Repository;
   Console : aliased Test_Console;

   procedure Completions (Input : String; Lang : Scripting_Language);
   procedure Completions (Input : String; Lang : Scripting_Language) is
      Completions : String_Lists.List;
      C : String_Lists.Cursor;
   begin
      Put_Line ("Completions for " & Input & " (" & Get_Name (Lang) & "):");
      Complete (Lang, Input, Completions);
      C := First (Completions);
      while Has_Element (C) loop
         Put (Element (C) & ", ");
         Next (C);
      end loop;
      New_Line;
   end Completions;

   Py : Scripting_Language;
   Sh : Scripting_Language;

begin
   Initialize (Repo);
   Register_Shell_Scripting (Repo);
   Register_Python_Scripting (Repo, "M");
   Register_Standard_Classes (Repo, "Console");

   Testsuite_Export.Register_Functions (Repo);

   Py := Lookup_Scripting_Language (Repo, "python");
   Sh := Lookup_Scripting_Language (Repo, "shell");

   Set_Default_Console (Py, Console'Unchecked_Access);
   Completions ("No", Py);
   Completions ("M", Py);
   Completions ("M.C", Py);
   Completions ("M.C1.", Py);
   Completions ("M.C1.me", Py);

   Set_Default_Console (Sh, Console'Unchecked_Access);
   Completions ("C1", Sh);
   Completions ("", Sh);
   Completions ("Ba", Sh);

   Free (Console);
   Destroy (Repo);
end TestAPI;
