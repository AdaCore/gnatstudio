with Ada.Text_IO;    use Ada.Text_IO;
with Scripts;        use Scripts;
with Common;         use Common;
with TextConsole;    use TextConsole;

procedure NewClass is
   Repo   : Scripts_Repository :=
     Common.Register_Scripts_And_Functions;
   Buffer : String (1 .. 1000);
   Last   : Integer;
   Errors : Boolean;
   Console : aliased Text_Console;
begin
   Put_Line ("Please type python commands:");

   Set_Default_Console
     (Lookup_Scripting_Language (Repo, "python"), Console'Unchecked_Access);

   loop
      Get_Line (Buffer, Last);
      Execute_Command
        (Script       => Lookup_Scripting_Language (Repo, "python"),
         Command      => Buffer (1 .. Last),
         Show_Command => False,
         Hide_Output  => False,
         Errors       => Errors);
   end loop;

exception
   when End_Error =>
      Destroy (Repo);
end NewClass;
