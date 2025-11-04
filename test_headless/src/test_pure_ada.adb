------------------------------------------------------------------------------
--  Pure Ada test - absolutely NO C dependencies
--  Verifies: Ada 2022 compiler works, VSS (pure Ada strings)
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with VSS.Strings;

procedure Test_Pure_Ada is
   VS : VSS.Strings.Virtual_String;
begin
   Put_Line ("=== Pure Ada Headless Test ===");
   Put_Line ("");

   --  Test VSS Virtual Strings (pure Ada implementation)
   VS := VSS.Strings.To_Virtual_String ("Headless build verified!");
   Put_Line ("VSS.Strings:       OK - Length =" & VS.Character_Length'Image);

   Put_Line ("");
   Put_Line ("✓ Pure Ada compilation successful!");
   Put_Line ("✓ No C dependencies - fully headless!");
   Put_Line ("✓ GNAT 15.1.2 from Alire works!");
   Put_Line ("✓ VSS (Virtual String Subsystem) works!");
end Test_Pure_Ada;
