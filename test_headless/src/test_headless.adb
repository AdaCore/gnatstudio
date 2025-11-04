------------------------------------------------------------------------------
--  Minimal headless test to verify core stack compiles
--  Tests: GNATCOLL core, VFS, VSS (string handling), XMLAda
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNATCOLL.Projects;
with VSS.Strings;
with Sax.Readers;

procedure Test_Headless is
   VF : Virtual_File;
   VS : VSS.Strings.Virtual_String;
begin
   Put_Line ("=== GNATstudio Headless Test ===");
   Put_Line ("");

   --  Test GNATCOLL.VFS
   VF := Create_From_UTF8 ("/tmp/test.txt");
   Put_Line ("GNATCOLL.VFS:      OK - Created: " & VF.Display_Full_Name);

   --  Test VSS strings
   VS := VSS.Strings.To_Virtual_String ("Hello from VSS!");
   Put_Line ("VSS.Strings:       OK - Virtual strings working");

   --  Test basic types available
   Put_Line ("GNATCOLL.Projects: OK - Package available");
   Put_Line ("Sax.Readers:       OK - XMLAda SAX available");

   Put_Line ("");
   Put_Line ("✓ All core libraries compile and link successfully!");
   Put_Line ("✓ Headless build verified - ready for TUI!");
end Test_Headless;
