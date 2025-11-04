------------------------------------------------------------------------------
--  Absolute minimal test - just core libraries, no kernel
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.VFS_Utils;
with VSS.Strings;

procedure Test_Minimal is
   VS : VSS.Strings.Virtual_String;
begin
   Put_Line ("=== Minimal Headless Test ===");
   Put_Line ("");

   --  Test VSS strings
   VS := VSS.Strings.To_Virtual_String ("Headless build works!");
   Put_Line ("VSS.Strings:       OK");

   --  Test GNATCOLL available
   Put_Line ("GNATCOLL.VFS_Utils: OK");

   Put_Line ("");
   Put_Line ("✓ Core libraries compile headless!");
   Put_Line ("✓ Ready for TUI implementation!");
end Test_Minimal;
