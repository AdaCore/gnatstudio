------------------------------------------------------------------------------
--  AUnit Test Runner for GNATstudio TUI
--
--  This is the main test harness. Add test suites here as we build them.
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites; use AUnit.Test_Suites;

with String_Utils_Suite;

procedure Test_Runner is

   procedure Run is new AUnit.Run.Test_Runner (String_Utils_Suite.Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   Put_Line ("=== GNATstudio TUI Test Suite ===");
   Put_Line ("");

   Run (Reporter);

   Put_Line ("");
   Put_Line ("=== Test run complete ===");
end Test_Runner;
