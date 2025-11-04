------------------------------------------------------------------------------
--  Test cases for string utility functions
--
--  Module under test: common/core/src/string_utils.adb
--  Ported from: testsuite/tests/TB09-023.text_utils.delete (concepts)
------------------------------------------------------------------------------

with AUnit;
with AUnit.Test_Cases;

package String_Utils_Tests is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Test_Case);

   function Name (T : Test_Case) return AUnit.Message_String;

   --  Individual test procedures
   procedure Test_String_Hash (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_String_Equal (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_VSS_Strings (T : in out AUnit.Test_Cases.Test_Case'Class);

end String_Utils_Tests;
