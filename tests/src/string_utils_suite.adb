------------------------------------------------------------------------------
--  Test suite for string utility functions
------------------------------------------------------------------------------

with AUnit.Test_Suites;
with String_Utils_Tests;

package body String_Utils_Suite is

   use AUnit.Test_Suites;

   Result : aliased Test_Suite;
   Test_Case : aliased String_Utils_Tests.Test_Case;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Case'Access);
      return Result'Access;
   end Suite;

end String_Utils_Suite;
