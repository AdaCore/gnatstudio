------------------------------------------------------------------------------
--  Test cases for string utility functions
------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with VSS.Strings; use VSS.Strings;

package body String_Utils_Tests is

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_VSS_Strings'Access, "VSS Virtual Strings");
      Register_Routine (T, Test_String_Hash'Access, "VSS String Length");
      Register_Routine (T, Test_String_Equal'Access, "String Equality");
   end Register_Tests;

   function Name (T : Test_Case) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("String Utilities Tests");
   end Name;

   --  Test VSS string creation and length
   procedure Test_String_Hash (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      VS : VSS.Strings.Virtual_String;
   begin
      VS := VSS.Strings.To_Virtual_String ("Test");
      Assert (VS.Character_Length = 4,
              "Virtual string should have length 4");

      VS := VSS.Strings.To_Virtual_String ("");
      Assert (VS.Is_Empty,
              "Empty virtual string should report Is_Empty");
   end Test_String_Hash;

   --  Test string equality (basic smoke test)
   procedure Test_String_Equal (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      S1 : constant String := "Hello";
      S2 : constant String := "Hello";
      S3 : constant String := "World";
   begin
      Assert (S1 = S2, "Equal strings should be equal");
      Assert (S1 /= S3, "Different strings should not be equal");
   end Test_String_Equal;

   --  Test VSS string operations
   procedure Test_VSS_Strings (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      VS1, VS2 : VSS.Strings.Virtual_String;
   begin
      VS1 := VSS.Strings.To_Virtual_String ("Hello");
      VS2 := VSS.Strings.To_Virtual_String ("Hello");

      Assert (VS1 = VS2, "Equal VSS strings should be equal");
      Assert (not VS1.Is_Empty, "Non-empty string should not be empty");

      VS1 := VSS.Strings.Empty_Virtual_String;
      Assert (VS1.Is_Empty, "Empty VSS string should be empty");
      Assert (VS1.Character_Length = 0, "Empty string should have length 0");
   end Test_VSS_Strings;

end String_Utils_Tests;
