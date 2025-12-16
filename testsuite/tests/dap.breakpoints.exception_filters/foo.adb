with Ada.Text_IO;

procedure Foo is
   A : Natural;
begin
   A := 1;

   begin
      A := A - 3;
   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line ("Caught Constraint_Error as expected.");
         A := 42;
   end;

   raise Program_Error with "Just testing exception filters.";

exception
   when Program_Error =>
      Ada.Text_IO.Put_Line ("Caught Program_Error as expected.");
end Foo;

