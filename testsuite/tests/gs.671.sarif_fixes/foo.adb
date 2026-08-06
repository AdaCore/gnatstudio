with Ada.Text_IO; use Ada.Text_IO;

procedure Foo is
   S : constant String := "Hello";
   C : Character;
begin
   null;
   null;

   for I in S'Range loop
      C := S (I);
   end loop;

   Put_Line ("Done");
end Foo;
