with Ada.Text_IO; use Ada.Text_IO;

procedure Foo is
   X    : Integer := 12;
   Y    : Integer := 15;
   Name : String := "Leo";
begin
   Put_Line (fThe name is {Name} and the sum is {X + Y}.");
end Foo;
