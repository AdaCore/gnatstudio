with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   task Foo is
      entry Start;
      entry Wait;
   end Foo;
   task Bar is
      entry Start;
   end Bar;

   task body Foo is
   begin
      accept Start;
      Bar.Start;
      accept Wait;
      Put_Line ("In task Foo");
   end Foo;

   task body Bar is
   begin
      accept Start;
      Put_Line ("In task Bar");
      Foo.Wait;
   end Bar;
begin
   Put_Line ("In main");
   Foo.Start;
end Main;
