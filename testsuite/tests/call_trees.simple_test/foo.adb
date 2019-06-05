with Ada.Text_IO;

procedure Foo is

   procedure Bar;
   procedure FooBar;
 
   procedure Bar is
   begin
      FooBar;
      FooBar;
   end Bar;

   procedure FooBar is
   begin
      Ada.Text_IO.Put_Line ("!");
   end FooBar;
begin
   Bar;
   FooBar;
end Foo;
