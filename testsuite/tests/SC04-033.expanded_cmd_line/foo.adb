procedure Foo 
is

   procedure Bar;
   procedure FooBar;

   procedure Bar is
   begin
      FooBar;
   end Bar;
   procedure FooBar is
   begin
      null;
   end FooBar;

begin
   Bar;
end Foo;
