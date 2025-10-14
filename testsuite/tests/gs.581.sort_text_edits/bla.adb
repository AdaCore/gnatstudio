procedure Bla is
   procedure Foo (I, J, K, Hello : Integer);

   ---------
   -- Foo --
   ---------

   procedure Foo (I, J, K, Hello : Integer) is
   begin
      null;
   end Foo;
begin
   Foo (1, 27, 32, 1000);
   Foo (1,
        27,
        32,
        1000);
end Bla;
