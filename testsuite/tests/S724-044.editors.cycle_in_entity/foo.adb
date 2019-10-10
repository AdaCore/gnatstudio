procedure Foo is
   procedure Bar;

   ---------
   -- Bar --
   ---------

   procedure Bar is
   begin
      null;
   end Bar;
begin
   declare
      I : Integer := 0;
   begin
      I := I + 1;
   end;
end Foo;
