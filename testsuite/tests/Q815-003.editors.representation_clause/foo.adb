with A;

procedure Foo is
   type Bar is new Integer range 1 .. 100;
begin
   A.E := 42;
   A.C;
end Foo;
