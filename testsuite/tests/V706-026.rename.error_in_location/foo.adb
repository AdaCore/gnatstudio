procedure Foo is
   Bar : Integer := 1;
   function Foo_Bar (I : Integer) return Integer is (I + 1);
begin
   Bar := Bar + Foo_Bar (Bar);
end Foo;
