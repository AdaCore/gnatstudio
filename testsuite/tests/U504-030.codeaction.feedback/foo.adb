procedure Foo is
   procedure Bar (X : Integer; Y : Integer);
   procedure Bar (X : Integer; Y : Integer) is
   begin
      null;
   end Bar;
begin
   Bar (Y => 1, X => 2);
end Foo;
