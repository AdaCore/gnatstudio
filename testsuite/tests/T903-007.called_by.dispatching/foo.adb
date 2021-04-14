with Pack; use Pack;

procedure Foo is
   Y : constant My_Record := (I => 42);
   Z : constant Abstract_Record'Class := Y;
begin
   Print_I (Y);
   Hello (Y);
   Print_I (Z);
   Hello (Z);
end Foo;
