procedure Foo is
   --  A
   Res : Integer := 0;
   S1 : constant String := "Hello";
   S2 : constant String := "World";
begin
   --  B
   for I in 1 .. 10 loop
      --  C
      if I mod 2 = 0 then
         --  D
         Res := Res + I;
      end if;
   end loop;
   --  E
end Foo;
--  F
