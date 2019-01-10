with Hidden;

package body Pack is

   procedure Foo (I : in out Integer) is
   begin
      if I < 10 then
         I := I + 1;
         Foo (I);
      elsif I < 20 then
        Hidden.Secret (I);
      end if;
   end Foo;

   procedure Bar (I : in out Integer) is
   begin
      Foo (I);
   end Bar;
end Pack;
