package body a is
   procedure fuzzable (X : Integer) is
      Y : Integer;
   begin
      Y := X + 1;
      if Y > 2 then
         Y := 2;
      end if;
   end;


end a;
