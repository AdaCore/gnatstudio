---------
-- Foo --
---------

procedure Foo is

   procedure Bar (Number : Integer; Res : out Integer);

   ---------
   -- Bar --
   ---------

   procedure Bar (Number : Integer; Res : out Integer)
   is
      Dummy : Integer;
   begin
      for J in 1 .. Number loop
         if J > 50 then
            raise Constraint_Error;
         end if;
      end loop;

      if Number < 50 then
         Res := Number + 1;
      elsif Number = 50 then
         raise Constraint_Error;
      else
         Dummy := Dummy + 1;
      end if;
   end Bar;

   Res : Integer;
begin
   Bar (49, Res);
   Bar (51, Res);
exception
   when Constraint_Error =>
      null;
end Foo;
