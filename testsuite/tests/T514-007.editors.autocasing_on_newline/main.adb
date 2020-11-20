procedure Main is

   procedure Example (A, B : Boolean) is
   begin
      if A and then B then
         null;
      end if;
   end Example;

begin
   Example (True, False);
end Main;
