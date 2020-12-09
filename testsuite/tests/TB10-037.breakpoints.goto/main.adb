procedure Main is

   task Count_Down;

   task body Count_Down is
      X : Integer := 10_000;
   begin
      while X > 0 loop
         X := X - 1;
         delay 1.0;
      end loop;
   end Count_Down;

   task type Worker is
      entry Start;
   end Worker;

   task body Worker is
   begin
      accept Start;
   end Worker;

   Workers : array (1 .. 4) of Worker;
begin
   null;
end Main;  --  Stop debuger here
