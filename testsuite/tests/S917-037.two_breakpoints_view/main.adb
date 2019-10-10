procedure Main is

   task type Worker is
      entry Start;
   end Worker;

   task body Worker is
   begin
      accept Start;
      accept Start;
   end Worker;

   Workers : array (1 .. 4) of Worker;

   task Count_Down is
      entry Start;
   end Count_Down;

   task body Count_Down is
   begin
      accept Start;
      for J of Workers loop
         J.Start;
         delay 1.0;
      end loop;
      accept Start;
   end Count_Down;

begin
   Count_Down.Start;  --  Stop debuger here
   Count_Down.Start;
end Main;
