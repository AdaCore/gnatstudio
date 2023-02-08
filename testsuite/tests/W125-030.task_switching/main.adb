
with Ada.Text_IO;  use Ada.Text_IO;

procedure Main is
   task type My_Task is
      entry Start;
   end My_Task;

   task body My_Task is
   begin
      accept Start;
      loop
         Put_Line ("T");
      end loop;
   end My_Task;

   M1, M2, M3 : My_Task;
begin
   M1.Start;
   M2.Start;
   M3.Start;
   Put_Line (".");
end Main;
