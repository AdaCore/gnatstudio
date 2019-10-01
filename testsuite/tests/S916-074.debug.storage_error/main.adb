with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
begin
   for J in 1 .. 4 loop
      Put_Line ("Put_Line");
      delay 0.2;
   end loop;
   Put_Line ("finished!");
end Main;
