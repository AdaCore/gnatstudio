with Ada.Text_IO;

procedure Main
is
begin
   loop
      delay (1.0);
      Ada.Text_IO.Put_Line ("Hello");
   end loop;
end Main;
