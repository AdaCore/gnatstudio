with Ada.Command_Line;
with Ada.Text_IO;

procedure Main is
begin
   if Ada.Command_Line.Argument_Count /= 0 then
      Ada.Text_IO.Put_Line ("Failed: " & Ada.Command_Line.Argument (1));
   else
      Ada.Text_IO.Put_Line ("Success");
   end if;
end Main;
