with Ada.Text_IO;

procedure Main is
begin
   declare
      S : constant String := Ada.Text_IO.Get_Line;
   begin
      Ada.Text_IO.Put_Line (S);
   end;
end Main;
