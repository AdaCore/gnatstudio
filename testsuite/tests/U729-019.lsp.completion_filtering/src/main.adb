with Ada.Text_IO;

procedure Main is
   procedure Do_Nothing (A : Integer) is null;
begin
   Ada.Text_IO.Put_Line ("Hello");

end Main;
