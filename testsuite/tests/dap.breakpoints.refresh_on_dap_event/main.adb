with Ada.Text_IO;

procedure Main is
   procedure Print is
   begin
      Ada.Text_IO.Put_Line ("Hello");
   end;
begin
   Ada.Text_IO.Put_Line ("Hello");
   Print;
   Ada.Text_IO.Put_Line ("Hello");
end Main;