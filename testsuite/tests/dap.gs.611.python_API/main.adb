with Ada.Text_IO;

procedure Main is
   procedure Print is
      A : Integer := 3;
   begin
      Ada.Text_IO.Put_Line (A'Img);
   end;
begin
   Ada.Text_IO.Put_Line ("Hello");
   Print;
   Ada.Text_IO.Put_Line ("Hello");
end Main;
