with Ada.Text_IO;
with Test;

procedure Main is
   procedure Print is
      A : constant Integer := Test.Add (3, 3);
   begin
      Ada.Text_IO.Put_Line (A'Img);
   end;
begin
   Ada.Text_IO.Put_Line ("Hello");
   Print;
   Ada.Text_IO.Put_Line ("Hello");
end Main;