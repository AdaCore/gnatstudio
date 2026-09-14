with Ada.Text_IO;

procedure Main is
   X : Integer := 0;
begin
   X := X + 1;
   Ada.Text_IO.Put_Line ("Hello" & Integer'Image (X));
   X := X + 2;
   Ada.Text_IO.Put_Line ("Bye" & Integer'Image (X));
end Main;
