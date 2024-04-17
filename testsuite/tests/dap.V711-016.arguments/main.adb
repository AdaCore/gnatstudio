with Ada.Text_IO;

procedure Main is
   procedure Print (I : Integer; B : Boolean) is
   begin
      Ada.Text_IO.Put_Line ("Hello" & B'Img & I'Img);
   end;

   I : constant Integer := 0;
   B : constant Boolean := False;
   Arguments : constant Integer := 5;
begin
   Print (I, B);
end Main;
