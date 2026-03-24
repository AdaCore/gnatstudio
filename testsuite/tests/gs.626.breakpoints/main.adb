with Ada.Text_IO;

procedure Main is
   X : Integer := 0;

   procedure Hello is
   begin
      Ada.Text_IO.Put_Line ("Hello");
   end Hello;
begin
   X := X + 1;
   Hello;
end Main;
