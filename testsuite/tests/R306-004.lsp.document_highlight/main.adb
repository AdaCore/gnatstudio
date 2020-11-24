with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   procedure Entity_One (A : Integer) is null;

begin
   Entity_One (1);

   declare
      Entity_One : Integer := 0;
   begin
      Entity_One := Entity_One + 1;

      Ada.Text_IO.Put_Line ("Entity_One: " & Entity_One'Img);
   end ;
end Main;
