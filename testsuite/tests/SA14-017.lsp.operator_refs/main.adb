with Ada.Text_IO;

procedure Main is
   type My_Int is new Integer;

   function "/" (Left, Right : My_Int) return My_Int
   is
      (My_Int (Integer (Left) / Integer (Right)));

   A, B : My_Int := 3;
   Result : My_Int := A / B;
begin
   Ada.Text_IO.Put_Line(Result'Img);
end Main;
