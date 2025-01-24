with Ada.Text_IO;

procedure Main
is
   type Kind is (This_Kind, That_Kind, Something_Kind);
   My_Var : Kind := This_Kind;

begin
   Ada.Text_IO.Put_Line (My_Var'Img);
end Main;
