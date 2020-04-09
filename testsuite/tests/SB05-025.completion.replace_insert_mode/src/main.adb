with Ada.Text_IO;

procedure Main is
begin
   Ada.Text_IO.Put_Line ("Hello World!");

   declare
      Var_1 : Integer := 0;
      Var_2 : Integer := 1;
   begin
      Var_1 := Var_1 + Var_2;
   end;

end Main;
