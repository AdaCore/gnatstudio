
with Ada.Text_IO;
with Decl.A;
with Decl.B;

procedure Hello
is
   T : Decl.Kind := Decl.A.Do_Get;
begin
   Ada.Text_IO.Put_Line (T'Img);
   Ada.Text_IO.Put_Line (Decl.B.Do_Get_B'Img);
end;
