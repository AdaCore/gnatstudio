with hello_h;
with Ada.Text_IO;

procedure Main is
   package H renames Hello_H.Class_Hello;
   Zero : Integer := 0;
   Hi : aliased H.Hello := H.New_Hello;
   Three : Integer := 3;
begin
   Ada.Text_IO.Put_Line ("Zero = " & Zero'Img);
   H.DoSomething (Hi'Access, 1, 2);
   Ada.Text_IO.Put_Line ("Three = " & Three'Img);
end Main;
