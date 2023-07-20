with Ada.Text_IO;

procedure Foo
is
	X : Integer := 1;
begin
	X := X + 1;
	declare
		Y : Integer;
	begin
		Ada.Text_IO.Put_Line (Integer'Image (X + Y));
	end;

	X := X + X;
	Ada.Text_IO.Put_Line (Integer'Image (X));
end Foo;
