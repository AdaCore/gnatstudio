with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Foo is
   U : Unbounded_String := To_Unbounded_String ("Hello World");
begin
   Ada.Text_IO.Put (U);
end Foo;
