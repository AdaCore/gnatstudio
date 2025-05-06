with Ada.Text_IO;
with GNATCOLL.Symbols;

procedure Main is

   Foo : String := "Hello World!";

   Bar : array (1 .. 10) of Character := (others => 'a');

   I : Integer := 1;

begin
   Ada.Text_IO.Put_Line (Foo);
end Main;
