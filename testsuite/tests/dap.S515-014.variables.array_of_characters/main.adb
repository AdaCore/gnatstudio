with Ada.Text_IO;
with GNATCOLL.Symbols;

procedure Main is

   Foo : String := "Hello World!";

   Bar : array (1 .. 10) of Character := (others => 'a');

   Sym : GNATCOLL.Symbols.Symbol := GNATCOLL.Symbols.Empty_String;

   type Normalized_Symbol is new GNATCOLL.Symbols.Symbol;

   Norm : Normalized_Symbol := Normalized_Symbol (Sym);

begin
   Ada.Text_IO.Put_Line (Foo);
end Main;
