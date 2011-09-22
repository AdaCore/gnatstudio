pragma Warnings (Off);
with Header;
with Gen_Header;

procedure Test1 is
   package G is new Gen_Header (Character);
   package G2 is new G.Nested_Gen (Boolean);
   B : Integer renames Header.A;
   E : Integer renames G.C;
   F : G.F;
   F2 : G2.F2;
begin
   B := 2;
   G.D := 'B';
   F := 'C';
end Test1;
