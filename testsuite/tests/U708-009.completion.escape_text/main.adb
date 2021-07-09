with Ada.Containers.Vectors;

procedure Main is

   subtype My_Int_2 is Integer;

   package MyVectors is new Ada.Containers.Vectors
     (My_Int_2, My_Int_2);

   Vect : MyVectors.Vector;
begin
   Vect
end Main;
