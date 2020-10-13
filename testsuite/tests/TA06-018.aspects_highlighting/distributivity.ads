pragma Ada_2020;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

package Distributivity is

   function Big (X : Integer) return Big_Integer renames To_Big_Integer;

   procedure Lemma_Add_Mult (X, Y, Z : Integer) with
     Ghost,
     Post => (declare
                BX : constant Big_Integer := Big(X);
                BY : constant Big_Integer := Big(Y);
                BZ : constant Big_Integer := Big(Z);
              begin
                (BX + BY) * BZ = BX * BZ + BY * BZ);

end Distributivity;
