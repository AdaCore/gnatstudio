
package Types is
   type A_Type is null record;
   type B_Type is null record;

   type Rec is record
      A : A_Type;
      B : B_Type;
   end record;
end Types;
