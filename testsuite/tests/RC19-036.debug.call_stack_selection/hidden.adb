with Pack;

package body Hidden is

   procedure Secret (I : in out Integer) is
   begin
      I := I + 2;
      Pack.Bar (I);
   end Secret;

end Hidden;
