with Ada.Text_IO;

package body Pack is

   ---------
   -- Obs --
   ---------

   procedure Obs (I, J, K, L : Integer) is
      pragma Unreferenced (L);
   begin
      Real (I, J, K);
   end Obs;

   ----------
   -- Real --
   ----------

   procedure Real (I, J, K : Integer) is
   begin
      if I = J and then K = I + J then
         Ada.Text_IO.Put_Line ("Hello World");
      end if;
   end Real;

end Pack;
