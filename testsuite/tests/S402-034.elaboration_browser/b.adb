
with A;
pragma Elaborate_All (A);

package body B is

   procedure B is
   begin
      A.A;
   end B;

end B;
