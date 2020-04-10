with Ada.Unchecked_Deallocation;
package P is
   type T is tagged null record;
   type T_Access is access all T;
   procedure Free is new Ada.Unchecked_Deallocation
     (T, T_Access);
end P;
