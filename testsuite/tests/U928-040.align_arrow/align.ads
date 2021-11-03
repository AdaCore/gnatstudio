with Ada.Containers.Hashed_Maps;
package Align is
   function Hash (N : Integer) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (N));

   package Values is new Ada.Containers.Hashed_Maps
     (Key_Type => Integer,
      Element_Type => Integer,
      Hash => Hash,
      Equivalent_Keys => "=");

end Align;
