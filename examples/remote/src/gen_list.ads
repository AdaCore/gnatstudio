generic
   type Elmt is private;
package Gen_List is

   type Internal;
   type List is access Internal;

   type Internal is record
      E : Elmt;
      Next : List;
   end record;

   Nil : constant List := null;

   function Append (E1 : Elmt) return List;
   function Append (E1, E2 : Elmt) return List;
   function Append (E1 : Elmt; L1 : List) return List;
   function Append (L1 : List; E1 : Elmt) return List;
   function Append (L1, L2 : List) return List;

   function Element (L : List; Number : Positive := 1) return Elmt;
   function Tail    (L : List; Skip : Positive := 1) return List;
   function Length  (L : List) return Natural;

   Error_List : exception;

end Gen_List;
