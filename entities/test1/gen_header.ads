generic
   type Formal is private;
package Gen_Header is
   subtype F is Formal;
   C : Integer;
   D : F;
   E : Formal;

   generic
      type Formal2 is private;
   package Nested_Gen is
      subtype F2 is Formal2;
   end Nested_Gen;
end Gen_Header;

