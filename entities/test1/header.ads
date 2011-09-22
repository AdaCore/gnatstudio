with Header2;
package Header is
   type My_Integer is new Integer;
   type My_Integer2 is new My_Integer;

   type My_Array is array (My_Integer range <>) of My_Integer2;

   A : Integer := 1;

   type Interf2 is interface;
   type My_Tagged is new Header2.Interf and Interf2 with null record;

end Header;
