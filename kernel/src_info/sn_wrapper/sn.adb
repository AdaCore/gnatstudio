with GNAT.OS_Lib; use GNAT.OS_Lib;

package body SN is

   ---------
   -- "<" --
   ---------

   function "<" (P1, P2 : Point) return Boolean is
   begin
      if P1.Line = P2.Line then
         return P1.Column < P2.Column;
      else
         return P1.Line < P2.Line;
      end if;
   end "<";

   ------------
   -- Length --
   ------------

   function Length (s : Segment) return Integer is
   begin
      return (s.Last - s.First + 1);
   end Length;

   ---------------
   -- To_String --
   ---------------

   function To_String (Buffer : String_Access; Seg : Segment) return String is
   begin
      return Buffer (Seg.First .. Seg.Last);
   end To_String;

end SN;
