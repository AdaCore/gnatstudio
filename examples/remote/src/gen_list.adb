package body Gen_List is

   function Append (E1 : Elmt) return List is
   begin
      return new Internal'(E1, Nil);
   end Append;

   function Append (E1, E2 : Elmt) return List is
   begin
      return new Internal'(E1, new Internal'(E2, Nil));
   end Append;

   function Append (E1 : Elmt; L1 : List) return List is
   begin
      return new Internal'(E1, L1);
   end Append;

   function Append (L1 : List; E1 : Elmt) return List is
      L : List;
   begin
      if L1 = Nil then
         return new Internal'(E1, Nil);
      else
         L := L1;
         while L.Next /= Nil loop
            L := L.Next;
         end loop;
         L.Next := new Internal'(E1, Nil);
         return L1;
      end if;
   end Append;

   function Append (L1, L2 : List) return List is
      L : List;
   begin
      if L1 = Nil then
         return L2;
      else
         L := L1;
         while L.Next /= Nil loop
            L := L.Next;
         end loop;
         L.Next := L2;
         return L1;
      end if;
   end Append;


   function Element (L : List; Number : Positive := 1) return Elmt is
      L1 : List := L;
   begin
      for I in 2 .. Number loop
         if L1 = Nil then
            raise Error_List;
         end if;
         L1 := L1.Next;
      end loop;
      return L1.E;
   end Element;

   function Tail (L : List; Skip : Positive := 1) return List is
      L1 : List := L;
   begin
      for I in 1 .. Skip loop
         if L1 = Nil then
            raise Error_List;
         end if;
         L1 := L1.Next;
      end loop;
      return L1;
   end Tail;

   function Length (L : List) return Natural is
      C : Natural := 0;
      L1 : List := L;
   begin
      while L1 /= Nil loop
         C := C + 1;
         L1 := L1.Next;
      end loop;
      return C;
   end Length;

end Gen_List;
