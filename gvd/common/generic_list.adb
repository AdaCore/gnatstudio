package body Generic_List is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure List_Split (L : in out List; L1, L2 : out List);
   --  Splits a list into two lists of equal size.
   --  The cost is O(n) where n = length(L).

   function List_Fuse
     (L1, L2   : in List;
      Inferior : Comparison)
     return List;
   --  Fuses two sorted lists into a sorted list.
   --  The cost is O(n) where n = (length(L1), length(L2)).

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (L    : in out List;
      Item : Data_Type)
   is
      L2 : List := L;
   begin
      L := new List_Node'
        (Element => new Data_Type'(Item),
         Next    => L2);
   end Prepend;

   ------------
   -- Append --
   ------------

   procedure Append
     (L    : in out List;
      Item : Data_Type)
   is
      Current_Node : List := L;
   begin
      if Current_Node = null then
         L := new List_Node'
           (Element => new Data_Type'(Item),
            Next    => null);
      else
         while Current_Node.Next /= null loop
            Current_Node := Current_Node.Next;
         end loop;
         Current_Node.Next := new List_Node'
           (Element => new Data_Type'(Item),
            Next    => null);
      end if;
   end Append;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List) return Boolean
   is
   begin
      if L = null then
         return True;
      else
         return False;
      end if;
   end Is_Empty;

   ----------
   -- Sort --
   ----------

   procedure Sort
     (L        : in out List;
      Inferior : Comparison)
   is
      L1, L2 : List;
   begin
      if L /= null
        and then L.Next /= null
      then
         List_Split (L, L1, L2);
         Sort (L1, Inferior);
         Sort (L2, Inferior);
         L := List_Fuse (L1, L2, Inferior);
      end if;
   end Sort;

   ----------------
   -- List_Split --
   ----------------

   procedure List_Split (L : in out List; L1, L2 : out List) is
      Append_To_L1 : Boolean := True;
      L1_First : List;
      L2_First : List;
      L1_Last  : List;
      L2_Last  : List;
   begin
      if L = null then
         L1 := null;
         L2 := null;
      elsif L.Next = null then
         L1 := L;
         L2 := null;
      else
         L1_First := L;
         L2_First := L.Next;
         L1_Last := L1_First;
         L2_Last := L2_First;
         L := L.Next.Next;

         while L /= null loop
            if Append_To_L1 then
               L1_Last.Next := L;
               L1_Last := L1_Last.Next;
               Append_To_L1 := False;
            else
               L2_Last.Next := L;
               L2_Last := L2_Last.Next;
               Append_To_L1 := True;
            end if;

            L := L.Next;
         end loop;

         L1_Last.Next := null;

         if L2_Last /= null then
            L2_Last.Next := null;
         end if;

         L1 := L1_First;
         L2 := L2_First;
      end if;
   end List_Split;

   ---------------
   -- List_Fuse --
   ---------------

   function List_Fuse
     (L1, L2   : in List;
      Inferior : Comparison)
     return List
   is
      List_First : List;
      List_Last : List;
      LL1 : List := L1;
      LL2 : List := L2;
   begin
      if LL1 = null then
         return LL2;
      elsif LL2 = null then
         return LL1;
      else
         if Inferior (LL1.Element.all, LL2.Element.all) then
            List_First := LL1;
            LL1 := LL1.Next;
         else
            List_First := LL2;
            LL2 := LL2.Next;
         end if;
      end if;

      List_Last := List_First;

      while LL1 /= null and then LL2 /= null loop
         if Inferior (LL1.Element.all, LL2.Element.all) then
            List_Last.Next := LL1;
            LL1 := LL1.Next;
         else
            List_Last.Next := LL2;
            LL2 := LL2.Next;
         end if;

         List_Last := List_Last.Next;
      end loop;

      if LL1 = null then
         List_Last.Next := LL2;
      else
         List_Last.Next := LL1;
      end if;

      return List_First;
   end List_Fuse;

   ----------
   -- Free --
   ----------

   procedure Free (L : in out List)
   is
      Previous : List;
   begin
      while L /= null loop
         Previous := L;
         Free (L.Element);
         L := L.Next;
         Free_Node (Previous);
      end loop;
      L := null;
   end Free;

   ----------
   -- Tail --
   ----------

   function Tail (L : List) return List
   is
   begin
      if L = null then
         raise List_Empty;
      else
         return L.Next;
      end if;
   end Tail;

   ----------
   -- Head --
   ----------

   function Head (L : List) return Data_Type
   is
   begin
      if L = null
        or else L.Element = null
      then
         raise List_Empty;
      else
         return L.Element.all;
      end if;
   end Head;

end Generic_List;
