-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

package body Generic_List is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure List_Split (L : in out List_Access; L1, L2 : out List_Access);
   --  Splits a list into two lists of equal size.
   --  The cost is O(n) where n = length(L).

   function List_Fuse
     (L1, L2   : in List_Access;
      Inferior : Comparison) return List_Access;
   --  Fuses two sorted lists into a sorted list.
   --  The cost is O(n) where n = (length(L1), length(L2)).

   procedure Sort
     (L        : in out List_Access;
      Inferior : Comparison);
   --  Sorts L using Inferior as comparison.

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (L    : in out List;
      Item : Data_Type)
   is
      L2 : List_Access := L.First;
   begin
      L.First := new List_Node'
        (Element => new Data_Type'(Item),
         Next    => L2);

      if L2 = null then
         L.Last := L.First;
      end if;
   end Prepend;

   ------------
   -- Append --
   ------------

   procedure Append
     (L    : in out List;
      Item : Data_Type) is
   begin
      if L.Last = null then
         L.First := new List_Node'
           (Element => new Data_Type'(Item),
            Next    => null);
         L.Last := L.First;
      else
         L.Last.Next := new List_Node'
           (Element => new Data_Type'(Item),
            Next    => null);
         L.Last := L.Last.Next;
      end if;
   end Append;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List) return Boolean is
   begin
      return L.First = null or else L.First.Element = null;
   end Is_Empty;

   ---------
   -- Rev --
   ---------

   procedure Rev (L : in out List) is
      L_Buffer   : List_Access;
      L_Previous : List_Access;
      L_Current  : List_Access;
      L_Next     : List_Access;

   begin
      if Is_Empty (L) then
         return;
      else
         L_Previous := null;
         L_Current  := L.First;
         L_Next     := L.First.Next;
         L.Last     := L.First;

         while L_Next /= null loop
            L_Buffer := L_Current.Next;
            L_Current.Next := L_Previous;
            L_Previous := L_Current;
            L_Current := L_Buffer;
            L_Next := L_Current.Next;
         end loop;

         L_Current.Next := L_Previous;
         L.First := L_Current;
      end if;
   end Rev;

   ------------
   -- Length --
   ------------

   function Length (L : List) return Natural is
      L_Current : List_Access := L.First;
      Result    : Natural := 0;

   begin
      while L_Current /= null loop
         Result := Result + 1;
         L_Current := L_Current.Next;
      end loop;

      return Result;
   end Length;

   ----------
   -- Sort --
   ----------

   procedure Sort
     (L        : in out List_Access;
      Inferior : Comparison)
   is
      L1, L2 : List_Access;
   begin
      if L /= null and then L.Next /= null then
         List_Split (L, L1, L2);
         Sort (L1, Inferior);
         Sort (L2, Inferior);
         L := List_Fuse (L1, L2, Inferior);
      end if;
   end Sort;

   ----------
   -- Sort --
   ----------

   procedure Sort
     (L        : in out List;
      Inferior : Comparison) is
   begin
      if L.First /= null
        and then L.First.Next /= null
      then
         Sort (L.First, Inferior);

         while L.Last.Next /= null loop
            L.Last := L.Last.Next;
         end loop;
      end if;
   end Sort;

   ----------------
   -- List_Split --
   ----------------

   procedure List_Split (L : in out List_Access; L1, L2 : out List_Access) is
      Append_To_L1 : Boolean := True;
      L1_First     : List_Access;
      L2_First     : List_Access;
      L1_Last      : List_Access;
      L2_Last      : List_Access;

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
     (L1, L2   : in List_Access;
      Inferior : Comparison) return List_Access
   is
      List_First : List_Access;
      List_Last  : List_Access;
      LL1        : List_Access := L1;
      LL2        : List_Access := L2;

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

   ------------
   -- Concat --
   ------------

   procedure Concat
     (L1 : in out List;
      L2 : List) is
   begin
      if Is_Empty (L1) then
         L1.First := L2.First;
         L1.Last  := L2.Last;
      else
         L1.Last.Next := L2.First;
         L1.Last := L2.Last;
      end if;
   end Concat;

   ----------
   -- Free --
   ----------

   procedure Free (L : in out List) is
      Previous : List_Access;
      Current  : List_Access;

   begin
      if L.First = null then
         return;
      end if;

      Current := L.First.Next;
      Previous := L.First;

      while Current /= null loop
         if Previous /= null then
            if Previous.Element /= null then
               Free (Previous.Element.all);
            end if;

            Free_Element (Previous.Element);
         end if;

         Free_Node (Previous);

         Previous := Current;
         Current := Current.Next;
      end loop;

      if Previous.Element /= null then
         Free (Previous.Element.all);
      end if;

      Free_Element (Previous.Element);
      Free_Node (Previous);

      L.First := null;
      L.Last  := null;
   end Free;

   ----------
   -- Next --
   ----------

   function Next (L : List) return List is
   begin
      if L.First = null then
         raise List_Empty;
      else
         if L.First.Next = null then
            return Null_List;
         else
            return List' (L.First.Next, L.Last);
         end if;
      end if;
   end Next;

   ----------
   -- Tail --
   ----------

   procedure Tail (L : in out List)
   is
      First : List_Access := L.First;
   begin
      if L.First = null then
         raise List_Empty;
      else
         First := L.First;
         L.First := L.First.Next;

         if L.First = null then
            L.Last := null;
         end if;

         if First.Element /= null then
            Free (First.Element.all);
         end if;

         Free_Element (First.Element);
         Free_Node (First);
      end if;
   end Tail;

   ----------
   -- Head --
   ----------

   function Head (L : List) return Data_Type is
   begin
      if L.First = null
        or else L.First.Element = null
      then
         raise List_Empty;
      else
         return L.First.Element.all;
      end if;
   end Head;

   ------------------
   -- Replace_Head --
   ------------------

   procedure Replace_Head
     (L : List;
      D : Data_Type) is
   begin
      if L.First = null
        or else L.First.Element = null
      then
         raise List_Empty;
      else
         Free (L.First.Element.all);
         L.First.Element := new Data_Type' (D);
      end if;
   end Replace_Head;

end Generic_List;
