-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

package body Generic_List is

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (L    : in out List;
      Item : Data_Type)
   is
      L2 : List_Node;
   begin
      if L.First = null then
         L.First := new List_Node' (Null_Node);
      end if;

      L2 := L.First.all;

      if L.Last = null then
         L.Last := new List_Node' (Null_Node);
      end if;

      L.First.all :=
        new List_Node_Record'
        (Element => new Data_Type' (Item),
         Next    => L2);

      if L2 = null then
         L.Last.all := L.First.all;
      end if;
   end Prepend;

   procedure Prepend
     (L    : in out List;
      Node : List_Node;
      Item : Data_Type)
   is
      Current : List_Node;
   begin
      if L.First = null then
         L.First := new List_Node' (Null_Node);
      end if;

      if L.Last = null then
         L.Last := new List_Node' (Null_Node);
      end if;

      Current :=  L.First.all;

      if Node = null then
         Append (L, Item);
      elsif Node = L.First.all then
         Prepend (L, Item);
      else
         while Current /= null and then Current.Next /= Node loop
            Current := Current.Next;
         end loop;

         if Current = null then
            raise List_Empty;
         end if;

         Current.Next := new List_Node_Record'
           (Element => new Data_Type' (Item),
            Next    => Current.Next);
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
         L.Last := new List_Node' (Null_Node);
      end if;

      if L.First = null then
         L.First := new List_Node' (Null_Node);
      end if;

      if L.Last.all = null then
         L.First.all := new List_Node_Record'
           (Element => new Data_Type' (Item),
            Next    => null);
         L.Last.all := L.First.all;

      else
         L.Last.all.Next := new List_Node_Record'
           (Element => new Data_Type' (Item),
            Next    => null);
         L.Last.all := L.Last.all.Next;
      end if;
   end Append;

   procedure Append
     (L    : in out List;
      Node : List_Node;
      Item : Data_Type) is
   begin
      if L.Last = null then
         L.Last := new List_Node' (Null_Node);
      end if;

      if L.First = null then
         L.First := new List_Node' (Null_Node);
      end if;

      if Node = null then
         Prepend (L, Item);
      elsif Node = L.Last.all then
         Append (L, Item);
      else
         Node.Next := new List_Node_Record'
           (Element => new Data_Type' (Item),
            Next    => Node.Next);
      end if;
   end Append;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List) return Boolean is
   begin
      return L.First = null
        or else L.First.all = null
        or else L.First.all.Element = null;
   end Is_Empty;

   ------------
   -- Length --
   ------------

   function Length (L : List) return Natural is
      L_Current : List_Node;
      Result    : Natural := 0;

   begin
      if L.First = null then
         return 0;
      end if;

      L_Current := L.First.all;

      while L_Current /= null loop
         Result := Result + 1;
         L_Current := L_Current.Next;
      end loop;

      return Result;
   end Length;

   ------------
   -- Concat --
   ------------

   procedure Concat
     (L1 : in out List;
      L2 : List)
   is
      F1 : List_Node_Access := L2.First;
      F2 : List_Node_Access := L2.Last;
   begin
      if L1.Last = null then
         L1.Last := new List_Node' (Null_Node);
      end if;

      if L1.First = null then
         L1.First := new List_Node' (Null_Node);
      end if;

      if Is_Empty (L1) then
         L1.First.all := L2.First.all;
         L1.Last.all  := L2.Last.all;
      else
         L1.Last.all.Next := L2.First.all;
         L1.Last.all      := L2.Last.all;
      end if;

      Free_Node_Access (F1);
      Free_Node_Access (F2);
   end Concat;

   ------------------
   -- Remove_Nodes --
   ------------------

   procedure Remove_Nodes
     (L1         : in out List;
      Start_Node : List_Node;
      End_Node   : List_Node := Null_Node)
   is
      Next_Node : List_Node;
      Current   : List_Node;
      Next_End  : List_Node := Null_Node;
   begin
      if End_Node /= Null_Node then
         Next_End := Next (End_Node);
      end if;

      if Start_Node = Null_Node then
         while not Is_Empty (L1) loop
            Current := First (L1);
            exit when Current = Next_End;
            Next (L1);
         end loop;

         return;
      end if;

      Next_Node := Next (Start_Node);

      while Next_Node /= Null_Node
        and then Next_Node /= End_Node
      loop
         Current := Next_Node;
         Next_Node := Next (Next_Node);

         if Current.Element /= null then
            Free (Current.Element.all);
            Free_Element (Current.Element);
         end if;
         Free_Node (Current);
      end loop;

      if Next_Node = Null_Node then
         L1.Last.all := Start_Node;
         Start_Node.Next := null;
      else
         Start_Node.Next := Next_End;
      end if;
   end Remove_Nodes;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (L1   : in out List;
      Node : List_Node;
      L2   : List)
   is
   begin
      if Is_Empty (L2) then
         return;

      elsif Is_Empty (L1) or else Node = L1.Last.all then
         L1.First.all := L2.First.all;
         L1.Last.all  := L2.Last.all;

      elsif Node = null then
         L2.Last.all.Next := L1.First.all;
         L1.First.all     := L2.First.all;

      else
         L2.Last.all.Next := Node.Next;
         Node.Next    := L2.First.all;
      end if;
   end Insert;

   ----------
   -- Free --
   ----------

   procedure Free (L : in out List) is
      Current : List_Node;
      Tmp     : List_Node;

   begin
      if L.First = null or else L.Last.all = null then
         return;
      end if;

      Current := L.First.all;
      L.First.all := null;
      L.Last.all  := null;

      while Current /= null loop
         Tmp := Current;
         Current := Current.Next;

         if Tmp.Element /= null then
            Free (Tmp.Element.all);
         end if;

         Free_Element (Tmp.Element);
         Free_Node (Tmp);
      end loop;

      Free_Node_Access (L.First);
      Free_Node_Access (L.Last);
   end Free;

   -----------
   -- First --
   -----------

   function First (L : List) return List_Node is
   begin
      if L.First = null then
         return Null_Node;
      else
         return L.First.all;
      end if;
   end First;

   ----------
   -- Last --
   ----------

   function Last (L : List) return List_Node is
   begin
      if L.Last = null then
         return Null_Node;
      else
         return L.Last.all;
      end if;
   end Last;

   ----------
   -- Prev --
   ----------

   function Prev (L : List; Node : List_Node) return List_Node is
      Current : List_Node;
   begin
      if L.First = null then
         raise List_Empty;
      end if;

      Current := L.First.all;

      if Current = Node then
         return null;
      end if;

      while Current /= null and then Current.Next /= Node loop
         Current := Current.Next;
      end loop;

      if Current = null then
         raise List_Empty;
      else
         return Current;
      end if;
   end Prev;

   ----------
   -- Next --
   ----------

   function Next (Node : List_Node) return List_Node is
   begin
      if Node = null then
         raise List_Empty;
      else
         return Node.Next;
      end if;
   end Next;

   procedure Next (L : in out List) is
      First : List_Node;
   begin
      if L.First = null or else L.First.all = null then
         raise List_Empty;
      else
         First   := L.First.all;
         L.First.all := L.First.all.Next;

         if L.First.all = null then
            L.Last.all := null;
         end if;

         if First.Element /= null then
            Free (First.Element.all);
         end if;

         Free_Element (First.Element);
         Free_Node (First);

         if L.First.all = null then
            Free_Node_Access (L.First);
         end if;

         if L.Last.all = null then
            Free_Node_Access (L.Last);
         end if;
      end if;
   end Next;

   ----------
   -- Head --
   ----------

   function Head (L : List) return Data_Type is
   begin
      if L.First.all = null or else L.First.all.Element = null then
         raise List_Empty;
      else
         return L.First.all.Element.all;
      end if;
   end Head;

   ----------
   -- Data --
   ----------

   function Data (Node : List_Node) return Data_Type is
   begin
      if Node = null or else Node.Element = null then
         raise List_Empty;
      else
         return Node.Element.all;
      end if;
   end Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Node : List_Node;
      D    : Data_Type) is
   begin
      if Node = null or else Node.Element = null then
         raise List_Empty;
      else
         Free (Node.Element.all);
         Free_Element (Node.Element);
         Node.Element := new Data_Type' (D);
      end if;
   end Set_Data;

end Generic_List;
