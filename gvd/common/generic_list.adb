-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (L    : in out List;
      Item : Data_Type)
   is
      L2 : List_Node := L.First;
   begin
      L.First := new List_Node_Record'
        (Element => new Data_Type' (Item),
         Next    => L2);

      if L2 = null then
         L.Last := L.First;
      end if;
   end Prepend;

   procedure Prepend
     (L    : in out List;
      Node : List_Node;
      Item : Data_Type)
   is
      Current : List_Node := L.First;
   begin
      if Node = null then
         Append (L, Item);
      elsif Node = L.First then
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
         L.First := new List_Node_Record'
           (Element => new Data_Type' (Item),
            Next    => null);
         L.Last := L.First;

      else
         L.Last.Next := new List_Node_Record'
           (Element => new Data_Type' (Item),
            Next    => null);
         L.Last := L.Last.Next;
      end if;
   end Append;

   procedure Append
     (L    : in out List;
      Node : List_Node;
      Item : Data_Type) is
   begin
      if Node = null then
         Prepend (L, Item);
      elsif Node = L.Last then
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
      return L.First = null or else L.First.Element = null;
   end Is_Empty;

   ------------
   -- Length --
   ------------

   function Length (L : List) return Natural is
      L_Current : List_Node := L.First;
      Result    : Natural := 0;

   begin
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
      L2 : List) is
   begin
      if Is_Empty (L1) then
         L1.First := L2.First;
         L1.Last  := L2.Last;
      else
         L1.Last.Next := L2.First;
         L1.Last      := L2.Last;
      end if;
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
      if Start_Node = Null_Node then
         while First (L1) /= End_Node
           and then not Is_Empty (L1)
         loop
            Next (L1);
         end loop;

         return;
      end if;

      if End_Node /= Null_Node then
         Next_End := Next (End_Node);
      end if;

      Next_Node := Next (Start_Node);

      while Next_Node /= Null_Node
        and then Next_Node /= End_Node
      loop
         Current := Next_Node;
         Next_Node := Next (Next_Node);

         Free_Element (Current.Element);
         Free_Node (Current);
      end loop;

      if Next_Node = Null_Node then
         L1.Last := Start_Node;
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

      elsif Is_Empty (L1) or else Node = L1.Last then
         L1.First := L2.First;
         L1.Last  := L2.Last;

      elsif Node = null then
         L2.Last.Next := L1.First;
         L1.First     := L2.First;

      else
         L2.Last.Next := Node.Next;
         Node.Next    := L2.First;
      end if;
   end Insert;

   ----------
   -- Free --
   ----------

   procedure Free (L : in out List) is
      Current : List_Node;
      Tmp     : List_Node;

   begin
      Current := L.First;

      while Current /= null loop
         Tmp := Current;
         Current := Current.Next;

         if Tmp.Element /= null then
            Free (Tmp.Element.all);
         end if;

         Free_Element (Tmp.Element);
         Free_Node (Tmp);
      end loop;

      L.First := null;
      L.Last  := null;
   end Free;

   -----------
   -- First --
   -----------

   function First (L : List) return List_Node is
   begin
      return L.First;
   end First;

   ----------
   -- Last --
   ----------

   function Last (L : List) return List_Node is
   begin
      return L.Last;
   end Last;

   ----------
   -- Prev --
   ----------

   function Prev (L : List; Node : List_Node) return List_Node is
      Current : List_Node := L.First;
   begin
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
      First : List_Node := L.First;
   begin
      if L.First = null then
         raise List_Empty;
      else
         First   := L.First;
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
   end Next;

   ----------
   -- Head --
   ----------

   function Head (L : List) return Data_Type is
   begin
      if L.First = null or else L.First.Element = null then
         raise List_Empty;
      else
         return L.First.Element.all;
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
         Node.Element := new Data_Type' (D);
      end if;
   end Set_Data;

end Generic_List;
