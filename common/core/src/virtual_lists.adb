------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

package body Virtual_Lists is

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Virtual_List)
   is
      Current : List_Node;
      Tmp     : List_Node;
   begin
      if This.First = null
        or else This.Last = null
        or else This.Last.all = null
      then
         return;
      end if;

      Current := This.First.all;
      This.First.all := null;
      This.Last.all  := null;

      while Current /= null loop
         Tmp     := Current;
         Current := Current.Next;

         if Tmp.Element /= null then
            Free (Tmp.Element.all);
         end if;

         Free_Element (Tmp.Element);
         Free_Node (Tmp);
      end loop;

      Free_Node_Access (This.First);
      Free_Node_Access (This.Last);
   end Free;

   ------------
   -- Concat --
   ------------

   procedure Concat (This : in out Virtual_List; List : Virtual_List)
   is
      F1 : List_Node_Access := List.First;
      F2 : List_Node_Access := List.Last;
   begin
      if Is_Empty (List) then
         return;
      end if;

      if This.Last = null then
         This.Last := new List_Node'(Null_Node);
      end if;

      if This.First = null then
         This.First := new List_Node'(Null_Node);
      end if;

      if Is_Empty (This) then
         This.First.all := List.First.all;
         This.Last.all  := List.Last.all;
      else
         This.Last.all.Next := List.First.all;
         This.Last.all      := List.Last.all;
      end if;

      Free_Node_Access (F1);
      Free_Node_Access (F2);
   end Concat;

   ------------
   -- Append --
   ------------

   procedure Append
     (List : in out Virtual_List; Component : Virtual_List_Component'Class) is
   begin
      if List.Last = null then
         List.Last := new List_Node'(Null_Node);
      end if;

      if List.First = null then
         List.First := new List_Node'(Null_Node);
      end if;

      if List.Last.all = null then
         List.First.all := new List_Node_Record'
           (Element => new Virtual_List_Component_Access'
              (new Virtual_List_Component'Class'(Component)),
            Next    => null);
         List.Last.all := List.First.all;

      else
         List.Last.all.Next := new List_Node_Record'
           (Element => new Virtual_List_Component_Access'
              (new Virtual_List_Component'Class'(Component)),
            Next    => null);
         List.Last.all := List.Last.all.Next;
      end if;
   end Append;

   ----------
   -- Data --
   ----------

   function Data (Node : List_Node) return Virtual_List_Component_Access is
   begin
      if Node = null or else Node.Element = null then
         raise List_Empty;
      else
         return Node.Element.all;
      end if;
   end Data;

   -----------
   -- First --
   -----------

   function First (List : Virtual_List) return Virtual_List_Iterator is
      It : Virtual_List_Iterator;
   begin
      It.Current_Component := First (List);

      if It.Current_Component /= Null_Node then
         It.Current_Iterator := new Virtual_List_Component_Iterator'Class'
           (First (Data (It.Current_Component).all));
      end if;

      while It.Current_Component /= Null_Node
        and then At_End (It.Current_Iterator.all)
      loop
         Free (It.Current_Iterator);

         It.Current_Component := Next (It.Current_Component);

         if It.Current_Component /= Null_Node then
            It.Current_Iterator := new Virtual_List_Component_Iterator'Class'
              (First (Data (It.Current_Component).all));
         end if;
      end loop;

      return It;
   end First;

   -----------
   -- First --
   -----------

   function First (List : Virtual_List) return List_Node is
   begin
      if List.First = null then
         return Null_Node;
      else
         return List.First.all;
      end if;
   end First;

   ------------
   -- At_End --
   ------------

   function At_End (It : Virtual_List_Iterator) return Boolean is
   begin
      return It.Current_Component = Null_Node;
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Virtual_List_Iterator) is
   begin
      pragma Assert (not At_End (It));

      Next (It.Current_Iterator.all);

      while It.Current_Component /= Null_Node
        and then At_End (It.Current_Iterator.all)
      loop
         Free (It.Current_Iterator);

         It.Current_Component := Next (It.Current_Component);

         if It.Current_Component /= Null_Node then
            It.Current_Iterator := new Virtual_List_Component_Iterator'Class'
              (First (Data (It.Current_Component).all));
         end if;
      end loop;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (It : Virtual_List_Iterator) return Data_Type is
   begin
      return Get (It.Current_Iterator.all);
   end Get;

   ----------
   -- Free --
   ----------

   procedure Free (It : in out Virtual_List_Iterator) is
   begin
      Free (It.Current_Iterator);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Virtual_List_Component_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Virtual_List_Component'Class, Virtual_List_Component_Access);
   begin
      Free (This.all);
      Unchecked_Free (This);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Virtual_List_Component_Iterator_Access) is
      procedure Internal_Free is new Ada.Unchecked_Deallocation
        (Virtual_List_Component_Iterator'Class,
         Virtual_List_Component_Iterator_Access);
   begin
      if This /= null then
         Free (This.all);
         Internal_Free (This);
      end if;
   end Free;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Virtual_List) return Boolean is
   begin
      return This.First = null
        or else This.First.all = null
        or else This.First.all.Element = null;
   end Is_Empty;

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

end Virtual_Lists;
