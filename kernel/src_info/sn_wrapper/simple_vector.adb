-------------------------------------------------------------------------
--  $Id: simple_vector.adb,v 1.2 2002/01/30 11:15:18 taras Exp
--  $Helper package - simple list based vector
-------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Simple_Vector is
   procedure Append (Item : in Element_Type; Root : in out Node_Access) is
   begin
      Root := new Node'(Item, Root);
   end Append;

   procedure Append (Root : in out Node_Access; Item : in Element_Type) is
      Node_Ptr : Node_Access := Root;
   begin
      if Root = null then
         Root := new Node'(Item, null);
      else
         while Node_Ptr.next /= null loop
            Node_Ptr := Node_Ptr.next;
         end loop;
         Node_Ptr.next := new Node'(Item, null);
      end if;
   end Append;

   function Get_Element_At (Root : in Node_Access; Index : in Integer)
         return Element_Type is
      i        : Integer     := 1;
      Node_Ptr : Node_Access := Root;
   begin
      while Node_Ptr /= null loop
         if i = Index then
            return Node_Ptr.data;
         end if;
         i := i + 1;
         Node_Ptr := Node_Ptr.next;
      end loop;
      raise Element_Not_Found;
   end Get_Element_At;

   function Size (Root : in Node_Access) return Integer is
      c        : Integer     := 0;
      Node_Ptr : Node_Access := Root;
   begin
      while Node_Ptr /= null loop
         c := c + 1;
         Node_Ptr := Node_Ptr.next;
      end loop;
      return c;
   end Size;

   procedure Release_Vector (Root : in out Node_Access) is
      Node_Ptr : Node_Access := Root;
      Temp_Ptr : Node_Access;
      procedure Delete is new Ada.Unchecked_Deallocation (Node, Node_Access);
   begin
      while Node_Ptr /= null loop
         Temp_Ptr := Node_Ptr.next;
         --  Delete_Element (Node_Ptr.data);
         Delete (Node_Ptr);
         Node_Ptr := Temp_Ptr;
      end loop;
      Root := null;
   end Release_Vector;

end Simple_Vector;
