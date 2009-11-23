-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

--  Helper package - simple list based vector

with Ada.Unchecked_Deallocation;

package body Simple_Vector is

   ------------
   -- Append --
   ------------

   procedure Append (Item : Element_Type; Root : in out Node_Access) is
   begin
      Root := new Node'(Item, Root);
   end Append;

   procedure Append (Root : in out Node_Access; Item : Element_Type) is
      Node_Ptr : Node_Access := Root;
   begin
      if Root = null then
         Root := new Node'(Item, null);
      else
         while Node_Ptr.Next /= null loop
            Node_Ptr := Node_Ptr.Next;
         end loop;

         Node_Ptr.Next := new Node'(Item, null);
      end if;
   end Append;

   --------------------
   -- Get_Element_At --
   --------------------

   function Get_Element_At
     (Root : Node_Access; Index : Integer) return Element_Type
   is
      J        : Integer     := 1;
      Node_Ptr : Node_Access := Root;

   begin
      while Node_Ptr /= null loop
         if J = Index then
            return Node_Ptr.Data;
         end if;

         J := J + 1;
         Node_Ptr := Node_Ptr.Next;
      end loop;

      raise Element_Not_Found;
   end Get_Element_At;

   ----------
   -- Size --
   ----------

   function Size (Root : Node_Access) return Integer is
      C        : Integer     := 0;
      Node_Ptr : Node_Access := Root;
   begin
      while Node_Ptr /= null loop
         C := C + 1;
         Node_Ptr := Node_Ptr.Next;
      end loop;

      return C;
   end Size;

   --------------------
   -- Release_Vector --
   --------------------

   procedure Release_Vector (Root : in out Node_Access) is
      Node_Ptr : Node_Access := Root;
      Temp_Ptr : Node_Access;

      procedure Delete is new Ada.Unchecked_Deallocation (Node, Node_Access);

   begin
      while Node_Ptr /= null loop
         Temp_Ptr := Node_Ptr.Next;
         --  Delete_Element (Node_Ptr.Data);
         Delete (Node_Ptr);
         Node_Ptr := Temp_Ptr;
      end loop;

      Root := null;
   end Release_Vector;

end Simple_Vector;
