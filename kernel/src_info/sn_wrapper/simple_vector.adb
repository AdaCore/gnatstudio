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
         while Node_Ptr.Next /= null loop
            Node_Ptr := Node_Ptr.Next;
         end loop;
         Node_Ptr.Next := new Node'(Item, null);
      end if;
   end Append;

   function Get_Element_At (Root : in Node_Access; Index : in Integer)
         return Element_Type is
      i        : Integer     := 1;
      Node_Ptr : Node_Access := Root;
   begin
      while Node_Ptr /= null loop
         if i = Index then
            return Node_Ptr.Data;
         end if;
         i := i + 1;
         Node_Ptr := Node_Ptr.Next;
      end loop;
      raise Element_Not_Found;
   end Get_Element_At;

   function Size (Root : in Node_Access) return Integer is
      c        : Integer     := 0;
      Node_Ptr : Node_Access := Root;
   begin
      while Node_Ptr /= null loop
         c := c + 1;
         Node_Ptr := Node_Ptr.Next;
      end loop;
      return c;
   end Size;

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
