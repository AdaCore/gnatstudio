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

--  A generic simple linked list, with an efficient sorting function.

with Unchecked_Deallocation;

generic
   type Data_Type (<>) is private;

   with procedure Free (Data : in out Data_Type) is <>;
   --  Free any dynamic memory associated with Data.

package Generic_List is

   type List is private;
   type List_Node is private;

   Null_List : constant List;
   Null_Node : constant List_Node;

   List_Empty : exception;

   procedure Prepend
     (L    : in out List;
      Item : Data_Type);
   --  Add an item at the beginning of a list. The cost is O(1).

   procedure Prepend
     (L    : in out List;
      Node : List_Node;
      Item : Data_Type);
   --  Prepend an item before Node in list L. The cost is O(n).
   --  If Node is null, Item is appended at the end of the list.

   procedure Append
     (L    : in out List;
      Item : Data_Type);
   --  Add an item at the end of a list. The cost is O(1).

   procedure Append
     (L    : in out List;
      Node : List_Node;
      Item : Data_Type);
   --  Add an item after Node in list L. The cost is O(1).
   --  If Node is null, Item is inserted at the beginning of the list.

   function Is_Empty (L : List) return Boolean;
   --  True if L does not contain any element.

   function Length (L : List) return Natural;
   --  Return the number of elements in L. Cost is O(n).

   procedure Concat
     (L1 : in out List;
      L2 : List);
   --  Append L2 at the end of L1. Cost is O(1).
   --  Note that no deep copy of L2 is done, which means that L1 and L2
   --  will share the same nodes.

   procedure Insert
     (L1   : in out List;
      Node : List_Node;
      L2   : List);
   --  Insert L2 after Node in L1. Cost is O(1).
   --  Note that no deep copy of L2 is done, which means that L1 and L2
   --  will share the same nodes.
   --  If Node is Null_Node, L2 is inserted at the beginning of L1.

   procedure Free (L : in out List);
   --  Free memory associated to L.

   function First (L : List) return List_Node;
   --  Return the first node contained in L.

   function Last (L : List) return List_Node;
   --  Return the last node contained in L.

   function Prev (L : List; Node : List_Node) return List_Node;
   --  Return the node before Node in L. The cost is O(n).
   --  If Node is the first element, return null.
   --  If Node cannot be found in L, raise List_Empty.

   function Next (Node : List_Node) return List_Node;
   --  Return the node following Node. The cost is O(1).

   procedure Next (L : in out List);
   --  Return the list following the first element.
   --  Raise List_Empty if L is empty.
   --  The first element is removed from the list and freed.

   function Head (L : List) return Data_Type;
   --  Return the first data associated with L.
   --  Raise List_Empty if L is null.

   function Data (Node : List_Node) return Data_Type;
   --  Return the data associated with L.
   --  Raise List_Empty if L is null.

   procedure Set_Data
     (Node : List_Node;
      D    : Data_Type);
   --  Free the data associated with L and replace it by D.

private

   type List_Node_Record;
   type List_Node is access List_Node_Record;

   type List is record
      First : List_Node;
      Last  : List_Node;
   end record;

   Null_List : constant List := List' (null, null);
   Null_Node : constant List_Node := null;

   type Data_Access is access Data_Type;

   type List_Node_Record is record
      Element : Data_Access;
      Next    : List_Node;
   end record;

   procedure Free_Element is new
     Unchecked_Deallocation (Data_Type, Data_Access);

   procedure Free_Node is new
     Unchecked_Deallocation (List_Node_Record, List_Node);

   pragma Inline (First);
   pragma Inline (Prepend);
   pragma Inline (Is_Empty);
   pragma Inline (Data);
   pragma Inline (Next);
   pragma Inline (Head);

end Generic_List;
