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

--  A generic simple linked list, with an efficient sorting function.

with Unchecked_Deallocation;

generic
   type Data_Type (<>) is private;

package Generic_List is

   type List is private;

   Null_List : constant List;

   type Comparison is access
     function (Arg1, Arg2 : Data_Type) return Boolean;

   procedure Prepend
     (L    : in out List;
      Item : Data_Type);
   --  Add an item at the beginning of a list. The cost is O(1).

   procedure Append
     (L    : in out List;
      Item : Data_Type);
   --  Add an item at the end of a list. The cost is O(n) !

   function Is_Empty (L : List) return Boolean;
   --  True if L does not contain any element.

   procedure Rev (L : in out List);
   --  Reverse the order of elements in the list. Cost is O(n).

   function Length (L : List) return Natural;
   --  Return the number of elements in L. Cost is O(n).

   procedure Sort
     (L        : in out List;
      Inferior : Comparison);
   --  Sorts a List. The cost is O(n*log(n)).
   --  Inferior is a Comparison that returns True if Arg1 is strictly
   --  inferior to Arg2.

   procedure Concat
     (L1 : in out List;
      L2 : List);
   --  Append L2 at the end of L1.

   procedure Free (L : in out List);
   --  Free memory associated to L.

   function Next (L : List) return List;
   --  Return the list following the first element.
   --  Raise List_Empty if L is empty.
   --  Attention : the first element is not freed !!
   --  If you want to move through a list and free it on
   --  the fly, use Tail instead.

   procedure Tail (L : in out List);
   --  Return the list following the first element.
   --  Raise List_Empty if L is empty.
   --  Attention : the first element is freed !!

   function Head (L : List) return Data_Type;
   --  Return the first element contained in the list.
   --  Raise List_Empty if L is empty;

   List_Empty : exception;

private

   type List_Node;
   type List is access List_Node;

   Null_List : constant List := null;

   type Data_Access is access Data_Type;

   type List_Node is record
      Element : Data_Access;
      Next    : List;
   end record;

   procedure Free_Element is new
     Unchecked_Deallocation (Data_Type, Data_Access);
   procedure Free_Node is new Unchecked_Deallocation (List_Node, List);

   pragma Inline (Prepend);
   pragma Inline (Head);
   pragma Inline (Tail);

end Generic_List;
