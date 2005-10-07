-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2005                      --
--                              AdaCore                              --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package implements red-black trees, which can be used to provide
--  priority queues, or associative containers (sets, htables,...) as is done
--  in the C++ stl's GNU implementation.
--  All operations on this tree are at worst O(log (n)).
--
--  The algorithms are adapted from "Introduction to algorithms" by
--  Cormen, Leiserson, Rivest  -- McGraw Hill (1990)

generic
   type Item is private;
   --  Only the ":=" operator is required for Item, not "=".
   --  Making it a limited private type would just require an extra formal for
   --  copying Items

   with procedure Free (It : in out Item);
   with function "<" (It1, It2 : Item) return Boolean is <>;
package Rbtrees is

   type Rbtree is private;
   type Rbtree_Iterator is private;
   Null_Iterator : constant Rbtree_Iterator;

   procedure Insert (Tree : in out Rbtree; Value : Item);
   --  Insert a new item in the tree

   procedure Remove (Tree : in out Rbtree; Iter : Rbtree_Iterator);
   --  Remove the item pointed to by Iter.
   --  Iter is invalid on exit, and should not be used

   function Minimum (Tree : Rbtree) return Rbtree_Iterator;
   --  Return the minimum value in the tree

   function Maximum (Tree : Rbtree) return Rbtree_Iterator;
   --  Return the maximum value in the tree

   function Find (Tree : Rbtree; Value : Item) return Rbtree_Iterator;
   --  Return the first instance of value in the tree

   function Next (Iter : Rbtree_Iterator) return Rbtree_Iterator;
   --  Return the next element in the tree, or Null_Iterator if there are no
   --  more items in the tree.

   function Previous (Iter : Rbtree_Iterator) return Rbtree_Iterator;
   --  Return the previous element in the tree.

   function Get (Iter : Rbtree_Iterator) return Item;
   --  Return the value pointed to by Iter. Constraint_Error is raised if Iter
   --  is Null_Iterator.

   function Index (Iter : Rbtree_Iterator) return Natural;
   --  Return the index in the tree of the iterator

   function Length (Tree : Rbtree) return Natural;
   --  Return the number of items in the tree

   function Get_Nth (Tree : Rbtree; Nth : Positive) return Rbtree_Iterator;
   --  Return the nth element of the tree.

   function Is_Empty (Tree : Rbtree) return Boolean;
   --  Return true if Tree is empty

   procedure Clear (Tree : in out Rbtree);
   --  Empty the tree. This is more efficient than removing the minimum value
   --  over and over again

private
   type Node;
   type Rbtree_Iterator is access all Node;
   Null_Iterator : constant Rbtree_Iterator := null;

   type Rbtree is record
      Root : Rbtree_Iterator;
   end record;
end Rbtrees;
