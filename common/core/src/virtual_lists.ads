------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

--  This package implements a "Virtual List". A virtual list is a
--  meta-container, containing serveal heterogeneous lists, but providing a way
--  to access to their elements in an homogeneous manner. Moreover, these
--  contained lists are not necessary actual lists, but can be viewed as
--  structures providing an iterator.

private with Ada.Unchecked_Deallocation;

generic
   type Data_Type (<>) is private;
   --  Type of the data contained in the list
package Virtual_Lists is

   type Virtual_List is private;
   --  This list, contains heterogeneous sub-list and provides a way to access
   --  to their elements in an homogeneous way.

   List_Empty : exception;

   procedure Free (This : in out Virtual_List);
   --  Free the virtual list

   procedure Concat (This : in out Virtual_List; List : Virtual_List);
   --  Concat the elements of two virtual lists.  Note that no deep copy of
   --  List is done, which means that This and List will share the same nodes.

   Null_Virtual_List : constant Virtual_List;
   --  Default value for an empty list.

   --------------------
   -- List_Component --
   --------------------

   type Virtual_List_Component is abstract tagged null record;
   --  Root type of a sub-list of a virtual list.
   --  ??? This should be implemented as an interface in Ada 2005.

   type Virtual_List_Component_Iterator is abstract tagged null record;
   --  Root type of an interator of a list
   --  ??? This should be implemented as an interface in Ada 2005.

   function First
     (List : Virtual_List_Component)
      return Virtual_List_Component_Iterator'Class is abstract;
   --  Return an iterator pointing on the first element of a list

   function At_End
     (It : Virtual_List_Component_Iterator) return Boolean
      is abstract;
   --  Return true if the iterator is at the end of the list, which means after
   --  the last element.

   procedure Next (It : in out Virtual_List_Component_Iterator) is abstract;
   --  Moves the iterator to the next element of the list.

   function Get
     (It : in out Virtual_List_Component_Iterator)
      return Data_Type is abstract;
   --  Return the element contained in this iterator.

   procedure Append
     (List : in out Virtual_List; Component : Virtual_List_Component'Class);
   --  Adds an actual sub-list to a virtual list.

   procedure Free (Component : in out Virtual_List_Component) is null;
   --  Free a list component. The default implementation of this is empty.

   procedure Free (It : in out Virtual_List_Component_Iterator) is null;
   --  Free a list iterator. The default implementation of this is empty.

   ---------------------------
   -- Virtual_List_Iterator --
   ---------------------------

   type Virtual_List_Iterator is private;
   --  This iterator is capable of iterate over the elements contained in each
   --  sub-list of a virtual list.

   function First (List : Virtual_List) return Virtual_List_Iterator;
   --  Return the first element of a virtual list.

   function At_End (It : Virtual_List_Iterator) return Boolean;
   --  Return true if the iterator is after the last element of the list.

   procedure Next (It : in out Virtual_List_Iterator);
   --  Moves the iterator to the next element.

   function Get (It : Virtual_List_Iterator) return Data_Type;
   --  Return the element contained in the iterator.

   procedure Free (It : in out Virtual_List_Iterator);
   --  Free the data associated to this iterator.

   Null_Virtual_List_Iterator : constant Virtual_List_Iterator;
   --  Default value for an empty iterator.

private

   type Virtual_List_Component_Access is access all
     Virtual_List_Component'Class;

   procedure Free (This : in out Virtual_List_Component_Access);
   --  Free the data associated to a Virtual_List_Component_Access.

   type List_Node_Record;
   type List_Node is access List_Node_Record;
   type List_Node_Access is access List_Node;

   Null_Node : constant List_Node := null;

   type Virtual_List is record
      First : List_Node_Access;
      Last  : List_Node_Access;
   end record;

   function Is_Empty (This : Virtual_List) return Boolean;
   function First (List : Virtual_List) return List_Node;
   function Data (Node : List_Node) return Virtual_List_Component_Access;
   function Next (Node : List_Node) return List_Node;

   Null_Virtual_List : constant Virtual_List := (null, null);

   type Data_Access is access Virtual_List_Component_Access;

   type List_Node_Record is record
      Element : Data_Access;
      Next    : List_Node;
   end record;

   procedure Free_Element is new
     Ada.Unchecked_Deallocation (Virtual_List_Component_Access, Data_Access);

   procedure Free_Node is new
     Ada.Unchecked_Deallocation (List_Node_Record, List_Node);

   procedure Free_Node_Access is new
     Ada.Unchecked_Deallocation (List_Node, List_Node_Access);

   type Virtual_List_Component_Iterator_Access is access all
     Virtual_List_Component_Iterator'Class;

   procedure Free (This : in out Virtual_List_Component_Iterator_Access);
   --  Free the data associated to a Virtual_List_Component_Iterator_Access.

   type Virtual_List_Iterator is record
      Current_Component : List_Node;
      Current_Iterator  : Virtual_List_Component_Iterator_Access;
   end record;

   Null_Virtual_List_Iterator : constant Virtual_List_Iterator :=
     (Current_Component => Null_Node,
      Current_Iterator  => null);

end Virtual_Lists;
