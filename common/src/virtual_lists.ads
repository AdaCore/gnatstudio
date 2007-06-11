-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package implements a "Virtual List". A virtual list is a
--  meta-container, containing serveal heterogeneous lists, but providing a way
--  to access to their elements in an homogeneous manner. Moreover, these
--  contained lists are not necessary actual lists, but can be viewed as
--  structures providing an iterator.

with Generic_List;

generic
   type Data_Type (<>) is private;
   --  Type of the data contained in the list

   with procedure Free (Data : in out Data_Type) is <>;
   --  Free the given data

   pragma Unreferenced (Free);
package Virtual_Lists is

   type Virtual_List is private;
   --  This list, contains heterogeneous sub-list and provides a way to access
   --  to their elements in an homgeneous way.

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
     (It : Virtual_List_Component_Iterator) return Data_Type is abstract;
   --  Return the element contained in this iterator.

   procedure Append
     (List : in out Virtual_List; Component : Virtual_List_Component'Class);
   --  Adds an actual sub-list to a virtual list.

   procedure Free (Component : in out Virtual_List_Component);
   --  Free a list component. The default implementation of this is empty.
   --  ??? This should be implemented as a null procedure in Ada 2005

   procedure Free (It : in out Virtual_List_Component_Iterator);
   --  Free a list iterator. The default implementation of this is empty.
   --  ??? This should be implemented as a null procedure in Ada 2005

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

   package Components_Pckg is new Generic_List (Virtual_List_Component_Access);

   type Virtual_List is record
      Contents : Components_Pckg.List;
   end record;

   Null_Virtual_List : constant Virtual_List :=
     (Contents => Components_Pckg.Null_List);

   type Virtual_List_Component_Iterator_Access is access all
     Virtual_List_Component_Iterator'Class;

   procedure Free (This : in out Virtual_List_Component_Iterator_Access);
   --  Free the data associated to a Virtual_List_Component_Iterator_Access.

   type Virtual_List_Iterator is record
      Current_Component : Components_Pckg.List_Node;
      Current_Iterator  : Virtual_List_Component_Iterator_Access;
   end record;

   Null_Virtual_List_Iterator : constant Virtual_List_Iterator :=
     (Current_Component => Components_Pckg.Null_Node,
      Current_Iterator  => null);

end Virtual_Lists;
