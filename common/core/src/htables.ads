------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2018, AdaCore                     --
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

--  Hash table searching routines

--  This package contains two separate packages. The Simple_HTable package
--  provides a very simple abstraction that associates one element to one
--  key value and takes care of all allocations automatically using the heap.
--  The Static_HTable package provides a more complex interface that allows
--  complete control over allocation.

--  This package provides a facility similar to that of GNAT.HTable, except
--  that this package declares types that can be used to define dynamic
--  instances of hash tables, while instantiations of GNAT.HTable create a
--  single instance of the hash table.

--  Note that this interface should remain synchronized with those in
--  GNAT.HTable to keep as much coherency as possible between these two
--  related units.

package HTables is
pragma Preelaborate (HTables);

   -------------------
   -- Static_HTable --
   -------------------

   --  A low-level Hash-Table abstraction, not as easy to instantiate as
   --  Simple_HTable but designed to allow complete control over the
   --  allocation of necessary data structures. Particularly useful when
   --  dynamic allocation is not desired. The model is that each Element
   --  contains its own Key that can be retrieved by Get_Key. Furthermore,
   --  Element provides a link that can be used by the HTable for linking
   --  elements with same hash codes:

   --       Element

   --         +-------------------+
   --         |       Key         |
   --         +-------------------+
   --         :    other data     :
   --         +-------------------+
   --         |     Next Elmt     |
   --         +-------------------+

   generic
      type Header_Num is range <>;
      --  An integer type indicating the number and range of hash headers

      type Element (<>) is limited private;
      --  The type of element to be stored. This type is unused in this
      --  package, and kept for backward compatibility only.

      type Elmt_Ptr is private;
      --  The type used to reference an element (will usually be an access
      --  type, but could be some other form of type such as an integer type).

      Null_Ptr : Elmt_Ptr;
      --  The null value of the Elmt_Ptr type

      with procedure Set_Next (E : Elmt_Ptr; Next : Elmt_Ptr);
      with function  Next     (E : Elmt_Ptr) return Elmt_Ptr;
      --  The type must provide an internal link for the sake of the
      --  staticness of the HTable.

      type Key (<>) is limited private;
      with function Get_Key (E : Elmt_Ptr) return Key;
      with function Hash    (F : Key)      return Header_Num;
      with function Equal   (F1, F2 : Key) return Boolean;

      with procedure Free_Elmt_Ptr (E : in out Elmt_Ptr);
      --  Free the memory occupied by E

   package Static_HTable is

      type Instance is private;
      Nil : constant Instance;

      procedure Reset (T : in out Instance);
      --  Resets the hash table by releasing all memory associated with
      --  it. The hash table can safely be reused after this call. For the
      --  most common case where Elmt_Ptr is an access type, and Null_Ptr is
      --  null, this is only needed if the same table is reused in a new
      --  context. If Elmt_Ptr is other than an access type, or Null_Ptr is
      --  other than null, then Reset must be called before the first use of
      --  the hash table.

      procedure Set (T : in out Instance; E : Elmt_Ptr);
      --  Insert the element pointer in the HTable

      function Get (T : Instance; K : Key) return Elmt_Ptr;
      --  Returns the latest inserted element pointer with the given Key
      --  or null if none.

      procedure Remove (T : Instance; K : Key);
      --  Removes the latest inserted element pointer associated with the
      --  given key if any, does nothing if none.

      type Cursor is private;

      procedure Get_First (T : Instance; Iter : out Cursor);
      --  Points to the first element in the table.
      --  There is no guarantee that 2 calls to this function will point to the
      --  same element.

      procedure Get_Next (T : Instance; Iter : in out Cursor);
      --  Move to the next element in the table.
      --  If there is no call to Set or Remove between Get_Next calls, all the
      --  elements of the htable will be traversed

      procedure Remove_And_Get_Next
        (T : in out Instance; Iter : in out Cursor);
      --  Remove the current element from the table, and moves to the next
      --  element. This is the only safe way to alter the table while iterating

      function Get_Element (Iter : Cursor) return Elmt_Ptr;
      --  Return the current element

      function Get_First (T : Instance) return Elmt_Ptr;
      function Get_Next (T : Instance) return Elmt_Ptr;
      --  These two subprograms are provided for backward compatibility, and
      --  have the same semantic has the ones above. They use a hidden iterator
      --  associated with the table.

   private

      type Cursor is record
         Iterator_Index   : Header_Num;
         Iterator_Ptr     : Elmt_Ptr;
         Iterator_Started : Boolean := False;
      end record;

      type Instance_Data;
      type Instance is access all Instance_Data;
      Nil : constant Instance := null;

   end Static_HTable;

   -------------------
   -- Simple_HTable --
   -------------------

   --  A simple hash table abstraction, easy to instantiate, easy to use.
   --  The table associates one element to one key with the procedure Set.
   --  Get retrieves the Element stored for a given Key. The efficiency of
   --  retrieval is function of the size of the Table parameterized by
   --  Header_Num and the hashing function Hash.

   generic
      type Header_Num is range <>;
      --  An integer type indicating the number and range of hash headers

      type Element is private;
      --  The type of element to be stored

      No_Element : Element;
      --  The object that is returned by Get when no element has been set for
      --  a given key

      type Key (<>) is private;
      with function Hash  (F : Key)      return Header_Num;
      with function Equal (F1, F2 : Key) return Boolean;

      with procedure Free_Element (X : in out Element);
      --  Free memory occupied by the element (called when the element is
      --  removed from the table)

   package Simple_HTable is

      type Instance is private;
      Nil : constant Instance;

      procedure Set (T : in out Instance; K : Key; E : Element);
      --  Associates an element with a given key. Overrides any previously
      --  associated element.

      procedure Reset (T : in out Instance);
      --  Releases all memory associated with the table. The table can be
      --  reused after this call (it is automatically allocated on the first
      --  access to the table).

      function Get (T : Instance; K : Key) return Element;
      --  Returns the Element associated with a key or No_Element if the
      --  given key has not associated element

      procedure Remove (T : Instance; K : Key);
      --  Removes the latest inserted element pointer associated with the
      --  given key if any, does nothing if none.

      type Cursor is private;

      procedure Get_First (T : Instance; Iter : out Cursor);
      --  Returns No_Element if the Htable is empty, otherwise returns one
      --  non specified element. There is no guarantee that 2 calls to this
      --  function will return the same element.

      procedure Get_Next (T : Instance; Iter : in out Cursor);
      --  Returns a non-specified element that has not been returned by the
      --  same function since the last call to Get_First or No_Element if
      --  there is no such element. If there is no call to 'Set' in between
      --  Get_Next calls, all the elements of the Htable will be traversed.

      function Get_Element (Iter : Cursor) return Element;
      --  Return the element pointed to by Iter

      procedure Set_Element (Iter : Cursor; E : Element);
      --  Replacing the elment at the given position. This does not impact
      --  the key, so the iterator remains valid.

      procedure Remove_And_Get_Next
        (T : in out Instance; Iter : in out Cursor);
      --  Remove current element and move to next one

      function Get_Key (Iter : Cursor) return Key;
      --  Return the key pointed to by Iter

      function Get_First (T : Instance) return Element;
      function Get_Next (T : Instance) return Element;
      --  These two subprograms are provided for backward compatibility, and
      --  have the same semantic has the ones above. They use a hidden iterator
      --  associated with the table.

   private

      type Key_Access is access Key;

      type Element_Wrapper;
      type Elmt_Ptr is access all Element_Wrapper;
      type Element_Wrapper is record
         K    : Key_Access;
         E    : Element;
         Next : Elmt_Ptr;
      end record;

      procedure Free (X : in out Elmt_Ptr);

      procedure Set_Next (E : Elmt_Ptr; Next : Elmt_Ptr);
      function  Next     (E : Elmt_Ptr) return Elmt_Ptr;
      function  Get_Key  (E : Elmt_Ptr) return Key;

      package Tab is new Static_HTable (
        Header_Num    => Header_Num,
        Element       => Element,
        Elmt_Ptr      => Elmt_Ptr,
        Null_Ptr      => null,
        Set_Next      => Set_Next,
        Next          => Next,
        Key           => Key,
        Get_Key       => Get_Key,
        Hash          => Hash,
        Equal         => Equal,
        Free_Elmt_Ptr => Free);

      type Cursor is record
         Iter : Tab.Cursor;
      end record;

      type Instance is record
         Table : Tab.Instance;
      end record;

      Nil : constant Instance := (Table => Tab.Nil);
   end Simple_HTable;

end HTables;
