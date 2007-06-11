-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2006                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

--  Hash table searching routines

--  This package contains two separate packages. The Simple_Htable package
--  provides a very simple abstraction that associates one element to one
--  key value and takes care of all allocation automatically using the heap.
--  The Static_Htable package provides a more complex interface that allows
--  complete control over allocation.

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
      --  An integer type indicating the number and range of hash headers.

      type Elmt_Ptr is private;
      --  The type used to reference an element (will usually be an access
      --  type, but could be some other form of type such as an integer type).

      Null_Ptr : Elmt_Ptr;
      --  The null value of the Elmt_Ptr type.

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

      type HTable is private;

      procedure Reset (Hash_Table : in out HTable);
      --  Resets the hash table by setting all its elements to Null_Ptr. The
      --  effect is to clear the hash table so that it can be reused. For the
      --  most common case where Elmt_Ptr is an access type, and Null_Ptr is
      --  null, this is only needed if the same table is reused in a new
      --  context. If Elmt_Ptr is other than an access type, or Null_Ptr is
      --  other than null, then Reset must be called before the first use
      --  of the hash table.

      procedure Set (Hash_Table : in out HTable; E : Elmt_Ptr);
      --  Insert the element pointer in the HTable

      function Get (Hash_Table : HTable; K : Key) return Elmt_Ptr;
      --  Returns the latest inserted element pointer with the given Key
      --  or null if none.

      procedure Remove (Hash_Table : in out HTable; K : Key);
      --  Removes the latest inserted element pointer associated with the
      --  given key if any, does nothing if none.

      type Iterator is private;

      procedure Get_First (Hash_Table : HTable; Iter : out Iterator);
      --  Returns Null_Ptr if the Htable is empty, otherwise returns one
      --  non specified element. There is no guarantee that 2 calls to this
      --  function will return the same element.

      procedure Get_Next (Hash_Table : HTable; Iter : in out Iterator);
      --  Returns a non-specified element that has not been returned by the
      --  same function since the last call to Get_First or Null_Ptr if
      --  there is no such element or Get_First has bever been called. If
      --  there is no call to 'Set' in between Get_Next calls, all the
      --  elements of the Htable will be traversed.

      function Get_Element (Iter : Iterator) return Elmt_Ptr;
      --  Return the current element

   private

      type HTable_Array is array (Header_Num) of Elmt_Ptr;

      type Iterator is record
         Iterator_Index   : Header_Num;
         Iterator_Ptr     : Elmt_Ptr;
         Iterator_Started : Boolean := False;
      end record;

      type HTable is record
         Table            : HTable_Array;
      end record;

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
      --  An integer type indicating the number and range of hash headers.

      type Element is private;
      --  The type of element to be stored

      with procedure Free_Element (X : in out Element);

      No_Element : Element;
      --  The object that is returned by Get when no element has been set for
      --  a given key

      type Key (<>) is private;
      with function Hash  (F : Key)      return Header_Num;
      with function Equal (F1, F2 : Key) return Boolean;

   package Simple_HTable is

      type HTable is private;

      procedure Set (Hash_Table : in out HTable; K : Key; E : Element);
      --  Associates an element with a given key. Overrides any previously
      --  associated element.

      procedure Reset (Hash_Table : in out HTable);
      --  Removes and frees all elements in the table

      function Get (Hash_Table : HTable; K : Key) return Element;
      --  Returns the Element associated with a key or No_Element if the
      --  given key has not associated element

      procedure Remove (Hash_Table : in out HTable; K : Key);
      --  Removes the latest inserted element pointer associated with the
      --  given key if any, does nothing if none.

      type Iterator is private;

      procedure Get_First (Hash_Table : HTable; Iter : out Iterator);
      --  Returns No_Element if the Htable is empty, otherwise returns one
      --  non specified element. There is no guarantee that 2 calls to this
      --  function will return the same element.

      procedure Get_Next (Hash_Table : HTable; Iter : in out Iterator);
      --  Returns a non-specified element that has not been returned by the
      --  same function since the last call to Get_First or No_Element if
      --  there is no such element. If there is no call to 'Set' in between
      --  Get_Next calls, all the elements of the Htable will be traversed.

      function Get_Element (Iter : Iterator) return Element;
      --  Return the element pointed to by Iter

      function Get_Key (Iter : Iterator) return Key;
      --  Return the key pointed to by Iter

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
        Elmt_Ptr      => Elmt_Ptr,
        Null_Ptr      => null,
        Set_Next      => Set_Next,
        Next          => Next,
        Key           => Key,
        Get_Key       => Get_Key,
        Hash          => Hash,
        Equal         => Equal,
        Free_Elmt_Ptr => Free);

      type Iterator is record
         Iter : Tab.Iterator;
      end record;

      type HTable is record
         Table : Tab.HTable;
      end record;

   end Simple_HTable;

   ----------
   -- Hash --
   ----------

   --  A generic hashing function working on String keys

   generic
      type Header_Num is range <>;
   function Hash (Key : String) return Header_Num;

   generic
      type Header_Num is range <>;
   function Case_Insensitive_Hash (Key : String) return Header_Num;

end HTables;
