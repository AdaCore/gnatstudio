-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

--  Items used for array types
--  See the package Items for more information on all the private subprograms.

with Unchecked_Deallocation;

package Items.Arrays is

   ----------------
   -- Array_Type --
   ----------------

   type Array_Type (<>) is new Generic_Type with private;
   type Array_Type_Access is access all Array_Type'Class;
   --  An array type.
   --  Arrays can have multiple dimensions.
   --  Currently, all indexes must be integers, altough we should also handle
   --  any Simple_Type indexes.
   --  The value of an array is stored in a sparse table, ie all equal adjacent
   --  values are merged into a single element of type Repeat_Type, as returned
   --  for instance by gdb in <repeat .. times>.
   --  Note also that for unidimensional arrays of characters, their value is
   --  stored as a single item (a string).

   type Dimension is record
      First, Last : Long_Integer;
   end record;
   --  If Last < First, and either First = Long_Integer'Last or
   --  Last = Long_Integer'First, then the bounds are considered as dynamic,
   --  ie the real bounds are not known until we parse the value itself.

   function New_Array_Type
     (Num_Dimensions : Positive) return Generic_Type_Access;
   --  Create a new array type with a given number of dimensions.

   procedure Set_Dimensions
     (Item : in out Array_Type;
      Dim  : Positive;
      Size : Dimension);
   --  Set the bounds of the Dim-nth dimensions of the array.

   function Num_Dimensions
     (Item : Array_Type)
     return Positive;
   --  Return the number of dimensions in the array Item.

   function Get_Dimensions
     (Item : Array_Type;
      Dim  : Positive)
     return Dimension;
   --  Return the bounds of the Dim-nth dimension in Item.

   procedure Set_Item_Type
     (Item     : in out Array_Type;
      The_Type : access Generic_Type'Class);
   --  Set the type of items contained in Item.
   --  The_Type is not duplicated, we just keep an access to it.

   function Get_Item_Type (Item : Array_Type) return Generic_Type_Access;
   --  Return the Item_Type for the array.
   --  The returned structure should not be modified!
   --  ??? Could we use an access to constant ???

   procedure Set_Value (Item       : in out Array_Type;
                        Elem_Value : access Generic_Type'Class;
                        Elem_Index : Long_Integer;
                        Repeat_Num : Positive := 1);
   --  Set a value in the array.
   --
   --  The sparse table that stores the values of Item is dynamically resized
   --  if need be.
   --
   --  Elem_Index should be the index in a one-dimensional table that would
   --  contain the same number of elements as Item (as in C).
   --  The new value is repeat Repeat_Num times in adjacent cells in the array.
   --  Elem_Value is not duplicated!

   function Get_Value
     (Item       : Array_Type;
      Elem_Index : Long_Integer) return Generic_Type_Access;
   --  Read a value in the array at a specific Index.
   --  If that index is covered by a Repeat_Type, a clone of the value is
   --  returned.
   --  Otherwise, the value is deleted from the array.
   --  Thus, the returned value can be simply modified, and then put back in
   --  the array.
   --  null is returned if there is no such item in the array.

   procedure Shrink_Values (Item : in out Array_Type);
   --  Shrink the sparse table used to store the values of the array to its
   --  minimal size.
   --  This is never mandatory, but saves some memory in some cases.

   procedure Propagate_Width
     (Item  : in out Array_Type;
      Width : Glib.Gint);

private

   --  The structure for arrays is slightly complex, since we need to be able
   --  to handle multi-dimensional arrays as well as store the values in a
   --  sparse table.

   type Dimension_Array is array (Positive range <>) of Dimension;
   --  Represents one of the dimension of the array.

   type Array_Item is record
      Index : Long_Integer;
      Value : Generic_Type_Access := null;
   end record;
   type Array_Item_Array is array (Positive range <>) of Array_Item;
   --  One of the item of in the array.
   --  We need to store both its value and its index. The index is calculated
   --  as the index in a one-dimensional array that would store the same number
   --  of values as the array, starting from 0 as in C.

   type Array_Item_Array_Access is access Array_Item_Array;
   procedure Free is new Unchecked_Deallocation
     (Array_Item_Array, Array_Item_Array_Access);

   type Array_Type (Num_Dimensions : Positive) is new Generic_Type with record
      Dimensions  : Dimension_Array (1 .. Num_Dimensions);
      Values      : Array_Item_Array_Access := null;
      Item_Type   : Generic_Type_Access := null;
      Last_Value  : Natural := 0;

      Index_Width : Glib.Gint := 0;

      Type_Height : Glib.Gint := 0;
      --  Height of the first line used to display the type of the item.
   end record;
   --  Last_Value is the last value that is relevant in Values, or 0 if the
   --  array is empty.
   --  Note that once the last value has been inserted into the array (ie at
   --  the end of the parsing), Values will have the following properties:
   --    - items are sorted by their Array_Item.Index field
   --    - Values'Last = Last_Value, so that one can use 'Range for easier
   --      access

   procedure Print (Value : Array_Type; Indent : Natural := 0);
   procedure Free
     (Item : access Array_Type;
      Only_Value : Boolean := False);
   procedure Clone_Dispatching
     (Item  : Array_Type;
      Clone : out Generic_Type_Access);
   procedure Paint
     (Item    : in out Array_Type;
      Context : Drawing_Context;
      X, Y    : Glib.Gint := 0);
   procedure Size_Request
     (Item           : in out Array_Type;
      Context        : Drawing_Context;
      Hide_Big_Items : Boolean := False);
   function Get_Component_Name
     (Item : access Array_Type;
      Lang : access Language.Language_Root'Class;
      Name : String;
      X, Y : Glib.Gint) return String;
   function Get_Component
     (Item : access Array_Type;
      X, Y : Glib.Gint) return Generic_Type_Access;
   function Replace
     (Parent       : access Array_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class) return Generic_Type_Access;

   type Array_Iterator is new Generic_Iterator with record
      Item  : Array_Type_Access;
      Child : Natural;
   end record;
   function Start (Item : access Array_Type) return Generic_Iterator'Class;
   procedure Next (Iter : in out Array_Iterator);
   function At_End (Iter : Array_Iterator) return Boolean;
   function Data (Iter : Array_Iterator) return Generic_Type_Access;

end Items.Arrays;
