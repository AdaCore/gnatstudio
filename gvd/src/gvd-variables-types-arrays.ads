------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017, AdaCore                          --
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

--  Items used for array types
--  See the package Items for more information on all the private subprograms.

with Ada.Containers.Vectors;

package GVD.Variables.Types.Arrays is

   ----------------
   -- Array_Type --
   ----------------

   type GVD_Array_Type (<>) is new GVD_Generic_Type with private;
   type GVD_Array_Type_Access is access all GVD_Array_Type'Class;
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
     (Num_Dimensions : Positive) return GVD_Type_Holder;
   --  Create a new array type with a given number of dimensions.

   procedure Set_Dimensions
     (Self : not null access GVD_Array_Type;
      Dim  : Positive;
      Size : Dimension);
   --  Set the bounds of the Dim-nth dimensions of the array.

   function Num_Dimensions
     (Self : not null access GVD_Array_Type)
     return Positive;
   --  Return the number of dimensions in the array Item.

   function Get_Dimensions
     (Self : not null access GVD_Array_Type;
      Dim  : Positive)
      return Dimension;
   --  Return the bounds of the Dim-nth dimension in Item.

   procedure Set_Item_Type
     (Self     : not null access GVD_Array_Type;
      The_Type : GVD_Type_Holder);
   --  Set the type of items contained in Item.
   --  The_Type is not duplicated, we just keep an access to it.

   function Get_Item_Type
     (Self : not null access GVD_Array_Type)
      return GVD_Type_Holder;
   --  Return the Item_Type for the array.
   --  The returned structure should not be modified!
   --  ??? Could we use an access to constant ???

   procedure Set_Value
     (Self       : not null access GVD_Array_Type;
      Elem_Value : GVD_Type_Holder;
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
     (Self       : not null access GVD_Array_Type;
      Elem_Index : Long_Integer)
      return GVD_Type_Holder;
   --  Read a value in the array at a specific Index.
   --  If that index is covered by a Repeat_Type, a clone of the value is
   --  returned.
   --  Otherwise, the value is deleted from the array.
   --  Thus, the returned value can be simply modified, and then put back in
   --  the array.
   --  null is returned if there is no such item in the array.

   procedure Shrink_Values (Self : not null access GVD_Array_Type);
   --  Shrink the sparse table used to store the values of the array to its
   --  minimal size.
   --  This is never mandatory, but saves some memory in some cases.

private

   --  The structure for arrays is slightly complex, since we need to be able
   --  to handle multi-dimensional arrays as well as store the values in a
   --  sparse table.

   type Dimension_Array is array (Positive range <>) of Dimension;
   --  Represents one of the dimension of the array.

   type Array_Item is record
      Index : Long_Integer;
      Value : GVD_Type_Holder := Empty_GVD_Type_Holder;
   end record;
   package Array_Item_Vectors is new Ada.Containers.Vectors
     (Positive, Array_Item);
   --  One of the item of in the array.
   --  We need to store both its value and its index. The index is calculated
   --  as the index in a one-dimensional array that would store the same number
   --  of values as the array, starting from 0 as in C.

   type GVD_Array_Type (Num_Dimensions : Positive) is
     new GVD_Generic_Type with record
      Values      : Array_Item_Vectors.Vector;
      Item_Type   : GVD_Type_Holder := Empty_GVD_Type_Holder;
      Last_Value  : Natural := 0;

      Index_Width : Glib.Gint := 0;

      Type_Height : Glib.Gint := 0;
      --  Height of the first line used to display the type of the item.

      Dimensions  : Dimension_Array (1 .. Num_Dimensions);
   end record;
   --  Last_Value is the last value that is relevant in Values, or 0 if the
   --  array is empty.
   --  Note that once the last value has been inserted into the array (ie at
   --  the end of the parsing), Values will have the following properties:
   --    - items are sorted by their Array_Item.Index field
   --    - Values'Last = Last_Value, so that one can use 'Range for easier
   --      access

   overriding function Get_Type_Descr
     (Self : not null access GVD_Array_Type) return String;

   overriding procedure Clear (Self : not null access GVD_Array_Type);

   overriding procedure Clone
     (Self : not null access GVD_Array_Type;
      Item : not null GVD_Generic_Type_Access);

   overriding procedure Free (Self : not null access GVD_Array_Type);

   overriding function Replace
     (Self         : not null access GVD_Array_Type;
      Current      : GVD_Type_Holder'Class;
      Replace_With : GVD_Type_Holder'Class)
      return GVD_Type_Holder'Class;

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Array_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean;

   overriding function Start
     (Self : not null access GVD_Array_Type) return Generic_Iterator'Class;

end GVD.Variables.Types.Arrays;
