package Tables is

   --  This generic package provides an unconstrained array which size
   --  slowly grows as needed. It is not possible to shrink the table
   --  size other than destroying it.

   --  !!! WARNING !!! This code has not been fully tested yet.

   generic

      type Index is range <>;
      --  The type used to index the array

      Low_Bound          : Index;
      --  The lowest bound of the array.

      Initial_High_Bound : Index;
      --  The highest bound the of the array when initialized.

      Table_Increment    : Natural;
      --  When the array size needs to be increased, it will be increased by
      --  blocks Table_Increment elements.

      type Data is private;
      --  The data type for the elements stored in this array.

      No_Data : Data;
      --  A value used to signal an element which value is not set.

   package Dynamic_Table is

      type Table is private;
      --  The unconstrained array.

      Usage_Error : exception;
      --  An exception raised when the table is incorrectly used.

      procedure Initialize (T : in out Table);
      --  Initializes the given Table. All elements in the new table are set
      --  to No_Data. Raise Usage_Error is already initialized.

      procedure Destroy (T : in out Table);
      --  Destroys the given table. It will need to be initialized before
      --  it can be used again. Does nothing if the table has already been
      --  destroyed.

      procedure Set (T : in out Table; Idx : Index; D : Data);
      --  Set the data at index I to D. Automatically grows the table if needed
      --  (all empty slots created are set to No_Data). Raise Usage_Error if
      --  the table is not initialized of if the index is smaller than
      --  Low_Bound.

      function Get (T : Table; Idx : Index) return Data;
      --  Return the data stored at index I. Raise Usage_Error if I is outside
      --  of the table bounds or if the table is not initialized.

      function High_Bound (T : Table) return Index;
      --  Return the highest index of the table. Raise Usage_Error if the
      --  table is not initialized.

   private

      type Data_Array is array (Index range <>) of Data;
      type Data_Array_Ptr is access Data_Array;

      type Table is record
         Ptr : Data_Array_Ptr;
      end record;

   end Dynamic_Table;

end Tables;
