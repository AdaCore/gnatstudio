with Ada.Unchecked_Deallocation;

package body Tables is

   -------------------
   -- Dynamic_Table --
   -------------------

   package body Dynamic_Table is

      procedure Free is new Ada.Unchecked_Deallocation
        (Data_Array, Data_Array_Ptr);

      procedure Grow (T : in out Table; Idx : Index);
      --  Expand the table size by chunks of Table_Increment so that the
      --  resulting table is large enough to store an element at index Idx.
      --  The newly created slots are set to No_Data.

      ----------
      -- Grow --
      ----------

      procedure Grow (T : in out Table; Idx : Index) is
         Number_Of_New_Chunks : constant Natural :=
           Natural (Idx - T.Ptr'Last - 1) / Table_Increment + 1;
         New_High_Bound       : constant Index :=
           T.Ptr'Last + Index (Number_Of_New_Chunks * Table_Increment);
         New_Array            : Data_Array_Ptr :=
           new Data_Array (Low_Bound .. New_High_Bound);
      begin
         New_Array (Low_Bound .. T.Ptr'Last) := T.Ptr.all;
         New_Array (T.Ptr'Last + 1 .. New_Array'Last) := (others => No_Data);
         Free (T.Ptr);
         T.Ptr := New_Array;
      end Grow;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (T : in out Table) is
      begin
         if T.Ptr /= null then
            raise Usage_Error;
         end if;
         T.Ptr := new Data_Array'(Low_Bound .. Initial_High_Bound => No_Data);
      end Initialize;

      -------------
      -- Destroy --
      -------------

      procedure Destroy (T : in out Table) is
      begin
         Free (T.Ptr);
      end Destroy;

      ---------
      -- Set --
      ---------

      procedure Set (T : in out Table; Idx : Index; D : Data) is
      begin
         if T.Ptr = null
           or else Idx < T.Ptr'First
         then
            raise Usage_Error;
         end if;

         if Idx > T.Ptr'Last then
            Grow (T, Idx);
         end if;

         T.Ptr (Idx) := D;
      end Set;

      ---------
      -- Get --
      ---------

      function Get (T : Table; Idx : Index) return Data is
      begin
         if T.Ptr = null
           or else Idx not in T.Ptr'Range
         then
            raise Usage_Error;
         end if;

         return T.Ptr (Idx);
      end Get;

      ----------------
      -- High_Bound --
      ----------------

      function High_Bound (T : Table) return Index is
      begin
         return T.Ptr'Last;
      end High_Bound;

   end Dynamic_Table;

end Tables;

