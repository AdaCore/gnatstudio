-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2006                      --
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

--  This package is very similar to what is provided by GNAT.Dynamic_Table,
--  except it provides better control over reallocation of the array (we
--  do not necessary increment it proportionaly to its current size).
--
--  NOTE: Controlled types are NOT supported.

generic
   type Data is private;
   Table_Multiplier        : Positive := 2;
   Table_Minimum_Increment : Positive := 10;
   Table_Initial_Size      : Positive := 1;
   with function "=" (D1, D2 : Data) return Boolean is <>;

package Dynamic_Arrays is
   --  Data is the type of component stored in the array.
   --  Table_Multiplier and Table_Minimal_Increment are used to specify the
   --  policy used to grow the table: every time the current array is too
   --  small to accomodate a new element, its current size is multiplied
   --  by Table_Multiplier. We then make sure that at least
   --  Table_Minimal_Increment new elements have been added to the table.
   --  Using 1 for Table_Multiplier with result in a constant growth, less
   --  efficient than doubling the size every time, but this saves memory.
   --  Table_Initial_Size specifies the size that should be allocated initially
   --  for the table.

   type Index_Type is new Integer range 0 .. Integer'Last;

   First : constant Index_Type := Index_Type'Succ (Index_Type'First);
   --  The index of the first allocated element in Table. This leaves space to
   --  handle empty tables where Last < First.

   type Table_Private is private;
   type Table_Type is array (Index_Type range <>) of Data;
   subtype Big_Table_Type is Table_Type (First .. Index_Type'Last);
   --  We implement these arrays through flat arrays and thin pointers,
   --  which are more efficient.

   type Table_Ptr is access all Big_Table_Type;

   type Instance is record
      Table : Table_Ptr;
      P     : Table_Private;
   end record;
   Empty_Instance : constant Instance;

   function Last (T : Instance) return Index_Type;
   pragma Inline (Last);
   --  Return the index of the last allocated element in T

   function Length (T : Instance) return Index_Type;
   pragma Inline (Length);
   --  Return the number of allocated elements in T

   procedure Append (T : in out Instance; Item : Data);
   --  Add a new element at the end of the table which grows if needed

   procedure Remove (T : in out Instance; Item : Data);
   pragma Inline (Remove);
   --  Remove the first occurrence of Item in the table. The table is not
   --  shrunk, but keeps its current size for efficiency.

   procedure Remove (T : in out Instance; Index : Index_Type);
   procedure Remove (T : in out Instance; From, To : Index_Type);
   pragma Inline (Remove);
   --  Remove the element at a given position, or between two positions
   --  (included).

   function Find (T : Instance; Item : Data) return Index_Type;
   --  Return the position of the first occurrence of Item in the table,
   --  or (First - 1) if the table doesn't contain that element.

   procedure Free (T : in out Instance);
   --  Free the memory allocated for T

private

   type Table_Private is record
      Next_To_Last : Index_Type := First;
      --  Pointer to the element after the last logical element in the table

      Last_Allocated : Index_Type;
      --  Size of the currently allocated array
   end record;

   Empty_Instance : constant Instance := (null, (First, First));
end Dynamic_Arrays;
