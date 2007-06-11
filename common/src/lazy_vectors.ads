-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006                           --
--                             AdaCore                               --
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

generic
   type Data_Type is private;

   Null_Data_Type : Data_Type;
   --  Denotes a value where there's no item.
package Lazy_Vectors is

   type Lazy_Vector_Record is private;
   type Lazy_Vector is access all Lazy_Vector_Record;
   --  A lazy vector is a container storing where we try to do as less extend
   --  operations as possible. It's implemented as an array, reallocated
   --  when there's no more room. When data are removed, they create a hole
   --  in the vector, which is filled by the next element addition. A lazy
   --  vector is never shortened.

   Null_Lazy_Vector : constant Lazy_Vector;

   procedure Free (This : in out Lazy_Vector);
   --  Free the data associated to a lazy vector.

   procedure Insert (Vector : access Lazy_Vector_Record; Data : Data_Type);
   --  Insert a data in a lazy vector. There is no garantee on the actual
   --  position of the data regarding already existing one. The first hole
   --  available will be taken, if any. If no, then the vector size will be
   --  extended in order to create available space.

   type Iterator is private;
   --  This type is used to iter over the various items of a vector.

   Null_Iterator : constant Iterator;

   procedure Insert
     (Vector : access Lazy_Vector_Record;
      Data   : Data_Type;
      Pos    : out Iterator);
   --  Same as previous function insert, but gives the position where the data
   --  has been inserted.

   function First (Vector : Lazy_Vector) return Iterator;
   --  Return the first element of a lazy vector.

   procedure Next (It : in out Iterator);
   --  Goes to the next available item on the vector.

   function At_End (It : Iterator) return Boolean;
   --  Return true if the iterator is at the end of the vector.

   function Get (It : Iterator) return Data_Type;
   --  Return the data store at the position of the iterator.

   procedure Delete (It : Iterator);
   --  Deletes the data at the position of the iterator

private

   type Data_Array is array (Natural range <>) of Data_Type;

   type Data_Array_Access is access all Data_Array;

   procedure Free (This : in out Data_Array_Access);
   --  Free the data associated to a data array access

   type Lazy_Vector_Record is record
      Datas  : Data_Array_Access;
   end record;

   type Iterator is record
      Vector : access Lazy_Vector_Record;
      Index  : Natural;
   end record;

   function Is_Valid (It : Iterator) return Boolean;
   --  Return true if the iterator can be returned to the user, false otherwise

   Null_Lazy_Vector : constant Lazy_Vector := null;

   Null_Iterator : constant Iterator := (null, 0);

end Lazy_Vectors;
