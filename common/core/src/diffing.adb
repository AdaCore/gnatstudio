------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

package body Diffing is

   ----------
   -- Diff --
   ----------

   procedure Diff
     (Old_Container, New_Container : Container;
      Callback                     : Diff_Callback)
   is
      type Diff_Matrix is array
        (Integer range <>, Integer range <>) of Natural;

      type Diff_Matrix_Access is access all Diff_Matrix;

      procedure Free is new Ada.Unchecked_Deallocation
        (Diff_Matrix, Diff_Matrix_Access);

      type Iterator_Array is array (Integer range <>) of Iterator;

      type Iterator_Array_Access is access all Iterator_Array;

      procedure Free is new Ada.Unchecked_Deallocation
        (Iterator_Array, Iterator_Array_Access);

      type Diff_Node is record
         Old_Ind : Integer;
         New_Ind : Integer;
         State : Diff_State;
      end record;

      type Diff_Node_Array is array (Integer range <>) of Diff_Node;

      type Diff_Node_Array_Access is access all Diff_Node_Array;

      procedure Free is new Ada.Unchecked_Deallocation
        (Diff_Node_Array, Diff_Node_Array_Access);

      --  We don't know how complex is the actual length computation, so
      --  we store the value in order to avoid extra processing.

      Old_Container_Length : constant Integer := Length (Old_Container);
      New_Container_Length : constant Integer := Length (New_Container);

      Old_It, New_It : Iterator;
      Start_Old_It, Start_New_It : Iterator;
      End_Old_It, End_New_It : Iterator;

      --  These arrays are access types because we don't want to create them on
      --  the stack - might need too much memory in case of a task.

      Matrix         : Diff_Matrix_Access;
      Old_Iterators  : Iterator_Array_Access;
      New_Iterators  : Iterator_Array_Access;
      Diffs          : Diff_Node_Array_Access;
      Last_Diff_Info : Integer := 0;

      Common_Begin_Length, Common_End_Length : Integer := 0;

      Old_Ind, New_Ind : Integer;

   begin
      Old_It := First (Old_Container);
      New_It := First (New_Container);

      --  First, skip the beginning of the container which is equal. This will
      --  be the case in most cases, and we try to limit the size of the
      --  matrix with that.

      while not At_End (Old_It)
        and then not At_End (New_It)
        and then Get (Old_It) = Get (New_It)
      loop
         Callback (Get (Old_It), Get (New_It), Equal);

         Common_Begin_Length := Common_Begin_Length + 1;

         Old_It := Next (Old_It);
         New_It := Next (New_It);
      end loop;

      if At_End (Old_It) and then At_End (New_It) then
         return;
      end if;

      Start_New_It := New_It;
      Start_Old_It := Old_It;

      --  Then, compute the size of the back of the containers. We'll call
      --  the callback later, since we want to call it in the proper order.

      Old_It := Last (Old_Container);
      New_It := Last (New_Container);

      if not At_End (Start_Old_It)
        and then not At_End (Start_New_It)
      then
         while not At_End (Old_It)
           and then not At_End (New_It)
           and then Old_It /= Start_Old_It
           and then New_It /= Start_New_It
           and then Get (Old_It) = Get (New_It)
         loop
            Common_End_Length := Common_End_Length + 1;

            End_New_It := New_It;
            End_Old_It := Old_It;

            Old_It := Prev (Old_It);
            New_It := Prev (New_It);
         end loop;
      end if;

      --  Creates the matrix for the diff

      Matrix := new Diff_Matrix
        (0 .. Old_Container_Length - Common_Begin_Length - Common_End_Length,
         0 .. New_Container_Length - Common_Begin_Length - Common_End_Length);
      Old_Iterators := new Iterator_Array
        (1 .. Old_Container_Length - Common_Begin_Length - Common_End_Length);
      New_Iterators := new Iterator_Array
        (1 .. New_Container_Length - Common_Begin_Length - Common_End_Length);
      Diffs := new Diff_Node_Array
        (1 .. Old_Container_Length + New_Container_Length
           - (Common_Begin_Length + Common_End_Length) * 2);

      Old_It := Start_Old_It;
      New_It := Start_New_It;

      Matrix (0, 0) := 0;

      for J in 1 .. Matrix'Last (1) loop
         Matrix (J, 0) := 0;
         Old_Iterators (J) := Old_It;
         Old_It := Next (Old_It);
      end loop;

      for J in 1 .. Matrix'Last (2) loop
         Matrix (0, J) := 0;
         New_Iterators (J) := New_It;
         New_It := Next (New_It);
      end loop;

      for Old_Ind in 1 .. Matrix'Last (1) loop
         for New_Ind in 1 .. Matrix'Last (2) loop
            if Get (Old_Iterators (Old_Ind))
              = Get (New_Iterators (New_Ind))
            then
               Matrix (Old_Ind, New_Ind) :=
                 Matrix (Old_Ind - 1, New_Ind - 1) + 1;
            else
               if Matrix
                 (Old_Ind - 1, New_Ind) > Matrix (Old_Ind, New_Ind - 1)
               then
                  Matrix (Old_Ind, New_Ind) := Matrix (Old_Ind - 1, New_Ind);
               else
                  Matrix (Old_Ind, New_Ind) := Matrix (Old_Ind, New_Ind - 1);
               end if;
            end if;
         end loop;
      end loop;

      --  Computes the result

      Old_Ind := Matrix'Last (1);
      New_Ind := Matrix'Last (2);

      while New_Ind > 0 or else Old_Ind > 0 loop
         if Old_Ind > 0
           and then New_Ind > 0
           and then Get (Old_Iterators (Old_Ind))
           = Get (New_Iterators (New_Ind))
         then
            Last_Diff_Info := Last_Diff_Info + 1;
            Diffs (Last_Diff_Info) := (Old_Ind, New_Ind, Equal);
            Old_Ind := Old_Ind - 1;
            New_Ind := New_Ind - 1;
         elsif New_Ind > 0
           and then
             (Old_Ind = 0
              or else Matrix (Old_Ind, New_Ind - 1)
              >= Matrix (Old_Ind - 1, New_Ind))
         then
            Last_Diff_Info := Last_Diff_Info + 1;
            Diffs (Last_Diff_Info) := (Old_Ind, New_Ind, Added);
            New_Ind := New_Ind - 1;
         elsif Old_Ind > 0
           and then
             (New_Ind = 0
              or else Matrix (Old_Ind, New_Ind - 1)
              < Matrix (Old_Ind - 1, New_Ind))
         then
            Last_Diff_Info := Last_Diff_Info + 1;
            Diffs (Last_Diff_Info) := (Old_Ind, New_Ind, Removed);
            Old_Ind := Old_Ind - 1;
         end if;
      end loop;

      --  Calls the callback on the result

      for J in reverse 1 .. Last_Diff_Info loop
         case Diffs (J).State is
            when Equal =>
               Callback
                 (Get (Old_Iterators (Diffs (J).Old_Ind)),
                  Get (New_Iterators (Diffs (J).New_Ind)),
                  Equal);

            when Added =>
               Callback
                 (Null_Object,
                  Get (New_Iterators (Diffs (J).New_Ind)),
                  Added);

            when Removed =>
               Callback
                 (Get (Old_Iterators (Diffs (J).Old_Ind)),
                  Null_Object,
                  Removed);

         end case;
      end loop;

      --  Finally, call the callback on the remaining equals objects

      if Common_End_Length > 0 then
         Old_It := End_Old_It;
         New_It := End_New_It;

         while not At_End (Old_It) loop
            Callback (Get (Old_It), Get (New_It), Equal);

            Old_It := Next (Old_It);
            New_It := Next (New_It);
         end loop;
      end if;

      Free (Matrix);
      Free (Old_Iterators);
      Free (New_Iterators);
      Free (Diffs);
   end Diff;

end Diffing;
