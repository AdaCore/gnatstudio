-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
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

with GNAT.Table;
with GNAT.HTable;
--  with GNAT.Heap_Sort_A;

package body Hashtable_With_Iterator is
   -----------------------------------------------------------------------
   --  Defining Hashtable
   type Element_With_Number is record
      Number : Integer;
      Element : Element_Type;
   end record;
   No_Element_With_Number : Element_With_Number := (0, No_Element);

   package My_Hash is new GNAT.HTable.Simple_HTable
     (Key_Hash_Range,
      Element_With_Number,
      No_Element_With_Number,
      Key_Type,
      Key_Hash,
      "=");

   -----------------------------------------------------------------------
   --  Defining array of keys
   package Key_Array is new GNAT.Table
     (Key_Type, Integer, 0, 1000, 100);
   -----------------------------------------------------------------------

   -----------------------------------------------------------------------

   Current_Cursor : Integer;

   procedure Init is
   begin
      Key_Array.Init;
      Key_Array.Set_Last (1);
      Current_Cursor := 1;
   end Init;

   procedure Set (Key : Key_Type; Value : Element_Type) is
      el_with_num, exists_el : Element_With_Number;
   begin
      el_with_num.Element := Value;
      exists_el := My_Hash.Get (Key);
      if exists_el.Element = No_Element then
         Key_Array.Table (Key_Array.Last) := Key;
         el_with_num.Number := Key_Array.Last;
         Key_Array.Increment_Last;
      else
         el_with_num.Number := exists_el.Number;
      end if;
      My_Hash.Set (Key, el_with_num);
   end Set;

   function Get (Key : Key_Type) return Element_Type is
   begin
      return My_Hash.Get (Key).Element;
   end Get;

   procedure Move (From : Natural; To : Natural);
   function Lt (Op1, Op2 : Natural) return Boolean;
   --  These routines are needed by Sort procedure

   procedure Sort is
      N : Integer := Key_Array.Last - 1;
   begin
      for j in reverse 1 .. (N - 1) loop
         for i in 1 .. j loop
            if Lt (i + 1, i) then
               Move (i + 1, i);
            end if;
         end loop;
      end loop;
   end Sort;
   --  Bubble Sort

   procedure Reset_Cursor is
   begin
      Current_Cursor := 1;
   end Reset_Cursor;

   function Get_Next return Key_Value_Pair is
      tmp_pair : Key_Value_Pair;
      tmp_el_with_num : Element_With_Number;
   begin
      if Key_Array.Last > Current_Cursor then
         tmp_pair.Key := Key_Array.Table (Current_Cursor);
         tmp_el_with_num := My_Hash.Get (tmp_pair.Key);
         tmp_pair.Value := tmp_el_with_num.Element;
         Current_Cursor := Current_Cursor + 1;
      else
         tmp_pair.Value := No_Element;
      end if;
      return tmp_pair;
   end Get_Next;

   procedure Free is
   begin
      Key_Array.Free;
      My_Hash.Reset;
   end Free;

   -----------------------------------------------------------------------

   procedure Move (From : Natural; To : Natural) is
      tmp_key : Key_Type;
   begin
      tmp_key := Key_Array.Table (To);
      Key_Array.Table (To) := Key_Array.Table (From);
      Key_Array.Table (From) := tmp_key;
   end Move;

   function Lt (Op1, Op2 : Natural) return Boolean is
   begin
      return Key_Array.Table (Op1) < Key_Array.Table (Op2);
   end Lt;

   -----------------------------------------------------------------------

end Hashtable_With_Iterator;
