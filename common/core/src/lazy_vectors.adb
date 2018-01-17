------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

package body Lazy_Vectors is

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Lazy_Vector) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Lazy_Vector_Record, Lazy_Vector);
   begin
      Free (This.Datas);
      Internal (This);
   end Free;

   ------------
   -- Insert --
   ------------

   procedure Insert  (Vector : Lazy_Vector; Data : Data_Type) is
      Dummy : Iterator;
      pragma Unreferenced (Dummy);
   begin
      Insert (Vector, Data, Dummy);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Vector : Lazy_Vector;
      Data   : Data_Type;
      Pos    : out Iterator)
   is
   begin
      Pos.Vector := Vector;

      if Vector.Datas = null then
         Vector.Datas := new Data_Array'(1 => Data);
         Pos.Index := 1;
         Vector.Last_Item_Index := 1;
         return;
      end if;

      for J in Vector.Datas'Range loop
         if Vector.Datas (J) = Null_Data_Type then
            Vector.Datas (J) := Data;
            Pos.Index := J;

            if J > Vector.Last_Item_Index then
               Vector.Last_Item_Index := J;
            end if;

            return;
         end if;
      end loop;

      declare
         Old_Array : Data_Array_Access := Vector.Datas;
      begin
         Vector.Datas := new Data_Array (1 .. Vector.Datas'Length * 2);
         Vector.Datas (1 .. Old_Array.all'Last) := Old_Array.all;
         Vector.Datas (Old_Array'Last + 1) := Data;
         Pos.Index := Old_Array'Last + 1;
         Vector.Datas (Old_Array'Last + 2 .. Vector.Datas'Last) :=
           (others => Null_Data_Type);
         Free (Old_Array);

         Vector.Last_Item_Index := Pos.Index;
      end;
   end Insert;

   -----------
   -- First --
   -----------

   function First (Vector : Lazy_Vector) return Iterator is
      It : Iterator;
   begin
      It.Vector := Vector;
      It.Index := 1;

      if not Is_Valid (It) then
         Next (It);
      end if;

      return It;
   end First;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Iterator) is
   begin
      loop
         It.Index := It.Index + 1;

         exit when Is_Valid (It);
      end loop;
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (It : Iterator) return Boolean is
   begin
      return It.Vector = null or else It.Index > It.Vector.Last_Item_Index;
   end At_End;

   ---------
   -- Get --
   ---------

   function Get (It : Iterator) return Data_Type is
   begin
      return It.Vector.Datas (It.Index);
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (It : Iterator; Value : Data_Type) is
   begin
      It.Vector.Datas (It.Index) := Value;
   end Set;

   ----------------
   -- Get_Vector --
   ----------------

   function Get_Vector (It : Iterator) return access Lazy_Vector_Record is
   begin
      return It.Vector;
   end Get_Vector;

   ------------
   -- Delete --
   ------------

   procedure Delete (It : Iterator) is
   begin
      It.Vector.Datas (It.Index) := Null_Data_Type;

      if It.Index = It.Vector.Last_Item_Index then
         It.Vector.Last_Item_Index := 0;

         for J in reverse 1 .. It.Index - 1 loop
            if It.Vector.Datas (J) /= Null_Data_Type then
               It.Vector.Last_Item_Index := J;

               exit;
            end if;
         end loop;
      end if;
   end Delete;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (It : Iterator) return Boolean is
   begin
      return At_End (It) or else It.Vector.Datas (It.Index) /= Null_Data_Type;
   end Is_Valid;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Data_Array_Access) is
      procedure Internal_Free is new Ada.Unchecked_Deallocation
        (Data_Array, Data_Array_Access);
   begin
      Internal_Free (This);
   end Free;

end Lazy_Vectors;
