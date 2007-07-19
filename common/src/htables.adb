-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2007, AdaCore             --
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

with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;    use Ada.Characters.Handling;

package body HTables is

   -------------------
   -- Static_HTable --
   -------------------

   package body Static_HTable is

      procedure Get_Non_Null (Hash_Table : HTable; Iter : in out Iterator);
      --  Returns Null_Ptr if Iterator_Started is false of the Table is
      --  empty. Returns Iterator_Ptr if non null, or the next non null
      --  element in table if any.

      procedure Remove
        (Hash_Table : in out HTable;
         Elmt       : in out Elmt_Ptr;
         Index      : Header_Num;
         K          : Key);
      --  Low-level implementation for Remove

      ---------
      -- Get --
      ---------

      function Get (Hash_Table : HTable; K : Key) return Elmt_Ptr is
         Elmt  : Elmt_Ptr;
      begin
         Elmt := Hash_Table.Table (Hash (K));

         loop
            if Elmt = Null_Ptr then
               return Null_Ptr;

            elsif Equal (Get_Key (Elmt), K) then
               return Elmt;

            else
               Elmt := Next (Elmt);
            end if;
         end loop;
      end Get;

      -----------------
      -- Get_Element --
      -----------------

      function Get_Element (Iter : Iterator) return Elmt_Ptr is
      begin
         if not Iter.Iterator_Started then
            return Null_Ptr;
         else
            return Iter.Iterator_Ptr;
         end if;
      end Get_Element;

      ---------------
      -- Get_First --
      ---------------

      procedure Get_First (Hash_Table : HTable; Iter : out Iterator) is
      begin
         Iter.Iterator_Started := True;
         Iter.Iterator_Index   := Hash_Table.Table'First;
         Iter.Iterator_Ptr     := Hash_Table.Table (Iter.Iterator_Index);
         Get_Non_Null (Hash_Table, Iter);
      end Get_First;

      --------------
      -- Get_Next --
      --------------

      procedure Get_Next (Hash_Table : HTable; Iter : in out Iterator) is
      begin
         if not Iter.Iterator_Started then
            return;
         end if;

         Iter.Iterator_Ptr := Next (Iter.Iterator_Ptr);
         Get_Non_Null (Hash_Table, Iter);
      end Get_Next;

      -------------------------
      -- Remove_And_Get_Next --
      -------------------------

      procedure Remove_And_Get_Next
        (Hash_Table : in out HTable; Iter : in out Iterator)
      is
         Tmp : Elmt_Ptr;
         Index : Header_Num;
      begin
         if not Iter.Iterator_Started then
            return;
         end if;

         --  Save current setup
         Tmp   := Iter.Iterator_Ptr;
         Index := Iter.Iterator_Index;

         --  Move to next element
         Iter.Iterator_Ptr := Next (Iter.Iterator_Ptr);
         Get_Non_Null (Hash_Table, Iter);

         --  Remove old one
         Remove (Hash_Table, Tmp, Index, Get_Key (Tmp));
      end Remove_And_Get_Next;

      ------------------
      -- Get_Non_Null --
      ------------------

      procedure Get_Non_Null (Hash_Table : HTable; Iter : in out Iterator) is
      begin
         while Iter.Iterator_Ptr = Null_Ptr  loop
            if Iter.Iterator_Index = Hash_Table.Table'Last then
               Iter.Iterator_Started := False;
               return;
            end if;

            Iter.Iterator_Index := Iter.Iterator_Index + 1;
            Iter.Iterator_Ptr   := Hash_Table.Table (Iter.Iterator_Index);
         end loop;
      end Get_Non_Null;

      ------------
      -- Remove --
      ------------

      procedure Remove (Hash_Table : in out HTable; K : Key) is
         Index     : constant Header_Num := Hash (K);
         Elmt      : Elmt_Ptr;
      begin
         Elmt := Hash_Table.Table (Index);
         Remove (Hash_Table, Elmt, Index, K);
      end Remove;

      ------------
      -- Remove --
      ------------

      procedure Remove
        (Hash_Table : in out HTable;
         Elmt       : in out Elmt_Ptr;
         Index      : Header_Num;
         K          : Key)
      is
         Next_Elmt : Elmt_Ptr;
      begin
         if Elmt = Null_Ptr then
            return;

         elsif Equal (Get_Key (Elmt), K) then
            Hash_Table.Table (Index) := Next (Elmt);
            Free_Elmt_Ptr (Elmt);

         else
            loop
               Next_Elmt :=  Next (Elmt);

               if Next_Elmt = Null_Ptr then
                  return;

               elsif Equal (Get_Key (Next_Elmt), K) then
                  Set_Next (Elmt, Next (Next_Elmt));
                  Free_Elmt_Ptr (Next_Elmt);
                  return;

               else
                  Elmt := Next_Elmt;
               end if;
            end loop;
         end if;
      end Remove;

      -----------
      -- Reset --
      -----------

      procedure Reset (Hash_Table : in out HTable) is
         Tmp : Elmt_Ptr;
      begin
         for J in Hash_Table.Table'Range loop
            while Hash_Table.Table (J) /= Null_Ptr loop
               Tmp := Next (Hash_Table.Table (J));
               Free_Elmt_Ptr (Hash_Table.Table (J));
               Hash_Table.Table (J) := Tmp;
            end loop;
         end loop;
      end Reset;

      ---------
      -- Set --
      ---------

      procedure Set (Hash_Table : in out HTable; E : Elmt_Ptr) is
         Index : Header_Num;
      begin
         Index := Hash (Get_Key (E));
         Set_Next (E, Hash_Table.Table (Index));
         Hash_Table.Table (Index) := E;
      end Set;

   end Static_HTable;

   -------------------
   -- Simple_HTable --
   -------------------

   package body Simple_HTable is

      use Tab;

      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation (Element_Wrapper, Elmt_Ptr);
      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation (Key, Key_Access);

      ----------
      -- Free --
      ----------

      procedure Free (X : in out Elmt_Ptr) is
      begin
         Unchecked_Free (X.K);
         Free_Element (X.E);
         Unchecked_Free (X);
      end Free;

      ---------
      -- Get --
      ---------

      function Get (Hash_Table : HTable; K : Key) return Element is
         Tmp : constant Elmt_Ptr := Get (Hash_Table.Table, K);
      begin
         if Tmp = null then
            return No_Element;
         else
            return Tmp.E;
         end if;
      end Get;

      ---------------
      -- Get_First --
      ---------------

      procedure Get_First (Hash_Table : HTable; Iter : out Iterator) is
      begin
         Get_First (Hash_Table.Table, Iter.Iter);
      end Get_First;

      -------------
      -- Get_Key --
      -------------

      function Get_Key (E : Elmt_Ptr) return Key is
      begin
         return E.K.all;
      end Get_Key;

      --------------
      -- Get_Next --
      --------------

      procedure Get_Next (Hash_Table : HTable; Iter : in out Iterator) is
      begin
         Get_Next (Hash_Table.Table, Iter.Iter);
      end Get_Next;

      -------------------------
      -- Remove_And_Get_Next --
      -------------------------

      procedure Remove_And_Get_Next
        (Hash_Table : in out HTable; Iter : in out Iterator) is
      begin
         Remove_And_Get_Next (Hash_Table.Table, Iter.Iter);
      end Remove_And_Get_Next;

      -------------
      -- Get_Key --
      -------------

      function Get_Key (Iter : Iterator) return Key is
      begin
         return Get_Element (Iter.Iter).K.all;
      end Get_Key;

      -----------------
      -- Get_Element --
      -----------------

      function Get_Element (Iter : Iterator) return Element is
         Ptr : constant Elmt_Ptr := Get_Element (Iter.Iter);
      begin
         if Ptr = null then
            return No_Element;
         else
            return Get_Element (Iter.Iter).E;
         end if;
      end Get_Element;

      ----------
      -- Next --
      ----------

      function Next (E : Elmt_Ptr) return Elmt_Ptr is
      begin
         return E.Next;
      end Next;

      ------------
      -- Remove --
      ------------

      procedure Remove (Hash_Table : in out HTable; K : Key) is
      begin
         Remove (Hash_Table.Table, K);
      end Remove;

      -----------
      -- Reset --
      -----------

      procedure Reset (Hash_Table : in out HTable) is
      begin
         Reset (Hash_Table.Table);
      end Reset;

      ---------
      -- Set --
      ---------

      procedure Set (Hash_Table : in out HTable; K : Key; E : Element) is
         Tmp : constant Elmt_Ptr := Get (Hash_Table.Table, K);
      begin
         if Tmp = null then
            Set (Hash_Table.Table, new Element_Wrapper'(new Key'(K), E, null));
         else
            Free_Element (Tmp.E);
            Tmp.E := E;
         end if;
      end Set;

      --------------
      -- Set_Next --
      --------------

      procedure Set_Next (E : Elmt_Ptr; Next : Elmt_Ptr) is
      begin
         E.Next := Next;
      end Set_Next;

   end Simple_HTable;

   ----------
   -- Hash --
   ----------

   function Hash (Key : String) return Header_Num is
      type Uns is mod 2 ** 32;

      function Rotate_Left (Value : Uns; Amount : Natural) return Uns;
      pragma Import (Intrinsic, Rotate_Left);

      Tmp : Uns := 0;

   begin
      for J in Key'Range loop
         Tmp := Rotate_Left (Tmp, 1) + Character'Pos (Key (J));
      end loop;

      return Header_Num'First +
               Header_Num'Base (Tmp mod Header_Num'Range_Length);
   end Hash;

   ---------------------------
   -- Case_Insensitive_Hash --
   ---------------------------

   function Case_Insensitive_Hash (Key : String) return Header_Num is
      type Uns is mod 2 ** 32;

      function Rotate_Left (Value : Uns; Amount : Natural) return Uns;
      pragma Import (Intrinsic, Rotate_Left);

      Tmp : Uns := 0;

   begin
      for J in Key'Range loop
         Tmp := Rotate_Left (Tmp, 1) + Character'Pos (To_Lower (Key (J)));
      end loop;

      return Header_Num'First +
               Header_Num'Base (Tmp mod Header_Num'Range_Length);
   end Case_Insensitive_Hash;

end HTables;
