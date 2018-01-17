------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2018, AdaCore                     --
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

package body HTables is

   -------------------
   -- Static_HTable --
   -------------------

   package body Static_HTable is

      type HTable_Array is array (Header_Num) of Elmt_Ptr;

      type Instance_Data is record
         Table        : HTable_Array;
         Default_Iter : Cursor;
      end record;

      procedure Get_Non_Null (T : Instance; Iter : in out Cursor);
      --  Returns Null_Ptr if Iterator_Started is false of the Table is
      --  empty. Returns Iterator_Ptr if non null, or the next non null
      --  element in table if any.

      procedure Remove
        (T      : Instance;
         Index  : Header_Num;
         K      : Key);
      --  Low-level implementation for Remove

      ---------
      -- Get --
      ---------

      function Get (T : Instance; K : Key) return Elmt_Ptr is
         Elmt  : Elmt_Ptr;
      begin
         if T = null then
            return Null_Ptr;
         end if;

         Elmt := T.Table (Hash (K));

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

      function Get_Element (Iter : Cursor) return Elmt_Ptr is
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

      procedure Get_First (T : Instance; Iter : out Cursor) is
      begin
         if T = null then
            Iter.Iterator_Started := False;
         else
            Iter.Iterator_Started := True;
            Iter.Iterator_Index   := T.Table'First;
            Iter.Iterator_Ptr     := T.Table (Iter.Iterator_Index);
            Get_Non_Null (T, Iter);
         end if;
      end Get_First;

      --------------
      -- Get_Next --
      --------------

      procedure Get_Next (T : Instance; Iter : in out Cursor) is
      begin
         if not Iter.Iterator_Started then
            return;
         end if;

         Iter.Iterator_Ptr := Next (Iter.Iterator_Ptr);
         Get_Non_Null (T, Iter);
      end Get_Next;

      ---------------
      -- Get_First --
      ---------------

      function Get_First (T : Instance) return Elmt_Ptr is
      begin
         if T = null then
            return Null_Ptr;
         end if;

         Get_First (T, T.Default_Iter);
         return Get_Element (T.Default_Iter);
      end Get_First;

      --------------
      -- Get_Next --
      --------------

      function Get_Next (T : Instance) return Elmt_Ptr is
      begin
         if T = null then
            return Null_Ptr;
         end if;

         Get_Next (T, T.Default_Iter);
         return Get_Element (T.Default_Iter);
      end Get_Next;

      -------------------------
      -- Remove_And_Get_Next --
      -------------------------

      procedure Remove_And_Get_Next
        (T : in out Instance; Iter : in out Cursor)
      is
         Tmp : Elmt_Ptr;
         Index : Header_Num;
      begin
         if T = null or else not Iter.Iterator_Started then
            Iter.Iterator_Started := False;
            return;
         end if;

         --  Save current setup
         Tmp   := Iter.Iterator_Ptr;
         Index := Iter.Iterator_Index;

         --  Move to next element
         Iter.Iterator_Ptr := Next (Iter.Iterator_Ptr);
         Get_Non_Null (T, Iter);

         --  Remove old one
         Remove (T, Index, Get_Key (Tmp));
      end Remove_And_Get_Next;

      ------------------
      -- Get_Non_Null --
      ------------------

      procedure Get_Non_Null (T : Instance; Iter : in out Cursor) is
      begin
         if T = null then
            Iter.Iterator_Started := False;
         else
            while Iter.Iterator_Ptr = Null_Ptr  loop
               if Iter.Iterator_Index = T.Table'Last then
                  Iter.Iterator_Started := False;
                  return;
               end if;

               Iter.Iterator_Index := Iter.Iterator_Index + 1;
               Iter.Iterator_Ptr   := T.Table (Iter.Iterator_Index);
            end loop;
         end if;
      end Get_Non_Null;

      ------------
      -- Remove --
      ------------

      procedure Remove (T : Instance; K : Key) is
         Index     : constant Header_Num := Hash (K);
      begin
         Remove (T, Index, K);
      end Remove;

      ------------
      -- Remove --
      ------------

      procedure Remove
        (T     : Instance;
         Index : Header_Num;
         K     : Key)
      is
         Next_Elmt : Elmt_Ptr;
         Elmt      : Elmt_Ptr;
      begin
         if T = null then
            return;
         end if;

         Elmt := T.Table (Index);

         if Elmt = Null_Ptr then
            return;
         end if;

         if Equal (Get_Key (Elmt), K) then
            T.Table (Index) := Next (Elmt);
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

      procedure Reset (T : in out Instance) is
         procedure Unchecked_Free is
           new Ada.Unchecked_Deallocation (Instance_Data, Instance);

         Tmp : Elmt_Ptr;
      begin
         if T = null then
            return;
         end if;

         for J in T.Table'Range loop
            while T.Table (J) /= Null_Ptr loop
               Tmp := Next (T.Table (J));
               Free_Elmt_Ptr (T.Table (J));
               T.Table (J) := Tmp;
            end loop;
         end loop;

         Unchecked_Free (T);
      end Reset;

      ---------
      -- Set --
      ---------

      procedure Set (T : in out Instance; E : Elmt_Ptr) is
         Index : Header_Num;
      begin
         if T = null then
            T := new Instance_Data;
         end if;

         Index := Hash (Get_Key (E));
         Set_Next (E, T.Table (Index));
         T.Table (Index) := E;
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

      function Get (T : Instance; K : Key) return Element is
         Tmp : constant Elmt_Ptr := Get (T.Table, K);
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

      procedure Get_First (T : Instance; Iter : out Cursor) is
      begin
         Get_First (T.Table, Iter.Iter);
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

      procedure Get_Next (T : Instance; Iter : in out Cursor) is
      begin
         Get_Next (T.Table, Iter.Iter);
      end Get_Next;

      ---------------
      -- Get_First --
      ---------------

      function Get_First (T : Instance) return Element is
         Tmp : constant Elmt_Ptr := Get_First (T.Table);
      begin
         if Tmp = null then
            return No_Element;
         else
            return Tmp.E;
         end if;
      end Get_First;

      --------------
      -- Get_Next --
      --------------

      function Get_Next (T : Instance) return Element is
         Tmp : constant Elmt_Ptr := Get_Next (T.Table);
      begin
         if Tmp = null then
            return No_Element;
         else
            return Tmp.E;
         end if;
      end Get_Next;

      -------------------------
      -- Remove_And_Get_Next --
      -------------------------

      procedure Remove_And_Get_Next
        (T : in out Instance; Iter : in out Cursor) is
      begin
         Remove_And_Get_Next (T.Table, Iter.Iter);
      end Remove_And_Get_Next;

      -------------
      -- Get_Key --
      -------------

      function Get_Key (Iter : Cursor) return Key is
         Ptr : constant Elmt_Ptr := Get_Element (Iter.Iter);
      begin
         if Ptr = null then
            raise Program_Error;
         else
            return Ptr.K.all;
         end if;
      end Get_Key;

      -----------------
      -- Set_Element --
      -----------------

      procedure Set_Element (Iter : Cursor; E : Element) is
         Ptr : constant Elmt_Ptr := Get_Element (Iter.Iter);
      begin
         if Ptr /= null then
            Ptr.E := E;
         end if;
      end Set_Element;

      -----------------
      -- Get_Element --
      -----------------

      function Get_Element (Iter : Cursor) return Element is
         Ptr : constant Elmt_Ptr := Get_Element (Iter.Iter);
      begin
         if Ptr = null then
            return No_Element;
         else
            return Ptr.E;
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

      procedure Remove (T : Instance; K : Key) is
      begin
         Remove (T.Table, K);
      end Remove;

      -----------
      -- Reset --
      -----------

      procedure Reset (T : in out Instance) is
      begin
         Reset (T.Table);
      end Reset;

      ---------
      -- Set --
      ---------

      procedure Set (T : in out Instance; K : Key; E : Element) is
         Tmp : constant Elmt_Ptr := Get (T.Table, K);
      begin
         if Tmp = null then
            Set (T.Table, new Element_Wrapper'(new Key'(K), E, null));
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

end HTables;
