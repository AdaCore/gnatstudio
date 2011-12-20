------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with System.Memory; use System.Memory;

package body Dynamic_Arrays is

   pragma Suppress (All_Checks);

   pragma Warnings (Off);
   --  This UC is safe aliasing-wise, so kill warning
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Table_Ptr);
   function Convert is new Ada.Unchecked_Conversion
     (Table_Ptr, System.Address);
   pragma Warnings (On);

   Component_Size : constant size_t :=
     Table_Type'Component_Size / System.Storage_Unit;

   ----------
   -- Last --
   ----------

   function Last (T : Instance) return Index_Type is
   begin
      return Index_Type'Pred (T.P.Next_To_Last);
   end Last;

   ------------
   -- Length --
   ------------

   function Length (T : Instance) return Index_Type is
   begin
      return T.P.Next_To_Last - First;
   end Length;

   ------------
   -- Append --
   ------------

   procedure Append (T : in out Instance; Item : Data) is
      New_Size : Index_Type;
      Old_Size : Index_Type;
   begin
      if T.Table = null then
         T := (Table => Convert
                 (Alloc (size_t (Table_Initial_Size) * Component_Size)),
               P     => (Next_To_Last   => First,
                         Last_Allocated => Index_Type'Pred
                           (First + Index_Type (Table_Initial_Size))));

      elsif T.P.Next_To_Last > T.P.Last_Allocated then
         Old_Size := T.P.Last_Allocated - First + 1;
         New_Size := Index_Type'Max
           (Old_Size + Index_Type (Table_Minimum_Increment),
            Old_Size * Index_Type (Table_Multiplier));
         T.Table := Convert
           (Realloc (Convert (T.Table), size_t (New_Size) * Component_Size));
         T.P.Last_Allocated := New_Size + First - 1;
      end if;

      T.Table (T.P.Next_To_Last) := Item;
      T.P.Next_To_Last := Index_Type'Succ (T.P.Next_To_Last);
   end Append;

   ------------
   -- Remove --
   ------------

   procedure Remove (T : in out Instance; Item : Data) is
   begin
      Remove (T, Find (T, Item));
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove (T : in out Instance; Index : Index_Type) is
   begin
      if T.Table /= null
        and then Index >= First
        and then Index < T.P.Next_To_Last
      then
         T.Table (Index .. T.P.Next_To_Last - 2) :=
           T.Table (Index + 1 .. T.P.Next_To_Last - 1);
         T.P.Next_To_Last := T.P.Next_To_Last - 1;

         if T.P.Next_To_Last = First then
            --  Might as well save some memory
            Free (T);
         end if;
      end if;
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove (T : in out Instance; From, To : Index_Type) is
   begin
      if T.Table /= null
        and then From >= First
        and then To < T.P.Next_To_Last
      then
         T.Table (From .. T.P.Next_To_Last - 2 - To + From) :=
           T.Table (To + 1 .. T.P.Next_To_Last - 1);
         T.P.Next_To_Last := T.P.Next_To_Last - 1 - To + From;

         if T.P.Next_To_Last = First then
            --  Might as well save some memory
            Free (T);
         end if;
      end if;
   end Remove;

   ----------
   -- Find --
   ----------

   function Find (T : Instance; Item : Data) return Index_Type is
   begin
      for Index in First .. T.P.Next_To_Last - 1 loop
         if T.Table (Index) = Item then
            return Index;
         end if;
      end loop;
      return Index_Type'Pred (First);
   end Find;

   ----------
   -- Free --
   ----------

   procedure Free (T : in out Instance) is
   begin
      if T.Table /= null then
         Free (Convert (T.Table));
         T.Table := null;
         T.P.Next_To_Last := First;
      end if;
   end Free;
end Dynamic_Arrays;
