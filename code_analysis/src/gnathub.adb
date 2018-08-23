------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2018, AdaCore                   --
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

with Ada.Strings.Hash;
with GNATCOLL.Utils;
with String_Utils;     use String_Utils;

package body GNAThub is

   -----------------------
   -- Get_Current_Count --
   -----------------------

   function Get_Current_Count (Self : Filterable_Item) return Natural
   is
      (Self.Current);

   ---------------------
   -- Get_Total_Count --
   ---------------------

   function Get_Total_Count (Self : Filterable_Item) return Natural
   is
      (Self.Total);

   --------------------
   -- Reset_Counters --
   --------------------

   procedure Reset_Counters (Self : in out Filterable_Item) is
   begin
      Self.Current := 0;
      Self.Total := 0;
   end Reset_Counters;

   -----------------------------
   -- Increment_Current_Count --
   -----------------------------

   procedure Increment_Current_Count (Self : in out Filterable_Item) is
   begin
      Self.Current := Self.Current + 1;
   end Increment_Current_Count;

   -----------------------------
   -- Decrement_Current_Count --
   -----------------------------

   procedure Decrement_Current_Count (Self : in out Filterable_Item) is
   begin
      if Self.Current > 0 then
         Self.Current := Self.Current - 1;
      end if;
   end Decrement_Current_Count;

   ---------------------------
   -- Increment_Total_Count --
   ---------------------------

   procedure Increment_Total_Count (Self : in out Filterable_Item) is
   begin
      Self.Total := Self.Total + 1;
   end Increment_Total_Count;

   -----------
   -- Image --
   -----------

   function Image (Self : Filterable_Item) return String is
   begin
      if Self.Current = Self.Total then
         return GNATCOLL.Utils.Image (Self.Total, Min_Width => 1);
      else
         return GNATCOLL.Utils.Image
           (Self.Current,
            Min_Width => 1)
           & " ("
           & GNATCOLL.Utils.Image
           (Self.Total,
            Min_Width => 1)
           & ")";
      end if;
   end Image;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Item : Severity_Record)
      return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return To_Unbounded_String
        (Format_Title (Message_Importance_Type'Image (Item.Ranking)));
   end Get_Name;

   ----------
   -- Hash --
   ----------

   function Hash (Item : Severity_Access) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Message_Importance_Type'Image (Item.Ranking));
   end Hash;

   ----------
   -- Less --
   ----------

   function Less (L, R : GNAThub.Tool_Access) return Boolean
   is
     (L.Name < R.Name);

   ----------
   -- Less --
   ----------

   function Less (Left : Rule_Access; Right : Rule_Access) return Boolean is
      use type Ada.Strings.Unbounded.Unbounded_String;

   begin
      if Left.Name /= Right.Name then
         return Left.Name < Right.Name;
      else
         return Left.Identifier < Right.Identifier;
      end if;
   end Less;

   ----------
   -- Less --
   ----------

   function Less (L, R : GNAThub.Severity_Access) return Boolean is
   begin
      return L.Ranking > R.Ranking;
   end Less;

end GNAThub;
