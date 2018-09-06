------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2018, AdaCore                     --
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

with Glib;                   use Glib;
with GNATCOLL.Utils;         use GNATCOLL.Utils;

package body GVD.Variables.Types.Repeats is

   -----------
   -- Clone --
   -----------

   overriding procedure Clone
     (Self : not null access GVD_Repeat_Type;
      Item : not null GVD_Generic_Type_Access) is
   begin
      GVD_Generic_Type (Self.all).Clone (Item);

      --  duplicate the type of the repeated item.
      --  The value itself is in fact not duplicated, since the leafs of the
      --  type tree is a simple_type (or one of its children), that does not
      --  clone the value.
      Self.Value := GVD_Repeat_Type_Access (Item).Value.Clone;
   end Clone;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Self : not null access GVD_Repeat_Type) is
   begin
      Self.Value := Empty_GVD_Type_Holder;
      GVD_Generic_Type (Self.all).Free;
   end Free;

   --------------------
   -- Get_Repeat_Num --
   --------------------

   function Get_Repeat_Num
     (Self : not null access GVD_Repeat_Type)
      return Integer is
   begin
      return Self.Repeat_Num;
   end Get_Repeat_Num;

   ----------------------
   -- Get_Simple_Value --
   ----------------------

   overriding function Get_Simple_Value
     (Self : not null access GVD_Repeat_Type) return String is
   begin
      return
        (if Self.Value.Data = null
         then ""
         else Self.Value.Get_Type.Get_Simple_Value)
        & " <" & Image (Self.Repeat_Num, Min_Width => 0) & " times>";
   end Get_Simple_Value;

   -------------------
   -- Get_Type_Name --
   -------------------

   overriding function Get_Type_Name
     (Self : not null access GVD_Repeat_Type)
      return String is
   begin
      --  So that we display  "(record) <repeat 11 times>", and not
      --  "() <repeat 11 times>".  The latter requires one extra level of
      --  expansion in the variables view.
      if Self.Value.Data = null then
         return "";
      else
         return Self.Value.Get_Type.Get_Type_Name;
      end if;
   end Get_Type_Name;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Self : not null access GVD_Repeat_Type)
      return GVD_Type_Holder is
   begin
      return Self.Value;
   end Get_Value;

   ---------------------
   -- New_Repeat_Type --
   ---------------------

   function New_Repeat_Type return GVD_Type_Holder is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_Repeat_Type);
   begin
      return GVD_Type_Holder'(Ada.Finalization.Controlled with Data);
   end New_Repeat_Type;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Self         : not null access GVD_Repeat_Type;
      Current      : GVD_Type_Holder'Class;
      Replace_With : GVD_Type_Holder'Class)
      return GVD_Type_Holder'Class is
   begin
      if Self.Value.Data = Current.Data then
         Self.Value := GVD_Type_Holder (Replace_With);
         return Replace_With;
      end if;

      return Empty_GVD_Type_Holder;
   end Replace;

   --------------------
   -- Set_Repeat_Num --
   --------------------

   procedure Set_Repeat_Num
     (Self : not null access GVD_Repeat_Type;
      Num  : Integer) is
   begin
      Self.Repeat_Num := Num;
   end Set_Repeat_Num;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Self  : not null access GVD_Repeat_Type;
      Value : GVD_Type_Holder) is
   begin
      Self.Valid := True;
      Self.Value := Value;
   end Set_Value;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Self : not null access GVD_Repeat_Type) return Generic_Iterator'Class is
   begin
      --  No child ? Return an iterator that does nothing
      if Self.Value.Data = null then
         return Create_Empty_Iterator;
      else
         --  Else iterate directly the components of the repeated type, so that
         --  we have one less level in the Variables view
         return Self.Value.Get_Type.Start;
      end if;
   end Start;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Repeat_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean is
   begin
      return Item.Data /= null
        and then Item.Data.Instance /= null
        and then Item.Data.Instance.all in GVD_Repeat_Type'Class
        and then Self.Value.Data.Instance.Structurally_Equivalent
          (GVD_Repeat_Type_Access (Item.Data.Instance).Value);
   end Structurally_Equivalent;

end GVD.Variables.Types.Repeats;
