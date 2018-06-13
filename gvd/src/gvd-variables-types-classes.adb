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

with Ada.Unchecked_Deallocation;

with GVD.Variables.Types.Records; use GVD.Variables.Types.Records;

package body GVD.Variables.Types.Classes is

   type Generic_Iterator_Access is access all Generic_Iterator'Class;
   procedure Free is new Ada.Unchecked_Deallocation
     (Generic_Iterator'Class, Generic_Iterator_Access);

   type Class_Iterator is new Generic_Iterator with record
      Item     : GVD_Class_Type_Access;
      Ancestor : Natural;
      Child    : Generic_Iterator_Access;
   end record;

   overriding procedure Adjust   (Self : in out Class_Iterator);
   overriding procedure Finalize (Self : in out Class_Iterator);

   overriding procedure Next (Iter : in out Class_Iterator);
   overriding function At_End (Iter : Class_Iterator) return Boolean;
   overriding function Data
     (Iter : Class_Iterator)
      return GVD_Type_Holder'Class;
   overriding function Field_Name
     (Iter : Class_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String;

   ------------------
   -- Add_Ancestor --
   ------------------

   procedure Add_Ancestor
     (Self     : not null access GVD_Class_Type;
      Num      : Positive;
      Ancestor : GVD_Type_Holder) is
   begin
      pragma Assert (Num <= Self.Num_Ancestors);

      Self.Ancestors (Num) := Ancestor;
      Self.Valid := True;
      Self.Ancestors (Num).Get_Type.Valid := True;
   end Add_Ancestor;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Class_Iterator) is
   begin
      if Self.Child /= null then
         Self.Child := new Generic_Iterator'Class'(Self.Child.all);
      end if;
   end Adjust;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Class_Iterator) return Boolean is
   begin
      if Iter.Ancestor <= Iter.Item.Ancestors'Last then
         return False;
      end if;

      if Iter.Child /= null then
         return At_End (Iter.Child.all);
      end if;

      if Iter.Ancestor = Iter.Item.Ancestors'Last + 1 then
         return Iter.Item.Child.Data /= null;
      else
         return True;
      end if;
   end At_End;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : not null access GVD_Class_Type) is
      T : GVD_Generic_Type_Access;
   begin
      for A in Self.Ancestors'Range loop
         T := Self.Ancestors (A).Get_Type;
         if T /= null then
            T.Clear;
         end if;
      end loop;

      T := Self.Child.Get_Type;
      if T /= null then
         T.Clear;
      end if;
   end Clear;

   -----------
   -- Clone --
   -----------

   overriding procedure Clone
     (Self : not null access GVD_Class_Type;
      Item : not null GVD_Generic_Type_Access)
   is
      Src : constant GVD_Class_Type_Access := GVD_Class_Type_Access (Item);
   begin
      GVD_Generic_Type (Self.all).Clone (Item);

      for A in Src.Ancestors'Range loop
         Self.Ancestors (A) := Src.Ancestors (A).Clone;
      end loop;

      Self.Child := Src.Child.Clone;
   end Clone;

   ----------
   -- Data --
   ----------

   overriding function Data
     (Iter : Class_Iterator) return GVD_Type_Holder'Class is
   begin
      if Iter.Ancestor <= Iter.Item.Ancestors'Last then
         return Iter.Item.Ancestors (Iter.Ancestor);

      elsif Iter.Child /= null then
         return Data (Iter.Child.all);

      else
         return Iter.Item.Child;
      end if;
   end Data;

   ----------------
   -- Field_Name --
   ----------------

   overriding function Field_Name
     (Iter : Class_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String is
   begin
      if Iter.Ancestor <= Iter.Item.Ancestors'Last then
         if Base /= "" then
            return Base;
         else
            return "<parent class>";
         end if;

      elsif Iter.Child /= null then
         return Field_Name (Iter.Child.all, Lang, Base);

      else
         return Base;
      end if;
   end Field_Name;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Class_Iterator) is
   begin
      Free (Self.Child);
   end Finalize;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : not null access GVD_Class_Type) is
   begin
      for A in Self.Ancestors'Range loop
         Self.Ancestors (A) := Empty_GVD_Type_Holder;
      end loop;

      Self.Child := Empty_GVD_Type_Holder;
      GVD_Generic_Type (Self.all).Free;
   end Free;

   ------------------
   -- Get_Ancestor --
   ------------------

   function Get_Ancestor
     (Self : not null access GVD_Class_Type;
      Num  : Positive)
      return GVD_Type_Holder is
   begin
      pragma Assert (Num <= Self.Num_Ancestors);
      return Self.Ancestors (Num);
   end Get_Ancestor;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
     (Self : not null access GVD_Class_Type)
      return GVD_Type_Holder is
   begin
      return Self.Child;
   end Get_Child;

   -----------------------
   -- Get_Num_Ancestors --
   -----------------------

   function Get_Num_Ancestors
     (Self : not null access GVD_Class_Type)
      return Natural is
   begin
      return Self.Num_Ancestors;
   end Get_Num_Ancestors;

   -------------------
   -- Get_Type_Name --
   -------------------

   overriding function Get_Type_Name
     (Self : not null access GVD_Class_Type;
      Lang : Language.Language_Access)
      return String is
   begin
      if Self.Child.Data /= null then
         return Self.Child.Get_Type.Get_Type_Name (Lang);
      else
         return GVD.Variables.Types.Get_Type_Name
           (GVD_Generic_Type_Access (Self), Lang);
      end if;
   end Get_Type_Name;

   --------------------
   -- New_Class_Type --
   --------------------

   function New_Class_Type
     (Num_Ancestors : Natural) return GVD_Type_Holder
   is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_Class_Type (Num_Ancestors));
   begin
      return GVD_Type_Holder'(Ada.Finalization.Controlled with Data);
   end New_Class_Type;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Class_Iterator) is
   begin
      if Iter.Ancestor <= Iter.Item.Ancestors'Last then
         Iter.Ancestor := Iter.Ancestor + 1;

      else
         if Iter.Child /= null then
            Iter.Child.Next;
         else
            Iter.Ancestor := Iter.Ancestor + 1;
         end if;
      end if;
   end Next;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Self         : not null access GVD_Class_Type;
      Current      : GVD_Type_Holder'Class;
      Replace_With : GVD_Type_Holder'Class)
      return GVD_Type_Holder'Class is
   begin
      for A in Self.Ancestors'Range loop
         if Self.Ancestors (A).Data = Current.Data then
            Self.Ancestors (A) := GVD_Type_Holder (Replace_With);
            return Replace_With;
         end if;
      end loop;

      if Self.Child.Data = Current.Data then
         Self.Child := GVD_Type_Holder (Replace_With);
         return Replace_With;
      end if;

      return Empty_GVD_Type_Holder;
   end Replace;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Self  : not null access GVD_Class_Type;
      Child : GVD_Type_Holder) is
   begin
      pragma Assert (Self.Child.Data = null);

      Self.Child := Child;
      GVD_Record_Type_Access (Child.Get_Type).Draw_Border (False);
   end Set_Child;

   -------------------
   -- Set_Type_Name --
   -------------------

   overriding procedure Set_Type_Name
     (Self : not null access GVD_Class_Type;
      Name : String) is
   begin
      if Self.Child.Data /= null then
         Self.Child.Get_Type.Set_Type_Name (Name);
      end if;
   end Set_Type_Name;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Self : not null access GVD_Class_Type) return Generic_Iterator'Class
   is
      Iter : Class_Iterator;
   begin
      Iter.Item := GVD_Class_Type_Access (Self);

      if Self.Ancestors'Length = 0 then
         Iter.Ancestor := Self.Ancestors'Last + 1;
      else
         Iter.Ancestor := Self.Ancestors'First;
      end if;

      if Self.Child.Data /= null then
         Iter.Child := new Generic_Iterator'Class'
           (Start (Self.Child.Get_Type));

         if At_End (Iter.Child.all) then
            Free (Iter.Child);
         end if;
      end if;

      return Iter;
   end Start;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Class_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean
   is
      Result : Boolean;
   begin
      if Item.Data = null
        or else Item.Data.Instance = null
        or else Item.Data.Instance.all not in GVD_Class_Type'Class
      then
         return False;
      end if;

      Result := Self.Num_Ancestors = GVD_Class_Type_Access
        (Item.Get_Type).Num_Ancestors
        and then Self.Child.Get_Type.Structurally_Equivalent
          (GVD_Class_Type_Access (Item.Get_Type).Child);

      if Result then
         for A in Self.Ancestors'Range loop
            Result := Result
              and then Self.Ancestors (A).Get_Type.Structurally_Equivalent
              (GVD_Class_Type_Access (Item.Get_Type).Ancestors (A));
         end loop;
      end if;

      --  We should consider two classes to be structurally equivalent if one
      --  of them is an ancestor for the other, to handle the following case:
      --    - type A is access Root'Class;
      --  If a structure contains a field of type A, and there is already an
      --  item of type Child (extending Root), it is possible that the type
      --  pointed two is the same.
      --
      --  We only need to test the first parent, since this is the only one
      --  whose data will have the same address.

      if not Result then
         if Self.Num_Ancestors /= 0 then
            Result := Self.Ancestors (1).Get_Type.Structurally_Equivalent
              (Item);
         end if;

         if GVD_Class_Type_Access (Item.Get_Type).Num_Ancestors /= 0 then
            Result := Result or else Self.Structurally_Equivalent
              (GVD_Class_Type_Access (Item.Get_Type).Ancestors (1));
         end if;
      end if;

      return Result;
   end Structurally_Equivalent;

end GVD.Variables.Types.Classes;
