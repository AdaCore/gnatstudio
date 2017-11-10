------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017, AdaCore                          --
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

with Glib; use Glib;

package body GVD.Variables.Types.Records is

   type Record_Iterator is new Generic_Iterator with record
      Item    : GVD_Record_Type_Access;
      Field   : Natural;
      Variant : Natural;
   end record;
   overriding procedure Next (Iter : in out Record_Iterator);
   overriding function At_End (Iter : Record_Iterator) return Boolean;
   overriding function Data
     (Iter : Record_Iterator) return GVD_Type_Holder'Class;
   overriding function Field_Name
     (Iter : Record_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String;

   procedure Internal_Free is new Ada.Unchecked_Deallocation
     (Record_Type_Array, Record_Type_Array_Access);

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Record_Iterator) return Boolean is
   begin
      return Iter.Field > Iter.Item.Fields'Last;
   end At_End;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : not null access GVD_Record_Type) is
   begin
      for J in Self.Fields'Range loop
         if Self.Fields (J).Value.Data /= null then
            Self.Fields (J).Value.Get_Type.Clear;

            if Self.Fields (J).Variant_Part /= null then
               for V in Self.Fields (J).Variant_Part'Range loop
                  Self.Fields (J).Variant_Part (V).Get_Type.Clear;
               end loop;
            end if;
         end if;
      end loop;
   end Clear;

   -----------
   -- Clone --
   -----------

   overriding procedure Clone
     (Self : not null access GVD_Record_Type;
      Item : not null GVD_Generic_Type_Access)
   is
      Src : constant GVD_Record_Type_Access := GVD_Record_Type_Access (Item);
   begin
      GVD_Generic_Type (Self.all).Clone (Item);

      for J in Src.Fields'Range loop
         Self.Fields (J).Name := Src.Fields (J).Name;
         --  Duplicate the type structure, but not the value itself.

         if Src.Fields (J).Value /= Empty_GVD_Type_Holder then
            Self.Fields (J).Value := Src.Fields (J).Value.Clone;
         end if;

         if Src.Fields (J).Variant_Part /= null then
            Self.Fields (J).Variant_Part :=
              new Record_Type_Array'(Src.Fields (J).Variant_Part.all);
            for V in Src.Fields (J).Variant_Part'Range loop
               Self.Fields (J).Variant_Part (V) :=
                 Src.Fields (J).Variant_Part (V).Clone;
            end loop;
         end if;
      end loop;
   end Clone;

   ----------
   -- Data --
   ----------

   overriding function Data
     (Iter : Record_Iterator) return GVD_Type_Holder'Class
   is
      Var : constant Record_Type_Array_Access :=
        Iter.Item.Fields (Iter.Field).Variant_Part;
   begin
      if Var /= null then
         return Var (Iter.Variant);
      else
         return Iter.Item.Fields (Iter.Field).Value;
      end if;
   end Data;

   -----------------
   -- Draw_Border --
   -----------------

   procedure Draw_Border
     (Self : not null access GVD_Record_Type;
      Draw : Boolean := True) is
   begin
      if Draw then
         Self.Border_Spacing := Border_Spacing;
      else
         Self.Border_Spacing := 0;
      end if;
   end Draw_Border;

   --
   ----------------
   -- Field_Name --
   ----------------

   overriding function Field_Name
     (Iter : Record_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String
   is
      Var : constant Record_Type_Array_Access :=
        Iter.Item.Fields (Iter.Field).Variant_Part;
   begin
      if Var /= null then
         return "<variant part>";
      else
         return Lang.Record_Field_Name
           (Base, To_String (Iter.Item.Fields (Iter.Field).Name));
      end if;
   end Field_Name;

   -----------------------
   -- Find_Variant_Part --
   -----------------------

   function Find_Variant_Part
     (Self     : not null access GVD_Record_Type;
      Field    : Positive;
      Contains : String) return GVD_Type_Holder is
   begin
      for J in Self.Fields (Field).Variant_Part'Range loop
         Self.Fields (Field).Variant_Part (J).Data.Instance.Valid := False;
      end loop;

      for J in Self.Fields (Field).Variant_Part'Range loop
         if (Contains'Length = 0
             and then GVD_Record_Type_Access
               (Self.Fields (Field).Variant_Part
                (J).Data.Instance).Fields'Length = 0)
           or else
             (GVD_Record_Type_Access
                (Self.Fields (Field).Variant_Part
                 (J).Data.Instance).Fields'Length /= 0
              and then
              GVD_Record_Type_Access
                (Self.Fields (Field).Variant_Part
                 (J).Data.Instance).Fields (1).Name = Contains)
         then
            Self.Fields (Field).Variant_Part (J).Data.Instance.Valid := True;
            return Self.Fields (Field).Variant_Part (J);
         end if;
      end loop;

      return Empty_GVD_Type_Holder;
   end Find_Variant_Part;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : not null access GVD_Record_Type) is
   begin
      for J in Self.Fields'Range loop
         if Self.Fields (J).Value.Data /= null then
            Self.Fields (J).Value := Empty_GVD_Type_Holder;

            if Self.Fields (J).Variant_Part /= null then
               for V in Self.Fields (J).Variant_Part'Range loop
                  Self.Fields (J).Variant_Part (V) := Empty_GVD_Type_Holder;
               end loop;
            end if;

            Self.Fields (J).Name := Null_Unbounded_String;
            Free (Self.Fields (J).Variant_Part);
         end if;
      end loop;

      GVD_Generic_Type (Self.all).Free;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Value : in out Record_Type_Array_Access) is
   begin
      if Value = null then
         return;
      end if;

      for Index in Value'Range loop
         Value (Index) := Empty_GVD_Type_Holder;
      end loop;
      Internal_Free (Value);
   end Free;

   --------------------
   -- Get_Field_Name --
   --------------------

   function Get_Field_Name
     (Self  : not null access GVD_Record_Type;
      Index : Positive)
      return String is
   begin
      return To_String (Self.Fields (Index).Name);
   end Get_Field_Name;

   ----------------------
   -- Get_Simple_Value --
   ----------------------

   overriding function Get_Simple_Value
     (Self : not null access GVD_Record_Type) return String is
   begin
      if Self.Fields'Length = 0 then
         return "null record";
      else
         return "";  --  Value given in the fields
      end if;
   end Get_Simple_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Self  : not null access GVD_Record_Type;
      Field : Positive)
      return GVD_Type_Holder is
   begin
      return Self.Fields (Field).Value;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Self  : not null access GVD_Record_Type;
      Field : String)
      return GVD_Type_Holder is
   begin
      for J in Self.Fields'Range loop
         if Self.Fields (J).Name = Field then
            return Self.Fields (J).Value;
         end if;
      end loop;

      return Empty_GVD_Type_Holder;
   end Get_Value;

   -----------------------
   -- Get_Variant_Parts --
   -----------------------

   function Get_Variant_Parts
     (Self  : not null access GVD_Record_Type;
      Field : Positive) return Natural is
   begin
      if Self.Fields (Field).Variant_Part = null then
         return 0;
      else
         return Self.Fields (Field).Variant_Part'Length;
      end if;
   end Get_Variant_Parts;

   ---------------------
   -- New_Record_Type --
   ---------------------

   function New_Record_Type
     (Num_Fields : Natural) return GVD_Type_Holder
   is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_Record_Type (Num_Fields));
   begin
      --  A null record is always valid.
      if Num_Fields = 0 then
         Data.Instance.Valid := True;
      end if;

      return GVD_Type_Holder'(Ada.Finalization.Controlled with Data);
   end New_Record_Type;

   --------------------
   -- New_Union_Type --
   --------------------

   function New_Union_Type
     (Num_Fields : Positive)
      return GVD_Type_Holder
   is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_Union_Type (Num_Fields));
   begin
      return GVD_Type_Holder'(Ada.Finalization.Controlled with Data);
   end New_Union_Type;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Record_Iterator) is
      Var : Record_Type_Array_Access :=
        Iter.Item.Fields (Iter.Field).Variant_Part;

   begin
      --  If the current field has a variant part, we need to iterate
      if Var /= null then
         Iter.Variant := Iter.Variant + 1;
         if Iter.Variant <= Var'Last then
            return;
         end if;
      end if;

      Iter.Field := Iter.Field + 1;
      if Iter.Field <= Iter.Item.Fields'Last then
         Var := Iter.Item.Fields (Iter.Field).Variant_Part;
         if Var /= null then
            Iter.Variant := Var'First;
         end if;
      end if;
   end Next;

   ----------------
   -- Num_Fields --
   ----------------

   function Num_Fields
     (Self : not null access GVD_Record_Type)
      return Natural is
   begin
      return Self.Num_Fields;
   end Num_Fields;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Self         : not null access GVD_Record_Type;
      Current      : GVD_Type_Holder'Class;
      Replace_With : GVD_Type_Holder'Class) return GVD_Type_Holder'Class is
   begin
      for F in Self.Fields'Range loop
         if Self.Fields (F).Value.Data = Current.Data then
            Self.Fields (F).Value := GVD_Type_Holder (Replace_With);
            return Replace_With;
         end if;

         if Self.Fields (F).Variant_Part /= null then
            for V in Self.Fields (F).Variant_Part'Range loop
               if Self.Fields (F).Variant_Part (V).Data = Current.Data then
                  Self.Fields (F).Variant_Part (V) :=
                    GVD_Type_Holder (Replace_With);
                  return Replace_With;
               end if;
            end loop;
         end if;
      end loop;

      return Empty_GVD_Type_Holder;
   end Replace;

   --------------------
   -- Set_Field_Name --
   --------------------

   procedure Set_Field_Name
     (Self          : not null access GVD_Record_Type;
      Index         : Positive;
      Name          : String;
      Variant_Parts : Natural := 0) is
   begin
      if Self.Fields (Index).Value.Data /= null then
         Self.Fields (Index).Value := Empty_GVD_Type_Holder;
      end if;

      if Self.Fields (Index).Variant_Part /= null then
         Free (Self.Fields (Index).Variant_Part);
      end if;

      Self.Fields (Index).Name := Null_Unbounded_String;

      if Variant_Parts = 0 then
         Self.Fields (Index) := Record_Field'
           (Name         => To_Unbounded_String (Name),
            Value        => Empty_GVD_Type_Holder,
            Variant_Part => null);

      else
         Self.Fields (Index) := Record_Field'
           (Name         => To_Unbounded_String (Name),
            Value        => Empty_GVD_Type_Holder,
            Variant_Part => new Record_Type_Array (1 .. Variant_Parts));
      end if;
   end Set_Field_Name;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Self  : not null access GVD_Record_Type;
      Value : GVD_Type_Holder;
      Field : Positive) is
   begin
      Self.Fields (Field).Value := Value;
      Self.Fields (Field).Value.Data.Instance.Valid := True;
      --  If there is at least one field, the record is valid.
      Self.Valid := True;
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Self  : not null access GVD_Record_Type;
      Value : GVD_Type_Holder;
      Field : String) is
   begin
      for J in Self.Fields'Range loop
         if Self.Fields (J).Name = Field then
            Self.Fields (J).Value := Value;
            Self.Fields (J).Value.Data.Instance.Valid := True;
         end if;
      end loop;

      --  If there is at least one field, the record is valid.
      Self.Valid := True;
   end Set_Value;

   -----------------------
   -- Set_Variant_Field --
   -----------------------

   procedure Set_Variant_Field
     (Self          : not null access GVD_Record_Type;
      Index         : Positive;
      Variant_Index : Positive;
      Value         : GVD_Type_Holder) is
   begin
      if Self.Fields (Index).Variant_Part /= null then
         if Self.Fields (Index).Variant_Part (Variant_Index).Data /= null then
            Self.Fields (Index).Variant_Part (Variant_Index) :=
              Empty_GVD_Type_Holder;
         end if;

         Self.Fields (Index).Variant_Part (Variant_Index) := Value;

         --  If there is at least one field, the record is valid.
         Self.Valid := True;
      end if;
   end Set_Variant_Field;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Self : not null access GVD_Record_Type) return Generic_Iterator'Class
   is
      Iter : Record_Iterator;
   begin
      Iter.Item := GVD_Record_Type_Access (Self);
      Iter.Field := Self.Fields'First;
      Iter.Variant := Natural'Last;

      if Iter.Field <= Iter.Item.Fields'Last
        and then Iter.Item.Fields (Iter.Field).Variant_Part /= null
      then
         Iter.Variant := Iter.Item.Fields (Iter.Field).Variant_Part'First;
      end if;

      return Iter;
   end Start;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Record_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean is
   begin
      if Item.Data = null
        or else Item.Data.Instance = null
        or else not (Item.Data.Instance.all in GVD_Record_Type'Class)
        or else Self.Num_Fields /=
          GVD_Record_Type_Access (Item.Get_Type).Num_Fields
      then
         return False;
      end if;

      for F in Self.Fields'Range loop
         if (Self.Fields (F).Variant_Part /= null
             and then GVD_Record_Type_Access
                        (Item.Get_Type).Fields (F).Variant_Part = null)
           or else
           (Self.Fields (F).Variant_Part = null
            and then GVD_Record_Type_Access
                       (Item.Get_Type).Fields (F).Variant_Part /= null)
         then
            return False;
         end if;

         --  Protect against the null Name, which happens for variant parts
         if Self.Fields (F).Value.Data /= null
           and then GVD_Record_Type_Access
             (Item.Get_Type).Fields (F).Value.Data /= null
           and then not Self.Fields (F).Value.Get_Type.Structurally_Equivalent
           (GVD_Record_Type_Access (Item.Get_Type).Fields (F).Value)
         then
            return False;
         end if;

         if Self.Fields (F).Variant_Part /= null then
            if Self.Fields (F).Variant_Part'Length /=
              GVD_Record_Type_Access
                (Item.Get_Type).Fields (F).Variant_Part'Length
            then
               return False;
            end if;

            for V in Self.Fields (F).Variant_Part'Range loop
               if not Self.Fields (F).Variant_Part
                 (V).Get_Type.Structurally_Equivalent
                 (GVD_Record_Type_Access
                    (Item.Get_Type).Fields (F).Variant_Part (V))
               then
                  return False;
               end if;
            end loop;
         end if;
      end loop;
      return True;
   end Structurally_Equivalent;

end GVD.Variables.Types.Records;
