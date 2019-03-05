------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

with Ada.Tags;                    use Ada.Tags;
with Glib;                        use Glib;
with GVD.Variables.Types.Repeats; use GVD.Variables.Types.Repeats;

package body GVD.Variables.Types.Arrays is

   type Array_Iterator is new Generic_Iterator with record
      Item  : GVD_Array_Type_Access;
      Child : Natural;
   end record;
   overriding procedure Next (Iter : in out Array_Iterator);
   overriding function At_End (Iter : Array_Iterator) return Boolean;
   overriding function Data
     (Iter : Array_Iterator)
      return GVD_Type_Holder'Class;
   overriding function Field_Name
     (Iter : Array_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String;

   function Index_String
     (Item    : GVD_Array_Type'Class;
      Index   : Long_Integer;
      Dim_Num : Positive) return String;
   --  Return the string indicating the coordinates in the array, for the
   --  element at Index.

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Array_Iterator) return Boolean is
   begin
      return Iter.Item.Values.Is_Empty
        or else Iter.Child > Positive (Iter.Item.Values.Length);
   end At_End;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear
     (Self : not null access GVD_Array_Type) is
   begin
      if not Self.Values.Is_Empty then
         --  Free the whole memory for the items, since the type is in fact
         --  stored in a separate field.
         Self.Values.Clear;
      end if;

      Self.Item_Type.Get_Type.Clear;
   end Clear;

   -----------
   -- Clone --
   -----------

   overriding procedure Clone
     (Self : not null access GVD_Array_Type;
      Item : not null GVD_Generic_Type_Access)
   is
      Src : constant GVD_Array_Type_Access := GVD_Array_Type_Access (Item);
   begin
      GVD_Generic_Type (Self.all).Clone (Item);

      --  ??? Should duplicate the values as well....
      Self.Values    := Array_Item_Vectors.Empty_Vector;
      Self.Item_Type := Src.Item_Type.Clone;
   end Clone;

   ----------
   -- Data --
   ----------

   overriding function Data
     (Iter : Array_Iterator) return GVD_Type_Holder'Class is
   begin
      return Iter.Item.Values (Iter.Child).Value;
   end Data;

   ----------------
   -- Field_Name --
   ----------------

   overriding function Field_Name
     (Iter : Array_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String
   is
      Idx : constant String := Index_String
        (Iter.Item.all,
         Iter.Item.Values (Iter.Child).Index,
         Iter.Item.Num_Dimensions);
   begin
      return Lang.Array_Item_Name (Name  => Base, Index => Idx);
   end Field_Name;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Self : not null access GVD_Array_Type) is
   begin
      if not Self.Values.Is_Empty then
         --  Free the whole memory for the items, since the type is in fact
         --  stored in a separate field.
         Self.Values.Clear;
      end if;

      Self.Item_Type := Empty_GVD_Type_Holder;
      GVD_Generic_Type (Self.all).Free;
   end Free;

   --------------------
   -- Get_Dimensions --
   --------------------

   function Get_Dimensions
     (Self : not null access GVD_Array_Type;
      Dim  : Positive)
      return Dimension is
   begin
      return Self.Dimensions (Dim);
   end Get_Dimensions;

   -------------------
   -- Get_Item_Type --
   -------------------

   function Get_Item_Type
     (Self : not null access GVD_Array_Type) return GVD_Type_Holder is
   begin
      return Self.Item_Type;
   end Get_Item_Type;

   --------------------
   -- Get_Type_Descr --
   --------------------

   overriding function Get_Type_Descr
     (Self : not null access GVD_Array_Type) return String
   is
      Result : Unbounded_String := To_Unbounded_String ("Array (");
   begin
      for J in 1 .. Self.Num_Dimensions loop
         Append
           (Result,
            Self.Dimensions (J).First'Img & " .. "
            & Self.Dimensions (J).Last'Img);
         if J /= Self.Num_Dimensions then
            Append (Result, ", ");
         end if;
      end loop;

      Append (Result, ")");
      return To_String (Result);
   end Get_Type_Descr;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Self       : not null access GVD_Array_Type;
      Elem_Index : Long_Integer) return GVD_Type_Holder
   is
      Return_Type : GVD_Type_Holder;
   begin
      if Self.Values.Is_Empty then
         return Empty_GVD_Type_Holder;
      end if;

      for J in 1 .. Self.Last_Value loop
         if Self.Values (J).Value.Data /= null
           and then Self.Values (J).Value.Get_Type'Tag = GVD_Repeat_Type'Tag
           and then Self.Values (J).Index <= Elem_Index
           and then Self.Values (J).Index
           + Long_Integer
           (GVD_Repeat_Type_Access
              (Self.Values (J).Value.Get_Type).Get_Repeat_Num) > Elem_Index
         then
            return GVD_Repeat_Type_Access
              (Self.Values (J).Value.Get_Type).Get_Value.Clone;

         elsif Self.Values (J).Index = Elem_Index then
            declare
               I : Array_Item := Self.Values (J);
            begin
               Return_Type := I.Value;
               I.Value := Empty_GVD_Type_Holder;
               Self.Values (J) := I;
               return Return_Type;
            end;
         end if;
      end loop;

      return Empty_GVD_Type_Holder;
   end Get_Value;

   ------------------
   -- Index_String --
   ------------------

   function Index_String
     (Item    : GVD_Array_Type'Class;
      Index   : Long_Integer;
      Dim_Num : Positive) return String
   is
      Length : constant Long_Integer :=
        (if Item.Dimensions (Dim_Num).Last = Long_Integer'First or
             Item.Dimensions (Dim_Num).First = Long_Integer'Last
         then
            0 --  if we found special values for dynamic bounds
         else
            Item.Dimensions (Dim_Num).Last -
             Item.Dimensions (Dim_Num).First + 1);
   begin
      --  Do we have an array with no element ?

      if Length <= 0 then
         if Item.Num_Dimensions = 1 then
            --  Special case for one dimensional arrays, since these are often
            --  strings whose length was not known when parsing the type info.

            return Long_Integer'Image (Item.Dimensions (Dim_Num).First);
         else
            --  As an approximation, return Index itself

            return Long_Integer'Image (Index);
         end if;

      else
         declare
            Dim : constant String := Long_Integer'Image
              (Index mod Length + Item.Dimensions (Dim_Num).First);
         begin
            if Dim_Num /= 1 then
               if Dim (Dim'First) = '-' then
                  return Index_String (Item, Index / Length, Dim_Num - 1)
                    & "," & Dim (Dim'First .. Dim'Last);
               else
                  return Index_String (Item, Index / Length, Dim_Num - 1)
                    & "," & Dim (Dim'First + 1 .. Dim'Last);
               end if;
            else
               if Dim (Dim'First) = '-' then
                  return Dim (Dim'First .. Dim'Last);
               else
                  return Dim (Dim'First + 1 .. Dim'Last);
               end if;
            end if;
         end;
      end if;
   end Index_String;

   --------------------
   -- New_Array_Type --
   --------------------

   function New_Array_Type
     (Num_Dimensions : Positive)
      return GVD_Type_Holder
   is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_Array_Type (Num_Dimensions => Num_Dimensions));
   begin
      return GVD_Type_Holder'(Ada.Finalization.Controlled with Data);
   end New_Array_Type;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Array_Iterator) is
   begin
      Iter.Child := Iter.Child + 1;
   end Next;

   --------------------
   -- Num_Dimensions --
   --------------------

   function Num_Dimensions
     (Self : not null access GVD_Array_Type) return Positive is
   begin
      return Self.Num_Dimensions;
   end Num_Dimensions;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Self         : not null access GVD_Array_Type;
      Current      : GVD_Type_Holder'Class;
      Replace_With : GVD_Type_Holder'Class)
      return GVD_Type_Holder'Class is
   begin
      --  Since all values should be replaced, do nothing if there is any
      --  value defined.

      if not Self.Values.Is_Empty then
         return Empty_GVD_Type_Holder;
      end if;

      --  Only the Item_Type can be substituted

      if Self.Item_Type.Data /= Current.Data then
         return Empty_GVD_Type_Holder;
      end if;

      Self.Item_Type := GVD_Type_Holder (Replace_With);
      return Replace_With;
   end Replace;

   --------------------
   -- Set_Dimensions --
   --------------------

   procedure Set_Dimensions
     (Self : not null access GVD_Array_Type;
      Dim  : Positive;
      Size : Dimension) is
   begin
      Self.Dimensions (Dim) := Size;
   end Set_Dimensions;

   -------------------
   -- Set_Item_Type --
   -------------------

   procedure Set_Item_Type
     (Self     : not null access GVD_Array_Type;
      The_Type : GVD_Type_Holder) is
   begin
      Self.Item_Type := The_Type;
   end Set_Item_Type;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Self       : not null access GVD_Array_Type;
      Elem_Value : GVD_Type_Holder;
      Elem_Index : Long_Integer;
      Repeat_Num : Positive := 1)
   is
      use Array_Item_Vectors;
      use type Ada.Containers.Count_Type;

      Tmp       : Vector;
      To_Insert : GVD_Type_Holder;
      Index     : Positive;
      I         : Array_Item;
   begin
      --  Create the real new value (ie including the Repeat_Num)

      if Repeat_Num = 1 then
         To_Insert := Elem_Value;
         To_Insert.Get_Type.Valid := True;
      else
         To_Insert := New_Repeat_Type;
         GVD_Repeat_Type_Access (To_Insert.Get_Type).Set_Value (Elem_Value);
         GVD_Repeat_Type_Access (To_Insert.Get_Type).Set_Repeat_Num
           (Repeat_Num);
      end if;

      --  If we are inserting a range, delete all old items that are covered
      --  by this new range.
      --  This means we have to split up existing ranges (possibly transforming
      --  them to non-repeat values if the repeat_num becomes 1), and delete
      --  all simple values that are covered by the new range.
      --  We also need to keep the values sorted in Item.Values

      if Repeat_Num > 1
        and then not Self.Values.Is_Empty
        and then Elem_Index <= Self.Values (Self.Last_Value).Index
      then
         declare
            Min   : constant Long_Integer := Elem_Index;
            Max   : constant Long_Integer :=
              Long_Integer (Repeat_Num) + Min - 1;
            Min2, Max2 : Long_Integer;
            --  Since the range can be split into two parts, keep enough space.
            Tmp      : Vector := To_Vector (Self.Values.Length * 2);
            Save     : Vector := Self.Values;
            Index    : Positive := 1;

         begin
            for J in 1 .. Self.Last_Value loop

               --  If we have an old repeat type, we might have to split it.

               if Self.Values (J).Value.Data /= null
                 and then Self.Values (J).Value.Get_Type.all in
                 GVD_Repeat_Type'Class
               then
                  Min2 := Self.Values (J).Index;
                  Max2 := Min2 - 1 + Long_Integer
                    (GVD_Repeat_Type_Access
                       (Self.Values (J).Value.Get_Type).Get_Repeat_Num);

                  --  Old one completly inside the new one => delete it
                  --      |---- new ---------|
                  --         |---- old --|

                  if Min2 >= Min and then Max2 <= Max then
                     null;

                  --  New one completly inside the old one => Split it
                  --      |----- new --------|
                  --   |--------- old ------------|

                  elsif Min2 < Min and then Max2 > Max then
                     Tmp (Index).Index := Self.Values (J).Index;
                     if Min - Min2 > 1 then
                        I := Tmp (Index);
                        I.Value := Self.Values (J).Value.Clone;
                        Tmp (Index) := I;

                        GVD_Repeat_Type_Access
                          (Tmp (Index).Value.Get_Type).Set_Repeat_Num
                            (Integer (Min - Min2));
                     else
                        I := Tmp (Index);
                        I.Value := GVD_Repeat_Type_Access
                          (Self.Values (J).Value.Get_Type).Get_Value.Clone;
                        Tmp (Index) := I;
                     end if;
                     Index := Index + 1;

                     Tmp (Index).Index := Max + 1;
                     if Max2 - Max - 1 > 1 then
                        I := Tmp (Index);
                        I.Value := Self.Values (J).Value;
                        Tmp (Index) := I;

                        GVD_Repeat_Type_Access
                          (Tmp (Index).Value.Get_Type).Set_Repeat_Num
                            (Integer (Max2 - Max - 1));
                     else
                        I := Tmp (Index);
                        I.Value := GVD_Repeat_Type_Access
                          (Self.Values (J).Value.Get_Type).Get_Value;
                        Tmp (Index) := I;

                        GVD_Repeat_Type_Access
                          (Self.Values (J).Value.Get_Type).Set_Value
                            (Empty_GVD_Type_Holder);

                        I := Self.Values (J);
                        I.Value := Empty_GVD_Type_Holder;
                        Self.Values (J) := I;
                     end if;
                     Index := Index + 1;

                  --  Old one on the "left" of the old one => Split it
                  --       |------- new ---------|
                  --    |-------- old -------|

                  elsif Min2 < Min and then Max2 >= Min then
                     Tmp (Index).Index := Self.Values (J).Index;
                     if Min - Min2 > 1 then
                        I := Tmp (Index);
                        I.Value := Self.Values (J).Value;
                        Tmp (Index) := I;

                        GVD_Repeat_Type_Access
                          (Tmp (Index).Value.Get_Type).Set_Repeat_Num
                            (Integer (Min - Min2));
                     else
                        I := Tmp (Index);
                        I.Value := GVD_Repeat_Type_Access
                          (Self.Values (J).Value.Get_Type).Get_Value;
                        Tmp (Index) := I;

                        GVD_Repeat_Type_Access
                          (Self.Values (J).Value.Get_Type).Set_Value
                            (Empty_GVD_Type_Holder);

                        I := Self.Values (J);
                        I.Value := Empty_GVD_Type_Holder;
                        Self.Values (J) := I;
                     end if;
                     Index := Index + 1;

                  --  Old one on the "right" ofthe old one => Split it
                  --       |------- new ---------|
                  --             |-------- old -------|

                  elsif Min2 <= Max and then Max2 > Max then
                     Tmp (Index).Index := Max + 1;
                     if Max2 - Max - 1 > 1 then
                        I := Tmp (Index);
                        I.Value := Self.Values (J).Value;
                        Tmp (Index) := I;
                        GVD_Repeat_Type_Access
                          (Tmp (Index).Value.Get_Type).Set_Repeat_Num
                            (Integer (Max2 - Max - 1));
                     else
                        I := Tmp (Index);
                        I.Value := GVD_Repeat_Type_Access
                          (Self.Values (J).Value.Get_Type).Get_Value;
                        Tmp (Index) := I;

                        GVD_Repeat_Type_Access
                          (Self.Values (J).Value.Get_Type).Set_Value
                            (Empty_GVD_Type_Holder);

                        I := Self.Values (J);
                        I.Value := Empty_GVD_Type_Holder;
                        Self.Values (J) := I;
                     end if;
                     Index := Index + 1;

                  --  No intersection between the two ranges => Keep it

                  else
                     Tmp (Index) := Self.Values (J);
                     Index := Index + 1;
                  end if;

               --  Not a repeat type

               elsif Self.Values (J).Index < Min
                 or else Self.Values (J).Index > Max
               then
                  Tmp (Index) := Self.Values (J);
                  Index := Index + 1;
               end if;
            end loop;

            if Index = 1 then
               --  Will be reallocated below
               Self.Values := Empty_Vector;
               Self.Last_Value := 0;
            else
               for J in 1 .. Index - 1 loop
                  Self.Values (J) := Tmp (J);
               end loop;
               Self.Last_Value := Index - 1;
            end if;
            Save.Clear;
         end;
      end if;

      --  Check whether we already have an element with the same Elem_Index.
      --  If yes, reuse it.

      if not Self.Values.Is_Empty then
         for J in 1 .. Self.Last_Value loop

            --  Do we have a range that contains the index ?
            --  If yes, we have to split the range. This wouldn't be necessary
            --  since gdb itself provides the remaining values. However, we
            --  need to do this so as to preserve the old value in case we
            --  want to highlight the ones that changed.

            if Self.Values (J).Index <= Elem_Index
              and then Self.Values (J).Value.Data /= null
              and then Self.Values (J).Value.Data.Instance.all in
              GVD_Repeat_Type'Class
              and then Elem_Index < Self.Values (J).Index
              + Long_Integer
              (GVD_Repeat_Type_Access
                 (Self.Values (J).Value.Get_Type).Get_Repeat_Num)
            then
               declare
                  Repeat_Num      : constant Integer := GVD_Repeat_Type_Access
                    (Self.Values (J).Value.Get_Type).Get_Repeat_Num;
                  Range_Index     : constant Long_Integer :=
                    Self.Values (J).Index;
                  Tmp             : GVD_Type_Holder;

               begin
                  Tmp := GVD_Repeat_Type_Access
                    (Self.Values (J).Value.Get_Type).Get_Value;
                  if Elem_Index - Range_Index >= 1 then
                     GVD_Repeat_Type_Access
                       (Self.Values (J).Value.Get_Type).Set_Repeat_Num
                         (Positive (Elem_Index - Range_Index));
                     Self.Set_Value
                       (Elem_Value => Elem_Value,
                        Elem_Index => Elem_Index,
                           Repeat_Num => 1);

                  else
                     GVD_Repeat_Type_Access
                       (Self.Values (J).Value.Get_Type).Set_Value
                         (Empty_GVD_Type_Holder);
                     I := Self.Values (J);
                     I.Value := Elem_Value;
                     Self.Values (J) := I;
                  end if;

                  if Integer (Range_Index - Elem_Index) +
                    Repeat_Num - 1 > 0
                  then
                     Self.Set_Value
                       (Elem_Value => Tmp.Clone,
                        Elem_Index => Elem_Index + 1,
                        Repeat_Num => Integer (Range_Index - Elem_Index)
                        + Repeat_Num - 1);
                  end if;
               end;

               --  Nothing else to do, this has been done recursively.
               return;

            elsif Self.Values (J).Index = Elem_Index then
               if Self.Values (J).Value.Data /= null then
                  I := Self.Values (J);
                  I.Value := Empty_GVD_Type_Holder;
                  Self.Values (J) := I;
               end if;

               I := Self.Values (J);
               I.Value := To_Insert;
               Self.Values (J) := I;
               return;
            end if;
         end loop;
      end if;

      --  Reserve enough space to insert the new array.

      if Self.Values.Is_Empty then
         Self.Values := To_Vector (100);
         Self.Last_Value := 1;

      else
         Self.Last_Value := Self.Last_Value + 1;
      end if;

      if Self.Last_Value > Positive (Self.Values.Length) then
         Tmp := Self.Values;
         Self.Values := To_Vector (2 * Tmp.Length);
         for J in 1 .. Positive (Tmp.Length) loop
            Self.Values (J) := Tmp (J);
         end loop;
         Tmp.Clear;
      end if;

      --  Insert the item, but make sure that the Values array is still sorted.

      Index := 1;

      while Index < Self.Last_Value
        and then Self.Values (Index).Index < Elem_Index
      loop
         Index := Index + 1;
      end loop;

      if Index < Self.Last_Value then
         for J in reverse Index .. Self.Last_Value - 1 loop
            Self.Values (J + 1) := Self.Values (J);
         end loop;

         Self.Values (Index) :=
           Array_Item'(Index => Elem_Index, Value => To_Insert);

      else
         Self.Values (Self.Last_Value) :=
           Array_Item'(Index => Elem_Index, Value => To_Insert);
      end if;
   end Set_Value;

   -------------------
   -- Shrink_Values --
   -------------------

   procedure Shrink_Values (Self : not null access GVD_Array_Type) is
      Tmp : Array_Item_Vectors.Vector;
   begin
      Tmp := Self.Values;
      Self.Values := Array_Item_Vectors.To_Vector
        (Ada.Containers.Count_Type (Self.Last_Value));

      if Self.Last_Value > 0 then
         for J in 1 .. Self.Last_Value loop
            Self.Values (J) := Tmp (J);
         end loop;
      end if;
      Tmp.Clear;
   end Shrink_Values;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Self : not null access GVD_Array_Type) return Generic_Iterator'Class
   is
      Iter : Array_Iterator;
   begin
      Iter.Item := GVD_Array_Type_Access (Self);

      if not Self.Values.Is_Empty then
         Iter.Child := 1;
      end if;

      return Iter;
   end Start;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Array_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean is
   begin
      return Item.Data /= null
        and then Item.Data.Instance /= null
        and then Item.Get_Type.all in GVD_Array_Type'Class
        and then Self.Dimensions = GVD_Array_Type_Access
          (Item.Get_Type).Dimensions
        and then Self.Item_Type.Get_Type.Structurally_Equivalent
          (GVD_Array_Type_Access (Item.Get_Type).Item_Type);
   end Structurally_Equivalent;

end GVD.Variables.Types.Arrays;
