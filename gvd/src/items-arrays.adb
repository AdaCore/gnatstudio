------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Tags;              use Ada.Tags;
with Glib;                  use Glib;
with Items.Repeats;         use Items.Repeats;
with Language;              use Language;

package body Items.Arrays is

   type Array_Iterator is new Generic_Iterator with record
      Item  : Array_Type_Access;
      Child : Natural;
   end record;
   overriding procedure Next (Iter : in out Array_Iterator);
   overriding function At_End (Iter : Array_Iterator) return Boolean;
   overriding function Data (Iter : Array_Iterator) return Generic_Type_Access;
   overriding function Field_Name
     (Iter : Array_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String;

   function Index_String
     (Item    : Array_Type'Class;
      Index   : Long_Integer;
      Dim_Num : Positive) return String;
   --  Return the string indicating the coordinates in the array, for the
   --  element at Index.

   ------------------
   -- Index_String --
   ------------------

   function Index_String
     (Item    : Array_Type'Class;
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
     (Num_Dimensions : Positive) return Generic_Type_Access is
   begin
      return new Array_Type (Num_Dimensions => Num_Dimensions);
   end New_Array_Type;

   --------------------
   -- Set_Dimensions --
   --------------------

   procedure Set_Dimensions
     (Item : in out Array_Type;
      Dim  : Positive;
      Size : Dimension) is
   begin
      Item.Dimensions (Dim) := Size;
   end Set_Dimensions;

   --------------------
   -- Num_Dimensions --
   --------------------

   function Num_Dimensions (Item : Array_Type) return Positive is
   begin
      return Item.Num_Dimensions;
   end Num_Dimensions;

   --------------------
   -- Get_Dimensions --
   --------------------

   function Get_Dimensions
     (Item : Array_Type;
      Dim  : Positive) return Dimension is
   begin
      return Item.Dimensions (Dim);
   end Get_Dimensions;

   -------------------
   -- Set_Item_Type --
   -------------------

   procedure Set_Item_Type
     (Item     : in out Array_Type;
      The_Type : access Generic_Type'Class) is
   begin
      Item.Item_Type := Generic_Type_Access (The_Type);
   end Set_Item_Type;

   -------------------
   -- Get_Item_Type --
   -------------------

   function Get_Item_Type
     (Item : Array_Type) return Generic_Type_Access is
   begin
      return Item.Item_Type;
   end Get_Item_Type;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Item  : in out Array_Type;
      Elem_Value : access Generic_Type'Class;
      Elem_Index : Long_Integer;
      Repeat_Num : Positive := 1)
   is
      use Array_Item_Vectors;
      use type Ada.Containers.Count_Type;

      Tmp       : Vector;
      To_Insert : Generic_Type_Access;
      Index     : Positive;
   begin
      --  Create the real new value (ie including the Repeat_Num)

      if Repeat_Num = 1 then
         To_Insert := Generic_Type_Access (Elem_Value);
         To_Insert.Valid := True;
      else
         To_Insert := New_Repeat_Type;
         Set_Value
           (Repeat_Type (To_Insert.all),
            Generic_Type_Access (Elem_Value));
         Set_Repeat_Num (Repeat_Type_Access (To_Insert).all, Repeat_Num);
      end if;

      --  If we are inserting a range, delete all old items that are covered
      --  by this new range.
      --  This means we have to split up existing ranges (possibly transforming
      --  them to non-repeat values if the repeat_num becomes 1), and delete
      --  all simple values that are covered by the new range.
      --  We also need to keep the values sorted in Item.Values

      if Repeat_Num > 1
        and then not Item.Values.Is_Empty
        and then Elem_Index <= Item.Values (Item.Last_Value).Index
      then
         declare
            Min   : constant Long_Integer := Elem_Index;
            Max   : constant Long_Integer :=
              Long_Integer (Repeat_Num) + Min - 1;
            Min2, Max2 : Long_Integer;
            --  Since the range can be split into two parts, keep enough space.
            Tmp      : Vector := To_Vector (Item.Values.Length * 2);
            Save     : Vector := Item.Values;
            Index    : Positive := 1;

         begin
            for J in 1 .. Item.Last_Value loop

               --  If we have an old repeat type, we might have to split it.

               if Item.Values (J).Value /= null
                 and then Item.Values (J).Value.all in Repeat_Type'Class
               then
                  Min2 := Item.Values (J).Index;
                  Max2 := Min2 - 1 + Long_Integer
                    (Get_Repeat_Num (Repeat_Type_Access
                                     (Item.Values (J).Value)));

                  --  Old one completly inside the new one => delete it
                  --      |---- new ---------|
                  --         |---- old --|

                  if Min2 >= Min and then Max2 <= Max then
                     null;

                  --  New one completly inside the old one => Split it
                  --      |----- new --------|
                  --   |--------- old ------------|

                  elsif Min2 < Min and then Max2 > Max then
                     Tmp (Index).Index := Item.Values (J).Index;
                     if Min - Min2 > 1 then
                        Tmp (Index).Value := Clone (Item.Values (J).Value.all);
                        Set_Repeat_Num (Repeat_Type (Tmp (Index).Value.all),
                                        Integer (Min - Min2));
                     else
                        Tmp (Index).Value := Clone
                          (Get_Value (Repeat_Type
                                      (Item.Values (J).Value.all)).all);
                     end if;
                     Index := Index + 1;

                     Tmp (Index).Index := Max + 1;
                     if Max2 - Max - 1 > 1 then
                        Tmp (Index).Value := Item.Values (J).Value;
                        Set_Repeat_Num (Repeat_Type (Tmp (Index).Value.all),
                                        Integer (Max2 - Max - 1));
                     else
                        Tmp (Index).Value := Get_Value
                          (Repeat_Type (Item.Values (J).Value.all));
                        Set_Value
                          (Repeat_Type (Item.Values (J).Value.all), null);
                        Free (Item.Values (J).Value, Only_Value => False);
                     end if;
                     Index := Index + 1;

                  --  Old one on the "left" of the old one => Split it
                  --       |------- new ---------|
                  --    |-------- old -------|

                  elsif Min2 < Min and then Max2 >= Min then
                     Tmp (Index).Index := Item.Values (J).Index;
                     if Min - Min2 > 1 then
                        Tmp (Index).Value := Item.Values (J).Value;
                        Set_Repeat_Num (Repeat_Type (Tmp (Index).Value.all),
                                        Integer (Min - Min2));
                     else
                        Tmp (Index).Value :=
                          Get_Value (Repeat_Type (Item.Values (J).Value.all));
                        Set_Value
                          (Repeat_Type (Item.Values (J).Value.all), null);
                        Free (Item.Values (J).Value, Only_Value => False);
                     end if;
                     Index := Index + 1;

                  --  Old one on the "right" ofthe old one => Split it
                  --       |------- new ---------|
                  --             |-------- old -------|

                  elsif Min2 <= Max and then Max2 > Max then
                     Tmp (Index).Index := Max + 1;
                     if Max2 - Max - 1 > 1 then
                        Tmp (Index).Value := Item.Values (J).Value;
                        Set_Repeat_Num (Repeat_Type (Tmp (Index).Value.all),
                          Integer (Max2 - Max - 1));
                     else
                        Tmp (Index).Value :=
                          Get_Value (Repeat_Type (Item.Values (J).Value.all));
                        Set_Value
                          (Repeat_Type (Item.Values (J).Value.all), null);
                        Free (Item.Values (J).Value, Only_Value => False);
                     end if;
                     Index := Index + 1;

                  --  No intersection between the two ranges => Keep it

                  else
                     Tmp (Index) := Item.Values (J);
                     Index := Index + 1;
                  end if;

               --  Not a repeat type

               elsif Item.Values (J).Index < Min
                 or else Item.Values (J).Index > Max
               then
                  Tmp (Index) := Item.Values (J);
                  Index := Index + 1;
               end if;
            end loop;

            if Index = 1 then
               --  Will be reallocated below
               Item.Values := Empty_Vector;
               Item.Last_Value := 0;
            else
               for J in 1 .. Index - 1 loop
                  Item.Values (J) := Tmp (J);
               end loop;
               Item.Last_Value := Index - 1;
            end if;
            Save.Clear;
         end;
      end if;

      --  Check whether we already have an element with the same Elem_Index.
      --  If yes, reuse it.

      if not Item.Values.Is_Empty then
         for J in 1 .. Item.Last_Value loop

            --  Do we have a range that contains the index ?
            --  If yes, we have to split the range. This wouldn't be necessary
            --  since gdb itself provides the remaining values. However, we
            --  need to do this so as to preserve the old value in case we
            --  want to highlight the ones that changed.

            if Item.Values (J).Index <= Elem_Index
              and then Item.Values (J).Value /= null
              and then Item.Values (J).Value.all in Repeat_Type'Class
              and then Elem_Index < Item.Values (J).Index
              + Long_Integer
              (Get_Repeat_Num (Repeat_Type_Access (Item.Values (J).Value)))
            then
               declare
                  Repeat_Num      : constant Integer := Get_Repeat_Num
                    (Repeat_Type_Access (Item.Values (J).Value));
                  Range_Index     : constant Long_Integer :=
                    Item.Values (J).Index;
                  Tmp             : Generic_Type_Access;

               begin
                  Tmp := Get_Value (Repeat_Type (Item.Values (J).Value.all));
                  if Elem_Index - Range_Index >= 1 then
                     Set_Repeat_Num
                       (Repeat_Type (Item.Values (J).Value.all),
                        Positive (Elem_Index - Range_Index));
                     Set_Value
                       (Item,
                        Elem_Value => Elem_Value,
                        Elem_Index => Elem_Index,
                           Repeat_Num => 1);

                  else
                     Set_Value (Repeat_Type (Item.Values (J).Value.all), null);
                     Free (Item.Values (J).Value, Only_Value => False);
                     Item.Values (J).Value := Generic_Type_Access (Elem_Value);
                  end if;

                  if Integer (Range_Index - Elem_Index) +
                    Repeat_Num - 1 > 0
                  then
                     Set_Value
                       (Item,
                        Elem_Value => Clone (Tmp.all),
                        Elem_Index => Elem_Index + 1,
                        Repeat_Num => Integer (Range_Index - Elem_Index)
                           + Repeat_Num - 1);
                  end if;
               end;

               --  Nothing else to do, this has been done recursively.
               return;

            elsif Item.Values (J).Index = Elem_Index then
               if Item.Values (J).Value /= null then
                  Free (Item.Values (J).Value, Only_Value => False);
               end if;

               Item.Values (J).Value := To_Insert;
               return;
            end if;
         end loop;
      end if;

      --  Reserve enough space to insert the new array.

      if Item.Values.Is_Empty then
         Item.Values := To_Vector (100);
         Item.Last_Value := 1;

      else
         Item.Last_Value := Item.Last_Value + 1;
      end if;

      if Item.Last_Value > Positive (Item.Values.Length) then
         Tmp := Item.Values;
         Item.Values := To_Vector (2 * Tmp.Length);
         for J in 1 .. Positive (Tmp.Length) loop
            Item.Values (J) := Tmp (J);
         end loop;
         Tmp.Clear;
      end if;

      --  Insert the item, but make sure that the Values array is still sorted.

      Index := 1;

      while Index < Item.Last_Value
        and then Item.Values (Index).Index < Elem_Index
      loop
         Index := Index + 1;
      end loop;

      if Index < Item.Last_Value then
         for J in reverse Index .. Item.Last_Value - 1 loop
            Item.Values (J + 1) := Item.Values (J);
         end loop;

         Item.Values (Index) :=
           Array_Item'(Index => Elem_Index, Value => To_Insert);

      else
         Item.Values (Item.Last_Value) :=
           Array_Item'(Index => Elem_Index, Value => To_Insert);
      end if;
   end Set_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Item       : in out Array_Type;
      Elem_Index : Long_Integer) return Generic_Type_Access
   is
      Return_Type : Generic_Type_Access;
   begin
      if Item.Values.Is_Empty then
         return null;
      end if;

      for J in 1 .. Item.Last_Value loop
         if Item.Values (J).Value /= null
           and then Item.Values (J).Value'Tag = Repeat_Type'Tag
           and then Item.Values (J).Index <= Elem_Index
           and then Item.Values (J).Index
           + Long_Integer (Get_Repeat_Num (Repeat_Type_Access
                           (Item.Values (J).Value))) > Elem_Index
         then
            return Clone
              (Get_Value (Repeat_Type (Item.Values (J).Value.all)).all);

         elsif Item.Values (J).Index = Elem_Index then
            Return_Type := Item.Values (J).Value;
            Item.Values (J).Value := null;
            return Return_Type;
         end if;
      end loop;

      return null;
   end Get_Value;

   -------------------
   -- Shrink_Values --
   -------------------

   procedure Shrink_Values (Item : in out Array_Type) is
      Tmp : Array_Item_Vectors.Vector;
   begin
      Tmp := Item.Values;
      Item.Values := Array_Item_Vectors.To_Vector
        (Ada.Containers.Count_Type (Item.Last_Value));

      if Item.Last_Value > 0 then
         for J in 1 .. Item.Last_Value loop
            Item.Values (J) := Tmp (J);
         end loop;
      end if;
      Tmp.Clear;
   end Shrink_Values;

   --------------------
   -- Get_Type_Descr --
   --------------------

   overriding function Get_Type_Descr
     (Self    : not null access Array_Type) return String
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
     (Item : access Array_Type; Only_Value : Boolean := False) is
   begin
      if not Item.Values.Is_Empty then
         --  Free the whole memory for the items, since the type is in fact
         --  stored in a separate field.
         for J in 1 .. Item.Last_Value loop
            Free (Item.Values (J).Value, Only_Value => False);
         end loop;
         Item.Values.Clear;
      end if;

      if not Only_Value then
         Free (Item.Item_Type, Only_Value);
      end if;

      Free (Generic_Type (Item.all)'Access, Only_Value);
   end Free;

   -----------------------
   -- Clone_Dispatching --
   -----------------------

   overriding procedure Clone_Dispatching
     (Item  : Array_Type;
      Clone : in out Generic_Type_Access)
   is
      R : Array_Type_Access;
   begin
      Clone_Dispatching (Generic_Type (Item), Clone);
      R := Array_Type_Access (Clone);

      --  ??? Should duplicate the values as well....
      R.Values := Array_Item_Vectors.Empty_Vector;
      R.Item_Type := Items.Clone (Item.Item_Type.all);
   end Clone_Dispatching;

   -------------------
   -- Build_Display --
   -------------------

   overriding function Build_Display
     (Self : not null access Array_Type;
      Name : String;
      View : not null access Debugger_Data_View_Record'Class;
      Lang : Language.Language_Access;
      Mode : Display_Mode) return Component_Item
   is
      Styles : constant access Browser_Styles := View.Get_View.Get_Styles;
      R    : Collapsible_Item;
      Rect : constant Component_Item :=
        New_Component_Item (Styles, Self, Name);
   begin
      --  If we have a real empty array (ie the dimensions were not considered
      --  as dynamic
      --  In case we were able to parse the value despite the range information

      if Self.Dimensions (1).First > Self.Dimensions (1).Last
        and then Self.Dimensions (1).First /= Long_Integer'Last
        and then Self.Dimensions (1).Last /= Long_Integer'First
        and then Self.Values.Is_Empty
      then
         null;

      elsif not Self.Visible then
         Rect.Add_Child (View.Item_Hidden);

      else
         if Show_Type (Mode)
           and then Self.Type_Name /= Null_Unbounded_String
         then
            Rect.Add_Child
              (Gtk_New_Text (Styles.Text_Font, Self.Get_Type_Name (Lang)));
         end if;

         if Show_Value (Mode) and then not Self.Values.Is_Empty then
            for V in 1 .. Self.Last_Value loop
               R := new Collapsible_Item_Record;
               R.For_Component := Self.Values (V).Value;
               R.Initialize_Rect (Styles.Invisible);
               R.Set_Child_Layout (Horizontal_Stack);
               Rect.Add_Child (R);

               declare
                  Idx : constant String := Index_String
                    (Self.all, Self.Values (V).Index, Self.Num_Dimensions);
               begin
                  if Self.Values (V).Value /= null then
                     R.Add_Child
                       (Gtk_New_Text
                          (Styles.Text_Font, Idx & ASCII.HT & " => "));
                     R.Add_Child
                       (Self.Values (V).Value.all.Build_Display
                        (Array_Item_Name (Lang, Name, Idx), View, Lang, Mode));
                  end if;
               end;
            end loop;
         end if;
      end if;

      return Rect;
   end Build_Display;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Parent       : access Array_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class) return Generic_Type_Access is
   begin
      --  Since all values should be replaced, do nothing if there is any
      --  value defined.

      if not Parent.Values.Is_Empty then
         return null;
      end if;

      --  Only the Item_Type can be substituted

      if Parent.Item_Type /= Generic_Type_Access (Current) then
         return null;
      end if;

      Free (Parent.Item_Type, Only_Value => False);
      Parent.Item_Type := Generic_Type_Access (Replace_With);
      return Generic_Type_Access (Replace_With);
   end Replace;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Item : access Array_Type) return Generic_Iterator'Class
   is
      Iter : Array_Iterator;
   begin
      Iter.Item := Array_Type_Access (Item);

      if not Item.Values.Is_Empty then
         Iter.Child := 1;
      end if;

      return Iter;
   end Start;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Array_Iterator) is
   begin
      Iter.Child := Iter.Child + 1;
   end Next;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Array_Iterator) return Boolean is
   begin
      return Iter.Item.Values.Is_Empty
        or else Iter.Child > Positive (Iter.Item.Values.Length);
   end At_End;

   ----------
   -- Data --
   ----------

   overriding function Data
     (Iter : Array_Iterator) return Generic_Type_Access is
   begin
      return Iter.Item.Values (Iter.Child).Value;
   end Data;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Item1 : access Array_Type; Item2 : access Generic_Type'Class)
      return Boolean is
   begin
      return Item2.all in Array_Type'Class
        and then Item1.Dimensions = Array_Type_Access (Item2).Dimensions
        and then Structurally_Equivalent
        (Item1.Item_Type, Array_Type_Access (Item2).Item_Type);
   end Structurally_Equivalent;

end Items.Arrays;
