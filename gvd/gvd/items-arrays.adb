------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with GNAT.IO;         use GNAT.IO;
with Ada.Tags;        use Ada.Tags;

with Glib;            use Glib;
with Pango.Layout;    use Pango.Layout;

with Gtkada.Style;    use Gtkada.Style;

with Language;        use Language;
with Items.Repeats;   use Items.Repeats;

package body Items.Arrays is

   use type GNAT.Strings.String_Access;

   function Index_String
     (Item    : Array_Type;
      Index   : Long_Integer;
      Dim_Num : Positive) return String;
   --  Return the string indicating the coordinates in the array, for the
   --  element at Index.

   ------------------
   -- Index_String --
   ------------------

   function Index_String
     (Item    : Array_Type;
      Index   : Long_Integer;
      Dim_Num : Positive) return String
   is
      Length : constant Long_Integer := Item.Dimensions (Dim_Num).Last -
        Item.Dimensions (Dim_Num).First + 1;
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
      Tmp       : Array_Item_Array_Access;
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
        and then Item.Values /= null
        and then Elem_Index <= Item.Values (Item.Last_Value).Index
      then
         declare
            Min   : constant Long_Integer := Elem_Index;
            Max   : constant Long_Integer :=
              Long_Integer (Repeat_Num) + Min - 1;
            Min2, Max2 : Long_Integer;

            --  Since the range can be split into two parts, keep enough space.
            Tmp      : Array_Item_Array (1 .. Item.Values'Last * 2);
            Save     : Array_Item_Array_Access := Item.Values;
            Index    : Positive := Tmp'First;

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
               Item.Values := null;
               Item.Last_Value := 0;
            else
               Item.Values := new Array_Item_Array'(Tmp (1 .. Index - 1));
               Item.Last_Value := Index - 1;
            end if;
            Free (Save);
         end;
      end if;

      --  Check whether we already have an element with the same Elem_Index.
      --  If yes, reuse it.

      if Item.Values /= null then
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

      if Item.Values = null then
         Item.Values := new Array_Item_Array (1 .. 100);
         Item.Last_Value := 1;

      else
         Item.Last_Value := Item.Last_Value + 1;
      end if;

      if Item.Last_Value > Item.Values'Last then
         Tmp := Item.Values;
         Item.Values := new Array_Item_Array (1 .. 2 * Tmp'Last);
         Item.Values (1 .. Tmp'Last) := Tmp.all;
         Free (Tmp);
      end if;

      --  Insert the item, but make sure that the Values array is still sorted.

      Index := 1;

      while Index < Item.Last_Value
        and then Item.Values (Index).Index < Elem_Index
      loop
         Index := Index + 1;
      end loop;

      if Index < Item.Last_Value then
         Item.Values (Index + 1 .. Item.Last_Value) :=
           Item.Values (Index .. Item.Last_Value - 1);
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
     (Item       : Array_Type;
      Elem_Index : Long_Integer) return Generic_Type_Access
   is
      Return_Type : Generic_Type_Access;
   begin
      if Item.Values = null then
         return null;
      end if;

      for J in Item.Values'First .. Item.Last_Value loop
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
      Tmp : Array_Item_Array_Access;
   begin
      Tmp := Item.Values;
      Item.Values := new Array_Item_Array (1 .. Item.Last_Value);

      if Item.Last_Value > 0 then
         Item.Values.all := Tmp (1 .. Item.Last_Value);
      end if;

      Free (Tmp);
   end Shrink_Values;

   -----------
   -- Print --
   -----------

   overriding procedure Print (Value : Array_Type; Indent : Natural := 0) is
   begin
      Put ("{Array (");
      for J in 1 .. Value.Num_Dimensions loop
         Put (Value.Dimensions (J).First'Img & " .. "
              & Value.Dimensions (J).Last'Img);
         if J /= Value.Num_Dimensions then
            Put (", ");
         end if;
      end loop;

      Put (")");
      --  Print (Value.Item_Type.all);

      Put ("= (");
      New_Line;
      Put (String'(1 .. Indent + 3 => ' '));

      if Value.Values /= null then
         for J in 1 .. Value.Last_Value loop
            Put (Value.Values (J).Index'Img & " => ");
            Print (Value.Values (J).Value.all, Indent + 6);
            if J /= Value.Values'Last then
               Put (", ");
               New_Line;
               Put (String'(1 .. Indent + 3 => ' '));
            end if;
         end loop;
      end if;
      Put (")}");
   end Print;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Item : access Array_Type; Only_Value : Boolean := False) is
   begin
      if Item.Values /= null then
         --  Free the whole memory for the items, since the type is in fact
         --  stored in a separate field.
         for J in 1 .. Item.Last_Value loop
            Free (Item.Values (J).Value, Only_Value => False);
         end loop;
         Free (Item.Values);
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
      R.Values := null;
      R.Item_Type := Items.Clone (Item.Item_Type.all);
   end Clone_Dispatching;

   -----------
   -- Paint --
   -----------

   overriding procedure Paint
     (Item    : in out Array_Type;
      Context : Drawing_Context;
      Cr      : Cairo.Cairo_Context;
      Lang    : Language.Language_Access;
      Mode    : Display_Mode;
      X, Y    : Gint := 0)
   is
      Current_Y : Gint := Y + Border_Spacing;
      W, H : Gint;

   begin
      Item.X := X;
      Item.Y := Y;

      --  If we have a real empty array (ie the dimensions were not considered
      --  as dynamic

      if Item.Dimensions (1).First > Item.Dimensions (1).Last
        and then Item.Dimensions (1).First /= Long_Integer'Last
        and then Item.Dimensions (1).Last /= Long_Integer'First
        and then Item.Values = null
      --  In case we were able to parse the value despite the range information
      then
         return;
      end if;

      if not Item.Valid then
         Draw_Pixbuf (Cr, Context.Unknown_Pixmap, X + Left_Border, Y);
         return;
      end if;

      if not Item.Visible then
         Draw_Pixbuf (Cr, Context.Hidden_Pixmap, X + Left_Border, Y);
         return;
      end if;

      if Item.Selected then
         Draw_Rectangle
           (Cr, Context.Selection_Color,
            Filled => True,
            X      => X,
            Y      => Y,
            Width  => Item.Width,
            Height => Item.Height);
      end if;

      if Show_Type (Mode)
        and then Item.Type_Name /= null
      then
         Set_Text (Context.Type_Layout, Get_Type_Name (Item'Access, Lang));
         Draw_Layout
           (Cr, Context.Foreground,
            X        => X,
            Y        => Current_Y,
            Layout   => Context.Type_Layout);
         Get_Pixel_Size (Context.Type_Layout, W, H);
         Current_Y := Current_Y + H;
      end if;

      if Show_Value (Mode) and then Item.Values /= null then
         for V in Item.Values'Range loop
            Set_Text
              (Context.Text_Layout, Index_String
               (Item, Item.Values (V).Index, Item.Num_Dimensions)
               & ASCII.HT & " => ");
            Draw_Layout
              (Cr, Context.Foreground,
               X        => X,
               Y        => Current_Y,
               Layout   => Context.Text_Layout);

            Paint
              (Item.Values (V).Value.all, Context, Cr, Lang, Mode,
               X + Left_Border + Border_Spacing + Item.Index_Width,
               Current_Y);
            Current_Y :=
              Current_Y + Item.Values (V).Value.Height + Line_Spacing;
         end loop;
      end if;

      --  Draw a border
      Draw_Rectangle
        (Cr, Context.Foreground,
         Filled => False,
         X      => X,
         Y      => Y,
         Width  => Item.Width - 1,
         Height => Item.Height - 1);
   end Paint;

   ------------------
   -- Size_Request --
   ------------------

   overriding procedure Size_Request
     (Item           : in out Array_Type;
      Context        : Drawing_Context;
      Lang           : Language.Language_Access;
      Mode           : Display_Mode;
      Hide_Big_Items : Boolean := False)
   is
      Total_Height, Total_Width : Gint := 0;
      W, H : Gint;
   begin
      if not Item.Valid then
         Item.Width := Get_Width (Context.Unknown_Pixmap);
         Item.Height := Get_Height (Context.Unknown_Pixmap);

         return;
      end if;

      --  If we have a real empty array (ie the dimensions were not considered
      --  as dynamic

      if Item.Dimensions (1).First > Item.Dimensions (1).Last
        and then Item.Dimensions (1).First /= Long_Integer'Last
        and then Item.Dimensions (1).Last /= Long_Integer'First
        and then Item.Values = null --  Sometimes (ex/ Unbounded_Strings), we
                                    --  could parse the value anyway
      then
         Item.Width := 20;
         Item.Height := 0;
         return;
      end if;

      if Item.Visible then
         if Show_Type (Mode)
           and then Item.Type_Name /= null
         then
            Set_Text (Context.Type_Layout, Get_Type_Name (Item'Access, Lang));
            Get_Pixel_Size (Context.Type_Layout, Total_Width, Total_Height);
            Item.Type_Height := Total_Height;
         else
            Item.Type_Height := 0;
         end if;

         if Show_Value (Mode) then
            Item.Index_Width := 20;  --  minimal width

            if Item.Values /= null then
               for V in Item.Values'Range loop
                  Set_Text
                    (Context.Text_Layout, Index_String
                     (Item, Item.Values (V).Index, Item.Num_Dimensions)
                     & ASCII.HT & " => ");
                  Get_Pixel_Size (Context.Text_Layout, W, H);
                  Item.Index_Width := Gint'Max (Item.Index_Width, W);

                  Size_Request
                    (Item.Values (V).Value.all, Context, Lang, Mode,
                     Hide_Big_Items);
                  Total_Width  :=
                    Gint'Max (Total_Width, Item.Values (V).Value.Width);
                  Total_Height := Total_Height + Item.Values (V).Value.Height;
               end loop;

               Total_Height :=
                 Total_Height + (Item.Values'Length - 1) * Line_Spacing;
            end if;
         end if;

         --  Keep enough space for the border (Border_Spacing on each side)

         Item.Width := Total_Width + Item.Index_Width + Left_Border +
           2 * Border_Spacing;
         Item.Height := Total_Height + 2 * Border_Spacing;

         --  Hide big items for efficiency
         if Hide_Big_Items
           and then Item.Height > Context.Big_Item_Height
         then
            Item.Visible := False;
         end if;
      end if;

      if not Item.Visible then
         Item.Index_Width := 0;
         Item.Width :=
           Get_Width (Context.Hidden_Pixmap) +
           Left_Border + 2 * Border_Spacing;
         Item.Height :=
           Get_Height (Context.Hidden_Pixmap) + 2 * Border_Spacing;
      end if;
   end Size_Request;

   ---------------------
   -- Propagate_Width --
   ---------------------

   overriding procedure Propagate_Width
     (Item  : in out Array_Type; Width : Glib.Gint)
   is
      W : constant Gint := Width - Item.Index_Width - 2 * Border_Spacing -
        Left_Border;
   begin
      Item.Width := Width;

      if Item.Visible and then Item.Values /= null then
         for V in Item.Values'Range loop
            if Item.Values (V).Value /= null then
               Propagate_Width (Item.Values (V).Value.all, W);
            end if;
         end loop;
      end if;
   end Propagate_Width;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   overriding function Get_Component_Name
     (Item : access Array_Type;
      Lang : access Language.Language_Root'Class;
      Name : String;
      Comp : Generic_Type_Access) return String
   is
   begin
      for C in Item.Values'Range loop
         if Item.Values (C).Value = Comp then
            return Array_Item_Name
              (Lang, Name, Index_String
                 (Item.all, Item.Values (C).Index, Item.Num_Dimensions));
         end if;
      end loop;
      return Name;
   end Get_Component_Name;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   overriding function Get_Component_Name
     (Item : access Array_Type;
      Lang : access Language_Root'Class;
      Name : String;
      X, Y : Glib.Gint) return String
   is
      Total_Height : Gint := Border_Spacing + Item.Type_Height;
      Tmp_Height   : Gint;
      Field_Name_Start : constant Gint := Left_Border + Border_Spacing;
      Field_Start  : constant Gint := Field_Name_Start + Item.Index_Width;
   begin
      if not Item.Visible then
         return Name;
      end if;

      --  Click in the left column ? => Select the whole item

      if X < Field_Name_Start
        or else Item.Values = null
      then
         return Name;
      end if;

      --  Did we click the type of the item

      if Y < Item.Type_Height then
         return Name;
      end if;

      --  Else, find the relevant item

      for V in Item.Values'Range loop
         Tmp_Height := Total_Height + Item.Values (V).Value.Height +
           Line_Spacing;

         if Y <= Tmp_Height then
            declare
               Item_Name : constant String :=
                 Array_Item_Name
                 (Lang, Name, Index_String
                  (Item.all, Item.Values (V).Index, Item.Num_Dimensions));

            begin
               --  ??? Should be able to get a range of values (ie all the
               --  values that are displayed in a single box).

               if X < Field_Start then
                  return Item_Name;
               end if;

               return Get_Component_Name
                 (Item.Values (V).Value, Lang, Item_Name,
                  X - Field_Start, Y - Total_Height);
            end;
         end if;

         Total_Height := Tmp_Height;
      end loop;

      return Name;
   end Get_Component_Name;

   -------------------
   -- Get_Component --
   -------------------

   overriding function Get_Component
     (Item : access Array_Type; X, Y : Glib.Gint) return Generic_Type_Access
   is
      Total_Height : Gint := Border_Spacing + Item.Type_Height;
      Tmp_Height   : Gint;
      Field_Name_Start : constant Gint := Left_Border + Border_Spacing;
      Field_Start  : constant Gint := Field_Name_Start + Item.Index_Width;

   begin
      if not Item.Valid or else not Item.Visible then
         return Generic_Type_Access (Item);
      end if;

      --  Click in the left column ? => Select the whole item

      if X < Field_Name_Start
        or else Item.Values = null
      then
         return Generic_Type_Access (Item);
      end if;

      --  Did we click the type of the item

      if Y < Item.Type_Height then
         return Generic_Type_Access (Item);
      end if;

      --  Else, find the relevant item

      for V in Item.Values'Range loop
         Tmp_Height := Total_Height + Item.Values (V).Value.Height +
           Line_Spacing;

         if Y <= Tmp_Height then
            if X < Field_Start then
               return Generic_Type_Access (Item);
            end if;

            return Get_Component
              (Item.Values (V).Value,
               X - Field_Start,
               Y - Total_Height);
         end if;

         Total_Height := Tmp_Height;
      end loop;

      return Generic_Type_Access (Item);
   end Get_Component;

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

      if Parent.Values /= null then
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

      if Item.Values /= null then
         Iter.Child := Item.Values'First;
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
      return Iter.Item.Values = null
        or else Iter.Child > Iter.Item.Values'Last;
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
