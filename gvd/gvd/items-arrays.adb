-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with GNAT.IO;  use GNAT.IO;
with Ada.Tags; use Ada.Tags;

with Glib;         use Glib;
with Gdk.Font;     use Gdk.Font;
with Gdk.Drawable; use Gdk.Drawable;
with Gdk.GC;       use Gdk.GC;
with Gdk.Types;    use Gdk.Types;
with Language;     use Language;
with Unchecked_Deallocation;

with Odd.Types;    use Odd.Types;

with Items.Repeats; use Items.Repeats;

package body Items.Arrays is

   function Index_String (Item    : Array_Type;
                          Index   : Long_Integer;
                          Dim_Num : Positive)
                         return String;
   --  Return the string indicating the coordinates in the array, for the
   --  element at Index.

   ------------------
   -- Index_String --
   ------------------

   function Index_String (Item    : Array_Type;
                          Index   : Long_Integer;
                          Dim_Num : Positive)
                         return String
   is
      Length : constant Long_Integer := Item.Dimensions (Dim_Num).Last
        - Item.Dimensions (Dim_Num).First + 1;
   begin
      --  Do we have an array with no element ?
      if Length <= 0 then
         return "??";

      else
         declare
            Dim : constant String := Long_Integer'Image
              (Index mod Length + Item.Dimensions (Dim_Num).First);
         begin
            if Dim_Num /= 1 then
               return Index_String (Item, Index / Length, Dim_Num - 1)
                 & "," & Dim (Dim'First + 1 .. Dim'Last);
            else
               return Dim (Dim'First + 1 .. Dim'Last);
            end if;
         end;
      end if;
   end Index_String;

   --------------------
   -- New_Array_Type --
   --------------------

   function New_Array_Type (Num_Dimensions : Positive)
                           return Generic_Type_Access
   is
   begin
      return new Array_Type (Num_Dimensions => Num_Dimensions);
   end New_Array_Type;

   --------------------
   -- Set_Dimensions --
   --------------------

   procedure Set_Dimensions (Item : in out Array_Type;
                             Dim  : Positive;
                             Size : Dimension)
   is
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

   function Get_Dimensions (Item : Array_Type;
                            Dim  : Positive)
                           return Dimension
   is
   begin
      return Item.Dimensions (Dim);
   end Get_Dimensions;

   -------------------
   -- Set_Item_Type --
   -------------------

   procedure Set_Item_Type
     (Item     : in out Array_Type;
      The_Type : access Generic_Type'Class)
   is
   begin
      Item.Item_Type := Generic_Type_Access (The_Type);
   end Set_Item_Type;

   -------------------
   -- Get_Item_Type --
   -------------------

   function Get_Item_Type (Item : Array_Type)
                          return Generic_Type_Access
   is
   begin
      return Item.Item_Type;
   end Get_Item_Type;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Item  : in out Array_Type;
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
         Set_Value (Repeat_Type (To_Insert.all),
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

                  --  Old one on the "left" ofthe old one => Split it
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
                  Range_Index     : Long_Integer := Item.Values (J).Index;
                  Tmp             : Generic_Type_Access;
               begin

                  Tmp := Get_Value (Repeat_Type (Item.Values (J).Value.all));
                  if Elem_Index - Range_Index >= 1 then
                     Set_Repeat_Num (Repeat_Type (Item.Values (J).Value.all),
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

   function Get_Value (Item       : Array_Type;
                       Elem_Index : Long_Integer)
                      return Generic_Type_Access
   is
      Return_Type : Generic_Type_Access;
   begin
      if Item.Values = null then
         return null;
      end if;

      for J in Item.Values'Range loop
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
      Item.Values.all := Tmp (1 .. Item.Last_Value);
      Free (Tmp);
   end Shrink_Values;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Array_Type; Indent : Natural := 0) is
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

   procedure Free (Item : access Array_Type;
                   Only_Value : Boolean := False)
   is
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

   procedure Clone_Dispatching
     (Item  : Array_Type;
      Clone : out Generic_Type_Access)
   is
      R : Array_Type_Access := Array_Type_Access (Clone);
   begin
      Clone_Dispatching (Generic_Type (Item), Clone);

      --  ??? Should duplicate the values as well....
      R.Values := null;
      R.Item_Type := Items.Clone (Item.Item_Type.all);
   end Clone_Dispatching;

   -----------
   -- Paint --
   -----------

   procedure Paint (Item    : in out Array_Type;
                    Context : Drawing_Context;
                    X, Y    : Gint := 0)
   is
      Current_Y : Gint := Y + Border_Spacing;
      Arrow_Pos : constant Gint := X + Border_Spacing + Item.Index_Width +
        Left_Border - Text_Width (Context.Font, String' (" => "));
   begin
      Item.X := X;
      Item.Y := Y;

      --  If we have a real empty array (ie the dimensions were not considered
      --  as dynamic
      if Item.Dimensions (1).First > Item.Dimensions (1).Last
        and then Item.Dimensions (1).First /= Long_Integer'Last
        and then Item.Dimensions (1).Last /= Long_Integer'First
      then
         return;
      end if;

      if not Item.Valid then
         Display_Pixmap (Context.Pixmap, Context.GC, Unknown_Pixmap,
                         Unknown_Mask, X + Left_Border, Y);
         return;
      end if;

      if not Item.Visible then
         Display_Pixmap (Context.Pixmap, Context.GC, Hidden_Pixmap,
                         Hidden_Mask, X + Left_Border, Current_Y);
         return;
      end if;

      if Item.Selected then
         Draw_Rectangle
           (Context.Pixmap,
            Context.GC,
            Filled => True,
            X      => X,
            Y      => Y,
            Width  => Item.Width,
            Height => Item.Height);
         Set_Function (Context.GC, Copy_Invert);
      end if;

      if Show_Type (Context.Mode)
        and then Item.Type_Name /= null
      then
         Draw_Text (Context.Pixmap,
                    Font => Context.Type_Font,
                    GC   => Context.GC,
                    X    => X + Border_Spacing,
                    Y    => Current_Y + Get_Ascent (Context.Type_Font),
                    Text => Item.Type_Name.all);
         Current_Y := Current_Y
           + Get_Ascent (Context.Type_Font) + Get_Descent (Context.Type_Font);
      end if;

      if Show_Value (Context.Mode) then
         for V in Item.Values'Range loop

            Draw_Text (Context.Pixmap,
                       Font => Context.Font,
                       GC   => Context.GC,
                       X    => X + Left_Border + Border_Spacing,
                       Y    => Current_Y + Get_Ascent (Context.Font),
                       Text => Index_String (Item,
                                             Item.Values (V).Index,
                                             Item.Num_Dimensions));
            Draw_Text (Context.Pixmap,
                       Font => Context.Font,
                       GC   => Context.GC,
                       X    => Arrow_Pos,
                       Y    => Current_Y + Get_Ascent (Context.Font),
                       Text => " => ");
            Paint (Item.Values (V).Value.all, Context,
                   X + Left_Border + Border_Spacing + Item.Index_Width,
                   Current_Y);
            Current_Y :=
              Current_Y + Item.Values (V).Value.Height + Line_Spacing;
         end loop;
      end if;

      --  Draw a border
      Draw_Rectangle (Context.Pixmap,
                      Context.GC,
                      Filled => False,
                      X      => X,
                      Y      => Y,
                      Width  => Item.Width - 1,
                      Height => Item.Height - 1);

      if Item.Selected then
         Set_Function (Context.GC, Copy);
      end if;
   end Paint;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Item           : in out Array_Type;
      Context        : Drawing_Context;
      Hide_Big_Items : Boolean := False)
   is
      Total_Height, Total_Width : Gint := 0;
   begin
      if not Item.Valid then
         Item.Width := Unknown_Width;
         Item.Height := Unknown_Height;
         return;
      end if;

      --  If we have a real empty array (ie the dimensions were not considered
      --  as dynamic
      if Item.Dimensions (1).First > Item.Dimensions (1).Last
        and then Item.Dimensions (1).First /= Long_Integer'Last
        and then Item.Dimensions (1).Last /= Long_Integer'First
      then
         Item.Width := 20;
         Item.Height := 0;
         return;
      end if;

      if Item.Visible then
         if Show_Type (Context.Mode)
           and then Item.Type_Name /= null
         then
            Item.Type_Height := Get_Descent (Context.Type_Font)
              + Get_Ascent (Context.Type_Font);
            Total_Height := Total_Height + Item.Type_Height;
            Total_Width := Gint'Max
              (Total_Width,
               Text_Width (Context.Type_Font, Item.Type_Name.all));
         else
            Item.Type_Height := 0;
         end if;

         if Show_Value (Context.Mode) then
            Item.Index_Width := 20;  --  minimal width
            if Item.Values /= null then
               for V in Item.Values'Range loop
                  Size_Request
                    (Item.Values (V).Value.all, Context, Hide_Big_Items);
                  Total_Width  :=
                    Gint'Max (Total_Width, Item.Values (V).Value.Width);
                  Total_Height := Total_Height + Item.Values (V).Value.Height;
                  Item.Index_Width :=
                    Gint'Max (Item.Index_Width, String_Width
                              (Context.Font,
                               Index_String (Item, Item.Values (V).Index,
                                             Item.Num_Dimensions)));
               end loop;
               Total_Height :=
                 Total_Height + (Item.Values'Length - 1) * Line_Spacing;
            end if;

            Item.Index_Width :=
              Item.Index_Width + Text_Width (Context.Font, String'(" => "));
         end if;

         --  Keep enough space for the border (Border_Spacing on each side)

         Item.Width  := Total_Width + Item.Index_Width + Left_Border
           + 2 * Border_Spacing;
         Item.Height := Total_Height + 2 * Border_Spacing;

         --  Hide big items for efficiency
         if Hide_Big_Items and then Item.Height > Big_Item_Height then
            Item.Visible := False;
         end if;
      end if;

      if not Item.Visible then
         Item.Index_Width := 0;
         Item.Width := Left_Border + 2 * Border_Spacing + Hidden_Width;
         Item.Height := 2 * Border_Spacing + Hidden_Height;
      end if;
   end Size_Request;

   ---------------------
   -- Propagate_Width --
   ---------------------

   procedure Propagate_Width (Item  : in out Array_Type;
                              Width : Glib.Gint)
   is
      W : constant Gint := Width - Item.Index_Width - 2 * Border_Spacing
        - Left_Border;
   begin
      Item.Width := Width;
      if Item.Visible and then Item.Values /= null then
         for V in Item.Values'Range loop
            Propagate_Width (Item.Values (V).Value.all, W);
         end loop;
      end if;
   end Propagate_Width;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   function Get_Component_Name (Item : access Array_Type;
                                Lang : access Language_Root'Class;
                                Name : String;
                                X, Y : Glib.Gint)
                               return String
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
         Tmp_Height := Total_Height + Item.Values (V).Value.Height
           + Line_Spacing;
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

   function Get_Component (Item : access Array_Type;
                           X, Y : Glib.Gint)
                          return Generic_Type_Access
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
         Tmp_Height := Total_Height + Item.Values (V).Value.Height
           + Line_Spacing;
         if Y <= Tmp_Height then
            if X < Field_Start then
               return Generic_Type_Access (Item);
            end if;

            return Get_Component (Item.Values (V).Value,
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

   function Replace
     (Parent       : access Array_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class)
     return Generic_Type_Access
   is
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

   function Start (Item : access Array_Type) return Generic_Iterator'Class is
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

   procedure Next (Iter : in out Array_Iterator) is
   begin
      Iter.Child := Iter.Child + 1;
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Array_Iterator) return Boolean is
   begin
      return Iter.Item.Values = null
        or else Iter.Child > Iter.Item.Values'Last;
   end At_End;

   ----------
   -- Data --
   ----------

   function Data (Iter : Array_Iterator) return Generic_Type_Access is
   begin
      return Iter.Item.Values (Iter.Child).Value;
   end Data;

end Items.Arrays;
