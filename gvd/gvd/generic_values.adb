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
with Gdk.Pixmap;   use Gdk.Pixmap;
with Gdk.Bitmap;   use Gdk.Bitmap;
with Gdk.GC;       use Gdk.GC;
with Gdk.Window;   use Gdk.Window;
with Gdk.Types;    use Gdk.Types;
with Language;     use Language;
with Unchecked_Deallocation;

package body Generic_Values is

   Line_Spacing : constant Gint := 1;
   --  Space between lines in the display of items in a pixmap.
   --  This is the extra space added between two lines of an array or two
   --  fields of a record

   Border_Spacing : constant Gint := 2;
   --  Space between the rectangle and the item on each side, for complex
   --  items.

   Left_Border : constant Gint := 5;
   --  Space of the column on the left of records and arrays, where the user
   --  can click to select the whole array or record.

   Big_Item_Height : constant Gint := 150;
   --  Items taller than this value will start hidden.

   Hidden_Pixmap : Gdk_Pixmap;
   Hidden_Mask   : Gdk_Bitmap;
   Hidden_Height : Gint;
   Hidden_Width  : Gint;
   --  pixmap used when items are hidden

   Unknown_Pixmap : Gdk_Pixmap;
   Unknown_Mask   : Gdk_Bitmap;
   Unknown_Height : Gint;
   Unknown_Width  : Gint;

   function Index_String (Item    : Array_Type;
                          Index   : Long_Integer;
                          Dim_Num : Positive)
                         return String;
   --  Return the string indicating the coordinates in the array, for the
   --  element at Index.

   procedure Display_Pixmap (On_Pixmap : Gdk_Pixmap;
                             GC        : Gdk_GC;
                             Pixmap    : Gdk_Pixmap;
                             Mask      : Gdk_Bitmap;
                             X, Y      : Gint);
   --  Display a masked pixmap at specific coordinates.

   procedure Free_Internal is new Unchecked_Deallocation
     (Generic_Type'Class, Generic_Type_Access);

   -----------------------
   -- Set_Hidden_Pixmap --
   -----------------------

   procedure Set_Hidden_Pixmap (Pixmap : Gdk.Pixmap.Gdk_Pixmap;
                                Mask   : Gdk.Bitmap.Gdk_Bitmap)
   is
   begin
      Hidden_Pixmap := Pixmap;
      Hidden_Mask   := Mask;
      Get_Size (Hidden_Pixmap, Hidden_Width, Hidden_Height);
   end Set_Hidden_Pixmap;

   ------------------------
   -- Set_Unknown_Pixmap --
   ------------------------

   procedure Set_Unknown_Pixmap (Pixmap : Gdk.Pixmap.Gdk_Pixmap;
                                 Mask   : Gdk.Bitmap.Gdk_Bitmap)
   is
   begin
      Unknown_Pixmap := Pixmap;
      Unknown_Mask   := Mask;
      Get_Size (Unknown_Pixmap, Unknown_Width, Unknown_Height);
   end Set_Unknown_Pixmap;

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
                 & "," & Dim;
            else
               return Dim;
            end if;
         end;
      end if;
   end Index_String;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (Item : Generic_Type) return Glib.Gint is
   begin
      return Item.Width;
   end Get_Width;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (Item : Generic_Type) return Glib.Gint is
   begin
      return Item.Height;
   end Get_Height;

   -----------
   -- Get_X --
   -----------

   function Get_X (Item : Generic_Type) return Glib.Gint is
   begin
      return Item.X;
   end Get_X;

   -----------
   -- Get_Y --
   -----------

   function Get_Y (Item : Generic_Type) return Glib.Gint is
   begin
      return Item.Y;
   end Get_Y;

   ---------------
   -- Set_Valid --
   ---------------

   procedure Set_Valid (Item  : access Generic_Type;
                        Valid : Boolean := True)
   is
   begin
      Item.Valid := Valid;
   end Set_Valid;


   --------------------
   -- Set_Visibility --
   --------------------

   procedure Set_Visibility (Item    : in out Generic_Type;
                             Visible : Boolean)
   is
   begin
      Item.Visible := Visible;
   end Set_Visibility;

   --------------------
   -- Get_Visibility --
   --------------------

   function Get_Visibility (Item : Generic_Type) return Boolean is
   begin
      return Item.Visible;
   end Get_Visibility;

   ---------------------
   -- New_Simple_Type --
   ---------------------

   function New_Simple_Type return Generic_Type_Access is
   begin
      return new Simple_Type;
   end New_Simple_Type;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Item : Simple_Type) return String_Access is
   begin
      return Item.Value;
   end Get_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Item : in out Simple_Type; Value : String) is
   begin
      if Item.Value /= null then
         Free (Item.Value);
      end if;
      Item.Value := new String'(Value);
      Item.Valid := True;
   end Set_Value;

   --------------------
   -- New_Range_Type --
   --------------------

   function New_Range_Type (Min, Max : Long_Integer)
                           return Generic_Type_Access
   is
   begin
      return new Range_Type'(Value    => null,
                             Visible  => True,
                             Selected => False,
                             Min      => Min,
                             Max      => Max,
                             Width    => 0,
                             Height   => 0,
                             Valid    => False,
                             X        => -1,
                             Y        => -1);
   end New_Range_Type;

   ------------------
   -- New_Mod_Type --
   ------------------

   function New_Mod_Type (Modulo : Long_Integer) return Generic_Type_Access is
   begin
      return new Mod_Type'(Value    => null,
                           Modulo   => Modulo,
                           Visible  => True,
                           Selected => False,
                           Width    => 0,
                           Height   => 0,
                           Valid    => False,
                           X        => -1,
                           Y        => -1);
   end New_Mod_Type;

   ---------------------
   -- New_Access_Type --
   ---------------------

   function New_Access_Type return Generic_Type_Access is
   begin
      return new Access_Type;
   end New_Access_Type;

   -------------------
   -- New_Enum_Type --
   -------------------

   function New_Enum_Type return Generic_Type_Access is
   begin
      return new Enum_Type;
   end New_Enum_Type;

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
         To_Insert := new Repeat_Type'
           (Repeat_Num => Repeat_Num,
            Value      => Generic_Type_Access (Elem_Value),
            Visible    => True,
            Selected   => False,
            Valid      => True,
            Width      => 0,
            Height     => 0,
            X          => -1,
            Y          => -1);
      end if;

      --  If we are inserting a range, delete all old items that are covered
      --  by this new range.
      --  Since components are sorted by Index in Item.Values, we can speed
      --  things up a little bit.

      if Repeat_Num > 1
        and then Item.Values /= null
        and then Elem_Index <= Item.Values (Item.Last_Value).Index
      then
         declare
            Min, Max   : Long_Integer;
            Min2, Max2 : Long_Integer;

            --  Since the range can be split in two, keep enough space.
            Tmp      : Array_Item_Array (1 .. Item.Values'Last * 2);
            Save     : Array_Item_Array_Access := item.Values;
            Index    : Positive := Tmp'First;
         begin
            Min := Elem_Index;
            Max := Long_Integer (Repeat_Num) + Min - 1;

            for J in 1 .. Item.Last_Value loop

               --  If we have a repeat type, we might have to split it.

               if Item.Values (J).Value.all in Repeat_Type'Class then
                  Min2 := Item.Values (J).Index;
                  Max2 := Min2 - 1 + Long_Integer
                    (Repeat_Type_Access (Item.Values (J).Value).Repeat_Num);

                  --  Old one completly inside the new one => delete it

                  if Min2 >= Min and then Max2 <= Max then
                     null;

                  elsif Min2 < Min and then Max2 > Max then
                     Tmp (Index).Index := Item.Values (J).Index;
                     Tmp (Index).Value := Clone (Item.Values (J).Value.all);
                     Repeat_Type (Tmp (Index).Value.all).Repeat_Num :=
                       Integer (Min - Min2);
                     Index := Index + 1;

                     Tmp (Index) := Item.Values (J);
                     Tmp (Index).Index := Max + 1;
                     Repeat_Type (Tmp (Index).Value.all).Repeat_Num :=
                       Integer (Max2 - Max - 1);
                     Index := Index + 1;

                  elsif Min2 < Min and then Max2 >= Min then
                     Tmp (Index) := Item.Values (J);
                     Repeat_Type (Tmp (Index).Value.all).Repeat_Num :=
                       Integer (Min - Min2);
                     Index := Index + 1;

                  elsif Min2 <= Max and then Max2 > Max then
                     Tmp (Index) := Item.Values (J);
                     Tmp (Index).Index := Max + 1;
                     Repeat_Type (Tmp (Index).Value.all).Repeat_Num :=
                       Integer (Max2 - Max - 1);
                     Index := Index + 1;

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
               --  Will be reallocated right below
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
      --  No need to handle ranges specially, since this is in fact done by
      --  the debugger itselft (splitting ranges,...)

      if Item.Values /= null then
         for J in 1 .. Item.Last_Value loop
            if Item.Values (J).Index = Elem_Index then
               declare
                  Was_Visible : Boolean := Item.Values (J).Value.Visible;
               begin
                  Free (Item.Values (J).Value, Only_Value => False);
                  Item.Values (J).Value := To_Insert;
                  Item.Values (J).Value.Visible := Was_Visible;
               end;
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
   begin
      for J in Item.Values'Range loop
         if Item.Values (J).Index = Elem_Index then
            return Item.Values (J).Value;

         elsif Item.Values (J).Value'Tag = Repeat_Type'Tag
           and then Item.Values (J).Index < Elem_Index
           and then Item.Values (J).Index
           + Long_Integer (Repeat_Type_Access
                           (Item.Values (J).Value).Repeat_Num) > Elem_Index
         then
            return Repeat_Type_Access (Item.Values (J).Value).Value;
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

   ---------------------
   -- New_Record_Type --
   ---------------------

   function New_Record_Type (Num_Fields : Natural)
                            return Generic_Type_Access
   is
      R : Generic_Type_Access := new Record_Type (Num_Fields);
   begin
      --  A null record is always valid.
      if Num_Fields = 0 then
         R.Valid := True;
      end if;
      return R;
   end New_Record_Type;

   ----------------
   -- Num_Fields --
   ----------------

   function Num_Fields (Item : Record_Type) return Natural is
   begin
      return Item.Num_Fields;
   end Num_Fields;

   --------------------
   -- Set_Field_Name --
   --------------------

   procedure Set_Field_Name (Item          : in out Record_Type;
                             Index         : Positive;
                             Name          : String;
                             Variant_Parts : Natural := 0)
   is
   begin
      if Item.Fields (Index).Value /= null then
         Free (Item.Fields (Index).Value, Only_Value => False);
      end if;

      if Item.Fields (Index).Variant_Part /= null then
         Free (Item.Fields (Index).Variant_Part);
      end if;

      if Item.Fields (Index).Name /= null then
         Free (Item.Fields (Index).Name);
      end if;

      if Variant_Parts = 0 then
         Item.Fields (Index) := Record_Field'
           (Name          => new String'(Name),
            Value        => null,
            Variant_Part => null);
      else
         Item.Fields (Index) := Record_Field'
           (Name         => new String'(Name),
            Value        => null,
            Variant_Part => new Record_Type_Array (1 .. Variant_Parts));
      end if;
   end Set_Field_Name;

   --------------------
   -- Get_Field_Name --
   --------------------

   function Get_Field_Name (Item  : in Record_Type;
                            Index : Positive)
                           return String_Access
   is
   begin
      return Item.Fields (Index).Name;
   end Get_Field_Name;

   -----------------------
   -- Set_Variant_Field --
   -----------------------

   procedure Set_Variant_Field (Item          : in out Record_Type;
                                Index         : Positive;
                                Variant_Index : Positive;
                                Value         : access Record_Type'Class)
   is
   begin
      if Item.Fields (Index).Variant_Part /= null then
         if Item.Fields (Index).Variant_Part (Variant_Index) /= null then
            Free (Item.Fields (Index).Variant_Part (Variant_Index),
                  Only_Value => False);
         end if;

         Item.Fields (Index).Variant_Part (Variant_Index) :=
           Record_Type_Access (Value);

         --  If there is at least one field, the record is valid.
         Item.Valid := True;
      end if;
   end Set_Variant_Field;

   -----------------------
   -- Get_Variant_Parts --
   -----------------------

   function Get_Variant_Parts (Item  : Record_Type;
                               Field : Positive)
                              return Natural
   is
   begin
      if Item.Fields (Field).Variant_Part = null then
         return 0;
      else
         return Item.Fields (Field).Variant_Part'Length;
      end if;
   end Get_Variant_Parts;

   -----------------------
   -- Find_Variant_Part --
   -----------------------

   function Find_Variant_Part (Item     : Record_Type;
                               Field    : Positive;
                               Contains : String)
                              return Generic_Type_Access
   is
   begin
      for J in Item.Fields (Field).Variant_Part'Range loop
         if Item.Fields (Field).Variant_Part (J).Fields (1).Name.all
           = Contains
         then
            return Generic_Type_Access (Item.Fields (Field).Variant_Part (J));
         end if;
      end loop;
      return null;
   end Find_Variant_Part;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Item  : in out Record_Type;
                        Value : access Generic_Type'Class;
                        Field : String)
   is
   begin
      for J in Item.Fields'Range loop
         if Item.Fields (J).Name.all = Field then
            if Item.Fields (J).Value /= null then
               Free (Item.Fields (J).Value, Only_Value => False);
            end if;
            Item.Fields (J).Value := Generic_Type_Access (Value);
            Item.Fields (J).Value.Valid := True;
         end if;
      end loop;

      --  If there is at least one field, the record is valid.
      Item.Valid := True;
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Item  : in out Record_Type;
                        Value : access Generic_Type'Class;
                        Field : Positive)
   is
   begin
      if Item.Fields (Field).Value /= null then
         Free (Item.Fields (Field).Value, Only_Value => False);
      end if;
      Item.Fields (Field).Value := Generic_Type_Access (Value);
      Item.Fields (Field).Value.Valid := True;
      --  If there is at least one field, the record is valid.
      Item.Valid := True;
   end Set_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Item  : Record_Type;
                       Field : String)
                      return Generic_Type_Access
   is
   begin
      for J in Item.Fields'Range loop
         if Item.Fields (J).Name.all = Field then
            return Item.Fields (J).Value;
         end if;
      end loop;
      return null;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Item  : Record_Type;
                       Field : Positive)
                      return Generic_Type_Access
   is
   begin
      return Item.Fields (Field).Value;
   end Get_Value;

   --------------------
   -- New_Union_Type --
   --------------------

   function New_Union_Type (Num_Fields : Positive)
                           return Generic_Type_Access
   is
   begin
      return new Union_Type (Num_Fields);
   end New_Union_Type;

   --------------------
   -- New_Class_Type --
   --------------------

   function New_Class_Type (Num_Ancestors : Natural)
                           return Generic_Type_Access
   is
   begin
      return new Class_Type (Num_Ancestors);
   end New_Class_Type;

   ------------------
   -- Add_Ancestor --
   ------------------

   procedure Add_Ancestor (Item     : in out Class_Type;
                           Num      : Positive;
                           Ancestor : Class_Type_Access)
   is
   begin
      pragma Assert (Num <= Item.Num_Ancestors);
      if Item.Ancestors (Num) /= null then
         Free (Item.Ancestors (Num), Only_Value => False);
      end if;
      Item.Ancestors (Num) := Ancestor;
      Item.Valid := True;
      Item.Ancestors (Num).Valid := True;
   end Add_Ancestor;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child (Item  : in out Class_Type;
                        Child : Record_Type_Access)
   is
   begin
      pragma Assert (Item.Child = null);
      if Item.Child /= null then
         Free (Item.Child, Only_Value => False);
      end if;
      Item.Child := Child;
   end Set_Child;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (Item : Class_Type) return Generic_Type_Access is
   begin
      return Generic_Type_Access (Item.Child);
   end Get_Child;

   ------------------
   -- Get_Ancestor --
   ------------------

   function Get_Ancestor (Item : Class_Type;
                          Num  : Positive)
                         return Generic_Type_Access
   is
   begin
      pragma Assert (Num <= Item.Num_Ancestors);
      return Generic_Type_Access (Item.Ancestors (Num));
   end Get_Ancestor;

   -----------------------
   -- Get_Num_Ancestors --
   -----------------------

   function Get_Num_Ancestors (Item : Class_Type) return Natural is
   begin
      return Item.Num_Ancestors;
   end Get_Num_Ancestors;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Simple_Type; Indent : Natural := 0) is
   begin
      if Value.Value = null then
         Put ("{Simple: <null>}");
      else
         Put ("{Simple: " & Value.Value.all & "}");
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Range_Type; Indent : Natural := 0) is
   begin
      Put ("{Range" & Value.Min'Img & " .." & Value.Max'Img & " = ");
      if Value.Value /= null then
         Put (Value.Value.all);
      end if;
      Put ("}");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Mod_Type; Indent : Natural := 0) is
   begin
      Put ("{Modulo " & Value.Modulo'Img & " = ");
      if Value.Value /= null then
         Put (Value.Value.all);
      end if;
      Put ("}");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Access_Type; Indent : Natural := 0) is
   begin
      Put ("{Access ");
      if Value.Value = null then
         Put ("<null>)");
      else
         Put (Value.Value.all & "}");
      end if;
   end Print;

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

   -----------
   -- Print --
   -----------

   procedure Print (Value : Repeat_Type; Indent : Natural := 0) is
   begin
      Put ("{<" & Value.Repeat_Num'Img & " times> : ");
      if Value.Value /= null then
         Print (Value.Value.all, Indent + 3);
         Put ("}");
      else
         Put ("<null>}");
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Record_Type; Indent : Natural := 0) is
   begin
      Put ("{Record: ");
      if Value.Fields'Length = 0 then
         Put ("null record");
      else
         New_Line;
         Put (String'(1 .. Indent + 3 => ' '));
         for J in Value.Fields'Range loop
            if Value.Fields (J).Variant_Part /= null then
               Put ("<variant_part> => ");
               for P in Value.Fields (J).Variant_Part'Range loop
                  New_Line;
                  Put (String'(1 .. Indent + 6 => ' '));
                  Print (Value.Fields (J).Variant_Part (P).all,
                         Indent + 9);
               end loop;
               New_Line;
               Put (String'(1 .. Indent + 3 => ' '));

            else
               Put (Value.Fields (J).Name.all & " => ");
               if Value.Fields (J).Value /= null then
                  Print (Value.Fields (J).Value.all, Indent + 6);
               end if;
            end if;
            if J /= Value.Fields'Last then
               Put (", ");
               New_Line;
               Put (String'(1 .. Indent + 3 => ' '));
            end if;
         end loop;
      end if;
      Put ("}");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Union_Type; Indent : Natural := 0) is
   begin
      Put ("{Union: ");
      for J in Value.Fields'Range loop
         Put (Value.Fields (J).Name.all & " => ");
         if Value.Fields (J).Value /= null then
            Print (Value.Fields (J).Value.all, Indent + 6);
         end if;
         if J /= Value.Fields'Last then
            Put (", ");
            New_Line;
            Put (String'(1 .. Indent + 3 => ' '));
         end if;
      end loop;
      Put ("}");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Enum_Type; Indent : Natural := 0) is
   begin
      Put ("{Enumeration = ");
      if Value.Value = null then
         Put ("<Unknown>}");
      else
         Put (Value.Value.all & "}");
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Class_Type; Indent : Natural := 0) is
   begin
      Put ("{Class ");
      for A in Value.Ancestors'Range loop
         New_Line;
         Put (String'(1 .. Indent + 3 => ' '));
         Put ("Ancestor" & A'Img & " => ");

         if Value.Ancestors (A) = null then
            Put (" <unknown>");
         else
            Print (Value.Ancestors (A).all, Indent + 3);
         end if;
      end loop;

      New_Line;
      Put (String'(1 .. Indent + 3 => ' '));
      Put ("Child => ");

      Print (Value.Child.all, Indent + 3);
      Put ("}");
   end Print;

   ----------
   -- Free --
   ----------

   procedure Free (Item : access Simple_Type;
                   Only_Value : Boolean := False)
   is
      I : Generic_Type_Access := Generic_Type_Access (Item);
   begin
      if Item.Value /= null then
         Free (Item.Value);
      end if;
      if not Only_Value then
         Free_Internal (I);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Item : access Array_Type;
                   Only_Value : Boolean := False)
   is
      I : Generic_Type_Access := Generic_Type_Access (Item);
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
         Free_Internal (I);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Item : access Repeat_Type;
                   Only_Value : Boolean := False)
   is
      I : Generic_Type_Access := Generic_Type_Access (Item);
   begin
      if Item.Value /= null then
         --  Keep the structure of the item that is repeated, if required.
         Free (Item.Value, Only_Value);
      end if;
      if not Only_Value then
         Free_Internal (I);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Item : access Record_Type;
                   Only_Value : Boolean := False)
   is
      I : Generic_Type_Access := Generic_Type_Access (Item);
   begin
      for J in Item.Fields'Range loop
         if Item.Fields (J).Value /= null then
            Free (Item.Fields (J).Value, Only_Value);
            if Item.Fields (J).Variant_Part /= null then
               for V in Item.Fields (J).Variant_Part'Range loop
                  Free (Item.Fields (J).Variant_Part (V), Only_Value);
               end loop;
            end if;
            if not Only_Value then
               Free (Item.Fields (J).Name);
               Free (Item.Fields (J).Variant_Part);
            end if;
         end if;
      end loop;
      if not Only_Value then
         Free_Internal (I);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Item : access Class_Type;
                   Only_Value : Boolean := False)
   is
      I : Generic_Type_Access := Generic_Type_Access (Item);
   begin
      for A in Item.Ancestors'Range loop
         Free (Item.Ancestors (A), Only_Value);
      end loop;
      Free (Item.Child, Only_Value);
      if not Only_Value then
         Free_Internal (I);
      end if;
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Simple_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Simple_Type'(Value);
   begin
      Simple_Type_Access (R).Value := null;
      return R;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Range_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Range_Type'(Value);
   begin
      Simple_Type_Access (R).Value := null;
      return R;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Mod_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Mod_Type'(Value);
   begin
      Simple_Type_Access (R).Value := null;
      return R;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Access_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Access_Type'(Value);
   begin
      Simple_Type_Access (R).Value := null;
      return R;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Enum_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Enum_Type'(Value);
   begin
      Simple_Type_Access (R).Value := null;
      return R;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Array_Type)
                  return Generic_Type_Access
   is
      R : Array_Type_Access := new Array_Type'(Value);
   begin
      R.Values := null;
      R.Item_Type := Clone (Value.Item_Type.all);
      return Generic_Type_Access (R);
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Repeat_Type)
                  return Generic_Type_Access
   is
      R : Repeat_Type_Access := new Repeat_Type'(Value);
   begin
      --  duplicate the type of the repeated item.
      --  The value itself is in fact not duplicated, since the leafs of the
      --  type tree is a simple_type (or one of its children), that does not
      --  clone the value.
      R.Value := Clone (Value.Value.all);
      return Generic_Type_Access (R);
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Record_Type)
                  return Generic_Type_Access
   is
      R : Record_Type_Access := new Record_Type'(Value);
   begin
      for J in R.Fields'Range loop
         R.Fields (J).Name := new String'(Value.Fields (J).Name.all);
         --  Duplicate the type structure, but not the value itself.
         R.Fields (J).Value := Clone (Value.Fields (J).Value.all);
         if Value.Fields (J).Variant_Part /= null then
            R.Fields (J).Variant_Part :=
              new Record_Type_Array'(Value.Fields (J).Variant_Part.all);
            for V in R.Fields (J).Variant_Part'Range loop
               R.Fields (J).Variant_Part (V) := Record_Type_Access
                 (Clone (Value.Fields (J).Variant_Part (V).all));
            end loop;
         end if;
      end loop;
      return Generic_Type_Access (R);
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Union_Type)
                  return Generic_Type_Access
   is
      R : Union_Type_Access := new Union_Type'(Value);
   begin
      for J in R.Fields'Range loop
         R.Fields (J).Name := new String'(R.Fields (J).Name.all);
         R.Fields (J).Value := Clone (R.Fields (J).Value.all);
      end loop;
      return Generic_Type_Access (R);
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Class_Type) return Generic_Type_Access
   is
      R : Class_Type_Access := new Class_Type (Value.Num_Ancestors);
   begin
      for A in Value.Ancestors'Range loop
         R.Ancestors (A) :=
           Class_Type_Access (Clone (Value.Ancestors (A).all));
      end loop;
      R.Child := Record_Type_Access (Clone (Value.Child.all));
      return Generic_Type_Access (R);
   end Clone;

   --------------------
   -- Display_Pixmap --
   --------------------

   procedure Display_Pixmap (On_Pixmap : Gdk_Pixmap;
                             GC        : Gdk_GC;
                             Pixmap    : Gdk_Pixmap;
                             Mask      : Gdk_Bitmap;
                             X, Y      : Gint)
   is
   begin
      Set_Clip_Mask (GC, Mask);
      Set_Clip_Origin (GC, X, Y);
      Draw_Pixmap (On_Pixmap, GC, Pixmap, 0, 0, X, Y);
      Set_Clip_Mask (GC, Null_Pixmap);
      Set_Clip_Origin (GC, 0, 0);
   end Display_Pixmap;

   -----------
   -- Paint --
   -----------

   procedure Paint (Item    : in out Simple_Type;
                    GC      : Gdk.GC.Gdk_GC;
                    Xref_Gc : Gdk.GC.Gdk_GC;
                    Font    : Gdk.Font.Gdk_Font;
                    Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
                    X, Y    : Gint := 0)
   is
   begin
      Item.X := X;
      Item.Y := Y;

      if not Item.Valid or else Item.Value = null then
         Display_Pixmap (Pixmap, GC, Unknown_Pixmap, Unknown_Mask,
                         X + Border_Spacing, Y);
         return;
      end if;

      if Item.Selected then
         Draw_Rectangle (Pixmap,
                         GC,
                         Filled => True,
                         X      => X,
                         Y      => Y,
                         Width  => Item.Width,
                         Height => Item.Height);
         Set_Function (GC, Copy_Invert);
      end if;

      Draw_Text (Pixmap,
                 Font => Font,
                 GC   => GC,
                 X    => X,
                 Y    => Y + Get_Ascent (Font),
                 Text => Item.Value.all);

      if Item.Selected then
         Set_Function (GC, Copy);
      end if;
   end Paint;

   -----------
   -- Paint --
   -----------

   procedure Paint (Item    : in out Access_Type;
                    GC      : Gdk.GC.Gdk_GC;
                    Xref_Gc : Gdk.GC.Gdk_GC;
                    Font    : Gdk.Font.Gdk_Font;
                    Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
                    X, Y    : Glib.Gint := 0)
   is
   begin
      Item.X := X;
      Item.Y := Y;

      if not Item.Valid then
         Display_Pixmap (Pixmap, GC, Unknown_Pixmap, Unknown_Mask,
                         X + Border_Spacing, Y);
         return;
      end if;

      if Item.Selected then
        Draw_Rectangle (Pixmap,
                        GC,
                        Filled => True,
                        X      => X,
                        Y      => Y,
                        Width  => Item.Width,
                        Height => Item.Height);
        Set_Function (GC, Copy_Invert);
      end if;

      Draw_Text (Pixmap,
                 Font => Font,
                 GC   => Xref_Gc,
                 X    => X,
                 Y    => Y + Get_Ascent (Font),
                 Text => Item.Value.all);

      if Item.Selected then
        Set_Function (GC, Copy);
      end if;
   end Paint;

   -----------
   -- Paint --
   -----------

   procedure Paint (Item    : in out Array_Type;
                    GC      : Gdk.GC.Gdk_GC;
                    Xref_Gc : Gdk.GC.Gdk_GC;
                    Font    : Gdk.Font.Gdk_Font;
                    Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
                    X, Y    : Gint := 0)
   is
      Current_Y : Gint := Y + Border_Spacing;
      Arrow_Pos : constant Gint := X + Border_Spacing + Item.Index_Width
        +Left_Border - Text_Width (Font, String'(" => "));
   begin
      Item.X := X;
      Item.Y := Y;

      if Item.Dimensions (1).First > Item.Dimensions (1).Last then
         return;
      end if;

      if not Item.Valid then
         Display_Pixmap (Pixmap, GC, Unknown_Pixmap, Unknown_Mask,
                         X + Left_Border, Y);
         return;
      end if;

      if not Item.Visible then
         Display_Pixmap (Pixmap, GC, Hidden_Pixmap, Hidden_Mask,
                         X + Left_Border, Current_Y);
         return;
      end if;

      if Item.Selected then
        Draw_Rectangle (Pixmap,
                        GC,
                        Filled => True,
                        X      => X,
                        Y      => Y,
                        Width  => Item.Width,
                        Height => Item.Height);
        Set_Function (GC, Copy_Invert);
      end if;

      for V in Item.Values'Range loop

         Draw_Text (Pixmap,
                    Font => Font,
                    GC   => GC,
                    X    => X + Left_Border + Border_Spacing,
                    Y    => Current_Y + Get_Ascent (Font),
                    Text => Index_String (Item,
                                          Item.Values (V).Index,
                                          Item.Num_Dimensions));
         Draw_Text (Pixmap,
                    Font => Font,
                    GC   => GC,
                    X    => Arrow_Pos,
                    Y    => Current_Y + Get_Ascent (Font),
                    Text => " => ");
         Paint (Item.Values (V).Value.all, GC, Xref_Gc, Font, Pixmap,
                X + Left_Border + Border_Spacing + Item.Index_Width,
                Current_Y);
         Current_Y :=
           Current_Y + Item.Values (V).Value.Height + Line_Spacing;
      end loop;

      --  Draw a border
      Draw_Rectangle (Pixmap,
                      GC,
                      Filled => False,
                      X      => X,
                      Y      => Y,
                      Width  => Item.Width - 1,
                      Height => Item.Height - 1);

      if Item.Selected then
         Set_Function (GC, Copy);
      end if;
   end Paint;

   -----------
   -- Paint --
   -----------

   procedure Paint (Item    : in out Record_Type;
                    GC      : Gdk.GC.Gdk_GC;
                    Xref_Gc : Gdk.GC.Gdk_GC;
                    Font    : Gdk.Font.Gdk_Font;
                    Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
                    X, Y    : Gint := 0)
   is
      Current_Y : Gint := Y + Border_Spacing;
      Arrow_Pos : constant Gint :=
        X + Left_Border + Border_Spacing + Item.Gui_Fields_Width -
        Text_Width (Font, String'(" => "));
   begin
      Item.X := X;
      Item.Y := Y;

      if not Item.Valid then
         Display_Pixmap (Pixmap, GC, Unknown_Pixmap, Unknown_Mask,
                         X + Left_Border, Y);
         return;
      end if;

      --  A null record ?

      if Item.Fields'Length = 0 then
         return;
      end if;

      if not Item.Visible then
         Display_Pixmap (Pixmap, GC, Hidden_Pixmap, Hidden_Mask,
                         X + Left_Border, Current_Y);
         return;
      end if;

      if Item.Selected then
        Draw_Rectangle (Pixmap,
                        GC,
                        Filled => True,
                        X      => X,
                        Y      => Y,
                        Width  => Item.Width,
                        Height => Item.Height);
        Set_Function (GC, Copy_Invert);
      end if;

      for F in Item.Fields'Range loop

         Draw_Text (Pixmap,
                    Font => Font,
                    GC   => GC,
                    X    => X + Left_Border + Border_Spacing,
                    Y    => Current_Y + Get_Ascent (Font),
                    Text => Item.Fields (F).Name.all);
         Draw_Text (Pixmap,
                    Font => Font,
                    GC   => GC,
                    X    => Arrow_Pos,
                    Y    => Current_Y + Get_Ascent (Font),
                    Text => " => ");

         --  not a variant part ?

         if Item.Fields (F).Value /= null then
            Paint (Item.Fields (F).Value.all, GC, Xref_Gc, Font, Pixmap,
                   X + Left_Border + Border_Spacing + Item.Gui_Fields_Width,
                   Current_Y);
            Current_Y :=
              Current_Y + Item.Fields (F).Value.Height + Line_Spacing;
         end if;

         --  a variant part ?

         if Item.Fields (F).Variant_Part /= null then
            for V in Item.Fields (F).Variant_Part'Range loop
               Paint (Item.Fields (F).Variant_Part (V).all, GC, Xref_Gc, Font,
                      Pixmap,
                      X + Left_Border + Border_Spacing + Item.Gui_Fields_Width,
                      Current_Y);
               Current_Y := Current_Y +
                 Item.Fields (F).Variant_Part (V).Height + Line_Spacing;
            end loop;
         end if;
      end loop;

      --  Draw a border
      Draw_Rectangle (Pixmap,
                      GC,
                      Filled => False,
                      X      => X,
                      Y      => Y,
                      Width  => Item.Width - 1,
                      Height => Item.Height - 1);

      if Item.Selected then
        Set_Function (GC, Copy);
      end if;
   end Paint;

   -----------
   -- Paint --
   -----------

   procedure Paint (Item    : in out Repeat_Type;
                    GC      : Gdk.GC.Gdk_GC;
                    Xref_Gc : Gdk.GC.Gdk_GC;
                    Font    : Gdk.Font.Gdk_Font;
                    Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
                    X, Y    : Gint := 0)
   is
      Str : String := "<repeat " & Integer'Image (Item.Repeat_Num) & "> ";
   begin
      Item.X := X;
      Item.Y := Y;

      if not Item.Valid then
         Display_Pixmap (Pixmap, GC, Unknown_Pixmap, Unknown_Mask,
                         X + Border_Spacing, Y);
         return;
      end if;

      if Item.Selected then
        Draw_Rectangle (Pixmap,
                        GC,
                        Filled => True,
                        X      => X,
                        Y      => Y,
                        Width  => Item.Width,
                        Height => Item.Height);
        Set_Function (GC, Copy_Invert);
      end if;

      Draw_Text (Pixmap,
                 Font => Font,
                 GC   => GC,
                 X    => X + Border_Spacing,
                 Y    => Y + Border_Spacing + Get_Ascent (Font),
                 Text => Str);

      Paint (Item.Value.all, GC, Xref_Gc, Font, Pixmap,
             X + Text_Width (Font, Str), Y + Border_Spacing);

      --  Draw a border
      Draw_Rectangle (Pixmap,
                      GC,
                      Filled => False,
                      X      => X,
                      Y      => Y,
                      Width  => Item.Width - 1,
                      Height => Item.Height - 1);

      if Item.Selected then
        Set_Function (GC, Copy);
      end if;
   end Paint;

   -----------
   -- Paint --
   -----------

   procedure Paint (Item    : in out Class_Type;
                    GC      : Gdk.GC.Gdk_GC;
                    Xref_Gc : Gdk.GC.Gdk_GC;
                    Font    : Gdk.Font.Gdk_Font;
                    Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
                    X, Y    : Glib.Gint := 0)
   is
      Current_Y : Gint := Y;
   begin
      Item.X := X;
      Item.Y := Y;

      if not Item.Valid
        or else (Item.Ancestors'Length = 0
                 and then not Item.Child.Valid)
      then
         Display_Pixmap (Pixmap, GC, Unknown_Pixmap, Unknown_Mask,
                         X + Left_Border, Y + Border_Spacing);
         return;
      end if;

      if not Item.Visible then
         Display_Pixmap (Pixmap, GC, Hidden_Pixmap, Hidden_Mask,
                         X + Left_Border, Current_Y);
         return;
      end if;

      if Item.Selected then
        Draw_Rectangle (Pixmap,
                        GC,
                        Filled => True,
                        X      => X,
                        Y      => Y,
                        Width  => Item.Width,
                        Height => Item.Height);
        Set_Function (GC, Copy_Invert);
      end if;

      for A in Item.Ancestors'Range loop

         --  Draw the ancestor if it isn't a null record.

         if Item.Ancestors (A) /= null
           and then Item.Ancestors (A).Height /= 0
         then
            --  Do not add Left_Border to X, since each of the ancestor is
            --  itself a Class_Type and will already draw it.
            Paint (Item.Ancestors (A).all, GC, Xref_Gc, Font, Pixmap,
                   X, Current_Y);
            Current_Y := Current_Y + Item.Ancestors (A).Height + Line_Spacing;
         end if;
      end loop;

      if Item.Child.Height /= 0 then
         Paint (Item.Child.all, GC, Xref_Gc, Font, Pixmap,
                X + Left_Border, Current_Y);
      end if;

      if Item.Selected then
        Set_Function (GC, Copy);
      end if;
   end Paint;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Item           : in out Simple_Type;
      Font           : Gdk.Font.Gdk_Font;
      Hide_Big_Items : Boolean := False)
   is
   begin
      if Item.Valid and then Item.Value /= null then
         Item.Width  := Text_Width (Font, Item.Value.all);
         Item.Height := Get_Ascent (Font) + Get_Descent (Font);
      else
         Item.Width := Unknown_Width;
         Item.Height := Unknown_Height;
      end if;
   end Size_Request;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Item           : in out Array_Type;
      Font           : Gdk.Font.Gdk_Font;
      Hide_Big_Items : Boolean := False)
   is
      Total_Height, Total_Width : Gint := 0;
   begin
      if not Item.Valid then
         Item.Width := Unknown_Width;
         Item.Height := Unknown_Height;
         return;
      end if;

      --  Empty arrays
      if Item.Dimensions (1).First > Item.Dimensions (1).Last then
         Item.Width := 20;
         Item.Height := 0;
         return;
      end if;

      if Item.Visible then
         Item.Index_Width := 20;  --  minimal width
         if Item.Values /= null then
            for V in Item.Values'Range loop
               Size_Request (Item.Values (V).Value.all, Font, Hide_Big_Items);
               Total_Width  :=
                 Gint'Max (Total_Width, Item.Values (V).Value.Width);
               Total_Height := Total_Height + Item.Values (V).Value.Height;
               Item.Index_Width :=
                 Gint'Max (Item.Index_Width, String_Width
                           (Font,
                            Index_String (Item, Item.Values (V).Index,
                                          Item.Num_Dimensions)));
            end loop;
            Total_Height :=
              Total_Height + (Item.Values'Length - 1) * Line_Spacing;
         end if;

         Item.Index_Width :=
           Item.Index_Width + Text_Width (Font, String'(" => "));

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

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Item           : in out Record_Type;
      Font           : Gdk.Font.Gdk_Font;
      Hide_Big_Items : Boolean := False)
   is
      Total_Height, Total_Width : Gint := 0;
      Largest_Name : String_Access := null;
   begin

      if not Item.Valid then
         Item.Width := Unknown_Width;
         Item.Height := Unknown_Height;
         return;
      end if;

      --  null record ?

      if Item.Fields'Length = 0 then
         Item.Gui_Fields_Width := 0;
         Item.Width := 0;
         Item.Height := 0;
         return;
      end if;

      if Item.Visible then
         for F in Item.Fields'Range loop
            if Largest_Name = null
              or else Item.Fields (F).Name.all'Length > Largest_Name'Length
            then
               Largest_Name := Item.Fields (F).Name;
            end if;

            --  not a variant part ?

            if Item.Fields (F).Value /= null then
               Size_Request (Item.Fields (F).Value.all, Font, Hide_Big_Items);

               Total_Width  :=
                 Gint'Max (Total_Width, Item.Fields (F).Value.Width);
               Total_Height :=
                 Total_Height + Item.Fields (F).Value.Height;
            end if;

            --  a variant part ?

            if Item.Fields (F).Variant_Part /= null then
               for V in Item.Fields (F).Variant_Part'Range loop
                  Size_Request (Item.Fields (F).Variant_Part (V).all, Font,
                                Hide_Big_Items);

                  Total_Width  := Gint'Max
                    (Total_Width,
                     Item.Fields (F).Variant_Part (V).Width);
                  Total_Height := Total_Height
                    + Item.Fields (F).Variant_Part (V).Height;
               end loop;
               Total_Height := Total_Height +
                 (Item.Fields (F).Variant_Part'Length - 1) * Line_Spacing;
            end if;
         end loop;

         Total_Height := Total_Height
           + (Item.Fields'Length - 1) * Line_Spacing;

         if Largest_Name = null then
            Item.Gui_Fields_Width := Text_Width (Font, String' (" => "));
         else
            Item.Gui_Fields_Width :=
              Text_Width (Font, Largest_Name.all & " => ");
         end if;

         --  Keep enough space for the border (Border_Spacing on each side)
         Item.Width  := Total_Width + Item.Gui_Fields_Width + Left_Border
           + 2 * Border_Spacing;
         Item.Height := Total_Height + 2 * Border_Spacing;

         if Hide_Big_Items and then Item.Height > Big_Item_Height then
            Item.Visible := False;
         end if;
      end if;

      if not Item.Visible then
         Item.Gui_Fields_Width := 0;
         Item.Width := Left_Border + 2 * Border_Spacing + Hidden_Width;
         Item.Height := 2 * Border_Spacing + Hidden_Height;
      end if;
   end Size_Request;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Item           : in out Repeat_Type;
      Font           : Gdk.Font.Gdk_Font;
      Hide_Big_Items : Boolean := False)
   is
      Str : String := "<repeat " & Integer'Image (Item.Repeat_Num) & "> ";
   begin
      if not Item.Valid then
         Item.Width := Unknown_Width;
         Item.Height := Unknown_Height;
      else
         Size_Request (Item.Value.all, Font, Hide_Big_Items);
         Item.Width :=
           Item.Value.Width + Text_Width (Font, Str) + 2 * Border_Spacing;
         Item.Height :=
           Gint'Max (Item.Value.Height, Get_Ascent (Font) + Get_Descent (Font))
           + 2 * Border_Spacing;
      end if;
   end Size_Request;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Item           : in out Class_Type;
      Font           : Gdk.Font.Gdk_Font;
      Hide_Big_Items : Boolean := False)
   is
      Total_Height, Total_Width : Gint := 0;
   begin

      if not Item.Valid then
         Item.Width := Unknown_Width;
         Item.Height := Unknown_Height;
         return;
      end if;

      if Item.Visible then
         for A in Item.Ancestors'Range loop
            if Item.Ancestors (A) /= null then
               Size_Request (Item.Ancestors (A).all, Font, Hide_Big_Items);

               --  If we don't have an null record
               if Item.Ancestors (A).Height /= 0 then
                  Total_Height := Total_Height + Item.Ancestors (A).Height
                    + Line_Spacing;
               end if;
               Total_Width := Gint'Max (Total_Width, Item.Ancestors (A).Width);
            end if;
         end loop;

         Size_Request (Item.Child.all, Font, Hide_Big_Items);

         Total_Width := Gint'Max (Total_Width, Item.Child.Width) + Left_Border;
         Item.Child.Width := Total_Width;

         --  Dont print an extra border around, since each ancestors and child
         --  are records and already have their own borders.
         Item.Width  := Total_Width;
         Item.Height := Total_Height + Item.Child.Height;

         if Hide_Big_Items and then Item.Height > Big_Item_Height then
            Item.Visible := False;
         end if;
      end if;

      if not Item.Visible then
         Item.Width := Left_Border + 2 * Border_Spacing + Hidden_Width;
         Item.Height := 2 * Border_Spacing + Hidden_Height;
      end if;
   end Size_Request;

   ---------------------
   -- Propagate_Width --
   ---------------------

   procedure Propagate_Width (Item  : in out Generic_Type;
                              Width : Glib.Gint)
   is
   begin
      Item.Width := Width;
   end Propagate_Width;

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

   ---------------------
   -- Propagate_Width --
   ---------------------

   procedure Propagate_Width (Item  : in out Record_Type;
                              Width : Glib.Gint)
   is
      W : constant Gint := Width - Item.Gui_Fields_Width - 2 * Border_Spacing
        - Left_Border;
   begin
      Item.Width := Width;
      if Item.Visible then
         for F in Item.Fields'Range loop
            if Item.Fields (F).Value /= null then
               Propagate_Width (Item.Fields (F).Value.all, W);
            else
               for V in Item.Fields (F).Variant_Part'Range loop
                  Propagate_Width (Item.Fields (F).Variant_Part (V).all, W);
               end loop;
            end if;
         end loop;
      end if;
   end Propagate_Width;

   ---------------------
   -- Propagate_Width --
   ---------------------

   procedure Propagate_Width (Item  : in out Class_Type;
                              Width : Glib.Gint)
   is
   begin
      Item.Width := Width;
      if Item.Visible then
         for A in Item.Ancestors'Range loop
            Propagate_Width (Item.Ancestors (A).all, Width);
         end loop;
         Propagate_Width (Item.Child.all, Width - Left_Border);
      end if;
   end Propagate_Width;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   function Get_Component_Name (Item : access Simple_Type;
                                Lang : access Language_Root'Class;
                                Name : String;
                                X, Y : Glib.Gint)
                               return String
   is
   begin
      return Name;
   end Get_Component_Name;

   -------------------
   -- Get_Component --
   -------------------

   function Get_Component (Item : access Simple_Type;
                           X, Y : Glib.Gint)
                          return Generic_Type_Access
   is
   begin
      return Generic_Type_Access (Item);
   end Get_Component;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   function Get_Component_Name (Item : access Array_Type;
                                Lang : access Language_Root'Class;
                                Name : String;
                                X, Y : Glib.Gint)
                               return String
   is
      Total_Height : Gint := Border_Spacing;
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
        or else Item.Values'Length = 1
      then
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
      Total_Height : Gint := Border_Spacing;
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

   ------------------------
   -- Get_Component_Name --
   ------------------------

   function Get_Component_Name (Item : access Record_Type;
                                Lang : access Language_Root'Class;
                                Name : String;
                                X, Y : Glib.Gint)
                               return String
   is
      Total_Height : Gint := Border_Spacing;
      Tmp_Height   : Gint;
      Field_Name_Start : constant Gint := Left_Border + Border_Spacing;
      Field_Start  : constant Gint := Field_Name_Start + Item.Gui_Fields_Width;
   begin
      if not Item.Visible then
         return Name;
      end if;

      --  Click in the left column ? => Select the whole item

      if X < Field_Name_Start then
         return Name;
      end if;

      --  Else, find the relevant item

      for F in Item.Fields'Range loop
         Tmp_Height := Total_Height + Item.Fields (F).Value.Height
           + Line_Spacing;
         if Y <= Tmp_Height then
            declare
               Field_Name : constant String :=
                 Record_Field_Name (Lang, Name, Item.Fields (F).Name.all);
            begin
               if X < Field_Start then
                  return Field_Name;
               end if;

               return Get_Component_Name
                 (Item.Fields (F).Value, Lang, Field_Name,
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

   function Get_Component (Item : access Record_Type;
                           X, Y : Glib.Gint)
                          return Generic_Type_Access
   is
      Total_Height : Gint := Border_Spacing;
      Tmp_Height   : Gint;
      Field_Name_Start : constant Gint := Left_Border + Border_Spacing;
      Field_Start  : constant Gint := Field_Name_Start + Item.Gui_Fields_Width;
   begin
      if not Item.Valid or else not Item.Visible then
         return Generic_Type_Access (Item);
      end if;

      --  Click in the left column ? => Select the whole item

      if X < Field_Name_Start then
         return Generic_Type_Access (Item);
      end if;

      --  Else, find the relevant item

      for F in Item.Fields'Range loop
         Tmp_Height := Total_Height + Item.Fields (F).Value.Height
           + Line_Spacing;
         if Y <= Tmp_Height then
            if X < Field_Start then
               return Item.Fields (F).Value;
            end if;

            return Get_Component
              (Item.Fields (F).Value, X - Field_Start, Y - Total_Height);
         end if;
         Total_Height := Tmp_Height;
      end loop;

      return Generic_Type_Access (Item);
   end Get_Component;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   function Get_Component_Name (Item : access Class_Type;
                                Lang : access Language_Root'Class;
                                Name : String;
                                X, Y : Glib.Gint)
                               return String
   is
      Total_Height : Gint := 0;
   begin
      if not Item.Visible then
         return Name;
      end if;

      --  Click in the left column ? => Select the whole item

      if X <= Left_Border then
         return Name;
      end if;

      for A in Item.Ancestors'Range loop
         if Y <= Total_Height + Item.Ancestors (A).Height then
            --  Do not substract Left_Border from X, since the ancestor is
            --  a Class_Type and already has it.
            return Get_Component_Name
              (Item.Ancestors (A), Lang, Name, X, Y - Total_Height);
         end if;
         Total_Height := Total_Height + Item.Ancestors (A).Height;
      end loop;

      return Get_Component_Name
        (Item.Child, Lang, Name, X - Left_Border, Y - Total_Height);
   end Get_Component_Name;

   -------------------
   -- Get_Component --
   -------------------

   function Get_Component (Item : access Class_Type;
                           X, Y : Glib.Gint)
                          return Generic_Type_Access
   is
      Total_Height : Gint := 0;
   begin
      if not Item.Valid or else not Item.Visible then
         return Generic_Type_Access (Item);
      end if;

      --  Click in the left column ? => Select the whole item

      if X < Left_Border then
         return Generic_Type_Access (Item);
      end if;

      for A in Item.Ancestors'Range loop
         if Y <= Total_Height + Item.Ancestors (A).Height then
            return Get_Component (Item.Ancestors (A), X, Y - Total_Height);
         end if;
         Total_Height := Total_Height + Item.Ancestors (A).Height;
      end loop;

      return Get_Component (Item.Child, X - Left_Border, Y - Total_Height);
   end Get_Component;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   function Get_Component_Name (Item : access Repeat_Type;
                                Lang : access Language_Root'Class;
                                Name : String;
                                X, Y : Glib.Gint)
                               return String
   is
   begin
      return Name;
   end Get_Component_Name;

   -------------------
   -- Get_Component --
   -------------------

   function Get_Component (Item : access Repeat_Type;
                           X, Y : Glib.Gint)
                          return Generic_Type_Access
   is
   begin
      return Generic_Type_Access (Item);
   end Get_Component;

   ------------------
   -- Set_Selected --
   ------------------

   procedure Set_Selected (Item     : access Generic_Type;
                           Selected : Boolean := True)
   is
   begin
      Item.Selected := Selected;
   end Set_Selected;

   ------------------
   -- Get_Selected --
   ------------------

   function Get_Selected (Item : access Generic_Type) return Boolean is
   begin
      return Item.Selected;
   end Get_Selected;

   -------------
   -- Replace --
   -------------

   function Replace
     (Parent       : access Simple_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class)
     return Generic_Type_Access
   is
   begin
      return null;
   end Replace;

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

   -------------
   -- Replace --
   -------------

   function Replace
     (Parent       : access Repeat_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class)
     return Generic_Type_Access
   is
   begin
      if Parent.Value = Generic_Type_Access (Current) then
         Free (Parent.Value, Only_Value => False);
         Parent.Value := Generic_Type_Access (Replace_With);
         return Generic_Type_Access (Replace_With);
      end if;
      return null;
   end Replace;

   -------------
   -- Replace --
   -------------

   function Replace
     (Parent       : access Record_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class)
     return Generic_Type_Access
   is
   begin
      for F in Parent.Fields'Range loop
         if Parent.Fields (F).Value = Generic_Type_Access (Current) then
            Free (Parent.Fields (F).Value, Only_Value => False);
            Parent.Fields (F).Value := Generic_Type_Access (Replace_With);
            return Generic_Type_Access (Replace_With);
         end if;

         if Parent.Fields (F).Variant_Part /= null then
            for V in Parent.Fields (F).Variant_Part'Range loop
               if Generic_Type_Access (Parent.Fields (F).Variant_Part (V)) =
                 Generic_Type_Access (Current)
               then
                  Free (Parent.Fields (F).Variant_Part (V),
                        Only_Value => False);
                  Parent.Fields (F).Variant_Part (V) :=
                    Record_Type_Access (Replace_With);
                  return Generic_Type_Access (Replace_With);
               end if;
            end loop;
         end if;
      end loop;
      return null;
   end Replace;

   -------------
   -- Replace --
   -------------

   function Replace
     (Parent       : access Class_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class)
     return Generic_Type_Access
   is
   begin
      for A in Parent.Ancestors'Range loop
         if Parent.Ancestors (A) = Class_Type_Access (Current) then
            Free (Parent.Ancestors (A), Only_Value => False);
            Parent.Ancestors (A) := Class_Type_Access (Replace_With);
            return Generic_Type_Access (Replace_With);
         end if;
      end loop;
      if Parent.Child = Record_Type_Access (Current) then
         Free (Parent.Child, Only_Value => False);
         Parent.Child := Record_Type_Access (Replace_With);
         return Generic_Type_Access (Replace_With);
      end if;
      return null;
   end Replace;

end Generic_Values;
