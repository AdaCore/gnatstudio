-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Glib;         use Glib;
with Gdk.Font;     use Gdk.Font;
with Gdk.Drawable; use Gdk.Drawable;
with Gdk.GC;       use Gdk.GC;
with Gdk.Types;    use Gdk.Types;
with Gdk.Types;    use Gdk.Types;
with Gdk.Window;   use Gdk.Window;

with Language;        use Language;
with GVD.Types;       use GVD.Types;
with GVD.Preferences; use GVD.Preferences;

with Unchecked_Deallocation;

package body Items.Records is

   ---------------------
   -- New_Record_Type --
   ---------------------

   function New_Record_Type
     (Num_Fields : Natural) return Generic_Type_Access
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

   procedure Set_Field_Name
     (Item          : in out Record_Type;
      Index         : Positive;
      Name          : String;
      Variant_Parts : Natural := 0) is
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

   function Get_Field_Name
     (Item  : in Record_Type;
      Index : Positive) return String_Access is
   begin
      return Item.Fields (Index).Name;
   end Get_Field_Name;

   -----------------------
   -- Set_Variant_Field --
   -----------------------

   procedure Set_Variant_Field
     (Item          : in out Record_Type;
      Index         : Positive;
      Variant_Index : Positive;
      Value         : access Record_Type'Class) is
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

   function Get_Variant_Parts
     (Item  : Record_Type;
      Field : Positive) return Natural is
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

   function Find_Variant_Part
     (Item     : Record_Type;
      Field    : Positive;
      Contains : String) return Generic_Type_Access is
   begin
      for J in Item.Fields (Field).Variant_Part'Range loop
         Item.Fields (Field).Variant_Part (J).Valid := False;
      end loop;

      for J in Item.Fields (Field).Variant_Part'Range loop
         if (Contains'Length = 0
             and then Item.Fields (Field).Variant_Part (J).Fields'Length = 0)
           or else
           Item.Fields (Field).Variant_Part (J).Fields (1).Name.all =
           Contains
         then
            Item.Fields (Field).Variant_Part (J).Valid := True;
            return Generic_Type_Access (Item.Fields (Field).Variant_Part (J));
         end if;
      end loop;

      return null;
   end Find_Variant_Part;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Item  : in out Record_Type;
      Value : access Generic_Type'Class;
      Field : String) is
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

   procedure Set_Value
     (Item  : in out Record_Type;
      Value : access Generic_Type'Class;
      Field : Positive) is
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

   function Get_Value
     (Item  : Record_Type;
      Field : String) return Generic_Type_Access is
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

   function Get_Value
     (Item  : Record_Type;
      Field : Positive) return Generic_Type_Access is
   begin
      return Item.Fields (Field).Value;
   end Get_Value;

   --------------------
   -- New_Union_Type --
   --------------------

   function New_Union_Type
     (Num_Fields : Positive) return Generic_Type_Access is
   begin
      return new Union_Type (Num_Fields);
   end New_Union_Type;

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

   ----------
   -- Free --
   ----------

   procedure Free
     (Item : access Record_Type;
      Only_Value : Boolean := False) is
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

      Free (Generic_Type (Item.all)'Access, Only_Value);
   end Free;

   -----------------------
   -- Clone_Dispatching --
   -----------------------

   procedure Clone_Dispatching
     (Item  : Record_Type;
      Clone : out Generic_Type_Access)
   is
      R : Record_Type_Access := Record_Type_Access (Clone);
   begin
      Clone_Dispatching (Generic_Type (Item), Clone);

      for J in Item.Fields'Range loop
         R.Fields (J).Name := new String'(Item.Fields (J).Name.all);
         --  Duplicate the type structure, but not the value itself.
         R.Fields (J).Value := Items.Clone (Item.Fields (J).Value.all);

         if Item.Fields (J).Variant_Part /= null then
            R.Fields (J).Variant_Part :=
              new Record_Type_Array'(Item.Fields (J).Variant_Part.all);
            for V in Item.Fields (J).Variant_Part'Range loop
               R.Fields (J).Variant_Part (V) := Record_Type_Access
                 (Items.Clone (Item.Fields (J).Variant_Part (V).all));
            end loop;
         end if;
      end loop;
   end Clone_Dispatching;

   -----------
   -- Paint --
   -----------

   procedure Paint
     (Item    : in out Record_Type;
      Context : Drawing_Context;
      X, Y    : Gint := 0)
   is
      procedure Print_Field_Name (F : Integer);
      --  Print the name of the field F and an arrow

      Current_Y : Gint := Y + Item.Border_Spacing;
      Arrow_Pos : constant Gint :=
        X + Left_Border + Item.Border_Spacing + Item.Gui_Fields_Width -
        GVD_Text_Width (Context.Font, String'(" => "));

      ----------------------
      -- Print_Field_Name --
      ----------------------

      procedure Print_Field_Name (F : Integer) is
      begin
         if Context.Font /= null then
            Draw_Text
              (Context.Pixmap,
               Font => Context.Font,
               GC   => Context.GC,
               X    => X + Left_Border + Item.Border_Spacing,
               Y    => Current_Y + Get_Ascent (Context.Font),
               Text => Item.Fields (F).Name.all);
            Draw_Text
              (Context.Pixmap,
               Font => Context.Font,
               GC   => Context.GC,
               X    => Arrow_Pos,
               Y    => Current_Y + Get_Ascent (Context.Font),
               Text => " => ");
         end if;
      end Print_Field_Name;

   begin
      Item.X := X;
      Item.Y := Y;

      if not Item.Valid then
         Display_Pixmap
           (Context.Pixmap, Context.GC, Context.Unknown_Pixmap,
            Context.Unknown_Mask, X + Left_Border, Y);
         return;
      end if;

      --  A null record ?

      if Item.Num_Fields = 0 then
         return;
      end if;

      if not Item.Visible then
         Display_Pixmap
           (Context.Pixmap, Context.GC, Context.Hidden_Pixmap,
            Context.Hidden_Mask, X + Left_Border, Current_Y);
         return;
      end if;

      if Item.Selected then
         Draw_Rectangle
           (Context.Pixmap,
            Context.Selection_GC,
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
         Draw_Text
           (Context.Pixmap,
            Font => Context.Type_Font,
            GC   => Context.GC,
            X    => X + Left_Border + Item.Border_Spacing,
            Y    => Current_Y + Get_Ascent (Context.Type_Font),
            Text => Get_Type_Name (Item'Access, Context));
         Current_Y := Current_Y +
           Get_Ascent (Context.Type_Font) +
           Get_Descent (Context.Type_Font);
      end if;

      for F in Item.Fields'Range loop
         --  not a variant part ?

         if Item.Fields (F).Value /= null then
            Paint
              (Item.Fields (F).Value.all, Context,
               X + Left_Border + Item.Border_Spacing
               + Item.Gui_Fields_Width,
               Current_Y);
            Print_Field_Name (F);
            Current_Y :=
              Current_Y + Item.Fields (F).Value.Height + Line_Spacing;
         end if;

         --  a variant part ?

         if Item.Fields (F).Variant_Part /= null then
            for V in Item.Fields (F).Variant_Part'Range loop
               if Item.Fields (F).Variant_Part (V).Valid then
                  Paint
                    (Item.Fields (F).Variant_Part (V).all, Context,
                     X + Left_Border + Item.Border_Spacing
                     + Item.Gui_Fields_Width,
                     Current_Y);
                  if Item.Fields (F).Variant_Part (V).Num_Fields > 0 then
                     Print_Field_Name (F);
                  end if;
                  Current_Y := Current_Y +
                    Item.Fields (F).Variant_Part (V).Height + Line_Spacing;
               end if;
            end loop;
         end if;
      end loop;

      --  Draw a border
      if Item.Border_Spacing /= 0 then
         Draw_Rectangle
           (Context.Pixmap,
            Context.GC,
            Filled => False,
            X      => X,
            Y      => Y,
            Width  => Item.Width - 1,
            Height => Item.Height - 1);
      end if;

      if Item.Selected then
         Set_Function (Context.GC, Copy);
      end if;
   end Paint;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Item           : in out Record_Type;
      Context        : Drawing_Context;
      Hide_Big_Items : Boolean := False)
   is
      Total_Height, Total_Width : Gint := 0;
      Largest_Name : String_Access := null;

   begin
      if not Item.Valid then
         Get_Size (Context.Unknown_Pixmap, Item.Width, Item.Height);
         return;
      end if;

      --  null record ?

      if Item.Fields'Length = 0 then
         Item.Gui_Fields_Width := 0;
         Item.Width := 0;
         Item.Height := 0;
         return;
      end if;

      if Show_Type (Context.Mode)
        and then Item.Type_Name /= null
      then
         Item.Type_Height := GVD_Font_Height (Context.Type_Font);
         Total_Height := Total_Height + Item.Type_Height;
         Total_Width := Gint'Max
           (Total_Width,
            GVD_Text_Width (Context.Type_Font,
                            Get_Type_Name (Item'Access, Context)));

      else
         Item.Type_Height := 0;
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
               Size_Request
                 (Item.Fields (F).Value.all, Context, Hide_Big_Items);

               Total_Width  :=
                 Gint'Max (Total_Width, Item.Fields (F).Value.Width);

               --  Keep at least enough space to print the field name
               Item.Fields (F).Value.Height := Gint'Max
                 (Item.Fields (F).Value.Height,
                  GVD_Font_Height (Context.Font));
               Total_Height := Total_Height + Item.Fields (F).Value.Height;
            end if;

            --  a variant part ?

            if Item.Fields (F).Variant_Part /= null then
               for V in Item.Fields (F).Variant_Part'Range loop
                  if Item.Fields (F).Variant_Part (V).Valid then
                     Size_Request
                       (Item.Fields (F).Variant_Part (V).all, Context,
                        Hide_Big_Items);
                     Total_Width  := Gint'Max
                       (Total_Width,
                        Item.Fields (F).Variant_Part (V).Width);
                     Total_Height := Total_Height +
                       Item.Fields (F).Variant_Part (V).Height;
                  end if;
               end loop;
            end if;
         end loop;

         Total_Height := Total_Height +
           (Item.Fields'Length - 1) * Line_Spacing;

         if Largest_Name = null then
            Item.Gui_Fields_Width :=
              GVD_Text_Width (Context.Font, String' (" => "));
         else
            Item.Gui_Fields_Width :=
              GVD_Text_Width (Context.Font, Largest_Name.all & " => ");
         end if;

         --  Keep enough space for the border (Border_Spacing on each side)
         Item.Width  := Total_Width + Item.Gui_Fields_Width + Left_Border
           + 2 * Item.Border_Spacing;
         Item.Height := Total_Height + 2 * Item.Border_Spacing;

         if Hide_Big_Items
           and then Item.Height > Get_Pref (Big_Item_Height)
         then
            Item.Visible := False;
         end if;
      end if;

      if not Item.Visible then
         Item.Gui_Fields_Width := 0;
         Get_Size (Context.Hidden_Pixmap, Item.Width, Item.Height);
         Item.Width := Left_Border + 2 * Item.Border_Spacing + Item.Width;
         Item.Height := 2 * Item.Border_Spacing + Item.Height;
      end if;
   end Size_Request;

   ---------------------
   -- Propagate_Width --
   ---------------------

   procedure Propagate_Width
     (Item  : in out Record_Type;
      Width : Glib.Gint)
   is
      W : constant Gint := Width - Item.Gui_Fields_Width - Left_Border
        - 2 * Item.Border_Spacing;
      Iter : Generic_Iterator'Class := Start (Item'Unrestricted_Access);
      It   : Generic_Type_Access;
   begin
      Item.Width := Width;

      if Item.Visible then
         while not At_End (Iter) loop
            It := Data (Iter);

            if It /= null then
               Propagate_Width (It.all, W);
            end if;

            Next (Iter);
         end loop;
      end if;
   end Propagate_Width;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   function Get_Component_Name
     (Item : access Record_Type;
      Lang : access Language_Root'Class;
      Name : String;
      X, Y : Glib.Gint) return String
   is
      Total_Height : Gint := Item.Border_Spacing + Item.Type_Height;
      Tmp_Height   : Gint;
      Field_Name_Start : constant Gint := Left_Border + Item.Border_Spacing;
      Field_Start  : constant Gint := Field_Name_Start + Item.Gui_Fields_Width;
   begin
      if not Item.Visible then
         return Name;
      end if;

      --  Click in the left column ? => Select the whole item

      if X < Field_Name_Start then
         return Name;
      end if;

      --  Did we click the type of the item

      if Y < Item.Type_Height then
         return Name;
      end if;

      --  Else, find the relevant item

      for F in Item.Fields'Range loop
         if Item.Fields (F).Value /= null then
            Tmp_Height := Total_Height + Item.Fields (F).Value.Height +
              Line_Spacing;

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
         end if;

         if Item.Fields (F).Variant_Part /= null then
            for V in Item.Fields (F).Variant_Part'Range loop
               Tmp_Height := Total_Height +
                 Item.Fields (F).Variant_Part (V).Height + Line_Spacing;

               if Y <= Tmp_Height then
                  if X < Field_Start then
                     return Name;
                  end if;

                  return Get_Component_Name
                    (Item.Fields (F).Variant_Part (V), Lang, Name,
                     X - Field_Start, Y - Total_Height);
               end if;

               Total_Height := Tmp_Height;
            end loop;
         end if;
      end loop;

      return Name;
   end Get_Component_Name;

   -------------------
   -- Get_Component --
   -------------------

   function Get_Component
     (Item : access Record_Type; X, Y : Glib.Gint) return Generic_Type_Access
   is
      Total_Height : Gint := Item.Border_Spacing + Item.Type_Height;
      Tmp_Height   : Gint;
      Field_Name_Start : constant Gint := Left_Border + Item.Border_Spacing;
      Field_Start  : constant Gint := Field_Name_Start + Item.Gui_Fields_Width;
      Iter         : Generic_Iterator'Class := Start (Item);
      It           : Generic_Type_Access;
   begin
      if not Item.Valid or else not Item.Visible then
         return Generic_Type_Access (Item);
      end if;

      --  Click in the left column ? => Select the whole item

      if X < Field_Name_Start then
         return Generic_Type_Access (Item);
      end if;

      --  Did we click the type of the item

      if Y < Item.Type_Height then
         return Generic_Type_Access (Item);
      end if;

      --  Else, find the relevant item

      while not At_End (Iter) loop
         It := Data (Iter);

         if It /= null then
            Tmp_Height := Total_Height + It.Height + Line_Spacing;

            if Y <= Tmp_Height then
               if X < Field_Start then
                  return It;
               end if;

               return Get_Component (It, X - Field_Start, Y - Total_Height);
            end if;

            Total_Height := Tmp_Height;
         end if;

         Next (Iter);
      end loop;

      return Generic_Type_Access (Item);
   end Get_Component;

   -------------
   -- Replace --
   -------------

   function Replace
     (Parent       : access Record_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class) return Generic_Type_Access is
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

   -----------
   -- Start --
   -----------

   function Start (Item : access Record_Type) return Generic_Iterator'Class is
      Iter : Record_Iterator;
   begin
      Iter.Item := Record_Type_Access (Item);
      Iter.Field := Item.Fields'First;
      Iter.Variant := Natural'Last;
      return Iter;
   end Start;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Record_Iterator) is
   begin
      if Iter.Item.Fields (Iter.Field).Variant_Part /= null then
         if Iter.Variant = Natural'Last then
            Iter.Variant := Iter.Item.Fields (Iter.Field).Variant_Part'First;
         else
            Iter.Variant := Iter.Variant + 1;
         end if;

         if Iter.Variant >
           Iter.Item.Fields (Iter.Field).Variant_Part'Last
         then
            Iter.Variant := Natural'Last;
            Iter.Field := Iter.Field + 1;
         end if;

      else
         Iter.Field := Iter.Field + 1;
      end if;
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Record_Iterator) return Boolean is
   begin
      return Iter.Field > Iter.Item.Fields'Last;
   end At_End;

   ----------
   -- Data --
   ----------

   function Data (Iter : Record_Iterator) return Generic_Type_Access is
   begin
      if Iter.Variant = Natural'Last then
         return Generic_Type_Access (Iter.Item.Fields (Iter.Field).Value);
      else
         return Generic_Type_Access
           (Iter.Item.Fields (Iter.Field).Variant_Part (Iter.Variant));
      end if;
   end Data;

   -----------------
   -- Draw_Border --
   -----------------

   procedure Draw_Border
     (Item : access Record_Type;
      Draw : Boolean := True) is
   begin
      if Draw then
         Item.Border_Spacing := Border_Spacing;
      else
         Item.Border_Spacing := 0;
      end if;
   end Draw_Border;

end Items.Records;
