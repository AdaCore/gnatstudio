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

with Cairo;           use Cairo;
with Glib;            use Glib;
with Pango.Layout;    use Pango.Layout;

with Gtkada.Style;    use Gtkada.Style;

with Language;        use Language;

package body Items.Records is

   use type GNAT.Strings.String_Access;

   ---------------------
   -- New_Record_Type --
   ---------------------

   function New_Record_Type
     (Num_Fields : Natural) return Generic_Type_Access
   is
      R : constant Generic_Type_Access := new Record_Type (Num_Fields);
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
         GNAT.Strings.Free (Item.Fields (Index).Name);
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
     (Item  : Record_Type;
      Index : Positive) return GNAT.Strings.String_Access is
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
             (Item.Fields (Field).Variant_Part (J).Fields'Length /= 0
              and then
                Item.Fields (Field).Variant_Part (J).Fields (1).Name.all =
                Contains)
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

   overriding procedure Print (Value : Record_Type; Indent : Natural := 0) is
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

   overriding procedure Print (Value : Union_Type; Indent : Natural := 0) is
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

   overriding procedure Free
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
               GNAT.Strings.Free (Item.Fields (J).Name);
               Free (Item.Fields (J).Variant_Part);
            end if;
         end if;
      end loop;

      Free (Generic_Type (Item.all)'Access, Only_Value);
   end Free;

   -----------------------
   -- Clone_Dispatching --
   -----------------------

   overriding procedure Clone_Dispatching
     (Item  : Record_Type;
      Clone : in out Generic_Type_Access)
   is
      R : Record_Type_Access;
   begin
      Clone_Dispatching (Generic_Type (Item), Clone);
      R := Record_Type_Access (Clone);

      for J in Item.Fields'Range loop
         R.Fields (J).Name := new String'(Item.Fields (J).Name.all);
         --  Duplicate the type structure, but not the value itself.

         if Item.Fields (J).Value = null then
            R.Fields (J).Value := null;
         else
            R.Fields (J).Value := Items.Clone (Item.Fields (J).Value.all);
         end if;

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

   overriding procedure Paint
     (Item    : in out Record_Type;
      Context : Drawing_Context;
      Cr      : Cairo_Context;
      Lang    : Language.Language_Access;
      Mode    : Display_Mode;
      X, Y    : Gint := 0)
   is
      procedure Print_Field_Name (F : Integer);
      --  Print the name of the field F and an arrow

      Current_Y : Gint := Y + Item.Border_Spacing;
      --  Arrow_Pos : constant Gint :=
      --    X + Left_Border + Item.Border_Spacing + Item.Gui_Fields_Width -
      --    GVD_Text_Width (Context.Font, String'(" => "));

      ----------------------
      -- Print_Field_Name --
      ----------------------

      procedure Print_Field_Name (F : Integer) is
         use Gdk;
      begin
         Set_Text (Context.Text_Layout,
                   Item.Fields (F).Name.all & ASCII.HT & " => ");
         Draw_Layout
           (Cr, Context.Foreground,
            X        => X + Left_Border + Item.Border_Spacing,
            Y        => Current_Y,
            Layout   => Context.Text_Layout);
      end Print_Field_Name;

      W, H : Gint;
   begin
      Item.X := X;
      Item.Y := Y;

      if not Item.Valid then
         Draw_Pixbuf (Cr, Context.Unknown_Pixmap, X + Left_Border, Y);
         return;
      end if;

      --  A null record ?

      if Item.Num_Fields = 0 then
         return;
      end if;

      if not Item.Visible then
         Draw_Pixbuf (Cr, Context.Hidden_Pixmap, X + Left_Border, Current_Y);
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
            X        => X + Left_Border + Item.Border_Spacing,
            Y        => Current_Y,
            Layout   => Context.Type_Layout);
         Get_Pixel_Size (Context.Type_Layout, W, H);
         Current_Y := Current_Y + H;
      end if;

      for F in Item.Fields'Range loop
         --  not a variant part ?

         if Item.Fields (F).Value /= null then
            Paint
              (Item.Fields (F).Value.all, Context, Cr, Lang, Mode,
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
                    (Item.Fields (F).Variant_Part (V).all, Context, Cr,
                     Lang, Mode, X + Left_Border + Item.Border_Spacing
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
           (Cr, Context.Foreground,
            Filled => False,
            X      => X,
            Y      => Y,
            Width  => Item.Width - 1,
            Height => Item.Height - 1);
      end if;
   end Paint;

   ------------------
   -- Size_Request --
   ------------------

   overriding procedure Size_Request
     (Item           : in out Record_Type;
      Context        : Drawing_Context;
      Lang           : Language.Language_Access;
      Mode           : Display_Mode;
      Hide_Big_Items : Boolean := False)
   is
      Total_Height, Total_Width : Gint := 0;
      H : Gint;
      Largest_Name : GNAT.Strings.String_Access := null;

   begin
      if not Item.Valid then
         Item.Width := Get_Width (Context.Unknown_Pixmap);
         Item.Height := Get_Height (Context.Unknown_Pixmap);
         return;
      end if;

      --  null record ?

      if Item.Fields'Length = 0 then
         Item.Gui_Fields_Width := 0;
         Item.Width := 0;
         Item.Height := 0;
         return;
      end if;

      if Show_Type (Mode)
        and then Item.Type_Name /= null
      then
         Set_Text (Context.Type_Layout, Get_Type_Name (Item'Access, Lang));
         Get_Pixel_Size (Context.Type_Layout, Total_Width, Total_Height);
         Item.Type_Height := Total_Height;
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
                 (Item.Fields (F).Value.all, Context, Lang, Mode,
                  Hide_Big_Items);

               Total_Width  :=
                 Gint'Max (Total_Width, Item.Fields (F).Value.Width);

               --  Keep at least enough space to print the field name

               Item.Fields (F).Value.Height := Gint'Max
                 (Item.Fields (F).Value.Height, Context.Line_Height);
               Total_Height := Total_Height + Item.Fields (F).Value.Height;
            end if;

            --  a variant part ?

            if Item.Fields (F).Variant_Part /= null then
               for V in Item.Fields (F).Variant_Part'Range loop
                  if Item.Fields (F).Variant_Part (V).Valid then
                     Size_Request
                       (Item.Fields (F).Variant_Part (V).all, Context, Lang,
                        Mode, Hide_Big_Items);
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
            Set_Text (Context.Text_Layout, " => ");
         else
            Set_Text
              (Context.Text_Layout, Largest_Name.all & ASCII.HT & " => ");
         end if;

         Get_Pixel_Size (Context.Text_Layout, Item.Gui_Fields_Width, H);

         --  Keep enough space for the border (Border_Spacing on each side)
         Item.Width  := Total_Width + Item.Gui_Fields_Width + Left_Border
           + 2 * Item.Border_Spacing;
         Item.Height := Total_Height + 2 * Item.Border_Spacing;

         if Hide_Big_Items
           and then Item.Height > Context.Big_Item_Height
         then
            Item.Visible := False;
         end if;
      end if;

      if not Item.Visible then
         Item.Gui_Fields_Width := 0;
         Item.Width := Left_Border + 2 * Item.Border_Spacing +
           Get_Width (Context.Hidden_Pixmap);
         Item.Height := 2 * Item.Border_Spacing +
           Get_Height (Context.Hidden_Pixmap);
      end if;
   end Size_Request;

   ---------------------
   -- Propagate_Width --
   ---------------------

   procedure Propagate_Width
     (Item  : access Record_Type;
      Width : Glib.Gint)
   is
      W : constant Gint := Width - Item.Gui_Fields_Width - Left_Border
        - 2 * Item.Border_Spacing;
      Iter : Generic_Iterator'Class := Start (Item);
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

   overriding function Get_Component_Name
     (Item : access Record_Type;
      Lang : access Language.Language_Root'Class;
      Name : String;
      Comp : Generic_Type_Access) return String
   is
   begin
      for F in Item.Fields'Range loop
         if Item.Fields (F).Value = Comp then
            return Record_Field_Name (Lang, Name, Item.Fields (F).Name.all);

         elsif Item.Fields (F).Variant_Part /= null then
            for V in Item.Fields (F).Variant_Part'Range loop
               if Generic_Type_Access (Item.Fields (F).Variant_Part (V)) =
                 Comp
               then
                  return Get_Component_Name
                    (Item.Fields (F).Variant_Part (V),
                     Lang,
                     Name,
                     Comp);
               end if;
            end loop;
         end if;
      end loop;
      return Name;
   end Get_Component_Name;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   overriding function Get_Component_Name
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

   overriding function Get_Component
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

   overriding function Replace
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

   overriding function Start
     (Item : access Record_Type) return Generic_Iterator'Class
   is
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

   overriding procedure Next (Iter : in out Record_Iterator) is
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

   overriding function At_End (Iter : Record_Iterator) return Boolean is
   begin
      return Iter.Field > Iter.Item.Fields'Last;
   end At_End;

   ----------
   -- Data --
   ----------

   overriding function Data
     (Iter : Record_Iterator) return Generic_Type_Access is
   begin
      if Iter.Variant = Natural'Last then
         return Iter.Item.Fields (Iter.Field).Value;
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

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Item1 : access Record_Type; Item2 : access Generic_Type'Class)
      return Boolean is
   begin
      if not (Item2.all in Record_Type'Class)
        or else Item1.Num_Fields /= Record_Type_Access (Item2).Num_Fields
      then
         return False;
      end if;

      for F in Item1.Fields'Range loop
         if (Item1.Fields (F).Variant_Part /= null
             and then Record_Type_Access
                        (Item2).Fields (F).Variant_Part = null)
           or else
           (Item1.Fields (F).Variant_Part = null
            and then Record_Type_Access
                       (Item2).Fields (F).Variant_Part /= null)
         then
            return False;
         end if;

         --  Protect against the null Name, which happens for variant parts
         if Item1.Fields (F).Value /= null
           and then Record_Type_Access (Item2).Fields (F).Value /= null
           and then not Structurally_Equivalent
           (Item1.Fields (F).Value,
            Record_Type_Access (Item2).Fields (F).Value)
         then
            return False;
         end if;

         if Item1.Fields (F).Variant_Part /= null then
            if Item1.Fields (F).Variant_Part'Length /=
              Record_Type_Access (Item2).Fields (F).Variant_Part'Length
            then
               return False;
            end if;

            for V in Item1.Fields (F).Variant_Part'Range loop
               if not Structurally_Equivalent
                 (Item1.Fields (F).Variant_Part (V),
                  Record_Type_Access (Item2).Fields (F).Variant_Part (V))
               then
                  return False;
               end if;
            end loop;
         end if;
      end loop;
      return True;
   end Structurally_Equivalent;

end Items.Records;
