-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2000-2008, AdaCore               --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with GNAT.IO;         use GNAT.IO;
with Glib;            use Glib;
with Gdk.Drawable;    use Gdk.Drawable;
with Pango.Layout;    use Pango.Layout;
with Language;        use Language;

package body Items.Repeats is

   ---------------------
   -- New_Repeat_Type --
   ---------------------

   function New_Repeat_Type return Generic_Type_Access is
   begin
      return new Repeat_Type'
        (Generic_Type with
         Value            => null,
         Repeat_Str_Width => 0,
         Repeat_Num       => 0);
   end New_Repeat_Type;

   --------------------
   -- Get_Repeat_Num --
   --------------------

   function Get_Repeat_Num (Item : access Repeat_Type) return Integer is
   begin
      return Item.Repeat_Num;
   end Get_Repeat_Num;

   --------------------
   -- Set_Repeat_Num --
   --------------------

   procedure Set_Repeat_Num
     (Item : in out Repeat_Type;
      Num  : Integer) is
   begin
      Item.Repeat_Num := Num;
   end Set_Repeat_Num;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Item : Repeat_Type) return Generic_Type_Access is
   begin
      return Item.Value;
   end Get_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Item  : in out Repeat_Type;
      Value : Generic_Type_Access) is
   begin
      Item.Valid := True;
      Item.Value := Value;
   end Set_Value;

   -----------
   -- Print --
   -----------

   overriding procedure Print (Value : Repeat_Type; Indent : Natural := 0) is
   begin
      Put ("{<" & Value.Repeat_Num'Img & " times> : ");

      if Value.Value /= null then
         Print (Value.Value.all, Indent + 3);
         Put ("}");
      else
         Put ("<null>}");
      end if;
   end Print;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Item : access Repeat_Type; Only_Value : Boolean := False) is
   begin
      if Item.Value /= null then
         --  Keep the structure of the item that is repeated, if required.
         Free (Item.Value, Only_Value);
      end if;

      Free (Generic_Type (Item.all)'Access, Only_Value);
   end Free;

   -----------------------
   -- Clone_Dispatching --
   -----------------------

   overriding procedure Clone_Dispatching
     (Item  : Repeat_Type;
      Clone : in out Generic_Type_Access) is
   begin
      Clone_Dispatching (Generic_Type (Item), Clone);

      --  duplicate the type of the repeated item.
      --  The value itself is in fact not duplicated, since the leafs of the
      --  type tree is a simple_type (or one of its children), that does not
      --  clone the value.
      Repeat_Type_Access (Clone).Value := Items.Clone (Item.Value.all);
   end Clone_Dispatching;

   -----------
   -- Paint --
   -----------

   overriding procedure Paint
     (Item    : in out Repeat_Type;
      Context : Drawing_Context;
      Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
      Lang    : Language.Language_Access;
      Mode    : Display_Mode;
      X, Y    : Gint := 0)
   is
      Str : constant String :=
        "<repeat" & Integer'Image (Item.Repeat_Num) & "> ";

      use Gdk;

   begin
      Item.X := X;
      Item.Y := Y;

      if not Item.Valid then
         Display_Pixmap
           (Pixmap, Context.GC, Context.Unknown_Pixmap,
            Context.Unknown_Mask, X + Border_Spacing, Y);
         return;
      end if;

      if Item.Selected then
         Draw_Rectangle
           (Pixmap,
            Context.Selection_GC,
            Filled => True,
            X      => X,
            Y      => Y,
            Width  => Item.Width,
            Height => Item.Height);
      end if;

      Set_Text (Context.Text_Layout, Str);
      Draw_Layout
        (Drawable => Pixmap,
         GC       => Context.GC,
         X        => X + Border_Spacing,
         Y        => Y + Border_Spacing,
         Layout   => Context.Text_Layout);

      Paint (Item.Value.all, Context, Pixmap, Lang, Mode,
             X + Item.Repeat_Str_Width, Y + Border_Spacing);

      --  Draw a border
      Draw_Rectangle
        (Pixmap,
         Context.GC,
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
     (Item           : in out Repeat_Type;
      Context        : Drawing_Context;
      Lang           : Language.Language_Access;
      Mode           : Display_Mode;
      Hide_Big_Items : Boolean := False)
   is
      Str : constant String :=
        "<repeat" & Integer'Image (Item.Repeat_Num) & "> ";
   begin
      if not Item.Valid then
         Get_Size (Context.Unknown_Pixmap, Item.Width, Item.Height);
      else
         Size_Request (Item.Value.all, Context, Lang, Mode, Hide_Big_Items);
         Set_Text (Context.Text_Layout, Str);
         Get_Pixel_Size
           (Context.Text_Layout, Item.Repeat_Str_Width, Item.Height);
         Item.Width :=
           Item.Value.Width + Item.Repeat_Str_Width + 2 * Border_Spacing;
         Item.Height := Gint'Max (Item.Value.Height, Item.Height)
           + 2 * Border_Spacing;
      end if;
   end Size_Request;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   overriding function Get_Component_Name
     (Item : access Repeat_Type;
      Lang : access Language_Root'Class;
      Name : String;
      X, Y : Glib.Gint) return String is
   begin
      if X < Item.Repeat_Str_Width then
         return Name;
      end if;

      return Get_Component_Name
        (Item.Value, Lang, Name, X - Item.Repeat_Str_Width, Y);
   end Get_Component_Name;

   -------------------
   -- Get_Component --
   -------------------

   overriding function Get_Component
     (Item : access Repeat_Type;
      X, Y : Glib.Gint) return Generic_Type_Access is
   begin
      if X < Item.Repeat_Str_Width then
         return Generic_Type_Access (Item);
      end if;

      return Get_Component
        (Item.Value, X - Item.Repeat_Str_Width, Y);
   end Get_Component;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Parent       : access Repeat_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class) return Generic_Type_Access is
   begin
      if Parent.Value = Generic_Type_Access (Current) then
         Free (Parent.Value, Only_Value => False);
         Parent.Value := Generic_Type_Access (Replace_With);
         return Generic_Type_Access (Replace_With);
      end if;

      return null;
   end Replace;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Item : access Repeat_Type) return Generic_Iterator'Class is
      Iter : Repeat_Iterator;
   begin
      Iter.Item := Repeat_Type_Access (Item);
      Iter.At_End := False;
      return Iter;
   end Start;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Repeat_Iterator) is
   begin
      Iter.At_End := True;
   end Next;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Repeat_Iterator) return Boolean is
   begin
      return Iter.At_End;
   end At_End;

   ----------
   -- Data --
   ----------

   overriding function Data
     (Iter : Repeat_Iterator) return Generic_Type_Access is
   begin
      return Iter.Item.Value;
   end Data;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Item1 : access Repeat_Type; Item2 : access Generic_Type'Class)
      return Boolean is
   begin
      return Item2.all in Repeat_Type'Class
        and then Structurally_Equivalent
        (Item1.Value, Repeat_Type_Access (Item2).Value);
   end Structurally_Equivalent;

end Items.Repeats;
