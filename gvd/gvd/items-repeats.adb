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
with Gdk.Window;   use Gdk.Window;
with Language;     use Language;

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

   ----------
   -- Free --
   ----------

   procedure Free (Item : access Repeat_Type; Only_Value : Boolean := False) is
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

   procedure Clone_Dispatching
     (Item  : Repeat_Type;
      Clone : out Generic_Type_Access) is
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

   procedure Paint
     (Item    : in out Repeat_Type;
      Context : Drawing_Context;
      X, Y    : Gint := 0)
   is
      Str : String := "<repeat " & Integer'Image (Item.Repeat_Num) & "> ";
   begin
      Item.X := X;
      Item.Y := Y;

      if not Item.Valid then
         Display_Pixmap
           (Context.Pixmap, Context.GC, Context.Unknown_Pixmap,
            Context.Unknown_Mask, X + Border_Spacing, Y);
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

      Draw_Text
        (Context.Pixmap,
         Font => Context.Font,
         GC   => Context.GC,
         X    => X + Border_Spacing,
         Y    => Y + Border_Spacing + Get_Ascent (Context.Font),
         Text => Str);

      Paint (Item.Value.all, Context,
             X + Item.Repeat_Str_Width, Y + Border_Spacing);

      --  Draw a border
      Draw_Rectangle
        (Context.Pixmap,
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
     (Item           : in out Repeat_Type;
      Context        : Drawing_Context;
      Hide_Big_Items : Boolean := False)
   is
      Str : String := "<repeat " & Integer'Image (Item.Repeat_Num) & "> ";
   begin
      if not Item.Valid then
         Get_Size (Context.Unknown_Pixmap, Item.Width, Item.Height);
      else
         Size_Request (Item.Value.all, Context, Hide_Big_Items);
         Item.Repeat_Str_Width := Text_Width (Context.Font, Str);
         Item.Width :=
           Item.Value.Width + Item.Repeat_Str_Width + 2 * Border_Spacing;
         Item.Height :=
           Gint'Max (Item.Value.Height,
                     Get_Ascent (Context.Font) + Get_Descent (Context.Font)) +
           2 * Border_Spacing;
      end if;
   end Size_Request;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   function Get_Component_Name
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

   function Get_Component
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

   function Replace
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

   function Start (Item : access Repeat_Type) return Generic_Iterator'Class is
      Iter : Repeat_Iterator;
   begin
      Iter.Item := Repeat_Type_Access (Item);
      Iter.At_End := False;
      return Iter;
   end Start;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Repeat_Iterator) is
   begin
      Iter.At_End := True;
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Repeat_Iterator) return Boolean is
   begin
      return Iter.At_End;
   end At_End;

   ----------
   -- Data --
   ----------

   function Data (Iter : Repeat_Iterator) return Generic_Type_Access is
   begin
      return Iter.Item.Value;
   end Data;

end Items.Repeats;
