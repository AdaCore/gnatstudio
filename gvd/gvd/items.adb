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

with Glib;         use Glib;
with Gdk.Font;     use Gdk.Font;
with Gdk.Drawable; use Gdk.Drawable;
with Gdk.Pixmap;   use Gdk.Pixmap;
with Gdk.Bitmap;   use Gdk.Bitmap;
with Gdk.GC;       use Gdk.GC;
with Gdk.Window;   use Gdk.Window;
with Language;     use Language;
with Odd.Types;    use Odd.Types;

package body Items is

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

   procedure Set_Visibility
     (Item      : in out Generic_Type;
      Visible   : Boolean;
      Recursive : Boolean := False)
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

   ---------------------
   -- Propagate_Width --
   ---------------------

   procedure Propagate_Width (Item  : in out Generic_Type;
                              Width : Glib.Gint)
   is
   begin
      Item.Width := Width;
   end Propagate_Width;

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

   -------------------
   -- Set_Type_Name --
   -------------------

   procedure Set_Type_Name
     (Item : access Generic_Type;
      Name : String)
   is
   begin
      Free (Item.Type_Name);
      Item.Type_Name := new String'(Name);
   end Set_Type_Name;

   -------------------
   -- Get_Type_Name --
   -------------------

   function Get_Type_Name (Item : access Generic_Type) return String is
   begin
      if Item.Type_Name = null then
         return "";
      else
         return Item.Type_Name.all;
      end if;
   end Get_Type_Name;

   ----------------
   -- Show_Value --
   ----------------

   function Show_Value (Mode : Display_Mode) return Boolean is
   begin
      return Mode = Value or else Mode = Type_Value;
   end Show_Value;

   ---------------
   -- Show_Type --
   ---------------

   function Show_Type (Mode : Display_Mode) return Boolean is
   begin
      return Mode = Type_Only or else Mode = Type_Value;
   end Show_Type;

   -----------------------
   -- Clone_Dispatching --
   -----------------------

   procedure Clone_Dispatching
     (Item : Generic_Type;
      Clone : out Generic_Type_Access)
   is
   begin
      if Item.Type_Name /= null then
         Clone.Type_Name := new String'(Item.Type_Name.all);
      end if;
   end Clone_Dispatching;

   -----------
   -- Clone --
   -----------

   function Clone (Item : Generic_Type'Class) return Generic_Type_Access is
      R : Generic_Type_Access := new Generic_Type'Class'(Item);
   begin
      Clone_Dispatching (Item, R);
      return R;
   end Clone;

   ----------
   -- Free --
   ----------

   procedure Free (Item : access Generic_Type;
                   Only_Value : Boolean := False)
   is
      I : Generic_Type_Access := Generic_Type_Access (Item);
   begin
      if not Only_Value then
         Free (Item.Type_Name);
         Free_Internal (I);
      end if;
   end Free;

end Items;
