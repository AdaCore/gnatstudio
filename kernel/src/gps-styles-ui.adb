------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with Gtk.Enums;  use Gtk.Enums;

with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Traces; use Traces;

package body GPS.Styles.UI is

   use Gdk;
   use GNAT.Strings;

   Me : constant Debug_Handle := Create ("Styles");

   procedure Allocate_Color
     (Name : String; Color : out Gdk_Color);
   --  Allocates the low-level structures for Color.

   function Get_A_Widget return Gtk_Widget;
   --  Return a widget for purposes of getting the current style

   ------------------
   -- Get_A_Widget --
   ------------------

   function Get_A_Widget return Gtk_Widget is
      Widget  : Gtk_Widget;
      Tops    : Gtk.Widget.Widget_List.Glist;

   begin
      Tops := List_Toplevels;
      Widget := Widget_List.Get_Data (Tops);
      Widget_List.Free (Tops);
      return Widget;
   end Get_A_Widget;

   --------------------
   -- Allocate_Color --
   --------------------

   procedure Allocate_Color
     (Name : String; Color : out Gdk_Color)
   is
      Ignored : Boolean;
   begin
      Color := Null_Color;

      if Name = "" then
         Trace (Me, "Color field not filled");
         return;
      end if;

      begin
         Color := Parse (Name);
         Alloc_Color (Get_Default_Colormap, Color, Success => Ignored);
      exception
         when Wrong_Color =>
            Trace (Me, "Could not parse color " & Name);
            return;
      end;
   end Allocate_Color;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Style : in out Style_Record) is
   begin
      Free (Style.Name);
      Free (Style.Description);
      Free (Simple_Style_Record (Style));
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Style : in out Style_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Style_Record'Class, Style_Access);
   begin
      if Style /= null then
         Free (Style.all);
         Unchecked_Free (Style);
      end if;
   end Free;

   --------------------
   -- Set_Foreground --
   --------------------

   overriding procedure Set_Foreground
     (Style : not null access Style_Record; Color : String) is
   begin
      Set_Foreground (Simple_Style_Record (Style.all)'Access, Color);
      Style.Fg_Color := Null_Color;
   end Set_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   overriding procedure Set_Background
     (Style : not null access Style_Record; Color : String) is
   begin
      Set_Background (Simple_Style_Record (Style.all)'Access, Color);
      Style.Bg_Color := Null_Color;
   end Set_Background;

   --------------------------
   -- Get_Background_Color --
   --------------------------

   function Get_Background_Color
     (Style : not null access Style_Record) return Gdk_Color is
   begin
      if Style.Bg_Color = Null_Color
        and then Style.Background /= null
      then
         Allocate_Color (Get_Background (Style), Style.Bg_Color);
      end if;

      return Style.Bg_Color;
   end Get_Background_Color;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Style : Style_Access) return String is
   begin
      if Style /= null
        and then Style.Name /= null
      then
         return Style.Name.all;
      end if;

      return "";
   end Get_Name;

   ---------------------
   -- Get_Editor_Icon --
   ---------------------

   function Get_Editor_Icon
     (Style  : not null access Style_Record) return Gdk_Pixbuf
   is
      Widget : Gtk_Widget;
   begin
      if Style.Editor_Icon_Name /= null
        and then Style.Editor_Icon = Null_Pixbuf
      then
         Widget := Get_A_Widget;
         if Widget /= null
           and then Realized_Is_Set (Widget)
         then
            Style.Editor_Icon := Render_Icon
              (Widget, Style.Editor_Icon_Name.all, Icon_Size_Menu);
         end if;
      end if;

      return Style.Editor_Icon;
   end Get_Editor_Icon;

end GPS.Styles.UI;
