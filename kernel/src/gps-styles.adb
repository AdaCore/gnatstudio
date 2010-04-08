-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2010, AdaCore                      --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;

with Traces; use Traces;

package body GPS.Styles is

   use Gdk;
   use GNAT.Strings;

   Me : constant Debug_Handle := Create ("Styles");

   procedure Allocate_Color
     (Name : String; Color : out Gdk_Color; GC : out Gdk.Gdk_GC);
   --  Allocates the low-level structures for Color.

   --------------------
   -- Allocate_Color --
   --------------------

   procedure Allocate_Color
     (Name : String; Color : out Gdk_Color; GC : out Gdk.Gdk_GC)
   is
      Success : Boolean;
      Widget  : Gtk_Widget;
      Tops    : Gtk.Widget.Widget_List.Glist;
   begin
      Color := Null_Color;
      GC    := Null_GC;

      if Name = "" then
         Trace (Me, "Color field not filled");
         return;
      end if;

      begin
         Color := Parse (Name);
      exception
         when Wrong_Color =>
            Trace (Me, "Could not parse color " & Name);
            return;
      end;

      Alloc_Color (Get_Default_Colormap, Color, False, True, Success);

      if not Success then
         Trace (Me, "Could not allocate color " & Name);
         return;
      end if;

      Tops := List_Toplevels;
      Widget := Widget_List.Get_Data (Tops);
      Widget_List.Free (Tops);

      if Widget = null
        or else not Realized_Is_Set (Widget)
      then
         Trace (Me, "Cannot create GC: toplevel window not realized");
         return;
      end if;

      Gdk_New (GC, Get_Window (Widget));
      Set_Foreground (GC, Color);
   end Allocate_Color;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground
     (Style : not null access Simple_Style_Record; Color : String) is
   begin
      Free (Style.Foreground);
      Style.Foreground := new String'(Color);
   end Set_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Style : not null access Simple_Style_Record; Color : String) is
   begin
      Free (Style.Background);
      Style.Background := new String'(Color);
   end Set_Background;

   --------------------
   -- Get_Background --
   --------------------

   function Get_Background
     (Style : not null access Simple_Style_Record) return String is
   begin
      if Style.Background = null then
         return "";
      else
         return Style.Background.all;
      end if;
   end Get_Background;

   --------------------
   -- Get_Foreground --
   --------------------

   function Get_Foreground
     (Style : not null access Simple_Style_Record) return String is
   begin
      if Style.Foreground = null then
         return "";
      else
         return Style.Foreground.all;
      end if;
   end Get_Foreground;

   ---------------------
   -- Set_In_Speedbar --
   ---------------------

   procedure Set_In_Speedbar
     (Style       : not null access Simple_Style_Record;
      In_Speedbar : Boolean) is
   begin
      Style.Speedbar := In_Speedbar;
   end Set_In_Speedbar;

   -----------------
   -- In_Speedbar --
   -----------------

   function In_Speedbar
     (Style       : not null access Simple_Style_Record) return Boolean is
   begin
      return Style.Speedbar;
   end In_Speedbar;

   ----------
   -- Free --
   ----------

   procedure Free (Style : in out Simple_Style_Record) is
   begin
      Free (Style.Background);
      Free (Style.Foreground);
   end Free;

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
      Style.Fg_GC    := Null_GC;
   end Set_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   overriding procedure Set_Background
     (Style : not null access Style_Record; Color : String) is
   begin
      Set_Background (Simple_Style_Record (Style.all)'Access, Color);
      Style.Bg_Color := Null_Color;
      Style.Bg_GC    := Null_GC;
   end Set_Background;

   -----------------------
   -- Get_Background_GC --
   -----------------------

   function Get_Background_GC
     (Style : not null access Style_Record) return Gdk.GC.Gdk_GC is
   begin
      if Style.Bg_GC = null then
         Allocate_Color (Get_Background (Style), Style.Bg_Color, Style.Bg_GC);
      end if;

      return Style.Bg_GC;
   end Get_Background_GC;

   --------------------------
   -- Get_Background_Color --
   --------------------------

   function Get_Background_Color
     (Style : not null access Style_Record) return Gdk_Color is
   begin
      if Style.Bg_GC = null then
         Allocate_Color (Get_Background (Style), Style.Bg_Color, Style.Bg_GC);
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

end GPS.Styles;
