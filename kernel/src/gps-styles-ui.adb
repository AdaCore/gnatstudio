------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2015, AdaCore                     --
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
with GNATCOLL.Traces; use GNATCOLL.Traces;

package body GPS.Styles.UI is

   use Gdk;
   use GNAT.Strings;

   Me : constant Trace_Handle := Create ("Styles");

   procedure Allocate_Color
     (Name : String; Color : out Gdk_RGBA);
   --  Allocates the low-level structures for Color.

   --------------------
   -- Allocate_Color --
   --------------------

   procedure Allocate_Color
     (Name : String; Color : out Gdk_RGBA)
   is
      Success : Boolean;
   begin
      Color := Null_RGBA;

      if Name = "" then
         Trace (Me, "Color field not filled");
         return;
      end if;

      Parse (Color, Name, Success);
      if not Success then
         Trace (Me, "Could not parse color " & Name);
         return;
      end if;
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
      Style.Fg_Color := Null_RGBA;
   end Set_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   overriding procedure Set_Background
     (Style : not null access Style_Record; Color : String) is
   begin
      Set_Background (Simple_Style_Record (Style.all)'Access, Color);
      Style.Bg_Color := Null_RGBA;
   end Set_Background;

   --------------------------
   -- Get_Background_Color --
   --------------------------

   function Get_Background_Color
     (Style : not null access Style_Record) return Gdk_RGBA is
   begin
      if Style.Bg_Color = Null_RGBA
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

end GPS.Styles.UI;
