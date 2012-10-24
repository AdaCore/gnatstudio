------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Cairo.Image_Surface;       use Cairo.Image_Surface;

with Pango.Font;                use Pango.Font;
with Pango.Layout;              use Pango.Layout;
with Gdk.Display;               use Gdk.Display;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.RGBA;                  use Gdk.RGBA;
with Gdk.Screen;                use Gdk.Screen;
with Gdk.Window;                use Gdk.Window;
with Glib;                      use Glib;
with Glib.Convert;              use Glib.Convert;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Style;              use Gtkada.Style;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with Language;                  use Language;
with Language.Icons;            use Language.Icons;
with Language.Tree;             use Language.Tree;
with String_Utils;              use String_Utils;
with Xref;                      use Xref;

with Entities_Tooltips_Utility; use Entities_Tooltips_Utility;

package body Entities_Tooltips is

   function Get_Pixbuf
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : General_Entity) return Gdk_Pixbuf;
   --  Return the image associated to an entity

   function Draw_Tooltip
     (Kernel      : access Kernel_Handle_Record'Class;
      Header      : String;
      Doc         : String;
      Pixbuf      : Gdk_Pixbuf;
      Draw_Border : Boolean;
      Guess       : Boolean := False) return Cairo.Cairo_Surface;
   --  Helper function, factorizing the tooltip widget creation

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : General_Entity) return Gdk_Pixbuf
   is
      Info : constant Tooltip_Information :=
        Get_Tooltip_Information (Kernel, Entity);
   begin
      return Entity_Icons (Info.Is_Spec, Info.Visibility) (Info.Category);
   end Get_Pixbuf;

   ------------------
   -- Draw_Tooltip --
   ------------------

   function Draw_Tooltip
     (Kernel        : access Kernel_Handle_Record'Class;
      Entity        : General_Entity;
      Ref           : General_Entity_Reference;
      Draw_Border   : Boolean) return Cairo.Cairo_Surface
   is
   begin
      return Draw_Tooltip
        (Kernel      => Kernel,
         Guess       => Is_Guess (Entity),
         Header      => Get_Tooltip_Header (Kernel, Entity),
         Pixbuf      => Get_Pixbuf (Kernel, Entity),
         Draw_Border => Draw_Border,
         Doc         => Get_Tooltip_Documentation (Kernel, Entity, Ref));
   end Draw_Tooltip;

   ------------------
   -- Draw_Tooltip --
   ------------------

   function Draw_Tooltip
     (Kernel      : access Kernel_Handle_Record'Class;
      Entity      : Entity_Access;
      Draw_Border : Boolean;
      Guess       : Boolean := False) return Cairo.Cairo_Surface
   is
      pragma Unreferenced (Guess);

      Construct : constant access Simple_Construct_Information :=
        Get_Construct (Entity);
   begin
      return Draw_Tooltip
        (Kernel => Kernel,
         Guess  => False,
         Header => Get_Tooltip_Header (Entity),
         Draw_Border => Draw_Border,
         Doc => Get_Tooltip_Documentation (Kernel, Entity),
         Pixbuf => Entity_Icons
           (Construct.Is_Declaration, Construct.Visibility)
           (Construct.Category));
   end Draw_Tooltip;

   ------------------
   -- Draw_Tooltip --
   ------------------

   function Draw_Tooltip
     (Kernel      : access Kernel_Handle_Record'Class;
      Header      : String;
      Doc         : String;
      Pixbuf      : Gdk_Pixbuf;
      Draw_Border : Boolean;
      Guess       : Boolean := False) return Cairo.Cairo_Surface
   is
      Widget : constant Gtk_Widget := Gtk_Widget (Get_Main_Window (Kernel));
      Pixmap : Cairo.Cairo_Surface;
      Cr     : Cairo.Cairo_Context;

      Font   : constant Pango_Font_Description := Default_Font.Get_Pref_Font;
      Fixed  : constant Pango_Font_Description := View_Fixed_Font.Get_Pref;

      Header_Layout, Doc_Layout : Pango_Layout;

      Width, Height, W1, H1, W2, H2 : Gint := 0;
      Color  : Cairo_Color;
      Max_Height, Max_Width : Gint;

      H_Pad : constant := 4;
      V_Pad : constant := 3;
   begin
      Header_Layout := Create_Pango_Layout (Widget, "");

      if Guess then
         Set_Markup
           (Header_Layout, "<span foreground =""#555555"">" &
              Tooltip_Guess_Message & "</span>" & ASCII.LF & Header);
      else
         Set_Markup (Header_Layout, Header);
      end if;

      Set_Font_Description (Header_Layout, Font);
      Get_Pixel_Size (Header_Layout, W1, H1);
      Height := Height + V_Pad * 2 + H1;

      if Doc /= "" then
         Doc_Layout := Create_Pango_Layout (Widget, "");
         Set_Markup (Doc_Layout, Doc);
         Set_Font_Description (Doc_Layout, Fixed);
         Get_Pixel_Size (Doc_Layout, W2, H2);
         Height := Height + V_Pad * 2 + 1 + H2;
      end if;

      Width  := Gint'Max (W1 + Get_Width (Pixbuf) + H_Pad * 2, W2 + H_Pad * 2);

      Color := To_Cairo (Gdk_RGBA'(Tooltip_Color.Get_Pref));

      Max_Height := Get_Height (Get_Default_Screen (Gdk.Display.Get_Default));
      Height := Gint'Min (Height, Max_Height);

      Max_Width := Get_Width (Get_Default_Screen (Gdk.Display.Get_Default));
      Width := Gint'Min (Width, Max_Width);

      Pixmap := Create (Cairo_Format_ARGB32, Width, Height);
      Cr := Create (Pixmap);
      Set_Line_Width (Cr, 0.5);
      Draw_Rectangle (Cr, Color, True, 0, 0, Width, Height);

      Color := To_Cairo (Black_RGBA);
      if Draw_Border then
         Draw_Rectangle (Cr, Color, False, 0, 0, Width, Height);
      end if;

      Draw_Pixbuf (Cr, Pixbuf, V_Pad, H_Pad);

      Draw_Layout
        (Cr, Color, V_Pad + Get_Width (Pixbuf), H_Pad, Header_Layout);
      Unref (Header_Layout);

      if Doc_Layout /= null then
         Draw_Line (Cr, Color, 0, H1 + V_Pad * 2, Width - 1, H1 + V_Pad * 2);
         Draw_Layout (Cr, Color, H_Pad, H1 + 1 + V_Pad * 3, Doc_Layout);
         Unref (Doc_Layout);
      end if;

      Destroy (Cr);

      return Pixmap;
   end Draw_Tooltip;

end Entities_Tooltips;
