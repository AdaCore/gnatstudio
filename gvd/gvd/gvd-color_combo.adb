-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
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

with Glib;                use Glib;
with Gtk.Extra.Combo_Box; use Gtk.Extra.Combo_Box;
with Gdk.Color;           use Gdk.Color;
with Gtk.Handlers;        use Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtkada.Handlers;     use Gtkada.Handlers;
with Gtk.Color_Selection; use Gtk.Color_Selection;
with Gtk.Widget;          use Gtk.Widget;
with Gdk.Drawable;        use Gdk.Drawable;
with Gtk.Pixmap;          use Gtk.Pixmap;
with Gdk.GC;              use Gdk.GC;
with Gdk.Rectangle;       use Gdk.Rectangle;
with Gdk.Pixmap;          use Gdk.Pixmap;
with Gdk.Bitmap;          use Gdk.Bitmap;
with Pixmaps_IDE;         use Pixmaps_IDE;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Button;          use Gtk.Button;
with Gtk.Object;          use Gtk.Object;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Gdk.Window;          use Gdk.Window;

package body GVD.Color_Combo is

   Combo_Class_Record : GObject_Class := Uninitialized_Class;
   Combo_Signals : constant chars_ptr_array :=
     (1 => New_String ("color_changed"));

   --  ??? Should implement a destroy callback

   package Color_Cb is new Gtk.Handlers.Callback
     (Gvd_Color_Combo_Record);

   procedure Color_Selected
     (Combo : access Gvd_Color_Combo_Record'Class);
   --  Called when a new color is selected in the combo

   procedure Display_Button
     (Combo : access Gvd_Color_Combo_Record'Class);
   --  Redisplay the contents of the button

   procedure Arrow_Selected
     (Combo : access Gvd_Color_Combo_Record'Class);
   --  Called whenever the arrow button is selected to open or hide the
   --  popup window.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Combo : out Gvd_Color_Combo) is
   begin
      Combo := new Gvd_Color_Combo_Record;
      GVD.Color_Combo.Initialize (Combo);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Combo : access Gvd_Color_Combo_Record'Class) is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => GType_None));
   begin
      Gtk.Extra.Combo_Box.Initialize (Combo);

      Initialize_Class_Record
        (Combo,
         Signals      => Combo_Signals,
         Class_Record => Combo_Class_Record,
         Type_Name    => "GvdColorCombo",
         Parameters   => Signal_Parameters);

      Gtk_New (Combo.Selection);
      Add (Get_Frame (Combo), Combo.Selection);
      Color_Cb.Object_Connect
        (Combo.Selection,
         "color_changed",
         Color_Cb.To_Marshaller (Color_Selected'Access),
         Combo);
      Color_Cb.Connect
        (Combo,
         "map",
         Color_Cb.To_Marshaller (Display_Button'Access),
         After => True);
      Color_Cb.Object_Connect
        (Get_Arrow (Combo),
         "toggled",
         Color_Cb.To_Marshaller (Arrow_Selected'Access),
         Combo);
      Show (Combo.Selection);
   end Initialize;

   -------------------
   -- Color_Changed --
   -------------------

   procedure Color_Changed (Combo : access Gvd_Color_Combo_Record'Class) is
   begin
      Widget_Callback.Emit_By_Name (Combo, "color_changed");
   end Color_Changed;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Combo : access Gvd_Color_Combo_Record;
      Color : Gdk.Color.Gdk_Color)
   is
      Components : Color_Array;
   begin
      Combo.Color := Color;
      Components :=
        (Red     => Gdouble (Red (Combo.Color)) / Gdouble (Gushort'Last),
         Green   => Gdouble (Green (Combo.Color)) / Gdouble (Gushort'Last),
         Blue    => Gdouble (Blue (Combo.Color)) / Gdouble (Gushort'Last),
         Opacity => 1.0);
      Set_Color (Combo.Selection, Components);
      Display_Button (Combo);
   end Set_Color;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color (Combo : access Gvd_Color_Combo_Record)
      return Gdk.Color.Gdk_Color is
   begin
      return Combo.Color;
   end Get_Color;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color (Combo : access Gvd_Color_Combo_Record)
      return String
   is
      function Normalize (V : Gcolor_Int) return String;

      function Normalize (V : Gcolor_Int) return String is
         S : String (1 .. 8);  --  "16#....#" or "16#.#", ....
         O : String (1 .. 4) := "0000";
         Index : Natural := S'Last;
         O_Index : Natural := O'Last;

      begin
         Put (S, Integer (V), 16);

         while S (Index) /= '#' loop
            Index := Index - 1;
         end loop;

         Index := Index - 1;

         while S (Index) /= '#' loop
            O (O_Index) := S (Index);
            Index := Index - 1;
            O_Index := O_Index - 1;
         end loop;

         return O;
      end Normalize;

   begin
      return '#'
        & Normalize (Red (Combo.Color))
        & Normalize (Green (Combo.Color))
        & Normalize (Blue (Combo.Color));
   end Get_Color;

   --------------------
   -- Color_Selected --
   --------------------

   procedure Color_Selected
     (Combo : access Gvd_Color_Combo_Record'Class)
   is
      Color : Gdk_Color;
      Components : Color_Array;
   begin
      Get_Color (Combo.Selection, Components);
      Set_Rgb (Color,
               Gcolor_Int (Components (Red) * Gdouble (Guint16'Last)),
               Gcolor_Int (Components (Green) * Gdouble (Guint16'Last)),
               Gcolor_Int (Components (Blue) * Gdouble (Guint16'Last)));
      Alloc (Gtk.Widget.Get_Default_Colormap, Color);
      Combo.Color := Color;
      Display_Button (Combo);
      Color_Changed (Combo);
   end Color_Selected;

   --------------------
   -- Display_Button --
   --------------------

   procedure Display_Button (Combo : access Gvd_Color_Combo_Record'Class) is
      Tmp_Gc : Gdk_GC;
      Pixmap : Gtk_Pixmap;
      Val    : Gdk_Pixmap;
      Mask   : Gdk_Bitmap;
      use type Gdk_Window;

   begin
      if Get_Window (Combo) = null then
         return;
      end if;

      Pixmap := Gtk_Pixmap (Get_Child (Get_Button (Combo)));

      if Pixmap = null then
         Create_From_Xpm_D
           (Val,
            Window      => null,
            Colormap    => Get_System,
            Mask        => Mask,
            Transparent => Null_Color,
            Data        => paint_xpm);
         Gtk_New (Pixmap, Val, Mask);
         Add (Get_Button (Combo), Pixmap);
         Show (Pixmap);
      end if;

      Gdk_New (Tmp_Gc, Get_Window (Combo));
      Set_Foreground (Tmp_Gc, Combo.Color);
      Get (Pixmap, Val, Mask);
      Draw_Rectangle (Val, Tmp_Gc, True, 5, 20, 16, 4);
      Draw (Pixmap, Full_Area);
      Unref (Tmp_Gc);
   end Display_Button;

   --------------------
   -- Arrow_Selected --
   --------------------

   procedure Arrow_Selected
     (Combo : access Gvd_Color_Combo_Record'Class) is
   begin
      --  The following call should be used to set the current color,
      --  however it raises some warnings in gtk+ unless the color selection
      --  dialog is first hidden.
      Hide (Combo.Selection);
      Set_Color (Combo, Combo.Color);
      Show (Combo.Selection);
   end Arrow_Selected;

end GVD.Color_Combo;
