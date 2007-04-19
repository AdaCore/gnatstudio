-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2001-2007 AdaCore                    --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with System;

with Gdk.Color;       use Gdk.Color;
with Gdk.Drawable;    use Gdk.Drawable;
with Gdk.GC;          use Gdk.GC;
with Gdk.Rectangle;   use Gdk.Rectangle;
with Gdk;             use Gdk;

with Glib.Object;     use Glib.Object;
with Glib.Values;     use Glib.Values;
with Glib;            use Glib;

with Gtk.Enums;       use Gtk.Enums;
with Gtk.Style;       use Gtk.Style;
with Gtk.Widget;      use Gtk.Widget;

with Gtkada.Handlers; use Gtkada.Handlers;

package body Gtkada.Tree_View is

   procedure Set_Draw_Expander
     (Style : Gtk_Style; Expander : System.Address);
   pragma Import (C, Set_Draw_Expander, "ada_style_set_draw_expander");
   --  Set the draw_expander function for the widget's class.

   procedure Draw_Expander
     (Style          : Gtk_Style;
      Window         : Gdk_Window;
      State_Type     : Gtk_State_Type;
      Area           : Gdk_Rectangle_Access;
      Widget         : System.Address;
      Detail         : System.Address;
      X              : Gint;
      Y              : Gint;
      Expander_Style : Gtk_Expander_Style);
   pragma Convention (C, Draw_Expander);
   --  Override default draw_expander function style.

   procedure Row_Expanded_Callback
     (Widget : access GObject_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "row_expanded" signal.

   procedure Row_Collapsed_Callback
     (Widget : access GObject_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "row_collapsed" signal.

   procedure Row_Inserted_Callback
     (Widget : access GObject_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "row_inserted" signal.

   procedure Draw_Expander
     (Style          : Gtk_Style;
      Window         : Gdk_Window;
      State_Type     : Gtk_State_Type;
      Area           : Gdk_Rectangle_Access;
      Widget         : System.Address;
      Detail         : System.Address;
      X              : Gint;
      Y              : Gint;
      Expander_Style : Gtk_Expander_Style)
   is
      pragma Unreferenced (Detail);

      Red   : constant := 120 * 256;
      Green : constant := 152 * 256;
      Blue  : constant := 181 * 256;
      --  RGB values to emulate Windows XP style.

      Expander_Size      : Gint;
      Expander_Semi_Size : Gint;
      Local_X            : Gint;
      Local_Y            : Gint;
      GC                 : Gdk.Gdk_GC;
      Color              : Gdk_Color;
      Color_Saved        : Gdk_Color;
      Success            : Boolean;

      procedure Style_Get
        (Widget   : System.Address;
         Property : String;
         Size     : out Gint);
      pragma Import (C, Style_Get, "ada_gtk_widget_style_get_int");

   begin
      Style_Get (Widget, "expander_size" & ASCII.NUL, Expander_Size);

      if Expander_Size > 2 then
         Expander_Size := Expander_Size - 2;
      end if;

      GC := Get_Fg_GC (Style, State_Type);

      if Area /= null then
         Set_Clip_Rectangle (GC, Area);
      end if;

      Expander_Semi_Size := Expander_Size / 2;
      Local_X := X - Expander_Semi_Size;
      Local_Y := Y - Expander_Semi_Size;

      Color_Saved := Get_Foreground (Style, State_Type);
      Set_Rgb (Color, Red, Green, Blue);
      Alloc_Color (Get_Default_Colormap, Color, Success => Success);

      if Success then
         Set_Foreground (GC, Color);
      end if;

      Draw_Line
        (Window, GC,
         Local_X + 1, Local_Y, Local_X + Expander_Size - 1, Local_Y);
      Draw_Line
        (Window, GC,
         Local_X + 1, Local_Y + Expander_Size,
         Local_X + Expander_Size - 1, Local_Y + Expander_Size);
      Draw_Line
        (Window, GC,
         Local_X, Local_Y + 1, Local_X, Local_Y + Expander_Size - 1);
      Draw_Line
        (Window, GC,
         Local_X + Expander_Size, Local_Y + 1,
         Local_X + Expander_Size, Local_Y + Expander_Size - 1);

      if Success then
         Set_Foreground (GC, Color_Saved);
      end if;

      Draw_Line
        (Window, GC,
         Local_X + 2, Local_Y + Expander_Semi_Size,
         Local_X + Expander_Size - 2, Local_Y + Expander_Semi_Size);

      case Expander_Style is
         when Expander_Collapsed | Expander_Semi_Collapsed =>
            Draw_Line
              (Window, GC,
               Local_X + Expander_Semi_Size, Local_Y + 2,
               Local_X + Expander_Semi_Size, Local_Y + Expander_Size - 2);

         when Expander_Semi_Expanded | Expander_Expanded =>
            null;
      end case;

      if Area /= null then
         Set_Clip_Rectangle (GC);
      end if;
   end Draw_Expander;

   ---------------------------
   -- Row_Expanded_Callback --
   ---------------------------

   procedure Row_Expanded_Callback
     (Widget : access GObject_Record'Class;
      Params : Glib.Values.GValues)
   is
      Tree  : constant Tree_View := Tree_View (Widget);
      Iter  : Gtk_Tree_Iter;
      Path  : Gtk_Tree_Path;
      Dummy : Boolean;
      pragma Unreferenced (Dummy);

   begin
      if Tree.Lock then
         return;
      end if;

      Get_Tree_Iter (Nth (Params, 1), Iter);
      Set (Tree.Model, Iter, Tree.Expanded_State_Column, True);

      Iter := Children (Tree.Model, Iter);

      while Iter /= Null_Iter loop
         if Get_Boolean (Tree.Model, Iter, Tree.Expanded_State_Column) then
            Path := Get_Path (Tree.Model, Iter);
            Dummy := Expand_Row (Tree, Path, False);
            Path_Free (Path);
         end if;

         Next (Tree.Model, Iter);
      end loop;
   end Row_Expanded_Callback;

   ----------------------------
   -- Row_Collapsed_Callback --
   ----------------------------

   procedure Row_Collapsed_Callback
     (Widget : access GObject_Record'Class;
      Params : Glib.Values.GValues)
   is
      Tree : constant Tree_View := Tree_View (Widget);
      Iter : Gtk_Tree_Iter;
   begin
      Get_Tree_Iter (Nth (Params, 1), Iter);
      Set (Tree.Model, Iter, Tree.Expanded_State_Column, False);
   end Row_Collapsed_Callback;

   ---------------------------
   -- Row_Inserted_Callback --
   ---------------------------

   procedure Row_Inserted_Callback
     (Widget : access GObject_Record'Class;
      Params : Glib.Values.GValues)
   is
      Tree : constant Tree_View := Tree_View (Widget);
      Iter : Gtk_Tree_Iter;
   begin
      Get_Tree_Iter (Nth (Params, 2), Iter);
      Set (Tree.Model, Iter, Tree.Expanded_State_Column, False);
   end Row_Inserted_Callback;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget       : out Tree_View;
      Column_Types : GType_Array) is
   begin
      Widget := new Tree_View_Record;
      Initialize (Widget, Column_Types);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget       : access Tree_View_Record'Class;
      Column_Types : GType_Array)
   is
      Real_Column_Types : GType_Array
        (Column_Types'First .. Column_Types'Last + 1);
   begin
      Real_Column_Types := Column_Types & (GType_Boolean);
      Widget.Expanded_State_Column :=
        Gint (Real_Column_Types'Length - 1);

      Gtk_New (Widget.Model, Real_Column_Types);
      Initialize (Gtk_Tree_View (Widget), Widget.Model);
      Set_Draw_Expander (Get_Style (Widget), Draw_Expander'Address);

      Gtkada.Handlers.Object_Callback.Object_Connect
        (Widget,
         Signal_Row_Expanded,
         Row_Expanded_Callback'Access,
         Widget,
         After => True);

      Gtkada.Handlers.Object_Callback.Object_Connect
        (Widget,
         Signal_Row_Collapsed,
         Row_Collapsed_Callback'Access,
         Widget,
         After => True);

      --  Consider any newly inserted row as a collapsed row,
      --  set the flag accordingly.

      Gtkada.Handlers.Object_Callback.Object_Connect
        (Widget.Model,
         Signal_Row_Inserted,
         Row_Inserted_Callback'Access,
         Widget,
         After => True);
   end Initialize;

   ------------------
   -- Get_Expanded --
   ------------------

   function Get_Expanded
     (Widget : access Tree_View_Record;
      Iter   : Gtk_Tree_Iter) return Boolean is
   begin
      if Iter = Null_Iter then
         return True;
      else
         return Get_Boolean (Widget.Model, Iter, Widget.Expanded_State_Column);
      end if;
   end Get_Expanded;

end Gtkada.Tree_View;
