-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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
with Glib.Graphs;         use Glib.Graphs;
with Glib.Object;         use Glib.Object;
with Gdk.Color;           use Gdk.Color;
with Gdk.GC;              use Gdk.GC;
with Gtkada.Canvas;       use Gtkada.Canvas;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Gdk.Drawable;        use Gdk.Drawable;
with Gdk.Event;           use Gdk.Event;
with Gdk.Font;            use Gdk.Font;
with Gdk.Rectangle;       use Gdk.Rectangle;
with Gdk.Types.Keysyms;   use Gdk.Types.Keysyms;
with Gdk.Window;          use Gdk.Window;
with Gtk.Accel_Group;     use Gtk.Accel_Group;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Menu;            use Gtk.Menu;
with Gtk.Menu_Item;       use Gtk.Menu_Item;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Style;           use Gtk.Style;
with Gtk.Widget;          use Gtk.Widget;
with Pango.Font;          use Pango.Font;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Intl;                use Glide_Intl;
with Layouts;                   use Layouts;

package body Browsers.Canvas is

   Selected_Link_Color : constant String := "#FF0000";
   --  <preferences> Color to use links whose ends are selected.

   Selected_Item_Color : constant String := "#888888";
   --  <preferences> Color to use to draw the selected item.

   Parent_Linked_Item_Color : constant String := "#AAAAAA";
   Child_Linked_Item_Color : constant String := "#DDDDDD";
   --  <preferences> Color to use to draw the items that are linked to the
   --  selected item.

   Zoom_Levels : constant array (Positive range <>) of Guint :=
     (25, 50, 75, 100, 150, 200, 300, 400);
   --  All the possible zoom levels. We have to use such an array, instead
   --  of doing the computation directly, so as to avoid rounding errors that
   --  would appear in the computation and make zoom_in not the reverse of
   --  zoom_out.

   Zoom_Steps : constant := 7;
   --  Number of steps while zooming in or out.

   type Cb_Data is record
      Browser : Glide_Browser;
      Item    : Canvas_Item;
      Zoom    : Guint;
   end record;

   package Contextual_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Cb_Data);

   procedure Zoom_In (Browser : access Gtk_Widget_Record'Class);
   --  Zoom in to the previous zoom level, if any

   procedure Zoom_Out (Browser : access Gtk_Widget_Record'Class);
   --  Zoom out to the next zoom level, if any

   procedure Zoom_Level
     (Browser : access Gtk_Widget_Record'Class; Data : Cb_Data);
   --  Zoom directly to a specific level (Data.Zoom)

   procedure Realized (Browser : access Gtk_Widget_Record'Class);
   --  Callback for the "realized" signal.

   function Key_Press
     (Browser : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean;
   --  Callback for the key press event

   procedure On_Refresh (Browser : access Gtk_Widget_Record'Class);
   --  Recompute the layout of the canvas

   procedure Toggle_Links
     (Mitem : access Gtk_Widget_Record'Class; Data : Cb_Data);
   --  Toggle the display of links for the item

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Browser : access Glide_Browser_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Gtk.Scrolled_Window.Initialize (Browser);
      Set_Policy (Browser, Policy_Automatic, Policy_Automatic);
      Gtk_New (Browser.Canvas);
      Add (Browser, Browser.Canvas);
      Add_Events (Browser.Canvas, Key_Press_Mask);
      Browser.Kernel := Kernel_Handle (Kernel);

      Set_Layout_Algorithm (Browser.Canvas, Layer_Layout'Access);
      Set_Auto_Layout (Browser.Canvas, False);

      Widget_Callback.Object_Connect
        (Browser.Canvas, "realize",
         Widget_Callback.To_Marshaller (Realized'Access), Browser);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Browser.Canvas, "key_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller (Key_Press'Access),
         Browser);
   end Initialize;

   ---------------
   -- Key_Press --
   ---------------

   function Key_Press
     (Browser : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean is
   begin
      case Get_Key_Val (Event) is
         when GDK_equal => Zoom_In (Browser);
         when GDK_minus => Zoom_Out (Browser);
         when others    => null;
      end case;
      return False;
   end Key_Press;

   --------------
   -- Realized --
   --------------

   procedure Realized (Browser : access Gtk_Widget_Record'Class) is
      use type Gdk.Gdk_GC;
      B : Glide_Browser := Glide_Browser (Browser);
      Color : Gdk_Color;
      Desc : Pango_Font_Description;
   begin
      if B.Selected_Link_GC = null then
         Gdk_New (B.Selected_Link_GC, Get_Window (B.Canvas));
         Color := Parse (Selected_Link_Color);
         Alloc (Get_Default_Colormap, Color);
         Set_Foreground (B.Selected_Link_GC, Color);

         Gdk_New (B.Selected_Item_GC, Get_Window (B.Canvas));
         Color := Parse (Selected_Item_Color);
         Alloc (Get_Default_Colormap, Color);
         Set_Foreground (B.Selected_Item_GC, Color);

         Gdk_New (B.Parent_Linked_Item_GC, Get_Window (B.Canvas));
         Color := Parse (Parent_Linked_Item_Color);
         Alloc (Get_Default_Colormap, Color);
         Set_Foreground (B.Parent_Linked_Item_GC, Color);

         Gdk_New (B.Child_Linked_Item_GC, Get_Window (B.Canvas));
         Color := Parse (Child_Linked_Item_Color);
         Alloc (Get_Default_Colormap, Color);
         Set_Foreground (B.Child_Linked_Item_GC, Color);

         Gdk_New (B.Text_GC, Get_Window (B.Canvas));
         Set_Foreground (B.Text_GC, Get_Pref (B.Kernel, Browsers_Link_Color));

         Desc := Get_Pref (B.Kernel, Browsers_Link_Font);
         B.Text_Font := From_Description (Desc);
         Free (Desc);
      end if;
   end Realized;

   ----------------
   -- Get_Canvas --
   ----------------

   function Get_Canvas (Browser : access Glide_Browser_Record)
      return Interactive_Canvas is
   begin
      return Browser.Canvas;
   end Get_Canvas;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   function Contextual_Factory
     (Item  : access Glide_Browser_Item_Record;
      Browser : access Glide_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Glide_Kernel.Selection_Context_Access
   is
   begin
      return null;
   end Contextual_Factory;

   -------------------------------------
   -- Default_Browser_Context_Factory --
   -------------------------------------

   function Default_Browser_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
      return Glide_Kernel.Selection_Context_Access
   is
      B          : Glide_Browser := Glide_Browser (Object);
      Context    : Selection_Context_Access;
      Mitem      : Gtk_Menu_Item;
      Zooms_Menu : Gtk_Menu;
      Item       : Canvas_Item;
      Xr, Yr     : Gint;
      Xsave, Ysave : Gdouble;
      Success    : Boolean;

   begin
      --  Click on an item: this is a file selection
      --  ??? Should we convert to world coordinates here ?

      Get_Origin (Get_Window (B.Canvas), Xr, Yr, Success);
      Set_X (Event, Get_X_Root (Event) - Gdouble (Xr));
      Set_Y (Event, Get_Y_Root (Event) - Gdouble (Yr));

      Item := Item_At_Coordinates (B.Canvas, Event);

      if Item /= null then
         if Glide_Browser_Item (Item).Hide_Links then
            Gtk_New (Mitem, Label => -"Show links");
         else
            Gtk_New (Mitem, Label => -"Hide links");
         end if;
         Append (Menu, Mitem);
         Contextual_Cb.Connect
           (Mitem, "activate",
            Contextual_Cb.To_Marshaller (Toggle_Links'Access),
            (Browser => B, Item => Item, Zoom => 100));

         Xsave := Get_X (Event);
         Ysave := Get_Y (Event);
         Set_X (Event, Get_X (Event) - Gdouble (Get_Coord (Item).X));
         Set_Y (Event, Get_Y (Event) - Gdouble (Get_Coord (Item).Y));
         Context := Contextual_Factory
           (Glide_Browser_Item (Item), B, Event, Menu);
         Set_X (Event, Xsave);
         Set_Y (Event, Ysave);
      end if;

      if Context = null then
         Unlock (Get_Default_Accelerators (Kernel));

         Context := new Selection_Context;

         Gtk_New (Mitem, Label => -"Refresh");
         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, "activate",
            Widget_Callback.To_Marshaller (On_Refresh'Access), B);

         Gtk_New (Mitem, Label => -"Zoom in");
         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, "activate",
            Widget_Callback.To_Marshaller (Zoom_In'Access), B);
         Add_Accelerator
           (Mitem, "activate",
            Get_Default_Accelerators (Kernel), GDK_equal, 0, Accel_Visible);

         Gtk_New (Mitem, Label => -"Zoom out");
         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, "activate",
            Widget_Callback.To_Marshaller (Zoom_Out'Access), B);
         Add_Accelerator
           (Mitem, "activate",
            Get_Default_Accelerators (Kernel), GDK_minus, 0, Accel_Visible);

         Gtk_New (Zooms_Menu);

         for J in Zoom_Levels'Range loop
            Gtk_New (Mitem, Label => Guint'Image (Zoom_Levels (J)) & '%');
            Append (Zooms_Menu, Mitem);
            Contextual_Cb.Connect
              (Mitem, "activate",
               Contextual_Cb.To_Marshaller (Zoom_Level'Access),
               (Browser => B,
                Item    => null,
                Zoom    => Zoom_Levels (J)));
         end loop;

         Gtk_New (Mitem, Label => -"Zoom");
         Append (Menu, Mitem);
         Set_Submenu (Mitem, Zooms_Menu);

         Lock (Get_Default_Accelerators (Kernel));
      end if;

      return Context;
   end Default_Browser_Context_Factory;

   ------------------
   -- Toggle_Links --
   ------------------

   procedure Toggle_Links
     (Mitem : access Gtk_Widget_Record'Class; Data : Cb_Data)
   is
      It : Glide_Browser_Item := Glide_Browser_Item (Data.Item);
   begin
      It.Hide_Links := not It.Hide_Links;
      Refresh_Canvas (Get_Canvas (Data.Browser));
   end Toggle_Links;

   ----------------
   -- On_Refresh --
   ----------------

   procedure On_Refresh (Browser : access Gtk_Widget_Record'Class) is
      B : Glide_Browser := Glide_Browser (Browser);
   begin
      Layout (Get_Canvas (B), Force => False, Vertical_Layout => True);
      Refresh_Canvas (Get_Canvas (B));
   end On_Refresh;

   -------------
   -- Zoom_In --
   -------------

   procedure Zoom_In (Browser : access Gtk_Widget_Record'Class) is
      Canvas : Interactive_Canvas := Glide_Browser (Browser).Canvas;
      Z : constant Guint := Get_Zoom (Canvas);
   begin
      for J in Zoom_Levels'Range loop
         if Zoom_Levels (J) = Z then
            if J /= Zoom_Levels'Last then
               Zoom (Canvas, Zoom_Levels (J + 1), Zoom_Steps);
            end if;
         end if;
      end loop;
   end Zoom_In;

   --------------
   -- Zoom_Out --
   --------------

   procedure Zoom_Out (Browser : access Gtk_Widget_Record'Class) is
      Canvas : Interactive_Canvas := Glide_Browser (Browser).Canvas;
      Z : constant Guint := Get_Zoom (Canvas);
   begin
      for J in Zoom_Levels'Range loop
         if Zoom_Levels (J) = Z then
            if J /= Zoom_Levels'First then
               Zoom (Canvas, Zoom_Levels (J - 1), Zoom_Steps);
            end if;
         end if;
      end loop;
   end Zoom_Out;

   ----------------
   -- Zoom_Level --
   ----------------

   procedure Zoom_Level
     (Browser : access Gtk_Widget_Record'Class; Data : Cb_Data) is
   begin
      Zoom (Data.Browser.Canvas, Data.Zoom, 1);
   end Zoom_Level;

   ---------------
   -- To_Brower --
   ---------------

   function To_Brower
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class)
      return Glide_Browser is
   begin
      return Glide_Browser (Get_Parent (Canvas));
   end To_Brower;

   -------------------
   -- Selected_Item --
   -------------------

   function Selected_Item (Browser : access Glide_Browser_Record)
      return Gtkada.Canvas.Canvas_Item is
   begin
      return Browser.Selected_Item;
   end Selected_Item;

   --------------------------
   -- Draw_Item_Background --
   --------------------------

   procedure Draw_Item_Background
     (Browser : access Glide_Browser_Record;
      Item    : access Gtkada.Canvas.Buffered_Item_Record'Class)
   is
      Bg_GC : Gdk_GC;
      Coord : Gdk_Rectangle := Get_Coord (Item);
   begin
      if Canvas_Item (Item) = Selected_Item (Browser) then
         Bg_GC := Browser.Selected_Item_GC;

      elsif Selected_Item (Browser) /= null
        and then Has_Link (Browser.Canvas,
                           From => Item, To => Selected_Item (Browser))
      then
         Bg_GC := Browser.Parent_Linked_Item_GC;

      elsif Selected_Item (Browser) /= null
        and then Has_Link (Browser.Canvas,
                           From => Selected_Item (Browser), To => Item)
      then
         Bg_GC := Browser.Child_Linked_Item_GC;

      else
         Bg_GC := Get_White_GC (Get_Style (Browser.Canvas));
      end if;

      Set_Screen_Size_And_Pixmap
        (Item, Get_Window (Browser), Gint (Coord.Width), Gint (Coord.Height));

      Draw_Rectangle
        (Pixmap (Item),
         GC     => Bg_GC,
         Filled => True,
         X      => 0,
         Y      => 0,
         Width  => Coord.Width,
         Height => Coord.Height);

      Draw_Shadow
        (Style       => Get_Style (Browser.Canvas),
         Window      => Pixmap (Item),
         State_Type  => State_Normal,
         Shadow_Type => Shadow_Out,
         X           => 0,
         Y           => 0,
         Width       => Coord.Width,
         Height      => Coord.Height);
   end Draw_Item_Background;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
     (Browser : access Glide_Browser_Record;
      Item    : access Gtkada.Canvas.Canvas_Item_Record'Class;
      Refresh_Items : Boolean := False)
   is
      function Refresh_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  Refresh the display of an item.

      ------------------
      -- Refresh_Item --
      ------------------

      function Refresh_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean is
      begin
         Refresh (Browser, Glide_Browser_Item (Item));
         return True;
      end Refresh_Item;

   begin
      Browser.Selected_Item := Canvas_Item (Item);

      if Refresh_Items then
         --  ??? We should redraw only the items that were previously
         --  ??? highlighted, and the new ones.
         For_Each_Item (Browser.Canvas, Refresh_Item'Unrestricted_Access);
         Refresh_Canvas (Browser.Canvas);
      end if;
   end Select_Item;

   -----------------
   -- Get_Text_GC --
   -----------------

   function Get_Text_GC
     (Browser : access Glide_Browser_Record) return Gdk.GC.Gdk_GC is
   begin
      return Browser.Text_GC;
   end Get_Text_GC;

   -------------------
   -- Get_Text_Font --
   -------------------

   function Get_Text_Font
     (Browser : access Glide_Browser_Record) return Gdk.Font.Gdk_Font is
   begin
      return Browser.Text_Font;
   end Get_Text_Font;

   ---------------
   -- Draw_Link --
   ---------------

   procedure Draw_Link
     (Canvas      : access Interactive_Canvas_Record'Class;
      Link        : access Glide_Browser_Link_Record;
      Window      : Gdk.Window.Gdk_Window;
      Invert_Mode : Boolean;
      GC          : Gdk.GC.Gdk_GC;
      Edge_Number : Glib.Gint)
   is
      Browser : Glide_Browser := To_Brower (Canvas);
   begin
      if not Glide_Browser_Item (Get_Src (Link)).Hide_Links
        and then not Glide_Browser_Item (Get_Dest (Link)).Hide_Links
      then
         if Invert_Mode
           or else
           (Get_Src (Link) /= Vertex_Access (Selected_Item (Browser))
            and then Get_Dest (Link) /=
               Vertex_Access (Selected_Item (Browser)))
         then
            Draw_Link
              (Canvas, Canvas_Link_Access (Link), Window,
               Invert_Mode, GC, Edge_Number);
         else
            Draw_Link
              (Canvas, Canvas_Link_Access (Link),
               Window, Invert_Mode, Browser.Selected_Link_GC,
               Edge_Number);
         end if;
      end if;
   end Draw_Link;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Browser : access Glide_Browser_Record'Class;
                      Item    : access Glide_Browser_Item_Record) is
   begin
      Draw_Item_Background (Browser, Item);
   end Refresh;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel (Browser : access Glide_Browser_Record)
      return Glide_Kernel.Kernel_Handle is
   begin
      return Browser.Kernel;
   end Get_Kernel;

end Browsers.Canvas;
