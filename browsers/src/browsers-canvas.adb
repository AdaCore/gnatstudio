-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2004                      --
--                             ACT-Europe                            --
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

with Glib;                use Glib;
with Glib.Convert;        use Glib.Convert;
with Glib.Error;          use Glib.Error;
with Glib.Graphs;         use Glib.Graphs;
with Glib.Object;         use Glib.Object;
with Pango.Enums;         use Pango.Enums;
with Gdk.Color;           use Gdk.Color;
with Gdk.GC;              use Gdk.GC;
with Gdk.Drawable;        use Gdk.Drawable;
with Gdk.Event;           use Gdk.Event;
with Gdk.Pixbuf;          use Gdk.Pixbuf;
with Gdk.Pixmap;          use Gdk.Pixmap;
with Gdk.Rectangle;       use Gdk.Rectangle;
with Gdk.Types.Keysyms;   use Gdk.Types.Keysyms;
with Gdk.Window;          use Gdk.Window;
with Gtk.Accel_Group;     use Gtk.Accel_Group;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Button;          use Gtk.Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Hbutton_Box;     use Gtk.Hbutton_Box;
with Gtk.Image;           use Gtk.Image;
with Gtk.Menu;            use Gtk.Menu;
with Gtk.Menu_Item;       use Gtk.Menu_Item;
with Gdk.Rectangle;       use Gdk.Rectangle;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Stock;           use Gtk.Stock;
with Gtk.Style;           use Gtk.Style;
with Gtk.Widget;          use Gtk.Widget;
with Pango.Layout;        use Pango.Layout;
with Pango.Context;       use Pango.Context;
with Pango.Font;          use Pango.Font;

with Ada.Exceptions;      use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;        use GNAT.Strings;

with Gtkada.Canvas;       use Gtkada.Canvas;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Hooks;        use Glide_Kernel.Hooks;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with GVD.Preferences;
with Glide_Intl;                use Glide_Intl;
with Layouts;                   use Layouts;
with VFS;                       use VFS;
with Traces;                    use Traces;

package body Browsers.Canvas is

   Zoom_Levels : constant array (Positive range <>) of Guint :=
     (25, 50, 75, 100, 150, 200, 300, 400);
   --  All the possible zoom levels. We have to use such an array, instead
   --  of doing the computation directly, so as to avoid rounding errors that
   --  would appear in the computation and make zoom_in not the reverse of
   --  zoom_out.

   Zoom_Steps : constant := 3;
   --  Number of steps while zooming in or out.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Active_Area_Tree_Array, Active_Area_Tree_Array_Access);

   type Cb_Data is record
      Browser : General_Browser;
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

   procedure On_Export (Browser : access Gtk_Widget_Record'Class);
   --  Export the contents of the browser as an image.

   procedure On_Refresh (Browser : access Gtk_Widget_Record'Class);
   --  Recompute the layout of the canvas

   procedure Toggle_Links
     (Mitem : access Gtk_Widget_Record'Class; Data : Cb_Data);
   --  Toggle the display of links for the item

   procedure Toggle_Orthogonal (Browser : access Gtk_Widget_Record'Class);
   --  Toggle the layout of links.

   procedure Set_Root
     (Mitem : access Gtk_Widget_Record'Class; Data : Cb_Data);
   --  Remove all items except the one described in Data from the canvas.

   procedure Close_Item
     (Event : Gdk.Event.Gdk_Event; User  : access Browser_Item_Record'Class);
   --  Close an item when the user presses on the title bar button.

   procedure Dump
     (Me : Debug_Handle; Tree : Active_Area_Tree; Indent : Natural := 0);
   --  For debugging purposes, dump the tree to Me.

   procedure Destroyed (Browser : access Gtk_Widget_Record'Class);
   --  Called when the browser is destroyed

   type Preferences_Hook_Record is new Hook_No_Args_Record with record
      Browser : General_Browser;
   end record;
   type Preferences_Hook is access all Preferences_Hook_Record'Class;
   procedure Execute (Hook : Preferences_Hook_Record;
                      Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   procedure Compute_Parents
     (Event : Gdk_Event; Item : access Browser_Item_Record'Class);
   procedure Compute_Children
     (Event : Gdk_Event; Item : access Browser_Item_Record'Class);
   --  Callbacks for the title bar buttons of Arrow_item

   type Image_Canvas_Record is new
     Gtkada.Canvas.Interactive_Canvas_Record with
   record
      Background        : Gdk.Pixbuf.Gdk_Pixbuf;
      Scaled_Background : Gdk.Pixbuf.Gdk_Pixbuf;
      Bg_GC             : Gdk.GC.Gdk_GC;
      Draw_Grid         : Boolean;
      Pixmap            : Gdk.Gdk_Pixmap;
   end record;
   type Image_Canvas is access all Image_Canvas_Record'Class;

   function Get_Window
     (Canvas : access Image_Canvas_Record) return Gdk.Window.Gdk_Window;
   --  Override Gtk.Widget.Get_Window, so that a different Window can be
   --  returned if needed (e.g. when exporting the canvas).

   procedure Draw_Background
     (Canvas        : access Image_Canvas_Record;
      Screen_Rect   : Gdk.Rectangle.Gdk_Rectangle);

   procedure On_Zoom (Canvas : access Gtk_Widget_Record'Class);
   --  Called when the canvas has been zoomed

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
     (Canvas : access Image_Canvas_Record) return Gdk.Window.Gdk_Window
   is
      use type Gdk.Gdk_Drawable;
   begin
      if Canvas.Pixmap = null then
         return Get_Window (Gtk_Widget_Record (Canvas.all)'Access);
      else
         return Canvas.Pixmap;
      end if;
   end Get_Window;

   -------------
   -- On_Zoom --
   -------------

   procedure On_Zoom (Canvas : access Gtk_Widget_Record'Class) is
      C : constant Image_Canvas := Image_Canvas (Canvas);
   begin
      if C.Scaled_Background /= null
        and then C.Scaled_Background /= C.Background
      then
         Unref (C.Scaled_Background);
      end if;

      if Get_Zoom (C) /= 100
        and then C.Background /= null
      then
         C.Scaled_Background := Scale_Simple
           (C.Background,
            To_Canvas_Coordinates (C, Get_Width (C.Background)),
            To_Canvas_Coordinates (C, Get_Height (C.Background)));
      else
         C.Scaled_Background := C.Background;
      end if;
   end On_Zoom;

   ---------------------
   -- Draw_Background --
   ---------------------

   procedure Draw_Background
     (Canvas        : access Image_Canvas_Record;
      Screen_Rect   : Gdk.Rectangle.Gdk_Rectangle)
   is
      X_Left : constant Gint := Left_World_Coordinates (Canvas);
      Y_Top  : constant Gint := Top_World_Coordinates (Canvas);
   begin
      if Canvas.Background = null then
         Draw_Rectangle
           (Get_Window (Canvas),
            Canvas.Bg_GC,
            Filled => True,
            X      => Screen_Rect.X,
            Y      => Screen_Rect.Y,
            Width  => Gint (Screen_Rect.Width),
            Height => Gint (Screen_Rect.Height));
      else
         declare
            X, Y, W, H, Ys : Gint;
            Xs : Gint := Screen_Rect.X;
            Bw : constant Gint := Get_Width (Canvas.Scaled_Background);
            Bh : constant Gint := Get_Height (Canvas.Scaled_Background);
         begin
            while Xs < Screen_Rect.X + Screen_Rect.Width loop
               Ys := Screen_Rect.Y;
               X := (X_Left + Xs) mod Bw;
               W := Gint'Min (Screen_Rect.Width + Screen_Rect.X - Xs, Bw - X);

               while Ys < Screen_Rect.Y + Screen_Rect.Height loop
                  Y := (Y_Top  + Ys) mod Bh;
                  H := Gint'Min
                    (Screen_Rect.Height + Screen_Rect.Y - Ys, Bh - Y);

                  Render_To_Drawable
                    (Pixbuf       => Canvas.Scaled_Background,
                     Drawable     => Get_Window (Canvas),
                     Gc           => Get_Black_GC (Get_Style (Canvas)),
                     Src_X        => X,
                     Src_Y        => Y,
                     Dest_X       => Xs,
                     Dest_Y       => Ys,
                     Width        => W,
                     Height       => H);
                  Ys := Ys + H;
               end loop;

               Xs := Xs + W;
            end loop;
         end;
      end if;

      if Canvas.Draw_Grid then
         Draw_Grid
           (Interactive_Canvas (Canvas),
            Get_Black_GC (Get_Style (Canvas)),
            Screen_Rect);
      end if;
   end Draw_Background;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Browser : access General_Browser_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Create_Toolbar  : Boolean;
      Parents_Pixmap  : String := Stock_Go_Back;
      Children_Pixmap : String := Stock_Go_Forward)
   is
      Hook : Preferences_Hook;
      Scrolled : Gtk_Scrolled_Window;
      Canvas : Image_Canvas;
   begin
      Gtk.Box.Initialize_Vbox (Browser, Homogeneous => False);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Pack_Start (Browser, Scrolled, Expand => True, Fill => True);

      Canvas := new Image_Canvas_Record;
      Gtkada.Canvas.Initialize (Canvas);
      Browser.Canvas := Interactive_Canvas (Canvas);

      Widget_Callback.Connect
        (Browser.Canvas, "zoomed",
         Widget_Callback.To_Marshaller (On_Zoom'Access));

      Add (Scrolled, Browser.Canvas);
      Add_Events (Browser.Canvas, Key_Press_Mask);
      Browser.Kernel := Kernel_Handle (Kernel);

      Set_Layout_Algorithm (Browser.Canvas, Layer_Layout'Access);
      Set_Auto_Layout (Browser.Canvas, False);

      if Create_Toolbar then
         Gtk_New (Browser.Toolbar);
         Set_Layout (Browser.Toolbar, Buttonbox_Spread);
         Pack_Start (Browser, Browser.Toolbar, Expand => False);
      end if;

      Browser.Up_Arrow := Render_Icon
        (Browser, Parents_Pixmap, Icon_Size_Menu);
      Browser.Down_Arrow := Render_Icon
        (Browser, Children_Pixmap, Icon_Size_Menu);
      Browser.Close_Pixmap := Render_Icon
        (Browser, Stock_Close, Icon_Size_Menu);

      Widget_Callback.Object_Connect
        (Browser, "destroy",
         Widget_Callback.To_Marshaller (Destroyed'Access), Browser);

      Widget_Callback.Object_Connect
        (Browser.Canvas, "realize",
         Widget_Callback.To_Marshaller (Realized'Access), Browser);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Browser.Canvas, "key_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller (Key_Press'Access),
         Browser);

      Hook := new Preferences_Hook_Record;
      Hook.Browser := General_Browser (Browser);
      Add_Hook
        (Kernel, Preferences_Changed_Hook, Hook, Watch => GObject (Browser));
      Execute (Hook.all, Kernel);
   end Initialize;

   ---------------
   -- Destroyed --
   ---------------

   procedure Destroyed (Browser : access Gtk_Widget_Record'Class) is
      use type Gdk_GC, Gdk_Pixmap;
      B : constant General_Browser := General_Browser (Browser);
   begin
      if B.Up_Arrow /= null then
         Unref (B.Up_Arrow);
      end if;

      if B.Down_Arrow /= null then
         Unref (B.Down_Arrow);
      end if;

      if B.Close_Pixmap /= null then
         Unref (B.Close_Pixmap);
      end if;

      if B.Default_Item_GC /= null then
         Unref (B.Default_Item_GC);
      end if;

      if B.Selected_Item_GC /= null then
         Unref (B.Selected_Item_GC);
      end if;

      if B.Parent_Linked_Item_GC /= null then
         Unref (B.Parent_Linked_Item_GC);
      end if;

      if B.Child_Linked_Item_GC /= null then
         Unref (B.Child_Linked_Item_GC);
      end if;

      if B.Text_GC /= null then
         Unref (B.Text_GC);
      end if;

      if B.Title_GC /= null then
         Unref (B.Title_GC);
      end if;

      if Image_Canvas (B.Canvas).Background /= null then
         Unref (Image_Canvas (B.Canvas).Background);
      end if;

      if Image_Canvas (B.Canvas).Bg_GC /= null then
         Unref (Image_Canvas (B.Canvas).Bg_GC);
      end if;
   end Destroyed;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Error           : GError;
      Iter            : Item_Iterator;
      Annotation_Font : Pango_Font_Description;

   begin
      if Realized_Is_Set (Hook.Browser) then
         Hook.Browser.Selected_Link_Color :=
           Get_Pref (Kernel, Selected_Link_Color);
         Hook.Browser.Unselected_Link_Color :=
           Get_Pref (Kernel, Unselected_Link_Color);

         Set_Foreground
           (Hook.Browser.Selected_Item_GC,
            Get_Pref (Kernel, Glide_Kernel.Preferences.Selected_Item_Color));
         Set_Foreground
           (Hook.Browser.Parent_Linked_Item_GC,
            Get_Pref (Kernel, Parent_Linked_Item_Color));
         Set_Foreground
           (Hook.Browser.Child_Linked_Item_GC,
            Get_Pref (Kernel, Child_Linked_Item_Color));
         Set_Foreground
           (Hook.Browser.Text_GC,
            Get_Pref (Kernel, Browsers_Hyper_Link_Color));
         Set_Foreground
           (Hook.Browser.Title_GC,
            Get_Pref (Kernel, GVD.Preferences.Title_Color));
         Set_Foreground (Image_Canvas (Hook.Browser.Canvas).Bg_GC,
                         Get_Pref (Hook.Browser.Kernel, Browsers_Bg_Color));
      end if;

      Annotation_Font := Copy (Get_Pref (Kernel, Default_Font));
      Set_Size
        (Annotation_Font,
         Gint'Max (Pango_Scale, Get_Size (Annotation_Font) - 2 * Pango_Scale));
      Configure (Hook.Browser.Canvas, Annotation_Font => Annotation_Font);
      Free (Annotation_Font);

      Image_Canvas (Hook.Browser.Canvas).Draw_Grid :=
        Get_Pref (Kernel, Browsers_Draw_Grid);

      if Image_Canvas (Hook.Browser.Canvas).Background /= null then
         Unref (Image_Canvas (Hook.Browser.Canvas).Background);
      end if;

      if Get_Pref (Kernel, Browsers_Bg_Image) /= "" then
         Gdk_New_From_File
           (Image_Canvas (Hook.Browser.Canvas).Background,
            Filename => Get_Pref (Kernel, Browsers_Bg_Image),
            Error    => Error);
      else
         Image_Canvas (Hook.Browser.Canvas).Background := null;
      end if;

      On_Zoom (Hook.Browser.Canvas);

      Iter := Start (Hook.Browser.Canvas);
      while Get (Iter) /= null loop
         Set_Font_Description
           (Browser_Item (Get (Iter)).Title_Layout,
            Get_Pref (Kernel, Default_Font));

         Refresh (Browser_Item (Get (Iter)));
         Next (Iter);
      end loop;

      Refresh_Canvas (Hook.Browser.Canvas);
   end Execute;

   ---------------------------
   -- Setup_Default_Toolbar --
   ---------------------------

   procedure Setup_Default_Toolbar (Browser : access General_Browser_Record) is
      Button : Gtk_Button;
      Image  : Gtk_Image;
   begin
      if Browser.Toolbar /= null then
         Gtk_New (Button);
         Gtk_New (Image, Stock_Zoom_Out, Icon_Size_Small_Toolbar);
         Add (Button, Image);
         Pack_End (Browser.Toolbar, Button, Expand => False);
         Widget_Callback.Object_Connect
           (Button, "clicked",
            Widget_Callback.To_Marshaller (Zoom_Out'Access), Browser);

         Gtk_New (Button);
         Gtk_New (Image, Stock_Zoom_In, Icon_Size_Small_Toolbar);
         Add (Button, Image);
         Pack_End (Browser.Toolbar, Button, Expand => False);
         Widget_Callback.Object_Connect
           (Button, "clicked",
            Widget_Callback.To_Marshaller (Zoom_In'Access), Browser);
      end if;
   end Setup_Default_Toolbar;

   -----------------
   -- Get_Toolbar --
   -----------------

   function Get_Toolbar (Browser : access General_Browser_Record)
      return Gtk.Hbutton_Box.Gtk_Hbutton_Box is
   begin
      return Browser.Toolbar;
   end Get_Toolbar;

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
      use type Gdk_GC;

      B      : constant General_Browser := General_Browser (Browser);
      Color  : Gdk_Color;
      Kernel : constant Kernel_Handle := Get_Kernel (B);

   begin
      if B.Selected_Item_GC = null then
         B.Selected_Link_Color := Get_Pref (Kernel, Selected_Link_Color);
         B.Unselected_Link_Color := Get_Pref (Kernel, Unselected_Link_Color);

         Gdk_New (B.Selected_Item_GC, Get_Window (B.Canvas));
         Color := Get_Pref
           (Kernel, Glide_Kernel.Preferences.Selected_Item_Color);
         Set_Foreground (B.Selected_Item_GC, Color);

         Gdk_New (B.Default_Item_GC, Get_Window (B.Canvas));
         Color := Parse ("#FEFEFE");
         Alloc (Get_Default_Colormap, Color);
         Set_Foreground (B.Default_Item_GC, Color);

         Gdk_New (B.Parent_Linked_Item_GC, Get_Window (B.Canvas));
         Color := Get_Pref (Kernel, Parent_Linked_Item_Color);
         Set_Foreground (B.Parent_Linked_Item_GC, Color);

         Gdk_New (B.Child_Linked_Item_GC, Get_Window (B.Canvas));
         Color := Get_Pref (Kernel, Child_Linked_Item_Color);
         Set_Foreground (B.Child_Linked_Item_GC, Color);

         Gdk_New (B.Text_GC, Get_Window (B.Canvas));
         Set_Foreground
           (B.Text_GC, Get_Pref (B.Kernel, Browsers_Hyper_Link_Color));

         Gdk_New (B.Title_GC, Get_Window (B.Canvas));
         Set_Foreground (B.Title_GC,
                         Get_Pref (B.Kernel, GVD.Preferences.Title_Color));

         Gdk_New (Image_Canvas (B.Canvas).Bg_GC, Get_Window (B.Canvas));
         Set_Foreground (Image_Canvas (B.Canvas).Bg_GC,
                         Get_Pref (B.Kernel, Browsers_Bg_Color));
      end if;
   end Realized;

   ----------------
   -- Get_Canvas --
   ----------------

   function Get_Canvas (Browser : access General_Browser_Record)
      return Interactive_Canvas is
   begin
      return Browser.Canvas;
   end Get_Canvas;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   function Contextual_Factory
     (Item  : access Browser_Item_Record;
      Browser : access General_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Glide_Kernel.Selection_Context_Access
   is
      pragma Unreferenced (Item, Browser, Event, Menu);
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
      pragma Unreferenced (Event_Widget);
      B          : constant General_Browser := General_Browser (Object);
      Context    : Selection_Context_Access;
      Mitem      : Gtk_Menu_Item;
      Zooms_Menu : Gtk_Menu;
      Item       : Canvas_Item;
      Xr, Yr     : Gint;
      Xsave, Ysave : Gdouble;
      Success    : Boolean;

   begin
      if Get_Event_Type (Event) in Button_Press .. Button_Release then
         --  Click on an item: this is a file selection
         --  ??? Should we convert to world coordinates here ?

         Get_Origin (Get_Window (B.Canvas), Xr, Yr, Success);
         Set_X (Event, Get_X_Root (Event) - Gdouble (Xr));
         Set_Y (Event, Get_Y_Root (Event) - Gdouble (Yr));

         Item := Item_At_Coordinates (B.Canvas, Event);
      end if;

      if Item /= null then
         if Browser_Item (Item).Hide_Links then
            Gtk_New (Mitem, Label => -"Show links");
         else
            Gtk_New (Mitem, Label => -"Hide links");
         end if;
         Append (Menu, Mitem);
         Contextual_Cb.Connect
           (Mitem, "activate",
            Contextual_Cb.To_Marshaller (Toggle_Links'Access),
            (Browser => B, Item => Item, Zoom => 100));

         Gtk_New (Mitem, Label => -"Remove all other items");
         Append (Menu, Mitem);
         Contextual_Cb.Connect
           (Mitem, "activate",
            Contextual_Cb.To_Marshaller (Set_Root'Access),
            (Browser => B, Item => Item, Zoom => 100));

         Xsave := Get_X (Event);
         Ysave := Get_Y (Event);
         Set_X (Event, Get_X (Event) - Gdouble (Get_Coord (Item).X));
         Set_Y (Event, Get_Y (Event) - Gdouble (Get_Coord (Item).Y));
         Context := Contextual_Factory
           (Browser_Item (Item), B, Event, Menu);
         Set_X (Event, Xsave);
         Set_Y (Event, Ysave);
      end if;

      if Context = null then
         Context := new Selection_Context;

         Gtk_New (Mitem, Label => -"Refresh Layout");
         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, "activate",
            Widget_Callback.To_Marshaller (On_Refresh'Access), B);

         if Get_Orthogonal_Links (Get_Canvas (B)) then
            Gtk_New (Mitem, Label => -"Straight links");
         else
            Gtk_New (Mitem, Label => -"Orthogonal links");
         end if;

         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, "activate",
            Widget_Callback.To_Marshaller (Toggle_Orthogonal'Access), B);

         Gtk_New (Mitem, Label => -"Export...");
         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, "activate",
            Widget_Callback.To_Marshaller (On_Export'Access), B);

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
      end if;

      return Context;
   end Default_Browser_Context_Factory;

   --------------
   -- Set_Root --
   --------------

   procedure Set_Root
     (Mitem : access Gtk_Widget_Record'Class; Data : Cb_Data)
   is
      pragma Unreferenced (Mitem);
      Iter : Item_Iterator;
      Item : Canvas_Item;

   begin
      Push_State (Get_Kernel (Data.Browser), Busy);

      Iter := Start (Get_Canvas (Data.Browser));
      loop
         Item := Get (Iter);
         exit when Item = null;

         Next (Iter);

         if Item /= Data.Item then
            Remove (Get_Canvas (Data.Browser), Item);
         end if;
      end loop;

      Reset (Browser_Item (Data.Item), True, True);
      Refresh (Browser_Item (Data.Item));

      Layout (Data.Browser);
      Refresh_Canvas (Get_Canvas (Data.Browser));

      Show_Item (Get_Canvas (Data.Browser), Data.Item);

      Pop_State (Get_Kernel (Data.Browser));

   exception
      when E : others =>
         Pop_State (Get_Kernel (Data.Browser));
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Set_Root;

   ------------------
   -- Toggle_Links --
   ------------------

   procedure Toggle_Links
     (Mitem : access Gtk_Widget_Record'Class; Data : Cb_Data)
   is
      pragma Unreferenced (Mitem);
      It : constant Browser_Item := Browser_Item (Data.Item);
   begin
      It.Hide_Links := not It.Hide_Links;
      Refresh_Canvas (Get_Canvas (Data.Browser));
   end Toggle_Links;

   ---------------
   -- On_Export --
   ---------------

   procedure On_Export (Browser : access Gtk_Widget_Record'Class) is
      State_Pushed : Boolean := False;
      B            : constant General_Browser := General_Browser (Browser);
      Kernel       : constant Kernel_Handle := Get_Kernel (B);

   begin
      declare
         Name   : constant Virtual_File :=
           Select_File
             (Title             => -"Export Browser As PNG Image",
              Parent            => Get_Main_Window (Kernel),
              Default_Name      => "noname.png",
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Save_File,
              History           => Get_History (Kernel));
         Pixbuf : Gdk_Pixbuf;
         Error  : GError;

      begin
         if Name /= VFS.No_File then
            Push_State (Get_Kernel (B), Busy);
            State_Pushed := True;
            Pixbuf := Get_Pixbuf (B);

            if Pixbuf /= null then
               Save
                 (Pixbuf,
                  Locale_From_UTF8 (Full_Name (Name).all),
                  PNG,
                  Error);
               Unref (Pixbuf);
            end if;

            State_Pushed := False;
            Pop_State (Get_Kernel (B));
         end if;
      end;

   exception
      when E : others =>
         if State_Pushed then
            Pop_State (Get_Kernel (B));
         end if;

         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Export;

   ----------------
   -- On_Refresh --
   ----------------

   procedure On_Refresh (Browser : access Gtk_Widget_Record'Class) is
      B : constant General_Browser := General_Browser (Browser);
   begin
      Push_State (Get_Kernel (B), Busy);
      Layout (B, Force => True);
      Refresh_Canvas (Get_Canvas (B));
      Pop_State (Get_Kernel (B));

   exception
      when E : others =>
         Pop_State (Get_Kernel (B));
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Refresh;

   -----------------------
   -- Toggle_Orthogonal --
   -----------------------

   procedure Toggle_Orthogonal (Browser : access Gtk_Widget_Record'Class) is
      B : constant General_Browser := General_Browser (Browser);
   begin
      Set_Orthogonal_Links
        (Get_Canvas (B), not Get_Orthogonal_Links (Get_Canvas (B)));
      Refresh_Canvas (Get_Canvas (B));
   end Toggle_Orthogonal;

   -------------
   -- Zoom_In --
   -------------

   procedure Zoom_In (Browser : access Gtk_Widget_Record'Class) is
      Canvas : constant Interactive_Canvas := General_Browser (Browser).Canvas;
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
      Canvas : constant Interactive_Canvas := General_Browser (Browser).Canvas;
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
     (Browser : access Gtk_Widget_Record'Class; Data : Cb_Data)
   is
      pragma Unreferenced (Browser);
   begin
      Zoom (Data.Browser.Canvas, Data.Zoom, 1);
   end Zoom_Level;

   ---------------
   -- To_Brower --
   ---------------

   function To_Brower
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class)
      return General_Browser is
   begin
      return General_Browser (Get_Parent (Get_Parent (Canvas)));
   end To_Brower;

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight (Item : access Browser_Item_Record) is
   begin
      Redraw_Title_Bar (Browser_Item (Item));
   end Highlight;

   ---------------
   -- Draw_Link --
   ---------------

   procedure Draw_Link
     (Canvas      : access Interactive_Canvas_Record'Class;
      Link        : access Browser_Link_Record;
      Invert_Mode : Boolean;
      GC          : Gdk.GC.Gdk_GC;
      Edge_Number : Glib.Gint)
   is
      Browser : constant General_Browser := To_Brower (Canvas);
   begin
      if not Browser_Item (Get_Src (Link)).Hide_Links
        and then not Browser_Item (Get_Dest (Link)).Hide_Links
      then
         if Invert_Mode
           or else not
           (Is_Selected (Canvas, Canvas_Item (Get_Src (Link)))
            or else Is_Selected (Canvas, Canvas_Item (Get_Dest (Link))))
         then
            --  ??? Should force recomputation of the annotations so that they
            --  can have the same color as the link themselves. But since they
            --  are cached, the cache needs to be freed whenever we change the
            --  GC.
            Set_Foreground (GC, Browser.Unselected_Link_Color);
            Draw_Link
              (Canvas, Canvas_Link_Access (Link),
               Invert_Mode, GC, Edge_Number);
         else
            Set_Foreground (GC, Browser.Selected_Link_Color);
            Draw_Link
              (Canvas, Canvas_Link_Access (Link),
               Invert_Mode, GC, Edge_Number);
         end if;
      end if;
   end Draw_Link;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel (Browser : access General_Browser_Record)
      return Glide_Kernel.Kernel_Handle is
   begin
      return Browser.Kernel;
   end Get_Kernel;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Item    : access Browser_Item_Record;
      Parent_Removed, Child_Removed : Boolean)
   is
      pragma Unreferenced (Item, Parent_Removed, Child_Removed);
   begin
      null;
   end Reset;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Item : access Browser_Item_Record'Class;  Title : String := "") is
   begin
      if Title = "" then
         if Item.Title_Layout /= null then
            Unref (Item.Title_Layout);
            Item.Title_Layout := null;
         end if;
      else
         if Item.Title_Layout = null then
            Item.Title_Layout := Create_Pango_Layout (Item.Browser, Title);
            Set_Font_Description
              (Item.Title_Layout,
               Get_Pref (Get_Kernel (Get_Browser (Item)), Default_Font));
         else
            Set_Text (Item.Title_Layout, Title);
         end if;
      end if;
   end Set_Title;

   ----------------
   -- Close_Item --
   ----------------

   procedure Close_Item
     (Event : Gdk.Event.Gdk_Event;
      User  : access Browser_Item_Record'Class)
   is
      B    : constant General_Browser := Get_Browser (User);

      function Reset_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean;
      --  Reset the items linked to User.

      function Reset_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean
      is
         pragma Unreferenced (Canvas);
      begin
         if Get_Src (Link) = Vertex_Access (User) then
            Reset (Browser_Item (Get_Dest (Link)),
                   Parent_Removed => True,
                   Child_Removed  => False);
            Refresh (Browser_Item (Get_Dest (Link)));
         end if;

         if Get_Dest (Link) = Vertex_Access (User) then
            Reset (Browser_Item (Get_Src (Link)),
                   Parent_Removed => False,
                   Child_Removed  => True);
            Refresh (Browser_Item (Get_Src (Link)));
         end if;

         return True;
      end Reset_Item;

   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Release
      then
         For_Each_Link
           (Get_Canvas (B), Reset_Item'Unrestricted_Access,
            From => Canvas_Item (User));
         For_Each_Link
           (Get_Canvas (B), Reset_Item'Unrestricted_Access,
            To => Canvas_Item (User));

         Remove (Get_Canvas (B), User);
      end if;
   end Close_Item;

   ---------------------------
   -- Draw_Title_Bar_Button --
   ---------------------------

   procedure Draw_Title_Bar_Button
     (Item   : access Browser_Item_Record;
      Num    : Gint;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      Cb     : Active_Area_Callback'Class)
   is
      Button_Width  : constant Gint := Get_Width  (Item.Browser.Close_Pixmap);
      Button_Height : constant Gint := Get_Height (Item.Browser.Close_Pixmap);
      X             : constant Gint := Item.Title_Width + Item.Title_X -
        (Num + 1) * (Margin + Button_Width);
      Y, W, H       : Gint;
      Thick         : constant Gint :=
        Y_Thickness (Get_Style (Item.Browser.Canvas));

      use type Gdk.Gdk_Drawable;

   begin
      --  No title ? Don't draw any button

      if Item.Title_Layout = null
        or else Pixmap (Item) = null
      then
         return;
      end if;

      Get_Pixel_Size (Item.Title_Layout, W, H);
      Y := (H - Button_Height) / 2 + Thick + Item.Title_Y;

      Draw_Shadow
        (Style       => Get_Style (Item.Browser),
         Window      => Pixmap (Item),
         State_Type  => State_Normal,
         Shadow_Type => Shadow_Out,
         X           => X,
         Y           => Y,
         Width       => Button_Width,
         Height      => Button_Height);

      Render_To_Drawable_Alpha
        (Pixbuf       => Pixbuf,
         Drawable     => Pixmap (Item),
         Src_X        => 0,
         Src_Y        => 0,
         Dest_X       => X,
         Dest_Y       => Y,
         Width        => -1,
         Height       => -1,
         Alpha        => Alpha_Full,
         Alpha_Threshold => 128);

      Add_Active_Area
        (Item,
         Gdk_Rectangle'(X, Y, Button_Width, Button_Height),
         Cb);
   end Draw_Title_Bar_Button;

   ----------------------
   -- Redraw_Title_Bar --
   ----------------------

   procedure Redraw_Title_Bar (Item : access Browser_Item_Record) is
      W, H : Gint;
      XThick : constant Gint := X_Thickness (Get_Style (Item.Browser.Canvas));
      YThick : constant Gint := Y_Thickness (Get_Style (Item.Browser.Canvas));

      use type Gdk.Gdk_Drawable;

   begin
      if Item.Title_Layout = null
        or else Pixmap (Item) = null
      then
         return;
      end if;

      Reset_Active_Areas (Item.all, Other_Areas => False);

      Get_Pixel_Size (Item.Title_Layout, W, H);
      Draw_Rectangle
        (Pixmap (Item),
         GC     => Get_Title_Background_GC (Browser_Item (Item)),
         Filled => True,
         X      => Item.Title_X + YThick,
         Y      => Item.Title_Y + YThick,
         Width  => Item.Title_Width - 2 * XThick,
         Height => H);
      Draw_Layout
        (Drawable => Pixmap (Item),
         GC       => Get_Black_GC (Get_Style (Item.Browser)),
         X        => Margin + XThick + Item.Title_X,
         Y        => YThick + Item.Title_Y,
         Layout   => Item.Title_Layout);
      Draw_Line
        (Pixmap (Item),
         Gc     => Get_Black_GC (Get_Style (Item.Browser)),
         X1     => XThick + Item.Title_X,
         Y1     => H + YThick + Item.Title_Y,
         X2     => Item.Title_Width + Item.Title_X - XThick - 1,
         Y2     => H + YThick + Item.Title_Y);
      Draw_Title_Bar_Button
        (Item   => Item,
         Num    => 0,
         Pixbuf => Item.Browser.Close_Pixmap,
         Cb     => Build (Close_Item'Access, Item));
   end Redraw_Title_Bar;

   ----------------------------
   -- Get_Last_Button_Number --
   ----------------------------

   function Get_Last_Button_Number (Item : access Browser_Item_Record)
      return Gint
   is
      pragma Unreferenced (Item);
   begin
      return 0;
   end Get_Last_Button_Number;

   -----------------------------
   -- Get_Title_Background_GC --
   -----------------------------

   function Get_Title_Background_GC
     (Item : access Browser_Item_Record) return Gdk.GC.Gdk_GC
   is
      B : constant General_Browser := Get_Browser (Item);
      Canvas : constant Interactive_Canvas := Get_Canvas (B);
      Iter : Item_Iterator;
   begin
      if Is_Selected (Canvas, Item) then
         return Get_Selected_Item_GC (B);

      else
         Iter := Start (Canvas, Linked_From_Or_To => Canvas_Item (Item));
         while Get (Iter) /= null loop
            if Is_Selected (Canvas, Get (Iter)) then
               if Is_Linked_From (Iter) then
                  return Get_Child_Linked_Item_GC (B);
               else
                  return Get_Parent_Linked_Item_GC (B);
               end if;
            end if;
            Next (Iter);
         end loop;
      end if;

      --  ??? Should use different color
      return Item.Browser.Title_GC;
   end Get_Title_Background_GC;

   -----------------------
   -- Get_Background_GC --
   -----------------------

   function Get_Background_GC
     (Item : access Browser_Item_Record) return Gdk.GC.Gdk_GC is
   begin
      return Item.Browser.Default_Item_GC;
   end Get_Background_GC;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   procedure Resize_And_Draw
     (Item             : access Browser_Item_Record;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class)
   is
      pragma Unreferenced (Layout);
      Num_Buttons   : constant Gint := 1 + Get_Last_Button_Number
        (Browser_Item (Item));  --  dispatching call
      Button_Width  : constant Gint := Get_Width (Item.Browser.Close_Pixmap);
      W, H  : Gint;
      Layout_H : Gint := 0;
      Bg_GC : Gdk_GC;
   begin
      Reset_Active_Areas (Item.all, Title_Bar_Areas => False);

      if Item.Title_Layout /= null then
         Get_Pixel_Size (Item.Title_Layout, W, Layout_H);
         W := Gint'Max
           (W + 2 * Margin + Num_Buttons * (Margin + Button_Width), Width);
         H := Layout_H + Height
           + 2 * Y_Thickness (Get_Style (Item.Browser.Canvas));
      else
         W := Width;
         H := Height;
      end if;

      Item.Title_Width := W;
      Item.Title_X := Xoffset;
      Item.Title_Y := Yoffset;

      W := W + Width_Offset;
      H := H + Height_Offset;

      Set_Screen_Size (Browser_Item (Item), W, H);

      Bg_GC := Get_Background_GC (Browser_Item (Item));

      Draw_Rectangle
        (Pixmap (Item),
         GC     => Bg_GC,
         Filled => True,
         X      => Xoffset,
         Y      => Layout_H + Yoffset,
         Width  => W - Width_Offset,
         Height => H - Layout_H - Height_Offset);

      Draw_Shadow
        (Style       => Get_Style (Item.Browser.Canvas),
         Window      => Pixmap (Item),
         State_Type  => State_Normal,
         Shadow_Type => Shadow_Out,
         X           => Xoffset,
         Y           => Yoffset,
         Width       => W - Width_Offset - Xoffset,
         Height      => H - Height_Offset - Yoffset);

      if Item.Title_Layout /= null then
         Yoffset := Yoffset + Layout_H;
      end if;
   end Resize_And_Draw;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
     (Item  : access Browser_Item_Record;
      Event : Gdk.Event.Gdk_Event_Button) is
   begin
      Raise_Item (Get_Canvas (Get_Browser (Item)), Item);
      Activate (Browser_Item (Item), Event);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Button_Click;

   -----------------
   -- Get_Browser --
   -----------------

   function Get_Browser (Item : access Browser_Item_Record'Class)
      return General_Browser is
   begin
      return Item.Browser;
   end Get_Browser;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item    : access Browser_Item_Record'Class;
      Browser : access General_Browser_Record'Class) is
   begin
      Item.Browser := General_Browser (Browser);
   end Initialize;

   --------------
   -- Selected --
   --------------

   procedure Selected
     (Item        : access Browser_Item_Record;
      Canvas      : access Interactive_Canvas_Record'Class;
      Is_Selected : Boolean)
   is
      pragma Unreferenced (Is_Selected);
      --  Call Highlight on Item and all its siblings. If the selection status
      --  has changed, this will result in a change of background color for
      --  these items.
      Iter : Item_Iterator;
      It  : Canvas_Item;
   begin
      Highlight (Browser_Item (Item));

      Iter := Start (Canvas, Canvas_Item (Item));
      loop
         It := Get (Iter);
         exit when It = null;

         Highlight (Browser_Item (It));

         Next (Iter);
      end loop;

      --  We need to redraw the whole canvas, so that the links are correctly
      --  updated. If multiple items are selected, this isn't a problem since
      --  gtk+ will coalesce the two events anyway.
      Refresh_Canvas (Canvas);
   end Selected;

   ----------
   -- Call --
   ----------

   function Call
     (Callback : Item_Active_Area_Callback;
      Event    : Gdk.Event.Gdk_Event) return Boolean is
   begin
      Callback.Cb (Event, Callback.User_Data);
      return True;
   end Call;

   ---------------------
   -- Add_Active_Area --
   ---------------------

   procedure Add_Active_Area
     (Item      : access Browser_Item_Record;
      Rectangle : Gdk.Rectangle.Gdk_Rectangle;
      Callback  : Active_Area_Callback'Class)
   is
      Tmp   : Active_Area_Tree;

      function Rectangle_In (Out_Rect, In_Rect : Gdk_Rectangle) return Boolean;
      --  Return true if In_Rect is fully contained in Out_Rect

      procedure Join_Areas (Area : in out Active_Area_Tree);
      --  Create a new area that englobs both Area and Tmp as its children

      procedure Process_Area
        (Area : in out Active_Area_Tree;
         Inserted : out Boolean);
      --  Insert the new Tmp item below area, or possibly to replace area (the
      --  latter becoming one of the children of Tmp.
      --  Inserted is set to True if the item was inserted, to False if it
      --  didn't belong to the hierarchy of Area.

      ------------------
      -- Rectangle_In --
      ------------------

      function Rectangle_In (Out_Rect, In_Rect : Gdk_Rectangle)
         return Boolean is
      begin
         return In_Rect.X >= Out_Rect.X
           and then In_Rect.X + In_Rect.Width <= Out_Rect.X + Out_Rect.Width
           and then In_Rect.Y >= Out_Rect.Y
           and then In_Rect.Y + In_Rect.Height <= Out_Rect.Y + Out_Rect.Height;
      end Rectangle_In;

      ----------------
      -- Join_Areas --
      ----------------

      procedure Join_Areas (Area : in out Active_Area_Tree) is
         Tmp_R : Gdk_Rectangle;
         Tmp_Children : Active_Area_Tree_Array_Access;
      begin
         Tmp_R.X := Gint'Min (Rectangle.X, Area.Rectangle.X);
         Tmp_R.Y := Gint'Min (Rectangle.Y, Area.Rectangle.Y);
         Tmp_R.Width :=
           Gint'Max (Rectangle.X + Rectangle.Width,
                        Area.Rectangle.X + Area.Rectangle.Width) - Tmp_R.X;
         Tmp_R.Height :=
           Gint'Max (Rectangle.Y + Rectangle.Height,
                     Area.Rectangle.Y + Area.Rectangle.Height) - Tmp_R.Y;

         --  Slight optimization: expand an existing rectangle instead of
         --  creating a new one. This avoids creating a tree too deep
         if Area.Callback = null then
            Area.Rectangle := Tmp_R;

            Tmp_Children := Area.Children;
            Area.Children := new Active_Area_Tree_Array'
              (Tmp_Children.all & Tmp);
            Unchecked_Free (Tmp_Children);

         --  We can't optimize here, since the area is associated with a
         --  callback and was specified by the user
         else
            Area := new Active_Area_Tree_Record'
              (Rectangle => Tmp_R,
               Callback  => null,
               Children  => new Active_Area_Tree_Array'(1 => Area, 2 => Tmp));
         end if;
      end Join_Areas;

      ------------------
      -- Process_Area --
      ------------------

      procedure Process_Area
        (Area : in out Active_Area_Tree; Inserted : out Boolean)
      is
         Tmp_Children : Active_Area_Tree_Array_Access;
      begin
         Inserted := False;

         --  If Area is a child of the new Tmp area.
         if Rectangle_In (Rectangle, Area.Rectangle) then
            Tmp.Children := new Active_Area_Tree_Array'(1 => Area);
            Area := Tmp;
            Inserted := True;

         --  The new item is a child of item.Active_Areas, or one of its
         --  grand-children...
         elsif Rectangle_In (Area.Rectangle, Rectangle) then
            if Area.Children /= null then
               for C in Area.Children'Range loop
                  Process_Area (Area.Children (C), Inserted);
                  exit when Inserted;
               end loop;
            end if;

            if not Inserted then
               Tmp_Children := Area.Children;
               Area.Children := new Active_Area_Tree_Array'
                 (Tmp_Children.all & Tmp);
               Unchecked_Free (Tmp_Children);
               Inserted := True;
            end if;
         end if;
      end Process_Area;

      Inserted : Boolean;
   begin
      Tmp := new Active_Area_Tree_Record'
        (Rectangle => Rectangle,
         Callback  => new Active_Area_Callback'Class'(Callback),
         Children  => null);

      if Item.Active_Areas = null then
         Item.Active_Areas := Tmp;
      else
         Process_Area (Item.Active_Areas, Inserted);
         if not Inserted then
            Join_Areas (Item.Active_Areas);
         end if;
      end if;
   end Add_Active_Area;

   --------------
   -- Activate --
   --------------

   procedure Activate
     (Item  : access Browser_Item_Record;
      Event : Gdk.Event.Gdk_Event)
   is
      X : constant Glib.Gint := Gint (Get_X (Event));
      Y : constant Glib.Gint := Gint (Get_Y (Event));

      function Check_Area (Area : Active_Area_Tree) return Boolean;
      --  Return True if Area or one of its children was activated

      ----------------
      -- Check_Area --
      ----------------

      function Check_Area (Area : Active_Area_Tree) return Boolean is
      begin
         if Area /= null
           and then X >= Area.Rectangle.X
           and then X <= Area.Rectangle.X + Area.Rectangle.Width
           and then Y >= Area.Rectangle.Y
           and then Y <= Area.Rectangle.Y + Area.Rectangle.Height
         then
            if Area.Children /= null then
               for C in Area.Children'Range loop
                  if Check_Area (Area.Children (C)) then
                     return True;
                  end if;
               end loop;
            end if;

            if Area.Callback /= null then
               return Call (Area.Callback.all, Event);
            end if;
         end if;
         return False;
      end Check_Area;

      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Tmp := Check_Area (Item.Active_Areas);
   end Activate;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Me : Debug_Handle; Tree : Active_Area_Tree; Indent : Natural := 0)
   is
      Id : constant String := (1 .. Indent => ' ');
   begin
      Trace (Me, Id & Tree.Rectangle.X'Img
             & Tree.Rectangle.Y'Img & Tree.Rectangle.Width'Img
             & Tree.Rectangle.Height'Img);
      if Tree.Children /= null then
         for C in Tree.Children'Range loop
            Dump (Me, Tree.Children (C), Indent + 2);
         end loop;
      end if;
   end Dump;

   ------------------------
   -- Reset_Active_Areas --
   ------------------------

   procedure Reset_Active_Areas
     (Item            : in out Browser_Item_Record;
      Title_Bar_Areas : Boolean := True;
      Other_Areas     : Boolean := True)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Active_Area_Tree_Record, Active_Area_Tree);

      Title_Bar_Height : Gint;
      W : Gint;

      procedure Free (Area : in out Active_Area_Tree);
      --  Free Area and its children.
      --  Area might not be destroyed, depending on Title_Bar_Areas and
      --  Other_Areas.

      procedure Free (Area : in out Active_Area_Tree) is
         Should_Free : Boolean := True;
         In_Title : Boolean;
         Count : Natural := 0;
         Tmp_Children : Active_Area_Tree_Array_Access;
      begin
         if Area.Children /= null then
            for C in Area.Children'Range loop
               Free (Area.Children (C));
               if Area.Children (C) /= null then
                  Should_Free := False;
                  Count := Count + 1;
               end if;
            end loop;

            if Should_Free then
               Unchecked_Free (Area.Children);
            elsif Count /= Area.Children'Length then
               --  Make sure there are no null values in Children, to keep the
               --  tree simple.
               Tmp_Children := Area.Children;
               Area.Children := new Active_Area_Tree_Array (1 .. Count);

               Count := Area.Children'First;
               for C in Tmp_Children'Range loop
                  if Tmp_Children (C) /= null then
                     Area.Children (Count) := Tmp_Children (C);
                     Count := Count + 1;
                  end if;
               end loop;
               Unchecked_Free (Tmp_Children);
            end if;
         end if;

         In_Title :=
           Area.Rectangle.Y + Area.Rectangle.Height <= Title_Bar_Height;

         if (In_Title and then Title_Bar_Areas)
           or else (not In_Title and then Other_Areas)
         then
            if Area.Callback /= null then
               Destroy (Area.Callback.all);
               Unchecked_Free (Area.Callback);
            end if;
         else
            Should_Free := False;
         end if;

         if Should_Free then
            Unchecked_Free (Area);
         end if;
      end Free;

   begin
      if Item.Active_Areas /= null then
         if Item.Title_Layout = null then
            Title_Bar_Height := 0;
         else
            Get_Pixel_Size (Item.Title_Layout, W, Title_Bar_Height);
         end if;

         Free (Item.Active_Areas);
      end if;
   end Reset_Active_Areas;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Callback : in out Active_Area_Callback) is
      pragma Unreferenced (Callback);
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out Browser_Item_Record) is
   begin
      Reset_Active_Areas (Browser_Item_Record'Class (Item));
      Destroy (Buffered_Item_Record (Item));
   end Destroy;

   -----------
   -- Build --
   -----------

   function Build
     (Cb : Item_Active_Callback;
      User : access Browser_Item_Record'Class)
      return Item_Active_Area_Callback'Class is
   begin
      return Item_Active_Area_Callback'
        (Active_Area_Callback with
         User_Data => Browser_Item (User),
         Cb        => Cb);
   end Build;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Item : access Browser_Item_Record'Class) is
      Xoffset, Yoffset : Gint := 0;
      Layout : Pango_Layout;
   begin
      Layout := Create_Pango_Layout (Get_Browser (Item), "");
      Set_Font_Description
        (Layout,
         Get_Pref (Get_Kernel (Get_Browser (Item)), Default_Font));

      Resize_And_Draw (Item, 0, 0, 0, 0, Xoffset, Yoffset, Layout);
      Redraw_Title_Bar (Item);

      Unref (Layout);
   end Refresh;

   --------------------------
   -- Refresh_Linked_Items --
   --------------------------

   procedure Refresh_Linked_Items
     (Item             : access Browser_Item_Record'Class;
      Refresh_Parents  : Boolean := False;
      Refresh_Children : Boolean := False)
   is
      Iter : Item_Iterator :=
        Start (Get_Canvas (Get_Browser (Item)), Canvas_Item (Item));
      It   : Browser_Item;
   begin
      loop
         It := Browser_Item (Get (Iter));
         exit when It = null;

         if (Refresh_Children and then not Is_Linked_From (Iter))
           or else (Refresh_Parents and then Is_Linked_From (Iter))
         then
            Refresh (It);
         end if;

         Next (Iter);
      end loop;
   end Refresh_Linked_Items;

   ------------
   -- Layout --
   ------------

   procedure Layout
     (Browser : access General_Browser_Record;
      Force : Boolean := False) is
   begin
      Layout
        (Get_Canvas (Browser),
         Force => Force,
         Vertical_Layout =>
           Get_Pref (Get_Kernel (Browser), Browsers_Vertical_Layout));
   end Layout;

   ------------------------------------
   -- Get_Default_Item_Background_GC --
   ------------------------------------

   function Get_Default_Item_Background_GC
     (Browser : access General_Browser_Record) return Gdk.GC.Gdk_GC is
   begin
      return Browser.Default_Item_GC;
   end Get_Default_Item_Background_GC;

   --------------------------
   -- Get_Selected_Item_GC --
   --------------------------

   function Get_Selected_Item_GC
     (Browser : access General_Browser_Record) return Gdk.GC.Gdk_GC is
   begin
      return Browser.Selected_Item_GC;
   end Get_Selected_Item_GC;

   -------------------------------
   -- Get_Parent_Linked_Item_GC --
   -------------------------------

   function Get_Parent_Linked_Item_GC
     (Browser : access General_Browser_Record) return Gdk.GC.Gdk_GC is
   begin
      return Browser.Parent_Linked_Item_GC;
   end Get_Parent_Linked_Item_GC;

   ------------------------------
   -- Get_Child_Linked_Item_GC --
   ------------------------------

   function Get_Child_Linked_Item_GC
     (Browser : access General_Browser_Record) return Gdk.GC.Gdk_GC is
   begin
      return Browser.Child_Linked_Item_GC;
   end Get_Child_Linked_Item_GC;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Xref_List) is
   begin
      if List.Lines /= null then
         for L in List.Lines'Range loop
            Free (List.Lines (L).Text);

            for C in List.Lines (L).Callbacks'Range loop
               --  Do not actually destroy, since these are still used in the
               --  callbacks.
               Unchecked_Free (List.Lines (L).Callbacks (C));
            end loop;

            Unchecked_Free (List.Lines (L).Callbacks);
         end loop;
      end if;
   end Free;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (List     : Xref_List;
      Num      : Positive;
      Num_In_Line : Positive := 1;
      Callback : out Active_Area_Cb;
      Text     : out GNAT.OS_Lib.String_Access)
   is
      N : Natural;
   begin
      if List.Lines = null
        or else Num > List.Lines'Length
      then
         Callback := null;
         Text     := null;
      else
         N := List.Lines'First + Num - 1;
         Text := List.Lines (N).Text;

         if Num_In_Line > List.Lines (N).Callbacks'Length then
            Callback := null;
         else
            Callback := List.Lines (N).Callbacks
              (Num_In_Line - 1 + List.Lines (N).Callbacks'First);
         end if;
      end if;
   end Get_Line;

   -----------------
   -- Remove_Line --
   -----------------

   procedure Remove_Line (List : in out Xref_List; Num : Positive) is
      L : Xref_Line_Array_Access := List.Lines;
   begin
      if List.Lines /= null and then Num <= List.Lines'Length then
         List.Lines := new Xref_Line_Array (L'First .. L'Last - 1);
         List.Lines.all := L (L'First .. Num - 1) & L (Num + 1 .. L'Last);
         Unchecked_Free (L);
      end if;
   end Remove_Line;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (List     : in out Xref_List;
      Str      : String;
      Length1  : Natural              := Natural'Last;
      Callback : Active_Area_Cb_Array := Empty_Cb_Array)
   is
      L : Xref_Line_Array_Access := List.Lines;
   begin
      if L /= null then
         List.Lines := new Xref_Line_Array (L'First .. L'Last + 1);
         List.Lines (L'Range) := L.all;
         Unchecked_Free (L);
      else
         List.Lines := new Xref_Line_Array (1 .. 1);
      end if;

      List.Lines (List.Lines'Last) :=
        (Text      => new String'(Str),
         Callbacks => new Active_Area_Cb_Array'(Callback),
         Length    => Length1);
   end Add_Line;

   -----------------
   -- Expand_Line --
   -----------------

   procedure Expand_Line
     (List     : in out Xref_List;
      Num      : Positive;
      Str      : String;
      Callback : Active_Area_Cb_Array := Empty_Cb_Array)
   is
      Tmp : Xref_Line;
   begin
      if List.Lines = null or else Num > List.Lines'Length then
         Add_Line (List, Str, Callback => Callback);
      else
         Tmp := List.Lines (Num - 1 + List.Lines'First);
         List.Lines (Num - 1 + List.Lines'First) :=
           (Text      => new String'(Tmp.Text.all & Str),
            Callbacks => new Active_Area_Cb_Array'
              (Tmp.Callbacks.all & Callback),
            Length    => Tmp.Length);
         Free (Tmp.Text);
         Unchecked_Free (Tmp.Callbacks);
      end if;
   end Expand_Line;

   --------------------
   -- Get_Pixel_Size --
   --------------------

   procedure Get_Pixel_Size
     (Browser   : access General_Browser_Record'Class;
      List      : Xref_List;
      W1, W2, H : out Gint;
      Layout    : access Pango_Layout_Record'Class)
   is
      Descr              : constant Pango_Font_Description :=
        Get_Pref (Get_Kernel (Browser), Default_Font);
      Font               : Pango_Font;
      Metrics            : Pango_Font_Metrics;
      Longest1, Longest2 : Gint := 0;
      H2, W              : Gint;
      Last               : Natural;
      Line               : GNAT.Strings.String_Access;

   begin
      H := 0;

      if List.Lines /= null then
         Font := Load_Font (Get_Pango_Context (Browser), Descr);
         Metrics := Get_Metrics (Font);

         for L in List.Lines'Range loop
            Line := List.Lines (L).Text;
            Last := Natural'Min (List.Lines (L).Length, Line'Length);

            --  First column
            declare
               Str   : String (1 .. Last);
               Index : Natural := Str'First;
            begin
               --  ??? Should use UTF8-functions to traverse the string
               for S in Line'First .. Line'First + Last - 1 loop
                  if Line (S) /= '@' then
                     Str (Index) := Line (S);
                     Index := Index + 1;
                  end if;
               end loop;

               Set_Text (Layout, Str (Str'First .. Index - 1));
               Get_Pixel_Size (Layout, W, H2);
               H := H + H2;
               Longest1 := Gint'Max (Longest1, W);
            end;

            --  Second column
            if L < Line'Length then
               declare
                  Str   : String (1 .. Line'Length - Last);
                  Index : Natural := Str'First;
               begin
                  for S in Line'First + Last .. Line'Last loop
                     if Line (S) /= '@' then
                        Str (Index) := Line (S);
                        Index := Index + 1;
                     end if;
                  end loop;

                  Set_Text (Layout, Str (Str'First .. Index - 1));
                  Get_Pixel_Size (Layout, W, H2);
                  Longest2 := Gint'Max (Longest2, W);
               end;
            end if;
         end loop;

         Unref (Metrics);
         Unref (Font);
      end if;

      W1 := Longest1;
      W2 := Longest2;
   end Get_Pixel_Size;

   -------------------
   -- Display_Lines --
   -------------------

   procedure Display_Lines
     (Item          : access Browser_Item_Record'Class;
      List          : Xref_List;
      X             : Gint;
      Y             : in out Gint;
      Second_Column : Gint;
      Layout        : access Pango_Layout_Record'Class)
   is
      Browser     : constant General_Browser := Get_Browser (Item);
      X2          : Gint;
      First, Last : Integer;
      In_Xref     : Boolean;
      GC          : Gdk_GC;
      W, H        : Gint;
      Num_In_Line : Natural;
      Text        : String_Access;

      procedure Display (Text : String; Line : Xref_Line);
      --  Display Text on Item.

      procedure Display (Text : String; Line : Xref_Line) is
      begin
         if Text'Length = 0 then
            return;
         end if;

         Set_Text (Layout, Text);

         if In_Xref then
            GC := Browser.Text_GC;
         else
            GC := Get_Black_GC (Get_Style (Browser));
         end if;

         Draw_Layout
           (Drawable => Pixmap (Item),
            GC       => GC,
            X        => X2,
            Y        => Y,
            Layout   => Pango_Layout (Layout));

         Get_Pixel_Size (Layout, W, H);

         if In_Xref then
            Draw_Line (Pixmap (Item), GC, X2, Y + H, X2 + W, Y + H);

            if Num_In_Line <= Line.Callbacks'Length
              and then Line.Callbacks
                (Num_In_Line - 1 + Line.Callbacks'First) /= null
            then
               Add_Active_Area
                 (Item,
                  Gdk_Rectangle'(X2, Y, W, H),
                  Line.Callbacks
                    (Num_In_Line - 1 + Line.Callbacks'First).all);
            end if;
         end if;

         X2 := X2 + W;
      end Display;

   begin
      if List.Lines = null then
         return;
      end if;

      for L in List.Lines'Range loop
         Text        := List.Lines (L).Text;
         First       := Text'First;
         Last        := First;
         X2          := X;
         In_Xref     := False;
         Num_In_Line := 0;

         while Last <= Text'Last loop
            if Last - Text'First = List.Lines (L).Length then
               Display (Text (First .. Last - 1), List.Lines (L));
               First   := Last;
               X2      := X + Second_Column;
            end if;

            if Text (Last) = '@' then
               Display (Text (First .. Last - 1), List.Lines (L));
               First   := Last + 1;
               In_Xref := not In_Xref;

               if In_Xref then
                  Num_In_Line := Num_In_Line + 1;
               end if;
            end if;

            Last := Last + 1;
         end loop;

         Display (Text (First .. Last - 1), List.Lines (L));

         --  No need to query the size again, we just did

         Y := Y + H;
      end loop;
   end Display_Lines;

   -----------------------
   -- Get_Parents_Arrow --
   -----------------------

   function Get_Parents_Arrow
     (Browser : access General_Browser_Record) return Gdk.Pixbuf.Gdk_Pixbuf is
   begin
      return Browser.Up_Arrow;
   end Get_Parents_Arrow;

   ------------------------
   -- Get_Children_Arrow --
   ------------------------

   function Get_Children_Arrow
     (Browser : access General_Browser_Record) return Gdk.Pixbuf.Gdk_Pixbuf is
   begin
      return Browser.Down_Arrow;
   end Get_Children_Arrow;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item    : access Arrow_Item_Record'Class;
      Browser : access General_Browser_Record'Class;
      Title   : String;
      Parents_Cb, Children_Cb : Arrow_Item_Callback) is
   begin
      Initialize (Item, Browser);
      Set_Title (Item, Title);
      Item.Parents_Cb  := Parents_Cb;
      Item.Children_Cb := Children_Cb;
   end Initialize;

   -------------------
   -- Parents_Shown --
   -------------------

   function Parents_Shown (Item : access Arrow_Item_Record) return Boolean is
   begin
      return Item.Parents_Shown;
   end Parents_Shown;

   --------------------
   -- Children_Shown --
   --------------------

   function Children_Shown (Item : access Arrow_Item_Record) return Boolean is
   begin
      return Item.Children_Shown;
   end Children_Shown;

   -----------------------
   -- Set_Parents_Shown --
   -----------------------

   procedure Set_Parents_Shown
     (Item : access Arrow_Item_Record; All_Shown : Boolean) is
   begin
      Item.Parents_Shown := All_Shown;
   end Set_Parents_Shown;

   ------------------------
   -- Set_Children_Shown --
   ------------------------

   procedure Set_Children_Shown
     (Item : access Arrow_Item_Record; All_Shown : Boolean) is
   begin
      Item.Children_Shown := All_Shown;
   end Set_Children_Shown;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Item : access Arrow_Item_Record;
      Parent_Removed, Child_Removed : Boolean) is
   begin
      if Parent_Removed then
         Item.Parents_Shown := False;
      end if;

      if Child_Removed then
         Item.Children_Shown := False;
      end if;
   end Reset;

   ----------------------------
   -- Get_Last_Button_Number --
   ----------------------------

   function Get_Last_Button_Number (Item : access Arrow_Item_Record)
      return Glib.Gint is
   begin
      return Get_Last_Button_Number
        (Browser_Item_Record (Item.all)'Access) + 2;
   end Get_Last_Button_Number;

   ---------------------
   -- Compute_Parents --
   ---------------------

   procedure Compute_Parents
     (Event : Gdk_Event; Item : access Browser_Item_Record'Class) is
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Release
      then
         Arrow_Item (Item).Parents_Cb (Arrow_Item (Item));
      end if;
   end Compute_Parents;

   ----------------------
   -- Compute_Children --
   ----------------------

   procedure Compute_Children
     (Event : Gdk_Event; Item : access Browser_Item_Record'Class) is
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Release
      then
         Arrow_Item (Item).Children_Cb (Arrow_Item (Item));
      end if;
   end Compute_Children;

   ----------------------
   -- Redraw_Title_Bar --
   ----------------------

   procedure Redraw_Title_Bar (Item : access Arrow_Item_Record) is
   begin
      Redraw_Title_Bar (Browser_Item_Record (Item.all)'Access);

      if not Item.Parents_Shown then
         Draw_Title_Bar_Button
           (Item,
            Num    => Get_Last_Button_Number (Item),
            Pixbuf => Get_Parents_Arrow (Get_Browser (Item)),
            Cb     => Build (Compute_Parents'Access, Item));
      end if;

      if not Item.Children_Shown then
         Draw_Title_Bar_Button
           (Item,
            Num    => Get_Last_Button_Number (Item) - 1,
            Pixbuf => Get_Children_Arrow (Get_Browser (Item)),
            Cb     => Build (Compute_Children'Access, Item));
      end if;
   end Redraw_Title_Bar;

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf
     (Browser : access General_Browser_Record) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      use type Gdk.Gdk_Drawable;

      Canvas              : constant Image_Canvas :=
                              Image_Canvas (Browser.Canvas);
      X, Y, Width, Height : Gint;
      X_Value, Y_Value    : Gdouble;
      Src                 : Gdk_Window;
      Pixbuf              : Gdk_Pixbuf;
      Zoom_Level          : Guint;

   begin
      Src := Get_Window (Canvas);

      --  Temporarily reset zoom level

      Zoom_Level := Get_Zoom (Canvas);
      Zoom (Canvas);

      Get_World_Coordinates (Canvas, X, Y, Width, Height);
      Gdk_New (Canvas.Pixmap, Src, Width, Height);

      if Canvas.Pixmap = null then
         return null;
      end if;

      --  Temporarily reset scroll values

      X_Value := Get_Value (Get_Hadj (Canvas));
      Y_Value := Get_Value (Get_Vadj (Canvas));
      Set_Value (Get_Hadj (Canvas), 0.0);
      Set_Value (Get_Vadj (Canvas), 0.0);

      --  Force a complete redraw on Canvas.Pixmap, so that we can
      --  copy the whole contents of Canvas to a pixbuf.
      --  ??? Does not work properly when X < 0 or Y < 0

      Draw_Area (Canvas, (0, 0, Width, Height));
      Pixbuf := Get_From_Drawable
        (Dest   => null,
         Src    => Canvas.Pixmap,
         Cmap   => null,
         Src_X  => 0,
         Src_Y  => 0,
         Dest_X => 0,
         Dest_Y => 0,
         Width  => Width,
         Height => Height);
      Set_Value (Get_Hadj (Canvas), X_Value);
      Set_Value (Get_Vadj (Canvas), Y_Value);
      Gdk.Pixmap.Unref (Canvas.Pixmap);
      Canvas.Pixmap := null;
      Zoom (Canvas, Zoom_Level);

      return Pixbuf;
   end Get_Pixbuf;

end Browsers.Canvas;
