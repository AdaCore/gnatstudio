-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2001-2002                    --
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
with Glib.Graphs;         use Glib.Graphs;
with Glib.Object;         use Glib.Object;
with Glib.Values;         use Glib.Values;
with Gdk.Color;           use Gdk.Color;
with Gdk.GC;              use Gdk.GC;
with Gtkada.Canvas;       use Gtkada.Canvas;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Gdk.Drawable;        use Gdk.Drawable;
with Gdk.Event;           use Gdk.Event;
with Gdk.Pixbuf;          use Gdk.Pixbuf;
with Gdk.Rectangle;       use Gdk.Rectangle;
with Gdk.Types.Keysyms;   use Gdk.Types.Keysyms;
with Gdk.Window;          use Gdk.Window;
with Gtk.Accel_Group;     use Gtk.Accel_Group;
with Gtk.Arguments;       use Gtk.Arguments;
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

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with GVD.Preferences;           use GVD.Preferences;
with Glide_Intl;                use Glide_Intl;
with Layouts;                   use Layouts;
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

   Me : constant Debug_Handle := Create ("Browsers.Canvas");

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

   procedure On_Refresh (Browser : access Gtk_Widget_Record'Class);
   --  Recompute the layout of the canvas

   procedure Toggle_Links
     (Mitem : access Gtk_Widget_Record'Class; Data : Cb_Data);
   --  Toggle the display of links for the item

   procedure Toggle_Orthogonal (Browser : access Gtk_Widget_Record'Class);
   --  Toggle the layout of links.

   procedure On_Draw_Links
     (Browser : access Gtk_Widget_Record'Class;
      Args    : Glib.Values.GValues);
   --  Make sure that the highlighted links are always drawn last.

   procedure Set_Root
     (Mitem : access Gtk_Widget_Record'Class; Data : Cb_Data);
   --  Remove all items except the one described in Data from the canvas.

   procedure On_Background_Click
     (Browser : access Gtk_Widget_Record'Class);
   --  Called when the user clicked in the background of the canvas

   procedure Close_Item
     (Event : Gdk.Event.Gdk_Event; User  : access Browser_Item_Record'Class);
   --  Close an item when the user presses on the title bar button.

   procedure Dump
     (Me : Debug_Handle; Tree : Active_Area_Tree; Indent : Natural := 0);
   --  For debugging purposes, dump the tree to Me.

   procedure Highlight_Item_And_Siblings
     (Browser : access General_Browser_Record'Class;
      Item    : access Gtkada.Canvas.Canvas_Item_Record'Class;
      Old     : Gtkada.Canvas.Canvas_Item := null);
   --  Call Highlight on Item and all its siblings. If the selection status has
   --  changed, this will result in a change of background color for these
   --  items.
   --  If Old is not null, then it is also refreshed along with all its
   --  siblings. This subprogram is optimized so that the items are refreshed
   --  only once.
   --  The item is not selected.

   procedure Compute_Parents
     (Event : Gdk_Event; Item : access Browser_Item_Record'Class);
   procedure Compute_Children
     (Event : Gdk_Event; Item : access Browser_Item_Record'Class);
   --  Callbacks for the title bar buttons of Arrow_item

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Browser : access General_Browser_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Create_Toolbar : Boolean)
   is
      Scrolled : Gtk_Scrolled_Window;
   begin
      Gtk.Box.Initialize_Vbox (Browser, Homogeneous => False);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Pack_Start (Browser, Scrolled, Expand => True, Fill => True);

      Gtk_New (Browser.Canvas);
      Add (Scrolled, Browser.Canvas);
      Add_Events (Browser.Canvas, Key_Press_Mask);
      Browser.Kernel := Kernel_Handle (Kernel);

      Configure
        (Browser.Canvas,
         Annotation_Font => Get_Pref (GVD_Prefs, Annotation_Font));

      Set_Layout_Algorithm (Browser.Canvas, Layer_Layout'Access);
      Set_Auto_Layout (Browser.Canvas, False);

      if Create_Toolbar then
         Gtk_New (Browser.Toolbar);
         Set_Layout (Browser.Toolbar, Buttonbox_Spread);
         Pack_Start (Browser, Browser.Toolbar, Expand => False);
      end if;

      --  ??? Should be freed when browser is destroyed.
      Browser.Up_Arrow := Render_Icon
        (Browser, Stock_Go_Up, Icon_Size_Menu);
      Browser.Down_Arrow := Render_Icon
        (Browser, Stock_Go_Down, Icon_Size_Menu);
      Browser.Close_Pixmap := Render_Icon
        (Browser, Stock_Close, Icon_Size_Menu);

      Widget_Callback.Object_Connect
        (Browser.Canvas, "realize",
         Widget_Callback.To_Marshaller (Realized'Access), Browser);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Browser.Canvas, "key_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller (Key_Press'Access),
         Browser);

      Widget_Callback.Object_Connect
        (Browser.Canvas, "draw_links", On_Draw_Links'Access,
         Browser, After => True);

      Widget_Callback.Object_Connect
        (Browser.Canvas, "background_click",
         Widget_Callback.To_Marshaller (On_Background_Click'Access), Browser);

      --  ??? Should free pixmaps on destroy.

      Set_Size_Request
        (Browser,
         Get_Pref (Kernel, Default_Widget_Width),
         Get_Pref (Kernel, Default_Widget_Height));
   end Initialize;

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

   ------------------
   -- On_Draw_Link --
   ------------------

   procedure On_Draw_Links
     (Browser : access Gtk_Widget_Record'Class;
      Args    : Glib.Values.GValues)
   is
      B   : constant General_Browser := General_Browser (Browser);
      Win : constant Gdk_Window := Gdk_Window (To_C_Proxy (Args, 1));
   begin
      --  Redraw the selected links if needed.
      --  IF we don't do that, then it might happen that unselected links
      --  overlap selected links. This also means that these links are drawn
      --  twice...
      if B.Selected_Item /= null
        and then Get_Orthogonal_Links (Get_Canvas (B))
      then
         Update_Links (Get_Canvas (B), Win, B.Selected_Item);
      end if;
   end On_Draw_Links;

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

      B      : General_Browser := General_Browser (Browser);
      Color  : Gdk_Color;
      Kernel : constant Kernel_Handle := Get_Kernel (B);

   begin
      if B.Selected_Item_GC = null then
         B.Selected_Link_Color := Get_Pref (Kernel, Selected_Link_Color);

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
         Set_Foreground (B.Text_GC, Get_Pref (B.Kernel, Browsers_Link_Color));

         Gdk_New (B.Title_GC, Get_Window (B.Canvas));
         Set_Foreground (B.Title_GC,
                         Get_Pref (B.Kernel, GVD.Preferences.Title_Color));
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
      --  Click on an item: this is a file selection
      --  ??? Should we convert to world coordinates here ?

      Get_Origin (Get_Window (B.Canvas), Xr, Yr, Success);
      Set_X (Event, Get_X_Root (Event) - Gdouble (Xr));
      Set_Y (Event, Get_Y_Root (Event) - Gdouble (Yr));

      Item := Item_At_Coordinates (B.Canvas, Event);

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

         Gtk_New (Mitem, Label => -"Refresh");
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
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Set_Root;

   ------------------
   -- Toggle_Links --
   ------------------

   procedure Toggle_Links
     (Mitem : access Gtk_Widget_Record'Class; Data : Cb_Data)
   is
      pragma Unreferenced (Mitem);
      It : Browser_Item := Browser_Item (Data.Item);
   begin
      It.Hide_Links := not It.Hide_Links;
      Refresh_Canvas (Get_Canvas (Data.Browser));
   end Toggle_Links;

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
         Trace (Me, "Unexpected exception " & Exception_Information (E));
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

   -------------------
   -- Selected_Item --
   -------------------

   function Selected_Item (Browser : access General_Browser_Record)
      return Gtkada.Canvas.Canvas_Item is
   begin
      return Browser.Selected_Item;
   end Selected_Item;

   ---------------------------------
   -- Highlight_Item_And_Siblings --
   ---------------------------------

   procedure Highlight_Item_And_Siblings
     (Browser : access General_Browser_Record'Class;
      Item    : access Gtkada.Canvas.Canvas_Item_Record'Class;
      Old     : Gtkada.Canvas.Canvas_Item := null)
   is
      Iter : Item_Iterator;
      It  : Canvas_Item;
   begin
      if Old /= null then
         Highlight (Browser_Item (Old));

         Iter := Start (Browser.Canvas, Old);
         loop
            It := Get (Iter);
            exit when It = null;
            Highlight (Browser_Item (It));
            Next (Iter);
         end loop;
      end if;

      if Canvas_Item (Item) /= Old then
         Highlight (Browser_Item (Item));
      end if;

      Iter := Start (Browser.Canvas, Canvas_Item (Item));
      loop
         It := Get (Iter);
         exit when It = null;

         --  Do not refresh items that have already been refreshed
         if Old = null
           or else (not Has_Link (Browser.Canvas, It, Old)
                    and then not Has_Link (Browser.Canvas, Old, It))
         then
            Highlight (Browser_Item (It));
         end if;

         Next (Iter);
      end loop;

      --  We need to redraw the whole canvas, so that the links are correctly
      --  updated. If Highlight_Item_And_Siblings is called twice, this isn't a
      --  problem since gtk+ will coalesce the two events anyway.
      Refresh_Canvas (Browser.Canvas);
   end Highlight_Item_And_Siblings;

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight (Item : access Browser_Item_Record) is
   begin
      Redraw_Title_Bar (Browser_Item (Item));
   end Highlight;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
     (Browser : access General_Browser_Record;
      Item    : access Gtkada.Canvas.Canvas_Item_Record'Class)
   is
      Old : constant Canvas_Item := Browser.Selected_Item;
   begin
      Browser.Selected_Item := Canvas_Item (Item);
      if Canvas_Item (Item) /= Old then
         Highlight_Item_And_Siblings (Browser, Item, Old);
      end if;
   end Select_Item;

   -------------------------
   -- On_Background_Click --
   -------------------------

   procedure On_Background_Click
     (Browser : access Gtk_Widget_Record'Class) is
   begin
      Grab_Focus (General_Browser (Browser).Canvas);
      Unselect_All (General_Browser (Browser));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Background_Click;

   ------------------
   -- Unselect_All --
   ------------------

   procedure Unselect_All (Browser : access General_Browser_Record) is
      Old : constant Canvas_Item := Browser.Selected_Item;
   begin
      Browser.Selected_Item := null;
      if Old /= null then
         Highlight_Item_And_Siblings (Browser, Old);
      end if;
   end Unselect_All;

   -----------------
   -- Get_Text_GC --
   -----------------

   function Get_Text_GC
     (Browser : access General_Browser_Record) return Gdk.GC.Gdk_GC is
   begin
      return Browser.Text_GC;
   end Get_Text_GC;

   ---------------
   -- Draw_Link --
   ---------------

   procedure Draw_Link
     (Canvas      : access Interactive_Canvas_Record'Class;
      Link        : access Browser_Link_Record;
      Window      : Gdk.Window.Gdk_Window;
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
           or else
           (Get_Src (Link) /= Vertex_Access (Selected_Item (Browser))
            and then Get_Dest (Link) /=
               Vertex_Access (Selected_Item (Browser)))
         then
            --  ??? Should force recomputation of the annotations so that they
            --  can have the same color as the link themselves. But since they
            --  are cached, the cache needs to be freed whenever we change the
            --  GC.
            Draw_Link
              (Canvas, Canvas_Link_Access (Link), Window,
               Invert_Mode, GC, Edge_Number);
         else
            Set_Foreground (GC, Browser.Selected_Link_Color);
            Draw_Link
              (Canvas, Canvas_Link_Access (Link),
               Window, Invert_Mode, GC, Edge_Number);
            Set_Foreground (GC, Black (Get_Default_Colormap));
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
               Get_Pref (Get_Kernel (Get_Browser (Item)), Browsers_Link_Font));
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
         if B.Selected_Item = Canvas_Item (User) then
            Unselect_All (B);
         end if;

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
      X : constant Gint := Get_Coord (Item).Width
        - (Num + 1) * (Margin + Button_Width);
      Y, W, H : Gint;
      Thick : constant Gint := Y_Thickness (Get_Style (Item.Browser.Canvas));
   begin
      --  No title ? Don't draw any button
      if Item.Title_Layout = null then
         return;
      end if;

      Get_Pixel_Size (Item.Title_Layout, W, H);
      Y := (H - Button_Height) / 2 + Thick;

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
   begin
      if Item.Title_Layout /= null then
         Reset_Active_Areas (Item.all, Other_Areas => False);

         Get_Pixel_Size (Item.Title_Layout, W, H);
         Draw_Rectangle
           (Pixmap (Item),
            GC     => Get_Title_Background_GC (Browser_Item (Item)),
            Filled => True,
            X      => XThick,
            Y      => YThick,
            Width  => Get_Coord (Item).Width - 2 * XThick,
            Height => H);
         Draw_Layout
           (Drawable => Pixmap (Item),
            GC       => Get_Black_GC (Get_Style (Item.Browser)),
            X        => Margin + XThick,
            Y        => YThick,
            Layout   => Item.Title_Layout);
         Draw_Line
           (Pixmap (Item),
            Gc     => Get_Black_GC (Get_Style (Item.Browser)),
            X1     => XThick,
            Y1     => H + YThick,
            X2     => Get_Coord (Item).Width - XThick - 1,
            Y2     => H + YThick);
         Draw_Title_Bar_Button
           (Item   => Item,
            Num    => 0,
            Pixbuf => Item.Browser.Close_Pixmap,
            Cb     => Build (Close_Item'Access, Item));
      end if;
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
      Selected : constant Canvas_Item := Selected_Item (B);
   begin
      if Canvas_Item (Item) = Selected then
         return Get_Selected_Item_GC (B);

      elsif Selected /= null
        and then Has_Link (Get_Canvas (B), From => Item, To => Selected)
      then
         return Get_Parent_Linked_Item_GC (B);

      elsif Selected /= null
        and then Has_Link (Get_Canvas (B), From => Selected, To => Item)
      then
         return Get_Child_Linked_Item_GC (B);

      else
         --  ??? Should use different color
         return Item.Browser.Title_GC;
      end if;
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
      pragma Unreferenced (Xoffset, Layout);
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

      W := W + Width_Offset;
      H := H + Height_Offset;

      Set_Screen_Size (Browser_Item (Item), W, H);

      Bg_GC := Get_Background_GC (Browser_Item (Item));

      Draw_Rectangle
        (Pixmap (Item),
         GC     => Bg_GC,
         Filled => True,
         X      => 0,
         Y      => Layout_H,
         Width  => W,
         Height => H - Layout_H);

      Draw_Shadow
        (Style       => Get_Style (Item.Browser.Canvas),
         Window      => Pixmap (Item),
         State_Type  => State_Normal,
         Shadow_Type => Shadow_Out,
         X           => 0,
         Y           => 0,
         Width       => W,
         Height      => H);

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
      if Get_Event_Type (Event) = Button_Press then
         Select_Item (Item.Browser, Item);
      end if;

      Raise_Item (Get_Canvas (Get_Browser (Item)), Item);
      Activate (Browser_Item (Item), Event);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
      begin
         if Area.Children /= null then
            for C in Area.Children'Range loop
               Free (Area.Children (C));
               if Area.Children (C) /= null then
                  Should_Free := False;
               end if;
            end loop;

            if Should_Free then
               Unchecked_Free (Area.Children);
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
         Get_Pref (Get_Kernel (Get_Browser (Item)), Browsers_Link_Font));

      Resize_And_Draw (Item, 0, 0, 0, 0, Xoffset, Yoffset, Layout);
      Redraw_Title_Bar (Item);

      Unref (Layout);
   end Refresh;

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
      Free (List.Lines);
      Unchecked_Free (List.Lengths);

      if List.Callbacks /= null then
         for A in List.Callbacks'Range loop
            --  Do not actually destroy, since these are still used in the
            --  callbacks.
            Unchecked_Free (List.Callbacks (A));
         end loop;
         Unchecked_Free (List.Callbacks);
      end if;
   end Free;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (List     : in out Xref_List;
      Str      : String;
      Length1  : Natural        := Natural'Last;
      Callback : Active_Area_Cb := null)
   is
      Tmp : GNAT.Strings.String_List_Access := List.Lines;
      Cbs : Active_Area_Cb_Array_Access := List.Callbacks;
      Tmp2 : Natural_Array_Access := List.Lengths;
   begin
      if Tmp /= null then
         List.Lines :=
           new GNAT.Strings.String_List'(Tmp.all & new String'(Str));
         Unchecked_Free (Tmp);
      else
         List.Lines := new GNAT.Strings.String_List'(1 => new String'(Str));
      end if;

      if Cbs /= null then
         List.Callbacks := new Active_Area_Cb_Array'(Cbs.all & Callback);
         Unchecked_Free (Cbs);
      else
         List.Callbacks := new Active_Area_Cb_Array'(1 => Callback);
      end if;

      if Tmp2 /= null then
         List.Lengths := new Natural_Array'(Tmp2.all & Length1);
         Unchecked_Free (Tmp2);
      else
         List.Lengths := new Natural_Array'(1 => Length1);
      end if;
   end Add_Line;

   --------------------
   -- Get_Pixel_Size --
   --------------------

   procedure Get_Pixel_Size
     (Browser   : access General_Browser_Record'Class;
      List      : Xref_List;
      W1, W2, H : out Gint;
      Layout    : access Pango_Layout_Record'Class)
   is
      Descr : constant Pango_Font_Description :=
        Get_Pref (Get_Kernel (Browser), Browsers_Link_Font);
      Font  : Pango_Font;
      Metrics : Pango_Font_Metrics;
      Longest1, Longest2 : Gint := 0;
      H2, W    : Gint;
      Last : Natural;
   begin
      H := 0;

      if List.Lines /= null then
         Font := Load_Font (Get_Pango_Context (Browser), Descr);
         Metrics := Get_Metrics (Font);

         for L in List.Lines'Range loop
            declare
               Line : GNAT.Strings.String_Access renames List.Lines (L);
            begin
               Last := Natural'Min (List.Lengths (L), Line'Length);

               --  First column
               declare
                  Str   : String (1 .. Last);
                  Index : Natural := Str'First;
               begin
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
            end;
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
      Browser : constant General_Browser := Get_Browser (Item);
      X2     : Gint;
      First, Last : Integer;
      In_Xref : Boolean;
      GC     : Gdk_GC;
      W, H   : Gint;

      procedure Display (L : Natural);
      --  Display the slice First .. Last - 1

      procedure Display (L : Natural) is
      begin
         if First <= Last - 1 then
            Set_Text (Layout, List.Lines (L)(First .. Last - 1));

            if In_Xref then
               GC := Get_Text_GC (Browser);
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

               if List.Callbacks (L) /= null then
                  Add_Active_Area
                    (Item,
                     Gdk_Rectangle'(X2, Y, W, H),
                     List.Callbacks (L).all);
               end if;
            end if;

            X2 := X2 + W;
         end if;
      end Display;

   begin
      if List.Lines = null then
         return;
      end if;

      for L in List.Lines'Range loop
         First := List.Lines (L)'First;
         Last := First;
         X2   := X;
         In_Xref := False;

         while Last <= List.Lines (L)'Last loop
            if Last - List.Lines (L)'First = List.Lengths (L) then
               Display (L);
               First   := Last;
               X2      := X + Second_Column;
            end if;

            if List.Lines (L)(Last) = '@' then
               Display (L);
               First   := Last + 1;
               In_Xref := not In_Xref;
            end if;

            Last := Last + 1;
         end loop;

         Display (L);

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

end Browsers.Canvas;
