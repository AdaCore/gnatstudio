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
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Stock;           use Gtk.Stock;
with Gtk.Style;           use Gtk.Style;
with Gtk.Widget;          use Gtk.Widget;
with Pango.Layout;        use Pango.Layout;

with Ada.Exceptions;      use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;         use GNAT.OS_Lib;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with GVD.Preferences;
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

   procedure Internal_Select
     (Browser : access General_Browser_Record'Class;
      Item    : Canvas_Item := null;
      Refresh_Items : Boolean := False);
   --  Internal version of Select_Item, that can also be used to unselect an
   --  item.

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

      Set_Layout_Algorithm (Browser.Canvas, Layer_Layout'Access);
      Set_Auto_Layout (Browser.Canvas, True);

      if Create_Toolbar then
         Gtk_New (Browser.Toolbar);
         Set_Layout (Browser.Toolbar, Buttonbox_Spread);
         Pack_Start (Browser, Browser.Toolbar, Expand => False);
      end if;

      Browser.Left_Arrow := Render_Icon
        (Browser, Stock_Go_Back, Icon_Size_Menu);
      Browser.Right_Arrow := Render_Icon
        (Browser, Stock_Go_Forward, Icon_Size_Menu);

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
         Color := Get_Pref (Kernel, Selected_Item_Color);
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

      Set_Auto_Layout (Get_Canvas (Data.Browser), False);

      Iter := Start (Get_Canvas (Data.Browser));
      loop
         Item := Get (Iter);
         exit when Item = null;

         Next (Iter);

         if Item /= Data.Item then
            Remove (Get_Canvas (Data.Browser), Item);
         end if;
      end loop;

      Reset (Data.Browser, Browser_Item (Data.Item));
      Refresh (Browser_Item (Data.Item));

      Set_Auto_Layout (Get_Canvas (Data.Browser), True);
      Layout
        (Get_Canvas (Data.Browser),
         Force => False,
         Vertical_Layout =>
           Get_Pref (Get_Kernel (Data.Browser), Browsers_Vertical_Layout));
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
      Layout (Get_Canvas (B), Force => True, Vertical_Layout => True);
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

   ---------------------
   -- Internal_Select --
   ---------------------

   procedure Internal_Select
     (Browser : access General_Browser_Record'Class;
      Item    : Canvas_Item := null;
      Refresh_Items : Boolean := False)
   is
      Old : constant Canvas_Item := Browser.Selected_Item;
      Iter : Item_Iterator;
      It  : Canvas_Item;
   begin
      Browser.Selected_Item := Item;

      if Item /= Old and then Refresh_Items then

         --  Note: it might happen that some items are drawn several
         --  times. However, it can only happen for Old and Item, or to items
         --  that are linked to both Old and item. On the whole, we save some
         --  time
         if Old /= null then
            Refresh (Browser_Item (Old));

            Iter := Start (Browser.Canvas, Old);
            loop
               It := Get (Iter);
               exit when It = null;
               Refresh (Browser_Item (It));
               Next (Iter);
            end loop;
         end if;

         if Item /= null then
            Refresh (Browser_Item (Item));

            Iter := Start (Browser.Canvas, Item);
            loop
               It := Get (Iter);
               exit when It = null;

               --  Do not refresh items that have already been refreshed
               if Old = null
                 or else (not Has_Link (Browser.Canvas, It, Old)
                          and then not Has_Link (Browser.Canvas, Old, It))
               then
                  Refresh (Browser_Item (It));
               end if;

               Next (Iter);
            end loop;
         end if;

         Refresh_Canvas (Browser.Canvas);
      end if;
   end Internal_Select;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
     (Browser : access General_Browser_Record;
      Item    : access Gtkada.Canvas.Canvas_Item_Record'Class;
      Refresh_Items : Boolean := False) is
   begin
      Internal_Select (Browser, Canvas_Item (Item), Refresh_Items);
   end Select_Item;

   -------------------------
   -- On_Background_Click --
   -------------------------

   procedure On_Background_Click
     (Browser : access Gtk_Widget_Record'Class) is
   begin
      Grab_Focus (General_Browser (Browser).Canvas);
      Internal_Select (General_Browser (Browser), null, True);
   end On_Background_Click;

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

   procedure Reset (Browser : access General_Browser_Record'Class;
                    Item : access Browser_Item_Record)
   is
      pragma Unreferenced (Browser, Item);
   begin
      null;
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Browser : access General_Browser_Record'Class;
                    Item : access Text_Item_With_Arrows_Record)
   is
      pragma Unreferenced (Browser);
   begin
      Item.Left_Arrow  := True;
      Item.Right_Arrow := True;
   end Reset;

   --------------------
   -- Get_Left_Arrow --
   --------------------

   function Get_Left_Arrow (Item : access Text_Item_With_Arrows_Record)
      return Boolean is
   begin
      return Item.Left_Arrow;
   end Get_Left_Arrow;

   ---------------------
   -- Get_Right_Arrow --
   ---------------------

   function Get_Right_Arrow (Item : access Text_Item_With_Arrows_Record)
      return Boolean is
   begin
      return Item.Right_Arrow;
   end Get_Right_Arrow;

   --------------------
   -- Set_Left_Arrow --
   --------------------

   procedure Set_Left_Arrow
     (Item : access Text_Item_With_Arrows_Record; Display : Boolean) is
   begin
      Item.Left_Arrow := Display;
   end Set_Left_Arrow;

   ---------------------
   -- Set_Right_Arrow --
   ---------------------

   procedure Set_Right_Arrow
     (Item : access Text_Item_With_Arrows_Record; Display : Boolean) is
   begin
      Item.Right_Arrow := Display;
   end Set_Right_Arrow;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Item : access Browser_Item_Record'Class;  Title : String := "") is
   begin
      Free (Item.Title);

      if Title = "" then
         if Item.Title_Layout /= null then
            Unref (Item.Title_Layout);
            Item.Title_Layout := null;
         end if;
      else
         Item.Title := new String'(Title);

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

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   procedure Resize_And_Draw
     (Item             : access Browser_Item_Record;
      Width, Height    : Glib.Gint;
      Width_Offset, Height_Offset : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint)
   is
      W, H  : Gint;
      Layout_H : Gint := 0;
      Bg_GC : Gdk_GC;
   begin
      if Item.Title /= null then
         Get_Pixel_Size (Item.Title_Layout, W, Layout_H);
         W := Gint'Max (W + 2 * Margin, Width);
         H := Layout_H + Height;
      else
         W := Width;
         H := Height;
      end if;

      W := W + Width_Offset;
      H := H + Height_Offset;

      Set_Screen_Size (Browser_Item (Item), W, H);

      if Canvas_Item (Item) = Selected_Item (Item.Browser) then
         Bg_GC := Item.Browser.Selected_Item_GC;
      elsif Selected_Item (Item.Browser) /= null
        and then Has_Link
          (Item.Browser.Canvas,
           From => Item, To => Selected_Item (Item.Browser))
      then
         Bg_GC := Item.Browser.Parent_Linked_Item_GC;
      elsif Selected_Item (Item.Browser) /= null
        and then Has_Link (Item.Browser.Canvas,
                           From => Selected_Item (Item.Browser), To => Item)
      then
         Bg_GC := Item.Browser.Child_Linked_Item_GC;
      else
         Bg_GC := Item.Browser.Default_Item_GC;
      end if;

      Draw_Rectangle
        (Pixmap (Item),
         GC     => Bg_GC,
         Filled => True,
         X      => 0,
         Y      => 0,
         Width  => W,
         Height => H);

      if Item.Title /= null then
         Draw_Rectangle
           (Pixmap (Item),
            GC     => Item.Browser.Title_GC,
            Filled => True,
            X      => 0,
            Y      => 0,
            Width  => W,
            Height => Layout_H);
         Draw_Layout
           (Drawable => Pixmap (Item),
            GC       => Get_Black_GC (Get_Style (Item.Browser)),
            X        => Xoffset + Margin,
            Y        => Yoffset,
            Layout   => Item.Title_Layout);
         Draw_Line
           (Pixmap (Item),
            Gc     => Get_Black_GC (Get_Style (Item.Browser)),
            X1     => 0,
            Y1     => Layout_H,
            X2     => W,
            Y2     => Layout_H);
         Yoffset := Yoffset + Layout_H;
      end if;

      Draw_Shadow
        (Style       => Get_Style (Item.Browser.Canvas),
         Window      => Pixmap (Item),
         State_Type  => State_Normal,
         Shadow_Type => Shadow_Out,
         X           => 0,
         Y           => 0,
         Width       => W,
         Height      => H);
   end Resize_And_Draw;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   procedure Resize_And_Draw
     (Item             : access Text_Item_Record;
      Width, Height    : Glib.Gint;
      Width_Offset, Height_Offset : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint)
   is
      W, H : Gint;
   begin
      Get_Pixel_Size (Item.Layout, W, H);
      W := Gint'Max (W + 2 * Margin, Width);
      H := Gint'Max (H + 2 * Margin, Height);
      Resize_And_Draw
        (Browser_Item_Record (Item.all)'Access, W, H,
         Width_Offset, Height_Offset, Xoffset, Yoffset);

      Draw_Layout
        (Drawable => Pixmap (Item),
         GC       => Get_Text_GC (Item.Browser),
         X        => Xoffset + Margin,
         Y        => Yoffset + Margin,
         Layout   => Item.Layout);
   end Resize_And_Draw;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   procedure Resize_And_Draw
     (Item             : access Text_Item_With_Arrows_Record;
      Width, Height    : Glib.Gint;
      Width_Offset, Height_Offset : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint)
   is
      X, Y, H : Gint;
   begin
      H := Gint'Max (Get_Height (Item.Browser.Left_Arrow), Height);
      X := Xoffset + Get_Width (Item.Browser.Left_Arrow);
      Resize_And_Draw
        (Text_Item_Record (Item.all)'Access,
         Width, H, Width_Offset + 2 * Get_Width (Item.Browser.Left_Arrow),
         Height_Offset, X, Yoffset);

      --  Only compute this after calling the parent, since otherwise the item
      --  might not have been resized yet.

      Y := (Get_Coord (Item).Height - Yoffset
            - Get_Height (Item.Browser.Left_Arrow)) / 2;

      if Item.Left_Arrow then
         Render_To_Drawable_Alpha
           (Pixbuf          => Item.Browser.Left_Arrow,
            Drawable        => Pixmap (Item),
            Src_X           => 0,
            Src_Y           => 0,
            Dest_X          => Xoffset,
            Dest_Y          => Y,
            Width           => -1,
            Height          => -1,
            Alpha           => Alpha_Full,
            Alpha_Threshold => 128);
      end if;

      if Item.Right_Arrow then
         Render_To_Drawable_Alpha
           (Pixbuf          => Item.Browser.Right_Arrow,
            Drawable        => Pixmap (Item),
            Src_X           => 0,
            Src_Y           => 0,
            Dest_X          => Xoffset + Gint (Get_Coord (Item).Width)
            - Get_Width (Item.Browser.Right_Arrow),
            Dest_Y          => Y,
            Width           => -1,
            Height          => -1,
            Alpha           => Alpha_Full,
            Alpha_Threshold => 128);
      end if;
   end Resize_And_Draw;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item    : access Text_Item_Record'Class;
      Browser : access General_Browser_Record'Class;
      Text    : String) is
   begin
      Initialize (Item, Browser);
      Item.Layout := Create_Pango_Layout (Browser, Text);
      Set_Font_Description
        (Item.Layout, Get_Pref (Get_Kernel (Browser), Browsers_Link_Font));
   end Initialize;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Item    : access Text_Item_Record'Class;
      Text    : String)
   is
   begin
      Set_Text (Item.Layout, Text);

      --  Force a recomputation of the size next time the item is displayed
      Set_Screen_Size (Item, 1, 1);
      Refresh (Browser_Item (Item));
   end Set_Text;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out Text_Item_Record) is
   begin
      Unref (Item.Layout);
   end Destroy;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
     (Item  : access Text_Item_With_Arrows_Record;
      Event : Gdk.Event.Gdk_Event_Button) is
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
      then
         if Gint (Get_X (Event)) < Get_Coord (Item).Width / 2 then
            Button_Click_On_Left (Text_Item_With_Arrows (Item));
         else
            Button_Click_On_Right (Text_Item_With_Arrows (Item));
         end if;

         --  Make sure that the item we clicked on is still visible
         Show_Item (Get_Canvas (Item.Browser), Item);

      elsif Get_Event_Type (Event) = Button_Press then
         Select_Item (Item.Browser, Item, True);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Button_Click;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
     (Item  : access Browser_Item_Record;
      Event : Gdk.Event.Gdk_Event_Button) is
   begin
      Raise_Item (Get_Canvas (Get_Browser (Item)), Item);
      Activate (Browser_Item (Item), Event);
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

   procedure Call (Callback : Widget_Active_Area_Callback;
                   Event    : Gdk.Event.Gdk_Event)
   is
   begin
      Callback.Cb (Event, Callback.User_Data);
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
      begin
         Tmp_R.X := Gint'Min (Rectangle.X, Area.Rectangle.X);
         Tmp_R.Y := Gint'Min (Rectangle.Y, Area.Rectangle.Y);
         Tmp_R.Width :=
           Gint'Max (Rectangle.X + Rectangle.Width,
                     Area.Rectangle.X + Area.Rectangle.Width) - Tmp_R.X;
         Tmp_R.Height :=
           Gint'Max (Rectangle.Y + Rectangle.Height,
                     Area.Rectangle.Y + Area.Rectangle.Height) - Tmp_R.Y;

         Area := new Active_Area_Tree_Record'
           (Rectangle => Tmp_R,
            Callback  => null,
            Children  => new Active_Area_Tree_Array'(1 => Area, 2 => Tmp));
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
               Join_Areas (Area);
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
               Call (Area.Callback.all, Event);
               return True;
            end if;
         end if;
         return False;
      end Check_Area;

      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Tmp := Check_Area (Item.Active_Areas);
   end Activate;

   ------------------------
   -- Reset_Active_Areas --
   ------------------------

   procedure Reset_Active_Areas (Item : access Browser_Item_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Active_Area_Tree_Record, Active_Area_Tree);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Active_Area_Callback'Class, Active_Area_Cb);

      procedure Free (Area : in out Active_Area_Tree);
      --  Free Area and its children

      procedure Free (Area : in out Active_Area_Tree) is
      begin
         if Area.Children /= null then
            for C in Area.Children'Range loop
               Free (Area.Children (C));
            end loop;
            Unchecked_Free (Area.Children);
         end if;

         Destroy (Area.Callback.all);
         Unchecked_Free (Area.Callback);
         Unchecked_Free (Area);
      end Free;

   begin
      Free (Item.Active_Areas);
   end Reset_Active_Areas;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Callback : in out Active_Area_Callback) is
      pragma Unreferenced (Callback);
   begin
      null;
   end Destroy;

   -----------
   -- Build --
   -----------

   function Build (Cb : Widget_Active_Callback;
                   User : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Widget_Active_Area_Callback'Class
   is
   begin
      return Widget_Active_Area_Callback'
        (Active_Area_Callback with
         User_Data => Gtk_Widget (User),
         Cb        => Cb);
   end Build;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Item    : access Browser_Item_Record'Class) is
      Xoffset, Yoffset : Gint := 0;
   begin
      Resize_And_Draw (Item, 0, 0, 0, 0, Xoffset, Yoffset);
   end Refresh;

end Browsers.Canvas;
