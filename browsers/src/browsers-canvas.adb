-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2011, AdaCore                  --
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

with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;

with GNAT.Strings;                      use GNAT.Strings;
with GNATCOLL.VFS;                      use GNATCOLL.VFS;

with Glib;                              use Glib;
with Glib.Error;                        use Glib.Error;
with Glib.Graphs;                       use Glib.Graphs;
with Glib.Main;                         use Glib.Main;
with Glib.Object;                       use Glib.Object;

with Cairo;                             use Cairo;
with Cairo.Pattern;                     use Cairo.Pattern;
with Cairo.PDF;                         use Cairo.PDF;
with Cairo.Png;                         use Cairo.Png;
with Cairo.Surface;                     use Cairo.Surface;
with Cairo.SVG;                         use Cairo.SVG;

with Pango.Cairo;                       use Pango.Cairo;
with Pango.Enums;                       use Pango.Enums;
with Pango.Font;                        use Pango.Font;
with Pango.Layout;                      use Pango.Layout;

with Gdk.Cairo;                         use Gdk.Cairo;
with Gdk.Color;                         use Gdk.Color;
with Gdk.Event;                         use Gdk.Event;
with Gdk.Pixbuf;                        use Gdk.Pixbuf;
with Gdk.Rectangle;                     use Gdk.Rectangle;
with Gdk.Types.Keysyms;                 use Gdk.Types.Keysyms;
with Gdk.Window;                        use Gdk.Window;

with Gtk.Accel_Group;                   use Gtk.Accel_Group;
with Gtk.Button;                        use Gtk.Button;
with Gtk.Check_Menu_Item;               use Gtk.Check_Menu_Item;
with Gtk.Enums;                         use Gtk.Enums;
with Gtk.Handlers;                      use Gtk.Handlers;
with Gtk.Hbutton_Box;                   use Gtk.Hbutton_Box;
with Gtk.Image;                         use Gtk.Image;
with Gtk.Menu;                          use Gtk.Menu;
with Gtk.Menu_Item;                     use Gtk.Menu_Item;
with Gtk.Object;                        use Gtk.Object;
with Gtk.Separator_Menu_Item;           use Gtk.Separator_Menu_Item;
with Gtk.Scrolled_Window;               use Gtk.Scrolled_Window;
with Gtk.Stock;                         use Gtk.Stock;
with Gtk.Style;                         use Gtk.Style;
with Gtk.Widget;                        use Gtk.Widget;

with Gtkada.Canvas;                     use Gtkada.Canvas;
with Gtkada.File_Selector;              use Gtkada.File_Selector;
with Gtkada.Handlers;                   use Gtkada.Handlers;
with Gtkada.MDI;                        use Gtkada.MDI;
with Gtkada.Style;                      use Gtkada.Style;

with Commands;                          use Commands;
with Commands.Interactive;              use Commands.Interactive;
with GPS.Intl;                          use GPS.Intl;
with GPS.Kernel;                        use GPS.Kernel;
with GPS.Kernel.Actions;                use GPS.Kernel.Actions;
with GPS.Kernel.Console;                use GPS.Kernel.Console;
with GPS.Kernel.Hooks;                  use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;            use GPS.Kernel.Preferences;
with GPS.Kernel.MDI;                    use GPS.Kernel.MDI;
with Layouts;                           use Layouts;
with Traces;                            use Traces;
with XML_Utils;                         use XML_Utils;

package body Browsers.Canvas is

   Zoom_Levels : constant array (Positive range <>) of Gdouble :=
                   (0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0);
   --  All the possible zoom levels. We have to use such an array, instead
   --  of doing the computation directly, so as to avoid rounding errors that
   --  would appear in the computation and make zoom_in not the reverse of
   --  zoom_out.

   Zoom_Duration : constant := 0.25;
   --  Duration of the zoom animation

   type Export_File_Format is
     (Export_PNG, Export_PDF, Export_SVG);

   type Export_Idle_Data is record
      Browser : General_Browser;
      Format  : Export_File_Format;
   end record;

   package Export_Idle is
     new Glib.Main.Generic_Sources (Export_Idle_Data);

   package Check_Canvas_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Check_Menu_Item_Record, General_Browser);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Active_Area_Tree_Array, Active_Area_Tree_Array_Access);

   type Cb_Data is record
      Browser       : General_Browser;
      Item          : Canvas_Item;
      Zoom          : Gdouble;
      Keep_Selected : Boolean;
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
   --  Callback for the "realized" signal

   function Key_Press
     (Browser : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean;
   --  Callback for the key press event

   function On_Export_Idle (Data : Export_Idle_Data) return Boolean;
   --  Does the actual export in an idle, called by the bellow export
   --  subprograms

   procedure On_Export_To_PNG (Browser : access Gtk_Widget_Record'Class);
   --  Export the contents of the browser as an image

   procedure On_Export_To_SVG (Browser : access Gtk_Widget_Record'Class);
   --  Export the contents of the browser to SVG

   procedure On_Export_To_PDF (Browser : access Gtk_Widget_Record'Class);
   --  Export the contents of the browser to PDF

   procedure On_Refresh (Browser : access Gtk_Widget_Record'Class);
   --  Recompute the layout of the canvas

   procedure Change_Align_On_Grid
     (Item    : access Gtk_Check_Menu_Item_Record'Class;
      Browser : General_Browser);
   --  Callback for the "align on grid" contextual menu item

   procedure On_Select_All (Browser : access Gtk_Widget_Record'Class);
   --  Select all the items in the canvas

   procedure On_Data_Clear (Browser : access Gtk_Widget_Record'Class);
   --  "Clear" contextual menu

   type Select_All_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Select_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   procedure Toggle_Links
     (Mitem : access Gtk_Widget_Record'Class; Data : Cb_Data);
   --  Toggle the display of links for the item

   procedure Toggle_Orthogonal (Browser : access Gtk_Widget_Record'Class);
   --  Toggle the layout of links

   procedure Set_Root
     (Mitem : access Gtk_Widget_Record'Class; Data : Cb_Data);
   --  Remove all items except the one described in Data from the canvas

   procedure Close_Item
     (Event : Gdk.Event.Gdk_Event; User  : access Browser_Item_Record'Class);
   --  Close an item when the user presses on the title bar button

   procedure Dump
     (Me : Debug_Handle; Tree : Active_Area_Tree; Indent : Natural := 0);
   pragma Warnings (Off, Dump);
   --  For debugging purposes, dump the tree to Me

   procedure Destroyed (Browser : access Gtk_Widget_Record'Class);
   --  Called when the browser is destroyed

   type Preferences_Hook_Record is new Function_No_Args with record
      Browser : General_Browser;
   end record;
   type Preferences_Hook is access all Preferences_Hook_Record'Class;
   overriding procedure Execute (Hook : Preferences_Hook_Record;
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
      Draw_Grid         : Boolean;
      Pixmap            : Gdk.Gdk_Pixmap;
   end record;
   type Image_Canvas is access all Image_Canvas_Record'Class;

   overriding function Get_Window
     (Canvas : access Image_Canvas_Record) return Gdk.Window.Gdk_Window;
   --  Override Gtk.Widget.Get_Window, so that a different Window can be
   --  returned if needed (e.g. when exporting the canvas).

   overriding procedure Draw_Grid
     (Canvas : access Image_Canvas_Record;
      Cr     : Cairo_Context);

   overriding procedure Draw_Background
     (Canvas : access Image_Canvas_Record;
      Cr     : Cairo_Context);

   procedure Foreach_Active_Area
     (Str : String;
      Cb  : access procedure (Link : String));
   --  For each xref in Str (blocks surrounded by @..@), call Cb

   -------------
   -- Markers --
   -------------

   type Browser_Marker_Record is new Location_Marker_Record with record
      Title : GNAT.Strings.String_Access;
   end record;
   type Browser_Marker is access all Browser_Marker_Record'Class;

   overriding function Go_To
     (Marker : access Browser_Marker_Record;
      Kernel : access Kernel_Handle_Record'Class) return Boolean;
   overriding function Clone
     (Marker : access Browser_Marker_Record)
      return Location_Marker;
   overriding procedure Destroy (Marker : in out Browser_Marker_Record);
   overriding function To_String
     (Marker : access Browser_Marker_Record) return String;
   overriding function Save
     (Marker : access Browser_Marker_Record) return XML_Utils.Node_Ptr;
   overriding function Similar
     (Left  : access Browser_Marker_Record;
      Right : access Location_Marker_Record'Class) return Boolean;
   --  See inherited documentation

   function Create_Browser_Marker
     (Browser_Name : String) return Browser_Marker;
   --  Create a new marker that will bring the user back to the browser

   ----------------
   -- Get_Window --
   ----------------

   overriding function Get_Window
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

   ---------------
   -- Draw_Grid --
   ---------------

   overriding procedure Draw_Grid
     (Canvas : access Image_Canvas_Record;
      Cr     : Cairo_Context)
   is
   begin
      if Canvas.Draw_Grid then
         Draw_Grid (Interactive_Canvas_Record (Canvas.all)'Access, Cr);
      end if;
   end Draw_Grid;

   ---------------------
   -- Draw_Background --
   ---------------------

   overriding procedure Draw_Background
     (Canvas : access Image_Canvas_Record;
      Cr     : Cairo_Context)
   is
   begin
      if Canvas.Background = null then
         Draw_Background (Interactive_Canvas_Record (Canvas.all)'Access, Cr);
      else
         Gdk.Cairo.Set_Source_Pixbuf (Cr, Canvas.Background, 0.0, 0.0);
         Cairo.Paint (Cr);
      end if;
   end Draw_Background;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Browser         : access General_Browser_Record'Class;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Create_Toolbar  : Boolean;
      Parents_Pixmap  : String := Stock_Go_Back;
      Children_Pixmap : String := Stock_Go_Forward)
   is
      Hook     : Preferences_Hook;
      Scrolled : Gtk_Scrolled_Window;
      Canvas   : Image_Canvas;
   begin
      Gtk.Box.Initialize_Vbox (Browser, Homogeneous => False);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Pack_Start (Browser, Scrolled, Expand => True, Fill => True);

      Canvas := new Image_Canvas_Record;
      Gtkada.Canvas.Initialize (Canvas);
      Browser.Canvas := Interactive_Canvas (Canvas);

      Add (Scrolled, Browser.Canvas);
      Add_Events (Browser.Canvas, Key_Press_Mask);
      Browser.Kernel := Kernel_Handle (Kernel);

      Set_Layout_Algorithm (Browser.Canvas, Simple_Layout'Access);
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
        (Browser, Signal_Destroy, Destroyed'Access, Browser);

      Widget_Callback.Object_Connect
        (Browser.Canvas, Gtk.Widget.Signal_Realize,
         Widget_Callback.To_Marshaller (Realized'Access), Browser);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Browser.Canvas, Signal_Key_Press_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller (Key_Press'Access),
         Browser);

      Hook := new Preferences_Hook_Record;
      Hook.Browser := General_Browser (Browser);
      Add_Hook
        (Kernel, Preferences_Changed_Hook, Hook,
         Name => "browsers.preferences_changed", Watch => GObject (Browser));
      Execute (Hook.all, Kernel);
   end Initialize;

   ---------------
   -- Destroyed --
   ---------------

   procedure Destroyed (Browser : access Gtk_Widget_Record'Class) is
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

      if B.Item_Style /= null then
         Unref (B.Item_Style);
      end if;

      if B.Item_Style_Parent_Linked /= null then
         Unref (B.Item_Style_Parent_Linked);
      end if;

      if B.Item_Style_Child_Linked /= null then
         Unref (B.Item_Style_Child_Linked);
      end if;

      if Image_Canvas (B.Canvas).Background /= null then
         Unref (Image_Canvas (B.Canvas).Background);
      end if;
   end Destroyed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
      Error           : GError;
      Iter            : Item_Iterator;
      Annotation_Font : Pango_Font_Description;
      Style           : Gtk_Style;
      Need_Refresh    : Boolean := False;

      use type Gdk.Gdk_GC;
   begin
      if Hook.Browser.Item_Style /= null then
         Hook.Browser.Selected_Link_Color :=
           To_Cairo (Selected_Link_Color.Get_Pref);
         Hook.Browser.Unselected_Link_Color :=
           To_Cairo (Unselected_Link_Color.Get_Pref);

         Style := Hook.Browser.Item_Style;

         Set_Bg (Style, State_Normal,
                 Gdk_Color'(Get_Light (Get_Default_Style, State_Normal)));
         Set_Bg (Style, State_Selected,
                 Gdk_Color'(Get_Light (Get_Default_Style, State_Normal)));

         Set_Text (Style, State_Active,
                   Browsers_Hyper_Link_Color.Get_Pref);

         Set_Base (Style, State_Normal,
                   Title_Color.Get_Pref);
         Set_Base (Style, State_Selected,
                   GPS.Kernel.Preferences.Selected_Item_Color.Get_Pref);

         if Hook.Browser.Item_Style_Parent_Linked /= null then
            Unref (Hook.Browser.Item_Style_Parent_Linked);
            Unref (Hook.Browser.Item_Style_Child_Linked);
         end if;

         Hook.Browser.Item_Style_Parent_Linked := Gtk.Style.Copy (Style);
         Hook.Browser.Item_Style_Child_Linked := Gtk.Style.Copy (Style);

         Set_Base
           (Hook.Browser.Item_Style_Parent_Linked, State_Normal,
            Parent_Linked_Item_Color.Get_Pref);
         Set_Base
           (Hook.Browser.Item_Style_Child_Linked, State_Normal,
            Child_Linked_Item_Color.Get_Pref);

         Set_Bg (Get_Style (Hook.Browser.Canvas), State_Normal,
                 Browsers_Bg_Color.Get_Pref);
      end if;

      Annotation_Font := Copy (Default_Font.Get_Pref_Font);
      Set_Size
        (Annotation_Font,
         Gint'Max (Pango_Scale, Get_Size (Annotation_Font) - 2 * Pango_Scale));
      Configure (Hook.Browser.Canvas, Annotation_Font => Annotation_Font);
      Free (Annotation_Font);

      Image_Canvas (Hook.Browser.Canvas).Draw_Grid :=
        Browsers_Draw_Grid.Get_Pref;

      if Image_Canvas (Hook.Browser.Canvas).Background /= null then
         Unref (Image_Canvas (Hook.Browser.Canvas).Background);
      end if;

      if Browsers_Bg_Image.Get_Pref /= "" then
         Gdk_New_From_File
           (Image_Canvas (Hook.Browser.Canvas).Background,
            Filename => Browsers_Bg_Image.Get_Pref,
            Error    => Error);
      else
         Image_Canvas (Hook.Browser.Canvas).Background := null;
      end if;

      Refresh_Layout_Orientation (Hook.Browser);

      Iter := Start (Hook.Browser.Canvas);
      while Get (Iter) /= null loop
         declare
            Item   : constant Browser_Item := Browser_Item (Get (Iter));
            Layout : constant Pango.Layout.Pango_Layout :=
                      Item.Title_Layout;
         begin
            if Layout /= null then
               Need_Refresh := True;
               Set_Font_Description (Layout, Default_Font.Get_Pref_Font);
               Refresh (Item);
            end if;
         end;

         Next (Iter);
      end loop;

      if Need_Refresh then
         Refresh_Canvas (Hook.Browser.Canvas);
      end if;
   end Execute;

   --------------------------------
   -- Refresh_Layout_Orientation --
   --------------------------------

   procedure Refresh_Layout_Orientation
     (Browser : access General_Browser_Record) is
   begin
      Set_Layout_Orientation
        (Browser.Canvas,
         Vertical_Layout => Browsers_Vertical_Layout.Get_Pref);
   end Refresh_Layout_Orientation;

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
           (Button, Signal_Clicked, Zoom_Out'Access, Browser);

         Gtk_New (Button);
         Gtk_New (Image, Stock_Zoom_In, Icon_Size_Small_Toolbar);
         Add (Button, Image);
         Pack_End (Browser.Toolbar, Button, Expand => False);
         Widget_Callback.Object_Connect
           (Button, Signal_Clicked, Zoom_In'Access, Browser);
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
      B      : constant General_Browser := General_Browser (Browser);
      Hook   : Preferences_Hook_Record;

   begin
      if B.Item_Style = null then
         B.Selected_Link_Color := To_Cairo (Selected_Link_Color.Get_Pref);
         B.Unselected_Link_Color := To_Cairo (Unselected_Link_Color.Get_Pref);

         B.Item_Style := Copy (Get_Style (B.Canvas));
         Hook.Browser := B;
         Hook.Execute (null);
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

   procedure Contextual_Factory
     (Item    : access Browser_Item_Record;
      Context : in out GPS.Kernel.Selection_Context;
      Browser : access General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Item, Browser, Event, Menu, Context);
   begin
      null;
   end Contextual_Factory;

   -------------------------------------
   -- Default_Browser_Context_Factory --
   -------------------------------------

   procedure Default_Browser_Context_Factory
     (Context      : in out GPS.Kernel.Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Event_Widget);
      B            : constant General_Browser := General_Browser (Object);
      Mitem        : Gtk_Menu_Item;
      Sep          : Gtk_Separator_Menu_Item;
      Check        : Gtk_Check_Menu_Item;
      Zooms_Menu   : Gtk_Menu;
      Export_Menu  : Gtk_Menu;
      Item         : Canvas_Item;
      Xr, Yr       : Gint;
      Xsave, Ysave : Gdouble;
      Success      : Boolean;

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
           (Mitem, Gtk.Menu_Item.Signal_Activate, Toggle_Links'Access,
            (Browser => B, Item => Item, Zoom => 1.0, Keep_Selected => True));

         Gtk_New (Mitem, Label => -"Remove unselected items");
         Append (Menu, Mitem);
         Contextual_Cb.Connect
           (Mitem, Gtk.Menu_Item.Signal_Activate, Set_Root'Access,
            (Browser => B, Item => Item, Zoom => 1.0, Keep_Selected => True));

         Gtk_New (Mitem, Label => -"Remove selected items");
         Append (Menu, Mitem);
         Contextual_Cb.Connect
           (Mitem, Gtk.Menu_Item.Signal_Activate, Set_Root'Access,
            (Browser => B, Item => Item, Zoom => 1.0, Keep_Selected => False));

         Xsave := Get_X (Event);
         Ysave := Get_Y (Event);
         Set_X (Event, Get_X (Event) - Gdouble (Get_Coord (Item).X));
         Set_Y (Event, Get_Y (Event) - Gdouble (Get_Coord (Item).Y));
         Contextual_Factory
           (Browser_Item (Item), Context, B, Event, Menu);
         Set_X (Event, Xsave);
         Set_Y (Event, Ysave);
      end if;

      if Item = null then
         Gtk_New (Mitem, Label => -"Refresh Layout");
         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, Gtk.Menu_Item.Signal_Activate, On_Refresh'Access, B);

         if Get_Orthogonal_Links (Get_Canvas (B)) then
            Gtk_New (Mitem, Label => -"Straight links");
         else
            Gtk_New (Mitem, Label => -"Orthogonal links");
         end if;

         Gtk_New (Check, Label => -"Align On Grid");
         Set_Active (Check, Get_Align_On_Grid (Get_Canvas (B)));
         Append (Menu, Check);
         Check_Canvas_Handler.Connect
           (Check, Gtk.Menu_Item.Signal_Activate,
            Check_Canvas_Handler.To_Marshaller (Change_Align_On_Grid'Access),
            B);

         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, Gtk.Menu_Item.Signal_Activate, Toggle_Orthogonal'Access, B);

         Gtk_New (Export_Menu);

         Gtk_New (Mitem, Label => -"PDF");
         Append (Export_Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, Gtk.Menu_Item.Signal_Activate, On_Export_To_PDF'Access, B);

         Gtk_New (Mitem, Label => -"PNG");
         Append (Export_Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, Gtk.Menu_Item.Signal_Activate, On_Export_To_PNG'Access, B);

         Gtk_New (Mitem, Label => -"SVG");
         Append (Export_Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, Gtk.Menu_Item.Signal_Activate, On_Export_To_SVG'Access, B);

         Gtk_New (Mitem, Label => -"Export to ...");
         Append (Menu, Mitem);
         Set_Submenu (Mitem, Export_Menu);

         Gtk_New (Mitem, Label => -"Zoom in");
         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, Gtk.Menu_Item.Signal_Activate, Zoom_In'Access, B);
         Add_Accelerator
           (Mitem, Gtk.Menu_Item.Signal_Activate,
            Get_Default_Accelerators (Kernel), GDK_equal, 0, Accel_Visible);

         Gtk_New (Mitem, Label => -"Zoom out");
         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, Gtk.Menu_Item.Signal_Activate, Zoom_Out'Access, B);
         Add_Accelerator
           (Mitem, Gtk.Menu_Item.Signal_Activate,
            Get_Default_Accelerators (Kernel), GDK_minus, 0, Accel_Visible);

         Gtk_New (Zooms_Menu);

         for J in Zoom_Levels'Range loop
            Gtk_New
              (Mitem,
               Label => Guint'Image (Guint (Zoom_Levels (J) * 100.0)) & '%');
            Append (Zooms_Menu, Mitem);
            Contextual_Cb.Connect
              (Mitem, Gtk.Menu_Item.Signal_Activate, Zoom_Level'Access,
               (Browser => B,
                Item    => null,
                Keep_Selected => True,
                Zoom    => Zoom_Levels (J)));
         end loop;

         Gtk_New (Mitem, Label => -"Zoom");
         Append (Menu, Mitem);
         Set_Submenu (Mitem, Zooms_Menu);
      end if;

      Gtk_New (Mitem, Label => -"Select all");
      Append (Menu, Mitem);
      Widget_Callback.Object_Connect
        (Mitem, Gtk.Menu_Item.Signal_Activate, On_Select_All'Access, B);

      Gtk.Separator_Menu_Item.Gtk_New (Sep);
      Append (Menu, Sep);

      Gtk_New (Mitem, Label => -"Clear");
      Append (Menu, Mitem);
      Widget_Callback.Object_Connect
        (Mitem, Gtk.Menu_Item.Signal_Activate, On_Data_Clear'Access, B);

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

         if Data.Keep_Selected then
            if Item /= Data.Item
              and then not Is_Selected (Get_Canvas (Data.Browser), Item)
            then
               Remove (Get_Canvas (Data.Browser), Item);
            end if;
         else
            if Is_Selected (Get_Canvas (Data.Browser), Item) then
               Remove (Get_Canvas (Data.Browser), Item);
            end if;
         end if;
      end loop;

      if Data.Keep_Selected then
         Reset (Browser_Item (Data.Item), True, True);

         --  ??? We used to do the following call, but this results in resizing
         --  the item to 0x0, and thus makes it invisible. Not sure why we had
         --  it.
         --  Refresh (Browser_Item (Data.Item));
      end if;

      Layout (Data.Browser);
      Refresh_Canvas (Get_Canvas (Data.Browser));

      if Data.Keep_Selected then
         Show_Item (Get_Canvas (Data.Browser), Data.Item);
      end if;

      Pop_State (Get_Kernel (Data.Browser));

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Pop_State (Get_Kernel (Data.Browser));
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

   --------------------
   -- On_Export_Idle --
   --------------------

   function On_Export_Idle (Data : Export_Idle_Data) return Boolean is
      State_Pushed : Boolean := False;
      Canvas       : constant Interactive_Canvas := Get_Canvas (Data.Browser);
      Kernel       : constant Kernel_Handle := Get_Kernel (Data.Browser);

      function Extension return String;
      --  Return the expected extension according to the exported format

      function Description return String;
      --  Return the file format description

      ---------------
      -- Extension --
      ---------------

      function Extension return String is
      begin
         case Data.Format is
            when Export_PNG =>
               return ".png";
            when Export_PDF =>
               return ".pdf";
            when Export_SVG =>
               return ".svg";
         end case;
      end Extension;

      -----------------
      -- Description --
      -----------------

      function Description return String is
      begin
         case Data.Format is
            when Export_PNG =>
               return "PNG Image";
            when Export_PDF =>
               return "PDF Image";
            when Export_SVG =>
               return "SVG Image";
         end case;
      end Description;

   begin
      declare
         Name   : constant Virtual_File :=
                    Select_File
                      (Title             => -("Export Browser As " &
                                                Description),
                       Parent            => Get_Main_Window (Kernel),
                       File_Pattern      => +("*" & Extension),
                       Pattern_Name      => -Description,
                       Default_Name      => +("gpsbrowser" & Extension),
                       Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                       Kind              => Save_File,
                       History           => Get_History (Kernel));
         Surface : Cairo_Surface;
         Width   : Gdouble;
         Height  : Gdouble;
         Cr      : Cairo_Context;
         Status  : Cairo_Status;

      begin
         if Name /= GNATCOLL.VFS.No_File then
            Push_State (Kernel, Busy);
            State_Pushed := True;

            Gtkada.Canvas.Get_Bounding_Box (Canvas, Width, Height);

            case Data.Format is
               when Export_PNG =>
                  Surface := Create_Similar_Surface
                    (Get_Window (Canvas), Cairo.Cairo_Content_Color_Alpha,
                     Gint (Width), Gint (Height));
               when Export_PDF =>
                  Surface := Cairo.PDF.Create
                    (+Full_Name (Name), Width, Height);
               when Export_SVG =>
                  Surface := Cairo.SVG.Create
                    (+Full_Name (Name), Width, Height);
            end case;

            Status := Cairo.Surface.Status (Surface);

            if Status = Cairo_Status_Success then
               Cr := Create (Surface);
               Set_Line_Width (Cr, 0.5);

               Canvas.Draw_All (Cr);

               Destroy (Cr);
            end if;

            case Data.Format is
               when Export_PNG =>
                  Status :=
                    Cairo.Png.Write_To_Png (Surface, +Full_Name (Name));
               when Export_PDF | Export_SVG =>
                  Status := Cairo.Surface.Status (Surface);
            end case;

            Destroy (Surface);

            if Status /= Cairo_Status_Success then
               GPS.Kernel.Console.Insert
                 (Kernel, "Cannot create " & Display_Full_Name (Name),
                  Mode => GPS.Kernel.Console.Error);
            end if;

            State_Pushed := False;
            Pop_State (Kernel);
         end if;
      end;

      return False;
   exception
      when E : others => Trace (Exception_Handle, E);

         if State_Pushed then
            Pop_State (Kernel);
         end if;

         return False;
   end On_Export_Idle;

   ---------------
   -- On_Export --
   ---------------

   procedure On_Export_To_PNG (Browser : access Gtk_Widget_Record'Class) is
      Id : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      Id := Export_Idle.Idle_Add
        (On_Export_Idle'Access,
         (Browser => General_Browser (Browser),
          Format  => Export_PNG));
   end On_Export_To_PNG;

   ----------------------
   -- On_Export_To_PDF --
   ----------------------

   procedure On_Export_To_PDF (Browser : access Gtk_Widget_Record'Class) is
      Id : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      Id := Export_Idle.Idle_Add
        (On_Export_Idle'Access,
         (Browser => General_Browser (Browser),
          Format  => Export_PDF));
   end On_Export_To_PDF;

   ----------------------
   -- On_Export_To_SVG --
   ----------------------

   procedure On_Export_To_SVG (Browser : access Gtk_Widget_Record'Class) is
      Id : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      Id := Export_Idle.Idle_Add
        (On_Export_Idle'Access,
         (Browser => General_Browser (Browser),
          Format  => Export_SVG));
   end On_Export_To_SVG;

   ----------------
   -- On_Refresh --
   ----------------

   procedure On_Refresh (Browser : access Gtk_Widget_Record'Class) is
      B : constant General_Browser := General_Browser (Browser);
   begin
      Push_State (Get_Kernel (B), Busy);
      Set_Layout_Algorithm (B.Canvas, Layer_Layout'Access);
      Layout (B, Force => True);
      Refresh_Canvas (Get_Canvas (B));
      Set_Layout_Algorithm (B.Canvas, Simple_Layout'Access);
      Pop_State (Get_Kernel (B));

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Pop_State (Get_Kernel (B));
   end On_Refresh;

   -------------------
   -- On_Data_Clear --
   -------------------

   procedure On_Data_Clear (Browser : access Gtk_Widget_Record'Class) is
   begin
      Clear (Get_Canvas (General_Browser (Browser)));
   end On_Data_Clear;

   --------------------------
   -- Change_Align_On_Grid --
   --------------------------

   procedure Change_Align_On_Grid
     (Item    : access Gtk_Check_Menu_Item_Record'Class;
      Browser : General_Browser) is
   begin
      Align_On_Grid (Get_Canvas (Browser), Get_Active (Item));

   exception
      when E : others => Traces.Trace (Exception_Handle, E);
   end Change_Align_On_Grid;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All (Browser : access Gtk_Widget_Record'Class)  is
      Canvas : constant Interactive_Canvas := General_Browser (Browser).Canvas;
   begin
      Select_All (Canvas);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Select_All;

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
      Z : constant Gdouble := Get_Zoom (Canvas);
   begin
      for J in Zoom_Levels'Range loop
         if Zoom_Levels (J) = Z then
            if J /= Zoom_Levels'Last then
               Zoom (Canvas, Zoom_Levels (J + 1), Zoom_Duration);
            end if;
         end if;
      end loop;
   end Zoom_In;

   --------------
   -- Zoom_Out --
   --------------

   procedure Zoom_Out (Browser : access Gtk_Widget_Record'Class) is
      Canvas : constant Interactive_Canvas := General_Browser (Browser).Canvas;
      Z      : constant Gdouble := Get_Zoom (Canvas);
   begin
      for J in Zoom_Levels'Range loop
         if Zoom_Levels (J) = Z then
            if J /= Zoom_Levels'First then
               Zoom (Canvas, Zoom_Levels (J - 1), Zoom_Duration);
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
      Zoom (Data.Browser.Canvas, Data.Zoom, 0.0);
   end Zoom_Level;

   ---------------
   -- To_Brower --
   ---------------

   function To_Browser
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class)
      return General_Browser is
   begin
      return General_Browser (Get_Parent (Get_Parent (Canvas)));
   end To_Browser;

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight (Item : access Browser_Item_Record) is
      Cr : Cairo_Context;
   begin
      Cr := Create (Item);
      Redraw_Title_Bar (Browser_Item (Item), Cr);
      Destroy (Cr);
   end Highlight;

   ---------------
   -- Draw_Link --
   ---------------

   overriding procedure Draw_Link
     (Canvas      : access Interactive_Canvas_Record'Class;
      Link        : access Browser_Link_Record;
      Cr          : Cairo_Context;
      Edge_Number : Glib.Gint;
      Show_Annotation : Boolean := True)
   is
      Browser : constant General_Browser := To_Browser (Canvas);
   begin
      if not Show_Annotation then
         Draw_Link (Canvas, Canvas_Link_Access (Link), Cr, Edge_Number, False);

         return;
      end if;

      if not Browser_Item (Get_Src (Link)).Hide_Links
        and then not Browser_Item (Get_Dest (Link)).Hide_Links
      then
         if not Is_Selected (Canvas, Canvas_Item (Get_Src (Link)))
           and then not Is_Selected (Canvas, Canvas_Item (Get_Dest (Link)))
         then
            Set_Source_Color (Cr, Browser.Unselected_Link_Color);
         else
            Set_Source_Color (Cr, Browser.Selected_Link_Color);
         end if;

         Draw_Link
           (Canvas, Canvas_Link_Access (Link), Cr, Edge_Number, True);
      end if;
   end Draw_Link;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel (Browser : access General_Browser_Record)
      return GPS.Kernel.Kernel_Handle is
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
     (Item : access Browser_Item_Record'Class; Title : String := "") is
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
              (Item.Title_Layout, Default_Font.Get_Pref_Font);
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
      B : constant General_Browser := Get_Browser (User);

      function Reset_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean;
      --  Reset the items linked to User

      ----------------
      -- Reset_Item --
      ----------------

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
      Cr     : Cairo_Context;
      Num    : Gint;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      Cb     : Active_Area_Callback'Class)
   is
      Thick         : constant Gint :=
                        X_Thickness (Get_Item_Style (Item));
      Button_Width  : constant Gint :=
                        Get_Width  (Item.Browser.Close_Pixmap);
      Button_Height : constant Gint :=
                        Get_Height (Item.Browser.Close_Pixmap);
      X             : Gint :=
                        Item.Title_Coord.X + Item.Title_Coord.Width + Thick -
                          (Num + 1) * (Margin + Button_Width);
      Y, W, H       : Gint;
      Base          : Cairo_Color;
      Color         : Cairo_Color;
      Style         : Gtk_Style renames Get_Item_Style (Item);
      Ptrn          : Cairo_Pattern;
      State         : Gtk_State_Type;

      use type Gdk.Gdk_Drawable;

   begin
      --  No title ? Don't draw any button

      if Item.Title_Layout = null
        or else Surface (Item) = Null_Surface
      then
         return;
      end if;

      Get_Pixel_Size (Item.Title_Layout, W, H);
      Y := Item.Title_Coord.Y + (Item.Title_Coord.Height - Button_Height) / 2;

      Add_Active_Area
        (Item,
         Gdk_Rectangle'(X, Y, Button_Width, Button_Height),
         Cb);

      --  Button's background
      Rounded_Rectangle
        (Cr,
         Gdouble (X), Gdouble (Y),
         Gdouble (Button_Width), Gdouble (Button_Height),
         2.0);
      Ptrn := Cairo.Pattern.Create_Linear
        (0.0, Gdouble (Y), 0.0, Gdouble (Y + Button_Height - 1));

      if Is_Selected (Canvas (Item), Item) then
         State := State_Selected;
      else
         State := State_Normal;
      end if;

      Base := To_Cairo (Get_Base (Style, State));

      Color := Shade (Base, 1.3);
      Cairo.Pattern.Add_Color_Stop_Rgb (Ptrn, 0.0, Color.R, Color.G, Color.B);
      Color := Shade (Base, 0.95);
      Cairo.Pattern.Add_Color_Stop_Rgb (Ptrn, 1.0, Color.R, Color.G, Color.B);
      Set_Source (Cr, Ptrn);
      Destroy (Ptrn);
      Cairo.Fill (Cr);

      --  Button's borders
      Draw_Shadow
        (Cr, Get_Item_Style (Item),
         Shadow_Out,
         X - X_Thickness (Style),
         Y - Y_Thickness (Style),
         Button_Width + 2 * X_Thickness (Style),
         Button_Height + 2 * Y_Thickness (Style),
         2.0);

      --  The icon
      X := X + (Button_Width - Get_Width (Pixbuf)) / 2;
      Y := Item.Title_Coord.Y +
        (Item.Title_Coord.Height - Get_Height (Pixbuf)) / 2;
      Draw_Pixbuf (Cr, Pixbuf, X, Y);
   end Draw_Title_Bar_Button;

   ------------
   -- Create --
   ------------

   function Create
     (Item : access Browser_Item_Record) return Cairo.Cairo_Context
   is
      Ret : Cairo_Context;
   begin
      Ret := Create (Surface (Item));
      Set_Line_Width (Ret, 0.5);
      return Ret;
   end Create;

   ----------------------
   -- Redraw_Title_Bar --
   ----------------------

   procedure Redraw_Title_Bar
     (Item : access Browser_Item_Record;
      Cr   : Cairo.Cairo_Context)
   is
      State    : Gtk_State_Type := State_Normal;
      Style    : constant Gtk_Style := Get_Item_Style (Item);
      W_L, H_L : Gint;
      Ptrn     : Cairo_Pattern;
      Base     : Cairo_Color;
      Color    : Cairo_Color;

      use type Gdk.Gdk_Drawable;

   begin
      if Item.Title_Layout = null
        or else Surface (Item) = Null_Surface
      then
         return;
      end if;

      Reset_Active_Areas (Item.all, Other_Areas => False);

      if Is_Selected (Item.Browser.Canvas, Item) then
         State := State_Selected;
      end if;

      --  The title background
      Rectangle
        (Cr, Gdouble (Item.Title_Coord.X), Gdouble (Item.Title_Coord.Y),
         Gdouble (Item.Title_Coord.Width), Gdouble (Item.Title_Coord.Height));

      Ptrn := Create_Linear
        (0.0, Gdouble (Item.Title_Coord.Y), 0.0,
         Gdouble (Item.Title_Coord.Y + Item.Title_Coord.Height - 1));
      Base := To_Cairo (Get_Base (Style, State));

      Color := Shade (Base, 1.15);
      Add_Color_Stop_Rgb (Ptrn, 0.0, Color.R, Color.G, Color.B);

      Color := Shade (Base, 0.85);
      Add_Color_Stop_Rgb (Ptrn, 1.0, Color.R, Color.G, Color.B);

      Set_Source (Cr, Ptrn);
      Destroy (Ptrn);

      Cairo.Fill (Cr);

      --  The title string
      Get_Pixel_Size (Item.Title_Layout, W_L, H_L);
      Draw_Layout
        (Cr, Get_Black (Style),
         X      => Margin + Item.Title_Coord.X - X_Thickness (Style),
         Y      => Item.Title_Coord.Y + (Item.Title_Coord.Height - H_L) / 2,
         Layout => Item.Title_Layout);

      --  The separator between the title and the child part
      Draw_Line
        (Cr, Get_Black (Style),
         X1     => Item.Title_Coord.X,
         Y1     => Item.Title_Coord.Y + Item.Title_Coord.Height,
         X2     => Item.Title_Coord.X + Item.Title_Coord.Width,
         Y2     => Item.Title_Coord.Y + Item.Title_Coord.Height);

      --  And now the buttons
      Draw_Title_Bar_Button
        (Item   => Item,
         Cr     => Cr,
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

   --------------------
   -- Get_Item_Style --
   --------------------

   function Get_Item_Style (Item : access Browser_Item_Record) return Gtk_Style
   is
      B      : constant General_Browser := Get_Browser (Item);
      Canvas : constant Interactive_Canvas := Get_Canvas (B);
      Iter   : Item_Iterator;

   begin
      if Is_Selected (Canvas, Item) then
         return B.Item_Style;
      else
         Iter := Start (Canvas, Linked_From_Or_To => Canvas_Item (Item));
         while Get (Iter) /= null loop
            if Is_Selected (Canvas, Get (Iter)) then
               if Is_Linked_From (Iter) then
                  return B.Item_Style_Child_Linked;
               else
                  return B.Item_Style_Parent_Linked;
               end if;
            end if;
            Next (Iter);
         end loop;
      end if;

      return B.Item_Style;
   end Get_Item_Style;

   ----------------------------
   -- Get_Default_Item_Style --
   ----------------------------

   function Get_Default_Item_Style
     (Item : access Browser_Item_Record) return Gtk.Style.Gtk_Style is
   begin
      return Item.Browser.Item_Style;
   end Get_Default_Item_Style;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   procedure Resize_And_Draw
     (Item             : access Browser_Item_Record;
      Cr               : in out Cairo_Context;
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
      Button_Height : constant Gint := Get_Height (Item.Browser.Close_Pixmap);
      W, H          : Gint;
      Layout_H      : Gint := 0;
      State         : Gtk_State_Type := State_Normal;
      Style         : constant Gtk_Style := Get_Item_Style (Item);

      use Gdk;

   begin
      Reset_Active_Areas (Item.all, Title_Bar_Areas => False);

      if Item.Title_Layout /= null then
         Get_Pixel_Size (Item.Title_Layout, W, Layout_H);
         W := Gint'Max
           (W + 2 * Margin +
              Num_Buttons * (Margin + Button_Width),
            Width);
         Item.Title_Coord :=
           (X      => Xoffset + X_Thickness (Style),
            Y      => Yoffset + Y_Thickness (Style),
            Width  => W - 2 * X_Thickness (Style),
            Height =>
              Gint'Max
                (Layout_H,
                 Button_Height + 2 * Y_Thickness (Style)));

         H := Item.Title_Coord.Height + 1 + Height + 2 * Y_Thickness (Style);

      else
         Item.Title_Coord := (others => 0);
         W := Width;
         H := Height;
      end if;

      W := W + Width_Offset;
      H := H + Height_Offset;

      --  Actual resize of the item. Let's replace the context, as we're going
      --  to replace the surface.
      if Cr /= Cairo.Null_Context then
         Destroy (Cr);
      end if;
      Set_Screen_Size (Browser_Item (Item), W, H);
      Cr := Create (Item);

      if Is_Selected (Canvas (Item), Item) then
         State := State_Selected;
      end if;

      Draw_Rectangle
        (Cr, Get_Bg (Style, State),
         Filled => True,
         X      => Xoffset,
         Y      => Layout_H + Yoffset,
         Width  => W - Width_Offset,
         Height => H - Layout_H - Height_Offset,
         Corner_Radius => Gdouble (Margin - X_Thickness (Style)));

      Draw_Shadow
        (Cr, Style, Shadow_Out,
         X      => Xoffset,
         Y      => Yoffset,
         Width  => W - Width_Offset - Xoffset,
         Height => H - Height_Offset - Yoffset,
         Corner_Radius => Gdouble (Margin - X_Thickness (Style)));

      if Item.Title_Layout /= null then
         Yoffset := Yoffset + Item.Title_Coord.Height + 1;
      end if;
   end Resize_And_Draw;

   ---------------------
   -- On_Button_Click --
   ---------------------

   overriding function On_Button_Click
     (Item  : access Browser_Item_Record;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean is
   begin
      Raise_Item (Get_Canvas (Get_Browser (Item)), Item);
      return Activate (Browser_Item (Item), Event);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
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

   overriding procedure Selected
     (Item        : access Browser_Item_Record;
      Canvas      : access Interactive_Canvas_Record'Class;
      Is_Selected : Boolean)
   is
      pragma Unreferenced (Is_Selected);
      --  Call Highlight on Item and all its siblings. If the selection status
      --  has changed, this will result in a change of background color for
      --  these items.
      Iter : Item_Iterator;
      It   : Canvas_Item;
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

   -------------------
   -- Draw_Selected --
   -------------------

   overriding procedure Draw_Selected
     (Item : access Browser_Item_Record;
      Cr   : Cairo.Cairo_Context) is
   begin
      --  Just use the regular Draw method, as hilighting is handled in the
      --  Selected procedure above.
      Draw (Item, Cr);
   end Draw_Selected;

   ----------
   -- Call --
   ----------

   overriding function Call
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

         --  If Area is a child of the new Tmp area
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

   function Activate
     (Item  : access Browser_Item_Record;
      Event : Gdk.Event.Gdk_Event) return Boolean
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

   begin
      return Check_Area (Item.Active_Areas);
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

   overriding procedure Destroy (Item : in out Browser_Item_Record) is
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
      Layout           : Pango_Layout;
      Cr               : Cairo_Context;
   begin
      Layout := Create_Pango_Layout (Get_Browser (Item), "");
      Set_Font_Description (Layout, Default_Font.Get_Pref_Font);

      Cr := Cairo.Null_Context;
      Resize_And_Draw (Item, Cr, 0, 0, 0, 0, Xoffset, Yoffset, Layout);
      Redraw_Title_Bar (Item, Cr);
      Destroy (Cr);

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
      Layout (Get_Canvas (Browser), Force => Force);
   end Layout;

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
      Text     : out GNAT.Strings.String_Access)
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

   -------------------------
   -- Foreach_Active_Area --
   -------------------------

   procedure Foreach_Active_Area
     (Str : String;
      Cb  : access procedure (Link : String))
   is
      S  : Integer := Str'First;
      S2 : Integer;
   begin
      while S /= 0 loop
         S := Ada.Strings.Fixed.Index (Str (S .. Str'Last), "@");
         if S /= 0 then
            S2 := Ada.Strings.Fixed.Index (Str (S + 1 .. Str'Last), "@");
            if S2 /= 0 then
               Cb (Str (S + 1 .. S2 - 1));
               S := S2 + 1;
            else
               exit;
            end if;
         end if;
      end loop;
   end Foreach_Active_Area;

   -----------------
   -- Expand_Line --
   -----------------

   procedure Expand_Line
     (List     : in out Xref_List;
      Num      : Positive;
      Str      : String;
      Callback : Active_Area_Cb_Array := Empty_Cb_Array;
      Check_Duplicates : Boolean)
   is
      Tmp : Xref_Line;
      Has_Duplicate : Boolean := False;

      procedure On_Str_Active (Link : String);
      --  For each link in Str

      procedure On_Str_Active (Link : String) is
         procedure On_Existing_Active (Link2 : String);
         --  For each link on the existing line

         procedure On_Existing_Active (Link2 : String) is
         begin
            if Link = Link2 then
               Has_Duplicate := True;
            end if;
         end On_Existing_Active;

      begin
         Foreach_Active_Area
           (List.Lines (Num - 1 + List.Lines'First).Text.all,
            On_Existing_Active'Unrestricted_Access);
      end On_Str_Active;

   begin
      if List.Lines = null or else Num > List.Lines'Length then
         Add_Line (List, Str, Callback => Callback);
      else
         --  Check whether the cross-references are already there. If they
         --  are, do not add Str again to avoid duplicates

         if Check_Duplicates then
            Foreach_Active_Area (Str, On_Str_Active'Unrestricted_Access);
         end if;

         if not Has_Duplicate then
            Tmp := List.Lines (Num - 1 + List.Lines'First);
            List.Lines (Num - 1 + List.Lines'First) :=
              (Text      => new String'(Tmp.Text.all & Str),
               Callbacks => new Active_Area_Cb_Array'
                 (Tmp.Callbacks.all & Callback),
               Length    => Tmp.Length);
            Free (Tmp.Text);
            Unchecked_Free (Tmp.Callbacks);
         end if;
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
      pragma Unreferenced (Browser);
      Longest1, Longest2 : Gint := 0;
      H2, W              : Gint;
      Last               : Natural;
      Line               : GNAT.Strings.String_Access;

   begin
      H := 0;

      if List.Lines /= null then
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
            if Last < Line'Length then
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
      end if;

      W1 := Longest1;
      W2 := Longest2;
   end Get_Pixel_Size;

   -------------------
   -- Display_Lines --
   -------------------

   procedure Display_Lines
     (Item          : access Browser_Item_Record'Class;
      Cr            : Cairo_Context;
      List          : Xref_List;
      X             : Gint;
      Y             : in out Gint;
      Second_Column : Gint;
      Layout        : access Pango_Layout_Record'Class)
   is
      X2          : Gint;
      First, Last : Integer;
      In_Xref     : Boolean;
      Color       : Gdk_Color;
      W, H        : Gint;
      Num_In_Line : Natural;
      Text        : String_Access;
      Style       : constant Gtk_Style := Get_Item_Style (Item);

      procedure Display (Text : String; Line : Xref_Line);
      --  Display Text on Item

      procedure Display (Text : String; Line : Xref_Line) is
      begin
         if Text'Length = 0 then
            return;
         end if;

         Set_Text (Layout, Text);

         if In_Xref then
            Color := Get_Text (Style, State_Active);
         else
            Color := Get_Text (Style, State_Normal);
         end if;

         Set_Source_Color (Cr, Color);
         Move_To (Cr, Gdouble (X2), Gdouble (Y));
         Pango.Cairo.Show_Layout (Cr, Pango_Layout (Layout));

         Get_Pixel_Size (Layout, W, H);

         if In_Xref then
            Move_To (Cr, Gdouble (X2), Gdouble (Y + H));
            Line_To (Cr, Gdouble (X2 + W), Gdouble (Y + H));
            Stroke (Cr);

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

   overriding procedure Reset
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

   overriding function Get_Last_Button_Number (Item : access Arrow_Item_Record)
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

   overriding procedure Redraw_Title_Bar
     (Item : access Arrow_Item_Record;
      Cr   : Cairo_Context)
   is
   begin
      Redraw_Title_Bar (Browser_Item_Record (Item.all)'Access, Cr);

      if not Item.Parents_Shown then
         Draw_Title_Bar_Button
           (Item, Cr,
            Num    => Get_Last_Button_Number (Item),
            Pixbuf => Get_Parents_Arrow (Get_Browser (Item)),
            Cb     => Build (Compute_Parents'Access, Item));
      end if;

      if not Item.Children_Shown then
         Draw_Title_Bar_Button
           (Item, Cr,
            Num    => Get_Last_Button_Number (Item) - 1,
            Pixbuf => Get_Children_Arrow (Get_Browser (Item)),
            Cb     => Build (Compute_Children'Access, Item));
      end if;
   end Redraw_Title_Bar;

   -----------------------------
   -- Add_Navigation_Location --
   -----------------------------

   procedure Add_Navigation_Location
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Title  : String) is
   begin
      Push_Marker_In_History
        (Kernel, Create_Browser_Marker (Browser_Name => Title));
   end Add_Navigation_Location;

   -----------
   -- Go_To --
   -----------

   overriding function Go_To
     (Marker : access Browser_Marker_Record;
      Kernel : access Kernel_Handle_Record'Class) return Boolean
   is
      Child : constant MDI_Child := Find_MDI_Child_By_Name
        (Get_MDI (Kernel), Marker.Title.all);
   begin
      if Child = null then
         return False;
      else
         Raise_Child (Child, Give_Focus => True);
         return True;
      end if;
   end Go_To;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Marker : in out Browser_Marker_Record) is
   begin
      Free (Marker.Title);
   end Destroy;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Marker : access Browser_Marker_Record) return String is
   begin
      return "Browser: " & Marker.Title.all;
   end To_String;

   ----------
   -- Save --
   ----------

   overriding function Save
     (Marker : access Browser_Marker_Record) return XML_Utils.Node_Ptr
   is
      N : constant Node_Ptr := new Node;
   begin
      N.Tag   := new String'("browser_marker");
      N.Value := new String'(Marker.Title.all);
      return N;
   end Save;

   -------------
   -- Similar --
   -------------

   overriding function Similar
     (Left  : access Browser_Marker_Record;
      Right : access Location_Marker_Record'Class) return Boolean
   is
      pragma Unreferenced (Left, Right);
   begin
      return False;
   end Similar;

   ---------------------------
   -- Create_Browser_Marker --
   ---------------------------

   function Create_Browser_Marker
     (Browser_Name : String) return Browser_Marker is
   begin
      return new Browser_Marker_Record'
        (Location_Marker_Record with Title => new String'(Browser_Name));
   end Create_Browser_Marker;

   -----------
   -- Clone --
   -----------

   overriding function Clone
     (Marker : access Browser_Marker_Record)
      return Location_Marker is
   begin
      return Location_Marker (Create_Browser_Marker (Marker.Title.all));
   end Clone;

   --------------------
   -- Get_Orthogonal --
   --------------------

   function Get_Orthogonal (E : access Browser_Link_Record) return Boolean is
   begin
      return E.Orthogonal;
   end Get_Orthogonal;

   ----------------------
   -- Register_Actions --
   ----------------------

   procedure Register_Actions (Kernel : access Kernel_Handle_Record'Class) is
      Command : constant Interactive_Command_Access := new Select_All_Command;
   begin
      Register_Action
        (Kernel,
         "Select All In Browser",
         Command,
         -"Select all items in a browser",
         Category => "Browsers");
   end Register_Actions;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Select_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Child  : constant MDI_Child :=
                  Get (First_Child (Get_MDI (Get_Kernel (Context.Context))));
      Widget : constant Gtk_Widget := Get_Widget (Child);
   begin
      if Widget.all in General_Browser_Record'Class then
         On_Select_All (General_Browser (Widget));
         return Commands.Success;
      else
         return
           Commands.Failure;
      end if;
   end Execute;

end Browsers.Canvas;
