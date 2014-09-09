------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
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

with GNAT.Strings;                      use GNAT.Strings;
with GNATCOLL.VFS;                      use GNATCOLL.VFS;

with Glib;                              use Glib;
with Glib.Main;                         use Glib.Main;
with Glib.Object;                       use Glib.Object;

with Cairo;                             use Cairo;
with Cairo.PDF;                         use Cairo.PDF;
with Cairo.Png;                         use Cairo.Png;
with Cairo.Surface;                     use Cairo.Surface;
with Cairo.SVG;                         use Cairo.SVG;

with Pango.Enums;                       use Pango.Enums;
with Pango.Font;                        use Pango.Font;
with Pango.Layout;                      use Pango.Layout;

with Gdk;                               use Gdk;
with Gdk.Event;                         use Gdk.Event;
with Gdk.Pixbuf;                        use Gdk.Pixbuf;
with Gdk.RGBA;                          use Gdk.RGBA;
with Gdk.Rectangle;                     use Gdk.Rectangle;
with Gdk.Window;                        use Gdk.Window;

with Gtk.Box;                           use Gtk.Box;
with Gtk.Check_Menu_Item;               use Gtk.Check_Menu_Item;
with Gtk.Enums;                         use Gtk.Enums;
with Gtk.Handlers;                      use Gtk.Handlers;
with Gtk.Hbutton_Box;                   use Gtk.Hbutton_Box;
with Gtk.Menu;                          use Gtk.Menu;
with Gtk.Menu_Item;                     use Gtk.Menu_Item;
with Gtk.Menu_Tool_Button;              use Gtk.Menu_Tool_Button;
with Gtk.Scrolled_Window;               use Gtk.Scrolled_Window;
with Gtk.Stock;                         use Gtk.Stock;
with Gtk.Style;                         use Gtk.Style;
with Gtk.Style_Context;                 use Gtk.Style_Context;
with Gtk.Toolbar;                       use Gtk.Toolbar;
with Gtk.Tool_Button;                   use Gtk.Tool_Button;
with Gtk.Widget;                        use Gtk.Widget;

with Gtkada.Canvas;                     use Gtkada.Canvas;
with Gtkada.Canvas_View;                use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Models.Layers; use Gtkada.Canvas_View.Models.Layers;
with Gtkada.File_Selector;              use Gtkada.File_Selector;
with Gtkada.Handlers;                   use Gtkada.Handlers;
with Gtkada.MDI;                        use Gtkada.MDI;
with Gtkada.Style;                      use Gtkada.Style;

with Commands;                          use Commands;
with Commands.Interactive;              use Commands.Interactive;
with Default_Preferences;               use Default_Preferences;
with Generic_Views;                     use Generic_Views;
with GPS.Intl;                          use GPS.Intl;
with GPS.Kernel;                        use GPS.Kernel;
with GPS.Kernel.Actions;                use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;                  use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;            use GPS.Kernel.Preferences;
with GPS.Kernel.MDI;                    use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;             use GPS.Kernel.Modules.UI;
with GPS.Stock_Icons;                   use GPS.Stock_Icons;
with Histories;                         use Histories;
with Layouts;                           use Layouts;
with XML_Utils;                         use XML_Utils;

package body Browsers.Canvas is

   Zoom_Levels : constant array (Positive range <>) of Gdouble :=
                   (0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0);
   --  All the possible zoom levels. We have to use such an array, instead
   --  of doing the computation directly, so as to avoid rounding errors that
   --  would appear in the computation and make zoom_in not the reverse of
   --  zoom_out.

   Hist_Align_On_Grid : constant History_Key := "browsers-align-on-grid";
   Hist_Straight_Links : constant History_Key := "browsers-straight-links";
   Hist_Draw_Grid : constant History_Key := "browsers-display-grid";
   Hist_Add_Waypoints : constant History_Key := "browsers-add-waypoints";
   Hist_Vertical      : constant History_Key := "browsers-vertical";

   Zoom_Duration : constant := 0.25;
   --  Duration of the zoom animation

   type Export_Idle_Data is record
      Browser : General_Browser;
      Format  : Gtkada.Canvas_View.Export_Format;
   end record;

   package Export_Idle is
     new Glib.Main.Generic_Sources (Export_Idle_Data);

   type Cb_Data is record
      Browser       : General_Browser;
      Item          : Gtkada.Canvas.Canvas_Item;
      Zoom          : Gdouble;
      Keep_Selected : Boolean;
   end record;

   package Contextual_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Cb_Data);

   type Zoom_In_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Zoom_In_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Zoom in to the previous zoom level, if any

   type Zoom_Out_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Zoom_Out_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Zoom out to the next zoom level, if any

   procedure Zoom_Level
     (Item : access Gtk_Widget_Record'Class; Data : Cb_Data);
   --  Zoom directly to a specific level (Data.Zoom)

   function On_Export_Idle (Data : Export_Idle_Data) return Boolean;
   --  Does the actual export in an idle, called by the bellow export
   --  subprograms

   procedure On_Export_To_PNG (Browser : access Gtk_Widget_Record'Class);
   --  Export the contents of the browser as an image

   procedure On_Export_To_SVG (Browser : access Gtk_Widget_Record'Class);
   --  Export the contents of the browser to SVG

   procedure On_Export_To_PDF (Browser : access Gtk_Widget_Record'Class);
   --  Export the contents of the browser to PDF

   type Refresh_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Refresh_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Recompute the layout of the canvas

   procedure Change_Align_On_Grid (Browser : access Gtk_Widget_Record'Class);
   --  Callback for the "align on grid" contextual menu item

   type Clear_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Clear_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  "Clear" contextual menu

   type Select_All_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Select_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Is_In_Browser is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_In_Browser;
      Context : Selection_Context) return Boolean;
   --  Whether the focus is currently on a browser

   type Toggle_Links is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Toggle_Links;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Toggle the display of links for the item

   procedure Toggle_Orthogonal (Browser : access Gtk_Widget_Record'Class);
   --  Toggle the layout of links

   procedure Toggle_Draw_Grid (Browser : access Gtk_Widget_Record'Class);
   --  Toggle the drawing of the grid, and refresh the canvas.

   type Remove_Unselected_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Remove_Unselected_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Remove all unselected items

   function Browser_From_Context
     (Context : Selection_Context) return General_Browser;
   --  Get the browser from the context

   type Remove_Selected_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Remove_Selected_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Remove all selected items

   procedure Destroyed (Browser : access Gtk_Widget_Record'Class);
   --  Called when the browser is destroyed

   type Preferences_Hook_Record is new Function_With_Args with record
      Browser : General_Browser;
   end record;
   type Preferences_Hook is access all Preferences_Hook_Record'Class;
   overriding procedure Execute
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when the preferences have changed

   type Image_Canvas_Record is new
     Gtkada.Canvas.Interactive_Canvas_Record with null record;
   type Image_Canvas is access all Image_Canvas_Record'Class;

   overriding function Get_Window
     (Canvas : access Image_Canvas_Record) return Gdk.Gdk_Window;
   --  Override Gtk.Widget.Get_Window, so that a different Window can be
   --  returned if needed (e.g. when exporting the canvas).

   procedure On_Selection_Changed
     (Self  : not null access GObject_Record'Class;
      Item  : Abstract_Item);
   --  Called when the selection changes. This highlights links to and from
   --  that item.

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
   overriding function Distance
     (Left  : access Browser_Marker_Record;
      Right : access Location_Marker_Record'Class) return Integer;
   --  See inherited documentation

   function Create_Browser_Marker
     (Browser_Name : String) return Browser_Marker;
   --  Create a new marker that will bring the user back to the browser

   ----------------
   -- Get_Window --
   ----------------

   overriding function Get_Window
     (Canvas : access Image_Canvas_Record) return Gdk.Gdk_Window is
   begin
      return Get_Window (Gtk_Widget_Record (Canvas.all)'Access);
   end Get_Window;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
     (Self  : not null access GObject_Record'Class;
      Item  : Abstract_Item)
   is
   begin
      Highlight_Related_Items (General_Browser (Self).Get_View, Item);
   end On_Selection_Changed;

   -----------------------------
   -- Highlight_Related_Items --
   -----------------------------

   procedure Highlight_Related_Items
     (Self   : not null access GPS_Canvas_View_Record'Class;
      Item   : access Gtkada.Canvas_View.Abstract_Item_Record'Class := null)
   is
      Styles   : constant access Browser_Styles := Self.Get_Styles;
      Selected : Outline_Mode;

      procedure On_Link (Link : not null access Abstract_Item_Record'Class);
      procedure On_Link (Link : not null access Abstract_Item_Record'Class) is
         It   : GPS_Link;
         Dest : Abstract_Item;
      begin
         if Link.all in GPS_Link_Record'Class then
            It := GPS_Link (Link);
            if not It.Invisible then
               case Selected is
                  when Outline_None =>
                     It.Set_Style (It.Default_Style);

                  when Outline_As_Linked | Outline_As_Match =>
                     It.Set_Style (Styles.Selected_Link);
               end case;
            end if;

            Dest := Get_From (It);
            if Dest /= Item then
               GPS_Item (Dest).Outline := Selected;
            end if;

            Dest := Get_To (It);
            if Dest /= Item then
               GPS_Item (Dest).Outline := Selected;
            end if;
         end if;
      end On_Link;

      S : Item_Sets.Set;

   begin
      if Item = null then
         --  clear selection
         Selected := Outline_None;
         Self.Model.For_Each_Item (On_Link'Access, Filter => Kind_Link);
      else
         if Self.Model.Is_Selected (Item) then
            Selected := Outline_As_Linked;
         else
            Selected := Outline_None;
         end if;

         S.Include (Item);
         Self.Model.For_Each_Link (On_Link'Access, From_Or_To => S);
      end if;
   end Highlight_Related_Items;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (Browser : not null access General_Browser_Record;
      Context : in out GPS.Kernel.Selection_Context)
   is
      Topmost_Selected : Abstract_Item;

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         Topmost_Selected := Abstract_Item (Item);
      end On_Item;

   begin
      Browser.Get_View.Model.For_Each_Item
        (Callback      => On_Item'Unrestricted_Access,
         Selected_Only => True);

      if Topmost_Selected /= null then
         GPS_Item (Topmost_Selected).Set_Context (Context);
      end if;
   end Set_Context;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Browser         : access General_Browser_Record'Class;
      Create_Toolbar  : Boolean := False;
      Parents_Pixmap  : String := Stock_Go_Back;
      Children_Pixmap : String := Stock_Go_Forward;
      Use_Canvas_View : Boolean := False)
   is
      Hook     : Preferences_Hook;
      Scrolled : Gtk_Scrolled_Window;
      Canvas   : Image_Canvas;
      Id       : Handler_Id;
      pragma Unreferenced (Id);
   begin
      Gtk.Box.Initialize_Vbox (Browser, Homogeneous => False);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Pack_Start (Browser, Scrolled, Expand => True, Fill => True);

      Browser.Use_Canvas_View := Use_Canvas_View;

      if Browser.Use_Canvas_View then
         List_Rtree.Gtk_New (Browser.Model);
         Browser.Model.Set_Selection_Mode (Selection_Multiple);
         Gtk_New (Browser.View, Browser.Model);
         Scrolled.Add (Browser.View);

         Id := Browser.Get_View.Model.On_Selection_Changed
           (On_Selection_Changed'Access, Browser);

      else
         Canvas := new Image_Canvas_Record;
         Gtkada.Canvas.Initialize (Canvas);
         Browser.Canvas := Interactive_Canvas (Canvas);
         Scrolled.Add (Browser.Canvas);
         Canvas.Add_Events (Key_Press_Mask);
         Set_Layout_Algorithm (Browser.Canvas, Simple_Layout'Access);
         Set_Auto_Layout (Browser.Canvas, False);
      end if;

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

      Create_New_Boolean_Key_If_Necessary
        (Get_History (Browser.Kernel).all,
         Hist_Draw_Grid, Default_Value => False);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Browser.Kernel).all,
         Hist_Straight_Links, Default_Value => True);

      Hook := new Preferences_Hook_Record;
      Hook.Browser := General_Browser (Browser);
      Add_Hook
        (Browser.Kernel, Preference_Changed_Hook, Hook,
         Name => "browsers.preferences_changed", Watch => GObject (Browser));
      Execute (Hook.all, Browser.Kernel, Data => null);

      Change_Align_On_Grid (Browser);
      Toggle_Orthogonal (Browser);
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
   end Destroyed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel, Data);
      Iter : Item_Iterator;
      B    : constant General_Browser := Hook.Browser;
      F    : Pango_Font_Description;

   begin
      Refresh_Layout_Orientation (B);

      if B.Use_Canvas_View then
         Create_Styles (B.View);

         --  ??? Unused preference Child_Linked_Item_Color

      else
         F := GPS.Kernel.Preferences.Default_Font.Get_Pref_Font;

         Iter := Start (B.Canvas);
         while Get (Iter) /= null loop
            declare
               Item   : constant Browser_Item := Browser_Item (Get (Iter));
               Layout : constant Pango.Layout.Pango_Layout :=
                 Item.Title_Layout;
            begin
               if Layout /= null then
                  Set_Font_Description (Layout, Copy (F));
               end if;
            end;

            Next (Iter);
         end loop;
      end if;

      Toggle_Draw_Grid (B);
   end Execute;

   --------------------------------
   -- Refresh_Layout_Orientation --
   --------------------------------

   procedure Refresh_Layout_Orientation
     (Browser : access General_Browser_Record) is
   begin
      if not Browser.Use_Canvas_View then
         Set_Layout_Orientation
           (Browser.Canvas,
            Vertical_Layout => Browsers_Vertical_Layout.Get_Pref);
      end if;
   end Refresh_Layout_Orientation;

   -----------------
   -- Get_Toolbar --
   -----------------

   function Get_Toolbar (Browser : access General_Browser_Record)
      return Gtk.Hbutton_Box.Gtk_Hbutton_Box is
   begin
      return Browser.Toolbar;
   end Get_Toolbar;

   ----------------
   -- Get_Canvas --
   ----------------

   function Get_Canvas (Browser : access General_Browser_Record)
      return Interactive_Canvas is
   begin
      return Browser.Canvas;
   end Get_Canvas;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Browser : access General_Browser_Record) return GPS_Canvas_View is
   begin
      return Browser.View;
   end Get_View;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access General_Browser_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
   is
      Kernel     : constant Kernel_Handle := View.Kernel;
      Menu       : Gtk_Menu_Tool_Button;
      Zooms_Menu : Gtk_Menu;
      Export_Menu  : Gtk_Menu;
      Mitem      : Gtk_Menu_Item;
   begin
      Gtk_New (Zooms_Menu);

      for J in Zoom_Levels'Range loop
         Gtk_New
           (Mitem,
            Label => Guint'Image (Guint (Zoom_Levels (J) * 100.0)) & '%');
         Zooms_Menu.Append (Mitem);
         Contextual_Cb.Connect
           (Mitem, Gtk.Menu_Item.Signal_Activate, Zoom_Level'Access,
            (Browser => General_Browser (View),
             Item    => null,
             Keep_Selected => True,
             Zoom    => Zoom_Levels (J)));
      end loop;

      Gtk_New_From_Stock (Menu, Stock_Zoom_100);
      Menu.Set_Tooltip_Text (-"Reset zoom level");
      Menu.Set_Menu (Zooms_Menu);
      Zooms_Menu.Show_All;
      Toolbar.Insert (Menu, Get_Toolbar_Section (Kernel, Toolbar, "zoom"));
      Contextual_Cb.Connect
        (Menu, Gtk.Tool_Button.Signal_Clicked, Zoom_Level'Access,
         (Browser => General_Browser (View),
          Item    => null,
          Keep_Selected => True,
          Zoom    => 1.0));

      Gtk_New (Export_Menu);

      Gtk_New (Mitem, Label => -"PDF");
      Export_Menu.Append (Mitem);
      Widget_Callback.Object_Connect
        (Mitem, Gtk.Menu_Item.Signal_Activate, On_Export_To_PDF'Access, View);

      Gtk_New (Mitem, Label => -"PNG");
      Export_Menu.Append (Mitem);
      Widget_Callback.Object_Connect
        (Mitem, Gtk.Menu_Item.Signal_Activate, On_Export_To_PNG'Access, View);

      Gtk_New (Mitem, Label => -"SVG");
      Export_Menu.Append (Mitem);
      Widget_Callback.Object_Connect
        (Mitem, Gtk.Menu_Item.Signal_Activate, On_Export_To_SVG'Access, View);

      Gtk_New_From_Stock (Menu, GPS_Save);
      Menu.Set_Tooltip_Text (-"Export to...");
      Menu.Set_Menu (Export_Menu);
      Export_Menu.Show_All;
      Toolbar.Insert (Menu, Get_Toolbar_Section (Kernel, Toolbar, "export"));
      Widget_Callback.Object_Connect
        (Menu, Gtk.Tool_Button.Signal_Clicked, On_Export_To_PDF'Access, View);
   end Create_Toolbar;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access General_Browser_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Check : Gtk_Check_Menu_Item;
   begin
      Gtk_New (Check, Label => -"Align On Grid");
      Associate
        (Get_History (View.Kernel).all, Hist_Align_On_Grid,
         Check, Default => True);
      Menu.Append (Check);
      Widget_Callback.Object_Connect
        (Check, Gtk.Check_Menu_Item.Signal_Toggled,
         Change_Align_On_Grid'Access, View);

      Gtk_New (Check, Label => -"Draw grid");
      Check.Set_Tooltip_Text
        (-"Whether to draw a grid in the background");
      Associate
        (Get_History (View.Kernel).all, Hist_Draw_Grid,
         Check, Default => False);
      Menu.Append (Check);
      Widget_Callback.Object_Connect
        (Check, Gtk.Check_Menu_Item.Signal_Toggled,
         Toggle_Draw_Grid'Access, View);

      if not View.Use_Canvas_View then
         Gtk_New (Check, Label => -"Straight links");
         Check.Set_Tooltip_Text
           (-("Whether to force only horizontal and vertical links, or allow"
            & " straight lines"));
         Associate
           (Get_History (View.Kernel).all, Hist_Straight_Links,
            Check, Default => True);
         Menu.Append (Check);
         Widget_Callback.Object_Connect
           (Check, Gtk.Check_Menu_Item.Signal_Toggled,
            Toggle_Orthogonal'Access, View);
      end if;

      if View.Use_Canvas_View then
         Gtk_New (Check, Label => -"Vertical layout");
         Check.Set_Tooltip_Text
           (-("General orientation of the layout: either from left to right,"
            & " or from top to bottom"));
         Menu.Append (Check);
         Associate
           (Get_History (View.Kernel).all, Hist_Vertical, Check);
         Widget_Callback.Object_Connect
           (Check, Gtk.Check_Menu_Item.Signal_Toggled,
            Force_Refresh'Access, View);

         Gtk_New (Check, Label => -"Use waypoints");
         Check.Set_Tooltip_Text
           (-("Whether to insert waypoints in long edges when performing the"
            & " layout of the graph. This might result in less edge crossings"
            & " but is sometimes harder to use interactively"));
         Menu.Append (Check);
         Associate
           (Get_History (View.Kernel).all, Hist_Add_Waypoints, Check,
            Default => False);
         Widget_Callback.Object_Connect
           (Check, Gtk.Check_Menu_Item.Signal_Toggled,
            Force_Refresh'Access, View);
      end if;
   end Create_Menu;

   --------------
   -- Has_Link --
   --------------

   function Has_Link
     (Browser   : not null access General_Browser_Record'Class;
      Src, Dest : not null access GPS_Item_Record'Class) return Boolean
   is
      Exists : Boolean := False;

      procedure On_Link (Item : not null access Abstract_Item_Record'Class);
      procedure On_Link (Item : not null access Abstract_Item_Record'Class) is
      begin
         if not Exists
           and then Item.all in GPS_Link_Record'Class
           and then GPS_Link (Item).Get_To = Abstract_Item (Dest)
         then
            Exists := True;
         end if;
      end On_Link;

      S : Item_Sets.Set;

   begin
      S.Include (Abstract_Item (Src));
      Browser.Get_View.Model.For_Each_Link (On_Link'Access, From_Or_To => S);
      return Exists;
   end Has_Link;

   -----------------
   -- Count_Links --
   -----------------

   function Count_Links
     (Browser   : not null access General_Browser_Record'Class;
      Src, Dest : not null access GPS_Item_Record'Class;
      Oriented  : Boolean := True) return Natural
   is
      Count : Natural := 0;

      procedure On_Link (Item : not null access Abstract_Item_Record'Class);
      procedure On_Link (Item : not null access Abstract_Item_Record'Class) is
      begin
         if Item.all in GPS_Link_Record'Class and then
           (GPS_Link (Item).Get_To = Abstract_Item (Dest)
           or else
           (Oriented and then GPS_Link (Item).Get_From = Abstract_Item (Dest)))
         then
            Count := Count + 1;
         end if;
      end On_Link;

      S : Item_Sets.Set;

   begin
      S.Include (Abstract_Item (Src));
      Browser.Get_View.Model.For_Each_Link (On_Link'Access, From_Or_To => S);
      return Count;
   end Count_Links;

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
      pragma Unreferenced (Event_Widget, Kernel);
      B            : constant General_Browser := General_Browser (Object);
      Item         : Gtkada.Canvas.Canvas_Item;
      Xr, Yr       : Gint;
      Xroot, Yroot : Gdouble;
      Xsave, Ysave : Gdouble;
      Details      : Canvas_Event_Details;

   begin
      if Get_Event_Type (Event) in Button_Press .. Button_Release then
         if B.Use_Canvas_View then
            B.View.Set_Details (Details, Event.Button);
            if Details.Toplevel_Item /= null then
               GPS_Item (Details.Toplevel_Item).Set_Context (Context);
            end if;
         else
            Get_Origin (Get_Window (B.Canvas), Xr, Yr);
            Get_Root_Coords (Event, Xroot, Yroot);
            Event.Button.X := Xroot - Gdouble (Xr);
            Event.Button.Y := Yroot - Gdouble (Yr);
            Item := Item_At_Coordinates (B.Canvas, Event);

            if Item /= null then
               Get_Coords (Event, Xsave, Ysave);
               Event.Button.X := Xsave - Gdouble (Get_Coord (Item).X);
               Event.Button.Y := Ysave - Gdouble (Get_Coord (Item).Y);
               Contextual_Factory
                 (Browser_Item (Item), Context, B, Event, Menu);
               Event.Button.X := Xsave;
               Event.Button.Y := Ysave;
            end if;
         end if;
      end if;
   end Default_Browser_Context_Factory;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Remove_Selected_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      B : constant General_Browser := Browser_From_Context (Context.Context);
      Iter : Item_Iterator;
      Item : Gtkada.Canvas.Canvas_Item;
      To_Remove : Item_Sets.Set;

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         if B.Model.Is_Selected (Item) then
            B.Model.Include_Related_Items (Item, To_Remove);
         end if;
      end On_Item;

   begin
      if B.Use_Canvas_View then
         B.Model.For_Each_Item (On_Item'Access, Filter => Kind_Item);
         B.Model.Remove (To_Remove);
         B.Model.Refresh_Layout;
         B.View.Queue_Draw;
      else
         Iter := Start (B.Canvas);
         loop
            Item := Get (Iter);
            exit when Item = null;

            Next (Iter);

            if Is_Selected (B.Canvas, Item) then
               Remove (B.Canvas, Item);
            else
               Reset (Browser_Item (Item), True, True);
            end if;
         end loop;

         Layout (B);
         Refresh_Canvas (B.Canvas);

         Iter := Start (B.Canvas);
         Item := Get (Iter);
         if Item /= null then
            Show_Item (B.Canvas, Item);
         end if;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Remove_Unselected_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      B : constant General_Browser := Browser_From_Context (Context.Context);
      Iter : Item_Iterator;
      Item : Gtkada.Canvas.Canvas_Item;
      To_Remove : Item_Sets.Set;

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         if not B.Model.Is_Selected (Item) then
            B.Model.Include_Related_Items (Item, To_Remove);
         end if;
      end On_Item;

   begin
      if B.Use_Canvas_View then
         B.Model.For_Each_Item (On_Item'Access, Filter => Kind_Item);
         B.Model.Remove (To_Remove);
         B.Model.Refresh_Layout;
         B.View.Queue_Draw;
      else
         Iter := Start (B.Canvas);
         loop
            Item := Get (Iter);
            exit when Item = null;

            Next (Iter);

            if not Is_Selected (B.Canvas, Item) then
               Remove (B.Canvas, Item);
            else
               Reset (Browser_Item (Item), True, True);
            end if;
         end loop;

         Layout (B);
         Refresh_Canvas (B.Canvas);

         Iter := Start (B.Canvas);
         Item := Get (Iter);
         if Item /= null then
            Show_Item (B.Canvas, Item);
         end if;
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Toggle_Links;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      B : constant General_Browser := Browser_From_Context (Context.Context);
      Iter : Item_Iterator;
      Item : Gtkada.Canvas.Canvas_Item;
      S    : Item_Sets.Set;
      Is_First_Link : Boolean := True;
      Make_Invisible : Boolean;

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         S.Include (Abstract_Item (Item));
      end On_Item;

      procedure On_Link (Item : not null access Abstract_Item_Record'Class);
      procedure On_Link (Item : not null access Abstract_Item_Record'Class) is
         L : constant GPS_Link := GPS_Link (Item);
      begin
         if Is_First_Link then
            --  All links must be made invisible or visible.
            Make_Invisible := not L.Invisible;
            Is_First_Link := False;
         end if;

         L.Invisible := Make_Invisible;

         if not L.Invisible then
            L.Set_Style (L.Default_Style);
         else
            L.Set_Style (B.Get_View.Get_Styles.Invisible);
         end if;
      end On_Link;

   begin
      if B.Use_Canvas_View then
         B.Model.For_Each_Item
           (On_Item'Access, Filter => Kind_Item, Selected_Only => True);
         B.View.Model.For_Each_Link (On_Link'Access, From_Or_To => S);
         B.View.Queue_Draw;

      else
         Iter := Start (B.Canvas);
         loop
            Item := Get (Iter);
            exit when Item = null;

            Browser_Item (Item).Hide_Links :=
              not Browser_Item (Item).Hide_Links;
            Next (Iter);
         end loop;
         Refresh_Canvas (B.Canvas);
      end if;
      return Commands.Success;
   end Execute;

   --------------------
   -- On_Export_Idle --
   --------------------

   function On_Export_Idle (Data : Export_Idle_Data) return Boolean is
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
         Success : Boolean;
         Canvas  : Interactive_Canvas;

      begin
         if Name /= GNATCOLL.VFS.No_File then
            if Data.Browser.Use_Canvas_View then
               Success := Data.Browser.View.Export
                 (Filename          => Name.Display_Full_Name,
                  Page              => A4_Landscape,
                  Format            => Data.Format,
                  Visible_Area_Only => True);
            else
               Canvas := Get_Canvas (Data.Browser);
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
               Success := Status = Cairo_Status_Success;
            end if;

            if not Success then
               Kernel.Insert
                 ("Cannot create " & Name.Display_Full_Name,
                  Mode => GPS.Kernel.Error);
            end if;
         end if;
      end;

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

   -----------------------
   -- Horizontal_Layout --
   -----------------------

   function Horizontal_Layout
     (Self : not null access General_Browser_Record) return Boolean is
   begin
      return not Get_History (Get_History (Self.Kernel).all, Hist_Vertical);
   end Horizontal_Layout;

   --------------------
   -- Refresh_Layout --
   --------------------

   procedure Refresh_Layout
     (Self                 : not null access General_Browser_Record;
      Rescale              : Boolean := False;
      Space_Between_Items  : Gdouble := Default_Space_Between_Items;
      Space_Between_Layers : Gdouble := Default_Space_Between_Layers)
   is
   begin
      if Self.Use_Canvas_View then
         --  Recompute the size of all boxes
         Self.View.Model.Refresh_Layout;

         --  Now compute the position of the boxes.

         Gtkada.Canvas_View.Models.Layers.Layout
           (Self.View.Model,
            View                 => Self.View,  --  animate
            Horizontal           => Self.Horizontal_Layout,
            Add_Waypoints        =>
              Get_History (Get_History (Self.Kernel).all, Hist_Add_Waypoints),
            Space_Between_Items  => Space_Between_Items,
            Space_Between_Layers => Space_Between_Layers);

         if Rescale then
            Self.View.Scale_To_Fit
              (Min_Scale => 0.5, Max_Scale => 2.0, Duration => 0.8);
         end if;

      else
         Set_Layout_Algorithm (Self.Canvas, Layer_Layout'Access);
         Layout (Self, Force => True);
         Refresh_Canvas (Get_Canvas (Self));
         Set_Layout_Algorithm (Self.Canvas, Simple_Layout'Access);
      end if;
   end Refresh_Layout;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Refresh_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      B : constant General_Browser := Browser_From_Context (Context.Context);
   begin
      Refresh_Layout (B, Rescale => True);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Clear_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      B : constant General_Browser := Browser_From_Context (Context.Context);
   begin
      if B.Use_Canvas_View then
         Browser_Model (B.View.Model).Clear;
         Browser_Model (B.View.Model).Refresh_Layout;
      else
         B.Canvas.Clear;
      end if;
      return Commands.Success;
   end Execute;

   --------------------------
   -- Change_Align_On_Grid --
   --------------------------

   procedure Change_Align_On_Grid (Browser : access Gtk_Widget_Record'Class) is
      View  : constant General_Browser := General_Browser (Browser);
      Align : constant Boolean :=
        Get_History (Get_History (View.Kernel).all, Hist_Align_On_Grid);
   begin
      if View.Use_Canvas_View then
         View.View.Set_Snap
           (Snap_To_Grid   => Align,
            Snap_To_Guides => False);
      else
         Align_On_Grid (Get_Canvas (View), Align);
      end if;
   end Change_Align_On_Grid;

   -----------------------
   -- Toggle_Orthogonal --
   -----------------------

   procedure Toggle_Orthogonal (Browser : access Gtk_Widget_Record'Class) is
      View  : constant General_Browser := General_Browser (Browser);
      Straight : constant Boolean :=
        Get_History (Get_History (View.Kernel).all, Hist_Straight_Links);
   begin
      if not View.Use_Canvas_View then
         Set_Orthogonal_Links (Get_Canvas (View), not Straight);
         Refresh_Canvas (Get_Canvas (View));
      end if;
   end Toggle_Orthogonal;

   -------------------
   -- Force_Refresh --
   -------------------

   procedure Force_Refresh (Browser : access Gtk_Widget_Record'Class) is
      View  : constant General_Browser := General_Browser (Browser);
   begin
      Refresh_Layout (View);
   end Force_Refresh;

   ----------------------
   -- Toggle_Draw_Grid --
   ----------------------

   procedure Toggle_Draw_Grid (Browser : access Gtk_Widget_Record'Class) is
      View  : constant General_Browser := General_Browser (Browser);
      Draw_Grid : constant Boolean :=
        Get_History (Get_History (View.Kernel).all, Hist_Draw_Grid);
      Annotation_Font : Pango_Font_Description;
   begin
      Annotation_Font := Copy (Preferences.Default_Font.Get_Pref_Font);
      Set_Size
        (Annotation_Font,
         Gint'Max (Pango_Scale, Get_Size (Annotation_Font) - 2 * Pango_Scale));

      if View.Use_Canvas_View then
         View.View.Set_Grid_Size
           (Size => Model_Coordinate (Gtkada.Canvas.Default_Grid_Size));

         if not Draw_Grid then
            View.View.Background := Background_Color;
         else
            View.View.Background := Background_Grid_Lines;
         end if;

         View.View.Grid_Style := Gtk_New
           (Stroke     => (0.9, 0.9, 0.9, 0.5),   --  the grid color
            Line_Width => 1.0,                    --  the grid line width
            Fill       => Create_Rgba_Pattern
              (Browsers_Bg_Color.Get_Pref));  --  the background color
      else
         Configure
           (View.Canvas,
            Annotation_Font => Annotation_Font,
            Grid_Size       =>
              (if Draw_Grid then Gtkada.Canvas.Default_Grid_Size else 0),
            Background      => Browsers_Bg_Color.Get_Pref);
      end if;

      Free (Annotation_Font);

      if View.Use_Canvas_View then
         View.View.Queue_Draw;
      else
         Refresh_Canvas (Get_Canvas (View));
      end if;
   end Toggle_Draw_Grid;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Zoom_In_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      B : constant General_Browser := Browser_From_Context (Context.Context);
      Z : Gdouble;
   begin
      if B.Use_Canvas_View then
         Z := B.View.Get_Scale;
      else
         Z := Get_Zoom (B.Canvas);
      end if;

      for J in Zoom_Levels'First .. Zoom_Levels'Last - 1 loop
         if Zoom_Levels (J) <= Z
           and then Z < Zoom_Levels (J + 1)
         then
            if B.Use_Canvas_View then
               B.View.Set_Scale (Zoom_Levels (J + 1));
            else
               Zoom (B.Canvas, Zoom_Levels (J + 1), Zoom_Duration);
            end if;
         end if;
      end loop;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Zoom_Out_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      B : constant General_Browser := Browser_From_Context (Context.Context);
      Z : Gdouble;
   begin
      if B.Use_Canvas_View then
         Z := B.View.Get_Scale;
      else
         Z := Get_Zoom (B.Canvas);
      end if;

      for J in Zoom_Levels'First + 1 .. Zoom_Levels'Last loop
         if Zoom_Levels (J - 1) < Z
           and then Z <= Zoom_Levels (J)
         then
            if B.Use_Canvas_View then
               B.View.Set_Scale (Zoom_Levels (J - 1));
            else
               Zoom (B.Canvas, Zoom_Levels (J - 1), Zoom_Duration);
            end if;
         end if;
      end loop;
      return Commands.Success;
   end Execute;

   ----------------
   -- Zoom_Level --
   ----------------

   procedure Zoom_Level
     (Item : access Gtk_Widget_Record'Class; Data : Cb_Data)
   is
      pragma Unreferenced (Item);
   begin
      if Data.Browser.Use_Canvas_View then
         Data.Browser.View.Set_Scale (Data.Zoom);
      else
         Zoom (Data.Browser.Canvas, Data.Zoom, 0.0);
      end if;
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
   -- Draw_Link --
   ---------------

   overriding procedure Draw_Link
     (Canvas      : access Interactive_Canvas_Record'Class;
      Link        : access Browser_Link_Record;
      Cr          : Cairo_Context;
      Edge_Number : Glib.Gint;
      Show_Annotation : Boolean := True)
   is
   begin
      if not Show_Annotation then
         Set_Source_Color (Cr, Black_RGBA);
         Draw_Link (Canvas, Canvas_Link_Access (Link), Cr, Edge_Number, False);

         return;
      end if;

      if not Browser_Item (Get_Src (Link)).Hide_Links
        and then not Browser_Item (Get_Dest (Link)).Hide_Links
      then
         if not Is_Selected
           (Canvas, Gtkada.Canvas.Canvas_Item (Get_Src (Link)))
           and then not Is_Selected
             (Canvas, Gtkada.Canvas.Canvas_Item (Get_Dest (Link)))
         then
            Set_Source_Color (Cr, Black_RGBA);
         else
            Set_Source_Color (Cr, Selected_Link_Color.Get_Pref);
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
              (Item.Title_Layout, Preferences.Default_Font.Get_Pref_Font);
         else
            Set_Text (Item.Title_Layout, Title);
         end if;
      end if;
   end Set_Title;

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

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   procedure Resize_And_Draw
     (Item             : access Browser_Item_Record;
      Cr               : Cairo_Context;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class)
   is
      pragma Unreferenced (Layout, Width, Height);
   begin
      Cairo.Rectangle
        (Cr,
         Gdouble (Xoffset),
         Gdouble (Item.Title_Coord.Y + Item.Title_Coord.Height + Yoffset),
         Gdouble (Item.Get_Coord.Width - Width_Offset),
         Gdouble (Item.Get_Coord.Height - Item.Title_Coord.Height
           - Item.Title_Coord.Y - Height_Offset));

      Set_Source_Color (Cr, White_RGBA);
      Fill_Preserve (Cr);

      Set_Line_Width (Cr, 1.0);
      Set_Source_Color (Cr, Shade (White_RGBA, 0.3));
      Stroke (Cr);

      if Item.Title_Layout /= null then
         Yoffset := Yoffset + Item.Title_Coord.Height + 1;
      end if;
   end Resize_And_Draw;

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
      pragma Unreferenced (Item, Is_Selected);
      --  Call Highlight on Item and all its siblings. If the selection status
      --  has changed, this will result in a change of background color for
      --  these items.
   begin
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
      Draw (Browser_Item_Record'Class (Item.all)'Access, Cr);
   end Draw_Selected;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Item : in out Browser_Item_Record) is
   begin
      Destroy (Gtkada.Canvas.Canvas_Item_Record (Item));
   end Destroy;

   ------------------
   -- Compute_Size --
   ------------------

   procedure Compute_Size
     (Item   : not null access Browser_Item_Record;
      Layout : not null access Pango.Layout.Pango_Layout_Record'Class;
      Width, Height : out Glib.Gint;
      Title_Box     : in out Cairo.Region.Cairo_Rectangle_Int)
   is
      pragma Unreferenced (Item, Layout, Title_Box);
   begin
      Width := 0;
      Height := 0;
   end Compute_Size;

   --------------------
   -- Recompute_Size --
   --------------------

   procedure Recompute_Size
     (Item   : not null access Browser_Item_Record'Class)
   is
      Num_Buttons   : constant Gint := 1 + Get_Last_Button_Number
        (Browser_Item (Item));  --  dispatching call
      Button_Width  : constant Gint := Get_Width (Item.Browser.Close_Pixmap);
      Button_Height : constant Gint := Get_Height (Item.Browser.Close_Pixmap);

      Layout : Pango_Layout;
      Width, Height  : Gint;
      W, H     : Gint := 0;
      Border : Gtk.Style.Gtk_Border;
      Layout_H : Gint := 0;

   begin
      Layout := Create_Pango_Layout (Get_Browser (Item), "");
      Set_Font_Description (Layout, Preferences.Default_Font.Get_Pref_Font);

      Get_Style_Context (Get_Browser (Item)).Get_Border
        (Gtk_State_Flag_Normal, Border);

      if Item.Title_Layout /= null then
         Get_Pixel_Size (Item.Title_Layout, W, Layout_H);
         W := W + 2 * Margin + Num_Buttons * (Margin + Button_Width);
         Item.Title_Coord :=
           (X      => Gint (Border.Right),
            Y      => Gint (Border.Top),
            Width  => W - Gint (Border.Left + Border.Right),
            Height =>
              Gint'Max
                (Layout_H, Button_Height + Gint (Border.Top + Border.Bottom)));
      else
         Item.Title_Coord := (others => 0);
      end if;

      Compute_Size (Item, Layout, Width, Height, Item.Title_Coord);
      Unref (Layout);

      H := Item.Title_Coord.Height + 1 + Height +
        Gint (Border.Top + Border.Bottom);

      Set_Screen_Size (Browser_Item (Item), Width, H);
   end Recompute_Size;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Item : access Browser_Item_Record;
      Cr   : Cairo.Cairo_Context)
   is
      Xoffset, Yoffset : Gint := 0;
      Layout           : Pango_Layout;
   begin
      Layout := Create_Pango_Layout (Get_Browser (Item), "");
      Set_Font_Description (Layout, Preferences.Default_Font.Get_Pref_Font);

      Resize_And_Draw
        (Browser_Item_Record'Class (Item.all)'Access,
         Cr, 0, 0, 0, 0, Xoffset, Yoffset, Layout);

      Unref (Layout);
   end Draw;

   --------------------------
   -- Refresh_Linked_Items --
   --------------------------

   procedure Refresh_Linked_Items
     (Item             : access Browser_Item_Record'Class;
      Refresh_Parents  : Boolean := False;
      Refresh_Children : Boolean := False)
   is
      pragma Unreferenced (Refresh_Parents, Refresh_Children);
   begin
      Refresh_Canvas (Get_Canvas (Get_Browser (Item)));
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

   --------------
   -- Distance --
   --------------

   overriding function Distance
     (Left  : access Browser_Marker_Record;
      Right : access Location_Marker_Record'Class) return Integer
   is
      pragma Unreferenced (Left, Right);
   begin
      return Integer'Last;
   end Distance;

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
      Filter : constant Action_Filter := new Is_In_Browser;
   begin
      Register_Action
        (Kernel, "browser select all", new Select_All_Command,
         -"Select all items in a browser",
         Stock_Id => Stock_Select_All,
         Filter   => Filter,
         Category => "Browsers");

      Register_Action
        (Kernel, "browser zoom out", new Zoom_Out_Command,
         -"Zoom out",
         Stock_Id  => Stock_Zoom_Out,
         Category  => -"Browsers",
         Filter    => Filter);

      Register_Action
        (Kernel, "browser zoom in", new Zoom_In_Command,
         -"Zoom in",
         Stock_Id  => Stock_Zoom_In,
         Category  => -"Browsers",
         Filter    => Filter);

      Register_Action
        (Kernel, "browser toggle links", new Toggle_Links,
         -"Toggle display of links for the selected items",
         Stock_Id => GPS_Toggle_Links,
         Filter   => Filter,
         Category => -"Browsers");

      Register_Action
        (Kernel, "browser refresh", new Refresh_Command,
         -"Refresh layout",
         Stock_Id => GPS_Refresh,
         Filter   => Filter,
         Category => -"Browsers");

      Register_Action
        (Kernel, "browser clear", new Clear_Command,
         -"Clear the contents of the browser",
         Stock_Id => Stock_Clear,
         Filter   => Filter,
         Category => -"Browsers");

      Register_Action
        (Kernel, "browser remove unselected", new Remove_Unselected_Command,
         -"Remove unselected items",
         Stock_Id => GPS_Remove_Unselected,
         Filter   => Filter,
         Category => -"Browsers");

      Register_Action
        (Kernel, "browser remove selected", new Remove_Selected_Command,
         -"Remove selected items",
         Stock_Id => Stock_Remove,
         Filter   => Filter,
         Category => -"Browsers");
   end Register_Actions;

   --------------------------
   -- Browser_From_Context --
   --------------------------

   function Browser_From_Context
     (Context : Selection_Context) return General_Browser
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Child  : constant MDI_Child := Get_Focus_Child (Get_MDI (Kernel));
      W      : Gtk_Widget;
   begin
      if Child /= null
        and then Child.all in MDI_Child_With_Local_Toolbar'Class
      then
         W := MDI_Child_With_Local_Toolbar_Access (Child).Get_Actual_Widget;
         if W.all in General_Browser_Record'Class then
            return General_Browser (W);
         end if;
      end if;

      return null;
   end Browser_From_Context;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Select_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      B : constant General_Browser := Browser_From_Context (Context.Context);

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         B.View.Model.Add_To_Selection (Item);
      end On_Item;

   begin
      if B.Use_Canvas_View then
         B.View.Model.For_Each_Item (On_Item'Access, Filter => Kind_Item);
      else
         Select_All (B.Canvas);
      end if;
      return Commands.Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_In_Browser;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Browser_From_Context (Context) /= null;
   end Filter_Matches_Primitive;

   --------------------
   -- Setup_Titlebar --
   --------------------

   procedure Setup_Titlebar
     (Item    : not null access GPS_Item_Record'Class;
      Browser : not null access General_Browser_Record'Class;
      Name    : String;
      Left    : access Left_Arrow_Record'Class := null;
      Right   : access Right_Arrow_Record'Class := null;
      Buttons : Button_Array := No_Buttons)
   is
      Text   : Text_Item;
      Title  : Rect_Item;
      Styles : constant access Browser_Styles := Browser.Get_View.Get_Styles;
   begin
      Title := Gtk_New_Rect (Styles.Title);
      Title.Set_Child_Layout (Horizontal_Stack);
      Item.Add_Child (Title);

      if Left /= null then
         Item.Left := Abstract_Item (Left);
         Initialize (Left);
         Title.Add_Child (Left);
      end if;

      Text := Gtk_New_Text (Styles.Title_Font, Name);
      Title.Add_Child (Text, Margin => (2.0, 10.0, 0.0, 10.0));

      if Right /= null then
         Item.Right := Abstract_Item (Right);
         Initialize (Right);
         Title.Add_Child (Right);
      end if;

      for B in reverse Buttons'Range loop
         Title.Add_Child
           (Buttons (B), Align => Align_Center, Pack_End => True);
      end loop;
   end Setup_Titlebar;

   ---------------------
   -- Show_Left_Arrow --
   ---------------------

   procedure Show_Left_Arrow (Self : not null access GPS_Item_Record) is
   begin
      Self.Left.Show;
   end Show_Left_Arrow;

   ----------------------
   -- Show_Right_Arrow --
   ----------------------

   procedure Show_Right_Arrow (Self : not null access GPS_Item_Record) is
   begin
      Self.Right.Show;
   end Show_Right_Arrow;

   ---------------------
   -- Hide_Left_Arrow --
   ---------------------

   procedure Hide_Left_Arrow (Self : not null access GPS_Item_Record) is
   begin
      Self.Left.Hide;
   end Hide_Left_Arrow;

   ----------------------
   -- Hide_Right_Arrow --
   ----------------------

   procedure Hide_Right_Arrow (Self : not null access GPS_Item_Record) is
   begin
      Self.Right.Hide;
   end Hide_Right_Arrow;

   -------------------
   -- Is_Selectable --
   -------------------

   overriding function Is_Selectable
     (Self : not null access Browser_Model_Record;
      Item : not null access Abstract_Item_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return Item.all not in Gtkada.Canvas_View.Canvas_Link_Record'Class;
   end Is_Selectable;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : not null access GPS_Item_Record; Context : Draw_Context)
   is
   begin
      Save (Context.Cr);
      Rect_Item_Record (Self.all).Draw (Context);  --  inherited
      Restore (Context.Cr);

      case Self.Outline is
         when Outline_None =>
            null;

         when Outline_As_Linked =>
            Self.Draw_Outline
              (Self.Browser.Get_View.Styles.Highlight, Context);

         when Outline_As_Match =>
            Self.Draw_Outline
              (Self.Browser.Get_View.Styles.Search, Context);
      end case;
   end Draw;

end Browsers.Canvas;
