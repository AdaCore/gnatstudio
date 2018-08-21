------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with GNAT.Strings;                      use GNAT.Strings;
with GNATCOLL.VFS;                      use GNATCOLL.VFS;
with System.Address_Image;

with Glib;                              use Glib;
with Glib.Main;                         use Glib.Main;
with Glib.Object;                       use Glib.Object;

with Cairo;                             use Cairo;

with Pango.Enums;                       use Pango.Enums;
with Pango.Font;                        use Pango.Font;

with Gdk;                               use Gdk;
with Gdk.Event;                         use Gdk.Event;
with Gdk.RGBA;                          use Gdk.RGBA;
with Gdk.Window;                        use Gdk.Window;

with Gtk.Box;                           use Gtk.Box;
with Gtk.Enums;                         use Gtk.Enums;
with Gtk.Handlers;                      use Gtk.Handlers;
with Gtk.Image;                         use Gtk.Image;
with Gtk.Menu;                          use Gtk.Menu;
with Gtk.Menu_Item;                     use Gtk.Menu_Item;
with Gtk.Menu_Tool_Button;              use Gtk.Menu_Tool_Button;
with Gtk.Scrolled_Window;               use Gtk.Scrolled_Window;
with Gtk.Style_Context;                 use Gtk.Style_Context;
with Gtk.Toolbar;                       use Gtk.Toolbar;
with Gtk.Tool_Button;                   use Gtk.Tool_Button;
with Gtk.Widget;                        use Gtk.Widget;

with Gtkada.Canvas_View;                use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Views;          use Gtkada.Canvas_View.Views;
with Gtkada.Canvas_View.Models.Layers;  use Gtkada.Canvas_View.Models.Layers;
with Gtkada.File_Selector;              use Gtkada.File_Selector;
with Gtkada.Handlers;                   use Gtkada.Handlers;
with Gtkada.MDI;                        use Gtkada.MDI;
with Gtkada.Style;                      use Gtkada.Style;

with Commands;                          use Commands;
with Commands.Interactive;              use Commands.Interactive;
with Generic_Views;                     use Generic_Views;
with GPS.Core_Kernels;                  use GPS.Core_Kernels;
with GPS.Intl;                          use GPS.Intl;
with GPS.Kernel;                        use GPS.Kernel;
with GPS.Kernel.Actions;                use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;               use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                  use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;            use GPS.Kernel.Preferences;
with GPS.Kernel.Modules.UI;             use GPS.Kernel.Modules.UI;
with GPS.Markers;                       use GPS.Markers;
with GPS.Stock_Icons;                   use GPS.Stock_Icons;
with Histories;                         use Histories;
with XML_Utils;                         use XML_Utils;

package body Browsers.Canvas is

   Zoom_Levels : constant array (Positive range <>) of Gdouble :=
                   (0.125, 0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0);
   --  All the possible zoom levels. We have to use such an array, instead
   --  of doing the computation directly, so as to avoid rounding errors that
   --  would appear in the computation and make zoom_in not the reverse of
   --  zoom_out.

   Align_On_Grid      : Boolean_Preference;
   Draw_Grid          : Boolean_Preference;
   Add_Waypoints      : Boolean_Preference;
   Vertical           : Boolean_Preference;

   type Export_Idle_Data is record
      Browser : General_Browser;
      Format  : Gtkada.Canvas_View.Export_Format;
   end record;

   package Export_Idle is
     new Glib.Main.Generic_Sources (Export_Idle_Data);

   type Cb_Data is record
      Browser       : General_Browser;
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

   type Is_Writable_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Writable_Filter;
      Context : Selection_Context) return Boolean;
   --  Whether the browser is writable

   type Toggle_Links is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Toggle_Links;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Toggle the display of links for the item

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

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      Browser : General_Browser;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);
   --  Called when the preferences have changed

   procedure On_Selection_Changed
     (Self  : not null access GObject_Record'Class;
      Item  : Abstract_Item);
   --  Called when the selection changes. This highlights links to and from
   --  that item.

   procedure Request_Context_Changed
      (Self : access Gtk_Widget_Record'Class);
   --  Invalid the current context in the kernel, and ask it to recompute

   -------------
   -- Markers --
   -------------

   type Browser_Marker_Data is new Location_Marker_Data with record
      Title  : GNAT.Strings.String_Access;
      Kernel : not null access Kernel_Handle_Record'Class;
   end record;
   overriding function Go_To
     (Marker : not null access Browser_Marker_Data) return Boolean;
   overriding procedure Destroy (Marker : in out Browser_Marker_Data);
   overriding function To_String
     (Marker : not null access Browser_Marker_Data) return String;
   overriding function Save
     (Marker : not null access Browser_Marker_Data) return XML_Utils.Node_Ptr;
   overriding function Similar
     (Left        : not null access Browser_Marker_Data;
      Dummy_Right : not null access Location_Marker_Data'Class) return Boolean
     is (False);
   overriding function Distance
     (Left        : not null access Browser_Marker_Data;
      Dummy_Right : not null access Location_Marker_Data'Class) return Integer
     is (Integer'Last);
   --  See inherited documentation

   function Create_Browser_Marker
     (Kernel       : not null access Kernel_Handle_Record'Class;
      Browser_Name : String) return Location_Marker;
   --  Create a new marker that will bring the user back to the browser

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
     (Self  : not null access GObject_Record'Class;
      Item  : Abstract_Item)
   is
   begin
      Highlight_Related_Items (General_Browser (Self).Get_View, Item);
      General_Browser (Self).Kernel.Refresh_Context;
   end On_Selection_Changed;

   -----------------------------
   -- Request_Context_Changed --
   -----------------------------

   procedure Request_Context_Changed
      (Self : access Gtk_Widget_Record'Class) is
   begin
      General_Browser (Self).Kernel.Refresh_Context;
   end Request_Context_Changed;

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
     (Browser         : access General_Browser_Record'Class)
   is
      Hook     : access On_Pref_Changed;
      Scrolled : Gtk_Scrolled_Window;
      Id       : Handler_Id;
      pragma Unreferenced (Id);
   begin
      Gtk.Box.Initialize_Vbox (Browser, Homogeneous => False);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Pack_Start (Browser, Scrolled, Expand => True, Fill => True);

      List_Rtree.Gtk_New (Browser.Model);
      Browser.Model.Set_Selection_Mode (Selection_Multiple);

      Browsers.Gtk_New (Browser.View, Browser.Model);
      Scrolled.Add (Browser.View);

      Id := Browser.Get_View.Model.On_Selection_Changed
        (On_Selection_Changed'Access, Browser);

      --  Refresh the kernel contexts in a number of cases. In particular,
      --  this flushes the cache for the filters, for instance if a filter
      --  tests which items are visible, or which zoom level we have,...
      Widget_Callback.Object_Connect
         (Browser.Get_View, Signal_Viewport_Changed,
          Request_Context_Changed'Access, Slot_Object => Browser);
      Widget_Callback.Object_Connect
         (Browser.Get_View, Signal_Inline_Editing_Started,
          Request_Context_Changed'Access, Slot_Object => Browser);
      Widget_Callback.Object_Connect
         (Browser.Get_View, Signal_Inline_Editing_Finished,
          Request_Context_Changed'Access, Slot_Object => Browser);
      Widget_Callback.Object_Connect
         (Browser.Get_View.Model, Signal_Item_Destroyed,
          Request_Context_Changed'Access, Slot_Object => Browser);

      Align_On_Grid := Browser.Kernel.Get_Preferences.Create_Invisible_Pref
        ("browsers-align-on-grid", True, Label => -"Align On Grid");
      Draw_Grid := Browser.Kernel.Get_Preferences.Create_Invisible_Pref
        ("browsers-display-grid", False, Label => -"Draw grid",
         Doc => -"Draw a grid on background.");
      Vertical := Browser.Kernel.Get_Preferences.Create_Invisible_Pref
        ("browsers-vertical", False, Label => -"Vertical layout",
         Doc => -("General orientation of the layout: from left"
           & " to right, or from top to bottom."));
      Add_Waypoints := Browser.Kernel.Get_Preferences.Create_Invisible_Pref
        ("browsers-add-waypoints", False, Label => -"Use waypoints",
         Doc =>
           (-("Insert waypoints in long edges when performing the"
            & " layout of the graph. This might result in less edge crossings"
            & " but is sometimes harder to use interactively.")));

      --  Set css style for scrollbars
      Get_Style_Context
        (Scrolled.Get_Vscrollbar).Add_Class ("gps_browser_decoration");
      Get_Style_Context
        (Scrolled.Get_Hscrollbar).Add_Class ("gps_browser_decoration");

      Hook := new On_Pref_Changed;
      Hook.Browser := General_Browser (Browser);
      Preferences_Changed_Hook.Add (Hook, Watch => Browser);
      Hook.Execute (Browser.Kernel, Pref => null);

      Change_Align_On_Grid (Browser);
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference)
   is
      pragma Unreferenced (Kernel);
      B    : constant General_Browser := Self.Browser;
   begin
      Create_Styles (B.View);

      if Pref = null
        or else Pref = Preference (Browsers_Bg_Color)
      then
         B.Override_Background_Color
           (Gtk_State_Flag_Normal, Browsers_Bg_Color.Get_Pref);
      end if;

      if Pref = null
        or else Pref = Preference (Draw_Grid)
      then
         Toggle_Draw_Grid (B);
      end if;

      if Pref = null
        or else Pref = Preference (Align_On_Grid)
      then
         Change_Align_On_Grid (B);
      end if;

      if Pref = null
        or else Pref = Preference (Vertical)
        or else Pref = Preference (Add_Waypoints)
      then
         Force_Refresh (B);
      end if;

      B.Preferences_Changed (Pref);
   end Execute;

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
      Menu       : Gtk_Menu_Tool_Button;
      Zooms_Menu : Gtk_Menu;
      Export_Menu  : Gtk_Menu;
      Mitem      : Gtk_Menu_Item;
      Image      : Gtk_Image;
   begin
      Gtk_New (Zooms_Menu);

      for J in Zoom_Levels'Range loop
         if Zoom_Levels (J) = 0.0 then
            Gtk_New (Mitem, Label => "Fit");
         else
            Gtk_New
              (Mitem,
               Label => Guint'Image (Guint (Zoom_Levels (J) * 100.0)) & '%');
         end if;
         Zooms_Menu.Append (Mitem);
         Contextual_Cb.Connect
           (Mitem, Gtk.Menu_Item.Signal_Activate, Zoom_Level'Access,
            (Browser => General_Browser (View),
             Keep_Selected => True,
             Zoom    => Zoom_Levels (J)));
      end loop;

      Gtk_New_From_Icon_Name
         (Image, "gps-zoom-100-symbolic", Icon_Size_Local_Toolbar);
      Gtk.Menu_Tool_Button.Gtk_New (Menu, Gtk_Widget (Image), "100%");
      Menu.Set_Tooltip_Text (-"Reset zoom level (or Alt-mousewheel)");
      Menu.Set_Menu (Zooms_Menu);
      Zooms_Menu.Show_All;
      Toolbar.Insert (Menu, Get_Toolbar_Section (Toolbar, "zoom"));
      Contextual_Cb.Connect
        (Menu, Gtk.Tool_Button.Signal_Clicked, Zoom_Level'Access,
         (Browser => General_Browser (View),
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

      Gtk_New_From_Icon_Name
         (Image, "gps-save-symbolic", Icon_Size_Local_Toolbar);
      Gtk_New (Menu, Gtk_Widget (Image), "save");
      Menu.Set_Tooltip_Text (-"Export to...");
      Menu.Set_Menu (Export_Menu);
      Export_Menu.Show_All;
      Toolbar.Insert (Menu, Get_Toolbar_Section (Toolbar, "export"));
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
      K     : constant Kernel_Handle := View.Kernel;
   begin
      Append_Menu (Menu, K, Align_On_Grid);
      Append_Menu (Menu, K, Draw_Grid);
      Append_Menu (Menu, K, Vertical);
      Append_Menu (Menu, K, Add_Waypoints);
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

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access Browser_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return GPS.Kernel.Selection_Context
   is
      B : constant General_Browser := General_Browser
        (GPS_MDI_Child (Self).Get_Actual_Widget);
      Details : Canvas_Event_Details;
      Context : Selection_Context;
      Done    : Boolean := False;

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         if not Done and then B.Model.Is_Selected (Item) then
            GPS_Item (Item).Set_Context (Context);
            Done := True;
         end if;
      end On_Item;

   begin
      Context := GPS_MDI_Child_Record (Self.all).Build_Context (Event);

      if Event = null
        or else Get_Event_Type (Event) not in Button_Press .. Button_Release
      then
         B.View.Initialize_Details (Details);
         Set_Browser_Information (Context, Details);

         --  Look at the current selection
         B.Model.For_Each_Item (On_Item'Access, Filter => Kind_Item);

      else
         B.View.Set_Details (Details, Event.Button);
         Set_Browser_Information (Context, Details);
         if Details.Toplevel_Item /= null
            and then Details.Toplevel_Item.all in GPS_Item_Record'Class
         then
            GPS_Item (Details.Toplevel_Item).Set_Context (Context);
         end if;
      end if;
      return Context;
   end Build_Context;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Remove_Selected_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      B : constant General_Browser := Browser_From_Context (Context.Context);
      To_Remove : Item_Sets.Set;

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         if B.Model.Is_Selected (Item) then
            B.Model.Include_Related_Items (Item, To_Remove);
         end if;
      end On_Item;

   begin
      if not B.Get_View.Is_Read_Only then
         B.Model.For_Each_Item (On_Item'Access, Filter => Kind_Item);
         B.Model.Remove (To_Remove);
         B.Model.Refresh_Layout;
         B.View.Queue_Draw;
         return Commands.Success;
      end if;
      return Commands.Failure;
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
      To_Remove : Item_Sets.Set;

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         if not B.Model.Is_Selected (Item) then
            B.Model.Include_Related_Items (Item, To_Remove);
         end if;
      end On_Item;

   begin
      if not B.Get_View.Is_Read_Only then
         B.Model.For_Each_Item (On_Item'Access, Filter => Kind_Item);
         B.Model.Remove (To_Remove);
         B.Model.Refresh_Layout;
         B.View.Queue_Draw;
         return Commands.Success;
      end if;
      return Commands.Failure;
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
      B.Model.For_Each_Item
        (On_Item'Access, Filter => Kind_Item, Selected_Only => True);
      B.View.Model.For_Each_Link (On_Link'Access, From_Or_To => S);
      B.View.Queue_Draw;
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
         Success : Boolean;
      begin
         if Name /= GNATCOLL.VFS.No_File then
            Success := Data.Browser.View.Export
              (Filename          => Name.Display_Full_Name,
               Page              => A4_Landscape,
               Format            => Data.Format,
               Visible_Area_Only => True);

            if not Success then
               Kernel.Insert
                 ("Cannot create " & Name.Display_Full_Name,
                  Mode => GPS.Kernel.Error);
            end if;
         end if;
      end;

      return False;
   end On_Export_Idle;

   ----------------------
   -- On_Export_To_PNG --
   ----------------------

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
     (Self : not null access General_Browser_Record) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return not Vertical.Get_Pref;
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
      if not Self.Get_View.Is_Read_Only then
         --  Recompute the size of all boxes
         Self.View.Model.Refresh_Layout;

         --  Now compute the position of the boxes.

         Gtkada.Canvas_View.Models.Layers.Layout
           (Self.View.Model,
            View                 => Self.View,  --  animate
            Horizontal           => Self.Horizontal_Layout,
            Add_Waypoints        => Add_Waypoints.Get_Pref,
            Space_Between_Items  => Space_Between_Items,
            Space_Between_Layers => Space_Between_Layers);
      end if;

      if Rescale then
         Self.View.Scale_To_Fit
           (Min_Scale => 0.5, Max_Scale => 2.0, Duration => 0.8);
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
      if not B.Get_View.Is_Read_Only then
         Refresh_Layout (B, Rescale => True);
      end if;
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
      if not B.Get_View.Is_Read_Only then
         Browser_Model (B.View.Model).Clear;
         Browser_Model (B.View.Model).Refresh_Layout;
      end if;
      return Commands.Success;
   end Execute;

   --------------------------
   -- Change_Align_On_Grid --
   --------------------------

   procedure Change_Align_On_Grid (Browser : access Gtk_Widget_Record'Class) is
      View  : constant General_Browser := General_Browser (Browser);
      Align : constant Boolean := Align_On_Grid.Get_Pref;
   begin
      View.View.Set_Snap
        (Snap_To_Grid   => Align,
         Snap_To_Guides => False);
   end Change_Align_On_Grid;

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
      Grid : constant Boolean := Draw_Grid.Get_Pref;
      Annotation_Font : Pango_Font_Description;
   begin
      Annotation_Font := Copy (Preferences.Default_Font.Get_Pref);
      Set_Size
        (Annotation_Font,
         Gint'Max (Pango_Scale, Get_Size (Annotation_Font) - 2 * Pango_Scale));

      View.View.Set_Grid_Size (Size => 15.0);

      if not Grid then
         View.View.Background := Background_Color;
      else
         View.View.Background := Background_Grid_Lines;
      end if;

      View.View.Grid_Style := Gtk_New
        (Stroke     => (0.9, 0.9, 0.9, 0.5),   --  the grid color
         Line_Width => 1.0);                   --  the grid line width

      Free (Annotation_Font);

      View.View.Queue_Draw;
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
      Z := B.View.Get_Scale;

      for J in Zoom_Levels'First .. Zoom_Levels'Last - 1 loop
         if Zoom_Levels (J) <= Z
           and then Z < Zoom_Levels (J + 1)
         then
            B.View.Scale_To_Fit
               (Min_Scale => Zoom_Levels (J + 1),
                Max_Scale => Zoom_Levels (J + 1));
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
      Z := B.View.Get_Scale;

      for J in Zoom_Levels'First + 1 .. Zoom_Levels'Last loop
         if Zoom_Levels (J - 1) < Z
           and then Z <= Zoom_Levels (J)
         then
            B.View.Scale_To_Fit
               (Min_Scale => Zoom_Levels (J - 1),
                Max_Scale => Zoom_Levels (J - 1));
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
      if Data.Zoom = 0.0 then
         Data.Browser.View.Scale_To_Fit (Duration => 0.4);
      else
         Animate_Scale
            (View        => Data.Browser.View,
             Final_Scale => Data.Zoom).Start (Data.Browser.View);
         --  Data.Browser.View.Set_Scale (Data.Zoom);
      end if;
   end Zoom_Level;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel (Browser : access General_Browser_Record)
      return GPS.Kernel.Kernel_Handle is
   begin
      return Browser.Kernel;
   end Get_Kernel;

   -----------------------------
   -- Add_Navigation_Location --
   -----------------------------

   procedure Add_Navigation_Location
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Title  : String) is
   begin
      Push_Marker_In_History
        (Kernel, Create_Browser_Marker (Kernel, Browser_Name => Title));
   end Add_Navigation_Location;

   -----------
   -- Go_To --
   -----------

   overriding function Go_To
     (Marker : not null access Browser_Marker_Data) return Boolean
   is
      Child : constant MDI_Child := Find_MDI_Child_By_Name
        (Get_MDI (Marker.Kernel), Marker.Title.all);
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

   overriding procedure Destroy (Marker : in out Browser_Marker_Data) is
   begin
      Free (Marker.Title);
   end Destroy;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Marker : not null access Browser_Marker_Data) return String is
   begin
      return "Browser: " & Marker.Title.all;
   end To_String;

   ----------
   -- Save --
   ----------

   overriding function Save
     (Marker : not null access Browser_Marker_Data) return XML_Utils.Node_Ptr
   is
      N : constant Node_Ptr := new Node;
   begin
      N.Tag   := new String'("browser_marker");
      N.Value := new String'(Marker.Title.all);
      return N;
   end Save;

   ---------------------------
   -- Create_Browser_Marker --
   ---------------------------

   function Create_Browser_Marker
     (Kernel       : not null access Kernel_Handle_Record'Class;
      Browser_Name : String) return Location_Marker is
   begin
      return L : Location_Marker do
         L.Set (Browser_Marker_Data'
                  (Kernel => Kernel,
                   Title  => new String'(Browser_Name)));
      end return;
   end Create_Browser_Marker;

   ----------------------
   -- Register_Actions --
   ----------------------

   procedure Register_Actions (Kernel : access Kernel_Handle_Record'Class) is
      Is_Writable : constant Action_Filter := new Is_Writable_Filter;
   begin
      Register_Action
        (Kernel, "browser select all", new Select_All_Command,
         -"Select all items in a browser",
         Icon_Name => "gps-select-all-symbolic",
         Category => "Browsers");

      Register_Action
        (Kernel, "browser zoom out", new Zoom_Out_Command,
         -"Zoom out",
         Icon_Name                => "gps-zoom-out-symbolic",
         Category                 => -"Browsers",
         Shortcut_Active_For_View => Browser_Child_Record'Tag);

      Register_Action
        (Kernel, "browser zoom in", new Zoom_In_Command,
         -"Zoom in",
         Icon_Name                => "gps-zoom-in-symbolic",
         Category                 => -"Browsers",
         Shortcut_Active_For_View => Browser_Child_Record'Tag);

      Register_Action
        (Kernel, "browser toggle links", new Toggle_Links,
         -"Toggle display of links for the selected items",
         Icon_Name => "gps-toggle-links-symbolic",
         Category => -"Browsers");

      Register_Action
        (Kernel, "browser refresh", new Refresh_Command,
         -"Refresh layout",
         Icon_Name => "gps-refresh-symbolic",
         Filter   => Is_Writable,
         Category => -"Browsers");

      Register_Action
        (Kernel, "browser clear", new Clear_Command,
         -"Clear the contents of the browser",
         Icon_Name => "gps-clear-symbolic",
         Filter   => Is_Writable,
         Category => -"Browsers");

      Register_Action
        (Kernel, "browser remove unselected", new Remove_Unselected_Command,
         -"Remove unselected items",
         Icon_Name => "gps-remove-unselected-symbolic",
         Filter   => Is_Writable,
         Category => -"Browsers");

      Register_Action
        (Kernel, "browser remove selected", new Remove_Selected_Command,
         -"Remove selected items",
         Icon_Name => "gps-remove-symbolic",
         Filter   => Is_Writable,
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
      if Child /= null then
         if Child.all in GPS_MDI_Child_Record'Class then
            W := GPS_MDI_Child (Child).Get_Actual_Widget;
         else
            W := Child.Get_Widget;
         end if;

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
      B.View.Model.For_Each_Item (On_Item'Access, Filter => Kind_Item);
      return Commands.Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Writable_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      B : constant General_Browser := Browser_From_Context (Context);
   begin
      return Get_Creator (Context) /= null
        and then B /= null
        and then not B.Get_View.Read_Only;
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
         Title.Add_Child
           (Left, Align => Align_Center, Margin => (0.0, 0.0, 0.0, 4.0));
      end if;

      Text := Gtk_New_Text (Styles.Title_Font, Name);
      Title.Add_Child (Text, Margin => (2.0, 6.0, 0.0, 6.0));

      if Right /= null then
         Item.Right := Abstract_Item (Right);
         Initialize (Right);
         Title.Add_Child
           (Right, Align => Align_Center, Pack_End => True,
            Margin => (0.0, 4.0, 0.0, 0.0));
      end if;

      for B in reverse Buttons'Range loop
         Title.Add_Child
           (Buttons (B), Align => Align_Center, Pack_End => True,
            Margin => (0.0, 2.0, 0.0, 0.0));
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

   --------------------
   -- Set_Left_Arrow --
   --------------------

   procedure Set_Left_Arrow
     (Self : not null access GPS_Item_Record; Text : Glib.UTF8_String) is
   begin
      Gtkada.Canvas_View.Text_Item (Self.Left).Set_Text (Text);
   end Set_Left_Arrow;

   ---------------------
   -- Set_Right_Arrow --
   ---------------------

   procedure Set_Right_Arrow
     (Self : not null access GPS_Item_Record; Text : Glib.UTF8_String) is
   begin
      Gtkada.Canvas_View.Text_Item (Self.Right).Set_Text (Text);
   end Set_Right_Arrow;

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

   -----------------
   -- Save_To_XML --
   -----------------

   overriding procedure Save_To_XML
     (View : access General_Browser_Record;
      XML  : in out XML_Utils.Node_Ptr)
   is
      procedure On_Item (It : not null access Abstract_Item_Record'Class);
      procedure On_Link (It : not null access Abstract_Item_Record'Class);

      procedure On_Item (It : not null access Abstract_Item_Record'Class) is
         E    : constant GPS_Item := GPS_Item (It);
         N    : constant Node_Ptr := E.Save_To_XML;
         Pos  : constant Point := It.Position;
      begin
         if N /= null then
            Set_Attribute (N, "id", System.Address_Image (E.all'Address));
            Set_Attribute (N, "x", Pos.X'Img);
            Set_Attribute (N, "y", Pos.Y'Img);
            Add_Child (XML, N);
         end if;
      end On_Item;

      procedure On_Link (It : not null access Abstract_Item_Record'Class) is
         L : constant GPS_Link := GPS_Link (It);
         N : Node_Ptr;
      begin
         N := new Node;
         N.Tag := new String'("link");
         Set_Attribute
           (N, "from", System.Address_Image (L.Get_From.all'Address));
         Set_Attribute
           (N, "to", System.Address_Image (L.Get_To.all'Address));
         Add_Child (XML, N, Append => True);
      end On_Link;

      Area : constant Model_Rectangle := View.Get_View.Get_Visible_Area;
   begin
      View.Get_View.Model.For_Each_Item (On_Item'Access, Filter => Kind_Item);
      View.Get_View.Model.For_Each_Item (On_Link'Access, Filter => Kind_Link);

      Set_Attribute (XML, "scale", View.Get_View.Get_Scale'Img);
      Set_Attribute (XML, "top", Gdouble'Image (Area.Y));
      Set_Attribute (XML, "left", Gdouble'Image (Area.X));
   end Save_To_XML;

   -------------------
   -- Load_From_XML --
   -------------------

   overriding procedure Load_From_XML
     (View : access General_Browser_Record; XML : XML_Utils.Node_Ptr)
   is
      package Addr_To_Items is new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Hash            => Ada.Strings.Hash,
         Element_Type    => GPS_Item,
         Equivalent_Keys => "=");
      use Addr_To_Items;

      Items         : Addr_To_Items.Map;
      Elem          : Addr_To_Items.Cursor;
      C             : Node_Ptr;
      It, It2       : GPS_Item;
   begin
      if Get_Attribute (XML, "scale") = "" then
         --  No contents was saved
         return;
      end if;

      C := XML.Child;
      while C /= null loop
         if C.Tag.all /= "link" then
            It := General_Browser (View).Load_From_XML (C);
            if It /= null then
               It.Set_Position
                 ((X => Gdouble'Value (Get_Attribute (C, "x")),
                   Y => Gdouble'Value (Get_Attribute (C, "y"))));
               Items.Include (Get_Attribute (C, "id"), It);
            end if;

         else
            Elem := Items.Find (Get_Attribute (C, "from"));
            if Has_Element (Elem) then
               It := Element (Elem);

               Elem := Items.Find (Get_Attribute (C, "to"));
               if Has_Element (Elem) then
                  It2 := Element (Elem);
                  General_Browser (View).Load_From_XML (C, It, It2);
               end if;
            end if;
         end if;

         C := C.Next;
      end loop;

      View.Get_View.Model.Refresh_Layout;

      View.Get_View.Set_Scale (Gdouble'Value (Get_Attribute (XML, "scale")));
      View.Get_View.Set_Topleft
        ((X => Gdouble'Value (Get_Attribute (XML, "left")),
          Y => Gdouble'Value (Get_Attribute (XML, "top"))));
   end Load_From_XML;

end Browsers.Canvas;
