------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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
with Gtk.Check_Menu_Item;               use Gtk.Check_Menu_Item;
with Gtk.Enums;                         use Gtk.Enums;
with Gtk.Handlers;                      use Gtk.Handlers;
with Gtk.Menu;                          use Gtk.Menu;
with Gtk.Menu_Item;                     use Gtk.Menu_Item;
with Gtk.Menu_Tool_Button;              use Gtk.Menu_Tool_Button;
with Gtk.Scrolled_Window;               use Gtk.Scrolled_Window;
with Gtk.Stock;                         use Gtk.Stock;
with Gtk.Toolbar;                       use Gtk.Toolbar;
with Gtk.Tool_Button;                   use Gtk.Tool_Button;
with Gtk.Widget;                        use Gtk.Widget;

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
with GPS.Core_Kernels;                  use GPS.Core_Kernels;
with GPS.Intl;                          use GPS.Intl;
with GPS.Kernel;                        use GPS.Kernel;
with GPS.Kernel.Actions;                use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;               use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                  use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;            use GPS.Kernel.Preferences;
with GPS.Kernel.Modules.UI;             use GPS.Kernel.Modules.UI;
with GPS.Stock_Icons;                   use GPS.Stock_Icons;
with Histories;                         use Histories;
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

   type Preferences_Hook_Record is new Function_With_Args with record
      Browser : General_Browser;
   end record;
   type Preferences_Hook is access all Preferences_Hook_Record'Class;
   overriding procedure Execute
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when the preferences have changed

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
     (Browser         : access General_Browser_Record'Class)
   is
      Hook     : Preferences_Hook;
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
      Gtk_New (Browser.View, Browser.Model);
      Scrolled.Add (Browser.View);

      Id := Browser.Get_View.Model.On_Selection_Changed
        (On_Selection_Changed'Access, Browser);

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
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel, Data);
      B    : constant General_Browser := Hook.Browser;
   begin
      Create_Styles (B.View);
      Toggle_Draw_Grid (B);
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
         --  Look at the current selection
         B.Model.For_Each_Item (On_Item'Access, Filter => Kind_Item);

      else
         B.View.Set_Details (Details, Event.Button);
         Set_Browser_Information (Context, Details);
         if Details.Toplevel_Item /= null then
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
      B.Model.For_Each_Item (On_Item'Access, Filter => Kind_Item);
      B.Model.Remove (To_Remove);
      B.Model.Refresh_Layout;
      B.View.Queue_Draw;
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
      To_Remove : Item_Sets.Set;

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         if not B.Model.Is_Selected (Item) then
            B.Model.Include_Related_Items (Item, To_Remove);
         end if;
      end On_Item;

   begin
      B.Model.For_Each_Item (On_Item'Access, Filter => Kind_Item);
      B.Model.Remove (To_Remove);
      B.Model.Refresh_Layout;
      B.View.Queue_Draw;
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
      Browser_Model (B.View.Model).Clear;
      Browser_Model (B.View.Model).Refresh_Layout;
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
      Draw_Grid : constant Boolean :=
        Get_History (Get_History (View.Kernel).all, Hist_Draw_Grid);
      Annotation_Font : Pango_Font_Description;
   begin
      Annotation_Font := Copy (Preferences.Default_Font.Get_Pref_Font);
      Set_Size
        (Annotation_Font,
         Gint'Max (Pango_Scale, Get_Size (Annotation_Font) - 2 * Pango_Scale));

      View.View.Set_Grid_Size (Size => 15.0);

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
            B.View.Set_Scale (Zoom_Levels (J + 1));
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
            B.View.Set_Scale (Zoom_Levels (J - 1));
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
      Data.Browser.View.Set_Scale (Data.Zoom);
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
     (Filter  : access Is_In_Browser;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Get_Creator (Context) /= null
        and then Browser_From_Context (Context) /= null;
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
