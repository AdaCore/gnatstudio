------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
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

with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;

with GNAT.Strings;                      use GNAT.Strings;
with GNATCOLL.VFS;                      use GNATCOLL.VFS;

with Glib;                              use Glib;
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

with Gdk;                               use Gdk;
with Gdk.Event;                         use Gdk.Event;
with Gdk.Pixbuf;                        use Gdk.Pixbuf;
with Gdk.RGBA;                          use Gdk.RGBA;
with Gdk.Rectangle;                     use Gdk.Rectangle;
with Gdk.Types.Keysyms;                 use Gdk.Types.Keysyms;
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
with GNATCOLL.Traces;                   use GNATCOLL.Traces;
with XML_Utils;                         use XML_Utils;

package body Browsers.Canvas is
   Me : constant Trace_Handle := Create ("CANVAS");

   Zoom_Levels : constant array (Positive range <>) of Gdouble :=
                   (0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0);
   --  All the possible zoom levels. We have to use such an array, instead
   --  of doing the computation directly, so as to avoid rounding errors that
   --  would appear in the computation and make zoom_in not the reverse of
   --  zoom_out.

   Hist_Align_On_Grid : constant History_Key := "browsers-align-on-grid";
   Hist_Straight_Links : constant History_Key := "browsers-straight-links";
   Hist_Draw_Grid : constant History_Key := "browsers-draw-grid";

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

   procedure Close_Item
     (Event : Gdk.Event.Gdk_Event_Button;
      User  : access Browser_Item_Record'Class);
   --  Close an item when the user presses on the title bar button

   procedure Dump
     (Me : Trace_Handle; Tree : Active_Area_Tree; Indent : Natural := 0);
   pragma Warnings (Off, Dump);
   --  For debugging purposes, dump the tree to Me

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

   procedure Compute_Parents
     (Event : Gdk_Event_Button; Item : access Browser_Item_Record'Class);
   procedure Compute_Children
     (Event : Gdk_Event_Button; Item : access Browser_Item_Record'Class);
   --  Callbacks for the title bar buttons of Arrow_item

   type Image_Canvas_Record is new
     Gtkada.Canvas.Interactive_Canvas_Record with null record;
   type Image_Canvas is access all Image_Canvas_Record'Class;

   overriding function Get_Window
     (Canvas : access Image_Canvas_Record) return Gdk.Gdk_Window;
   --  Override Gtk.Widget.Get_Window, so that a different Window can be
   --  returned if needed (e.g. when exporting the canvas).

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Browser         : access General_Browser_Record'Class;
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

      Create_New_Boolean_Key_If_Necessary
        (Get_History (Browser.Kernel).all,
         Hist_Draw_Grid, Default_Value => True);
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
      Iter            : Item_Iterator;
   begin
      Refresh_Layout_Orientation (Hook.Browser);

      Iter := Start (Hook.Browser.Canvas);
      while Get (Iter) /= null loop
         declare
            Item   : constant Browser_Item := Browser_Item (Get (Iter));
            Layout : constant Pango.Layout.Pango_Layout :=
                      Item.Title_Layout;
         begin
            if Layout /= null then
               Set_Font_Description (Layout, Default_Font.Get_Pref_Font);
            end if;
         end;

         Next (Iter);
      end loop;

      Toggle_Draw_Grid (Hook.Browser);
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
         Check, Default => True);
      Menu.Append (Check);
      Widget_Callback.Object_Connect
        (Check, Gtk.Check_Menu_Item.Signal_Toggled,
         Toggle_Draw_Grid'Access, View);

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
   end Create_Menu;

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
      Item         : Canvas_Item;
      Xr, Yr       : Gint;
      Xroot, Yroot : Gdouble;
      Xsave, Ysave : Gdouble;

   begin
      if Get_Event_Type (Event) in Button_Press .. Button_Release then
         --  Click on an item: this is a file selections
         --  ??? Should we convert to world coordinates here ?

         Get_Origin (Get_Window (B.Canvas), Xr, Yr);
         Get_Root_Coords (Event, Xroot, Yroot);
         Event.Button.X := Xroot - Gdouble (Xr);
         Event.Button.Y := Yroot - Gdouble (Yr);
         Item := Item_At_Coordinates (B.Canvas, Event);
      end if;

      if Item /= null then
         Get_Coords (Event, Xsave, Ysave);
         Event.Button.X := Xsave - Gdouble (Get_Coord (Item).X);
         Event.Button.Y := Ysave - Gdouble (Get_Coord (Item).Y);
         Contextual_Factory
           (Browser_Item (Item), Context, B, Event, Menu);
         Event.Button.X := Xsave;
         Event.Button.Y := Ysave;
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
      Item : Canvas_Item;
   begin
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
      Item : Canvas_Item;
   begin
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
      Item : Canvas_Item;
   begin
      Iter := Start (B.Canvas);
      loop
         Item := Get (Iter);
         exit when Item = null;

         Browser_Item (Item).Hide_Links := not Browser_Item (Item).Hide_Links;
         Next (Iter);
      end loop;
      Refresh_Canvas (B.Canvas);
      return Commands.Success;
   end Execute;

   --------------------
   -- On_Export_Idle --
   --------------------

   function On_Export_Idle (Data : Export_Idle_Data) return Boolean is
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
               Kernel.Insert
                 ("Cannot create " & Display_Full_Name (Name),
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
      Set_Layout_Algorithm (B.Canvas, Layer_Layout'Access);
      Layout (B, Force => True);
      Refresh_Canvas (Get_Canvas (B));
      Set_Layout_Algorithm (B.Canvas, Simple_Layout'Access);
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
      Clear (B.Canvas);
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
      Align_On_Grid (Get_Canvas (View), Align);
   end Change_Align_On_Grid;

   -----------------------
   -- Toggle_Orthogonal --
   -----------------------

   procedure Toggle_Orthogonal (Browser : access Gtk_Widget_Record'Class) is
      View  : constant General_Browser := General_Browser (Browser);
      Straight : constant Boolean :=
        Get_History (Get_History (View.Kernel).all, Hist_Straight_Links);
   begin
      Set_Orthogonal_Links (Get_Canvas (View), not Straight);
      Refresh_Canvas (Get_Canvas (View));
   end Toggle_Orthogonal;

   ----------------------
   -- Toggle_Draw_Grid --
   ----------------------

   procedure Toggle_Draw_Grid (Browser : access Gtk_Widget_Record'Class) is
      View  : constant General_Browser := General_Browser (Browser);
      Grid            : Guint := Gtkada.Canvas.Default_Grid_Size;
      Annotation_Font : Pango_Font_Description;
   begin
      Annotation_Font := Copy (Default_Font.Get_Pref_Font);
      Set_Size
        (Annotation_Font,
         Gint'Max (Pango_Scale, Get_Size (Annotation_Font) - 2 * Pango_Scale));

      if not Get_History (Get_History (View.Kernel).all, Hist_Draw_Grid) then
         Grid := 0;
      end if;

      Configure
        (View.Canvas,
         Annotation_Font => Annotation_Font,
         Grid_Size       => Grid,
         Background      => Browsers_Bg_Color.Get_Pref);

      Free (Annotation_Font);

      Refresh_Canvas (Get_Canvas (View));
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
      Z : constant Gdouble := Get_Zoom (B.Canvas);
   begin
      for J in Zoom_Levels'Range loop
         if Zoom_Levels (J) = Z then
            if J /= Zoom_Levels'Last then
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
      Z : constant Gdouble := Get_Zoom (B.Canvas);
   begin
      for J in Zoom_Levels'Range loop
         if Zoom_Levels (J) = Z then
            if J /= Zoom_Levels'First then
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
         if not Is_Selected (Canvas, Canvas_Item (Get_Src (Link)))
           and then not Is_Selected (Canvas, Canvas_Item (Get_Dest (Link)))
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
     (Event : Gdk.Event.Gdk_Event_Button;
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
         end if;

         if Get_Dest (Link) = Vertex_Access (User) then
            Reset (Browser_Item (Get_Src (Link)),
                   Parent_Removed => False,
                   Child_Removed  => True);
         end if;

         return True;
      end Reset_Item;

   begin
      if Event.Button = 1
        and then Event.The_Type = Button_Release
      then
         For_Each_Link
           (Get_Canvas (B), Reset_Item'Unrestricted_Access,
            From => Canvas_Item (User));
         For_Each_Link
           (Get_Canvas (B), Reset_Item'Unrestricted_Access,
            To => Canvas_Item (User));

         Remove (Get_Canvas (B), User);
         Refresh_Canvas (Get_Canvas (B));
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
      Style_Context : constant Gtk_Style_Context :=
        Get_Style_Context (Get_Browser (Item));
      Border        : Gtk.Style.Gtk_Border;
      The_Border    : Gtk.Style.Gtk_Border;
      Thick         : Gint16;
      Button_Width  : Gint;
      Button_Height : Gint;
      X, Y, W, H    : Gint;

   begin
      Style_Context.Get_Border (Gtk_State_Flag_Normal, The_Border);
      Border := The_Border;

      Thick         := Border.Bottom;
      Button_Width  := Get_Width  (Item.Browser.Close_Pixmap);
      Button_Height := Get_Height (Item.Browser.Close_Pixmap);
      X             := Item.Title_Coord.X +
        Item.Title_Coord.Width + Gint (Thick) -
        (Num + 1) * (Margin + Button_Width);

      --  No title ? Don't draw any button
      if Item.Title_Layout = null then
         return;
      end if;

      Get_Pixel_Size (Item.Title_Layout, W, H);
      Y := Item.Title_Coord.Y + (Item.Title_Coord.Height - Button_Height) / 2;

      Add_Active_Area
        (Item,
         Gdk_Rectangle'(X, Y, Button_Width, Button_Height),
         Cb);

      X := X + (Button_Width - Get_Width (Pixbuf)) / 2;
      Y := Item.Title_Coord.Y +
        (Item.Title_Coord.Height - Get_Height (Pixbuf)) / 2;
      Draw_Pixbuf (Cr, Pixbuf, X, Y);
   end Draw_Title_Bar_Button;

   ----------------------
   -- Redraw_Title_Bar --
   ----------------------

   procedure Redraw_Title_Bar
     (Item : access Browser_Item_Record;
      Cr   : Cairo.Cairo_Context)
   is
      B      : constant General_Browser := Get_Browser (Item);
      Canvas : constant Interactive_Canvas := Get_Canvas (B);

      Style_Context : constant Gtk_Style_Context :=
        Get_Style_Context (Get_Browser (Item));
      Border        : Gtk.Style.Gtk_Border;
      The_Border    : Gtk.Style.Gtk_Border;
      W_L, H_L : Gint;
      Ptrn     : Cairo_Pattern;
      Base     : Cairo_Color;
      Color    : Cairo_Color;
      Iter   : Item_Iterator;

   begin
      if Item.Title_Layout = null then
         return;
      end if;

      Reset_Active_Areas (Item.all, Other_Areas => False);

      if Is_Selected (Item.Browser.Canvas, Item) then
         Base := Selected_Item_Color.Get_Pref;
      else
         Base := White_RGBA;

         Iter := Start (Canvas,
                        Linked_From_Or_To => Canvas_Item (Item));
         while Get (Iter) /= null loop
            if Is_Selected (Canvas, Get (Iter)) then
               if Is_Linked_From (Iter) then
                  Base := Child_Linked_Item_Color.Get_Pref;
                  exit;
               else
                  Base := Parent_Linked_Item_Color.Get_Pref;
                  exit;
               end if;
            end if;
            Next (Iter);
         end loop;
      end if;

      --  The title background
      Cairo.Rectangle
        (Cr, Gdouble (Item.Title_Coord.X), Gdouble (Item.Title_Coord.Y),
         Gdouble (Item.Title_Coord.Width), Gdouble (Item.Title_Coord.Height));

      Ptrn := Create_Linear
        (0.0, Gdouble (Item.Title_Coord.Y), 0.0,
         Gdouble (Item.Title_Coord.Y + Item.Title_Coord.Height - 1));

      Color := Lighten (Base, 0.1);
      Add_Color_Stop_Rgb (Ptrn, 0.0, Color.Red, Color.Green, Color.Blue);

      Color := Shade (Base, 0.1);
      Add_Color_Stop_Rgb (Ptrn, 1.0, Color.Red, Color.Green, Color.Blue);

      Set_Source (Cr, Ptrn);
      Destroy (Ptrn);

      Cairo.Fill (Cr);

      --  The title string
      Get_Pixel_Size (Item.Title_Layout, W_L, H_L);
      Style_Context.Get_Border (Gtk_State_Flag_Normal, The_Border);
      Border := The_Border;

      Draw_Layout
        (Cr     => Cr,
         Color  => Black_RGBA,
         X      => Margin + Item.Title_Coord.X - Gint (Border.Left),
         Y      => Item.Title_Coord.Y + (Item.Title_Coord.Height - H_L) / 2,
         Layout => Item.Title_Layout);

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

      Redraw_Title_Bar (Browser_Item_Record'Class (Item.all)'Access, Cr);
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
         Trace (Me, E);
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

   ----------
   -- Call --
   ----------

   overriding function Call
     (Callback : Item_Active_Area_Callback;
      Event    : Gdk.Event.Gdk_Event_Button) return Boolean is
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
               if Tmp_Children = null then
                  Area.Children := new Active_Area_Tree_Array'
                    (1 .. 1 => Tmp);
               else
                  Area.Children := new Active_Area_Tree_Array'
                    (Tmp_Children.all & Tmp);
                  Unchecked_Free (Tmp_Children);
               end if;

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
      Event : Gdk.Event.Gdk_Event_Button) return Boolean
   is
      X : constant Glib.Gint := Gint (Event.X);
      Y : constant Glib.Gint := Gint (Event.Y);

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
     (Me : Trace_Handle; Tree : Active_Area_Tree; Indent : Natural := 0)
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
      Destroy (Canvas_Item_Record (Item));
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
      Reset_Active_Areas (Item.all, Title_Bar_Areas => False);

      Layout := Create_Pango_Layout (Get_Browser (Item), "");
      Set_Font_Description (Layout, Default_Font.Get_Pref_Font);

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
      Set_Font_Description (Layout, Default_Font.Get_Pref_Font);

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
      Color       : Gdk_RGBA;
      W, H        : Gint;
      Num_In_Line : Natural;
      Text        : String_Access;

      procedure Display (Text : String; Line : Xref_Line);
      --  Display Text on Item

      procedure Display (Text : String; Line : Xref_Line) is
      begin
         if Text'Length = 0 then
            return;
         end if;

         Layout.Set_Text (Text);

         if In_Xref then
            Color := Browsers_Hyper_Link_Color.Get_Pref;
         else
            Color := Black_RGBA;
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

      H := 0;

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
     (Event : Gdk_Event_Button; Item : access Browser_Item_Record'Class) is
   begin
      if Event.Button = 1
        and then Event.The_Type = Button_Release
      then
         Arrow_Item (Item).Parents_Cb (Arrow_Item (Item));
      end if;
   end Compute_Parents;

   ----------------------
   -- Compute_Children --
   ----------------------

   procedure Compute_Children
     (Event : Gdk_Event_Button; Item : access Browser_Item_Record'Class) is
   begin
      if Event.Button = 1
        and then Event.The_Type = Button_Release
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
         Filter    => Filter,
         Accel_Key => GDK_minus);

      Register_Action
        (Kernel, "browser zoom in", new Zoom_In_Command,
         -"Zoom in",
         Stock_Id  => Stock_Zoom_In,
         Category  => -"Browsers",
         Filter    => Filter,
         Accel_Key => GDK_equal);

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
   begin
      Select_All (B.Canvas);
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

end Browsers.Canvas;
