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

with Default_Preferences;      use Default_Preferences;
with GPS.Kernel;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with Gdk.Event;
with Generic_Views;
with Gtk.Menu;
with Gtk.Toolbar;
with Gtk.Widget;
with Gtkada.Canvas_View;
with Gtkada.Canvas_View.Models;
with XML_Utils;

package Browsers.Canvas is

   type Browser_Model_Record is new Gtkada.Canvas_View.List_Canvas_Model_Record
   with null record;
   type Browser_Model is access all Browser_Model_Record'Class;
   --  which type of model we are using

   overriding function Is_Selectable
     (Self : not null access Browser_Model_Record;
      Item : not null access Gtkada.Canvas_View.Abstract_Item_Record'Class)
      return Boolean;

   type General_Browser_Record is new Generic_Views.View_Record with private;
   type General_Browser is access all General_Browser_Record'Class;
   --  Encapsulates a browser, based either on Gtkada.Canvas or
   --  Gtk.Canvas_View.

   overriding procedure Create_Toolbar
     (View    : not null access General_Browser_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Create_Menu
     (View    : not null access General_Browser_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure Save_To_XML
     (View : access General_Browser_Record;
      XML  : in out XML_Utils.Node_Ptr);
   overriding procedure Load_From_XML
     (View : access General_Browser_Record; XML : XML_Utils.Node_Ptr);

   procedure Initialize
     (Browser         : access General_Browser_Record'Class);
   --  Initialize a new browser.
   --  It sets up all the contextual menu for this browser, as well as the key
   --  shortcuts to manipulate the browser.

   procedure Register_Actions
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register some actions in the GPS kernel. Since there is no module
   --  associated with the current package, this subprogram is intended to be
   --  called by a module while it registers itself in the kernel
   --  (Browsers.Call_Graph for now).

   function Get_View
     (Browser : access General_Browser_Record) return GPS_Canvas_View;
   --  Return the canvas embedded in Browser

   function Get_Kernel (Browser : access General_Browser_Record)
      return GPS.Kernel.Kernel_Handle;
   --  Return the kernel associated with the browser

   function Horizontal_Layout
     (Self : not null access General_Browser_Record) return Boolean;
   --  Whether the layout should be horizontal

   procedure Refresh_Layout
     (Self                 : not null access General_Browser_Record;
      Rescale              : Boolean := False;
      Space_Between_Items  : Glib.Gdouble := Default_Space_Between_Items;
      Space_Between_Layers : Glib.Gdouble := Default_Space_Between_Layers);
   --  Recompute the position of all items.
   --  If Rescale is true, the scaling factor and position of the canvas are
   --  modified to show as many items as possible

   procedure Force_Refresh
     (Browser : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Toggle when the user choses to use waypoints or not.
   --  This is suitable for use as callback (for instance when the callback
   --  modifies one of the settings for the browser)

   procedure Preferences_Changed
     (Self : not null access General_Browser_Record;
      Pref : Default_Preferences.Preference) is null;
   --  Override if you need to monitor preferences

   -----------
   -- Items --
   -----------

   type Outline_Mode is (Outline_None, Outline_As_Linked, Outline_As_Match);
   --  Whether the item should be highlighted with an outline (because it
   --  is linked to one of the selected items).

   type GPS_Item_Record is abstract new Gtkada.Canvas_View.Rect_Item_Record
   with record
      Outline : Outline_Mode := Outline_None;

      Browser     : General_Browser;
      --  The browser in which the item is displayed.

      Left, Right : Gtkada.Canvas_View.Abstract_Item;
      --  left and right arrows in the title bar (if any)
   end record;
   type GPS_Item is access all GPS_Item_Record'Class;

   procedure Set_Context
     (Item    : not null access GPS_Item_Record;
      Context : in out GPS.Kernel.Selection_Context) is null;
   --  Set the GPS context from a selected item.

   function Save_To_XML
     (Self : not null access GPS_Item_Record)
      return XML_Utils.Node_Ptr is (null);
   --  Override this function to save an item in the GPS desktop, so that it
   --  is restored in the next GPS session.
   --  By default, items are not saved in the desktop, so a browser is restored
   --  empty.
   --  You will also need to override the browser's Load_From_ML

   overriding procedure Draw
     (Self    : not null access GPS_Item_Record;
      Context : Gtkada.Canvas_View.Draw_Context);

   procedure Set_Context
     (Browser : not null access General_Browser_Record;
      Context : in out GPS.Kernel.Selection_Context);
   --  Set the context from the topmost selected item

   function Load_From_XML
     (Self : not null access General_Browser_Record;
      Dummy_Node : XML_Utils.Node_Ptr)
      return access GPS_Item_Record'Class is (null);
   procedure Load_From_XML
     (Self     : not null access General_Browser_Record;
      Node     : XML_Utils.Node_Ptr;
      From, To : not null access GPS_Item_Record'Class) is null;
   --  Recreates an item from information saved in the desktop.
   --  In both cases, the item or the link *must* be added to the browser
   --  before returning. This allows the reuse of existing items and links.

   function Has_Link
     (Browser   : not null access General_Browser_Record'Class;
      Src, Dest : not null access GPS_Item_Record'Class) return Boolean;
   function Count_Links
     (Browser   : not null access General_Browser_Record'Class;
      Src, Dest : not null access GPS_Item_Record'Class;
      Oriented  : Boolean := True) return Natural;
   --  Whether there is already a link between the two items

   type Button_Array
     is array (Natural range <>)
     of access Gtkada.Canvas_View.Container_Item_Record'Class;
   No_Buttons : constant Button_Array := (1 .. 0 => null);

   procedure Setup_Titlebar
     (Item    : not null access GPS_Item_Record'Class;
      Browser : not null access General_Browser_Record'Class;
      Name    : String;
      Left    : access Left_Arrow_Record'Class := null;
      Right   : access Right_Arrow_Record'Class := null;
      Buttons : Button_Array := No_Buttons);
   --  Add the title bar items (title, arrows, close button,...)
   --  The two arrows should have been created and passed as argument, so that
   --  the proper callback is set on them. There Initialize primitive operation
   --  is automatically called.
   --  Extra buttons can be added through the Buttons parameter. They must all
   --  have been initialized already, but not put in the model.

   procedure Show_Left_Arrow (Self : not null access GPS_Item_Record);
   procedure Show_Right_Arrow (Self : not null access GPS_Item_Record);
   procedure Hide_Left_Arrow (Self : not null access GPS_Item_Record);
   procedure Hide_Right_Arrow (Self : not null access GPS_Item_Record);
   --  Control the visibility of the arrows in an item's title bar

   procedure Highlight_Related_Items
     (Self   : not null access GPS_Canvas_View_Record'Class;
      Item   : access Gtkada.Canvas_View.Abstract_Item_Record'Class := null);
   --  Mark related items specially so that they are outlined on the display.

   ---------
   -- MDI --
   ---------

   type Browser_Child_Record is new GPS_MDI_Child_Record with null record;
   overriding function Has_Menu_Bar_When_Floating
     (Child : not null access Browser_Child_Record) return Boolean
      is (True) with Inline;
   overriding function Build_Context
     (Self  : not null access Browser_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return GPS.Kernel.Selection_Context;
   --  The base type for putting browsers in the MDI.

   ----------------
   -- Navigation --
   ----------------

   procedure Add_Navigation_Location
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Title  : String);
   --  Add a location command to open the browser named Title

private

   package List_Rtree is new Gtkada.Canvas_View.Models.Rtree_Models
     (Browser_Model_Record);

   type General_Browser_Record is new Generic_Views.View_Record with record
      View     : GPS_Canvas_View;
      Model    : List_Rtree.Rtree_Model;
   end record;
end Browsers.Canvas;
