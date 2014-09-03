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

with Ada.Unchecked_Deallocation;
with Cairo.Region;
with GNAT.Strings;
with GPS.Kernel;
with Gdk.Event;
with Gdk.Pixbuf;
with Gdk.Rectangle;
with Generic_Views;
with Glib.Object;
with Gtk.Handlers;
with Gtk.Hbutton_Box;
with Gtk.Menu;
with Gtk.Stock;
with Gtk.Toolbar;
with Gtk.Widget;
with Gtkada.Canvas;
with Gtkada.Canvas_View;
with Gtkada.Canvas_View.Models;
with Pango.Layout;

package Browsers.Canvas is

   Margin : constant := 5;
   --  Margin used when drawing the items, to leave space around the arrows and
   --  the actual contents of the item

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

   type Browser_Link_Record is new Gtkada.Canvas.Canvas_Link_Record
   with record
      Orthogonal : Boolean := False;
      --  See Get_Orthogonal
   end record;
   type Browser_Link is access all Browser_Link_Record'Class;
   --  The type of links that are put in the canvas. These are automatically
   --  highlighted if they connect a selected item to another one.

   procedure Initialize
     (Browser         : access General_Browser_Record'Class;
      Create_Toolbar  : Boolean := False;
      Parents_Pixmap  : String := Gtk.Stock.Stock_Go_Back;
      Children_Pixmap : String := Gtk.Stock.Stock_Go_Forward;
      Use_Canvas_View : Boolean := False);
   --  Initialize a new browser.
   --  It sets up all the contextual menu for this browser, as well as the key
   --  shortcuts to manipulate the browser.
   --  If Create_Toolbar is True, then a button_bar is added at the bottom.
   --  Parents_Pixmap and Children_Pixmap are the pixmaps to use in the title
   --  bar to get access to the parents/ancestors of an item or its children.

   function Get_Orthogonal (E : access Browser_Link_Record) return Boolean;
   --  If true, the two vertices will be layed out on the same row/column.
   --  If False, Dest will appear to the right (resp. bottom) of Src.
   --  This is only a recommendation, and the layout algorithm is allowed
   --  to chose other organization. Likewise, Dest might not end up on the
   --  same row if for some reason it should also be to the bottom of
   --  another item.

   procedure Register_Actions
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register some actions in the GPS kernel. Since there is no module
   --  associated with the current package, this subprogram is intended to be
   --  called by a module while it registers itself in the kernel
   --  (Browsers.Call_Graph for now).

   function Get_Toolbar (Browser : access General_Browser_Record)
      return Gtk.Hbutton_Box.Gtk_Hbutton_Box;
   --  Return the toolbar at the bottom of the browser. This returns null if no
   --  toolbar was created in the call to Initialize.

   function Get_Canvas
     (Browser : access General_Browser_Record)
      return Gtkada.Canvas.Interactive_Canvas;
   function Get_View
     (Browser : access General_Browser_Record) return GPS_Canvas_View;
   --  Return the canvas embedded in Browser

   function Get_Kernel (Browser : access General_Browser_Record)
      return GPS.Kernel.Kernel_Handle;
   --  Return the kernel associated with the browser

   function To_Browser
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class)
      return General_Browser;
   --  Return the browser that contains Canvas

   function Horizontal_Layout
     (Self : not null access General_Browser_Record) return Boolean;
   --  Whether the layout should be horizontal

   procedure Layout
     (Browser : access General_Browser_Record;
      Force   : Boolean := False);
   --  Recompute the layout of items in the browser.
   --  If Force is true, then even the items that have been moved manually by
   --  the user are recomputed.

   procedure Refresh_Layout
     (Self                 : not null access General_Browser_Record;
      Rescale              : Boolean := False;
      Space_Between_Items  : Glib.Gdouble := Default_Space_Between_Items;
      Space_Between_Layers : Glib.Gdouble := Default_Space_Between_Layers);
   --  Recompute the position of all items.
   --  If Rescale is true, the scaling factor and position of the canvas are
   --  modified to show as many items as possible

   procedure Refresh_Layout_Orientation
     (Browser : access General_Browser_Record);
   --  Called when the orientation of the layout might have changed (ie while
   --  taking into account the new preferences). By default, the orientation is
   --  read from the preferences, although by overriding this subprogram a
   --  default orientation can be forced

   ------------------
   -- Active areas --
   ------------------
   --  The items have a general mechanism to define active areas and nested
   --  active areas. When the user clicks in the item, the appropriate callback
   --  is called. The one chosen is the inner most one.
   --  The callback chosen is undefined if two areas only partially overlap.

   type Active_Area_Callback is abstract tagged null record;
   type Active_Area_Cb is access all Active_Area_Callback'Class;
   function Call (Callback : Active_Area_Callback;
                  Event    : Gdk.Event.Gdk_Event_Button)
     return Boolean is abstract;
   --  A callback for the active areas. Event is the mouse event that started
   --  the chain that lead to callback.
   --  This type provides an easy encapsulation for any user data you might
   --  need.
   --  Should return True if the event was handled, False otherwise. In the
   --  latter case, the even is transmitted to the parent area

   type Active_Area_Cb_Array is array (Natural range <>) of Active_Area_Cb;
   Empty_Cb_Array : constant Active_Area_Cb_Array;

   procedure Destroy (Callback : in out Active_Area_Callback);
   --  Destroy the callback

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Active_Area_Callback'Class, Active_Area_Cb);

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
      Context : in out GPS.Kernel.Selection_Context) is abstract;
   --  Set the GPS context from a selected item.

   overriding procedure Draw
     (Self    : not null access GPS_Item_Record;
      Context : Gtkada.Canvas_View.Draw_Context);

   type Browser_Item_Record is new Gtkada.Canvas.Canvas_Item_Record
     with private;
   type Browser_Item is access all Browser_Item_Record'Class;
   --  The type of items that are put in the canvas. They are associated with
   --  contextual menus, and also allows hiding the links to and from this
   --  item.

   procedure Set_Context
     (Browser : not null access General_Browser_Record;
      Context : in out GPS.Kernel.Selection_Context);
   --  Set the context from the topmost selected item

   function Has_Link
     (Browser   : not null access General_Browser_Record'Class;
      Src, Dest : not null access GPS_Item_Record'Class) return Boolean;
   function Count_Links
     (Browser   : not null access General_Browser_Record'Class;
      Src, Dest : not null access GPS_Item_Record'Class;
      Oriented  : Boolean := True) return Natural;
   --  Whether there is already a link between the two items

   package Item_Cb is new Gtk.Handlers.User_Callback
     (Gtk.Widget.Gtk_Widget_Record, Browser_Item);

   procedure Initialize
     (Item    : access Browser_Item_Record'Class;
      Browser : access General_Browser_Record'Class);
   --  Associate the item with a browser.

   procedure Set_Title
     (Item : access Browser_Item_Record'Class; Title : String := "");
   --  Set a title for the item. This is displayed in a special title bar, with
   --  a different background color.
   --  If Title is the empty string, no title bar is shown.
   --  Title must be UTF8-encoded.

   procedure Contextual_Factory
     (Item    : access Browser_Item_Record;
      Context : in out GPS.Kernel.Selection_Context;
      Browser : access General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu) is null;
   --  Return the selection context to use when an item is clicked on.
   --  The coordinates in Event are relative to the upper-left corner of the
   --  item.
   --  If there are specific contextual menu entries that should be used for
   --  this item, they should be added to Menu.
   --  null should be return if there is no special contextual for this
   --  item. In that case, the context for the browser itself will be used.
   --
   --  You shoud make sure that this function can be used with a null event and
   --  a null menu, which is the case when creating a current context for
   --  GPS.Kernel.Get_Current_Context.
   --
   --  Only one of these is used, depending on the type of canvas

   procedure Recompute_Size
     (Item   : not null access Browser_Item_Record'Class);
   --  Recompute the size of the item (via a dispatching call to Compute_Size

   procedure Compute_Size
     (Item   : not null access Browser_Item_Record;
      Layout : not null access Pango.Layout.Pango_Layout_Record'Class;
      Width, Height : out Glib.Gint;
      Title_Box     : in out Cairo.Region.Cairo_Rectangle_Int);
   --  Compute the size for the item.
   --  This size should not include the size of the title bar.
   --
   --  Title_Box is the minimum area for the title (as if it was displayed at
   --  the top). Most often, overridings of this procedure will want to at
   --  least increase the width to be that of the box contents itself.

   overriding procedure Draw
     (Item : access Browser_Item_Record;
      Cr   : Cairo.Cairo_Context);
   --  See inherited documentation

   procedure Refresh_Linked_Items
     (Item             : access Browser_Item_Record'Class;
      Refresh_Parents  : Boolean := False;
      Refresh_Children : Boolean := False);
   --  Refresh either the parents or the children of Item. For each of these,
   --  this calls Refresh above

   procedure Resize_And_Draw
     (Item             : access Browser_Item_Record;
      Cr               : Cairo.Cairo_Context;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class);
   --  Resize the item, and then redraw it.
   --  The chain of events should be the following:
   --   - Compute the desired size for the item
   --   - Merge it with Width, Height, Width_Offset and Height_Offset.
   --
   --            xoffset
   --     |------|------------------------|------|
   --     |      |    child               |      |
   --     |      |------------------------|      |
   --     |          item                        |
   --     |------|------------------------|------|
   --
   --     Width is the size of the part where the child will be drawn. It
   --     should be computed by taking the maximum of the item's desired size
   --     and the Width parameter
   --     Width_Offset is the total width on each size of the child. It should
   --     be computed by adding the new desired offset to Width_Offset. The
   --     addition to Width_Offset will generally be the same as Xoffset
   --
   --   - Call the parent's Resize_And_Draw procedure. This will
   --     ultimately resize the item.
   --   - Draw the item at coordinates Xoffset, Yoffset in the double-buffer.
   --   - Modify Xoffset, Yoffset to the position that a child of item should
   --     be drawn at. However, it shouldn't redraw the title bar buttons,
   --     which are handled through Redraw_Title_Bar.
   --
   --  This procedure doesn't need to reset the active areas, this is done
   --  automatically.
   --
   --  Layout can be modified freely (but not destroyed) to display text. It is
   --  preconfigured with the correct font, and is passed for efficiency reason
   --  to avoid creating a layout every time.

   procedure Draw_Title_Bar_Button
     (Item   : access Browser_Item_Record;
      Cr     : Cairo.Cairo_Context;
      Num    : Glib.Gint;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      Cb     : Active_Area_Callback'Class);
   --  Draw the nth title bar button. They are numbered from right to left:
   --       |--------------------------|
   --       |  Title         [2][1][0] |
   --       |--------------------------|
   --  You can draw as many buttons as you want. However, make sure you have
   --  reserved enough space (through Get_Last_Button_Number) or the buttons
   --  will override the title itself.  Cb is called when the button is
   --  pressed.
   --
   --  An item should use button numbers from the parent's
   --  Get_Last_Button_Number + 1 upward.
   --
   --  The button (and its callback) will be destroyed the next time
   --  Resize_And_Draw is called for the item.
   --  All buttons are assumed to have the size Gtk.Enums.Icon_Size_Menu.
   --
   --  No button is drawn if no title was set for the item.

   function Get_Last_Button_Number (Item : access Browser_Item_Record)
      return Glib.Gint;
   --  Return the last number of the button set by this item. This function is
   --  used to make sure that no two items set the same button.

   procedure Redraw_Title_Bar
     (Item : access Browser_Item_Record;
      Cr   : Cairo.Cairo_Context);
   --  This function should redraw the title bar buttons, after calling the
   --  inherited subprogram.
   --
   --  This procedure doesn't need to reset the active areas for the buttons,
   --  this is done automatically.

   procedure Setup_Titlebar
     (Item    : not null access GPS_Item_Record'Class;
      Browser : not null access General_Browser_Record'Class;
      Name    : String;
      Left    : access Left_Arrow_Record'Class := null;
      Right   : access Right_Arrow_Record'Class := null);
   --  Add the title bar items (title, arrows, close button,...)
   --  The two arrows should have been created and passed as argument, so that
   --  the proper callback is set on them. There Initialize primitive operation
   --  is automatically called.

   procedure Show_Left_Arrow (Self : not null access GPS_Item_Record);
   procedure Show_Right_Arrow (Self : not null access GPS_Item_Record);
   procedure Hide_Left_Arrow (Self : not null access GPS_Item_Record);
   procedure Hide_Right_Arrow (Self : not null access GPS_Item_Record);
   --  Control the visibility of the arrows in an item's title bar

   procedure Reset
     (Item : access Browser_Item_Record;
      Parent_Removed, Child_Removed : Boolean);
   --  Reset the internal state of the item, as if it had never been expanded,
   --  analyzed,... This is called for instance after the item has been defined
   --  as the root of the canvas (and thus all other items have been removed).
   --  It doesn't need to redraw the item.
   --  Parent_Removed and Child_Removed indicate which type of sibling was
   --  removed. Both are set to True if both types of items have been removed

   function Get_Browser
     (Item : access Browser_Item_Record'Class) return General_Browser;
   --  Return the browser associated with this item

   procedure Highlight_Related_Items
     (Self   : not null access GPS_Canvas_View_Record'Class;
      Item   : access Gtkada.Canvas_View.Abstract_Item_Record'Class := null);
   --  Mark related items specially so that they are outlined on the display.

   procedure Add_Active_Area
     (Item      : access Browser_Item_Record;
      Rectangle : Gdk.Rectangle.Gdk_Rectangle;
      Callback  : Active_Area_Callback'Class);
   --  Define a new clickable active area in the item. Callback will be called
   --  whenever the user clicks in Rectangle, provided there is no smaller area
   --  that also contains the click location.

   function Activate
     (Item  : access Browser_Item_Record;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean;
   --  Calls the callback that is activated when the user clicks in the
   --  item. The coordinates returned by Get_X and Get_Y in Event should be
   --  relative to the top-left corner of the Item.

   procedure Reset_Active_Areas
     (Item            : in out Browser_Item_Record;
      Title_Bar_Areas : Boolean := True;
      Other_Areas     : Boolean := True);
   --  Remove all active areas in Item.
   --  If Title_Bar_Areas is False, then the areas that are in the title bar
   --  (supposedly buttons) are not removed
   --  If Other_Areas is False, then the areas that are not in the title bar
   --  are not removed.

   -----------------
   -- Xrefs lists --
   -----------------
   --  This type comes as an addition to the active areas. These are a simple
   --  way to prepare a the lines to display in an item, along with hyper
   --  links. The recommended use is to use in Resize_And_Draw subprograms:
   --  First pass is to prepare the lines through these Xref lists, then
   --  compute the total size of the item, call the parent's Resize_And_Draw,
   --  and finally draw the xrefs list on the item.

   type Xref_List is private;

   procedure Add_Line
     (List     : in out Xref_List;
      Str      : String;
      Length1  : Natural              := Natural'Last;
      Callback : Active_Area_Cb_Array := Empty_Cb_Array);
   --  Add a new line that will be displayed in a layout.
   --  Str can contain any number of substrings delimited by @...@. When the
   --  user clicks on that zone, the matching callback will be called. It must
   --  be UTF8-encoded.
   --  Length1 is the number of characters in the first column. The first
   --  character in the second column will always be aligned. Set to
   --  Natural'Last if there is only one column.

   procedure Expand_Line
     (List     : in out Xref_List;
      Num      : Positive;
      Str      : String;
      Callback : Active_Area_Cb_Array := Empty_Cb_Array;
      Check_Duplicates : Boolean);
   --  Add some contents to an existing line.
   --  A new line is appended if there is no such line already.
   --  If Check_Duplicates is true, then the new contents is not added if any
   --  of the xref in Str is already found on the existing line (it is
   --  recommended that a single xref be added at a time).

   procedure Display_Lines
     (Item          : access Browser_Item_Record'Class;
      Cr            : Cairo.Cairo_Context;
      List          : Xref_List;
      X             : Glib.Gint;
      Y             : in out Glib.Gint;
      Second_Column : Glib.Gint;
      Layout        : access Pango.Layout.Pango_Layout_Record'Class);
   --  Display the lines from List into Pixmap, starting at X, Y, and setup
   --  appropriate callbacks.
   --  Layout is used while drawing the strings. It has to be provided for
   --  efficiency reasons, so that it doesn't need to be recreated every time.
   --  The first characters of the second column (if any) will all be displayed
   --  at X coordinate (Second_Column + X).

   procedure Free (List : in out Xref_List);
   --  Free the data in List.
   --  This doesn't free any of the callbacks, since these are still in use for
   --  the hyper links. They will be freed when Reset_Active_Areas is called.

   procedure Get_Pixel_Size
     (Browser   : access General_Browser_Record'Class;
      List      : Xref_List;
      W1, W2, H : out Glib.Gint;
      Layout    : access Pango.Layout.Pango_Layout_Record'Class);
   --  Compute the approximate pixels size for List.
   --  W1, W2 are the widths of the two columns (depending on how each line was
   --  split).
   --  Layout is used while computing the length, and has to be provided for
   --  efficiency issues, to avoid recreating it every time.

   procedure Get_Line
     (List        : Xref_List;
      Num         : Positive;
      Num_In_Line : Positive := 1;
      Callback    : out Active_Area_Cb;
      Text        : out GNAT.Strings.String_Access);
   --  Return the contents of the Nth line in the list in Text. This is set to
   --  null if there is no such line. Do not free Text.
   --  It also returns the num_in_line-th link in this line in Callback or null
   --  if there is no such callback

   procedure Remove_Line (List : in out Xref_List; Num : Positive);
   --  Remove the Nth line from List

   ---------------
   -- Item area --
   ---------------

   type Item_Active_Callback is access
     procedure (Event : Gdk.Event.Gdk_Event_Button;
                User  : access Browser_Item_Record'Class);
   type Item_Active_Area_Callback is new Active_Area_Callback with private;
   --  A special instanciation of the callback for cases where the user data is
   --  a widget.

   function Build
     (Cb   : Item_Active_Callback;
      User : access Browser_Item_Record'Class)
      return Item_Active_Area_Callback'Class;
   --  Build a new callback

   -----------------
   -- Arrow_Items --
   -----------------
   --  This specialized type of items has a title bar with at least two
   --  buttons, to show the parents or the children of the items.
   --  You should override Resize_And_Draw to indicate what the contents of the
   --  item is.

   type Arrow_Item_Record is new Browser_Item_Record with private;
   type Arrow_Item is access all Arrow_Item_Record'Class;

   type Arrow_Item_Callback is access procedure
     (Item : access Arrow_Item_Record'Class);

   procedure Initialize
     (Item                    : access Arrow_Item_Record'Class;
      Browser                 : access General_Browser_Record'Class;
      Title                   : String;
      Parents_Cb, Children_Cb : Arrow_Item_Callback);
   --  Initialize a new item. Title is displayed in the title bar.
   --  Parents_Cb and Children_Cb are called when the two title bar buttons are
   --  pressed.

   function Parents_Shown (Item : access Arrow_Item_Record) return Boolean;
   function Children_Shown (Item : access Arrow_Item_Record) return Boolean;
   --  Return True if either all the parents or all the children are shown

   procedure Set_Parents_Shown
     (Item : access Arrow_Item_Record; All_Shown : Boolean);
   procedure Set_Children_Shown
     (Item : access Arrow_Item_Record; All_Shown : Boolean);
   --  Inidicate wether all the parents or all the children are shown

   -----------
   -- Links --
   -----------

   overriding procedure Draw_Link
     (Canvas      : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Link        : access Browser_Link_Record;
      Cr          : Cairo.Cairo_Context;
      Edge_Number : Glib.Gint;
      Show_Annotation : Boolean := True);
   --  Override the drawing of links (so that links can be drawn in different
   --  colors when an item is selected).

   ----------------------
   -- Graphic contexts --
   ----------------------

   function Get_Parents_Arrow
     (Browser : access General_Browser_Record) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Return the pixbuf that should be used for the button that displays the
   --  parents

   function Get_Children_Arrow
     (Browser : access General_Browser_Record) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Return the pixbuf that should be used for the button that displays the
   --  children

   ----------------------
   -- Contextual menus --
   ----------------------

   procedure Default_Browser_Context_Factory
     (Context      : in out GPS.Kernel.Selection_Context;
      Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Return the context to use for a contextual menu in the canvas.
   --  This version takes care of checking whether the user clicked on an item,
   --  and adds the standard menu entries

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
      Toolbar                  : Gtk.Hbutton_Box.Gtk_Hbutton_Box;

      Close_Pixmap             : Gdk.Pixbuf.Gdk_Pixbuf;
      Up_Arrow, Down_Arrow     : Gdk.Pixbuf.Gdk_Pixbuf;

      Use_Canvas_View : Boolean;

      View       : GPS_Canvas_View;
      Model      : List_Rtree.Rtree_Model;

      Canvas        : Gtkada.Canvas.Interactive_Canvas;
      Selected_Item : Gtkada.Canvas.Canvas_Item;
   end record;

   type Active_Area_Tree_Record;
   type Active_Area_Tree is access Active_Area_Tree_Record;
   type Active_Area_Tree_Array is array (Natural range <>) of Active_Area_Tree;
   type Active_Area_Tree_Array_Access is access Active_Area_Tree_Array;
   type Active_Area_Tree_Record is record
      Rectangle : Gdk.Rectangle.Gdk_Rectangle;
      Callback  : Active_Area_Cb;
      Children  : Active_Area_Tree_Array_Access;
   end record;

   overriding procedure Destroy (Item : in out Browser_Item_Record);
   overriding function On_Button_Click
     (Item  : access Browser_Item_Record;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean;
   overriding procedure Selected
     (Item        : access Browser_Item_Record;
      Canvas      : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Is_Selected : Boolean);
   overriding procedure Draw_Selected
     (Item : access Browser_Item_Record;
      Cr   : Cairo.Cairo_Context);
   --  See doc for inherited subprograms

   type Browser_Item_Record is new Gtkada.Canvas.Canvas_Item_Record with record
      Hide_Links : Boolean := False;
      Browser    : General_Browser;

      Title_Layout : Pango.Layout.Pango_Layout;
      --  Handling of the title bar. No title bar is shown if no title was
      --  set. In this case, Title_Layout is null.

      Active_Areas : Active_Area_Tree;

      Title_Coord  : Gdk.Rectangle.Gdk_Rectangle;
   end record;

   type Arrow_Item_Record is new Browser_Item_Record with record
      Parents_Shown, Children_Shown : Boolean := False;
      Parents_Cb, Children_Cb : Arrow_Item_Callback;
   end record;

   overriding procedure Redraw_Title_Bar
     (Item : access Arrow_Item_Record;
      Cr   : Cairo.Cairo_Context);
   overriding function Get_Last_Button_Number
     (Item : access Arrow_Item_Record) return Glib.Gint;
   overriding procedure Reset
     (Item : access Arrow_Item_Record;
      Parent_Removed, Child_Removed : Boolean);

   type Item_Active_Area_Callback is new Active_Area_Callback with record
      User_Data : Browser_Item;
      Cb        : Item_Active_Callback;
   end record;
   overriding function Call
     (Callback : Item_Active_Area_Callback;
      Event    : Gdk.Event.Gdk_Event_Button) return Boolean;
   --  See doc for inherited Call

   type Active_Area_Cb_Array_Access is access Active_Area_Cb_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Active_Area_Cb_Array, Active_Area_Cb_Array_Access);

   Empty_Cb_Array : constant Active_Area_Cb_Array (1 .. 0) := (others => null);

   type Xref_Line is record
      Text      : GNAT.Strings.String_Access;
      Callbacks : Active_Area_Cb_Array_Access;
      Length    : Natural;
   end record;
   type Xref_Line_Array is array (Natural range <>) of Xref_Line;
   type Xref_Line_Array_Access is access Xref_Line_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Xref_Line_Array, Xref_Line_Array_Access);

   type Xref_List is record
      Lines : Xref_Line_Array_Access;
   end record;

   pragma Inline (Get_Canvas);
end Browsers.Canvas;
