-----------------------------------------------------------------------
--                                GPS                                --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is  free software;  you can redistribute it and/or modify  it --
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

with Glib;
with Gdk.Color;
with Gdk.Event;
with Gdk.GC;
with Gdk.Pixbuf;
with Gdk.Window;
with Gtkada.Canvas;
with Glide_Kernel;
with Glib.Object;
with Gtk.Box;
with Gtk.Hbutton_Box;
with Gtk.Menu;
with Gtk.Widget;
with Pango.Layout;

package Browsers.Canvas is

   type General_Browser_Record is new Gtk.Box.Gtk_Box_Record with private;
   type General_Browser is access all General_Browser_Record'Class;

   type Browser_Link_Record is new Gtkada.Canvas.Canvas_Link_Record
     with private;
   type Browser_Link is access all Browser_Link_Record'Class;
   --  The type of links that are put in the canvas. These are automatically
   --  highlighted if they connect a selected item to another one.

   procedure Initialize
     (Browser : access General_Browser_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Create_Toolbar : Boolean);
   --  Initialize a new browser.
   --  It sets up all the contextual menu for this browser, as well as the key
   --  shortcuts to manipulate the browser.
   --  If Create_Toolbar is True, then a button_bar is added at the bottom.

   function Get_Toolbar (Browser : access General_Browser_Record)
      return Gtk.Hbutton_Box.Gtk_Hbutton_Box;
   --  Return the toolbar at the bottom of the browser. This returns null if no
   --  toolbar was created in the call to Initialize.

   procedure Setup_Default_Toolbar (Browser : access General_Browser_Record);
   --  Add the default buttons to the toolbar of browser. Nothing is done if no
   --  toolbar was created.

   function Get_Canvas (Browser : access General_Browser_Record)
      return Gtkada.Canvas.Interactive_Canvas;
   --  Return the canvas embedded in Browser

   function Get_Kernel (Browser : access General_Browser_Record)
      return Glide_Kernel.Kernel_Handle;
   --  Return the kernel associated with the browser

   function To_Brower
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class)
      return General_Browser;
   --  Return the browser that contains Canvas.

   function Selected_Item (Browser : access General_Browser_Record)
      return Gtkada.Canvas.Canvas_Item;
   --  Return the currently selected item, or null if there is none.

   procedure Select_Item
     (Browser : access General_Browser_Record;
      Item    : access Gtkada.Canvas.Canvas_Item_Record'Class;
      Refresh_Items : Boolean := False);
   --  Select Item.
   --  If Refresh_Items is True, this will redraw all the items in the canvas,
   --  by calling their Refresh subprogram

   -----------
   -- Items --
   -----------

   type Browser_Item_Record is new Gtkada.Canvas.Buffered_Item_Record
     with private;
   type Browser_Item is access all Browser_Item_Record'Class;
   --  The type of items that are put in the canvas. They are associated with
   --  contextual menus, and also allows hiding the links to and from this
   --  item.

   procedure Initialize
     (Item    : access Browser_Item_Record'Class;
      Browser : access General_Browser_Record'Class);
   --  Associate the item with a browser.

   function Contextual_Factory
     (Item  : access Browser_Item_Record;
      Browser : access General_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Glide_Kernel.Selection_Context_Access;
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
   --  Glide_Kernel.Get_Current_Context.

   procedure Refresh
     (Browser : access General_Browser_Record'Class;
      Item    : access Browser_Item_Record;
      Xoffset, Yoffset : Glib.Gint := 0);
   --  This procedure should redraw the item to its double buffer, and
   --  recompute its size if necessary. The drawing should be offset by
   --  *xoffset, yoffset", so that tagged types extending the current one can
   --  add drawings or text around the item.
   --  This is used when changing for instance the background color of items.
   --  By default, it only redraws the background color.
   --  This is called automatically when the status of the item has changed
   --  (for instance, when the item was selected).
   --
   --  This subprogram needn't call Item_Updated itself. However, if you are
   --  calling Refresh from your own subprograms, you need to refresh the
   --  screen either through a call to Item_Updated or a call to
   --  Refresh_Canvas.
   --
   --  The default behavior is the following:
   --  Draw the background of the item in the appropriate color, depending on
   --  the selection status.
   --  This will resize the item if it has never been resized before. However,
   --  it won't recompute its size every time, for efficiency reasons, so if
   --  you ever need to change the size, you should do call Set_Screen_Size
   --  directly.

   procedure Size_Request
     (Item   : access Browser_Item_Record;
      Width  : out Glib.Gint;
      Height : out Glib.Gint);
   --  This procedure should compute the preferred size for the item.
   --  It should not actually resize the item itself.

   procedure Reset (Browser : access General_Browser_Record'Class;
                    Item : access Browser_Item_Record);
   --  Reset the internal state of the item, as if it had never been expanded,
   --  analyzed,... This is called for instance after the item has been defined
   --  as the root of the canvas (and thus all other items have been removed).
   --  It doesn't need to redraw the item

   function Get_Browser (Item : access Browser_Item_Record'Class)
      return General_Browser;
   --  Return the browser associated with this item

   ---------------
   -- Text_Item --
   ---------------

   type Text_Item_Record is new Browser_Item_Record with private;
   type Text_Item is access all Text_Item_Record'Class;
   --  A special kind of item that contains some text. The text is displayed as
   --  a single block, centered in the item.

   procedure Initialize
     (Item    : access Text_Item_Record'Class;
      Browser : access General_Browser_Record'Class;
      Text    : String);
   --  Initialize a new item, that displays Text. Text can be a multi-line text

   ---------------------------
   -- Text_Item with arrows --
   ---------------------------
   --  This item is a standard text item, but displays one arrow on each side
   --  of the text. Clicking on any of these arrow triggers a call to one of
   --  the primitive subprograms.

   type Text_Item_With_Arrows_Record is abstract new
     Text_Item_Record with private;
   type Text_Item_With_Arrows is access all Text_Item_With_Arrows_Record'Class;

   function Get_Left_Arrow (Item : access Text_Item_With_Arrows_Record)
      return Boolean;
   --  Return True if the left arrow is displayed for this item

   function Get_Right_Arrow (Item : access Text_Item_With_Arrows_Record)
      return Boolean;
   --  Return True if the right arrow is displayed for this item

   procedure Set_Left_Arrow
     (Item : access Text_Item_With_Arrows_Record; Display : Boolean);
   --  Change the status of the left arrow

   procedure Set_Right_Arrow
     (Item : access Text_Item_With_Arrows_Record; Display : Boolean);
   --  Change the status of the right arrow

   procedure Button_Click_On_Left (Item : access Text_Item_With_Arrows_Record)
      is abstract;
   --  Handles button clicks on the left arrow.
   --  This is not called if you override On_Button_Click

   procedure Button_Click_On_Right (Item : access Text_Item_With_Arrows_Record)
      is abstract;
   --  Handles button clicks on the right arrow
   --  This is not called if you override On_Button_Click

   -----------
   -- Links --
   -----------

   procedure Draw_Link
     (Canvas      : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Link        : access Browser_Link_Record;
      Window      : Gdk.Window.Gdk_Window;
      Invert_Mode : Boolean;
      GC          : Gdk.GC.Gdk_GC;
      Edge_Number : Glib.Gint);
   --  Override the drawing of links (so that links can be drawn in different
   --  colors when an item is selected).

   ----------------------
   -- Graphic contexts --
   ----------------------

   function Get_Text_GC
     (Browser : access General_Browser_Record) return Gdk.GC.Gdk_GC;
   --  Return the graphic context to use to draw the text in the items.

   ----------------------
   -- Contextual menus --
   ----------------------

   function Default_Browser_Context_Factory
     (Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
      return Glide_Kernel.Selection_Context_Access;
   --  Return the context to use for a contextual menu in the canvas.
   --  This version takes care of checking whether the user clicked on an item,
   --  and adds the standard menu entries

private
   type General_Browser_Record is new Gtk.Box.Gtk_Box_Record with record
      Canvas    : Gtkada.Canvas.Interactive_Canvas;
      Kernel    : Glide_Kernel.Kernel_Handle;
      Toolbar   : Gtk.Hbutton_Box.Gtk_Hbutton_Box;

      Selected_Link_Color   : Gdk.Color.Gdk_Color;
      Default_Item_GC       : Gdk.GC.Gdk_GC;
      Selected_Item_GC      : Gdk.GC.Gdk_GC;
      Parent_Linked_Item_GC : Gdk.GC.Gdk_GC;
      Child_Linked_Item_GC  : Gdk.GC.Gdk_GC;
      Text_GC               : Gdk.GC.Gdk_GC;

      Selected_Item : Gtkada.Canvas.Canvas_Item;

      Left_Arrow, Right_Arrow : Gdk.Pixbuf.Gdk_Pixbuf;
   end record;

   type Browser_Link_Record is new Gtkada.Canvas.Canvas_Link_Record
     with null record;

   type Browser_Item_Record is new Gtkada.Canvas.Buffered_Item_Record
   with record
      Hide_Links : Boolean := False;
      Browser     : General_Browser;
   end record;

   type Text_Item_Record is new Browser_Item_Record  with
   record
      Layout : Pango.Layout.Pango_Layout;
   end record;

   procedure Size_Request
     (Item   : access Text_Item_Record;
      Width  : out Glib.Gint;
      Height : out Glib.Gint);
   procedure Destroy (Item : in out Text_Item_Record);
   procedure Refresh
     (Browser : access General_Browser_Record'Class;
      Item    : access Text_Item_Record;
      Xoffset, Yoffset : Glib.Gint := 0);
   --  See doc for inherited subprograms

   type Text_Item_With_Arrows_Record is abstract new
     Text_Item_Record with
   record
      Left_Arrow, Right_Arrow : Boolean := True;
   end record;

   procedure Size_Request
     (Item   : access Text_Item_With_Arrows_Record;
      Width  : out Glib.Gint;
      Height : out Glib.Gint);
   procedure On_Button_Click
     (Item  : access Text_Item_With_Arrows_Record;
      Event : Gdk.Event.Gdk_Event_Button);
   --  Handles button clicks on the item

   procedure Reset (Browser : access General_Browser_Record'Class;
                    Item : access Text_Item_With_Arrows_Record);
   procedure Refresh
     (Browser : access General_Browser_Record'Class;
      Item    : access Text_Item_With_Arrows_Record;
      Xoffset, Yoffset : Glib.Gint := 0);
   --  See doc for inherited Reset

   pragma Inline (Get_Canvas);
end Browsers.Canvas;
