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
with Gdk.Rectangle;
with Gtkada.Canvas;
with Glide_Kernel;
with Glib.Object;
with Gtk.Box;
with Gtk.Handlers;
with Gtk.Hbutton_Box;
with Gtk.Menu;
with Gtk.Stock;
with Gtk.Widget;
with Pango.Layout;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;

package Browsers.Canvas is

   Margin : constant := 5;
   --  Margin used when drawing the items, to leave space around the arrows and
   --  the actual contents of the item

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
      Create_Toolbar  : Boolean;
      Parents_Pixmap  : String := Gtk.Stock.Stock_Go_Back;
      Children_Pixmap : String := Gtk.Stock.Stock_Go_Forward);
   --  Initialize a new browser.
   --  It sets up all the contextual menu for this browser, as well as the key
   --  shortcuts to manipulate the browser.
   --  If Create_Toolbar is True, then a button_bar is added at the bottom.
   --  Parents_Pixmap and Children_Pixmap are the pixmaps to use in the title
   --  bar to get access to the parents/ancestors of an item or its children.

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

   procedure Layout
     (Browser : access General_Browser_Record;
      Force : Boolean := False);
   --  Recompute the layout of items in the browser.
   --  If Force is true, then even the items that have been moved manually by
   --  the user are recomputed.

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
                  Event    : Gdk.Event.Gdk_Event)
     return Boolean is abstract;
   --  A callback for the active areas. Event is the mouse event that started
   --  the chain that lead to callback.
   --  This type provides an easy encapsulation for any user data you might
   --  need.
   --  Should return True if the event was handler, False otherwise. In the
   --  latter case, the even is transmitted to the parent area

   procedure Destroy (Callback : in out Active_Area_Callback);
   --  Destroy the callback

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Active_Area_Callback'Class, Active_Area_Cb);

   -----------
   -- Items --
   -----------

   type Browser_Item_Record is new Gtkada.Canvas.Buffered_Item_Record
     with private;
   type Browser_Item is access all Browser_Item_Record'Class;
   --  The type of items that are put in the canvas. They are associated with
   --  contextual menus, and also allows hiding the links to and from this
   --  item.

   package Item_Cb is new Gtk.Handlers.User_Callback
     (Gtk.Widget.Gtk_Widget_Record, Browser_Item);

   procedure Initialize
     (Item    : access Browser_Item_Record'Class;
      Browser : access General_Browser_Record'Class);
   --  Associate the item with a browser.

   procedure Set_Title
     (Item : access Browser_Item_Record'Class;  Title : String := "");
   --  Set a title for the item. This is displayed in a special title bar, with
   --  a different background color.
   --  If Title is the empty string, no title bar is shown.
   --  Title must be UTF8-encoded.

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
     (Item           : access Browser_Item_Record'Class);
   --  Non dispatching variant of the Resize_And_Draw.
   --  You need to refresh the screen by calling either Item_Updated or
   --  Refresh_Canvas.

   procedure Highlight (Item : access Browser_Item_Record);
   --  Highlight the item, based on its selection status. This method is
   --  automatically called when the selection status has changed.
   --  The default is simply to redraw the item with a different background
   --  (from Get_Background_GC). It doesn't need to call Item_Updated or
   --  Refresh_Canvas, this is done automatically.

   function Get_Background_GC
     (Item : access Browser_Item_Record) return Gdk.GC.Gdk_GC;
   --  Return the graphic context to use for the background of the item. This
   --  can be used to draw the selected item with a different color for
   --  instance (the default behavior)

   function Get_Title_Background_GC
     (Item : access Browser_Item_Record) return Gdk.GC.Gdk_GC;
   --  Return the graphic context to use for the background of the title bar.

   procedure Resize_And_Draw
     (Item             : access Browser_Item_Record;
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
   --   - Call the parent's Size_Request_And_Draw procedure. This will
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

   procedure Redraw_Title_Bar (Item : access Browser_Item_Record);
   --  This function should redraw the title bar buttons, after calling the
   --  inherited subprogram.
   --
   --  This procedure doesn't need to reset the active areas for the buttons,
   --  this is done automatically.

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

   procedure Add_Active_Area
     (Item      : access Browser_Item_Record;
      Rectangle : Gdk.Rectangle.Gdk_Rectangle;
      Callback  : Active_Area_Callback'Class);
   --  Define a new clickable active area in the item. Callback will be called
   --  whenever the user clicks in Rectangle, provided there is no smaller area
   --  that also contains the click location.

   procedure Activate
     (Item  : access Browser_Item_Record;
      Event : Gdk.Event.Gdk_Event);
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
      Length1  : Natural        := Natural'Last;
      Callback : Active_Area_Cb := null);
   --  Add a new line that will be displayed in a layout.
   --  Str can contain one substring delimited by @...@. When the user
   --  clicks on that zone, Callback will be called. It must be UTF8-encoded.
   --  Length1 is the number of characters in the first column. The first
   --  character in the second column will always be aligned. Set to
   --  Natural'Last if there is only one column.

   procedure Display_Lines
     (Item          : access Browser_Item_Record'Class;
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

   ---------------
   -- Item area --
   ---------------

   type Item_Active_Callback is access
     procedure (Event : Gdk.Event.Gdk_Event;
                User : access Browser_Item_Record'Class);
   type Item_Active_Area_Callback is new Active_Area_Callback with private;
   --  A special instanciation of the callback for cases where the user data is
   --  a widget.

   function Build
     (Cb : Item_Active_Callback;
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
     (Item    : access Arrow_Item_Record'Class;
      Browser : access General_Browser_Record'Class;
      Title   : String;
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
   --  Inidicate wether all the parents or all the children are shown.

   -----------
   -- Links --
   -----------

   procedure Draw_Link
     (Canvas      : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Link        : access Browser_Link_Record;
      Invert_Mode : Boolean;
      GC          : Gdk.GC.Gdk_GC;
      Edge_Number : Glib.Gint);
   --  Override the drawing of links (so that links can be drawn in different
   --  colors when an item is selected).

   ----------------------
   -- Graphic contexts --
   ----------------------

   function Get_Default_Item_Background_GC
     (Browser : access General_Browser_Record) return Gdk.GC.Gdk_GC;
   --  Return the default graphic context to use for unselected items'
   --  background.

   function Get_Selected_Item_GC
     (Browser : access General_Browser_Record) return Gdk.GC.Gdk_GC;
   --  Return the graphic context for the background of selected items

   function Get_Parent_Linked_Item_GC
     (Browser : access General_Browser_Record) return Gdk.GC.Gdk_GC;
   --  Return the graphic context for the background of items that are parent
   --  of the selectd item.

   function Get_Child_Linked_Item_GC
     (Browser : access General_Browser_Record) return Gdk.GC.Gdk_GC;
   --  Return the graphic context for the background of items that are children
   --  of the selectd item.

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
      Unselected_Link_Color : Gdk.Color.Gdk_Color;
      Default_Item_GC       : Gdk.GC.Gdk_GC;
      Selected_Item_GC      : Gdk.GC.Gdk_GC;
      Parent_Linked_Item_GC : Gdk.GC.Gdk_GC;
      Child_Linked_Item_GC  : Gdk.GC.Gdk_GC;
      Text_GC               : Gdk.GC.Gdk_GC;
      Title_GC              : Gdk.GC.Gdk_GC;

      Selected_Item : Gtkada.Canvas.Canvas_Item;

      Close_Pixmap : Gdk.Pixbuf.Gdk_Pixbuf;
      Up_Arrow, Down_Arrow : Gdk.Pixbuf.Gdk_Pixbuf;
   end record;

   type Browser_Link_Record is new Gtkada.Canvas.Canvas_Link_Record
     with null record;

   type Active_Area_Tree_Record;
   type Active_Area_Tree is access Active_Area_Tree_Record;
   type Active_Area_Tree_Array is array (Natural range <>) of Active_Area_Tree;
   type Active_Area_Tree_Array_Access is access Active_Area_Tree_Array;
   type Active_Area_Tree_Record is record
      Rectangle : Gdk.Rectangle.Gdk_Rectangle;
      Callback  : Active_Area_Cb;
      Children  : Active_Area_Tree_Array_Access;
   end record;

   procedure Destroy (Item : in out Browser_Item_Record);
   procedure On_Button_Click
     (Item  : access Browser_Item_Record;
      Event : Gdk.Event.Gdk_Event_Button);
   procedure Selected
     (Item        : access Browser_Item_Record;
      Canvas      : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Is_Selected : Boolean);
   --  See doc for inherited subprograms

   type Browser_Item_Record is new Gtkada.Canvas.Buffered_Item_Record
   with record
      Hide_Links : Boolean := False;
      Browser    : General_Browser;

      Title_Layout : Pango.Layout.Pango_Layout;
      --  Handling of the title bar. No title bar is shown if no title was
      --  set. In this case, Title_Layout is null.

      Active_Areas : Active_Area_Tree;

      Title_X, Title_Width, Title_Y : Glib.Gint;
   end record;

   type Arrow_Item_Record is new Browser_Item_Record with record
      Parents_Shown, Children_Shown : Boolean := False;
      Parents_Cb, Children_Cb : Arrow_Item_Callback;
   end record;

   procedure Redraw_Title_Bar (Item : access Arrow_Item_Record);
   function Get_Last_Button_Number
     (Item : access Arrow_Item_Record) return Glib.Gint;
   procedure Reset
     (Item : access Arrow_Item_Record;
      Parent_Removed, Child_Removed : Boolean);

   type Item_Active_Area_Callback is new Active_Area_Callback with record
      User_Data : Browser_Item;
      Cb        : Item_Active_Callback;
   end record;
   function Call
     (Callback : Item_Active_Area_Callback;
      Event    : Gdk.Event.Gdk_Event) return Boolean;
   --  See doc for inherited Call

   type Active_Area_Cb_Array is array (Natural range <>) of Active_Area_Cb;
   type Active_Area_Cb_Array_Access is access Active_Area_Cb_Array;
   type Natural_Array is array (Natural range <>) of Natural;
   type Natural_Array_Access is access Natural_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Active_Area_Cb_Array, Active_Area_Cb_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Strings.String_List, GNAT.Strings.String_List_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Natural_Array, Natural_Array_Access);

   type Xref_List is record
      Lines      : GNAT.Strings.String_List_Access;
      Callbacks  : Active_Area_Cb_Array_Access;
      Lengths    : Natural_Array_Access;
   end record;

   pragma Inline (Get_Canvas);
end Browsers.Canvas;
