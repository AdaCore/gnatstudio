-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;
with Gdk.Event;
with Gdk.GC;
with Gdk.Font;
with Gdk.Window;
with Gtkada.Canvas;
with Glide_Kernel;
with Glib.Object;
with Gtk.Menu;
with Gtk.Scrolled_Window;
with Gtk.Widget;

package Browsers.Canvas is

   type Glide_Browser_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with private;
   type Glide_Browser is access all Glide_Browser_Record'Class;

   type Glide_Browser_Link_Record is new Gtkada.Canvas.Canvas_Link_Record
     with private;
   type Glide_Browser_Link is access all Glide_Browser_Link_Record'Class;
   --  The type of links that are put in the canvas. These are automatically
   --  highlighted if they connect a selected item to another one.

   procedure Initialize
     (Browser : access Glide_Browser_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialize a new browser.
   --  It sets up all the contextual menu for this browser, as well as the key
   --  shortcuts to manipulate the browser.

   function Get_Canvas (Browser : access Glide_Browser_Record)
      return Gtkada.Canvas.Interactive_Canvas;
   --  Return the canvas embedded in Browser

   function Get_Kernel (Browser : access Glide_Browser_Record)
      return Glide_Kernel.Kernel_Handle;
   --  Return the kernel associated with the browser

   function To_Brower
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class)
      return Glide_Browser;
   --  Return the browser that contains Canvas.

   function Selected_Item (Browser : access Glide_Browser_Record)
      return Gtkada.Canvas.Canvas_Item;
   --  Return the currently selected item, or null if there is none.

   procedure Select_Item
     (Browser : access Glide_Browser_Record;
      Item    : access Gtkada.Canvas.Canvas_Item_Record'Class;
      Refresh_Items : Boolean := False);
   --  Select Item.
   --  If Refresh_Items is True, this will redraw all the items in the canvas,
   --  by calling their Refresh subprogram

   -----------
   -- Items --
   -----------

   type Glide_Browser_Item_Record is new Gtkada.Canvas.Buffered_Item_Record
     with private;
   type Glide_Browser_Item is access all Glide_Browser_Item_Record'Class;
   --  The type of items that are put in the canvas. They are associated with
   --  contextual menus, and also allows hiding the links to and from this
   --  item.

   function Contextual_Factory
     (Item  : access Glide_Browser_Item_Record;
      Browser : access Glide_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Glide_Kernel.Selection_Context_Access;
   --  Return the selection context to use when an item is clicked on.
   --  The coordinates in Event are relative to the upper-left corner of the
   --  item.
   --  If there are specific contextual menu entries that should be used for
   --  this item, they should be added to Menu.
   --  null should be return if there is no special contextual for this
   --  item. In that case, the context for the browser itself will be used.

   procedure Refresh (Browser : access Glide_Browser_Record'Class;
                      Item    : access Glide_Browser_Item_Record);
   --  Redraw the item to its double buffer.
   --  This is used when changing for instance the background color of items.
   --  By default, it only redraws the background color.

   -----------
   -- Links --
   -----------

   procedure Draw_Link
     (Canvas      : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Link        : access Glide_Browser_Link_Record;
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
     (Browser : access Glide_Browser_Record) return Gdk.GC.Gdk_GC;
   --  Return the graphic context to use to draw the text in the items.

   function Get_Text_Font
     (Browser : access Glide_Browser_Record) return Gdk.Font.Gdk_Font;
   --  Return the font to use to draw the text in the items.

   procedure Draw_Item_Background
     (Browser : access Glide_Browser_Record;
      Item    : access Gtkada.Canvas.Buffered_Item_Record'Class);
   --  Draw the background of the item in the appropriate color.

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
   type Glide_Browser_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with
      record
         Canvas    : Gtkada.Canvas.Interactive_Canvas;
         Kernel    : Glide_Kernel.Kernel_Handle;

         Selected_Link_GC      : Gdk.GC.Gdk_GC;
         Selected_Item_GC      : Gdk.GC.Gdk_GC;
         Parent_Linked_Item_GC : Gdk.GC.Gdk_GC;
         Child_Linked_Item_GC  : Gdk.GC.Gdk_GC;
         Text_GC               : Gdk.GC.Gdk_GC;
         Text_Font             : Gdk.Font.Gdk_Font;

         Selected_Item : Gtkada.Canvas.Canvas_Item;
      end record;

   type Glide_Browser_Link_Record is new Gtkada.Canvas.Canvas_Link_Record
     with null record;

   type Glide_Browser_Item_Record is new Gtkada.Canvas.Buffered_Item_Record
   with record
      Hide_Links : Boolean := False;
   end record;

   pragma Inline (Get_Canvas);
end Browsers.Canvas;
