-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gdk.Event;
with Gdk.GC;
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

   procedure Gtk_New
     (Browser : out Glide_Browser;
      Mask    : Browser_Type_Mask;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new browser that can include items related to all types
   --  described in Browser_Type. For instance, if Browser_Type is Any_Browser,
   --  then Glide will include a single browser, and the class,
   --  dependency,... browsers will all be mixed.

   procedure Initialize
     (Browser : out Glide_Browser;
      Mask    : Browser_Type_Mask;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal Initialization function.

   function Get_Mask (Browser : access Glide_Browser_Record)
      return Browser_Type_Mask;
   --  Return the list of browser types supported by Browser.

   function Get_Canvas (Browser : access Glide_Browser_Record)
      return Gtkada.Canvas.Interactive_Canvas;
   --  Return the canvas embedded in Browser

   function To_Brower
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class)
      return Glide_Browser;
   --  Return the browser that contains Canvas.

   function Selected_Item (Browser : access Glide_Browser_Record)
      return Gtkada.Canvas.Canvas_Item;
   --  Return the currently selected item, or null if there is none.

   procedure Select_Item
     (Browser : access Glide_Browser_Record;
      Item    : access Gtkada.Canvas.Canvas_Item_Record'Class);
   --  Select Item.
   --  Depending on the specific time of Item, this might change its rendering
   --  and the one of the links to or from it.
   --  Note: This doesn't redraw the canvas, so you need to call Refresh_Canvas
   --  if you need to actually redraw it.

   ----------------------
   -- Graphic contexts --
   ----------------------

   function Get_Selected_Link_GC (Browser : access Glide_Browser_Record)
      return Gdk.GC.Gdk_GC;
   --  Return the graphic context to use to draw the links when one of their
   --  ends is selected.

   function Get_Selected_Item_GC (Browser : access Glide_Browser_Record)
      return Gdk.GC.Gdk_GC;
   --  Return the graphic context to use to draw the selected item

   function Get_Linked_Item_GC
     (Browser : access Glide_Browser_Record) return Gdk.GC.Gdk_GC;
   --  Return the graphic context to use to draw the items that are linked to
   --  the selected item

   ----------------------
   -- Contextual menus --
   ----------------------

   function Browser_Context_Factory
     (Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
      return Glide_Kernel.Selection_Context_Access;
   --  Return the context to use for a contextual menu in the canvas

private
   type Glide_Browser_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with
      record
         Canvas    : Gtkada.Canvas.Interactive_Canvas;
         Kernel    : Glide_Kernel.Kernel_Handle;
         Mask      : Browser_Type_Mask;

         Selected_Link_GC : Gdk.GC.Gdk_GC;
         Selected_Item_GC : Gdk.GC.Gdk_GC;
         Linked_Item_GC   : Gdk.GC.Gdk_GC;

         Selected_Item : Gtkada.Canvas.Canvas_Item;
      end record;

   pragma Inline (Get_Mask);
   pragma Inline (Get_Canvas);
   pragma Inline (Get_Selected_Link_GC);
   pragma Inline (Get_Selected_Item_GC);
   pragma Inline (Get_Linked_Item_GC);
end Browsers.Canvas;
