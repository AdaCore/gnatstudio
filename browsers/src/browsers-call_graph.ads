-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                       Copyright (C) 2001-2002                     --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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
with Gdk.Event;
with Gdk.GC;
with Gdk.Window;
with Gtk.Main;
with Gtk.Menu;
with Pango.Layout;
with Gtkada.Canvas;
with Glide_Kernel;
with Browsers.Canvas;
with Src_Info.Queries;

package Browsers.Call_Graph is

   type Call_Graph_Browser_Record is new Browsers.Canvas.General_Browser_Record
     with private;
   type Call_Graph_Browser is access all Call_Graph_Browser_Record'Class;

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   ------------------
   -- Entity items --
   ------------------

   type Entity_Item_Record is new Browsers.Canvas.Arrow_Item_Record
     with private;
   type Entity_Item is access all Entity_Item_Record'Class;

   procedure Gtk_New
     (Item    : out Entity_Item;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information;
      May_Have_To_Dependencies : Boolean);
   --  Create a new entity item.
   --  If May_Have_To_Dependencies is False, the right arrow will not be
   --  displayed in the items.

   procedure Initialize
     (Item    : access Entity_Item_Record'Class;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information;
      May_Have_To_Dependencies : Boolean);
   --  Internal initialization function

   procedure Destroy (Item : in out Entity_Item_Record);
   --  Free the memory occupied by the item. This is called automatically when
   --  the item is removed from the canvas.

   function Contextual_Factory
     (Item  : access Entity_Item_Record;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Glide_Kernel.Selection_Context_Access;
   --  Return the context to use for this item

   --------------------
   -- Renaming links --
   --------------------

   type Renaming_Link_Record is new Browsers.Canvas.Browser_Link_Record
     with private;
   type Renaming_Link is access all Renaming_Link_Record'Class;
   --  The tpye of link used between an entity and the entities that rename
   --  it.
   --  A renaming link should always be created from the renaming entity to the
   --  renamed entity.

   procedure Draw_Link
     (Canvas      : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Link        : access Renaming_Link_Record;
      Window      : Gdk.Window.Gdk_Window;
      Invert_Mode : Boolean;
      GC          : Gdk.GC.Gdk_GC;
      Edge_Number : Glib.Gint);
   --  Override the default drawing procedure for links

private
   procedure Resize_And_Draw
     (Item                        : access Entity_Item_Record;
      Width, Height               : Glib.Gint;
      Width_Offset, Height_Offset : Glib.Gint;
      Xoffset, Yoffset            : in out Glib.Gint;
      Layout                  : access Pango.Layout.Pango_Layout_Record'Class);
   --  See doc for inherited subprogram

   type Entity_Item_Record is new Browsers.Canvas.Arrow_Item_Record
   with record
      Entity : Src_Info.Queries.Entity_Information;
   end record;

   type Call_Graph_Browser_Record is new
     Browsers.Canvas.General_Browser_Record
   with record
      Idle_Id : Gtk.Main.Idle_Handler_Id;
   end record;

   type Renaming_Link_Record is new Browsers.Canvas.Browser_Link_Record
     with null record;
end Browsers.Call_Graph;
