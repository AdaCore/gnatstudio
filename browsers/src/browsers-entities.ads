-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Src_Info.Queries;
with Glide_Kernel;
with Gdk.Event;
with Gtk.Menu;
with Gdk.Window;
with Browsers.Canvas;
with Gtkada.Canvas;
with Glib;
with Gdk.GC;
with Gdk.Pixbuf;
with Pango.Layout;

package Browsers.Entities is

   type Type_Browser_Record is new Browsers.Canvas.General_Browser_Record
     with private;
   type Type_Browser is access all Type_Browser_Record'Class;

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   ---------------
   -- Type item --
   ---------------

   type Type_Item_Record is new
     Browsers.Canvas.Arrow_Item_Record with private;
   type Type_Item is access all Type_Item_Record'Class;

   procedure Gtk_New
     (Item    : out Type_Item;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information);
   --  Open a new item in the browser that represents Entity.
   --  A copy of Entity is made, thus the caller should free Entity.

   procedure Initialize
     (Item    : access Type_Item_Record'Class;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information);
   --  Internal initialization function

   -----------------
   -- Parent link --
   -----------------

   type Parent_Link_Record is new Browsers.Canvas.Browser_Link_Record
     with null record;
   type Parent_Link is access all Parent_Link_Record'Class;

private
   function Get_Background_GC
     (Item : access Type_Item_Record) return Gdk.GC.Gdk_GC;
   procedure Resize_And_Draw
     (Item                        : access Type_Item_Record;
      Width, Height               : Glib.Gint;
      Width_Offset, Height_Offset : Glib.Gint;
      Xoffset, Yoffset            : in out Glib.Gint;
      Layout                  : access Pango.Layout.Pango_Layout_Record'Class);
   function Contextual_Factory
     (Item  : access Type_Item_Record;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Glide_Kernel.Selection_Context_Access;
   function Get_Last_Button_Number (Item : access Type_Item_Record)
      return Glib.Gint;
   procedure Redraw_Title_Bar (Item : access Type_Item_Record);
   procedure Highlight (Item : access Type_Item_Record);
   --  See doc for inherited subprograms

   procedure Draw_Straight_Line
     (Link : access Parent_Link_Record;
      Window : Gdk.Window.Gdk_Window;
      GC : Gdk.GC.Gdk_GC;
      Parent_Side : Gtkada.Canvas.Item_Side;
      X1, Y1 : Glib.Gint;
      Child_Side : Gtkada.Canvas.Item_Side;
      X2, Y2 : Glib.Gint);
   --  See doc for inherited subprogram

   type Type_Item_Record is new Browsers.Canvas.Arrow_Item_Record with record
      Entity : Src_Info.Queries.Entity_Information;
      Inherited_Primitives : Boolean := False;
   end record;

   type Type_Browser_Record is new Browsers.Canvas.General_Browser_Record
   with record
      Primitive_Button : Gdk.Pixbuf.Gdk_Pixbuf;
   end record;

end Browsers.Entities;
