-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001-2002                    --
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

with Gdk.Event;
with Gdk.Pixbuf;
with Gtk.Main;
with Gtk.Menu;
with Glide_Kernel;
with Browsers.Canvas;
with Src_Info.Queries;

package Browsers.Call_Graph is

   type Call_Graph_Browser_Record is new Browsers.Canvas.Glide_Browser_Record
     with private;
   type Call_Graph_Browser is access all Call_Graph_Browser_Record'Class;

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   ------------------
   -- Entity items --
   ------------------

   type Entity_Item_Record is new Browsers.Canvas.Glide_Browser_Item_Record
     with private;
   type Entity_Item is access all Entity_Item_Record'Class;

   procedure Gtk_New
     (Item    : out Entity_Item;
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information);
   --  Create a new entity item.

   procedure Initialize
     (Item    : access Entity_Item_Record'Class;
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information);
   --  Internal initialization function

   procedure Refresh
     (Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Item    : access Entity_Item_Record);
   --  Redraw the item to its double buffer

   procedure Reset
     (Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Item    : access Entity_Item_Record);
   --  Reset the internal state of the item

   procedure Destroy (Item : in out Entity_Item_Record);
   --  Free the memory occupied by the item. This is called automatically when
   --  the item is removed from the canvas.

   procedure On_Button_Click
     (Item  : access Entity_Item_Record;
      Event : Gdk.Event.Gdk_Event_Button);
   --  Handle button clicks on the item

   function Contextual_Factory
     (Item  : access Entity_Item_Record;
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Glide_Kernel.Selection_Context_Access;
   --  Return the context to use for this item


private
   type Entity_Item_Record is new Browsers.Canvas.Glide_Browser_Item_Record
   with record
      Browser     : Browsers.Canvas.Glide_Browser;
      Entity      : Src_Info.Queries.Entity_Information;

      From_Parsed, To_Parsed : Boolean := False;
      --  These two booleans are set to True when the parents of the item have
      --  been fully parsed (ie all the subprograms that call Entity), or when
      --  all the children have been parsed.
   end record;

   type Call_Graph_Browser_Record is new Browsers.Canvas.Glide_Browser_Record
     with record
        Idle_Id                 : Gtk.Main.Idle_Handler_Id;
        Left_Arrow, Right_Arrow : Gdk.Pixbuf.Gdk_Pixbuf;
     end record;
end Browsers.Call_Graph;
