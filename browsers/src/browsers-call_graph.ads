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
with Browsers.Canvas;
with Src_Info.Queries;

package Browsers.Call_Graph is

   type Call_Graph_Browser_Record is new Browsers.Canvas.Glide_Browser_Record
     with null record;
   type Call_Graph_Browser is access all Call_Graph_Browser_Record'Class;

   procedure Register_Module;
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
   --  You shouldn't free Entity yourself, this will be taken care of by the
   --  item itself.

   procedure Initialize
     (Item    : access Entity_Item_Record'Class;
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information);
   --  Internal initialization function

   procedure Refresh
     (Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Item    : access Entity_Item_Record);
   --  Redraw the item to its double buffer

   procedure Destroy (Item : in out Entity_Item_Record);
   --  Free the memory occupied by the item. This is called automatically when
   --  the item is removed from the canvas.

   procedure On_Button_Click
     (Item  : access Entity_Item_Record;
      Event : Gdk.Event.Gdk_Event_Button);

private
   type Entity_Item_Record is new Browsers.Canvas.Glide_Browser_Item_Record
   with record
      Browser     : Browsers.Canvas.Glide_Browser;
      Entity      : Src_Info.Queries.Entity_Information;
   end record;

end Browsers.Call_Graph;
