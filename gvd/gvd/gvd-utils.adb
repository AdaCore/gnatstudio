-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                             ACT-Europe                            --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Gdk.Cursor;    use Gdk.Cursor;
with Gdk.Types;     use Gdk.Types;
with Gdk.Window;    use Gdk.Window;
with Gtk.Combo;     use Gtk.Combo;
with Gtk.Label;     use Gtk.Label;
with Gtk.List;      use Gtk.List;
with Gtk.List_Item; use Gtk.List_Item;
with Gtk.Widget;    use Gtk.Widget;

package body Odd.Utils is

   ---------------------------
   -- Add_Unique_List_Entry --
   ---------------------------

   procedure Add_Unique_List_Entry
     (List : access Gtk.List.Gtk_List_Record'Class;
      Text  : String)
   is
      use Widget_List;
      Item : Gtk_List_Item;
      Children : Widget_List.Glist := Get_Children (List);
   begin

      --  Check whether Text is already in the list
      while Children /= Null_List loop
         Item := Gtk_List_Item (Get_Data (Children));
         if Get (Gtk_Label (Get_Child (Item))) = Text then
            return;
         end if;
         Children := Next (Children);
      end loop;

      --  Add the new item in the list
      Gtk_New (Item, Text);
      Show (Item);
      Add (List, Item);
   end Add_Unique_List_Entry;

   ----------------------------
   -- Add_Unique_Combo_Entry --
   ----------------------------

   procedure Add_Unique_Combo_Entry
     (Combo : access Gtk.Combo.Gtk_Combo_Record'Class;
      Text  : String) is
   begin
      Add_Unique_List_Entry (Get_List (Combo), Text);
   end Add_Unique_Combo_Entry;

   ---------------------
   -- Set_Busy_Cursor --
   ---------------------

   procedure Set_Busy_Cursor
     (Window   : Gdk.Window.Gdk_Window;
      Busy     : Boolean := True)
   is
      Cursor   : Gdk_Cursor;
   begin
      if Busy then
         Gdk_New (Cursor, Gdk.Types.Watch);
      else
         Gdk_New (Cursor, Gdk.Types.Left_Ptr);
      end if;
      Set_Cursor (Window, Cursor);
      Destroy (Cursor);
   end Set_Busy_Cursor;

end Odd.Utils;
