-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                             ACT-Europe                            --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Glib;          use Glib;
with Gdk.Cursor;    use Gdk.Cursor;
--  ??? needed with GtkAda 1.2 but not GtkAda 2.0
pragma Warnings (Off);
with Gdk.Types;     use Gdk.Types;
pragma Warnings (On);
with Gdk.Event;     use Gdk.Event;
with Gdk.Window;    use Gdk.Window;
with Gtk.Combo;     use Gtk.Combo;
with Gtk.Label;     use Gtk.Label;
with Gtk.List;      use Gtk.List;
with Gtk.List_Item; use Gtk.List_Item;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Handlers;  use Gtk.Handlers;
with Gtk.Menu;      use Gtk.Menu;
with Gdk.Main;      use Gdk.Main;

package body GUI_Utils is

   package Contextual_Callback is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record, Boolean, Contextual_Menu_Create);

   function Button_Press_For_Contextual_Menu
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Menu_Creation : Contextual_Menu_Create) return Boolean;
   --  Callback that pops up the contextual menu if needed

   ---------------------------
   -- Add_Unique_List_Entry --
   ---------------------------

   procedure Add_Unique_List_Entry
     (List : access Gtk.List.Gtk_List_Record'Class;
      Text  : String)
   is
      use Widget_List;

      Item     : Gtk_List_Item;
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
     (Window        : Gdk.Window.Gdk_Window;
      Busy          : Boolean := True;
      Force_Refresh : Boolean := False)
   is
      Cursor     : Gdk_Cursor;
   begin
      if Busy then
         Gdk_New (Cursor, Watch);
      else
         Gdk_New (Cursor, Left_Ptr);
      end if;

      Set_Cursor (Window, Cursor);
      Destroy (Cursor);

      if Force_Refresh then
         Gdk.Main.Flush;
      end if;
   end Set_Busy_Cursor;

   --------------------------------------
   -- Button_Press_For_Contextual_Menu --
   --------------------------------------

   function Button_Press_For_Contextual_Menu
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Menu_Creation : Contextual_Menu_Create) return Boolean
   is
      Menu : Gtk_Menu;
   begin
      if Get_Button (Event) = 3
        and then Get_Event_Type (Event) = Button_Press
      then
         Menu := Menu_Creation (Widget, Event);
         Show_All (Menu);
         Popup (Menu,
                Button        => Gdk.Event.Get_Button (Event),
                Activate_Time => Gdk.Event.Get_Time (Event));
         Emit_Stop_By_Name (Widget, "button_press_event");
         return True;
      end if;
      return False;
   end Button_Press_For_Contextual_Menu;

   ------------------------------
   -- Register_Contextual_Menu --
   ------------------------------

   procedure Register_Contextual_Menu
     (Widget : access Gtk_Widget_Record'Class;
      Menu_Create : Contextual_Menu_Create) is
   begin
      Add_Events (Widget, Button_Press_Mask or Button_Release_Mask);
      Contextual_Callback.Connect
        (Widget, "button_press_event",
         Contextual_Callback.To_Marshaller
         (Button_Press_For_Contextual_Menu'Access),
         Menu_Create);
   end Register_Contextual_Menu;

   ---------------------------
   -- User_Contextual_Menus --
   ---------------------------

   package body User_Contextual_Menus is

      --------------------------------------
      -- Button_Press_For_Contextual_Menu --
      --------------------------------------

      function Button_Press_For_Contextual_Menu
        (Widget : access Gtk_Widget_Record'Class;
         Event  : Gdk.Event.Gdk_Event;
         User   : Callback_User_Data) return Boolean
      is
         Menu : Gtk_Menu;
      begin
         if Get_Button (Event) = 3
           and then Get_Event_Type (Event) = Button_Press
         then
            Menu := User.Menu_Create (User.User, Event);
            Show_All (Menu);
            Popup (Menu,
                   Button        => Gdk.Event.Get_Button (Event),
                   Activate_Time => Gdk.Event.Get_Time (Event));
            Emit_Stop_By_Name (Widget, "button_press_event");
            return True;
         end if;
         return False;
      end Button_Press_For_Contextual_Menu;

      ------------------------------
      -- Register_Contextual_Menu --
      ------------------------------

      procedure Register_Contextual_Menu
        (Widget      : access Gtk.Widget.Gtk_Widget_Record'Class;
         User        : User_Data;
         Menu_Create : Contextual_Menu_Create) is
      begin
         Add_Events (Widget, Button_Press_Mask or Button_Release_Mask);
         Contextual_Callback.Connect
           (Widget, "button_press_event",
            Contextual_Callback.To_Marshaller
            (User_Contextual_Menus.Button_Press_For_Contextual_Menu'
              Unrestricted_Access),
            (Menu_Create => Menu_Create, User => User));
      end Register_Contextual_Menu;
   end User_Contextual_Menus;

end GUI_Utils;
