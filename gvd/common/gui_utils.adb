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
with Gtk.Clist;     use Gtk.Clist;
with Gtk.Combo;     use Gtk.Combo;
with Gtk.Container; use Gtk.Container;
with Gtk.GEntry;    use Gtk.GEntry;
with Gtk.Label;     use Gtk.Label;
with Gtk.List;      use Gtk.List;
with Gtk.List_Item; use Gtk.List_Item;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Handlers;  use Gtk.Handlers;
with Gtk.Menu;      use Gtk.Menu;
with Gdk.Main;      use Gdk.Main;

package body GUI_Utils is

   type Contextual_Menu_Data is record
      Create  : Contextual_Menu_Create;
      Destroy : Contextual_Menu_Destroy;
      Widget  : Gtk_Widget;
   end record;

   package Contextual_Callback is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record, Boolean, Contextual_Menu_Data);

   function Button_Press_For_Contextual_Menu
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Data   : Contextual_Menu_Data) return Boolean;
   --  Callback that pops up the contextual menu if needed

   function Unmap_Menu
     (Menu : access Gtk_Widget_Record'Class;
      Data : Contextual_Menu_Data) return Boolean;

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

   -----------------------
   -- Get_Index_In_List --
   -----------------------

   function Get_Index_In_List
     (Combo : access Gtk.Combo.Gtk_Combo_Record'Class) return Integer
   is
      use type Widget_List.Glist;
      Entry_Text : constant String := Get_Text (Get_Entry (Combo));
      Children  : Widget_List.Glist := Get_Children (Get_List (Combo));
      Item      : Gtk_List_Item;
      Label     : Gtk_Label;
      Index     : Integer := -1;
   begin
      --  We have to search explicitely in the list, since the selection might
      --  be different from what is actually displayed in the list, for
      --  instance if the text in the entry was modified programmatically.

      while Children /= Widget_List.Null_List loop
         Item := Gtk_List_Item (Widget_List.Get_Data (Children));
         Label := Gtk_Label (Get_Child (Item));
         Index := Index + 1;

         if Get_Text (Label) = Entry_Text then
            return Index;
         end if;

         Children := Widget_List.Next (Children);
      end loop;

      return -1;
   end Get_Index_In_List;

   ----------------------------
   -- Propagate_Expose_Event --
   ----------------------------

   procedure Propagate_Expose_Event
     (Container : access Gtk.Container.Gtk_Container_Record'Class;
      Event     : Gdk.Event.Gdk_Event_Expose)
   is
      use Widget_List;
      Children, Tmp : Widget_List.Glist;
   begin
      Children := Get_Children (Container);
      Tmp := Children;
      while Tmp /= Null_List loop
         Propagate_Expose (Container, Get_Data (Tmp), Event);
         Tmp := Next (Tmp);
      end loop;

      Free (Children);
   end Propagate_Expose_Event;

   -----------------------------
   -- Find_First_Row_Matching --
   -----------------------------

   function Find_First_Row_Matching
     (Clist  : access Gtk.Clist.Gtk_Clist_Record'Class;
      Column : Gint;
      Text   : String) return Gint
   is
      Row : Gint := 0;
      Max : constant Gint := Get_Rows (Clist);
   begin
      while Row < Max loop
         if Get_Text (Clist, Row, Column) = Text then
            return Row;
         end if;
         Row := Row + 1;
      end loop;
      return -1;
   end Find_First_Row_Matching;

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
         Set_Cursor (Window, Cursor);
         Destroy (Cursor);
      else
         Set_Cursor (Window, null);
      end if;

      if Force_Refresh then
         Gdk.Main.Flush;
      end if;
   end Set_Busy_Cursor;

   ----------------
   -- Unmap_Menu --
   ----------------

   function Unmap_Menu
     (Menu : access Gtk_Widget_Record'Class;
      Data : Contextual_Menu_Data) return Boolean is
   begin
      if Data.Destroy /= null then
         Data.Destroy (Data.Widget, Gtk_Menu (Menu));
      end if;
      return False;
   end Unmap_Menu;

   --------------------------------------
   -- Button_Press_For_Contextual_Menu --
   --------------------------------------

   function Button_Press_For_Contextual_Menu
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Data   : Contextual_Menu_Data) return Boolean
   is
      Menu : Gtk_Menu;
   begin
      if Get_Button (Event) = 3
        and then Get_Event_Type (Event) = Button_Press
      then
         Menu := Data.Create (Widget, Event);
         if Menu /= null then
            Contextual_Callback.Connect
              (Menu, "unmap_event",
               Contextual_Callback.To_Marshaller
               (Unmap_Menu'Unrestricted_Access), Data);

            Grab_Focus (Widget);
            Show_All (Menu);
            Popup (Menu,
                   Button        => Gdk.Event.Get_Button (Event),
                   Activate_Time => Gdk.Event.Get_Time (Event));
            Emit_Stop_By_Name (Widget, "button_press_event");
            return True;
         end if;
      end if;
      return False;
   end Button_Press_For_Contextual_Menu;

   ------------------------------
   -- Register_Contextual_Menu --
   ------------------------------

   procedure Register_Contextual_Menu
     (Widget       : access Gtk_Widget_Record'Class;
      Menu_Create  : Contextual_Menu_Create  := null;
      Menu_Destroy : Contextual_Menu_Destroy := null) is
   begin
      --  If the widget doesn't have a window, it might not work. But then, if
      --  the children have windows and do not handle the event, this might get
      --  propagated, and the contextual menu will be properly displayed.
      --  So we just avoid a gtk warning
      if not No_Window_Is_Set (Widget) then
         Add_Events (Widget, Button_Press_Mask or Button_Release_Mask);
      end if;

      Contextual_Callback.Connect
        (Widget, "button_press_event",
         Contextual_Callback.To_Marshaller
         (Button_Press_For_Contextual_Menu'Access),
         (Menu_Create, Menu_Destroy, Gtk_Widget (Widget)));
   end Register_Contextual_Menu;

   ---------------------------
   -- User_Contextual_Menus --
   ---------------------------

   package body User_Contextual_Menus is

      function Button_Press_For_Contextual_Menu
        (Widget : access Gtk_Widget_Record'Class;
         Event  : Gdk.Event.Gdk_Event;
         User   : Callback_User_Data) return Boolean;

      function Unmap_User_Menu
        (Menu : access Gtk_Widget_Record'Class;
         User : Callback_User_Data) return Boolean;

      ---------------------
      -- Unmap_User_Menu --
      ---------------------

      function Unmap_User_Menu
        (Menu : access Gtk_Widget_Record'Class;
         User : Callback_User_Data) return Boolean is
      begin
         if User.Menu_Destroy /= null then
            User.Menu_Destroy (User.User, Gtk_Menu (Menu));
         end if;
         return False;
      end Unmap_User_Menu;

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
            if Menu /= null then
               Contextual_Callback.Connect
                 (Menu, "unmap_event",
                  Contextual_Callback.To_Marshaller
                  (Unmap_User_Menu'Unrestricted_Access), User);

               Grab_Focus (Widget);
               Show_All (Menu);
               Popup (Menu,
                      Button        => Gdk.Event.Get_Button (Event),
                      Activate_Time => Gdk.Event.Get_Time (Event));
               Emit_Stop_By_Name (Widget, "button_press_event");
               return True;
            end if;
         end if;
         return False;
      end Button_Press_For_Contextual_Menu;

      ------------------------------
      -- Register_Contextual_Menu --
      ------------------------------

      procedure Register_Contextual_Menu
        (Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
         User         : User_Data;
         Menu_Create  : Contextual_Menu_Create := null;
         Menu_Destroy : Contextual_Menu_Destroy := null) is
      begin
         if not No_Window_Is_Set (Widget) then
            Add_Events (Widget, Button_Press_Mask or Button_Release_Mask);
         end if;
         Contextual_Callback.Connect
           (Widget, "button_press_event",
            Contextual_Callback.To_Marshaller
            (User_Contextual_Menus.Button_Press_For_Contextual_Menu'
              Unrestricted_Access),
            (Menu_Create  => Menu_Create,
             Menu_Destroy => Menu_Destroy,
             User         => User));
      end Register_Contextual_Menu;
   end User_Contextual_Menus;

end GUI_Utils;
