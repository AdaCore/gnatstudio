-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2004                      --
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

with Gdk.Color;                use Gdk.Color;
with Gdk.Cursor;               use Gdk.Cursor;
with Gdk.Drawable;             use Gdk.Drawable;
with Gdk.Event;                use Gdk.Event;
with Gdk.GC;                   use Gdk.GC;
with Gdk.Keyval;               use Gdk.Keyval;
with Gdk.Main;                 use Gdk.Main;
with Gdk.Pixmap;               use Gdk.Pixmap;
with Gdk.Types;                use Gdk.Types;
with Gdk.Types.Keysyms;        use Gdk.Types.Keysyms;
with Gdk.Window;               use Gdk.Window;
with Glib.Convert;             use Glib.Convert;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Glib;                     use Glib;
with Gtk.Accel_Group;          use Gtk.Accel_Group;
with Gtk.Accel_Map;            use Gtk.Accel_Map;
with Gtk.Bin;                  use Gtk.Bin;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Clist;                use Gtk.Clist;
with Gtk.Combo;                use Gtk.Combo;
with Gtk.Container;            use Gtk.Container;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Event_Box;            use Gtk.Event_Box;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Item;                 use Gtk.Item;
with Gtk.Label;                use Gtk.Label;
with Gtk.List;                 use Gtk.List;
with Gtk.List_Item;            use Gtk.List_Item;
with Gtk.Main;                 use Gtk.Main;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Menu_Bar;             use Gtk.Menu_Bar;
with Gtk.Menu_Shell;           use Gtk.Menu_Shell;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Style;                use Gtk.Style;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_Mark;            use Gtk.Text_Mark;
with Gtk.Text_Tag;             use Gtk.Text_Tag;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Widget;               use Gtk.Widget;
with Pango.Enums;              use Pango.Enums;
with Pango.Font;               use Pango.Font;
with Pango.Layout;             use Pango.Layout;
with String_Utils;             use String_Utils;
with System;                   use System;
with String_List_Utils;        use String_List_Utils;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Exceptions;           use Ada.Exceptions;
with Traces;                   use Traces;

with GVD;                      use GVD;

package body GUI_Utils is

   Me : constant Debug_Handle := Create ("GUI_Utils");

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

   function Key_Press_For_Contextual_Menu
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Data   : Contextual_Menu_Data) return Boolean;
   --  Callback that pops up the contextual menu if needed

   function Unmap_Menu
     (Menu : access Gtk_Widget_Record'Class;
      Data : Contextual_Menu_Data) return Boolean;

   procedure Radio_Callback
     (Model  : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Glib.Gint);
   --  Callback for the toggle renderer

   procedure Edited_Callback
     (Model  : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Glib.Gint);
   --  Callback for the editable renderer

   function Add_Unique_List_Entry
     (List    : access Gtk.List.Gtk_List_Record'Class;
      Text    : String;
      Prepend : Boolean := False) return Gtk_List_Item;
   --  Internal version of Add_Unique_List_Entry, that also returns the added
   --  item.

   type Event_Access is access all Gdk_Event;
   package Event_Callback is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record, Boolean, Event_Access);

   function Key_Press_In_Grab
     (In_Widget : access Gtk_Widget_Record'Class;
      Event     : Gdk_Event;
      Output    : Event_Access) return Boolean;
   --  Temporary event filter set when grabing the key for a key preference

   ---------------------------
   -- Add_Unique_List_Entry --
   ---------------------------

   function Add_Unique_List_Entry
     (List    : access Gtk.List.Gtk_List_Record'Class;
      Text    : String;
      Prepend : Boolean := False) return Gtk_List_Item
   is
      use Widget_List;

      Item     : Gtk_List_Item;
      Children : Widget_List.Glist := Get_Children (List);

   begin
      --  Check whether Text is already in the list

      while Children /= Null_List loop
         Item := Gtk_List_Item (Get_Data (Children));

         if Get (Gtk_Label (Get_Child (Item))) = Text then
            return Item;
         end if;

         Children := Next (Children);
      end loop;

      --  Add the new item in the list

      Gtk_New (Item, Text);
      Show (Item);

      if Prepend then
         Append (Children, Gtk_Widget (Item));
         Prepend_Items (List, Children);
      else
         Add (List, Item);
      end if;

      return Item;
   end Add_Unique_List_Entry;

   ---------------------------
   -- Add_Unique_List_Entry --
   ---------------------------

   procedure Add_Unique_List_Entry
     (List : access Gtk.List.Gtk_List_Record'Class;
      Text : String;
      Prepend : Boolean := False)
   is
      Item : Gtk_List_Item;
      pragma Unreferenced (Item);
   begin
      Item := Add_Unique_List_Entry (List, Text, Prepend);
   end Add_Unique_List_Entry;

   ----------------------------
   -- Add_Unique_Combo_Entry --
   ----------------------------

   procedure Add_Unique_Combo_Entry
     (Combo           : access Gtk.Combo.Gtk_Combo_Record'Class;
      Text            : String;
      Item_String     : String  := "";
      Use_Item_String : Boolean := False;
      Prepend         : Boolean := False)
   is
      Item : Gtk_List_Item;
      pragma Unreferenced (Item);

   begin
      Item := Add_Unique_Combo_Entry
        (Combo, Text, Item_String, Use_Item_String, Prepend);
   end Add_Unique_Combo_Entry;

   ----------------------------
   -- Add_Unique_Combo_Entry --
   ----------------------------

   function Add_Unique_Combo_Entry
     (Combo       : access Gtk.Combo.Gtk_Combo_Record'Class;
      Text        : String;
      Item_String : String := "";
      Use_Item_String : Boolean := False;
      Prepend     : Boolean := False) return Gtk.List_Item.Gtk_List_Item
   is
      Item : Gtk_List_Item;
   begin
      Item := Add_Unique_List_Entry (Get_List (Combo), Text, Prepend);

      if Use_Item_String then
         Set_Item_String (Combo, Gtk_Item (Item), Item_String);
      end if;
      return Item;
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
      use type Gdk_Window;
      Cursor     : Gdk.Cursor.Gdk_Cursor;
   begin
      if Window /= null then
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

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Unmap_Menu;

   -----------------------------------
   -- Key_Press_For_Contextual_Menu --
   -----------------------------------

   function Key_Press_For_Contextual_Menu
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Data   : Contextual_Menu_Data) return Boolean
   is
      Menu : Gtk_Menu;
      use type Gdk.Types.Gdk_Key_Type;

   begin
      if Get_Key_Val (Event) = GDK_Menu then
         Menu := Data.Create (Widget, Event);

         if Menu /= null then
            Contextual_Callback.Connect
              (Menu, "unmap_event",
               Contextual_Callback.To_Marshaller (Unmap_Menu'Access), Data);

            Grab_Focus (Widget);
            Show_All (Menu);
            Popup (Menu,
                   Button        => 3,
                   Activate_Time => Gdk.Event.Get_Time (Event));
            Emit_Stop_By_Name (Widget, "key_press_event");
            return True;
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Key_Press_For_Contextual_Menu;

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
                 (Unmap_Menu'Access), Data);

            Grab_Focus (Widget);
            Show_All (Menu);

            --  Here we are calling Popup with an Activate_Time 200ms after
            --  the event time. This works around a bug under Windows that
            --  causes the menu to disappear on a click when there are too
            --  many entries.

            if Host = Windows then
               Popup (Menu,
                      Button        => Gdk.Event.Get_Button (Event),
                      Activate_Time => Gdk.Event.Get_Time (Event) + 200);
            else
               Popup (Menu,
                      Button        => Gdk.Event.Get_Button (Event),
                      Activate_Time => Gdk.Event.Get_Time (Event));
            end if;

            Emit_Stop_By_Name (Widget, "button_press_event");
            return True;
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
         Add_Events
           (Widget,
            Button_Press_Mask or Button_Release_Mask or Key_Press_Mask);
      end if;

      Contextual_Callback.Connect
        (Widget, "button_press_event",
         Contextual_Callback.To_Marshaller
           (Button_Press_For_Contextual_Menu'Access),
         (Menu_Create, Menu_Destroy, Gtk_Widget (Widget)));
      Contextual_Callback.Connect
        (Widget, "key_press_event",
         Contextual_Callback.To_Marshaller
           (Key_Press_For_Contextual_Menu'Access),
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

      function Key_Press_For_Contextual_Menu
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

      exception
         when E : others =>
            Trace (Exception_Handle,
                   "Unexpected exception: " & Exception_Information (E));
            return False;
      end Unmap_User_Menu;

      -----------------------------------
      -- Key_Press_For_Contextual_Menu --
      -----------------------------------

      function Key_Press_For_Contextual_Menu
        (Widget : access Gtk_Widget_Record'Class;
         Event  : Gdk.Event.Gdk_Event;
         User   : Callback_User_Data) return Boolean
      is
         Menu : Gtk_Menu;
         use type Gdk.Types.Gdk_Key_Type;

      begin
         if Get_Key_Val (Event) = GDK_Menu then
            Menu := User.Menu_Create (User.User, Event);

            if Menu /= null then
               Contextual_Callback.Connect
                 (Menu, "unmap_event",
                  Contextual_Callback.To_Marshaller
                    (Unmap_User_Menu'Unrestricted_Access), User);

               Grab_Focus (Widget);
               Show_All (Menu);
               Popup (Menu,
                      Button        => 3,
                      Activate_Time => Gdk.Event.Get_Time (Event));
               Show_All (Menu);
               Emit_Stop_By_Name (Widget, "key_press_event");
               return True;
            end if;
         end if;

         return False;

      exception
         when E : others =>
            Trace (Exception_Handle,
                   "Unexpected exception: " & Exception_Information (E));
            return False;
      end Key_Press_For_Contextual_Menu;

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

               --  Here we are calling Popup with an Activate_Time 200ms after
               --  the event time. This works around a bug under Windows that
               --  causes the menu to disappear on a click when there are too
               --  many entries.

               if Host = Windows then
                  Popup (Menu,
                         Button        => Gdk.Event.Get_Button (Event),
                         Activate_Time => Gdk.Event.Get_Time (Event) + 200);
               else
                  Popup (Menu,
                         Button        => Gdk.Event.Get_Button (Event),
                         Activate_Time => Gdk.Event.Get_Time (Event));
               end if;

               Emit_Stop_By_Name (Widget, "button_press_event");
               return True;
            end if;
         end if;

         return False;

      exception
         when E : others =>
            Trace (Exception_Handle,
                   "Unexpected exception: " & Exception_Information (E));
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
            Add_Events
              (Widget,
               Button_Press_Mask or Button_Release_Mask or Key_Press_Mask);
         end if;

         Contextual_Callback.Connect
           (Widget, "button_press_event",
            Contextual_Callback.To_Marshaller
              (User_Contextual_Menus.Button_Press_For_Contextual_Menu'
                 Unrestricted_Access),
            (Menu_Create  => Menu_Create,
             Menu_Destroy => Menu_Destroy,
             User         => User));
         Contextual_Callback.Connect
           (Widget, "key_press_event",
            Contextual_Callback.To_Marshaller
              (User_Contextual_Menus.Key_Press_For_Contextual_Menu'
                 Unrestricted_Access),
            (Menu_Create  => Menu_Create,
             Menu_Destroy => Menu_Destroy,
             User         => User));
      end Register_Contextual_Menu;

   end User_Contextual_Menus;

   -------------------------
   -- Remove_All_Children --
   -------------------------

   procedure Remove_All_Children
     (Container : access Gtk.Container.Gtk_Container_Record'Class;
      Filter    : Filter_Function := null)
   is
      use Widget_List;
      Children : Widget_List.Glist := Get_Children (Container);
      N : Widget_List.Glist;
      W : Gtk_Widget;
   begin
      while Children /= Null_List loop
         N := Children;
         Children := Next (Children);
         W := Widget_List.Get_Data (N);

         if Filter = null or else Filter (W) then
            --  Small workaround for a gtk+ bug: a Menu_Item containing an
            --  accel_label would never be destroyed because the label holds a
            --  reference to the menu item, and the latter holds a reference to
            --  the label (see gtk_accel_label_set_accel_widget).

            if W.all in Gtk_Bin_Record'Class then
               Destroy (Get_Child (Gtk_Bin (W)));
            end if;

            Remove (Container, W);
         end if;
      end loop;

      Widget_List.Free (Children);
   end Remove_All_Children;

   ----------------------------
   -- Set_Radio_And_Callback --
   ----------------------------

   procedure Set_Radio_And_Callback
     (Model    : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Renderer : access Gtk_Cell_Renderer_Toggle_Record'Class;
      Column   : Glib.Gint) is
   begin
      Set_Radio (Renderer, True);

      Tree_Model_Callback.Object_Connect
        (Renderer, "toggled",
         Radio_Callback'Access, Slot_Object => Model, User_Data => Column);
   end Set_Radio_And_Callback;

   --------------------
   -- Radio_Callback --
   --------------------

   procedure Radio_Callback
     (Model  : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Glib.Gint)
   is
      M           : constant Gtk_Tree_Store := Gtk_Tree_Store (Model);
      Iter, Tmp   : Gtk_Tree_Iter;
      Path_String : constant String := Get_String (Nth (Params, 1));

   begin
      Iter := Get_Iter_From_String (M, Path_String);

      if Iter /= Null_Iter then
         --  Can't click on an already active item, for a radio renderer, since
         --  we want at least one selected item

         if not Get_Boolean (M, Iter, Data) then
            Tmp := Get_Iter_First (M);

            while Tmp /= Null_Iter loop
               Set (M, Tmp, Data, False);
               Next (M, Tmp);
            end loop;

            Set (M, Iter, Data, True);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Radio_Callback;

   -------------------------------
   -- Set_Editable_And_Callback --
   -------------------------------

   procedure Set_Editable_And_Callback
     (Model    : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Renderer : access Gtk_Cell_Renderer_Text_Record'Class;
      Column   : Glib.Gint) is
   begin
      Tree_Model_Callback.Object_Connect
        (Renderer, "edited", Edited_Callback'Access,
         Slot_Object => Model, User_Data => Column);
   end Set_Editable_And_Callback;

   ---------------------
   -- Edited_Callback --
   ---------------------

   procedure Edited_Callback
     (Model  : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Glib.Gint)
   is
      M           : constant Gtk_Tree_Store := Gtk_Tree_Store (Model);
      Iter        : Gtk_Tree_Iter;
      Path_String : constant String := Get_String (Nth (Params, 1));
      Text_Value  : constant GValue := Nth (Params, 2);
   begin
      Iter := Get_Iter_From_String (M, Path_String);
      Set_Value (M, Iter, Data, Text_Value);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Edited_Callback;

   -----------------------------
   -- Create_Pixmap_From_Text --
   -----------------------------

   procedure Create_Pixmap_From_Text
     (Text       : String;
      Font       : Pango.Font.Pango_Font_Description;
      Bg_Color   : Gdk.Color.Gdk_Color;
      Widget     : access Gtk_Widget_Record'Class;
      Pixmap     : out Gdk.Gdk_Pixmap;
      Width      : out Glib.Gint;
      Height     : out Glib.Gint;
      Wrap_Width : Glib.Gint := -1)
   is
      Margin : constant := 2;
      GC     : Gdk_GC;
      Layout : Pango_Layout;
   begin
      Gdk_New (GC, Get_Window (Widget));
      Set_Foreground (GC, Bg_Color);

      Layout := Create_Pango_Layout (Widget, Text);
      Set_Font_Description (Layout, Font);

      if Wrap_Width /= -1 then
         Set_Wrap (Layout, Pango_Wrap_Char);
         Set_Width (Layout, Wrap_Width * Pango_Scale);
      end if;

      Get_Pixel_Size (Layout, Width, Height);
      Width := Width + Margin * 2;
      Height := Height + Margin * 2;

      Gdk.Pixmap.Gdk_New
        (Pixmap, Get_Window (Widget), Width, Height);
      Draw_Rectangle
        (Pixmap,
         GC,
         Filled => True,
         X      => 0,
         Y      => 0,
         Width  => Width - 1,
         Height => Height - 1);

      Set_Foreground (GC, Black (Get_Default_Colormap));
      Draw_Rectangle
        (Pixmap,
         GC,
         Filled => False,
         X      => 0,
         Y      => 0,
         Width  => Width - 1,
         Height => Height - 1);

      Draw_Layout (Pixmap, GC, Margin, Margin, Layout);

      Unref (Layout);
      Unref (GC);
   end Create_Pixmap_From_Text;

   -------------------------
   -- Full_Path_Menu_Item --
   -------------------------

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Menu_Item : out Full_Path_Menu_Item;
      Label     : String := "";
      Path      : String := "") is
   begin
      Menu_Item := new Full_Path_Menu_Item_Record (Path'Length);
      Initialize (Menu_Item, Label, Path);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Menu_Item : access Full_Path_Menu_Item_Record'Class;
      Label     : String;
      Path      : String) is
   begin
      Initialize (Gtk_Menu_Item (Menu_Item), Locale_To_UTF8 (Label));
      Menu_Item.Full_Path := Path;
   end Initialize;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
     (Menu_Item : access Full_Path_Menu_Item_Record) return String is
   begin
      return Menu_Item.Full_Path;
   end Get_Path;

   -------------------------
   -- Find_Iter_For_Event --
   -------------------------

   function Find_Iter_For_Event
     (Tree  : access Gtk_Tree_View_Record'Class;
      Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Event : Gdk_Event) return Gtk_Tree_Iter
   is
      X         : Gdouble;
      Y         : Gdouble;
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Row_Found : Boolean;
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column := null;
      Iter      : Gtk_Tree_Iter := Null_Iter;
      N_Model   : Gtk_Tree_Model;
   begin
      if Event /= null
        and then Get_Event_Type (Event) in Button_Press .. Button_Release
      then
         X := Get_X (Event);
         Y := Get_Y (Event);
         Path := Gtk_New;
         Get_Path_At_Pos
           (Tree,
            Gint (X),
            Gint (Y),
            Path,
            Column,
            Buffer_X,
            Buffer_Y,
            Row_Found);

         if Path = null then
            return Iter;
         end if;

         Iter := Get_Iter (Model, Path);
         Path_Free (Path);
      else
         Get_Selected (Get_Selection (Tree), N_Model, Iter);
      end if;

      return Iter;
   end Find_Iter_For_Event;

   --------------------------
   -- Search_Entity_Bounds --
   --------------------------

   procedure Search_Entity_Bounds
     (Start_Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter     : out Gtk.Text_Iter.Gtk_Text_Iter)
   is
      Ignored : Boolean;

   begin
      --  Search forward the end of the entity...
      Copy (Source => Start_Iter, Dest => End_Iter);

      if Is_Operator_Letter (Gunichar'(Get_Char (End_Iter))) then
         while not Is_End (End_Iter) loop
            exit when not Is_Operator_Letter (Gunichar'(Get_Char (End_Iter)));
            Forward_Char (End_Iter, Ignored);
         end loop;

         --  And search backward the begining of the entity...
         while not Is_Start (Start_Iter) loop
            Backward_Char (Start_Iter, Ignored);

            if not Is_Operator_Letter (Gunichar'(Get_Char (Start_Iter))) then
               Forward_Char (Start_Iter, Ignored);
               exit;
            end if;
         end loop;

      else
         while not Is_End (End_Iter) loop
            exit when not Is_Entity_Letter (Gunichar'(Get_Char (End_Iter)));
            Forward_Char (End_Iter, Ignored);
         end loop;

         --  And search backward the begining of the entity...
         while not Is_Start (Start_Iter) loop
            Backward_Char (Start_Iter, Ignored);

            if not Is_Entity_Letter (Gunichar'(Get_Char (Start_Iter))) then
               Forward_Char (Start_Iter, Ignored);
               exit;
            end if;
         end loop;
      end if;
   end Search_Entity_Bounds;

   -----------
   -- Image --
   -----------

   function Image
     (Key  : Gdk.Types.Gdk_Key_Type;
      Mods : Gdk.Types.Gdk_Modifier_Type) return String
   is
      Shift   : constant String := "shift-";
      Meta    : constant String := "alt-";
      Control : constant String := "control-";
      Max : constant Natural := Shift'Length + Control'Length + Meta'Length;
      Buffer : String (1 .. Max);
      Current : Natural := Buffer'First;
   begin
      if Key = 0 then
         return "";
      end if;

      case Key is
         when GDK_Shift_L
           | GDK_Shift_R
           | GDK_Control_L
           | GDK_Control_R
           | GDK_Caps_Lock
           | GDK_Shift_Lock
           | GDK_Meta_L
           | GDK_Meta_R
           | GDK_Alt_L
           | GDK_Alt_R
           =>
            return Special_Key_Binding;

         when others =>
            if (Mods and Shift_Mask) /= 0 then
               Buffer (Current .. Current + Shift'Length - 1) := Shift;
               Current := Current + Shift'Length;
            end if;

            if (Mods and Control_Mask) /= 0 then
               Buffer (Current .. Current + Control'Length - 1) := Control;
               Current := Current + Control'Length;
            end if;

            if (Mods and Mod1_Mask) /= 0 then
               Buffer (Current .. Current + Meta'Length - 1) := Meta;
               Current := Current + Meta'Length;
            end if;

            return
              Buffer (Buffer'First .. Current - 1) & Gdk.Keyval.Name (Key);
      end case;
   end Image;

   -----------
   -- Value --
   -----------

   procedure Value
     (From : String;
      Key  : out Gdk.Types.Gdk_Key_Type;
      Mods : out Gdk.Types.Gdk_Modifier_Type)
   is
      Start : Natural := From'First;
   begin
      Mods := 0;
      for D in From'Range loop
         if From (D) = '-' then
            if From (Start .. D) = "shift-" then
               Mods := Mods or Shift_Mask;
            elsif From (Start .. D) = "control-" then
               Mods := Mods or Control_Mask;
            elsif From (Start .. D) = "alt-" then
               Mods := Mods or Mod1_Mask;
            end if;
            Start := D + 1;
         end if;
      end loop;

      Key := From_Name (From (Start .. From'Last));
   end Value;

   -----------------------
   -- Key_Press_In_Grab --
   -----------------------

   function Key_Press_In_Grab
     (In_Widget : access Gtk_Widget_Record'Class;
      Event     : Gdk_Event;
      Output    : Event_Access) return Boolean
   is
      pragma Unreferenced (In_Widget);
      Text  : constant String :=
        Image (Get_Key_Val (Event), Get_State (Event));
   begin
      if Text /= Special_Key_Binding then
         Deep_Copy (From => Event, To => Output.all);
         Main_Quit;
      end if;
      return True;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Key_Press_In_Grab;

   --------------
   -- Key_Grab --
   --------------

   procedure Key_Grab
     (In_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Key  : out Gdk.Types.Gdk_Key_Type;
      Mods : out Gdk.Types.Gdk_Modifier_Type)
   is
      Tmp    : Gdk_Grab_Status;
      pragma Unreferenced (Tmp);

      Id     : Handler_Id;
      Cursor : Gdk.Cursor.Gdk_Cursor;
      Output : aliased Gdk_Event := null;
      O      : constant Event_Access := Output'Unchecked_Access;
   begin
      Tmp := Keyboard_Grab
        (Get_Window (In_Widget), Owner_Events => False, Time => 0);

      Gdk_New (Cursor, Watch);
      Tmp := Pointer_Grab
        (Window     => Get_Window (In_Widget),
         Event_Mask => Button_Press_Mask or Button_Release_Mask,
         Confine_To => Get_Window (In_Widget),
         Cursor     => Cursor,
         Time       => 0);
      Grab_Add (In_Widget);
      Unref (Cursor);

      Grab_Focus (In_Widget);

      Id := Event_Callback.Connect
        (In_Widget, "key_press_event",
         Event_Callback.To_Marshaller (Key_Press_In_Grab'Access),
         User_Data => O);

      Gtk.Main.Main;

      --  Output could be null if the main loop was exited
      if Output /= null then
         Key  := Get_Key_Val (Output);
         Mods := Get_State (Output);
         Free (Output);
      else
         Key  := 0;
         Mods := 0;
      end if;

      Grab_Remove (In_Widget);
      Keyboard_Ungrab (0);
      Pointer_Ungrab (0);
      Gtk.Handlers.Disconnect (In_Widget, Id);
   end Key_Grab;

   -------------------
   -- Do_Completion --
   -------------------

   procedure Do_Completion
     (View            : access Gtk_Text_View_Record'Class;
      Completion      : Completion_Handler;
      Prompt_End_Mark : Gtk_Text_Mark;
      Uneditable_Tag  : Gtk_Text_Tag;
      User_Data       : System.Address)
   is
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (View);
      Prompt_Iter, Last_Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Prompt_Iter, Prompt_End_Mark);
      Get_End_Iter (Buffer, Last_Iter);

      declare
         use String_List_Utils.String_List;
         Text        : constant String :=
           Get_Slice (Buffer, Prompt_Iter, Last_Iter);
         Completions : List :=  Completion (Text, User_Data);
         Prefix      : constant String := Longest_Prefix (Completions);
         Node        : List_Node;
         Line        : Gint;
         Offset      : Gint;
         More_Than_One : constant Boolean :=
           Completions /= Null_List
           and then Next (First (Completions)) /= Null_Node;
         Success     : Boolean;
         Prev_Begin  : Gtk_Text_Iter;
         Prev_Last   : Gtk_Text_Iter;
         Pos         : Gtk_Text_Iter;
      begin
         if More_Than_One then
            Node := First (Completions);

            --  Get the range copy the current line.

            Line := Get_Line (Last_Iter);

            --  Get the offset of the prompt.

            Offset := Get_Line_Offset (Prompt_Iter);

            Get_End_Iter (Buffer, Pos);
            Insert (Buffer, Pos, "" & ASCII.LF);

            while Node /= Null_Node loop
               Get_End_Iter (Buffer, Pos);
               Set_Line_Offset (Pos, 0);
               Insert (Buffer, Pos, Data (Node) & ASCII.LF);
               Node := Next (Node);
            end loop;

            Get_Iter_At_Line_Offset (Buffer, Prev_Begin, Line, 0);
            Copy (Prev_Begin, Prev_Last);
            Forward_To_Line_End (Prev_Last, Success);

            Get_End_Iter (Buffer, Pos);
            Insert_Range (Buffer, Pos, Prev_Begin, Prev_Last);

            Get_End_Iter (Buffer, Pos);
            Set_Line_Offset (Pos, 0);

            Get_Iter_At_Line_Offset (Buffer, Prev_Begin, Line, 0);
            Apply_Tag (Buffer, Uneditable_Tag, Prev_Begin, Pos);

            --  Restore the prompt

            Get_End_Iter (Buffer, Prompt_Iter);
            Set_Line_Offset (Prompt_Iter, Offset);

            Move_Mark (Buffer, Prompt_End_Mark, Prompt_Iter);
            Scroll_Mark_Onscreen (View, Prompt_End_Mark);
         end if;

         --  Insert the completion, if any.
         Get_End_Iter (Buffer, Pos);

         if Prefix'Length > Text'Length then
            Insert (Buffer, Pos,
                    Prefix (Prefix'First + Text'Length .. Prefix'Last));
         end if;

         if not More_Than_One then
            Insert (Buffer, Pos, " ");
         end if;

         Free (Completions);
      end;
   end Do_Completion;

   --------------------
   -- Save_Accel_Map --
   --------------------

   procedure Save_Accel_Map (Filename : String) is
      File : File_Type;

      procedure Save_Dynamic_Key
        (Data       : System.Address;
         Accel_Path : String;
         Accel_Key  : Gdk.Types.Gdk_Key_Type;
         Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
         Changed    : Boolean);
      --  Called for each key shortcut the user has defined interactively

      procedure Save_Dynamic_Key
        (Data       : System.Address;
         Accel_Path : String;
         Accel_Key  : Gdk.Types.Gdk_Key_Type;
         Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
         Changed    : Boolean)
      is
         pragma Unreferenced (Data);
      begin
         if Changed then
            Put_Line (File, "(gtk_accel_path """
                      & Accel_Path
                      & """ """
                      & Accelerator_Name (Accel_Key, Accel_Mods)
                      & """) ");
         end if;
      end Save_Dynamic_Key;

   begin
      Create (File, Out_File, Filename);
      Gtk.Accel_Map.Foreach
        (System.Null_Address, Save_Dynamic_Key'Unrestricted_Access);
      Close (File);
   end Save_Accel_Map;

   --------------------
   -- Query_Password --
   --------------------

   function Query_Password (Prompt : String) return String is
      Dialog : Gtk_Dialog;
      Button : Gtk_Widget;
      Label  : Gtk_Label;
      Passwd : Gtk_Entry;
   begin
      Gtk_New (Dialog,
               Title => Prompt,
               Parent => null,
               Flags => Destroy_With_Parent or Modal);

      Gtk_New (Label, Prompt);
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Get_Vbox (Dialog), Label, Expand => False);

      Gtk_New (Passwd);
      Pack_Start (Get_Vbox (Dialog), Passwd, Expand => False);
      Set_Activates_Default (Passwd, True);
      Set_Visibility (Passwd, Visible => False);

      Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Grab_Default (Button);
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         declare
            Pass : constant String := Get_Text (Passwd);
         begin
            Destroy (Dialog);
            return Pass;
         end;
      else
         Destroy (Dialog);
         return "";
      end if;
   end Query_Password;

   ----------------------------
   -- Find_Menu_Item_By_Name --
   ----------------------------

   procedure Find_Menu_Item_By_Name
     (Menu_Bar  : Gtk_Menu_Bar;
      Menu      : Gtk_Menu;
      Name      : String;
      Menu_Item : out Gtk_Menu_Item;
      Index     : out Gint)
   is
      use type Widget_List.Glist;
      Children, Tmp : Widget_List.Glist;
      Label         : Gtk_Label;
      New_Name      : String (Name'Range);
      Last          : Integer := New_Name'First;

   begin
      Menu_Item := null;

      if Name = "" then
         Index := -1;
         return;
      end if;

      for J in Name'Range loop
         if Name (J) = '_' then
            if J - 1 >= Name'First and then Name (J - 1) = '_' then
               New_Name (Last) := '_';
               Last := Last + 1;
            end if;
         else
            New_Name (Last) := Name (J);
            Last := Last + 1;
         end if;
      end loop;

      if Menu /= null then
         Children := Get_Children (Menu);
      elsif Menu_Bar /= null then
         Children := Get_Children (Menu_Bar);
      else
         Children := Widget_List.Null_List;
      end if;

      Index := 0;
      Tmp := Children;

      while Tmp /= Widget_List.Null_List loop
         Menu_Item := Gtk_Menu_Item (Widget_List.Get_Data (Tmp));

         if Get_Child (Menu_Item) /= null
           and then Get_Child (Menu_Item).all in Gtk_Label_Record'Class
         then
            Label := Gtk_Label (Get_Child (Menu_Item));
            exit when Case_Insensitive_Equal
              (Get_Text (Label), New_Name (New_Name'First .. Last - 1));
         end if;

         Index := Index + 1;
         Tmp := Widget_List.Next (Tmp);
         Menu_Item := null;
      end loop;

      Widget_List.Free (Children);

      if Menu_Item = null then
         Index := -1;
      end if;
   end Find_Menu_Item_By_Name;

   ------------------------------
   -- Find_Or_Create_Menu_Tree --
   ------------------------------

   function Find_Or_Create_Menu_Tree
     (Menu_Bar     : Gtk_Menu_Bar;
      Menu         : Gtk_Menu;
      Path         : String;
      Accelerators : Gtk.Accel_Group.Gtk_Accel_Group;
      Allow_Create : Boolean := True;
      Ref_Item     : String  := "";
      Add_Before   : Boolean := True) return Gtk_Menu_Item
   is
      First     : Natural := Path'First;
      Last      : Natural;
      Parent    : Gtk_Menu := Menu;
      Menu_Item : Gtk_Menu_Item;
      Pred      : Gtk_Menu_Item;
      M         : Gtk_Menu;
      Index     : Gint;
   begin
      if Path (First) = '/' then
         First := First + 1;
      end if;

      --  Find the existing parents

      while First <= Path'Last loop
         Last := First + 1;
         Skip_To_Char (Path, Last, '/');

         Find_Menu_Item_By_Name
           (Menu_Bar, Parent, Path (First .. Last - 1), Menu_Item, Index);
         exit when Menu_Item = null;

         --  Have we found the item ?
         First  := Last + 1;
         exit when Last >= Path'Last;

         if Get_Submenu (Menu_Item) = null then
            Trace
              (Me, "Find_Or_Create_Menu_Tree: "
               & Path (Path'First .. Last - 1) & " has no submenu");
            return null;
         end if;

         Parent := Gtk_Menu (Get_Submenu (Menu_Item));
      end loop;

      --  Create the missing parents

      if Allow_Create then
         while First <= Path'Last loop
            Last := First + 1;
            Skip_To_Char (Path, Last, '/');

            Gtk_New_With_Mnemonic (Menu_Item, Path (First .. Last - 1));

            --  Should we create a submenu ?
            if Last <= Path'Last then
               Gtk_New (M);
               Set_Submenu (Menu_Item, M);
               Set_Accel_Group (M, Accelerators);
            end if;

            Find_Menu_Item_By_Name
              (Menu_Bar, Parent, Ref_Item, Pred, Index);
            Add_Menu (Parent, Menu_Bar, Menu_Item, Index, Add_Before);
            Show_All (Menu_Item);
            Parent := M;

            First := Last + 1;
         end loop;

      elsif First <= Path'Last then
         --  No such item
         Menu_Item := null;
      end if;

      return Menu_Item;
   end Find_Or_Create_Menu_Tree;

   --------------
   -- Add_Menu --
   --------------

   procedure Add_Menu
     (Parent     : Gtk_Menu;
      Menu_Bar   : Gtk_Menu_Bar := null;
      Item       : Gtk_Menu_Item;
      Index      : Gint := -1;
      Add_Before : Boolean := True)
   is
      P : Gtk_Menu_Shell := Gtk_Menu_Shell (Parent);
   begin
      --  Insertion in the menu bar
      if Parent = null then
         P := Gtk_Menu_Shell (Menu_Bar);
      end if;

      if Index = -1 then
         Append (P, Item);
      elsif Add_Before then
         Insert (P, Item, Index);
      else
         Insert (P, Item, Index + 1);
      end if;
   end Add_Menu;

   ---------------
   -- Find_Node --
   ---------------

   function Find_Node
     (Model  : Gtk_Tree_Store;
      Name   : String;
      Column : Gint) return Gtk_Tree_Iter
   is
      Parent : Gtk_Tree_Iter := Get_Iter_First (Model);
   begin
      while Parent /= Null_Iter loop
         if Get_String (Model, Parent, Column) = Name then
            return Parent;
         end if;
         Next (Model, Parent);
      end loop;

      return Null_Iter;
   end Find_Node;

   -----------------------
   -- Create_Blue_Label --
   -----------------------

   procedure Create_Blue_Label
     (Label : out Gtk.Label.Gtk_Label;
      Event : out Gtk.Event_Box.Gtk_Event_Box)
   is
      Color : Gdk_Color;
   begin
      Gtk_New (Event);
      Color := Parse ("#0e79bd");
      Alloc (Get_Default_Colormap, Color);
      Set_Style (Event, Copy (Get_Style (Event)));
      Set_Background (Get_Style (Event), State_Normal, Color);

      Gtk_New (Label, "");
      Set_Alignment (Label, 0.1, 0.5);
      Add (Event, Label);
   end Create_Blue_Label;

end GUI_Utils;
