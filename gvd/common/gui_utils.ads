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

--  This package contains a series of subprograms that can be used
--  for misc. graphical tasks.

with Gdk.Event;
with Gdk.Window;
with Glib.Object;
with Glib;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Clist;
with Gtk.Combo;
with Gtk.Container;
with Gtk.Handlers;
with Gtk.Handlers;
with Gtk.List;
with Gtk.Menu;
with Gtk.Tree_Store;
with Gtk.Widget;

package GUI_Utils is

   procedure Add_Unique_List_Entry
     (List : access Gtk.List.Gtk_List_Record'Class;
      Text : String);
   --  Add Text to List if it is not already there. Nothing is done if Text
   --  is already visible in the list

   procedure Add_Unique_Combo_Entry
     (Combo : access Gtk.Combo.Gtk_Combo_Record'Class;
      Text  : String);
   --  Add Text to the popdown list of Combo, if it is not already there.
   --  If the Text is already in the combo box, nothing is done.

   function Get_Index_In_List
     (Combo : access Gtk.Combo.Gtk_Combo_Record'Class) return Integer;
   --  Return the index of the selected item in the list of Combo.
   --  This is will return -1 if the current value of the combo is not in the
   --  list, which can not happen if Combo is read-only.

   procedure Set_Busy_Cursor
     (Window        : Gdk.Window.Gdk_Window;
      Busy          : Boolean := True;
      Force_Refresh : Boolean := False);
   --  Enable or disable the "busy" cursor for a specific top-level window.
   --  If Force_Refresh is True, then all X11 events are processed so that the
   --  new cursor is immediately visible for the user.

   procedure Propagate_Expose_Event
     (Container : access Gtk.Container.Gtk_Container_Record'Class;
      Event     : Gdk.Event.Gdk_Event_Expose);
   --  Propagate the expose event Event to all the NO_WINDOW children of
   --  Container. You must call this when Container has a specific expose
   --  callback.

   function Find_First_Row_Matching
     (Clist  : access Gtk.Clist.Gtk_Clist_Record'Class;
      Column : Glib.Gint;
      Text   : String) return Glib.Gint;
   --  Return the index of the first row that contains Text in the column
   --  Column.
   --  -1 is returned if no such row was found.

   procedure Remove_All_Children
     (Container : access Gtk.Container.Gtk_Container_Record'Class);
   --  Remove and destroy all the children from Container.
   --  If Container is a menu, this empties the menu, thus allowing dynamic
   --  menus.

   package Tree_Model_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Glib.Gint);

   procedure Set_Radio_And_Callback
     (Model    : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Renderer : access Gtk_Cell_Renderer_Toggle_Record'Class;
      Column   : Glib.Gint);
   --  Equivalent of Gtk.Cell_Renderer_Toggle, but this also sets up a callback
   --  to make sure that only one line is active.

   procedure Set_Editable_And_Callback
     (Model    : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Renderer : access Gtk_Cell_Renderer_Text_Record'Class;
      Column   : Glib.Gint);
   --  Set the renderer as editable, and make sure that its text is updated
   --  when the user has finished editing it.

   ----------------------
   -- Contextual menus --
   ----------------------

   type Contextual_Menu_Create is access function
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Gtk.Menu.Gtk_Menu;
   --  Function used to create the contextual menu for Widget.
   --  This function is only called for the right mouse button, so it doesn't
   --  need to check that.

   type Contextual_Menu_Destroy is access procedure
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu   : Gtk.Menu.Gtk_Menu);
   --  Subprogram called when a contextual menu is unmapped from the
   --  screen. The Menu itself and any associated memory should be freed at
   --  that point, unless you are keeping this handle somewhere else for later
   --  use.
   --  If you do not destroy the menu yourself, then there is a memory leak.

   procedure Register_Contextual_Menu
     (Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Create  : Contextual_Menu_Create  := null;
      Menu_Destroy : Contextual_Menu_Destroy := null);
   --  Widget will have an associated contextual menu, that is automatically
   --  popped up when the right mouse button is pressed.
   --  This contextual menu can be fully dynamic, since it is created through
   --  the function Menu_Create.
   --  Menu_Create can return null, if no contextual menu should be displayed.

   generic
      type User_Data is private;
   package User_Contextual_Menus is

      type Contextual_Menu_Create is access function
        (User  : User_Data;
         Event : Gdk.Event.Gdk_Event) return Gtk.Menu.Gtk_Menu;

      type Contextual_Menu_Destroy is access procedure
        (User : User_Data;
         Menu : Gtk.Menu.Gtk_Menu);

      type Callback_User_Data is record
         Menu_Create  : Contextual_Menu_Create;
         Menu_Destroy : Contextual_Menu_Destroy;
         User         : User_Data;
      end record;

      package Contextual_Callback is new Gtk.Handlers.User_Return_Callback
        (Gtk.Widget.Gtk_Widget_Record, Boolean, Callback_User_Data);

      procedure Register_Contextual_Menu
        (Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
         User         : User_Data;
         Menu_Create  : Contextual_Menu_Create := null;
         Menu_Destroy : Contextual_Menu_Destroy := null);

   end User_Contextual_Menus;
   --  Same as the procedure Register_Contextual_Menu, but the callbacks for
   --  the menu creation takes any type as input.
   --  This package must be instantiated as library level.

end GUI_Utils;
