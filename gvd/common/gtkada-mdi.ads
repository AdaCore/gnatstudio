-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001 ACT-Europe                   --
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

with Glib;
with Gdk.GC;
pragma Warnings (Off);
with Gdk.Cursor;
with Gdk.Types;
pragma Warnings (On);
with Gtk.Button;
with Gtk.Event_Box;
with Gtk.Handlers;
with Gtk.Layout;
with Gtk.Menu;
with Gtk.Check_Menu_Item;
with Gtk.Radio_Menu_Item;
with Gtk.Widget;
with Gtk.Window;
with GNAT.OS_Lib;

--  TODO:
--  - handles multiple views of the MDI (through several top-level windows)
--  - Saving and restoring sessions (window location,...)
--  - Icons should be put at the bottom, and automatically moved when the
--    MDI window is resized.
--  - Icons should be placed correctly when there are also docked items
--  - Add support for groups (children are associated with groups, and groups
--    can have special colors, can be minimized,...). Groups could be
--    implemented as special MDI_Children ?
--  - Manipulation of the title bar for children (adding buttons, adding
--    pixmaps,...)
--  - define new signals ("float_child", ...)
--  - Automatically add a new menu bar when a child is floated (settable
--    on a per-child basis).
--  - contextual menu in the title bar of children to dock them, float them,...
--  - Maximize_Children should work even if there is no child in MDI.

package Gtkada.MDI is

   type MDI_Window_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type MDI_Window is access all MDI_Window_Record'Class;
   --  Although this widget is implemented as a gtk_layout, you shouldn't
   --  use the standard Gtk_Layout functions like Put and Move yourself.

   type MDI_Child_Record is new Gtk.Event_Box.Gtk_Event_Box_Record
     with private;
   type MDI_Child is access all MDI_Child_Record'Class;
   --  A child of the MDI, that encapsulates the widgets you have put in the
   --  MDI window.
   --  You can easily convert from this to the initial widget using the
   --  functions Find_MDI_Child and Get_Widget.

   type Dock_Side is (Left, Right, Top, Bottom, None);
   --  Side on which a child will be docked. If None, the child cannot be
   --  docked.
   --  Order is important, since items docked on the left or right will
   --  occupy the whole height of MDI, whereas the ones on top or bottom will
   --  occupy the full width minus the left and right docks.

   type Priorities_Array is array (Left .. Bottom) of Integer;
   --  The priorities for the docks on each side of MDI. The lower priority
   --  dock will be resized first, and thus will occupy the maximum space
   --  available (for instance, if Left has a lower priority that Bottom, then
   --  the dock on the left side will occupy the full height of MDI, whereas
   --  the dock at the bottom will occupy the full width minus the width of
   --  the left dock).

   procedure Gtk_New (MDI : out MDI_Window);
   --  Create a new MDI window.
   --  Note that it is recommended that you modify the style (Set_Background
   --  in State_Normal) to have a different color.

   procedure Initialize (MDI : access MDI_Window_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Put
     (MDI : access MDI_Window_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   function Put
     (MDI   : access MDI_Window_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class) return MDI_Child;
   --  Add a new child to the MDI window, and return its embedding widget.
   --  Note that there is a small difference between adding a toplevel
   --  Gtk_Window and a standard widget.
   --  In the former case, only the child of the window is inserted into MDI.
   --  However, every time the child is set as floating (ie in its own
   --  toplevel window), we reuse the window you give in parameter to Put.
   --  Likewise, before the child is destroyed, a "delete_event" is emitted
   --  on the window you give in parameter to Put).
   --
   --  In that case, you shouldn't access Child directly afterwards, but should
   --  manipulate its child instead. However, as a special exception, you can
   --  still pass Child as a parameter to the subprograms in this package to
   --  manipulate it (for instance in Raise_Child,...)
   --
   --  On the other hand, if you insert any other widget, toplevel windows
   --  are created on the fly when needed, and destroyed automatically.
   --
   --  Note: You might have to call Set_USize on Child to set its initial
   --  size. This won't prevent it from being resized by the user.
   --
   --  If Child is a MDI_Child, its location is recomputed automatically.

   procedure Set_Title (Child : access MDI_Child_Record; Title : String);
   --  Set the title for a child.
   --  If you have put a Gtk_Window in the MDI, then the default title is the
   --  same as for the Gtk_Window. Likewise, if you modify the title with this
   --  subprogram, it changes the title of the Gtk_Window.
   --  For any other widget, the default is the empty string.
   --  In every case, this title will be the one used for the window when the
   --  child is set to floating state.

   function Get_Title (Child : access MDI_Child_Record) return String;
   --  Return the title of a given child.

   procedure Raise_Child (Child : access MDI_Child_Record'Class);
   --  Put Child in the foreground.

   procedure Minimize_Child
     (Child : access MDI_Child_Record'Class; Minimize : Boolean);
   --  Change the minimized state of a child.
   --  If the child was floating, it is first put back in the MDI

   procedure Maximize_Children
     (MDI : access MDI_Window_Record; Maximize : Boolean := True);
   --  All windows, except docked and floating ones, are maximized and occupy
   --  as much space as possible in MDI.
   --  This function has no effect unless there is already at least one child
   --  in MDI.

   function Get_Focus_Child
     (MDI : access MDI_Window_Record) return MDI_Child;
   --  Return the child that currently has the focus.
   --  null is returned if no child has the focus.

   function Create_Menu
     (MDI : access MDI_Window_Record) return Gtk.Menu.Gtk_Menu;
   --  Create a dynamic menu that can then be inserted into a menu bar. This
   --  menu is dynamic, ie its content will changed based on the focus
   --  child.
   --  If this function is called several times, the same menu is returned
   --  every time.

   function Create_Child_Menu
     (Child : access MDI_Child_Record'Class) return Gtk.Menu.Gtk_Menu;
   --  Create and return a static menu that should be put in a child-specific
   --  menu bar. The recommended way to use this is to put this menu in the
   --  menu bar for a floating child. This will allow thie child to be
   --  unfloated, or even docked.
   --  Note: This menu will not be automatically updated, for instance if
   --  you change the fact that Child can or cannot be docked. You need to get
   --  a new instance of the menu in that case.

   -----------------------------------------
   -- MDI_Child and encapsulated children --
   -----------------------------------------

   function Get_Widget
     (Child : access MDI_Child_Record) return Gtk.Widget.Gtk_Widget;
   --  Return the widget that Child encapsulates. This is the widget you
   --  initially Put() in MDI.
   --  Note that if you put a toplevel Gtk_Window initially, this returns the
   --  child of the window.

   function Get_Window
     (Child : access MDI_Child_Record) return Gtk.Window.Gtk_Window;
   --  If you initially Put() a Gtk_Window in the MDI, this returns that
   --  window, although with no child (see Get_Widget instead).
   --  If you have put something else than a toplevel window, this function
   --  returns null.

   function Find_MDI_Child
     (MDI    : access MDI_Window_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return MDI_Child;
   --  Return the MDI_Child that encapsulates Widget.
   --  Widget must be the exact same one you gave in argument to Put.

   -----------------------------------
   -- Floating and docking children --
   -----------------------------------

   procedure Set_Priorities
     (MDI : access MDI_Window_Record; Prio : Priorities_Array);
   --  Set the priorities to use for the docks (see description of
   --  Priorities_Array).

   procedure Float_Child
     (Child : access MDI_Child_Record'Class; Float : Boolean);
   --  Change the floating state of a child

   function Is_Floating
     (Child : access MDI_Child_Record'Class) return Boolean;
   --  Return True if Child is currently in a separate window

   procedure Dock_Child
     (Child : access MDI_Child_Record'Class;
      Dock : Boolean := True;
      Side : Dock_Side := None);
   --  Change the docking start of a child.
   --  If the child was floating, it is first put back in the MDI.
   --  Side is the place where the item should be docked. If Side is None (its
   --  default value), then the location depends on the default value you
   --  defined with Set_Dock_Side).
   --  Note that if Side, or the default value set for the child, is None,
   --  then nothing is done.

   procedure Set_Dock_Side
     (Child : access MDI_Child_Record'Class; Side  : Dock_Side);
   --  Specify where a child should be docked. Note that this doesn't
   --  actually dock the child.
   --  If the child was already docked, its location is changed accordingly.

   ---------------------------
   -- Reorganizing children --
   ---------------------------

   procedure Cascade_Children (MDI : access MDI_Window_Record);
   --  All the children are stacked so that the focus widget is on top.
   --  They overlap each other, but all the title bars are left visible

   procedure Tile_Horizontally (MDI : access MDI_Window_Record);
   procedure Tile_Vertically (MDI : access MDI_Window_Record);
   --  The available space in the MDI is shared equally between all children.
   --  They do not overlap each other.
   --  Tile_Horizontally with put children next to each other, Tile_Vertically
   --  will put children one below another. This is the same behavior as for
   --  Gtk_Vbox and Gtk_Hbox

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "delete_event"
   --    function Handler (Child : access Gtk_Widget_Record'Class)
   --                     return Boolean;
   --
   --    This signal is emitted for each item in the MDI window before it is
   --    actually delete. The child is destroyed only if the handler returns
   --    False.
   --    Note that the Child passed in argument is exactly the one you passed
   --    to Put to insert it in the MDI window.
   --    Note that this is also the signal to use to prevent top level
   --    Gtk_Window from being destroyed.
   --  </signals>

private
   type State_Type is (Normal, Iconified, Floating, Docked, Embedded);
   --  This type indicates the state of an item in the MDI:
   --  - Normal: the item can be manipulated (moved and resized) by the user.
   --  - Iconified: the item has been minimized, and can only be moved by the
   --      user. No resized is taken into account
   --  - Floating: the item has its own toplevel window, and is thus managed
   --      by the window manager. It is not really part of the MDI
   --  - Docked: This only apply to Gtk_Notebook items that are found on each
   --      side of the MDI. These items can contain any other number of
   --      embedded items, but can not be moved. They can be resized only
   --      on one of their side (depending on their location).
   --      These can never have the focus. Instead, the embedded child has the
   --      focus.
   --  - Embedded: the item is invisible. The widget it normally contains has
   --      been put into one of the dock items, and is completely managed by
   --      them. The item itself can not be mainpulated, nor even seen, by the
   --      user. However, it is kept alive so that its docked contents can
   --      easily be undocked.

   type MDI_Child_Record is new Gtk.Event_Box.Gtk_Event_Box_Record with record
      Initial : Gtk.Widget.Gtk_Widget;
      Initial_Child : Gtk.Widget.Gtk_Widget;
      --  The widget we used to build this child. This is used in case it
      --  was a window, since we need to be able to reparent it in the future,
      --  just in case.

      X, Y : Glib.Gint;
      State : State_Type := Normal;

      Title : GNAT.OS_Lib.String_Access;
      --  Title of the item, as it appears in the title bar

      Dock : Dock_Side := None;
      --  The size on which the item should be docked. If None, then the item
      --  can not be docked, and nothing will happen when calling Dock_Child.

      Uniconified_Width, Uniconified_Height : Glib.Gint;
      --  The size of the window, when not iconified. When in normal state,
      --  this represents the size of the window, since we can not rely on
      --  Get_Allocation_Width and Get_Allocation_Height (in case for instance
      --  we just resized the widget but didn't go back to the main gtk loop)

      Uniconified_X, Uniconified_Y : Glib.Gint;
      --  Initial coordinates of the item when it is not iconified. These
      --  fields are only relevant while the item is iconified.

      MDI : MDI_Window;
      --  The MDI to which the child belongs. We cannot get this information
      --  directly from Get_Parent since some children are actually embedded
      --  in docks (aka Gtk_Notebooks), and do not belong to the MDI anymore.

      Menu_Item : Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item;
      --  The item in the dynamic menu that represents this child.

      Maximize_Button : Gtk.Button.Gtk_Button;
   end record;

   procedure Gtk_New (Child : out MDI_Child;
                      Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                      Is_Dock : Boolean := False;
                      Side : Dock_Side := None);
   --  Create a new MDI child that contains widget.
   --  If Is_Dock is True, then the child is created as a dock on one of the
   --  sides of MDI (on Side).

   procedure Initialize (Child : access MDI_Child_Record;
                         Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                         Is_Dock : Boolean := False;
                         Side : Dock_Side := None);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   type Notebook_Array is array (Dock_Side) of MDI_Child;

   use Gdk.Cursor;
   use Gdk.Types;

   type MDI_Window_Record is new Gtk.Layout.Gtk_Layout_Record with record
      X_Root, Y_Root : Glib.Gint;
      --  Root coordinates of the button_press event that generated a move

      Selected_Child : MDI_Child;
      --  The child being moved

      Current_Cursor : Gdk_Cursor_Type := Left_Ptr;

      Focus_Child : MDI_Child := null;
      --  The widget that currently has the focus.

      Initial_Width, Initial_Height : Glib.Gint;
      --  Size of the item at the beginning of the drag operation.

      Current_W, Current_H, Current_X, Current_Y : Glib.Gint;
      --  Current size and location of the widget that is currently
      --  resized. This is used for non-opaque resizing.

      Back_GC : Gdk.GC.Gdk_GC;
      GC   : Gdk.GC.Gdk_GC;
      Resize_GC : Gdk.GC.Gdk_GC;
      Focus_GC : Gdk.GC.Gdk_GC;
      Title_GC : Gdk.GC.Gdk_GC;
      --  The graphic contexts to draw the children.

      Docks : Notebook_Array;
      --  The docks on each side of the MDI, where items can be docked.
      --  Note that these are also inserted into the list of children of MDI.

      Invisible_Items : Gtk.Widget.Widget_List.Glist;
      --  The widgets that are embedded in one of the dock items, or that
      --  are floating. In both cases, the child of the item is already
      --  visible elsewhere, and we want to be sure that the MDI_Child is never
      --  shown on the canvas.

      MDI_Width, MDI_Height : Glib.Gint := 0;
      --  Size of the MDI. This is used to avoid infinite loops in
      --  size_allocate

      Priorities : Priorities_Array := (0, 1, 2, 3);

      Menu_Item_Group : Gtk.Widget.Widget_SList.GSlist;
      --  The group to which the menu items of the children should belong

      Menu : Gtk.Menu.Gtk_Menu;
      Dock_Menu_Item : Gtk.Check_Menu_Item.Gtk_Check_Menu_Item;
      Dock_Menu_Item_Id : Gtk.Handlers.Handler_Id;
      Float_Menu_Item : Gtk.Check_Menu_Item.Gtk_Check_Menu_Item;
      Float_Menu_Item_Id : Gtk.Handlers.Handler_Id;
      --  The dynamic menu used to provide access to the most common functions
      --  of MDI.
   end record;

   pragma Inline (Get_Window);
   pragma Inline (Get_Widget);
   pragma Inline (Get_Focus_Child);
end Gtkada.MDI;
