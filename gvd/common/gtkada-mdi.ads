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

with Ada.Tags;
with Glib;
with Gint_Xml;
with Gdk.GC;
pragma Warnings (Off);
--  Need to use a use_clause for compatibility with gtk+1.2 and gtk+2.0
with Gdk.Cursor;  use Gdk.Cursor;
with Gdk.Types;   use Gdk.Types;
pragma Warnings (On);
with Gdk.Window;
with Gtk.Button;
with Gtk.Fixed;
with Gtk.Event_Box;
with Gtk.Handlers;
with Gtk.Menu;
with Gtk.Notebook;
with Gtk.Check_Menu_Item;
with Gtk.Radio_Menu_Item;
with Gtk.Widget;
with Gtk.Window;
with GNAT.OS_Lib;

--  TODO:
--  - handles multiple views of the MDI (through several top-level windows)
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

   procedure Close
     (MDI : access MDI_Window_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Close the child, and remove its window from the MDI.
   --  This first checks through a delete_event callback whether the child
   --  accepts to be closed.

   procedure Set_Title
     (Child : access MDI_Child_Record;
      Title : String;
      Short_Title : String := "");
   --  Set the title for a child. Title is the title put in titlebar of
   --  the children, whereas Short_Title is the name of the notebook tab when
   --  children are maximized. By default, it is the same as Title.
   --
   --  If you have put a Gtk_Window in the MDI, then the default title is the
   --  same as for the Gtk_Window. Likewise, if you modify the title with this
   --  subprogram, it changes the title of the Gtk_Window.
   --  For any other widget, the default is the empty string.
   --  In every case, this title will be the one used for the window when the
   --  child is set to floating state.

   function Get_Title (Child : access MDI_Child_Record) return String;
   --  Return the title for a specific child

   function Get_Short_Title (Child : access MDI_Child_Record) return String;
   --  Return the name of the notebook tab used when children are maximized.

   procedure Raise_Child (Child : access MDI_Child_Record'Class);
   --  Put Child in the foreground.
   --  Note that this does not give the focus to this child.

   procedure Lower_Child (Child : access MDI_Child_Record'Class);
   --  Put Child in the background.
   --  If the children are maximized, this selected the next page from the
   --  notebook.

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

   procedure Set_Focus_Child
     (MDI : access MDI_Window_Record;
      Containing : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Give the focus to the child containing Containing.

   procedure Set_Focus_Child (Child : access MDI_Child_Record'Class);
   --  Make Child the active widget, and raise it at the top.

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

   --  Return the first child matching Tag
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

   function Find_MDI_Child_By_Tag
     (MDI    : access MDI_Window_Record;
      Tag    : Ada.Tags.Tag)
     return MDI_Child;
   --  Return the first child matching Tag

   type Child_Iterator is private;

   function First_Child (MDI : access MDI_Window_Record) return Child_Iterator;
   --  Return an access to the first child of the MDI

   procedure Next (Iterator : in out Child_Iterator);
   --  Move to the next child in the MDI

   function Get (Iterator : Child_Iterator) return MDI_Child;
   --  Return the child pointed to by Iterator.
   --  If Iterator is no longer valid, null is returned.

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
      Dock : Boolean := True);
   --  Change the docking start of a child.
   --  If the child was floating, it is first put back in the MDI.

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

   ----------------------
   -- Desktop Handling --
   ----------------------
   --  The MDI provides a way to save desktops, i.e the list of children
   --  currently open in the MDI and their location. It can then restore the
   --  desktop at some later point.
   --
   --  Desktops require support from the widgets that are put in the MDI. They
   --  need to register a function to save them and a function to recreate
   --  them. Using Ada streams for this didn't prove workable since some
   --  children might need extra parameters not available to them through
   --  streams. This is why the following subprograms are in a generic package,
   --  so that you can pass whatever parameter(s) is needed in your
   --  application.
   --
   --  Desktops are saved and restored in XML trees.
   --
   --  Note that you can instantiate several of these packages, but you need to
   --  call Save_Desktop and Restore_Desktop from each of them if you want to
   --  save the whole contents of the MDI. The resulting XML nodes can then be
   --  merged into a single XML tree if needed.


   generic
      type User_Data (<>) is private;
      --  Generic type of parameter that is passed to all the children's save
      --  and restore functions.

      --  This package needs to be instantiated at library level

   package Desktop is

      type Save_Desktop_Function is access function
        (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
         return Gint_Xml.Node_Ptr;
      --  A general function that dumps the parameters of a widget into an XML
      --  tree.
      --
      --  Note: you should register one such function for all the widget types
      --  you will put in the MDI and that need to be saved when a desktop is
      --  saved. The MDI will call all the registered functions one after the
      --  other. Therefore, your function should return null if Widget is not
      --  of a type that is it can handle.

      type Load_Desktop_Function is access function
        (Node : Gint_Xml.Node_Ptr; User : User_Data)
         return Gtk.Widget.Gtk_Widget;
      --  A general function that loads a widget from an XML tree.
      --
      --  As for Save_Desktop_Function, this function should return null if it
      --  doesn't know how to handle Node or if Node doesn't describe a widget
      --  type that it can handle.

      procedure Register_Desktop_Functions
        (Save : Save_Desktop_Function;
         Load : Load_Desktop_Function);
      --  Register a set of functions to save and load desktops for some
      --  specific widget types.
      --  Neither Save nor Load can be null.

      procedure Restore_Desktop
        (MDI       : access MDI_Window_Record'Class;
         From_Tree : Gint_Xml.Node_Ptr;
         User      : User_Data);
      --  Restore the contents of the MDI from its saved XML tree.
      --  User is passed as a parameter to all of the Load_Desktop_Function
      --  registered by the widgets.

      function Save_Desktop
        (MDI : access MDI_Window_Record'Class) return Gint_Xml.Node_Ptr;
      --  Return an XML tree that describes the current contents of the MDI.
      --  This function calls each of the registered function for the children
      --  of the MDI.

   private
      type Register_Node_Record;
      type Register_Node is access Register_Node_Record;
      type Register_Node_Record is record
         Save : Save_Desktop_Function;
         Load : Load_Desktop_Function;
         Next : Register_Node;
      end record;

      Registers : Register_Node;
      --  Global variable that contains the list of functions that have been
      --  registered.
   end Desktop;

   function Desktop_Was_Loaded (MDI : access MDI_Window_Record) return Boolean;
   --  Return True if a desktop was loaded, False if the MDI is only the result
   --  of calls to Gtk_New and Put.

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
   --    actually deleted. The child is destroyed only if the handler returns
   --    False.
   --    Note that the Child passed in argument is exactly the one you passed
   --    to Put to insert it in the MDI window.
   --    Note that this is also the signal to use to prevent top level
   --    Gtk_Window from being destroyed.
   --  </signals>

private

   type State_Type is (Normal, Iconified, Floating, Docked);
   --  This type indicates the state of an item in the MDI:
   --  - Normal: the item can be manipulated (moved and resized) by the user.
   --      It is found either in the middle notebook (maximized items), or
   --      in the layout.
   --  - Iconified: the item has been minimized, and can only be moved by the
   --      user. No resize is taken into account. The item is also to be
   --      found in the middle notebook or layout.
   --  - Floating: the item has its own toplevel window, and is thus managed
   --      by the window manager.
   --  - Docked: The item has been put in one of the notebooks on the sides.
   --      (the middle notebook only contains Normal items).

   type MDI_Child_Record is new Gtk.Event_Box.Gtk_Event_Box_Record with record
      Initial : Gtk.Widget.Gtk_Widget;
      Initial_Child : Gtk.Widget.Gtk_Widget;
      --  The widget we used to build this child. This is used in case it
      --  was a window, since we need to be able to reparent it in the future,
      --  just in case.

      X, Y : Glib.Gint;
      --  Note: the coordinates of children are the coordinates inside
      --  MDI.Layout.

      State : State_Type := Normal;

      Title : GNAT.OS_Lib.String_Access;
      Short_Title : GNAT.OS_Lib.String_Access;
      --  Title of the item, as it appears in the title bar

      Dock : Dock_Side := None;
      --  The size on which the item should be docked. If None, then the item
      --  can not be docked, and nothing will happen when calling Dock_Child.

      Uniconified_Width, Uniconified_Height : Glib.Gint;
      --  The size of the window, when not iconified. When in normal state,
      --  this represents the size of the window, since we can not rely on
      --  Get_Allocation_Width and Get_Allocation_Height (in case for instance
      --  we just resized the widget but didn't go back to the main gtk loop).
      --  If these are set to -1, the child will be assigned its requested
      --  size.

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
      Minimize_Button : Gtk.Button.Gtk_Button;
   end record;

   procedure Gtk_New (Child : out MDI_Child;
                      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Create a new MDI child that contains widget.

   procedure Initialize (Child : access MDI_Child_Record;
                         Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   type Child_Iterator is record
      Iter : Gtk.Widget.Widget_List.Glist;
   end record;

   type Notebook_Array is array (Dock_Side) of Gtk.Notebook.Gtk_Notebook;
   type Int_Array is array (Left .. Bottom) of Glib.Gint;
   type Window_Array is array (Left .. Bottom) of Gdk.Window.Gdk_Window;

   type MDI_Window_Record is new Gtk.Fixed.Gtk_Fixed_Record with record
      Items : Gtk.Widget.Widget_List.Glist := Gtk.Widget.Widget_List.Null_List;
      --  The list of all MDI children.

      Docks : Notebook_Array := (others => null);
      --  The five possible docks (one on each side and one in the middle.
      --  Note that the one in the middle might not be visible, or even
      --  created, if it is replaced by a Gtk_Layout.

      Layout : Gtk.Fixed.Gtk_Fixed;
      --  The layout in the middle. It will be hidden when the items are
      --  maximized and put in the middle dock.

      Docks_Size : Int_Array := (others => 0);
      --  The size (height or width, depending on the location) of each of
      --  the docks. The size of the middle dock depends on the size of all
      --  the others.
      --  If the value is 0, this means there is no dock on that size.
      --  If the value is -1, this means that the value should be
      --  recomputed based on the size requested by the dock itself.

      Desktop_Was_Loaded : Boolean := False;
      --  True if a desktop was loaded

      Handles : Window_Array;
      --  The four handles that can be manipulated by the user to resize
      --  the docks. We use separate windows so as not to handle the events
      --  ourselves, but rely on the X server for this.

      Selected : Dock_Side := None;
      --  The handle that was selected for the resize operation.

      Selected_Child : MDI_Child := null;
      --  The child that was selected for a resize or move operation

      Xor_GC   : Gdk.GC.Gdk_GC;
      --  GC used while resizing or moving a child

      X_Root, Y_Root : Glib.Gint;
      Current_X, Current_Y, Current_W, Current_H : Glib.Gint;
      --  The coordinates of the initial click in a move or resize
      --  operation.

      Initial_Width, Initial_Height : Glib.Gint;
      --  Initial size of the child currently being resized.

      Focus_GC     : Gdk.GC.Gdk_GC;
      Non_Focus_GC : Gdk.GC.Gdk_GC;
      --  The various graphic contexts used to draw the titles of the
      --  children.

      Current_Cursor : Gdk_Cursor_Type;
      --  The cursor currently used within the MDI. It also indicates which
      --  kind of operation is processing (moving, resizing a corner, ...)

      Focus_Child : MDI_Child := null;
      --  The child that currently has the focus. Some default actions will
      --  apply to this child only.
      --  ??? Keypress events should be redirected to this child.

      Priorities : Priorities_Array := (0, 1, 2, 3);
      --  The order in which the docks should be displayed. See the
      --  description of Priorities_Array.

      Menu               : Gtk.Menu.Gtk_Menu;
      Dock_Menu_Item     : Gtk.Check_Menu_Item.Gtk_Check_Menu_Item;
      Dock_Menu_Item_Id  : Gtk.Handlers.Handler_Id;
      Float_Menu_Item    : Gtk.Check_Menu_Item.Gtk_Check_Menu_Item;
      Float_Menu_Item_Id : Gtk.Handlers.Handler_Id;
      --  The dynamic menu used to provide access to the most common
      --  functions of MDI.

      Default_X, Default_Y : Glib.Gint := 10;
      --  Default position when placing a new child.
   end record;

   pragma Inline (Get_Window);
   pragma Inline (Get_Widget);
   pragma Inline (Get_Focus_Child);
   pragma Inline (Get);
   pragma Inline (Next);
   pragma Inline (First_Child);
end Gtkada.MDI;
