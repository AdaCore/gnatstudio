-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2003                      --
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

with Gdk.Color;
with Gdk.Event;
with Gtk.Menu_Item;
with Gdk.Types;
with Gdk.Window;
with Glib.Object;
with Glib;                     use Glib;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Clist;
with Gtk.Combo;
with Gtk.Container;
with Gtk.Handlers;
with Gtk.Handlers;
with Gtk.List;
with Gtk.List_Item;
with Gtk.Menu;
with Gtk.Text_Iter;
with Gtk.Text_Mark;
with Gtk.Text_Tag;
with Gtk.Text_View;
with Gtk.Tree_Store;
with Gtk.Tree_Model;
with Gtk.Tree_View;
with Gtk.Widget;
with Pango.Font;
with String_List_Utils;

package GUI_Utils is

   ----------------------
   -- Combos and lists --
   ----------------------

   procedure Add_Unique_List_Entry
     (List    : access Gtk.List.Gtk_List_Record'Class;
      Text    : String;
      Prepend : Boolean := False);
   --  Add Text to List if it is not already there. Nothing is done if Text
   --  is already visible in the list. Text must be UTF8-encoded.

   procedure Add_Unique_Combo_Entry
     (Combo       : access Gtk.Combo.Gtk_Combo_Record'Class;
      Text        : String;
      Item_String : String := "";
      Use_Item_String : Boolean := False;
      Prepend     : Boolean := False);
   --  Add Text to the popdown list of Combo, if it is not already there.
   --  If the Text is already in the combo box, nothing is done.
   --  If Use_Item_String is True, then Item_String will be inserted in the
   --  combo box instead of text.
   --  Text must be UTF8-encoded.

   function Add_Unique_Combo_Entry
     (Combo       : access Gtk.Combo.Gtk_Combo_Record'Class;
      Text        : String;
      Item_String : String := "";
      Use_Item_String : Boolean := False;
      Prepend     : Boolean := False) return Gtk.List_Item.Gtk_List_Item;
   --  Same as above, but return the inserted item (or the previously existing
   --  one).

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

   ---------------
   -- Text_View --
   ---------------

   procedure Search_Entity_Bounds
     (Start_Iter : in out Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter   : out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Find the position of the begining and the end of the entity pointed to
   --  by Start_Iter.

   type Completion_Handler is access function
     (Input     : String;
      User_Data : Glib.Object.GObject)
     return String_List_Utils.String_List.List;
   --  This function should return a list of adequate elements that all
   --  begin with Input, or a list containing only Input.
   --  The list if freed automatically by the interactive console.
   --
   --  The strings returned in the list are the full replacement for Input, not
   --  the suffixes that need to be added to Input

   procedure Do_Completion
     (View            : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Completion      : Completion_Handler;
      Prompt_End_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      Uneditable_Tag  : Gtk.Text_Tag.Gtk_Text_Tag;
      User_Data       : Glib.Object.GObject);
   --  Handles completion in a console: given the text typed since
   --  Prompt_End_Mark, Completion will return the list of possible completion
   --  commands. If there is only one possible completion, it is inserted
   --  immediately, otherwise the list of possible completions is inserted in
   --  the buffer.
   --  Uneditable_Tag should be a tag belonging to View, whose property
   --  Editable_Property is set to False.

   ---------------
   -- Tree view --
   ---------------

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

   function Find_Iter_For_Event
     (Tree  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Get the iter in the tree view under the cursor corresponding to Event,
   --  if any.
   --  If Event is null, then the current selection is returned.
   --  ??? The column is also available, but not returned.

   function Freeze_Sort
     (Tree : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class)
      return Gint;
   --  Freeze the sorting in the tree view, and returns the current
   --  sort_column_id, which should be used when thawing.

   procedure Thaw_Sort
     (Tree : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Column_Id : Gint);
   --  Thaw a freezed tree_view.

   -------------------------
   -- Full_Path_Menu_Item --
   -------------------------

   --  This widget is used to associate strings to menu items.

   type Full_Path_Menu_Item_Record (Length : Natural) is
     new Gtk.Menu_Item.Gtk_Menu_Item_Record with private;
   type Full_Path_Menu_Item is access all Full_Path_Menu_Item_Record'Class;

   procedure Gtk_New
     (Menu_Item : out Full_Path_Menu_Item;
      Label     : String := "";
      Path      : String := "");
   --  Create a new menu item with the given Path as associated string.

   procedure Initialize
     (Menu_Item : access Full_Path_Menu_Item_Record'Class;
      Label     : String;
      Path      : String);
   --  Internal initialization function.

   function Get_Path
     (Menu_Item : access Full_Path_Menu_Item_Record) return String;
   --  Return the string associated with Menu_Item.

   ------------
   -- Events --
   ------------

   function Image
     (Key  : Gdk.Types.Gdk_Key_Type;
      Mods : Gdk.Types.Gdk_Modifier_Type) return String;
   --  Return a string suitable for display to show the key binding.
   --  Special_Key_Binding is returned if the key cannot be described

   procedure Value
     (From : String;
      Key  : out Gdk.Types.Gdk_Key_Type;
      Mods : out Gdk.Types.Gdk_Modifier_Type);
   --  Revert of Image

   Special_Key_Binding : constant String := "<special>";

   procedure Key_Grab
     (In_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Key  : out Gdk.Types.Gdk_Key_Type;
      Mods : out Gdk.Types.Gdk_Modifier_Type);
   --  Temporarily grab the pointer and keyboards for In_Widget, and returns
   --  the first fully defined key that the user has pressed. (Key, Mods) is
   --  set to (0, 0) if no key could be grabbed.
   --  Nothing is done in In_Widget, it is only used as a target for the grab
   --  operations.
   --  In_Widget must be realized.
   --
   --  In_Widget mustn't be a modal dialog, since otherwise the handling of
   --  grabs will interfer with the dialog.

   --------------
   -- Tooltips --
   --------------

   procedure Create_Pixmap_From_Text
     (Text       : String;
      Font       : Pango.Font.Pango_Font_Description;
      Bg_Color   : Gdk.Color.Gdk_Color;
      Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Pixmap     : out Gdk.Gdk_Pixmap;
      Width      : out Glib.Gint;
      Height     : out Glib.Gint;
      Wrap_Width : Gint := -1);
   --  Create a new pixmap that contains Text. Bg_Color is used for the
   --  background of the pixmap.
   --  Widget is used to create the graphic context for the pango layout.
   --
   --  This procedure handles multi-lines text, as well as alignment of
   --  tabulations, right-to-left writting, ...
   --  Text must be a correct Utf8 text, see Glib.Convert
   --
   --  The maximal width of the text is Wrap_Width.

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

   ------------------
   -- Accelerators --
   ------------------

   procedure Save_Accel_Map (Filename : String);
   --  Save the current accel map.
   --  As opposed to Gtk.Accel_Map.Save, this one doesn't keep the lines which
   --  have no shortcut, thus keeping the file small.

private

   type Full_Path_Menu_Item_Record (Length : Natural) is
     new Gtk.Menu_Item.Gtk_Menu_Item_Record
   with record
      Full_Path : String (1 .. Length);
   end record;

end GUI_Utils;
