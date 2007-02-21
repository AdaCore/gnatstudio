-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2000-2007                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with System;

with GNAT.Strings;

with Glib.Object;
with Glib.Values;
with Glib;                     use Glib;
with Gdk.Color;
with Gdk.Event;
with Gtk.Menu_Item;
with Gdk.Types;
with Gdk.Window;
with Gtk.Accel_Group;          use Gtk.Accel_Group;
with Gtk.Button;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Clist;
with Gtk.Combo;
with Gtk.Container;
with Gtk.Enums;
with Gtk.Event_Box;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.List;
with Gtk.List_Item;
with Gtk.Menu;
with Gtk.Menu_Bar;
with Gtk.Text_Iter;
with Gtk.Text_Mark;
with Gtk.Text_Tag;
with Gtk.Text_View;
with Gtk.Tree_Store;
with Gtk.Tree_Model;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;
with Gtk.Widget;
with Gtk.Window;
with Pango.Font;
with String_List_Utils;

package GUI_Utils is

   function Query_User
     (Parent        : Gtk.Window.Gtk_Window;
      Prompt        : String;
      Password_Mode : Boolean) return String;
   --  Open a new Dialog to query a response to the user.
   --  If Password_Mode is set, then the query will print * instead of
   --   the entered characters.
   --  Return "" if the user hasn't entered anything

   -------------
   -- Buttons --
   -------------

   procedure Gtk_New_From_Stock_And_Label
     (Button   : out Gtk.Button.Gtk_Button;
      Stock_Id : String;
      Label    : String);
   --  Create a new button, that uses the image from a stock icon, but with
   --  a specific text

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
     (Combo           : access Gtk.Combo.Gtk_Combo_Record'Class;
      Text            : String;
      Item_String     : String := "";
      Use_Item_String : Boolean := False;
      Prepend         : Boolean := False);
   --  Add Text to the popdown list of Combo, if it is not already there.
   --  If the Text is already in the combo box, nothing is done.
   --  If Use_Item_String is True, then Item_String will be inserted in the
   --  combo box instead of text.
   --  Text must be UTF8-encoded.

   function Add_Unique_Combo_Entry
     (Combo           : access Gtk.Combo.Gtk_Combo_Record'Class;
      Text            : String;
      Item_String     : String := "";
      Use_Item_String : Boolean := False;
      Prepend         : Boolean := False) return Gtk.List_Item.Gtk_List_Item;
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

   function Find_First_Row_Matching
     (Clist  : access Gtk.Clist.Gtk_Clist_Record'Class;
      Column : Glib.Gint;
      Text   : String) return Glib.Gint;
   --  Return the index of the first row that contains Text in the column
   --  Column.
   --  -1 is returned if no such row was found.

   type Filter_Function is access
     function (W : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;

   procedure Remove_All_Children
     (Container : access Gtk.Container.Gtk_Container_Record'Class;
      Filter    : Filter_Function := null);
   --  Remove and destroy all the children from Container for which Filter
   --  returns True. All children are removed if Filter is null.
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
      User_Data : System.Address)
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
      User_Data       : System.Address);
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

   procedure Coordinates_For_Event
     (Tree   : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Iter   : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : out Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   --  Get the Iter and Column corresponding to the position under the
   --  cursor corresponding to Event, if any. Otherwise return the current
   --  selection.

   function Find_Node
     (Model  : Gtk.Tree_Store.Gtk_Tree_Store;
      Name   : String;
      Column : Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Find in Model a node matching Name in Column.
   --  return Gtk_Null_Iter if there is no such node

   procedure Expand_Row
     (Tree  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Expand the node at Iter

   function Get_Selection
     (Tree : access Gtk.Tree_View.Gtk_Tree_View_Record'Class) return String;
   --  Return the content of the current selection

   type Editable_Cb is access procedure
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   type Editable_Callback_Array is array (Natural range <>) of Editable_Cb;

   function Create_Tree_View
     (Column_Types       : Glib.GType_Array;
      Column_Names       : GNAT.Strings.String_List;
      Show_Column_Titles : Boolean := True;
      Selection_Mode     : Gtk.Enums.Gtk_Selection_Mode :=
        Gtk.Enums.Selection_Single;
      Sortable_Columns   : Boolean := True;
      Initial_Sort_On    : Integer := -1;
      Hide_Expander      : Boolean := False;
      Merge_Icon_Columns : Boolean := True;
      Editable_Columns   : Glib.Gint_Array := (1 .. 0 => -1);
      Editable_Callback  : Editable_Callback_Array := (1 .. 0 => null))
      return Gtk.Tree_View.Gtk_Tree_View;
   --  Create a new simple tree view, where each column in the view is
   --  associated with a column in the model.
   --  Column_Names'Length is the number of columns in the view. If there are
   --  less columns in the view than in the model, then the matching columns
   --  in the model will not be visible on screen. There can't be more columns
   --  in the view than in the model, extra columns will simply be ignored.
   --  The caller is responsible for freeing Column_Names.
   --
   --  Columns associated with a boolean value will be rendered as a toggle
   --  button. By default, they can be toggled interactively by the user. If
   --  you want to prevent that, you need to set the activatable property on
   --  the appropriate renderer, with code like:
   --     List := Get_Cell_Renderers (Get_Column (Tree, 0));
   --     Add_Attribute (Get_Column (Tree, 0),
   --                    Cell_Renderer_List.Get_Data (List),
   --                    "activatable", 1);
   --     Cell_Renderer_List.Free (List);
   --
   --  The resulting view should be added to a scrolled_window
   --
   --  If Sortable_Columns is True (and in this case Show_Column_Titles should
   --  be as well in most cases), then all columns in the view are sortable.
   --  If Initial_Sort_On is not -1, then the view will be sorted automatically
   --  initially (it is recommended not to do that if you have lots of info to
   --  store in the model afterward). The value of Initial_Sort_On is
   --  an index in Column_Names.
   --
   --  If Hide_Expander is true, then the expander will be hidden and no space
   --  reserved for it in the tree view. You can later decide to show it again
   --  through calls to Set_Visible (Get_Expander_Column(..), True).
   --  The latter will not work if you have set Hide_Expander to true, since no
   --  specific column will be created for the expander in this case.
   --
   --  Merge_Icon_Columns should be true if the icon and the following text
   --  should be in the same column (and thus indented the same for child
   --  nodes). In some specific cases, we might want the icon in a separate
   --  column, so that we can handle differently the click on the icon and the
   --  click on the text.
   --
   --  Editable_Columns can be used to indicate which columns should be
   --  editable (when the element is positive, it indicates a column in the
   --  model that contains a boolean. This boolean indicates whether the cell
   --  is editable). Its indexes match the ones in Column_Types.
   --  If Editable_Callback is specified, this is an additional callback called
   --  when the columns has been edited.
   --  This could be done afterwards with a code like:
   --      Col     := Get_Column (View, 2);
   --      Renders := Get_Cell_Renderers (Col);
   --      Text_Render := Gtk_Cell_Renderer_Text (Get_Data (First (Renders)));
   --      Add_Attribute (Col, Text_Render, "editable", 1);
   --      Free (Renders);
   --      Set_Editable_And_Callback (Get_Model (View), Text_Render, 1);
   --      Widget_Callback.Object_Connect
   --       (Text_Render, "edited", Callback'Access, Slot_Object => View);
   --
   --  Limitations:
   --     Radio buttons not supported,

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
      Key       : out Gdk.Types.Gdk_Key_Type;
      Mods      : out Gdk.Types.Gdk_Modifier_Type);
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
      Wrap_Width : Gint := -1;
      Use_Markup : Boolean := False);
   --  Create a new pixmap that contains Text. Bg_Color is used for the
   --  background of the pixmap.
   --  Widget is used to create the graphic context for the pango layout.
   --
   --  This procedure handles multi-lines text, as well as alignment of
   --  tabulations, right-to-left writting, ...
   --  Text must be a correct Utf8 text, see Glib.Convert
   --
   --  If the displayed text's height is greater than the screen's height, it
   --  will be truncated.
   --
   --  The maximal width of the text is Wrap_Width.
   --
   --  If Use_Markup is true, then some html markup will be taken into account
   --  when rendering the text, for instance <b>,...

   -----------
   -- Menus --
   -----------

   procedure Find_Menu_Item_By_Name
     (Menu_Bar      : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Menu          : Gtk.Menu.Gtk_Menu;
      Name          : String;
      Menu_Item     : out Gtk.Menu_Item.Gtk_Menu_Item;
      Index         : out Gint;
      Use_Mnemonics : Boolean := True);
   --  Return the menu item with name Name, either from Menu, or from Menu_Bar
   --  if the latter is null.
   --  If Use_Mnemonics is True, then '_' characters in the path will indicate
   --  keyboard shortcuts, and need to be doubled to actually display an
   --  underscore.

   function Find_Or_Create_Menu_Tree
     (Menu_Bar      : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Menu          : Gtk.Menu.Gtk_Menu;
      Path          : String;
      Accelerators  : Gtk.Accel_Group.Gtk_Accel_Group;
      Allow_Create  : Boolean := True;
      Ref_Item      : String  := "";
      Add_Before    : Boolean := True;
      Use_Mnemonics : Boolean := True;
      New_Item      : Gtk.Menu_Item.Gtk_Menu_Item := null)
      return Gtk.Menu_Item.Gtk_Menu_Item;
   --  Create or return the menu_item corresponding to Path in Menu.
   --  Path is a '/'-separated list of menu names, for instance File/New.
   --  Menu_Bar is used if Menu is null
   --  null is returned if the menu couldn't be created.
   --  If Use_Mnemonics is True, then '_' characters in the path will indicate
   --  keyboard shortcuts, and need to be doubled to actually display an
   --  underscore.
   --  New_Item, if specified, is used when a new item needs to be created. It
   --  shouldn't be Initialized, since that will be done automatically by this
   --  function. If it is used, it is returned by this function. If it isn't
   --  used, memory is deallocated for it through Unchecked_Deallocation.

   procedure Add_Menu
     (Parent     : Gtk.Menu.Gtk_Menu;
      Menu_Bar   : Gtk.Menu_Bar.Gtk_Menu_Bar := null;
      Item       : Gtk.Menu_Item.Gtk_Menu_Item;
      Index      : Gint    := -1;
      Add_Before : Boolean := True);
   --  Append Item either to Parent, if not null, or directly to the menu
   --  bar.
   --  The menu is appended if Index is -1

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

   -----------
   -- Label --
   -----------

   procedure Create_Blue_Label
     (Label : out Gtk.Label.Gtk_Label;
      Event : out Gtk.Event_Box.Gtk_Event_Box);
   --  Create a new label, with a blue background.
   --  Event should be inserted in the container, and the text should be set
   --  in Label.

private

   type Full_Path_Menu_Item_Record (Length : Natural) is
     new Gtk.Menu_Item.Gtk_Menu_Item_Record
   with record
      Full_Path : String (1 .. Length);
   end record;

end GUI_Utils;
