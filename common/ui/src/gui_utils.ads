------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

--  This package contains a series of subprograms that can be used
--  for misc. graphical tasks.

with System;

with GNAT.Strings;

with Glib.Object;
with Glib.Values;
with Glib;                     use Glib;
with Glib.Menu;                use Glib.Menu;

with Gdk.Event;
with Gdk.Types;
with Gdk.RGBA;

with Gtk.Accel_Group;
with Gtk.Button;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Combo_Box;
with Gtk.Container;
with Gtk.Enums;
with Gtk.Event_Box;
with Gtk.Handlers;
with Gtk.Info_Bar;
with Gtk.Label;
with Gtk.List_Store;
with Gtk.Menu;
with Gtk.Menu_Bar;
with Gtk.Menu_Item;
with Gtk.Message_Dialog;
with Gtk.Paned;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
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
with Gtkada.MDI;
with String_List_Utils;

package GUI_Utils is

   Icon_Size_Speedbar : constant Glib.Gint := 9;
   Icon_Size_Menus    : constant Glib.Gint := 16;
   Icon_Size_Buttons  : constant Glib.Gint := 16;
   --  Sizes corresponding to those defined in Gtk.Enums, but suitable for
   --  use with named icons (Gtk.Icon_Theme)

   function Query_User
     (Parent        : Gtk.Window.Gtk_Window;
      Prompt        : String;
      Password_Mode : Boolean;
      Urgent        : Boolean := True;
      Default       : String := "") return String;
   --  Open a new Dialog to query a response to the user.
   --  If Password_Mode is set, then the query will print * instead of
   --   the entered characters.
   --  If Urgent is set, then the Urgency_Hint will be set for the dialog.
   --  Default is the default string set in the user entry.
   --  Return "" if the user pressed cancel or did not enter anything

   ------------
   -- Colors --
   ------------

   function Darken (Color : Gdk.RGBA.Gdk_RGBA) return Gdk.RGBA.Gdk_RGBA;
   function Lighten (Color : Gdk.RGBA.Gdk_RGBA) return Gdk.RGBA.Gdk_RGBA;
   --  Darken or lighten a color. This is linear darkening for all of RGB
   --  components.
   --  The returned color has been allocated

   function Darken_Or_Lighten
     (Color : Gdk.RGBA.Gdk_RGBA) return Gdk.RGBA.Gdk_RGBA;
   --  Darken or lighten a color depending on its current luminosity. The goal
   --  is to obtain a contrast between the two
   --  The returned color has been allocated

   -----------
   -- Focus --
   -----------

   procedure Grab_Toplevel_Focus
     (MDI         : not null access Gtkada.MDI.MDI_Window_Record'Class;
      Widget      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Present     : Boolean := True);
   --  Grab the focus on the widget and its toplevel window.
   --  Simply calling Grab_Focus would not change the toplevel window, so the
   --  actual keyboard focus would not be given directly to the widget until
   --  the user explicitly changes the active toplevel window.
   --  If Present, then also call Gtk.Windows.Present on the window, and
   --  Raise the child that is being given the focus.

   -------------
   -- Buttons --
   -------------

   procedure Gtk_New_From_Name_And_Label
     (Button    : out Gtk.Button.Gtk_Button;
      Icon_Name : String;
      Label     : String);
   --  Create a new button, that uses the image from a stock icon, but with
   --  a specific text.

   ----------------------
   -- Combos and lists --
   ----------------------

   function Add_Unique_List_Entry
     (List    : access Gtk.List_Store.Gtk_List_Store_Record'Class;
      Text    : String;
      Prepend : Boolean := False;
      Col     : Gint := 0) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Add Text to List if it is not already there. Nothing is done if Text
   --  is already visible in the list. Text must be UTF8-encoded.

   procedure Add_Unique_Combo_Entry
     (Combo        : access Gtk.Combo_Box.Gtk_Combo_Box_Record'Class;
      Text         : String;
      Select_Text  : Boolean := False;
      Prepend      : Boolean := False;
      Col          : Gint := 0;
      Case_Sensitive : Boolean := True);
   --  Add Text to the popdown list of a text combo_box, if it is not already
   --  there.
   --  If the Text is already in the combo box, nothing is done.
   --  If Select_Text is set, then the already existing text or the newly
   --  inserted one will be selected.
   --  Text must be UTF8-encoded.

   function Add_Unique_Combo_Entry
     (Combo        : access Gtk.Combo_Box.Gtk_Combo_Box_Record'Class;
      Text         : String;
      Select_Text  : Boolean := False;
      Prepend      : Boolean := False;
      Col          : Gint := 0;
      Case_Sensitive : Boolean := True) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Same as above, but return the inserted iter (or the previously existing
   --  one).

   procedure Set_Active_Text
     (Combo        : access Gtk.Combo_Box.Gtk_Combo_Box_Record'Class;
      Text         : String;
      Col          : Gint := 0;
      Case_Sensitive : Boolean := True);
   --  Select the item containing Text in the Combo.

   procedure Set_Busy_Cursor
     (Window        : Gdk.Gdk_Window;
      Busy          : Boolean := True;
      Force_Refresh : Boolean := False);
   --  Enable or disable the "busy" cursor for a specific top-level window.
   --  If Force_Refresh is True, then all X11 events are processed so that the
   --  new cursor is immediately visible for the user.

   type Filter_Function is access
     function (W : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;

   procedure Remove_All_Children
     (Container : access Gtk.Container.Gtk_Container_Record'Class;
      Filter    : Filter_Function := null);
   --  Remove and destroy all the children from Container for which Filter
   --  returns True. All children are removed if Filter is null.
   --  If Container is a menu, this empties the menu, thus allowing dynamic
   --  menus.

   function Has_Children
     (Container : not null access Gtk.Container.Gtk_Container_Record'Class)
      return Boolean;
   --  Whether the container has at least one child widget.

   ----------------------
   -- Scrolled_Windows --
   ----------------------

   procedure Scroll_To_Child
     (Self  : not null access Gtk_Scrolled_Window_Record'Class;
      Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adjust the given Gtk_Scrolled_Window so that it displays the given Child
   --  widget.

   ---------------
   -- Text_View --
   ---------------

   procedure Search_Entity_Bounds
     (Start_Iter : in out Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
      Maybe_File : Boolean := False);
   --  Find the position of the begining and the end of the entity pointed to
   --  by Start_Iter.
   --  If Maybe_File is True, consider that the entity may be a filename.

   type Completion_Handler is access function
     (Input     : String;
      View      : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      User_Data : System.Address)
      return String_List_Utils.String_List.Vector;
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

   procedure Set_Placeholder
     (View    : not null access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Message : String);
   --  Add a placeholder in View.
   --  This is a message that is displayed in italic gray in the view while it
   --  is empty. As soon as the user gives the keyboard focus, the placeholder
   --  is removed to let the user type new text.

   procedure Show_Placeholder_If_Needed
     (View    : not null access Gtk.Text_View.Gtk_Text_View_Record'Class);
   --  If the view is empty, display the placeholder.
   --  This is only needed after programmatically setting the buffer's
   --  contents. Done automatically on focus in/out events

   function Get_Text_Without_Placeholder
     (View    : not null access Gtk.Text_View.Gtk_Text_View_Record'Class)
      return String;
   --  Return the full text of the view, ignoring the placeholder text if any

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
      Event : Gdk.Event.Gdk_Event_Button) return Gtk.Tree_Model.Gtk_Tree_Iter;
   function Find_Iter_For_Event
     (Tree  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Get the iter in the tree view under the cursor corresponding to Event,
   --  if any.
   --  If Event is null, then the current selection is returned.

   procedure Coordinates_For_Event
     (Tree   : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Button;
      Iter   : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : out Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   procedure Coordinates_For_Event
     (Tree   : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      X, Y   : Gdouble;
      Iter   : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : out Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   --  Get the Iter and Column corresponding to the position under the
   --  cursor corresponding to Event, if any. Otherwise return the current
   --  selection.

   procedure Move_Row
     (View    : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Iter    : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Forward : Boolean := True);
   --  Move the iterator to the next row.
   --  This row might be a child of the current position, a sibling or a
   --  parent.

   function Find_Node
     (Model  : Gtk.Tree_Store.Gtk_Tree_Store;
      Name   : String;
      Column : Gint;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Find in Model a node matching Name in Column.
   --  return Gtk_Null_Iter if there is no such node.
   --  If a Parent is specified, the subprogram will try to match the given
   --  Name in the Parent's children.

   procedure Remove_Child_Nodes
     (Model  : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Remove all children nodes of Parent

   procedure Expand_Row
     (Tree  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Expand the node at Iter

   function Get_Selection
     (Tree : access Gtk.Tree_View.Gtk_Tree_View_Record'Class) return String;
   --  Return the content of the current selection

   procedure Select_First_Row
     (Tree : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class);
   --  Select the first row of the given tree view, if it exists.

   type Editable_Cb is access procedure
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   type Editable_Callback_Array is array (Natural range <>) of Editable_Cb;

   type Editing_Canceled_Cb is access procedure
     (Tree   : access Glib.Object.GObject_Record'Class);

   GType_Icon_Name_String : constant GType := GType'Last - GType_String;
   --  Special value for use in Column_Types (and only there!) to indicate
   --  that the column contains the name of an icon

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
      Editable_Callback  : Editable_Callback_Array := (1 .. 0 => null);
      Editing_Canceled   : Editing_Canceled_Cb := null)
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
   --  The resulting view should be added to a scrolled_window.
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
   --  If specified, Editing_Canceled is called whenever editing stops, after
   --  the user presses <esc>. In this case, the Editable_Callback is not
   --  called.
   --
   --  Limitations:
   --     Radio buttons not supported,

   ------------
   -- Events --
   ------------

   function Image
     (Key    : Gdk.Types.Gdk_Key_Type;
      Button : Guint;
      Mods   : Gdk.Types.Gdk_Modifier_Type) return String;
   --  Return a string suitable for display to show the key binding.
   --  Special_Key_Binding is returned if the key cannot be described

   procedure Value
     (From   : String;
      Key    : out Gdk.Types.Gdk_Key_Type;
      Button : out Guint;
      Mods   : out Gdk.Types.Gdk_Modifier_Type);
   --  Revert of Image

   Special_Key_Binding    : constant String := "<special>";

   -----------
   -- Paned --
   -----------

   procedure Switch_Paned_Orientation
     (Paned : in out Gtk.Paned.Gtk_Paned);
   --  Switch the orientation of the given paned view.
   --
   --  The original paned view is destroyed by this procedure: make sure to
   --  update any holding references and to add it back again to it's original
   --  container. The two paned children are not destroyed though.
   --
   --  Code example:
   --
   --       --  Create an horizontal paned view
   --       Gtk_New_Hpaned (Paned);
   --
   --       --  Add some children to it
   --       Paned.Pack1 (Child_1);
   --       Paned.Pack2 (Child_2);
   --
   --       --  Add it to a container
   --       Parent.Add (Paned);
   --
   --       --  Switch the paned orientation
   --       Switch_Paned_Orientation (Paned);
   --
   --       --  Add it back again to its original parent container
   --       Parent.Add (Paned)

   function Get_Position_Percent
     (Paned   : not null access Gtk.Paned.Gtk_Paned_Record'Class) return Float;
   --  Return a percentage for the paned view's separator according to its
   --  width or height depending on its orientation.

   procedure Set_Position_Percent
     (Paned   : not null access Gtk.Paned.Gtk_Paned_Record'Class;
      Percent : Float);
   --  Set the position of the paned view's separator according to the given
   --  size percentage.
   --  If the paned view's size has not been allocated yet, this procedure
   --  does nothing.

   -----------
   -- Menus --
   -----------

   GPS_Id_Attribute : constant String := "gps-id";
   --  An attribute set on items in a menu model's separators, to name sections

   type Menu_Item_Info is record
      Item     : Gmenu_Item;
      Model    : Gmenu;
      Position : Gint;
   end record;
   No_Menu_Item : constant Menu_Item_Info := (null, null, -1);

   procedure Unref (Self : in out Menu_Item_Info);
   --  Free the memory as needed

   function Find_Or_Create_Menu
      (Model        : not null access Gmenu_Record'Class;
       Path         : String;
       Allow_Create : Boolean := True) return Menu_Item_Info;
   --  Search for the corresponding menu item.
   --  The result must be unrefed by the caller.
   --  Path still has double undescores and backslashes to protect slashes.
   --  The position of the item is unset when a new item is created.

   function Find_Or_Create_Single_Level
      (Model        : not null access Gmenu_Record'Class;
       Name         : String;
       Allow_Create : Boolean) return Menu_Item_Info;
   --  Find or create an item, only looking in Model (not recursive).
   --  Return value must be unrefered by caller.
   --  If Allow_Create is True, non-null will always be returned.
   --  Name must have been unescaped.

   procedure Find_Menu_Item_By_Name
     (Menu_Bar      : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Menu          : Gtk.Menu.Gtk_Menu;
      Name          : String;
      Menu_Item     : out Gtk.Menu_Item.Gtk_Menu_Item;
      Index         : out Gint);
   pragma Obsolescent (Find_Menu_Item_By_Name);
   --  Return the menu item with name Name, either from Menu, or from Menu_Bar
   --  if the latter is null.
   --  The name must be escape (via Escape_Menu_Name) as would be done to
   --  create a menu item.

   function Find_Or_Create_Menu_Tree
     (Menu_Bar      : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Menu          : Gtk.Menu.Gtk_Menu;
      Path          : String;
      Accelerators  : Gtk.Accel_Group.Gtk_Accel_Group;
      Allow_Create  : Boolean := True;
      Ref_Item      : String  := "";
      Add_Before    : Boolean := True;
      New_Item      : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class := null)
      return Gtk.Menu_Item.Gtk_Menu_Item;
   pragma Obsolescent (Find_Or_Create_Menu_Tree);
   --  Create or return the menu_item corresponding to Path in Menu.
   --  Path is a '/'-separated list of menu names, for instance File/New.
   --  Menu_Bar is used if Menu is null
   --  null is returned if the menu couldn't be created.
   --  New_Item, if specified, is used when a new item needs to be created. It
   --  shouldn't be Initialized, since that will be done automatically by this
   --  function. If it is used, it is returned by this function. If it isn't
   --  used, memory is deallocated for it through Unchecked_Deallocation.

   function Escape_Menu_Name (Name : String) return String;
   function Unescape_Menu_Name (Name : String) return String;
   --  Escape special characters (/ and \) in the menu name, so that
   --  Find_Or_Create_Menu_Tree keeps this as a single name, unsplit.

   function Escape_Underscore (Name : String) return String;
   function Unescape_Underscore (Name : String) return String;
   --  Protects single underscores so that they are preserved in the
   --  menu name.

   function Parent_Menu_Name (Name : String) return String;
   --  Return the path to the parent menu. The return value always ends with
   --  a trailing '/'

   function First_Parent_Menu_Name (Name : String) return String;
   --  Return the name of the first parent menu from the path. The return
   --  value ends with a trailing '/' or empty string if no parents.

   function Base_Menu_Name (Path : String) return String;
   --  Return the name of the menu item, from its path.
   --  This removes the escaping that might been added thought Escape_Menu_Name

   function Create_Menu_Path (Parent, Menu : String) return String;
   --  Create a menu from its parent (possibly empty) and its name (which might
   --  need to be escaped first).

   procedure Add_Menu
     (Parent     : Gtk.Menu.Gtk_Menu;
      Menu_Bar   : Gtk.Menu_Bar.Gtk_Menu_Bar := null;
      Item       : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      Index      : Gint    := -1;
      Add_Before : Boolean := True);
   --  Append Item either to Parent, if not null, or directly to the menu
   --  bar.
   --  The menu is appended if Index is -1

   function Format (S : String) return String;
   --  Call Format_Pathname on S and add ending slash.

   ----------------------
   -- Contextual menus --
   ----------------------

   type Contextual_Menu_Create is access function
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Gtk.Menu.Gtk_Menu;
   --  Function used to create the contextual menu for Widget.
   --  This function is only called for the right mouse button, so it doesn't
   --  need to check that.

   procedure Register_Contextual_Menu
     (Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Create  : Contextual_Menu_Create);
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

      type Callback_User_Data is record
         Menu_Create  : Contextual_Menu_Create;
         User         : User_Data;
      end record;

      package Contextual_Callback is new Gtk.Handlers.User_Return_Callback
        (Gtk.Widget.Gtk_Widget_Record, Boolean, Callback_User_Data);

      procedure Register_Contextual_Menu
        (Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
         User         : User_Data;
         Menu_Create  : Contextual_Menu_Create);

   end User_Contextual_Menus;
   --  Same as the procedure Register_Contextual_Menu, but the callbacks for
   --  the menu creation takes any type as input.
   --  This package must be instantiated as library level.

   -----------
   -- Label --
   -----------

   procedure Create_Blue_Label
     (Label : out Gtk.Label.Gtk_Label;
      Event : out Gtk.Event_Box.Gtk_Event_Box);
   --  Create a new label, with a blue background.
   --  Event should be inserted in the container, and the text should be set
   --  in Label.

   procedure Create_Warning_Label
     (Msg   : String;
      Label : out Gtk.Label.Gtk_Label);
   --  Create a new warning label, see the style defined in CSS

   --------------
   -- Infobars --
   --------------

   function Create_Info_Bar
     (Message      : String;
      Message_Type : Gtk.Message_Dialog.Gtk_Message_Type)
      return Gtk.Info_Bar.Gtk_Info_Bar;
   --  Create an info bar that displays the given Message.
   --  A little close button is added on the left of the infobar: when
   --  clicked, this button hides the info bar.
   --  The Message_Type parameter is used to set the style of the info bar.

   ----------
   -- Misc --
   ----------

   function Create_Logo_And_Title_Area return Gtk.Widget.Gtk_Widget;
   --  Create a widget displaying the GPS logo and title (See the Welcome view
   --  and the Welcome window for examples).

end GUI_Utils;
