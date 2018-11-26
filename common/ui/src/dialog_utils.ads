------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
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

--  This package defines widget types and subprograms to create views within
--  dialogs, in order to have a common look and feel for all the dialog views
--  in GPS.
--
--  See the comments below the defined types and subprograms to have more
--  details on how to use this package.

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Glib;                                  use Glib;
with Glib.Object;                           use Glib.Object;

with Gtk.Box;                               use Gtk.Box;
with Gtk.Button;                            use Gtk.Button;
with Gtk.Enums;                             use Gtk.Enums;
with Gtk.Frame;                             use Gtk.Frame;
with Gtk.Flow_Box;                          use Gtk.Flow_Box;
with Gtk.Flow_Box_Child;                    use Gtk.Flow_Box_Child;
with Gtk.Label;                             use Gtk.Label;
with Gtk.Scrolled_Window;                   use Gtk.Scrolled_Window;
with Gtk.Size_Group;                        use Gtk.Size_Group;
with Gtk.Widget;                            use Gtk.Widget;

package Dialog_Utils is

   ------------------
   -- Dialog Views --
   ------------------

   type Dialog_View_Record is new Gtk_Scrolled_Window_Record with private;
   type Dialog_View is access all Dialog_View_Record'Class;
   --  Type defining a dialog view.
   --
   --  This type should be used when creating views that are displayed in
   --  GPS dialogs, in order to have a common look and feel among GPS dialogs.
   --
   --  Dialog views are usually organized in groups (see the
   --  'Dialog_Group_Widget_Record' type definintion for more explanations
   --  about groups).
   --
   --  It can also be used as a general container for standalone widgets
   --  (e.g : a tree view).

   procedure Initialize
     (Self : not null access Dialog_View_Record'Class);
   --  Initialize the common attributes for all the dialog views

   function Get_Number_Of_Children
     (Self : not null access Dialog_View_Record'Class) return Natural;
   --  Return the number of children the given dialog view

   procedure Append
     (Self          : not null access Dialog_View_Record'Class;
      Widget        : not null access Gtk_Widget_Record'Class;
      Expand        : Boolean := True;
      Fill          : Boolean := True;
      Add_Separator : Boolean := True);
   --  Append an already built widget to the given dialog view
   --
   --  The Expand and Fill properties have the same role as in the
   --  Gtk.Box.Pack_Start procedure.
   --
   --  If Add_Separator is True, a separator is appended before the given
   --  Widget.

   procedure Insert
     (Self          : not null access Dialog_View_Record'Class;
      Widget        : not null access Gtk_Widget_Record'Class;
      Position      : Gint;
      Expand        : Boolean := True;
      Fill          : Boolean := True;
      Add_Separator : Boolean := True);
   --  Insert an already built widget in the given dialog view, at Position
   --  (starting from 0).
   --
   --  The Expand and Fill properties have the same role as in the
   --  Gtk.Box.Pack_Start procedure.
   --
   --  If Add_Separator is True, a separator is appended before the given
   --  Widget.

   procedure Remove_All_Children
     (Self : not null access Dialog_View_Record'Class);
   --  Remove all the children that have been appended to the dialog view

   procedure Remove_Child
     (Self      : not null access Dialog_View_Record'Class;
      Child_Key : String);
   --  Remove the child identified by Child_Key, if found.

   procedure Set_Child_Visible
     (Self      : not null access Dialog_View_Record'Class;
      Child_Key : String;
      Visible   : Boolean);
   --  Set the visibility of the dialog view child associated with Child_Key.
   --  Do nothing if there is no association for the given Child_Key.

   procedure Set_Child_Highlighted
     (Self      : not null access Dialog_View_Record'Class;
      Child_Key : String;
      Highlight : Boolean);
   --  If Highlight is True, highlight the dialog view child associated with
   --  Child_Key. Otherwise, unhighlight it.
   --  Do nothing if there is no association for the given Child_Key.

   procedure Display_Information_On_Child
     (Self      : not null access Dialog_View_Record'Class;
      Child_Key : String;
      Message   : String;
      Is_Error  : Boolean := False);
   --  Add a tooltip with the given message on the child widget associated with
   --  Child_Key.
   --  If Is_Error is True, some graphical changes (e.g: background color) are
   --  made on the child widget to indicate that there is an error.

   procedure Remove_Information_On_Child
     (Self      : not null access Dialog_View_Record'Class;
      Child_Key : String);
   --  Remove the tooltip and revert the graphical changes that have been set
   --  via the 'Display_Information_On_Child', if any.

   function Create_Dialog_View_With_Message
     (Message : String) return Dialog_View;
   --  Create a new dialog view with a message header at the top of it.

   ------------------------------------
   -- Dialog Views With Button Boxes --
   ------------------------------------

   type Dialog_View_With_Button_Box_Record is new Dialog_View_Record
   with private;
   type Dialog_View_With_Button_Box is
     access all Dialog_View_With_Button_Box_Record'Class;
   --  Type representing a dialog view with its associated button box.

   procedure Initialize
     (Self     : not null access Dialog_View_With_Button_Box_Record'Class;
      Position : Gtk_Position_Type);
   --  Initialize the dialog view and its associated button box, placing it at
   --  the given Position.

   procedure Append_Button
     (Self   : not null access Dialog_View_With_Button_Box_Record'Class;
      Button : not null access Gtk_Button_Record'Class);
   --  Append the given Button to dialog view's button box, aligning it with
   --  the previously appended buttons.

   --------------------------
   -- Dialog Group Widgets --
   --------------------------

   type Dialog_Group_Widget_Record is new Gtk_Frame_Record with private;
   type Dialog_Group_Widget is
     access all Dialog_Group_Widget_Record'Class;
   --  Type used to represent group widgets with dialog views.
   --
   --  This type should be used to gather and align related widgets within a
   --  dialog view (e.g: a 'Behaviour' group gathering widgets that customize
   --  the behavior of some tool).

   procedure Initialize
     (Self                : not null access Dialog_Group_Widget_Record'Class;
      Parent_View         : not null access Dialog_View_Record'Class;
      Group_Name          : String                 := "";
      Allow_Multi_Columns : Boolean                := True;
      Selection           : Gtk_Selection_Mode     := Selection_None;
      Sorting_Function    : Gtk_Flow_Box_Sort_Func := null;
      Filtering_Function  : Gtk_Flow_Box_Filter_Func := null);
   --  Initialize a group widget, associating it with it's parent dialog view.
   --  Group_Name is used to set the group's title label.
   --
   --  If Allow_Multi_Columns is True, widgets within this group can be
   --  distributed into multiple columns when resizing. If False,  widgets are
   --  always distributed into one column.
   --  Selection defines the selection mode for the children, by default
   --  the children are not selectable.
   --  Sorting_Function is used to sort the children when adding a new one,
   --  it is also used with Force_Sort to force a new sort.
   --  Filtering_Function is used to filter the children. You can force the
   --  refiltering of the whole group with Force_Refilter.

   procedure On_Child_Selected
     (Self : not null access Dialog_Group_Widget_Record'Class;
      Call : Cb_GObject_Gtk_Flow_Box_Child_Void;
      Slot : not null access GObject_Record'Class);
   --  Used to set a callback that will be called each time the user clicks
   --  on a child of the given group widget.
   --  This will take effect only if the group widget allows selection.

   function Get_Number_Of_Children
     (Self : not null access Dialog_Group_Widget_Record'Class) return Natural;
   --  Return the number of children the given group widget

   procedure Set_Column_Spacing
     (Self    : not null access Dialog_Group_Widget_Record'Class;
      Spacing : Guint);
   --  Set the spacing between the dialog group's columns

   procedure Set_Row_Spacing
     (Self    : not null access Dialog_Group_Widget_Record'Class;
      Spacing : Guint);
   --  Set the spacing between the dialog group's rows

   function Create_Child
     (Self        : not null access Dialog_Group_Widget_Record'Class;
      Widget      : not null access Gtk_Widget_Record'Class;
      Button      : access Gtk_Button_Record'Class := null;
      Label       : String := "";
      Doc         : String := "";
      Child_Key   : String := "";
      Expand      : Boolean := True;
      Fill        : Boolean := True;
      Same_Height : Boolean := True) return Gtk_Widget;
   --  Create a new child in the group widget containing the given Widget,
   --  associating it with an optional Child_Key.
   --
   --  If a Child_Key is specified, it will allow you to interact with this
   --  child (see the 'Set_Child_Highlighted' subprogram).
   --
   --  The final child's layout depends on the optional parameters:
   --
   --    . If Label if non-empty, a label widget is added on the left side of
   --      the newly created child, before Widget.
   --
   --    . If Button is non-null, it is added on the right side of the newly
   --      created child, next to Widget
   --
   --    . If Doc is non-empty, a widget displaying it is added at the bottom
   --      of the newly created child, below Widget
   --
   --  The newly created child will be aligned with the children belonging to
   --  the same dialog view (if created via this function too).
   --
   --  The function returns the widget corresponding to the entire newly
   --  created child.
   --
   --  The Expand and Fill properties have the same role as in the
   --  Gtk.Box.Pack_Start procedure.
   --
   --  If Same_Height is True, the widget's height will be equal to the height
   --  of the widgets previously inserted in the group.

   function Create_Child
     (Self         : not null access Dialog_Group_Widget_Record'Class;
      Widget       : not null access Gtk_Widget_Record'Class;
      Button       : access Gtk_Button_Record'Class := null;
      Label_Widget : access Gtk_Widget_Record'Class;
      Doc          : String := "";
      Child_Key    : String := "";
      Expand       : Boolean := True;
      Fill         : Boolean := True;
      Same_Height  : Boolean := True) return Gtk_Widget;
   --  Same as above, but allowing to add a custom label widget
   --  (e.g : a check box).

   procedure Create_Child
     (Self        : not null access Dialog_Group_Widget_Record'Class;
      Widget      : not null access Gtk_Widget_Record'Class;
      Button      : access Gtk_Button_Record'Class := null;
      Label       : String := "";
      Doc         : String := "";
      Child_Key   : String := "";
      Expand      : Boolean := True;
      Fill        : Boolean := True;
      Same_Height : Boolean := True);
   procedure Create_Child
     (Self         : not null access Dialog_Group_Widget_Record'Class;
      Widget       : not null access Gtk_Widget_Record'Class;
      Button       : access Gtk_Button_Record'Class := null;
      Label_Widget : access Gtk_Widget_Record'Class;
      Doc          : String := "";
      Child_Key    : String := "";
      Expand       : Boolean := True;
      Fill         : Boolean := True;
      Same_Height  : Boolean := True);
   --  Same as above, but without returning the newly created child

   procedure Append_Child
     (Self        : not null access Dialog_Group_Widget_Record'Class;
      Widget      : not null access Gtk_Widget_Record'Class;
      Expand      : Boolean := True;
      Fill        : Boolean := True;
      Homogeneous : Boolean := False;
      Child_Key   : String := "");
   --  Append an already built widget to the group, associating it with an
   --  optional Child_Key.
   --
   --  Children created via this procedure will not be aligned with the
   --  children created with the 'Create_Child' subprograms.
   --
   --  The Expand, Fill and Homogenous properties have the same role as in the
   --  Gtk.Box.Pack_Start procedure.

   function Get_Selected_Children
     (Self : not null access Dialog_Group_Widget_Record'Class)
      return Gtk.Widget.Widget_List.Glist;
   --  Returns the list of selected flow_box_child

   procedure Force_Sort
     (Self : not null access Dialog_Group_Widget_Record'Class);
   --  Resort the Dialog_Group_Widget children

   procedure Force_Refilter
     (Self : not null access Dialog_Group_Widget_Record'Class);
   --  Refilter the Dialog_Group_Widget children

   ------------------
   -- CSS Stlizers --
   ------------------

   procedure Apply_Doc_Style (Label : not null access Gtk_Label_Record'Class);
   --  Apply the CSS style used for dialog group widgets' documentation to the
   --  given label.

private

   package Gtk_Flow_Box_Child_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Gtk_Flow_Box_Child,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   type Dialog_View_Record is new Gtk_Scrolled_Window_Record
   with record
      Main_Box           : Gtk_Vbox;
      --  The main container of the dialog view

      Label_Size_Group   : Gtk_Size_Group;
      Widget_Size_Group  : Gtk_Size_Group;
      Button_Size_Group  : Gtk_Size_Group;
      --  Used to align the children created via the 'Create_Child' subprograms

      Number_Of_Children : Natural := 0;
      --  The current number of children of the dialog view

      Children_Map       : Gtk_Flow_Box_Child_Maps.Map;
      --  Contains all the flow box children that have been associated with a
      --  key.
   end record;

   type Dialog_View_With_Button_Box_Record is new Dialog_View_Record
   with record
      Button_Box : Gtk_Box;
      --  The button box containing all the appended buttons
   end record;

   type Dialog_Group_Widget_Record is new Gtk_Frame_Record with record
      Parent_View          : Dialog_View;
      --  The parent dialog view of the group

      Flow_Box             : Gtk_Flow_Box;
      --  The main container of the group widget

      Number_Of_Children   : Natural := 0;
      --  The current number of children of the group widget

      Filter_Func          : Gtk_Flow_Box_Filter_Func;
      --  The filtering function

      Has_Children_Visible : Boolean := False;
      --  Used to hide the group widget itself if all its children are not
      --  visible anymore after some filtering.

      Is_Filter_Func_Set   : Boolean := False;
      --  Used to know if the filtering function has been already set
   end record;

end Dialog_Utils;
