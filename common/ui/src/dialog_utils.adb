------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

with Glib;               use Glib;
with Gtk.Separator;      use Gtk.Separator;
with Gtk.Style_Context;  use Gtk.Style_Context;

with GUI_Utils;          use GUI_Utils;

package body Dialog_Utils is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : not null access Dialog_View_Record'Class) is
   begin
      Gtk.Scrolled_Window.Initialize (Self);
      Self.Set_Policy (Policy_Automatic, Policy_Automatic);

      Get_Style_Context (Self).Add_Class ("dialog-views");

      Gtk_New (Self.Label_Size_Group);
      Gtk_New (Self.Widget_Size_Group);
      Gtk_New (Self.Button_Size_Group);

      Gtk_New_Vbox (Self.Main_Box, Homogeneous => False);

      Self.Add (Self.Main_Box);
   end Initialize;

   ----------------------------
   -- Get_Number_Of_Children --
   ----------------------------

   function Get_Number_Of_Children
     (Self : not null access Dialog_View_Record'Class) return Natural
   is
      (Self.Number_Of_Children);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : not null access Dialog_View_With_Button_Box_Record'Class;
      Position : Gtk_Position_Type)
   is
      Box         : Gtk_Box;
      Sep         : Gtk_Separator;
      Orientation : constant Gtk_Orientation :=
                      (if Position in Pos_Left .. Pos_Right then
                          Orientation_Vertical
                       else
                          Orientation_Horizontal);
      At_Start    : constant Boolean :=
                      Position = Pos_Left or else Position = Pos_Top;
   begin
      Gtk.Scrolled_Window.Initialize (Self);
      Self.Set_Policy (Policy_Automatic, Policy_Automatic);

      Get_Style_Context (Self).Add_Class ("dialog-views");

      Gtk_New (Self.Label_Size_Group);
      Gtk_New (Self.Widget_Size_Group);
      Gtk_New (Self.Button_Size_Group);

      if Orientation = Orientation_Vertical then
         Gtk_New_Hbox (Box);
      else
         Gtk_New_Vbox (Box);
      end if;

      Self.Add (Box);

      --  Create the button box
      Gtk_New
        (Self.Button_Box,
         Orientation => Orientation);
      Self.Button_Box.Set_Layout (Buttonbox_Start);
      Get_Style_Context (Self.Button_Box).Add_Class
        ("dialog-views-button-boxes");

      if At_Start then
         Box.Pack_Start (Self.Button_Box, Expand => False);
      else
         Box.Pack_End (Self.Button_Box, Expand => False);
      end if;

      --  Add a separator
      Gtk_New (Sep, Orientation => Orientation);

      if At_Start then
         Box.Pack_Start (Sep, Expand => False);
      else
         Box.Pack_End (Sep, Expand => False);
      end if;

      --  Create the main box
      Gtk_New_Vbox (Self.Main_Box, Homogeneous => False);

      if At_Start then
         Box.Pack_Start (Self.Main_Box, Expand => True, Fill => True);
      else
         Box.Pack_End (Self.Main_Box, Expand => True, Fill => True);
      end if;
   end Initialize;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self          : not null access Dialog_View_Record'Class;
      Widget        : not null access Gtk_Widget_Record'Class;
      Expand        : Boolean := True;
      Fill          : Boolean := True;
      Add_Separator : Boolean := True)
   is
   begin
      --  Add a separator before the given widget if the dialog view already
      --  has children.
      if Add_Separator and then Has_Children (Self.Main_Box) then
         declare
            Sep : Gtk_Separator;
         begin
            Gtk_New_Hseparator (Sep);
            Self.Main_Box.Pack_Start (Sep, Expand => False);
         end;
      end if;

      Self.Main_Box.Pack_Start (Widget, Expand => Expand, Fill => Fill);

      --  Update the number of children
      Self.Number_Of_Children := Self.Number_Of_Children + 1;
   end Append;

   -------------------------
   -- Remove_All_Children --
   -------------------------

   procedure Remove_All_Children
     (Self : not null access Dialog_View_Record'Class) is
   begin
      GUI_Utils.Remove_All_Children (Self.Main_Box);
      Self.Children_Map.Clear;
      Self.Number_Of_Children := 0;
   end Remove_All_Children;

   -----------------------
   -- Set_Child_Visible --
   -----------------------

   procedure Set_Child_Visible
     (Self      : not null access Dialog_View_Record'Class;
      Child_Key : String;
      Visible   : Boolean)
   is
      Child : Gtk_Widget;
   begin
      --  Do nothing if the map does not contain any association for Row_Key
      if not Self.Children_Map.Contains (Child_Key) then
         return;
      end if;

      Child := Self.Children_Map (Child_Key);

      if Visible then
         --  Show all the row's children too
         Child.Set_No_Show_All (False);
         Child.Show_All;
      else
         Child.Hide;
      end if;
   end Set_Child_Visible;

   ---------------------------
   -- Set_Child_Highlighted --
   ---------------------------

   procedure Set_Child_Highlighted
     (Self      : not null access Dialog_View_Record'Class;
      Child_Key : String;
      Highlight : Boolean)
   is
      Child : Gtk_Widget;
   begin
      --  Do nothing if the map does not contain any association for Row_Key
      if not Self.Children_Map.Contains (Child_Key) then
         return;
      end if;

      Child := Self.Children_Map (Child_Key);

      if Highlight then
         Scroll_To_Child (Self, Child);
         Child.Set_State_Flags (Gtk_State_Flag_Selected, False);
      else
         Child.Set_State_Flags (Gtk_State_Flag_Normal, True);
      end if;
   end Set_Child_Highlighted;

   ----------------------------------
   -- Display_Information_On_Child --
   ----------------------------------

   procedure Display_Information_On_Child
     (Self      : not null access Dialog_View_Record'Class;
      Child_Key : String;
      Message   : String;
      Is_Error  : Boolean := False)
   is
      Child : Gtk_Widget;
   begin
      --  Do nothing if the map does not contain any association for Row_Key
      if not Self.Children_Map.Contains (Child_Key) then
         return;
      end if;

      Child := Self.Children_Map (Child_Key);

      Child.Set_Tooltip_Text (Message);

      if Is_Error then
         Get_Style_Context (Child).Add_Class ("display_error");
      else
         Get_Style_Context (Child).Remove_Class ("display_error");
      end if;
   end Display_Information_On_Child;

   ---------------------------------
   -- Remove_Information_On_Child --
   ---------------------------------

   procedure Remove_Information_On_Child
     (Self      : not null access Dialog_View_Record'Class;
      Child_Key : String)
   is
      Child : Gtk_Widget;
   begin
      --  Do nothing if the map does not contain any association for Row_Key
      if not Self.Children_Map.Contains (Child_Key) then
         return;
      end if;

      Child := Self.Children_Map (Child_Key);

      Child.Set_Has_Tooltip (False);
      Get_Style_Context (Child).Remove_Class ("display_error");
   end Remove_Information_On_Child;

   -------------------
   -- Append_Button --
   -------------------

   procedure Append_Button
     (Self   : not null access Dialog_View_With_Button_Box_Record'Class;
      Button : not null access Gtk_Button_Record'Class) is
   begin
      Self.Button_Box.Pack_Start (Button, Expand => False);
   end Append_Button;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                : not null access Dialog_Group_Widget_Record'Class;
      Parent_View         : not null access Dialog_View_Record'Class;
      Group_Name          : String                 := "";
      Allow_Multi_Columns : Boolean                := True;
      Selection           : Gtk_Selection_Mode     := Selection_None;
      Sorting_Function    : Gtk_Flow_Box_Sort_Func := null;
      Filtering_Function  : Gtk_Flow_Box_Filter_Func := null)
   is
      Max_Children_Per_Line : constant Guint := (if Allow_Multi_Columns then
                                                    2
                                                 else
                                                    1);
   begin
      Gtk.Frame.Initialize (Self, Group_Name);
      Get_Style_Context (Self).Add_Class ("dialog-views-groups");

      Gtk_New (Self.Flow_Box);
      Self.Flow_Box.Set_Orientation (Orientation_Horizontal);
      Self.Flow_Box.Set_Row_Spacing (0);
      Self.Flow_Box.Set_Max_Children_Per_Line (Max_Children_Per_Line);
      Self.Flow_Box.Set_Selection_Mode (Selection);
      Self.Flow_Box.Set_Homogeneous (False);

      if Sorting_Function /= null then
         Self.Flow_Box.Set_Sort_Func (Sorting_Function);
      end if;

      if Filtering_Function /= null then
         Self.Flow_Box.Set_Filter_Func (Filtering_Function);
      end if;

      Self.Add (Self.Flow_Box);

      Self.Parent_View := Dialog_View (Parent_View);
      Self.Parent_View.Append
        (Widget        => Self,
         Expand        => False,
         Add_Separator => False);
   end Initialize;

   -----------------------
   -- On_Child_Selected --
   -----------------------

   procedure On_Child_Selected
     (Self : not null access Dialog_Group_Widget_Record'Class;
      Call : Cb_GObject_Gtk_Flow_Box_Child_Void;
      Slot : not null access GObject_Record'Class) is
   begin
      Self.Flow_Box.On_Child_Activated
        (Call  => Call,
         Slot  => Slot);
   end On_Child_Selected;

   ----------------------------
   -- Get_Number_Of_Children --
   ----------------------------

   function Get_Number_Of_Children
     (Self : not null access Dialog_Group_Widget_Record'Class) return Natural
   is
      (Self.Number_Of_Children);

   ------------------
   -- Create_Child --
   ------------------

   function Create_Child
     (Self      : not null access Dialog_Group_Widget_Record'Class;
      Widget    : not null access Gtk_Widget_Record'Class;
      Button    : access Gtk_Button_Record'Class := null;
      Label     : String := "";
      Doc       : String := "";
      Child_Key : String := "";
      Expand    : Boolean := True;
      Fill      : Boolean := True) return Gtk_Widget
   is
      Label_Widget : Gtk_Label;
   begin
      --  Create the label widget from the given Label string, if non-empty
      if Label /= "" then
         Gtk_New (Label_Widget, Label);
         Label_Widget.Set_Alignment (0.0, 0.5);
      end if;

      --  Finally, create the child
      return Self.Create_Child
        (Widget       => Widget,
         Button       => Button,
         Label_Widget => Label_Widget,
         Doc          => Doc,
         Child_Key    => Child_Key,
         Expand       => Expand,
         Fill         => Fill);
   end Create_Child;

   ------------------
   -- Create_Child --
   ------------------

   function Create_Child
     (Self         : not null access Dialog_Group_Widget_Record'Class;
      Widget       : not null access Gtk_Widget_Record'Class;
      Button       : access Gtk_Button_Record'Class := null;
      Label_Widget : access Gtk_Widget_Record'Class;
      Doc          : String := "";
      Child_Key    : String := "";
      Expand       : Boolean := True;
      Fill         : Boolean := True) return Gtk_Widget
   is
      Child_Box : Gtk_Box;
      Spacing   : constant Gint := 5;
   begin
      Gtk_New_Hbox (Child_Box, Homogeneous => False);
      Child_Box.Set_Spacing (Spacing);

      if Label_Widget /= null then
         Self.Parent_View.Label_Size_Group.Add_Widget (Label_Widget);
         Child_Box.Pack_Start (Label_Widget, Expand => False);
      end if;

      Self.Parent_View.Widget_Size_Group.Add_Widget (Widget);
      Child_Box.Pack_Start
        (Widget,
         Expand  => Expand,
         Fill    => Fill);

      if Button /= null then
         Self.Parent_View.Button_Size_Group.Add_Widget (Button);
         Child_Box.Pack_Start (Button, Expand => False);
      end if;

      if Doc /= "" then
         declare
            Vbox        : Gtk_Box;
            Doc_Widget  : Gtk_Label;
         begin
            Gtk_New_Vbox (Vbox);
            Vbox.Pack_Start (Child_Box, Expand => False);
            Child_Box := Vbox;

            Gtk_New (Doc_Widget, Doc);
            Child_Box.Pack_Start (Doc_Widget, Expand => False);
            Doc_Widget.Set_Line_Wrap (True);
            Doc_Widget.Set_Alignment (0.0, 0.5);
            Doc_Widget.Set_Justify (Justify_Fill);

            --  Done to limit the size requested by the label, which is
            --  normally computed from the size required to display the label
            --  text without wrapping it.
            Doc_Widget.Set_Max_Width_Chars (50);

            Apply_Doc_Style (Doc_Widget);
         end;
      end if;

      Self.Append_Child (Child_Box, Expand => False);

      --  Insert it in the dialog view children map if a key has been specified
      if Child_Key /= "" then
         Self.Parent_View.Children_Map.Insert
           (Child_Key, Child_Box.Get_Parent);
      end if;

      return Child_Box.Get_Parent;
   end Create_Child;

   ------------------
   -- Create_Child --
   ------------------

   procedure Create_Child
     (Self      : not null access Dialog_Group_Widget_Record'Class;
      Widget    : not null access Gtk_Widget_Record'Class;
      Button    : access Gtk_Button_Record'Class := null;
      Label     : String := "";
      Doc       : String := "";
      Child_Key : String := "";
      Expand    : Boolean := True;
      Fill      : Boolean := True)
   is
      Row : constant Gtk_Widget :=
              Create_Child
                (Self      => Self,
                 Widget    => Widget,
                 Button    => Button,
                 Label     => Label,
                 Doc       => Doc,
                 Child_Key => Child_Key,
                 Expand    => Expand,
                 Fill      => Fill);
      pragma Unreferenced (Row);
   begin
      null;
   end Create_Child;

   ------------------
   -- Create_Child --
   ------------------

   procedure Create_Child
     (Self         : not null access Dialog_Group_Widget_Record'Class;
      Widget       : not null access Gtk_Widget_Record'Class;
      Button       : access Gtk_Button_Record'Class := null;
      Label_Widget : access Gtk_Widget_Record'Class;
      Doc          : String := "";
      Child_Key    : String := "";
      Expand       : Boolean := True;
      Fill         : Boolean := True)
   is
      Row : constant Gtk_Widget :=
              Create_Child
                (Self         => Self,
                 Widget       => Widget,
                 Button       => Button,
                 Label_Widget => Label_Widget,
                 Doc          => Doc,
                 Child_Key    => Child_Key,
                 Expand       => Expand,
                 Fill         => Fill);
      pragma Unreferenced (Row);
   begin
      null;
   end Create_Child;

   ------------------
   -- Append_Child --
   ------------------

   procedure Append_Child
     (Self      : not null access Dialog_Group_Widget_Record'Class;
      Widget    : not null access Gtk_Widget_Record'Class;
      Expand    : Boolean := True;
      Fill      : Boolean := True;
      Child_Key : String := "")
   is
      Valign : constant Gtk_Align := (if Expand and then Fill then
                                         Align_Fill
                                      else
                                         Align_Start);
   begin
      --  Set the Vexpand and Valign properties on the widget itself, depending
      --  on the Expand and Fill parameters.
      Widget.Set_Vexpand (Expand);
      Widget.Set_Valign (Valign);

      Self.Flow_Box.Add (Widget);
      Get_Style_Context (Widget.Get_Parent).Add_Class
        ("dialog-views-groups-rows");

      --  Insert it in the dialog view children map if a key has been specified
      if Child_Key /= "" then
         Self.Parent_View.Children_Map.Insert
           (Child_Key, Widget.Get_Parent);
      end if;

      --  Update the number of children
      Self.Number_Of_Children := Self.Number_Of_Children + 1;
   end Append_Child;

   ---------------------------
   -- Get_Selected_Children --
   ---------------------------

   function Get_Selected_Children
     (Self : not null access Dialog_Group_Widget_Record'Class)
      return Gtk.Widget.Widget_List.Glist
   is
   begin
      return Self.Flow_Box.Get_Selected_Children;
   end Get_Selected_Children;

   ------------
   -- Force_Sort --
   ------------

   procedure Force_Sort
     (Self : not null access Dialog_Group_Widget_Record'Class)
   is
   begin
      Self.Flow_Box.Invalidate_Sort;
   end Force_Sort;

   --------------------
   -- Force_Refilter --
   --------------------

   procedure Force_Refilter
     (Self : not null access Dialog_Group_Widget_Record'Class)
   is
   begin
      Self.Flow_Box.Invalidate_Filter;
   end Force_Refilter;

   ---------------------
   -- Apply_Doc_Style --
   ---------------------

   procedure Apply_Doc_Style (Label : not null access Gtk_Label_Record'Class)
   is
   begin
      Get_Style_Context (Label).Add_Class ("dialog-views-doc-labels");
      Label.Set_Line_Wrap (True);
      Label.Set_Alignment (0.0, 0.5);
      Label.Set_Justify (Justify_Fill);
   end Apply_Doc_Style;

end Dialog_Utils;
