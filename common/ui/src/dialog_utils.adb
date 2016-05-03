------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2016, AdaCore                     --
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
with Gtk.Label;          use Gtk.Label;
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

      Get_Style_Context (Self).Add_Class ("dialog-views-pages");

      Gtk_New (Self.Label_Size_Group);
      Gtk_New (Self.Widget_Size_Group);
      Gtk_New (Self.Button_Size_Group);

      Gtk_New_Vbox (Self.Page_Box);

      Self.Add (Self.Page_Box);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : not null access Dialog_View_With_Button_Box_Record'Class;
      Orientation : Gtk_Orientation)
   is
      Box : Gtk_Box;
      Sep : Gtk_Separator;
   begin
      Gtk.Scrolled_Window.Initialize (Self);
      Self.Set_Policy (Policy_Automatic, Policy_Automatic);

      Get_Style_Context (Self).Add_Class ("dialog-views-pages");

      Gtk_New (Self.Label_Size_Group);
      Gtk_New (Self.Widget_Size_Group);
      Gtk_New (Self.Button_Size_Group);
      Gtk_New (Self.Button_Box_Size_Group);

      if Orientation = Orientation_Vertical then
         Gtk_New_Hbox (Box);
         Gtk_New_Vseparator (Sep);
      else
         Gtk_New_Vbox (Box);
         Gtk_New_Hseparator (Sep);
      end if;

      Self.Add (Box);

      Gtk_New_Vbox (Self.Page_Box);
      Box.Pack_Start (Self.Page_Box, Expand => True, Fill => True);

      Box.Pack_Start (Sep, Expand => False);

      Gtk.Box.Gtk_New
        (Self.Button_Box,
         Orientation => Orientation,
         Spacing     => 5);
      Box.Pack_Start (Self.Button_Box, Expand => False);

      Get_Style_Context (Self.Button_Box).Add_Class
        ("dialog-views-button-boxes");
   end Initialize;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self   : not null access Dialog_View_Record'Class;
      Widget : not null access Gtk_Widget_Record'Class;
      Expand : Boolean := False;
      Fill   : Boolean := False)
   is
   begin
      --  Add a separator before the given widget if the dialog view already
      --  has children.
      if Has_Children (Self.Page_Box) then
         declare
            Sep : Gtk_Separator;
         begin
            Gtk_New_Hseparator (Sep);
            Self.Page_Box.Pack_Start (Sep, Expand => False);
         end;
      end if;

      Self.Page_Box.Pack_Start (Widget, Expand => Expand, Fill => Fill);
   end Append;

   -------------------------
   -- Remove_All_Children --
   -------------------------

   procedure Remove_All_Children
     (Self : not null access Dialog_View_Record'Class) is
   begin
      GUI_Utils.Remove_All_Children (Self.Page_Box);
      Self.Children_Map.Clear;
   end Remove_All_Children;

   -------------------------
   -- Set_Row_Highlighted --
   -------------------------

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
      Self.Button_Box_Size_Group.Add_Widget (Button);
      Self.Button_Box.Pack_Start (Button, Expand => False);
   end Append_Button;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : not null access Dialog_Group_Widget_Record'Class;
      Parent_View : not null access Dialog_View_Record'Class;
      Group_Name  : String := "")
   is
      VBox : Gtk_Box;
   begin
      Gtk.Frame.Initialize (Self, Group_Name);
      Get_Style_Context (Self).Add_Class ("dialog-views-groups");

      Gtk_New (Self.Flow_Box);
      Self.Flow_Box.Set_Orientation (Orientation_Horizontal);
      Self.Flow_Box.Set_Row_Spacing (0);
      Self.Flow_Box.Set_Max_Children_Per_Line (2);
      Self.Flow_Box.Set_Selection_Mode (Selection_None);
      Self.Flow_Box.Set_Homogeneous (False);

      Gtk_New_Vbox (VBox);
      VBox.Pack_Start (Self.Flow_Box, Expand => False);
      Self.Add (VBox);

      Self.Parent_View := Dialog_View (Parent_View);
      Self.Parent_View.Page_Box.Pack_Start (Self, Expand => False);
   end Initialize;

   ----------------
   -- Create_Row --
   ----------------

   function Create_Child
     (Self      : not null access Dialog_Group_Widget_Record'Class;
      Widget    : not null access Gtk_Widget_Record'Class;
      Button    : access Gtk_Button_Record'Class := null;
      Label     : String := "";
      Doc       : String := "";
      Child_Key : String := "") return Gtk_Widget
   is
      Child_Box    : Gtk_Box;
      Label_Widget : Gtk_Label;
      Padding      : constant Guint := (if Label = "" then 0 else 5);
   begin
      Gtk_New_Hbox (Child_Box, Homogeneous => False);

      if Label /= "" then
         Gtk_New (Label_Widget, Label);
         Label_Widget.Set_Alignment (0.0, 0.5);
         Self.Parent_View.Label_Size_Group.Add_Widget (Label_Widget);
         Child_Box.Pack_Start (Label_Widget, Expand => False);
      end if;

      Self.Parent_View.Widget_Size_Group.Add_Widget (Widget);
      Child_Box.Pack_Start (Widget, Expand => False, Padding => Padding);

      if Button /= null then
         Self.Parent_View.Button_Size_Group.Add_Widget (Button);
         Child_Box.Pack_Start (Button, Expand => False, Padding => Padding);
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

            Get_Style_Context (Doc_Widget).Add_Class
              ("dialog-views-doc-labels");
         end;
      end if;

      Self.Append_Child (Child_Box);

      --  Insert it in the dialog view children map if a key has been specified
      if Child_Key /= "" then
         Self.Parent_View.Children_Map.Insert
           (Child_Key, Child_Box.Get_Parent);
      end if;

      return Child_Box.Get_Parent;
   end Create_Child;

   ----------------
   -- Create_Row --
   ----------------

   procedure Create_Child
     (Self      : not null access Dialog_Group_Widget_Record'Class;
      Widget    : not null access Gtk_Widget_Record'Class;
      Button    : access Gtk_Button_Record'Class := null;
      Label     : String := "";
      Doc       : String := "";
      Child_Key : String := "")
   is
      Row : constant Gtk_Widget :=
              Create_Child
                (Self      => Self,
                 Widget    => Widget,
                 Button    => Button,
                 Label     => Label,
                 Doc       => Doc,
                 Child_Key => Child_Key);
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
      Child_Key : String := "") is
   begin
      Self.Flow_Box.Add (Widget);
      Get_Style_Context (Widget.Get_Parent).Add_Class
        ("dialog-views-groups-rows");

      --  Insert it in the dialog view children map if a key has been specified
      if Child_Key /= "" then
         Self.Parent_View.Children_Map.Insert
           (Child_Key, Widget.Get_Parent);
      end if;
   end Append_Child;

end Dialog_Utils;
