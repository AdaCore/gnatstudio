------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2013, AdaCore                     --
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

with Ada.Calendar;               use Ada.Calendar;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;               use GNAT.Strings;

with Commands;                   use Commands;
with Gdk.Event;                  use Gdk.Event;
with Gdk.Device;                 use Gdk.Device;
with Gdk.Device_Manager;         use Gdk.Device_Manager;
with Gdk.RGBA;                   use Gdk.RGBA;
with Gdk.Types;                  use Gdk.Types;
with Gdk.Types.Keysyms;          use Gdk.Types.Keysyms;
with Gdk.Window;                 use Gdk.Window;
with Glib;                       use Glib;
with Glib.Main;                  use Glib.Main;
with Glib.Object;                use Glib.Object;
with Glib.Properties;            use Glib.Properties;
with Glib.Values;                use Glib.Values;
with Gtk.Alignment;              use Gtk.Alignment;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Button;                 use Gtk.Button;
with Gtk.Cell_Renderer;          use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;           use Gtk.Check_Button;
with Gtk.Combo_Box_Text;         use Gtk.Combo_Box_Text;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Editable;               use Gtk.Editable;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Image;                  use Gtk.Image;
with Gtk.Label;                  use Gtk.Label;
with Gtk.List_Store;             use Gtk.List_Store;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Separator;              use Gtk.Separator;
with Gtk.Stock;                  use Gtk.Stock;
with Gtk.Style_Context;          use Gtk.Style_Context;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter;      use Gtk.Tree_Model_Filter;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtkada.Search_Entry;        use Gtkada.Search_Entry;
with Gtkada.Style;               use Gtkada.Style;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Search;          use GPS.Kernel.Search;
with GPS.Kernel.Task_Manager;    use GPS.Kernel.Task_Manager;
with GPS.Search;                 use GPS.Search;
with GUI_Utils;                  use GUI_Utils;
with Histories;                  use Histories;
with Pango.Layout;               use Pango.Layout;
with System;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtkada.Entry_Completion is
   Me : constant Trace_Handle := Create ("SEARCH");

   Max_Idle_Duration : constant Duration := 0.05;
   --  Maximum time spent in the idle callback to insert the possible
   --  completions.

   Do_Grabs : constant Boolean := False;
   --  Whether to attempt grabbing the pointer

   Preview_Right_Margin : constant := 10;
   --  between preview and completion popups

   Preview_Width       : constant := 500;
   Preview_Height      : constant := 400;

   Provider_Label_Width : constant := 100;
   Result_Width : constant := 300;
   --  Maximum width of the popup window

   procedure On_Entry_Destroy (Self : access Gtk_Widget_Record'Class);
   --  Callback when the widget is destroyed.

   procedure On_Entry_Changed (Self  : access GObject_Record'Class);
   --  Handles changes in the entry field.

   function On_Key_Press
     (Ent   : access GObject_Record'Class;
      Event : Gdk_Event_Key) return Boolean;
   --  Called when the user pressed a key in the completion window

   function On_Idle (Self : Gtkada_Entry) return Boolean;
   --  Called on idle to complete the completions

   function On_Preview_Idle (Self : Gtkada_Entry) return Boolean;
   --  Called on idle to display the preview of the current selection

   package Completion_Sources is new Glib.Main.Generic_Sources (Gtkada_Entry);

   procedure Clear (Self : access Gtkada_Entry_Record'Class);
   --  Clear the list of completions, and free the memory

   type Command_To_Locations is new Commands.Root_Command with record
      Completion : Gtkada_Entry;
      Provider   : Search_Provider_Access;
      Pattern    : Search_Pattern_Access;
   end record;
   type Command_To_Locations_Access is access all Command_To_Locations'Class;
   overriding function Name
     (Self : access Command_To_Locations) return String is ("search");
   overriding function Execute
     (Self : access Command_To_Locations) return Command_Return_Type;
   overriding procedure Free (Self : in out Command_To_Locations);

   procedure Insert_Proposal
     (Self : not null access Gtkada_Entry_Record'Class;
      Result : GPS.Search.Search_Result_Access);
   --  Create a new completion proposal showing result

   procedure On_Settings_Changed (Self : access GObject_Record'Class);
   --  One of the settings has changed

   function On_Button_Event
      (Ent   : access GObject_Record'Class;
       Event : Gdk_Event_Button) return Boolean;
   --  Called when a proposal is selected

   procedure On_Entry_Activate (Self : access GObject_Record'Class);
   --  Called when <enter> is pressed in the entry

   function On_Focus_Out
      (Self  : access GObject_Record'Class;
       Event : Gdk_Event_Focus) return Boolean;
   --  Focus leaves the entry, we should close the popup

   procedure Activate_Proposal
      (Self : not null access Gtkada_Entry_Record'Class;
       Force : Boolean);
   --  Activate the proposal that current has the focus. If none and Force
   --  is True, activates the first proposal in the list.

   procedure Show_Preview (Self : access Gtkada_Entry_Record'Class);
   --  Show the preview pane if there is a current selection

   function Need_Preview
     (Self : access Gtkada_Entry_Record'Class) return Boolean;
   --  Whether the preview should be displayed.

   function Auto_Resize_Popup
     (Self : access Gtkada_Entry_Record'Class) return Boolean;
   --  Whether the popup window should be automatically resized (True) or
   --  occupy the whole height.

   procedure Resize_Popup
      (Self : not null access Gtkada_Entry_Record'Class;
       Height_Only : Boolean);
   --  Resize the popup window depending on its contents

   procedure Get_Iter_Next
      (Tree : Gtk_Tree_Model;
       Iter : in out Gtk_Tree_Iter);
   procedure Get_Iter_Prev
      (Tree : Gtk_Tree_Model;
       Iter : in out Gtk_Tree_Iter);
   --  Returns the next result proposal. Pass Null_Iter to get the
   --  first or last item in the tree

   function Get_Last_Child
      (Tree : Gtk_Tree_Model;
       Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter;
   --  Return the last child of Iter

   procedure Model_Modify_Func
      (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
       Value  : in out Glib.Values.GValue;
       Column : Gint);
   --  This is a filter function applied to the list of completions, and is
   --  used to avoid duplicating the name of the provider on each row.

   type Provider_Column_Role is
     (Role_Provider, Role_To_Locations, Role_Unknown);
   --  The role of a specific line in the providers column

   function Get_Provider_Column_Role
      (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter   : Gtk.Tree_Model.Gtk_Tree_Iter)
       return Provider_Column_Role;
   --  The role for the provider column on the given row.
   --  Model and Iter apply to the filter model.

   procedure Show_Settings (Self : access GObject_Record'Class);
   --  Display the settings dialog

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Search_Result_Access);

   type Comp_Filter_Model_Record is new Gtk_Tree_Model_Filter_Record
     with record
       View : Gtkada_Entry;
     end record;
   type Comp_Filter_Model is access all Comp_Filter_Model_Record'Class;
   --  A filter model that has a pointer to the completion entry.

   Column_Label    : constant := 0;
   Column_Score    : constant := 1;
   Column_Data     : constant := 2;
   Column_Provider : constant := 3;

   Completion_Class_Record : Glib.Object.Ada_GObject_Class :=
      Glib.Object.Uninitialized_Class;

   Signals : constant chars_ptr_array :=
      (1 => New_String (String (Signal_Activate)),
       2 => New_String (String (Signal_Escape)));

   Col_Types : constant Glib.GType_Array :=
      (Column_Label    => GType_String,
       Column_Score    => GType_Int,
       Column_Data     => GType_Pointer,
       Column_Provider => GType_String);

   --------------------
   -- Get_Last_Child --
   --------------------

   function Get_Last_Child
      (Tree : Gtk_Tree_Model;
       Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      N : constant Gint := N_Children (Tree, Iter);
   begin
      if N > 0 then
         return Nth_Child (Tree, Iter, N - 1);
      else
         return Null_Iter;
      end if;
   end Get_Last_Child;

   -------------------
   -- Get_Iter_Next --
   -------------------

   procedure Get_Iter_Next
      (Tree : Gtk_Tree_Model;
       Iter : in out Gtk_Tree_Iter) is
   begin
      if Iter /= Null_Iter then
         Next (Tree, Iter);
         if Iter = Null_Iter then
            Iter := Get_Iter_First (Tree);
         end if;
      else
         Iter := Get_Iter_First (Tree);
      end if;
   end Get_Iter_Next;

   -------------------
   -- Get_Iter_Prev --
   -------------------

   procedure Get_Iter_Prev
      (Tree : Gtk_Tree_Model;
       Iter : in out Gtk_Tree_Iter) is
   begin
      if Iter /= Null_Iter then
         Previous (Tree, Iter);
         if Iter = Null_Iter then
            Iter := Get_Last_Child (Tree, Null_Iter);
         end if;
      else
         Iter := Get_Last_Child (Tree, Null_Iter);
      end if;
   end Get_Iter_Prev;

   -----------------------
   -- On_Entry_Activate --
   -----------------------

   procedure On_Entry_Activate (Self : access GObject_Record'Class) is
   begin
      Activate_Proposal (Gtkada_Entry (Self), Force => True);
   end On_Entry_Activate;

   ------------------------------
   -- Get_Provider_Column_Role --
   ------------------------------

   function Get_Provider_Column_Role
      (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter   : Gtk.Tree_Model.Gtk_Tree_Iter)
     return Provider_Column_Role
   is
      Filter : constant Gtk_Tree_Model_Filter := -Model;
      Child  : constant Gtk_Tree_Model := Filter.Get_Model;
      Child_It : Gtk_Tree_Iter;
      Prev, Prev2   : Gtk_Tree_Iter;
      Res : Search_Result_Access;
   begin
      Filter.Convert_Iter_To_Child_Iter (Child_It, Filter_Iter => Iter);

      Prev := Child_It;
      Previous (Child, Prev);
      Res := Convert (Get_Address (Child, Child_It, Column_Data));

      if Prev = Null_Iter
        or else Res.Provider /=
          Convert (Get_Address (Child, Prev, Column_Data)).Provider
      then
         return Role_Provider;

      elsif Res.Can_Display_In_Locations then
         Prev2 := Prev;
         Previous (Child, Prev2);
         if Prev2 = Null_Iter
           or else Res.Provider /=
             Convert (Get_Address (Child, Prev2, Column_Data)).Provider
         then
            return Role_To_Locations;
         end if;
      end if;

      return Role_Unknown;
   end Get_Provider_Column_Role;

   -----------------------
   -- Model_Modify_Func --
   -----------------------

   procedure Model_Modify_Func
      (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
       Value  : in out Glib.Values.GValue;
       Column : Gint)
   is
      Filter : constant Comp_Filter_Model :=
        Comp_Filter_Model (Gtk_Tree_Model_Filter'(-Model));
      Child  : constant Gtk_Tree_Model := Filter.Get_Model;
      Child_It : Gtk_Tree_Iter;
      Res : Search_Result_Access;
   begin
      Filter.Convert_Iter_To_Child_Iter (Child_It, Filter_Iter => Iter);

      if Column = Column_Provider then
         case Get_Provider_Column_Role (Model, Iter) is
            when Role_Provider =>
               Res := Convert (Get_Address (Child, Child_It, Column_Data));
               Set_String
                 (Value, Res.Provider.Display_Name & " ("
                  & Image (Res.Provider.Count, Min_Width => 0) & ")");
            when Role_To_Locations =>
               Set_String (Value,
                           --  encoding for U+21D2 (right arrow)
                           "<span foreground='"
                           & To_Hex (Filter.View.Color_To_Locations)
                           & "'><small>"
                           & Character'Val (16#E2#)
                           & Character'Val (16#87#)
                           & Character'Val (16#92#)
                           & " Locations</small></span>");
            when Role_Unknown =>
               null;
         end case;

      elsif Column = Column_Label then
         Set_String (Value, Get_String (Child, Child_It, Column));

      elsif Column = Column_Score then
         Set_Int (Value, Get_Int (Child, Child_It, Column));

      elsif Column = Column_Data then
         Set_Address (Value, Get_Address (Child, Child_It, Column));

      else
         raise Program_Error with "Unexpected column";
      end if;
   end Model_Modify_Func;

   --------------
   -- Get_Type --
   --------------

   function Get_Type return Glib.GType is
   begin
      Glib.Object.Initialize_Class_Record
         (Ancestor     => Gtk.Box.Get_Vbox_Type,
          Signals      => Signals,
          Class_Record => Completion_Class_Record,
          Type_Name    => "GtkAdaEntryCompletion");
      return Completion_Class_Record.The_Type;
   end Get_Type;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self             : out Gtkada_Entry;
      Kernel           : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Completion       : not null access GPS.Search.Search_Provider'Class;
      Name             : Histories.History_Key;
      Case_Sensitive   : Boolean := False;
      Completion_In_Popup : Boolean := True) is
   begin
      Self := new Gtkada_Entry_Record;
      Initialize
         (Self, Kernel, Completion, Name, Case_Sensitive,
          Completion_In_Popup);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self             : not null access Gtkada_Entry_Record'Class;
      Kernel           : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Completion       : not null access GPS.Search.Search_Provider'Class;
      Name             : Histories.History_Key;
      Case_Sensitive      : Boolean := False;
      Completion_In_Popup : Boolean := True)
   is
      Scrolled : Gtk_Scrolled_Window;
      Box  : Gtk_Box;
      Col  : Gint;
      Sep  : Gtk_Separator;
      Render : Gtk_Cell_Renderer_Text;
      Dummy  : Boolean;
      Color  : Gdk_RGBA;
      Filter : Comp_Filter_Model;
      Button : Gtk_Button;
      Image  : Gtk_Image;
      Frame  : Gtk_Frame;
      Popup  : Gtk_Window;
      pragma Unreferenced (Col, Dummy);

   begin
      G_New (Self, Get_Type);
      Gtk.Box.Initialize_Vbox (Self, Homogeneous => False);

      Self.Completion := GPS.Search.Search_Provider_Access (Completion);
      Self.Default_Completion := Self.Completion;
      Self.Kernel := Kernel_Handle (Kernel);
      Self.Name := new History_Key'(Name);

      Gtk_New (Self.GEntry);
      Self.Pack_Start (Self.GEntry, Expand => False, Fill => False);
      Self.GEntry.Set_Activates_Default (False);
      Self.GEntry.On_Activate (On_Entry_Activate'Access, Self);
      Self.GEntry.Set_Placeholder_Text ("search");
      Self.GEntry.Set_Tooltip_Markup (Completion.Documentation);
      Self.GEntry.Set_Name (String (Name));

      if Completion_In_Popup then
         Gtk_New (Self.Popup, Window_Popup);
         Self.Popup.Set_Name ("completion-list");
         Self.Popup.Set_Type_Hint (Window_Type_Hint_Combo);
         Self.Popup.Set_Resizable (False);
         Self.Popup.Set_Skip_Taskbar_Hint (True);
         Self.Popup.Set_Skip_Pager_Hint (True);
         Get_Style_Context (Self.Popup).Add_Class ("completion");

         Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 0);
         Gtk_New (Frame);
         Self.Popup.Add (Frame);
         Frame.Add (Box);

         Gtk_New (Popup, Window_Popup);
         Self.Notes_Popup := Gtk_Widget (Popup);
         Popup.Set_Name ("completion-preview");
         Popup.Set_Type_Hint (Window_Type_Hint_Combo);
         Popup.Set_Resizable (False);
         Popup.Set_Skip_Taskbar_Hint (True);
         Popup.Set_Skip_Pager_Hint (True);
         Get_Style_Context (Popup).Add_Class ("completion");

      else
         Box := Gtk_Box (Self);
      end if;

      Gtk_New_Hbox (Self.Completion_Box, Homogeneous => False);
      Box.Pack_Start (Self.Completion_Box, Expand => True, Fill => True);

      Gtk_New (Frame);
      Gtk_New (Self.Notes_Scroll);
      Self.Notes_Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Frame.Add (Self.Notes_Scroll);

      if Completion_In_Popup then
         Popup.Add (Frame);
      else
         Self.Notes_Popup := Gtk_Widget (Frame);
         Frame.Set_Shadow_Type (Shadow_None);
      end if;

      --  Scrolled window for the possible completions

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Scrolled.Set_Shadow_Type (Shadow_None);
      Self.Completion_Box.Pack_Start (Scrolled, Expand => True, Fill => True);

      Gtk_New (Self.Completions, Col_Types);
      Filter := new Comp_Filter_Model_Record;
      Filter.View := Gtkada_Entry (Self);
      Initialize (Filter, +Self.Completions);
      Unref (Self.Completions);
      Filter.Set_Modify_Func (Col_Types, Model_Modify_Func'Access);

      Gtk_New (Self.View, Filter);
      Unref (Filter);

      Self.View.Set_Headers_Visible (False);
      Self.View.Get_Selection.Set_Mode (Selection_Single);
      Self.View.Set_Rules_Hint (True);
      Self.View.Set_Expander_Column (null);
      Self.View.Set_Show_Expanders (False);
      Scrolled.Add (Self.View);

      Self.Completions.Set_Sort_Column_Id
         (Sort_Column_Id => Column_Score,
          Order          => Sort_Descending);

      Gtk_New (Self.Column_Provider);
      Col := Self.View.Append_Column (Self.Column_Provider);
      Gtk_New (Render);
      Self.Column_Provider.Pack_Start (Render, False);
      Self.Column_Provider.Add_Attribute
        (Render, "markup", Column_Provider);

      --  ??? Should onnect to preferences_changed for the color
      Get_Style_Context (Self.View).Get_Background_Color
        (Gtk_State_Flag_Normal, Color);
      Set_Property
        (Render, Gtk.Cell_Renderer_Text.Foreground_Rgba_Property,
         Shade_Or_Lighten (Color, 0.3));
      Set_Property
        (Render, Gtk.Cell_Renderer_Text.Background_Rgba_Property,
         Shade_Or_Lighten (Color, 0.05));
      Set_Property (Render, Gtk.Cell_Renderer.Xalign_Property, 1.0);
      Set_Property (Render, Gtk.Cell_Renderer.Yalign_Property, 0.0);

      Self.Color_To_Locations := Shade_Or_Lighten (Color, 0.3);

      Gtk_New (Self.Column_Match);
      Self.Column_Match.Set_Sort_Column_Id (Column_Score);
      Self.Column_Match.Set_Sort_Order (Sort_Descending);
      Self.Column_Match.Clicked;
      Col := Self.View.Append_Column (Self.Column_Match);
      Gtk_New (Render);
      Self.Column_Match.Pack_Start (Render, False);
      Self.Column_Match.Add_Attribute (Render, "markup", Column_Label);

      --  Debug: show score
      --  Code is left here for convenience

      --  Gtk_New (C);
      --  Col := Self.View.Append_Column (C);
      --  Gtk_New (Render);
      --  C.Pack_Start (Render, False);
      --  C.Add_Attribute (Render, "text", Column_Score);

      if not Completion_In_Popup then
         Self.Completion_Box.Pack_Start
            (Self.Notes_Popup, Expand => True, Fill => True);
      end if;

      --  The settings panel

      Gtk_New_Hseparator (Sep);
      Box.Pack_Start (Sep, Expand => False);

      Gtk_New_Hbox (Self.Settings, Homogeneous => False);
      Box.Pack_Start (Self.Settings, Expand => False);

      Gtk_New (Self.Settings_Case_Sensitive, -"Case Sensitive");
      Self.Settings_Case_Sensitive.Set_Name ("global-search-case-sensitive");
      Associate (Get_History (Kernel).all,
                 Name & "-case_sensitive",
                 Self.Settings_Case_Sensitive,
                 Default => Case_Sensitive);
      Self.Settings.Pack_Start (Self.Settings_Case_Sensitive, Expand => False);

      Gtk_New (Self.Settings_Whole_Word, -"Whole Word");
      Self.Settings_Whole_Word.Set_Name ("global-search-whole-word");
      Associate (Get_History (Kernel).all,
                 Name & "-whole_word",
                 Self.Settings_Whole_Word,
                 Default => False);
      Self.Settings.Pack_Start (Self.Settings_Whole_Word, Expand => False);

      Gtk_New (Self.Settings_Kind);
      Self.Settings_Kind.Set_Name ("global-search-kind");
      Self.Settings_Kind.Append
         (Search_Kind'Image (Full_Text), -"Substrings match");
      Self.Settings_Kind.Append
         (Search_Kind'Image (Fuzzy), -"Fuzzy match");
      Self.Settings_Kind.Append
         (Search_Kind'Image (Regexp), -"Regular expression");
      Self.Settings.Pack_Start (Self.Settings_Kind, Expand => False);

      Create_New_Key_If_Necessary
         (Get_History (Kernel).all, Name & "-kind", Strings);
      Set_Max_Length (Get_History (Kernel).all, 1, Name & "-kind");
      Dummy := Self.Settings_Kind.Set_Active_Id
         (Most_Recent (Get_History (Kernel), Name & "-kind",
                       Search_Kind'Image (Fuzzy)));

      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, "global-search-auto-resize",
         Default_Value => True);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, Name & "-preview",
         Default_Value => True);

      Gtk_New (Image, Stock_Id => Stock_Preferences, Size => Icon_Size_Menu);
      Gtk_New (Button);
      Button.Add (Image);
      Self.Settings.Pack_Start (Button, Expand => False);
      Button.On_Clicked (Show_Settings'Access, Self);

      Self.Settings_Case_Sensitive.On_Toggled
         (On_Settings_Changed'Access, Self);
      Self.Settings_Whole_Word.On_Toggled (On_Settings_Changed'Access, Self);
      Self.Settings_Kind.On_Changed (On_Settings_Changed'Access, Self);

      Self.On_Destroy (On_Entry_Destroy'Access);
      Self.View.On_Button_Press_Event (On_Button_Event'Access, Self);
      Self.GEntry.On_Key_Press_Event (On_Key_Press'Access, Self);
      Self.GEntry.On_Focus_Out_Event (On_Focus_Out'Access, Self);

      Grab_Toplevel_Focus (Get_MDI (Kernel), Self.GEntry);

      --  Connect after setting the default entry, so that we do not
      --  pop up the completion window immediately.
      Gtk.Editable.On_Changed
         (+Gtk_Entry (Self.GEntry), On_Entry_Changed'Access, Self);
   end Initialize;

   ------------------
   -- On_Focus_Out --
   ------------------

   function On_Focus_Out
      (Self  : access GObject_Record'Class;
       Event : Gdk_Event_Focus) return Boolean
   is
      S : constant Gtkada_Entry := Gtkada_Entry (Self);
      pragma Unreferenced (Event);
   begin
      Popdown (S);
      return False;
   end On_Focus_Out;

   -----------------------
   -- Activate_Proposal --
   -----------------------

   procedure Activate_Proposal
      (Self : not null access Gtkada_Entry_Record'Class;
       Force : Boolean)
   is
      M    : Gtk_Tree_Model;
      Iter : Gtk_Tree_Iter;
      W    : Gtk_Widget;
      Result : Search_Result_Access;
   begin
      Self.View.Get_Selection.Get_Selected (M, Iter);

      if Iter /= Null_Iter then
         Result := Convert (Get_Address (+M, Iter, Column_Data));
      else
         if Force then
            Iter := Null_Iter;
            Get_Iter_Next (M, Iter);
            if Iter /= Null_Iter then
               Result := Convert (Get_Address (+M, Iter, Column_Data));
            end if;
         end if;
      end if;

      if Result /= null then
         Popdown (Self);

         Widget_Callback.Emit_By_Name (Self, Signal_Activate);

         Result.Execute (Give_Focus => True);

         Result.Provider.On_Result_Executed (Result);

         --  Keep the interface leaner
         Self.GEntry.Set_Text ("");

         --  ??? Temporary workaround to close the parent dialog if any.
         --  The clean approach is to have a signal that a completion has
         --  been selected, and then capture this in src_editor_module.adb
         --  to close the dialog

         W := Self.Get_Toplevel;
         if W /= null and then W.all in Gtk_Dialog_Record'Class then
            Gtk_Dialog (W).Response (Gtk_Response_None);
         end if;
      end if;
   end Activate_Proposal;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
      (Self : not null access Gtkada_Entry_Record)
      return GPS.Kernel.Kernel_Handle is
   begin
      return Self.Kernel;
   end Get_Kernel;

   ---------------------
   -- On_Button_Event --
   ---------------------

   function On_Button_Event
      (Ent   : access GObject_Record'Class;
       Event : Gdk_Event_Button) return Boolean
   is
      Self : constant Gtkada_Entry := Gtkada_Entry (Ent);
      Path : Gtk_Tree_Path;
      Column : Gtk_Tree_View_Column;
      Cell_X, Cell_Y : Gint;
      Found : Boolean;
      M    : Gtk_Tree_Model;
      Iter : Gtk_Tree_Iter;
      Result : Search_Result_Access;
      Command  : Command_To_Locations_Access;
   begin
      if Event.Button = 1 then
         Self.View.Get_Path_At_Pos
            (X => Gint (Event.X),
             Y => Gint (Event.Y),
             Path => Path,
             Column => Column,
             Cell_X => Cell_X,
             Cell_Y => Cell_Y,
             Row_Found => Found);
         M := Self.View.Get_Model;  --  the filter model

         if Found then
            Iter := Get_Iter (M, Path);

            if Column = Self.Column_Match then
               Self.View.Get_Selection.Select_Iter (Iter);
               Activate_Proposal (Gtkada_Entry (Ent), Force => False);

            elsif Column = Self.Column_Provider then
               Result := Convert (Get_Address (+M, Iter, Column_Data));

               case Get_Provider_Column_Role (M, Iter) is
                  when Role_Provider =>
                     if Result.Provider = Self.Completion then
                        --  Back to all providers
                        Self.Set_Completion (Self.Default_Completion);

                     else
                        --  Restrict to just one provider
                        Self.Set_Completion (Result.Provider);
                     end if;

                     On_Entry_Changed (Self);

                  when Role_To_Locations =>
                     --  Work on a copy of the pattern, since the user might
                     --  be using the completion entry while we are adding to
                     --  the locations window.
                     Command := new Command_To_Locations'
                       (Root_Command with
                        Completion => Self,
                        Provider   => Search_Provider_Access (Result.Provider),
                        Pattern    => Build
                          (Self.Pattern, Kind => Self.Pattern.Get_Kind));

                     --  ??? Since we are reusing an existing provider, this
                     --  will be impacted if the user starts a new completion
                     --  entry while we are inserting in the locations.
                     Command.Provider.Set_Pattern (Command.Pattern);

                     --  Hides the popdown and kills the idles. This also
                     --  destroys Result, which should therefore not be
                     --  accessed anymore
                     Popdown (Self);

                     Launch_Background_Command
                       (Self.Kernel,
                        Command    => Command,
                        Active     => True,
                        Show_Bar   => True,
                        Block_Exit => False);

                  when Role_Unknown =>
                     null;
               end case;
            end if;
         end if;

         Path_Free (Path);
         return True;
      end if;

      return False;
   end On_Button_Event;

   ---------------------
   -- Insert_Proposal --
   ---------------------

   procedure Insert_Proposal
     (Self : not null access Gtkada_Entry_Record'Class;
      Result : GPS.Search.Search_Result_Access)
   is
      Iter  : Gtk_Tree_Iter;
      Val   : GValue;
      Score : Gint;
   begin
      Self.Completion.Count := Self.Completion.Count + 1;
      Self.Completions.Append (Iter);

      Score := Gint (Result.Score);

      --  Fill list of completions (no text in provider column)

      Self.Completions.Set (Iter, Column_Score, Score);
      Self.Completions.Set
         (Iter, Column_Provider, Result.Provider.Display_Name);

      if Result.Long /= null then
         Self.Completions.Set
            (Iter, Column_Label,
             Result.Short.all
             & ASCII.LF & "<small>" & Result.Long.all & "</small>");
      else
         Self.Completions.Set
            (Iter, Column_Label, Result.Short.all);
      end if;

      Init (Val, GType_Pointer);
      Set_Address (Val, Result.all'Address);
      Self.Completions.Set_Value (Iter, Column_Data, Val);
      Unset (Val);
   end Insert_Proposal;

   ------------------
   -- On_Key_Press --
   ------------------

   function On_Key_Press
     (Ent   : access GObject_Record'Class;
      Event : Gdk_Event_Key) return Boolean
   is
      Self : constant Gtkada_Entry := Gtkada_Entry (Ent);

      Iter : Gtk_Tree_Iter;
      M    : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
      Modified_Selection : Boolean := False;

   begin
      if Event.Keyval = GDK_Return then
         Activate_Proposal (Self, Force => True);
         return True;

      elsif Event.Keyval = GDK_Escape then
         Popdown (Self);

         --  Keep the interface leaner
         Self.GEntry.Set_Text ("");

         Widget_Callback.Emit_By_Name (Self, Signal_Escape);

      elsif Event.Keyval = GDK_Tab then
         if Self.Pattern /= null then
            declare
               Suffix : constant String :=
                 Self.Completion.Complete_Suffix (Self.Pattern);
               Position : Gint := -1;
            begin
               Self.GEntry.Insert_Text (Suffix, Position);
               Self.GEntry.Set_Position (-1);
               return True;
            end;
         end if;

      elsif Event.Keyval = GDK_KP_Down
         or else Event.Keyval = GDK_Down
      then
         Self.View.Get_Selection.Get_Selected (M, Iter);
         Get_Iter_Next (M, Iter);
         if Iter /= Null_Iter then
            Self.View.Get_Selection.Select_Iter (Iter);
            Modified_Selection := True;
         end if;

      elsif Event.Keyval = GDK_KP_Up
         or else Event.Keyval = GDK_Up
      then
         Self.View.Get_Selection.Get_Selected (M, Iter);
         Get_Iter_Prev (M, Iter);
         if Iter /= Null_Iter then
            Self.View.Get_Selection.Select_Iter (Iter);
            Modified_Selection := True;
         end if;
      end if;

      --  If we have selected an item, displays its full description
      if Modified_Selection then
         Path := Get_Path (M, Iter);
         Self.View.Scroll_To_Cell
            (Path      => Path,
             Column    => null,
             Use_Align => False,
             Row_Align => 0.0,
             Col_Align => 0.0);
         Path_Free (Path);

         Show_Preview (Self);
         return True;
      end if;

      return False;
   end On_Key_Press;

   ------------------
   -- Need_Preview --
   ------------------

   function Need_Preview
     (Self : access Gtkada_Entry_Record'Class) return Boolean is
   begin
      return Get_History
        (Get_History (Self.Kernel).all, Self.Name.all & "-preview");
   end Need_Preview;

   -----------------------
   -- Auto_Resize_Popup --
   -----------------------

   function Auto_Resize_Popup
     (Self : access Gtkada_Entry_Record'Class) return Boolean is
   begin
      return Get_History
        (Get_History (Self.Kernel).all, "global-search-auto-resize");
   end Auto_Resize_Popup;

   ------------------
   -- Show_Preview --
   ------------------

   procedure Show_Preview (Self : access Gtkada_Entry_Record'Class) is
   begin
      if Need_Preview (Self)
         and then Self.Notes_Idle = No_Source_Id
      then
         Self.Notes_Idle := Completion_Sources.Idle_Add
            (On_Preview_Idle'Access, Self);
      end if;
   end Show_Preview;

   ---------------------
   -- On_Preview_Idle --
   ---------------------

   function On_Preview_Idle (Self : Gtkada_Entry) return Boolean is
      Iter   : Gtk_Tree_Iter;
      M      : Gtk_Tree_Model;
      Result : Search_Result_Access;
      F      : Gtk_Widget;
      Align  : Gtk_Alignment;
   begin
      if Need_Preview (Self) then
         Self.View.Get_Selection.Get_Selected (M, Iter);
         Remove_All_Children (Self.Notes_Scroll);

         if Iter /= Null_Iter then
            Result := Convert (Get_Address (+M, Iter, Column_Data));

            if Result.all in Kernel_Search_Result'Class then
               F := Kernel_Search_Result'Class (Result.all).Full;
               if F /= null then
                  if F.all in Gtk_Label_Record'Class then
                     Gtk_New (Align, 0.0, 0.0, 0.0, 0.0);
                     Align.Add (F);
                     Self.Notes_Scroll.Add (Align);
                  else
                     Self.Notes_Scroll.Add (F);
                  end if;

                  Self.Notes_Scroll.Show_All;

                  if Self.Notes_Popup /= null then
                     Self.Notes_Popup.Show_All;
                  end if;
               end if;
            end if;
         end if;

      else
         Self.Notes_Popup.Hide;
      end if;

      --  No need to retry
      Self.Notes_Idle := No_Source_Id;
      return False;
   end On_Preview_Idle;

   ----------------------
   -- On_Entry_Destroy --
   ----------------------

   procedure On_Entry_Destroy (Self : access Gtk_Widget_Record'Class) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (History_Key, History_Key_Access);
      S : constant Gtkada_Entry := Gtkada_Entry (Self);
   begin
      Popdown (S);  --  destroy the idle events

      S.Clear;

      if S.Popup /= null then
         Destroy (S.Popup);
      end if;

      Unchecked_Free (S.Name);
      Free (S.Pattern);
      Free (S.Completion);
   end On_Entry_Destroy;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : access Gtkada_Entry_Record'Class) is
      Iter : Gtk_Tree_Iter := Self.Completions.Get_Iter_First;
      Result : Search_Result_Access;
   begin
      --  Free the completion proposals
      while Iter /= Null_Iter loop
         Result := Convert
            (Get_Address (+Self.Completions, Iter, Column_Data));
         Free (Result);
         Self.Completions.Next (Iter);
      end loop;

      Self.Completions.Clear;
      Self.Need_Clear := False;
   end Clear;

   -------------
   -- On_Idle --
   -------------

   function On_Idle (Self : Gtkada_Entry) return Boolean is
      Has_Next : Boolean;
      Result   : Search_Result_Access;
      Start    : Time;
      Count    : Natural := 0;
      Inserted : Boolean := False;
   begin
      Self.Completion.Next (Result => Result, Has_Next => Has_Next);

      if Self.Need_Clear then
         --  Clear at the last minute to limit flickering.
         Clear (Self);
      end if;

      Start := Clock;
      loop
         if Result /= null then
            Insert_Proposal (Self, Result);
            Inserted := True;
         end if;

         if not Has_Next then
            Self.Idle := No_Source_Id;
            Trace (Me, "No more after" & Count'Img);
            return False;
         end if;

         Count := Count + 1;

         exit when Clock - Start > Max_Idle_Duration;

         Self.Completion.Next (Result => Result, Has_Next => Has_Next);
      end loop;

      Trace (Me, "Inserted" & Count'Img & " matches");

      if Inserted then
         Resize_Popup (Self, Height_Only => True);
      end if;

      return True;
   end On_Idle;

   ------------------
   -- Resize_Popup --
   ------------------

   procedure Resize_Popup
      (Self : not null access Gtkada_Entry_Record'Class;
       Height_Only : Boolean)
   is
      Width : Gint;
      Gdk_X, Gdk_Y : Gint;
      X, Y : Gint;
      MaxX, MaxY : Gint;
      Root_X, Root_Y : Gint;
      Toplevel : Gtk_Widget;
      Alloc : Gtk_Allocation;
      Popup : Gtk_Window;
      Height : Gint;
      Iter : Gtk_Tree_Iter;
      Result : Search_Result_Access;
      Layout : Pango_Layout;
      W, H : Gint;
   begin
      if Self.Popup /= null then
         --  Position of the completion entry within its toplevel window
         Get_Origin (Get_Window (Self), Gdk_X, Gdk_Y);
         Self.Get_Allocation (Alloc);
         Gdk_X := Gdk_X + Alloc.X;
         Gdk_Y := Gdk_Y + Alloc.Y;

         --  Make sure window doesn't get past screen limits
         Toplevel := Self.Get_Toplevel;
         Get_Origin (Get_Window (Toplevel), Root_X, Root_Y);
         MaxX := Root_X + Toplevel.Get_Allocated_Width;
         MaxY := Root_Y + Toplevel.Get_Allocated_Height;

         --  Compute the ideal height. We do not compute the ideal width,
         --  since we don't want to have to move the window around and want
         --  it aligned on the GPS right side.

         Width := Result_Width + Provider_Label_Width;
         X := Gint'Min (Gdk_X, MaxX - Width);
         Y := Gdk_Y + Self.GEntry.Get_Allocated_Height;

         if Auto_Resize_Popup (Self) then
            --  Unfortunately, there doesn't seem to be a convenient way to
            --  compute the ideal height for a GtkTreeView (using the upper
            --  value of the vadjustment doesn't work either since it never
            --  decreases), and Get_Preferred_Height always returns 0 unless
            --  we are using Fixed_Height mode, but in our case the rows do not
            --  all have the same height. Self.View.Get_Cell_Area returns the
            --  height of a row, but only if it is visible which is not the
            --  case yet).

            Layout := Create_Pango_Layout (Self.View);
            Layout.Set_Text ("0mp");
            Layout.Get_Pixel_Size (W, H);
            Unref (Layout);

            --  '3' is the height of the separator. Should be computed from the
            --  widget itself perhaps
            Height := Self.Settings.Get_Allocated_Height + 3;

            Iter := Self.Completions.Get_Iter_First;
            while Iter /= Null_Iter loop
               Result := Convert
                 (Get_Address (+Self.Completions, Iter, Column_Data));

               --  5 is the height of separators between rows. It comes from
               --  the theme, though, so this is only an approximation. At
               --  worse the popup window will be too high
               Height := Height + H + 5;

               if Result.Long /= null then
                  Height := Height + H;
               end if;

               exit when Height > MaxY - Y;  --  no need to go further

               Next (Self.Completions, Iter);
            end loop;

            Height := Gint'Min (Height, MaxY - Y);

         else
            Height := MaxY - Y;
         end if;

         if not Height_Only then
            Self.Popup.Move (X, Y);
            Popup := Gtk_Window (Self.Notes_Popup);
            Popup.Set_Size_Request (Preview_Width, Preview_Height);
            Popup.Move (X - Preview_Width - Preview_Right_Margin, Y);
         end if;

         Self.Popup.Set_Size_Request (Width, Height);
         Self.Popup.Queue_Resize;
      end if;
   end Resize_Popup;

   -----------
   -- Popup --
   -----------

   procedure Popup (Self : not null access Gtkada_Entry_Record) is
      Toplevel : Gtk_Widget;
      Status : Gdk_Grab_Status;
   begin
      if Self.Popup /= null and then not Self.Popup.Get_Visible then
         Resize_Popup (Self, Height_Only => False);

         Toplevel := Self.Get_Toplevel;
         if Toplevel /= null
            and then Toplevel.all in Gtk_Window_Record'Class
         then
            Gtk_Window (Toplevel).Get_Group.Add_Window (Self.Popup);
            Self.Popup.Set_Transient_For (Gtk_Window (Toplevel));
         end if;

         Self.Popup.Set_Screen (Self.Get_Screen);
         Gtk_Window (Self.Notes_Popup).Set_Screen (Self.Get_Screen);
         Self.Popup.Show_All;

         --  Code from gtkcombobox.c
         if Do_Grabs then
            declare
               use Device_List;
               Mgr : constant Gdk_Device_Manager :=
                  Get_Device_Manager (Self.Get_Display);
               Devices : Device_List.Glist :=
                  Mgr.List_Devices (Gdk_Device_Type_Master);
            begin
               Self.Grab_Device := Get_Data (Devices);

               if Self.Grab_Device /= null
                  and then Self.Grab_Device.Get_Source = Source_Keyboard
               then
                  Self.Grab_Device := Self.Grab_Device.Get_Associated_Device;
               end if;

               Free (Devices);
            end;

            if Self.Grab_Device = null then
               Trace (Me, "No current device on which to grab");
            else
               --  ??? This seems to have no effect
               Status := Self.Grab_Device.Grab
                  (Window => Self.View.Get_Window,
                   Grab_Ownership => Ownership_Window,
                   Owner_Events   => True,
                   Event_Mask     => Button_Press_Mask or Button_Release_Mask,
                   Cursor         => null,
                   Time           => 0);
               if Status /= Grab_Success then
                  Trace (Me, "Grab failed");
                  Self.Grab_Device := null;
               else
                  Trace (Me, "Grab on "
                     & Self.Grab_Device.Get_Device_Type'Img & " "
                     & Self.Grab_Device.Get_Mode'Img & " "
                     & Self.Grab_Device.Get_Source'Img & " "
                     & Self.Grab_Device.Get_Name);
               end if;

               --  If we use Gtk.Main.Device_Grab_Add instead, we seem to
               --  properly capture all mouse events, but also keyboard events
               --  and the entry no longer receives them...
               --
               --     Device_Grab_Add (Self.View, Self.Grab_Device, True);
            end if;
         end if;
      end if;

      Self.Notes_Popup.Hide;

      --  Force the focus, so that focus-out-event is meaningful and the user
      --  can immediately interact through the keyboard
      if not Self.GEntry.Has_Focus then
         Grab_Toplevel_Focus (Get_MDI (Self.Kernel), Self.GEntry);
      end if;
   end Popup;

   -------------
   -- Popdown --
   -------------

   procedure Popdown (Self : not null access Gtkada_Entry_Record) is
   begin
      if Self.Idle /= No_Source_Id then
         Remove (Self.Idle);
         Self.Idle := No_Source_Id;
      end if;

      if Self.Notes_Idle /= No_Source_Id then
         Remove (Self.Notes_Idle);
         Self.Notes_Idle := No_Source_Id;
      end if;

      if Self.Popup /= null then
         if Do_Grabs and then Self.Grab_Device /= null then
            Self.Grab_Device.Ungrab (0);
            Self.Grab_Device := null;
         end if;

         Self.Popup.Hide;
         Self.Notes_Popup.Hide;
      end if;
   end Popdown;

   -------------------------
   -- On_Settings_Changed --
   -------------------------

   procedure On_Settings_Changed (Self : access GObject_Record'Class) is
      S : constant Gtkada_Entry := Gtkada_Entry (Self);
      T : constant String := S.Settings_Kind.Get_Active_Id;
      K : constant Search_Kind := Search_Kind'Value (T);
   begin
      S.Settings_Whole_Word.Set_Sensitive (K = Regexp);
      Add_To_History (Get_History (S.Kernel).all, S.Name.all & "-kind", T);

      Show_Preview (S);
      On_Entry_Changed (S);
   end On_Settings_Changed;

   ----------------------
   -- On_Entry_Changed --
   ----------------------

   procedure On_Entry_Changed (Self  : access GObject_Record'Class) is
      S : constant Gtkada_Entry := Gtkada_Entry (Self);
      Text : constant String := S.GEntry.Get_Text;
   begin
      Free (S.Pattern);
      S.Completion.Count := 0;

      if Text /= "" then
         S.Pattern := GPS.Search.Build
           (Pattern         => Text,
            Allow_Highlight => True,
            Case_Sensitive  => S.Settings_Case_Sensitive.Get_Active,
            Whole_Word      => S.Settings_Whole_Word.Get_Active,
            Kind        => Search_Kind'Value (S.Settings_Kind.Get_Active_Id));
         S.Completion.Set_Pattern (S.Pattern);
      end if;

      --  Since the list of completions will change shortly, give up on
      --  loading the preview.
      if S.Notes_Idle /= No_Source_Id then
         Remove (S.Notes_Idle);
         S.Notes_Idle := No_Source_Id;
      end if;

      if Text = "" then
         S.Clear;
         Popdown (S);

      else
         Popup (S);
         S.Need_Clear := True;

         if S.Idle = No_Source_Id then
            S.Idle := Completion_Sources.Idle_Add (On_Idle'Access, S);
         end if;
      end if;
   end On_Entry_Changed;

   --------------------
   -- Set_Completion --
   --------------------

   procedure Set_Completion
      (Self : not null access Gtkada_Entry_Record;
       Completion : not null access GPS.Search.Search_Provider'Class) is
   begin
      Self.Completion := Search_Provider_Access (Completion);
      Self.GEntry.Set_Tooltip_Markup (Completion.Documentation);
   end Set_Completion;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Self : not null access Gtkada_Entry_Record;
       Text : String) is
   begin
      Self.GEntry.Set_Text (Text);
      Self.GEntry.Select_Region (0, -1);
   end Set_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
      (Self : not null access Gtkada_Entry_Record) return String is
   begin
      return Self.GEntry.Get_Text;
   end Get_Text;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Command_To_Locations) return Command_Return_Type
   is
      Result   : Search_Result_Access;
      Has_Next : Boolean;
   begin
      Self.Provider.Next (Result, Has_Next);

      if Result /= null then
         Result.To_Message;
         Free (Result);
      end if;

      if Has_Next then
         return Commands.Execute_Again;
      else
         return Commands.Success;
      end if;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Command_To_Locations) is
   begin
      Free (Self.Pattern);
      Free (Root_Command (Self));  --  inherited
   end Free;

   -------------------
   -- Show_Settings --
   -------------------

   procedure Show_Settings (Self : access GObject_Record'Class) is
      S      : constant Gtkada_Entry := Gtkada_Entry (Self);
      Win    : Gtk_Dialog;
      Resp   : Gtk_Response_Type;
      Button : Gtk_Widget;
      pragma Unreferenced (Resp, Button);
      Preview : Gtk_Check_Button;
      Resize  : Gtk_Check_Button;
   begin
      Gtk_New
        (Win,
         Title  => -"Search settings",
         Parent => Get_Main_Window (S.Kernel),
         Flags  => Modal or Destroy_With_Parent or No_Separator);
      Win.Set_Default_Size (500, 480);

      Gtk_New (Preview, -"Preview");
      Associate (Get_History (S.Kernel).all,
                 S.Name.all & "-preview",
                 Preview,
                 Default => True);
      Win.Get_Content_Area.Pack_Start (Preview, Expand => False);
      Preview.On_Toggled (On_Settings_Changed'Access, Self);

      Gtk_New (Resize, -"Auto-resize popup");
      Associate (Get_History (S.Kernel).all,
                 "global-search-auto-resize",
                 Resize,
                 Default => False);
      Win.Get_Content_Area.Pack_Start (Resize, Expand => False);
      Resize.On_Toggled (On_Settings_Changed'Access, Self);

      Kernel_Search_Provider_Access (S.Completion).Edit_Settings
        (Win.Get_Content_Area, Self, On_Settings_Changed'Access);

      Button := Win.Add_Button (Stock_Ok, Gtk_Response_OK);

      Win.Show_All;
      Resp := Win.Run;
      Win.Destroy;
   end Show_Settings;

end Gtkada.Entry_Completion;
