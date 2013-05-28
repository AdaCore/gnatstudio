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

with Pango.Layout;               use Pango.Layout;
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
with Gtk.Box;                    use Gtk.Box;
with Gtk.Cell_Renderer;          use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;           use Gtk.Check_Button;
with Gtk.Combo_Box_Text;         use Gtk.Combo_Box_Text;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Editable;               use Gtk.Editable;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Text_Buffer;            use Gtk.Text_Buffer;
with Gtk.Text_View;              use Gtk.Text_View;
with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtkada.Search_Entry;        use Gtkada.Search_Entry;
with Gtkada.Style;               use Gtkada.Style;
with Gtk.Separator;              use Gtk.Separator;
with Gtk.Style_Context;          use Gtk.Style_Context;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Intl;                   use GPS.Intl;
with GPS.Search;                 use GPS.Search;
with GUI_Utils;                  use GUI_Utils;
with Histories;                  use Histories;
with System;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtkada.Entry_Completion is
   Me : constant Trace_Handle := Create ("SEARCH");

   Max_Idle_Duration : constant Duration := 0.05;
   --  Maximum time spent in the idle callback to insert the possible
   --  completions.

   Do_Grabs : constant Boolean := False;
   --  Whether to attempt grabbing the pointer

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

   procedure Clear (Self : access Gtkada_Entry_Record'Class);
   --  Clear the list of completions, and free the memory

   package Completion_Sources is new Glib.Main.Generic_Sources (Gtkada_Entry);

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

   procedure Get_Iter_Next
      (Tree : not null access Gtk_Tree_Store_Record'Class;
       Iter : in out Gtk_Tree_Iter);
   procedure Get_Iter_Prev
      (Tree : not null access Gtk_Tree_Store_Record'Class;
       Iter : in out Gtk_Tree_Iter);
   --  Returns the next result proposal. Pass Null_Iter to get the
   --  first or last item in the tree

   function Get_Last_Child
      (Tree : not null access Gtk_Tree_Store_Record'Class;
       Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter;
   --  Return the last child of Iter

   function Convert is new Ada.Unchecked_Conversion
      (System.Address, Search_Result_Access);

   Column_Label    : constant := 0;
   Column_Score    : constant := 1;
   Column_Data     : constant := 2;
   Column_Provider : constant := 3;

   Completion_Class_Record : Glib.Object.Ada_GObject_Class :=
      Glib.Object.Uninitialized_Class;

   Signals : constant chars_ptr_array :=
      (1 => New_String (String (Signal_Activate)),
       2 => New_String (String (Signal_Escape)));

   --------------------
   -- Get_Last_Child --
   --------------------

   function Get_Last_Child
      (Tree : not null access Gtk_Tree_Store_Record'Class;
       Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      N : constant Gint := Tree.N_Children (Iter);
   begin
      if N > 0 then
         return Tree.Nth_Child (Iter, N - 1);
      else
         return Null_Iter;
      end if;
   end Get_Last_Child;

   -------------------
   -- Get_Iter_Next --
   -------------------

   procedure Get_Iter_Next
      (Tree : not null access Gtk_Tree_Store_Record'Class;
       Iter : in out Gtk_Tree_Iter)
   is
      Iter2 : Gtk_Tree_Iter;
   begin
      if Iter /= Null_Iter then
         Iter2 := Iter;
         Tree.Next (Iter2);

         if Iter2 = Null_Iter then
            --  No more iter at the current level

            Iter2 := Tree.Parent (Iter);
            Tree.Next (Iter2);
            if Iter2 = Null_Iter then
               Iter2 := Tree.Get_Iter_First;
            end if;

            Iter := Tree.Children (Iter2);
         else
            Iter := Iter2;
         end if;

      else
         Iter := Tree.Get_Iter_First;
         if Iter /= Null_Iter then
            Iter := Tree.Children (Iter);
         end if;
      end if;
   end Get_Iter_Next;

   -------------------
   -- Get_Iter_Prev --
   -------------------

   procedure Get_Iter_Prev
      (Tree : not null access Gtk_Tree_Store_Record'Class;
       Iter : in out Gtk_Tree_Iter)
   is
      Iter2 : Gtk_Tree_Iter;
   begin
      if Iter /= Null_Iter then
         Iter2 := Iter;
         Tree.Previous (Iter2);

         if Iter2 = Null_Iter then
            --  No more iter at the current level
            Iter2 := Tree.Parent (Iter);
            Tree.Previous (Iter2);
            if Iter2 = Null_Iter then
               Iter2 := Get_Last_Child (Tree, Null_Iter);
            end if;

            Iter := Get_Last_Child (Tree, Iter2);
         else
            Iter := Iter2;
         end if;

      else
         Iter := Get_Last_Child (Tree, Get_Last_Child (Tree, Null_Iter));
      end if;
   end Get_Iter_Prev;

   -----------------------
   -- On_Entry_Activate --
   -----------------------

   procedure On_Entry_Activate (Self : access GObject_Record'Class) is
   begin
      Activate_Proposal (Gtkada_Entry (Self), Force => True);
   end On_Entry_Activate;

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
      Preview          : Boolean := True;
      Completion_In_Popup : Boolean := True) is
   begin
      Self := new Gtkada_Entry_Record;
      Initialize
         (Self, Kernel, Completion, Name, Case_Sensitive, Preview,
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
      Preview          : Boolean := True;
      Completion_In_Popup : Boolean := True)
   is
      Scrolled : Gtk_Scrolled_Window;
      Box  : Gtk_Box;
      Settings : Gtk_Box;
      Col  : Gint;
      C    : Gtk_Tree_View_Column;
      Sep  : Gtk_Separator;
      Render : Gtk_Cell_Renderer_Text;
      Dummy  : Boolean;
      Color  : Gdk_RGBA;
      pragma Unreferenced (Col, Dummy);

   begin
      G_New (Self, Get_Type);
      Gtk.Box.Initialize_Vbox (Self, Homogeneous => False);

      Self.Completion := GPS.Search.Search_Provider_Access (Completion);
      Self.Kernel := Kernel_Handle (Kernel);
      Self.Name := new History_Key'(Name);

      Gtk_New (Self.GEntry);
      Self.Pack_Start (Self.GEntry, Expand => False, Fill => False);
      Self.GEntry.Set_Activates_Default (False);
      Self.GEntry.On_Activate (On_Entry_Activate'Access, Self);
      Self.GEntry.Set_Placeholder_Text ("search");
      Self.GEntry.Set_Tooltip_Markup (Completion.Documentation);

      if Completion_In_Popup then
         Gtk_New (Self.Popup, Window_Popup);
         Self.Popup.Set_Name ("completion-list");
         Self.Popup.Set_Decorated (False);
         Self.Popup.Set_Type_Hint (Window_Type_Hint_Combo);
         Self.Popup.Set_Resizable (False);
         Self.Popup.Set_Skip_Taskbar_Hint (True);
         Self.Popup.Set_Skip_Pager_Hint (True);
         Get_Style_Context (Self.Popup).Add_Class ("completion");

         Gtk_New (Self.Notes_Popup, Window_Popup);
         Self.Notes_Popup.Set_Name ("completion-preview");
         Self.Notes_Popup.Set_Decorated (False);
         Self.Notes_Popup.Set_Type_Hint (Window_Type_Hint_Combo);
         Self.Notes_Popup.Set_Resizable (False);
         Self.Notes_Popup.Set_Skip_Taskbar_Hint (True);
         Self.Notes_Popup.Set_Skip_Pager_Hint (True);
         Get_Style_Context (Self.Notes_Popup).Add_Class ("completion");

         Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 0);
         Self.Popup.Add (Box);

      else
         Box := Gtk_Box (Self);
      end if;

      Gtk_New_Hbox (Self.Completion_Box, Homogeneous => False);
      Box.Pack_Start (Self.Completion_Box, Expand => True, Fill => True);

      --  Scrolled window for the possible completions

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Scrolled.Set_Shadow_Type (Shadow_None);
      Self.Completion_Box.Pack_Start (Scrolled, Expand => True, Fill => True);

      Gtk_New (Self.Completions,
               (Column_Label    => GType_String,
                Column_Score    => GType_Int,
                Column_Data     => GType_Pointer,
                Column_Provider => GType_String));
      Gtk_New (Self.View, Self.Completions);
      Unref (Self.Completions);
      Self.View.Set_Headers_Visible (False);
      Self.View.Get_Selection.Set_Mode (Selection_Single);
      Self.View.Set_Rules_Hint (True);
      Self.View.Set_Expander_Column (null);
      Self.View.Set_Show_Expanders (False);
      Scrolled.Add (Self.View);

      Self.Completions.Set_Sort_Column_Id
         (Sort_Column_Id => Column_Score,
          Order          => Sort_Descending);

      Gtk_New (C);
      Col := Self.View.Append_Column (C);
      Gtk_New (Render);
      C.Pack_Start (Render, False);
      C.Add_Attribute (Render, "text", Column_Provider);

      Get_Style_Context (Self.View).Get_Background_Color
         (Gtk_State_Flag_Normal, Color);
      Set_Property
         (Render, Gtk.Cell_Renderer_Text.Foreground_Property, "gray");
      Set_Property
         (Render, Gtk.Cell_Renderer_Text.Background_Rgba_Property,
          Shade_Or_Lighten (Color, 0.05));
      Set_Property (Render, Gtk.Cell_Renderer.Xalign_Property, 1.0);
      Set_Property (Render, Gtk.Cell_Renderer.Yalign_Property, 0.0);

      Gtk_New (C);
      C.Set_Sort_Column_Id (Column_Score);
      C.Set_Sort_Order (Sort_Descending);
      C.Clicked;
      Col := Self.View.Append_Column (C);

      Gtk_New (Render);
      C.Pack_Start (Render, False);
      C.Add_Attribute (Render, "markup", Column_Label);

      --  Extra notes for selected item

      Gtk_New (Self.Notes_Scroll);
      Gtk_New (Self.Notes_Buffer);
      Gtk_New (Self.Notes_View, Self.Notes_Buffer);
      Unref (Self.Notes_Buffer);
      Self.Notes_View.Set_Editable (False);
      Self.Notes_Scroll.Add (Self.Notes_View);

      if Self.Notes_Popup /= null then
         Self.Notes_Popup.Add (Self.Notes_Scroll);
      else
         Self.Completion_Box.Pack_Start
            (Self.Notes_Scroll, Expand => True, Fill => True);
      end if;

      --  The settings panel

      Gtk_New_Hseparator (Sep);
      Box.Pack_Start (Sep, Expand => False);

      Gtk_New_Hbox (Settings, Homogeneous => False);
      Box.Pack_Start (Settings, Expand => False);

      Gtk_New (Self.Settings_Case_Sensitive, -"Case Sensitive");
      Associate (Get_History (Kernel).all,
                 Name & "-case_sensitive",
                 Self.Settings_Case_Sensitive,
                 Default => Case_Sensitive);
      Settings.Pack_Start (Self.Settings_Case_Sensitive, Expand => False);

      Gtk_New (Self.Settings_Whole_Word, -"Whole Word");
      Associate (Get_History (Kernel).all,
                 Name & "-whole_word",
                 Self.Settings_Whole_Word,
                 Default => False);
      Settings.Pack_Start (Self.Settings_Whole_Word, Expand => False);

      Gtk_New (Self.Settings_Preview, -"Preview");
      Associate (Get_History (Kernel).all,
                 Name & "-preview",
                 Self.Settings_Preview,
                 Default => Preview);
      Settings.Pack_Start (Self.Settings_Preview, Expand => False);

      Gtk_New (Self.Settings_Kind);
      Self.Settings_Kind.Set_Name ("global-search-kind");
      Self.Settings_Kind.Append
         (Search_Kind'Image (Full_Text), -"Substrings match");
      Self.Settings_Kind.Append
         (Search_Kind'Image (Fuzzy), -"Fuzzy match");
      Self.Settings_Kind.Append
         (Search_Kind'Image (Regexp), -"Regular expression");
      Settings.Pack_Start (Self.Settings_Kind, Expand => False);

      Create_New_Key_If_Necessary
         (Get_History (Kernel).all, Name & "-kind", Strings);
      Set_Max_Length (Get_History (Kernel).all, 1, Name & "-kind");
      Dummy := Self.Settings_Kind.Set_Active_Id
         (Most_Recent (Get_History (Kernel), Name & "-kind",
                       Search_Kind'Image (Fuzzy)));

      Self.Settings_Case_Sensitive.On_Toggled
         (On_Settings_Changed'Access, Self);
      Self.Settings_Whole_Word.On_Toggled (On_Settings_Changed'Access, Self);
      Self.Settings_Preview.On_Toggled (On_Settings_Changed'Access, Self);
      Self.Settings_Kind.On_Changed (On_Settings_Changed'Access, Self);

      Self.On_Destroy (On_Entry_Destroy'Access);
      Self.View.On_Button_Press_Event (On_Button_Event'Access, Self);
      Self.GEntry.On_Key_Press_Event (On_Key_Press'Access, Self);
      Self.GEntry.On_Focus_Out_Event (On_Focus_Out'Access, Self);

      --  Set the current entry to be the previously inserted one
      Set_Max_Length (Kernel.Get_History.all, 5, Name);
      Self.Hist := Get_History (Kernel.Get_History.all, Name);

      if Self.Hist /= null
         and then Self.Hist (Self.Hist'First).all /= ""

         --  When the completion is in a popup, having a prefilled entry is
         --  not convenient: use need to click in it to get the focus, then
         --  triple-click to reselect the full text
         and then not Completion_In_Popup
      then
         Self.GEntry.Set_Text (Self.Hist (Self.Hist'First).all);
         Self.GEntry.Select_Region (0, -1);
      end if;

      Self.GEntry.Grab_Focus;

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
         Result := Convert
            (Get_Address (+Self.Completions, Iter, Column_Data));
      else
         if Force then
            Iter := Null_Iter;
            Get_Iter_Next (Self.Completions, Iter);

            if Iter /= Null_Iter then
               Result := Convert
                  (Get_Address (+Self.Completions, Iter, Column_Data));
            end if;
         end if;
      end if;

      if Result /= null then
         Self.Hist := null;  --  do not free

         if Result.Id /= null then
            Self.Kernel.Add_To_History (Self.Name.all, Result.Id.all);
         end if;

         Popdown (Self);

         Widget_Callback.Emit_By_Name (Self, Signal_Activate);

         Result.Execute (Give_Focus => True);

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
      Iter : Gtk_Tree_Iter;
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

         if Found then
            Iter := Self.Completions.Get_Iter (Path);
            Self.View.Get_Selection.Select_Iter (Iter);
            Activate_Proposal (Gtkada_Entry (Ent), Force => False);
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
      Max_History : constant := 5;
      --  Maximum number of history items that are taken into account for
      --  the scoring.

      Prov : constant String := Result.Provider.Display_Name;

      Iter  : Gtk_Tree_Iter;
      Val   : GValue;
      Score : Gint;
      M     : Integer;
      Parent : Gtk_Tree_Iter;
      Need_To_Expand : Boolean := False;
   begin
      Parent := Self.Completions.Get_Iter_First;
      while Parent /= Null_Iter loop
         if Self.Completions.Get_String (Parent, Column_Provider) = Prov then
            exit;
         end if;
         Self.Completions.Next (Parent);
      end loop;

      if Parent = Null_Iter then
         Need_To_Expand := True;
         Self.Completions.Append (Iter => Parent, Parent => Null_Iter);
         Self.Completions.Set (Parent, Column_Provider, Prov);

         --  ??? Should order provider based on user preferences
         Self.Completions.Set (Parent, Column_Score, 100);
      end if;

      Self.Completions.Append (Iter => Iter, Parent => Parent);

      if Need_To_Expand then
         Expand_Row (Self.View, Parent);
      end if;

      Score := Gint (Result.Score);

      --  Give priority to shorter items (which means the pattern
      --  matched a bigger portion of it). This way, "buffer" matches
      --  "src_editor_buffer.adb" before "src_editor_buffer-hooks.adb".

      Score := 100 * Score - Gint (Result.Short'Length);

      --  Take history into account as well (most recent items first)

      if Self.Hist /= null then
         M := Integer'Min (Self.Hist'First + Max_History, Self.Hist'Last);
         for H in Self.Hist'First .. M loop
            if Self.Hist (H) /= null
               and then Result.Id.all = Self.Hist (H).all
            then
               Score := Score + Gint ((Max_History + 1 - H) * 20);
               exit;
            end if;
         end loop;
      end if;

      --  Fill list of completions (no text in provider column)

      Self.Completions.Set (Iter, Column_Score, Score);

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

      elsif Event.Keyval = GDK_Tab
         or else Event.Keyval = GDK_KP_Down
         or else Event.Keyval = GDK_Down
      then
         Self.View.Get_Selection.Get_Selected (M, Iter);
         Get_Iter_Next (Self.Completions, Iter);
         if Iter /= Null_Iter then
            Self.View.Get_Selection.Select_Iter (Iter);
            Modified_Selection := True;
         end if;

      elsif Event.Keyval = GDK_KP_Up
         or else Event.Keyval = GDK_Up
      then
         Self.View.Get_Selection.Get_Selected (M, Iter);
         Get_Iter_Prev (Self.Completions, Iter);
         if Iter /= Null_Iter then
            Self.View.Get_Selection.Select_Iter (Iter);
            Modified_Selection := True;
         end if;
      end if;

      --  If we have selected an item, displays its full description
      if Modified_Selection then
         Path := Self.Completions.Get_Path (Iter);
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
   -- Show_Preview --
   ------------------

   procedure Show_Preview (Self : access Gtkada_Entry_Record'Class) is
   begin
      if Self.Settings_Preview.Get_Active
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
   begin
      if Self.Settings_Preview.Get_Active then
         Self.View.Get_Selection.Get_Selected (M, Iter);
         Self.Notes_Buffer.Set_Text ("");

         if Iter /= Null_Iter then
            Result := Convert
               (Get_Address (+Self.Completions, Iter, Column_Data));

            declare
               F : constant String := Result.Full;
            begin
               if F /= "" then
                  --  Reset the font, in case the prefs have changed
                  Self.Notes_View.Modify_Font (View_Fixed_Font.Get_Pref);

                  Self.Notes_View.Set_Wrap_Mode (Wrap_None); --  or Wrap_Word

                  Self.Notes_Buffer.Set_Text (F);

                  Self.Notes_Scroll.Show_All;

                  if Self.Notes_Popup /= null then
                     Self.Notes_Popup.Show_All;
                  end if;
               end if;
            end;
         end if;

      else
         if Self.Notes_Popup /= null then
            Self.Notes_Popup.Hide;
         end if;

         Self.Notes_Scroll.Hide;
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
      if S.Idle /= No_Source_Id then
         Remove (S.Idle);
         S.Idle := No_Source_Id;
      end if;

      if S.Notes_Idle /= No_Source_Id then
         Remove (S.Notes_Idle);
         S.Notes_Idle := No_Source_Id;
      end if;

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
   begin
      Self.Completion.Next (Result => Result, Has_Next => Has_Next);

      if Self.Need_Clear then
         --  Clear at the last minute to limit flickering.
         Self.Completions.Clear;
      end if;

      Start := Clock;
      loop
         if not Has_Next then
            Self.Idle := No_Source_Id;
            Trace (Me, "No more after" & Count'Img);
            return False;
         end if;

         Count := Count + 1;

         if Result /= null then
            Insert_Proposal (Self, Result);
         end if;

         exit when Clock - Start > Max_Idle_Duration;

         Self.Completion.Next (Result => Result, Has_Next => Has_Next);
      end loop;

      Trace (Me, "Inserted" & Count'Img & " matches");
      return True;
   end On_Idle;

   -----------
   -- Popup --
   -----------

   procedure Popup (Self : not null access Gtkada_Entry_Record) is
      Provider_Name_Width : constant := 100;
      Preview_Width       : constant := 300;

      Max_Window_Width : constant := 300;
      --  Maximum width of the popup window

      Char_Width, Char_Height : Gint;
      Width, Height : Gint;
      Gdk_X, Gdk_Y : Gint;
      X, Y : Gint;
      MaxX : Gint;
      Root_X, Root_Y : Gint;
      Layout : Pango_Layout;
      Toplevel : Gtk_Widget;
      Alloc : Gtk_Allocation;
      Status : Gdk_Grab_Status;
   begin
      if Self.Popup /= null and then not Self.Popup.Get_Visible then
         Layout := Create_Pango_Layout (Self.View);
         Layout.Set_Text ("0m");
         Layout.Get_Pixel_Size (Char_Width, Char_Height);

         Width := Gint'Max
             (Self.Get_Allocated_Width,  --  minimum width is that of Self
              Gint'Min (Max_Window_Width, Char_Width * 20)) + 5
             + Provider_Name_Width;
         Height := Char_Height * 22 + 5;

         --  This is the origin of the GPS window
         Get_Origin (Get_Window (Self), Gdk_X, Gdk_Y);
         Self.Get_Allocation (Alloc);
         Gdk_X := Gdk_X + Alloc.X;
         Y := Gdk_Y + Alloc.Y + Self.GEntry.Get_Allocated_Height;

         Toplevel := Self.Get_Toplevel;
         if Toplevel /= null
            and then Toplevel.all in Gtk_Window_Record'Class
         then
            Gtk_Window (Toplevel).Get_Group.Add_Window (Self.Popup);
            Self.Popup.Set_Transient_For (Gtk_Window (Toplevel));
         end if;

         --  Make sure window doesn't get past screen limits

         Get_Origin (Get_Window (Toplevel), Root_X, Root_Y);
         MaxX := Root_X + Toplevel.Get_Allocated_Width;
         X := Gint'Min (Gdk_X, MaxX - Width);

         Self.Popup.Set_Screen (Self.Get_Screen);
         Self.Popup.Move (X, Y);
         Self.Popup.Set_Size_Request (Width, Height);
         Self.Popup.Show_All;

         Self.Notes_Popup.Set_Screen (Self.Get_Screen);
         Self.Notes_Popup.Move (X - Preview_Width, Y);
         Self.Notes_Popup.Set_Size_Request (Preview_Width, Height);

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

      Self.Notes_Scroll.Hide;

      if Self.Notes_Popup /= null then
         Self.Notes_Popup.Hide;
      end if;

      --  Force the focus, so that focus-out-event is meaningful and the user
      --  can immediately interact through the keyboard
      if not Self.GEntry.Has_Focus then
         Self.GEntry.Grab_Focus;
      end if;
   end Popup;

   -------------
   -- Popdown --
   -------------

   procedure Popdown (Self : not null access Gtkada_Entry_Record) is
   begin
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
      S.Pattern := GPS.Search.Build
        (Pattern        => Text,
         Case_Sensitive => S.Settings_Case_Sensitive.Get_Active,
         Whole_Word     => S.Settings_Whole_Word.Get_Active,
         Kind          => Search_Kind'Value (S.Settings_Kind.Get_Active_Id));
      S.Completion.Set_Pattern (S.Pattern);

      --  Since the list of completions will change shortly, give up on
      --  loading the preview.
      if S.Notes_Idle /= No_Source_Id then
         Remove (S.Notes_Idle);
         S.Notes_Idle := No_Source_Id;
      end if;

      if Text = "" then
         if S.Idle /= No_Source_Id then
            Remove (S.Idle);
            S.Idle := No_Source_Id;
         end if;

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
end Gtkada.Entry_Completion;
