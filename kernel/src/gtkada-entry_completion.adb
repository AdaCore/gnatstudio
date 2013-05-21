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

with Gdk.Event;                  use Gdk.Event;
with Gdk.Types;                  use Gdk.Types;
with Gdk.Types.Keysyms;          use Gdk.Types.Keysyms;
with Glib;                       use Glib;
with Glib.Main;                  use Glib.Main;
with Glib.Object;                use Glib.Object;
with Glib.Values;                use Glib.Values;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Editable;               use Gtk.Editable;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.List_Store;             use Gtk.List_Store;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Style_Context;          use Gtk.Style_Context;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Widget;                 use Gtk.Widget;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Search;                 use GPS.Search;
with Histories;                  use Histories;
with System;

package body Gtkada.Entry_Completion is
   Me : constant Trace_Handle := Create ("SEARCH");

   Max_Idle_Duration : constant Duration := 0.05;
   --  Maximum time spent in the idle callback to insert the possible
   --  completions.

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

   procedure Clear (Self : access Gtkada_Entry_Record'Class);
   --  Clear the list of completions, and free the memory

   package Completion_Sources is new Glib.Main.Generic_Sources (Gtkada_Entry);

   procedure Insert_Proposal
     (Self : not null access Gtkada_Entry_Record'Class;
      Result : GPS.Search.Search_Result_Access);
   --  Create a new completion proposal showing result

   function On_Proposal_Click
      (Ent   : access GObject_Record'Class;
       Event : Gdk_Event_Button) return Boolean;
   --  Called when a proposal is selected

   procedure On_Entry_Activate (Self : access GObject_Record'Class);
   --  Called when <enter> is pressed in the entry

   procedure Activate_Proposal
      (Self : not null access Gtkada_Entry_Record'Class;
       Force : Boolean);
   --  Activate the proposal that current has the focus. If none and Force
   --  is True, activates the first proposal in the list.

   function Convert is new Ada.Unchecked_Conversion
      (System.Address, Search_Result_Access);

   Column_Label : constant := 0;
   Column_Score : constant := 1;
   Column_Data  : constant := 2;

   -----------------------
   -- On_Entry_Activate --
   -----------------------

   procedure On_Entry_Activate (Self : access GObject_Record'Class) is
   begin
      Activate_Proposal (Gtkada_Entry (Self), Force => True);
   end On_Entry_Activate;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self           : out Gtkada_Entry;
      Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Completion     : access GPS.Search.Search_Provider'Class;
      Case_Sensitive : Boolean := True;
      History        : Histories.History_Key := "") is
   begin
      Self := new Gtkada_Entry_Record;
      Initialize (Self, Kernel, Completion, Case_Sensitive, History);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self           : not null access Gtkada_Entry_Record'Class;
      Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Completion     : access GPS.Search.Search_Provider'Class;
      Case_Sensitive : Boolean := True;
      History        : Histories.History_Key := "")
   is
      Scrolled : Gtk_Scrolled_Window;
      List : Widget_List.Glist := Widget_List.Null_List;
      Col  : Gint;
      C    : Gtk_Tree_View_Column;
      Render : Gtk_Cell_Renderer_Text;
      pragma Unreferenced (Col);

   begin
      Initialize_Vbox (Self, Homogeneous => False, Spacing => 5);
      Self.Case_Sensitive := Case_Sensitive;
      Self.Completion := GPS.Search.Search_Provider_Access (Completion);
      Self.Kernel := Kernel_Handle (Kernel);

      Gtk_New (Self.GEntry);
      Self.GEntry.Set_Activates_Default (False);
      Self.GEntry.On_Activate (On_Entry_Activate'Access, Self);
      Self.GEntry.Set_Placeholder_Text ("filename");
      Self.GEntry.Set_Width_Chars (25);
      Self.Pack_Start (Self.GEntry, Expand => False);

      Gtk_New (Scrolled);
      Get_Style_Context (Scrolled).Add_Class ("completion-list");
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Scrolled.Set_Shadow_Type (Shadow_None);
      Self.Pack_Start (Scrolled, Expand => True, Fill => True);

      Gtk_New (Self.Completions,
               (Column_Label => GType_String,
                Column_Score => GType_Int,
                Column_Data  => GType_Pointer));
      Gtk_New (Self.View, Self.Completions);
      Unref (Self.Completions);
      Self.View.Set_Headers_Visible (False);
      Self.View.Get_Selection.Set_Mode (Selection_Single);
      Self.View.Set_Rules_Hint (True);
      Scrolled.Add (Self.View);

      Self.Completions.Set_Sort_Column_Id
         (Sort_Column_Id => Column_Score,
          Order          => Sort_Descending);

      Gtk_New (C);
      C.Set_Sort_Column_Id (Column_Score);
      C.Set_Sort_Order (Sort_Descending);
      C.Clicked;
      Col := Self.View.Append_Column (C);

      Gtk_New (Render);
      C.Pack_Start (Render, False);
      C.Add_Attribute (Render, "markup", Column_Label);

      Widget_List.Append (List, Gtk_Widget (Self.GEntry));
      Widget_List.Append (List, Gtk_Widget (Scrolled));
      Self.Set_Focus_Chain (List);
      Widget_List.Free (List);

      Self.On_Destroy (On_Entry_Destroy'Access);
      Gtk.Editable.On_Changed (+Self.GEntry, On_Entry_Changed'Access, Self);

      Self.View.On_Button_Press_Event (On_Proposal_Click'Access, Self);
      Self.GEntry.On_Key_Press_Event (On_Key_Press'Access, Self);

      --  Set the current entry to be the previously inserted one
      if History /= "" then
         Set_Max_Length (Kernel.Get_History.all, 5, History);
         Self.Hist := Get_History (Kernel.Get_History.all, History);
         Self.History_Key := new History_Key'(History);

         if Self.Hist /= null then
            Self.GEntry.Set_Text (Self.Hist (Self.Hist'First).all);
            Self.GEntry.Select_Region (0, -1);
         end if;
      end if;

      Self.GEntry.Grab_Focus;
   end Initialize;

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
      Result_Need_Free : Boolean := False;
   begin
      Self.View.Get_Selection.Get_Selected (M, Iter);

      if Iter /= Null_Iter then
         Result := Convert
            (Get_Address (+Self.Completions, Iter, Column_Data));
      else
         if Force then
            Iter := Self.Completions.Get_Iter_First;
            if Iter /= Null_Iter then
               Result := Convert
                  (Get_Address (+Self.Completions, Iter, Column_Data));
            else
               Result := Self.Fallback (Self.GEntry.Get_Text);
               Result_Need_Free := True;
            end if;
         end if;
      end if;

      if Result /= null then
         Self.Hist := null;  --  do not free
         Self.Kernel.Add_To_History
            (Self.History_Key.all, Result.Short.all);
         Result.Execute (Give_Focus => True);

         if Result_Need_Free then
            Free (Result);
         end if;

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

   -----------------------
   -- On_Proposal_Click --
   -----------------------

   function On_Proposal_Click
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

      return False;
   end On_Proposal_Click;

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

      Iter  : Gtk_Tree_Iter;
      Val   : GValue;
      Score : Gint;
      M     : Integer;
   begin
      Self.Completions.Append (Iter);

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
               and then Result.Short.all = Self.Hist (H).all
            then
               Score := Score + Gint ((Max_History + 1 - H) * 20);
               exit;
            end if;
         end loop;
      end if;

      --  Fill list of completions

      Self.Completions.Set (Iter, Column_Score, Score);

      if Result.Long /= null then
         Self.Completions.Set
            (Iter, Column_Label,
             "<big><b>" & Result.Short.all & "</b></big>"
             & ASCII.LF & Result.Long.all);
      else
         Self.Completions.Set
            (Iter, Column_Label,
             "<big><b>" & Result.Short.all & "</b></big>");
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
   begin
      if Event.Keyval = GDK_Return then
         Activate_Proposal (Self, Force => True);
         return True;

      elsif Event.Keyval = GDK_Tab
         or else Event.Keyval = GDK_KP_Down
         or else Event.Keyval = GDK_Down
      then
         Self.View.Get_Selection.Get_Selected (M, Iter);
         if Iter = Null_Iter then
            Iter := Self.Completions.Get_Iter_First;
         else
            Self.Completions.Next (Iter);
            if Iter = Null_Iter then
               Iter := Self.Completions.Get_Iter_First;
            end if;
         end if;

         if Iter /= Null_Iter then
            Self.View.Get_Selection.Select_Iter (Iter);
         end if;

         return True;

      elsif Event.Keyval = GDK_KP_Up
         or else Event.Keyval = GDK_Up
      then
         Self.View.Get_Selection.Get_Selected (M, Iter);
         if Iter = Null_Iter
            or else Iter = Self.Completions.Get_Iter_First
         then
            Iter := Self.Completions.Nth_Child
               (Null_Iter, Self.Completions.N_Children);
         else
            Self.Completions.Previous (Iter);
         end if;

         if Iter /= Null_Iter then
            Self.View.Get_Selection.Select_Iter (Iter);
         end if;

         return True;
      end if;
      return False;
   end On_Key_Press;

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

      S.Clear;

      Unchecked_Free (S.History_Key);
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
         Case_Sensitive => S.Case_Sensitive,
         Kind           => Full_Text);  --  Fuzzy
      S.Completion.Set_Pattern (S.Pattern);

      if Text = "" then
         if S.Idle /= No_Source_Id then
            Remove (S.Idle);
            S.Idle := No_Source_Id;
         end if;
         S.Clear;
      else
         S.Need_Clear := True;

         if S.Idle = No_Source_Id then
            S.Idle := Completion_Sources.Idle_Add (On_Idle'Access, S);
         end if;
      end if;
   end On_Entry_Changed;

end Gtkada.Entry_Completion;
