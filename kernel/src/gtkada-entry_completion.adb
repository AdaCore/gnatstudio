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
with GNAT.Strings;               use GNAT.Strings;

with Gdk.Event;                  use Gdk.Event;
with Gdk.Types;                  use Gdk.Types;
with Gdk.Types.Keysyms;          use Gdk.Types.Keysyms;
with Glib;                       use Glib;
with Glib.Main;                  use Glib.Main;
with Glib.Object;                use Glib.Object;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Button;                 use Gtk.Button;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Editable;               use Gtk.Editable;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.Label;                  use Gtk.Label;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Style_Context;          use Gtk.Style_Context;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GPS.Search;                 use GPS.Search;
with GUI_Utils;                  use GUI_Utils;
with Histories;                  use Histories;

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
     (Self  : access GObject_Record'Class;
      Event : Gdk_Event_Key) return Boolean;
   --  Called when the user pressed a key in the completion window

   function On_Idle (Self : Gtkada_Entry) return Boolean;
   --  Called on idle to complete the completions

   procedure Clear (Self : access Gtkada_Entry_Record'Class);
   --  Clear the list of completions, and free the memory

   package Completion_Sources is new Glib.Main.Generic_Sources (Gtkada_Entry);

   type Completion_Proposal_Record is new Gtk_Button_Record with record
      Result : GPS.Search.Search_Result_Access;
   end record;
   type Completion_Proposal is access all Completion_Proposal_Record'Class;

   procedure Gtk_New
     (Self   : out Completion_Proposal;
      Result : GPS.Search.Search_Result_Access);
   --  Create a new completion proposal showing result

   procedure On_Proposal_Click (Widget : access Gtk_Button_Record'Class);
   --  Called when a proposal is selected

   procedure On_Entry_Activate (Self : access GObject_Record'Class);
   --  Called when <enter> is pressed in the entry

   -----------------------
   -- On_Entry_Activate --
   -----------------------

   procedure On_Entry_Activate (Self : access GObject_Record'Class) is
      use Widget_List;
      S : constant Gtkada_Entry := Gtkada_Entry (Self);
      L : Widget_List.Glist := S.View.Get_Children;
      W : Gtk_Widget;
   begin
      if L /= Widget_List.Null_List then
         W := Widget_List.Get_Data (L);
         Gtk_Button (W).Clicked;
         Free (L);
      end if;
   end On_Entry_Activate;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self           : out Gtkada_Entry;
      Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Completion     : access GPS.Search.Search_Provider'Class;
      Case_Sensitive : Boolean := True;
      History        : Histories.History_Key := "")
   is
      pragma Unreferenced (Kernel);
      Scrolled : Gtk_Scrolled_Window;
      Frame    : Gtk_Frame;
      List : Widget_List.Glist := Widget_List.Null_List;

   begin
      Self := new Gtkada_Entry_Record;

      Initialize_Vbox (Self, Homogeneous => False, Spacing => 5);
      Self.Case_Sensitive := Case_Sensitive;
      Self.Completion := GPS.Search.Search_Provider_Access (Completion);

      Gtk_New (Self.GEntry);
      Self.GEntry.Set_Activates_Default (False);
      Self.Pack_Start (Self.GEntry, Expand => False);

      Self.GEntry.On_Activate (On_Entry_Activate'Access, Self);

      --  ??? Should we set initial contents to be that of the history ?
      if History /= "" then
         Self.GEntry.Set_Text ("");
         Self.GEntry.Select_Region (0, -1);
      end if;

      Self.GEntry.Set_Width_Chars (25);

      Gtk_New (Frame);
      Self.Pack_Start (Frame, Expand => True, Fill => True);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Frame.Add (Scrolled);

      Gtk_New_Vbox (Self.View, Homogeneous => False, Spacing => 0);
      Scrolled.Add_With_Viewport (Self.View);

      Widget_List.Append (List, Gtk_Widget (Self.GEntry));
      Widget_List.Append (List, Gtk_Widget (Frame));
      Self.Set_Focus_Chain (List);
      Widget_List.Free (List);

      Self.View.On_Key_Press_Event (On_Key_Press'Access, Self, After => True);
      Self.On_Destroy (On_Entry_Destroy'Access);
      Gtk.Editable.On_Changed (+Self.GEntry, On_Entry_Changed'Access, Self);
   end Gtk_New;

   -----------------------
   -- On_Proposal_Click --
   -----------------------

   procedure On_Proposal_Click (Widget : access Gtk_Button_Record'Class) is
      Prop : constant Completion_Proposal := Completion_Proposal (Widget);
      W    : Gtk_Widget;
   begin
      if Prop.Result /= null then
         Prop.Result.Execute (Give_Focus => True);

         --  ??? Temporary workaround to close the parent dialog if any.
         --  The clean approach is to have a signal that a completion has
         --  been selected, and then capture this in src_editor_module.adb
         --  to close the dialog

         W := Prop.Get_Toplevel;
         if W /= null and then W.all in Gtk_Dialog_Record'Class then
            Gtk_Dialog (W).Response (Gtk_Response_None);
         end if;
      end if;
   end On_Proposal_Click;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self   : out Completion_Proposal;
      Result : GPS.Search.Search_Result_Access)
   is
      Label : Gtk_Label;
      B     : Gtk_Box;
   begin
      Self := new Completion_Proposal_Record;
      Gtk.Button.Initialize (Self);

      Gtk_New_Vbox (B, Homogeneous => False, Spacing => 0);
      Self.Add (B);
      Self.Result := Result;

      Gtk_New (Label, Result.Short.all);
      Label.Set_Use_Markup (True);
      Label.Set_Line_Wrap (False);
      Label.Set_Alignment (0.0, 0.5);
      Get_Style_Context (Label).Add_Class ("completion-short");
      B.Pack_Start (Label, Expand => False, Fill => False);

      if Result.Long /= null then
         Gtk_New (Label, Result.Long.all);
         Label.Set_Use_Markup (True);
         Label.Set_Line_Wrap (False);
         Label.Set_Alignment (0.0, 0.5);
         Get_Style_Context (Label).Add_Class ("completion-long");
         B.Pack_Start (Label, Expand => False, Fill => False);
      end if;

      Self.On_Clicked (On_Proposal_Click'Access);
   end Gtk_New;

   ------------------
   -- On_Key_Press --
   ------------------

   function On_Key_Press
     (Self  : access GObject_Record'Class;
      Event : Gdk_Event_Key) return Boolean is
   begin
      if Event.Keyval = GDK_Return then
         return Activate_Default
           (Gtk_Window (Get_Toplevel (Gtkada_Entry (Self))));
      end if;

      return False;
   end On_Key_Press;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry
     (Self : access Gtkada_Entry_Record) return Gtk.GEntry.Gtk_Entry is
   begin
      return Self.GEntry;
   end Get_Entry;

   ----------------------
   -- On_Entry_Destroy --
   ----------------------

   procedure On_Entry_Destroy (Self : access Gtk_Widget_Record'Class) is
      S : constant Gtkada_Entry := Gtkada_Entry (Self);
   begin
      if S.Idle /= No_Source_Id then
         Remove (S.Idle);
         S.Idle := No_Source_Id;
      end if;

      S.Clear;

      Free (S.Pattern);
      Free (S.Completion);
   end On_Entry_Destroy;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : access Gtkada_Entry_Record'Class) is
   begin
      Remove_All_Children (Self.View);
      Self.Need_Clear := False;
   end Clear;

   -------------
   -- On_Idle --
   -------------

   function On_Idle (Self : Gtkada_Entry) return Boolean is
      Has_Next : Boolean;
      Result   : Search_Result_Access;
      Prop     : Completion_Proposal;
      Start    : Time;
      Count    : Natural := 0;
   begin
      Self.Completion.Next (Result => Result, Has_Next => Has_Next);

      if Self.Need_Clear then
         --  Clear at the last minute to limit flickering.
         Self.Clear;
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
            --  ??? Should sort based on score, then on history (most recent
            --  entries first), then alphabetical.
            Gtk_New (Prop, Result);
            Self.View.Pack_Start (Prop, Expand => False, Fill => False);
            Prop.Show_All;
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
         Kind           => Fuzzy);  --  Fuzzy
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
