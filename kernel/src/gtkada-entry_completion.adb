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
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Editable;               use Gtk.Editable;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GPS.Search;                 use GPS.Search;
with Histories;                  use Histories;

package body Gtkada.Entry_Completion is
   Me : constant Trace_Handle := Create ("SEARCH");

   Max_Idle_Duration : constant Duration := 0.05;
   --  Maximum time spent in the idle callback to insert the possible
   --  completions.

   procedure On_Destroy (Self : access Gtk_Widget_Record'Class);
   --  Callback when the widget is destroyed.

   procedure On_Entry_Changed (Self  : access GObject_Record'Class);
   --  Handles changes in the entry field.

   function On_Button_Press
     (Self  : access GObject_Record'Class;
      Event : Gdk_Event_Button) return Boolean;
   --  Called when the user clicked in the list

   function On_Key_Press
     (Self  : access GObject_Record'Class;
      Event : Gdk_Event_Key) return Boolean;
   --  Called when the user pressed a key in the completion window

   function On_Idle (Self : Gtkada_Entry) return Boolean;
   --  Called on idle to complete the completions

   procedure Clear (Self : access Gtkada_Entry_Record'Class);
   --  Clear the list of completions, and free the memory

   package Completion_Sources is new Glib.Main.Generic_Sources (Gtkada_Entry);

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
      Renderer : Gtk_Cell_Renderer_Text;
      Col      : Gtk_Tree_View_Column;
      Num      : Gint;
      pragma Unreferenced (Num, Kernel);

      Scrolled : Gtk_Scrolled_Window;
      Frame    : Gtk_Frame;
      List : Widget_List.Glist := Widget_List.Null_List;

   begin
      Self := new Gtkada_Entry_Record;

      Initialize_Vbox (Self, Homogeneous => False, Spacing => 5);
      Self.Case_Sensitive := Case_Sensitive;
      Self.Completion := GPS.Search.Search_Provider_Access (Completion);

      Gtk_New (Self.GEntry);
      Self.GEntry.Set_Activates_Default (True);
      Self.Pack_Start (Self.GEntry, Expand => False);

      --  ??? Should we set initial contents to be that of the history ?
      if History /= "" then
         Self.GEntry.Set_Text ("history");
         Self.GEntry.Select_Region (0, -1);
      end if;

      Self.GEntry.Set_Width_Chars (25);

      Gtk_New (Frame);
      Self.Pack_Start (Frame, Expand => True, Fill => True);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Frame.Add (Scrolled);

      Gtk_New (Self.View);
      Scrolled.Add (Self.View);
      Self.View.Get_Selection.Set_Mode (Selection_None);

      Gtk_New (Self.List, (0 => GType_String,    --  short
                           1 => GType_Pointer,   --  Search result
                           2 => GType_String));  --  long
      Self.View.Set_Model (+Self.List);

      Gtk_New (Renderer);

      Gtk_New (Col);
      Col.Set_Title ("Completions");
      Col.Set_Sort_Column_Id (0);
      Num := Self.View.Append_Column (Col);
      Col.Pack_Start (Renderer, False);
      Col.Add_Attribute (Renderer, "text", 0);

      Col.Clicked;

      Gtk_New (Col);
      Col.Set_Title ("");
      Col.Set_Sort_Column_Id (2);
      Num := Self.View.Append_Column (Col);
      Col.Pack_Start (Renderer, False);
      Col.Add_Attribute (Renderer, "text", 2);
      Col.Set_Visible (False);

      Widget_List.Append (List, Gtk_Widget (Self.GEntry));
      Widget_List.Append (List, Gtk_Widget (Frame));
      Self.Set_Focus_Chain (List);
      Widget_List.Free (List);

      Self.View.On_Button_Press_Event (On_Button_Press'Access, Self);
      Self.View.On_Key_Press_Event (On_Key_Press'Access, Self, After => True);
      Self.On_Destroy (On_Destroy'Access);
      Gtk.Editable.On_Changed (+Self.GEntry, On_Entry_Changed'Access, Self);
   end Gtk_New;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Self  : access GObject_Record'Class;
      Event : Gdk_Event_Button) return Boolean
   is
      GEntry : constant Gtkada_Entry := Gtkada_Entry (Self);
      Window : Gtk_Window;
   begin
      if Get_Mode (Get_Selection (GEntry.View)) = Selection_Single
        and then Event.The_Type = Gdk_2button_Press
      then
         Window := Gtk_Window (Get_Toplevel (GEntry));
         return Activate_Default (Window);
      end if;
      return False;
   end On_Button_Press;

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

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Self : access Gtk_Widget_Record'Class) is
      S : constant Gtkada_Entry := Gtkada_Entry (Self);
   begin
      if S.Idle /= No_Source_Id then
         Remove (S.Idle);
         S.Idle := No_Source_Id;
      end if;

      S.Clear;

      Free (S.Pattern);
      Free (S.Completion);
   end On_Destroy;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : access Gtkada_Entry_Record'Class) is
   begin
      Self.List.Clear;
      Self.Need_Clear := False;
      Set_Visible (Get_Column (Self.View, 1), False);
   end Clear;

   -------------
   -- On_Idle --
   -------------

   function On_Idle (Self : Gtkada_Entry) return Boolean is
      Has_Next : Boolean;
      Result   : Search_Result_Access;
      Iter     : Gtk_Tree_Iter;
      Start    : Time;
      Count : Natural := 0;
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
            Self.List.Append (Iter, Null_Iter);
            Self.List.Set (Iter, 0, Result.Short.all);
            Self.List.Set (Iter, 1, Result.all'Address);

            if Result.Long /= null then
               Self.List.Set (Iter, 2, Result.Long.all);
               Set_Visible (Get_Column (Self.View, 1), True);
            end if;
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
         Kind           => Fuzzy);
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
