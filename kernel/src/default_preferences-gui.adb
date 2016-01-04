------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

with Gtk.Box;                     use Gtk.Box;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Event_Box;               use Gtk.Event_Box;
with Gtk.Label;                   use Gtk.Label;
with Gtk.Style_Context;           use Gtk.Style_Context;

-----------------------------
-- Default_Preferences.GUI --
-----------------------------

package body Default_Preferences.GUI is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : not null access Preferences_Page_View_Record'Class) is
   begin
      Gtk.Scrolled_Window.Initialize (Self);
      Self.Set_Policy (Policy_Automatic, Policy_Automatic);
      Self.On_Destroy (On_Destroy_Page_View'Access);
      Get_Style_Context (Self).Add_Class
        ("gps-preferences-pages");
   end Initialize;

   --------------------------
   -- Set_Pref_Highlighted --
   --------------------------

   procedure Set_Pref_Highlighted
     (Self      : not null access Preferences_Page_View_Record;
      Pref      : not null access Preference_Record'Class;
      Highlight : Boolean)
   is
      Widget : Gtk_Widget;
   begin
      if Self.Prefs_Box = null then
         return;
      end if;

      --  Do nothing if the preference is not mapped
      if not Self.Prefs_Box.Pref_Widgets.Contains (Pref.Get_Name) then
         return;
      end if;

      Widget := Self.Prefs_Box.Pref_Widgets (Pref.Get_Name);

      if Highlight then
         Widget.Set_State_Flags (Gtk_State_Flag_Selected, False);
      else
         Widget.Set_State_Flags (Gtk_State_Flag_Normal, True);
      end if;
   end Set_Pref_Highlighted;

   --------------------------
   -- On_Destroy_Page_View --
   --------------------------

   procedure On_Destroy_Page_View
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Page_View : constant Preferences_Page_View :=
                    Preferences_Page_View (Widget);
   begin
      if Page_View.Prefs_Box /= null then
         Page_View.Prefs_Box.Pref_Widgets.Clear;
      end if;
   end On_Destroy_Page_View;

   -------------------
   -- Set_Prefs_Box --
   -------------------

   procedure Set_Prefs_Box
     (Self      : not null access Preferences_Page_View_Record'Class;
      Prefs_Box : not null access Preferences_Box_Record'Class) is
   begin
      Self.Prefs_Box := Preferences_Box (Prefs_Box);
   end Set_Prefs_Box;

   -------------------
   -- Get_Prefs_Box --
   -------------------

   function Get_Prefs_Box
     (Self      : not null access Preferences_Page_View_Record'Class)
      return Preferences_Box is (Self.Prefs_Box);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : not null access Preferences_Group_Widget_Record'Class;
      Group_Name  : String;
      Align       : Boolean := True) is
   begin
      Gtk.Frame.Initialize (Self, Group_Name);

      Get_Style_Context (Self).Add_Class
        ("gps-preferences-groups");

      Gtk_New (Self.List_Box);
      Self.List_Box.Set_Selection_Mode (Selection_None);
      Self.Add (Self.List_Box);

      if Align then
         Gtk_New (Self.Label_Size_Group);
         Gtk_New (Self.Pref_Widget_Size_Group);
      end if;
   end Initialize;

   ---------------------
   -- Create_Pref_Row --
   ---------------------

   function Create_Pref_Row
     (Self    : not null access Preferences_Group_Widget_Record'Class;
      Pref    : not null access Preference_Record'Class;
      Manager : not null access Preferences_Manager_Record'Class)
      return Gtk_List_Box_Row
   is
      Pref_Row    : Gtk_Box;
      Event       : Gtk_Event_Box;
      Label       : Gtk_Label;
      Pref_Widget : Gtk_Widget;
   begin
      Gtk_New_Hbox (Pref_Row);

      if Pref.Editor_Needs_Label then
         Gtk_New (Event);
         Gtk_New (Label, Pref.Get_Label);
         Event.Add (Label);
         Event.Set_Tooltip_Text (Pref.Get_Doc);
         Label.Set_Alignment (0.0, 0.5);

         Self.Label_Size_Group.Add_Widget (Event);
         Pref_Row.Pack_Start (Event);

         Pref_Widget := Edit (Pref, Manager);

         if Pref_Widget /= null then
            Pref_Widget.Set_Hexpand (False);
            Self.Pref_Widget_Size_Group.Add_Widget (Pref_Widget);
            Pref_Row.Pack_Start (Pref_Widget);
         end if;
      else
         Pref_Widget := Edit
           (Pref      => Pref,
            Manager   => Manager);
         Pref_Widget.Set_Tooltip_Text (Pref.Get_Doc);

         if Pref_Widget /= null then
            Pref_Widget.Set_Hexpand (False);
            Self.Pref_Widget_Size_Group.Add_Widget (Pref_Widget);
            Pref_Row.Pack_Start (Pref_Widget);
         end if;
      end if;
      Self.List_Box.Add (Pref_Row);

      return Gtk_List_Box_Row (Pref_Row.Get_Parent);
   end Create_Pref_Row;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self   : not null access Preferences_Group_Widget_Record'Class;
      Widget : not null Gtk_Widget) is
   begin
      Self.List_Box.Add (Widget);
   end Append;

   -----------
   -- Build --
   -----------

   procedure Build
     (Self    : not null access Preferences_Box_Record'Class;
      Page    : not null access Preferences_Page_Record'Class;
      Manager : not null access Preferences_Manager_Record'Class)
   is
      Group      : Preferences_Group;

      procedure Add_Group_Widget;
      --  Create and add the widget for Group

      ----------------------
      -- Add_Group_Widget --
      ----------------------

      procedure Add_Group_Widget is
         Group_Widget : Preferences_Group_Widget;
         Pref         : Preference;

         procedure Add_Pref_Widget;
         --  Create the widget for Pref and add it to Group_Widget

         ---------------------
         -- Add_Pref_Widget --
         ---------------------

         procedure Add_Pref_Widget
         is
            Pref_Row : Gtk_List_Box_Row;
         begin
            Pref_Row := Create_Pref_Row
              (Self    => Group_Widget,
               Pref    => Pref,
               Manager => Manager);

            Self.Pref_Widgets.Insert
              (Pref.Get_Name, Gtk_Widget (Pref_Row));
         end Add_Pref_Widget;

      begin
         Group_Widget := new Preferences_Group_Widget_Record;
         Group_Widget.Initialize (Group_Name => Group.Get_Name,
                                  Align      => True);

         Self.Pack_Start (Group_Widget, Expand => False);

         for Pref_Iter in Group.Preferences.Iterate loop
            Pref := Preferences_Lists.Element (Pref_Iter);

            Add_Pref_Widget;
         end loop;
      end Add_Group_Widget;

   begin
      Initialize_Vbox (Self);

      --  Iterate over all the groups registered in this page and append
      --  their widgets.
      for Group_Iter in Page.Groups.Iterate loop
         Group := Groups_Lists.Element (Group_Iter);
         Add_Group_Widget;
      end loop;
   end Build;

end Default_Preferences.GUI;
