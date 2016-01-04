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

--  This package defines types and subprograms used to create preferences
--  dialog pages and related widgets.

with Gtk.Frame;              use Gtk.Frame;
with Gtk.List_Box;           use Gtk.List_Box;
with Gtk.List_Box_Row;       use Gtk.List_Box_Row;
with Gtk.Size_Group;         use Gtk.Size_Group;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;
with Gtk.Widget;             use Gtk.Widget;

package Default_Preferences.GUI is

   type Preferences_Group_Widget_Record is new Gtk_Frame_Record with private;
   type Preferences_Group_Widget is
     access all Preferences_Group_Widget_Record'Class;
   --  Type used to represent group widgets in the preferences dialog.

   type Preferences_Page_View_Record is
     new Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with private;
   type Preferences_Page_View is access all Preferences_Page_View_Record'Class;
   --  Type defining a preferences dialog page view.
   --  This is used to define a common API for all the pages views of the
   --  preferences editor dialog.

   type Preferences_Box_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Preferences_Box is access all Preferences_Box_Record'Class;
   --  Type used to represent the parent container of all the preferences
   --  related widgets displayed on a Preferences_Page_View.

   ---------------------------
   -- Preferences_Page_View --
   ---------------------------

   procedure Initialize
     (Self : not null access Preferences_Page_View_Record'Class);
   --  Initialize the common attributes for all the Preferences_Page_Views.

   procedure Set_Pref_Highlighted
     (Self      : not null access Preferences_Page_View_Record;
      Pref      : not null access Preference_Record'Class;
      Highlight : Boolean);
   --  If Highlight is True, Highlight the widget displaying the preference
   --  given in parameter.
   --  If not, unhighlight it.

   procedure On_Destroy_Page_View
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Called when a preferences page view is destroyed.

   procedure Set_Prefs_Box
     (Self      : not null access Preferences_Page_View_Record'Class;
      Prefs_Box : not null access Preferences_Box_Record'Class);
   function Get_Prefs_Box
     (Self      : not null access Preferences_Page_View_Record'Class)
      return Preferences_Box;
   --  Setter and getter for the Prefs_Box attribute, which is the container
   --  containing all the preferences-related widgets for a given page view.

   procedure Display_Subpage
     (Self         : not null access Preferences_Page_View_Record;
      Subpage_Name : String) is null;
   --  Display the subpage associated to Subpage_Name.
   --  Override this procedure if the subpages are displayed directly in the
   --  root page view (e.g: inside a list contained in the root page view).

   ------------------------------
   -- Preferences_Group_Widget --
   ------------------------------

   procedure Initialize
     (Self        : not null access Preferences_Group_Widget_Record'Class;
      Group_Name  : String;
      Align       : Boolean := True);
   --  Initialize a Preferences_Group_Widget.
   --  Group_Name is used to set the frame's label.
   --  If Align is True, this group widget will align all the preferences when
   --  calling Create_Pref_Row. If Align is False, the preferences rows will
   --  not be aligned.

   function Create_Pref_Row
     (Self    : not null access Preferences_Group_Widget_Record'Class;
      Pref    : not null access Preference_Record'Class;
      Manager : not null access Preferences_Manager_Record'Class)
      return Gtk_List_Box_Row;
   --  Create a row for the given preference and append it at the bottom of
   --  the group widget.

   procedure Append
     (Self   : not null access Preferences_Group_Widget_Record'Class;
      Widget : not null Gtk_Widget);
   --  Append an already built widget to Self's Gtk_List_Box.

   ---------------------
   -- Preferences_Box --
   ---------------------

   procedure Build
     (Self    : not null access Preferences_Box_Record'Class;
      Page    : not null access Preferences_Page_Record'Class;
      Manager : not null access Preferences_Manager_Record'Class);
   --  Build all the widgets for every group and every preference and add them
   --  to Self.

private

   type Preferences_Group_Widget_Record is new Gtk_Frame_Record with record
      List_Box               : Gtk_List_Box;
      Label_Size_Group       : Gtk_Size_Group;
      Pref_Widget_Size_Group : Gtk_Size_Group;
   end record;

   type Preferences_Box_Record is new Gtk.Box.Gtk_Box_Record with record
      Pref_Widgets : Preferences_Widgets_Maps.Map;
      --  Used to map preferences with their highlightable parent widget.
   end record;

   type Preferences_Page_View_Record is
     new Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with record
      Prefs_Box : Preferences_Box;
      --  Container for all the preferences related widgets.
      --  This can be set to null if no preference is displayed on this page.
   end record;

end Default_Preferences.GUI;
