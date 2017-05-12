------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with Gtk.Menu;               use Gtk.Menu;
with Gtk.Widget;             use Gtk.Widget;

with Dialog_Utils;           use Dialog_Utils;

package Default_Preferences.GUI is

   type Preferences_Group_Widget_Record is new Dialog_Group_Widget_Record
   with private;
   type Preferences_Group_Widget is
     access all Preferences_Group_Widget_Record'Class;
   --  Type used to represent group widgets in the preferences dialog.

   type Preferences_Page_View_Record is new Dialog_View_Record
   with private;
   type Preferences_Page_View is access all Preferences_Page_View_Record'Class;
   --  Type defining a preferences dialog page view.
   --  This is used to define a common API for all the pages views of the
   --  preferences editor dialog.

   procedure Create_Menu
     (Self : not null access Preferences_Page_View_Record;
      Menu : not null access Gtk_Menu_Record'Class) is null;
   --  Called by the preferences dialog editor to build a unique local
   --  configuration menu.
   --  Override this function if the page needs a local configuration menu.

   function Needs_Apply_Button
     (Self : not null access Preferences_Page_View_Record) return Boolean
   is
     (False);
   --  Called to know if the editor should set the sensitivity of the 'Apply'
   --  button.
   --  Override this function if your page type needs an 'Apply' button.

   procedure On_Apply_Button_Clicked
     (Self : not null access Preferences_Page_View_Record) is null;
   --  Called when the 'Apply' button is clicked.

   ---------------------------
   -- Preferences_Page_View --
   ---------------------------

   procedure Set_Pref_Highlighted
     (Self      : not null access Preferences_Page_View_Record;
      Pref      : not null access Preference_Record'Class;
      Highlight : Boolean);
   --  If Highlight is True, Highlight the widget displaying the preference
   --  given in parameter.
   --  If not, unhighlight it.

   procedure Display_Subpage
     (Self         : not null access Preferences_Page_View_Record;
      Subpage_Name : String) is null;
   --  Display the subpage associated to Subpage_Name.
   --  Override this procedure if the subpages are displayed directly in the
   --  root page view (e.g: inside a list contained in the root page view).

   ------------------------------
   -- Preferences_Group_Widget --
   ------------------------------

   procedure Create_Pref_Row
     (Self      : not null access Preferences_Group_Widget_Record'Class;
      Pref      : not null access Preference_Record'Class;
      Manager   : not null access Preferences_Manager_Record'Class);
   --  Create a row for the given preference and append it at the bottom of
   --  the group widget.

   ---------------------
   -- Preferences_Box --
   ---------------------

   procedure Build
     (Self    : not null access Preferences_Page_View_Record'Class;
      Page    : not null access Preferences_Page_Record'Class;
      Manager : not null access Preferences_Manager_Record'Class);
   --  Build all the widgets for every group and every preference and add them
   --  to Self.

private

   type Preferences_Group_Widget_Record is new Dialog_Group_Widget_Record
   with null record;

   type Preferences_Page_View_Record is new Dialog_View_Record
   with null record;

end Default_Preferences.GUI;
