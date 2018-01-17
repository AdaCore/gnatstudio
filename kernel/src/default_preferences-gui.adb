------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Gtk.Enums;         use Gtk.Enums;

-----------------------------
-- Default_Preferences.GUI --
-----------------------------

package body Default_Preferences.GUI is

   --------------------------
   -- Set_Pref_Highlighted --
   --------------------------

   procedure Set_Pref_Highlighted
     (Self      : not null access Preferences_Page_View_Record;
      Pref      : not null access Preference_Record'Class;
      Highlight : Boolean) is
   begin
      Self.Set_Child_Highlighted (Child_Key => Pref.Get_Name,
                                  Highlight => Highlight);
   end Set_Pref_Highlighted;

   ---------------------
   -- Create_Pref_Row --
   ---------------------

   procedure Create_Pref_Row
     (Self      : not null access Preferences_Group_Widget_Record'Class;
      Pref      : not null access Preference_Record'Class;
      Manager   : not null access Preferences_Manager_Record'Class)
   is
      Doc         : constant String := Pref.Get_Doc;
      Label       : constant String := (if Pref.Editor_Needs_Label then
                                           Pref.Get_Label
                                        else
                                           "");
   begin
      Self.Create_Child (Widget    => Pref.Edit (Manager),
                         Label     => Label,
                         Doc       => Doc,
                         Child_Key => Pref.Get_Name,
                         Expand    => False);
   end Create_Pref_Row;

   -----------
   -- Build --
   -----------

   procedure Build
     (Self    : not null access Preferences_Page_View_Record'Class;
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
      begin
         Group_Widget := new Preferences_Group_Widget_Record;
         Group_Widget.Initialize
           (Group_Name  => Group.Get_Name,
            Parent_View => Self);

         for Pref_Iter in Group.Preferences.Iterate loop
            Pref :=
              Manager.Get_Pref_From_Name
                (Preferences_Names_Lists.Element (Pref_Iter));

            --  Create the row in the group widget for Pref
            Create_Pref_Row
              (Self    => Group_Widget,
               Pref    => Pref,
               Manager => Manager);
         end loop;
      end Add_Group_Widget;

   begin
      --  Iterate over all the groups registered in this page and append
      --  their widgets.
      for Group_Iter in Page.Groups.Iterate loop
         Group := Groups_Lists.Element (Group_Iter);
         Add_Group_Widget;
      end loop;
   end Build;

end Default_Preferences.GUI;
