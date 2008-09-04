-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                Copyright (C) 2001-2008, AdaCore                   --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

package Default_Preferences.Enums is

   type Choice_Preference_Record is new Enum_Preference_Record with private;
   type Choice_Preference is access all Choice_Preference_Record'Class;

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Choices                   : GNAT.Strings.String_List_Access;
      Default                   : Integer)
      return Choice_Preference;
   --  Create a new preference whose values are among Choices.
   --  Choices will be freed when the preference itself is destroyed, and must
   --  not be destroyed by the caller

   generic
      type Enumeration is (<>);
   package Generics is
      type Preference_Record is new Enum_Preference_Record with null record;
      type Preference is access all Preference_Record'Class;

      function Create
        (Manager                   : access Preferences_Manager_Record'Class;
         Name, Label, Page, Doc    : String;
         Default                   : Enumeration)
         return Preference;
      --  Create a new preference whose possibles values are given by
      --  Enumeration

      function Get_Pref
        (Pref : access Preference_Record) return Enumeration;
      --  Return the value of the enumeration

   private
      overriding function Edit
        (Pref               : access Preference_Record;
         Manager            : access Preferences_Manager_Record'Class;
         Tips               : Gtk.Tooltips.Gtk_Tooltips)
         return Gtk.Widget.Gtk_Widget;
      overriding function Get_Pref
        (Pref : access Preference_Record) return String;
      overriding procedure Set_Pref
        (Pref    : access Preference_Record;
         Manager : access Preferences_Manager_Record'Class;
         Value   : String);
   end Generics;

private
   type Choice_Preference_Record is new Enum_Preference_Record with record
      Choices : GNAT.Strings.String_List_Access;
   end record;

   overriding function Edit
     (Pref               : access Choice_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget;
   overriding function Get_Pref
     (Pref : access Choice_Preference_Record) return String;
   overriding procedure Set_Pref
     (Pref    : access Choice_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding procedure Free (Pref : in out Choice_Preference_Record);

end Default_Preferences.Enums;
