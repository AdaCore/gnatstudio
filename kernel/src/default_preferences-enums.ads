------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2026, AdaCore                     --
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

with Ada.Containers.Ordered_Sets;
with VSS.Strings;             use VSS.Strings;
with VSS.String_Vectors;
with VSS.Transformers.Casing; use VSS.Transformers.Casing;

package Default_Preferences.Enums is

   type Choice_Preference_Record is new Preference_Record with private;
   type Choice_Preference is access all Choice_Preference_Record'Class;

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Path                      : Preference_Path;
      Name, Label, Doc          : String;
      Choices                   : VSS.String_Vectors.Virtual_String_Vector;
      Default                   : VSS.Strings.Virtual_String;
      Priority                  : Integer := -1;
      Combo_Threshold           : Integer := 3)
      return Choice_Preference;
   --  Create a new preference whose values are among Choices.
   --  Combo_Threshold can be set to -1 to force a combobox and not use radio
   --  buttons.

   function Enum_Value_To_Label (Value : String) return String;
   --  Return a suitable label for UI representation of a given enum value (the
   --  one retrieved with 'Image).

   generic
      type Enumeration is (<>);
   package Generics is
      type Preference_Record is new Enum_Preference_Record with private;
      type Preference is access all Preference_Record'Class;

      function Create
        (Manager                   : access Preferences_Manager_Record'Class;
         Path                      : Preference_Path;
         Name, Label, Doc          : String;
         Default                   : Enumeration;
         Priority                  : Integer := -1;
         Combo_Threshold           : Integer := 3)
         return Preference;
      --  Create a new preference whose possibles values are given by
      --  Enumeration

      function Get_Pref
        (Pref : access Preference_Record) return Enumeration;
      --  Return the value of the enumeration

      procedure Hide
        (Pref  : access Preference_Record;
         Value : Enumeration);
      --  The Value will be invisible in GUI for users

   private
      package Hidden_Set is new Ada.Containers.Ordered_Sets (Enumeration);

      type Preference_Record is new Enum_Preference_Record with record
         Hidden : Hidden_Set.Set;
      end record;

      overriding function Edit
        (Pref               : access Preference_Record;
         Manager            : access Preferences_Manager_Record'Class)
         return Gtk.Widget.Gtk_Widget;
      overriding function Get_Pref
        (Pref : access Preference_Record) return String;
      overriding procedure Set_Pref
        (Pref    : access Preference_Record;
         Manager : access Preferences_Manager_Record'Class;
         Value   : String);
      overriding procedure Update_On_Pref_Changed
        (Pref   : access Preference_Record;
         Widget : access GObject_Record'Class);
      overriding function Editor_Needs_Label
        (Pref : not null access Preference_Record) return Boolean;
   end Generics;

private
   type Choice_Preference_Record is new Preference_Record with record
      Current_Choice  : VSS.Strings.Virtual_String;
      Default_Choice  : VSS.Strings.Virtual_String;
      Choices         : VSS.String_Vectors.Virtual_String_Vector;
      Combo_Threshold : Integer;
   end record;
   overriding function Edit
     (Pref               : access Choice_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   overriding function Get_Pref
     (Pref : access Choice_Preference_Record) return String;
   overriding procedure Set_Pref
     (Pref    : access Choice_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Is_Default
     (Self : not null access Choice_Preference_Record) return Boolean
   is (To_Lowercase.Transform (Self.Current_Choice)
       = To_Lowercase.Transform (Self.Default_Choice));
   overriding procedure Free (Pref : in out Choice_Preference_Record);
   overriding procedure Update_On_Pref_Changed
     (Pref   : access Choice_Preference_Record;
      Widget : access GObject_Record'Class);
   overriding function Editor_Needs_Label
     (Pref : not null access Choice_Preference_Record) return Boolean;
end Default_Preferences.Enums;
