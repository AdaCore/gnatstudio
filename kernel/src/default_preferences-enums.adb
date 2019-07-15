------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Strings.Maps;   use Ada.Strings.Maps;
with GNATCOLL.Utils;     use GNATCOLL.Utils;
with GNAT.Strings;       use GNAT.Strings;

with Case_Handling;      use Case_Handling;
with GUI_Utils;          use GUI_Utils;

with Gtk.Box;            use Gtk.Box;
with Gtk.Combo_Box;      use Gtk.Combo_Box;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Radio_Button;   use Gtk.Radio_Button;
with Gtk.Toggle_Button;
with Gtk.Widget;         use Gtk.Widget;
with Glib.Object;        use Glib.Object;

package body Default_Preferences.Enums is

   Needs_Combo_Threshold : constant := 3;
   --  Number of choices from which enum preferences are displayed using
   --  combo boxes instead of radio buttons.

   type Enum_Radio_Button_Record is new Gtk_Radio_Button_Record with record
      Enum_Value : Integer;
      --  The enumeration value associated with this radio button
   end record;
   type Enum_Radio_Button is access all Enum_Radio_Button_Record'Class;
   --  Type representing radio buttons associated with enumeration
   --  preferences.

   procedure Enum_Combo_Changed
     (Widget : access GObject_Record'Class;
      Data   : Manager_Preference);
   --  Called when an enumeration preference with a combo box has changed.

   procedure Enum_Radio_Changed
     (Widget : access GObject_Record'Class;
      Data   : Manager_Preference);
   --  Called when an enumeration preference with a radio button group
   --  has changed.

   function Create_Combo_Box
     (Pref    : not null access Enum_Preference_Record'Class;
      Manager : not null access Preferences_Manager_Record'Class;
      Choices : not null GNAT.Strings.String_List_Access)
      return Gtk_Combo_Box_Text;
   --  Create a combo box listing all the given choices, and updating the given
   --  pref when the selected choice changes.

   function Create_Enum_Radio_Buttons_Box
     (Pref    : not null access Enum_Preference_Record'Class;
      Manager : not null access Preferences_Manager_Record'Class;
      Choices : not null GNAT.Strings.String_List_Access)
      return Gtk_Box;
   --  Create a horizontal box containing radio buttons listing all the given
   --  choices, and updating the given pref when the selected radio button
   --  changes.

   function Choice_To_Label (Choice : String) return String;
   --  Return a label for UI representation of a given choice value

   ---------------------
   -- Choice_To_Label --
   ---------------------

   function Choice_To_Label (Choice : String) return String is
      Label : String := Choice;
   begin
      Ada.Strings.Fixed.Translate
        (Source  => Label,
         Mapping => To_Mapping
           (From => To_Sequence (To_Set (('_'))),
            To   => To_Sequence (To_Set ((' ')))));
      return Mixed_Case (Label);
   end Choice_To_Label;

   ----------------------
   -- Create_Combo_Box --
   ----------------------

   function Create_Combo_Box
     (Pref    : not null access Enum_Preference_Record'Class;
      Manager : not null access Preferences_Manager_Record'Class;
      Choices : not null GNAT.Strings.String_List_Access)
      return Gtk_Combo_Box_Text
   is
      I     : Gint := 0;
      Combo : Gtk_Combo_Box_Text;
   begin
      Gtk_New (Combo);

      for K in Choices'Range loop
         Combo.Append_Text (Choice_To_Label (Choices (K).all));

         if K = Pref.Enum_Value + Choices'First then
            Combo.Set_Active (I);
         end if;

         I := I + 1;
      end loop;

      Preference_Handlers.Connect
        (Combo, Gtk.Combo_Box.Signal_Changed,
         Enum_Combo_Changed'Access,
         User_Data   => (Preferences_Manager (Manager), Preference (Pref)));

      Set_GObject_To_Update (Pref, GObject (Combo));

      return Combo;
   end Create_Combo_Box;

   -----------------------------------
   -- Create_Enum_Radio_Buttons_Box --
   -----------------------------------

   function Create_Enum_Radio_Buttons_Box
     (Pref    : not null access Enum_Preference_Record'Class;
      Manager : not null access Preferences_Manager_Record'Class;
      Choices : not null GNAT.Strings.String_List_Access)
      return Gtk_Box
   is
      Radio_Box : Gtk_Box;
      Radio     : array (Choices'Range) of Enum_Radio_Button;
   begin
      Gtk_New_Hbox (Radio_Box, Homogeneous => False);

      for K in Choices'Range loop
         Radio (K) := new Enum_Radio_Button_Record;
         Initialize
           (Radio_Button => Gtk_Radio_Button (Radio (K)),
            Group        => Radio (Radio'First),
            Label        => Choice_To_Label (Choices (K).all));
         Radio (K).Enum_Value := K - Choices'First;
         Radio_Box.Pack_Start (Radio (K), Expand => False);

         Preference_Handlers.Connect
           (Radio (K), Gtk.Toggle_Button.Signal_Toggled,
            Enum_Radio_Changed'Access,
            User_Data =>
              (Preferences_Manager (Manager), Preference (Pref)));

         if K = Pref.Enum_Value + Choices'First then
            Radio (K).Set_Active (True);
         end if;
      end loop;

      return Radio_Box;
   end Create_Enum_Radio_Buttons_Box;

   ------------------------
   -- Enum_Combo_Changed --
   ------------------------

   procedure Enum_Combo_Changed
     (Widget : access GObject_Record'Class;
      Data   : Manager_Preference)
   is
      Combo : constant Gtk_Combo_Box_Text :=
                Gtk_Combo_Box_Text (Widget);
   begin
      Enum_Preference (Data.Pref).Enum_Value :=
        Integer (Get_Active (Combo));
      Data.Manager.Notify_Pref_Changed (Data.Pref);
   end Enum_Combo_Changed;

   ------------------------
   -- Enum_Radio_Changed --
   ------------------------

   procedure Enum_Radio_Changed
     (Widget : access GObject_Record'Class;
      Data   : Manager_Preference)
   is
      Radio : constant Enum_Radio_Button := Enum_Radio_Button (Widget);
   begin
      Enum_Preference (Data.Pref).Enum_Value := Radio.Enum_Value;
      Data.Manager.Notify_Pref_Changed (Data.Pref);
   end Enum_Radio_Changed;

   ------------
   -- Create --
   ------------

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Path                      : Preference_Path;
      Name, Label, Doc          : String;
      Choices                   : GNAT.Strings.String_List_Access;
      Default                   : Integer;
      Priority                  : Integer := -1)
      return Choice_Preference
   is
      Result : constant Choice_Preference := new Choice_Preference_Record;
   begin
      Result.Choices := Choices;
      Result.Enum_Value := Default;

      --  If the preference should be displayed with radio buttons, place them
      --  in a group named using the preference's label.
      if Choices'Length > Needs_Combo_Threshold then
         Manager.Register
           (Name     => Name,
            Path     => Path,
            Label    => Label,
            Doc      => Doc,
            Pref     => Result,
            Priority => Priority);
      else
         declare
            Page_Name       : Unbounded_String;
            Group_Name      : Unbounded_String;
            Registered_Page : Preferences_Page;
         begin
            Extract_Page_And_Group_Names (Path       => Path,
                                          Page_Name  => Page_Name,
                                          Group_Name => Group_Name);

            --  Get/create the corresponding preferences page and register
            --  the preference as a group, without forgetting to set the
            --  group's priority to the preference's priority since it will
            --  contain only this preference.

            Registered_Page := Manager.Get_Registered_Page
              (Name             => To_String (Page_Name),
               Create_If_Needed => True);
            Registered_Page.Register_Group
              (Name             => Label,
               Group            => new Preferences_Group_Record,
               Priority         => Priority);

            Manager.Register
              (Name     => Name,
               Path     => To_String (Page_Name) & ':' & Label,
               Label    => Label,
               Doc      => Doc,
               Pref     => Result,
               Priority => Priority);
         end;
      end if;
      return Result;
   end Create;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Pref : in out Choice_Preference_Record) is
   begin
      Free (Pref.Choices);
      Free (Preference_Record (Pref));
   end Free;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Choice_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
     (if Pref.Choices'Length > Needs_Combo_Threshold then
         Gtk_Widget (Create_Combo_Box (Pref    => Pref,
                                       Manager => Manager,
                                       Choices => Pref.Choices))
      else
         Gtk_Widget (Create_Enum_Radio_Buttons_Box (Pref    => Pref,
                                                    Manager => Manager,
                                                    Choices => Pref.Choices)));

   --------------
   -- Get_Pref --
   --------------

   overriding function Get_Pref
     (Pref : access Choice_Preference_Record) return String is
   begin
      return Pref.Choices (Pref.Enum_Value + Pref.Choices'First).all;
   exception
      when Constraint_Error =>
         return Pref.Choices (Pref.Choices'First).all;
   end Get_Pref;

   --------------
   -- Set_Pref --
   --------------

   overriding procedure Set_Pref
     (Pref    : access Choice_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      for C in Pref.Choices'Range loop
         if Equal (Pref.Choices (C).all, Value, Case_Sensitive => False) then
            Pref.Enum_Value := C - Pref.Choices'First;
            Manager.Notify_Pref_Changed (Pref);
            exit;
         end if;
      end loop;
   end Set_Pref;

   ----------------------------
   -- Update_On_Pref_Changed --
   ----------------------------

   overriding procedure Update_On_Pref_Changed
     (Pref   : access Choice_Preference_Record;
      Widget : access GObject_Record'Class) is
   begin
      Set_Active_Text
        (Gtk_Combo_Box_Text (Widget), String'(Get_Pref (Pref)));
   end Update_On_Pref_Changed;

   ------------------------
   -- Editor_Needs_Label --
   ------------------------

   overriding function Editor_Needs_Label
     (Pref : not null access Choice_Preference_Record) return Boolean
   is
     (Pref.Choices'Length > Needs_Combo_Threshold);

   --------------
   -- Generics --
   --------------

   package body Generics is

      ------------
      -- Create --
      ------------

      function Create
        (Manager                   : access Preferences_Manager_Record'Class;
         Path                      : Preference_Path;
         Name, Label, Doc          : String;
         Default                   : Enumeration;
         Priority                  : Integer := -1)
         return Preference
      is
         P      : constant Default_Preferences.Preference :=
                    Get_Pref_From_Name
                      (Manager, Name, Create_If_Necessary => False);
         Result : constant Preference := new Preference_Record;

      begin
         if P /= null then
            --  Might already have been created implicitly when loading the
            --  preferences file, but likely with the wrong type

            begin
               Enum_Preference (Result).Enum_Value :=
                 Enumeration'Pos
                   (Enumeration'Value (String'(Get_Pref (P))));
            exception
               when Constraint_Error =>
                  Enum_Preference (Result).Enum_Value :=
                    Enumeration'Pos (Default);
            end;

         else
            Enum_Preference (Result).Enum_Value := Enumeration'Pos (Default);
         end if;

         Enum_Preference (Result).Default := Enumeration'Pos (Default);

         --  If the preference should be displayed with radio buttons, place
         --  them in a group named using the preference's label.
         if Enumeration'Range_Length > Needs_Combo_Threshold then
            Manager.Register
              (Path     => Path,
               Name     => Name,
               Label    => Label,
               Doc      => Doc,
               Pref     => Result,
               Priority => Priority);
         else
            declare
               Page_Name       : Unbounded_String;
               Group_Name      : Unbounded_String;
               Registered_Page : Preferences_Page;
            begin
               Extract_Page_And_Group_Names (Path       => Path,
                                             Page_Name  => Page_Name,
                                             Group_Name => Group_Name);

               --  Get/create the corresponding preferences page and register
               --  the preference as a group, without forgetting to set the
               --  group's priority to the preference's priority since it will
               --  contain only this preference.

               Registered_Page := Manager.Get_Registered_Page
                 (Name             => To_String (Page_Name),
                  Create_If_Needed => True);
               Registered_Page.Register_Group
                 (Name             => Label,
                  Group            => new Preferences_Group_Record,
                  Priority         => Priority);

               Manager.Register
                 (Name     => Name,
                  Path     => To_String (Page_Name) & ':' & Label,
                  Label    => Label,
                  Doc      => Doc,
                  Pref     => Result,
                  Priority => Priority);
            end;
         end if;

         return Result;
      end Create;

      ----------
      -- Hide --
      ----------

      procedure Hide
        (Pref  : access Preference_Record;
         Value : Enumeration) is
      begin
         if not Pref.Hidden.Contains (Value) then
            Pref.Hidden.Insert (Value);
         end if;
      end Hide;

      ----------
      -- Edit --
      ----------

      overriding function Edit
        (Pref               : access Preference_Record;
         Manager            : access Preferences_Manager_Record'Class)
         return Gtk.Widget.Gtk_Widget
      is
         subtype Enumeration_Choices is
           GNAT.Strings.String_List
             (1 .. Enumeration'Range_Length - Natural (Pref.Hidden.Length));
         Widget  : Gtk_Widget;
         Choices : GNAT.Strings.String_List_Access := new Enumeration_Choices;

         procedure Build_Choices;
         --  Build a strings list of all the possible choices for this
         --  enum preference, using the Enumeration type possible values.

         -------------------
         -- Build_Choices --
         -------------------

         procedure Build_Choices is
            I : Integer := Choices'First;
         begin
            for K in Enumeration'Range loop
               if not Pref.Hidden.Contains (K) then
                  Choices (I) :=
                    new String'(Enumeration'Image (K));
                  I := I + 1;
               end if;
            end loop;
         end Build_Choices;

      begin
         Build_Choices;

         if Enumeration_Choices'Last > Needs_Combo_Threshold then
            Widget := Gtk_Widget
              (Create_Combo_Box (Pref    => Pref,
                                 Manager => Manager,
                                 Choices => Choices));
         else
            Widget := Gtk_Widget
              (Create_Enum_Radio_Buttons_Box (Pref    => Pref,
                                              Manager => Manager,
                                              Choices => Choices));
         end if;

         Free (Choices);

         return Widget;
      end Edit;

      ----------------------------
      -- Update_On_Pref_Changed --
      ----------------------------

      overriding procedure Update_On_Pref_Changed
        (Pref   : access Preference_Record;
         Widget : access GObject_Record'Class) is
      begin
         Set_Active_Text
           (Gtk_Combo_Box_Text (Widget), String'(Get_Pref (Pref)));
      end Update_On_Pref_Changed;

      --------------
      -- Get_Pref --
      --------------

      overriding function Get_Pref
        (Pref : access Preference_Record) return String
      is
         S : constant String := Enumeration'Image
           (Enumeration'Val (Enum_Preference (Pref).Enum_Value));
      begin
         return Mixed_Case (S);
      end Get_Pref;

      --------------
      -- Get_Pref --
      --------------

      function Get_Pref
        (Pref : access Preference_Record) return Enumeration is
      begin
         return Enumeration'Val (Enum_Preference (Pref).Enum_Value);
      end Get_Pref;

      --------------
      -- Set_Pref --
      --------------

      overriding procedure Set_Pref
        (Pref    : access Preference_Record;
         Manager : access Preferences_Manager_Record'Class;
         Value   : String) is
      begin
         --  Test if we have a string representation of the enumeration value
         Enum_Preference (Pref).Enum_Value :=
           Enumeration'Pos (Enumeration'Value (Value));
         Manager.Notify_Pref_Changed (Pref);

      exception
         when Constraint_Error =>
            --  Else we might have an integer representing the Pos
            Set_Pref (Enum_Preference_Record (Pref.all)'Access,
                      Manager, Value);
      end Set_Pref;

      ------------------------
      -- Editor_Needs_Label --
      ------------------------

      overriding function Editor_Needs_Label
        (Pref : not null access Preference_Record) return Boolean
      is
        (Enumeration'Range_Length > Needs_Combo_Threshold);

   end Generics;

end Default_Preferences.Enums;
