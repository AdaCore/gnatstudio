------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Case_Handling;            use Case_Handling;
with Gtk.Combo_Box;            use Gtk.Combo_Box;
with Glib.Object;              use Glib.Object;
with GUI_Utils;                use GUI_Utils;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GNAT.Strings;             use GNAT.Strings;

package body Default_Preferences.Enums is

   procedure Enum_Changed
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference);
   --  Called when an enumeration preference has been changed.

   procedure Update_Combo
     (Ent  : access GObject_Record'Class;
      Data : Manager_Preference);
   --  Called when the preference Data has changed, to update Ent

   ------------------
   -- Enum_Changed --
   ------------------

   procedure Enum_Changed
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference)
   is
      C     : constant Gtk_Combo_Box := Gtk_Combo_Box (Combo);
   begin
      Enum_Preference (Data.Pref).Enum_Value := Integer (Get_Active (C));
   end Enum_Changed;

   ------------------
   -- Update_Combo --
   ------------------

   procedure Update_Combo
     (Ent  : access GObject_Record'Class;
      Data : Manager_Preference) is
   begin
      Set_Active_Text (Gtk_Combo_Box (Ent), String'(Get_Pref (Data.Pref)));
   end Update_Combo;

   ------------
   -- Create --
   ------------

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Choices                   : GNAT.Strings.String_List_Access;
      Default                   : Integer)
      return Choice_Preference
   is
      Result : constant Choice_Preference := new Choice_Preference_Record;
   begin
      Result.Choices := Choices;
      Result.Enum_Value := Default;
      Register (Manager, Name, Label, Page, Doc, Result);
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
      Combo : Gtk_Combo_Box;
      Idx   : Gint := 0;

   begin
      Gtk_New_Text (Combo);

      for K in Pref.Choices'Range loop
         declare
            S : String := Pref.Choices (K).all;
         begin
            Mixed_Case (S);
            Combo.Append_Text (S);
         end;

         if K = Pref.Enum_Value + Pref.Choices'First then
            Combo.Set_Active (Idx);
         end if;

         Idx := Idx + 1;
      end loop;

      Preference_Handlers.Connect
        (Combo, Gtk.Combo_Box.Signal_Changed,
         Enum_Changed'Access,
         User_Data   => (Preferences_Manager (Manager), Preference (Pref)));
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Combo'Access,
         Combo,
         User_Data => (Preferences_Manager (Manager), Preference (Pref)));

      return Gtk.Widget.Gtk_Widget (Combo);
   end Edit;

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
            Emit_Pref_Changed (Manager);
            exit;
         end if;
      end loop;
   end Set_Pref;

   --------------
   -- Generics --
   --------------

   package body Generics is

      ------------
      -- Create --
      ------------

      function Create
        (Manager                   : access Preferences_Manager_Record'Class;
         Name, Label, Page, Doc    : String;
         Default                   : Enumeration)
         return Preference
      is
         Result : constant Preference := new Preference_Record;
      begin
         Enum_Preference (Result).Enum_Value := Enumeration'Pos (Default);
         Register (Manager, Name, Label, Page, Doc, Result);
         return Result;
      end Create;

      ----------
      -- Edit --
      ----------

      overriding function Edit
        (Pref               : access Preference_Record;
         Manager            : access Preferences_Manager_Record'Class)
         return Gtk.Widget.Gtk_Widget
      is
         V       : constant Integer := Enum_Preference (Pref).Enum_Value;
         Combo   : Gtk_Combo_Box;
         Idx     : Gint := 0;

      begin
         Gtk_New_Text (Combo);

         for K in Enumeration'Range loop
            declare
               S : String := Enumeration'Image (K);
            begin
               Mixed_Case (S);
               Combo.Append_Text (S);
            end;

            if Enumeration'Pos (K) = V then
               Combo.Set_Active (Idx);
            end if;

            Idx := Idx + 1;
         end loop;

         Preference_Handlers.Connect
           (Combo, Gtk.Combo_Box.Signal_Changed,
            Enum_Changed'Access,
            User_Data   => (Preferences_Manager (Manager),
                            Default_Preferences.Preference (Pref)));
         Preference_Handlers.Object_Connect
           (Manager.Pref_Editor, Signal_Preferences_Changed,
            Update_Combo'Access,
            Combo,
            User_Data => (Preferences_Manager (Manager),
                          Default_Preferences.Preference (Pref)));

         return Gtk.Widget.Gtk_Widget (Combo);
      end Edit;

      --------------
      -- Get_Pref --
      --------------

      overriding function Get_Pref
        (Pref : access Preference_Record) return String
      is
         S : String := Enumeration'Image
           (Enumeration'Val (Enum_Preference (Pref).Enum_Value));
      begin
         Mixed_Case (S);
         return S;
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
         Emit_Pref_Changed (Manager);

      exception
         when Constraint_Error =>
            --  Else we might have an integer representing the Pos
            Set_Pref (Enum_Preference_Record (Pref.all)'Access,
                      Manager, Value);
            Emit_Pref_Changed (Manager);
      end Set_Pref;
   end Generics;

end Default_Preferences.Enums;
