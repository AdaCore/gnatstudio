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

with Case_Handling;            use Case_Handling;
with Gtk.Combo;                use Gtk.Combo;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.List_Item;            use Gtk.List_Item;
with Gtk.List;                 use Gtk.List;
with Glib.Object;              use Glib.Object;
with GUI_Utils;                use GUI_Utils;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GNAT.Strings;             use GNAT.Strings;
with GNATCOLL.Traces; use GNATCOLL.Traces;

package body Default_Preferences.Enums is
   Me : constant Trace_Handle := Create ("ENUMS");

   procedure Enum_Changed
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference);
   --  Called when an enumeration preference has been changed.

   procedure Update_Entry
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
      C     : constant Gtk_Combo := Gtk_Combo (Combo);
   begin
      Enum_Preference (Data.Pref).Enum_Value :=
        Integer (Get_Index_In_List (C));
      Trace (Me, "MANU New value="
             & Enum_Preference (Data.Pref).Enum_Value'Img);
      Trace (Me, "MANU New value=" & Get_Pref (Data.Pref));
   end Enum_Changed;

   ------------------
   -- Update_Entry --
   ------------------

   procedure Update_Entry
     (Ent  : access GObject_Record'Class;
      Data : Manager_Preference) is
   begin
      Set_Text (Gtk_Entry (Ent), String'(Get_Pref (Data.Pref)));
   end Update_Entry;

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
      Result.Enum_Value := Default - Choices'First;
      Register (Manager, Name, Label, Page, Doc, Result);
      return Result;
   end Create;

   ----------
   -- Free --
   ----------

   procedure Free (Pref : in out Choice_Preference_Record) is
   begin
      Free (Pref.Choices);
      Free (Preference_Record (Pref));
   end Free;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Choice_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Tips);
      Combo   : Gtk_Combo;
      Item    : Gtk_List_Item;
   begin
      Gtk_New (Combo);
      Set_Value_In_List (Combo, True, Ok_If_Empty => False);
      Set_Editable (Get_Entry (Combo), False);

      for K in Pref.Choices'Range loop
         declare
            S : String := Pref.Choices (K).all;
         begin
            Mixed_Case (S);
            Gtk_New (Item, S);
         end;

         Add (Get_List (Combo), Item);
         if K = Pref.Enum_Value then
            Set_Text (Get_Entry (Combo), Pref.Choices (K).all);
         end if;
         Show_All (Item);
      end loop;

      Preference_Handlers.Object_Connect
        (Get_List (Combo), "select_child",
         Enum_Changed'Access,
         Slot_Object => Combo,
         User_Data   => (Preferences_Manager (Manager), Preference (Pref)));
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Entry'Access,
         Get_Entry (Combo),
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
         Manager            : access Preferences_Manager_Record'Class;
         Tips               : Gtk.Tooltips.Gtk_Tooltips)
         return Gtk.Widget.Gtk_Widget
      is
         pragma Unreferenced (Tips);
         V       : constant Integer := Enum_Preference (Pref).Enum_Value;
         Combo   : Gtk_Combo;
         Item    : Gtk_List_Item;
      begin
         Gtk_New (Combo);
         Set_Value_In_List (Combo, True, Ok_If_Empty => False);
         Set_Editable (Get_Entry (Combo), False);

         for K in Enumeration'Range loop
            declare
               S : String := Enumeration'Image (K);
            begin
               Mixed_Case (S);
               Gtk_New (Item, S);
            end;

            Add (Get_List (Combo), Item);
            if Enumeration'Pos (K) = V then
               Set_Text (Get_Entry (Combo), Enumeration'Image (K));
            end if;
            Show_All (Item);
         end loop;

         Preference_Handlers.Object_Connect
           (Get_List (Combo), "select_child",
            Enum_Changed'Access,
            Slot_Object => Combo,
            User_Data   => (Preferences_Manager (Manager),
                            Default_Preferences.Preference (Pref)));
         Preference_Handlers.Object_Connect
           (Manager.Pref_Editor, Signal_Preferences_Changed,
            Update_Entry'Access,
            Get_Entry (Combo),
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
