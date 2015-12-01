------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Strings;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.Utils;           use GNATCOLL.Utils;

with XML_Utils;                use XML_Utils;

with Gtk.Adjustment;           use Gtk.Adjustment;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Color_Button;         use Gtk.Color_Button;
with Gtk.Combo_Box;            use Gtk.Combo_Box;
with Gtk.Combo_Box_Text;       use Gtk.Combo_Box_Text;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Editable;             use Gtk.Editable;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Event_Box;            use Gtk.Event_Box;
with Gtk.Font_Selection;       use Gtk.Font_Selection;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Label;                use Gtk.Label;
with Gtk.List_Box;             use Gtk.List_Box;
with Gtk.Rc;                   use Gtk.Rc;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Size_Group;           use Gtk.Size_Group;
with Gtk.Spin_Button;          use Gtk.Spin_Button;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Style_Context;        use Gtk.Style_Context;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;

with Pango.Context;            use Pango.Context;
with Pango.Enums;              use Pango.Enums;
with Pango.Font_Family;        use Pango.Font_Family;

with Config;
with Defaults;
with GPS.Intl;                 use GPS.Intl;
with GUI_Utils;                use GUI_Utils;
with XML_Parsers;

-------------------------
-- Default_Preferences --
-------------------------

package body Default_Preferences is

   Me : constant Trace_Handle := Create ("Default_Prefs");

   ------------------
   -- Saved_Params --
   ------------------

   package Str_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, String, Ada.Strings.Hash, "=");
   use Str_Maps;

   -------------------------------
   -- Preferences_GObjects_Maps --
   -------------------------------

   Preferences_GObjects_Map : Preferences_GObjects_Maps.Map;
   --  Used to map preferences names with their main widget.
   --  This is done to update the prefereces widgets
   --  when preferences changed: when the Preferences_Changed_Hook runs,
   --  it will call the Update_On_Pref_Changed primitive for each preference
   --  with the widget they need to update by retrieving it from this map.

   -------------------------
   --  Preferences Editor --
   -------------------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Theme_Descr_Array, Theme_Descr_Array_Access);

   function Append_Dir_Delimitor_If_Needed
     (Page_Name : String) return String;
   --  Append a '/' delimitor to the page's name if needed.

   procedure Create_Color_Buttons
     (Pref            : access Style_Preference_Record'Class;
      Manager         : access Preferences_Manager_Record'Class;
      Fg_Color_Button : out Gtk_Color_Button;
      Bg_Color_Button : out Gtk_Color_Button);
   --  Factorize code that creates the color buttons

   procedure Free (Page : in out Preferences_Page);
   --  Free the memory associated with Page

   procedure Free (Pref : in out Preference);
   --  Free the memory associated with Pref

   function From_String (Color : String) return Gdk_RGBA;
   --  Parse the name of the color, and default to black if color is not found

   procedure Gint_Changed
     (Adj  : access GObject_Record'Class;
      Data : Manager_Preference);
   --  Called when a Gint preference has been changed.

   procedure Boolean_Changed
     (Toggle : access GObject_Record'Class;
      Data   : Manager_Preference);
   --  Called when a boolean preference has been changed.

   procedure Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Manager_Preference);
   --  Called when the text in an entry field has changed.

   procedure Combo_Changed
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference);
   --  Called when the combo_box changed

   function Font_Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Manager_Preference) return Boolean;
   --  Called when the entry for a font selection has changed.

   procedure Text_Buffer_Changed
     (Buffer : access GObject_Record'Class;
      Data   : Manager_Preference);
   --  Called when the buffer has changed, to update the preference

   procedure Reset_Font (Ent : access Gtk_Widget_Record'Class);
   --  Update the font used for the entry Ent, based on its contents.

   procedure Color_Changed
     (Button : access GObject_Record'Class;
      Data   : Manager_Preference);
   --  Called when a color has changed.

   procedure Bg_Color_Changed
     (Combo : access GObject_Record'Class; Data  : Manager_Preference);
   --  Called when the background color of a style has changed.

   procedure Fg_Color_Changed
     (Combo : access GObject_Record'Class; Data  : Manager_Preference);
   --  Called when the foreground color of a style has changed.

   procedure Variant_Changed
     (Combo : access GObject_Record'Class; Data  : Manager_Preference);
   --  Called when the font variant of a variant_preference has changed.

   procedure Select_Font
     (Ent : access GObject_Record'Class; Data : Manager_Preference);
   --  Open a dialog to select a new font

   function To_String (Font : String; Fg, Bg : Gdk_RGBA) return String;
   function Style_Token (Value : String; Num : Positive) return String;
   --  Handling of Param_Spec_Style

   function Create_Box_For_Font
     (Manager      : access Preferences_Manager_Record'Class;
      Pref         : Preference;
      Desc         : Pango_Font_Description;
      Button_Label : String) return My_Font_Box;
   --  Create a box suitable for editing fonts

   function From_Multi_String
     (M : String) return Pango.Font.Pango_Font_Description;
   --  Return a font matching M.
   --  M is a string containing a list of descriptions separated by commas,
   --  for instance "Consolas 10, Courier New 9, Courier 10".
   --  The first font that matches a registered family is returned.

   function Find_Group
     (Groups : in out Groups_Lists.List;
      Name   : String) return Preferences_Group;
   --  Find a group associated with Name in the given list.
   --  If no group is found, return null.

   procedure Insert_Pref
     (Preferences      : in out Preferences_Lists.List;
      Pref             : not null Preference;
      Replace_If_Exist : Boolean := False);
   --  Insert Pref at the end of the the given list.
   --  If Replace_If_Exist is True, delete the previous Pref associated with
   --  Name in the given list, and insert Pref instead. Otherwise, do nothing.

   -----------------------
   -- Group_Name_Equals --
   -----------------------

   function Group_Name_Equals (Left, Right : Preferences_Group) return Boolean
   is
     (Left.Get_Name = Right.Get_Name);

   ----------------------
   -- Pref_Name_Equals --
   ----------------------

   function Pref_Name_Equals (Left, Right : Preference) return Boolean
   is
      (Left.Get_Name = Right.Get_Name);

   ---------------
   -- Find_Group --
   ---------------

   function Find_Group
     (Groups : in out Groups_Lists.List;
      Name   : String) return Preferences_Group is
   begin
      for Group of Groups loop
         if Group.Get_Name = Name then
            return Group;
         end if;
      end loop;

      return null;
   end Find_Group;

   -----------------
   -- Insert_Pref --
   -----------------

   procedure Insert_Pref
     (Preferences      : in out Preferences_Lists.List;
      Pref             : not null Preference;
      Replace_If_Exist : Boolean := False)
   is
      Pref_Iter : Preferences_Lists.Cursor;
   begin
      Pref_Iter := Preferences.Find (Pref);

      if Preferences_Lists.Has_Element (Pref_Iter) then
         if Replace_If_Exist then
            Preferences.Delete (Pref_Iter);
         else
            return;
         end if;
      end if;

      Preferences.Append (Pref);
   end Insert_Pref;

   ---------------------------
   -- Set_GObject_To_Update --
   ---------------------------

   procedure Set_GObject_To_Update
     (Pref   : not null access Preference_Record;
      Obj    : not null access GObject_Record'Class) is
   begin
      if Preferences_GObjects_Map.Contains (Pref.Name.all) then
         Preferences_GObjects_Map (Pref.Name.all) := Obj;
      else
         Preferences_GObjects_Map.Insert (Pref.Name.all, Obj);
      end if;
   end Set_GObject_To_Update;

   ---------------------------
   -- Get_GObject_To_Update --
   ---------------------------

   function Get_GObject_To_Update
     (Pref : not null access Preference_Record) return GObject is
     (Preferences_GObjects_Map (Pref.Name.all));

   --------------------------------
   -- Pref_Has_Gobject_To_Update --
   --------------------------------

   function Has_GObject_To_Update
     (Pref : not null access Preference_Record) return Boolean is
     (not Preferences_GObjects_Map.Is_Empty
      and then Preferences_GObjects_Map.Contains (Pref.Name.all));

   -----------------------------------
   -- Remove_All_GObjects_To_Update --
   -----------------------------------

   procedure Remove_All_GObjects_To_Update is
   begin
      Preferences_GObjects_Map.Clear;
   end Remove_All_GObjects_To_Update;

   -----------------------
   -- From_Multi_String --
   -----------------------

   function From_Multi_String
     (M : String) return Pango.Font.Pango_Font_Description
   is
      Descs    : GNAT.Strings.String_List_Access := Split (M, ',');
      Result   : Pango_Font_Description;
      Context  : Pango_Context;

      T : Gtk_Text_View;
   begin
      --  We need to create this widget to access the list of available font
      --  families that apply to a text view.
      Gtk_New (T);
      Ref_Sink (T);

      Context := T.Get_Pango_Context;

      declare
         Families : constant Pango_Font_Family_Array := Context.List_Families;

         function Find (Family : String) return Boolean;
         --  Return True iff Family is in Families

         ----------
         -- Find --
         ----------

         function Find (Family : String) return Boolean is
            Lower : constant String := To_Lower (Family);
         begin
            for F in Families'Range loop
               if Lower = To_Lower (Families (F).Get_Name) then
                  return True;
               end if;
            end loop;
            return False;
         end Find;

      begin
         for J in Descs'Range loop
            Result := From_String (Descs (J).all);

            exit when Find (Pango.Font.Get_Family (Result));
         end loop;
      end;

      Free (Descs);
      Unref (T);

      return Result;
   end From_Multi_String;

   --------------
   -- Get_Name --
   --------------

   function Get_Name  (Pref : access Preference_Record'Class) return String is
   begin
      return Pref.Name.all;
   end Get_Name;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Pref : access Preference_Record'Class) return String is
   begin
      return Pref.Label.all;
   end Get_Label;

   --------------
   -- Get_Path --
   --------------

   function Get_Path  (Pref : access Preference_Record'Class) return String is
     (if Pref.Path /= null then Pref.Path.all else "");

   ------------------------------------
   -- Append_Dir_Delimitor_If_Needed --
   ------------------------------------

   function Append_Dir_Delimitor_If_Needed
     (Page_Name : String) return String is
   begin
      if Page_Name /= "" and then Page_Name (Page_Name'Last) /= '/' then
         return Page_Name & '/';
      end if;

      return Page_Name;
   end Append_Dir_Delimitor_If_Needed;

   -------------------
   -- Get_Page_Name --
   -------------------

   function Get_Page_Name (Pref : not null Preference) return String
   is
      Full_Page_Name : constant String := Pref.Get_Path;
      Current_Index  : Integer := Full_Page_Name'First;
   begin
      --  Find the delimitor for group names
      while Current_Index <= Full_Page_Name'Last
        and then Full_Page_Name (Current_Index) /= ':' loop
         Current_Index := Current_Index + 1;
      end loop;

      --  No group specified in the preference name
      if Current_Index >= Full_Page_Name'Last then
         return Append_Dir_Delimitor_If_Needed (Full_Page_Name);
      end if;

      return Append_Dir_Delimitor_If_Needed
        (Full_Page_Name (Full_Page_Name'First .. Current_Index - 1));
   end Get_Page_Name;

   --------------------
   -- Get_Group_Name --
   --------------------

   function Get_Group_Name (Pref : not null Preference) return String is
      Full_Page_Name : constant String := Pref.Get_Path;
      Current_Index  : Integer := Full_Page_Name'First;
   begin
      --  Find the delimitor for group names
      while Current_Index <= Full_Page_Name'Last
        and then Full_Page_Name (Current_Index) /= ':' loop
         Current_Index := Current_Index + 1;
      end loop;

      --  No group specified in the preference name
      if Current_Index >= Full_Page_Name'Last then
         return "";
      end if;

      return Full_Page_Name (Current_Index + 1 .. Full_Page_Name'Last);
   end Get_Group_Name;

   -------------
   -- Get_Doc --
   -------------

   function Get_Doc (Pref : access Preference_Record'Class) return String is
   begin
      return Pref.Doc.all;
   end Get_Doc;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Manager : in out Preferences_Manager_Record) is
      C : Pages_Lists.Cursor := Manager.Pages.First;
      Page : Preferences_Page;
   begin
      while Pages_Lists.Has_Element (C) loop
         Page := Pages_Lists.Element (C);
         Free (Page);
         Pages_Lists.Next (C);
      end loop;

      Manager.Pages.Clear;
   end Destroy;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Preferences_Page) return Boolean is
   begin
      if Left.Priority = Right.Priority then
         if Left.Name /= null and then Right.Name /= null then
            return Left.Name.all < Right.Name.all;
         else
            return Left /= null;
         end if;
      else
         return Left.Priority < Right.Priority;
      end if;
   end "<";

   ----------
   -- Free --
   ----------

   procedure Free (Pref : in out Preference)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Preference_Record'Class, Preference);
   begin
      if Pref /= null then
         Free (Pref.all);
         Unchecked_Free (Pref);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Page : in out Preferences_Page)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Preferences_Page_Record'Class, Preferences_Page);
   begin
      if Page /= null then
         Free (Page.all);
         Unchecked_Free (Page);
      end if;
   end Free;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Manager : in out Preferences_Manager) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Preferences_Manager_Record'Class, Preferences_Manager);
   begin
      Destroy (Manager.all);
      Unchecked_Free (Manager);
   end Destroy;

   --------------------------
   -- Set_Pref_Highlighted --
   --------------------------

   procedure Set_Pref_Highlighted
     (Self      : not null access Preferences_Page_View_Record'Class;
      Pref      : not null Preference;
      Highlight : Boolean)
   is
      Widget : Gtk_Widget;
   begin
      --  Do nothing if the preference is not mapped
      if not Self.Pref_Widgets.Contains (Pref.Get_Name) then
         return;
      end if;

      Widget := Self.Pref_Widgets (Pref.Get_Name);

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
     (Page_View : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Preferences_Page_View (Page_View).Pref_Widgets.Clear;
   end On_Destroy_Page_View;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Self : not null access Preferences_Group_Record) return String is
     (if Self.Name = null then "" else Self.Name.all);

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Self : not null access Preferences_Page_Record) return String is
     (if Self.Name = null then "" else Self.Name.all);

   ----------------
   -- Get_Widget --
   ----------------

   overriding function Get_Widget
     (Self    : not null access Default_Preferences_Page_Record;
      Manager : not null Preferences_Manager)
      return Preferences_Page_View
   is
      Page_View : Preferences_Page_View;
      Page_Box  : Gtk_Box;
      Group     : Preferences_Group;

      procedure Add_Group_Widget;

      ----------------------
      -- Add_Group_Widget --
      ----------------------

      procedure Add_Group_Widget is
         Group_Widget           : Gtk_Frame;
         Pref_List_Box          : Gtk_List_Box;
         Label_Size_Group       : Gtk_Size_Group;
         Pref_Widget_Size_Group : Gtk_Size_Group;
         Pref                   : Preference;
         procedure Add_Pref_Widget;

         ---------------------
         -- Add_Pref_Widget --
         ---------------------

         procedure Add_Pref_Widget is
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

               Label_Size_Group.Add_Widget (Event);
               Pref_Row.Pack_Start (Event);

               Pref_Widget := Edit (Pref, Manager);

               if Pref_Widget /= null then
                  Pref_Widget.Set_Hexpand (False);
                  Pref_Widget_Size_Group.Add_Widget (Pref_Widget);
                  Pref_Row.Pack_Start (Pref_Widget);
               end if;
            else
               Pref_Widget := Edit
                 (Pref      => Pref,
                  Manager   => Manager);
               Pref_Widget.Set_Tooltip_Text (Pref.Get_Doc);

               if Pref_Widget /= null then
                  Pref_Widget.Set_Hexpand (False);
                  Pref_Widget_Size_Group.Add_Widget (Pref_Widget);
                  Pref_Row.Pack_Start (Pref_Widget);
               end if;
            end if;
            Pref_List_Box.Add (Pref_Row);
            Page_View.Pref_Widgets.Insert
              (Pref.Get_Name, Pref_Row.Get_Parent);
         end Add_Pref_Widget;

      begin
         Gtk_New (Group_Widget);
         Page_Box.Pack_Start (Group_Widget);
         Get_Style_Context (Group_Widget).Add_Class
           ("gps-preferences-groups");

         Gtk_New (Pref_List_Box);
         Pref_List_Box.Set_Selection_Mode (Selection_None);
         Gtk_New (Label_Size_Group);
         Gtk_New (Pref_Widget_Size_Group);
         Group_Widget.Add (Pref_List_Box);

         if Group.Name /= null then
            Group_Widget.Set_Label (Group.Name.all);
         end if;

         --  Iterate over all the preferences registered in this group and
         --  append their widgets.
         for Pref_Iter in Group.Preferences.Iterate loop
            Pref := Preferences_Lists.Element (Pref_Iter);
            Add_Pref_Widget;
         end loop;
      end Add_Group_Widget;

   begin
      --  Create a new page
      Page_View := new Preferences_Page_View_Record;
      Gtk.Scrolled_Window.Initialize (Page_View);
      Page_View.Set_Policy (Policy_Automatic, Policy_Automatic);
      Page_View.On_Destroy (On_Destroy_Page_View'Access);

      --  Create the new Vbox which will hold the preferences
      --  groups
      Gtk_New_Vbox (Page_Box);
      Page_View.Add (Page_Box);
      Get_Style_Context (Page_Box).Add_Class
        ("gps-preferences-pages");

      --  Iterate over all the groups registered in this page and append
      --  their widgets.
      for Group_Iter in Self.Groups.Iterate loop
         Group := Groups_Lists.Element (Group_Iter);
         Add_Group_Widget;
      end loop;

      return Page_View;
   end Get_Widget;

   --------------
   -- Add_Pref --
   --------------

   procedure Add_Pref
     (Self : not null access Preferences_Page_Record;
      Pref : not null Preference)
   is
      Group_Name : constant String := Get_Group_Name (Pref);
      Group      : Preferences_Group;
   begin
      --  Find or create the group that should contain the preference
      Group := Find_Group (Groups => Self.Groups,
                           Name   => Group_Name);

      if Group = null then
         Group := new Preferences_Group_Record;

         if Group_Name /= "" then
            Group.Name := new String'(Group_Name);
         end if;

         Self.Groups.Append (Group);
      end if;

      --  If a preference is already associated to this name, replace it.
      --  If not, insert it to the preferences map.
      Insert_Pref (Preferences      => Group.Preferences,
                   Pref             => Pref,
                   Replace_If_Exist => True);
   end Add_Pref;

   -----------------
   -- Remove_Pref --
   -----------------

   procedure Remove_Pref
     (Self : not null access Preferences_Page_Record;
      Pref : not null Preference)
   is
      Group : constant Preferences_Group
        := Find_Group (Groups => Self.Groups,
                       Name   => Get_Group_Name (Pref));
      Pref_Iter : Preferences_Lists.Cursor := Group.Preferences.Find (Pref);
   begin
      if Preferences_Lists.Has_Element (Pref_Iter) then
         Group.Preferences.Delete (Pref_Iter);
      end if;
   end Remove_Pref;

   ----------
   -- Free --
   ----------

   procedure Free
     (Self : in out Preferences_Page_Record)
   is
      Group : Preferences_Group;
      Pref  : Preference;
   begin
      for Group_iter in Self.Groups.Iterate loop
         Group := Groups_Lists.Element (Group_iter);

         for Pref_Iter in Group.Preferences.Iterate loop
            Pref := Preferences_Lists.Element (Pref_Iter);
            Free (Pref);
         end loop;

         Preferences_Lists.Clear (Group.Preferences);
         Free (Group.Name);
      end loop;

      Groups_Lists.Clear (Self.Groups);
   end Free;

   ------------
   -- Create --
   ------------

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Minimum, Maximum, Default : Integer)
      return Integer_Preference
   is
      Pref : Preference :=
        Get_Pref_From_Name (Manager, Name, Create_If_Necessary => False);
      Val : Integer;
   begin
      if Pref /= null
        and then Pref.all in String_Preference_Record'Class
      then
         Val := Integer'Value (String_Preference (Pref).Get_Pref);
         Pref := new Integer_Preference_Record;
         Integer_Preference (Pref).Int_Value := Val;
      elsif Pref = null
        or else Pref.all not in Integer_Preference_Record'Class
      then
         Pref := new Integer_Preference_Record;
         Integer_Preference (Pref).Int_Value := Default;
      end if;

      Integer_Preference (Pref).Default := Default;
      Integer_Preference (Pref).Int_Min_Value := Minimum;
      Integer_Preference (Pref).Int_Max_Value := Maximum;
      Register (Manager, Name, Label, Page, Doc, Pref);
      return Integer_Preference (Pref);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Default                   : Boolean)
      return Boolean_Preference
   is
      Pref : Preference :=
        Get_Pref_From_Name (Manager, Name, Create_If_Necessary => False);
      Val : Boolean;
   begin
      --  Preference might have been created from loading the XML files before
      --  we actually registered it.
      if Pref = null
        or else Pref.all not in Boolean_Preference_Record'Class
      then
         if Pref /= null
           and then Pref.all in String_Preference_Record'Class
         then
            Val := Boolean'Value (String_Preference (Pref).Get_Pref);
         else
            Val := Default;
         end if;

         Pref := new Boolean_Preference_Record;
         Boolean_Preference (Pref).Bool_Value := Val;
      end if;

      Boolean_Preference (Pref).Default := Default;
      Register (Manager, Name, Label, Page, Doc, Pref);
      return Boolean_Preference (Pref);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Default                   : String;
      Multi_Line                : Boolean := False)
      return String_Preference
   is
      Pref : Preference :=
        Get_Pref_From_Name (Manager, Name, Create_If_Necessary => False);
   begin
      if Pref = null
        or else Pref.all not in String_Preference_Record'Class
      then
         Pref := new String_Preference_Record;
         String_Preference (Pref).Str_Value := new String'(Default);
      end if;

      if String_Preference (Pref).Default = null then
         String_Preference (Pref).Default := new String'(Default);
      end if;

      String_Preference (Pref).Multi_Line := Multi_Line;
      Register (Manager, Name, Label, Page, Doc, Pref);  --  override previous
      return String_Preference (Pref);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Default                   : String)
      return Color_Preference
   is
      Pref : Preference :=
               Get_Pref_From_Name
                 (Manager, Name, Create_If_Necessary => False);
      Val  : Gdk_RGBA;
   begin
      if Pref /= null
        and then Pref.all in String_Preference_Record'Class
      then
         Val := From_String (String_Preference (Pref).Get_Pref);
         Pref := new Color_Preference_Record;
         Color_Preference (Pref).Color := Val;
      elsif Pref = null
        or else Pref.all not in Color_Preference_Record'Class
      then
         Pref := new Color_Preference_Record;
         Color_Preference (Pref).Color := From_String (Default);
      end if;

      Color_Preference (Pref).Default := From_String (Default);
      Register (Manager, Name, Label, Page, Doc, Pref);
      return Color_Preference (Pref);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Default                   : String)
      return Font_Preference
   is
      Pref : Preference :=
               Get_Pref_From_Name
                 (Manager, Name, Create_If_Necessary => False);
      Val  : Pango_Font_Description;
   begin
      if Pref /= null
        and then Pref.all in String_Preference_Record'Class
      then
         Val := From_String (String_Preference (Pref).Get_Pref);
         Pref := new Font_Preference_Record;
         Font_Preference (Pref).Descr := Val;
      elsif Pref = null
        or else Pref.all not in Font_Preference_Record'Class
      then
         Pref := new Font_Preference_Record;
         Font_Preference (Pref).Descr := From_Multi_String (Default);
      end if;

      Font_Preference (Pref).Default := From_Multi_String (Default);
      Register (Manager, Name, Label, Page, Doc, Pref);
      return Font_Preference (Pref);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Default_Font              : String;
      Default_Fg                : String;
      Default_Bg                : String)
      return Style_Preference
   is
      Result : constant Style_Preference := new Style_Preference_Record;
   begin
      Result.Fg_Color := From_String (Default_Fg);
      Result.Fg_Default := Result.Fg_Color;

      Result.Bg_Color := From_String (Default_Bg);
      Result.Bg_Default := Result.Bg_Color;

      Result.Font_Descr := From_Multi_String (Default_Font);
      Result.Font_Default := Copy (Result.Font_Descr);

      Register (Manager, Name, Label, Page, Doc, Result);
      return Result;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Base                      : Style_Preference;
      Default_Variant           : Variant_Enum;
      Default_Fg                : String;
      Default_Bg                : String)
      return Variant_Preference
   is
      Result : constant Variant_Preference := new Variant_Preference_Record;
   begin
      Result.Fg_Color := From_String (Default_Fg);
      Result.Fg_Default := Result.Fg_Color;

      Result.Bg_Color := From_String (Default_Bg);
      Result.Bg_Default := Result.Bg_Color;

      Result.Variant := Default_Variant;
      Result.Default_Variant := Default_Variant;
      Result.Base_Font := Base;

      Register (Manager, Name, Label, Page, Doc, Result);
      return Result;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Manager                : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc : String)
      return Theme_Preference
   is
      use GNAT.OS_Lib;
      Ret         : constant Theme_Preference := new Theme_Preference_Record;
      Search_Path : constant Filesystem_String :=
                      (Get_Home_Directory.Full_Name.all &
                                     Directory_Separator & ".themes")
                    & Path_Separator
                      & (+Gtk.Rc.Get_Theme_Dir);

      --  Do not attempt to use the system default for gtk+. On most systems,
      --  it will be Raleigh because gtk+ is not standard. On linux, since we
      --  are using our own gtk+, chances are that the user's theme is not
      --  available with our own gtk+ anyway.
      Default     : constant String := "<unknown>";
      --                        Glib.Properties.Get_Property
      --                          (Gtk.Settings.Get_Default,
      --                           Gtk.Settings.Gtk_Theme_Name_Property);

      Dirs        : constant File_Array := From_Path (Search_Path);
      Dir         : GNATCOLL.VFS.Virtual_File;
      Subdirs     : GNATCOLL.VFS.File_Array_Access;
      Rc_File     : Virtual_File;
      use type Config.Host_Type;

      procedure Add_Theme (Name : String; Dark : Boolean);
      --  Register a new theme

      procedure Add_Theme (Name : String; Dark : Boolean) is
         Tmp : Theme_Descr_Array_Access := Ret.Themes;
      begin
         if Tmp = null then
            Ret.Themes := new Theme_Descr_Array (1 .. 1);
         else
            --  There are themes defined already: check that we are not
            --  adding a theme twice

            for J in Ret.Themes'Range loop
               if Ret.Themes (J).Name.all = Name
                 and then Ret.Themes (J).Dark = Dark
               then
                  --  This theme is already registered, do nothing
                  return;
               end if;
            end loop;

            Ret.Themes := new Theme_Descr_Array (Tmp'First .. Tmp'Last + 1);
            Ret.Themes (Tmp'Range) := Tmp.all;
            Unchecked_Free (Tmp);
         end if;

         if Dark then
            Ret.Themes (Ret.Themes'Last) :=
              (Name      => new String'(Name & " (Dark)"),
               Directory => new String'(Name),
               Dark      => Dark);
         else
            Ret.Themes (Ret.Themes'Last) :=
              (Name      => new String'(Name),
               Directory => new String'(Name),
               Dark      => Dark);
         end if;

         if not Dark
           and then
             (Default = Ret.Themes (Ret.Themes'Last).Name.all
              or else (Ret.Current = Natural'Last
                       and then
                       Ret.Themes (Ret.Themes'Last).Name.all = "Adwaita"))
         then
            Ret.Current := Ret.Themes'Last;
         end if;
      end Add_Theme;

   begin
      if Active (Me) then
         Trace (Me, "Theme search path is " & (+Search_Path));
         Trace (Me, "System's default theme is " & Default);
      end if;

      Ret.Current := Natural'Last;
      Ret.Themes  := null;

      for D in Dirs'Range loop
         Dir := Dirs (D);

         if Dir.Is_Directory then
            Subdirs := Dir.Read_Dir (Dirs_Only);

            for Subdir of Subdirs.all loop
               Rc_File := Subdir.Create_From_Dir ("gtk-3.0/gtk.css");

               if Rc_File.Is_Regular_File then
                  Add_Theme (+Base_Dir_Name (Subdir), Dark => False);
               end if;

               --  Check for a "dark" variant. We cannot unfortunately guess
               --  the possible variants of a theme just by looking at the
               --  file names, since many themes for instance provide a
               --  "gtk-widgets.css", where "widgets" is not a variant. In any
               --  case, the only variant supported by GSettings is "dark"...

               Rc_File := Subdir.Create_From_Dir ("gtk-3.0/gtk-dark.css");

               if Rc_File.Is_Regular_File then
                  Add_Theme (+Base_Dir_Name (Subdir), Dark => True);
               end if;
            end loop;

            Unchecked_Free (Subdirs);

         else
            if Active (Me) then
               Trace (Me, "Theme search path not found on disk: "
                      & Dir.Display_Full_Name);
            end if;
         end if;
      end loop;

      --  The 'gtk-win32' and 'Raleigh' themes are now directly embedded inside
      --  the gtk library, so exist event without any directory in the themes
      --  directories. We thus need to add them manually to the list of
      --  available themes.
      if Config.Host = Config.Windows then
         Add_Theme ("gtk-win32", Dark => False);
         Add_Theme ("gtk-win32-xp", Dark => False);
         Add_Theme ("gtk-win32-classic", Dark => False);
      end if;

      --  Adwaita and Raleigh are builtin themes
      Add_Theme ("Adwaita", Dark => False);
      Add_Theme ("Adwaita", Dark => True);

      Add_Theme ("Raleigh", Dark => False);

      if Ret.Current = Natural'Last then
         --  Should not happen
         Ret.Current := Ret.Themes'First;
      end if;

      Register (Manager, Name, Label, Page, Doc, Ret);

      return Ret;
   end Create;

   ------------------------
   -- Get_Pref_From_Name --
   ------------------------

   function Get_Pref_From_Name
     (Self                : not null access Preferences_Manager_Record;
      Name                : String;
      Create_If_Necessary : Boolean) return Preference is
   begin
      if Self.Preferences.Contains (Name) then
         return Self.Preferences (Name);
      end if;

      --  If Create_If_Necessary is True, return a temporary preference and
      --  and don't register it.
      if Create_If_Necessary then
         return Preference
           (String_Preference'
              (Create
                   (Manager => Self,
                    Name    => Name,
                    Label   => Name,
                    Page    => "",
                    Doc     => "",
                    Default => "")));
      else
         return null;
      end if;
   end Get_Pref_From_Name;

   -------------------
   -- Register_Page --
   -------------------

   procedure Register_Page
     (Self             : not null access Preferences_Manager_Record;
      Name             : String;
      Page             : not null Preferences_Page;
      Priority         : Integer := -1;
      Replace_If_Exist : Boolean := False)
   is
      use Pages_Lists;
      Page_Iter : Pages_Lists.Cursor;
   begin
      Free (Page.Name);
      Page.Name := new String'(Name);
      Page.Priority := Priority;

      Page_Iter := Self.Pages.Find (Page);

      if Page_Iter /= Pages_Lists.No_Element then
         if Replace_If_Exist then
            Self.Pages.Delete (Page_Iter);
         else
            return;
         end if;
      end if;

      Self.Pages.Append (Page);
   end Register_Page;

   -------------------------
   -- Get_Registered_Page --
   -------------------------

   function Get_Registered_Page
     (Self : not null access Preferences_Manager_Record;
      Name : String) return Preferences_Page
   is
      Page : Preferences_Page;
   begin
      for Page_Iter in Self.Pages.Iterate loop
         Page := Pages_Lists.Element (Page_Iter);

         if Page.Name /= null and then Page.Name.all = Name then
            return Page;
         end if;
      end loop;

      return null;
   end Get_Registered_Page;

   --------------
   -- Register --
   --------------

   procedure Register
     (Manager                : not null access Preferences_Manager_Record;
      Name, Label, Path, Doc : String;
      Pref                   : not null access Preference_Record'Class)
   is
      Old_Pref        : Preference :=
                          Manager.Get_Pref_From_Name (Name, False);
      Old_Page        : Preferences_Page;
      Registered_Page : Preferences_Page;
   begin
      --  If the preference was already in the list, remove the old value.
      --  It was probably inserted when reading the preferences file, which is
      --  in no specific order. Instead, we want to preserve the order based
      --  on the actual registration of preferences by the various modules, so
      --  that the preferences dialog is always displayed in the same order.
      if Old_Pref /= null then
         Old_Page := Get_Registered_Page (Manager, Get_Page_Name (Old_Pref));
         Old_Page.Remove_Pref (Old_Pref);
         Preferences_Maps.Delete (Manager.Preferences, Old_Pref.Get_Name);

         if Pref /= Old_Pref then
            Free (Old_Pref);
         end if;
      end if;

      --  Set the fields of the preference we want to register
      Free (Pref.Name);
      Pref.Name := new String'(Name);

      Free (Pref.Label);
      Pref.Label := new String'(Label);

      Free (Pref.Path);
      if Path /= "" and then Path /= "/" then
         Pref.Path := new String'(Path);
      end if;

      Free (Pref.Doc);
      Pref.Doc := new String'(Doc);

      --  Register the preference in the manager's global map
      Manager.Preferences.Insert (Name, Pref);

      --  Check if a page has already been registered for the preference's
      --  page name
      Registered_Page := Get_Registered_Page (Manager, Get_Page_Name (Pref));

      if Registered_Page = null then
         Registered_Page := new Default_Preferences_Page_Record;
         Manager.Register_Page (Get_Page_Name (Pref), Registered_Page);
      end if;

      --  Add the preference to the already/newly registered page
      Registered_Page.Add_Pref (Pref);
   end Register;

   --------------
   -- Get_Pref --
   --------------

   overriding function Get_Pref
     (Pref : access Integer_Preference_Record) return String is
   begin
      return Integer'Image (Pref.Int_Value);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Pref : access Integer_Preference_Record) return Integer is
   begin
      return Pref.Int_Value;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   overriding function Get_Pref
     (Pref : access Boolean_Preference_Record) return String is
   begin
      return Boolean'Image (Pref.Bool_Value);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Pref : access Boolean_Preference_Record) return Boolean is
   begin
      return Pref.Bool_Value;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   overriding function Get_Pref
     (Pref : access String_Preference_Record) return String is
   begin
      return Pref.Str_Value.all;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   overriding function Get_Pref
     (Pref : access Color_Preference_Record) return String is
   begin
      return To_String (Pref.Color);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Pref : access Color_Preference_Record) return Gdk.RGBA.Gdk_RGBA is
   begin
      return Pref.Color;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   overriding function Get_Pref
     (Pref : access Enum_Preference_Record) return String is
   begin
      return Integer'Image (Pref.Enum_Value);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Pref : access Enum_Preference_Record) return Integer is
   begin
      return Pref.Enum_Value;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   overriding function Get_Pref
     (Pref : access Font_Preference_Record) return String is
   begin
      return To_String (Pref.Descr);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Pref    : access Font_Preference_Record)
      return Pango.Font.Pango_Font_Description is
   begin
      return Pref.Descr;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   overriding function Get_Pref
     (Pref : access Style_Preference_Record) return String is
   begin
      return To_String
        (To_String (Pref.Font_Descr), Pref.Fg_Color, Pref.Bg_Color);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   overriding function Get_Pref
     (Pref : access Variant_Preference_Record) return String is
   begin
      return To_String
        (Pref.Variant'Img, Pref.Fg_Color, Pref.Bg_Color);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   overriding function Get_Pref
     (Pref : access Theme_Preference_Record) return String is
   begin
      if Pref = null
        or else Pref.Themes = null
        or else Pref.Current not in Pref.Themes'Range
      then
         return "";
      else
         return Pref.Themes (Pref.Current).Name.all;
      end if;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Pref : access Theme_Preference_Record) return Theme_Descr is
   begin
      if Pref = null
        or else Pref.Themes = null
        or else Pref.Current not in Pref.Themes'Range
      then
         return (null, null, False);
      else
         return Pref.Themes (Pref.Current);
      end if;
   end Get_Pref;

   -------------------
   -- Get_Pref_Font --
   -------------------

   function Get_Pref_Font
     (Pref : access Style_Preference_Record) return Pango_Font_Description is
   begin
      return Pref.Font_Descr;
   end Get_Pref_Font;

   -------------------
   -- Get_Pref_Font --
   -------------------

   overriding function Get_Pref_Font
     (Pref     : access Variant_Preference_Record)
      return Pango.Font.Pango_Font_Description
   is
   begin
      if Pref.Font_Descr /= null then
         Free (Pref.Font_Descr);
      end if;

      Pref.Font_Descr := Copy (Pref.Base_Font.Get_Pref_Font);

      case Pref.Variant is
         when Default =>
            null;
         when Normal =>
            Set_Weight (Pref.Font_Descr, Pango_Weight_Normal);
            Set_Style (Pref.Font_Descr, Pango_Style_Normal);
         when Bold =>
            Set_Weight (Pref.Font_Descr, Pango_Weight_Bold);
            Set_Style (Pref.Font_Descr, Pango_Style_Normal);
         when Italic =>
            Set_Weight (Pref.Font_Descr, Pango_Weight_Normal);
            Set_Style (Pref.Font_Descr, Pango_Style_Italic);
         when Bold_Italic =>
            Set_Weight (Pref.Font_Descr, Pango_Weight_Bold);
            Set_Style (Pref.Font_Descr, Pango_Style_Italic);
      end case;

      return Pref.Font_Descr;
   end Get_Pref_Font;

   ----------------------
   -- Get_Pref_Variant --
   ----------------------

   function Get_Pref_Variant
     (Pref     : access Variant_Preference_Record)
      return Variant_Enum is
   begin
      return Pref.Variant;
   end Get_Pref_Variant;

   -----------------------
   -- Get_Pref_Fg_Color --
   -----------------------

   function Get_Pref_Fg_Color
     (Pref     : access Variant_Preference_Record)
      return Gdk.RGBA.Gdk_RGBA is
   begin
      return Pref.Fg_Color;
   end Get_Pref_Fg_Color;

   -----------------------
   -- Get_Pref_Bg_Color --
   -----------------------

   function Get_Pref_Bg_Color
     (Pref     : access Variant_Preference_Record)
      return Gdk.RGBA.Gdk_RGBA is
   begin
      return Pref.Bg_Color;
   end Get_Pref_Bg_Color;

   -----------------
   -- Get_Pref_Fg --
   -----------------

   function Get_Pref_Fg
     (Pref : access Style_Preference_Record'Class)
      return Gdk.RGBA.Gdk_RGBA is
   begin
      return Pref.Fg_Color;
   end Get_Pref_Fg;

   -----------------
   -- Get_Pref_Bg --
   -----------------

   function Get_Pref_Bg
     (Pref : access Style_Preference_Record'Class)
      return Gdk.RGBA.Gdk_RGBA is
   begin
      return Pref.Bg_Color;
   end Get_Pref_Bg;

   --------------
   -- Set_Pref --
   --------------

   overriding procedure Set_Pref
     (Pref    : access Integer_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Pref.Int_Value := Integer'Value (Value);
      Manager.Notify_Pref_Changed (Pref);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Pref    : Integer_Preference;
      Manager : access Preferences_Manager_Record'Class;
      Value   : Integer) is
   begin
      Pref.Int_Value := Value;
      Manager.Notify_Pref_Changed (Pref);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   overriding procedure Set_Pref
     (Pref    : access Boolean_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Pref.Bool_Value := Boolean'Value (Value);
      Manager.Notify_Pref_Changed (Pref);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Pref    : Boolean_Preference;
      Manager : access Preferences_Manager_Record'Class;
      Value   : Boolean) is
   begin
      Pref.Bool_Value := Value;
      Manager.Notify_Pref_Changed (Pref);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   overriding procedure Set_Pref
     (Pref    : access String_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Free (Pref.Str_Value);
      Pref.Str_Value := new String'(Value);
      Manager.Notify_Pref_Changed (Pref);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   overriding procedure Set_Pref
     (Pref    : access Color_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Pref.Color := From_String (Value);
      Manager.Notify_Pref_Changed (Pref);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   overriding procedure Set_Pref
     (Pref    : access Font_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Free (Pref.Descr);
      Pref.Descr := From_String (Value);
      Manager.Notify_Pref_Changed (Pref);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   overriding procedure Set_Pref
     (Pref    : access Style_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Set_Pref (Style_Preference (Pref), Manager,
                Font => Style_Token (Value, 1),
                Fg   => Style_Token (Value, 2),
                Bg   => Style_Token (Value, 3));
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   overriding procedure Set_Pref
     (Pref    : access Variant_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Set_Pref (Variant_Preference (Pref), Manager,
                Variant => Variant_Enum'Value (Style_Token (Value, 1)),
                Fg      => Style_Token (Value, 2),
                Bg      => Style_Token (Value, 3));
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   overriding procedure Set_Pref
     (Pref    : access Theme_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      if Pref.Themes = null then
         return;
      end if;

      for J in Pref.Themes'Range loop
         if Pref.Themes (J).Name.all = Value then
            Pref.Current := J;
            Manager.Notify_Pref_Changed (Pref);
            return;
         end if;
      end loop;
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Pref         : Style_Preference;
      Manager      : access Preferences_Manager_Record'Class;
      Font, Fg, Bg : String) is
   begin
      Free (Pref.Font_Descr);

      Pref.Fg_Color := From_String (Fg);
      Pref.Bg_Color := From_String (Bg);
      Pref.Font_Descr := From_String (Font);

      Manager.Notify_Pref_Changed (Pref);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Pref         : Font_Preference;
      Manager      : access Preferences_Manager_Record'Class;
      Font         : Pango_Font_Description) is
   begin
      Free (Pref.Descr);
      Pref.Descr := Copy (Font);
      Manager.Notify_Pref_Changed (Pref);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Pref         : Variant_Preference;
      Manager      : access Preferences_Manager_Record'Class;
      Variant      : Variant_Enum;
      Fg, Bg       : String) is
   begin
      Free (Pref.Font_Descr);
      Pref.Fg_Color := From_String (Fg);
      Pref.Bg_Color := From_String (Bg);
      Pref.Variant    := Variant;

      Manager.Notify_Pref_Changed (Pref);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   overriding procedure Set_Pref
     (Pref    : access Enum_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Pref.Enum_Value := Integer'Value (Value);
      Manager.Notify_Pref_Changed (Pref);
   end Set_Pref;

   ----------------------
   -- Load_Preferences --
   ----------------------

   procedure Load_Preferences
     (Manager   : access  Preferences_Manager_Record;
      File_Name : Virtual_File)
   is
      File, Node : Node_Ptr;
      Err        : String_Access;
      Old_Prefs_File : Virtual_File;
      Ign        : Boolean;
   begin
      --  Attempt to import the "preferences" file
      if not Is_Regular_File (File_Name) then
         Old_Prefs_File := Create_From_Dir (File_Name.Dir, "preferences");

         if Is_Regular_File (Old_Prefs_File) then
            GNATCOLL.VFS.Copy (Old_Prefs_File, File_Name.Full_Name, Ign);
         end if;
      end if;

      if Is_Regular_File (File_Name) then
         Manager.Loading_Prefs := True;
         XML_Parsers.Parse (File_Name, File, Err);
         if File /= null then
            Node := File.Child;
            if File.Tag.all = "Preferences" then
               Trace (Me, "Load old style preferences");
               --  ??? Would be nice to save a copy of the preferences file
               --  e.g. ~/.gps/preferences.bak for downward compatibility

               while Node /= null loop
                  Set_Pref
                    (Manager => Manager,
                     Pref  => Get_Pref_From_Name (Manager, Node.Tag.all, True),
                     Value => Node.Value.all);
                  Node := Node.Next;
               end loop;
            else
               Trace (Me, "Load new style preferences from " &
                      File_Name.Display_Full_Name);
               while Node /= null loop
                  if Node.Tag.all = "pref" then
                     declare
                        Name : constant String := Get_Attribute (Node, "name");
                     begin
                        Set_Pref
                          (Manager => Manager,
                           Pref    => Get_Pref_From_Name (Manager, Name, True),
                           Value   => Node.Value.all);
                     end;
                  end if;
                  Node := Node.Next;
               end loop;
            end if;

         else
            Trace (Me, "Error while parsing preferences file " & Err.all);
            Free (Err);
         end if;

         Free (File);
         Manager.Loading_Prefs := False;
      end if;

   exception
      when E : others =>
         Manager.Loading_Prefs := False;
         Trace (Me, E);
   end Load_Preferences;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences
     (Manager   : access Preferences_Manager_Record;
      File_Name : Virtual_File;
      Success   : out Boolean)
   is
      File, Node  : Node_Ptr;
      Pref        : Preference;
   begin
      File := new XML_Utils.Node;
      File.Tag := new String'("Prefs");

      for Pref_Iter in Manager.Preferences.Iterate loop
         Pref := Preferences_Maps.Element (Pref_Iter);

         if not Pref.Is_Default then
            Node     := new XML_Utils.Node;
            Node.Tag := new String'("pref");
            Set_Attribute (Node, "name", Get_Name (Pref));
            Node.Value := new String'(Get_Pref (Pref));
            Add_Child (File, Node);
         end if;
      end loop;

      Print (File, File_Name, Success);
   exception
      when E : others => Trace (Me, E);
   end Save_Preferences;

   -----------------
   -- From_String --
   -----------------

   function From_String (Color : String) return Gdk_RGBA is
      Success : Boolean;
      Result  : Gdk_RGBA;
   begin
      Parse (Result, Color, Success);
      if not Success then
         return Black_RGBA;
      end if;
      return Result;
   end From_String;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Integer_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Spin : Gtk_Spin_Button;
      Adj  : Gtk_Adjustment;
      P    : constant Manager_Preference :=
               (Preferences_Manager (Manager), Preference (Pref));
   begin
      Gtk_New (Adj,
               Value => Gdouble (Pref.Int_Value),
               Lower => Gdouble (Pref.Int_Min_Value),
               Upper => Gdouble (Pref.Int_Max_Value),
               Step_Increment => 1.0,
               Page_Increment => 10.0);
      Gtk_New (Spin, Adj, 1.0, The_Digits => 0);
      Spin.Set_Editable (True);

      Preference_Handlers.Connect
        (Adj, Gtk.Adjustment.Signal_Value_Changed, Gint_Changed'Access, P);

      Set_GObject_To_Update (Pref, GObject (Adj));

      return Gtk_Widget (Spin);
   end Edit;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Boolean_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Toggle : Gtk_Check_Button;
      P      : constant Manager_Preference :=
                 (Preferences_Manager (Manager), Preference (Pref));
   begin
      Gtk_New (Toggle, Pref.Get_Label);
      Toggle.Set_Active (Pref.Bool_Value);

      Preference_Handlers.Connect
        (Toggle, Gtk.Toggle_Button.Signal_Toggled, Boolean_Changed'Access, P);

      Set_GObject_To_Update (Pref, GObject (Toggle));

      return Gtk_Widget (Toggle);
   end Edit;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access String_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Ent      : Gtk_Entry;
      Text     : Gtk_Text_View;
      Scrolled : Gtk_Scrolled_Window;
      P        : constant Manager_Preference :=
                   (Preferences_Manager (Manager), Preference (Pref));
   begin
      if Pref.Multi_Line then
         Gtk_New (Scrolled);
         Set_Size_Request (Scrolled, -1, 200);
         Gtk_New (Text);
         Add (Scrolled, Text);
         Set_Text (Get_Buffer (Text), Pref.Str_Value.all);
         Preference_Handlers.Connect
           (Get_Buffer (Text), "changed", Text_Buffer_Changed'Access, P);

         Set_GObject_To_Update (Pref, GObject (Scrolled));

         return Gtk.Widget.Gtk_Widget (Scrolled);
      else
         Gtk_New (Ent);
         Set_Text (Ent, Pref.Str_Value.all);

         Preference_Handlers.Connect
           (Ent, Gtk.Editable.Signal_Insert_Text,
            Entry_Changed'Access, P, After => True);
         Preference_Handlers.Connect
           (Ent, Signal_Delete_Text,
            Entry_Changed'Access, P, After => True);

         Set_GObject_To_Update (Pref, GObject (Ent));

         return Gtk_Widget (Ent);
      end if;
   end Edit;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Color_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Box    : Gtk_Box;
      Button : Gtk_Color_Button;
      P      : constant Manager_Preference :=
                 (Preferences_Manager (Manager), Preference (Pref));
   begin
      Gtk_New_Hbox (Box, Homogeneous => False);

      Gtk_New_With_Rgba (Button, Get_Pref (Color_Preference (Pref)));
      Button.Set_Use_Alpha (True);
      Box.Pack_Start (Button, Expand => False, Fill => False);

      Preference_Handlers.Connect
        (Button, Signal_Color_Set, Color_Changed'Access, P);

      Set_GObject_To_Update (Pref, GObject (Button));

      return Gtk_Widget (Box);
   end Edit;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Font_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Font_Box : My_Font_Box;
   begin
      Font_Box := Create_Box_For_Font
        (Manager, Preference (Pref),
         Get_Pref (Font_Preference (Pref)), -"...");

      Set_Tooltip_Text
        (Gtk_Widget (Font_Box), -"Click on ... to display the font selector");

      Set_GObject_To_Update (Pref, GObject (Font_Box));

      return Gtk_Widget (Font_Box);
   end Edit;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Style_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Event     : Gtk_Event_Box;
      Style_Box : My_Style_Box;
      Font_Box  : constant My_Font_Box :=
                    Create_Box_For_Font
                      (Manager, Preference (Pref),
                       Get_Pref_Font (Style_Preference (Pref)), "...");
   begin
      Gtk_New (Event);
      Add (Event, Font_Box);
      Set_Tooltip_Text (Event, -"Click on ... to display the font selector");
      Style_Box := new My_Style_Box_Record;
      Initialize_Hbox (Style_Box, Homogeneous => False);
      Pack_Start (Gtk_Box (Style_Box), Event, Expand => True, Fill => True);

      Style_Box.Font_Box := Font_Box;

      Create_Color_Buttons (Pref            => Pref,
                            Manager         => Manager,
                            Fg_Color_Button => Style_Box.Fg_Color_Button,
                            Bg_Color_Button => Style_Box.Bg_Color_Button);
      Pack_Start (Style_Box, Style_Box.Fg_Color_Button, Expand => False);
      Pack_Start (Style_Box, Style_Box.Bg_Color_Button, Expand => False);

      Set_GObject_To_Update (Pref, GObject (Style_Box));

      return Gtk_Widget (Style_Box);
   end Edit;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Variant_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Variant_Box   : My_Variant_Box;
      Variant_Combo : Gtk_Combo_Box_Text;
      Count         : Gint := 0;
      P             : constant Manager_Preference :=
                        (Preferences_Manager (Manager), Preference (Pref));
   begin
      Gtk_New (Variant_Combo);
      for J in Variant_Enum loop
         Append_Text (Variant_Combo, To_String (J));
         if J = Pref.Variant then
            Set_Active (Variant_Combo, Count);
         end if;
         Count := Count + 1;
      end loop;

      Set_Tooltip_Text (Variant_Combo, -"Font variant");
      Variant_Box := new My_Variant_Box_Record;
      Initialize_Hbox (Variant_Box, Homogeneous => False);
      Pack_Start
        (Gtk_Box (Variant_Box), Variant_Combo, Expand => True, Fill => True);
      Preference_Handlers.Connect
        (Variant_Combo, Gtk.Combo_Box.Signal_Changed,
         Variant_Changed'Access, P);
      Variant_Box.Combo := Variant_Combo;

      Create_Color_Buttons (Pref            => Pref,
                            Manager         => Manager,
                            Fg_Color_Button => Variant_Box.Fg_Color_Button,
                            Bg_Color_Button => Variant_Box.Bg_Color_Button);
      Pack_Start (Variant_Box, Variant_Box.Fg_Color_Button, Expand => False);
      Pack_Start (Variant_Box, Variant_Box.Bg_Color_Button, Expand => False);

      Set_GObject_To_Update (Pref, GObject (Variant_Box));

      return Gtk_Widget (Variant_Box);
   end Edit;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref      : access Theme_Preference_Record;
      Manager   : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      P           : constant Manager_Preference :=
                      (Preferences_Manager (Manager), Preference (Pref));
      Theme_Combo : Gtk_Combo_Box_Text;
   begin
      Gtk_New (Theme_Combo);
      Set_Tooltip_Text (Theme_Combo, -"Theme list");

      for J in Pref.Themes'Range loop
         Append_Text (Theme_Combo, Pref.Themes (J).Name.all);

         if J = Pref.Current then
            Theme_Combo.Set_Active (Gint (J) - Gint (Pref.Themes'First));
         end if;
      end loop;

      Preference_Handlers.Connect
        (Theme_Combo, Gtk.Combo_Box.Signal_Changed, Combo_Changed'Access, P);

      Set_GObject_To_Update (Pref, GObject (Theme_Combo));

      return Gtk_Widget (Theme_Combo);
   end Edit;

   ----------------------------
   -- Update_On_Pref_Changed --
   ----------------------------

   overriding procedure Update_On_Pref_Changed
     (Pref   : access Variant_Preference_Record;
      Widget : access GObject_Record'Class) is
      Variant_Box : constant My_Variant_Box := My_Variant_Box (Widget);
      Count       : Gint := 0;
      Old, Val    : Gdk_RGBA;
   begin
      Variant_Box.Fg_Color_Button.Get_Rgba (Old);
      Val := Get_Pref_Fg (Style_Preference (Pref));

      if Old /= Val then
         Variant_Box.Fg_Color_Button.Set_Rgba (Val);
      end if;

      Variant_Box.Bg_Color_Button.Get_Rgba (Old);
      Val := Get_Pref_Bg (Style_Preference (Pref));

      if Old /= Val then
         Variant_Box.Bg_Color_Button.Set_Rgba (Val);
      end if;

      for J in Variant_Enum loop
         if J = Pref.Variant then
            Variant_Box.Combo.Set_Active (Count);
            exit;
         end if;
         Count := Count + 1;
      end loop;
   end Update_On_Pref_Changed;

   ----------------------------
   -- Update_On_Pref_Changed --
   ----------------------------

   overriding procedure Update_On_Pref_Changed
     (Pref   : access Color_Preference_Record;
      Widget : access GObject_Record'Class)
   is
      R, Old  : Gdk_RGBA;
      Success : Boolean;
   begin
      Parse (R, Get_Pref (Pref), Success);

      if Success then
         Gtk_Color_Button (Widget).Get_Rgba (Old);
         if Old /= R then
            Set_Rgba (Gtk_Color_Button (Widget), R);
         end if;
      end if;
   end Update_On_Pref_Changed;

   ----------------------------
   -- Update_On_Pref_Changed --
   ----------------------------

   overriding procedure Update_On_Pref_Changed
     (Pref   : access Integer_Preference_Record;
      Widget : access GObject_Record'Class) is
   begin
      Set_Value
        (Gtk_Adjustment (Widget),
         Gdouble (Integer_Preference (Pref).Int_Value));
   end Update_On_Pref_Changed;

   ----------------------------
   -- Update_On_Pref_Changed --
   ----------------------------

   overriding procedure Update_On_Pref_Changed
     (Pref   : access Boolean_Preference_Record;
      Widget : access GObject_Record'Class) is
   begin
      Set_Active (Gtk_Toggle_Button (Widget),
                  Boolean_Preference (Pref).Bool_Value);
   end Update_On_Pref_Changed;

   ----------------------------
   -- Update_On_Pref_Changed --
   ----------------------------

   overriding procedure Update_On_Pref_Changed
     (Pref   : access String_Preference_Record;
      Widget : access GObject_Record'Class) is
   begin
      if Widget.all in Gtk_Entry_Record'Class then
         Set_Text (Gtk_Entry (Widget), String'(Get_Pref (Pref)));
      else
         Set_Text (Gtk_Text_Buffer (Widget), String'(Get_Pref (Pref)));
      end if;
   end Update_On_Pref_Changed;

   ----------------------------
   -- Update_On_Pref_Changed --
   ----------------------------

   overriding procedure Update_On_Pref_Changed
     (Pref   : access Style_Preference_Record;
      Widget : access GObject_Record'Class)
   is
      Style_Box : constant My_Style_Box := My_Style_Box (Widget);
      Old, Val  : Gdk_RGBA;
   begin
      Set_Text (Style_Box.Font_Box.Ent,
                To_String (Pref.Font_Descr));

      Style_Box.Fg_Color_Button.Get_Rgba (Old);
      Val := Get_Pref_Fg (Style_Preference (Pref));

      if Old /= Val then
         Style_Box.Fg_Color_Button.Set_Rgba (Val);
      end if;

      Style_Box.Bg_Color_Button.Get_Rgba (Old);
      Val := Get_Pref_Bg (Style_Preference (Pref));

      if Old /= Val then
         Style_Box.Bg_Color_Button.Set_Rgba (Val);
      end if;
   end Update_On_Pref_Changed;

   ----------------------------
   -- Update_On_Pref_Changed --
   ----------------------------

   overriding procedure Update_On_Pref_Changed
     (Pref   : access Font_Preference_Record;
      Widget : access GObject_Record'Class) is
   begin
      Set_Text (My_Font_Box (Widget).Ent,
                To_String (Pref.Descr));
   end Update_On_Pref_Changed;

   ----------------------------
   -- Update_On_Pref_Changed --
   ----------------------------

   overriding procedure Update_On_Pref_Changed
     (Pref   : access Theme_Preference_Record;
      Widget : access GObject_Record'Class) is
   begin
      Set_Active_Text
        (Gtk_Combo_Box_Text (Widget), String'(Get_Pref (Pref)));
   end Update_On_Pref_Changed;

   ----------
   -- Free --
   ----------

   procedure Free (Pref : in out Preference_Record) is
   begin
      Free (Pref.Name);
      Free (Pref.Label);
      Free (Pref.Path);
      Free (Pref.Doc);
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Pref : in out String_Preference_Record) is
   begin
      Free (Pref.Str_Value);
      Free (Pref.Default);
      Free (Preference_Record (Pref));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Pref : in out Font_Preference_Record) is
   begin
      Free (Pref.Default);
      Free (Pref.Descr);
      Free (Preference_Record (Pref));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Pref : in out Variant_Preference_Record) is
   begin
      Free (Pref.Font_Descr);
      Free (Preference_Record (Pref));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Pref : in out Style_Preference_Record) is
   begin
      Free (Pref.Font_Descr);
      Free (Pref.Font_Default);
      Free (Preference_Record (Pref));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Pref : in out Theme_Preference_Record) is
   begin
      if Pref.Themes /= null then
         for T in Pref.Themes'Range loop
            Free (Pref.Themes (T).Name);
            Free (Pref.Themes (T).Directory);
         end loop;
         Unchecked_Free (Pref.Themes);
      end if;

      Free (Preference_Record (Pref));
   end Free;

   --------------------------
   -- Create_Color_Buttons --
   --------------------------

   procedure Create_Color_Buttons
     (Pref            : access Style_Preference_Record'Class;
      Manager         : access Preferences_Manager_Record'Class;
      Fg_Color_Button : out Gtk_Color_Button;
      Bg_Color_Button : out Gtk_Color_Button)
   is
      Button : Gtk_Color_Button;
      P      : constant Manager_Preference :=
                 (Preferences_Manager (Manager), Preference (Pref));
   begin
      Gtk_New_With_Rgba (Button, Get_Pref_Fg (Style_Preference (Pref)));
      Button.Set_Use_Alpha (True);
      Set_Tooltip_Text (Button, -"Foreground color");
      Preference_Handlers.Connect
        (Button, Signal_Color_Set, Fg_Color_Changed'Access, P);
      Fg_Color_Button := Button;

      Gtk_New_With_Rgba (Button, Get_Pref_Bg (Style_Preference (Pref)));
      Button.Set_Use_Alpha (True);
      Set_Tooltip_Text (Button, -"Background color");
      Preference_Handlers.Connect
        (Button, Signal_Color_Set, Bg_Color_Changed'Access, P);
      Bg_Color_Button := Button;
   end Create_Color_Buttons;

   ----------------
   -- Set_Editor --
   ----------------

   procedure Set_Editor
     (Manager : access Preferences_Manager_Record;
      Editor  : access Preferences_Editor_Interface'Class) is
   begin
      Manager.Pref_Editor := Editor;
   end Set_Editor;

   ----------------
   -- Get_Editor --
   ----------------

   function Get_Editor
     (Manager : access Preferences_Manager_Record)
      return access Preferences_Editor_Interface'Class is
   begin
      return Manager.Pref_Editor;
   end Get_Editor;

   ---------------
   -- To_String --
   ---------------

   function To_String (V : Variant_Enum) return String is
      R : String := V'Img;
   begin
      for J in R'First + 1 .. R'Last loop
         if R (J) = '_' then
            R (J) := ' ';
         else
            R (J) := To_Lower (R (J));
         end if;
      end loop;

      return R;
   end To_String;

   -----------------
   -- From_String --
   -----------------

   function From_String (S : String) return Variant_Enum is
      R : String := S;
   begin
      for J in R'Range loop
         if R (J) = ' ' then
            R (J) := '_';
         end if;
      end loop;
      return Variant_Enum'Value (R);
   end From_String;

   ----------------------------
   -- Is_Loading_Preferences --
   ----------------------------

   function Is_Loading_Preferences
     (Self : not null access Preferences_Manager_Record'Class) return Boolean
   is
   begin
      return Self.Loading_Prefs;
   end Is_Loading_Preferences;

   --------------------------
   -- Set_Is_Loading_Prefs --
   --------------------------

   procedure Set_Is_Loading_Prefs
     (Self : not null access Preferences_Manager_Record'Class;
      Loading : Boolean)
   is
   begin
      Self.Loading_Prefs := Loading;
   end Set_Is_Loading_Prefs;

   -------------------------
   -- Get_First_Reference --
   -------------------------

   function Get_First_Reference
     (Manager : not null access Preferences_Manager_Record)
      return Page_Cursor is
   begin
      return (C => Manager.Pages.First);
   end Get_First_Reference;

   ----------
   -- Next --
   ----------

   procedure Next
     (Manager : not null access Preferences_Manager_Record;
      C       : in out Page_Cursor)
   is
      pragma Unreferenced (Manager);
   begin
      Pages_Lists.Next (C.C);
   end Next;

   --------------
   -- Get_Page --
   --------------

   function Get_Page (Self : Page_Cursor) return Preferences_Page is
   begin
      if not Pages_Lists.Has_Element (Self.C) then
         return null;
      else
         return Pages_Lists.Element (Self.C);
      end if;
   end Get_Page;

   -------------------------
   -- Get_First_Reference --
   -------------------------

   function Get_First_Reference
     (Manager : not null access Preferences_Manager_Record)
      return Preference_Cursor is
   begin
      return (C => Manager.Preferences.First);
   end Get_First_Reference;

   ----------
   -- Next --
   ----------

   procedure Next
     (Manager : not null access Preferences_Manager_Record;
      C       : in out Preference_Cursor)
   is
      pragma Unreferenced (Manager);
   begin
      Preferences_Maps.Next (C.C);
   end Next;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Self : Preference_Cursor) return Preference is
   begin
      if not Preferences_Maps.Has_Element (Self.C) then
         return null;
      else
         return Preferences_Maps.Element (Self.C);
      end if;
   end Get_Pref;

   ------------
   -- Freeze --
   ------------

   procedure Freeze
     (Self : not null access Preferences_Manager_Record) is
   begin
      Self.Freeze_Count := Self.Freeze_Count + 1;
   end Freeze;

   ----------
   -- Thaw --
   ----------

   procedure Thaw
     (Self : not null access Preferences_Manager_Record) is
   begin
      if Self.Freeze_Count > 0 then
         Self.Freeze_Count := Self.Freeze_Count - 1;
      end if;
   end Thaw;

   ---------------
   -- Is_Frozen --
   ---------------

   function Is_Frozen
     (Self : not null access Preferences_Manager_Record'Class)
      return Boolean is
   begin
      return Self.Freeze_Count > 0;
   end Is_Frozen;

   ------------------
   -- Gint_Changed --
   ------------------

   procedure Gint_Changed
     (Adj  : access GObject_Record'Class;
      Data : Manager_Preference)
   is
      A : constant Gtk_Adjustment := Gtk_Adjustment (Adj);
   begin
      Set_Pref
        (Integer_Preference (Data.Pref),
         Data.Manager, Integer (Get_Value (A)));
   end Gint_Changed;

   ---------------------
   -- Boolean_Changed --
   ---------------------

   procedure Boolean_Changed
     (Toggle : access GObject_Record'Class;
      Data   : Manager_Preference)
   is
      T     : constant Gtk_Toggle_Button := Gtk_Toggle_Button (Toggle);
   begin
      Set_Pref (Boolean_Preference (Data.Pref), Data.Manager, Get_Active (T));
   end Boolean_Changed;

   -------------------------
   -- Text_Buffer_Changed --
   -------------------------

   procedure Text_Buffer_Changed
     (Buffer : access GObject_Record'Class;
      Data   : Manager_Preference)
   is
      E        : constant Gtk_Text_Buffer := Gtk_Text_Buffer (Buffer);
      From, To : Gtk_Text_Iter;
   begin
      Get_Start_Iter (E, From);
      Get_End_Iter   (E, To);
      Set_Pref
        (String_Preference (Data.Pref), Data.Manager, Get_Text (E, From, To));
   end Text_Buffer_Changed;

   -------------------
   -- Entry_Changed --
   -------------------

   procedure Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Manager_Preference)
   is
      E     : constant Gtk_Entry := Gtk_Entry (Ent);
   begin
      Set_Pref (String_Preference (Data.Pref), Data.Manager, Get_Text (E));
   end Entry_Changed;

   -------------------
   -- Combo_Changed --
   -------------------

   procedure Combo_Changed
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference)
   is
      C : constant Gtk_Combo_Box_Text := Gtk_Combo_Box_Text (Combo);
   begin
      Set_Pref (Data.Pref, Data.Manager, Get_Active_Text (C));
   end Combo_Changed;

   ----------------
   -- Reset_Font --
   ----------------

   procedure Reset_Font (Ent : access Gtk_Widget_Record'Class) is
      E    : constant Gtk_Entry := Gtk_Entry (Ent);
      Desc : Pango_Font_Description := From_String (Get_Text (E));
   begin
      --  Also set the context, so that every time the pango layout is
      --  recreated by the entry (key press,...), we still use the correct
      --  font.

      if Pango.Context.Load_Font (Get_Pango_Context (E), Desc) = null then
         Free (Desc);
         Desc := From_Multi_String (Defaults.Default_Font);
      else
         Pango.Context.Set_Font_Description (Get_Pango_Context (E), Desc);
      end if;

      Modify_Font (E, Desc);
   end Reset_Font;

   ------------------------
   -- Font_Entry_Changed --
   ------------------------

   function Font_Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Manager_Preference) return Boolean
   is
      E     : constant Gtk_Entry := Gtk_Entry (Ent);
   begin
      if Data.Pref.all in Font_Preference_Record'Class then
         Set_Pref (Font_Preference (Data.Pref), Data.Manager, Get_Text (E));

      else
         Free (Style_Preference (Data.Pref).Font_Descr);
         Style_Preference (Data.Pref).Font_Descr := From_String (E.Get_Text);
         Data.Manager.Notify_Pref_Changed (Data.Pref);
      end if;

      Reset_Font (E);
      return False;
   end Font_Entry_Changed;

   -------------------
   -- Color_Changed --
   -------------------

   procedure Color_Changed
     (Button : access GObject_Record'Class;
      Data   : Manager_Preference)
   is
      Btn  : constant Gtk_Color_Button := Gtk_Color_Button (Button);
      Rgba : Gdk_RGBA;
      Val  : constant Gdk_RGBA := Color_Preference (Data.Pref).Get_Pref;
   begin
      Btn.Get_Rgba (Rgba);
      if Rgba /= Val then
         Set_Pref
           (Color_Preference (Data.Pref), Data.Manager, To_String (Rgba));
      end if;
   end Color_Changed;

   ----------------------
   -- Fg_Color_Changed --
   ----------------------

   procedure Fg_Color_Changed
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference)
   is
      C : constant Gtk_Color_Button := Gtk_Color_Button (Combo);
      R : Gdk_RGBA;
   begin
      C.Get_Rgba (R);
      if Style_Preference (Data.Pref).Fg_Color /= R then
         Style_Preference (Data.Pref).Fg_Color := R;
         Data.Manager.Notify_Pref_Changed (Data.Pref);
      end if;
   end Fg_Color_Changed;

   ---------------------
   -- Variant_Changed --
   ---------------------

   procedure Variant_Changed
     (Combo : access GObject_Record'Class; Data  : Manager_Preference)
   is
      C : constant Gtk_Combo_Box_Text := Gtk_Combo_Box_Text (Combo);
      V : constant Variant_Enum := From_String (C.Get_Active_Text);
   begin
      if Variant_Preference (Data.Pref).Variant /= V then
         Variant_Preference (Data.Pref).Variant := V;
         Data.Manager.Notify_Pref_Changed (Data.Pref);
      end if;
   end Variant_Changed;

   ----------------------
   -- Bg_Color_Changed --
   ----------------------

   procedure Bg_Color_Changed
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference)
   is
      C : constant Gtk_Color_Button := Gtk_Color_Button (Combo);
      R : Gdk_RGBA;
   begin
      C.Get_Rgba (R);
      if Style_Preference (Data.Pref).Bg_Color /= R then
         Style_Preference (Data.Pref).Bg_Color := R;
         Data.Manager.Notify_Pref_Changed (Data.Pref);
      end if;
   end Bg_Color_Changed;

   ---------------
   -- To_String --
   ---------------

   function To_String (Font : String; Fg, Bg : Gdk_RGBA) return String is
   begin
      return Font & '@' & To_String (Fg) & '@' & To_String (Bg);
   end To_String;

   -----------------
   -- Style_Token --
   -----------------

   function Style_Token (Value : String; Num : Positive) return String is
      Start, Last : Natural := Value'First;
      N           : Natural := Num;
   begin
      loop
         if Last > Value'Last then
            return Value (Start .. Last - 1);

         elsif Value (Last) = '@' then
            N := N - 1;
            if N = 0 then
               return Value (Start .. Last - 1);
            end if;

            Start := Last + 1;
         end if;

         Last := Last + 1;
      end loop;
   end Style_Token;

   -----------------
   -- Select_Font --
   -----------------

   procedure Select_Font
     (Ent  : access GObject_Record'Class;
      Data : Manager_Preference)
   is
      E      : constant Gtk_Entry := Gtk_Entry (Ent);
      F      : Gtk_Font_Selection;
      Dialog : Gtk_Dialog;
      Result : Boolean;
      Tmp    : Gtk_Widget;
      pragma Unreferenced (Result, Tmp);

   begin
      Gtk_New (Dialog,
               Title  => -"Select font",
               Parent => Gtk_Window (Get_Toplevel (E)),
               Flags  => Modal or Destroy_With_Parent);

      Gtk_New (F);
      Pack_Start (Get_Content_Area (Dialog), F, Expand => True, Fill => True);

      Tmp := Add_Button (Dialog, Stock_Ok,     Gtk_Response_OK);
      Tmp := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);

      --  Must be done only after the widget is visible
      Result := Set_Font_Name (F, Get_Text (E));

      if Run (Dialog) = Gtk_Response_OK then
         Set_Text (E, Get_Font_Name (F));

         if Data.Pref.all in Font_Preference_Record'Class then
            Free (Font_Preference (Data.Pref).Descr);
            Font_Preference (Data.Pref).Descr := From_String (E.Get_Text);

         else
            Free (Style_Preference (Data.Pref).Font_Descr);
            Style_Preference (Data.Pref).Font_Descr :=
              From_String (E.Get_Text);
         end if;
         Data.Manager.Notify_Pref_Changed (Data.Pref);
         Reset_Font (E);
      end if;

      Destroy (Dialog);
   end Select_Font;

   -------------------------
   -- Create_Box_For_Font --
   -------------------------

   function Create_Box_For_Font
     (Manager      : access Preferences_Manager_Record'Class;
      Pref         : Preference;
      Desc         : Pango_Font_Description;
      Button_Label : String) return My_Font_Box
   is
      Font_Box    : My_Font_Box;
      Ent         : Gtk_Entry;
      Button      : Gtk_Button;
      P           : constant Manager_Preference :=
                      (Preferences_Manager (Manager), Pref);
   begin
      Font_Box := new My_Font_Box_Record;
      Initialize_Hbox (Font_Box, Homogeneous => False);
      Gtk_New (Ent);
      Pack_Start (Gtk_Box (Font_Box), Ent, Expand => True, Fill => True);

      Font_Box.Ent := Ent;

      Gtk_New (Button, Button_Label);
      Pack_Start (Gtk_Box (Font_Box), Button, Expand => False, Fill => False);
      Preference_Handlers.Object_Connect
        (Button, Gtk.Button.Signal_Clicked,
         Preference_Handlers.To_Marshaller (Select_Font'Access),
         Slot_Object => Ent, User_Data => P);

      Return_Preference_Handlers.Connect
        (Ent, Signal_Focus_Out_Event, Font_Entry_Changed'Access, P);

      if Pango.Context.Load_Font
        (Get_Pango_Context (Manager.Pref_Editor.Get_Widget), Desc) /= null
      then
         Modify_Font (Ent, Desc);
      end if;

      Set_Text (Ent, To_String (Desc));
      Reset_Font (Ent);
      return Font_Box;
   end Create_Box_For_Font;

end Default_Preferences;
