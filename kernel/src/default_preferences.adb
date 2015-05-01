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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Strings.Hash;
with GNAT.OS_Lib;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.Utils;           use GNATCOLL.Utils;

with Glib.Object;              use Glib.Object;
with XML_Utils;                use XML_Utils;

with Gtk.Adjustment;           use Gtk.Adjustment;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Color_Button;         use Gtk.Color_Button;
with Gtk.Combo_Box;
with Gtk.Combo_Box_Text;       use Gtk.Combo_Box_Text;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Editable;             use Gtk.Editable;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Event_Box;            use Gtk.Event_Box;
with Gtk.Font_Selection;       use Gtk.Font_Selection;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Rc;                   use Gtk.Rc;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Spin_Button;          use Gtk.Spin_Button;
with Gtk.Stock;                use Gtk.Stock;
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

package body Default_Preferences is

   Me : constant Trace_Handle := Create ("Default_Prefs");

   use Preferences_Maps;

   ------------------
   -- Saved_Params --
   ------------------

   package Str_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, String, Ada.Strings.Hash, "=");
   use Str_Maps;

   -------------------------
   --  Preferences Editor --
   -------------------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Theme_Descr_Array, Theme_Descr_Array_Access);

   procedure Create_Color_Buttons
     (Box     : Gtk_Box;
      Pref    : access Style_Preference_Record'Class;
      Manager : access Preferences_Manager_Record'Class);
   --  Factorize code that creates the color buttons

   procedure Free (Pref : in out Preference);
   --  Free the memory associated with Pref

   function From_String (Color : String) return Gdk_RGBA;
   --  Parse the name of the color, and default to black if color is not found

   procedure Gint_Changed
     (Adj  : access GObject_Record'Class;
      Data : Manager_Preference);
   --  Called when a Gint preference has been changed.

   procedure Update_Gint
     (Adj  : access GObject_Record'Class;
      Data : Manager_Preference);
   --  Called when the preference Data has changed, to update Adj

   procedure Boolean_Changed
     (Toggle : access GObject_Record'Class;
      Data   : Manager_Preference);
   --  Called when a boolean preference has been changed.

   procedure Update_Variant
     (Button : access GObject_Record'Class;
      Data   : Manager_Preference);
   --  Called when a variant preference is changed.

   procedure Update_Boolean
     (Toggle : access GObject_Record'Class;
      Data   : Manager_Preference);
   --  Called when the preference Data has changed, to update Toggle

   procedure Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Manager_Preference);
   --  Called when the text in an entry field has changed.

   procedure Update_Entry
     (Ent  : access GObject_Record'Class;
      Data : Manager_Preference);
   --  Called when the preference Data has changed, to update Ent

   procedure Combo_Changed
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference);
   --  Called when the combo_box changed

   procedure Update_Combo
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference);
   --  Called when the preference Data has changed, to update Combo

   function Font_Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Manager_Preference) return Boolean;
   --  Called when the entry for a font selection has changed.

   procedure Text_Buffer_Changed
     (Buffer : access GObject_Record'Class;
      Data   : Manager_Preference);
   --  Called when the buffer has changed, to update the preference

   procedure Update_Text_Buffer
     (Buffer : access GObject_Record'Class;
      Data  : Manager_Preference);
   --  Called when the preference has changed, to update the buffer

   procedure Reset_Font (Ent : access Gtk_Widget_Record'Class);
   --  Update the font used for the entry Ent, based on its contents.

   procedure Color_Changed
     (Button : access GObject_Record'Class;
      Data   : Manager_Preference);
   --  Called when a color has changed.

   procedure Update_Font_Entry
     (Ent  : access GObject_Record'Class;
      Data : Manager_Preference);
   procedure Update_Fg
     (Button  : access GObject_Record'Class;
      Data    : Manager_Preference);
   procedure Update_Bg
     (Button : access GObject_Record'Class;
      Data   : Manager_Preference);
   --  Called when the preference Data has changed, to update Combo for the
   --  Font, foreground or background of the preference

   procedure Update_Color
     (Button : access GObject_Record'Class;
      Data   : Manager_Preference);
   --  Update the contents of the combo based on the color found in Data

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
      Button_Label : String) return Gtk_Box;
   --  Create a box suitable for editing fonts

   function Get_Pref_From_Name
     (Manager             : access Preferences_Manager_Record'Class;
      Name                : String) return Preferences_Maps.Cursor;
   --  Return a pointer to the preference Name

   function From_Multi_String
     (M : String) return Pango.Font.Pango_Font_Description;
   --  Return a font matching M.
   --  M is a string containing a list of descriptions separated by commas,
   --  for instance "Consolas 10, Courier New 9, Courier 10".
   --  The first font that matches a registered family is returned.

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
   -- Get_Page --
   --------------

   function Get_Page (Pref : access Preference_Record'Class) return String is
   begin
      if Pref.Page = null then
         return "";
      else
         return Pref.Page.all;
      end if;
   end Get_Page;

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
      C : Preferences_Maps.Cursor := First (Manager.Preferences);
      Pref : Preference;
   begin
      while Has_Element (C) loop
         Pref := Element (C);
         Free (Pref);
         Next (C);
      end loop;
      Clear (Manager.Preferences);
   end Destroy;

   ----------
   -- Free --
   ----------

   procedure Free (Pref : in out Preference) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Preference_Record'Class, Preference);
   begin
      if Pref /= null then
         Free (Pref.all);
         Unchecked_Free (Pref);
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
        Get_Pref_From_Name (Manager, Name, Create_If_Necessary => False);
      Val : Gdk_RGBA;
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
        Get_Pref_From_Name (Manager, Name, Create_If_Necessary => False);
      Val : Pango_Font_Description;
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

   ------------------------
   -- Get_Pref_From_Name --
   ------------------------

   function Get_Pref_From_Name
     (Manager             : access Preferences_Manager_Record'Class;
      Name                : String) return Preferences_Maps.Cursor
   is
      C : Preferences_Maps.Cursor := First (Manager.Preferences);
   begin
      while Has_Element (C) loop
         if Element (C).Name.all = Name then
            return C;
         end if;
         Next (C);
      end loop;

      return Preferences_Maps.No_Element;
   end Get_Pref_From_Name;

   function Get_Pref_From_Name
     (Manager             : access Preferences_Manager_Record;
      Name                : String;
      Create_If_Necessary : Boolean) return Preference
   is
      C : constant Preferences_Maps.Cursor :=
        Get_Pref_From_Name (Manager, Name);
   begin
      if Has_Element (C) then
         return Element (C);
      end if;

      if Create_If_Necessary then
         return Preference
           (String_Preference'
              (Create
                 (Manager => Manager,
                  Name    => Name,
                  Label   => Name,
                  Page    => "",
                  Doc     => "",
                  Default => "")));
      else
         return null;
      end if;
   end Get_Pref_From_Name;

   --------------
   -- Register --
   --------------

   procedure Register
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Pref                      : access Preference_Record'Class)
   is
      Old : Preferences_Maps.Cursor :=
        Get_Pref_From_Name (Manager, Name);
      Old_Pref : Preference;
   begin
      Free (Pref.Name);
      Pref.Name := new String'(Name);

      Free (Pref.Label);
      Pref.Label := new String'(Label);

      Free (Pref.Page);
      if Page /= "" and then Page /= "/" then
         Pref.Page := new String'(Page);
      end if;

      Free (Pref.Doc);
      Pref.Doc := new String'(Doc);

      --  If the preference was already in the list, remove the old value.
      --  It was probably inserted when reading the preferences file, which is
      --  in no specific order. Instead, we want to preserve the order based
      --  on the actual registration of preferences by the various modules, so
      --  that the preferences dialog is always displayed in the same order.

      if Has_Element (Old) then
         Old_Pref := Element (Old);
         Delete (Manager.Preferences, Old);

         if Old_Pref /= Preference (Pref) then
            Free (Old_Pref);
         end if;
      end if;

      Append
        (Container => Manager.Preferences,
         New_Item  => Preference (Pref));
   end Register;

   --------------
   -- Get_Pref --
   --------------

   overriding function Get_Pref
     (Pref : access Integer_Preference_Record) return String is
   begin
      return Integer'Image (Pref.Int_Value);
   end Get_Pref;

   function Get_Pref
     (Pref : access Integer_Preference_Record) return Integer is
   begin
      return Pref.Int_Value;
   end Get_Pref;

   overriding function Get_Pref
     (Pref : access Boolean_Preference_Record) return String is
   begin
      return Boolean'Image (Pref.Bool_Value);
   end Get_Pref;

   function Get_Pref
     (Pref : access Boolean_Preference_Record) return Boolean is
   begin
      return Pref.Bool_Value;
   end Get_Pref;

   overriding function Get_Pref
     (Pref : access String_Preference_Record) return String is
   begin
      return Pref.Str_Value.all;
   end Get_Pref;

   overriding function Get_Pref
     (Pref : access Color_Preference_Record) return String is
   begin
      return To_String (Pref.Color);
   end Get_Pref;

   function Get_Pref
     (Pref : access Color_Preference_Record) return Gdk.RGBA.Gdk_RGBA is
   begin
      return Pref.Color;
   end Get_Pref;

   overriding function Get_Pref
     (Pref : access Enum_Preference_Record) return String is
   begin
      return Integer'Image (Pref.Enum_Value);
   end Get_Pref;

   function Get_Pref
     (Pref : access Enum_Preference_Record) return Integer is
   begin
      return Pref.Enum_Value;
   end Get_Pref;

   overriding function Get_Pref
     (Pref : access Font_Preference_Record) return String is
   begin
      return To_String (Pref.Descr);
   end Get_Pref;

   function Get_Pref
     (Pref    : access Font_Preference_Record)
      return Pango.Font.Pango_Font_Description is
   begin
      return Pref.Descr;
   end Get_Pref;

   overriding function Get_Pref
     (Pref : access Style_Preference_Record) return String is
   begin
      return To_String
        (To_String (Pref.Font_Descr), Pref.Fg_Color, Pref.Bg_Color);
   end Get_Pref;

   overriding function Get_Pref
     (Pref : access Variant_Preference_Record) return String is
   begin
      return To_String
        (Pref.Variant'Img, Pref.Fg_Color, Pref.Bg_Color);
   end Get_Pref;

   function Get_Pref_Font
     (Pref : access Style_Preference_Record) return Pango_Font_Description is
   begin
      return Pref.Font_Descr;
   end Get_Pref_Font;

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
      Manager.On_Pref_Changed (Pref);
   end Set_Pref;

   procedure Set_Pref
     (Pref    : Integer_Preference;
      Manager : access Preferences_Manager_Record'Class;
      Value   : Integer) is
   begin
      Pref.Int_Value := Value;
      Manager.On_Pref_Changed (Pref);
   end Set_Pref;

   overriding procedure Set_Pref
     (Pref    : access Boolean_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Pref.Bool_Value := Boolean'Value (Value);
      Manager.On_Pref_Changed (Pref);
   end Set_Pref;

   procedure Set_Pref
     (Pref    : Boolean_Preference;
      Manager : access Preferences_Manager_Record'Class;
      Value   : Boolean) is
   begin
      Pref.Bool_Value := Value;
      Manager.On_Pref_Changed (Pref);
   end Set_Pref;

   overriding procedure Set_Pref
     (Pref    : access String_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Free (Pref.Str_Value);
      Pref.Str_Value := new String'(Value);
      Manager.On_Pref_Changed (Pref);
   end Set_Pref;

   overriding procedure Set_Pref
     (Pref    : access Color_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Pref.Color := From_String (Value);
      Manager.On_Pref_Changed (Pref);
   end Set_Pref;

   overriding procedure Set_Pref
     (Pref    : access Font_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Free (Pref.Descr);
      Pref.Descr := From_String (Value);
      Manager.On_Pref_Changed (Pref);
   end Set_Pref;

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

   procedure Set_Pref
     (Pref         : Style_Preference;
      Manager      : access Preferences_Manager_Record'Class;
      Font, Fg, Bg : String) is
   begin
      Free (Pref.Font_Descr);

      Pref.Fg_Color := From_String (Fg);
      Pref.Bg_Color := From_String (Bg);
      Pref.Font_Descr := From_String (Font);

      Manager.On_Pref_Changed (Pref);
   end Set_Pref;

   procedure Set_Pref
     (Pref         : Font_Preference;
      Manager      : access Preferences_Manager_Record'Class;
      Font         : Pango_Font_Description) is
   begin
      Free (Pref.Descr);
      Pref.Descr := Copy (Font);
      Manager.On_Pref_Changed (Pref);
   end Set_Pref;

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

      Manager.On_Pref_Changed (Pref);
   end Set_Pref;

   overriding procedure Set_Pref
     (Pref    : access Enum_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Pref.Enum_Value := Integer'Value (Value);
      Manager.On_Pref_Changed (Pref);
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
      File, Node : Node_Ptr;
      C          : Preferences_Maps.Cursor := First (Manager.Preferences);
      P          : Preference;
   begin
      File := new XML_Utils.Node;
      File.Tag := new String'("Prefs");

      while Has_Element (C) loop
         P := Element (C);
         if not P.Is_Default then
            Node     := new XML_Utils.Node;
            Node.Tag := new String'("pref");
            Set_Attribute (Node, "name", Get_Name (P));
            Node.Value := new String'(Get_Pref (P));
            Add_Child (File, Node);
         end if;

         Next (C);
      end loop;

      Print (File, File_Name, Success);

   exception
      when E : others => Trace (Me, E);
   end Save_Preferences;

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

   -----------------
   -- Update_Gint --
   -----------------

   procedure Update_Gint
     (Adj  : access GObject_Record'Class;
      Data : Manager_Preference) is
   begin
      Set_Value
        (Gtk_Adjustment (Adj),
         Gdouble (Integer_Preference (Data.Pref).Int_Value));
   end Update_Gint;

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

   --------------------
   -- Update_Boolean --
   --------------------

   procedure Update_Boolean
     (Toggle : access GObject_Record'Class;
      Data   : Manager_Preference) is
   begin
      Set_Active (Gtk_Toggle_Button (Toggle),
                  Boolean_Preference (Data.Pref).Bool_Value);
   end Update_Boolean;

   -------------------------
   -- Text_Buffer_Changed --
   -------------------------

   procedure Text_Buffer_Changed
     (Buffer : access GObject_Record'Class;
      Data   : Manager_Preference)
   is
      E     : constant Gtk_Text_Buffer := Gtk_Text_Buffer (Buffer);
      From, To : Gtk_Text_Iter;
   begin
      Get_Start_Iter (E, From);
      Get_End_Iter   (E, To);
      Set_Pref
        (String_Preference (Data.Pref), Data.Manager, Get_Text (E, From, To));
   end Text_Buffer_Changed;

   ------------------------
   -- Update_Text_Buffer --
   ------------------------

   procedure Update_Text_Buffer
     (Buffer : access GObject_Record'Class;
      Data   : Manager_Preference) is
   begin
      Set_Text (Gtk_Text_Buffer (Buffer), Get_Pref (Data.Pref));
   end Update_Text_Buffer;

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

   ------------------
   -- Update_Entry --
   ------------------

   procedure Update_Entry
     (Ent  : access GObject_Record'Class;
      Data : Manager_Preference) is
   begin
      Set_Text (Gtk_Entry (Ent), String'(Get_Pref (Data.Pref)));
   end Update_Entry;

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

   ------------------
   -- Update_Combo --
   ------------------

   procedure Update_Combo
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference)
   is
   begin
      Set_Active_Text
        (Gtk_Combo_Box_Text (Combo), String'(Get_Pref (Data.Pref)));
   end Update_Combo;

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
         Data.Manager.On_Pref_Changed (Data.Pref);
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
      Btn : constant Gtk_Color_Button := Gtk_Color_Button (Button);
      Rgba : Gdk_RGBA;
      Val  : constant Gdk_RGBA := Color_Preference (Data.Pref).Get_Pref;
   begin
      Btn.Get_Rgba (Rgba);
      if Rgba /= Val then
         Set_Pref
           (Color_Preference (Data.Pref), Data.Manager, To_String (Rgba));
      end if;
   end Color_Changed;

   --------------------
   -- Update_Variant --
   --------------------

   procedure Update_Variant
     (Button : access GObject_Record'Class;
      Data   : Manager_Preference)
   is
      Count : Gint := 0;
   begin
      for J in Variant_Enum loop
         if J = Variant_Preference (Data.Pref).Variant then
            Gtk_Combo_Box_Text (Button).Set_Active (Count);
            exit;
         end if;
         Count := Count + 1;
      end loop;
   end Update_Variant;

   ---------------
   -- Update_Fg --
   ---------------

   procedure Update_Fg
     (Button : access GObject_Record'Class;
      Data   : Manager_Preference)
   is
      Old, Val : Gdk_RGBA;
   begin
      Gtk_Color_Button (Button).Get_Rgba (Old);
      Val := Get_Pref_Fg (Style_Preference (Data.Pref));

      if Old /= Val then
         Gtk_Color_Button (Button).Set_Rgba (Val);
      end if;
   end Update_Fg;

   ---------------
   -- Update_Bg --
   ---------------

   procedure Update_Bg
     (Button : access GObject_Record'Class;
      Data   : Manager_Preference)
   is
      Old, Val : Gdk_RGBA;
   begin
      Gtk_Color_Button (Button).Get_Rgba (Old);
      Val := Get_Pref_Bg (Style_Preference (Data.Pref));

      if Old /= Val then
         Gtk_Color_Button (Button).Set_Rgba (Val);
      end if;
   end Update_Bg;

   -----------------------
   -- Update_Font_Entry --
   -----------------------

   procedure Update_Font_Entry
     (Ent  : access GObject_Record'Class;
      Data : Manager_Preference) is
   begin
      if Data.Pref.all in Font_Preference_Record'Class then
         Set_Text (Gtk_Entry (Ent),
                   To_String (Font_Preference (Data.Pref).Descr));
      else
         Set_Text (Gtk_Entry (Ent),
                   To_String (Style_Preference (Data.Pref).Font_Descr));
      end if;
   end Update_Font_Entry;

   ------------------
   -- Update_Color --
   ------------------

   procedure Update_Color
     (Button : access GObject_Record'Class;
      Data   : Manager_Preference)
   is
      R, Old : Gdk_RGBA;
      Success : Boolean;
   begin
      Parse (R, Get_Pref (Color_Preference (Data.Pref)), Success);

      if Success then
         Gtk_Color_Button (Button).Get_Rgba (Old);
         if Old /= R then
            Set_Rgba (Gtk_Color_Button (Button), R);
         end if;
      end if;
   end Update_Color;

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
         Data.Manager.On_Pref_Changed (Data.Pref);
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
         Data.Manager.On_Pref_Changed (Data.Pref);
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
         Data.Manager.On_Pref_Changed (Data.Pref);
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
         Data.Manager.On_Pref_Changed (Data.Pref);
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
      Button_Label : String) return Gtk_Box
   is
      Box    : Gtk_Box;
      Ent    : Gtk_Entry;
      Button : Gtk_Button;
      P : constant Manager_Preference := (Preferences_Manager (Manager), Pref);
   begin
      Gtk_New_Hbox (Box, Homogeneous => False);
      Gtk_New (Ent);
      Pack_Start (Box, Ent, Expand => True, Fill => True);

      Gtk_New (Button, Button_Label);
      Pack_Start (Box, Button, Expand => False, Fill => False);
      Preference_Handlers.Object_Connect
        (Button, Gtk.Button.Signal_Clicked,
         Preference_Handlers.To_Marshaller (Select_Font'Access),
         Slot_Object => Ent, User_Data => P);

      Return_Preference_Handlers.Connect
        (Ent, Signal_Focus_Out_Event, Font_Entry_Changed'Access, P);
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Font_Entry'Access, Ent, P);

      if Pango.Context.Load_Font
        (Get_Pango_Context (Manager.Pref_Editor), Desc) /= null
      then
         Modify_Font (Ent, Desc);
      end if;

      Set_Text (Ent, To_String (Desc));
      Reset_Font (Ent);
      return Box;
   end Create_Box_For_Font;

   ----------
   -- Free --
   ----------

   procedure Free (Pref : in out Preference_Record) is
   begin
      Free (Pref.Name);
      Free (Pref.Label);
      Free (Pref.Page);
      Free (Pref.Doc);
   end Free;

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
      P : constant Manager_Preference :=
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
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Gint'Access, Adj, P);
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
      P : constant Manager_Preference :=
        (Preferences_Manager (Manager), Preference (Pref));
   begin
      Gtk_New (Toggle, Pref.Get_Label);
      Toggle.Set_Active (Pref.Bool_Value);

      Preference_Handlers.Connect
        (Toggle, Gtk.Toggle_Button.Signal_Toggled, Boolean_Changed'Access, P);
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Boolean'Access, Toggle, P);
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
      Ent  : Gtk_Entry;
      Text : Gtk_Text_View;
      Scrolled : Gtk_Scrolled_Window;
      P : constant Manager_Preference :=
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
         Preference_Handlers.Object_Connect
           (Manager.Pref_Editor, Signal_Preferences_Changed,
            Update_Text_Buffer'Access, Get_Buffer (Text), P);
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
         Preference_Handlers.Object_Connect
           (Manager.Pref_Editor, Signal_Preferences_Changed,
            Update_Entry'Access,
            Ent, P);
         return Gtk_Widget (Ent);
      end if;
   end Edit;

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
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Color_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Button : Gtk_Color_Button;
      P : constant Manager_Preference :=
        (Preferences_Manager (Manager), Preference (Pref));
   begin
      Gtk_New_With_Rgba (Button, Get_Pref (Color_Preference (Pref)));
      Button.Set_Use_Alpha (True);

      Preference_Handlers.Connect
        (Button, Signal_Color_Set, Color_Changed'Access, P);
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Color'Access, Button, P);
      return Gtk_Widget (Button);
   end Edit;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Font_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      W : Gtk_Widget;
   begin
      W := Gtk_Widget
        (Create_Box_For_Font
           (Manager, Preference (Pref),
            Get_Pref (Font_Preference (Pref)), -"..."));
      Set_Tooltip_Text (W, -"Click on ... to display the font selector");
      return W;
   end Edit;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Pref : in out Font_Preference_Record) is
   begin
      Free (Pref.Default);
      Free (Pref.Descr);
      Free (Preference_Record (Pref));
   end Free;

   --------------------------
   -- Create_Color_Buttons --
   --------------------------

   procedure Create_Color_Buttons
     (Box     : Gtk_Box;
      Pref    : access Style_Preference_Record'Class;
      Manager : access Preferences_Manager_Record'Class)
   is
      Button : Gtk_Color_Button;
      P : constant Manager_Preference :=
        (Preferences_Manager (Manager), Preference (Pref));
   begin
      Gtk_New_With_Rgba (Button, Get_Pref_Fg (Style_Preference (Pref)));
      Button.Set_Use_Alpha (True);
      Set_Tooltip_Text (Button, -"Foreground color");
      Pack_Start (Box, Button, Expand => False);
      Preference_Handlers.Connect
        (Button, Signal_Color_Set, Fg_Color_Changed'Access, P);
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Fg'Access, Button, P);

      Gtk_New_With_Rgba (Button, Get_Pref_Bg (Style_Preference (Pref)));
      Button.Set_Use_Alpha (True);
      Set_Tooltip_Text (Button, -"Background color");
      Pack_Start (Box, Button, Expand => False);
      Preference_Handlers.Connect
        (Button, Signal_Color_Set, Bg_Color_Changed'Access, P);
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Bg'Access, Button, P);
   end Create_Color_Buttons;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Style_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Event : Gtk_Event_Box;
      Box   : Gtk_Box;
      F     : constant Gtk_Box :=
                Create_Box_For_Font
                  (Manager, Preference (Pref),
                   Get_Pref_Font (Style_Preference (Pref)), "...");

   begin
      Gtk_New (Event);
      Add (Event, F);
      Set_Tooltip_Text (Event, -"Click on ... to display the font selector");
      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Box, Event, Expand => True, Fill => True);

      Create_Color_Buttons (Box, Pref, Manager);

      return Gtk_Widget (Box);
   end Edit;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Variant_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Box   : Gtk_Box;
      Variant_Combo : Gtk_Combo_Box_Text;
      Count : Gint := 0;
      P : constant Manager_Preference :=
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
      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Box, Variant_Combo, Expand => True, Fill => True);
      Preference_Handlers.Connect
        (Variant_Combo, Gtk.Combo_Box.Signal_Changed,
         Variant_Changed'Access, P);
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Variant'Access, Variant_Combo, P);

      Create_Color_Buttons (Box, Pref, Manager);

      return Gtk_Widget (Box);
   end Edit;

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
            Manager.On_Pref_Changed (Pref);
            return;
         end if;
      end loop;
   end Set_Pref;

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
         (Get_Home_Directory.Full_Name.all & Directory_Separator & ".themes")
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

      Dirs       : constant File_Array := From_Path (Search_Path);
      Dir        : GNATCOLL.VFS.Virtual_File;
      Subdirs    : GNATCOLL.VFS.File_Array_Access;
      Rc_File    : Virtual_File;
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

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref      : access Theme_Preference_Record;
      Manager   : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      P : constant Manager_Preference :=
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
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Combo'Access, Theme_Combo, P);

      return Gtk_Widget (Theme_Combo);
   end Edit;

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

   ----------
   -- Undo --
   ----------

   procedure Undo
     (Pref   : access Preferences_Page_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Pref, Widget);
   begin
      null;
   end Undo;

   ----------------
   -- Set_Editor --
   ----------------

   procedure Set_Editor
     (Manager : access Preferences_Manager_Record;
      Editor  : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Manager.Pref_Editor := Gtk_Widget (Editor);
   end Set_Editor;

   ----------------
   -- Get_Editor --
   ----------------

   function Get_Editor
     (Manager : access Preferences_Manager_Record)
      return Gtk.Widget.Gtk_Widget is
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
      return Cursor
   is
   begin
      return (C => First (Manager.Preferences));
   end Get_First_Reference;

   ----------
   -- Next --
   ----------

   procedure Next
     (Manager : not null access Preferences_Manager_Record;
      C       : in out Cursor)
   is
      pragma Unreferenced (Manager);
   begin
      Next (C.C);
   end Next;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Self : Cursor) return Preference is
   begin
      if not Has_Element (Self.C) then
         return null;
      else
         return Element (Self.C);
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

end Default_Preferences;
