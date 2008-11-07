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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Interfaces.C.Strings;     use Interfaces.C.Strings;

with Gdk.Color;                use Gdk.Color;
with Gdk.Font;                 use Gdk.Font;
with Gdk.Types;                use Gdk.Types;

with Glib.Object;              use Glib.Object;
with Glib.Properties;          use Glib.Properties;
with Glib.Xml_Int;             use Glib.Xml_Int;

with Gtk.Adjustment;           use Gtk.Adjustment;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Color_Selection;      use Gtk.Color_Selection;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Editable;             use Gtk.Editable;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Event_Box;            use Gtk.Event_Box;
with Gtk.Font_Selection;       use Gtk.Font_Selection;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Label;                use Gtk.Label;
with Gtk.Object;               use Gtk.Object;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Separator;            use Gtk.Separator;
with Gtk.Spin_Button;          use Gtk.Spin_Button;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Table;                use Gtk.Table;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
with Gtk.Tooltips;             use Gtk.Tooltips;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;

with Gtkada.Color_Combo;       use Gtkada.Color_Combo;
with Gtkada.Handlers;          use Gtkada.Handlers;

with Pango.Font;               use Pango.Font;

with Config;
with GPS.Intl;                 use GPS.Intl;
with GUI_Utils;                use GUI_Utils;
with Traces;                   use Traces;
with XML_Parsers;

package body Default_Preferences is

   Me : constant Debug_Handle := Create ("Default_Prefs");

   use Preferences_Maps;
   use type Gdk.Gdk_Font;

   ------------------
   -- Saved_Params --
   ------------------

   package Str_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, String, Ada.Strings.Hash, "=");
   use Str_Maps;

   type Saved_Prefs_Data_Record is record
      Preferences : Str_Maps.Map;
      Manager     : Preferences_Manager;
   end record;

   -------------------------
   --  Preferences Editor --
   -------------------------

   type Preferences_Editor_Record is new Gtk_Dialog_Record with null record;
   type Preferences_Editor is access all Preferences_Editor_Record'Class;
   Preferences_Editor_Class_Record : Gtk.Object.GObject_Class :=
     Gtk.Object.Uninitialized_Class;
   Preferences_Editor_Signals : constant chars_ptr_array :=
                       (1 => New_String (String (Signal_Preferences_Changed)));

   procedure Free (Pref : in out Preference);
   --  Free the memory associated with Pref

   procedure Toggled_Boolean (Toggle : access Gtk_Widget_Record'Class);
   --  Called when a toggle button has changed, to display the appropriate text
   --  in it.

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
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference);
   --  Called when a color has changed.

   procedure Update_Font_Entry
     (Ent  : access GObject_Record'Class;
      Data : Manager_Preference);
   procedure Update_Fg
     (Combo  : access GObject_Record'Class;
      Data : Manager_Preference);
   procedure Update_Bg
     (Combo  : access GObject_Record'Class;
      Data : Manager_Preference);
   --  Called when the preference Data has changed, to update Combo for the
   --  Font, foreground or background of the preference

   procedure Update_Color
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference);
   --  Update the contents of the combo based on the color found in Data

   procedure Bg_Color_Changed
     (Combo : access GObject_Record'Class; Data  : Manager_Preference);
   --  Called when the background color of a style has changed.

   procedure Fg_Color_Changed
     (Combo : access GObject_Record'Class; Data  : Manager_Preference);
   --  Called when the foreground color of a style has changed.

   procedure Select_Font
     (Ent : access GObject_Record'Class; Data : Manager_Preference);
   --  Open a dialog to select a new font

   procedure Key_Grab (Ent : access Gtk_Widget_Record'Class);
   --  Callback for the "grab" button when editing a key preference

   function To_String (Font, Fg, Bg : String) return String;
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
      Result : constant Integer_Preference := new Integer_Preference_Record;
   begin
      Result.Int_Min_Value := Minimum;
      Result.Int_Max_Value := Maximum;
      Result.Int_Value     := Default;
      Register (Manager, Name, Label, Page, Doc, Result);
      return Result;
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
      Result : constant Boolean_Preference := new Boolean_Preference_Record;
   begin
      Result.Bool_Value := Default;
      Register (Manager, Name, Label, Page, Doc, Result);
      return Result;
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
      Result : constant String_Preference := new String_Preference_Record;
   begin
      Result.Str_Value := new String'(Default);
      Result.Multi_Line := Multi_Line;
      Register (Manager, Name, Label, Page, Doc, Result);
      return Result;
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
      Result : constant Color_Preference := new Color_Preference_Record;
   begin
      Result.Color_Value := new String'(Default);
      Register (Manager, Name, Label, Page, Doc, Result);
      return Result;
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
      Result : constant Font_Preference := new Font_Preference_Record;
   begin
      Result.Font_Value := new String'(Default);
      Register (Manager, Name, Label, Page, Doc, Result);
      return Result;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Default_Modifier          : Gdk.Types.Gdk_Modifier_Type;
      Default_Key               : Gdk.Types.Gdk_Key_Type)
      return Key_Preference
   is
      Result : constant Key_Preference := new Key_Preference_Record;
   begin
      Result.Key_Modifier := Default_Modifier;
      Result.Key_Value    := Default_Key;
      Register (Manager, Name, Label, Page, Doc, Result);
      return Result;
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
      Result.Style_Font := new String'(Default_Font);
      Result.Style_Fg   := new String'(Default_Fg);
      Result.Style_Bg   := new String'(Default_Bg);
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
      Old : constant Preferences_Maps.Cursor :=
        Get_Pref_From_Name (Manager, Name);
      Old_Pref : Preference;
   begin
      Free (Pref.Name);
      Pref.Name := new String'(Name);

      Free (Pref.Label);
      Pref.Label := new String'(Label);

      Free (Pref.Page);
      if Page /= "" then
         Pref.Page := new String'(Page);
      end if;

      Free (Pref.Doc);
      Pref.Doc := new String'(Doc);

      if Has_Element (Old) then
         Old_Pref := Element (Old);

         --  Override the current value with the one that was already stored,
         --  since the latter was most probably read from the XML file

         Set_Pref
           (Pref    => Pref,
            Manager => Manager,
            Value   => String'(Get_Pref (Pref)));
         Free (Old_Pref);

         Replace_Element (Manager.Preferences, Old, Preference (Pref));
      else
         Append
           (Container => Manager.Preferences,
            New_Item  => Preference (Pref));
      end if;
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
      return Pref.Color_Value.all;
   end Get_Pref;

   function Get_Pref
     (Pref : access Color_Preference_Record) return Gdk.Color.Gdk_Color is
   begin
      if Pref.Color = Gdk.Color.Null_Color then
         Pref.Color := Parse (Pref.Color_Value.all);
         Alloc (Gtk.Widget.Get_Default_Colormap, Pref.Color);
      end if;
      return Pref.Color;
   exception
      when Wrong_Color =>
         Pref.Color := Black (Get_Default_Colormap);
         return Pref.Color;
   end Get_Pref;

   overriding function Get_Pref
     (Pref : access Key_Preference_Record) return String is
   begin
      return Image (Pref.Key_Value, Pref.Key_Modifier);
   end Get_Pref;

   procedure Get_Pref
     (Pref     : access Key_Preference_Record;
      Modifier : out Gdk_Modifier_Type;
      Key      : out Gdk_Key_Type) is
   begin
      Modifier := Pref.Key_Modifier;
      Key      := Pref.Key_Value;
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
      return Pref.Font_Value.all;
   end Get_Pref;

   function Get_Pref
     (Pref    : access Font_Preference_Record)
      return Pango.Font.Pango_Font_Description is
   begin
      if Pref.Descr = null then
         Pref.Descr := From_String (Pref.Font_Value.all);

         --  Check that the font exists, or use a default, to avoid crashes
         if From_Description (Pref.Descr) = null then
            Free (Pref.Descr);
            Pref.Descr := From_String (Config.Default_Font);
         end if;
      end if;
      return Pref.Descr;
   end Get_Pref;

   overriding function Get_Pref
     (Pref : access Style_Preference_Record) return String is
   begin
      return To_String
        (Pref.Style_Font.all, Pref.Style_Fg.all, Pref.Style_Bg.all);
   end Get_Pref;

   function Get_Pref_Font
     (Pref : access Style_Preference_Record) return Pango_Font_Description is
   begin
      if Pref.Font_Descr = null then
         Pref.Font_Descr := From_String (Pref.Style_Font.all);

         --  Check that the font exists, or use a default, to avoid crashes
         if From_Description (Pref.Font_Descr) = null then
            Free (Pref.Font_Descr);
            Pref.Font_Descr := From_String (Config.Default_Font);
         end if;
      end if;
      return Pref.Font_Descr;
   end Get_Pref_Font;

   function Get_Pref_Fg
     (Pref    : access Style_Preference_Record) return Gdk.Color.Gdk_Color is
   begin
      if Pref.Fg_Color = Gdk.Color.Null_Color then
         Pref.Fg_Color := Parse (Pref.Style_Fg.all);
         Alloc (Gtk.Widget.Get_Default_Colormap, Pref.Fg_Color);
      end if;
      return Pref.Fg_Color;
   exception
      when Wrong_Color =>
         Pref.Fg_Color := Black (Get_Default_Colormap);
         return Pref.Fg_Color;
   end Get_Pref_Fg;

   function Get_Pref_Bg
     (Pref    : access Style_Preference_Record) return Gdk.Color.Gdk_Color is
   begin
      if Pref.Bg_Color = Gdk.Color.Null_Color then
         Pref.Bg_Color := Parse (Pref.Style_Bg.all);
         Alloc (Gtk.Widget.Get_Default_Colormap, Pref.Bg_Color);
      end if;
      return Pref.Bg_Color;
   exception
      when Wrong_Color =>
         Pref.Bg_Color := Black (Get_Default_Colormap);
         return Pref.Bg_Color;
   end Get_Pref_Bg;

   -----------------------
   -- Emit_Pref_Changed --
   -----------------------

   procedure Emit_Pref_Changed
     (Manager : access Preferences_Manager_Record'Class) is
   begin
      if Manager /= null and then Manager.Pref_Editor /= null then
         Widget_Callback.Emit_By_Name
           (Manager.Pref_Editor, Signal_Preferences_Changed);
      end if;
   end Emit_Pref_Changed;

   --------------
   -- Set_Pref --
   --------------

   overriding procedure Set_Pref
     (Pref    : access Key_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      GUI_Utils.Value (Value, Pref.Key_Value, Pref.Key_Modifier);
      Emit_Pref_Changed (Manager);
   end Set_Pref;

   procedure Set_Pref
     (Pref     : Key_Preference;
      Manager  : access Preferences_Manager_Record'Class;
      Modifier : Gdk.Types.Gdk_Modifier_Type;
      Key      : Gdk.Types.Gdk_Key_Type) is
   begin
      Pref.Key_Value := Key;
      Pref.Key_Modifier := Modifier;
      Emit_Pref_Changed (Manager);
   end Set_Pref;

   overriding procedure Set_Pref
     (Pref    : access Integer_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Pref.Int_Value := Integer'Value (Value);
      Emit_Pref_Changed (Manager);
   end Set_Pref;

   procedure Set_Pref
     (Pref    : Integer_Preference;
      Manager : access Preferences_Manager_Record'Class;
      Value   : Integer) is
   begin
      Pref.Int_Value := Value;
      Emit_Pref_Changed (Manager);
   end Set_Pref;

   overriding procedure Set_Pref
     (Pref    : access Boolean_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Pref.Bool_Value := Boolean'Value (Value);
      Emit_Pref_Changed (Manager);
   end Set_Pref;

   procedure Set_Pref
     (Pref    : Boolean_Preference;
      Manager : access Preferences_Manager_Record'Class;
      Value   : Boolean) is
   begin
      Pref.Bool_Value := Value;
      Emit_Pref_Changed (Manager);
   end Set_Pref;

   overriding procedure Set_Pref
     (Pref    : access String_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Free (Pref.Str_Value);
      Pref.Str_Value := new String'(Value);
      Emit_Pref_Changed (Manager);
   end Set_Pref;

   overriding procedure Set_Pref
     (Pref    : access Color_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Free (Pref.Color_Value);
      Pref.Color_Value := new String'(Value);

      if Pref.Color /= Null_Color then
         Free_Colors (Gtk.Widget.Get_Default_Colormap, (1 => Pref.Color));
         Pref.Color := Null_Color;
      end if;

      Emit_Pref_Changed (Manager);
   end Set_Pref;

   overriding procedure Set_Pref
     (Pref    : access Font_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Free (Pref.Font_Value);
      Free (Pref.Descr);
      Pref.Font_Value := new String'(Value);
      Emit_Pref_Changed (Manager);
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

   procedure Set_Pref
     (Pref         : Style_Preference;
      Manager      : access Preferences_Manager_Record'Class;
      Font, Fg, Bg : String) is
   begin
      Free (Pref.Style_Font);
      Free (Pref.Font_Descr);
      Free (Pref.Style_Fg);
      Free (Pref.Style_Bg);

      if Pref.Fg_Color /= Null_Color then
         Free_Colors (Gtk.Widget.Get_Default_Colormap,
                      (1 => Pref.Fg_Color, 2 => Pref.Bg_Color));
         Pref.Fg_Color := Null_Color;
         Pref.Bg_Color := Null_Color;
      end if;

      Pref.Style_Font := new String'(Font);
      Pref.Style_Fg   := new String'(Fg);
      Pref.Style_Bg   := new String'(Bg);

      Emit_Pref_Changed (Manager);
   end Set_Pref;

   overriding procedure Set_Pref
     (Pref    : access Enum_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is
   begin
      Pref.Enum_Value := Integer'Value (Value);
      Emit_Pref_Changed (Manager);
   end Set_Pref;

   ----------------------
   -- Load_Preferences --
   ----------------------

   procedure Load_Preferences
     (Manager : access  Preferences_Manager_Record; File_Name : String)
   is
      File, Node : Node_Ptr;
      Err        : String_Access;
   begin
      if Is_Regular_File (File_Name) then
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
               Trace (Me, "Load new style preferences");
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
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Load_Preferences;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences
     (Manager   : access Preferences_Manager_Record;
      File_Name : String;
      Success   : out Boolean)
   is
      File, Node : Node_Ptr;
      C          : Preferences_Maps.Cursor := First (Manager.Preferences);
      P          : Preference;
   begin
      File := new Glib.Xml_Int.Node;
      File.Tag := new String'("Prefs");

      while Has_Element (C) loop
         P := Element (C);
         Node     := new Glib.Xml_Int.Node;
         Node.Tag := new String'("pref");
         Set_Attribute (Node, "name", Get_Name (P));
         Node.Value := new String'(Get_Pref (P));
         Add_Child (File, Node);
         Next (C);
      end loop;

      Print (File, File_Name, Success);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Save_Preferences;

   ---------------------
   -- Toggled_Boolean --
   ---------------------

   procedure Toggled_Boolean (Toggle : access Gtk_Widget_Record'Class) is
      T : constant Gtk_Toggle_Button := Gtk_Toggle_Button (Toggle);
   begin
      if Get_Active (T) then
         Set_Text (Gtk_Label (Get_Child (T)), -"(Enabled)");
      else
         Set_Text (Gtk_Label (Get_Child (T)), -"(Disabled)");
      end if;
   end Toggled_Boolean;

   ------------------
   -- Gint_Changed --
   ------------------

   procedure Gint_Changed
     (Adj  : access GObject_Record'Class;
      Data : Manager_Preference)
   is
      A : constant Gtk_Adjustment := Gtk_Adjustment (Adj);
   begin
      Set_Pref (Integer_Preference (Data.Pref), null, Integer (Get_Value (A)));
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
      Set_Pref (Boolean_Preference (Data.Pref), null, Get_Active (T));
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
      Set_Pref (String_Preference (Data.Pref), null, Get_Text (E, From, To));
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
      Set_Pref (String_Preference (Data.Pref), null, Get_Text (E));
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
      --  ??? Right now, the mechanism described above will cause gtk to
      --  crash when Desc doesn't correspond to a drawable font, therefore
      --  the following code is commented out.
      --  Set_Font_Description (Get_Pango_Context (E), Desc);

      if From_Description (Desc) = null then
         Free (Desc);
         Desc := From_String (Config.Default_Font);
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
         Set_Pref (Font_Preference (Data.Pref), null, Get_Text (E));

      else
         Free (Style_Preference (Data.Pref).Style_Font);
         Free (Style_Preference (Data.Pref).Font_Descr);
         Style_Preference (Data.Pref).Style_Font := new String'(Get_Text (E));
      end if;

      Reset_Font (E);
      return False;
   end Font_Entry_Changed;

   --------------
   -- Key_Grab --
   --------------

   procedure Key_Grab (Ent : access Gtk_Widget_Record'Class) is
      E    : constant Gtk_Entry := Gtk_Entry (Ent);
      Key  : Gdk.Types.Gdk_Key_Type;
      Mods : Gdk.Types.Gdk_Modifier_Type;
   begin
      GUI_Utils.Key_Grab (E, Key, Mods);
      Set_Text (E, Image (Key, Mods));
   end Key_Grab;

   -------------------
   -- Color_Changed --
   -------------------

   procedure Color_Changed
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference)
   is
      C     : constant Gtk_Color_Combo := Gtk_Color_Combo (Combo);
   begin
      Set_Pref (Color_Preference (Data.Pref), null, Get_Color (C));
   end Color_Changed;

   ---------------
   -- Update_Fg --
   ---------------

   procedure Update_Fg
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference) is
   begin
      Set_Color (Gtk_Color_Combo (Combo),
                 Get_Pref_Fg (Style_Preference (Data.Pref)));
   end Update_Fg;

   ---------------
   -- Update_Bg --
   ---------------

   procedure Update_Bg
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference) is
   begin
      Set_Color (Gtk_Color_Combo (Combo),
                 Get_Pref_Bg (Style_Preference (Data.Pref)));
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
                   Font_Preference (Data.Pref).Font_Value.all);
      else
         Set_Text (Gtk_Entry (Ent),
                   Style_Preference (Data.Pref).Style_Font.all);
      end if;
   end Update_Font_Entry;

   ------------------
   -- Update_Color --
   ------------------

   procedure Update_Color
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference) is
   begin
      Set_Color
        (Gtk_Color_Combo (Combo), Get_Pref (Color_Preference (Data.Pref)));
   end Update_Color;

   ----------------------
   -- Fg_Color_Changed --
   ----------------------

   procedure Fg_Color_Changed
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference)
   is
      C     : constant Gtk_Color_Combo := Gtk_Color_Combo (Combo);
   begin
      Free (Style_Preference (Data.Pref).Style_Fg);
      Style_Preference (Data.Pref).Style_Fg := new String'(Get_Color (C));
      Style_Preference (Data.Pref).Fg_Color := Get_Color (C);
   end Fg_Color_Changed;

   ----------------------
   -- Bg_Color_Changed --
   ----------------------

   procedure Bg_Color_Changed
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference)
   is
      C     : constant Gtk_Color_Combo := Gtk_Color_Combo (Combo);
   begin
      Free (Style_Preference (Data.Pref).Style_Bg);
      Style_Preference (Data.Pref).Style_Bg := new String'(Get_Color (C));
      Style_Preference (Data.Pref).Bg_Color := Get_Color (C);
   end Bg_Color_Changed;

   ---------------
   -- To_String --
   ---------------

   function To_String (Font, Fg, Bg : String) return String is
   begin
      return Font & '@' & Fg & '@' & Bg;
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
      Pack_Start (Get_Vbox (Dialog), F, Expand => True, Fill => True);

      Tmp := Add_Button (Dialog, Stock_Ok,     Gtk_Response_OK);
      Tmp := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);

      --  Must be done only after the widget is visible
      Result := Set_Font_Name (F, Get_Text (E));

      if Run (Dialog) = Gtk_Response_OK then
         Set_Text (E, Get_Font_Name (F));

         if Data.Pref.all in Font_Preference_Record'Class then
            Free (Font_Preference (Data.Pref).Font_Value);
            Free (Font_Preference (Data.Pref).Descr);
            Font_Preference (Data.Pref).Font_Value :=
              new String'(Get_Text (E));

         else
            Free (Style_Preference (Data.Pref).Style_Font);
            Free (Style_Preference (Data.Pref).Font_Descr);
            Style_Preference (Data.Pref).Style_Font :=
              new String'(Get_Text (E));
         end if;
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
   begin
      Gtk_New_Hbox (Box, Homogeneous => False);
      Gtk_New (Ent);
      Pack_Start (Box, Ent, Expand => True, Fill => True);

      Gtk_New (Button, Button_Label);
      Pack_Start (Box, Button, Expand => False, Fill => False);
      Preference_Handlers.Object_Connect
        (Button, Gtk.Button.Signal_Clicked,
         Preference_Handlers.To_Marshaller (Select_Font'Access),
         Slot_Object => Ent,
         User_Data => (Preferences_Manager (Manager), Pref));

      Return_Preference_Handlers.Connect
        (Ent, Signal_Focus_Out_Event, Font_Entry_Changed'Access,
         User_Data => (Preferences_Manager (Manager), Pref));
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Font_Entry'Access, Ent,
         User_Data => (Preferences_Manager (Manager), Pref));

      if From_Description (Desc) /= null then
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
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Tips);
      Spin : Gtk_Spin_Button;
      Adj  : Gtk_Adjustment;
   begin
      Gtk_New (Adj,
               Value => Gdouble (Pref.Int_Value),
               Lower => Gdouble (Pref.Int_Min_Value),
               Upper => Gdouble (Pref.Int_Max_Value),
               Step_Increment => 1.0,
               Page_Increment => 10.0);
      Gtk_New (Spin, Adj, 1.0, The_Digits => 0);
      Set_Editable (Spin, True);

      Preference_Handlers.Connect
        (Adj, Gtk.Adjustment.Signal_Value_Changed, Gint_Changed'Access,
         User_Data => (Preferences_Manager (Manager), Preference (Pref)));
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Gint'Access, Adj,
         User_Data => (Preferences_Manager (Manager), Preference (Pref)));

      return Gtk_Widget (Spin);
   end Edit;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Boolean_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Tips);
      Toggle : Gtk_Check_Button;
   begin
      Gtk_New (Toggle, -"Enabled");
      Widget_Callback.Connect
        (Toggle, Signal_Toggled, Toggled_Boolean'Access);
      Set_Active (Toggle, True); --  Forces a toggle
      Set_Active (Toggle, Pref.Bool_Value);

      Preference_Handlers.Connect
        (Toggle, Signal_Toggled, Boolean_Changed'Access,
         User_Data => (Preferences_Manager (Manager), Preference (Pref)));
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Boolean'Access, Toggle,
         User_Data => (Preferences_Manager (Manager), Preference (Pref)));
      return Gtk_Widget (Toggle);
   end Edit;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access String_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Tips);
      Ent  : Gtk_Entry;
      Text : Gtk_Text_View;
      Scrolled : Gtk_Scrolled_Window;
   begin
      if Pref.Multi_Line then
         Gtk_New (Scrolled);
         Set_Size_Request (Scrolled, -1, 200);
         Gtk_New (Text);
         Add (Scrolled, Text);
         Set_Text (Get_Buffer (Text), Pref.Str_Value.all);
         Preference_Handlers.Connect
           (Get_Buffer (Text), "changed", Text_Buffer_Changed'Access,
            User_Data => (Preferences_Manager (Manager), Preference (Pref)));
         Preference_Handlers.Object_Connect
           (Manager.Pref_Editor, Signal_Preferences_Changed,
            Update_Text_Buffer'Access, Get_Buffer (Text),
            User_Data => (Preferences_Manager (Manager), Preference (Pref)));

         return Gtk.Widget.Gtk_Widget (Scrolled);

      else
         Gtk_New (Ent);
         Set_Text (Ent, Pref.Str_Value.all);

         Preference_Handlers.Connect
           (Ent, Gtk.Editable.Signal_Insert_Text,
            Entry_Changed'Access,
            User_Data   => (Preferences_Manager (Manager), Preference (Pref)),
            After       => True);
         Preference_Handlers.Connect
           (Ent, Signal_Delete_Text,
            Entry_Changed'Access,
            User_Data   => (Preferences_Manager (Manager), Preference (Pref)),
            After       => True);
         Preference_Handlers.Object_Connect
           (Manager.Pref_Editor, Signal_Preferences_Changed,
            Update_Entry'Access,
            Ent,
            User_Data => (Preferences_Manager (Manager), Preference (Pref)));
         return Gtk_Widget (Ent);
      end if;
   end Edit;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Pref : in out String_Preference_Record) is
   begin
      Free (Pref.Str_Value);
      Free (Preference_Record (Pref));
   end Free;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Color_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Tips);
      Combo : Gtk_Color_Combo;
   begin
      Gtk_New (Combo);
      Set_Color (Combo, Get_Pref (Color_Preference (Pref)));

      Preference_Handlers.Connect
        (Combo, Signal_Color_Changed,
         Color_Changed'Access,
         User_Data   => (Preferences_Manager (Manager), Preference (Pref)));
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Color'Access, Combo,
         User_Data => (Preferences_Manager (Manager), Preference (Pref)));

      return Gtk_Widget (Combo);
   end Edit;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Pref : in out Color_Preference_Record) is
   begin
      Free (Pref.Color_Value);
      Free (Preference_Record (Pref));
   end Free;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Font_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Tips);
   begin
      return Gtk_Widget
        (Create_Box_For_Font
           (Manager, Preference (Pref),
            Get_Pref (Font_Preference (Pref)), -"Browse"));
   end Edit;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Pref : in out Font_Preference_Record) is
   begin
      Free (Pref.Font_Value);
      Free (Pref.Descr);
      Free (Preference_Record (Pref));
   end Free;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Key_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Tips);
      Ent    : Gtk_Entry;
      Modif  : Gdk_Modifier_Type;
      Key    : Gdk_Key_Type;
      Button : Gtk_Button;
      Box    : Gtk_Box;
   begin
      Gtk_New_Hbox (Box);
      Gtk_New (Ent);
      Set_Editable (Ent, False);
      Pack_Start (Box, Ent, Expand => True, Fill => True);

      Gtk_New (Button, -"Grab...");
      Pack_Start (Box, Button, Expand => False);

      Get_Pref (Key_Preference (Pref), Modif, Key);

      Append_Text (Ent, Image (Key, Modif));

      Widget_Callback.Object_Connect
        (Button, Gtk.Button.Signal_Clicked, Key_Grab'Access,
         Slot_Object => Ent);
      Preference_Handlers.Connect
        (Ent, Gtk.Editable.Signal_Insert_Text, Entry_Changed'Access,
         User_Data   => (Preferences_Manager (Manager), Preference (Pref)),
         After       => True);
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Entry'Access,
         Ent, User_Data => (Preferences_Manager (Manager), Preference (Pref)));

      return Gtk_Widget (Box);
   end Edit;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Style_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget
   is
      Event : Gtk_Event_Box;
      Box   : Gtk_Box;
      F     : constant Gtk_Box := Create_Box_For_Font
        (Manager, Preference (Pref),
         Get_Pref_Font (Style_Preference (Pref)), "...");
      Combo : Gtk_Color_Combo;

   begin
      Gtk_New (Event);
      Add (Event, F);
      Set_Tip
        (Tips, Event, -"Click on ... to display the font selector");
      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Box, Event, Expand => True, Fill => True);

      Gtk_New (Event);
      Gtk_New (Combo);
      Add (Event, Combo);
      Set_Tip (Tips, Event, -"Foreground color");
      Pack_Start (Box, Event, Expand => False);
      Set_Color (Combo, Get_Pref_Fg (Style_Preference (Pref)));
      Preference_Handlers.Connect
        (Combo, Signal_Color_Changed,
         Fg_Color_Changed'Access,
         User_Data   => (Preferences_Manager (Manager), Preference (Pref)));
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Fg'Access, Combo,
         User_Data => (Preferences_Manager (Manager), Preference (Pref)));

      Gtk_New (Event);
      Gtk_New (Combo);
      Add (Event, Combo);
      Set_Tip (Tips, Event, -"Background color");
      Pack_Start (Box, Event, Expand => False);
      Set_Color (Combo, Get_Pref_Bg (Style_Preference (Pref)));
      Preference_Handlers.Connect
        (Combo, Signal_Color_Changed,
         Bg_Color_Changed'Access,
         User_Data => (Preferences_Manager (Manager), Preference (Pref)));
      Preference_Handlers.Object_Connect
        (Manager.Pref_Editor, Signal_Preferences_Changed,
         Update_Bg'Access, Combo,
         User_Data => (Preferences_Manager (Manager), Preference (Pref)));

      return Gtk_Widget (Box);
   end Edit;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Pref : in out Style_Preference_Record) is
   begin
      Free (Pref.Style_Font);
      Free (Pref.Font_Descr);
      Free (Pref.Style_Fg);
      Free (Pref.Style_Bg);
      Free (Preference_Record (Pref));
   end Free;

   ----------------------
   -- Edit_Preferences --
   ----------------------

   procedure Edit_Preferences
     (Manager      : access Preferences_Manager_Record;
      Parent       : access Gtk.Window.Gtk_Window_Record'Class;
      On_Changed   : Action_Callback;
      Custom_Pages : Preferences_Page_Array)
   is
      Model             : Gtk_Tree_Store;
      Main_Table        : Gtk_Table;
      Current_Selection : Gtk_Widget;
      Title             : Gtk_Label;

      function Find_Or_Create_Page
        (Name : String; Widget : Gtk_Widget) return Gtk_Widget;
      --  Return the iterator in Model matching Name.
      --  If no such page already exists, then eithe Widget (if non null) is
      --  inserted for it, or a new table is created and inserted

      procedure Selection_Changed (Tree : access Gtk_Widget_Record'Class);
      --  Called when the selected page has changed.

      -------------------------
      -- Find_Or_Create_Page --
      -------------------------

      function Find_Or_Create_Page
        (Name : String; Widget : Gtk_Widget) return Gtk_Widget
      is
         Current     : Gtk_Tree_Iter := Null_Iter;
         Child       : Gtk_Tree_Iter;
         First, Last : Integer := Name'First;
         Table       : Gtk_Table;
         W           : Gtk_Widget;

      begin
         while First <= Name'Last loop
            Last := First;

            while Last <= Name'Last
              and then Name (Last) /= '/'
            loop
               Last := Last + 1;
            end loop;

            if Current = Null_Iter then
               Child := Get_Iter_First (Model);
            else
               Child := Children (Model, Current);
            end if;

            while Child /= Null_Iter
              and then Get_String (Model, Child, 0) /= Name (First .. Last - 1)
            loop
               Next (Model, Child);
            end loop;

            if Child = Null_Iter then
               if Widget = null then
                  Gtk_New (Table, Rows => 0, Columns => 2,
                           Homogeneous => False);
                  Set_Row_Spacings (Table, 1);
                  Set_Col_Spacings (Table, 5);
                  W := Gtk_Widget (Table);

               else
                  W := Widget;
               end if;

               Append (Model, Child, Current);
               Set (Model, Child, 0, Name (First .. Last - 1));
               Set (Model, Child, 1, GObject (W));

               Attach (Main_Table, W, 1, 2, 2, 3,
                       Ypadding => 0, Xpadding => 10);
               Set_Child_Visible (W, False);
            end if;

            Current := Child;

            First := Last + 1;
         end loop;

         return Gtk_Widget (Get_Object (Model, Current, 1));
      end Find_Or_Create_Page;

      -----------------------
      -- Selection_Changed --
      -----------------------

      procedure Selection_Changed (Tree : access Gtk_Widget_Record'Class) is
         Iter : Gtk_Tree_Iter;
         M    : Gtk_Tree_Model;
      begin
         if Current_Selection /= null then
            Set_Child_Visible (Current_Selection, False);
            Current_Selection := null;
         end if;

         Get_Selected (Get_Selection (Gtk_Tree_View (Tree)), M, Iter);

         if Iter /= Null_Iter then
            Current_Selection := Gtk_Widget (Get_Object (Model, Iter, 1));
            Set_Child_Visible (Current_Selection, True);
            Set_Text (Title, Get_String (Model, Iter, 0));
         end if;
      end Selection_Changed;

      Dialog     : Preferences_Editor;
      Frame      : Gtk_Frame;
      Table      : Gtk_Table;
      View       : Gtk_Tree_View;
      Col        : Gtk_Tree_View_Column;
      Render     : Gtk_Cell_Renderer_Text;
      Num        : Gint;
      Scrolled   : Gtk_Scrolled_Window;

      Signal_Parameters : constant Gtk.Object.Signal_Parameter_Types :=
        (1 => (1 => GType_None));

      Custom_Widgets : array (Custom_Pages'Range) of Gtk_Widget;

      Tmp        : Gtk_Widget;
      pragma Unreferenced (Tmp, Num);

      Saved      : Saved_Prefs_Data;
      Pref       : Preference;

      Had_Apply  : Boolean := False;
      Row        : Guint;
      Widget     : Gtk_Widget;
      Tips       : Gtk_Tooltips;
      Event      : Gtk_Event_Box;
      Label      : Gtk_Label;
      Separator  : Gtk_Separator;
      C          : Preferences_Maps.Cursor := First (Manager.Preferences);
   begin
      Save_Preferences (Manager, Saved);

      Dialog := new Preferences_Editor_Record;
      Initialize
        (Dialog => Dialog,
         Title  => -"Preferences",
         Parent => Gtk_Window (Parent),
         Flags  => Modal or Destroy_With_Parent);
      Set_Name (Dialog, "Preferences");  --  for the testsuite
      Set_Position (Dialog, Win_Pos_Mouse);
      Set_Default_Size (Dialog, 620, 400);
      Gtk_New (Tips);

      Gtk.Object.Initialize_Class_Record
        (Dialog,
         Signals      => Preferences_Editor_Signals,
         Class_Record => Preferences_Editor_Class_Record,
         Type_Name    => "PreferencesEditor",
         Parameters   => Signal_Parameters);
      Manager.Pref_Editor := Gtk_Widget (Dialog);

      Gtk_New (Main_Table, Rows => 3, Columns => 2, Homogeneous => False);
      Pack_Start (Get_Vbox (Dialog), Main_Table);

      Gtk_New (Frame);
      Attach (Main_Table, Frame, 0, 1, 0, 3);

      Create_Blue_Label (Title, Event);
      Attach (Main_Table, Event, 1, 2, 0, 1, Yoptions => 0);

      Gtk_New_Hseparator (Separator);
      Attach (Main_Table, Separator, 1, 2, 1, 2, Yoptions => 0, Ypadding => 1);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
      Add (Frame, Scrolled);

      Gtk_New (Model, (0 => GType_String, 1 => GType_Object));
      Gtk_New (View, Model);
      Unref (Model);
      Set_Headers_Visible (View, False);

      Gtk_New (Col);
      Num := Append_Column (View, Col);
      Gtk_New (Render);
      Pack_Start (Col, Render, Expand => True);
      Add_Attribute (Col, Render, "text", 0);

      Widget_Callback.Object_Connect
        (Get_Selection (View), Gtk.Tree_Selection.Signal_Changed,
         Selection_Changed'Unrestricted_Access,
         View);

      Add (Scrolled, View);

      --  Add all custom pages first

      for P in Custom_Pages'Range loop
         Widget := Create (Custom_Pages (P));

         if Widget /= null then
            Custom_Widgets (P) := Find_Or_Create_Page
              (Name (Custom_Pages (P)), Widget);
         end if;
      end loop;

      --  Then add all implicitely defined pages

      while Has_Element (C) loop
         Pref := Element (C);

         if Pref.Page /= null then
            Table := Gtk_Table (Find_Or_Create_Page (Pref.Page.all, null));
            Row := Get_Property (Table, N_Rows_Property);
            Resize (Table, Rows => Row + 1, Columns => 2);

            Gtk_New (Event);
            Gtk_New (Label, Pref.Label.all);
            Add (Event, Label);
            Set_Tip (Tips, Event, Pref.Doc.all);
            Set_Alignment (Label, 0.0, 0.5);
            Attach (Table, Event, 0, 1, Row, Row + 1,
                    Xoptions => Fill, Yoptions => 0);

            Widget := Edit
              (Pref               => Pref,
               Manager            => Manager,
               Tips               => Tips);

            if Widget /= null then
               Attach (Table, Widget, 1, 2, Row, Row + 1, Yoptions => 0);
            end if;
         end if;

         Next (C);
      end loop;

      Tmp := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Tmp := Add_Button (Dialog, Stock_Apply, Gtk_Response_Apply);
      Tmp := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Enable (Tips);

      Show_All (Dialog);

      loop
         case Run (Dialog) is
            when Gtk_Response_OK =>
               for P in Custom_Pages'Range loop
                  if Custom_Widgets (P) /= null then
                     Validate (Custom_Pages (P), Custom_Widgets (P));
                  end if;
               end loop;

               Manager.Pref_Editor := null;
               Destroy (Saved);
               Destroy (Dialog);

               if On_Changed /= null then
                  On_Changed (Manager);
               end if;

               exit;

            when Gtk_Response_Apply =>
               for P in Custom_Pages'Range loop
                  if Custom_Widgets (P) /= null then
                     Validate (Custom_Pages (P), Custom_Widgets (P));
                  end if;
               end loop;

               if On_Changed /= null then
                  On_Changed (Manager);
               end if;

               Had_Apply := True;

            when others =>  --  including Cancel
               for P in Custom_Pages'Range loop
                  if Custom_Widgets (P) /= null then
                     Undo (Custom_Pages (P), Custom_Widgets (P));
                  end if;
               end loop;

               Manager.Pref_Editor := null;
               Destroy (Dialog);
               Restore_Preferences (Saved);

               if Had_Apply and then On_Changed /= null then
                  On_Changed (Manager);
               end if;

               exit;
         end case;
      end loop;

      Destroy (Tips);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Destroy (Dialog);
   end Edit_Preferences;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences
     (Manager : access Preferences_Manager_Record;
      Saved   : out Saved_Prefs_Data)
   is
      C    : Preferences_Maps.Cursor := First (Manager.Preferences);
      Pref : Preference;
   begin
      Saved := new Saved_Prefs_Data_Record;
      Saved.Manager := Preferences_Manager (Manager);

      while Has_Element (C) loop
         Pref := Element (C);
         Include
           (Saved.Preferences,
            Pref.Name.all,
            String'(Get_Pref (Pref)));
         Next (C);
      end loop;
   end Save_Preferences;

   -------------------------
   -- Restore_Preferences --
   -------------------------

   procedure Restore_Preferences (Saved : Saved_Prefs_Data) is
      C    : Preferences_Maps.Cursor := First (Saved.Manager.Preferences);
      Pref : Preference;
   begin
      while Has_Element (C) loop
         Pref := Element (C);
         begin
            Set_Pref (Pref,
                      Manager => null,
                      Value   => Element (Saved.Preferences, Pref.Name.all));
         exception
            when Constraint_Error =>
               null;
         end;

         Next (C);
      end loop;
   end Restore_Preferences;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Data : in out Saved_Prefs_Data) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Saved_Prefs_Data_Record, Saved_Prefs_Data);
   begin
      Unchecked_Free (Data);
   end Destroy;

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
   -- Get_Editor --
   ----------------

   function Get_Editor
     (Manager : access Preferences_Manager_Record)
      return Gtk.Widget.Gtk_Widget is
   begin
      return Manager.Pref_Editor;
   end Get_Editor;

end Default_Preferences;
