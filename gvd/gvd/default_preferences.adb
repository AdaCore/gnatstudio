-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;     use Interfaces.C.Strings;

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Properties;          use Glib.Properties;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Glib.Xml_Int;             use Glib.Xml_Int;
with XML_Parsers;
with Gdk.Color;                use Gdk.Color;
with Gdk.Font;                 use Gdk.Font;
with Gdk.Keyval;               use Gdk.Keyval;
with Gdk.Types;                use Gdk.Types;
with Gtk.Adjustment;           use Gtk.Adjustment;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Combo;                use Gtk.Combo;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Event_Box;            use Gtk.Event_Box;
with Gtk.Font_Selection;       use Gtk.Font_Selection;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Label;                use Gtk.Label;
with Gtk.List;                 use Gtk.List;
with Gtk.List_Item;            use Gtk.List_Item;
with Gtk.Object;               use Gtk.Object;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Separator;            use Gtk.Separator;
with Gtk.Spin_Button;          use Gtk.Spin_Button;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Style;                use Gtk.Style;
with Gtk.Table;                use Gtk.Table;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
with Gtk.Tooltips;             use Gtk.Tooltips;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Gtkada.Handlers;          use Gtkada.Handlers;
with GVD.Color_Combo;          use GVD.Color_Combo;
with Pango.Font;               use Pango.Font;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with GUI_Utils;                use GUI_Utils;
with Odd_Intl;                 use Odd_Intl;
with Pango.Layout;             use Pango.Layout;
with String_Hash;
with Traces;                   use Traces;
with Case_Handling;            use Case_Handling;

package body Default_Preferences is

   Me : constant Debug_Handle := Create ("Default_Prefs");

   Fallback_Font : constant String := "Sans 10";
   --  The name of a font that should always work on all systems. This is used
   --  in case the user-specified fonts can not be found.

   type Pref_Description is record
      Value : GNAT.OS_Lib.String_Access;
      Page  : GNAT.OS_Lib.String_Access;
      Param : Glib.Param_Spec;
      --  The definition of the preference. This can be null if the preference
      --  has not been registered yet, but its value has been read in the
      --  preferences file.

      Descr : Pango.Font.Pango_Font_Description;
      Index : Natural;
   end record;

   type Preferences_Editor_Record is new Gtk_Dialog_Record with null record;
   type Preferences_Editor is access all Preferences_Editor_Record'Class;
   Preferences_Editor_Class_Record : Gtk.Object.GObject_Class :=
     Gtk.Object.Uninitialized_Class;
   Preferences_Editor_Signals : constant chars_ptr_array :=
     (1 => New_String ("preferences_changed"));


   use Pref_Hash.String_Hash_Table;

   package Param_Handlers is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Pref_Description_Access);
   package Return_Param_Handlers is new Gtk.Handlers.User_Return_Callback
     (Glib.Object.GObject_Record, Boolean, Pref_Description_Access);

   generic
      type Param is private;
      P : Param_Spec;
      type Result (<>) is private;
      Val_Type : GType;
      with function Convert (S : String) return Result;
      with function Default (P : Param) return Result is <>;
   function Generic_Get_Pref
     (Manager : access Preferences_Manager_Record'Class; Pref : Param)
      return Result;
   --  ???

   function Clone
     (Info : Pref_Description_Access) return Pref_Description_Access;
   --  Return a deep copy  of Info.
   --  Return value must be freed by caller.

   procedure Toggled_Boolean (Toggle : access Gtk_Widget_Record'Class);
   --  Called when a toggle button has changed, to display the appropriate text
   --  in it.

   procedure Enum_Changed
     (Combo : access GObject_Record'Class;
      Data  : Pref_Description_Access);
   --  Called when an enumeration preference has been changed.

   procedure Gint_Changed
     (Adj  : access GObject_Record'Class;
      Data : Pref_Description_Access);
   --  Called when a Gint preference has been changed.

   procedure Update_Gint
     (Adj  : access GObject_Record'Class;
      Data : Pref_Description_Access);
   --  Called when the preference Data has changed, to update Adj

   procedure Boolean_Changed
     (Toggle : access GObject_Record'Class;
      Data   : Pref_Description_Access);
   --  Called when a boolean preference has been changed.

   procedure Update_Boolean
     (Toggle : access GObject_Record'Class;
      Data   : Pref_Description_Access);
   --  Called when the preference Data has changed, to update Toggle

   procedure Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Pref_Description_Access);
   --  Called when the text in an entry field has changed.

   procedure Update_Entry
     (Ent  : access GObject_Record'Class;
      Data : Pref_Description_Access);
   --  Called when the preference Data has changed, to update Ent

   function Font_Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Pref_Description_Access) return Boolean;
   --  Called when the entry for a font selection has changed.

   procedure Reset_Font (Ent : access Gtk_Widget_Record'Class);
   --  Update the font used for the entry Ent, based on its contents.

   procedure Color_Changed
     (Combo : access GObject_Record'Class;
      Data  : Pref_Description_Access);
   --  Called when a color has changed.

   procedure Update_Font_Entry
     (Ent  : access GObject_Record'Class;
      Data : Pref_Description_Access);
   procedure Update_Fg
     (Combo  : access GObject_Record'Class;
      Data : Pref_Description_Access);
   procedure Update_Bg
     (Combo  : access GObject_Record'Class;
      Data : Pref_Description_Access);
   --  Called when the preference Data has changed, to update Combo for the
   --  Font, foreground or background of the preference

   procedure Update_Color
     (Combo  : access GObject_Record'Class;
      Data : Pref_Description_Access);
   --  Update the contents of the combo based on the color found in Data

   procedure Bg_Color_Changed
     (Combo : access GObject_Record'Class; Data  : Pref_Description_Access);
   --  Called when the background color of a style has changed.

   procedure Fg_Color_Changed
     (Combo : access GObject_Record'Class; Data  : Pref_Description_Access);
   --  Called when the foreground color of a style has changed.

   function Value (S : String) return String;
   --  Return the string as is (used for instantiation of Generic_Get_Pref)

   procedure Select_Font
     (Ent : access GObject_Record'Class; Data : Pref_Description_Access);
   --  Open a dialog to select a new font

   procedure Key_Grab (Ent : access Gtk_Widget_Record'Class);
   --  Callback for the "grab" button when editing a key preference

   function To_String (Font, Fg, Bg : String) return String;
   function Style_Token (Value : String; Num : Positive) return String;
   --  Handling of Param_Spec_Style

   procedure Get_Font
     (Info : in out Pref_Description_Access;
      Desc : in out Pango_Font_Description);
   --  Check that Desc is a valid font, and associate it with the node N.

   function Create_Box_For_Font
     (Manager      : access Preferences_Manager_Record;
      N            : Pref_Description_Access;
      Desc         : Pango_Font_Description;
      Button_Label : String) return Gtk_Box;
   --  Create a box suitable for editing fonts

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Pref_Description_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Pref_Description, Pref_Description_Access);
   begin
      GNAT.OS_Lib.Free (Data.Value);
      Free (Data.Page);
      Unchecked_Free (Data);
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone
     (Info : Pref_Description_Access) return Pref_Description_Access
   is
      Clone : constant Pref_Description_Access := new Pref_Description'
        (Page  => new String'(Info.Page.all),
         Value => null,
         Param => Info.Param,
         Index => Info.Index,
         Descr => null);
   begin
      if Info.Value /= null then
         Clone.Value := new String'(Info.Value.all);
      end if;

      return Clone;
   end Clone;

   -----------
   -- Value --
   -----------

   function Value (S : String) return String is
   begin
      return S;
   end Value;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Manager : in out Preferences_Manager_Record) is
   begin
      Reset (Manager.Preferences);
   end Destroy;

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

   ----------------
   -- Gnew_Color --
   ----------------

   function Gnew_Color
     (Name, Nick, Blurb : String;
      Default           : String;
      Flags             : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec_Color
   is
      P : constant Param_Spec_Color := Param_Spec_Color
        (Gnew_String (Name, Nick, Blurb, Default, Flags));
   begin
      Set_Value_Type (Param_Spec (P), Gdk.Color.Gdk_Color_Type);
      return P;
   end Gnew_Color;

   ---------------
   -- Gnew_Font --
   ---------------

   function Gnew_Font
     (Name, Nick, Blurb : String;
      Default           : String;
      Flags             : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec_Font
   is
      P : constant Param_Spec_Font := Param_Spec_Font
        (Gnew_String (Name, Nick, Blurb, Default, Flags));
   begin
      Set_Value_Type (Param_Spec (P), Pango.Font.Get_Type);
      return P;
   end Gnew_Font;

   --------------
   -- Gnew_Key --
   --------------

   function Gnew_Key
     (Name, Nick, Blurb : String;
      Default_Modifier  : Gdk.Types.Gdk_Modifier_Type;
      Default_Key       : Gdk.Types.Gdk_Key_Type;
      Flags             : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec_Key
   is
      P : constant Param_Spec_Key := Param_Spec_Key
        (Gnew_String (Name, Nick, Blurb,
                      Image (Default_Key, Default_Modifier),
                      Flags));
   begin
      Set_Value_Type (Param_Spec (P), Gdk.Keyval.Get_Type);
      return P;
   end Gnew_Key;

   ----------------
   -- Gnew_Style --
   ----------------

   function Gnew_Style
     (Name, Nick, Blurb : String;
      Default_Font      : String;
      Default_Fg        : String;
      Default_Bg        : String;
      Flags             : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec_Style
   is
      P : constant Param_Spec_Style := Param_Spec_Style
        (Gnew_String (Name, Nick, Blurb,
                      To_String (Default_Font, Default_Fg, Default_Bg),
                      Flags));
   begin
      Set_Value_Type (Param_Spec (P), Gtk.Style.Get_Type);
      return P;
   end Gnew_Style;

   -----------------------
   -- Register_Property --
   -----------------------

   procedure Register_Property
     (Manager : access Preferences_Manager_Record;
      Param   : Glib.Param_Spec;
      Page    : String)
   is
      Info : Pref_Description_Access :=
        Get (Manager.Preferences, Pspec_Name (Param));
   begin
      if Info = null then
         Info := new Pref_Description'
           (Page  => new String'(Page),
            Param => Param,
            Descr => null,
            Index => Manager.Current_Index,
            Value => null);
         Set (Manager.Preferences, Pspec_Name (Param), Info);
      else
         Free (Info.Page);
         Info.Page := new String'(Page);
         Info.Index := Manager.Current_Index;
         Info.Param := Param;
      end if;

      Manager.Current_Index := Manager.Current_Index + 1;
   end Register_Property;

   ---------
   -- Get --
   ---------

   function Get
     (Manager : access Preferences_Manager_Record;
      Name    : String)
      return Param_Spec
   is
      Info : constant Pref_Description_Access :=
               Get (Manager.Preferences, Name);
   begin
      if Info = null then
         return null;
      else
         return Info.Param;
      end if;
   end Get;

   ----------------------
   -- Generic_Get_Pref --
   ----------------------

   function Generic_Get_Pref
     (Manager : access Preferences_Manager_Record'Class; Pref : Param)
      return Result
   is
      N : constant Pref_Description_Access :=
        Get (Manager.Preferences, Pspec_Name (P));
   begin
      if N /= null
        and then (Value_Type (P) = Val_Type
                  or else Fundamental (Value_Type (P)) = Val_Type)
        and then N.Value /= null
      then
         return Convert (N.Value.all);
      else
         return Default (Pref);
      end if;

   exception
      when Constraint_Error =>
         return Default (Pref);
   end Generic_Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Manager : access Preferences_Manager_Record; Pref : Param_Spec_Int)
      return Gint
   is
      function Internal is new Generic_Get_Pref
        (Param_Spec_Int, Param_Spec (Pref), Gint, GType_Int, Gint'Value);
   begin
      return Internal (Manager, Pref);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Manager : access Preferences_Manager_Record; Pref : Param_Spec_Boolean)
      return Boolean
   is
      function Internal is new Generic_Get_Pref
        (Param_Spec_Boolean, Param_Spec (Pref), Boolean, GType_Boolean,
         Boolean'Value);
   begin
      return Internal (Manager, Pref);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Manager : access Preferences_Manager_Record; Pref : Param_Spec_Enum)
      return Gint
   is
      function Internal is new Generic_Get_Pref
        (Param_Spec_Enum, Param_Spec (Pref), Gint, GType_Enum, Gint'Value);
   begin
      return Internal (Manager, Pref);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Manager : access Preferences_Manager_Record; Pref : Param_Spec_String)
      return String
   is
      function Internal is new Generic_Get_Pref
        (Param_Spec_String, Param_Spec (Pref), String, GType_String, Value);
   begin
      return Internal (Manager, Pref);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Manager : access Preferences_Manager_Record;
      Pref   : Param_Spec_Color) return Gdk.Color.Gdk_Color
   is
      function Internal is new Generic_Get_Pref
        (Param_Spec_Color, Param_Spec (Pref), String, Gdk_Color_Type, Value);
      S : constant String := Internal (Manager, Pref);
      Color : Gdk_Color;
   begin
      Color := Parse (S);
      Alloc (Gtk.Widget.Get_Default_Colormap, Color);
      return Color;

   exception
      when Wrong_Color =>
         Color := Black (Get_Default_Colormap);
         return Color;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   procedure Get_Pref
     (Manager  : access Preferences_Manager_Record;
      Pref     : Param_Spec_Key;
      Modifier : out Gdk_Modifier_Type;
      Key      : out Gdk_Key_Type)
   is
      Info : constant Pref_Description_Access :=
        Get (Manager.Preferences, Pspec_Name (Param_Spec (Pref)));
   begin
      if Info /= null
        and then Info.Value /= null
      then
         Value (Info.Value.all, Key, Modifier);
      else
         Value (Default (Param_Spec_String (Pref)), Key, Modifier);
      end if;
   end Get_Pref;

   -------------------
   -- Get_Pref_Font --
   -------------------

   function Get_Pref_Font
     (Manager : access Preferences_Manager_Record;
      Pref    : Param_Spec_Style) return Pango_Font_Description
   is
      Info : Pref_Description_Access :=
        Get (Manager.Preferences, Pspec_Name (Param_Spec (Pref)));
      Desc : Pango_Font_Description;
   begin
      if Info /= null and then Info.Value /= null then
         if Info.Descr /= null then
            return Info.Descr;
         end if;

         Desc := From_String (Style_Token (Info.Value.all, 1));
      else
         Desc := From_String
           (Style_Token (Default (Param_Spec_String (Pref)), 1));
      end if;

      Get_Font (Info, Desc);
      return Desc;
   end Get_Pref_Font;

   --------------
   -- Get_Font --
   --------------

   procedure Get_Font
     (Info : in out Pref_Description_Access;
      Desc : in out Pango_Font_Description)
   is
      use type Gdk.Gdk_Font;
   begin
      --  ??? We have a memory leak if Info is null, but seems impossible
      if Info = null then
         return;
      end if;

      --  Check that the font exists, or use a default, to avoid crashes
      if From_Description (Desc) = null then
         Free (Desc);
         Desc := From_String (Fallback_Font);
      end if;

      Info.Descr := Desc;
   end Get_Font;

   -----------------
   -- Get_Pref_Fg --
   -----------------

   function Get_Pref_Fg
     (Manager : access Preferences_Manager_Record;
      Pref    : Param_Spec_Style) return Gdk.Color.Gdk_Color
   is
      Info : constant Pref_Description_Access  :=
        Get (Manager.Preferences, Pspec_Name (Param_Spec (Pref)));
      Color : Gdk_Color;
   begin
      if Info = null or else Info.Value = null then
         Color := Parse (Style_Token (Default (Param_Spec_String (Pref)), 2));
      else
         Color := Parse (Style_Token (Info.Value.all, 2));
      end if;

      Alloc (Gtk.Widget.Get_Default_Colormap, Color);
      return Color;
   end Get_Pref_Fg;

   -----------------
   -- Get_Pref_Bg --
   -----------------

   function Get_Pref_Bg
     (Manager : access Preferences_Manager_Record;
      Pref    : Param_Spec_Style) return Gdk.Color.Gdk_Color
   is
      Info : constant Pref_Description_Access :=
        Get (Manager.Preferences, Pspec_Name (Param_Spec (Pref)));
      Color : Gdk_Color;
   begin
      if Info = null or else Info.Value = null then
         Color := Parse (Style_Token (Default (Param_Spec_String (Pref)), 3));
      else
         Color := Parse (Style_Token (Info.Value.all, 3));
      end if;

      Alloc (Gtk.Widget.Get_Default_Colormap, Color);
      return Color;
   end Get_Pref_Bg;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Manager : access Preferences_Manager_Record;
      Pref    : Param_Spec_Font) return Pango.Font.Pango_Font_Description
   is
      Info : Pref_Description_Access :=
        Get (Manager.Preferences, Pspec_Name (Param_Spec (Pref)));
      Desc : Pango_Font_Description;
   begin
      if Info /= null
        and then Info.Value /= null
      then
         if Info.Descr /= null then
            return Info.Descr;
         end if;

         Desc := From_String (Info.Value.all);
      else
         Desc := From_String (Default (Pref));
      end if;

      Get_Font (Info, Desc);
      return Desc;
   end Get_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Manager : access Preferences_Manager_Record;
      Name    : String;
      Value   : String)
   is
      Info : Pref_Description_Access := Get (Manager.Preferences, Name);
   begin
      if Info = null then
         --  Create a dummy entry
         Info := new Pref_Description'
           (Page  => new String'("General"),
            Param => null,
            Descr => null,
            Index => Manager.Current_Index,
            Value => null);
         Manager.Current_Index := Manager.Current_Index + 1;
         Set (Manager.Preferences, Name, Info);
      end if;

      Free (Info.Value);
      Info.Value := new String'(Value);

      if Manager.Pref_Editor /= null then
         Widget_Callback.Emit_By_Name
           (Manager.Pref_Editor, "preferences_changed");
      end if;
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Manager : access Preferences_Manager_Record;
      Name    : String;
      Value   : Gint) is
   begin
      Set_Pref (Manager, Name, Gint'Image (Value));
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Manager  : access Preferences_Manager_Record;
      Name     : String;
      Modifier : Gdk_Modifier_Type;
      Key      : Gdk_Key_Type) is
   begin
      Set_Pref (Manager, Name, Image (Key, Modifier));
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Manager      : access Preferences_Manager_Record;
      Name         : String;
      Font, Fg, Bg : String) is
   begin
      Set_Pref (Manager, Name, To_String (Font, Fg, Bg));
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Manager : access Preferences_Manager_Record;
      Name    : String;
      Value   : Boolean) is
   begin
      Set_Pref (Manager, Name, Boolean'Image (Value));
   end Set_Pref;

   --------------
   -- Get_Page --
   --------------

   function Get_Page
     (Manager : access Preferences_Manager_Record;
      Param   : Param_Spec) return String
   is
      Info : constant Pref_Description_Access :=
        Get (Manager.Preferences, Pspec_Name (Param));
   begin
      if Info = null then
         return "";
      else
         return Info.Page.all;
      end if;
   end Get_Page;

   ----------------------
   -- Load_Preferences --
   ----------------------

   procedure Load_Preferences
     (Manager : access  Preferences_Manager_Record; File_Name : String)
   is
      File, Node : Node_Ptr;
      Err : String_Access;
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
                  Set_Pref (Manager => Manager,
                            Name    => Node.Tag.all,
                            Value   => Node.Value.all);
                  Node := Node.Next;
               end loop;
            else
               Trace (Me, "Load new style preferences");
               while Node /= null loop
                  if Node.Tag.all = "pref" then
                     declare
                        Name : constant String := Get_Attribute (Node, "name");
                     begin
                        Set_Pref (Manager => Manager,
                                  Name    => Name,
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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Load_Preferences;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences
     (Manager : access Preferences_Manager_Record; File_Name : String)
   is
      Iter : Iterator;
      File : File_Type;
      Info : Pref_Description_Access;
   begin
      Create (File, Out_File, File_Name);
      Put_Line (File, "<?xml version=""1.0""?>");
      Put_Line (File, "<Prefs>");

      Get_First (Manager.Preferences, Iter);
      loop
         Info := Get_Element (Iter);
         exit when Info = null;

         if Info.Value /= null then
            Put_Line (File, "   <pref name=""" & Get_Key (Iter)
                      & """ >" & Info.Value.all & "</pref>");
         end if;

         Get_Next (Manager.Preferences, Iter);
      end loop;

      Put_Line (File, "</Prefs>");
   end Save_Preferences;

   --------------------
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
   -- Enum_Changed --
   ------------------

   procedure Enum_Changed
     (Combo : access GObject_Record'Class;
      Data  : Pref_Description_Access)
   is
      C : constant Gtk_Combo := Gtk_Combo (Combo);
   begin
      Free (Data.Value);
      Data.Value := new String'(Integer'Image (Get_Index_In_List (C)));
   end Enum_Changed;

   ------------------
   -- Gint_Changed --
   ------------------

   procedure Gint_Changed
     (Adj  : access GObject_Record'Class;
      Data : Pref_Description_Access)
   is
      A : constant Gtk_Adjustment := Gtk_Adjustment (Adj);
   begin
      Free (Data.Value);
      Data.Value := new String'(Gint'Image (Gint (Get_Value (A))));
   end Gint_Changed;

   -----------------
   -- Update_Gint --
   -----------------

   procedure Update_Gint
     (Adj  : access GObject_Record'Class;
      Data : Pref_Description_Access) is
   begin
      if Data.Value /= null then
         Set_Value (Gtk_Adjustment (Adj), Gdouble'Value (Data.Value.all));
      end if;
   end Update_Gint;

   ---------------------
   -- Boolean_Changed --
   ---------------------

   procedure Boolean_Changed
     (Toggle : access GObject_Record'Class;
      Data   : Pref_Description_Access)
   is
      T : constant Gtk_Toggle_Button := Gtk_Toggle_Button (Toggle);
   begin
      Free (Data.Value);
      Data.Value := new String'(Boolean'Image (Get_Active (T)));
   end Boolean_Changed;

   --------------------
   -- Update_Boolean --
   --------------------

   procedure Update_Boolean
     (Toggle : access GObject_Record'Class;
      Data   : Pref_Description_Access) is
   begin
      if Data.Value /= null then
         Set_Active
           (Gtk_Toggle_Button (Toggle), Boolean'Value (Data.Value.all));
      end if;
   end Update_Boolean;

   -------------------
   -- Entry_Changed --
   -------------------

   procedure Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Pref_Description_Access)
   is
      E : constant Gtk_Entry := Gtk_Entry (Ent);
   begin
      Free (Data.Value);
      Data.Value := new String'(Get_Text (E));
   end Entry_Changed;

   ------------------
   -- Update_Entry --
   ------------------

   procedure Update_Entry
     (Ent  : access GObject_Record'Class;
      Data : Pref_Description_Access) is
   begin
      if Data.Value /= null then
         if Fundamental (Value_Type (Param_Spec (Data.Param))) =
           GType_Enum
         then
            Set_Text
              (Gtk_Entry (Ent),
               Name (Get_Value (Enumeration (Param_Spec_Enum (Data.Param)),
                                Gint'Value (Data.Value.all))));
         else
            Set_Text (Gtk_Entry (Ent), Data.Value.all);
         end if;
      end if;
   end Update_Entry;

   ----------------
   -- Reset_Font --
   ----------------

   procedure Reset_Font (Ent : access Gtk_Widget_Record'Class) is
      E    : constant Gtk_Entry := Gtk_Entry (Ent);
      Desc : constant Pango_Font_Description := From_String (Get_Text (E));
   begin
      --  Also set the context, so that every time the pango layout is
      --  recreated by the entry (key press,...), we still use the correct
      --  font.
      --  ??? Right now, the mechanism described above will cause gtk to
      --  crash when Desc doesn't correspond to a drawable font, therefore
      --  the following code is commented out.
      --  Set_Font_Description (Get_Pango_Context (E), Desc);

      Set_Font_Description (Get_Layout (E), Desc);
   end Reset_Font;

   ------------------------
   -- Font_Entry_Changed --
   ------------------------

   function Font_Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Pref_Description_Access) return Boolean
   is
      E     : constant Gtk_Entry := Gtk_Entry (Ent);
      Value : String_Access;
   begin
      Data.Descr := null;

      --  Cannot free Data.Value yet, since it will be used in the
      --  test below to know whether a previous value has been set.

      if Value_Type (Data.Param) = Pango.Font.Get_Type then
         Free (Data.Value);
         Data.Value := new String'(Get_Text (E));

      elsif Data.Value /= null then
         Value := Data.Value;
         Data.Value := new String'
              (To_String (Font => Get_Text (E),
                          Fg   => Style_Token (Data.Value.all, 2),
                          Bg   => Style_Token (Data.Value.all, 3)));
         Free (Value);

      else
         Free (Data.Value);
         Data.Value := new String'
           (To_String (Font => Get_Text (E),
                       Fg   => Style_Token
                         (Default (Param_Spec_String (Data.Param)), 2),
                       Bg   => Style_Token
                         (Default (Param_Spec_String (Data.Param)), 3)));
      end if;

      Reset_Font (E);
      return False;
   end Font_Entry_Changed;

   --------------
   -- Key_Grab --
   --------------

   procedure Key_Grab (Ent : access Gtk_Widget_Record'Class) is
      E      : constant Gtk_Entry := Gtk_Entry (Ent);
      Key    : Gdk.Types.Gdk_Key_Type;
      Mods   : Gdk.Types.Gdk_Modifier_Type;
   begin
      GUI_Utils.Key_Grab (E, Key, Mods);
      Set_Text (E, Image (Key, Mods));
   end Key_Grab;

   -------------------
   -- Color_Changed --
   -------------------

   procedure Color_Changed
     (Combo : access GObject_Record'Class;
      Data  : Pref_Description_Access)
   is
      C : constant Gvd_Color_Combo := Gvd_Color_Combo (Combo);
   begin
      Free (Data.Value);
      Data.Value := new String'(Get_Color (C));
   end Color_Changed;

   ---------------
   -- Update_Fg --
   ---------------

   procedure Update_Fg
     (Combo : access GObject_Record'Class;
      Data  : Pref_Description_Access)
   is
      Color : Gdk_Color;
   begin
      if Data.Value /= null then
         Color := Parse (Style_Token (Data.Value.all, 2));
         Alloc (Gtk.Widget.Get_Default_Colormap, Color);
         Set_Color (Gvd_Color_Combo (Combo), Color);
      end if;
   end Update_Fg;

   ---------------
   -- Update_Bg --
   ---------------

   procedure Update_Bg
     (Combo : access GObject_Record'Class;
      Data  : Pref_Description_Access)
   is
      Color : Gdk_Color;
   begin
      if Data.Value /= null then
         Color := Parse (Style_Token (Data.Value.all, 3));
         Alloc (Gtk.Widget.Get_Default_Colormap, Color);
         Set_Color (Gvd_Color_Combo (Combo), Color);
      end if;
   end Update_Bg;

   -----------------------
   -- Update_Font_Entry --
   -----------------------

   procedure Update_Font_Entry
     (Ent  : access GObject_Record'Class;
      Data : Pref_Description_Access) is
   begin
      if Data.Value /= null then
         Set_Text (Gtk_Entry (Ent), Style_Token (Data.Value.all, 1));
      end if;
   end Update_Font_Entry;

   ------------------
   -- Update_Color --
   ------------------

   procedure Update_Color
     (Combo : access GObject_Record'Class;
      Data  : Pref_Description_Access)
   is
      Color : Gdk_Color;
   begin
      if Data.Value /= null then
         Color := Parse (Data.Value.all);
         Alloc (Gtk.Widget.Get_Default_Colormap, Color);
         Set_Color (Gvd_Color_Combo (Combo), Color);
      end if;
   end Update_Color;

   ----------------------
   -- Fg_Color_Changed --
   ----------------------

   procedure Fg_Color_Changed
     (Combo : access GObject_Record'Class;
      Data  : Pref_Description_Access)
   is
      C : constant Gvd_Color_Combo := Gvd_Color_Combo (Combo);
   begin
      if Data.Value = null then
         declare
            V : constant String := Default (Param_Spec_String (Data.Param));
         begin
            Data.Value := new String'
              (To_String (Font => Style_Token (V, 1),
                          Fg   => Get_Color (C),
                          Bg   => Style_Token (V, 3)));
         end;

      else
         declare
            V : constant String := To_String
              (Font => Style_Token (Data.Value.all, 1),
               Fg   => Get_Color (C),
               Bg   => Style_Token (Data.Value.all, 3));
         begin
            Free (Data.Value);
            Data.Value := new String'(V);
         end;
      end if;
   end Fg_Color_Changed;

   ----------------------
   -- Bg_Color_Changed --
   ----------------------

   procedure Bg_Color_Changed
     (Combo : access GObject_Record'Class;
      Data  : Pref_Description_Access)
   is
      C : constant Gvd_Color_Combo := Gvd_Color_Combo (Combo);
   begin
      if Data.Value = null then
         declare
            V : constant String := Default (Param_Spec_String (Data.Param));
         begin
            Data.Value := new String'
              (To_String (Font => Style_Token (V, 1),
                          Fg   => Style_Token (V, 2),
                          Bg   => Get_Color (C)));
         end;

      else
         declare
            V : constant String := To_String
              (Font => Style_Token (Data.Value.all, 1),
               Fg   => Style_Token (Data.Value.all, 2),
               Bg   => Get_Color (C));
         begin
            Free (Data.Value);
            Data.Value := new String'(V);
         end;
      end if;
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
      N : Natural := Num;
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

      return "";
   end Style_Token;

   -----------------
   -- Select_Font --
   -----------------

   procedure Select_Font
     (Ent  : access GObject_Record'Class;
      Data : Pref_Description_Access)
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
      Result := Set_Font_Name (F, Get_Text (E));
      Pack_Start (Get_Vbox (Dialog), F, Expand => True, Fill => True);

      Tmp := Add_Button (Dialog, Stock_Ok,     Gtk_Response_OK);
      Tmp := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         Set_Text (E, Get_Font_Name (F));

         if Value_Type (Data.Param) = Pango.Font.Get_Type then
            Free (Data.Value);
            Data.Value := new String'(Get_Text (E));
         else
            if Data.Value /= null then
               declare
                  V : constant String := To_String
                    (Font => Get_Text (E),
                     Fg   => Style_Token (Data.Value.all, 2),
                     Bg   => Style_Token (Data.Value.all, 3));
               begin
                  Free (Data.Value);
                  Data.Value := new String'(V);
               end;

            else
               Data.Value := new String'
                 (To_String (Font => Get_Text (E),
                             Fg   => Style_Token
                               (Default (Param_Spec_String (Data.Param)), 2),
                             Bg   => Style_Token
                               (Default (Param_Spec_String (Data.Param)), 3)));
            end if;
         end if;
         Reset_Font (E);
      end if;

      Destroy (Dialog);
   end Select_Font;

   -------------------------
   -- Create_Box_For_Font --
   -------------------------

   function Create_Box_For_Font
     (Manager      : access Preferences_Manager_Record;
      N            : Pref_Description_Access;
      Desc         : Pango_Font_Description;
      Button_Label : String) return Gtk_Box
   is
      Box : Gtk_Box;
      Ent : Gtk_Entry;
      Button : Gtk_Button;
   begin
      Gtk_New_Hbox (Box, Homogeneous => False);
      Gtk_New (Ent);
      Pack_Start (Box, Ent, Expand => True, Fill => True);

      Gtk_New (Button, Button_Label);
      Pack_Start (Box, Button, Expand => False, Fill => False);
      Param_Handlers.Object_Connect
        (Button, "clicked",
         Param_Handlers.To_Marshaller (Select_Font'Access),
         Slot_Object => Ent,
         User_Data => N);

      Return_Param_Handlers.Connect
        (Ent, "focus_out_event",
         Return_Param_Handlers.To_Marshaller (Font_Entry_Changed'Access),
         User_Data   => N);
      Param_Handlers.Object_Connect
        (Manager.Pref_Editor, "preferences_changed",
         Param_Handlers.To_Marshaller (Update_Font_Entry'Access),
         Ent, User_Data => N);

      Set_Style (Ent, Copy (Get_Style (Ent)));
      Set_Font_Description (Get_Style (Ent), Desc);
      Set_Text (Ent, To_String (Desc));
      Reset_Font (Ent);
      return Box;
   end Create_Box_For_Font;

   -------------------
   -- Editor_Widget --
   -------------------

   function Editor_Widget
     (Manager : access Preferences_Manager_Record;
      Param   : Param_Spec;
      Tips    : Gtk_Tooltips) return Gtk.Widget.Gtk_Widget
   is
      Typ : constant GType := Value_Type (Param);
      N   : constant Pref_Description_Access :=
        Get (Manager.Preferences, Pspec_Name (Param));
   begin
      if Typ = GType_Int then
         declare
            Prop : constant Param_Spec_Int := Param_Spec_Int (Param);
            Spin : Gtk_Spin_Button;
            Adj  : Gtk_Adjustment;
         begin
            Gtk_New (Adj,
                     Value => Gdouble (Get_Pref (Manager, Prop)),
                     Lower => Gdouble (Minimum (Prop)),
                     Upper => Gdouble (Maximum (Prop)),
                     Step_Increment => 1.0,
                     Page_Increment => 10.0,
                     Page_Size      => 10.0);
            Gtk_New (Spin, Adj, 1.0, The_Digits => 0);
            Set_Editable (Spin, True);

            Param_Handlers.Connect
              (Adj, "value_changed",
               Param_Handlers.To_Marshaller (Gint_Changed'Access),
               User_Data   => N);
            Param_Handlers.Object_Connect
              (Manager.Pref_Editor, "preferences_changed",
               Param_Handlers.To_Marshaller (Update_Gint'Access),
               Adj, User_Data => N);

            return Gtk_Widget (Spin);
         end;

      elsif Typ = GType_Boolean then
         declare
            Prop   : constant Param_Spec_Boolean := Param_Spec_Boolean (Param);
            Toggle : Gtk_Check_Button;
         begin
            Gtk_New (Toggle, -"Enabled");
            Widget_Callback.Connect
              (Toggle, "toggled",
               Widget_Callback.To_Marshaller
               (Toggled_Boolean'Access));
            Set_Active (Toggle, True); --  Forces a toggle
            Set_Active (Toggle, Get_Pref (Manager, Prop));

            Param_Handlers.Connect
              (Toggle, "toggled",
               Param_Handlers.To_Marshaller (Boolean_Changed'Access),
               User_Data   => N);
            Param_Handlers.Object_Connect
              (Manager.Pref_Editor, "preferences_changed",
               Param_Handlers.To_Marshaller (Update_Boolean'Access),
               Toggle, User_Data => N);

            return Gtk_Widget (Toggle);
         end;

      elsif Typ = Gdk.Keyval.Get_Type then
         declare
            Prop   : constant Param_Spec_Key := Param_Spec_Key (Param);
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

            Get_Pref (Manager, Prop, Modif, Key);

            Append_Text (Ent, Image (Key, Modif));

            Widget_Callback.Object_Connect
              (Button, "clicked",
               Widget_Callback.To_Marshaller (Key_Grab'Access),
               Slot_Object => Ent);
            Param_Handlers.Connect
              (Ent, "insert_text",
               Param_Handlers.To_Marshaller (Entry_Changed'Access),
               User_Data   => N,
               After       => True);
            Param_Handlers.Object_Connect
              (Manager.Pref_Editor, "preferences_changed",
               Param_Handlers.To_Marshaller (Update_Entry'Access),
               Ent, User_Data => N);

            return Gtk_Widget (Box);
         end;

      elsif Typ = GType_String then
         declare
            Prop : constant Param_Spec_String := Param_Spec_String (Param);
            Ent  : Gtk_Entry;
         begin
            Gtk_New (Ent);
            Set_Text (Ent, Get_Pref (Manager, Prop));

            Param_Handlers.Connect
              (Ent, "insert_text",
               Param_Handlers.To_Marshaller (Entry_Changed'Access),
               User_Data   => N,
               After       => True);
            Param_Handlers.Connect
              (Ent, "delete_text",
               Param_Handlers.To_Marshaller (Entry_Changed'Access),
               User_Data   => N,
               After       => True);
            Param_Handlers.Object_Connect
              (Manager.Pref_Editor, "preferences_changed",
               Param_Handlers.To_Marshaller (Update_Entry'Access),
               Ent, User_Data => N);

            return Gtk_Widget (Ent);
         end;

      elsif Typ = Gtk.Style.Get_Type then
         declare
            Prop  : constant Param_Spec_Style := Param_Spec_Style (Param);
            Event : Gtk_Event_Box;
            Box   : Gtk_Box;
            F     : constant Gtk_Box := Create_Box_For_Font
              (Manager, N, Get_Pref_Font (Manager, Prop), "...");
            Combo : Gvd_Color_Combo;

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
            Set_Color (Combo, Get_Pref_Fg (Manager, Prop));
            Param_Handlers.Connect
              (Combo, "color_changed",
               Param_Handlers.To_Marshaller (Fg_Color_Changed'Access),
               User_Data   => N);
            Param_Handlers.Object_Connect
              (Manager.Pref_Editor, "preferences_changed",
               Param_Handlers.To_Marshaller (Update_Fg'Access),
               Combo, User_Data => N);

            Gtk_New (Event);
            Gtk_New (Combo);
            Add (Event, Combo);
            Set_Tip (Tips, Event, -"Background color");
            Pack_Start (Box, Event, Expand => False);
            Set_Color (Combo, Get_Pref_Bg (Manager, Prop));
            Param_Handlers.Connect
              (Combo, "color_changed",
               Param_Handlers.To_Marshaller (Bg_Color_Changed'Access),
               User_Data   => N);
            Param_Handlers.Object_Connect
              (Manager.Pref_Editor, "preferences_changed",
               Param_Handlers.To_Marshaller (Update_Bg'Access),
               Combo, User_Data => N);

            return Gtk_Widget (Box);
         end;

      elsif Typ = Gdk.Color.Gdk_Color_Type then
         declare
            Prop  : constant Param_Spec_Color := Param_Spec_Color (Param);
            Combo : Gvd_Color_Combo;
         begin
            Gtk_New (Combo);
            Set_Color (Combo, Get_Pref (Manager, Prop));

            Param_Handlers.Connect
              (Combo, "color_changed",
               Param_Handlers.To_Marshaller (Color_Changed'Access),
               User_Data   => N);
            Param_Handlers.Object_Connect
              (Manager.Pref_Editor, "preferences_changed",
               Param_Handlers.To_Marshaller (Update_Color'Access),
               Combo, User_Data => N);

            return Gtk_Widget (Combo);
         end;

      elsif Typ = Pango.Font.Get_Type then
         declare
            Prop : constant Param_Spec_Font := Param_Spec_Font (Param);
         begin
            return Gtk_Widget
              (Create_Box_For_Font
                 (Manager, N, Get_Pref (Manager, Prop), -"Browse"));
         end;

      elsif Fundamental (Typ) = GType_Enum then
         declare
            Prop    : constant Param_Spec_Enum := Param_Spec_Enum (Param);
            V       : constant Gint := Get_Pref (Manager, Prop);
            Combo   : Gtk_Combo;
            E_Klass : constant Enum_Class := Enumeration (Prop);
            Val     : Enum_Value;
            K       : Guint := 0;
            Item    : Gtk_List_Item;
         begin
            Gtk_New (Combo);
            Set_Value_In_List (Combo, True, Ok_If_Empty => False);
            Set_Editable (Get_Entry (Combo), False);

            loop
               Val := Nth_Value (E_Klass, K);
               exit when Val = null;
               declare
                  S : String := Nick (Val);
               begin
                  Mixed_Case (S);
                  Gtk_New (Item, S);
               end;
               Add (Get_List (Combo), Item);
               if Value (Val) = V then
                  Set_Text (Get_Entry (Combo), Nick (Val));
               end if;
               Show_All (Item);
               K := K + 1;
            end loop;

            Param_Handlers.Object_Connect
              (Get_List (Combo), "select_child",
               Param_Handlers.To_Marshaller (Enum_Changed'Access),
               Slot_Object => Combo,
               User_Data   => N);
            Param_Handlers.Object_Connect
              (Manager.Pref_Editor, "preferences_changed",
               Param_Handlers.To_Marshaller (Update_Entry'Access),
               Get_Entry (Combo), User_Data => N);

            return Gtk_Widget (Combo);
         end;

      else
         declare
            Label : Gtk_Label;
         begin
            Gtk_New (Label, -"Preference cannot be edited");
            return Gtk_Widget (Label);
         end;
      end if;
   end Editor_Widget;

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

            while Last <= Name'Last and then Name (Last) /= ':' loop
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
      Iter       : Iterator;
      Info       : Pref_Description_Access;

      Sorted_Prefs : array (0 .. Manager.Current_Index - 1)
        of Pref_Description_Access;
      --  All the preferences sorted by order of registration. This is needed
      --  so that the order in the preferences dialog is predictible.

      Had_Apply  : Boolean := False;
      Row        : Guint;
      Widget     : Gtk_Widget;
      Tips       : Gtk_Tooltips;
      Event      : Gtk_Event_Box;
      Label      : Gtk_Label;
      Separator  : Gtk_Separator;

   begin
      Save_Preferences (Manager, Saved);

      Get_First (Manager.Preferences, Iter);
      loop
         Info := Get_Element (Iter);
         exit when Info = null;
         Sorted_Prefs (Info.Index) := Info;
         Get_Next (Manager.Preferences, Iter);
      end loop;

      Dialog := new Preferences_Editor_Record;
      Initialize
        (Dialog => Dialog,
         Title  => -"Preferences",
         Parent => Gtk_Window (Parent),
         Flags  => Modal or Destroy_With_Parent);
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
      Set_Headers_Visible (View, False);

      Gtk_New (Col);
      Num := Append_Column (View, Col);
      Gtk_New (Render);
      Pack_Start (Col, Render, Expand => True);
      Add_Attribute (Col, Render, "text", 0);

      Widget_Callback.Object_Connect
        (Get_Selection (View), "changed",
         Widget_Callback.To_Marshaller (Selection_Changed'Unrestricted_Access),
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

      for Index in Sorted_Prefs'Range loop
         Info := Sorted_Prefs (Index);

         if Info /= null
           and then Info.Param /= null
           and then (Flags (Info.Param) and Param_Writable) /= 0
         then
            Table := Gtk_Table (Find_Or_Create_Page (Info.Page.all, null));
            Row := Get_Property (Table, N_Rows_Property);
            Resize (Table, Rows => Row + 1, Columns => 2);

            Gtk_New (Event);
            Gtk_New (Label, Nick_Name (Info.Param));
            Add (Event, Label);
            Set_Tip (Tips, Event, Description (Info.Param));
            Set_Alignment (Label, 0.0, 0.5);
            Attach (Table, Event, 0, 1, Row, Row + 1,
                    Xoptions => Fill, Yoptions => 0);

            Widget := Editor_Widget (Manager, Info.Param, Tips);

            if Widget /= null then
               Attach (Table, Widget, 1, 2, Row, Row + 1, Yoptions => 0);
            end if;
         end if;
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
               Restore_Preferences (Manager, Saved);

               if Had_Apply and then On_Changed /= null then
                  On_Changed (Manager);
               end if;

               exit;
         end case;
      end loop;

      Destroy (Tips);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         Destroy (Dialog);
   end Edit_Preferences;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences
     (Manager : access Preferences_Manager_Record;
      Saved   : out Default_Preferences.Saved_Prefs_Data)
   is
      Iter        : Iterator;
      Info        : Pref_Description_Access;
   begin
      Get_First (Manager.Preferences, Iter);
      loop
         Info := Get_Element (Iter);
         exit when Info = null;
         Set (Saved.Preferences, Get_Key (Iter), Clone (Info));
         Get_Next (Manager.Preferences, Iter);
      end loop;
   end Save_Preferences;

   -------------------------
   -- Restore_Preferences --
   -------------------------

   procedure Restore_Preferences
     (Manager : access Preferences_Manager_Record;
      Saved   : Default_Preferences.Saved_Prefs_Data) is
   begin
      Reset (Manager.Preferences);
      Manager.Preferences := Saved.Preferences;
   end Restore_Preferences;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Data : in out Default_Preferences.Saved_Prefs_Data) is
   begin
      Reset (Data.Preferences);
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

end Default_Preferences;
