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

--  This package provides support for the handling of preferences in GPS. The
--  preferences system is based on the glib types Param_Spec. These include
--  natively the description of the param_spec, which can be used in a
--  graphical editor, as well as precise information about the allowed values
--  for the type. It also provides support for closely associating Ada
--  enumeration types with C types, thus allowing almost any type of
--  preference.

with Ada.Containers.Doubly_Linked_Lists;

with GNAT.Strings;        use GNAT.Strings;

with GNATCOLL.VFS;        use GNATCOLL.VFS;

with Gdk.RGBA;            use Gdk.RGBA;
with Glib;                use Glib;
with Glib.Object;         use Glib.Object;

with Gtk.Box;
with Gtk.Color_Button;
with Gtk.Combo_Box_Text;
with Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Widget;

with Pango.Font;          use Pango.Font;

package Default_Preferences is

   type Preferences_Manager_Record is tagged private;
   type Preferences_Manager is access all Preferences_Manager_Record'Class;
   --  Manages a set of preferences.
   --  You can extend this type for instance if you need a fast access to some
   --  of the preferences, by providing a cache system, and redefining the
   --  Set_Pref subprograms.

   procedure Destroy (Manager : in out Preferences_Manager_Record);
   procedure Destroy (Manager : in out Preferences_Manager);
   --  Free the memory used by Manager, including all the registered
   --  preferences. Get_Pref mustn't be used afterwards.

   type Preference_Record is abstract tagged private;
   type Preference is access all Preference_Record'Class;
   --  Type used to represent a general preference.
   --  You can extend this type to represent more specific preferences
   --  (e.g: Integer_Preference_Record)

   type Preferences_Editor_Interface is interface;
   type Preferences_Editor is access all Preferences_Editor_Interface'Class;
   --  Interface defining a common API for all the preferences editor
   --  widgets.

   type Preferences_Page_Interface is interface;
   type Preferences_Page is access all Preferences_Page_Interface'Class;
   --  Interface defining a common API for all the preferences editor pages.
   --  This is used to manipulate in a same way the pages directly related with
   --  preferences (preferences.xml) and pages that are related with other
   --  modules and other customization files (e.g: key shortcuts).

   function Edit (Self : not null access Preferences_Page_Interface)
                  return Gtk.Widget.Gtk_Widget is abstract;
   --  Return the page's widget in which we can insert new children.

   function Get_Widget
     (Self : not null access Preferences_Editor_Interface)
      return Gtk.Widget.Gtk_Widget is abstract;
   --  Return the main Gtk_Widget of the preferences editor view

   procedure Display_Pref
     (Self : not null access Preferences_Editor_Interface;
      Pref : not null access Preference_Record'Class) is abstract;
   --  Display the preference given in parameter. This means that the
   --  preferences editor widget should make appear the preference on the
   --  screen (e.g: open the page containing the preference if it is organized
   --  with different pages).

   procedure Highlight_Pref
     (Self : not null access Preferences_Editor_Interface;
      Pref : not null access Preference_Record'Class) is null;
   --  Highlight the preference given in parameter.
   --  The way preferences are highlighted is editor-specific: override
   --  this procedure if preferences highlighting is needed.

   procedure Unhighlight_Previous_Pref
     (Self : not null access Preferences_Editor_Interface) is null;
   --  If a preference has already been highlighted (via a call to
   --  Highlight_Pref), unhighlight it.

   procedure Set_GObject_To_Update
     (Pref   : not null access Preference_Record;
      Obj    : not null access GObject_Record'Class);
   --  Add/Update the key association in the Preferemces_GObjects_Map
   --  between the preference's name and the GObject which needs
   --  to be updated when preferences changed.

   function Get_GObject_To_Update
     (Pref : not null access Preference_Record) return GObject;
   --  Return the GObject associated with the preference's name in the
   --  Preferences_GObject_Map. This function is used to retrieve the GObject
   --  we want to update when preferences changed.

   function Has_GObject_To_Update
     (Pref : not null access Preference_Record) return Boolean;
   --  Return True if a the Preferences_GObjects_Map contains a key association
   --  for the preference given in parameter, False otherwise.

   procedure Remove_All_GObjects_To_Update;
   --  Reset the Preferences_GObjects_Map. This is used to avoid calling
   --  Update_On_Pref_Changed for all the preferences when the preferences
   --  dialog has already been destroyed.

   procedure Notify_Pref_Changed
     (Self : not null access Preferences_Manager_Record;
      Pref : not null access Preference_Record'Class) is null;
   --  Called when a preference is changed.
   --  If the Preference dialog is displayed, runs the
   --  Preferences_Changed_Hook. This is only useful when writing
   --  your own type of preference and overriding Set_Pref.

   procedure Set_Is_Loading_Prefs
     (Self : not null access Preferences_Manager_Record'Class;
      Loading : Boolean);
   function Is_Loading_Preferences
     (Self : not null access Preferences_Manager_Record'Class) return Boolean;
   --  True while we are loading the preferences. This is used to disable
   --  saving the preferences when we are setting their initial value.

   procedure Freeze (Self : not null access Preferences_Manager_Record);
   procedure Thaw (Self : not null access Preferences_Manager_Record);
   --  Freeze/Thaw the emission of the preferences_changed signal
   procedure Register
     (Manager                : not null access Preferences_Manager_Record;
      Name, Label, Page, Doc : String;
      Pref                   : not null access Preference_Record'Class);
   --  Set common attributes of all preferences, and register that preference
   --  in the manager. This function only needs to be called if you are
   --  creating your own types of preferences, and is already called
   --  automatically when using one of the Create functions below.

   function Is_Frozen
     (Self : not null access Preferences_Manager_Record'Class) return Boolean;
   --  Return True if Self is frozen

   ----------------
   -- Preference --
   ----------------
   --  This type represents a preference (type + value)

   function Get_Pref
     (Pref : access Preference_Record) return String is abstract;
   --  Convert the preference to a string suitable for inclusion in an XML
   --  file. The string can itself contain well-formed XML

   procedure Set_Pref
     (Pref    : access Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String) is abstract;
   --  Set the value of the preference from a string read in an XML file.
   --  This emits the preferences_changed signal on Manager if necessary, so
   --  that the preferences dialog is refreshed appropriately if it is open.
   --  Manager could be null if we never want to refresh the dialog

   function Is_Default
     (Pref    : not null access Preference_Record) return Boolean is (False);
   --  Whether the current value of the preference is also the default value.

   function Edit
     (Pref               : access Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget is abstract;
   --  Return the widget that should be used to edit the preference described
   --  in Pref.
   --  This widget should connect to the "preferences_changed" signal on
   --  Preferences_Editor, to refresh its contents when the preferences have
   --  changed (for instance the user has pressed Reset in the dialog).
   --  Likewise, if the value stored in the widget is changed, it should also
   --  change the value stored in Pref. Such connection to signals will
   --  generally be done through Preference_Handlers below.

   function Editor_Needs_Label
     (Pref : not null access Preference_Record) return Boolean is (True);
   --  Whether a separate label should be displayed for the preference in
   --  the editor. If False is returned, it is assumed that the widget returned
   --  by Edit already shows the label.

   procedure Update_On_Pref_Changed
     (Pref   : access Preference_Record;
      Widget : access GObject_Record'Class) is abstract;
   --  Called when preferences changed. This procedure allows to update
   --  the GObject associated with the preference's name in the
   --  Preferences_GObjects_Map.
   --  Don't forget to insert/update the map when the widget you want to update
   --  is created (in the Edit function).

   type Variant_Enum is (Default, Normal, Italic, Bold, Bold_Italic);
   --  Auxiliary type to list text variants offered in Variant_Preferences

   function To_String (V : Variant_Enum) return String;
   function From_String (S : String) return Variant_Enum;
   --  Conversion between a Variant_Enum and a pretty string for displaying in
   --  the dialog.

   procedure Free (Pref : in out Preference_Record);
   --  Free the memory associated with Pref

   type Integer_Preference_Record is new Preference_Record with private;
   type Boolean_Preference_Record is new Preference_Record with private;
   type String_Preference_Record  is new Preference_Record with private;
   type Color_Preference_Record   is new Preference_Record with private;
   type Font_Preference_Record    is new Preference_Record with private;
   type Style_Preference_Record   is new Preference_Record with private;
   type Variant_Preference_Record is new Style_Preference_Record with private;
   type Enum_Preference_Record  is abstract new Preference_Record with private;
   type Theme_Preference_Record   is new Preference_Record with private;

   type Integer_Preference is access all Integer_Preference_Record'Class;
   type Boolean_Preference is access all Boolean_Preference_Record'Class;
   type String_Preference  is access all String_Preference_Record'Class;
   type Color_Preference   is access all Color_Preference_Record'Class;
   type Font_Preference    is access all Font_Preference_Record'Class;
   type Style_Preference   is access all Style_Preference_Record'Class;
   type Variant_Preference is access all Variant_Preference_Record'Class;
   type Enum_Preference    is access all Enum_Preference_Record'Class;
   type Theme_Preference   is access all Theme_Preference_Record'Class;

   -------------------------------
   -- Editing preferences types --
   -------------------------------
   --  Prefernces can be edited graphically through the preferences dialog.
   --  By default, GPS knows how to edit the basic types of preferences, but if
   --  you use a custom type of preference, you should provide your own editor
   --  through Create below.
   --  See for instance GPS.Kernel.Charsets for more examples.

   type Manager_Preference is record
      Manager   : Preferences_Manager;
      Pref      : Preference;
   end record;
   package Preference_Handlers is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Manager_Preference);
   package Return_Preference_Handlers is new Gtk.Handlers.User_Return_Callback
     (Glib.Object.GObject_Record, Boolean, Manager_Preference);

   ------------------------------
   -- Creating new preferences --
   ------------------------------

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Minimum, Maximum, Default : Integer)
      return Integer_Preference;
   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Default                   : Boolean)
      return Boolean_Preference;
   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Default                   : String;
      Multi_Line                : Boolean := False)
      return String_Preference;
   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Default                   : String)
      return Color_Preference;
   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Default                   : String)
      return Font_Preference;
   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Default_Font              : String;
      Default_Fg                : String;
      Default_Bg                : String)
      return Style_Preference;
   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Base                      : Style_Preference;
      Default_Variant           : Variant_Enum;
      Default_Fg                : String;
      Default_Bg                : String)
      return Variant_Preference;
   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String)
      return Theme_Preference;
   --  Create a new preference and register it in the Manager.
   --  Name is the name used when saving in the XML file, and when referencing
   --    that preference from a python file. It can contain any character.
   --  Label is the label used in the preferences dialog. It must be human
   --    readable.
   --  Page is the page in the preference dialog. Subpages are separated by '/'
   --    chars. This is set to the empty string if the preference should not be
   --    visible in the preferences dialog, but can be edited directly in the
   --    XML file.
   --  Doc is the documentation for this preference, at it appears in the
   --    tooltip of the preferences dialog
   --  See Default_Preferences.Generics to create preferences associated with
   --    enumerations

   function Create_Invisible_Pref
     (Manager : access Preferences_Manager_Record'Class;
      Name    : String;
      Default : Boolean;
      Label   : String := "";
      Doc     : String := "")
      return Boolean_Preference
   is (Create (Manager, Name, Label => Label, Page => "", Doc => Doc,
               Default => Default));
   --  Convenience function for creating an invisible preference.
   --  Such preferences are changed via other GUI elements (like a check
   --  button for instance), rather than in the preferences dialog. They are
   --  similar to what histories.ads provides, but are also associated with
   --  standard hooks so that views can be refreshed when the preference
   --  changes.

   function Get_Pref_From_Name
     (Manager             : access Preferences_Manager_Record;
      Name                : String;
      Create_If_Necessary : Boolean) return Preference;
   --  Return the corresponding preference.
   --  If it doesn't exist, a temporary preference is created. When the
   --  actual preference is registered, it will replace that temporary entry
   --  but preserve the default value.

   --------------------------------------
   -- Getting the value of preferences --
   --------------------------------------

   function Get_Name  (Pref : access Preference_Record'Class) return String;
   function Get_Label (Pref : access Preference_Record'Class) return String;
   function Get_Doc   (Pref : access Preference_Record'Class) return String;
   function Get_Page  (Pref : access Preference_Record'Class) return String;

   overriding function Get_Pref
     (Pref : access String_Preference_Record) return String;

   function Get_Pref (Pref : access Integer_Preference_Record) return Integer;
   function Get_Pref (Pref : access Boolean_Preference_Record) return Boolean;

   overriding function Get_Pref
     (Pref : access Color_Preference_Record) return String;
   function Get_Pref
     (Pref : access Color_Preference_Record) return Gdk.RGBA.Gdk_RGBA;

   overriding function Get_Pref
     (Pref : access Font_Preference_Record) return String;
   function Get_Pref
     (Pref : access Font_Preference_Record)
      return Pango.Font.Pango_Font_Description;

   overriding function Get_Pref
     (Pref : access Style_Preference_Record) return String;
   function Get_Pref_Font
     (Pref     : access Style_Preference_Record)
      return Pango.Font.Pango_Font_Description;
   function Get_Pref_Fg
     (Pref     : access Style_Preference_Record'Class)
      return Gdk.RGBA.Gdk_RGBA;
   function Get_Pref_Bg
     (Pref     : access Style_Preference_Record'Class)
      return Gdk.RGBA.Gdk_RGBA;

   overriding function Get_Pref
     (Pref : access Variant_Preference_Record) return String;
   function Get_Pref_Font
     (Pref     : access Variant_Preference_Record)
      return Pango.Font.Pango_Font_Description;
   function Get_Pref_Fg_Color
     (Pref     : access Variant_Preference_Record)
      return Gdk.RGBA.Gdk_RGBA;
   function Get_Pref_Bg_Color
     (Pref     : access Variant_Preference_Record)
      return Gdk.RGBA.Gdk_RGBA;
   function Get_Pref (Pref : access Enum_Preference_Record) return Integer;
   function Get_Pref_Variant
     (Pref     : access Variant_Preference_Record)
      return Variant_Enum;
   --  Get the value for a preference. The default value is returned if the
   --  user hasn't explicitely overridden it.
   --  Colors have already been allocated when they are returned.
   --  The Font_Description must not be freed by the caller
   --  For enumeration, it returns the 'Pos of the enumeration value.

   type Theme_Descr is record
      Name : GNAT.Strings.String_Access;
      --  Display name for the theme, including the variant

      Directory : GNAT.Strings.String_Access;
      --  Name for the theme (not including the variant name)

      Dark    : Boolean := False;
      --  Whether to use a dark variant
   end record;
   function Get_Pref
     (Pref : access Theme_Preference_Record) return Theme_Descr;

   --------------------------------------
   -- Setting the value of preferences --
   --------------------------------------

   --  General version. This can set any type of preference from a string,
   --  which is in particular useful when saving to an XML file. Value must be
   --  compatible with XML, but can itself contain an XML extract.

   procedure Set_Pref
     (Pref    : Integer_Preference;
      Manager : access Preferences_Manager_Record'Class;
      Value   : Integer);
   procedure Set_Pref
     (Pref    : Boolean_Preference;
      Manager : access Preferences_Manager_Record'Class;
      Value   : Boolean);
   procedure Set_Pref
     (Pref         : Style_Preference;
      Manager      : access Preferences_Manager_Record'Class;
      Font, Fg, Bg : String);
   procedure Set_Pref
     (Pref         : Font_Preference;
      Manager      : access Preferences_Manager_Record'Class;
      Font         : Pango.Font.Pango_Font_Description);
   procedure Set_Pref
     (Pref         : Variant_Preference;
      Manager      : access Preferences_Manager_Record'Class;
      Variant      : Variant_Enum;
      Fg, Bg       : String);
   --  Change the value of a preference. This overrides the default value if
   --  this preference is set for the first time.

   ---------------------------------------------
   -- Loading and saving preferences to files --
   ---------------------------------------------

   procedure Load_Preferences
     (Manager   : access Preferences_Manager_Record;
      File_Name : Virtual_File);
   --  Load the preferences from a specific file.
   --  The preferences can be loaded even if they have not been registered
   --  yet.

   procedure Save_Preferences
     (Manager   : access Preferences_Manager_Record;
      File_Name : Virtual_File;
      Success   : out Boolean);
   --  Save the default preferences to File_Name

   ---------------
   -- iterators --
   ---------------

   type Cursor is private;

   function Get_First_Reference
     (Manager : not null access Preferences_Manager_Record)
      return Cursor;
   procedure Next
     (Manager : not null access Preferences_Manager_Record;
      C       : in out Cursor);
   function Get_Pref (Self : Cursor) return Preference;
   --  Iterate over all registered preferences

   -------------------
   -- Editors views --
   -------------------

   function Get_Editor
     (Manager : access Preferences_Manager_Record)
      return access Preferences_Editor_Interface'Class;
   --  Return the Preferences dialog widget if it is currently open
   procedure Set_Editor
     (Manager : access Preferences_Manager_Record;
      Editor  : access Preferences_Editor_Interface'Class);

private

   type Preference_Record is abstract tagged record
      Name  : GNAT.Strings.String_Access;
      --  Name in the .xml file, and used for external references

      Label : GNAT.Strings.String_Access;
      --  Label used in the preferences dialog

      Page  : GNAT.Strings.String_Access;
      --  Page in the preference dialog. Subpages are separated by '/' chars.
      --  This is set to null if the preference should not be visible in the
      --  preferences dialog, but can be edited directly in the XML file.

      Doc   : GNAT.Strings.String_Access;
      --  The documentation for this preference
   end record;

   type Integer_Preference_Record is new Preference_Record with record
      Int_Value     : Integer;
      Int_Min_Value : Integer;
      Int_Max_Value : Integer;
      Default       : Integer;
   end record;
   overriding function Get_Pref
     (Pref : access Integer_Preference_Record) return String;
   overriding procedure Set_Pref
     (Pref    : access Integer_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Edit
     (Pref               : access Integer_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   overriding function Is_Default
     (Self : not null access Integer_Preference_Record) return Boolean
   is (Self.Default = Self.Int_Value);
   overriding procedure Update_On_Pref_Changed
     (Pref   : access Integer_Preference_Record;
      Widget : access GObject_Record'Class);

   type Boolean_Preference_Record is new Preference_Record with record
      Bool_Value    : Boolean;
      Default       : Boolean;
   end record;
   overriding function Get_Pref
     (Pref : access Boolean_Preference_Record) return String;
   overriding procedure Set_Pref
     (Pref    : access Boolean_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Edit
     (Pref               : access Boolean_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   overriding function Is_Default
     (Self : not null access Boolean_Preference_Record) return Boolean
     is (Self.Default = Self.Bool_Value);
   overriding function Editor_Needs_Label
     (Pref : not null access Boolean_Preference_Record) return Boolean
   is (False);
   overriding procedure Update_On_Pref_Changed
     (Pref   : access Boolean_Preference_Record;
      Widget : access GObject_Record'Class);

   type String_Preference_Record is new Preference_Record with record
      Str_Value     : GNAT.Strings.String_Access;
      Default       : GNAT.Strings.String_Access;
      Multi_Line    : Boolean := False;
   end record;
   overriding procedure Set_Pref
     (Pref    : access String_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Edit
     (Pref               : access String_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   overriding procedure Free (Pref : in out String_Preference_Record);
   overriding function Is_Default
     (Self : not null access String_Preference_Record) return Boolean
   is (Self.Default /= null and then Self.Default.all = Self.Str_Value.all);
   overriding procedure Update_On_Pref_Changed
     (Pref   : access String_Preference_Record;
      Widget : access GObject_Record'Class);

   type Color_Preference_Record is new Preference_Record with record
      Default       : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA;
      Color         : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA;
   end record;
   overriding procedure Set_Pref
     (Pref    : access Color_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Edit
     (Pref               : access Color_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   overriding function Is_Default
     (Self : not null access Color_Preference_Record) return Boolean
   is (Self.Default = Self.Color);
   overriding procedure Update_On_Pref_Changed
     (Pref   : access Color_Preference_Record;
      Widget : access GObject_Record'Class);

   type Font_Preference_Record is new Preference_Record with record
      Default       : Pango.Font.Pango_Font_Description;
      Descr         : Pango.Font.Pango_Font_Description;
   end record;
   type My_Font_Box_Record is new Gtk.Box.Gtk_Box_Record with record
      Ent : Gtk.GEntry.Gtk_Entry;
   end record;
   type My_Font_Box is access all My_Font_Box_Record'Class;
   overriding procedure Set_Pref
     (Pref    : access Font_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Edit
     (Pref               : access Font_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   overriding procedure Free (Pref : in out Font_Preference_Record);
   overriding function Is_Default
     (Self : not null access Font_Preference_Record) return Boolean
   is (Equal (Self.Default, Self.Descr));
   overriding procedure Update_On_Pref_Changed
     (Pref   : access Font_Preference_Record;
      Widget : access GObject_Record'Class);

   type Style_Preference_Record is new Preference_Record with record
      Font_Default  : Pango.Font.Pango_Font_Description;
      Font_Descr    : Pango.Font.Pango_Font_Description;

      Fg_Color      : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA;
      Fg_Default    : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA;

      Bg_Color      : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA;
      Bg_Default    : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA;
   end record;
   type My_Style_Box_Record is new Gtk.Box.Gtk_Box_Record with record
      Font_Box        : My_Font_Box;
      Fg_Color_Button : Gtk.Color_Button.Gtk_Color_Button;
      Bg_Color_Button : Gtk.Color_Button.Gtk_Color_Button;
   end record;
   type My_Style_Box is access all My_Style_Box_Record'Class;
   overriding procedure Set_Pref
     (Pref    : access Style_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Edit
     (Pref               : access Style_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   overriding procedure Free (Pref : in out Style_Preference_Record);
   overriding function Is_Default
     (Self : not null access Style_Preference_Record) return Boolean
     is (Equal (Self.Font_Default, Self.Font_Descr)
         and then Self.Fg_Default = Self.Fg_Color
         and then Self.Bg_Default = Self.Bg_Color);
   overriding procedure Update_On_Pref_Changed
     (Pref   : access Style_Preference_Record;
      Widget : access GObject_Record'Class);

   type Variant_Preference_Record is new Style_Preference_Record with record
      Base_Font     : Style_Preference;
      Variant       : Variant_Enum;
      Default_Variant : Variant_Enum;
      --  never sets the Style_Font and Font_Default fields
   end record;
   type My_Variant_Box_Record is new Gtk.Box.Gtk_Box_Record with record
      Combo           : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
      Fg_Color_Button : Gtk.Color_Button.Gtk_Color_Button;
      Bg_Color_Button : Gtk.Color_Button.Gtk_Color_Button;
   end record;
   type My_Variant_Box is access all My_Variant_Box_Record'Class;
   overriding procedure Set_Pref
     (Pref    : access Variant_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Edit
     (Pref               : access Variant_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   overriding procedure Free (Pref : in out Variant_Preference_Record);
   overriding function Is_Default
     (Self : not null access Variant_Preference_Record) return Boolean
     is (Self.Variant = Self.Default_Variant
         and then Self.Fg_Default = Self.Fg_Color
         and then Self.Bg_Default = Self.Bg_Color);
   overriding procedure Update_On_Pref_Changed
     (Pref   : access Variant_Preference_Record;
      Widget : access GObject_Record'Class);

   type Enum_Preference_Record is abstract new Preference_Record with record
      Enum_Value    : Integer;
      Default       : Integer := -1;
   end record;
   overriding function Get_Pref
     (Pref : access Enum_Preference_Record) return String;
   overriding procedure Set_Pref
     (Pref    : access Enum_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Is_Default
     (Self : not null access Enum_Preference_Record) return Boolean
   is (Self.Enum_Value = Self.Default);

   type Theme_Descr_Array is array (Natural range <>) of Theme_Descr;
   type Theme_Descr_Array_Access is access all Theme_Descr_Array;

   type Theme_Preference_Record is new Preference_Record with record
      Themes  : Theme_Descr_Array_Access;
      Current : Natural := 0;
   end record;
   overriding function Get_Pref
     (Pref : access Theme_Preference_Record) return String;
   overriding procedure Set_Pref
     (Pref    : access Theme_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Edit
     (Pref    : access Theme_Preference_Record;
      Manager : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   overriding procedure Free (Pref : in out Theme_Preference_Record);
   overriding procedure Update_On_Pref_Changed
     (Pref   : access Theme_Preference_Record;
      Widget : access GObject_Record'Class);

   package Preferences_Maps is new Ada.Containers.Doubly_Linked_Lists
     (Preference);

   type Cursor is record
      C : Preferences_Maps.Cursor;
   end record;

   type Preferences_Manager_Record is tagged record
      Preferences   : Preferences_Maps.List;

      Pref_Editor   : access Preferences_Editor_Interface'Class;
      --  The current preferences editor. This is set to null if there is no
      --  editor open currently

      Loading_Prefs : Boolean := False;
      --  True while we are loading the preferences. This is used to disable
      --  saving the preferences when we are setting their initial value.

      Freeze_Count : Integer := 0;
      --  How many calls have been made to "Freeze". If 0, this manager is
      --  not frozen.
   end record;

end Default_Preferences;
