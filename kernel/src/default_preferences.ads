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

--  This package provides support for the handling of preferences in GPS. The
--  preferences system is based on the glib types Param_Spec. These include
--  natively the description of the param_spec, which can be used in a
--  graphical editor, as well as precise information about the allowed values
--  for the type. It also provides support for closely associating Ada
--  enumeration types with C types, thus allowing almost any type of
--  preference.

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;

with Gdk.Color;
with Gdk.Types;
with Glib;                      use Glib;
with Glib.Object;
with Gtk.Handlers;
with Gtk.Tooltips;
with Gtk.Widget;
with Gtk.Window;
with Pango.Font;

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

   ----------------
   -- Preference --
   ----------------
   --  This type represents a preference (type + value)

   type Preference_Record is abstract tagged private;
   type Preference is access all Preference_Record'Class;

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

   function Edit
     (Pref               : access Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget is abstract;
   --  Return the widget that should be used to edit the preference described
   --  in Pref.
   --  This widget should connect to the "preferences_changed" signal on
   --  Preferences_Editor, to refresh its contents when the preferences have
   --  changed (for instance the user has pressed Reset in the dialog).
   --  Likewise, if the value stored in the widget is changed, it should also
   --  change the value stored in Pref. Such connection to signals will
   --  generally be done through Preference_Handlers below

   procedure Free (Pref : in out Preference_Record);
   --  Free the memory associated with Pref

   type Integer_Preference_Record is new Preference_Record with private;
   type Boolean_Preference_Record is new Preference_Record with private;
   type String_Preference_Record  is new Preference_Record with private;
   type Color_Preference_Record   is new Preference_Record with private;
   type Font_Preference_Record    is new Preference_Record with private;
   type Key_Preference_Record     is new Preference_Record with private;
   type Style_Preference_Record   is new Preference_Record with private;
   type Enum_Preference_Record  is abstract new Preference_Record with private;

   type Integer_Preference is access all Integer_Preference_Record'Class;
   type Boolean_Preference is access all Boolean_Preference_Record'Class;
   type String_Preference  is access all String_Preference_Record'Class;
   type Color_Preference   is access all Color_Preference_Record'Class;
   type Font_Preference    is access all Font_Preference_Record'Class;
   type Key_Preference     is access all Key_Preference_Record'Class;
   type Style_Preference   is access all Style_Preference_Record'Class;
   type Enum_Preference    is access all Enum_Preference_Record'Class;

   procedure Register
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Pref                      : access Preference_Record'Class);
   --  Set common attributes of all preferences, and register that preference
   --  in the manager. This function only needs to be called if you are
   --  creating your own types of preferences, and is already called
   --  automatically when using one of the Create functions below.

   -------------------------------
   -- Editing preferences types --
   -------------------------------
   --  Prefernces can be edited graphically through the preferences dialog.
   --  By default, GPS knows how to edit the basic types of preferences, but if
   --  you use a custom type of preference, you should provide your own editor
   --  through Create below.
   --  See for instance GPS.Kernel.Charsets for more examples.

   type Manager_Preference is record
      Manager : Preferences_Manager;
      Pref    : Preference;
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
      Default                   : String)
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
      Default_Modifier          : Gdk.Types.Gdk_Modifier_Type;
      Default_Key               : Gdk.Types.Gdk_Key_Type)
      return Key_Preference;
   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Default_Font              : String;
      Default_Fg                : String;
      Default_Bg                : String)
      return Style_Preference;
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

   overriding function Get_Pref
     (Pref : access String_Preference_Record) return String;

   function Get_Pref (Pref : access Integer_Preference_Record) return Integer;
   function Get_Pref (Pref : access Boolean_Preference_Record) return Boolean;

   overriding function Get_Pref
     (Pref : access Color_Preference_Record) return String;
   function Get_Pref
     (Pref : access Color_Preference_Record) return Gdk.Color.Gdk_Color;

   overriding function Get_Pref
     (Pref : access Font_Preference_Record) return String;
   function Get_Pref
     (Pref : access Font_Preference_Record)
      return Pango.Font.Pango_Font_Description;

   procedure Get_Pref
     (Pref     : access Key_Preference_Record;
      Modifier : out Gdk.Types.Gdk_Modifier_Type;
      Key      : out Gdk.Types.Gdk_Key_Type);

   overriding function Get_Pref
     (Pref : access Style_Preference_Record) return String;
   function Get_Pref_Font
     (Pref     : access Style_Preference_Record)
      return Pango.Font.Pango_Font_Description;
   function Get_Pref_Fg
     (Pref     : access Style_Preference_Record) return Gdk.Color.Gdk_Color;
   function Get_Pref_Bg
     (Pref     : access Style_Preference_Record) return Gdk.Color.Gdk_Color;

   function Get_Pref (Pref : access Enum_Preference_Record) return Integer;
   --  Get the value for a preference. The default value is returned if the
   --  user hasn't explicitely overriden it.
   --  Colors have already been allocated when they are returned.
   --  The Font_Description must not be freed by the caller
   --  For enumeration, it returns the 'Pos of the enumeration value.

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
     (Pref     : Key_Preference;
      Manager  : access Preferences_Manager_Record'Class;
      Modifier : Gdk.Types.Gdk_Modifier_Type;
      Key      : Gdk.Types.Gdk_Key_Type);
   procedure Set_Pref
     (Pref         : Style_Preference;
      Manager      : access Preferences_Manager_Record'Class;
      Font, Fg, Bg : String);
   --  Change the value of a preference. This overrides the default value if
   --  this preference is set for the first time.

   procedure Emit_Pref_Changed
     (Manager : access Preferences_Manager_Record'Class);
   --  If the Preference dialog is displayed, emit the preferences_changed
   --  signal to invite all widgets to refresh themselves. This is only useful
   --  when writing your own type of preference and overriding Set_Pref.

   ---------------------------------------------
   -- Loading and saving preferences to files --
   ---------------------------------------------

   procedure Load_Preferences
     (Manager : access Preferences_Manager_Record; File_Name : String);
   --  Load the preferences from a specific file.
   --  The preferences can be loaded even if they have not been registered
   --  yet.

   procedure Save_Preferences
     (Manager   : access Preferences_Manager_Record;
      File_Name : String;
      Success   : out Boolean);
   --  Save the default preferences to File_Name.

   -------------------------
   -- Editing preferences --
   -------------------------
   --  The following types can be used to add your own pages to the preferences
   --  dialog. Most of the time, though, the pages will be created
   --  automatically by GPS to show the registered preferences.

   type Preferences_Page_Record is abstract tagged null record;
   type Preferences_Page is access all Preferences_Page_Record'Class;

   type Preferences_Page_Array is array (Natural range <>)
      of Preferences_Page;
   type Preferences_Page_Array_Access is access Preferences_Page_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Preferences_Page_Array, Preferences_Page_Array_Access);

   function Name
     (Pref : access Preferences_Page_Record) return String is abstract;
   --  Return the name to use for this page in the list on the left of the
   --  preferences dialog.

   function Create
     (Pref : access Preferences_Page_Record) return Gtk.Widget.Gtk_Widget
      is abstract;
   --  Return a widget to display in the preferences dialog.

   procedure Validate
     (Pref   : access Preferences_Page_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is abstract;
   --  Take into acount the contents of the page. This would for instance
   --  modify the current preferences. There is no need to save previous
   --  values of the preferences, since this is done automatically prior to
   --  validating all the pages.
   --  This can be called any number of times if the user presses Apply in the
   --  preferences dialog.

   procedure Undo
     (Pref   : access Preferences_Page_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Undo the previous effect of Validate.
   --  There is nothing to be done if Undo would simply restore the previous
   --  value of the preferences, since this is done automatically prior to
   --  undoing the effect of all the pages. This is the default behavior.
   --  This might be called even if Validate has not previously been called for
   --  this page.

   type Action_Callback is access procedure
     (Manager : access Preferences_Manager_Record'Class);

   procedure Edit_Preferences
     (Manager            : access Preferences_Manager_Record;
      Parent             : access Gtk.Window.Gtk_Window_Record'Class;
      On_Changed         : Action_Callback;
      Custom_Pages       : Preferences_Page_Array);
   --  Open a dialog to edit the registered preferences.
   --  When OK is clicked, the preferences in Manager are changed, the dialog
   --  is destroyed, and On_Changed is called.
   --  When Apply is clicked, the preferences in Manager are changed, the
   --  dialog is not destroyed, and On_Changed is called.
   --  When Cancel is clicked, the preferences are restored as they were before
   --  Manager was displayed, the dialog is destroyed, and On_Changed is called
   --  if at least one apply was emitted before (since we need to restore the
   --  widgets to their appropriate state).

   function Get_Editor
     (Manager : access Preferences_Manager_Record)
      return Gtk.Widget.Gtk_Widget;
   --  Return the Preferences dialog if it is currently open

   --------------------------
   -- Saving and restoring --
   --------------------------
   --  The following subprograms can be used to temporarily save the value of
   --  the registered preferences. This is used mostly to implemented the
   --  Apply/Cancel buttons.

   type Saved_Prefs_Data is private;

   procedure Save_Preferences
     (Manager : access Preferences_Manager_Record;
      Saved   : out Default_Preferences.Saved_Prefs_Data);
   --  Save the current value of the preferences

   procedure Restore_Preferences
     (Saved   : Default_Preferences.Saved_Prefs_Data);
   --  Restore the previous value of the preferences.
   --  Saved must not be destroyed afterwards

   procedure Destroy (Data : in out Saved_Prefs_Data);
   --  Free the memory occupied by Data

   Signal_Preferences_Changed : constant Glib.Signal_Name :=
                                  "preferences_changed";

private

   type Preference_Record is abstract tagged record
      Name : GNAT.Strings.String_Access;
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
   end record;
   overriding function Get_Pref
     (Pref : access Integer_Preference_Record) return String;
   overriding procedure Set_Pref
     (Pref    : access Integer_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Edit
     (Pref               : access Integer_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget;

   type Boolean_Preference_Record is new Preference_Record with record
      Bool_Value    : Boolean;
   end record;
   overriding function Get_Pref
     (Pref : access Boolean_Preference_Record) return String;
   overriding procedure Set_Pref
     (Pref    : access Boolean_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Edit
     (Pref               : access Boolean_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget;

   type String_Preference_Record is new Preference_Record with record
      Str_Value     : GNAT.Strings.String_Access;
   end record;
   overriding procedure Set_Pref
     (Pref    : access String_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Edit
     (Pref               : access String_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget;
   overriding procedure Free (Pref : in out String_Preference_Record);

   type Color_Preference_Record is new Preference_Record with record
      Color_Value   : GNAT.Strings.String_Access;
      Color         : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
   end record;
   overriding procedure Set_Pref
     (Pref    : access Color_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Edit
     (Pref               : access Color_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget;
   overriding procedure Free (Pref : in out Color_Preference_Record);

   type Font_Preference_Record is new Preference_Record with record
      Font_Value    : GNAT.Strings.String_Access;
      Descr         : Pango.Font.Pango_Font_Description;
   end record;
   overriding procedure Set_Pref
     (Pref    : access Font_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Edit
     (Pref               : access Font_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget;
   overriding procedure Free (Pref : in out Font_Preference_Record);

   type Key_Preference_Record is new Preference_Record with record
      Key_Modifier  : Gdk.Types.Gdk_Modifier_Type;
      Key_Value     : Gdk.Types.Gdk_Key_Type;
   end record;
   overriding function Get_Pref
     (Pref : access Key_Preference_Record) return String;
   overriding procedure Set_Pref
     (Pref    : access Key_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Edit
     (Pref               : access Key_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget;

   type Style_Preference_Record is new Preference_Record with record
      Style_Font    : GNAT.Strings.String_Access;
      Font_Descr    : Pango.Font.Pango_Font_Description;
      Style_Fg      : GNAT.Strings.String_Access;
      Fg_Color      : Gdk.Color.Gdk_Color;
      Style_Bg      : GNAT.Strings.String_Access;
      Bg_Color      : Gdk.Color.Gdk_Color;
   end record;
   overriding procedure Set_Pref
     (Pref    : access Style_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);
   overriding function Edit
     (Pref               : access Style_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget;
   overriding procedure Free (Pref : in out Style_Preference_Record);

   type Enum_Preference_Record is abstract new Preference_Record with record
      Enum_Value    : Integer;
   end record;
   overriding function Get_Pref
     (Pref : access Enum_Preference_Record) return String;
   overriding procedure Set_Pref
     (Pref    : access Enum_Preference_Record;
      Manager : access Preferences_Manager_Record'Class;
      Value   : String);

   package Preferences_Maps is new Ada.Containers.Doubly_Linked_Lists
     (Preference);

   type Preferences_Manager_Record is tagged record
      Preferences   : Preferences_Maps.List;
      Pref_Editor   : Gtk.Widget.Gtk_Widget;
      --  The current preferences editor. This is set to null if there is no
      --  editor open currently
   end record;

   type Saved_Prefs_Data_Record;
   type Saved_Prefs_Data is access Saved_Prefs_Data_Record;

end Default_Preferences;
