------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;                 use Ada.Strings.Unbounded;

with GNATCOLL.VFS;                          use GNATCOLL.VFS;

with Gdk.RGBA;                              use Gdk.RGBA;
with Glib;                                  use Glib;
with Glib.Object;                           use Glib.Object;

with Gtk.Box;
with Gtk.Color_Button;
with Gtk.Combo_Box_Text;
with Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Widget;

with Pango.Font;                            use Pango.Font;

package Default_Preferences is

   type Preferences_Manager_Record is tagged private;
   type Preferences_Manager is access all Preferences_Manager_Record'Class;
   --  Manages a set of preferences.
   --  You can extend this type for instance if you need a fast access to some
   --  of the preferences, by providing a cache system, and redefining the
   --  Set_Pref subprograms.

   type Preference_Record is abstract tagged private;
   type Preference is access all Preference_Record'Class;
   --  Type used to represent a general preference.
   --  You can extend this type to represent more specific preferences
   --  (e.g: Integer_Preference_Record)

   type Preferences_Editor_Interface is interface;
   type Preferences_Editor is access all Preferences_Editor_Interface'Class;
   --  Interface defining a common API for all the preferences editor
   --  widgets.

   type Preferences_Group_Record is tagged private;
   type Preferences_Group is access all Preferences_Group_Record'Class;
   --  Type used to represent a group of preferences.
   --  Preferences groups are used to gather preferences within a same page,
   --  according to their objective.

   type Preferences_Page_Record is abstract tagged private;
   type Preferences_Page is access all Preferences_Page_Record'Class;
   --  Type used to represent a page containing preferences.
   --  This is only a model : the way the preferences are stored and displayed
   --  must be defined when extending this type, by overriding its primitives.

   subtype Preferences_Page_Name is String;
   --  Subtype representing a name for preferences pages and used to
   --  specify where a given page should be in the preferences dialog.
   --
   --  A Preferences_Page_Name may designate:
   --
   --    . A non-visible page if it's set to an empty string
   --
   --    . A root page if it's set to a non empty string, without containing a
   --      '/' delimiter followed by other caracters
   --      (e.g : "Editor" or "Editor/")
   --
   --    . A subpage it's set to a non empty string containing a root page name
   --      followed by a '/' delimiter and the subpage name
   --      (e.g : "Editor/Fonts" or "Editor/Fonts/")

   subtype Preferences_Group_Name is String;
   --  Subtype representing a name for the preferences groups.
   --
   --  A Preference_Group_Name may designate any String.

   subtype Preference_Path is String;
   --  Subtype representing a preference path and used to specify where a the
   --  preference should be displayed (which page/groups) in the preferences
   --  dialog.
   --
   --  A Preference_Path may designate:
   --
   --    . A Preferences_Page_Name, if the preference does not belong to a
   --      specific group (e.g: "Editor" or "Editor/")
   --
   --    . A Preferences_Page_Name, followed by a ':' delimiter and the name
   --      of the group of the preference
   --      (e.g : "Editor:General" or "Editor/:General")

   type Preferences_Page_Type is
     (Visible_Page, Integrated_Page, Assistant_Page, Hidden_Page);
   --  Used by the preferences editor dialog to know if a page should be
   --  visible from the editor or not.
   --
   --  Visible_Page: denotes the pages that should be displayed by the editor.
   --
   --  Integrated_Page: denotes pages that should be displayed within other
   --  pages (e.g: a specific plugin page can be displayed in a root 'Plugins'
   --  page. Then, the preferences editor should not
   --  create a specific entry for it.
   --
   --  Assistant_Page: a page that is displayed in the Preferences Assistant.
   --  The preferences editor should not create a specific entry for it and the
   --  preferences search module shopuld not match preferences listed in the
   --  assistant.
   --
   --  Hidden_Page: denotes the page containing all the hidden preferences.

   procedure Extract_Page_And_Group_Names
     (Path       : String;
      Page_Name  : out Unbounded_String;
      Group_Name : out Unbounded_String);
   --  Extract the page's name and the group's name (if any) from the given
   --  preference's path.

   procedure Set_GObject_To_Update
     (Pref   : not null access Preference_Record;
      Obj    : not null access GObject_Record'Class);
   --  Add/Update the key association in the Preferences_GObjects_Map
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

   -------------------------
   -- Preferences_Manager --
   --------------------------

   procedure Destroy (Manager : in out Preferences_Manager_Record);
   procedure Destroy (Manager : in out Preferences_Manager);
   --  Free the memory used by Manager, including all the registered
   --  preferences. Get_Pref mustn't be used afterwards.

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
      Path                   : Preference_Path;
      Name, Label, Doc       : String;
      Pref                   : not null access Preference_Record'Class;
      Priority               : Integer := -1);
   --  Set common attributes of all preferences, and register that preference
   --  in the manager. This function only needs to be called if you are
   --  creating your own types of preferences, and is already called
   --  automatically when using one of the Create functions below.
   --  Priority is used to order the preferences in their respective groups,
   --  in decreasing order.

   procedure Register_Page
     (Self             : not null access Preferences_Manager_Record;
      Name             : Preferences_Page_Name;
      Page             : not null Preferences_Page;
      Priority         : Integer := -1;
      Page_Type        : Preferences_Page_Type := Visible_Page;
      Replace_If_Exist : Boolean := False);
   --  Register a new preferences page in the manager.
   --  If Replace_If_Exist is True and if an association for this name already
   --  exists, replace the Preferences_Page associated to Name with the one
   --  given in parameter and copy the preferences registered in the previous
   --  page.

   function Get_Registered_Page
     (Self             : not null access Preferences_Manager_Record'Class;
      Name             : Preferences_Page_Name;
      Create_If_Needed : Boolean := False) return Preferences_Page;
   --  Return the page registered in the manager for this name.
   --  If no page has been registered for this name, return null if
   --  Create_If_Needed is False. Otherwise, create and register a default
   --  page for the given name.

   function Is_Frozen
     (Self : not null access Preferences_Manager_Record'Class) return Boolean;
   --  Return True if Self is frozen

   ----------------------------------
   -- Preferences_Editor_Interface --
   ----------------------------------

   function Get_Widget
     (Self : not null access Preferences_Editor_Interface)
      return Gtk.Widget.Gtk_Widget is abstract;
   --  Return the main Gtk_Widget of the preferences editor view

   procedure Display_Page
     (Self      : not null access Preferences_Editor_Interface;
      Page_Name : Preferences_Page_Name) is abstract;
   --  Display the page view associated with Page_Name.

   procedure Display_Pref
     (Self      : not null access Preferences_Editor_Interface;
      Pref      : not null Preference;
      Highlight : Boolean := False) is abstract;
   --  Display the page view in which Pref is contained, highlighting the
   --  preference or not depending on Highlight.

   function Get_Page_View
     (Self      : not null access Preferences_Editor_Interface;
      Page_Name : Preferences_Page_Name)
      return Gtk.Widget.Gtk_Widget is abstract;
   --  Return the page view associated with Page_Name.

   function Is_Displaying_Hidden_Preferences
     (Self : not null access Preferences_Editor_Interface)
      return Boolean is abstract;
   --  Return True if the editor is currently displaying hidden preferences,
   --  False otherwise.

   -----------------------
   -- Preferences_Group --
   -----------------------

   function Get_Name
     (Self : not null access Preferences_Group_Record) return String;
   --  Return the group's name.

   procedure Add_Pref
     (Self    : not null access Preferences_Group_Record;
      Manager : not null access Preferences_Manager_Record'Class;
      Pref    : not null Preference);
   --  Add a new preference to the given group.

   procedure Remove_Pref
     (Self : not null access Preferences_Group_Record;
      Pref : not null Preference);
   --  Remove Pref of the given group.

   procedure Free (Self : in out Preferences_Group_Record);
   --  Free the memory associated with the given group.

   ----------------------
   -- Preferences_Page --
   ----------------------

   function Get_Name
     (Self : not null access Preferences_Page_Record)
      return Preferences_Page_Name;
   --  Return the page's name.

   function Get_Page_Type
     (Self : not null access Preferences_Page_Record)
      return Preferences_Page_Type;
   --  Return the page's type.

   function Get_Widget
     (Self    : not null access Preferences_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget is abstract;
   --  Return the main widget of the preferences page.
   --  This is the widget we want to append to the preferences editor.

   procedure Register_Group
     (Self             : not null access Preferences_Page_Record;
      Name             : String;
      Group            : not null access Preferences_Group_Record'Class;
      Priority         : Integer := -1;
      Replace_If_Exist : Boolean := False);
   --  Register a new group in the given preferences page.
   --  Priority is used to order the groups in their respective page, in
   --  decreasing order.
   --  If Replace_If_Exist is True and if an association for this name already
   --  exists, replace the group associated to Name with the one given in
   --  parameter and copy the preferences registered in the previous
   --  group.

   function Get_Registered_Group
     (Self             : not null access Preferences_Page_Record'Class;
      Name             : String;
      Create_If_Needed : Boolean := False) return Preferences_Group;
   --  Return the group registered in the given Page for this name.
   --  If no group has been registered for this name, return null if
   --  Create_If_Needed is False. Otherwise, create and register a new group
   --  and return it.

   function Is_Root_Page (Page_Name : Preferences_Page_Name) return Boolean;
   --  Return True if the given page name refers to a root page, False
   --  otherwise.

   function Get_Root_Page (Page_Name : Preferences_Page_Name) return String;
   --  Return the root page for a given page name (e.g: for "Editor/Ada/",
   --  return "Editor/").
   --  Return "" if Page_Name refers already to a root page.

   procedure Free (Self : in out Preferences_Page_Record);
   --  Free the memory associated with the given page.

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

   procedure Free (Pref : in out Preference_Record) is null;
   --  Free the memory associated with Pref

   type Integer_Preference_Record is new Preference_Record with private;
   type Boolean_Preference_Record is new Preference_Record with private;
   type String_Preference_Record  is new Preference_Record with private;
   type Color_Preference_Record   is new Preference_Record with private;
   type Font_Preference_Record    is new Preference_Record with private;
   type Style_Preference_Record is tagged;
   type Variant_Preference_Record is tagged;
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
      Path                      : Preference_Path;
      Name, Label, Doc          : String;
      Minimum, Maximum, Default : Integer;
      Priority                  : Integer := -1)
      return Integer_Preference;
   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Path                      : Preference_Path;
      Name, Label, Doc          : String;
      Default                   : Boolean;
      Priority                  : Integer := -1)
      return Boolean_Preference;
   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Path                      : Preference_Path;
      Name, Label, Doc    : String;
      Default                   : String;
      Multi_Line                : Boolean := False;
      Priority                  : Integer := -1)
      return String_Preference;
   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Path                      : Preference_Path;
      Name, Label, Doc          : String;
      Default                   : String;
      Priority                  : Integer := -1)
      return Color_Preference;
   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Path                      : Preference_Path;
      Name, Label, Doc         : String;
      Default                   : String;
      Priority                  : Integer := -1)
      return Font_Preference;
   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Path                      : Preference_Path;
      Name, Label, Doc          : String;
      Default_Font              : String;
      Default_Fg                : String;
      Default_Bg                : String;
      Priority                  : Integer := -1)
      return Style_Preference;
   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Path                      : Preference_Path;
      Name, Label, Doc          : String;
      Base                      : Style_Preference;
      Default_Variant           : Variant_Enum;
      Default_Fg                : String;
      Default_Bg                : String;
      Priority                  : Integer := -1)
      return Variant_Preference;
   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Path                      : Preference_Path;
      Name, Label, Doc          : String;
      Priority                  : Integer := -1)
      return Theme_Preference;
   --  Create a new preference and register it in the Manager.
   --  Path designates where the preference should go in the preferences
   --    dialog. Read the Preference_Path documentation for more details.
   --  Name is the name used when saving in the XML file, and when referencing
   --    that preference from a python file. It can contain any character.
   --  Label is the label used in the preferences dialog. It must be human
   --    readable.
   --  Doc is the documentation for this preference, at it appears in the
   --    tooltip of the preferences dialog
   --  Priority is used to order the preferences in their respective groups,
   --    in decreasing order.
   --  See Default_Preferences.Generics to create preferences associated with
   --    enumerations

   function Create_Invisible_Pref
     (Manager : access Preferences_Manager_Record'Class;
      Name    : String;
      Default : Boolean;
      Label   : String;
      Doc     : String := "")
      return Boolean_Preference
   is (Create (Manager,
               Path    => ":Local Configuration",
               Name    => Name,
               Label   => Label,
               Doc     => Doc,
               Default => Default));
   --  Convenience function for creating a boolean invisible preference.
   --  Such preferences are changed via other GUI elements (e.g: view local
   --  menus) or in the advanced preferences page when displayed.

   function Get_Pref_From_Name
     (Self                : not null access Preferences_Manager_Record;
      Name                : String;
      Create_If_Necessary : Boolean := False) return Preference;
   --  Return the corresponding preference.
   --  If it doesn't exist, a temporary preference is created. When the
   --  actual preference is registered, it will replace that temporary entry
   --  but preserve the default value.

   --------------------------------------
   -- Getting the value of preferences --
   --------------------------------------

   function Get_Name  (Pref : access Preference_Record'Class) return String;
   function Get_Label (Pref : access Preference_Record'Class) return String;
   function Get_Path
     (Pref : access Preference_Record'Class) return Preference_Path;
   function Get_Doc   (Pref : access Preference_Record'Class) return String;
   function Get_Page_Name
     (Pref : access Preference_Record'Class) return Preferences_Page_Name;
   function Get_Group_Name
     (Pref : access Preference_Record'Class) return String;

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

   -----------------------------
   -- Style_Preference_Record --
   -----------------------------

   type Style_Preference_Record is new Preference_Record with private;

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

   -------------------------------
   -- Variant_Preference_Record --
   -------------------------------

   type Variant_Preference_Record is new Style_Preference_Record with private;

   overriding function Get_Pref
     (Pref : access Variant_Preference_Record) return String;
   overriding function Get_Pref_Font
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
      Name      : Unbounded_String;
      --  Display name for the theme, including the variant

      Directory : Unbounded_String;
      --  Name for the theme (not including the variant name)

      Dark      : Boolean := False;
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

   type Page_Cursor is private;

   function Get_First_Reference
     (Manager : not null access Preferences_Manager_Record)
      return Page_Cursor;
   procedure Next (C : in out Page_Cursor);
   function Get_Page (Self : in out Page_Cursor) return Preferences_Page;
   --  Iterate all over the registered pages.

   type Subpage_Cursor is private;

   function Get_First_Reference
     (Parent : not null access Preferences_Page_Record)
      return Subpage_Cursor;
   procedure Next (C : in out Subpage_Cursor);
   function Get_Subpage (Self : in out Subpage_Cursor) return Preferences_Page;
   --  Iterate all over the registered subpages

   type Group_Cursor is private;

   function Get_First_Reference
     (Page : not null access Preferences_Page_Record) return Group_Cursor;
   procedure Next (C : in out Group_Cursor);
   function Get_Group (Self : Group_Cursor) return Preferences_Group;
   --  Iterate all over the registered groups for a given page.

   type Preference_Cursor_Type is (From_Manager, From_Group);
   type Preference_Cursor (Cursor_Type : Preference_Cursor_Type) is private;

   function Get_First_Reference
     (Manager : not null access Preferences_Manager_Record)
      return Preference_Cursor;
   function Get_First_Reference
     (Group : not null access Preferences_Group_Record)
      return Preference_Cursor;
   procedure Next (C : in out Preference_Cursor);
   function Get_Pref
     (Self    : Preference_Cursor;
      Manager : not null access Preferences_Manager_Record) return Preference;
   --  Iterate all over the registered preferences.

   ------------------------------
   -- Default_Preferences_Page --
   ------------------------------

   type Default_Preferences_Page_Record is
     new Preferences_Page_Record with private;
   type Default_Preferences_Page is
     access all Default_Preferences_Page_Record'Class;
   --  Default preferences pages. This type of pages is the default one in GPS:
   --  pages of this type will be created when registering a preference that
   --  resides in a page that has not been registered previously.

   overriding function Get_Widget
     (Self    : not null access Default_Preferences_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget;
   --  See inherited documentation.

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

   function Page_Name_Equals (Left, Right : Preferences_Page) return Boolean;
   --  Used to search in pages lists using name equality.

   package Pages_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Preferences_Page, Page_Name_Equals);
   type Page_Cursor is record
      Root_Pages_Curs   : Pages_Lists.Cursor;
      Subpages_Curs     : Pages_Lists.Cursor;
      Is_Root           : Boolean;
   end record;
   type Subpage_Cursor is record
      C : Pages_Lists.Cursor;
   end record;
   --  Used to store pages in the preferences manager.

   function Group_Name_Equals (Left, Right : Preferences_Group) return Boolean;
   --  Used to search in groups lists using name equality.

   package Groups_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Preferences_Group, Group_Name_Equals);
   --  Used to store groups in the pages.

   package Preferences_Names_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (String, "=");
   --  Used to store the names of the preferences that belong to a given group

   package Preferences_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Preference, Ada.Strings.Hash, "=");
   type Preference_Cursor (Cursor_Type : Preference_Cursor_Type) is record
      case Cursor_Type is
         when From_Manager =>
            Map_Curs  : Preferences_Maps.Cursor;
         when From_Group =>
            List_Curs : Preferences_Names_Lists.Cursor;
      end case;
   end record;
   --  Used to map preferences with their names.

   package Preferences_GObjects_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (String, GObject, Ada.Strings.Hash, "=");
   --  Used to map preferences with the GObjects we want to update when
   --  preferences change.

   function "=" (Left, Right : Gtk.Widget.Gtk_Widget) return Boolean
   is (GObject (Left) = GObject (Right));

   package Preferences_Widgets_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (String, Gtk.Widget.Gtk_Widget, Ada.Strings.Hash, "=");
   --  Used to map preferences with the widgets displaying them.

   type Preference_Record is abstract tagged record
      Name       : Unbounded_String;
      --  Name in the .xml file, and used for external references

      Label      : Unbounded_String;
      --  Label used in the preferences dialog

      Path       : Unbounded_String;
      --  Preferences's full path in the preference dialog. Read the
      --  Preference_Path documentation for more details.

      Page_Name  : Unbounded_String;
      --  Name of the preference's page. This is set to null if the preference
      --  is hidden.

      Group_Name : Unbounded_String;
      --  Name of the preference's group. This is set to null if no group has
      --  been specified in its path.

      Doc        : Unbounded_String;
      --  The documentation for this preference

      Priority   : Integer;
      --  Preference's priority. This is used to sort the preferences according
      --  to the order we want to display it.
   end record;

   type Preferences_Group_Record is tagged record
      Name        : Unbounded_String;
      --  Group's name.

      Priority    : Integer;
      --  Group's priority. This is used to sort the groups according to the
      --  order we want to display it.

      Preferences : Preferences_Names_Lists.List;
      --  List of preferences belonging to this group.
   end record;

   package Groups_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Preferences_Group, Ada.Strings.Hash, "=");
   --  Used to map groups with their names.
   type Group_Cursor is record
      C : Groups_Lists.Cursor;
   end record;

   type Preferences_Page_Record is abstract tagged record
      Name      : Unbounded_String;
      --  Name of the page. Read the Preferences_Page_Name documentation for
      --  more details.

      Priority  : Integer;
      --  Page's priority. This is used to sort the pages according to the
      --  order we want to display it.

      Groups    : Groups_Lists.List;
      --  List of groups belonging to this page.

      Page_Type : Preferences_Page_Type;
      --  Type of the page. Used by the editor to know if the page should be
      --  displayed or not.

      Subpages  : Pages_Lists.List;
      --  List of all the subpages. This list is empty if Is_Root is False.
   end record;

   type Default_Preferences_Page_Record is new Preferences_Page_Record
   with null record;

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
      Str_Value     : Unbounded_String;
      Default       : Unbounded_String;
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
   overriding function Is_Default
     (Self : not null access String_Preference_Record) return Boolean
   is
     (Self.Default = Self.Str_Value);
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

   type Preferences_Manager_Record is tagged record
      Pages         : Pages_Lists.List;
      --  List of the preferences pages belonging to this manager.

      Preferences   : Preferences_Maps.Map;
      --  Global map containing all the preferences.
      --  This is used when we need to iterate over all the preferences,
      --  without needing to know in which page it resides (e.g: when saving
      --  preferences).

      Pref_Editor   : access Preferences_Editor_Interface'Class;
      --  The current preferences editor. This is set to null if there is no
      --  editor open currently.

      Loading_Prefs : Boolean := False;
      --  True while we are loading the preferences. This is used to disable
      --  saving the preferences when we are setting their initial value.

      Freeze_Count : Integer := 0;
      --  How many calls have been made to "Freeze". If 0, this manager is
      --  not frozen.
   end record;

end Default_Preferences;
