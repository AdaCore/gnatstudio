-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

--  This package provides support for the handling of preferences in GPS. The
--  preferences system is based on the glib types Param_Spec. These include
--  natively the description of the param_spec, which can be used in a
--  graphical editor, as well as precise information about the allowed values
--  for the type. It also provides support for closely associating Ada
--  enumeration types with C types, thus allowing almost any type of
--  preference.

with Glib; use Glib;
with Glib.Properties;
with Glib.Properties.Creation;
with Basic_Types;
with Gdk.Color;
with Pango.Font;
with Glib.XML;
with Gtk.Widget;
with Gtk.Window;

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

   type Param_Spec_Array is array (Natural range <>) of Glib.Param_Spec;

   type Param_Spec_Color is new Glib.Properties.Creation.Param_Spec_String;
   function Gnew_Color
     (Name, Nick, Blurb : String;
      Default           : String;
      Flags             : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec_Color;

   type Param_Spec_Font is new Glib.Properties.Creation.Param_Spec_String;
   function Gnew_Font
     (Name, Nick, Blurb : String;
      Default           : String;
      Flags             : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec_Font;

   procedure Register_Property
     (Manager : access Preferences_Manager_Record;
      Param   : Glib.Param_Spec;
      Page    : String);
   --  Register a new property.
   --  If the flags in param have Param_Writable, then this preference can be
   --  edited graphically through the preferences dialog.
   --  The Name is the name used when saving in the XML file. It shouldn't
   --  contains any space, and should be a valid XML tag.
   --  The nick_name field is used in the preferences dialog.
   --  The description is used in the tooltips to provide more information the
   --  preference.
   --  Page is the name of the preferences dialog page that should contain this
   --  property. A new page will be created if no such page already exists. If
   --  the name is of the form Page:subpage, then a notebook will be created
   --  for Page, with a page called subpage.
   --  Due to some limitations in glib, the name in Param must only use
   --  alphanumeric characters or '-'.

   function Get_Pref
     (Manager : access Preferences_Manager_Record;
      Pref    : Glib.Properties.Creation.Param_Spec_Int) return Glib.Gint;
   function Get_Pref
     (Manager : access Preferences_Manager_Record;
      Pref   : Glib.Properties.Creation.Param_Spec_Boolean) return Boolean;
   function Get_Pref
     (Manager : access Preferences_Manager_Record;
      Pref   : Glib.Properties.Creation.Param_Spec_String) return String;
   function Get_Pref
     (Manager : access Preferences_Manager_Record;
      Pref   : Param_Spec_Color) return Gdk.Color.Gdk_Color;
   function Get_Pref
     (Manager : access Preferences_Manager_Record;
      Pref   : Param_Spec_Font) return Pango.Font.Pango_Font_Description;
   function Get_Pref
     (Manager : access Preferences_Manager_Record;
      Pref   : Glib.Properties.Creation.Param_Spec_Enum) return Gint;
   --  Get the value for a preference. The default value is returned if the
   --  user hasn't explicitely overriden it.
   --  Colors have already been allocated when they are returned.
   --  The Font_Description must not be freed by the caller
   --  For enumeration, it returns the 'Pos of the enumeration value.

   procedure Set_Pref
     (Manager : access Preferences_Manager_Record;
      Name    : String;
      Value   : Glib.Gint);
   procedure Set_Pref
     (Manager : access Preferences_Manager_Record;
      Name    : String;
      Value   : Boolean);
   procedure Set_Pref  --   String, Font, Color
     (Manager : access Preferences_Manager_Record;
      Name    : String;
      Value   : String);
   --  Change the value of a preference. This overrides the default value if
   --  this preference is set for the first time.
   --  Checks are made to make sure the type of Name is valid.

   function Get_Page
     (Manager : access Preferences_Manager_Record;
      Param : Param_Spec) return String;
   --  Return the name of the page for the Name preference.
   --  Constraint_Error is raised if the preference doesn't exist.

   procedure Load_Preferences
     (Manager : access Preferences_Manager_Record; File_Name : String);
   --  Load the preferences from a specific file.
   --  The preferences can be loaded even if they have not been registered
   --  yet.

   procedure Save_Preferences
     (Manager : access Preferences_Manager_Record; File_Name : String);
   --  Save the default preferences to File_Name.

   function Editor_Widget
     (Manager : access Preferences_Manager_Record; Param : Param_Spec)
      return Gtk.Widget.Gtk_Widget;
   --  Return a widget for graphical editing of Param. The exact type of widget
   --  depends on the type of data in Param.
   --  When the widget is modified, the corresponding preference is
   --  automatically changed as well. However, nobody gets informed that the
   --  preference has changed.

   type Action_Callback is access procedure
     (Manager : access Preferences_Manager_Record'Class);

   procedure Edit_Preferences
     (Manager            : access Preferences_Manager_Record;
      Parent             : access Gtk.Window.Gtk_Window_Record'Class;
      On_Changed         : Action_Callback);
   --  Open a dialog to edit the registered preferences.
   --  When OK is clicked, the preferences in Manager are changed, the dialog
   --  is destroyed, and On_Changed is called.
   --  When Apply is clicked, the preferences in Manager are changed, the
   --  dialog is not destroyed, and On_Changed is called.
   --  When Cancel is clicked, the preferences are restored as they were before
   --  Manager was displayed, the dialog is destroyed, and On_Changed is called
   --  if at least one apply was emitted before (since we need to restore the
   --  widgets to their appropriate state).


private
   type Preference_Information;
   type Preference_Information_Access is access Preference_Information;
   type Preference_Information is record
      Page       : Basic_Types.String_Access;
      Param      : Glib.Param_Spec;
      Next       : Preference_Information_Access;
   end record;

   type XML_Cache is record
      Descr      : Pango.Font.Pango_Font_Description;
   end record;
   package XML_Font is new Glib.XML (XML_Cache);

   type Preferences_Manager_Record is tagged record
      Default     : Preference_Information_Access;
      Preferences : XML_Font.Node_Ptr;
   end record;

end Default_Preferences;
