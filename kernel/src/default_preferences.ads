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

--  This package handles the default value for all the preferences in GPS.
--  Note that all the preferences must be registered through this package
--  before being available to the user.
--  This package is separated from Glide_Kernel.Preferences for elaboration
--  circularity reasons.

with Glib.Properties;
with Basic_Types;

package Default_Preferences is

   type Property_Color is new Glib.Property;
   type Property_Font  is new Glib.Property;

   function Register_Property
     (Name            : String;
      Default         : Glib.Gint;
      Description     : String;
      GUI_Description : String;
      Page            : String;
      Editable        : Boolean := True) return Glib.Properties.Property_Int;
   --  Register a new property and its default value.
   --  The returned value should be used later on to retrieve the value of the
   --  property.
   --  Description is the help string for this property. It is displayed in
   --  tooltips in the preferences dialog.
   --  GUI_Description is the label to use in the preferences dialog for this
   --  property.
   --  Page is the name of the preferences dialog page that should contain this
   --  property. A new page will be created if no such page already exists. If
   --  the name is of the form Page::subpage, then a notebook will be created
   --  for Page, with a page called subpage.
   --
   --  If Editable is False, then this preference cannot be edited graphically
   --  by the user, although it can be changed directly in the preferences
   --  file.

   function Register_Property
     (Name            : String;
      Default         : Glib.Guint;
      Description     : String;
      GUI_Description : String;
      Page            : String;
      Editable        : Boolean := True) return Glib.Properties.Property_Uint;

   function Register_Property
     (Name            : String;
      Default         : Boolean;
      Description     : String;
      GUI_Description : String;
      Page            : String;
      Editable        : Boolean := True)
      return Glib.Properties.Property_Boolean;

   function Register_Property
     (Name            : String;
      Default         : String;
      Description     : String;
      GUI_Description : String;
      Page            : String;
      Editable        : Boolean := True)
      return Glib.Properties.Property_String;

   function Register_Property
     (Name            : String;
      Default         : String;
      Description     : String;
      GUI_Description : String;
      Page            : String;
      Editable        : Boolean := True) return Property_Color;

   function Register_Property
     (Name            : String;
      Default         : String;
      Description     : String;
      GUI_Description : String;
      Page            : String;
      Editable        : Boolean := True) return Property_Font;

   function Find_Default_Pref (Name : String) return String;
   --  Return the value, as a string, in the default preferences that matches
   --  Name

   procedure Save_Default_Preferences (File_Name : String);
   --  Save the default preferences to File_Name.

   function Get_Description (Name : String) return String;
   --  Return the full description of the Name preference.

   function Get_GUI_Description (Name : String) return String;
   --  Return the GUI description of the Name preference.

   function Get_Page (Name : String) return String;
   --  Return the name of the page for the Name preference.

   function Get_Type (Name : String) return Glib.GType;
   --  Return the type of the Name preference.

   function Is_Editable (Name : String) return Boolean;
   --  Return True if the preference can be edited graphically.

   function Get_All_Preferences return Basic_Types.String_Array;
   --  Return the name of all the registered preferences.
   --  It is the responsability of the caller to free the returned array.

end Default_Preferences;
