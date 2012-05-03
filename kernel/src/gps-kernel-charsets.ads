------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

--  This package handles the charsets supported by GPS

with Default_Preferences;  use Default_Preferences;
with Gtk.Combo_Box;
with Gtk.Widget;

package GPS.Kernel.Charsets is

   -----------------
   -- Preferences --
   -----------------

   type Charset_Preference_Record is new
     String_Preference_Record with null record;
   type Charset_Preference is access all Charset_Preference_Record'Class;

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Default                   : String)
      return Charset_Preference;
   --  Create a new preference representing a charset

   overriding function Edit
     (Pref               : access Charset_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget;

   procedure Register_Preferences
     (Kernel : access Kernel_Handle_Record'Class);
   --  Register the charset-related preferences

   ------------
   -- Widget --
   ------------

   function Create_Charset_Combo
     (File    : GNATCOLL.VFS.Virtual_File;
      Default : String := "") return Gtk.Combo_Box.Gtk_Combo_Box;
   --  Return a combo box that can be used to edit the charset associated with
   --  a file.
   --  Default is used if File is VFS.No_File. If unspecified, the
   --  corresponding preference is used.

   function Selected_Charset
     (Combo : Gtk.Combo_Box.Gtk_Combo_Box) return String;
   --  Return Charset name from Charset_Combo

   function Get_File_Charset (File : GNATCOLL.VFS.Virtual_File) return String;
   --  Return the charset that should be used to edit File.
   --  If File is VFS.No_File, the default charset is returned.

   procedure Set_File_Charset
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Charset : String := "");
   --  Set the charset that should be used to edit File.
   --  If Charset is left to the empty string, the default charset specified in
   --  the preferences will be used

end GPS.Kernel.Charsets;
