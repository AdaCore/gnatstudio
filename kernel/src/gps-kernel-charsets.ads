------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

with Glib.Object;          use Glib.Object;
with Gtk.Combo_Box_Text;
with Gtk.Widget;
with Gtkada.Types;

with GNATCOLL.VFS;

package GPS.Kernel.Charsets is

   -----------------
   -- Preferences --
   -----------------

   type Charset_Preference_Record is new
     String_Preference_Record with null record;
   type Charset_Preference is access all Charset_Preference_Record'Class;

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Path                      : Preference_Path;
      Name, Label, Doc          : String;
      Default                   : String)
      return Charset_Preference;
   --  Create a new preference representing a charset

   overriding function Edit
     (Pref               : access Charset_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget;

   overriding procedure Update_On_Pref_Changed
     (Pref   : access Charset_Preference_Record;
      Widget : access GObject_Record'Class);

   procedure Register_Preferences
     (Kernel : access Kernel_Handle_Record'Class);
   --  Register the charset-related preferences

   ------------
   -- Widget --
   ------------

   function Create_Charset_Combo
     (File    : GNATCOLL.VFS.Virtual_File;
      Default : String := "") return Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
   --  Return a combo box that can be used to edit the charset associated with
   --  a file.
   --  Default is used if File is VFS.No_File. If unspecified, the
   --  corresponding preference is used.

   function Selected_Charset
     (Combo : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text) return String;
   --  Return Charset name from Charset_Combo

   function Get_Default_Charset return String;
   --  Returns default charset.

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

   type File_Props is record
      CR_Found              : Boolean;
      NUL_Found             : Boolean;
      Trailing_Spaces_Found : Boolean;
      Trailing_Lines_Found  : Boolean;
      Invalid_UTF8          : Boolean;
   end record;
   --  Various properties automatically detected for files

   procedure Read_File_With_Charset
     (File     : GNATCOLL.VFS.Virtual_File;
      UTF8     : out Gtkada.Types.Chars_Ptr;
      UTF8_Len : out Natural;
      Props    : out File_Props);
   --  Read the file from the disk, and apply charset conversions as needed.
   --  The returned string is UTF8. Only the UTF8_Len first characters should
   --  be used.
   --  This also normalizes newline characters.
   --  Caller must free UTF8. NOTE: make sure to free the string with g_free,
   --  not Interfaces.C.Strings.Free!

end GPS.Kernel.Charsets;
