-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2005-2007, AdaCore           --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package handles the charsets supported by GPS

with Glib;
with Gtk.Combo;

package GPS.Kernel.Charsets is

   -----------------
   -- Preferences --
   -----------------

   type Param_Spec_Charset is new Glib.Param_Spec;

   function Gnew_Charset
     (Name, Nick, Blurb   : String;
      Default             : String;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec_Charset;
   --  Create a new preference representing a charset

   function Get_Pref (Pref : Param_Spec_Charset) return String;
   --  Return the currently selected charset

   procedure Register_Preferences
     (Kernel : access Kernel_Handle_Record'Class);
   --  Register the charset-related preferences

   ------------
   -- Widget --
   ------------

   function Create_Charset_Combo
     (File    : VFS.Virtual_File;
      Default : String := "") return Gtk.Combo.Gtk_Combo;
   --  Return a combo box that can be used to edit the charset associated with
   --  a file.
   --  Default is used if File is VFS.No_File. If unspecified, the
   --  corresponding preference is used.

   function Get_File_Charset (File : VFS.Virtual_File) return String;
   --  Return the charset that should be used to edit File.
   --  If File is VFS.No_File, the default charset is returned.

   procedure Set_File_Charset
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      File    : VFS.Virtual_File;
      Charset : String := "");
   --  Set the charset that should be used to edit File.
   --  If Charset is left to the empty string, the default charset specified in
   --  the preferences will be used

end GPS.Kernel.Charsets;
