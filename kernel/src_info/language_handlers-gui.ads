-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2007                         --
--                              AdaCore                              --
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

--  This package contains graphical utilities related to Language_Handlers.

with Gtk.Combo;
with GNATCOLL.VFS;

package Language_Handlers.GUI is

   function Create_Language_Combo
     (Handler : access Language_Handler_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Default : String := "") return Gtk.Combo.Gtk_Combo;
   --  Create a combo box to select the language for File.
   --  File is used to select the default value in the combo.
   --  The first entry in the combo always indicates that the language from the
   --  project should be used.
   --  Default is used when File is VFS.No_File (and thus has no associated
   --  language)

end Language_Handlers.GUI;
