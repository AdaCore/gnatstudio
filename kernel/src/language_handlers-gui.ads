------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

--  This package contains graphical utilities related to Language_Handlers.

with Gtk.Combo_Box_Text;
with GNATCOLL.VFS;

package Language_Handlers.GUI is

   function Create_Language_Combo
     (Handler : access Language_Handler_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Default : String := "") return Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
   --  Create a combo box to select the language for File.
   --  File is used to select the default value in the combo.
   --  The first entry in the combo always indicates that the language from the
   --  project should be used.
   --  Default is used when File is VFS.No_File (and thus has no associated
   --  language)

end Language_Handlers.GUI;
