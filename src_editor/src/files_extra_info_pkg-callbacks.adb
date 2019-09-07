------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Gtk.Window;             use Gtk.Window;
with Gtkada.File_Selector;   use Gtkada.File_Selector;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Intl;               use GPS.Intl;

package body Files_Extra_Info_Pkg.Callbacks is

   ------------------------------
   -- On_Browse_Button_Clicked --
   ------------------------------

   procedure On_Browse_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Extra : constant Files_Extra_Info_Access :=
                Files_Extra_Info_Access (Object);
      S     : constant GNATCOLL.VFS.Virtual_File :=
                Select_Directory
                  (-"Select a directory",
                   Parent  => Gtk_Window (Get_Toplevel (Object)),
                   Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                   History           => null);  --  ??? No history

   begin
      if S /= No_File then
         Set_Text (Extra.Directory_Entry, GNATCOLL.VFS.Display_Full_Name (S));
         --  ??? What if the filesystem path is non-UTF8?
      end if;
   end On_Browse_Button_Clicked;

end Files_Extra_Info_Pkg.Callbacks;
