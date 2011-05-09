-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2007-2011, AdaCore                 --
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

with Case_Handling;             use Case_Handling;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GPS.Intl;                  use GPS.Intl;
with Gtk.Combo_Box;             use Gtk.Combo_Box;

with GUI_Utils;                 use GUI_Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

package body Language_Handlers.GUI is

   ---------------------------
   -- Create_Language_Combo --
   ---------------------------

   function Create_Language_Combo
     (Handler : access Language_Handler_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Default : String := "") return Gtk_Combo_Box
   is
      Combo     : Gtk_Combo_Box;
      Languages : Argument_List := Known_Languages (Handler, Sorted => True);
      Project_Lang : String :=
        Get_Language_From_File (Handler, File, From_Project_Only => True);

   begin
      Gtk_New_Text (Combo);

      if Project_Lang = "" then
         Combo.Append_Text (-"(From project) unknown");
      else
         Mixed_Case (Project_Lang);
         Combo.Append_Text (-"(From project) " & Project_Lang);
      end if;

      for L in Languages'Range loop
         Combo.Append_Text (Languages (L).all);
      end loop;

      Free (Languages);

      if File = GNATCOLL.VFS.No_File and then Default /= "" then
         Set_Active_Text (Combo, Default);
      elsif File /= GNATCOLL.VFS.No_File
        and then Language_Is_Overriden (Handler, File)
      then
         Set_Active_Text (Combo, Get_Language_From_File (Handler, File));
      elsif Project_Lang = "" then
         Set_Active_Text (Combo, -"(From project) unknown");
      else
         Set_Active_Text (Combo, -"(From project) " & Project_Lang);
      end if;

      return Combo;
   end Create_Language_Combo;

end Language_Handlers.GUI;
