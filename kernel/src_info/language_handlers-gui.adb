-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2007-2008, AdaCore           --
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
with Gtk.Combo;                 use Gtk.Combo;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.List;                  use Gtk.List;
with Gtk.List_Item;             use Gtk.List_Item;
with GNATCOLL.VFS;                       use GNATCOLL.VFS;

package body Language_Handlers.GUI is

   ---------------------------
   -- Create_Language_Combo --
   ---------------------------

   function Create_Language_Combo
     (Handler : access Language_Handler_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Default : String := "") return Gtk_Combo
   is
      Combo     : Gtk_Combo;
      Languages : Argument_List := Known_Languages (Handler, Sorted => True);
      Project_Lang : String :=
        Get_Language_From_File (Handler, File, From_Project_Only => True);
      Item : Gtk_List_Item;
   begin
      Gtk_New (Combo);
      Set_Editable (Get_Entry (Combo), False);

      if Project_Lang = "" then
         Gtk_New (Item, -"(From project) unknown");
      else
         Mixed_Case (Project_Lang);
         Gtk_New (Item, -"(From project) " & Project_Lang);
      end if;

      Add (Get_List (Combo), Item);
      Show_All (Item);

      for L in Languages'Range loop
         Gtk_New (Item, Languages (L).all);
         Add (Get_List (Combo), Item);
         Show_All (Item);
      end loop;

      Free (Languages);

      if File = GNATCOLL.VFS.No_File and then Default /= "" then
         Set_Text (Get_Entry (Combo), Default);
      elsif File /= GNATCOLL.VFS.No_File
        and then Language_Is_Overriden (Handler, File)
      then
         Set_Text (Get_Entry (Combo), Get_Language_From_File (Handler, File));
      elsif Project_Lang = "" then
         Set_Text (Get_Entry (Combo), -"(From project) unknown");
      else
         Set_Text (Get_Entry (Combo), -"(From project) " & Project_Lang);
      end if;

      return Combo;
   end Create_Language_Combo;

end Language_Handlers.GUI;
