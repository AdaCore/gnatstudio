------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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

with Case_Handling;             use Case_Handling;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GPS.Intl;                  use GPS.Intl;
with Gtk.Combo_Box_Text;        use Gtk.Combo_Box_Text;

with GUI_Utils;                 use GUI_Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

package body Language_Handlers.GUI is

   ---------------------------
   -- Create_Language_Combo --
   ---------------------------

   function Create_Language_Combo
     (Handler : access Language_Handler_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Default : String := "") return Gtk.Combo_Box_Text.Gtk_Combo_Box_Text
   is
      Combo     : Gtk_Combo_Box_Text;
      Languages : Argument_List := Known_Languages (Handler, Sorted => True);
      Project_Lang : constant String := Mixed_Case
        (Get_Language_From_File (Handler, File, From_Project_Only => True));

   begin
      Gtk_New (Combo);

      if Project_Lang = "" then
         Combo.Append_Text (-"(From project) unknown");
      else
         Combo.Append_Text (-"(From project) " & Project_Lang);
      end if;

      for L in Languages'Range loop
         Combo.Append_Text (Languages (L).all);
      end loop;

      Free (Languages);

      if File = GNATCOLL.VFS.No_File and then Default /= "" then
         Set_Active_Text
           (Combo          => Combo,
            Text           => Default,
            Case_Sensitive => False);

      elsif File /= GNATCOLL.VFS.No_File
        and then Language_Is_Overridden (Handler, File)
      then
         Set_Active_Text
           (Combo          => Combo,
            Text           => Get_Language_From_File (Handler, File),
            Case_Sensitive => False);

      elsif Project_Lang = "" then
         Set_Active_Text (Combo, -"(From project) unknown");

      else
         Set_Active_Text
           (Combo          => Combo,
            Text           => -"(From project) " & Project_Lang,
            Case_Sensitive => False);
      end if;

      return Combo;
   end Create_Language_Combo;

end Language_Handlers.GUI;
