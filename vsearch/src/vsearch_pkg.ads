-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                            AdaCore                                --
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

with Gtk.Box; use Gtk.Box;
with Gtk.Table; use Gtk.Table;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Check_Button; use Gtk.Check_Button;
with GPS.Kernel;

package Vsearch_Pkg is

   type Vsearch_Record is new Gtk_Box_Record with record
      Table : Gtk_Table;
      Replace_Label : Gtk_Label;
      Search_For_Label : Gtk_Label;
      Search_In_Label : Gtk_Label;
      Replace_Combo : Gtk_Combo;
      Replace_Entry : Gtk_Entry;
      Context_Combo : Gtk_Combo;
      Context_Entry : Gtk_Entry;
      Pattern_Combo : Gtk_Combo;
      Pattern_Entry : Gtk_Entry;
      Buttons_Hbox : Gtk_Hbox;
      Options_Frame : Gtk_Box;
      Options_Vbox : Gtk_Table;
      Select_Editor_Check : Gtk_Check_Button;
      Search_All_Check : Gtk_Check_Button;
      Case_Check : Gtk_Check_Button;
      Whole_Word_Check : Gtk_Check_Button;
      Auto_Hide_Check : Gtk_Check_Button;
      Regexp_Check : Gtk_Check_Button;
      Context_Specific : Gtk_Box;
   end record;
   type Vsearch_Access is access all Vsearch_Record'Class;

   procedure Gtk_New
     (Vsearch : out Vsearch_Access;
      Handle  : access GPS.Kernel.Kernel_Handle_Record'Class);
   procedure Initialize
     (Vsearch : access Vsearch_Record'Class;
      Handle  : access GPS.Kernel.Kernel_Handle_Record'Class);

end Vsearch_Pkg;
