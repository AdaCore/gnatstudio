-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Window; use Gtk.Window;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Button; use Gtk.Button;
with Gtk.Status_Bar; use Gtk.Status_Bar;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GtkAda.File_Selector; use GtkAda.File_Selector;

package Make_Harness_Window_Pkg is

   type Make_Harness_Window_Record is new Gtk_Window_Record with record
      Suite_Name      : String_Access;
      Procedure_Name  : String_Access;
      Explorer        : File_Selector_Window_Access;
      Procedure_Entry : Gtk_Entry;
      File_Name_Entry : Gtk_Entry;
      Browse          : Gtk_Button;
      Ok              : Gtk_Button;
      Cancel          : Gtk_Button;
      Help            : Gtk_Button;
      Statusbar       : Gtk_Statusbar;
   end record;
   type Make_Harness_Window_Access is
     access all Make_Harness_Window_Record'Class;

   procedure Gtk_New (Make_Harness_Window : out Make_Harness_Window_Access);
   procedure Initialize
     (Make_Harness_Window : access Make_Harness_Window_Record'Class);

end Make_Harness_Window_Pkg;
