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
with Gtk.Clist; use Gtk.Clist;
with Gtk.Button; use Gtk.Button;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with GtkAda.File_Selector; use GtkAda.File_Selector;

package Make_Suite_Window_Pkg is

   type Make_Suite_Window_Record is new Gtk_Window_Record with record
      Explorer   : File_Selector_Window_Access;
      Name       : String_Access;
      Name_Entry : Gtk_Entry;
      Test_List  : Gtk_Clist;
      Add        : Gtk_Button;
      Remove     : Gtk_Button;
      Ok         : Gtk_Button;
      Cancel     : Gtk_Button;
   end record;
   type Make_Suite_Window_Access is access all Make_Suite_Window_Record'Class;

   procedure Gtk_New (Make_Suite_Window : out Make_Suite_Window_Access);
   procedure Initialize
     (Make_Suite_Window : access Make_Suite_Window_Record'Class);

end Make_Suite_Window_Pkg;
