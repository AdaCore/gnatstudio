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

with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Box; use Gtk.Box;
with Gtk.Table; use Gtk.Table;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Viewport; use Gtk.Viewport;
with Gtk.Box; use Gtk.Box;
--  with Gtk.Clist; use Gtk.Clist;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
with Gtk.Tree_View; use Gtk.Tree_View;

package New_Variable_Editor_Pkg is

   type New_Variable_Editor_Record is new Gtk_Dialog_Record with record
      Dialog_Vbox1 : Gtk_Vbox;
      Dialog_Action_Area1 : Gtk_Hbox;
      Table1 : Gtk_Table;
      Label58 : Gtk_Label;
      Variable_Name : Gtk_Combo;
      Combo_Entry9 : Gtk_Entry;
      Label60 : Gtk_Label;
      Scrolledwindow2 : Gtk_Scrolled_Window;
      Viewport1 : Gtk_Viewport;
      Vbox54 : Gtk_Vbox;
      --  Values_List : Gtk_Clist;
      Values_List : Gtk_Tree_View;
      Label61 : Gtk_Label;
      Hbuttonbox4 : Gtk_Hbutton_Box;
      Delete_Variable : Gtk_Button;
      New_Variable : Gtk_Button;
      Rename_Variable : Gtk_Button;
   end record;
   type New_Variable_Editor_Access is access all New_Variable_Editor_Record'Class;

   procedure Gtk_New (New_Variable_Editor : out New_Variable_Editor_Access);
   procedure Initialize (New_Variable_Editor : access New_Variable_Editor_Record'Class);

end New_Variable_Editor_Pkg;
