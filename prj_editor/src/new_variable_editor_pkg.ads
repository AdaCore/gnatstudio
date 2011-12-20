------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Combo_Box;       use Gtk.Combo_Box;
with Gtk.Dialog;          use Gtk.Dialog;
with Gtk.Hbutton_Box;     use Gtk.Hbutton_Box;
with Gtk.Label;           use Gtk.Label;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Table;           use Gtk.Table;
with Gtk.Tree_View;       use Gtk.Tree_View;
with Gtk.Viewport;        use Gtk.Viewport;

package New_Variable_Editor_Pkg is

   type New_Variable_Editor_Record is new Gtk_Dialog_Record with record
      Dialog_Vbox1        : Gtk_Vbox;
      Dialog_Action_Area1 : Gtk_Hbox;
      Table1              : Gtk_Table;
      Label58             : Gtk_Label;
      Variable_Name       : Gtk_Combo_Box;
      Label60             : Gtk_Label;
      Scrolledwindow2     : Gtk_Scrolled_Window;
      Viewport1           : Gtk_Viewport;
      Vbox54              : Gtk_Vbox;
      --  Values_List : Gtk_Clist;
      Values_List         : Gtk_Tree_View;
      Label61             : Gtk_Label;
      Hbuttonbox4         : Gtk_Hbutton_Box;
      Delete_Variable     : Gtk_Button;
      New_Variable        : Gtk_Button;
      Rename_Variable     : Gtk_Button;
   end record;
   type New_Variable_Editor_Access is
     access all New_Variable_Editor_Record'Class;

   procedure Gtk_New (New_Variable_Editor : out New_Variable_Editor_Access);
   procedure Initialize
     (New_Variable_Editor : access New_Variable_Editor_Record'Class);

end New_Variable_Editor_Pkg;
