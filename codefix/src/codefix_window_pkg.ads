-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
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

with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Vbutton_Box; use Gtk.Vbutton_Box;
with Gtk.Alignment; use Gtk.Alignment;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
package Codefix_Window_Pkg is

   type Codefix_Window_Record is new Gtk_Hbox_Record with record
      Vbox6 : Gtk_Vbox;
      Error_Caption : Gtk_Label;
      Choices_Proposed : Gtk_Notebook;
      Vbox4 : Gtk_Vbox;
      Label3 : Gtk_Label;
      Fix_Caption_List : Gtk_Combo;
      Fix_Entry : Gtk_Entry;
      Vbox5 : Gtk_Vbox;
      Vbuttonbox1 : Gtk_Vbutton_Box;
      Alignment1 : Gtk_Alignment;
      Skip_Correction : Gtk_Button;
      Accept_Correction : Gtk_Button;
      Skip_All_Corrections : Gtk_Button;
      Accept_All_Corrections : Gtk_Button;
      Undo : Gtk_Button;
      Refresh : Gtk_Button;
      Vbuttonbox2 : Gtk_Vbutton_Box;
      Cancel_Changes : Gtk_Button;
      Apply_Changes : Gtk_Button;
   end record;
   type Codefix_Window_Access is access all Codefix_Window_Record'Class;

   procedure Gtk_New (Codefix_Window : out Codefix_Window_Access);
   procedure Initialize (Codefix_Window : access Codefix_Window_Record'Class);

end Codefix_Window_Pkg;
