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

with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Box; use Gtk.Box;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
with Gtk.Label; use Gtk.Label;

with Codefix.Graphics; use Codefix.Graphics;

package Final_Window_Pkg is

   type Final_Window_Record is new Gtk_Dialog_Record with record
      Dialog_Vbox1 : Gtk_Vbox;
      Dialog_Action_Area1 : Gtk_Hbox;
      Hbuttonbox1 : Gtk_Hbutton_Box;
      Final_Validation : Gtk_Button;
      Final_Cancel : Gtk_Button;
      Label4 : Gtk_Label;
      Graphic_Codefix : Graphic_Codefix_Access;
   end record;
   type Final_Window_Access is access all Final_Window_Record'Class;

   procedure Gtk_New (Final_Window : out Final_Window_Access);
   procedure Initialize (Final_Window : access Final_Window_Record'Class);

end Final_Window_Pkg;
