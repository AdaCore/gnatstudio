-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Odd.Types; use Odd.Types;
package Print_Dialog_Pkg is

   --  this is a temporary dialog that should be replaced in the near future
   --  so please don't put any major effort in it.

   type Print_Dialog_Record is new Gtk_Dialog_Record with record
      Dialog_Vbox1 : Gtk_Vbox;
      Vbox1 : Gtk_Vbox;
      Label1 : Gtk_Label;
      Combo1 : Gtk_Combo;
      Combo_Entry1 : Gtk_Entry;
      Dialog_Action_Area1 : Gtk_Hbox;
      Hbuttonbox1 : Gtk_Hbutton_Box;
      Print_Button : Gtk_Button;
      Cancel_Button : Gtk_Button;
      Help_Button : Gtk_Button;
      Variable : String_Access;
   end record;
   type Print_Dialog_Access is access all Print_Dialog_Record'Class;

   procedure Gtk_New (Print_Dialog : out Print_Dialog_Access);
   procedure Initialize (Print_Dialog : access Print_Dialog_Record'Class);

end Print_Dialog_Pkg;
