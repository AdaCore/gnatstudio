-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Table; use Gtk.Table;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Button; use Gtk.Button;
with Gtk.Label; use Gtk.Label;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
package Open_Program_Pkg is

   type Open_Program_Record is new Gtk_Window_Record with record
      Vbox13 : Gtk_Vbox;
      Frame8 : Gtk_Frame;
      Table7 : Gtk_Table;
      Gdb_Button : Gtk_Radio_Button;
      Dbx_Button : Gtk_Radio_Button;
      Xdb_Button : Gtk_Radio_Button;
      Jdb_Button : Gtk_Radio_Button;
      Pydb_Button : Gtk_Radio_Button;
      Perl_Button : Gtk_Radio_Button;
      Program_Combo : Gtk_Combo;
      Program_Entry : Gtk_Entry;
      Open_Button : Gtk_Button;
      Host_Combo : Gtk_Combo;
      Host_Entry : Gtk_Entry;
      Label57 : Gtk_Label;
      Label55 : Gtk_Label;
      Label56 : Gtk_Label;
      Protocol_Combo : Gtk_Combo;
      Protocol_Entry : Gtk_Entry;
      Label60 : Gtk_Label;
      Program_Host_Combo : Gtk_Combo;
      Target_Entry : Gtk_Entry;
      Label59 : Gtk_Label;
      Label73 : Gtk_Label;
      Debugger_Combo : Gtk_Combo;
      Debugger_Entry : Gtk_Entry;
      Replace_Check : Gtk_Check_Button;
      Hbuttonbox7 : Gtk_Hbutton_Box;
      Ok_Button : Gtk_Button;
      Cancel_Button : Gtk_Button;
   end record;
   type Open_Program_Access is access all Open_Program_Record'Class;

   procedure Gtk_New (Open_Program : out Open_Program_Access);
   procedure Initialize (Open_Program : access Open_Program_Record'Class);

end Open_Program_Pkg;
