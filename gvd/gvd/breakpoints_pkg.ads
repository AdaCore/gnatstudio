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

with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Button; use Gtk.Button;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Vbutton_Box; use Gtk.Vbutton_Box;
with Gtk.Object; use Gtk.Object;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
package Breakpoints_Pkg is

   type Breakpoints_Record is new Gtk_Window_Record with record
      Vbox1 : Gtk_Vbox;
      Notebook1 : Gtk_Notebook;
      Vbox2 : Gtk_Vbox;
      Label4 : Gtk_Label;
      Combo1 : Gtk_Combo;
      Combo_Entry1 : Gtk_Entry;
      Temporary_Location : Gtk_Check_Button;
      Location : Gtk_Label;
      Vbox7 : Gtk_Vbox;
      Label9 : Gtk_Label;
      Entry1 : Gtk_Entry;
      Label10 : Gtk_Label;
      Combo3 : Gtk_Combo;
      Combo_Entry3 : Gtk_Entry;
      Watchpoint : Gtk_Label;
      Vbox8 : Gtk_Vbox;
      Label11 : Gtk_Label;
      Combo4 : Gtk_Combo;
      Combo_Entry4 : Gtk_Entry;
      Frame4 : Gtk_Frame;
      Vbox9 : Gtk_Vbox;
      Stop_Always : Gtk_Radio_Button;
      Stop_Not_Handled : Gtk_Radio_Button;
      Except : Gtk_Label;
      Vbox10 : Gtk_Vbox;
      Label12 : Gtk_Label;
      Combo5 : Gtk_Combo;
      Combo_Entry5 : Gtk_Entry;
      Temporary_Regexp : Gtk_Check_Button;
      Regexp : Gtk_Label;
      Advanced_Button : Gtk_Button;
      Hbox1 : Gtk_Hbox;
      Scrolledwindow2 : Gtk_Scrolled_Window;
      Clist1 : Gtk_Clist;
      Label15 : Gtk_Label;
      Label16 : Gtk_Label;
      Vbuttonbox1 : Gtk_Vbutton_Box;
      Add_Button : Gtk_Button;
      Remove_Button : Gtk_Button;
      Remove_All_Button : Gtk_Button;
      Hseparator1 : Gtk_Hseparator;
      Hbuttonbox4 : Gtk_Hbutton_Box;
      Ok_Button : Gtk_Button;
      Cancel_Button : Gtk_Button;
   end record;
   type Breakpoints_Access is access all Breakpoints_Record'Class;

   type Breakpoint_Descriptor is record
      null;
   end record;

   procedure Breakpoint_Editor (Descriptor : out Breakpoint_Descriptor);
   --  Open a breakpoint editor and launch a main loop until the ok or cancel
   --  button has been pressed.
   --  Return the breakpoint descriptor.
   --  Note that this is your responsibility to free the memory associated with
   --  Descriptor, using Free below.

   procedure Free (Descriptor : in out Breakpoint_Descriptor);
   --  Free the dynamic memory associated with Descriptor.

private

   procedure Gtk_New (Breakpoints : out Breakpoints_Access);
   procedure Initialize (Breakpoints : access Breakpoints_Record'Class);

end Breakpoints_Pkg;
