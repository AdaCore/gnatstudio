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
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text; use Gtk.Text;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
package Advanced_Breakpoint_Pkg is

   type Advanced_Breakpoint_Record is new Gtk_Window_Record with record
      Vbox11 : Gtk_Vbox;
      Condition_Frame : Gtk_Frame;
      Vbox5 : Gtk_Vbox;
      Label7 : Gtk_Label;
      Condition_Combo : Gtk_Combo;
      Combo_Entry2 : Gtk_Entry;
      Ignore_Count_Frame : Gtk_Frame;
      Vbox6 : Gtk_Vbox;
      Label8 : Gtk_Label;
      Ignore_Count_Combo : Gtk_Spin_Button;
      Command_Frame : Gtk_Frame;
      Vbox12 : Gtk_Vbox;
      Label13 : Gtk_Label;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Command_Descr : Gtk_Text;
      Hbuttonbox3 : Gtk_Hbutton_Box;
      Record_Button : Gtk_Button;
      End_Button : Gtk_Button;
      Hbuttonbox5 : Gtk_Hbutton_Box;
      Ok_Button : Gtk_Button;
      Cancel_Button : Gtk_Button;
   end record;
   type Advanced_Breakpoint_Access is access all Advanced_Breakpoint_Record'Class;

   type Advanced_Breakpoint_Descriptor is record
      null;
   end record;

   procedure Advanced_Breakpoint_Editor
     (Descriptor : out Advanced_Breakpoint_Descriptor);
   --  Open an advanced breakpoint properties editor and launch a main loop
   --  until the ok or cancel button has been pressed.
   --  Return the advanced properties descriptor.
   --  Note that this is your responsibility to free the memory associated with
   --  Descriptor, using Free below.

   procedure Free (Descriptor : in out Advanced_Breakpoint_Descriptor);
   --  Free the dynamic memory associated with Descriptor.

private
   procedure Gtk_New (Advanced_Breakpoint : out Advanced_Breakpoint_Access);
   procedure Initialize (Advanced_Breakpoint : access Advanced_Breakpoint_Record'Class);

end Advanced_Breakpoint_Pkg;
