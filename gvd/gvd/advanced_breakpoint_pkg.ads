-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2003                      --
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
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text; use Gtk.Text;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Dialog; use Gtk.Dialog;
package Advanced_Breakpoint_Pkg is

   type Advanced_Breakpoint_Record is new Gtk_Window_Record with record
      Vbox34 : Gtk_Vbox;
      Main_Notebook : Gtk_Notebook;
      Condition_Box : Gtk_Vbox;
      Condition_Frame : Gtk_Frame;
      Vbox32 : Gtk_Vbox;
      Label104 : Gtk_Label;
      Condition_Combo : Gtk_Combo;
      Combo_Entry2 : Gtk_Entry;
      Ignore_Count_Frame : Gtk_Frame;
      Vbox33 : Gtk_Vbox;
      Label105 : Gtk_Label;
      Ignore_Count_Combo : Gtk_Spin_Button;
      Command_Frame : Gtk_Frame;
      Vbox35 : Gtk_Vbox;
      Label106 : Gtk_Label;
      Scrolledwindow12 : Gtk_Scrolled_Window;
      Command_Descr : Gtk_Text;
      Hbuttonbox12 : Gtk_Hbutton_Box;
      Record_Button : Gtk_Button;
      End_Button : Gtk_Button;
      Label102 : Gtk_Label;
      Scope_Box : Gtk_Vbox;
      Frame13 : Gtk_Frame;
      Vbox30 : Gtk_Vbox;
      Scope_Task : Gtk_Radio_Button;
      Scope_Pd : Gtk_Radio_Button;
      Scope_Any : Gtk_Radio_Button;
      Frame14 : Gtk_Frame;
      Vbox31 : Gtk_Vbox;
      Action_Task : Gtk_Radio_Button;
      Action_Pd : Gtk_Radio_Button;
      Action_All : Gtk_Radio_Button;
      Set_Default : Gtk_Check_Button;
      Scope : Gtk_Label;
      Hbuttonbox13 : Gtk_Hbutton_Box;
      Apply : Gtk_Button;
      Close : Gtk_Button;
      Response_Action : Gtk_Response_Type;
   end record;
   type Advanced_Breakpoint_Access is access all Advanced_Breakpoint_Record'Class;

   procedure Gtk_New (Advanced_Breakpoint : out Advanced_Breakpoint_Access);
   procedure Initialize (Advanced_Breakpoint : access Advanced_Breakpoint_Record'Class);

end Advanced_Breakpoint_Pkg;
