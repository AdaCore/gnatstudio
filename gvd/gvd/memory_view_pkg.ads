-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
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
with Gtk.Label; use Gtk.Label;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Vbutton_Box; use Gtk.Vbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text; use Gtk.Text;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Arrow; use Gtk.Arrow;
package Memory_View_Pkg is

   type Memory_View_Record is new Gtk_Window_Record with record
      Vbox20 : Gtk_Vbox;
      Frame : Gtk_Frame;
      Hbox8 : Gtk_Hbox;
      Hbox11 : Gtk_Hbox;
      Vbox23 : Gtk_Vbox;
      Label95 : Gtk_Label;
      Label96 : Gtk_Label;
      Vbox24 : Gtk_Vbox;
      Address_Entry : Gtk_Entry;
      Search_Entry : Gtk_Entry;
      Vbuttonbox6 : Gtk_Vbutton_Box;
      Address_View : Gtk_Button;
      Search_Button : Gtk_Button;
      Hbox12 : Gtk_Hbox;
      Vbuttonbox5 : Gtk_Vbutton_Box;
      Label98 : Gtk_Label;
      Size : Gtk_Combo;
      Size_Entry : Gtk_Entry;
      Vseparator7 : Gtk_Vseparator;
      Label97 : Gtk_Label;
      Format : Gtk_Combo;
      Data_Entry : Gtk_Entry;
      Scrolledwindow : Gtk_Scrolled_Window;
      View : Gtk_Text;
      Hbox13 : Gtk_Hbox;
      Label99 : Gtk_Label;
      Value : Gtk_Spin_Button;
      Label100 : Gtk_Label;
      Hbuttonbox12 : Gtk_Hbutton_Box;
      Page_Size_Button : Gtk_Button;
      Vseparator8 : Gtk_Vseparator;
      Pgup : Gtk_Button;
      Arrow1 : Gtk_Arrow;
      Pgdn : Gtk_Button;
      Arrow2 : Gtk_Arrow;
      Hseparator2 : Gtk_Hseparator;
      Hbuttonbox11 : Gtk_Hbutton_Box;
      Reset : Gtk_Button;
      Submit : Gtk_Button;
      Cancel : Gtk_Button;
      Help : Gtk_Button;
   end record;
   type Memory_View_Access is access all Memory_View_Record'Class;

   procedure Gtk_New (Memory_View : out Memory_View_Access);
   procedure Initialize (Memory_View : access Memory_View_Record'Class);

end Memory_View_Pkg;
