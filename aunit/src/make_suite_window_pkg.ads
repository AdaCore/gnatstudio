------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           Make_Suite_Window_Pkg                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--                Copyright (C) 2001 Ada Core Technologies, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Vbutton_Box; use Gtk.Vbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with GtkAda.File_Selector; use GtkAda.File_Selector;

package Make_Suite_Window_Pkg is


   type Make_Suite_Window_Record is new Gtk_Window_Record with record
      Explorer : File_Selector_Window_Access;
      Name     : String_Access;

      Vbox1 : Gtk_Vbox;
      Hbox1 : Gtk_Hbox;
      Vbox2 : Gtk_Vbox;
      Label1 : Gtk_Label;
      Vbox3 : Gtk_Vbox;
      Name_Entry : Gtk_Entry;
      Vbox4 : Gtk_Vbox;
      Label2 : Gtk_Label;
      Hbox2 : Gtk_Hbox;
      Vbox11 : Gtk_Vbox;
      Hbox5 : Gtk_Hbox;
      Vbox12 : Gtk_Vbox;
      Label7 : Gtk_Label;
      Vbox13 : Gtk_Vbox;
      Entry1 : Gtk_Entry;
      Hbox8 : Gtk_Hbox;
      Vbox14 : Gtk_Vbox;
      Label8 : Gtk_Label;
      Hbox6 : Gtk_Hbox;
      Test_List : Gtk_Clist;
      Label5 : Gtk_Label;
      Label6 : Gtk_Label;
      Vbuttonbox1 : Gtk_Vbutton_Box;
      Add : Gtk_Button;
      Remove : Gtk_Button;
      Hbuttonbox1 : Gtk_Hbutton_Box;
      Ok : Gtk_Button;
      Cancel : Gtk_Button;
      Help : Gtk_Button;
      Scrolledwindow2 : Gtk_Scrolled_Window;
   end record;
   type Make_Suite_Window_Access is access all Make_Suite_Window_Record'Class;

   procedure Gtk_New (Make_Suite_Window : out Make_Suite_Window_Access);
   procedure Initialize
     (Make_Suite_Window : access Make_Suite_Window_Record'Class);

end Make_Suite_Window_Pkg;
