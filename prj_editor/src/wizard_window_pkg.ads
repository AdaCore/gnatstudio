-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
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
with Gtk.Event_Box; use Gtk.Event_Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Pixmap; use Gtk.Pixmap;
package Wizard_Window_Pkg is

   type Wizard_Window_Record is new Gtk_Window_Record with record
      Vbox1 : Gtk_Vbox;
      Page_Box : Gtk_Hbox;
      Eventbox1 : Gtk_Event_Box;
      Toc_Box : Gtk_Vbox;
      Vbox2 : Gtk_Vbox;
      Title_Box : Gtk_Event_Box;
      Title : Gtk_Label;
      Page_Frame : Gtk_Frame;
      Hseparator1 : Gtk_Hseparator;
      Hbuttonbox1 : Gtk_Hbutton_Box;
      Previous : Gtk_Button;
      Hbox1 : Gtk_Hbox;
      Pixmap1 : Gtk_Pixmap;
      Label2 : Gtk_Label;
      Next : Gtk_Button;
      Hbox2 : Gtk_Hbox;
      Label3 : Gtk_Label;
      Pixmap2 : Gtk_Pixmap;
      Finish : Gtk_Button;
      Hbox5 : Gtk_Hbox;
      Pixmap4 : Gtk_Pixmap;
      Label5 : Gtk_Label;
      Cancel : Gtk_Button;
      Hbox4 : Gtk_Hbox;
      Pixmap3 : Gtk_Pixmap;
      Label4 : Gtk_Label;
   end record;
   type Wizard_Window_Access is access all Wizard_Window_Record'Class;

   procedure Gtk_New (Wizard_Window : out Wizard_Window_Access);
   procedure Initialize (Wizard_Window : access Wizard_Window_Record'Class);

end Wizard_Window_Pkg;
