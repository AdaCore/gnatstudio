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

with Gtk.Window;          use Gtk.Window;
with Gtk.Box;             use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Viewport;        use Gtk.Viewport;
with Gtk.List;            use Gtk.List;
with Gtk.Label;           use Gtk.Label;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Odd.Types; use Odd.Types;

package Open_Session_Pkg is

   type Open_Session_Record is new Gtk_Window_Record with record
      Vbox17           : Gtk_Vbox;
      Scrolledwindow10 : Gtk_Scrolled_Window;
      Viewport1        : Gtk_Viewport;
      List             : Gtk_List;
      Hbox6            : Gtk_Hbox;
      Label73          : Gtk_Label;
      Entry1           : Gtk_Entry;
      Hbuttonbox9      : Gtk_Hbutton_Box;
      Ok_Button        : Gtk_Button;
      Cancel_Button    : Gtk_Button;
      Help_Button      : Gtk_Button;
   end record;
   type Open_Session_Access is access all Open_Session_Record'Class;

   procedure Gtk_New (Open_Session : out Open_Session_Access);
   procedure Initialize (Open_Session : access Open_Session_Record'Class);

   procedure Open_Session (Open : in out Open_Session_Access;
                           Dir  : in String;
                           File : out String_Access);

end Open_Session_Pkg;
