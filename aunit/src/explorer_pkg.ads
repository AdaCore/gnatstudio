------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               Explorer_Pkg                               --
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
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Label; use Gtk.Label;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package Explorer_Pkg is

   type Explorer_Record is new Gtk_Window_Record with record
      Directory : String_Access;
      Suite_Window : Gtk_Window;

      Vbox5 : Gtk_Vbox;
      Scrolledwindow3 : Gtk_Scrolled_Window;
      Clist : Gtk_Clist;
      Label3 : Gtk_Label;
      Label4 : Gtk_Label;
      Hbuttonbox2 : Gtk_Hbutton_Box;
      Ok : Gtk_Button;
      Close : Gtk_Button;
   end record;
   type Explorer_Access is access all Explorer_Record'Class;

   procedure Gtk_New (Explorer : out Explorer_Access);
   procedure Initialize (Explorer : access Explorer_Record'Class);

   procedure Fill (Explorer : Explorer_Access);
   --  Fill the list with the files in the directory.

end Explorer_Pkg;
