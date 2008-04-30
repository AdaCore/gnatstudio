-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2006                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with GNATCOLL.VFS;

package Vdiff_Pkg is

   --  type Vdiff_Record is new Gtk_Window_Record with record
   type Vdiff_Record is new Gtk_Box_Record with record
      Ignore_Value_Changed : Boolean := False;
      --  Main_Box : Gtk_Hbox;
      Vbox1 : Gtk_Vbox;
      File_Hbox1 : Gtk_Hbox;
      Label1 : Gtk_Label;
      Frame_Label1 : Gtk_Frame;
      File_Label1 : Gtk_Label;
      File1_Box : Gtk_Hbox;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Clist1 : Gtk_Clist;
      Label8 : Gtk_Label;
      Label9 : Gtk_Label;
      Vbox2 : Gtk_Vbox;
      File_Hbox2 : Gtk_Hbox;
      Label2 : Gtk_Label;
      Frame_Label2 : Gtk_Frame;
      File_Label2 : Gtk_Label;
      File2_Box : Gtk_Hbox;
      Scrolledwindow2 : Gtk_Scrolled_Window;
      Clist2 : Gtk_Clist;
      Label10 : Gtk_Label;
      Label11 : Gtk_Label;
      File1, File2 : GNATCOLL.VFS.Virtual_File;
   end record;
   type Vdiff_Access is access all Vdiff_Record'Class;

   procedure Gtk_New (Vdiff : out Vdiff_Access);
   procedure Initialize (Vdiff : access Vdiff_Record'Class);

end Vdiff_Pkg;
