-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

pragma Style_Checks (Off);

with Glib; use Glib;
with Gtk; use Gtk;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Glide_Intl; use Glide_Intl;
with Vdiff_Utils;     use Vdiff_Utils;

package body Vdiff_Pkg is

procedure Gtk_New (Vdiff : out Vdiff_Access) is
begin
   Vdiff := new Vdiff_Record;
   Vdiff_Pkg.Initialize (Vdiff);
end Gtk_New;

procedure Initialize (Vdiff : access Vdiff_Record'Class) is
   pragma Suppress (All_Checks);
begin
   --  Gtk.Window.Initialize (Vdiff, Window_Toplevel);
   --  Set_Title (Vdiff, -"Visual Comparison");
   --  Set_Policy (Vdiff, False, True, False);
   --  Set_Position (Vdiff, Win_Pos_None);
   --  Set_Modal (Vdiff, False);

   --  Gtk_New_Hbox (Vdiff.Main_Box, True, 2);
   --  Add (Vdiff, Vdiff.Main_Box);

   Initialize_Hbox (Vdiff, True, 2);

   Gtk_New_Vbox (Vdiff.Vbox1, False, 0);
   --  Pack_Start (Vdiff.Main_Box, Vdiff.Vbox1, True, True, 0);
   Pack_Start (Vdiff, Vdiff.Vbox1, True, True, 0);

   Gtk_New_Hbox (Vdiff.File_Hbox1, False, 0);
   Pack_Start (Vdiff.Vbox1, Vdiff.File_Hbox1, False, False, 0);

   Gtk_New (Vdiff.Label1, -("File1:"));
   Set_Alignment (Vdiff.Label1, 0.5, 0.5);
   Set_Padding (Vdiff.Label1, 0, 0);
   Set_Justify (Vdiff.Label1, Justify_Center);
   Set_Line_Wrap (Vdiff.Label1, False);
   Pack_Start (Vdiff.File_Hbox1, Vdiff.Label1, False, False, 5);

   Gtk_New (Vdiff.Frame_Label1);
   Set_Shadow_Type (Vdiff.Frame_Label1, Shadow_In);
   Pack_Start (Vdiff.File_Hbox1, Vdiff.Frame_Label1, True, True, 0);

   Gtk_New (Vdiff.File_Label1, -("File"));
   Set_Alignment (Vdiff.File_Label1, 7.45058e-09, 0.5);
   Set_Padding (Vdiff.File_Label1, 0, 0);
   Set_Justify (Vdiff.File_Label1, Justify_Center);
   Set_Line_Wrap (Vdiff.File_Label1, False);
   Add (Vdiff.Frame_Label1, Vdiff.File_Label1);

   Gtk_New_Hbox (Vdiff.File1_Box, False, 5);
   Set_Border_Width (Vdiff.File1_Box, 5);
   Pack_Start (Vdiff.Vbox1, Vdiff.File1_Box, True, True, 0);

   Gtk_New (Vdiff.Scrolledwindow1);
   Set_Policy (Vdiff.Scrolledwindow1, Policy_Always, Policy_Always);
   Pack_Start (Vdiff.File1_Box, Vdiff.Scrolledwindow1, True, True, 0);

   Gtk_New (Vdiff.Clist1, 2);
   Set_Selection_Mode (Vdiff.Clist1, Selection_Single);
   Set_Shadow_Type (Vdiff.Clist1, Shadow_In);
   Set_Show_Titles (Vdiff.Clist1, False);
   Set_Column_Width (Vdiff.Clist1, 0, 32);
   Set_Column_Width (Vdiff.Clist1, 1, 80);
   Set_Column_Auto_Resize (Vdiff.Clist1, 1, True);
   Add (Vdiff.Scrolledwindow1, Vdiff.Clist1);

   Widget_Callback.Object_Connect
     (Get_Vadjustment (Vdiff.Scrolledwindow1),
      Name  => "value_changed",
      Marsh => Widget_Callback.To_Marshaller (Value1_Changed'Access),
      Slot_Object => Vdiff);

   Gtk_New (Vdiff.Label8);
   Set_Alignment (Vdiff.Label8, 0.5, 0.5);
   Set_Padding (Vdiff.Label8, 0, 0);
   Set_Justify (Vdiff.Label8, Justify_Center);
   Set_Line_Wrap (Vdiff.Label8, False);
   Set_Column_Widget (Vdiff.Clist1, 0, Vdiff.Label8);

   Gtk_New (Vdiff.Label9);
   Set_Alignment (Vdiff.Label9, 0.5, 0.5);
   Set_Padding (Vdiff.Label9, 0, 0);
   Set_Justify (Vdiff.Label9, Justify_Center);
   Set_Line_Wrap (Vdiff.Label9, False);
   Set_Column_Widget (Vdiff.Clist1, 1, Vdiff.Label9);

   Gtk_New_Vbox (Vdiff.Vbox2, False, 0);
   --  Pack_Start (Vdiff.Main_Box, Vdiff.Vbox2, True, True, 0);
   Pack_Start (Vdiff, Vdiff.Vbox2, True, True, 0);

   Gtk_New_Hbox (Vdiff.File_Hbox2, False, 0);
   Pack_Start (Vdiff.Vbox2, Vdiff.File_Hbox2, False, False, 0);

   Gtk_New (Vdiff.Label2, -("File2:"));
   Set_Alignment (Vdiff.Label2, 0.5, 0.5);
   Set_Padding (Vdiff.Label2, 0, 0);
   Set_Justify (Vdiff.Label2, Justify_Center);
   Set_Line_Wrap (Vdiff.Label2, False);
   Pack_Start (Vdiff.File_Hbox2, Vdiff.Label2, False, False, 5);

   Gtk_New (Vdiff.Frame_Label2);
   Set_Shadow_Type (Vdiff.Frame_Label2, Shadow_In);
   Pack_Start (Vdiff.File_Hbox2, Vdiff.Frame_Label2, True, True, 0);

   Gtk_New (Vdiff.File_Label2, -("File"));
   Set_Alignment (Vdiff.File_Label2, 7.45058e-09, 0.5);
   Set_Padding (Vdiff.File_Label2, 0, 0);
   Set_Justify (Vdiff.File_Label2, Justify_Center);
   Set_Line_Wrap (Vdiff.File_Label2, False);
   Add (Vdiff.Frame_Label2, Vdiff.File_Label2);

   Gtk_New_Hbox (Vdiff.File2_Box, False, 5);
   Set_Border_Width (Vdiff.File2_Box, 5);
   Pack_Start (Vdiff.Vbox2, Vdiff.File2_Box, True, True, 0);

   Gtk_New (Vdiff.Scrolledwindow2);
   Set_Policy (Vdiff.Scrolledwindow2, Policy_Always, Policy_Always);
   Pack_Start (Vdiff.File2_Box, Vdiff.Scrolledwindow2, True, True, 0);

   Gtk_New (Vdiff.Clist2, 2);
   Set_Selection_Mode (Vdiff.Clist2, Selection_Single);
   Set_Shadow_Type (Vdiff.Clist2, Shadow_In);
   Set_Show_Titles (Vdiff.Clist2, False);
   Set_Column_Width (Vdiff.Clist2, 0, 36);
   Set_Column_Width (Vdiff.Clist2, 1, 80);
   Set_Column_Auto_Resize (Vdiff.Clist2, 1, True);
   Add (Vdiff.Scrolledwindow2, Vdiff.Clist2);

   Widget_Callback.Object_Connect
     (Get_Vadjustment (Vdiff.Scrolledwindow2),
      Name  => "value_changed",
      Marsh => Widget_Callback.To_Marshaller (Value2_Changed'Access),
      Slot_Object => Vdiff);

   Gtk_New (Vdiff.Label10);
   Set_Alignment (Vdiff.Label10, 0.5, 0.5);
   Set_Padding (Vdiff.Label10, 0, 0);
   Set_Justify (Vdiff.Label10, Justify_Center);
   Set_Line_Wrap (Vdiff.Label10, False);
   Set_Column_Widget (Vdiff.Clist2, 0, Vdiff.Label10);

   Gtk_New (Vdiff.Label11);
   Set_Alignment (Vdiff.Label11, 0.5, 0.5);
   Set_Padding (Vdiff.Label11, 0, 0);
   Set_Justify (Vdiff.Label11, Justify_Center);
   Set_Line_Wrap (Vdiff.Label11, False);
   Set_Column_Widget (Vdiff.Clist2, 1, Vdiff.Label11);

end Initialize;

end Vdiff_Pkg;
