-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
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
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Label; use Gtk.Label;
with Gtk.Box; use Gtk.Box;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Text; use Gtk.Text;
with Gtk.Handlers;
with Gtkada.Canvas; use Gtkada.Canvas;
with GVD.Code_Editors; use GVD.Code_Editors;
with Dock_Paned; use Dock_Paned;

package Process_Tab_Pkg is

   type Process_Tab_Record is new Gtk_Window_Record with record
      Delete_Text_Handler_Id : Gtk.Handlers.Handler_Id;
      Stack_List_Select_Id : Gtk.Handlers.Handler_Id;
      Process_Hbox : Gtk_Hbox;
      Process_Paned : Dock_Vpaned;
      Data_Editor_Paned : Dock_Vpaned;
      Data_Paned : Dock_Hpaned;
      Stack_Scrolledwindow : Gtk_Scrolled_Window;
      Stack_List : Gtk_Clist;
      Label101 : Gtk_Label;
      Label201 : Gtk_Label;
      Label202 : Gtk_Label;
      Label203 : Gtk_Label;
      Label204 : Gtk_Label;
      Data_Scrolledwindow : Gtk_Scrolled_Window;
      Data_Canvas : Interactive_Canvas;
      Editor_Vbox : Gtk_Vbox;
      Label_Hbox : Gtk_Hbox;
      Explorer_Separator : Gtk_Hseparator;
      Editor_Label : Gtk_Label;
      Editor_Separator : Gtk_Hseparator;
      Editor_Text : Code_Editor;
      Command_Scrolledwindow : Gtk_Scrolled_Window;
      Debugger_Text : Gtk_Text;
   end record;
   type Process_Tab_Access is access all Process_Tab_Record'Class;

   procedure Gtk_New (Process_Tab : out Process_Tab_Access);
   procedure Initialize (Process_Tab : access Process_Tab_Record'Class);

end Process_Tab_Pkg;
