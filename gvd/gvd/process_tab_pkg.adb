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

with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with Process_Tab_Pkg.Callbacks; use Process_Tab_Pkg.Callbacks;

package body Process_Tab_Pkg is

pragma Suppress (All_Checks);
--  Checks are expensive (in code size) in this unit, and not needed,
--  since the following code is generated automatically.

procedure Gtk_New (Process_Tab : out Process_Tab_Access) is
begin
   Process_Tab := new Process_Tab_Record;
   Process_Tab_Pkg.Initialize (Process_Tab);
end Gtk_New;

procedure Initialize (Process_Tab : access Process_Tab_Record'Class) is
begin
   --  Gtk.Window.Initialize (Process_Tab, Window_Toplevel);
   --  Set_Title (Process_Tab, -"window1");
   --  Set_Policy (Process_Tab, False, True, False);
   --  Set_Position (Process_Tab, Win_Pos_None);
   --  Set_Modal (Process_Tab, False);

   Gtk_New_Vpaned (Process_Tab.Process_Paned);
   --  Add (Process_Tab, Process_Tab.Process_Paned);
   Set_Handle_Size (Process_Tab.Process_Paned, 10);
   Set_Gutter_Size (Process_Tab.Process_Paned, 6);

   Gtk_New_Vpaned (Process_Tab.Vpaned6);
   Add (Process_Tab.Process_Paned, Process_Tab.Vpaned6);
   Set_Handle_Size (Process_Tab.Vpaned6, 10);
   Set_Gutter_Size (Process_Tab.Vpaned6, 6);

   Gtk_New (Process_Tab.Scrolledwindow9);
   Add (Process_Tab.Vpaned6, Process_Tab.Scrolledwindow9);
   Set_Policy (Process_Tab.Scrolledwindow9, Policy_Automatic, Policy_Automatic);

   Gtk_New (Process_Tab.Data_Canvas);
   Add (Process_Tab.Scrolledwindow9, Process_Tab.Data_Canvas);
   Set_Shadow_Type (Process_Tab.Data_Canvas, Shadow_In);

   Gtk_New (Process_Tab.Thread_Notebook);
   Add (Process_Tab.Vpaned6, Process_Tab.Thread_Notebook);
   Set_Scrollable (Process_Tab.Thread_Notebook, True);
   Set_Show_Border (Process_Tab.Thread_Notebook, True);
   Set_Show_Tabs (Process_Tab.Thread_Notebook, True);
   Set_Tab_Hborder (Process_Tab.Thread_Notebook, 2);
   Set_Tab_Vborder (Process_Tab.Thread_Notebook, 2);
   Set_Tab_Pos (Process_Tab.Thread_Notebook, Pos_Top);

   Gtk_New (Process_Tab.Frame10);
   Add (Process_Tab.Thread_Notebook, Process_Tab.Frame10);
   Set_Shadow_Type (Process_Tab.Frame10, Shadow_Etched_In);

   Gtk_New_Hbox (Process_Tab.Editor_Text, False, 0);
   Add (Process_Tab.Frame10, Process_Tab.Editor_Text);

   Gtk_New (Process_Tab.Label52, -("Main Thread"));
   Set_Alignment (Process_Tab.Label52, 0.5, 0.5);
   Set_Padding (Process_Tab.Label52, 0, 0);
   Set_Justify (Process_Tab.Label52, Justify_Center);
   Set_Line_Wrap (Process_Tab.Label52, False);
   Set_Tab (Process_Tab.Thread_Notebook, 0, Process_Tab.Label52);

   Gtk_New (Process_Tab.Scrolledwindow7);
   Add (Process_Tab.Process_Paned, Process_Tab.Scrolledwindow7);
   Set_Policy (Process_Tab.Scrolledwindow7, Policy_Never, Policy_Always);

   Gtk_New (Process_Tab.Debugger_Text);
   Widget_Callback.Object_Connect
     (Process_Tab.Debugger_Text, "insert_text", On_Debugger_Text_Insert_Text'Access, Process_Tab);
   Widget_Callback.Object_Connect
     (Process_Tab.Debugger_Text, "delete_text", On_Debugger_Text_Delete_Text'Access, Process_Tab);
   Widget_Callback.Object_Connect
     (Process_Tab.Debugger_Text, "insert_text", On_Debugger_Text_Insert_Text2'Access, Process_Tab, True);
   Return_Callback.Object_Connect
     (Process_Tab.Debugger_Text, "key_press_event", On_Debugger_Text_Key_Press_Event'Access, Process_Tab);
   Add (Process_Tab.Scrolledwindow7, Process_Tab.Debugger_Text);
   Set_Editable (Process_Tab.Debugger_Text, True);

end Initialize;

end Process_Tab_Pkg;
