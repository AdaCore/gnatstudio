-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
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

with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;

with Process_Tab_Pkg.Callbacks; use Process_Tab_Pkg.Callbacks;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Gtk.Object;      use Gtk.Object;
with Odd_Intl;        use Odd_Intl;

package body Process_Tab_Pkg is

   Signals : constant chars_ptr_array :=
     (1 => New_String ("executable_changed"));
   Class_Record : GObject_Class := Uninitialized_Class;

procedure Gtk_New (Process_Tab : out Process_Tab_Access) is
begin
   Process_Tab := new Process_Tab_Record;
   Process_Tab_Pkg.Initialize (Process_Tab);
end Gtk_New;

procedure Initialize (Process_Tab : access Process_Tab_Record'Class) is
   pragma Suppress (All_Checks);

begin
   Gtk.Window.Initialize (Process_Tab, Window_Toplevel);
   Initialize_Class_Record
     (Process_Tab, Signals, Class_Record, Type_Name => "GvdProcessTab");
   Set_Title (Process_Tab, -"Data");
   Set_Policy (Process_Tab, False, True, False);
   Set_Position (Process_Tab, Win_Pos_None);
   Set_Modal (Process_Tab, False);
   Set_Default_Size (Process_Tab, 500, 300);
   Return_Callback.Connect
     (Process_Tab, "delete_event", On_Process_Tab_Delete_Event'Access);

   Gtk_New (Process_Tab.Process_Mdi);
   Set_Priorities
     (Process_Tab.Process_Mdi,
      (Left => 4, Right => 3, Top => 1, Bottom => 2));
   --  Add (Process_Tab, Process_Tab.Process_Mdi);

   Gtk_New_Hbox (Process_Tab.Editor_Text, Process_Tab);
   Return_Callback.Object_Connect
     (Process_Tab.Editor_Text, "delete_event", On_Editor_Text_Delete_Event'Access, Process_Tab);

end Initialize;

end Process_Tab_Pkg;
