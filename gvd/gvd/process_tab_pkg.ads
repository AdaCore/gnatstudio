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

with Gtk.Window; use Gtk.Window;
with Gtkada.MDI; use Gtkada.MDI;
with GVD.Code_Editors; use GVD.Code_Editors;
with Gtk.Accel_Group;

package Process_Tab_Pkg is

   type Process_Tab_Record is new Gtk_Window_Record with record
      Process_Mdi : MDI_Window;
      Editor_Text : Code_Editor;
   end record;
   type Process_Tab_Access is access all Process_Tab_Record'Class;

   procedure Gtk_New
     (Process_Tab : out Process_Tab_Access;
      Group       : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class);
   procedure Initialize
     (Process_Tab : access Process_Tab_Record'Class;
      Group       : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class);

end Process_Tab_Pkg;
