-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
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

with Gtk.Widget;  use Gtk.Widget;
with Gdk.Event;   use Gdk.Event;

package body Process_Tab_Pkg.Callbacks is

   use Gtk.Arguments;

   ---------------------------------
   -- On_Process_Tab_Delete_Event --
   ---------------------------------

   function On_Process_Tab_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      pragma Unreferenced (Params);
   begin
      Hide (Get_Toplevel (Object));
      return True;
   end On_Process_Tab_Delete_Event;

   ---------------------------------
   -- On_Editor_Text_Delete_Event --
   ---------------------------------

   function On_Editor_Text_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : constant Gdk_Event := To_Event (Params, 1);
      pragma Unreferenced (Object, Arg1);
   begin
      return False;
   end On_Editor_Text_Delete_Event;

end Process_Tab_Pkg.Callbacks;
