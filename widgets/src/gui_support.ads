-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Glib.Object;
with Gtk.Handlers;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Tree_Store;

package Gui_Support is

   package Tree_Model_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Glib.Gint);

   procedure Set_Radio_And_Callback
     (Model    : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Renderer : access Gtk_Cell_Renderer_Toggle_Record'Class;
      Column   : Glib.Gint);
   --  Equivalent of Gtk.Cell_Renderer_Toggle, but this also sets up a callback
   --  to make sure that only one line is active.

   procedure Set_Editable_And_Callback
     (Model    : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Renderer : access Gtk_Cell_Renderer_Text_Record'Class;
      Column   : Glib.Gint);
   --  Set the renderer as editable, and make sure that its text is updated
   --  when the user has finished editing it.

end Gui_Support;
