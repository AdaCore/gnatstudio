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

with Glide_Kernel; use Glide_Kernel;
with Gtk.Box;      use Gtk.Box;
with Gtk.Enums;    use Gtk.Enums;
with Gtk.Window;   use Gtk.Window;

package body Glide_Main_Window is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Main_Window : out Glide_Window;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array) is
   begin
      Main_Window := new Glide_Window_Record;
      Glide_Main_Window.Initialize (Main_Window, Key, Menu_Items);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Main_Window : access Glide_Window_Record'Class;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array) is
   begin
      GVD.Main_Window.Initialize (Main_Window, Key, Menu_Items);
      Gtk_New (Main_Window.Kernel, Gtk_Window (Main_Window));
      Gtk_New (Main_Window.Toolbar, Orientation_Horizontal, Toolbar_Icons);
      Set_Tooltips (Main_Window.Toolbar, True);
      Pack_Start (Main_Window.Toolbar_Box, Main_Window.Toolbar, False, False);
      Main_Window.Standalone := False;
   end Initialize;

end Glide_Main_Window;
