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

with GVD.Main_Window;
with Glide_Kernel;
with Gtk.Item_Factory; use Gtk.Item_Factory;

package Glide_Main_Window is

   type Glide_Window_Record is new GVD.Main_Window.GVD_Main_Window_Record with
   record
      Kernel      : Glide_Kernel.Kernel_Handle;
      Interrupted : Boolean := False;
   end record;
   type Glide_Window is access all Glide_Window_Record'Class;

   procedure Gtk_New
     (Main_Window : out Glide_Window;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array);
   --  Create a new main window.
   --  Key is a unique string identifying main_window.
   --  Menu_Items is used to create the menu bar.

   procedure Initialize
     (Main_Window : access Glide_Window_Record'Class;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array);
   --  Internal initialization function.

end Glide_Main_Window;
