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
with Gdk.Pixbuf; use Gdk.Pixbuf;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Image; use Gtk.Image;
with Gtk.Item_Factory; use Gtk.Item_Factory;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Main;

package Glide_Main_Window is

   type Glide_Window_Record is new GVD.Main_Window.GVD_Main_Window_Record with
   record
      Kernel          : Glide_Kernel.Kernel_Handle;
      Toolbar         : Gtk_Toolbar;
      Animation_Frame : Gtk_Frame;
      Animation       : Gdk_Pixbuf_Animation;
      Animation_Iter  : Gdk_Pixbuf_Animation_Iter;
      Animation_Image : Gtk_Image;
      Timeout_Id      : Gtk.Main.Timeout_Handler_Id;
      Busy_Level      : Integer := 0;
      Interrupted     : Boolean := False;
   end record;
   type Glide_Window is access all Glide_Window_Record'Class;

   procedure Gtk_New
     (Main_Window      : out Glide_Window;
      Key              : String;
      Menu_Items       : Gtk_Item_Factory_Entry_Array;
      Home_Dir         : String;
      Prefix_Directory : String);
   --  Create a new main window.
   --  Key is a unique string identifying main_window.
   --  Menu_Items is used to create the default menu bar.
   --  Home_Dir is the home directory (e.g ~/.glide) under which configuration
   --  files will be saved.
   --  Prefix_Directory is the prefix where Glide is installed (e.g /opt/gnu).

   procedure Initialize
     (Main_Window      : access Glide_Window_Record'Class;
      Key              : String;
      Menu_Items       : Gtk_Item_Factory_Entry_Array;
      Home_Dir         : String;
      Prefix_Directory : String);
   --  Internal initialization function.

   function Anim_Cb (Kernel : Glide_Kernel.Kernel_Handle) return Boolean;
   --  Function called when the glide animation needs to be updated.

end Glide_Main_Window;
