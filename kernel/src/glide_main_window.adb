-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Glib.Error;   use Glib.Error;
with Glide_Kernel; use Glide_Kernel;
with Gtk.Box;      use Gtk.Box;
with Gtk.Enums;    use Gtk.Enums;
with Gtk.Image;    use Gtk.Image;
with Gtk.Window;   use Gtk.Window;
with GNAT.OS_Lib;  use GNAT.OS_Lib;

package body Glide_Main_Window is

   Animated_Image : constant String := "glide-animation.gif";
   --  <preferences>

   -------------
   -- Anim_Cb --
   -------------

   function Anim_Cb (Kernel : Kernel_Handle) return Boolean is
      Window : Glide_Window := Glide_Window (Get_Main_Window (Kernel));
   begin
      if Advance (Window.Animation_Iter) then
         Set (Window.Animation_Image, Get_Pixbuf (Window.Animation_Iter));
      end if;

      return True;
   end Anim_Cb;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Main_Window      : out Glide_Window;
      Key              : String;
      Menu_Items       : Gtk_Item_Factory_Entry_Array;
      Home_Dir         : String;
      Prefix_Directory : String) is
   begin
      Main_Window := new Glide_Window_Record;
      Glide_Main_Window.Initialize
        (Main_Window, Key, Menu_Items, Home_Dir, Prefix_Directory);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Main_Window      : access Glide_Window_Record'Class;
      Key              : String;
      Menu_Items       : Gtk_Item_Factory_Entry_Array;
      Home_Dir         : String;
      Prefix_Directory : String)
   is
      Box1   : Gtk_Hbox;
      Error  : GError;
      Pixbuf : Gdk_Pixbuf;

   begin
      GVD.Main_Window.Initialize (Main_Window, Key, Menu_Items);
      Main_Window.Home_Dir := new String' (Home_Dir);
      Main_Window.Prefix_Directory := new String' (Prefix_Directory);
      Main_Window.Standalone := False;
      Gtk_New (Main_Window.Kernel, Gtk_Window (Main_Window), Home_Dir);

      Gtk_New_Hbox (Box1);
      Pack_Start (Main_Window.Toolbar_Box, Box1);
      Gtk_New (Main_Window.Toolbar, Orientation_Horizontal, Toolbar_Icons);
      Set_Tooltips (Main_Window.Toolbar, True);
      Pack_Start (Box1, Main_Window.Toolbar, True, True);

      declare
         File : constant String :=
           Prefix_Directory & Directory_Separator & "share" &
           Directory_Separator & "glide" & Directory_Separator &
           Animated_Image;

      begin
         if Is_Regular_File (File) then
            Gtk_New (Main_Window.Animation_Frame);
            Set_Shadow_Type (Main_Window.Animation_Frame, Shadow_In);
            Pack_End (Box1, Main_Window.Animation_Frame, False, False);

            Gdk_New_From_File (Main_Window.Animation, File, Error);
            Gtk_New (Main_Window.Animation_Image, Main_Window.Animation);
            Main_Window.Animation_Iter := Get_Iter (Main_Window.Animation);
            Pixbuf := Get_Pixbuf (Main_Window.Animation_Iter);
            Set (Main_Window.Animation_Image, Pixbuf);
            Add (Main_Window.Animation_Frame, Main_Window.Animation_Image);
         end if;
      end;
   end Initialize;

end Glide_Main_Window;
