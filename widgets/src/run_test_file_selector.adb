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

with Gtk; use Gtk;
with Gtk.Main;

with Gdk.Pixmap; use Gdk.Pixmap;
with Gdk.Color;  use Gdk.Color;
with Test_File_Selector; use Test_File_Selector;
with Gtkada.File_Selector; use Gtkada.File_Selector;
with Gtkada.Handlers; use Gtkada.Handlers;

with Pixmaps_IDE; use Pixmaps_IDE;

procedure Run_Test_File_Selector is
   File_Selector_Window : File_Selector_Window_Access;

   Filter_A : Filter_Show_All_Access := new Filter_Show_All;
   Filter_B : Filter_Show_Ada_Access := new Filter_Show_Ada;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (File_Selector_Window, "");

   Create_From_Xpm_D
     (Filter_B.Spec_Pixmap,
      Window => null,
      Colormap => Get_System,
      Mask => Filter_B.Spec_Bitmap,
      Transparent => Null_Color,
      Data => box_xpm);

   Create_From_Xpm_D
     (Filter_B.Body_Pixmap,
      Window => null,
      Colormap => Get_System,
      Mask => Filter_B.Body_Bitmap,
      Transparent => Null_Color,
      Data => package_xpm);

   Register_Filter (File_Selector_Window, Filter_A);
   Register_Filter (File_Selector_Window, Filter_B);

   Show_All (File_Selector_Window);
   Widget_Callback.Connect
     (Get_Ok_Button (File_Selector_Window), "clicked",
      Widget_Callback.To_Marshaller (On_Ok_Button_Clicked'Access));
   Widget_Callback.Connect
     (Get_Cancel_Button (File_Selector_Window), "clicked",
      Widget_Callback.To_Marshaller (On_Cancel_Button_Clicked'Access));
   Gtk.Main.Main;
end Run_Test_File_Selector;
