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

with Gtk.Main;
with Gtk.Window;           use Gtk.Window;
with Src_Editor_Box;       use Src_Editor_Box;
with Src_Menu;             use Src_Menu;

with Glide_Kernel.Project; use Glide_Kernel.Project;
with Language;             use Language;
with Language.Ada;         use Language.Ada;
with Language.C;           use Language.C;
with Language.Cpp;         use Language.Cpp;

procedure Src is
   My_Box      : Source_Editor_Box;
   Main_Window : Gtk_Window;
   Kernel      : Glide_Kernel.Kernel_Handle;
   --  Ignored     : Boolean;

begin
   Add_File_Extensions (Ada_Lang, ".ads;.adb;.ada");
   Add_File_Extensions (C_Lang, ".h;.c");
   Add_File_Extensions (Cpp_Lang, ".hh;.cc;.cpp");

   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   Glide_Kernel.Gtk_New (Kernel, null);
   Load_Project (Kernel, "../src_editor.gpr");

   Gtk_New (My_Box, Kernel);
   Create_Main_Window (Main_Window, My_Box);
   Set_Title (Main_Window, "The GLIDE Source Editor");
   Show_All (Main_Window);
   --  Load_File
   --    (My_Box, "src_editor_box.adb", Success => Ignored);

   Gtk.Main.Main;
end Src;

