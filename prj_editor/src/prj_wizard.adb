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

with Gtk.Main;     use Gtk.Main;
with Creation_Wizard; use Creation_Wizard;
with Glide_Kernel;  use Glide_Kernel;

procedure Prj_Wizard is
   Wiz  : Creation_Wizard.Prj_Wizard;
   Kernel : Kernel_Handle;
begin
   Gtk.Main.Init;
   Gtk_New (Kernel, null, "");

   Gtk_New (Wiz, Kernel);
   Set_Current_Page (Wiz, 1);
   Show_All (Wiz);

   Main;
end Prj_Wizard;
