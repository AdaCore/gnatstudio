-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Prj_Editor_Window; use Prj_Editor_Window;
with Gtk.Main;          use Gtk.Main;
with Ada.Command_Line;  use Ada.Command_Line;
with Glide_Kernel;      use Glide_Kernel;
with Glide_Kernel.Project; use Glide_Kernel.Project;

procedure Prj_Editor is
   Win : Project_Editor;
   Kernel : Kernel_Handle;
begin
   Gtk.Main.Init;
   Gtk_New (Kernel);
   Load_Project (Kernel, Argument (1));

   Gtk_New (Win, Kernel);
   Show_All (Win);
   Main;
end Prj_Editor;
