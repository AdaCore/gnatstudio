-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

--  Builder module.

with Gtk.Menu;
with GPS.Kernel.Modules;
with String_List_Utils;
with Projects;

package Builder_Module is

   type Builder_Module_ID_Record is
     new GPS.Kernel.Modules.Module_ID_Record
   with record
      Make_Menu  : Gtk.Menu.Gtk_Menu;
      Run_Menu   : Gtk.Menu.Gtk_Menu;
      --  The build menu, updated automatically every time the list of main
      --  units changes.

      Last_Project_For_Menu : Projects.Project_Type := Projects.No_Project;
      --  Project used to fill the Run_Menu and Make_Menu

      Output     : String_List_Utils.String_List.List;
      --  The last build output

      Build_Count : Natural := 0;
      --  Number of on-going builds
   end record;
   type Builder_Module_ID_Access is access all Builder_Module_ID_Record;
   --  Data stored with the module id

   Builder_Module_ID : Builder_Module_ID_Access;
   Builder_Module_Name : constant String := "Builder";

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module in the list

end Builder_Module;
