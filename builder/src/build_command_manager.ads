-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

--  Provides a high-level interface to launch commands.
--  This interface is the one that should be shared by scripts, hooks,
--  menus, toolbar buttons, etc.
--
--  See spec of Builder_Facility_Module for an overview of the build system.

with GPS.Kernel;
with Build_Configurations; use Build_Configurations;

package Build_Command_Manager is

   procedure Launch_Target
     (Kernel      : GPS.Kernel.Kernel_Handle;
      Registry    : Build_Config_Registry_Access;
      Target_Name : String);
   --  Launch a build of target named Target_Name

end Build_Command_Manager;
