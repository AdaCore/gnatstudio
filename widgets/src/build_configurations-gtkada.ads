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

--  This package implements a GtkAda-based GUI for managing build
--  configurations.
--
--  It is intended to depend on GtkAda but not on GPS.

package Build_Configurations.Gtkada is

   procedure Configuration_Dialog
     (Registry : Build_Config_Registry_Access);
   --  Launch the full configuration dialog

   function Single_Target_Dialog
     (Registry : Build_Config_Registry_Access;
      Target   : String) return GNAT.OS_Lib.Argument_List;
   --  Launch a dialog allowing to modify the command line for Target only.
   --  Return the resulting command followed by arguments, macros not
   --  expanded.

end Build_Configurations.Gtkada;
