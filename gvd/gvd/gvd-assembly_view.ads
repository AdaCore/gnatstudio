-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                Copyright (C) 2000-2008, AdaCore                   --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package implements a text area target to the display of assembly
--  code.

with GPS.Kernel;
with GVD.Process;

package GVD.Assembly_View is

   procedure Attach_To_Assembly_View
     (Debugger            : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean);
   --  Attach Debugger to an assembly view.
   --  If an unattached assembly view exists in the desktop, it is reused.
   --  Otherwise one, is created if Create_If_Necessary is true.
   --  Nothing is done when Debugger is already attached to an assembly view.
   --
   --  The debugger console should be created already. When it is closed (ie
   --  the debugger exits), the assembly view will be destroyed.

   procedure Update_Assembly_View
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class);
   --  Update the assembly view

   procedure Update_Breakpoints
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class);
   --  Refresh the breakpoint information associated with the assembly view

   procedure Set_Source_Line
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Line     : Natural);

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register menus and other functions to support the assembly view

end GVD.Assembly_View;
