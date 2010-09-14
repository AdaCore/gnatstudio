-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                   Copyright (C) 2000-2010, AdaCore                --
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

with GVD.Process;
with GPS.Kernel;

package GVD.Memory_View is

   procedure Attach_To_Memory
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean);
   --  Attach Debugger to the memory view editor.
   --  If an unattached editor exists in the desktop, it is reused.
   --  If no editor exists, one is created if Create_If_Necessary is true.
   --  Nothing is done when Debugger is already attached to an editor.
   --
   --  The debugger console should be created already

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register menus and other functions to support the breakpoint editor

   procedure Display_Memory
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Address : String);
   --  Open (if needed) a memory view, and display the memory at the given
   --  address

end GVD.Memory_View;
