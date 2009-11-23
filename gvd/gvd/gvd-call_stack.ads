-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                 Copyright (C) 2003-2008, AdaCore                  --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with GVD.Process;
with GPS.Kernel;

package GVD.Call_Stack is

   procedure Highlight_Call_Stack_Frame
     (Process : access GVD.Process.Visual_Debugger_Record'Class);
   --  Highlight frame number Frame based on the current debugger output
   --  stored in Process. Nothing is done if Process is not associated with
   --  a call stack

   procedure Attach_To_Call_Stack
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean);
   --  Attach Debugger to a call stack.
   --  If an unattached call stack exists in the desktop, it is reused.
   --  If no call stack exists, one is created if Create_If_Necessary is true.
   --  Nothing is done when Debugger is already attached to a call stack.
   --
   --  The debugger console should be created already. When it is closed (ie
   --  the debugger exits), the call stack will be destroyed.

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register menus and other functions to support the callstacks

end GVD.Call_Stack;
