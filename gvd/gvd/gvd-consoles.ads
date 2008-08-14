-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2005-2008, AdaCore          --
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

--  Handling of the debugger and debuggee consoles

with GVD.Process;
with GPS.Kernel;

package GVD.Consoles is

   procedure Attach_To_Debugger_Console
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean);
   --  Attach debugger to a console
   --  If an unattached console exists in the desktop, it is reused.
   --  If none exists, one is created if Create_If_Necessary is true.
   --  Nothing is done when Debugger is already attached to a console.

   procedure Attach_To_Debuggee_Console
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean);
   --  Attach to the console for the program that is debugged.

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register menus and other functions to support the consoles

end GVD.Consoles;
