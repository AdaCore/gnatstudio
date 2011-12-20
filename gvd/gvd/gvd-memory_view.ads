------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

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
