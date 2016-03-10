------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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

--  This package implements a text area target to the display of assembly
--  code.

with GPS.Kernel;
with GVD.Process;

package GVD.Assembly_View is

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
