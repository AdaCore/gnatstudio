-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2005-2008, AdaCore                 --
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

with GPS.Kernel;

package Completion_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register this module.
   --  Note this isn't a real module, and therefore shouldn't be register by
   --  gps-main.adb, only by the source editor itself. There are just too
   --  many links with the rest of the source editor.

   procedure Reset_Completion_Data;
   --  Reset the current completion data. It should be called only when
   --  the user performs a completion operation.

   procedure Remove_Completion;
   --  Remove the completion window

   function In_Smart_Completion return Boolean;
   --  Return True if we are currently showing a smart completion window.

end Completion_Module;
