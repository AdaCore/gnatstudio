-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2002 - 2004                      --
--                            ACT-Europe                             --
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

--  This package provides facilities for customizing menus,
--  keyboard shortcuts, and so on, in GPS.

with Glide_Kernel; use Glide_Kernel;

package Custom_Module is

   type Custom_Module_ID_Record is new Module_ID_Record with record
      Kernel        : Kernel_Handle;
   end record;
   type Custom_Module_ID_Access is access all Custom_Module_ID_Record'Class;

   Custom_Module_ID   : Custom_Module_ID_Access;


   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

end Custom_Module;
