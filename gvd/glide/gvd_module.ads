-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2001-2003                      --
--                             ACT-Europe                            --
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

--  This package defines the GLIDE module for communication with GVD.

with Glide_Kernel;
with Glib.Object;

package GVD_Module is

   GVD_Module_ID   : Glide_Kernel.Module_ID;
   GVD_Module_Name : constant String := "Debugger";

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   procedure Initialize_Debugger
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialize the debugger if needed.

   procedure On_Assembly
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : Glide_Kernel.Kernel_Handle);
   --  Display the assembly view.
   --  Used e.g. for implementing menu Debug->Data->Assembly
   --  Widget paramter is ignored.

end GVD_Module;
