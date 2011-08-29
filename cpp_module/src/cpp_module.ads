-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2011, AdaCore              --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with GPS.Kernel;
with Entities;
with Projects;
with Language_Handlers;

package Cpp_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the C/C++ parsers in GPS.
   --  If the external source navigator executables are not found on the path,
   --  an error is displayed in the console and the C/C++ browsing will not be
   --  available.

   function Create_CPP_Handler
     (Db           : Entities.Entities_Database;
      Registry     : Projects.Project_Registry'Class;
      Lang_Handler : Language_Handlers.Language_Handler)
      return Entities.LI_Handler;
   --  Create a new C++ handler

end Cpp_Module;
