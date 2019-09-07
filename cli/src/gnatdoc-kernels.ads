------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2016-2019, AdaCore                   --
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

with GNATCOLL.Projects;

with GPS.CLI_Kernels;
with Projects;

package GNATdoc.Kernels is

   type GNATdoc_Kernel_Record is
     new GPS.CLI_Kernels.CLI_Kernel_Record with null record;

   type GNATdoc_Kernel_Access is access all GNATdoc_Kernel_Record'Class;

   overriding procedure Create_Registry
     (Self   : not null access GNATdoc_Kernel_Record;
      Result : out Projects.Project_Registry_Access);

end GNATdoc.Kernels;
