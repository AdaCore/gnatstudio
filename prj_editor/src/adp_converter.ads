-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2004-2010, AdaCore                 --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Projects;

package Adp_Converter is

   procedure Convert_Adp_File
     (Adp_Filename   : Filesystem_String;
      Registry       : Projects.Project_Registry'Class;
      Project        : in out GNATCOLL.Projects.Project_Type;
      Spec_Extension : String;
      Body_Extension : String);
   --  Set in Project the properties read from the adp file

end Adp_Converter;
