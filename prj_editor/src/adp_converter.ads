------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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
