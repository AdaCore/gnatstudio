------------------------------------------------------------------------------
--                                  G P S                                   --
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

package GNATdoc.Project_Environments is

   type GNATdoc_Project_Environment is
     new GNATCOLL.Projects.Project_Environment with private;

   function Get_GNAT_Version
     (Self : GNATdoc_Project_Environment'Class) return String;
   --  Returns version of GNAT detected by GNATdoc.

private

   type GNATdoc_Project_Environment is
     new GNATCOLL.Projects.Project_Environment with record
      GNAT_Version : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding procedure Set_GNAT_Version
     (Self    : in out GNATdoc_Project_Environment;
      Version : String);

end GNATdoc.Project_Environments;
