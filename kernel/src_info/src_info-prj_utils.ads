-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

with Types;    use Types;

package Src_Info.Prj_Utils is

   function Get_Source_Filename
     (Unit_Name       : Unit_Name_Type;
      Project         : Projects.Project_Type) return String;
   --  Return the base name for the given Unit_Name.
   --  Project and all its imported projects are tested for possible naming
   --  schemes.
   --  Unit_Name must be encoded in the same format as the Unit_Name in the 'W'
   --  lines of the GNAT ALI files (the encoding is used to determine the unit
   --  part).

   function Get_Source_Filename
     (Unit_Name : String;
      Project   : Projects.Project_Type) return String;
   --  Same as function above, on a string.

   function Get_Unit_Name
     (Filename : File_Name_Type;
      Project  : Projects.Project_Type)
      return Name_Id;
   --  Compute the Unit Name associated with the given Filename using the
   --  Naming information, whithout taking the exceptions into account.
   --  ??? Note that this function does not handles krunched filenames at all
   --  (and will probably never do, since it does not have enough information
   --  to do so).

end Src_Info.Prj_Utils;
