-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Prj;      use Prj;
with Prj.Com;
with Types;    use Types;

package Src_Info.Prj_Utils is

   function Get_Spec_Filename (U : Prj.Com.Unit_Id) return File_Name_Type;
   --  Return Units.Table (U).File_Names (Specification).Name, or No_Name if
   --  U is equal to Prj_Unit /= Prj.Com.No_Unit.

   function Get_Body_Filename (U : Prj.Com.Unit_Id) return File_Name_Type;
   --  Return Units.Table (U).File_Names (Body_Part).Name, or No_Name if
   --  U is equal to Prj_Unit /= Prj.Com.No_Unit.

   function Get_Unit_Name (Id : Array_Element_Id) return Unit_Name_Type;
   --  Return Prj.Array_Elements.Table (Id).Index

   function Get_Filename (Id : Array_Element_Id) return File_Name_Type;
   --  Return Prj.Array_Elements.Table (Id).Value.Value

   function Search_Unit_Name
     (Exception_List : Prj.Array_Element_Id;
      Unit_Name      : Unit_Name_Type)
      return Prj.Array_Element_Id;
   --  Search the given Unit_Name in the exception list. return
   --  No_Array_Element if not found.

   function Search_Filename
     (Exception_List : Prj.Array_Element_Id;
      Filename       : File_Name_Type)
      return Prj.Array_Element_Id;
   --  Search the given Filename in the exception list. return
   --  No_Array_Element if not found.

   function Get_Source_Filename
     (Unit_Name : Unit_Name_Type;
      Project   : Prj.Project_Id)
      return String;
   --  Return the source filename for the given Unit_Name.
   --  Project and all its imported projects are tested for possible naming
   --  schemes.
   --  Unit_Name must be encoded in the same format as the Unit_Name in the 'W'
   --  lines of the GNAT ALI files (the encoding is used to determine the unit
   --  part).

   function Get_Source_Filename
     (Unit_Name : String;
      Project   : Prj.Project_Id) return String;
   --  Same as function above, on a string.

   function Get_Spec_Filename
     (Unit_Name : Unit_Name_Type;
      Naming    : Prj.Naming_Data)
      return File_Name_Type;
   --  Compute the file name of the spec for the given Unit_Name using the
   --  Naming_Data, without taking the exceptions into account.

   function Get_Body_Filename
     (Unit_Name : Unit_Name_Type;
      Naming    : Prj.Naming_Data)
      return File_Name_Type;
   --  Compute the file name of the body for the given Unit_Name using the
   --  Naming_Data, without taking the exceptions into account.

   function Get_Unit_Name
     (Filename : File_Name_Type;
      Project  : Prj.Project_Id)
      return Name_Id;
   --  Compute the Unit Name associated with the given Filename using the
   --  Naming information, whithout taking the exceptions into account.
   --  ??? Note that this function does not handles krunched filenames at all
   --  (and will probably never do, since it does not have enough information
   --  to do so).

   function Find_File
     (Short_File_Name, Path, Predefined_Path : String) return String;
   --  Try to locate the file on the given path.
   --  For an object file, Path should be set to the object path for the
   --  project. If the file is not found there, the file is looked for on the
   --  Predefined_Path.
   --  The empty string is returned if the file was not found.

end Src_Info.Prj_Utils;
