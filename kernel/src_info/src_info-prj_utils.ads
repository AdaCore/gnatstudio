-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
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

with Prj;      use Prj;
with Prj.Com;
with Types;    use Types;

package Src_Info.Prj_Utils is

   --  procedure Get_Source_Filenames
   --     (Project       : Prj.Project_Id;
   --      Unit_Name     : String;
   --      Spec_Filename : out Types.Name_Id;
   --      Body_Filename : out Types.Name_Id);
   --  Get the name of the files containing the specifications and the body
   --  of a given Unit_Name. The Unit_Name must be all lower-case. If the
   --  given unit does not have a spec or a body, the corresponding Name_Id
   --  return is set to No_Name.
   --  ??? This does not work for system files such as the RTL files for
   --  ??? instance.

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
      Naming    : Prj.Naming_Data)
      return String;
   --  Return the source filename for the given Unit_Name, using the
   --  Naming_Data (including the exception list). Unit_Name must be
   --  encoded in the same format as the Unit_Name in the 'W' lines
   --  of the GNAT ALI files (the encoding is used to determine the
   --  unit part).

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
   --  Compute the Unit Name associated to the given Filename using the
   --  Naming information, whithout taking the exceptions into account.
   --  ??? Note that this function does not handles krunched filenames
   --  ??? at all (and will probably never do, since it does not have
   --  ??? enough information to do so).

   function Find_Object_File
     (Project_View           : Prj.Project_Id;
      Short_File_Name        : String;
      Predefined_Object_Path : String := "")
      return String;
   --  Try to locate the given object file in the project object path. If
   --  not found, and Predefined_Project_Path is defined, then try to locate
   --  the object file there.
   --  The empty string is returned if the file was not found.

end Src_Info.Prj_Utils;
