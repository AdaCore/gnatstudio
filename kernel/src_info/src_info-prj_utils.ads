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
      Naming   : Prj.Naming_Data)
      return Name_Id;
   --  Compute the Unit Name associated to the given Filename using the
   --  Naming information, whithout taking the exceptions into account.

   function Extension_Matches
     (Filename  : File_Name_Type;
      Extension : Name_Id)
      return Boolean;
   --  Return true if the given filename has the given extension.
   --  Note that this function also return False when Filename = Extension
   --  as this does not make sense for a source filename.

end Src_Info.Prj_Utils;
