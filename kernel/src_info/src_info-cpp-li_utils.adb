package Src_Info.CPP.LI_Utils is

   procedure Insert_Declaration (
      File                    : in out LI_File_Ptr;
      Name                    : in String;
      Source_Filename         : in String
            := LI_File_Ptr.Body_Info.Source_Filename.all;
      Location                : in Point;
      Parent_Filename         : in String := NULL;
      Parent_Location         : in Point := Invalid_Point;
      Kind                    : in E_Kind;
      Scope                   : in E_Scope;
      End_Of_Scope_Location   : in Point := Invalid_Point;
      Declaration_Info        : in out E_Declaration_Info_List := NULL;
   );
   --  Inserts new declaration with specified parameters to given
   --  LI structure tree. The tree is specified by pointer to it's root element
   --  - File arg).
   --  This procedure:
   --    1. Inserts new declaration into the LI structrure tree
   --    2. Returns pointer to created declaration (Declaration_Info param)
   --  If last parameter is omitted then no pointer is returned.

   procedure Insert_Dependency_Declaration (
      File                    : in out LI_File_Ptr;
      Name                    : in String;
      Source_Filename         : in String
            := LI_File_Ptr.Body_Info.Source_Filename.all;
      Location                : in Point;
      Parent_Filename         : in String := NULL;
      Parent_Location         : in Point := Invalid_Point;
      Kind                    : in E_Kind;
      Scope                   : in E_Scope;
      End_Of_Scope_Location   : in Point := Invalid_Point;
      Declaration_Info        : in out E_Declaration_Info_List := NULL;
   );
   --  Inserts new dependency declaration with specified parameters to given
   --  LI structure tree.

   procedure Insert_Reference (
      Declaration_Info        : in out E_Declaration_Info_List;
      Source_Filename         : in String;
      Location                : in Point;
      Kind                    : Reference_Kind;
   );
   --  Inserts new reference to declaration. Declaration here is specified
   --  by pointer to appropriate E_Declaration_Info_Node object

   function Find_Declaration (
      File                    : in out LI_File_Ptr;
      Name                    : in String;
      Location                : in Point;
   ) return E_Declaration_Info_List;
   --  Finds declaration in LI tree by it's Name and Location

   function Find_Dependency_Declaration (
      File                    : in out LI_File_Ptr;
      Name                    : in String;
      Location                : in Point;
   ) return E_Declaration_Info_List;
   --  Finds declaration in LI tree by it's Name and Location

end Src_Info.CPP.LI_Utils;
