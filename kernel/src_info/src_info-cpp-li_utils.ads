with SN; use SN;
private package Src_Info.LI_Utils is

   procedure Insert_Declaration (
      Handler                 : in LI_Handler;
      --  Language Information handler
      File                    : in out LI_File_Ptr;
      --  root pointer to whole LI_File structure
      Symbol_Name             : in String;
      --  Name of the declared symbol
      Source_Filename         : in String;
      --  Name of the file in which symbol is declared
      Location                : in Point;
      --  Line & Column pair where declaration is occured
      Parent_Filename         : in String := "";
      --  (if parent available - for classes, subtypes and etc)
      --  Name of file where the parent is declared
      Parent_Location         : in Point := Invalid_Point;
      --  Location of parent declaration in file
      Kind                    : in E_Kind;
      --  Kind of symbol
      Scope                   : in E_Scope;
      End_Of_Scope_Location   : in Point := Invalid_Point;
      --  End of scope pointer (for functions, methods, classes and other
      --  entities that have a scope)
      Rename_Filename         : in String := "";
      --  for now skip that
      Rename_Location         : in Point := Invalid_Point;
      --  skip that
      Declaration_Info        : in out E_Declaration_Info_List
      --  pointer to created E_Declaration_Info_Node is returned in this
      --  parameter. If parameter is set to NULL then nothing is
      --  returned.
   );
   --  Inserts new declaration with specified parameters to given
   --  LI structure tree. The tree is specified by pointer to it's root element
   --  - File arg).
   --  This procedure:
   --    1. Inserts new declaration into the LI structrure tree
   --    2. Returns pointer to created declaration (Declaration_Info param)

   procedure Insert_Dependency_Declaration (
      Handler                 : in LI_Handler;
      File                    : in out LI_File_Ptr;
      Symbol_Name                    : in String;
      Source_Filename         : in String;
      Location                : in Point;
      Parent_Filename         : in String := "";
      Parent_Location         : in Point := Invalid_Point;
      Kind                    : in E_Kind;
      Scope                   : in E_Scope;
      End_Of_Scope_Location   : in Point := Invalid_Point;
      Rename_Filename         : in String := "";
      Rename_Location         : in Point := Invalid_Point;
      Declaration_Info        : in out E_Declaration_Info_List);
   --  Inserts new dependency declaration with specified parameters to given
   --  LI structure tree.

   procedure Insert_Reference (
      Declaration_Info        : in out E_Declaration_Info_List;
      Source_Filename         : in String;
      Location                : in Point;
      Kind                    : Reference_Kind);
   --  Inserts new reference to declaration. Declaration here is specified
   --  by pointer to appropriate E_Declaration_Info_Node object

   function Find_Declaration (
      File                    : in LI_File_Ptr;
      Name                    : in String;
      Location                : in Point) return E_Declaration_Info_List;
   --  Finds declaration in LI tree by it's Name and Location

   function Find_Dependency_Declaration (
      File                    : in LI_File_Ptr;
      Name                    : in String;
      Location                : in Point) return E_Declaration_Info_List;
   --  Finds declaration in LI tree by it's Name and Location

   Invalid_Source_Filename : exception;
   --  Thrown if specified Source_Filename differs from that found in
   --  File.Body_Info.Source_Filename

   Invalid_Handler : exception;
   --  Thrown if specified handler differs from that found in
   --  File.Handler

end Src_Info.LI_Utils;
