with SN; use SN;

private package Src_Info.LI_Utils is

   procedure Insert_Declaration (
      Handler                 : in LI_Handler;
      --  Library Information handler
      File                    : in out LI_File_Ptr;
      --  root pointer to whole LI_File structure
      Xref_Filename           : in String;
      --  this names will be assigned to the LI_File
      List                    : in out LI_File_List;
      --  common list of LI_Files
      Symbol_Name             : in String;
      --  Name of the declared symbol
      Source_Filename         : in String;
      --  Name of the file in which symbol is declared
      Location                : in Point;
      --  Line & Column pair where declaration is occured
      Parent_Filename         : in String := "";
      --  this filename is the Xref filename of the parent entity
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
      Declaration_Info        : out E_Declaration_Info_List
      --  pointer to created E_Declaration_Info_Node is returned in this
      --  parameter.
   );
   --  Inserts new declaration with specified parameters to given
   --  LI structure tree. The tree is specified by pointer to it's root
   --  element - File arg).
   --  This procedure:
   --    1. Inserts new declaration into the LI structrure tree
   --    2. Returns pointer to created declaration (Declaration_Info param)
   --  Throws Parent_Not_Available exception if LI_Structure for the file with
   --  parent is not created yet.

   procedure Insert_Dependency
     (Handler                 : in LI_Handler;
      File                    : in out LI_File_Ptr;
      Xref_Filename           : in String;
      List                    : in out LI_File_List;
      Source_Filename         : in String;
      Referred_Filename       : in String);
   --  Insert file-level dependency

   procedure Insert_Dependency_Declaration
     (Handler                 : in LI_Handler;
      File                    : in out LI_File_Ptr;
      Xref_Filename           : in String;
      List                    : in out LI_File_List;
      Symbol_Name             : in String;
      Referred_Filename       : in String;
      Source_Filename         : in String;
      Location                : in Point;
      Parent_Filename         : in String := "";
      --  this filename is the Xref filename of the parent entity
      Parent_Location         : in Point := Invalid_Point;
      Kind                    : in E_Kind;
      Scope                   : in E_Scope;
      End_Of_Scope_Location   : in Point := Invalid_Point;
      Rename_Filename         : in String := "";
      Rename_Location         : in Point := Invalid_Point;
      Declaration_Info        : out E_Declaration_Info_List);
   --  Inserts new dependency declaration with specified parameters
   --  to given LI structure tree.
   --  Throws Parent_Not_Available exception if LI_Structure for the
   --  file with parent is not created yet.

   procedure Add_Parent
     (Declaration_Info        : in out E_Declaration_Info_List;
      List                    : in LI_File_List;
      Parent_Filename         : in String;
      Parent_Location         : in Point);
      --  this filename is the Xref filename of the parent entity
   --  Adds a new parent to the list of parent locations for given declaration

   procedure Set_End_Of_Scope
     (Declaration_Info        : in out E_Declaration_Info_List;
      Location                : in Point;
      Kind                    : in Reference_Kind := End_Of_Body);
   --  Sets given value for End_Of_Scope attribute of specified declaration

   procedure Insert_Reference
     (Declaration_Info        : in out E_Declaration_Info_List;
      File                    : in LI_File_Ptr;
      Source_Filename         : in String;
      Location                : in Point;
      Kind                    : Reference_Kind);
   --  Inserts new reference to declaration. Declaration here is specified
   --  by pointer to appropriate E_Declaration_Info_Node object

   No_Kind : E_Kind := Task_Type;
   --  This constant is used to represent the absence of Kind.
   --  Task_Type is used here because it can never occur in C/CPP program.

   function Find_Declaration
     (File                    : in LI_File_Ptr;
      Symbol_Name             : in String := "";
      Class_Name              : in String := "";
      Kind                    : in E_Kind := No_Kind;
      Location                : in Point := Invalid_Point)
   return E_Declaration_Info_List;
   --  Finds declaration in LI tree by it's Name, Location or (and) Kind
   --  If value for some attribute is not given then this attribute doesn't
   --  affect on searching.
   --  Throws Declaration_Not_Found exception if not found.

   function Find_Dependency_Declaration
     (File                    : in LI_File_Ptr;
      Symbol_Name             : in String := "";
      Class_Name              : in String := "";
      Filename                : in String := "";
      Kind                    : in E_Kind := No_Kind;
      Location                : in Point := Invalid_Point)
   return E_Declaration_Info_List;
   --  Finds declaration in LI tree by it's Name and Location.
   --  If value for some attribute is not given then this attribute doesn't
   --  affect on searching.
   --  Throws Declaration_Not_Found exception if not found.

   Declaration_Not_Found : exception;
   --  Thrown by Find_Declaration functions if declaration is not found

   Parent_Not_Available : exception;
   --  Thrown if information on parent for current symbol is not available

end Src_Info.LI_Utils;
