-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2002-2009, AdaCore                  --
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

with Namet;
with Prj.Tree;          use Prj, Prj.Tree;
with Output;
with Projects.Registry;

--  All subprograms related to editing projects.
--  All subprograms in this package will automatically normalize the projects
--  before editing, and mark them as modified afterwards. However, you should
--  always send a Recompute_View request to the kernel to force a recomputation
--  after a series of modifications.

package Projects.Editor is

   type Project_Node_Array is array (Positive range <>) of Project_Node_Id;
   type Project_Id_Array is array (Positive range <>) of Prj.Project_Id;

   Normalize_Error : exception;
   --  Raised when a project could not be normalized (necessary step before any
   --  modification). The project cannot be edited by GPS.
   --  Any subprogram in this package might raise this exception. In that case,
   --  the exception message is set to the internationalized text of the error.

   Any_Attribute : constant String := "@@";
   Any_Attribute_Name : Namet.Name_Id;
   --  Special value for all the subprograms that take an Attribute_Index
   --  parameter. When this is used, no matching is done on the indexes.

   ----------
   -- Misc --
   ----------

   function Get_String (Str : String) return Namet.Name_Id;
   --  Convert Str to a name_id

   function Get_String (Str : String) return Namet.File_Name_Type;
   --  Convert Str to a file_name_type

   function Get_String (Str : String) return Namet.Path_Name_Type;
   --  Convert Str to a path_name_type

   function Length
     (Tree : Prj.Project_Tree_Ref; List : Prj.String_List_Id) return Natural;
   --  Return the number of elements in the list

   --------------
   -- Projects --
   --------------

   function Create_Project
     (Registry : Projects.Registry.Project_Registry'Class;
      Name     : String;
      Path     : GNATCOLL.VFS.Virtual_File) return Project_Type;
   --  Create a new empty project and its declaration.
   --  The project is also registered, so that it can be retrieved from one of
   --  its view.

   procedure Rename_And_Move
     (Root_Project  : Project_Type;
      Project       : Project_Type;
      New_Name      : Filesystem_String;
      New_Path      : GNATCOLL.VFS.Virtual_File;
      Report_Errors : Output.Output_Proc := null);
   --  Rename Project to New_Name. All the nodes in the project tree starting
   --  at Root_Project, that reference Project, are also updated accordingly.
   --  Also sets the path of the project file. Note that Path shouldn't include
   --  the file name, which is New_Name.
   --
   --  The paths internal to the project are not upgraded, and will remain
   --  relative paths if they were. Consider using Convert_Paths_To_Absolute if
   --  necessary.
   --
   --  If there is already a project by that name in the project hierarchy, an
   --  error is reported through Report_Errors.

   type Import_Project_Error is (Success,
                                 Project_Already_Exists,
                                 Imported_Project_Not_Found,
                                 Dependency_On_Self,
                                 Dependency_Already_Exists,
                                 Circular_Dependency
                                 );

   function Add_Imported_Project
     (Root_Project              : Project_Type;
      Project                   : Project_Type;
      Imported_Project_Location : GNATCOLL.VFS.Virtual_File;
      Report_Errors             : Output.Output_Proc := null;
      Use_Relative_Path         : Boolean;
      Use_Base_Name             : Boolean := False;
      Limited_With              : Boolean := False)
      return Import_Project_Error;
   --  Add a new with_statement for Imported_Project.
   --  Errors while parsing the project file are sent to Report_Errors.
   --  True is returned if the project was modified with success
   --  If Use_Base_Name is true then only the base name of the project is used
   --  in the with statement. Otherwise, if Use_Relative_Path is True, then a
   --  relative path is used in the with statement, otherwise an absolute path
   --  is used.

   function Add_Imported_Project
     (Root_Project      : Project_Type;
      Project           : Project_Type;
      Imported_Project  : Project_Type;
      Report_Errors     : Output.Output_Proc := null;
      Use_Relative_Path : Boolean;
      Use_Base_Name     : Boolean := False;
      Limited_With      : Boolean := False) return Import_Project_Error;
   --  Same as above, but the project is already in memory

   procedure Replace_Project_Occurrences
     (Root_Project      : Project_Type;
      Project           : Project_Type;
      Use_Relative_Path : Boolean);
   --  Replace all references to a project with the same name as Project by
   --  a reference to Project. These changes are done in the with clauses.

   procedure Remove_Imported_Project
     (Project : Project_Type; Imported_Project : Project_Type);
   --  Remove a dependency from Project.
   --  If Imported_Project is not already a dependency, then this subprogram
   --  does nothing.

   function Find_Project_Of_Package
     (Project  : Project_Type;
      Pkg_Name : String) return Project_Type;
   --  Return the id of the project that contains Pkg_Name. It will be
   --  different from Project if the package declaration is a renaming of
   --  another package.

   procedure Normalize_Cases (Project : Projects.Project_Type);
   --  Make sure that all possible values of a variable appear in a case
   --  statement, to avoid warnings from the project manager.
   --  This subprogram doesn't apply recursively to imported projects

   procedure Set_Extended_Project
     (Project            : Projects.Project_Type;
      Extended           : Projects.Project_Type;
      Extend_All         : Boolean := False;
      Use_Relative_Paths : Boolean := False);
   --  Set the project that Project extends. If Extend_All is True, then this
   --  is an "extend all" project.
   --  The project must be recomputed afterward

   -----------
   -- Paths --
   -----------

   function Contains_Path
     (Project : Project_Type;
      Path    : Filesystem_String) return Boolean;
   --  Return value is True if the path is defined in the project

   function Rename_Path
     (Project            : Project_Type;
      Old_Path           : Filesystem_String;
      New_Path           : Filesystem_String;
      Use_Relative_Paths : Boolean) return Boolean;
   --  Renames the path to New_Path.
   --  Return value is True if the directory path could be renamed.

   ---------------
   -- Variables --
   ---------------

   function Create_Environment_Variable
     (Project   : Project_Type;
      Name      : String;
      Type_Name : String;
      Env_Name  : String) return Scenario_Variable;
   --  Create a new typed environment variable, referencing Env_Name, and whose
   --  type is Type_Name.

   function Get_Environment
     (Tree             : Project_Node_Tree_Ref;
      Var_Or_Attribute : Project_Node_Id) return Namet.Name_Id;
   --  Return the name of the environment variable associated with
   --  Var_Or_Attribute. No_String is returned in case there is no such
   --  variable.

   function To_String
     (Tree : Prj.Project_Tree_Ref; Value : Variable_Value) return String;
   --  Convert a variable value to a string suitable for insertion in the list

   function To_Argument_List
     (Tree  : Prj.Project_Tree_Ref;
      Value : Prj.Variable_Value) return GNAT.OS_Lib.Argument_List;
   --  Convert a variable value to a list of arguments.
   --  The result must be freed by the caller.

   procedure Add_Scenario_Variable_Values
     (Root_Project : Project_Type;
      External_Var : Scenario_Variable;
      Values       : Name_Id_Array);
   --  Add some values to the list of possible values for the scenario
   --  variables associated with External_Variable_Name. The changes are done
   --  recursively in Root_Project and all its imported projects.

   procedure Rename_External_Variable
     (Root_Project : Project_Type;
      Variable     : in out Scenario_Variable;
      New_Name     : Namet.Name_Id);
   --  Rename all references to Old_Name in Root_Project and its imported
   --  projects.
   --  Old_Name is given as a string so that we don't need to allocate a new
   --  string_id.

   procedure Set_Default_Value_For_External_Variable
     (Root_Project      : Project_Type;
      Ext_Variable_Name : String;
      Default           : Namet.Name_Id);
   --  Change the default value for all the scenario variables based on
   --  Ext_Variable_Name.

   procedure Delete_External_Variable
     (Root_Project             : Project_Type;
      Ext_Variable_Name        : String;
      Keep_Choice              : String;
      Delete_Direct_References : Boolean := True);
   --  Remove all scenario variables that reference External_Variable_Name.
   --  All the case constructions where this variable occur are replaced by
   --  the case item corresponding to Keep_Choice.
   --  If Delete_Direct_References is True, then all direct references (ie
   --  external() statements in the project file) to Ext_Variable_Name are also
   --  removed, in addition to the scenario variables that reference it.

   procedure Rename_Value_For_External_Variable
     (Root_Project      : Project_Type;
      Ext_Variable_Name : String;
      Old_Value_Name    : String;
      New_Value_Name    : Namet.Name_Id);
   --  Rename one of the choices in the list of possible values for the
   --  scenario variables asociated with Ext_Variable_Name. This also changes
   --  the default value for external references.

   procedure Remove_Value
     (Root_Project      : Project_Type;
      Ext_Variable_Name : String;
      Value_Name        : String);
   --  Remove Value_Name from the list of possible values for the scenario
   --  variables that refer to Ext_Variable_Name. If this is the last possible
   --  value, then the result is the same as calling Delete_External_Variable.
   --  Direct N_External_Value nodes that use Value_Name as the default will
   --  no longer have a default.

   Invalid_Value : exception;

   ----------------
   -- Attributes --
   ----------------

   procedure Delete_Attribute
     (Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute          : Attribute_Pkg;
      Attribute_Index    : String := "");
   --  Remove all declarations for the attribute Attribute_Name in the current
   --  scenario. This effectively reverses to the default behavior for the
   --  attribute.
   --  If Attribute_Index is Any_Attribute, then this subprogram will not try
   --  to match the attribute.

   procedure Update_Attribute_Value_In_Scenario
     (Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute          : Attribute_Pkg;
      Values             : GNAT.OS_Lib.Argument_List;
      Attribute_Index    : String := "";
      Prepend            : Boolean := False);
   --  Update the value of the attribute Attribute_Name in Project/Pkg_Name.
   --  If Pkg_Name is the empty string "", then the value is updated at the
   --  top-level of the project.
   --  List is the list of new values for the variable. If Prepend is False,
   --  these values are the only values for the variable, and they override any
   --  other value that was there before. If Prepend is True, the values in
   --  List are prepended to the current value of the attribute.
   --  Attribute_Index is the associative array index for the attribute (for
   --  instance the file name when modifying the switches).
   --  This subprogram properly handles renaming packages (i.e the project
   --  that contains the real definition of the package is modified, not
   --  necessarily Project itself).
   --
   --  Values remains under the responsability of the caller, memory-wise.

   procedure Update_Attribute_Value_In_Scenario
     (Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute          : Attribute_Pkg;
      Value              : String;
      Attribute_Index    : String := "");
   --  Same as above, but for an attribute that contains a single value

   type Associative_Array_Value is record
      Index : GNAT.Strings.String_Access;
      Value : GNAT.Strings.String_Access;
   end record;
   type Associative_Array_Values is array (Natural range <>)
     of Associative_Array_Value;

   procedure Set_Attribute_Value_In_Scenario
     (Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute          : Attribute_Pkg;
      Values             : Associative_Array_Values);
   --  Remove all currently set values for the attribute, and replace them with
   --  Values. This is more efficient than calling
   --  Update_Attribute_Value_In_Scenario multiple times.

   function Get_Attribute_Value
     (Project      : Project_Type;
      Attribute    : Attribute_Pkg;
      Index        : String := "";
      Use_Extended : Boolean := False) return Variable_Value;
   --  Internal version of Get_Attribute_Value

   function Attribute_Is_Defined
     (Project   : Project_Type;
      Attribute : Attribute_Pkg;
      Index     : String := "") return Boolean;
   --  True if the attribute was defined in the project. Get_Attribute_Value
   --  will return Nil_Variable_Value either if the attribute is not defined
   --  or has a default empty value (for instance for the languages attribute)

   -----------
   -- Lists --
   -----------

   type String_List_Iterator is private;

   function Done (Iter : String_List_Iterator) return Boolean;
   --  Return True if Iter is past the end of the list of strings

   function Next
     (Tree : Prj.Tree.Project_Node_Tree_Ref;
      Iter : String_List_Iterator) return String_List_Iterator;
   --  Return the next item in the list

   function Data (Iter : String_List_Iterator) return Project_Node_Id;
   function Data
     (Tree : Prj.Tree.Project_Node_Tree_Ref;
      Iter : String_List_Iterator) return Namet.Name_Id;
   --  Return the value pointed to by Iter.
   --  This could be either a N_String_Literal or a N_Expression node in the
   --  first case.
   --  The second case only works if Iter points to N_String_Literal.

   function Type_Values
     (Tree        : Project_Node_Tree_Ref;
      Var_Or_Type : Project_Node_Id) return String_List_Iterator;
   --  Return an iterator over the list of possible values for the
   --  N_Typed_Variable_Declaration or N_String_Type_Declaration Var.

   function Value_Of
     (Tree : Project_Node_Tree_Ref;
      Var  : Scenario_Variable) return String_List_Iterator;
   --  Return an iterator over the possible values of the variable

private
   type String_List_Iterator is record
      Current : Project_Node_Id;
      --  pointer to N_Literal_String or N_Expression
   end record;

   -----------
   -- Nodes --
   -----------

   function Clone_Node
     (Tree       : Project_Node_Tree_Ref;
      Node       : Project_Node_Id;
      Deep_Clone : Boolean := False)
      return Project_Node_Id;
   --  Return a copy of Node. If Deep_Clone is true, then all the children of
   --  node are also copied.
   --  If Deep_Clone is false, then the two nodes will share part of their
   --  structure.
   --
   --  Note: nodes like variable or type declarations, packages,... are not
   --  chained up when they are cloned, you need to recreate the proper lists
   --  afterwards. See Post_Process_After_Clone below
   --
   --  A special case also occurs for a N_Typed_Variable_Declaration, since the
   --  type that is referenced is a pointer to the same node as the type for
   --  Node. No deep copy is done for this type. This needs to be fixed in a
   --  post-processing phase, as above.
   --
   --  The same limitation exists for N_Variable_Reference and
   --  N_Attribute_Reference and the package they are referencing

   procedure Post_Process_After_Clone
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Node_Id;
      Pkg     : Project_Node_Id := Empty_Node);
   --  Post-process a project, and make sure that all the internal lists for
   --  variables, packages, types,... are properly chained up, and that all the
   --  variables reference a type declaration in Project (and not in some other
   --  project), ...
   --  On exit, Project is fully independent of whatever old project is was
   --  created from.

   procedure Add_At_End
     (Tree                         : Project_Node_Tree_Ref;
      Parent                       : Project_Node_Id;
      Expr                         : Project_Node_Id;
      Add_Before_First_Pkg         : Boolean := False;
      Add_Before_First_Case        : Boolean := False);
   --  Add a new declarative item in the list in Parent.
   --  This new declarative item will contain Expr (unless Expr is already a
   --  declarative item, in which case it is added directly to the list).
   --  The new item is inserted at the end of the list, unless
   --  Add_Before_First_Pkg is True. In the latter case, it is added
   --  just before the first case construction is seen (in normalized project
   --  files, this corresponds to the end of the common section), or before the
   --  first package

   procedure Add_In_Front
     (Tree   : Project_Node_Tree_Ref;
      Parent : Project_Node_Id;
      Node   : Project_Node_Id);
   --  Add Node at the begining of the list for Parent.
   --  Node can also be a N_Declarative_Item (or a list of them).

   -----------
   -- Cases --
   -----------

   procedure Add_Case_Item
     (Tree      : Prj.Tree.Project_Node_Tree_Ref;
      Case_Node : Prj.Tree.Project_Node_Id;
      Choice    : Namet.Name_Id);
   --  Create a new case item in case_node (which is associated with a
   --  "case var is" statement

   ---------------
   -- Variables --
   ---------------

   function Create_Variable_Reference
     (Tree : Project_Node_Tree_Ref; Var : Project_Node_Id)
      return Project_Node_Id;
   --  Create and return a reference to the variable Var.
   --  Var must be a variable declaration

   procedure Set_Value_As_External
     (Tree          : Project_Node_Tree_Ref;
      Var           : Project_Node_Id;
      External_Name : String;
      Default       : String := "");
   --  Set the value of the variable as a reference to the environment variable
   --  External_Name. Var must be a single value, not a string.
   --  If Var is a typed variable, the default value is checked against the
   --  list of possible values (Invalid_Value raised if not).

   -----------
   -- Types --
   -----------

   function Create_Type
     (Tree       : Project_Node_Tree_Ref;
      Prj_Or_Pkg : Project_Node_Id;
      Name       : String) return Project_Node_Id;
   --  Create a new type. By default, there is no possible value, you
   --  must add some with Add_Possible_Value.
   --  The new declaration is added at the end of the declarative item list for
   --  Prj_Or_Pkg (but before any package declaration).

   function Find_Type_Declaration
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Node_Id;
      Name    : Namet.Name_Id) return Project_Node_Id;
   --  Return the declaration of the type whose name is Name

   procedure Add_Possible_Value
     (Tree   : Project_Node_Tree_Ref;
      Typ    : Project_Node_Id;
      Choice : Namet.Name_Id);
   --  Add a new choice in the list of possible values for the type Typ.
   --  If Choice is already available in Typ, then it is not added again.

   function Create_Typed_Variable
     (Tree                         : Project_Node_Tree_Ref;
      Prj_Or_Pkg                   : Project_Node_Id;
      Name                         : String;
      Typ                          : Project_Node_Id;
      Add_Before_First_Case_Or_Pkg : Boolean := False)
      return Project_Node_Id;
   --  Create a new variable of a specific type Typ.
   --  The declaration is appended at the end of the declarative items list in
   --  the project or the package, unless Add_Before_First_Case is True. In
   --  this case, it is put just before the first N_Case_Construction node is
   --  encountered (i.e the last position in the common section of a normalized
   --  project).

end Projects.Editor;
