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

--  <description>
--  This package contains a number of high-level functions to manipulate
--  a project tree (see Prj.Tree).
--
--  Note that normalized projects are handled and manipulated in the
--  Prj_Normalize package.
--
--  </description>

with Types;
with Unchecked_Deallocation;
with GNAT.OS_Lib;
with Prj;        use Prj;
with Prj.Tree;   use Prj.Tree;
with Src_Info;   use Src_Info;
with Output;
with Project_Browsers;
with Project_Hash;
with Basic_Types;

package Prj_API is

   type Project_Node_Array is array (Positive range <>) of Project_Node_Id;
   type Project_Node_Array_Access is access Project_Node_Array;
   type String_Id_Array is array (Positive range <>) of Types.String_Id;
   type String_Id_Array_Access is access String_Id_Array;
   type Project_Id_Array is array (Positive range <>) of Project_Id;
   type Project_Id_Array_Access is access Project_Id_Array;

   Ada_String : constant String := "ada";  --  See also Snames.Name_Ada
   C_String   : constant String := "c";    --  See also Snames.Name_C
   Cpp_String : constant String := "c++";  --  See also Snames.Name_Cpp
   --  Strings used for the various languages supported by Glide

   Any_Attribute : constant String := "@@";
   --  Special value for all the subprograms that take an Attribute_Index
   --  parameter. When this is used, no matching is done on the indexes.

   function Get_String (Str : Types.String_Id) return String;
   --  This functional form returns the value of Str as a string without
   --  affecting the contents of either Name_Buffer or Name_Len.

   procedure Free is new Unchecked_Deallocation
     (Project_Node_Array, Project_Node_Array_Access);
   procedure Free is new Unchecked_Deallocation
     (Project_Id_Array, Project_Id_Array_Access);
   procedure Free is new Unchecked_Deallocation
     (String_Id_Array, String_Id_Array_Access);

   function Get_Project_View_From_Name
     (Name : Types.Name_Id) return Project_Id;
   --  Return the project whose name is Name.
   --  Note that this returns an entry in the processed project, not in the
   --  tree itself.

   function Get_Project_From_Name
     (Name : Types.Name_Id) return Project_Node_Id;
   --  Return the project, from its name

   function Get_Project_View_From_Project
     (Project : Project_Node_Id) return Project_Id;
   --  Return the current project view

   function Get_Project_From_File
     (Root_Project_View : Prj.Project_Id; Source_Filename : String)
      return Prj.Project_Id;
   --  Return the project to which Source_Filename belongs.
   --  If the file was not found in any of the imported projects either,
   --  No_Project is returned.
   --  ??? Should we have a cache

   function Get_Project_From_Directory
     (Root_Project_View : Prj.Project_Id; Directory : String)
      return Prj.Project_Id;
   --  Get the project corresponding to Directory.
   --  Directory must be normalized by the caller.

   function Get_Project_From_View (View : Project_Id) return Project_Node_Id;
   --  Converts from a project view to the associated node in the tree.

   function Get_Or_Create_Package
     (Project : Project_Node_Id; Pkg : String) return Project_Node_Id;
   --  Create (or get an existing) package in project.
   --
   --  ??? Should always create, since we can use a find_* function to get an
   --  existing one.

   function Is_Direct_Source
     (Source_Filename : String; Of_Project : Prj.Project_Id) return Boolean;
   --  Return True if Source_Filename is a direct source of Of_Project.

   procedure Add_At_End
     (Parent                       : Project_Node_Id;
      Expr                         : Project_Node_Id;
      Add_Before_First_Case_Or_Pkg : Boolean := False);
   --  Add a new declarative item in the list in Parent.
   --  This new declarative item will contain Expr (unless Expr is already a
   --  declarative item, in which case it is added directly to the list).
   --  The new item is inserted at the end of the list, unless
   --  Add_Before_First_Case_Or_Pkg is True. In the latter case, it is added
   --  just before the first case construction is seen (in normalized project
   --  files, this corresponds to the end of the common section), or before the
   --  first package

   procedure Add_In_Front
     (Parent : Project_Node_Id;
      Node   : Project_Node_Id);
   --  Add Node at the begining of the list for Parent.
   --  Node can also be a N_Declarative_Item (or a list of them).

   function Enclose_In_Expression (Node : Project_Node_Id)
      return Project_Node_Id;
   --  Enclose the Node inside a N_Expression node, and return this expression.

   function Source_Dirs (Project : Prj.Project_Id) return String_Id_Array;
   --  Return the list of source directories for Prj, as an array

   procedure Save_Project
     (Project       : Prj.Tree.Project_Node_Id;
      Projects_Data : in out Project_Hash.Project_Htable.HTable;
      Recursive     : Boolean := False);
   --  Save the project to the corresponding file.
   --  If Recursive is True, all the imported projects are saved as well.

   function Include_Path
     (Project_View : Prj.Project_Id; Recursive : Boolean) return String;
   --  Return the source path for this project. If Recursive is True, it also
   --  includes the source path for all imported projects.

   function Object_Path
     (Project_View : Prj.Project_Id; Recursive : Boolean) return String;
   --  Return the object path for this project. If Recursive is True, it also
   --  includes the object path for all imported projects.

   function Get_Source_Files
     (Project_View : Prj.Project_Id;
      Recursive    : Boolean;
      Full_Path    : Boolean := True)
      return Basic_Types.String_Array_Access;
   --  Return the list of source files belonging to the project described in
   --  Handle. Only the direct sources of the project are currently returned,
   --  i.e. not those found in subprojects, unless Recursive is True.
   --  It is the caller's responsability to free the list.
   --  If Full_Path is true, then the file names will also include the
   --  directory.

   function Get_Source_Files
     (Project_View : Prj.Project_Id;
      Recursive : Boolean)
      return String_Id_Array;
   --  Same as above, but return the string ids.
   --  Only the short file names are returned.

   procedure Add_Foreign_Source_Files (Project_View : Prj.Project_Id);
   --  Add to Project_View the list of source files for languages other than
   --  Ada.

   function Executables_Directory (Project_View : Prj.Project_Id)
      return String;
   --  Return the directory that contains the executables generated for the
   --  main programs in Project_View. This is either Exec_Dir or Object_Dir.
   --  The returned string always ends with a directory separator.

   --------------------
   -- Creating nodes --
   --------------------

   function Create_Project (Name, Path : String) return Project_Node_Id;
   --  Create a new empty project and its declaration.
   --  The project is also registered, so that it can be retrieved from one of
   --  its view.

   function Project_Name (Project_View : Project_Id) return String;
   --  Return the name of the project.

   function Project_Path (Project_View : Project_Id) return String;
   --  Return the full path name to the project file

   function Create_Default_Project (Name, Path : String)
      return Project_Node_Id;
   --  Create a new project, whose source directory and object directory are is
   --  Path.

   procedure Convert_Paths_To_Absolute
     (Project : Project_Node_Id; Update_With_Statements : Boolean := False);
   --  Convert all the paths (source path, object path) to absolute
   --  directories in Project. This should always be used before moving a
   --  project file.
   --  This doesn't modify the with statements however if
   --  Update_With_Statements is False.

   procedure Rename_And_Move
     (Root_Project : Project_Node_Id;
      Project      : Project_Node_Id;
      New_Name     : String;
      New_Path     : String);
   --  Rename Project to New_Name. All the nodes in the project tree starting
   --  at Root_Project, that reference Project, are also updated accordingly.
   --  Also sets the path of the project file. Note that Path shouldn't include
   --  the file name, which is New_Name.
   --
   --  The paths internal to the project are not upgraded, and will remain
   --  relative paths if they were. Consider using Convert_Paths_To_Absolute if
   --  necessary.
   --
   --  If there is already a project by that name in the project hierarchy,
   --  Project_Error is raised

   function Create_Variable
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Kind : Variable_Kind := List)
      return Project_Node_Id;
   --  Create a variable inside the package or the project Prj_Or_Pkg.
   --  This creates an internal untyped variable.
   --  The new declaration is added at the end of the declarative item list for
   --  Prj_Or_Pkg (but before any package declaration).

   function Create_Type
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String)
      return Project_Node_Id;
   --  Create a new type. By default, there is no possible value, you
   --  must add some with Add_Possible_Value.
   --  The new declaration is added at the end of the declarative item list for
   --  Prj_Or_Pkg (but before any package declaration).

   procedure Add_Possible_Value
     (Typ : Project_Node_Id; Choice : Types.String_Id);
   --  Add a new choice in the list of possible values for the type Typ.
   --  If Choice is already available in Typ, then it is not added again.

   function Create_Typed_Variable
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Typ  : Project_Node_Id;
      Add_Before_First_Case_Or_Pkg : Boolean := False)
      return Project_Node_Id;
   --  Create a new variable of a specific type Typ.
   --  The declaration is appended at the end of the declarative items list in
   --  the project or the package, unless Add_Before_First_Case is True. In
   --  this case, it is put just before the first N_Case_Construction node is
   --  encountered (i.e the last position in the common section of a normalized
   --  project).

   function Create_Attribute
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Index_Name : String := "";
      Kind : Variable_Kind := List)
      return Project_Node_Id;
   --  Create a new attribute.
   --  The new declaration is added at the end of the declarative item list for
   --  Prj_Or_Pkg (but before any package declaration).
   --  If Index_Name is not "", then if creates an attribute value for a
   --  specific index
   --
   --  If the variable is a list, it also creates the associated
   --  N_Literal_String_List node.

   procedure Delete_Attribute
     (Project            : Project_Node_Id;
      Pkg_Name           : String := "";
      Scenario_Variables : Project_Node_Array;
      Attribute_Name     : String;
      Attribute_Index    : String := "");
   --  Remove all declarations for the attribute Attribute_Name in the current
   --  scenario. This effectively reverses to the default behavior for the
   --  attribute.
   --  If Attribute_Index is Any_Attribute, then this subprogram will not try
   --  to match the attribute.

   procedure Delete_Package (Project : Project_Node_Id; Pkg_Name : String);
   --  Remove package and all its declarative items from Project.

   function Create_Variable_Reference (Var : Project_Node_Id)
      return Project_Node_Id;
   --  Create and return a reference to the variable Var.
   --  Var must be a variable declaration

   function Create_Literal_String (Str : Types.String_Id)
      return Project_Node_Id;
   --  Create a literal string whose value is Str.

   -----------------------
   -- Imported projects --
   -----------------------

   procedure Add_Imported_Project
     (Project : Project_Node_Id;
      Imported_Project_Location : String;
      Report_Errors  : Output.Output_Proc := null);
   --  Add a new with_statement for Imported_Project.
   --  Errors while parsing the project file are sent to Report_Errors.

   procedure Remove_Imported_Project
     (Project : Project_Node_Id; Imported_Project : String);
   --  Remove a dependency from Project.
   --  If Imported_Project is not already a dependency, then this subprogram
   --  does nothing.

   type Imported_Project_Iterator (<>) is private;

   function Start (Root_Project : Project_Node_Id; Recursive : Boolean := True)
      return Imported_Project_Iterator;
   --  Initialize the iterator to start at Root_Project.
   --  It will process Root_Project and all its subprojects, recursively, but
   --  without processing the same project twice.
   --  The project nodes are returned sorted topologically (ie first the
   --  projects that don't depend on anything, then their parents, and so on
   --  until the root project).
   --
   --  If Recursive is False, then the only project ever returned is
   --  Root_Project. This is provided only to simplify the caller's code
   --
   --  ??? Should also process modified projects

   procedure Reset (Iterator : in out Imported_Project_Iterator);
   --  Reset the iterator to point to the first project node in the list

   function Current
     (Iterator : Imported_Project_Iterator) return Project_Id;
   function Current
     (Iterator : Imported_Project_Iterator) return Project_Node_Id;
   --  Return the project currently pointed to by the iterator.
   --  Empty_Node is returned if there are no more projects to process.

   procedure Next (Iterator : in out Imported_Project_Iterator);
   --  Move to the next imported project.

   function Find_All_Projects_Importing
     (Root_Project : Project_Node_Id; Project : Project_Id)
      return Project_Id_Array;
   --  Return the list of all the projects that import Project, either directly
   --  or indirectly.
   --  If Project is No_Project, the resulting array contains all the project
   --  in the hierarchy.

   -------------------
   -- Finding nodes --
   -------------------

   function Find_Type_Declaration
     (Project : Project_Node_Id; Name : Types.Name_Id)
      return Project_Node_Id;
   --  Return the declaration of the type whose name is Name.

   function Find_Package_Declaration
     (Project : Project_Node_Id; Name : Types.Name_Id)
     return Project_Node_Id;
   --  Return the package whose name is Name, or Empty_Node if there is none

   function Find_Project_In_Hierarchy
     (Root_Project : Project_Node_Id; Name : Types.Name_Id)
      return Project_Node_Id;
   --  Find in the project tree starting at Root_Project a subproject called
   --  Name.
   --  This is different from using directly Prj.Tree.Projects_Htable, since we
   --  only look for the project in the hierarchy of Root_Project.

   function Find_Scenario_Variable
     (Project : Project_Node_Id; External_Name : Types.String_Id)
      return Project_Node_Id;
   --  Return the declaration of the scenario variable associated with
   --  the external variable External_Name.
   --  In normalized projects, there should be only such variable.

   ------------------
   -- Node cloning --
   ------------------

   function Clone_Node
     (Node       : Project_Node_Id;
      Deep_Clone : Boolean := False) return Project_Node_Id;
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
     (Project : Project_Node_Id; Pkg : Project_Node_Id := Empty_Node);
   --  Post-process a project, and make sure that all the internal lists for
   --  variables, packages, types,... are properly chained up, and that all the
   --  variables reference a type declaration in Project (and not in some other
   --  project), ...
   --  On exit, Project is fully independent of whatever old project is was
   --  created from.

   ---------------------
   -- Variable values --
   ---------------------

   function String_As_Expression
     (Value : Types.String_Id) return Project_Node_Id;
   --  Return an N_Expression node that represents the static string Value.
   --  ??? Could be implemented in terms of Concatenate.

   procedure Append_To_List (Var : Project_Node_Id; Value : String);
   --  Append a simple string to Var.
   --  Var must be a list, and contain a N_Literal_String_List, as created
   --  by Get_Or_Create_Variable above

   procedure Set_Value (Var : Project_Node_Id; Value : String);
   --  Set the value for a variable. Var mustn't be a list.
   --  If Var is a typed variable, the value is checked against the list
   --  of possible values (Invalid_Value raised).
   --  For variables defined as references to environment variables, this
   --  changes the default value.

   procedure Set_Value_As_External
     (Var : Project_Node_Id; External_Name : String; Default : String := "");
   --  Set the value of the variable as a reference to the environment variable
   --  External_Name. Var must be a single value, not a string.
   --  If Var is a typed variable, the default value is checked against the
   --  list of possible values (Invalid_Value raised if not).

   function Get_Environment
     (Var_Or_Attribute : Project_Node_Id) return Types.String_Id;
   --  Return the name of the environment variable associated with
   --  Var_Or_Attribute. No_String is returned in case there is no such
   --  variable.

   function Typed_Values_Count
     (Var_Or_Attribute : Project_Node_Id) return Positive;
   --  Return the number of possible values Var_Or_Attribute can take.
   --  For a typed variable, this is the number of items in the type
   --  declaration. For other variables or attributes, this is Positive'Last.

   procedure Get_Switches
     (Project          : Project_Id;
      In_Pkg           : String;
      File             : String;
      Language         : Types.Name_Id;
      Value            : out Variable_Value;
      Is_Default_Value : out Boolean);
   --  Return the switches to use for a file in a given package (gnatmake,
   --  compiler, ...).
   --  Value is the list of switches to use for that variable.
   --  Is_Default_Value is set to true if file-specific switches were not
   --  specified, and Value is in fact the list of default switches defined
   --  at the package level.
   --  File can be the empty string if you want to find the default switches to
   --  use for all files in the project. In that case, this procedure returns
   --  the switches to use for Language

   function Length (Value : Variable_Value) return Integer;
   --  Return the number of elements in Value (1 if Value is of kind Single)

   function To_String (Value : Variable_Value) return String;
   --  Convert a variable value to a string suitable for insertion in the list.

   function To_Argument_List (Value : Variable_Value)
      return GNAT.OS_Lib.Argument_List;
   --  Convert a variable value to a list of arguments.

   procedure Update_Attribute_Value_In_Scenario
     (Project            : Project_Node_Id;
      Pkg_Name           : String := "";
      Scenario_Variables : Project_Node_Array;
      Attribute_Name     : String := "";
      Values             : GNAT.OS_Lib.Argument_List;
      Attribute_Index    : String := "";
      Prepend            : Boolean := False);
   --  Update the value of the attribute Attribute_Name in Project/Pkg_Name.
   --  If Pkg_Name is the empty string "", then the value is updated at the
   --  top-level of the project.
   --  List is the list of new values for the variable. If Prepend is True,
   --  these values are the only values for the variable, and they override any
   --  other value that was there before. If Prepend is False, the values in
   --  List are prepended to the current value of the attribute.
   --  Attribute_Index is the associative array index for the attribute (for
   --  instance the file name when modifying the switches).

   procedure Update_Attribute_Value_In_Scenario
     (Project            : Project_Node_Id;
      Pkg_Name           : String := "";
      Scenario_Variables : Project_Node_Array;
      Attribute_Name     : String := "";
      Value              : String;
      Attribute_Index    : String := "");
   --  Same as above, but for an attribute that contains a single value.

   Ide_Package                : constant String := "ide";
   Gnatlist_Attribute         : constant String := "gnatlist";
   Compiler_Command_Attribute : constant String := "compiler_command";
   Debugger_Command_Attribute : constant String := "debugger_command";
   Remote_Host_Attribute      : constant String := "remote_host";
   Main_Attribute             : constant String := "main";
   Languages_Attribute        : constant String := "languages";
   Exec_Dir_Attribute         : constant String := "exec_dir";

   function Get_Attribute_Value
     (Project_View   : Project_Id;
      Attribute_Name : String;
      Package_Name   : String := "";
      Default        : String := "";
      Index          : String := "") return String;
   --  Return the value for a single-string attribute.
   --  Default is returned if the attribute wasn't set by the user and
   --  has no default value.

   function Get_Attribute_Value
     (Project_View   : Project_Id;
      Attribute_Name : String;
      Package_Name   : String := "";
      Index          : String := "") return GNAT.OS_Lib.Argument_List;
   --  Same as above, but for an attribute whose value is a list. An empty
   --  array is returned if the attribute isn't defined.
   --  It is the responsability of the called to free the memory.

   -----------
   -- Files --
   -----------

   function Get_Unit_Part_From_Filename
     (Filename : String;
      Project  : Prj.Project_Id)
      return Src_Info.Unit_Part;
   --  Return the type of File. As opposed to Src_Info.Get_Unit_Part, this one
   --  doesn't require that the LI file has been parsed before.
   --  This function doesn't assume any knowledge of the language, and will
   --  check in all the languages known to the project.
   --  Note also that it doesn't take into account the exceptions to the naming
   --  schemes, just the file suffixes.
   --  Unit_Separate is returned if the file is neither a spec nor a body.

   function Delete_File_Suffix
     (Filename : String;
      Project  : Prj.Project_Id)
      return Natural;
   --  Return the last index in Filename before the beginning of the file
   --  suffix. Suffixes are searched independently from the language.
   --  If not matching suffix is found in project, the returned value will
   --  simply be Filename'Last.

   function Get_Language_Of
     (Project : Project_Id; Source_Filename : String) return Types.Name_Id;
   --  Return the name of the language used for Source_Filename.
   --  This is based on the naming scheme given in Project.

   -----------------
   -- Expressions --
   -----------------

   procedure Concatenate
     (Expr : in out Project_Node_Id; Node : Project_Node_Id);
   --  Concatenate Node to the last term in the N_Expression Expr.
   --
   --  If Expr is Empty_Node, then a new expression is created for Node.
   --  if Expr doesn't contain any term, Node is inserted as the first and
   --  only term.
   --
   --  If Node is a N_Term, it is added as is to the expression.
   --  If Node is a N_Expression, this is an error
   --  Otherwise, Node is first encapsulated into a N_Term

   procedure Concatenate_List
     (Expr : in out Project_Node_Id; Expr2 : Project_Node_Id);
   --  Concatenate two N_Expression nodes into a list.
   --  if Expr already contains a list, Expr2 is appended at the end.
   --  If Expr is Empty_Node, a new list is created.
   --  Expr2 mustn't be a N_Literal_String_List expression itself.

   -----------
   -- Lists --
   -----------

   type String_List_Iterator is private;

   function Done (Iter : String_List_Iterator) return Boolean;
   --  Return True if Iter is past the end of the list of strings

   function Next (Iter : String_List_Iterator) return String_List_Iterator;
   --  Return the next item in the list

   function Data (Iter : String_List_Iterator) return Project_Node_Id;
   function Data (Iter : String_List_Iterator) return Types.String_Id;
   --  Return the value pointed to by Iter.
   --  This could be either a N_String_Literal or a N_Expression node in the
   --  first case.
   --  The second case only works if Iter points to N_String_Literal.

   function Type_Values (Var_Or_Type : Project_Node_Id)
      return String_List_Iterator;
   --  Return an iterator over the list of possible values for the
   --  N_Typed_Variable_Declaration or N_String_Type_Declaration Var.

   function Value_Of (Var : Project_Node_Id) return String_List_Iterator;
   --  Return an iterator over the value of the variable.
   --  If the variable is a single element, the list will have only one item.
   --  If the variable is a list, this returns a pointer to the first
   --  expression in the list.
   --  If the variable is defined as an external reference, this return a
   --  pointer to the default value.

   ---------------
   -- Variables --
   ---------------

   function Find_Scenario_Variables
     (Project : Project_Node_Id;
      Parse_Imported : Boolean := True) return Project_Node_Array;
   --  Create and return an array that contains the declarations of all the
   --  scenario variables in Project and its packages. It also includes
   --  variables from imported projects if Parse_Imported is True.
   --  Two candidates are considered the same if they reference the same
   --  environment variable. The reason is that they might not have the same
   --  name internally in imported projects, however, they will always have the
   --  same value.
   --  We not check the list of possible values for efficiency reasons.
   --  ??? Probably we should report an error if they'don't have the same type.

   function Is_External_Variable (Var : Project_Node_Id) return Boolean;
   --  Return True if Var is a reference to an external variable

   function External_Reference_Of (Var : Project_Node_Id)
      return Types.String_Id;
   --  Returns the name of the external variable referenced by Var.
   --  No_String is returned if Var doesn't reference an external variable.
   --
   --  ??? Name conflicts with Prj.Tree.Name_Reference

   function External_Default (Var : Project_Node_Id)
      return Project_Node_Id;
   --  Return a pointer to the expression that defines the default of an
   --  external reference. Var must be a variable declaration.
   --  Empty_Node is returned if there is no default value

   function External_Variable_Name
     (Current_Project : Project_Node_Id; Ref : Project_Node_Id)
      return Types.String_Id;
   --  Return the name of the external variable referenced by Ref.
   --  The declaration of the variable is looked in Current_Project, unless
   --  another project is specified in the variable reference
   --
   --  Ref should be a N_Variable_Reference.
   --
   --  ??? Can this be merged with External_Reference_Of

   procedure Add_Scenario_Variable_Values
     (Root_Project           : Project_Node_Id;
      External_Variable_Name : Types.String_Id;
      Values                 : String_Id_Array);
   --  Add some values to the list of possible values for the scenario
   --  variables associated with External_Variable_Name. The changes are done
   --  recursively in Root_Project and all its imported projects.

   procedure Rename_External_Variable
     (Root_Project : Project_Node_Id;
      Old_Name     : String;
      New_Name     : Types.String_Id);
   --  Rename all references to Old_Name in Root_Project and its imported
   --  projects.
   --  Old_Name is given as a string so that we don't need to allocate a new
   --  string_id.

   procedure Set_Default_Value_For_External_Variable
     (Root_Project      : Project_Node_Id;
      Ext_Variable_Name : String;
      Default           : Types.String_Id);
   --  Change the default value for all the scenario variables based on
   --  Ext_Variable_Name.

   procedure Delete_External_Variable
     (Root_Project      : Project_Node_Id;
      Ext_Variable_Name : String;
      Keep_Choice       : String;
      Delete_Direct_References : Boolean := True);
   --  Remove all scenario variables that reference External_Variable_Name.
   --  All the case constructions where this variable occur are replaced by
   --  the case item corresponding to Keep_Choice.
   --  If Delete_Direct_References is True, then all direct references (ie
   --  external() statements in the project file) to Ext_Variable_Name are also
   --  removed, in addition to the scenario variables that reference it.

   procedure Rename_Value_For_External_Variable
     (Root_Project      : Project_Node_Id;
      Ext_Variable_Name : String;
      Old_Value_Name    : String;
      New_Value_Name    : Types.String_Id);
   --  Rename one of the choices in the list of possible values for the
   --  scenario variables asociated with Ext_Variable_Name. This also changes
   --  the default value for external references.

   procedure Remove_Value
     (Root_Project      : Project_Node_Id;
      Ext_Variable_Name : String;
      Value_Name        : String);
   --  Remove Value_Name from the list of possible values for the scenario
   --  variables that refer to Ext_Variable_Name. If this is the last possible
   --  value, then the result is the same as calling Delete_External_Variable.
   --  Direct N_External_Value nodes that use Value_Name as the default will
   --  no longer have a default.

   Invalid_Value : exception;

   Project_Error   : exception;
   Project_Warning : exception;
   --  Two general exceptions that are raised when a subprogram could not
   --  perform its duty. A warning means that nothing was done, but is not
   --  critical.

private
   type String_List_Iterator is record
      Current : Project_Node_Id;
      --  pointer to N_Literal_String or N_Expression
   end record;

   type Imported_Project_Iterator (Number : Natural) is record
      List    : Project_Browsers.Name_Id_Array (1 .. Number);
      Current : Integer;
   end record;
end Prj_API;
