-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
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

package Prj_API is

   type Project_Node_Array is array (Positive range <>) of Project_Node_Id;
   type Project_Node_Array_Access is access Project_Node_Array;
   type String_Id_Array is array (Positive range <>) of Types.String_Id;

   Ada_String : Types.String_Id;
   C_String   : Types.String_Id;
   Cpp_String : Types.String_Id;
   --  Strings used for the various languages supported by Glide

   function Get_String (Str : Types.String_Id) return String;
   --  This functional form returns the value of Str as a string without
   --  affecting the contents of either Name_Buffer or Name_Len.

   procedure Free is new Unchecked_Deallocation
     (Project_Node_Array, Project_Node_Array_Access);

   function Get_Project_View_From_Name
     (Name : Types.Name_Id) return Project_Id;
   --  Return the project whose name is Name.
   --  Note that this returns an entry in the processed project, not in the
   --  tree itself.

   function Get_Project_From_View (View : Project_Id) return Project_Node_Id;
   --  Converts from a project view to the associated node in the tree.

   function Get_Or_Create_Package
     (Project : Project_Node_Id; Pkg : String) return Project_Node_Id;
   --  Create (or get an existing) package in project.
   --
   --  ??? Should always create, since we can use a find_* function to get an
   --  existing one.

   procedure Add_Imported_Project
     (Project : Project_Node_Id; Imported_Project : Project_Node_Id);
   --  Add a new with_statement for Imported_Project.

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

   --------------------
   -- Creating nodes --
   --------------------

   function Create_Project (Name, Path : String) return Project_Node_Id;
   --  Create a new empty project and its declaration.
   --  The project is also registered, so that it can be retrieved from one of
   --  its view.

   procedure Rename
     (Root_Project : Project_Node_Id;
      Project      : Project_Node_Id;
      New_Name     : String);
   --  Rename Project to New_Name. All the nodes in the project tree starting
   --  at Root_Project, that reference Project, are also updated accordingly.
   --
   --  If there is already a project by that name in the project hierarchy,
   --  Renaming_Error_Name_Exists is raised

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
      Index_Name : Types.String_Id := Types.No_String;
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
      Attribute_Index    : Types.String_Id := Types.No_String);
   --  Remove all declarations for the attribute Attribute_Name in the current
   --  scenario. This effectively reverses to the default behavior for the
   --  attribute.

   function Create_Variable_Reference (Var : Project_Node_Id)
      return Project_Node_Id;
   --  Create and return a reference to the variable Var.
   --  Var must be a variable declaration

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

   function Clone_Node (Node : Project_Node_Id; Deep_Clone : Boolean := False)
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

   function String_As_Expression (Value : Types.String_Id)
      return Project_Node_Id;
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

   function Get_Environment (Var_Or_Attribute : Project_Node_Id)
      return Types.String_Id;
   --  Return the name of the environment variable associated with
   --  Var_Or_Attribute. No_String is returned in case there is no such
   --  variable.

   function Typed_Values_Count (Var_Or_Attribute : Project_Node_Id)
      return Positive;
   --  Return the number of possible values Var_Or_Attribute can take.
   --  For a typed variable, this is the number of items in the type
   --  declaration. For other variables or attributes, this is Positive'Last.

   procedure Get_Switches
     (Project          : Project_Id;
      In_Pkg           : String;
      File             : Types.Name_Id;
      Language         : Types.Name_Id;
      Value            : out Variable_Value;
      Is_Default_Value : out Boolean);
   --  Return the switches to use for a file in a given package (gnatmake,
   --  compiler, ...).
   --  Value is the list of switches to use for that variable.
   --  Is_Default_Value is set to true if file-specific switches were not
   --  specified, and Value is in fact the list of default switches defined
   --  at the package level.
   --  File can be No_Name if you want to find the default switches to use for
   --  all files in the project. In that case, this procedure returns the
   --  switches to use for Language

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
      Attribute_Index    : Types.String_Id := Types.No_String;
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
      Attribute_Index    : Types.String_Id := Types.No_String);
   --  Same as above, but for an attribute that contains a single value.

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

   Invalid_Value : exception;

   Renaming_Error_Name_Exists : exception;
   --  Raised when a project renaming could not be performed because the name
   --  was already used.

private
   type String_List_Iterator is record
      Current : Project_Node_Id;
      --  pointer to N_Literal_String or N_Expression
   end record;
end Prj_API;
