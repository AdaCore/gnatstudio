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
--  </description>

with Types;
with Prj;        use Prj;
with Prj.Tree;   use Prj.Tree;

package Prj_API is

   function Create_Project (Name, Path : String) return Project_Node_Id;
   --  Create a new empty project.
   --  You must have called "Project_Nodes.Set_Last (Empty_Node)" first.

   function Get_Or_Create_Declaration (Project : Project_Node_Id)
      return Project_Node_Id;
   --  Create (or get) the declaration associated with project
   --  This returns a N_Project_Declaration

   function Get_Or_Create_Variable
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Kind : Variable_Kind := List)
      return Project_Node_Id;
   --  Create a variable inside the package or the project Prj_Or_Pkg.
   --  This creates an internal untyped variable.

   function Get_Or_Create_Type
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String)
      return Project_Node_Id;
   --  Create type. By default, there is no possible value, you
   --  must add some with Add_Possible_Value.

   procedure Add_Possible_Value (Typ : Project_Node_Id; Choice : String);
   --  Add a new choice in the list of possible values for the type Typ.

   function Get_Or_Create_Typed_Variable
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Typ  : Project_Node_Id)
      return Project_Node_Id;
   --  Create a new variable of a specific type Typ.

   function Get_Or_Create_Attribute
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Index_Name : String := "";
      Kind : Variable_Kind := List)
      return Project_Node_Id;
   --  Create (or get an existing) variable by Name. This should be used
   --  only for the standard variables (for Var_Name use ...)
   --  The new variable will be added either to the project (global variable)
   --  or in one of its packages, both are specified in Prj_Or_Pkg.
   --  If Index_Name is not "", then if creates an attribute value for a
   --  specific index
   --
   --  If the variable is a list, it also creates the associated
   --  N_Literal_String_List node.

   function Get_Or_Create_Package
     (Project : Project_Node_Id; Pkg : String) return Project_Node_Id;
   --  Create (or get an existing) package in project.

   ---------------------
   -- Variable values --
   ---------------------

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

   procedure Get_Switches
     (Project          : Project_Id;
      In_Pkg           : String;
      File             : Types.Name_Id;
      Value            : out Variable_Value;
      Is_Default_Value : out Boolean);
   --  Return the switches to use for a file in a given package (gnatmake,
   --  compiler, ...).
   --  Value is the list of switches to use for that variable.
   --  Is_Default_Value is set to true if file-specific switches were not
   --  specified, and Value is in fact the list of default switches defined
   --  at the package level.

   function Length (Value : Variable_Value) return Integer;
   --  Return the number of elements in Value (1 if Value is of kind Single)

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

   type Variable_Decl_Array is array (Positive range <>) of Project_Node_Id;

   function Find_Scenario_Variables (Project : Project_Node_Id)
      return Variable_Decl_Array;
   --  Create and return an array that contains the declarations of all the
   --  scenario variables in Project and its packages.

   function Is_External_Variable (Var : Project_Node_Id) return Boolean;
   --  Return True if Var is a reference to an external variable


   Invalid_Value : exception;

private
   type String_List_Iterator is record
      Current : Project_Node_Id;
      --  pointer to N_Literal_String or N_Expression
   end record;
end Prj_API;
