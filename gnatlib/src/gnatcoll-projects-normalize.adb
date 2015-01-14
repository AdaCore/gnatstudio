------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2002-2015, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Maps;          use Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;
with GNAT.Case_Util;            use GNAT.Case_Util;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;        use GNATCOLL.VFS_Utils;
with Namet;                     use Namet;
with Output;                    use Output;
pragma Warnings (Off);
pragma Warnings (Off, "*license of withed unit*");
with Prj.Com;                   use Prj.Com;
with Prj.Tree;                  use Prj, Prj.Tree;
pragma Warnings (On, "*license of withed unit*");
pragma Warnings (On);
with Snames;
with Types;                     use Types;
with GNAT.Strings;              use GNAT.Strings;

package body GNATCOLL.Projects.Normalize is

   Me : constant Trace_Handle := Create ("Prj_Normalize");
   Exception_Handle : constant Trace_Handle :=
     Create ("UNEXPECTED_EXCEPTION", Default => On);

   type External_Variable_Value is record
      Variable_Type  : Prj.Tree.Project_Node_Id;
      Variable_Name  : Namet.Name_Id;
      Variable_Value : Namet.Name_Id;
      Negated        : Boolean := False;
   end record;
   --  Description for one possible value of an external variable. Through an
   --  array of such values, it is possible to reference multiple case items in
   --  a case construction of a normalized project.
   --  If Negated is True, then Variable_Name must not be Variable_Value for
   --  the case item to match.
   --  See the example in the description of External_Variable_Value_Array.

   type External_Variable_Value_Array is array (Natural range <>) of
     External_Variable_Value;
   type External_Variable_Value_Array_Access is access
     External_Variable_Value_Array;
   --  Description for a case item (or a set of them).
   --  The same variable name can appear multiple times in the array. In that
   --  case, the value of the variable must match any of the choises for the
   --  case item to match.
   --  If a variable name exists in the case construction but not in the array,
   --  then the variable can have any value.
   --
   --  As an example, given the following case construction:
   --      case V1 is
   --         when Val1 | Val2 =>
   --            case V2 is
   --                when Val2_1 => stmt1;
   --                when others => stmt2;
   --            end case;
   --         when others =>
   --            case V3 is
   --               when V3_1 => stmt3;
   --               when V3_2 => stmt4;
   --            end V3;
   --      end case;
   --
   --  Then stmt1 can be reach with an External_Variable_Value_Array equal to:
   --      ((V1, Val1), (V1, Val2), (V2, Val2_1))
   --  stmt2 can be reached with
   --      ((V1, Val1), (V1, Val2), (V2, Val2_1, True))
   --  stmt3 can be reached with
   --      ((V1, Val1, True), (V1, Val2, True), (V3, V3_1))
   --  Both stmt3 and stmt4 can be reached at the same time with
   --      ((V1, Val1, True), (V1, Val2, True))
   --
   --  If there was at least one non-negated element in the array, then at
   --  least one of the non-negated elements must be matched

   procedure Free is new Ada.Unchecked_Deallocation
     (External_Variable_Value_Array, External_Variable_Value_Array_Access);

   type Project_Node_Array is array (Positive range <>) of Project_Node_Id;
   type Project_Node_Array_Access is access Project_Node_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Project_Node_Array, Project_Node_Array_Access);

   No_Value : constant External_Variable_Value :=
     (Variable_Type  => Prj.Tree.Empty_Node,
      Variable_Name  => Namet.No_Name,
      Variable_Value => Namet.No_Name,
      Negated        => False);

   All_Case_Items : constant External_Variable_Value_Array (1 .. 0) :=
     (others => No_Value);

   function Clone_Project
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Node_Id) return Project_Node_Id;
   --  Return a duplicate of Project and its declarations. We do not duplicate
   --  declarative items.
   --  The new project is not independent of the old one, since most of the
   --  nodes are shared between the two for efficiency reasons.

   procedure Add_Value
     (To   : in out External_Variable_Value_Array_Access;
      Last : in out Natural;
      V    : External_Variable_Value);
   --  Add V to the array To. To is reallocated as necessary.
   --  Last is the index of the last item that was set in To.

   function External_Variable_Name
     (Tree            : Project_Node_Tree_Ref;
      Current_Project : Project_Node_Id;
      Ref             : Project_Node_Id) return Name_Id;
   --  Return the name of the external variable referenced by Ref.
   --  The declaration of the variable is looked in Current_Project, unless
   --  another project is specified in the variable reference
   --
   --  Ref should be a N_Variable_Reference.

   function Values_Matches
     (Tree      : Project_Node_Tree_Ref;
      Var_Name  : Name_Id;
      Case_Item : Project_Node_Id;
      Values    : External_Variable_Value_Array) return Boolean;
   --  Return True if (Var_Name, Var_Value) is valid with regards to Values

   procedure Set_Uniq_Type_Name
     (Tree     : Project_Node_Tree_Ref;
      Project  : Project_Node_Id;
      Var_Type : Project_Node_Id);
   --  Set the name for the N_String_Type_Declaration Var_Type, so that it is
   --  uniq in the project.
   --  Var_Type shouldn't have been added to the project yet.

   function Find_Node_By_Name
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Node_Id;
      Kind    : Project_Node_Kind;
      Name    : Name_Id) return Project_Node_Id;
   --  Find a node given its name

   type Matching_Item_Callback is access
     procedure (Item : Prj.Tree.Project_Node_Id);
   --  A callback function called for each case item that matches a specific
   --  set of values

   procedure For_Each_Matching_Case_Item
     (Tree    : Project_Node_Tree_Ref;
      Project : Prj.Tree.Project_Node_Id;
      Pkg     : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node;
      Case_Construct : in out Prj.Tree.Project_Node_Id;
      Values  : External_Variable_Value_Array;
      Action  : Matching_Item_Callback);
   --  Execute Action for all the case items in Project or Pkg that match
   --  Values.
   --  If no case item exists, Action is still called once on Project or Pkg
   --  itself.
   --  If Pkg is not the Empty_Node, then this subprogram only works on that
   --  package. However, the project must still be specified so that the
   --  declaration of the variables can be found.
   --  If a variable is referenced in Values, but doesn't have an associated
   --  case construction, a new case construction is added at the lowest level.
   --
   --  Case_Construct is a pointer to the case statement inside Pkg. It should
   --  be the result of Find_Or_Create_Case_Statement.
   --
   --  Important: Project must have been normalized first, and it is
   --  recommended to call Check_Case_Construction before
   --
   --  Action can be null, in which case a side effect of this subprogram is to
   --  create the case constructs for the variables referenced in Values that
   --  do not already have a case construct.

   function Create_Case_Construction
     (Tree          : Project_Node_Tree_Ref;
      Project       : Project_Node_Id;
      External_Name : Name_Id;
      Var_Type      : Project_Node_Id) return Project_Node_Id;
   --  Return a N_Case_Construction for the external variable Name.
   --  The declaration for the variable itself is added at the beginning of the
   --  project if no variable was found that already referenced Name.

   procedure Add_Case_Item
     (Tree      : Prj.Tree.Project_Node_Tree_Ref;
      Case_Node : Prj.Tree.Project_Node_Id;
      Choice    : Namet.Name_Id);
   --  Create a new case item in case_node (which is associated with a
   --  "case var is" statement

   procedure Add_To_Case_Items
     (Tree              : Project_Node_Tree_Ref;
      Case_Construction : Project_Node_Id;
      Decl_List         : Project_Node_Id);
   --  Copy all the declarative items from Decl_List into each of the case
   --  items of Case_Construction (at the beginning of each case item)

   procedure Set_Expression
     (Tree             : Project_Node_Tree_Ref;
      Var_Or_Attribute : Project_Node_Id;
      Expr             : Project_Node_Id);
   --  Set Var as the expression to use for the value of Var. This
   --  properly handles standard variables and variables defined through
   --  references to external environment variables.

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

   function Find_Package_Declaration
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Node_Id; Name : Namet.Name_Id) return Project_Node_Id;
   --  Return the package whose name is Name, or Empty_Node if there is none

   function Find_Project_Of_Package
     (Data     : Project_Tree_Data_Access;
      Project  : Project_Type;
      Pkg_Name : String) return Project_Type;
   --  Return the id of the project that contains Pkg_Name. It will be
   --  different from Project if the package declaration is a renaming of
   --  another package.

   function Find_Case_Statement
     (Tree    : Project_Node_Tree_Ref;
      Project : Prj.Tree.Project_Node_Id;
      Pkg     : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node)
      return Project_Node_Id;
   --  Return the first case statement in Project/Pkg.
   --  In a normalized project, this returns the only case statement that
   --  exists in a package or project.

   procedure Move_From_Common_To_Case_Construct
     (Tree               : Project_Node_Tree_Ref;
      Project            : Project_Node_Id;
      Pkg                : Project_Node_Id;
      Case_Construct     : in out Project_Node_Id;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute_Name     : Namet.Name_Id;
      Attribute_Index    : Namet.Name_Id := No_Name);
   --  Move any declaration for the attribute from the common part of the
   --  project into each branch of the nested case construct. Nothing is done
   --  if there is no such declaration.

   procedure Add_Node_To_List
     (To   : in out Project_Node_Array_Access;
      Last : in out Natural;
      Node : Project_Node_Id);
   --  Add a new node into the list of nodes To.
   --  To is resized as needed

   procedure For_Each_Scenario_Case_Item
     (Tree    : Project_Node_Tree_Ref;
      Project            : Prj.Tree.Project_Node_Id;
      Pkg                : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node;
      Case_Construct     : in out Prj.Tree.Project_Node_Id;
      Scenario_Variables : GNATCOLL.Projects.Scenario_Variable_Array;
      Action             : Matching_Item_Callback);
   --  Same above, but it works directly for the current scenario (ie its gets
   --  the value of the variables directly from the environment). For
   --  efficiency, the list of scenario variables has to be provided as a
   --  parameter.
   --  Important: Project must have been normalized first, and it is
   --  recommended to call Check_Case_Construction before
   --
   --  Case_Construct is a pointer to the case statement inside Pkg. It should
   --  be the result of Find_Or_Create_Case_Statement.
   --
   --  Action can be null, in which case a side effect of this subprogram is to
   --  create the nested case for all the scenario variables. All case items
   --  are empty.

   type Node_Callback is access procedure (Node : Project_Node_Id);

   procedure For_Each_Directory_Node
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Type;
      Action  : Node_Callback);
   --  For each node that deals with a procedure, calls Action

   function Attribute_Matches
     (Tree            : Project_Node_Tree_Ref;
      Node            : Project_Node_Id;
      Attribute_Name  : Name_Id;
      Attribute_Index : Name_Id) return Boolean;
   --  Return True if Node is an attribute declaration matching Attribute_Name
   --  and Attribute_Index.
   --  If Attribute_Index is Any_Attribute, no matching is done on the index.

   function Find_Last_Declaration_Of
     (Tree       : Project_Node_Tree_Ref;
      Parent     : Project_Node_Id;
      Attr_Name  : Namet.Name_Id;
      Attr_Index : Namet.Name_Id := No_Name) return Project_Node_Id;
   --  Find the last declaration for the attribute Attr_Name, in the
   --  declarative list contained in Parent.
   --  The returned value is the last such declaration, or Empty_Node if there
   --  was none.
   --  This returns the current item of the declarative item

   procedure Remove_Attribute_Declarations
     (Tree            : Project_Node_Tree_Ref;
      Parent          : Project_Node_Id;
      Attribute_Name  : Name_Id;
      Attribute_Index : Name_Id);
   --  Remove all declarations for Attribute_Name in the declarative item list
   --  of Parent.
   --  If Attribute_Index is Any_Attribute, no matching is done on the index.

   type Set_Attribute_Callback is access procedure
     (Tree_Node      : Project_Node_Tree_Ref;
      Project        : Project_Type;
      Attribute_Name : Name_Id;
      Index_Id       : Name_Id;
      Previous_Decl  : Project_Node_Id;
      Case_Item      : Project_Node_Id);

   procedure Internal_Set_Attribute
     (Tree      : Project_Tree_Data_Access;
      Project   : Project_Type;
      Attribute : String;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "";
      Callback  : Set_Attribute_Callback);
   --  Internal version of Set_Attribute

   function String_As_Expression
     (Value : Name_Id; Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Return an N_Expression node that represents the static string Value.
   --  ??? Could be implemented in terms of Concatenate.

   function Get_All_Possible_Values
     (Tree     : Project_Node_Tree_Ref;
      Variable : Project_Node_Id) return Name_Id_Array;
   --  Return the list of all possible values for Variable

   procedure Set_With_Clause_Path
     (Tree                      : Project_Node_Tree_Ref;
      With_Clause               : Project_Node_Id;
      Imported_Project_Location : Virtual_File;
      Imported_Project          : Project_Node_Id;
      Importing_Project         : Project_Node_Id;
      Use_Relative_Path         : Boolean;
      Use_Base_Name             : Boolean;
      Limited_With              : Boolean := False);
   --  Set the attributes of the with_clause (imported project node, imported
   --  project path,....)

   procedure Remove_Node
     (Tree   : Project_Node_Tree_Ref;
      Parent : Project_Node_Id;
      Node   : Project_Node_Id);
   --  Remove Node from the declaration list in Parent.
   --  This doesn't search recursively inside nested packages, case
   --  constructions, ...

   procedure Remove_Variable_Declaration
     (Tree               : Project_Node_Tree_Ref;
      Project_Or_Package : Project_Node_Id;
      Declaration        : Project_Node_Id);
   --  Remove the variable declaration from the list of variables in
   --  Project_Or_Package.

   -----------------------------------
   -- For_Each_Environment_Variable --
   -----------------------------------

   procedure For_Each_Environment_Variable
     (Tree              : Project_Node_Tree_Ref;
      Root_Project      : Project_Type;
      Ext_Variable_Name : Name_Id;
      Specific_Choice   : Name_Id;
      Action            : Environment_Variable_Callback)
   is
      Variable_Nodes      : Project_Node_Array_Access :=
                              new Project_Node_Array (1 .. 100);
      Variable_Nodes_Last : Natural := Variable_Nodes'First - 1;
      --  List of all the variables that reference Ext_Variable_Name
      --  in the current project.

      procedure Process_Expression
        (Project : Project_Node_Id; Expression : Project_Node_Id);
      --  Delete all references to the variable in Expr

      procedure Recurse_In_Project
        (Project : Project_Node_Id; Pkg_Or_Case_Item : Project_Node_Id);
      --  Delete the scenario variable in a specific part of Root_Project
      --  (either the project itself, if Pkg_Or_Case_Item is Empty_Node,
      --  or a package or a case item.

      function Is_Reference_To_Ext
        (Node : Project_Node_Id) return Boolean;
      --  Return True if Node is a reference (N_External_Value or
      --  N_Variable_Reference) to the external variable Ext_Variable_Name.
      --  Var_Declarations should contain the list of
      --  N_Typed_Variable_Declaration nodes that refer to Ext_Variable_Name.

      -------------------------
      -- Is_Reference_To_Ext --
      -------------------------

      function Is_Reference_To_Ext
        (Node : Project_Node_Id) return Boolean is
      begin
         case Kind_Of (Node, Tree) is
            when N_External_Value =>
               return String_Value_Of
                 (Prj.Tree.External_Reference_Of (Node, Tree), Tree) =
                 Ext_Variable_Name;

            when N_Variable_Reference =>
               for J in Variable_Nodes'First .. Variable_Nodes_Last loop
                  if Prj.Tree.Name_Of (Node, Tree) =
                    Prj.Tree.Name_Of (Variable_Nodes (J), Tree)
                  then
                     return True;
                  end if;
               end loop;

               return False;

            when others =>
               return False;
         end case;
      end Is_Reference_To_Ext;

      ------------------------
      -- Process_Expression --
      ------------------------

      procedure Process_Expression
        (Project : Project_Node_Id; Expression : Project_Node_Id)
      is
         Expr : Project_Node_Id := Expression;
         Term : Project_Node_Id;
      begin
         while Expr /= Empty_Node loop
            Term := First_Term (Expr, Tree);
            while Term /= Empty_Node loop
               case Kind_Of (Current_Term (Term, Tree), Tree) is

                  --  Handles ("-g" & A, "-O2" & external ("A"))
                  when N_Literal_String_List =>
                     Process_Expression
                       (Project,
                        First_Expression_In_List
                          (Current_Term (Term, Tree), Tree));

                  --  Handles "-g" & external ("A")
                  --  Replace A by the constant string representing its value
                  when N_External_Value =>
                     if Is_Reference_To_Ext (Current_Term (Term, Tree)) then
                        Action (Project, Term, Current_Term (Term, Tree),
                                Empty_Node);
                     end if;

                  --  Handles "-g" & Var
                  --  Where Var is a reference to the external variable
                  when N_Variable_Reference =>
                     if Is_Reference_To_Ext (Current_Term (Term, Tree)) then
                        Action (Project, Term, Current_Term (Term, Tree),
                                Empty_Node);
                     end if;

                  when others =>
                     null;
               end case;

               Term := Next_Term (Term, Tree);
            end loop;

            Expr := Next_Expression_In_List (Expr, Tree);
         end loop;
      end Process_Expression;

      ------------------------
      -- Recurse_In_Project --
      ------------------------

      procedure Recurse_In_Project
        (Project : Project_Node_Id; Pkg_Or_Case_Item : Project_Node_Id)
      is
         Decl, Current, Case_Item, Choice : Project_Node_Id;
         Match : Boolean;
      begin
         if Pkg_Or_Case_Item /= Empty_Node then
            Decl := First_Declarative_Item_Of (Pkg_Or_Case_Item, Tree);
         else
            Decl := First_Declarative_Item_Of
              (Project_Declaration_Of (Project, Tree), Tree);
         end if;

         while Decl /= Empty_Node loop
            Current := Current_Item_Node (Decl, Tree);
            case Kind_Of (Current, Tree) is
               when N_Typed_Variable_Declaration =>

                  if Is_External_Variable (Current, Tree)
                    and then
                      External_Reference_Of (Current, Tree) = Ext_Variable_Name
                  then
                     Add_Node_To_List
                       (Variable_Nodes, Variable_Nodes_Last, Current);

                     Action
                       (Project, Empty_Node, String_Type_Of (Current, Tree),
                        Empty_Node);
                     Action (Project, Decl, Current, Empty_Node);
                  end if;

                  Process_Expression (Project, Expression_Of (Current, Tree));

               when N_Case_Construction =>
                  if Is_Reference_To_Ext
                    (Case_Variable_Reference_Of (Current, Tree))
                  then
                     Case_Item := First_Case_Item_Of (Current, Tree);

                     while Case_Item /= Empty_Node loop
                        Choice := First_Choice_Of (Case_Item, Tree);

                        --  If we have reached Empty_Node and nothing matched
                        --  before, then that is the case item we want to keep.
                        --  This corresponds to "when others"
                        Match := Choice = Empty_Node
                          or else Specific_Choice = No_Name;

                        if not Match then
                           while Choice /= Empty_Node loop
                              if String_Value_Of (Choice, Tree) =
                                Specific_Choice
                              then
                                 Match := True;
                                 exit;
                              end if;

                              Choice := Next_Literal_String (Choice, Tree);
                           end loop;
                        end if;

                        if Match then
                           Action (Project, Decl, Case_Item, Choice);
                        end if;

                        Recurse_In_Project (Project, Case_Item);
                        Case_Item := Next_Case_Item (Case_Item, Tree);
                     end loop;

                  else
                     Case_Item := First_Case_Item_Of (Current, Tree);
                     while Case_Item /= Empty_Node loop
                        Recurse_In_Project (Project, Case_Item);
                        Case_Item := Next_Case_Item (Case_Item, Tree);
                     end loop;
                  end if;

               when N_Package_Declaration =>
                  Recurse_In_Project (Project, Current);

               when N_Variable_Declaration
                 |  N_Attribute_Declaration =>
                  Process_Expression (Project, Expression_Of (Current, Tree));

               when others =>
                  null;
            end case;

            Decl := Next_Declarative_Item (Decl, Tree);
         end loop;
      end Recurse_In_Project;

      Iter : Inner_Project_Iterator := Start (Root_Project);
   begin
      while Current (Iter) /= No_Project loop
         Recurse_In_Project (Current (Iter).Node, Empty_Node);
         Next (Iter);
      end loop;
      Free (Variable_Nodes);
   end For_Each_Environment_Variable;

   ---------------------------------
   -- Remove_Variable_Declaration --
   ---------------------------------

   procedure Remove_Variable_Declaration
     (Tree               : Project_Node_Tree_Ref;
      Project_Or_Package : Project_Node_Id;
      Declaration        : Project_Node_Id)
   is
      Tmp, Next : Project_Node_Id;
      Pkg       : Project_Node_Id := Project_Or_Package;
   begin
      while Pkg /= Empty_Node loop
         Tmp := First_Variable_Of (Pkg, Tree);

         if Tmp = Declaration then
            Set_First_Variable_Of (Pkg, Tree, Next_Variable (Tmp, Tree));
            return;
         else
            loop
               Next := Next_Variable (Tmp, Tree);
               exit when Next = Empty_Node;

               if Next = Declaration then
                  Set_Next_Variable (Tmp, Tree, Next_Variable (Next, Tree));
                  return;
               end if;
            end loop;
         end if;

         if Kind_Of (Pkg, Tree) = N_Project then
            Pkg := First_Package_Of (Pkg, Tree);
         else
            Pkg := Next_Package_In_Project (Pkg, Tree);
         end if;
      end loop;

      Trace (Me, "Remove_Variable_Declaration: did not find the declaration"
             & " for the variable");
   end Remove_Variable_Declaration;

   -----------------
   -- Remove_Node --
   -----------------

   procedure Remove_Node
     (Tree   : Project_Node_Tree_Ref;
      Parent : Project_Node_Id;
      Node   : Project_Node_Id)
   is
      P          : Project_Node_Id := Parent;
      Decl, Next : Project_Node_Id;
   begin
      --  ??? Should reset the list of Variables and Types if the node matches
      if Kind_Of (Parent, Tree) = N_Project then
         P := Project_Declaration_Of (Parent, Tree);
      end if;

      Decl := First_Declarative_Item_Of (P, Tree);

      if Current_Item_Node (Decl, Tree) = Node then
         Set_First_Declarative_Item_Of
           (P, Tree, Next_Declarative_Item (Decl, Tree));
      end if;

      while Decl /= Empty_Node loop
         Next := Next_Declarative_Item (Decl, Tree);
         if Next /= Empty_Node
           and then Current_Item_Node (Next, Tree) = Node
         then
            Set_Next_Declarative_Item
              (Decl, Tree, Next_Declarative_Item (Next, Tree));
            exit;
         end if;

         Decl := Next;
      end loop;
   end Remove_Node;

   -----------------------------
   -- Get_All_Possible_Values --
   -----------------------------

   function Get_All_Possible_Values
     (Tree     : Project_Node_Tree_Ref;
      Variable : Project_Node_Id) return Name_Id_Array
   is
      Choice        : Project_Node_Id := First_Literal_String
        (String_Type_Of (Variable, Tree), Tree);
      Choices_Count : Natural := 0;
   begin
      while Choice /= Empty_Node loop
         Choices_Count := Choices_Count + 1;
         Choice        := Next_Literal_String (Choice, Tree);
      end loop;

      declare
         Choices : Name_Id_Array (1 .. Choices_Count);
         Index   : Natural := Choices'First;
      begin
         Choice := First_Literal_String
           (String_Type_Of (Variable, Tree), Tree);
         while Choice /= Empty_Node loop
            Choices (Index) := String_Value_Of (Choice, Tree);
            Index := Index + 1;
            Choice := Next_Literal_String (Choice, Tree);
         end loop;

         return Choices;
      end;
   end Get_All_Possible_Values;

   --------------------------
   -- String_As_Expression --
   --------------------------

   function String_As_Expression
     (Value : Name_Id; Tree : Project_Node_Tree_Ref) return Project_Node_Id is
   begin
      return Enclose_In_Expression (Create_Literal_String (Value, Tree), Tree);
   end String_As_Expression;

   -----------------------------------
   -- Remove_Attribute_Declarations --
   -----------------------------------

   procedure Remove_Attribute_Declarations
     (Tree            : Project_Node_Tree_Ref;
      Parent          : Project_Node_Id;
      Attribute_Name  : Name_Id;
      Attribute_Index : Name_Id)
   is
      Decl     : Project_Node_Id := First_Declarative_Item_Of (Parent, Tree);
      Previous : Project_Node_Id := Empty_Node;
   begin
      while Decl /= Empty_Node loop
         if Attribute_Matches
           (Tree, Current_Item_Node (Decl, Tree),
            Attribute_Name, Attribute_Index)
         then
            if Previous = Empty_Node then
               Set_First_Declarative_Item_Of
                 (Parent, Tree, Next_Declarative_Item (Decl, Tree));
            else
               Set_Next_Declarative_Item
                 (Previous, Tree, Next_Declarative_Item (Decl, Tree));
            end if;
         else
            Previous := Decl;
         end if;

         Decl := Next_Declarative_Item (Decl, Tree);
      end loop;
   end Remove_Attribute_Declarations;

   ------------------------------
   -- Find_Last_Declaration_Of --
   ------------------------------

   function Find_Last_Declaration_Of
     (Tree       : Project_Node_Tree_Ref;
      Parent     : Project_Node_Id;
      Attr_Name  : Name_Id;
      Attr_Index : Name_Id := No_Name) return Project_Node_Id
   is
      Decl, Expr : Project_Node_Id;
      Result     : Project_Node_Id := Empty_Node;
   begin
      Decl := First_Declarative_Item_Of (Parent, Tree);

      while Decl /= Empty_Node loop
         Expr := Current_Item_Node (Decl, Tree);

         if Attribute_Matches (Tree, Expr, Attr_Name, Attr_Index) then
            Result := Expr;
         end if;

         Decl := Next_Declarative_Item (Decl, Tree);
      end loop;
      return Result;
   end Find_Last_Declaration_Of;

   -----------------------
   -- Attribute_Matches --
   -----------------------

   function Attribute_Matches
     (Tree            : Project_Node_Tree_Ref;
      Node            : Project_Node_Id;
      Attribute_Name  : Name_Id;
      Attribute_Index : Name_Id) return Boolean is
   begin
      return Kind_Of (Node, Tree) = N_Attribute_Declaration
        and then Prj.Tree.Name_Of (Node, Tree) = Attribute_Name
        and then
        (Attribute_Index = Get_String (Any_Attribute)
         or else (Attribute_Index = No_Name
          and then Associative_Array_Index_Of (Node, Tree) = No_Name)
         or else (Attribute_Index /= No_Name
                  and then Associative_Array_Index_Of (Node, Tree) /= No_Name
                  and then Associative_Array_Index_Of (Node, Tree) =
                     Attribute_Index));
   end Attribute_Matches;

   ---------------------------------
   -- For_Each_Scenario_Case_Item --
   ---------------------------------

   procedure For_Each_Scenario_Case_Item
     (Tree               : Project_Node_Tree_Ref;
      Project            : Prj.Tree.Project_Node_Id;
      Pkg                : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node;
      Case_Construct     : in out Prj.Tree.Project_Node_Id;
      Scenario_Variables : Scenario_Variable_Array;
      Action             : Matching_Item_Callback)
   is
      Values : External_Variable_Value_Array (1 .. Scenario_Variables'Length);
      Last_Values : Natural := Values'First - 1;
   begin
      for J in Scenario_Variables'Range loop
         Last_Values := Last_Values + 1;
         Values (Last_Values) := External_Variable_Value'
           (Variable_Type  => Scenario_Variables (J).String_Type,
            Variable_Name  => Scenario_Variables (J).Name,
            Variable_Value => Scenario_Variables (J).Value,
            Negated        => False);
      end loop;
      For_Each_Matching_Case_Item
        (Tree, Project, Pkg, Case_Construct, Values, Action);
   end For_Each_Scenario_Case_Item;

   ----------------------
   -- Add_Node_To_List --
   ----------------------

   procedure Add_Node_To_List
     (To   : in out Project_Node_Array_Access;
      Last : in out Natural;
      Node : Project_Node_Id)
   is
      Old : Project_Node_Array_Access := To;
   begin
      if Last = To'Last then
         To := new Project_Node_Array (1 .. Old'Last * 2);
         To (1 .. Old'Length) := Old.all;
         Free (Old);
      end if;

      Last := Last + 1;
      To (Last) := Node;
   end Add_Node_To_List;

   ------------------------------
   -- Find_Package_Declaration --
   ------------------------------

   function Find_Package_Declaration
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Node_Id; Name : Namet.Name_Id)
      return Project_Node_Id is
   begin
      return Find_Node_By_Name (Tree, Project, N_Package_Declaration, Name);
   end Find_Package_Declaration;

   --------------------
   -- Set_Expression --
   --------------------

   procedure Set_Expression
     (Tree             : Project_Node_Tree_Ref;
      Var_Or_Attribute : Project_Node_Id;
      Expr             : Project_Node_Id)
   is
      E : Project_Node_Id;
   begin
      E := Expression_Of (Var_Or_Attribute, Tree);

      if E = Empty_Node then
         Set_Expression_Of (Var_Or_Attribute, Tree, Expr);

      else
         case Kind_Of (E, Tree) is
            when N_Expression =>
               Set_Expression_Of (Var_Or_Attribute, Tree, Expr);
            when N_External_Value =>
               Set_External_Default_Of (E, Tree, Expr);
            when others =>
               raise Program_Error;
         end case;
      end if;
   end Set_Expression;

   ------------------------------
   -- Post_Process_After_Clone --
   ------------------------------

   procedure Post_Process_After_Clone
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Node_Id;
      Pkg     : Project_Node_Id := Empty_Node)
   is
      Last_Var     : Project_Node_Id := Empty_Node;
      Last_Type    : Project_Node_Id := Empty_Node;
      Last_Package : Project_Node_Id := Empty_Node;
      Decl_Item    : Project_Node_Id;
      Current_Node : Project_Node_Id;

   begin
      if Pkg = Empty_Node then
         Decl_Item := First_Declarative_Item_Of
           (Project_Declaration_Of (Project, Tree), Tree);
      else
         pragma Assert (Kind_Of (Pkg, Tree) = N_Package_Declaration);
         Decl_Item := First_Declarative_Item_Of (Pkg, Tree);
      end if;

      while Decl_Item /= Empty_Node loop
         Current_Node := Current_Item_Node (Decl_Item, Tree);
         case Kind_Of (Current_Node, Tree) is
            when N_Package_Declaration =>
               if Last_Package /= Empty_Node then
                  Set_Next_Package_In_Project
                    (Last_Package, Tree, Current_Node);
                  Last_Package := Current_Node;
               else
                  Last_Package := Current_Node;
                  Tree.Project_Nodes.Table (Project).Packages := Last_Package;
               end if;

               Post_Process_After_Clone (Tree, Project, Last_Package);

            when N_Variable_Declaration | N_Typed_Variable_Declaration =>
               if Last_Var /= Empty_Node then
                  Set_Next_Variable (Last_Var, Tree, Current_Node);
                  Set_Next_Variable (Current_Node, Tree, Empty_Node);
                  Last_Var := Current_Node;
               else
                  Last_Var := Current_Node;
                  Set_Next_Variable (Last_Var, Tree, Empty_Node);

                  if Pkg /= Empty_Node then
                     Tree.Project_Nodes.Table (Pkg).Variables := Last_Var;
                  else
                     Tree.Project_Nodes.Table (Project).Variables := Last_Var;
                  end if;
               end if;

               --  Make sure that we do reference the type defined in the new
               --  project, not in some older project
               if Kind_Of (Current_Node, Tree) =
                 N_Typed_Variable_Declaration
               then
                  Set_String_Type_Of
                    (Current_Node, Tree,
                     Find_Type_Declaration
                       (Tree,
                        Project,
                        Prj.Tree.Name_Of
                          (String_Type_Of (Current_Node, Tree), Tree)));
               end if;

            when N_Variable_Reference =>
               if String_Type_Of (Current_Node, Tree) /= Empty_Node then
                  Set_String_Type_Of
                    (Current_Node, Tree,
                     Find_Type_Declaration
                       (Tree,
                        Project,
                        Prj.Tree.Name_Of (String_Type_Of (Current_Node, Tree),
                                          Tree)));
               end if;

               if Package_Node_Of (Current_Node, Tree) /= Empty_Node then
                  Set_Package_Node_Of
                    (Current_Node, Tree,
                     Find_Package_Declaration
                       (Tree,
                        Project,
                        Prj.Tree.Name_Of (Package_Node_Of (Current_Node, Tree),
                                          Tree)));
               end if;

            when N_Attribute_Reference =>
               if Package_Node_Of (Current_Node, Tree) /= Empty_Node then
                  Set_Package_Node_Of
                    (Current_Node, Tree,
                     Find_Package_Declaration
                       (Tree,
                        Project,
                        Prj.Tree.Name_Of (Package_Node_Of (Current_Node, Tree),
                                          Tree)));
               end if;

            when N_String_Type_Declaration =>
               if Last_Type /= Empty_Node then
                  Set_Next_String_Type (Last_Type, Tree, Current_Node);
                  Last_Type := Current_Node;
               else
                  Last_Type := Current_Node;
                  Set_First_String_Type_Of (Project, Tree, Last_Type);
               end if;

            when others =>
               null;

         end case;

         Decl_Item := Next_Declarative_Item (Decl_Item, Tree);
      end loop;
   end Post_Process_After_Clone;

   -----------------------
   -- Add_To_Case_Items --
   -----------------------

   procedure Add_To_Case_Items
     (Tree              : Project_Node_Tree_Ref;
      Case_Construction : Project_Node_Id;
      Decl_List         : Project_Node_Id)
   is
      Case_Item : Project_Node_Id;
   begin
      Case_Item := First_Case_Item_Of (Case_Construction, Tree);
      while Case_Item /= Empty_Node loop
         Add_In_Front (Tree, Case_Item, Clone_Node (Tree, Decl_List, True));
         Case_Item := Next_Case_Item (Case_Item, Tree);
      end loop;
   end Add_To_Case_Items;

   ---------------------------
   -- Create_Typed_Variable --
   ---------------------------

   function Create_Typed_Variable
     (Tree                         : Project_Node_Tree_Ref;
      Prj_Or_Pkg                   : Project_Node_Id;
      Name                         : String;
      Typ                          : Project_Node_Id;
      Add_Before_First_Case_Or_Pkg : Boolean := False) return Project_Node_Id
   is
      Node : constant Project_Node_Id :=
        Default_Project_Node (Tree, N_Typed_Variable_Declaration, Prj.Single);
   begin
      Set_Name_Of (Node, Tree, Get_String (Name));
      Set_String_Type_Of (Node, Tree, Typ);

      Add_At_End (Tree, Prj_Or_Pkg, Node,
                  Add_Before_First_Pkg  => Add_Before_First_Case_Or_Pkg,
                  Add_Before_First_Case => Add_Before_First_Case_Or_Pkg);

      Set_Next_Variable (Node, Tree, First_Variable_Of (Prj_Or_Pkg, Tree));
      Set_First_Variable_Of (Prj_Or_Pkg, Tree, Node);

      return Node;
   end Create_Typed_Variable;

   -------------------
   -- Add_Case_Item --
   -------------------

   procedure Add_Case_Item
     (Tree      : Project_Node_Tree_Ref;
      Case_Node : Project_Node_Id;
      Choice    : Name_Id)
   is
      Item, S, In_List : Project_Node_Id;
   begin
      --  Add the new case item at the end of the list, so that the order of
      --  items is the same as in the type declaration (H222-027)

      Item := Default_Project_Node (Tree, N_Case_Item);
      S := Default_Project_Node (Tree, N_Literal_String);
      Set_String_Value_Of (S, Tree, Choice);
      Set_First_Choice_Of (Item, Tree, S);

      In_List := First_Case_Item_Of (Case_Node, Tree);
      if In_List = Empty_Node then
         Set_First_Case_Item_Of (Case_Node, Tree, Item);
      else
         while Next_Case_Item (In_List, Tree) /= Empty_Node loop
            In_List := Next_Case_Item (In_List, Tree);
         end loop;
         Set_Next_Case_Item (In_List, Tree, Item);
      end if;
   end Add_Case_Item;

   ------------------
   -- Add_In_Front --
   ------------------

   procedure Add_In_Front
     (Tree   : Project_Node_Tree_Ref;
      Parent : Project_Node_Id;
      Node   : Project_Node_Id)
   is
      Real_Parent    : Project_Node_Id;
      New_Decl, Decl : Project_Node_Id;
   begin
      if Kind_Of (Node, Tree) /= N_Declarative_Item then
         New_Decl := Default_Project_Node (Tree, N_Declarative_Item);
         Set_Current_Item_Node (New_Decl, Tree, Node);
      else
         New_Decl := Node;
      end if;

      if Kind_Of (Parent, Tree) = N_Project then
         Real_Parent := Project_Declaration_Of (Parent, Tree);
      else
         Real_Parent := Parent;
      end if;

      Decl := New_Decl;
      while Next_Declarative_Item (Decl, Tree) /= Empty_Node loop
         Decl := Next_Declarative_Item (Decl, Tree);
      end loop;

      Set_Next_Declarative_Item
        (Decl, Tree, First_Declarative_Item_Of (Real_Parent, Tree));
      Set_First_Declarative_Item_Of (Real_Parent, Tree, New_Decl);
   end Add_In_Front;

   -------------------------------
   -- Create_Variable_Reference --
   -------------------------------

   function Create_Variable_Reference
     (Tree : Project_Node_Tree_Ref; Var : Project_Node_Id)
      return Project_Node_Id
   is
      Ref : Project_Node_Id;
   begin
      Assert (Me,
              Kind_Of (Var, Tree) = N_Typed_Variable_Declaration
              or else Kind_Of (Var, Tree) = N_Variable_Declaration,
              "Create_Variable_Reference: unexpected node type "
              & Kind_Of (Var, Tree)'Img);

      Ref := Default_Project_Node (Tree, N_Variable_Reference);
      Set_Name_Of (Ref, Tree, Prj.Tree.Name_Of (Var, Tree));
      Set_Expression_Kind_Of (Ref, Tree, Expression_Kind_Of (Var, Tree));

      if Kind_Of (Var, Tree) = N_Typed_Variable_Declaration then
         Set_String_Type_Of (Ref, Tree, String_Type_Of (Var, Tree));
      end if;
      return Ref;
   end Create_Variable_Reference;

   ---------------------------
   -- Set_Value_As_External --
   ---------------------------

   procedure Set_Value_As_External
     (Tree          : Project_Node_Tree_Ref;
      Var           : Project_Node_Id;
      External_Name : String;
      Default       : String := "")
   is
      Ext : Project_Node_Id;
      Str : Project_Node_Id;
   begin
      pragma Assert (Expression_Kind_Of (Var, Tree) = Prj.Single);

      --  Create the expression if required

      Ext := Default_Project_Node (Tree, N_External_Value, Single);
      Set_Expression (Tree, Var, Enclose_In_Expression (Ext, Tree));

      Str := Create_Literal_String (Get_String (External_Name), Tree);

      Set_External_Reference_Of (Ext, Tree, Str);

      if Default /= "" then
         Str := Create_Literal_String (Get_String (Default), Tree);
         Set_External_Default_Of (Ext, Tree, Str);
      end if;
   end Set_Value_As_External;

   ------------------------------
   -- Create_Case_Construction --
   ------------------------------

   function Create_Case_Construction
     (Tree          : Project_Node_Tree_Ref;
      Project       : Project_Node_Id;
      External_Name : Name_Id;
      Var_Type      : Project_Node_Id) return Project_Node_Id
   is
      Construct, Str : Project_Node_Id;
      Item           : Project_Node_Id := Empty_Node;
      Ref            : Name_Id;
      Decl           : Project_Node_Id;
      New_Type       : Project_Node_Id;
   begin
      --  Make sure there is a definition for this variable (and its type) at
      --  the top-level of the project (not in a package nor in another
      --  project).
      --  This is required so that normalized projects might be standalone.

      --  Check if there is already a definition for the variable, and if not
      --  add it. Note: we do this before testing for the type, since we are
      --  adding in front of the declarative items list.
      --  We cannot use Project.Variables since the lists are not created
      --  before the post-processing phase of the normalization.

      Decl := First_Declarative_Item_Of
        (Project_Declaration_Of (Project, Tree), Tree);
      while Decl /= Empty_Node loop
         Item := Current_Item_Node (Decl, Tree);
         if Kind_Of (Item, Tree) = N_Typed_Variable_Declaration then
            Ref := External_Reference_Of (Item, Tree);

            exit when Ref /= No_Name and then Ref = External_Name;
         end if;
         Item := Empty_Node;
         Decl := Next_Declarative_Item (Decl, Tree);
      end loop;

      --  If not, add the variable and its expression

      if Item = Empty_Node then
         Get_Name_String (External_Name);
         Item := Create_Typed_Variable
           (Tree, Project, Name_Buffer (1 .. Name_Len), Var_Type,
            Add_Before_First_Case_Or_Pkg => True);
         Set_Value_As_External (Tree, Item, Name_Buffer (1 .. Name_Len));

         --  Make sure the type is only used for that variable, so that is can
         --  be freely modified. If we already have a type by the same name,
         --  find a new name.

         New_Type := Clone_Node (Tree, Var_Type, True);
         Set_Uniq_Type_Name (Tree, Project, New_Type);
         Set_String_Type_Of (Item, Tree, New_Type);
         Add_In_Front (Tree, Project, New_Type);
      end if;

      Construct := Default_Project_Node (Tree, N_Case_Construction);
      Set_Case_Variable_Reference_Of
        (Construct, Tree, Create_Variable_Reference (Tree, Item));

      Str := First_Literal_String (Var_Type, Tree);
      while Str /= Empty_Node loop
         Add_Case_Item (Tree, Construct, String_Value_Of (Str, Tree));
         Str := Next_Literal_String (Str, Tree);
      end loop;

      return Construct;
   end Create_Case_Construction;

   ---------------------------------
   -- For_Each_Matching_Case_Item --
   ---------------------------------

   procedure For_Each_Matching_Case_Item
     (Tree           : Project_Node_Tree_Ref;
      Project        : Prj.Tree.Project_Node_Id;
      Pkg            : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node;
      Case_Construct : in out Prj.Tree.Project_Node_Id;
      Values         : External_Variable_Value_Array;
      Action         : Matching_Item_Callback)
   is
      Var_Seen : External_Variable_Value_Array_Access :=
                   new External_Variable_Value_Array (Values'Range);
      Last_Var_Seen : Natural := Var_Seen'First - 1;
      --  Only the Variable_Name is relevant here, and this is used to detect
      --  the variables that didn't play any role in the current case construct

      procedure Process_Case_Recursive (Case_Stmt : Project_Node_Id);
      --  Act recursively on Case_Stmt and search within all its case items for
      --  the matching ones.

      function Create_Case_If_Necessary return Project_Node_Id;
      --  Create a new case construction if necessary (ie if some variable was
      --  referenced in Values, and there was no matching case construction so
      --  far).
      --  Empty_Node is returned if no case construction was created.

      ------------------------------
      -- Create_Case_If_Necessary --
      ------------------------------

      function Create_Case_If_Necessary return Project_Node_Id is
         Match : Boolean;
      begin
         for J in Values'Range loop
            Match := False;
            for K in Var_Seen'First .. Last_Var_Seen loop
               if Values (J).Variable_Name = Var_Seen (K).Variable_Name then
                  Match := True;
                  exit;
               end if;
            end loop;

            if not Match then
               return Create_Case_Construction
                 (Tree, Project,
                  Values (J).Variable_Name, Values (J).Variable_Type);
            end if;
         end loop;
         return Empty_Node;
      end Create_Case_If_Necessary;

      ----------------------------
      -- Process_Case_Recursive --
      ----------------------------

      procedure Process_Case_Recursive (Case_Stmt : Project_Node_Id) is
         Name                   : constant Name_Id := External_Variable_Name
           (Tree, Project, Case_Variable_Reference_Of (Case_Stmt, Tree));
         Current_Item, New_Case : Project_Node_Id;
         Handling_Done          : Boolean;

      begin
         pragma Assert (Name /= No_Name);

         --  Memorise the name of the variable we are processing, so that we
         --  can create missing case constructions at the end

         Add_Value
           (Var_Seen, Last_Var_Seen, (Empty_Node, Name, No_Name, False));

         --  For all possible values of the variable

         Current_Item := First_Case_Item_Of (Case_Stmt, Tree);
         while Current_Item /= Empty_Node loop
            if Values_Matches (Tree, Name, Current_Item, Values) then
               Handling_Done := False;
               New_Case := First_Declarative_Item_Of (Current_Item, Tree);

               --  Are there any nested case ?
               while New_Case /= Empty_Node loop
                  if Kind_Of (Current_Item_Node (New_Case, Tree), Tree)
                    = N_Case_Construction
                  then
                     Process_Case_Recursive
                       (Current_Item_Node (New_Case, Tree));
                     Handling_Done := True;
                     exit;
                  end if;

                  New_Case := Next_Declarative_Item (New_Case, Tree);
               end loop;

               if not Handling_Done then
                  New_Case := Create_Case_If_Necessary;
                  Handling_Done := New_Case /= Empty_Node;

                  if Handling_Done then
                     --  Move all the declarative items currently in the case
                     --  item to the nested case construction, so that we only
                     --  have declarative items in the most-nested case
                     --  constructions.
                     if First_Declarative_Item_Of (Current_Item, Tree) /=
                       Empty_Node
                     then
                        Add_To_Case_Items
                          (Tree,
                           New_Case,
                           First_Declarative_Item_Of (Current_Item, Tree));
                        Set_First_Declarative_Item_Of
                          (Current_Item, Tree, Empty_Node);
                     end if;

                     Add_At_End (Tree, Current_Item, New_Case);
                     Process_Case_Recursive (New_Case);
                  end if;
               end if;

               --  We can now report the matching case item
               if not Handling_Done and then Action /= null then
                  Action (Current_Item);
               end if;
            end if;

            Current_Item := Next_Case_Item (Current_Item, Tree);
         end loop;

         Last_Var_Seen := Last_Var_Seen - 1;
      end Process_Case_Recursive;

      Top : Project_Node_Id;
   begin
      if Pkg /= Empty_Node then
         Top := Pkg;
      else
         Top := Project_Declaration_Of (Project, Tree);
      end if;

      if Case_Construct = Empty_Node then
         Case_Construct := Create_Case_If_Necessary;
         if Case_Construct /= Empty_Node then
            Add_At_End (Tree, Top, Case_Construct);
         end if;
      end if;

      if Case_Construct = Empty_Node then
         if Action /= null then
            Action (Top);
         end if;
      else
         Process_Case_Recursive (Case_Construct);
      end if;

      Free (Var_Seen);
   end For_Each_Matching_Case_Item;

   -----------------------
   -- Find_Node_By_Name --
   -----------------------

   function Find_Node_By_Name
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Node_Id;
      Kind    : Project_Node_Kind;
      Name    : Name_Id) return Project_Node_Id
   is
      Decl    : Project_Node_Id := First_Declarative_Item_Of
        (Project_Declaration_Of (Project, Tree), Tree);
      Current : Project_Node_Id;
   begin
      while Decl /= Empty_Node loop
         Current := Current_Item_Node (Decl, Tree);
         if Kind_Of (Current, Tree) = Kind
           and then Prj.Tree.Name_Of (Current, Tree) = Name
         then
            return Current;
         end if;

         Decl := Next_Declarative_Item (Decl, Tree);
      end loop;
      return Empty_Node;
   end Find_Node_By_Name;

   ---------------------------
   -- Find_Type_Declaration --
   ---------------------------

   function Find_Type_Declaration
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Node_Id;
      Name    : Namet.Name_Id) return Project_Node_Id is
   begin
      return Find_Node_By_Name
        (Tree, Project, N_String_Type_Declaration, Name);
   end Find_Type_Declaration;

   ------------------------
   -- Get_Uniq_Type_Name --
   ------------------------

   procedure Set_Uniq_Type_Name
     (Tree     : Project_Node_Tree_Ref;
      Project  : Project_Node_Id;
      Var_Type : Project_Node_Id)
   is
      Candidate : Name_Id;
      Attempt   : Natural := 1;
   begin
      --  Check the type itself
      Candidate := Name_Of (Var_Type, Tree);

      while Find_Type_Declaration (Tree, Project, Candidate) /= Empty_Node loop
         Get_Name_String (Candidate);

         Get_Name_String (Name_Of (Var_Type, Tree));
         Add_Str_To_Name_Buffer (Image (Attempt, Min_Width => 0));
         Attempt := Attempt + 1;

         Candidate := Name_Find;
      end loop;

      Set_Name_Of (Var_Type, Tree, Candidate);
   end Set_Uniq_Type_Name;

   --------------------
   -- Values_Matches --
   --------------------

   function Values_Matches
     (Tree      : Project_Node_Tree_Ref;
      Var_Name  : Name_Id;
      Case_Item : Project_Node_Id;
      Values    : External_Variable_Value_Array) return Boolean
   is
      --  The rule is the following: if there is any non-negated item,
      --  then we must match at least one of them. If there are none,
      --  then the case item matches if non of the negated item matches
      Match  : Boolean := True;
      Choice : Project_Node_Id := First_Choice_Of (Case_Item, Tree);
   begin
      Choice_Loop :
      while Choice /= Empty_Node loop
         for J in Values'Range loop
            if Values (J).Variable_Name = Var_Name then
               --  Change the default value if needed
               Match := Values (J).Negated;

               if Values (J).Variable_Value =
                 String_Value_Of (Choice, Tree)
               then
                  Match := not Values (J).Negated;
                  exit Choice_Loop;
               end if;
            end if;
         end loop;

         Choice := Next_Literal_String (Choice, Tree);
      end loop Choice_Loop;

      return Match;
   end Values_Matches;

   ----------------------------
   -- External_Variable_Name --
   ----------------------------

   function External_Variable_Name
     (Tree            : Project_Node_Tree_Ref;
      Current_Project : Project_Node_Id;
      Ref             : Project_Node_Id) return Name_Id
   is
      N              : constant Name_Id := Prj.Tree.Name_Of (Ref, Tree);
      Pkg            : Project_Node_Id;
      Variable       : Variable_Node_Id;
      Recurse_In_Pkg : Boolean := False;

   begin
      if Package_Node_Of (Ref, Tree) /= Empty_Node then
         Pkg := Package_Node_Of (Ref, Tree);
      elsif Project_Node_Of (Ref, Tree) /= Empty_Node then
         Pkg := Project_Node_Of (Ref, Tree);
      else
         Pkg := Current_Project;
         Recurse_In_Pkg := True;
      end if;

      --  Should the project parser set the package_of field when the variable
      --  is defined inside a package ? Currently, it only sets this field if
      --  it is specified in the file itself.

      while Pkg /= Empty_Node loop
         Variable := First_Variable_Of (Pkg, Tree);
         while Variable /= Empty_Node loop
            if (Kind_Of (Variable, Tree) = N_Variable_Declaration
               or else Kind_Of (Variable, Tree) = N_Typed_Variable_Declaration)
              and then Prj.Tree.Name_Of (Variable, Tree) = N
            then
               return External_Reference_Of (Variable, Tree);
            end if;

            Variable := Next_Variable (Variable, Tree);
         end loop;

         if Recurse_In_Pkg then
            if Pkg = Current_Project then
               Pkg := First_Package_Of (Pkg, Tree);
            else
               Pkg := Next_Package_In_Project (Pkg, Tree);
            end if;
         end if;
      end loop;

      return No_Name;
   end External_Variable_Name;

   ---------------
   -- Add_Value --
   ---------------

   procedure Add_Value
     (To   : in out External_Variable_Value_Array_Access;
      Last : in out Natural;
      V    : External_Variable_Value)
   is
      Old      : External_Variable_Value_Array_Access := To;
      New_Last : Natural;
   begin
      if To = null or else Last = To'Last then
         if To = null or else To'Last = 0 then
            New_Last := 20;
         else
            New_Last :=  Old'Last * 2;
         end if;

         To :=  new External_Variable_Value_Array (Old'First .. New_Last);
         To (Old'Range) := Old.all;
         Free (Old);
      end if;

      Last := Last + 1;
      To (Last) := V;
   end Add_Value;

   -------------------
   -- Clone_Project --
   -------------------

   function Clone_Project
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Node_Id) return Project_Node_Id
   is
      Project2, Decl : Project_Node_Id;
   begin
      Project2 := Clone_Node (Tree, Project);
      Decl     := Clone_Node (Tree, Project_Declaration_Of (Project, Tree));
      Set_Project_Declaration_Of    (Project2, Tree, Decl);
      Set_First_Declarative_Item_Of (Decl, Tree, Empty_Node);
      return Project2;
   end Clone_Project;

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize
     (Tree    : Project_Tree_Data_Access;
      Project : Project_Type)
   is
      Tree_Node : constant Project_Node_Tree_Ref :=
        GNATCOLL.Projects.Tree (Tree);

      Values : External_Variable_Value_Array_Access := null;
      Last_Values : Natural;
      --  Representation of the state of the case construction that is being
      --  parsed.
      --  Each time a new case item is seen in the original project, an entry
      --  is added into this array for all the possible values of the variable.
      --  On exit of the case item, the items are negated, so that it is still
      --  possible to process "when others".
      --  On exit of the case construction, the entries for the variable are
      --  removed from this list.

      Project_Norm   : Project_Node_Id;
      Current_Pkg    : Project_Node_Id;
      Case_Construct : Project_Node_Id;

      Decl                   : Project_Node_Id;
      Project_Node           : Project_Node_Id;
      Max_Scenario_Variables : Integer;

      procedure Process_Declarative_List
        (From, To : Project_Node_Id);
      --  Process a declarative list (a project, a package, a case item,...).
      --  From is the first N_Declarative_Item in the list. To is the node to
      --  which the normalized declarative items are added.
      --  Note: It skips subpackages, and doesn't process them recursively,
      --  you should call this procedure once per subpackage.
      --
      --  The output is however added to the case construction Case_Stmt if it
      --  is not Empty_Node, or if the Values is not null (the first case
      --  represent the case where there was a case statement before in the
      --  project file, the second one represents the first case statement).

      procedure Check_Index_Sensitivity (Decl_Item : Project_Node_Id);
      --  If Decl_Item is the declaration of an attribute the index of which
      --  is case-insensitive, convert that index to lower-case so that GPS
      --  properly finds references to it later on.

      -----------------------------
      -- Check_Index_Sensitivity --
      -----------------------------

      procedure Check_Index_Sensitivity (Decl_Item : Project_Node_Id) is
         Current : constant Project_Node_Id :=
                     Current_Item_Node (Decl_Item, Tree_Node);
      begin
         if Kind_Of (Current, Tree_Node) = N_Attribute_Declaration then
            --  It is possible that the attribute doesn't have an index in the
            --  case of
            --    for Switches use Imported.Compiler'Switches;
            --  whereas in general Switches excepts an index.

            if Case_Insensitive (Current, Tree_Node)
              and then Associative_Array_Index_Of
                (Current, Tree_Node) /= No_Name
            then
               Get_Name_String
                 (Associative_Array_Index_Of (Current, Tree_Node));
               To_Lower (Name_Buffer (1 .. Name_Len));

               Set_Associative_Array_Index_Of (Current, Tree_Node, Name_Find);
            end if;
         end if;
      end Check_Index_Sensitivity;

      ------------------------------
      -- Process_Declarative_List --
      ------------------------------

      procedure Process_Declarative_List (From, To : Project_Node_Id) is
         Decl_Item, Current      : Project_Node_Id := From;
         Next_Item, Choice       : Project_Node_Id;
         Name                    : Name_Id;
         Case_Item               : Project_Node_Id;
         Index                   : Natural;
         Var_Type                : Project_Node_Id;
         Already_Have_Var, Match : Boolean;
         Decl_Item2              : Project_Node_Id;

         procedure Add_Decl_Item (To_Case_Item : Project_Node_Id);
         --  Add Decl_Item to To_Case_Item

         -------------------
         -- Add_Decl_Item --
         -------------------

         procedure Add_Decl_Item (To_Case_Item : Project_Node_Id) is
         begin
            Add_At_End
              (Tree_Node, To_Case_Item,
               Clone_Node (Tree_Node, Decl_Item, True));
         end Add_Decl_Item;

      begin
         --  Nothing to do if there is no project
         if From = Empty_Node then
            return;
         end if;

         pragma Assert (Kind_Of (Decl_Item, Tree_Node) = N_Declarative_Item);

         while Decl_Item /= Empty_Node loop
            Current := Current_Item_Node (Decl_Item, Tree_Node);

            --  Save the next item, since the current item will be inserted in
            --  a different list, and thus its next field will be modified.

            Next_Item := Next_Declarative_Item (Decl_Item, Tree_Node);
            Set_Next_Declarative_Item (Decl_Item, Tree_Node, Empty_Node);

            case Kind_Of (Current, Tree_Node) is
               when N_Package_Declaration =>
                  --  Skip subpackages, since these must appear after every
                  --  other declarative item in the normalized project.
                  null;

               when N_Case_Construction =>
                  Name := External_Variable_Name
                    (Tree_Node,
                     Project_Norm,
                     Case_Variable_Reference_Of (Current, Tree_Node));

                  if Name = No_Name then
                     Trace (Me, "Normalizing a project with a non-scenario "
                            & "variable in case construction");

                     Raise_Exception
                       (Normalize_Error'Identity,
                        Project.Name
                        & Prj.Project_File_Extension & ": "
                        & "Case constructions referencing non-external"
                        & " variables can not be modified");
                  end if;

                  Var_Type := String_Type_Of
                    (Case_Variable_Reference_Of (Current, Tree_Node),
                     Tree_Node);

                  --  Do we already have this variable in values
                  --  If yes, this means we have something similar to:
                  --    case A is
                  --       when "1" =>
                  --          case A is
                  --              when "1" => keep;
                  --              when "2" => ignore;
                  --  We should only keep the item in the nested case that
                  --  matches the value of the outer item.

                  Already_Have_Var := False;

                  for J in Values'First .. Last_Values loop
                     if Values (J).Variable_Name = Name then
                        Already_Have_Var := True;
                        exit;
                     end if;
                  end loop;

                  Case_Item := First_Case_Item_Of (Current, Tree_Node);

                  --  For all the case items in the current case construction

                  while Case_Item /= Empty_Node loop
                     Index := Last_Values + 1;

                     if Already_Have_Var then
                        Match := Values_Matches
                          (Tree_Node, Name, Case_Item,
                           Values (Values'First .. Last_Values));

                     else
                        Match := True;
                        Choice := First_Choice_Of (Case_Item, Tree_Node);
                        while Choice /= Empty_Node loop
                           Add_Value
                             (Values, Last_Values,
                              External_Variable_Value'
                              (Var_Type,
                               Name,
                               String_Value_Of (Choice, Tree_Node),
                               False));
                           Choice := Next_Literal_String (Choice, Tree_Node);
                        end loop;
                     end if;

                     if Match then
                        --  Process the declarative list of items

                        Process_Declarative_List
                          (First_Declarative_Item_Of (Case_Item, Tree_Node),
                           To);

                        --  Negate all the values

                        for J in Index .. Last_Values loop
                           Values (J).Negated := True;
                        end loop;
                     end if;

                     Case_Item := Next_Case_Item (Case_Item, Tree_Node);
                  end loop;

                  --  Remove all the entries for the variable in the array
                  --  Note that we do not need to use String_Equal, since we
                  --  know exactly the Name_Id we started with.

                  if not Already_Have_Var then
                     while Last_Values >= Values'First
                       and then Values (Last_Values).Variable_Name = Name
                     loop
                        Last_Values := Last_Values - 1;
                     end loop;
                  end if;

               when N_Typed_Variable_Declaration =>
                  declare
                     Save_Type : constant Project_Node_Id := String_Type_Of
                       (Current, Tree_Node);
                  begin
                     --  Make sure that the type declaration is unique for that
                     --  typed variable, since if we decide to remove the
                     --  variable we should remove the type as well.

                     Var_Type := Clone_Node
                       (Tree_Node, String_Type_Of (Current, Tree_Node), True);
                     Set_Uniq_Type_Name (Tree_Node, Project_Norm, Var_Type);
                     Set_String_Type_Of (Current, Tree_Node, Var_Type);
                     Add_At_End (Tree_Node, Project_Norm, Var_Type,
                                 Add_Before_First_Pkg => True,
                                 Add_Before_First_Case => True);

                     --  Scenario variables must be defined at the project
                     --  level
                     if Current_Pkg /= Empty_Node
                       and then Is_External_Variable (Current, Tree_Node)
                     then
                        Add_At_End
                          (Tree_Node,
                           Project_Norm,
                           Clone_Node (Tree_Node, Decl_Item, True),
                           Add_Before_First_Pkg => True,
                           Add_Before_First_Case => True);
                     else
                        For_Each_Matching_Case_Item
                          (Tree_Node, Project_Norm, Current_Pkg,
                           Case_Construct,
                           Values (Values'First .. Last_Values),
                           Add_Decl_Item'Unrestricted_Access);
                     end if;

                     Set_String_Type_Of (Current, Tree_Node, Save_Type);
                  end;

               when N_String_Type_Declaration =>
                  null;

               when others =>
                  --  Add as many items as possible at once. This speeds things
                  --  up, since we do not have to traverse all the case items
                  --  for all of them.

                  Check_Index_Sensitivity (Decl_Item);

                  Decl_Item2 := Decl_Item;
                  while Next_Item /= Empty_Node loop
                     case Kind_Of
                       (Current_Item_Node (Next_Item, Tree_Node), Tree_Node)
                     is
                        when N_Package_Declaration
                          | N_Case_Construction
                          | N_Typed_Variable_Declaration
                          | N_String_Type_Declaration =>
                           exit;

                        when others =>
                           Check_Index_Sensitivity (Next_Item);
                     end case;

                     Set_Next_Declarative_Item
                       (Decl_Item2, Tree_Node, Next_Item);
                     Decl_Item2 := Next_Item;
                     Next_Item  :=
                       Next_Declarative_Item (Next_Item, Tree_Node);
                  end loop;

                  Set_Next_Declarative_Item
                    (Decl_Item2, Tree_Node, Empty_Node);

                  For_Each_Matching_Case_Item
                    (Tree_Node, Project_Norm, Current_Pkg, Case_Construct,
                     Values (Values'First .. Last_Values),
                     Add_Decl_Item'Unrestricted_Access);
            end case;

            Decl_Item := Next_Item;
         end loop;
      end Process_Declarative_List;

   begin
      if not Project.Data.Normalized then
         Max_Scenario_Variables := Scenario_Variables (Tree)'Length;

         Project_Node := Project.Node;
         Trace (Me, "Normalize: imported=" & Project.Name);

         Values := new External_Variable_Value_Array
           (1 .. Max_Scenario_Variables);
         Last_Values := Values'First - 1;

         Project_Norm := Clone_Project (Tree_Node, Project_Node);
         Current_Pkg := Empty_Node;
         Case_Construct := Empty_Node;

         Project.Data.Normalized := True;

         --  The top-level part of the project
         Process_Declarative_List
           (From => First_Declarative_Item_Of
              (Project_Declaration_Of (Project_Node, Tree_Node), Tree_Node),
            To   => Project_Declaration_Of (Project_Norm, Tree_Node));

         if Last_Values /= Values'First - 1 then
            Free (Values);
            Raise_Exception
              (Normalize_Error'Identity,
               "Internal error while normalizing");
         end if;

         --  All the subpackages. Note that the project parser has at some
         --  point reverted the order of the package nodes, so we need to
         --  take that into account to preserve the order of packages in the
         --  file.

         Current_Pkg := First_Package_Of (Project_Node, Tree_Node);

         while Current_Pkg /= Empty_Node loop
            Decl := First_Declarative_Item_Of (Current_Pkg, Tree_Node);
            Set_First_Declarative_Item_Of
              (Current_Pkg, Tree_Node, Empty_Node);

            Case_Construct := Empty_Node;
            Process_Declarative_List
              (From      => Decl,
               To        => Current_Pkg);

            if Last_Values /= Values'First - 1 then
               Free (Values);
               Raise_Exception
                 (Normalize_Error'Identity,
                  "Internal error while normalizing");
            end if;

            declare
               Next_Pkg : constant Project_Node_Id :=
                 Next_Package_In_Project (Current_Pkg, Tree_Node);
            begin
               Set_Next_Package_In_Project
                 (Current_Pkg, Tree_Node, Empty_Node);
               Add_At_End
                 (Tree_Node,
                  Project_Declaration_Of
                    (Project_Norm, Tree_Node), Current_Pkg,
                  Add_Before_First_Pkg => True,
                  Add_Before_First_Case => False);
               Current_Pkg := Next_Pkg;
            end;
         end loop;

         Free (Values);

         Post_Process_After_Clone (Tree_Node, Project_Norm);

         --  Directly replace in the table, so that all references to this
         --  project are automatically updated. There is a small memory
         --  leak, but since most of the project tree is shared, it doesn't
         --  really matter in the life of the project editor

         Tree_Node.Project_Nodes.Table (Project_Node) :=
           Tree_Node.Project_Nodes.Table (Project_Norm);
         Trace (Me, "Done normalizing " & Project.Name);
      end if;
   end Normalize;

   ----------------
   -- Clone_Node --
   ----------------

   function Clone_Node
     (Tree       : Project_Node_Tree_Ref;
      Node       : Project_Node_Id;
      Deep_Clone : Boolean := False) return Project_Node_Id
   is
      New_Node : Project_Node_Id;

   begin
      if Node = Empty_Node then
         return Empty_Node;
      end if;

      Tree_Private_Part.Project_Node_Table.Increment_Last (Tree.Project_Nodes);
      New_Node :=
        Tree_Private_Part.Project_Node_Table.Last (Tree.Project_Nodes);

      --  Simple copy of all the fields. There is no need to duplicate
      --  Name_Id at this point, since nobody will modify them later on
      --  anyway. So we save some memory and keep them as is.
      --  Only the node ids will need to be copied for deep copies.

      Tree.Project_Nodes.Table (New_Node) := Tree.Project_Nodes.Table (Node);

      if Deep_Clone then
         case Kind_Of (Node, Tree) is
            when N_Project =>
               --  Packages, Variables, First_String_Type_Of must be outside of
               --  this subprogram.
               Set_First_With_Clause_Of
                 (New_Node, Tree,
                  Clone_Node (Tree, First_With_Clause_Of (Node, Tree), True));
               Set_Project_Declaration_Of
                 (New_Node, Tree,
                  Clone_Node (Tree,
                              Project_Declaration_Of (Node, Tree), True));
               Set_First_String_Type_Of (New_Node, Tree, Empty_Node);

            when N_With_Clause =>
               Set_Next_With_Clause_Of
                 (New_Node, Tree,
                  Clone_Node (Tree, Next_With_Clause_Of (Node, Tree), True));

            when N_Project_Declaration =>
               Set_First_Declarative_Item_Of
                 (New_Node, Tree,
                  Clone_Node
                    (Tree, First_Declarative_Item_Of (Node, Tree), True));

            when N_Declarative_Item =>
               Set_Current_Item_Node
                 (New_Node, Tree,
                  Clone_Node (Tree, Current_Item_Node (Node, Tree), True));
               Set_Next_Declarative_Item
                 (New_Node, Tree,
                  Clone_Node (Tree, Next_Declarative_Item (Node, Tree), True));

            when N_Package_Declaration =>
               --  Next_Package_In_Project and Variables must be set outside of
               --  this subprogram
               --  Pkg_Id doesn't need to be cloned, as per 9509-010.
               Set_First_Declarative_Item_Of
                 (New_Node, Tree,
                  Clone_Node
                    (Tree, First_Declarative_Item_Of (Node, Tree), True));
               Set_Next_Package_In_Project (New_Node, Tree, Empty_Node);

            when N_String_Type_Declaration =>
               --  Next_String_Type must be set outside of this
               Set_First_Literal_String
                 (New_Node, Tree,
                  Clone_Node (Tree, First_Literal_String (Node, Tree), True));
               Set_Next_String_Type (New_Node, Tree, Empty_Node);

            when N_Literal_String =>
               Set_Next_Literal_String
                 (New_Node, Tree,
                  Clone_Node (Tree, Next_Literal_String (Node, Tree), True));

            when N_Attribute_Declaration =>
               Set_Expression_Of
                 (New_Node, Tree,
                  Clone_Node (Tree, Expression_Of (Node, Tree), True));

            when N_Typed_Variable_Declaration =>
               --  Next_Variable must be set outside of this
               --  String_Type_Of is set to the same value as for Node, and
               --  this needs to be fixed in a post-processing phase.
               Set_Expression_Of
                 (New_Node, Tree,
                  Clone_Node (Tree, Expression_Of (Node, Tree), True));
               Set_String_Type_Of
                 (New_Node, Tree, String_Type_Of (Node, Tree));
               Set_Next_Variable (New_Node, Tree, Empty_Node);

            when N_Variable_Declaration =>
               --  Next_Variable must be set outside of this
               Set_Expression_Of
                 (New_Node, Tree,
                  Clone_Node (Tree, Expression_Of (Node, Tree), True));
               Set_Next_Variable (New_Node, Tree, Empty_Node);

            when N_Expression =>
               Set_First_Term
                 (New_Node, Tree,
                  Clone_Node (Tree, First_Term (Node, Tree), True));
               Set_Next_Expression_In_List
                 (New_Node, Tree,
                  Clone_Node (Tree,
                              Next_Expression_In_List (Node, Tree), True));

            when N_Term =>
               Set_Current_Term
                 (New_Node, Tree,
                  Clone_Node (Tree, Current_Term (Node, Tree), True));
               Set_Next_Term
                 (New_Node, Tree,
                  Clone_Node (Tree, Next_Term (Node, Tree), True));

            when N_Literal_String_List =>
               Set_First_Expression_In_List
                 (New_Node, Tree,
                  Clone_Node (Tree, First_Expression_In_List (Node, Tree),
                              True));

            when N_Variable_Reference =>
               --  String_Type_Of is set to the same value as for Node, and
               --  this needs to be fixed in a post-processing phase.
               --  Same for Package_Node_Of
               null;

            when N_External_Value =>
               Set_External_Reference_Of
                 (New_Node, Tree,
                  Clone_Node (Tree, External_Reference_Of (Node, Tree), True));
               Set_External_Default_Of
                 (New_Node, Tree,
                  Clone_Node (Tree, External_Default_Of (Node, Tree), True));

            when N_Attribute_Reference =>
               --  Package_Node_Of is set to the same value of for Node, and
               --  this needs to be fixed in a post-processing phase.
               null;

            when N_Case_Construction =>
               Set_Case_Variable_Reference_Of
                 (New_Node, Tree,
                  Clone_Node (Tree,
                              Case_Variable_Reference_Of (Node, Tree), True));
               Set_First_Case_Item_Of
                 (New_Node, Tree,
                  Clone_Node (Tree, First_Case_Item_Of (Node, Tree), True));

            when N_Case_Item =>
               Set_First_Choice_Of
                 (New_Node, Tree,
                  Clone_Node (Tree, First_Choice_Of (Node, Tree), True));
               Set_First_Declarative_Item_Of
                 (New_Node, Tree,
                  Clone_Node
                    (Tree, First_Declarative_Item_Of (Node, Tree), True));

            when N_Comment_Zones =>
               null;

            when N_Comment =>
               null;
         end case;
      end if;

      return New_Node;
   end Clone_Node;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Str : String) return Name_Id is
   begin
      if Str = "" then
         return No_Name;
      else
         Name_Len := Str'Length;
         Name_Buffer (1 .. Name_Len) := Str;
         return Name_Find;
      end if;
   end Get_String;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Id : Namet.Name_Id) return String is
   begin
      if Id = No_Name then
         return "";
      end if;

      return Get_Name_String (Id);
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return "";
   end Get_String;

   --------------------------
   -- Is_External_Variable --
   --------------------------

   function Is_External_Variable
     (Var  : Prj.Tree.Project_Node_Id;
      Tree : Project_Node_Tree_Ref) return Boolean is
   begin
      return Kind_Of
        (Current_Term (First_Term (Expression_Of (Var, Tree), Tree), Tree),
         Tree)
        = N_External_Value;
   end Is_External_Variable;

   ---------------------------
   -- External_Reference_Of --
   ---------------------------

   function External_Reference_Of
     (Var  : Prj.Tree.Project_Node_Id;
      Tree : Project_Node_Tree_Ref) return Namet.Name_Id
   is
      Expr : Project_Node_Id := Expression_Of (Var, Tree);
   begin
      Expr := First_Term   (Expr, Tree);
      Expr := Current_Term (Expr, Tree);

      if Kind_Of (Expr, Tree) = N_External_Value then
         Expr := External_Reference_Of (Expr, Tree);
         return String_Value_Of (Expr, Tree);
      else
         return No_Name;
      end if;
   end External_Reference_Of;

   -----------------------------
   -- Find_Project_Of_Package --
   -----------------------------

   function Find_Project_Of_Package
     (Data     : Project_Tree_Data_Access;
      Project  : Project_Type;
      Pkg_Name : String) return Project_Type
   is
      Pkg : Project_Node_Id;
   begin
      if Pkg_Name /= "" then
         Pkg := Find_Package_Declaration
           (Tree (Data), Project.Node, Get_String (Pkg_Name));

         if Pkg /= Empty_Node then
            Pkg := Project_Of_Renamed_Package_Of (Pkg, Tree (Data));
            if Pkg /= Empty_Node then
               return Project_Type
                 (Project_From_Name
                    (Data, Prj.Tree.Name_Of (Pkg, Tree (Data))));
            end if;
         end if;
      end if;

      return Project;
   end Find_Project_Of_Package;

   -------------------------
   -- Find_Case_Statement --
   -------------------------

   function Find_Case_Statement
     (Tree    : Project_Node_Tree_Ref;
      Project : Prj.Tree.Project_Node_Id;
      Pkg     : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node)
      return Project_Node_Id
   is
      Top            : Project_Node_Id;
      Decl_Item      : Project_Node_Id;
      Case_Construct : Project_Node_Id := Empty_Node;
   begin
      if Pkg /= Empty_Node then
         Top := Pkg;
      else
         Top := Project_Declaration_Of (Project, Tree);
      end if;

      Decl_Item := First_Declarative_Item_Of (Top, Tree);
      while Decl_Item /= Empty_Node loop
         if Kind_Of (Current_Item_Node (Decl_Item, Tree), Tree) =
           N_Case_Construction
         then
            Case_Construct := Current_Item_Node (Decl_Item, Tree);
            exit;
         end if;
         Decl_Item := Next_Declarative_Item (Decl_Item, Tree);
      end loop;

      return Case_Construct;
   end Find_Case_Statement;

   ----------------------------------------
   -- Move_From_Common_To_Case_Construct --
   ----------------------------------------

   procedure Move_From_Common_To_Case_Construct
     (Tree               : Project_Node_Tree_Ref;
      Project            : Project_Node_Id;
      Pkg                : Project_Node_Id;
      Case_Construct     : in out Project_Node_Id;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute_Name     : Namet.Name_Id;
      Attribute_Index    : Namet.Name_Id := No_Name)
   is
      Parent          : Project_Node_Id;
      Node, Tmp       : Project_Node_Id;
      Case_Items      : Project_Node_Array_Access :=
                          new Project_Node_Array (1 .. 100);
      Case_Items_Last : Natural := Case_Items'First - 1;

      procedure Add_Item (Case_Item : Project_Node_Id);
      --  Add the declaration Node to Case_Item, in front, since the
      --  declaration necessarily occured before the case item

      --------------
      -- Add_Item --
      --------------

      procedure Add_Item (Case_Item : Project_Node_Id) is
      begin
         Add_Node_To_List (Case_Items, Case_Items_Last, Case_Item);
      end Add_Item;

   begin
      if Pkg /= Empty_Node then
         Parent := Pkg;
      else
         Parent := Project_Declaration_Of (Project, Tree);
      end if;

      --  First, create the nested case for the scenario, and memorize each of
      --  them. This will easily allow to keep the order of all the
      --  declarations for this attribute that are currently in the common part
      For_Each_Scenario_Case_Item
        (Tree, Project, Pkg, Case_Construct, Scenario_Variables, null);
      For_Each_Matching_Case_Item
        (Tree, Project, Pkg, Case_Construct,
         All_Case_Items, Add_Item'Unrestricted_Access);

      --  Nothing to do if there are no case items
      if Case_Items_Last > Case_Items'First then
         Node := First_Declarative_Item_Of (Parent, Tree);

         while Node /= Empty_Node loop
            if Attribute_Matches
              (Tree, Current_Item_Node (Node, Tree),
               Attribute_Name, Attribute_Index)
            then
               for Parent in Case_Items'First .. Case_Items_Last loop
                  Tmp := Default_Project_Node (Tree, N_Declarative_Item);
                  Set_Current_Item_Node
                    (Tmp, Tree,
                     Clone_Node (Tree, Current_Item_Node (Node, Tree), True));

                  if Kind_Of (Case_Items (Parent), Tree) /=
                    N_Declarative_Item
                  then
                     Set_Next_Declarative_Item
                       (Tmp, Tree,
                        First_Declarative_Item_Of (Case_Items (Parent), Tree));
                     Set_First_Declarative_Item_Of
                       (Case_Items (Parent), Tree, Tmp);

                  else
                     Set_Next_Declarative_Item
                       (Tmp, Tree,
                        Next_Declarative_Item (Case_Items (Parent), Tree));
                     Set_Next_Declarative_Item
                       (Case_Items (Parent), Tree, Tmp);
                  end if;
                  Case_Items (Parent) := Tmp;
               end loop;
            end if;

            Node := Next_Declarative_Item (Node, Tree);
         end loop;

         Remove_Attribute_Declarations
           (Tree, Parent, Attribute_Name, Attribute_Index);
      end if;

      Free (Case_Items);
   end Move_From_Common_To_Case_Construct;

   ----------------------------
   -- Internal_Set_Attribute --
   ----------------------------

   procedure Internal_Set_Attribute
     (Tree      : Project_Tree_Data_Access;
      Project   : Project_Type;
      Attribute : String;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "";
      Callback  : Set_Attribute_Callback)
   is
      Tree_Node      : constant Project_Node_Tree_Ref :=
        GNATCOLL.Projects.Tree (Tree);
      Sep            : constant Natural :=
        Ada.Strings.Fixed.Index (Attribute, "#");
      Attribute_Name : constant Name_Id :=
        Get_String (Attribute (Sep + 1 .. Attribute'Last));
      Pkg_Name       : constant String :=
        Attribute (Attribute'First .. Sep - 1);
      Pkg_Prj        : constant Project_Type :=
        Find_Project_Of_Package (Tree, Project, Pkg_Name);
      Index_Id       : constant Name_Id := Get_String (Index);

      Pkg            : Project_Node_Id := Empty_Node;
      Case_Construct : Project_Node_Id;

      procedure Internal_Cb (Case_Item : Project_Node_Id);
      procedure Internal_Cb (Case_Item : Project_Node_Id) is
         Previous_Decl : Project_Node_Id;
      begin
         Previous_Decl := Find_Last_Declaration_Of
           (Tree_Node, Case_Item, Attribute_Name, Index_Id);

         if Previous_Decl /= Empty_Node then
            Previous_Decl := Expression_Of (Previous_Decl, Tree_Node);
         end if;

         Callback (Tree_Node, Pkg_Prj, Attribute_Name, Index_Id,
                   Previous_Decl, Case_Item);
      end Internal_Cb;

   begin
      if not Pkg_Prj.Is_Editable then
         Trace (Me, "Project is not editable");
      else
         Normalize (Tree, Pkg_Prj);

         if Pkg_Name /= "" then
            Pkg := Create_Package (Tree_Node, Pkg_Prj.Node, Pkg_Name);
         end if;

         Case_Construct := Find_Case_Statement (Tree_Node, Pkg_Prj.Node, Pkg);

         Move_From_Common_To_Case_Construct
           (Tree_Node,
            Project            => Pkg_Prj.Node,
            Pkg                => Pkg,
            Case_Construct     => Case_Construct,
            Scenario_Variables => Scenario,
            Attribute_Name     => Attribute_Name,
            Attribute_Index    => Index_Id);

         --  Create the node for the new value

         For_Each_Scenario_Case_Item
           (Tree_Node, Pkg_Prj.Node, Pkg, Case_Construct, Scenario,
            Internal_Cb'Unrestricted_Access);

         Pkg_Prj.Data.Modified := True;
      end if;
   end Internal_Set_Attribute;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute
     (Tree      : Project_Tree_Data_Access;
      Project   : Project_Type;
      Attribute : Attribute_Pkg_String;
      Value     : String;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "";
      At_Index  : Natural := 0)
   is
      V : Name_Id;

      Local_Val : constant String :=
        +Unix_Style_Full_Name (Create (+Value), Cygwin_Style => True);

      procedure Add_Or_Replace
        (Tree_Node      : Project_Node_Tree_Ref;
         Project        : Project_Type;
         Attribute_Name : Name_Id;
         Index_Id       : Name_Id;
         Previous_Decl  : Project_Node_Id;
         Case_Item      : Project_Node_Id);
      --  Add or replace the attribute Attribute_Name in the declarative list
      --  for Case_Item

      procedure Add_Or_Replace
        (Tree_Node      : Project_Node_Tree_Ref;
         Project        : Project_Type;
         Attribute_Name : Name_Id;
         Index_Id       : Name_Id;
         Previous_Decl  : Project_Node_Id;
         Case_Item      : Project_Node_Id)
      is
         Decl, Val : Project_Node_Id;
         pragma Unreferenced (Decl, Project);
      begin
         Val := Create_Literal_String (V, Tree_Node);

         if Previous_Decl /= Empty_Node then
            --  ??? Should we use At_Index here ?
            Set_Current_Term
              (First_Term (Previous_Decl, Tree_Node), Tree_Node, Val);
            Set_Source_Index_Of (Val, Tree_Node, Int (At_Index));
         else
            Decl := Create_Attribute
              (Tree_Node, Case_Item, Attribute_Name,
               Index_Id, Prj.Single, Value => Val, At_Index => At_Index);
         end if;
      end Add_Or_Replace;

   begin
      if Local_Val = "" then
         V := Empty_String;
      else
         V := Get_String (Local_Val);
      end if;

      Internal_Set_Attribute
        (Tree      => Tree,
         Project   => Project,
         Attribute => String (Attribute),
         Scenario  => Scenario,
         Index     => Index,
         Callback  => Add_Or_Replace'Unrestricted_Access);
   end Set_Attribute;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute
     (Tree      : Project_Tree_Data_Access;
      Project   : Project_Type;
      Attribute : Attribute_Pkg_List;
      Values    : GNAT.Strings.String_List;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "";
      Prepend   : Boolean := False)
   is
      List : Project_Node_Id := Empty_Node;

      function Convert_Dir_Separators
        (Values : GNAT.Strings.String_List)
      return GNAT.Strings.String_List;
      --  Replace all "\" with "/".

      procedure Add_Or_Replace
        (Tree_Node      : Project_Node_Tree_Ref;
         Project        : Project_Type;
         Attribute_Name : Name_Id;
         Index_Id       : Name_Id;
         Previous_Decl  : Project_Node_Id;
         Case_Item      : Project_Node_Id);
      --  Add or replace the attribute Attribute_Name in the declarative list
      --  for Case_Item

      function Convert_Dir_Separators
        (Values : GNAT.Strings.String_List)
         return GNAT.Strings.String_List
      is
         Result : GNAT.Strings.String_List (Values'Range);
      begin
         for A in Values'Range loop
            if Values (A) /= null then
               Result (A) := new String'
                 (+Unix_Style_Full_Name
                    (Create (+Values (A).all), Cygwin_Style => True));
            end if;
         end loop;

         return Result;
      end Convert_Dir_Separators;

      Vals : constant GNAT.Strings.String_List :=
        Convert_Dir_Separators (Values);

      procedure Add_Or_Replace
        (Tree_Node      : Project_Node_Tree_Ref;
         Project        : Project_Type;
         Attribute_Name : Name_Id;
         Index_Id       : Name_Id;
         Previous_Decl  : Project_Node_Id;
         Case_Item      : Project_Node_Id)
      is
         Decl, Expr, Term : Project_Node_Id;
         pragma Unreferenced (Decl);
      begin
         if List = Empty_Node then
            --  Create the string list for the new values.
            --  This can be prepended later on to the existing list of values.

            List := Default_Project_Node
              (Tree_Node, N_Literal_String_List, Prj.List);

            for A in reverse Vals'Range loop
               if Vals (A) /= null then
                  Expr := String_As_Expression
                    (Get_String (Vals (A).all), Tree_Node);
                  Set_Next_Expression_In_List
                    (Expr, Tree_Node,
                     First_Expression_In_List (List, Tree_Node));
                  Set_First_Expression_In_List (List, Tree_Node, Expr);
               end if;
            end loop;
         end if;

         if Previous_Decl /= Empty_Node then
            if Prepend then
               Expr := First_Expression_In_List (List, Tree_Node);
               while Next_Expression_In_List (Expr, Tree_Node) /=
                 Empty_Node
               loop
                  Expr := Next_Expression_In_List (Expr, Tree_Node);
               end loop;

               Set_Next_Expression_In_List
                 (Expr, Tree_Node, First_Expression_In_List
                    (Current_Term (First_Term (Previous_Decl, Tree_Node),
                                   Tree_Node),
                    Tree_Node));
            else
               Set_Next_Expression_In_List
                 (Previous_Decl, Tree_Node, Empty_Node);
               Set_Next_Term
                 (First_Term (Previous_Decl, Tree_Node), Tree_Node,
                  Empty_Node);
            end if;

            Set_Current_Term
              (First_Term (Previous_Decl, Tree_Node), Tree_Node, List);

         --  Else create the new instruction to be added to the project

         else
            Expr := Enclose_In_Expression (List, Tree_Node);

            if Prepend then
               Set_Next_Term
                 (First_Term (Expr, Tree_Node), Tree_Node,
                  Default_Project_Node (Tree_Node, N_Term, Prj.List));
               Term := Next_Term (First_Term (Expr, Tree_Node), Tree_Node);
               Set_Current_Term
                 (Term,
                  Tree_Node,
                  Default_Project_Node
                    (Tree_Node, N_Attribute_Reference, Prj.List));
               Term := Current_Term (Term, Tree_Node);

               Set_Name_Of (Term, Tree_Node, Attribute_Name);
               Set_Project_Node_Of (Term, Tree_Node, Project.Node);
            end if;

            Decl := Create_Attribute
              (Tree_Node, Case_Item, Attribute_Name, Index_Id, Value => Expr);
         end if;
      end Add_Or_Replace;

   begin
      Internal_Set_Attribute
        (Tree      => Tree,
         Project   => Project,
         Attribute => String (Attribute),
         Scenario  => Scenario,
         Index     => Index,
         Callback  => Add_Or_Replace'Unrestricted_Access);
   end Set_Attribute;

   ----------------------
   -- Delete_Attribute --
   ----------------------

   procedure Delete_Attribute
     (Tree      : Project_Tree_Data_Access;
      Project   : Project_Type;
      Attribute : String;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "")
   is
      Tree_Node      : constant Project_Node_Tree_Ref :=
        GNATCOLL.Projects.Tree (Tree);
      Sep            : constant Natural :=
        Ada.Strings.Fixed.Index (Attribute, "#");
      Attribute_Name : constant Name_Id :=
        Get_String (Attribute (Sep + 1 .. Attribute'Last));
      Pkg_Name       : constant String :=
        Attribute (Attribute'First .. Sep - 1);
      Pkg_Prj        : constant Project_Type :=
        Find_Project_Of_Package (Tree, Project, Pkg_Name);

      Pkg            : Project_Node_Id := Empty_Node;
      Case_Construct : Project_Node_Id;
      Index_Id       : constant Name_Id := Get_String (Index);

      procedure Delete_Attr (Case_Item : Project_Node_Id);
      --  Remove all definitions for the attribute in the case item

      -----------------
      -- Delete_Attr --
      -----------------

      procedure Delete_Attr (Case_Item : Project_Node_Id) is
      begin
         Remove_Attribute_Declarations
           (Tree_Node, Case_Item, Attribute_Name, Index_Id);
      end Delete_Attr;

   begin
      if not Pkg_Prj.Is_Editable then
         Trace (Me, "Project is not editable");
      else
         if Pkg_Name /= "" then
            Pkg := Find_Package_Declaration
              (Tree_Node, Pkg_Prj.Node, Get_String (Pkg_Name));

            --  If the package doesn't exist, no need to do anything
            if Pkg = Empty_Node then
               Trace (Me, "Delete attribute '"
                      & Get_Name_String (Attribute_Name)
                      & "': No such package '"
                      & Pkg_Name & "'");
               return;
            end if;
         end if;

         Normalize (Tree, Pkg_Prj);

         Case_Construct := Find_Case_Statement (Tree_Node, Pkg_Prj.Node, Pkg);
         Move_From_Common_To_Case_Construct
           (Tree_Node, Pkg_Prj.Node, Pkg, Case_Construct, Scenario,
            Attribute_Name, Index_Id);
         For_Each_Scenario_Case_Item
           (Tree_Node, Pkg_Prj.Node, Pkg, Case_Construct, Scenario,
            Delete_Attr'Unrestricted_Access);

         Pkg_Prj.Data.Modified := True;
      end if;
   end Delete_Attribute;

   -----------------------------
   -- For_Each_Directory_Node --
   -----------------------------

   procedure For_Each_Directory_Node
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Type;
      Action  : Node_Callback)
   is
      procedure Process_List (List : Project_Node_Id);
      --  Process a list of declarative items

      ------------------
      -- Process_List --
      ------------------

      procedure Process_List (List : Project_Node_Id) is
         Node : Project_Node_Id := List;
         Current, Expr, Term, Expr2 : Project_Node_Id;
      begin
         while Node /= Empty_Node loop
            Current := Current_Item_Node (Node, Tree);

            case Kind_Of (Current, Tree) is
               when N_Attribute_Declaration =>
                  --  ??? Should avoid a hard-coded list of directory
                  --  attributes. Ideally, we would take into account
                  --  the attributes defined in the XML file
                  if Prj.Tree.Name_Of (Current, Tree) = Snames.Name_Source_Dirs
                    or else Prj.Tree.Name_Of (Current, Tree) =
                      Snames.Name_Object_Dir
                    or else Prj.Tree.Name_Of (Current, Tree) =
                      Snames.Name_Exec_Dir
                  then
                     Expr := Expression_Of (Current, Tree);
                     while Expr /= Empty_Node loop
                        Term := First_Term (Expr, Tree);
                        while Term /= Empty_Node loop
                           Current := Current_Term (Term, Tree);

                           case Kind_Of (Current, Tree) is
                              when N_Literal_String_List =>
                                 Expr2 := First_Expression_In_List
                                   (Current, Tree);
                                 while Expr2 /= Empty_Node loop
                                    Current := Current_Term
                                      (First_Term (Expr2, Tree), Tree);
                                    if Kind_Of (Current, Tree) /=
                                      N_Literal_String
                                      or else Next_Term
                                        (First_Term (Expr2, Tree), Tree) /=
                                        Empty_Node
                                    then
                                       Trace
                                         (Me, "Cannot process lists of "
                                          & " non-literal string "
                                          & Kind_Of (Current, Tree)'Img);
                                    else
                                       Action (Current);
                                    end if;

                                    Expr2 := Next_Expression_In_List
                                      (Expr2, Tree);
                                 end loop;

                              when N_Literal_String =>
                                 Action (Current);

                              when others =>
                                 Trace (Me, "Ignoring "
                                        & Kind_Of (Current, Tree)'Img);
                                 null;
                           end case;

                           Term := Next_Term (Term, Tree);
                        end loop;

                        Expr := Next_Expression_In_List (Expr, Tree);
                     end loop;
                  end if;

               when N_Case_Construction =>
                  Expr := First_Case_Item_Of (Current, Tree);
                  while Expr /= Empty_Node loop
                     Process_List (First_Declarative_Item_Of (Expr, Tree));
                     Expr := Next_Case_Item (Expr, Tree);
                  end loop;

               when others =>
                  null;
            end case;

            Node := Next_Declarative_Item (Node, Tree);
         end loop;

      end Process_List;

   begin
      Process_List (First_Declarative_Item_Of
           (Project_Declaration_Of (Project.Node, Tree), Tree));
   end For_Each_Directory_Node;

   ---------------------
   -- Rename_And_Move --
   ---------------------

   procedure Rename_And_Move
     (Tree          : Project_Tree_Data_Access;
      Project       : Project_Type;
      New_Name      : String;
      New_Path      : GNATCOLL.VFS.Virtual_File;
      Errors        : Error_Report := null)
   is
      Use_Relative_Path : constant Boolean := True;
      --  Whether to use relative paths when we have to modify with clauses

      Tree_Node : constant Project_Node_Tree_Ref :=
        GNATCOLL.Projects.Tree (Tree);
      Old_Path : constant Filesystem_String := Project.Project_Path.Dir_Name;

      New_Dir : Virtual_File;

      procedure Change_Directory (Node : Project_Node_Id);
      --  Change the directory refered to by Node

      ----------------------
      -- Change_Directory --
      ----------------------

      procedure Change_Directory (Node : Project_Node_Id) is
      begin
         case Kind_Of (Node, Tree_Node) is
            when N_Literal_String =>
               declare
                  D : constant Filesystem_String :=
                        +Get_Name_String (String_Value_Of (Node, Tree_Node));
                  File : constant Virtual_File :=
                           Create
                             (Normalize_Pathname
                                (D, Old_Path, Resolve_Links => False),
                              Get_Host (New_Dir));
               begin
                  if not Is_Absolute_Path (D) then
                     Set_String_Value_Of
                       (Node, Tree_Node,
                        Get_String (+Relative_Path (File, New_Dir)));
                  end if;
               end;

            when others =>
               Trace (Me, "For_Each_Directory_Node: unknown node type: "
                      & Kind_Of (Node, Tree_Node)'Img);
         end case;
      end Change_Directory;

      Full_Path   : Name_Id;
      Name        : constant Name_Id := Get_String (New_Name);
      Old_Name    : constant Name_Id := Get_String (Project.Name);
      Old         : constant Project_Type :=
                      Project_Type (Project_From_Name (Tree, Name));
      Imported    : Project_Type;
      Iterator    : Project_Iterator;
      With_Clause : Project_Node_Id;
      Modified    : Boolean;
      P           : Path_Name_Type;

   begin
      if New_Path = GNATCOLL.VFS.No_File then
         New_Dir := Create (Old_Path);
      else
         New_Dir := New_Path;
      end if;

      if Project = No_Project then
         Trace (Me, "Unspecified project to remove");
         return;
      end if;

      if Old /= No_Project then
         Trace (Me, "Rename_And_Move: project " & New_Name
                & " already exists in the hierarchy");
         if Errors /= null then
            Errors
              ("Couldn't rename the project to " & New_Name
               & ASCII.LF & "Project already exists in the project graph");
         end if;
         return;
      end if;

      --  Replace all the with_clauses in the project hierarchy that points to
      --  Project.

      Iterator := Find_All_Projects_Importing
        (Project, Direct_Only => False);

      Full_Path := Get_String
         (+(Name_As_Directory (Full_Name (New_Dir))
            & (+Translate (To_Lower (New_Name), To_Mapping (".", "-")))
            & GNATCOLL.Projects.Project_File_Extension));

      loop
         Imported := Current (Iterator);
         exit when Imported = GNATCOLL.Projects.No_Project;

         With_Clause := First_With_Clause_Of (Imported.Node, Tree_Node);
         Modified := False;

         while With_Clause /= Empty_Node loop
            if Project_Node_Of (With_Clause, Tree_Node) = Project.Node then
               Set_Name_Of (With_Clause, Tree_Node, Name);
               Set_Path_Name_Of (With_Clause, Tree_Node,
                                 Path_Name_Of (Project.Node, Tree_Node));

               if Use_Relative_Path then
                  Set_String_Value_Of
                    (With_Clause, Tree_Node,
                     Get_String
                       (+Relative_Path
                            (File => Create (+Get_String (Full_Path)),
                             From => Create
                               (Imported.Project_Path.Dir_Name))));
               else
                  Set_String_Value_Of (With_Clause, Tree_Node, Full_Path);
               end if;
               Modified := True;
            end if;

            With_Clause := Next_With_Clause_Of (With_Clause, Tree_Node);
         end loop;

         if Modified then
            Imported.Data.Modified := True;
         end if;

         Next (Iterator);
      end loop;

      --  If the file was moved, update the source directories so that we still
      --  point to the same physical directories.

      if New_Dir.Full_Name /= Old_Path then
         For_Each_Directory_Node
           (Tree_Node, Project, Change_Directory'Unrestricted_Access);
      end if;

      Set_Name_Of (Project.Node, Tree_Node, Name);
      Set_Display_Name_Of (Project.Node, Tree_Node, Name);
      Set_Directory_Of
        (Project.Node, Tree_Node,
         Path_Name_Type (Get_String (+New_Dir.Full_Name)));

      P := Path_Name_Type (Full_Path);
      Set_Path_Name_Of (Project.Node, Tree_Node, P);

      if Get_View (Project) /= Prj.No_Project then
         Get_View (Project).Path := (Name => P, Display_Name => P);
      end if;

      --  We do not want to reread the display_name from the source, which is
      --  no longer up-to-date, so we'll force its refresh from the tree
      Set_Location_Of (Project.Node, Tree_Node, No_Location);

      --  Unregister the old name
      Prj.Tree.Tree_Private_Part.Projects_Htable.Set
        (Tree_Node.Projects_HT,
         Prj.Tree.Name_Of (Project.Node, Tree_Node),
         Prj.Tree.Tree_Private_Part.Project_Name_And_Node'
         (Name           => Old_Name,
          Node           => Empty_Node,
          Resolved_Path  => Path_Name_Type (Old_Name),
          Extended       => False,
          From_Extended  => False,
          Proj_Qualifier => Unspecified));

      --  Register the new name
      Prj.Tree.Tree_Private_Part.Projects_Htable.Set
        (Tree_Node.Projects_HT,
         Prj.Tree.Name_Of (Project.Node, Tree_Node),
         Prj.Tree.Tree_Private_Part.Project_Name_And_Node'
         (Name           => Name,
          Resolved_Path  => Path_Name_Type (Full_Path),
          Node           => Project.Node,
          Extended       => False,
          From_Extended  => False,
          Proj_Qualifier => Unspecified));

      Project.Data.Modified := True;
      Unchecked_Free (Project.Data.Imported_Projects);
      Unchecked_Free (Project.Data.Importing_Projects);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Rename_And_Move;

   ---------------------
   -- Normalize_Cases --
   ---------------------

   procedure Normalize_Cases
     (Tree    : Prj.Tree.Project_Node_Tree_Ref;
      Project : Project_Type)
   is
      procedure Process_Declarative_List (Node : Project_Node_Id);
      --  Check all case statements in the declarative list

      ------------------------------
      -- Process_Declarative_List --
      ------------------------------

      procedure Process_Declarative_List (Node : Project_Node_Id) is
         Decl_Item, Current : Project_Node_Id := Node;
      begin
         --  Nothing to do if there is no project
         if Node = Empty_Node then
            return;
         end if;

         pragma Assert (Kind_Of (Decl_Item, Tree) = N_Declarative_Item);

         while Decl_Item /= Empty_Node loop
            Current := Current_Item_Node (Decl_Item, Tree);
            exit when Current = Empty_Node;

            case Kind_Of (Current, Tree) is
               when N_Package_Declaration =>
                  Process_Declarative_List
                    (First_Declarative_Item_Of (Current, Tree));
               when N_Case_Construction =>
                  declare
                     Values : Name_Id_Array := Get_All_Possible_Values
                       (Tree, Case_Variable_Reference_Of (Current, Tree));
                     Case_Item : Project_Node_Id :=
                       First_Case_Item_Of (Current, Tree);
                     Choice    : Project_Node_Id;
                  begin
                     while Case_Item /= Empty_Node loop
                        Choice := First_Choice_Of (Case_Item, Tree);
                        while Choice /= Empty_Node loop
                           for N in Values'Range loop
                              if Values (N) =
                                String_Value_Of (Choice, Tree)
                              then
                                 Values (N) := No_Name;
                                 exit;
                              end if;
                           end loop;
                           Choice := Next_Literal_String (Choice, Tree);
                        end loop;

                        Process_Declarative_List
                          (First_Declarative_Item_Of (Case_Item, Tree));

                        Case_Item := Next_Case_Item (Case_Item, Tree);
                     end loop;

                     for V in Values'Range loop
                        if Values (V) /= No_Name then
                           Add_Case_Item
                             (Tree      => Tree,
                              Case_Node => Current,
                              Choice    => Values (V));
                        end if;
                     end loop;
                  end;

               when others =>
                  null;
            end case;

            Decl_Item := Next_Declarative_Item (Decl_Item, Tree);
         end loop;
      end Process_Declarative_List;

   begin
      Process_Declarative_List
        (First_Declarative_Item_Of
           (Project_Declaration_Of (Project.Node, Tree), Tree));
   end Normalize_Cases;

   --------------------------
   -- Set_With_Clause_Path --
   --------------------------

   procedure Set_With_Clause_Path
     (Tree                      : Project_Node_Tree_Ref;
      With_Clause               : Project_Node_Id;
      Imported_Project_Location : Virtual_File;
      Imported_Project          : Project_Node_Id;
      Importing_Project         : Project_Node_Id;
      Use_Relative_Path         : Boolean;
      Use_Base_Name             : Boolean;
      Limited_With              : Boolean := False)
   is
      Clause : Name_Id;
   begin
      if Use_Base_Name then
         Clause := Get_String (+Base_Name (Imported_Project_Location));

      elsif Use_Relative_Path then
         Clause := Get_String
           (+Imported_Project_Location.Relative_Path
              (Create (GNATCOLL.VFS_Utils.Dir_Name
               (+Get_String
                  (Name_Id (Path_Name_Of (Importing_Project, Tree)))))));

      else
         Clause := Get_String (+Imported_Project_Location.Full_Name);
      end if;

      Set_String_Value_Of (With_Clause, Tree, Clause);

      Set_Path_Name_Of (With_Clause, Tree,
                        Prj.Tree.Path_Name_Of (Imported_Project, Tree));
      Set_Project_Node_Of (With_Clause, Tree, Imported_Project,
                           Limited_With => Limited_With);
   end Set_With_Clause_Path;

   --------------------------
   -- Add_Imported_Project --
   --------------------------

   function Add_Imported_Project
     (Tree                      : Project_Tree_Data_Access;
      Project                   : Project_Type'Class;
      Imported_Project          : Project_Type'Class;
      Errors                    : Error_Report := null;
      Use_Relative_Path         : Boolean;
      Use_Base_Name             : Boolean;
      Limited_With              : Boolean := False)
      return Import_Project_Error
   is
      use Prj.Tree.Tree_Private_Part;
      Tree_Node : constant Project_Node_Tree_Ref :=
        GNATCOLL.Projects.Tree (Tree);

      procedure Fail (S : String);
      --  Replaces Osint.Fail

      ----------
      -- Fail --
      ----------

      procedure Fail (S : String) is
      begin
         if Errors /= null then
            Errors (S);
         end if;
      end Fail;

      With_Clause : Project_Node_Id;
      Imported_Name : constant Name_Id :=
        Prj.Tree.Name_Of (Imported_Project.Node, Tree_Node);

   begin
      Output.Set_Special_Output (Output.Output_Proc (Errors));
      Prj.Com.Fail := Fail'Unrestricted_Access;

      --  Make sure we are not trying to import ourselves, since otherwise it
      --  would result in an infinite loop when manipulating the project.

      if Prj.Tree.Name_Of (Project.Data.Node, Tree_Node) =
        Imported_Name
      then
         Fail ("Cannot add dependency to self");
         Output.Cancel_Special_Output;
         Prj.Com.Fail := null;
         return Dependency_On_Self;
      end if;

      --  Check if it is already there. If we have the same name but not the
      --  same path, we replace it anyway.

      With_Clause := First_With_Clause_Of (Project.Node, Tree_Node);

      while With_Clause /= Empty_Node loop
         if Prj.Tree.Name_Of
           (Project_Node_Of (With_Clause, Tree_Node), Tree_Node) =
           Imported_Name
         then
            Fail ("There is already a dependency on "
                  & Imported_Project.Name);
            Output.Cancel_Special_Output;
            Prj.Com.Fail := null;
            return Dependency_Already_Exists;
         end if;
         With_Clause := Next_With_Clause_Of (With_Clause, Tree_Node);
      end loop;

      --  Would we introduce a circular reference by adding this project ?

      if Project.Data.Importing_Projects /= null then
         for P in Project.Data.Importing_Projects'Range loop
            if To_Lower (Project_From_Path
                (Tree, Project.Data.Importing_Projects (P)).Name) =
              To_Lower (Get_Name_String (Imported_Name))
            then
               Fail ("Circular dependency detected in the project hierarchy");
               Output.Cancel_Special_Output;
               Prj.Com.Fail := null;
               return Circular_Dependency;
            end if;
         end loop;
      end if;

      --  Edit the project

      With_Clause := Default_Project_Node (Tree_Node, N_With_Clause);
      Set_Name_Of (With_Clause, Tree_Node, Imported_Name);

      Set_Next_With_Clause_Of
        (With_Clause, Tree_Node,
         First_With_Clause_Of (Project.Node, Tree_Node));
      Set_First_With_Clause_Of (Project.Node, Tree_Node, With_Clause);

      Set_With_Clause_Path
        (Tree_Node, With_Clause,
         Imported_Project.Project_Path,
         Imported_Project.Node, Project.Node,
         Use_Relative_Path => Use_Relative_Path,
         Use_Base_Name     => Use_Base_Name,
         Limited_With      => Limited_With);

      Output.Cancel_Special_Output;
      Prj.Com.Fail := null;
      Project.Data.Modified := True;
      Reset_All_Caches (Tree);
      return Success;

   exception
      when others =>
         Output.Cancel_Special_Output;
         Prj.Com.Fail := null;
         raise;
   end Add_Imported_Project;

   ------------------------------
   -- Delete_Scenario_Variable --
   ------------------------------

   procedure Delete_Scenario_Variable
     (Tree                     : Project_Tree_Data_Access;
      Root_Project             : Project_Type;
      External_Name            : String;
      Keep_Choice              : String;
      Delete_Direct_References : Boolean := True)
   is
      Tree_Node : constant Project_Node_Tree_Ref :=
        GNATCOLL.Projects.Tree (Tree);

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable

      --------------
      -- Callback --
      --------------

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Choice);
      begin
         case Kind_Of (Node, Tree_Node) is
            when N_External_Value =>
               if Delete_Direct_References then
                  Set_Current_Term
                    (Parent, Tree_Node,
                     Create_Literal_String
                       (Get_String (Keep_Choice), Tree_Node));
               end if;

            when N_Variable_Reference =>
               Set_Current_Term
                 (Parent, Tree_Node,
                  Create_Literal_String (Get_String (Keep_Choice), Tree_Node));

            when N_Typed_Variable_Declaration =>
               Remove_Node (Tree_Node, Project, Node);
               Remove_Variable_Declaration (Tree_Node, Project, Node);

            when N_String_Type_Declaration =>
               Remove_Node (Tree_Node, Project, Node);

            when N_Case_Item =>
               --  The first declarative item might be null when there was no
               --  actual "when ..." for Keep_Choice. In that case, Prj.Proc
               --  inserts an entry with no declarative item.

               if First_Declarative_Item_Of (Node, Tree_Node) /=
                 Empty_Node
               then
                  Tree_Node.Project_Nodes.Table (Parent) :=
                    Tree_Node.Project_Nodes.Table
                      (First_Declarative_Item_Of (Node, Tree_Node));

               else
                  Set_Current_Item_Node (Parent, Tree_Node, Empty_Node);
               end if;

            when others =>
               null;
               pragma Assert (False, "Unexpected node type");

         end case;
      end Callback;

   begin
      Normalize (Tree, Root_Project);
      For_Each_Environment_Variable
        (Tree_Node, Root_Project, Get_String (External_Name),
         Get_String (Keep_Choice), Callback'Unrestricted_Access);
   end Delete_Scenario_Variable;

   -----------------
   -- Rename_Path --
   -----------------

   function Rename_Path
     (Tree               : Project_Tree_Data_Access;
      Project            : Project_Type;
      Old_Path           : Virtual_File;
      New_Path           : Virtual_File;
      Use_Relative_Paths : Boolean) return Boolean
   is
      Tree_Node    : constant Project_Node_Tree_Ref :=
        GNATCOLL.Projects.Tree (Tree);
      Changed : Boolean := False;

      procedure Rename_P (Node : Project_Node_Id);
      --  Convert the path to an absolute path

      function Path (P : Filesystem_String) return Filesystem_String;
      --  Returns the path with relative or absolute convention

      ----------
      -- Path --
      ----------

      function Path (P : Filesystem_String) return Filesystem_String is
      begin
         --  We use to test whether P is an url, but in the context of projects
         --  we only have directories anyway

         if Use_Relative_Paths
           and then Is_Absolute_Path (P)
         then
            declare
               File : constant Virtual_File :=
                        Create (P, Project_Path (Project).Get_Host);
               Conv : constant Filesystem_String :=
                        Relative_Path (File, Dir (Project_Path (Project)));
            begin
               return Conv;
            end;
         elsif not Use_Relative_Paths then
            declare
               Conv : constant Filesystem_String := Normalize_Pathname
                 (P, Dir (Project_Path (Project)).Full_Name);
            begin
               return Conv;
            end;
         end if;

         return P;
      end Path;

      Old_P : constant Filesystem_String := Path (Old_Path.Full_Name);
      New_P : constant Filesystem_String := Path (New_Path.Full_Name);

      --------------
      -- Rename_P --
      --------------

      procedure Rename_P (Node : Project_Node_Id) is
         Node_P : constant Filesystem_String :=
           Path (+Get_String (String_Value_Of (Node, Tree_Node)));
      begin
         --  ??? We used to test File_Equal on the Build_Server, but there is
         --  no such notion in GNATCOLL

         if Node_P'Length >= Old_P'Length
           and then File_Equal
             (Node_P (Node_P'First ..  Node_P'First + Old_P'Length - 1),
              Old_P, Local_Host)
         then
            Set_String_Value_Of
              (Node, Tree_Node,
               Get_String
                 (+New_P &
                  (+Node_P (Node_P'First + Old_P'Length .. Node_P'Last))));
            Changed := True;
         end if;
      end Rename_P;

   begin
      --  Replace all the paths

      For_Each_Directory_Node
        (Tree_Node, Project, Rename_P'Unrestricted_Access);

      if Changed then
         Project.Set_Modified (True);
      end if;

      return Changed;
   end Rename_Path;

   -----------------
   -- Create_Type --
   -----------------

   function Create_Type
     (Tree       : Project_Node_Tree_Ref;
      Prj_Or_Pkg : Project_Node_Id;
      Name       : String) return Project_Node_Id
   is
      Node : Project_Node_Id;
   begin
      Node := Default_Project_Node (Tree, N_String_Type_Declaration);
      Set_Name_Of (Node, Tree, Get_String (Name));
      Add_At_End (Tree, Prj_Or_Pkg, Node, True, True);
      return Node;
   end Create_Type;

   ------------------------
   -- Add_Possible_Value --
   ------------------------

   procedure Add_Possible_Value
     (Tree   : Project_Node_Tree_Ref;
      Typ    : Project_Node_Id;
      Choice : String)
   is
      C       : constant Name_Id := Get_String (Choice);
      Str, S2 : Project_Node_Id;
   begin
      pragma Assert (Kind_Of (Typ, Tree) = N_String_Type_Declaration);

      Str := First_Literal_String (Typ, Tree);

      while Str /= Empty_Node loop
         if String_Value_Of (Str, Tree) = C then
            return;
         end if;

         Str := Next_Literal_String (Str, Tree);
      end loop;

      S2 := Create_Literal_String (C, Tree);
      Set_Next_Literal_String (S2, Tree, First_Literal_String (Typ, Tree));
      Set_First_Literal_String (Typ, Tree, S2);
   end Add_Possible_Value;

   --------------------------
   -- Expression_As_String --
   --------------------------

   function Expression_As_String
     (Tree       : Project_Node_Tree_Ref;
      Expression : Project_Node_Id) return Name_Id
   is
      Term : Project_Node_Id;
   begin
      case Kind_Of (Expression, Tree) is
         when N_Literal_String =>
            return String_Value_Of (Expression, Tree);
         when N_Expression =>
            Term := First_Term (Expression, Tree);
            if Term /= Empty_Node
              and then Next_Term (Term, Tree) = Empty_Node
              and then Kind_Of (Current_Term (Term, Tree), Tree) =
                N_Literal_String
            then
               return String_Value_Of (Current_Term (Term, Tree), Tree);
            else
               return No_Name;
            end if;
         when others =>
            return No_Name;
      end case;
   end Expression_As_String;

   ----------------------------
   -- Find_Scenario_Variable --
   ----------------------------

   function Find_Scenario_Variable
     (Tree          : Project_Node_Tree_Ref;
      Project       : Project_Type;
      External_Name : String)
      return Project_Node_Id
   is
      Decl    : Project_Node_Id := First_Declarative_Item_Of
        (Project_Declaration_Of (Project.Node, Tree), Tree);
      Current : Project_Node_Id;
   begin
      while Decl /= Empty_Node loop
         Current := Current_Item_Node (Decl, Tree);
         if Kind_Of (Current, Tree) = N_Typed_Variable_Declaration
           and then Is_External_Variable (Current, Tree)
         then
            Get_Name_String (External_Reference_Of (Current, Tree));
            if Name_Buffer (1 .. Name_Len) = External_Name then
               return Current;
            end if;
         end if;

         Decl := Next_Declarative_Item (Decl, Tree);
      end loop;
      return Empty_Node;
   end Find_Scenario_Variable;

   --------------------------
   -- Is_Virtual_Extending --
   --------------------------

   function Is_Virtual_Extending
     (Tree : Project_Node_Tree_Ref;
      Node : Prj.Tree.Project_Node_Id) return Boolean
   is
      Name : constant String :=
        Get_String (Prj.Tree.Name_Of (Node, Tree));
   begin
      return Name'Length > Virtual_Prefix'Length
        and then Name (1 .. Virtual_Prefix'Length) = Virtual_Prefix;
   end Is_Virtual_Extending;

end GNATCOLL.Projects.Normalize;
