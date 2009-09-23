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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with File_Utils;                use File_Utils;
with GPS.Intl;                  use GPS.Intl;
with Namet;                     use Namet;
with Prj.Com;                   use Prj.Com;
with Prj.Err;                   use Prj.Err;
with Prj.Ext;                   use Prj.Ext;
with Prj.Part;                  use Prj.Part;
with Prj.Util;                  use Prj.Util;
with Prj_Output;
with Projects.Editor.Normalize; use Projects.Editor.Normalize;
with Projects.Graphs;           use Projects.Graphs;
with Projects.Registry;         use Projects.Registry;
with Remote;                    use Remote;
with Snames;                    use Snames;
with Traces;                    use Traces;
with Types;                     use Types;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;        use GNATCOLL.VFS_Utils;

package body Projects.Editor is

   Me : constant Debug_Handle := Create ("Projects.Editor");

   type Project_Node_Array_Access is access Project_Node_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Project_Node_Array, Project_Node_Array_Access);

   -----------
   -- Nodes --
   -----------
   --  The following subprograms are the same as those defined in the spec, but
   --  apply directly on Project_Node_Id.
   --  They do not check that the project is indeed writable, and do not handle
   --  renaming packages: they always modify the tree given in parameter, and
   --  will not look whether the actual package definition is in fact in
   --  another project.

   procedure Delete_Attribute
     (Tree               : Project_Node_Tree_Ref;
      Node               : Project_Node_Id;
      Scenario_Variables : Scenario_Variable_Array;
      Pkg_Name, Attribute_Name : Name_Id;
      Attribute_Index    : String := "");
   procedure Set_Attribute_Value_In_Scenario
     (Tree               : Project_Node_Tree_Ref;
      Node               : Project_Node_Id;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute          : Attribute_Pkg;
      Values             : Associative_Array_Values);

   --------------
   -- Projects --
   --------------

   function Find_Project_In_Hierarchy
     (Root_Project : Project_Type; Name : Namet.Name_Id)
      return Project_Node_Id;
   --  Find in the project tree starting at Root_Project a subproject called
   --  Name.
   --  This is different from using directly Prj.Tree.Projects_Htable, since we
   --  only look for the project in the hierarchy of Root_Project.

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

   procedure Reset_All_Caches (Project : Project_Type);
   --  Reset all the caches for the whole project hierarchy

   function Add_Imported_Project
     (Root_Project              : Project_Type;
      Project                   : Project_Type;
      Imported_Project          : Project_Node_Id;
      Imported_Project_Location : GNATCOLL.VFS.Virtual_File;
      Report_Errors             : Output.Output_Proc := null;
      Use_Relative_Path         : Boolean;
      Use_Base_Name             : Boolean;
      Limited_With              : Boolean := False)
      return Import_Project_Error;
   --  Internal version of Add_Imported_Project

   ---------------
   -- Variables --
   ---------------

   function Find_Scenario_Variable
     (Tree          : Project_Node_Tree_Ref;
      Project       : Project_Type;
      External_Name : Name_Id) return Project_Node_Id;
   --  Return the declaration of the scenario variable associated with
   --  the external variable External_Name.
   --  In normalized projects, there should be only such variable.

   function Length
     (Tree  : Project_Tree_Ref;
      Value : Variable_Value) return Integer;
   --  Return the number of elements in Value (1 if Value is of kind Single)

   type Environment_Variable_Callback is access procedure
     (Project, Parent, Node, Choice : Project_Node_Id);
   --  Callback for For_Each_Environment_Variable.
   --  The various possible combinations are:
   --    Node                    Parent                 Choice
   --  N_Variable_Reference      N_Term in expression   Empty_Node
   --  N_External_Value          N_Term in expression   Empty_Node
   --  N_Case_Item               N_Declarative_Item of  matching choice
   --                            case construction      N_Literal_String
   --  N_String_Type_Declaration Empty_Node             Empty_Node
   --  N_Typed_Variable_Declaration N_Declarative_Item  Empty_Node

   procedure For_Each_Environment_Variable
     (Root_Project      : Project_Type;
      Ext_Variable_Name : Name_Id;
      Specific_Choice   : Name_Id;
      Action            : Environment_Variable_Callback);
   --  Iterate over all possible references to an external variable. This
   --  returns N_External_Value, N_Variable_Reference,
   --  N_Typed_Variable_Declaration and N_String_Type_Declaration (the last
   --  three are indirect references through a named variable.

   type Node_Callback is access procedure (Node : Project_Node_Id);

   procedure For_Each_Directory_Node
     (Project : Project_Type; Action  : Node_Callback);
   --  For each node that deals with a procedure, calls Action

   procedure Remove_Variable_Declaration
     (Tree               : Project_Node_Tree_Ref;
      Project_Or_Package : Project_Node_Id;
      Declaration        : Project_Node_Id);
   --  Remove the variable declaration from the list of variables in
   --  Project_Or_Package.

   function Get_All_Possible_Values
     (Tree     : Project_Node_Tree_Ref;
      Variable : Project_Node_Id) return Name_Id_Array;
   --  Return the list of all possible values for Variable

   ----------------
   -- Attributes --
   ----------------

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

   function Attribute_Matches
     (Tree            : Project_Node_Tree_Ref;
      Node            : Project_Node_Id;
      Attribute_Name  : Name_Id;
      Attribute_Index : Name_Id) return Boolean;
   --  Return True if Node is an attribute declaration matching Attribute_Name
   --  and Attribute_Index.
   --  If Attribute_Index is Any_Attribute, no matching is done on the index.

   procedure Update_Attribute_Value_In_Scenario
     (Tree               : Project_Node_Tree_Ref;
      Project            : Project_Node_Id;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute          : Attribute_Pkg;
      Values             : GNAT.OS_Lib.Argument_List;
      Attribute_Index    : String := "";
      Prepend            : Boolean := False);
   --  Internal version of Update_Attribute_Value_In_Scenario

   --------------
   -- Packages --
   --------------

   function Find_Package_Declaration
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Node_Id; Name : Namet.Name_Id) return Project_Node_Id;
   --  Return the package whose name is Name, or Empty_Node if there is none

   -----------------
   -- Expressions --
   -----------------

   function String_As_Expression
     (Value : Name_Id; Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Return an N_Expression node that represents the static string Value.
   --  ??? Could be implemented in terms of Concatenate.

   function Expression_As_String
     (Tree       : Project_Node_Tree_Ref;
      Expression : Project_Node_Id) return Name_Id;
   --  Return the string contained in an expression. If the expression contains
   --  more than a string literal, No_Name is returned.
   --  This also accepts cases when Expression itself is a string_literal

   procedure Set_Expression
     (Tree             : Project_Node_Tree_Ref;
      Var_Or_Attribute : Project_Node_Id;
      Expr             : Project_Node_Id);
   --  Set Var as the expression to use for the value of Var. This
   --  properly handles standard variables and variables defined through
   --  references to external environment variables.

   ----------
   -- Misc --
   ----------

   function Find_Node_By_Name
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Node_Id;
      Kind    : Project_Node_Kind;
      Name    : Name_Id) return Project_Node_Id;
   --  Find a node given its name

   procedure Remove_Node
     (Tree   : Project_Node_Tree_Ref;
      Parent : Project_Node_Id;
      Node   : Project_Node_Id);
   --  Remove Node from the declaration list in Parent.
   --  This doesn't search recursively inside nested packages, case
   --  constructions, ...

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

   procedure Common_Setup_For_Update_Attribute
     (Tree            : Project_Node_Tree_Ref;
      Project         : Project_Node_Id;
      Attribute       : Attribute_Pkg;
      Attribute_Index : String;
      Rename_Prj      : out Project_Node_Id;
      Pkg             : out Project_Node_Id;
      Package_Name    : out Name_Id;
      Attr_Name       : out Name_Id;
      Attr_Index      : out Name_Id;
      Case_Construct  : out Project_Node_Id);
   --  Common initialization function for all functions that update the
   --  attributes. The output parameters represent the project/package and
   --  attributes that should be modified, taking into account renaming
   --  clauses.

   procedure Add_Node_To_List
     (To   : in out Project_Node_Array_Access;
      Last : in out Natural;
      Node : Project_Node_Id);
   --  Add a new node into the list of nodes To.
   --  To is resized as needed

   ------------
   -- Length --
   ------------

   function Length
     (Tree : Prj.Project_Tree_Ref; List : Prj.String_List_Id) return Natural
   is
      L     : String_List_Id := List;
      Count : Natural        := 0;
   begin
      while L /= Nil_String loop
         Count := Count + 1;
         L := Tree.String_Elements.Table (L).Next;
      end loop;
      return Count;
   end Length;

   --------------------------
   -- String_As_Expression --
   --------------------------

   function String_As_Expression
     (Value : Name_Id; Tree : Project_Node_Tree_Ref) return Project_Node_Id is
   begin
      return Enclose_In_Expression (Create_Literal_String (Value, Tree), Tree);
   end String_As_Expression;

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

   ------------
   -- Length --
   ------------

   function Length
     (Tree  : Project_Tree_Ref;
      Value : Variable_Value) return Integer is
   begin
      case Value.Kind is
         when Undefined => return 0;
         when Single    => return 1;
         when List      => return Length (Tree, Value.Values);
      end case;
   end Length;

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

   ----------------------------
   -- Find_Scenario_Variable --
   ----------------------------

   function Find_Scenario_Variable
     (Tree          : Project_Node_Tree_Ref;
      Project       : Project_Type;
      External_Name : Name_Id)
      return Project_Node_Id
   is
      Name    : constant String := Get_String (External_Name);
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
            if Name_Buffer (1 .. Name_Len) = Name then
               return Current;
            end if;
         end if;

         Decl := Next_Declarative_Item (Decl, Tree);
      end loop;
      return Empty_Node;
   end Find_Scenario_Variable;

   -------------------------------
   -- Find_Project_In_Hierarchy --
   -------------------------------

   function Find_Project_In_Hierarchy
     (Root_Project : Project_Type; Name : Namet.Name_Id) return Project_Node_Id
   is
      Iter : Imported_Project_Iterator := Start (Root_Project);
   begin
      while Current (Iter) /= No_Project loop
         if Prj.Tree.Name_Of (Current (Iter).Node, Root_Project.Tree) =
           Name
         then
            return Current (Iter).Node;
         end if;

         Next (Iter);
      end loop;
      return Empty_Node;
   end Find_Project_In_Hierarchy;

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (Tree : Project_Node_Tree_Ref;
      Var  : Scenario_Variable) return String_List_Iterator
   is
      V, Expr : Project_Node_Id;
   begin
      case Kind_Of (Var.String_Type, Tree) is
         when N_String_Type_Declaration =>
            return (Current => First_Literal_String (Var.String_Type, Tree));

         when N_Attribute_Declaration
           |  N_Typed_Variable_Declaration
           |  N_Variable_Declaration =>

            V := Expression_Of (Var.String_Type, Tree);

            case Kind_Of (V, Tree) is
               when N_Expression =>
                  Expr := First_Term (V, Tree);
                  pragma Assert (Kind_Of (Expr, Tree) = N_Term);
                  Expr := Current_Term (Expr, Tree);

                  case Kind_Of (Expr, Tree) is
                     when N_Literal_String_List =>
                        return
                          (Current => First_Expression_In_List (Expr, Tree));

                     when N_External_Value =>
                        return
                          (Current => External_Default_Of (Expr, Tree));

                     when others =>
                        return (Current => V);
                  end case;

               when others =>
                  raise Program_Error;
            end case;

         when others =>
            raise Program_Error;
      end case;
   end Value_Of;

   -----------------
   -- Type_Values --
   -----------------

   function Type_Values
     (Tree        : Project_Node_Tree_Ref;
      Var_Or_Type : Project_Node_Id) return String_List_Iterator
   is
      Typ : Project_Node_Id := Var_Or_Type;
   begin
      if Kind_Of (Var_Or_Type, Tree) = N_Typed_Variable_Declaration then
         Typ := String_Type_Of (Var_Or_Type, Tree);
      end if;

      return (Current => First_Literal_String (Typ, Tree));
   end Type_Values;

   ----------
   -- Data --
   ----------

   function Data (Iter : String_List_Iterator) return Project_Node_Id is
   begin
      pragma Assert (Iter.Current /= Empty_Node);
      return Iter.Current;
   end Data;

   ----------
   -- Data --
   ----------

   function Data
     (Tree : Project_Node_Tree_Ref;
      Iter : String_List_Iterator) return Namet.Name_Id is
   begin
      pragma Assert (Kind_Of (Iter.Current, Tree) = N_Literal_String);
      return String_Value_Of (Iter.Current, Tree);
   end Data;

   ----------
   -- Done --
   ----------

   function Done (Iter : String_List_Iterator) return Boolean is
   begin
      return Iter.Current = Empty_Node;
   end Done;

   ----------
   -- Next --
   ----------

   function Next
     (Tree : Project_Node_Tree_Ref;
      Iter : String_List_Iterator) return String_List_Iterator is
   begin
      pragma Assert (Iter.Current /= Empty_Node);

      case Kind_Of (Iter.Current, Tree) is
         when N_Literal_String =>
            return (Current => Next_Literal_String (Iter.Current, Tree));

         when N_Expression =>
            return (Current => Next_Expression_In_List (Iter.Current, Tree));

         when others =>
            raise Program_Error;
      end case;
   end Next;

   -------------------------
   -- Get_Attribute_Value --
   -------------------------

   function Get_Attribute_Value
     (Project      : Project_Type;
      Attribute    : Attribute_Pkg;
      Index        : String := "";
      Use_Extended : Boolean := False) return Variable_Value
   is
      Sep            : constant Natural := Split_Package (Attribute);
      Attribute_Name : constant String :=
                         String (Attribute (Sep + 1 .. Attribute'Last));
      Pkg_Name       : constant String :=
                         String (Attribute (Attribute'First .. Sep - 1));
      Project_View   : constant Project_Id := Get_View (Project);
      Pkg            : Package_Id := No_Package;
      Value          : Variable_Value := Nil_Variable_Value;
      Var            : Variable_Id;
      Arr            : Array_Id;
      Elem           : Array_Element_Id;
      N              : Name_Id;
   begin
      if Project_View = Prj.No_Project then
         return Nil_Variable_Value;
      end if;

      if Pkg_Name /= "" then
         Pkg := Value_Of
           (Get_String (Pkg_Name),
            In_Packages => Project_View.Decl.Packages,
            In_Tree     => Project.View_Tree);
         if Pkg = No_Package then
            if Use_Extended
              and then Extended_Project (Project) /= No_Project
            then
               return Get_Attribute_Value
                 (Extended_Project (Project), Attribute, Index, Use_Extended);
            else
               return Nil_Variable_Value;
            end if;
         end if;
         Var := Project.View_Tree.Packages.Table (Pkg).Decl.Attributes;
         Arr := Project.View_Tree.Packages.Table (Pkg).Decl.Arrays;

      else
         Var := Project_View.Decl.Attributes;
         Arr := Project_View.Decl.Arrays;
      end if;

      N := Get_String (Attribute_Name);

      if Index /= "" then
         Elem := Value_Of (N, In_Arrays => Arr, In_Tree => Project.View_Tree);
         if Elem /= No_Array_Element then
            Value := Value_Of (Index => Get_String (Index), In_Array => Elem,
                               In_Tree => Project.View_Tree);
         end if;
      else
         Value := Value_Of (N, Var, In_Tree => Project.View_Tree);
      end if;

      if Value = Nil_Variable_Value
        and then Use_Extended
        and then Extended_Project (Project) /= No_Project
      then
         return Get_Attribute_Value
           (Extended_Project (Project), Attribute, Index, Use_Extended);
      else
         return Value;
      end if;
   end Get_Attribute_Value;

   --------------------------
   -- Attribute_Is_Defined --
   --------------------------

   function Attribute_Is_Defined
     (Project   : Project_Type;
      Attribute : Attribute_Pkg;
      Index     : String := "") return Boolean
   is
      Sep            : constant Natural := Split_Package (Attribute);
      Attribute_Name : constant String :=
                         String (Attribute (Sep + 1 .. Attribute'Last));
      Pkg_Name       : constant String :=
                         String (Attribute (Attribute'First .. Sep - 1));
      Project_View   : constant Project_Id := Get_View (Project);
      Pkg            : Package_Id := No_Package;
      Var            : Variable_Id;
      Arr            : Array_Id;
      N              : Name_Id;
   begin
      if Project_View = Prj.No_Project then
         return False;
      end if;

      if Pkg_Name /= "" then
         Pkg := Value_Of
           (Get_String (Pkg_Name),
            In_Packages => Project_View.Decl.Packages,
            In_Tree     => Project.View_Tree);
         if Pkg = No_Package then
            return False;
         end if;
         Var := Project.View_Tree.Packages.Table (Pkg).Decl.Attributes;
         Arr := Project.View_Tree.Packages.Table (Pkg).Decl.Arrays;
      else
         Var := Project_View.Decl.Attributes;
         Arr := Project_View.Decl.Arrays;
      end if;

      N := Get_String (Attribute_Name);

      if Index /= "" then
         return Value_Of (N, In_Arrays => Arr, In_Tree => Project.View_Tree)
           /= No_Array_Element;
      else
         return not Value_Of (N, Var, Project.View_Tree).Default;
      end if;
   end Attribute_Is_Defined;

   ----------------------------------------
   -- Update_Attribute_Value_In_Scenario --
   ----------------------------------------

   procedure Update_Attribute_Value_In_Scenario
     (Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute          : Attribute_Pkg;
      Values             : GNAT.OS_Lib.Argument_List;
      Attribute_Index    : String := "";
      Prepend            : Boolean := False)
   is
      Sep      : constant Natural := Split_Package (Attribute);
      Pkg_Name : constant String :=
                   String (Attribute (Attribute'First .. Sep - 1));
      Pkg_Prj  : constant Project_Type :=
                   Find_Project_Of_Package (Project, Pkg_Name);
   begin
      if not Is_Editable (Project) then
         Trace (Me, "Project is not editable");
         return;
      end if;

      Projects.Editor.Normalize.Normalize (Pkg_Prj);
      Update_Attribute_Value_In_Scenario
        (Project.Tree, Pkg_Prj.Node, Scenario_Variables, Attribute,
         Values, Attribute_Index, Prepend);
      Set_Project_Modified (Pkg_Prj, True);
   end Update_Attribute_Value_In_Scenario;

   ----------------------------------------
   -- Update_Attribute_Value_In_Scenario --
   ----------------------------------------

   procedure Update_Attribute_Value_In_Scenario
     (Tree               : Project_Node_Tree_Ref;
      Project            : Project_Node_Id;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute          : Attribute_Pkg;
      Values             : GNAT.OS_Lib.Argument_List;
      Attribute_Index    : String := "";
      Prepend            : Boolean := False)
   is
      Attribute_N     : Name_Id;
      Pkg_Name        : Name_Id;
      List            : Project_Node_Id := Empty_Node;
      Pkg, Term, Expr : Project_Node_Id;
      Rename_Prj      : Project_Node_Id;
      Case_Construct  : Project_Node_Id;
      Index           : Name_Id;

      procedure Add_Or_Replace (Case_Item : Project_Node_Id);
      --  Add or replace the attribute Attribute_Name in the declarative list
      --  for Case_Item

      --------------------
      -- Add_Or_Replace --
      --------------------

      procedure Add_Or_Replace (Case_Item : Project_Node_Id) is
         Previous_Decl, Decl, Expr : Project_Node_Id;
         pragma Unreferenced (Decl);
      begin
         Previous_Decl := Find_Last_Declaration_Of
           (Tree, Case_Item, Attribute_N, Index);

         --  Do we already have some declarations for this attribute ?
         --  If we found a previous declaration, update it

         if Previous_Decl /= Empty_Node then
            Previous_Decl := Expression_Of (Previous_Decl, Tree);
            if Prepend then
               Expr := First_Expression_In_List (List, Tree);
               while Next_Expression_In_List (Expr, Tree) /= Empty_Node loop
                  Expr := Next_Expression_In_List (Expr, Tree);
               end loop;

               Set_Next_Expression_In_List
                 (Expr, Tree, First_Expression_In_List
                    (Current_Term (First_Term (Previous_Decl, Tree), Tree),
                    Tree));
            else
               Set_Next_Expression_In_List (Previous_Decl, Tree, Empty_Node);
               Set_Next_Term
                 (First_Term (Previous_Decl, Tree), Tree, Empty_Node);
            end if;

            Set_Current_Term (First_Term (Previous_Decl, Tree), Tree, List);

         --  Else create the new instruction to be added to the project

         else
            Expr := Enclose_In_Expression (List, Tree);

            if Prepend then
               Set_Next_Term
                 (First_Term (Expr, Tree), Tree,
                  Default_Project_Node (Tree, N_Term, Prj.List));
               Term := Next_Term (First_Term (Expr, Tree), Tree);
               Set_Current_Term
                 (Term,
                  Tree,
                  Default_Project_Node
                    (Tree, N_Attribute_Reference, Prj.List));
               Term := Current_Term (Term, Tree);

               Set_Name_Of (Term, Tree, Attribute_N);
               Set_Project_Node_Of (Term, Tree, Rename_Prj);
            end if;

            Decl := Create_Attribute
              (Tree, Case_Item, Attribute_N, Index, Value => Expr);
         end if;
      end Add_Or_Replace;

   begin
      Common_Setup_For_Update_Attribute
        (Tree, Project, Attribute, Attribute_Index,
         Rename_Prj, Pkg, Pkg_Name, Attribute_N, Index, Case_Construct);
      Move_From_Common_To_Case_Construct
        (Tree, Rename_Prj, Pkg, Case_Construct, Scenario_Variables,
         Attribute_N, Index);

      --  Create the string list for the new values.
      --  This can be prepended later on to the existing list of values.

      List := Default_Project_Node (Tree, N_Literal_String_List, Prj.List);

      for A in reverse Values'Range loop
         Expr := String_As_Expression (Get_String (Values (A).all), Tree);
         Set_Next_Expression_In_List
           (Expr, Tree, First_Expression_In_List (List, Tree));
         Set_First_Expression_In_List (List, Tree, Expr);
      end loop;

      For_Each_Scenario_Case_Item
        (Tree, Rename_Prj, Pkg, Case_Construct, Scenario_Variables,
         Add_Or_Replace'Unrestricted_Access);
   end Update_Attribute_Value_In_Scenario;

   ----------------------------------------
   -- Update_Attribute_Value_In_Scenario --
   ----------------------------------------

   procedure Update_Attribute_Value_In_Scenario
     (Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute          : Attribute_Pkg;
      Value              : String;
      Attribute_Index    : String := "")
   is
      Sep     : constant Natural := Split_Package (Attribute);
      Pkg_Prj : Project_Type := Project;
   begin
      if not Is_Editable (Project) then
         Trace (Me, "Project is not editable");
         return;
      end if;

      if Sep > Attribute'First then
         Pkg_Prj := Find_Project_Of_Package
           (Project, String (Attribute (Attribute'First .. Sep - 1)));
      end if;

      Projects.Editor.Normalize.Normalize (Pkg_Prj);
      Update_Attribute_Value_In_Scenario
        (Project.Tree, Pkg_Prj.Node, Scenario_Variables, Attribute,
         Value, Attribute_Index);
      Set_Project_Modified (Pkg_Prj, True);
   end Update_Attribute_Value_In_Scenario;

   ----------------------------------------
   -- Update_Attribute_Value_In_Scenario --
   ----------------------------------------

   procedure Update_Attribute_Value_In_Scenario
     (Tree               : Project_Node_Tree_Ref;
      Project            : Project_Node_Id;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute          : Attribute_Pkg;
      Value              : String;
      Attribute_Index    : String := "")
   is
      Attribute_N    : Name_Id;
      Pkg_Name       : Name_Id;
      Val            : Project_Node_Id;
      Pkg            : Project_Node_Id;
      Rename_Prj     : Project_Node_Id := Project;
      Case_Construct : Project_Node_Id;
      Index          : Name_Id;

      procedure Add_Or_Replace (Case_Item : Project_Node_Id);
      --  Add or replace the attribute Attribute_Name in the declarative list
      --  for Case_Item

      --------------------
      -- Add_Or_Replace --
      --------------------

      procedure Add_Or_Replace (Case_Item : Project_Node_Id) is
         Previous_Decl, Decl : Project_Node_Id;
         pragma Unreferenced (Decl);
      begin
         Previous_Decl := Find_Last_Declaration_Of
           (Tree, Case_Item, Attribute_N, Index);

         --  If we found a previous declaration, update it

         if Previous_Decl /= Empty_Node then
            Previous_Decl := Expression_Of (Previous_Decl, Tree);
            Set_Current_Term (First_Term (Previous_Decl, Tree), Tree, Val);

         --  Else create the new instruction to be added to the project

         else
            Decl := Create_Attribute
              (Tree, Case_Item, Attribute_N, Index, Prj.Single, Value => Val);
         end if;
      end Add_Or_Replace;

   begin
      Common_Setup_For_Update_Attribute
        (Tree, Project, Attribute, Attribute_Index,
         Rename_Prj, Pkg, Pkg_Name, Attribute_N, Index, Case_Construct);
      Move_From_Common_To_Case_Construct
        (Tree, Rename_Prj, Pkg, Case_Construct, Scenario_Variables,
         Attribute_N, Index);

      --  Create the node for the new value

      Val := Create_Literal_String (Get_String (Value), Tree);

      For_Each_Scenario_Case_Item
        (Tree, Rename_Prj, Pkg, Case_Construct, Scenario_Variables,
         Add_Or_Replace'Unrestricted_Access);
   end Update_Attribute_Value_In_Scenario;

   ---------------------------------------
   -- Common_Setup_For_Update_Attribute --
   ---------------------------------------

   procedure Common_Setup_For_Update_Attribute
     (Tree            : Project_Node_Tree_Ref;
      Project         : Project_Node_Id;
      Attribute       : Attribute_Pkg;
      Attribute_Index : String;
      Rename_Prj      : out Project_Node_Id;
      Pkg             : out Project_Node_Id;
      Package_Name    : out Name_Id;
      Attr_Name       : out Name_Id;
      Attr_Index      : out Name_Id;
      Case_Construct  : out Project_Node_Id)
   is
      Sep            : constant Natural := Split_Package (Attribute);
      Pkg_Name       : constant String :=
                         String (Attribute (Attribute'First .. Sep - 1));
      Attribute_Name : constant String :=
                         String (Attribute (Sep + 1 .. Attribute'Last));
   begin
      Attr_Name    := Get_String (Attribute_Name);
      Package_Name := Get_String (Pkg_Name);

      if Attribute_Index /= "" then
         Attr_Index := Get_String (Attribute_Index);
      else
         Attr_Index := No_Name;
      end if;

      if Pkg_Name /= "" then
         Pkg := Create_Package (Tree, Project, Pkg_Name);

         --  If we have a renamed package, modify the target package
         Rename_Prj := Project_Of_Renamed_Package_Of (Pkg, Tree);
         if Rename_Prj /= Empty_Node then
            Pkg := Create_Package (Tree, Rename_Prj, Pkg_Name);
         else
            Rename_Prj := Project;
         end if;

      else
         Rename_Prj := Project;
         Pkg := Empty_Node;
      end if;

      Case_Construct := Find_Case_Statement (Tree, Rename_Prj, Pkg);
   end Common_Setup_For_Update_Attribute;

   -------------------------------------
   -- Set_Attribute_Value_In_Scenario --
   -------------------------------------

   procedure Set_Attribute_Value_In_Scenario
     (Tree               : Project_Node_Tree_Ref;
      Node               : Project_Node_Id;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute          : Attribute_Pkg;
      Values             : Associative_Array_Values)
   is
      Rename_Prj, Pkg, Case_Construct, Val : Project_Node_Id;
      Pkg_Name, Attr_Name, Attr_Index      : Name_Id;
      First                                : Project_Node_Id := Empty_Node;
      Next                                 : Project_Node_Id := Empty_Node;
      N                                    : Project_Node_Id;

      procedure Add_Or_Replace (Case_Item : Project_Node_Id);
      --  Add or replace the attribute Attribute_Name in the declarative list
      --  for Case_Item

      --------------------
      -- Add_Or_Replace --
      --------------------

      procedure Add_Or_Replace (Case_Item : Project_Node_Id) is
      begin
         Add_In_Front (Tree, Case_Item, Clone_Node (Tree, First, True));
      end Add_Or_Replace;

   begin
      --  The call to Delete_Attribute will normalize the right project
      Common_Setup_For_Update_Attribute
        (Tree, Node, Attribute, "",
         Rename_Prj, Pkg, Pkg_Name, Attr_Name, Attr_Index, Case_Construct);
      Delete_Attribute
        (Tree, Node, Scenario_Variables, Pkg_Name, Attr_Name, Any_Attribute);

      for V in Values'Range loop
         Attr_Index := Get_String (Values (V).Index.all);
         Val := Create_Literal_String
           (Get_String (Values (V).Value.all), Tree);

         N := Default_Project_Node
           (Tree, N_Declarative_Item, Prj.Single);
         if First = Empty_Node then
            First := N;
         else
            Set_Next_Declarative_Item (Next, Tree, N);
         end if;
         Next := N;

         Set_Current_Item_Node
           (N, Tree,
            Create_Attribute
              (Tree       => Tree,
               Prj_Or_Pkg => Empty_Node,
               Name       => Attr_Name,
               Index_Name => Attr_Index,
               Kind       => Prj.Single,
               At_Index   => Values (V).At_Index,
               Value      => Val));
      end loop;

      For_Each_Scenario_Case_Item
        (Tree, Rename_Prj, Pkg, Case_Construct, Scenario_Variables,
         Add_Or_Replace'Unrestricted_Access);
   end Set_Attribute_Value_In_Scenario;

   -------------------------------------
   -- Set_Attribute_Value_In_Scenario --
   -------------------------------------

   procedure Set_Attribute_Value_In_Scenario
     (Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute          : Attribute_Pkg;
      Values             : Associative_Array_Values)
   is
   begin
      if not Is_Editable (Project) then
         Trace (Me, "Project is not editable");
         return;
      end if;

      Set_Attribute_Value_In_Scenario
        (Project.Tree, Project.Node, Scenario_Variables, Attribute, Values);
   end Set_Attribute_Value_In_Scenario;

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
      --  ??? Should use Basic_Types.Add
      if Last = To'Last then
         To := new Project_Node_Array (1 .. Old'Last * 2);
         To (1 .. Old'Length) := Old.all;
         Free (Old);
      end if;

      Last := Last + 1;
      To (Last) := Node;
   end Add_Node_To_List;

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
        (Attribute_Index = Any_Attribute_Name
         or else (Attribute_Index = No_Name
          and then Associative_Array_Index_Of (Node, Tree) = No_Name)
         or else (Attribute_Index /= No_Name
                  and then Associative_Array_Index_Of (Node, Tree) /= No_Name
                  and then Associative_Array_Index_Of (Node, Tree) =
                     Attribute_Index));
   end Attribute_Matches;

   ----------------------
   -- Delete_Attribute --
   ----------------------

   procedure Delete_Attribute
     (Tree               : Project_Node_Tree_Ref;
      Node               : Project_Node_Id;
      Scenario_Variables : Scenario_Variable_Array;
      Pkg_Name, Attribute_Name : Name_Id;
      Attribute_Index    : String := "")
   is
      Pkg            : Project_Node_Id;
      Case_Construct : Project_Node_Id;
      Index          : Name_Id := No_Name;

      procedure Delete_Attr (Case_Item : Project_Node_Id);
      --  Remove all definitions for the attribute in the case item

      -----------------
      -- Delete_Attr --
      -----------------

      procedure Delete_Attr (Case_Item : Project_Node_Id) is
      begin
         Remove_Attribute_Declarations
           (Tree, Case_Item, Attribute_Name, Index);
      end Delete_Attr;

   begin
      if Pkg_Name /= No_Name and then Length_Of_Name (Pkg_Name) /= 0 then
         Pkg := Find_Package_Declaration (Tree, Node, Pkg_Name);

         --  If the package doesn't exist, no need to do anything
         if Pkg = Empty_Node then
            Trace (Me, "Delete attribute: No such package");
            return;
         end if;
      else
         Pkg := Empty_Node;
      end if;

      if Attribute_Index /= "" then
         Index := Get_String (Attribute_Index);
      end if;

      Case_Construct := Find_Case_Statement (Tree, Node, Pkg);
      Move_From_Common_To_Case_Construct
        (Tree, Node, Pkg, Case_Construct, Scenario_Variables,
         Attribute_Name, Index);

      For_Each_Scenario_Case_Item
        (Tree, Node, Pkg, Case_Construct, Scenario_Variables,
         Delete_Attr'Unrestricted_Access);
   end Delete_Attribute;

   procedure Delete_Attribute
     (Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute          : Attribute_Pkg;
      Attribute_Index    : String := "")
   is
      Sep            : constant Natural := Split_Package (Attribute);
      Pkg_Name       : constant String :=
        String (Attribute (Attribute'First .. Sep - 1));
      Attribute_Name : constant String :=
        String (Attribute (Sep + 1 .. Attribute'Last));
      Pkg_Prj        : constant Project_Type :=
        Find_Project_Of_Package (Project, Pkg_Name);
   begin
      if not Is_Editable (Project) then
         Trace (Me, "Project is not editable");
         return;
      end if;

      Projects.Editor.Normalize.Normalize (Pkg_Prj);

      Delete_Attribute
        (Pkg_Prj.Tree, Pkg_Prj.Node,
         Scenario_Variables, Get_String (Pkg_Name),
         Get_String (Attribute_Name), Attribute_Index);

      Set_Project_Modified (Pkg_Prj, True);
   end Delete_Attribute;

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

   ------------------------
   -- Add_Possible_Value --
   ------------------------

   procedure Add_Possible_Value
     (Tree   : Project_Node_Tree_Ref;
      Typ    : Project_Node_Id;
      Choice : Name_Id)
   is
      Str, S2 : Project_Node_Id;
   begin
      pragma Assert (Kind_Of (Typ, Tree) = N_String_Type_Declaration);

      Str := First_Literal_String (Typ, Tree);

      while Str /= Empty_Node loop
         if String_Value_Of (Str, Tree) = Choice then
            return;
         end if;

         Str := Next_Literal_String (Str, Tree);
      end loop;

      S2 := Create_Literal_String (Choice, Tree);
      Set_Next_Literal_String (S2, Tree, First_Literal_String (Typ, Tree));
      Set_First_Literal_String (Typ, Tree, S2);
   end Add_Possible_Value;

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

   ------------------
   -- Remove_Value --
   ------------------

   procedure Remove_Value
     (Root_Project      : Project_Type;
      Ext_Variable_Name : String;
      Value_Name        : String)
   is
      Tree            : constant Project_Node_Tree_Ref := Root_Project.Tree;
      Delete_Variable : exception;
      Type_Decl       : Project_Node_Id := Empty_Node;
      V_Name          : constant Name_Id := Get_String (Value_Name);

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each matching node for the env. variable

      --------------
      -- Callback --
      --------------

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Project, Choice);
         C, C2 : Project_Node_Id;
      begin
         case Kind_Of (Node, Root_Project.Tree) is
            when N_String_Type_Declaration =>
               Type_Decl := Node;

               C := First_Literal_String (Node, Tree);

               if Next_Literal_String (C, Tree) = Empty_Node then
                  raise Delete_Variable;
               end if;

               if String_Value_Of (C, Tree) = V_Name then
                  Set_First_Literal_String
                    (Node, Tree, Next_Literal_String (C, Tree));
                  return;
               end if;

               loop
                  C2 := Next_Literal_String (C, Tree);
                  exit when C2 = Empty_Node;

                  if String_Value_Of (C2, Tree) = V_Name then
                     Set_Next_Literal_String
                       (C, Tree, Next_Literal_String (C2, Tree));
                     exit;
                  end if;
                  C := C2;
               end loop;

            when N_External_Value =>
               if External_Default_Of (Node, Tree) /= Empty_Node
                 and then String_Value_Of
                   (External_Default_Of (Node, Tree), Tree) = V_Name
               then
                  Set_External_Default_Of (Node, Tree, Empty_Node);
               end if;

            when N_Case_Item =>
               C := First_Case_Item_Of
                 (Current_Item_Node (Parent, Tree), Tree);
               if C = Node then
                  Set_First_Case_Item_Of
                    (Current_Item_Node (Parent, Tree), Tree,
                     Next_Case_Item (C, Tree));
                  return;
               end if;

               loop
                  C2 := Next_Case_Item (C, Tree);
                  exit when C2 = Empty_Node;

                  if C2 = Node then
                     Set_Next_Case_Item (C, Tree, Next_Case_Item (C2, Tree));
                  end if;

                  C := C2;
               end loop;

            when others =>
               null;
         end case;
      end Callback;

      Ext_Var : constant Name_Id := Get_String (Ext_Variable_Name);

   begin
      if not Is_Editable (Root_Project) then
         Trace (Me, "Project is not editable");
         return;
      end if;

      Projects.Editor.Normalize.Normalize (Root_Project);
      For_Each_Environment_Variable
        (Root_Project, Ext_Var,
         Get_String (Value_Name), Callback'Unrestricted_Access);

      --  Reset the value of the external variable if needed

      if Value_Of (Root_Project.Tree, Ext_Var) = V_Name then
         if Type_Decl /= Empty_Node then
            Add (Root_Project.Tree,
                 Ext_Variable_Name,
                 Get_String (String_Value_Of
                               (First_Literal_String (Type_Decl, Tree),
                                Tree)));
         else
            Add (Root_Project.Tree, Ext_Variable_Name, "");
         end if;
      end if;

      Set_Project_Modified (Root_Project, True);

   exception
      when Delete_Variable =>
         Delete_External_Variable
           (Root_Project,
            Ext_Variable_Name        => Ext_Variable_Name,
            Keep_Choice              => Value_Name,
            Delete_Direct_References => False);
   end Remove_Value;

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

   ----------------------------------------
   -- Rename_Value_For_External_Variable --
   ----------------------------------------

   procedure Rename_Value_For_External_Variable
     (Root_Project      : Project_Type;
      Ext_Variable_Name : String;
      Old_Value_Name    : String;
      New_Value_Name    : Namet.Name_Id)
   is
      Tree       : constant Project_Node_Tree_Ref := Root_Project.Tree;
      Old_V_Name : constant Name_Id := Get_String (Old_Value_Name);

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable

      --------------
      -- Callback --
      --------------

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Project, Parent);
         C : Project_Node_Id;
      begin
         case Kind_Of (Node, Root_Project.Tree) is
            when N_External_Value =>
               if External_Default_Of (Node, Tree) /= Empty_Node
                 and then Expression_As_String
                   (Tree, External_Default_Of (Node, Tree)) = Old_V_Name
               then
                  if Kind_Of (External_Default_Of (Node, Tree), Tree) =
                    N_Literal_String
                  then
                     Set_String_Value_Of
                       (External_Default_Of (Node, Tree), Tree,
                        New_Value_Name);
                  else
                     Set_External_Default_Of
                       (Node, Tree,
                        Create_Literal_String (New_Value_Name, Tree));
                  end if;
               end if;

            when N_String_Type_Declaration =>
               C := First_Literal_String (Node, Tree);
               while C /= Empty_Node loop
                  if String_Value_Of (C, Tree) = Old_V_Name then
                     Set_String_Value_Of (C, Tree, New_Value_Name);
                     exit;
                  end if;
                  C := Next_Literal_String (C, Tree);
               end loop;

            when N_Case_Item =>
               Set_String_Value_Of (Choice, Tree, New_Value_Name);

            when others =>
               null;
         end case;
      end Callback;

      N : constant Name_Id := Get_String (Ext_Variable_Name);

   begin
      if not Is_Editable (Root_Project) then
         Trace (Me, "Project is not editable");
         return;
      end if;

      Projects.Editor.Normalize.Normalize (Root_Project);
      For_Each_Environment_Variable
        (Root_Project, N, Get_String (Old_Value_Name),
         Callback'Unrestricted_Access);

      if Value_Of (Root_Project.Tree, N) /= No_Name
        and then Value_Of (Root_Project.Tree, N) = Old_V_Name
      then
         Add (Root_Project.Tree,
              Ext_Variable_Name, Get_String (New_Value_Name));
      end if;

      Set_Project_Modified (Root_Project, True);
   end Rename_Value_For_External_Variable;

   -----------------------------------
   -- For_Each_Environment_Variable --
   -----------------------------------

   procedure For_Each_Environment_Variable
     (Root_Project      : Project_Type;
      Ext_Variable_Name : Name_Id;
      Specific_Choice   : Name_Id;
      Action            : Environment_Variable_Callback)
   is
      Tree                : constant Project_Node_Tree_Ref :=
                              Root_Project.Tree;
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

      Iter : Imported_Project_Iterator := Start (Root_Project);
   begin
      while Current (Iter) /= No_Project loop
         Recurse_In_Project (Current (Iter).Node, Empty_Node);
         Next (Iter);
      end loop;
      Free (Variable_Nodes);
   end For_Each_Environment_Variable;

   ------------------------------
   -- Delete_External_Variable --
   ------------------------------

   procedure Delete_External_Variable
     (Root_Project             : Project_Type;
      Ext_Variable_Name        : String;
      Keep_Choice              : String;
      Delete_Direct_References : Boolean := True)
   is
      Tree : constant Project_Node_Tree_Ref := Root_Project.Tree;

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable

      --------------
      -- Callback --
      --------------

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Choice);
      begin
         case Kind_Of (Node, Tree) is
            when N_External_Value =>
               if Delete_Direct_References then
                  Set_Current_Term
                    (Parent, Tree,
                     Create_Literal_String (Get_String (Keep_Choice), Tree));
               end if;

            when N_Variable_Reference =>
               Set_Current_Term
                 (Parent, Tree,
                  Create_Literal_String (Get_String (Keep_Choice), Tree));

            when N_Typed_Variable_Declaration =>
               Remove_Node (Tree, Project, Node);
               Remove_Variable_Declaration (Tree, Project, Node);

            when N_String_Type_Declaration =>
               Remove_Node (Tree, Project, Node);

            when N_Case_Item =>
               --  The first declarative item might be null when there was no
               --  actual "when ..." for Keep_Choice. In that case, Prj.Proc
               --  inserts an entry with no declarative item.

               if First_Declarative_Item_Of (Node, Tree) /= Empty_Node then
                  Tree.Project_Nodes.Table (Parent) := Tree.Project_Nodes.Table
                    (First_Declarative_Item_Of (Node, Tree));

               else
                  Set_Current_Item_Node (Parent, Tree, Empty_Node);
               end if;

            when others =>
               null;
               pragma Assert (False, "Unexpected node type");

         end case;
      end Callback;

   begin
      if not Is_Editable (Root_Project) then
         Trace (Me, "Project is not editable");
         return;
      end if;

      Projects.Editor.Normalize.Normalize (Root_Project);
      For_Each_Environment_Variable
        (Root_Project, Get_String (Ext_Variable_Name),
         Get_String (Keep_Choice), Callback'Unrestricted_Access);

      --  Mark all projects in the hierarchy as modified, since they are
      --  potentially all impacted.

      declare
         Iterator : Imported_Project_Iterator := Start
           (Root_Project, Recursive => True);
         P : Project_Type;
      begin
         loop
            P := Current (Iterator);
            exit when P = No_Project;
            Set_Project_Modified (P, True);
            Next (Iterator);
         end loop;
      end;
   end Delete_External_Variable;

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

   ---------------------------------------------
   -- Set_Default_Value_For_External_Variable --
   ---------------------------------------------

   procedure Set_Default_Value_For_External_Variable
     (Root_Project      : Project_Type;
      Ext_Variable_Name : String;
      Default           : Name_Id)
   is
      Tree : constant Project_Node_Tree_Ref := Root_Project.Tree;

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable

      --------------
      -- Callback --
      --------------

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Project, Parent, Choice);
      begin
         if Kind_Of (Node, Tree) = N_Typed_Variable_Declaration then
            Set_External_Default_Of
              (Current_Term
                 (First_Term (Expression_Of (Node, Tree), Tree), Tree),
               Tree,
               Enclose_In_Expression
                 (Create_Literal_String (Default, Tree), Tree));
         end if;
      end Callback;

   begin
      if not Is_Editable (Root_Project) then
         Trace (Me, "Project is not editable");
         return;
      end if;

      For_Each_Environment_Variable
        (Root_Project, Get_String (Ext_Variable_Name), No_Name,
         Callback'Unrestricted_Access);
      Set_Project_Modified (Root_Project, True);
   end Set_Default_Value_For_External_Variable;

   ------------------------------
   -- Rename_External_Variable --
   ------------------------------

   procedure Rename_External_Variable
     (Root_Project : Project_Type;
      Variable     : in out Scenario_Variable;
      New_Name     : Namet.Name_Id)
   is
      Tree : constant Project_Node_Tree_Ref := Root_Project.Tree;

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable

      --------------
      -- Callback --
      --------------

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Project, Parent, Choice);
      begin
         if Kind_Of (Node, Tree) = N_External_Value then
            Set_String_Value_Of
              (External_Reference_Of (Node, Tree), Tree, New_Name);
         end if;
      end Callback;

      Ext_Ref : constant Name_Id :=
        Get_String (External_Reference_Of (Variable));
   begin
      if not Is_Editable (Root_Project) then
         Trace (Me, "Project is not editable");
         return;
      end if;

      Projects.Editor.Normalize.Normalize (Root_Project);
      For_Each_Environment_Variable
        (Root_Project, Ext_Ref, No_Name, Callback'Unrestricted_Access);
      Set_Project_Modified (Root_Project, True);

      --  Create the new variable, to avoid errors when computing the view of
      --  the project.
      Variable.Name := New_Name;

      if Value_Of (Root_Project.Tree, Ext_Ref) /= No_Name then
         Set_Value (Project_Registry_Access (Get_Registry (Root_Project)),
                    Variable,
                    Get_String (Value_Of (Root_Project.Tree, Ext_Ref)));
      end if;
   end Rename_External_Variable;

   ----------------------------------
   -- Add_Scenario_Variable_Values --
   ----------------------------------

   procedure Add_Scenario_Variable_Values
     (Root_Project : Project_Type;
      External_Var : Scenario_Variable;
      Values       : Name_Id_Array)
   is
      Tree           : constant Project_Node_Tree_Ref := Root_Project.Tree;
      Type_Node, Var : Project_Node_Id;
      Iter           : Imported_Project_Iterator := Start (Root_Project);
      P              : Project_Type;
   begin
      loop
         P := Current (Iter);
         exit when P = No_Project;

         if not Is_Editable (P) then
            Trace (Me, "Project is not editable");
            return;
         end if;

         Projects.Editor.Normalize.Normalize (P);
         Var := Find_Scenario_Variable (Tree, P, External_Var.Name);

         --  If variable is defined in the current project, then modify the
         --  type to Values.

         if Var /= Empty_Node then
            Type_Node := String_Type_Of (Var, Tree);
            pragma Assert (Type_Node /= Empty_Node);
            --  Set_First_Literal_String (Type_Node, Empty_Node);

            for J in Values'Range loop
               Add_Possible_Value (Tree, Type_Node, Values (J));
            end loop;

            Set_Project_Modified (P, True);
         end if;

         Next (Iter);
      end loop;
   end Add_Scenario_Variable_Values;

   ----------------------
   -- To_Argument_List --
   ----------------------

   function To_Argument_List
     (Tree : Project_Tree_Ref; Value : Variable_Value) return Argument_List
   is
      S : Argument_List (1 .. Length (Tree, Value)) := (others => null);
      V : String_List_Id;

   begin
      case Value.Kind is
         when Undefined =>
            null;

         when Single =>
            Get_Name_String (Value.Value);
            S (1) := new String'(Name_Buffer (1 .. Name_Len));

         when List =>
            V := Value.Values;

            for J in S'Range loop
               Get_Name_String (Tree.String_Elements.Table (V).Value);
               S (J) := new String'(Name_Buffer (1 .. Name_Len));
               V := Tree.String_Elements.Table (V).Next;
            end loop;
      end case;
      return S;
   end To_Argument_List;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Tree : Project_Tree_Ref; Value : Variable_Value) return String
   is
      Buffer     : String (1 .. 1024);
      Current    : Prj.String_List_Id;
      The_String : String_Element;
      Index      : Natural := Buffer'First;

   begin
      case Value.Kind is
         when Prj.Undefined =>
            return "";

         when Prj.Single =>
            Get_Name_String (Value.Value);

            if Value.Index /= 0 then
               Name_Buffer (Name_Len + 1 .. Name_Len + 4) := " at ";
               Name_Len := Name_Len + 4;
               Add_Nat_To_Name_Buffer (Value.Index);
            end if;

            return Name_Buffer (1 .. Name_Len);

         when Prj.List =>
            Current := Value.Values;

            while Current /= Prj.Nil_String loop
               The_String := Tree.String_Elements.Table (Current);
               Get_Name_String (The_String.Value);

               if Index /= Buffer'First then
                  Buffer (Index) := ' ';
                  Index := Index + 1;
               end if;

               Buffer (Index .. Index + Name_Len - 1) :=
                 Name_Buffer (1 .. Name_Len);
               Index := Index + Name_Len;

               Current := The_String.Next;
            end loop;

            return Buffer (Buffer'First .. Index - 1);
      end case;
   end To_String;

   ---------------------
   -- Get_Environment --
   ---------------------

   function Get_Environment
     (Tree             : Project_Node_Tree_Ref;
      Var_Or_Attribute : Project_Node_Id) return Name_Id
   is
      Ext : Project_Node_Id;
   begin
      --  ??? We do not correctly detect constructions like
      --         A : A_Type := "A" & external ("OS");
      --  The external reference must be by itself in the reference

      pragma Assert (Var_Or_Attribute /= Empty_Node);

      Ext := Expression_Of (Var_Or_Attribute, Tree);
      pragma Assert (Kind_Of (Ext, Tree) = N_Expression);
      Ext := First_Term (Ext, Tree);
      pragma Assert (Kind_Of (Ext, Tree) = N_Term);
      Ext := Current_Term (Ext, Tree);

      if Kind_Of (Ext, Tree) = N_External_Value then
         Ext := External_Reference_Of (Ext, Tree);
         if Kind_Of (Ext, Tree) = N_Expression then
            Ext := Current_Term (First_Term (Ext, Tree), Tree);
         end if;

         pragma Assert (Kind_Of (Ext, Tree) = N_Literal_String);
         return String_Value_Of (Ext, Tree);
      else
         return No_Name;
      end if;
   end Get_Environment;

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

   -------------------
   -- Contains_Path --
   -------------------

   function Contains_Path
     (Project : Project_Type;
      Path    : Virtual_File) return Boolean
   is
      Tree   : constant Project_Node_Tree_Ref := Project.Tree;
      Result : Boolean;

      function Normalize (P : Filesystem_String) return Filesystem_String;
      --  Normalizes the paths before comparison

      procedure Test_P (Node : Project_Node_Id);
      --  Test the project's node for existing path

      ---------------
      -- Normalize --
      ---------------

      function Normalize (P : Filesystem_String) return Filesystem_String is
      begin
         if Is_Absolute_Path_Or_URL (P) then
            declare
               File : constant Virtual_File :=
                        Create (P, Project_Path (Project).Get_Host);
               Conv : constant Filesystem_String :=
                        Relative_Path (File, Dir (Project_Path (Project)));
            begin
               return Conv;
            end;
         end if;

         return P;
      end Normalize;

      The_Path : constant Filesystem_String := Normalize (Path.Full_Name);

      ------------
      -- Test_P --
      ------------

      procedure Test_P (Node : Project_Node_Id) is
         Node_P : constant Filesystem_String :=
           Normalize (+Get_String (String_Value_Of (Node, Tree)));
      begin
         --  Test if Node_P is a subdir of The_Path
         if Node_P'Length >= The_Path'Length
           and then File_Equal
             (Node_P (Node_P'First ..  Node_P'First + The_Path'Length - 1),
              The_Path, Get_Nickname (Build_Server))
         then
            Result := True;
         end if;
      end Test_P;

   begin
      Result := False;
      --  Test all paths
      For_Each_Directory_Node (Project, Test_P'Unrestricted_Access);

      return Result;
   end Contains_Path;

   -----------------
   -- Rename_Path --
   -----------------

   function Rename_Path
     (Project            : Project_Type;
      Old_Path           : Virtual_File;
      New_Path           : Virtual_File;
      Use_Relative_Paths : Boolean) return Boolean
   is
      Tree    : constant Project_Node_Tree_Ref := Project.Tree;
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
         if Use_Relative_Paths
           and then Is_Absolute_Path_Or_URL (P)
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
           Path (+Get_String (String_Value_Of (Node, Tree)));
      begin
         if Node_P'Length >= Old_P'Length
           and then File_Equal
             (Node_P (Node_P'First ..  Node_P'First + Old_P'Length - 1),
              Old_P, Get_Nickname (Build_Server))
         then
            Set_String_Value_Of
              (Node, Tree,
               Get_String
                 (+New_P &
                  (+Node_P (Node_P'First + Old_P'Length .. Node_P'Last))));
            Changed := True;
         end if;
      end Rename_P;

   begin
      --  Replace all the paths

      For_Each_Directory_Node (Project, Rename_P'Unrestricted_Access);

      if Changed then
         Set_Project_Modified (Project, True);
      end if;

      return Changed;
   end Rename_Path;

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

   -----------------------------
   -- Find_Project_Of_Package --
   -----------------------------

   function Find_Project_Of_Package
     (Project  : Project_Type;
      Pkg_Name : String) return Project_Type
   is
      Pkg  : Project_Node_Id;
      P    : Project_Type := No_Project;
   begin
      Pkg := Find_Package_Declaration
        (Project.Tree, Project.Node, Get_String (Pkg_Name));

      if Pkg /= Empty_Node
        and then Project_Of_Renamed_Package_Of (Pkg, Project.Tree) /=
        Empty_Node
      then
         Pkg := Project_Of_Renamed_Package_Of (Pkg, Project.Tree);
         P := Get_Project_From_Name
           (Project_Registry (Get_Registry (Project).all),
            Prj.Tree.Name_Of (Pkg, Project.Tree));
      end if;

      if P = No_Project then
         P := Project;
      end if;
      return P;
   end Find_Project_Of_Package;

   ----------------------
   -- Reset_All_Caches --
   ----------------------

   procedure Reset_All_Caches (Project : Project_Type) is
      Root  : constant Project_Type := Get_Root_Project
        (Project_Registry (Get_Registry (Project).all));
      Iter  : Imported_Project_Iterator := Start (Root, Recursive => True);
      Count : Natural := 0;
   begin
      while Current (Iter) /= No_Project loop
         Count := Count + 1;
         Projects.Next (Iter);
      end loop;

      declare
         Prjs : Project_Type_Array (1 .. Count);
      begin
         Count := Prjs'First;
         Iter := Start (Root, Recursive => True);
         while Current (Iter) /= No_Project loop
            Prjs (Count) := Current (Iter);
            Count := Count + 1;
            Projects.Next (Iter);
         end loop;

         for P in Prjs'Range loop
            Reset_Cache (Prjs (P), Imported_By => False);
            Reset_Cache (Prjs (P), Imported_By => True);
         end loop;
      end;
   end Reset_All_Caches;

   -----------------------------
   -- Remove_Imported_Project --
   -----------------------------

   procedure Remove_Imported_Project
     (Project : Project_Type; Imported_Project : Project_Type)
   is
      Tree        : constant Project_Node_Tree_Ref := Project.Tree;
      With_Clause : Project_Node_Id :=
                      First_With_Clause_Of (Project.Node, Tree);
      Next        : Project_Node_Id;
   begin
      --  ??? When the project is no longer found in the hierarchy, it should
      --  also be removed from the htable in Prj.Tree, so that another
      --  project by that name can be loaded.

      if With_Clause /= Empty_Node
        and then Prj.Tree.Name_Of (With_Clause, Tree) =
        Prj.Tree.Name_Of (Imported_Project.Node, Tree)
      then
         Set_First_With_Clause_Of
           (Project.Node, Tree, Next_With_Clause_Of (With_Clause, Tree));
      else
         loop
            Next := Next_With_Clause_Of (With_Clause, Tree);
            exit when Next = Empty_Node;

            if Prj.Tree.Name_Of (Next, Tree) =
              Prj.Tree.Name_Of (Imported_Project.Node, Tree)
            then
               Set_Next_With_Clause_Of
                 (With_Clause, Tree, Next_With_Clause_Of (Next, Tree));
            end if;

            With_Clause := Next;
         end loop;
      end if;

      Set_Project_Modified (Project, True);

      --  Need to reset all the caches, since the caches contain the indirect
      --  dependencies as well.
      Reset_All_Caches (Project);
   end Remove_Imported_Project;

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
              (Create (Dir_Name
                 (+Get_String (Path_Name_Of (Importing_Project, Tree))))));

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
     (Root_Project              : Project_Type;
      Project                   : Project_Type;
      Imported_Project          : Project_Node_Id;
      Imported_Project_Location : GNATCOLL.VFS.Virtual_File;
      Report_Errors             : Output.Output_Proc := null;
      Use_Relative_Path         : Boolean;
      Use_Base_Name             : Boolean;
      Limited_With              : Boolean := False)
      return Import_Project_Error
   is
      use Prj.Tree.Tree_Private_Part;
      use type Output.Output_Proc;

      Tree : constant Project_Node_Tree_Ref := Root_Project.Tree;

      procedure Fail (S : String);
      --  Replaces Osint.Fail

      ----------
      -- Fail --
      ----------

      procedure Fail (S : String) is
      begin
         if Report_Errors /= null then
            Report_Errors (S);
         end if;
      end Fail;

      With_Clause : Project_Node_Id;
      Iter        : Imported_Project_Iterator;
      Tmp_Prj     : Project_Type;

   begin
      Prj_Output.Set_Special_Output (Fail'Unrestricted_Access);
      Prj.Com.Fail := Fail'Unrestricted_Access;

      --  Make sure we are not trying to import ourselves, since otherwise it
      --  would result in an infinite loop when manipulating the project.

      if Prj.Tree.Name_Of (Project.Node, Tree) =
        Prj.Tree.Name_Of (Imported_Project, Tree)
      then
         Fail (-"Cannot add dependency to self");
         Output.Set_Special_Output (null);
         Prj.Com.Fail := null;
         return Dependency_On_Self;
      end if;

      --  Check if it is already there. If we have the same name but not the
      --  same path, we replace it anyway.

      With_Clause := First_With_Clause_Of (Project.Node, Tree);

      while With_Clause /= Empty_Node loop
         if Prj.Tree.Name_Of (Project_Node_Of (With_Clause, Tree), Tree) =
           Prj.Tree.Name_Of (Imported_Project, Tree)
         then
            Fail (-"There is already a dependency on "
                  & Get_String (Prj.Tree.Name_Of (Imported_Project, Tree)));
            Prj_Output.Cancel_Special_Output;
            Prj.Com.Fail := null;
            return Dependency_Already_Exists;
         end if;
         With_Clause := Next_With_Clause_Of (With_Clause, Tree);
      end loop;

      With_Clause := Default_Project_Node (Tree, N_With_Clause);
      Set_Name_Of
        (With_Clause, Tree, Prj.Tree.Name_Of (Imported_Project, Tree));

      Set_Next_With_Clause_Of
        (With_Clause, Tree, First_With_Clause_Of (Project.Node, Tree));
      Set_First_With_Clause_Of (Project.Node, Tree, With_Clause);

      Set_With_Clause_Path
        (Tree, With_Clause,
         Imported_Project_Location,
         Imported_Project, Project.Node,
         Use_Relative_Path => Use_Relative_Path,
         Use_Base_Name     => Use_Base_Name,
         Limited_With      => Limited_With);

      if Has_Circular_Dependencies (Project.Tree, Project.Node) then
         Set_First_With_Clause_Of
           (Project.Node, Tree, Next_With_Clause_Of (With_Clause, Tree));
         Fail (-"Circular dependency detected in the project hierarchy");
         Trace (Me, "Circular dependency detected in the project hierarchy");
         Prj_Output.Cancel_Special_Output;
         Prj.Com.Fail := null;
         return Circular_Dependency;
      end if;

      Prj_Output.Cancel_Special_Output;
      Prj.Com.Fail := null;

      Set_Project_Modified (Project, True);

      --  Reset all importing caches, since they store the list recursively

      Iter := Start (Root_Project);
      loop
         Tmp_Prj := Current (Iter);
         Reset_Cache (Tmp_Prj, Imported_By => False);
         exit when Tmp_Prj = Root_Project;
         Next (Iter);
      end loop;

      Reset_Cache
        (Get_Project_From_Name
           (Project_Registry'Class (Get_Registry (Project).all),
            Get_String
              (To_Lower
                 (+Base_Name
                    (Imported_Project_Location,  Project_File_Extension)))),
         Imported_By => True);
         return Success;
   exception
      when others =>
         Output.Set_Special_Output (null);
         Prj.Com.Fail := null;
         raise;
   end Add_Imported_Project;

   --------------------------
   -- Add_Imported_Project --
   --------------------------

   function Add_Imported_Project
     (Root_Project      : Project_Type;
      Project           : Project_Type;
      Imported_Project  : Project_Type;
      Report_Errors     : Output.Output_Proc := null;
      Use_Relative_Path : Boolean;
      Use_Base_Name     : Boolean := False;
      Limited_With      : Boolean := False) return Import_Project_Error is
   begin
      return Add_Imported_Project
        (Root_Project              => Root_Project,
         Project                   => Project,
         Imported_Project          => Imported_Project.Node,
         Imported_Project_Location => Project_Path (Imported_Project),
         Report_Errors             => Report_Errors,
         Use_Relative_Path         => Use_Relative_Path,
         Use_Base_Name             => Use_Base_Name,
         Limited_With              => Limited_With);
   end Add_Imported_Project;

   --------------------------
   -- Add_Imported_Project --
   --------------------------

   function Add_Imported_Project
     (Root_Project              : Project_Type;
      Project                   : Project_Type;
      Imported_Project_Location : GNATCOLL.VFS.Virtual_File;
      Report_Errors             : Output.Output_Proc := null;
      Use_Relative_Path         : Boolean;
      Use_Base_Name             : Boolean := False;
      Limited_With              : Boolean := False)
      return Import_Project_Error
   is
      use Prj.Tree.Tree_Private_Part;
      use type Output.Output_Proc;

      Tree : constant Project_Node_Tree_Ref := Root_Project.Tree;

      procedure Fail (S : String);
      --  Replaces Osint.Fail

      ----------
      -- Fail --
      ----------

      procedure Fail (S : String) is
      begin
         if Report_Errors /= null then
            Report_Errors (S);
         end if;
      end Fail;

      Basename         : constant Filesystem_String := Base_Name
        (Imported_Project_Location, Project_File_Extension);
      Imported_Project : Project_Node_Id := Empty_Node;
      Dep_ID           : Name_Id;
      Dep_Name         : Prj.Tree.Tree_Private_Part.Project_Name_And_Node;

   begin
      Prj_Output.Set_Special_Output (Fail'Unrestricted_Access);
      Prj.Com.Fail := Fail'Unrestricted_Access;

      Dep_ID := Get_String (+Basename);

      Dep_Name := Tree_Private_Part.Projects_Htable.Get
        (Tree.Projects_HT, Dep_ID);

      if Dep_Name /= No_Project_Name_And_Node then
         if not File_Equal
           (Format_Pathname
              (+Get_String (Path_Name_Of (Dep_Name.Node, Tree))),
            Full_Name (Imported_Project_Location),
            Get_Nickname (Build_Server))
         then
            Fail
              (-"A different project with the same name"
               & " already exists in the project tree.");
            Prj_Output.Cancel_Special_Output;
            Prj.Com.Fail := null;
            return Project_Already_Exists;
         else
            Imported_Project := Dep_Name.Node;
         end if;

      else
         Prj.Part.Parse
           (Tree, Imported_Project,
            +Full_Name (Imported_Project_Location),
            Is_Config_File         => False,
            Current_Directory      => Get_Current_Dir,
            Flags                  => Create_Flags (null),
            Always_Errout_Finalize => True);

         Prj.Err.Finalize;
      end if;

      if Imported_Project = Empty_Node then
         Trace (Me, "Add_Imported_Project: imported project not found ("
                & Imported_Project_Location.Display_Full_Name & ")");
         Prj_Output.Cancel_Special_Output;
         Prj.Com.Fail := null;
         return Imported_Project_Not_Found;
      end if;

      return Add_Imported_Project
        (Root_Project              => Root_Project,
         Project                   => Project,
         Imported_Project          => Imported_Project,
         Imported_Project_Location => Imported_Project_Location,
         Report_Errors             => Report_Errors,
         Use_Relative_Path         => Use_Relative_Path,
         Use_Base_Name             => Use_Base_Name,
         Limited_With              => Limited_With);
   end Add_Imported_Project;

   -----------------------------
   -- For_Each_Directory_Node --
   -----------------------------

   procedure For_Each_Directory_Node
     (Project : Project_Type; Action  : Node_Callback)
   is
      Tree : constant Project_Node_Tree_Ref := Project.Tree;

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
                  if Prj.Tree.Name_Of (Current, Tree) = Name_Source_Dirs
                    or else Prj.Tree.Name_Of (Current, Tree) = Name_Object_Dir
                    or else Prj.Tree.Name_Of (Current, Tree) = Name_Exec_Dir
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
     (Root_Project  : Project_Type;
      Project       : Project_Type;
      New_Name      : String;
      New_Path      : Virtual_File;
      Report_Errors : Output.Output_Proc := null)
   is
      Tree     : constant Project_Node_Tree_Ref := Root_Project.Tree;
      Old_Path : constant Virtual_File := Project_Directory (Project);

      procedure Change_Directory (Node : Project_Node_Id);
      --  Change the directory refered to by Node

      ----------------------
      -- Change_Directory --
      ----------------------

      procedure Change_Directory (Node : Project_Node_Id) is
      begin
         case Kind_Of (Node, Tree) is
            when N_Literal_String =>
               declare
                  D : constant Filesystem_String :=
                        +Get_String (String_Value_Of (Node, Tree));
                  File : constant Virtual_File :=
                           Create
                             (Normalize_Pathname
                                (D, Full_Name (Old_Path),
                                 Resolve_Links => False),
                              Get_Host (New_Path));
               begin
                  if not Is_Absolute_Path (D) then
                     Set_String_Value_Of
                       (Node, Tree,
                        Get_String
                          (+Relative_Path
                             (File, New_Path)));
                  end if;
               end;

            when others =>
               Trace (Me, "For_Each_Directory_Node: unknown node type: "
                      & Kind_Of (Node, Tree)'Img);
         end case;
      end Change_Directory;

      D           : constant Filesystem_String :=
                      Name_As_Directory (Full_Name (New_Path))
                      & To_File_Name (+New_Name) & Project_File_Extension;
      Full_Path   : Name_Id := No_Name;
      Name        : constant Name_Id := Get_String (New_Name);
      Old_Name    : constant Name_Id := Prj.Tree.Name_Of (Project.Node, Tree);
      Old         : constant Project_Node_Id :=
                      Find_Project_In_Hierarchy (Root_Project, Name);
      Imported    : Project_Type;
      Iterator    : Imported_Project_Iterator;
      With_Clause : Project_Node_Id;
      Modified    : Boolean;

   begin
      if Old /= Empty_Node
        and then Old /= Project.Node
      then
         Trace (Me, "Rename_And_Move: project " & New_Name
                & " already exists in the hierarchy");
         Report_Errors
           (-"Couldn't rename the project to " & New_Name
            & ASCII.LF & (-"Project already exists in the project graph"));
         return;
      end if;

      --  Replace all the with_clauses in the project hierarchy that points to
      --  Project.

      Iterator := Find_All_Projects_Importing (Project, Direct_Only => False);

      loop
         Imported := Current (Iterator);
         exit when Imported = No_Project;

         With_Clause := First_With_Clause_Of (Imported.Node, Tree);
         Modified := False;

         while With_Clause /= Empty_Node loop
            if Project_Node_Of (With_Clause, Tree) = Project.Node then
               Set_Name_Of (With_Clause, Tree, Name);

               Set_Path_Name_Of (With_Clause, Tree,
                                 Path_Name_Of (Project.Node, Tree));

               if Full_Path = No_Name then
                  Full_Path := Get_String (+D);
               end if;

               Set_String_Value_Of (With_Clause, Tree, Full_Path);
               Modified := True;
            end if;

            With_Clause := Next_With_Clause_Of (With_Clause, Tree);
         end loop;

         if Modified then
            Set_Project_Modified (Imported, True);
            Reset_Cache (Imported, Imported_By => True);
            Reset_Cache (Imported, Imported_By => False);
         end if;

         Next (Iterator);
      end loop;

      --  If the file was moved, update the source directories so that we still
      --  point to the same physical directories.

      if New_Path /= Project_Directory (Project) then
         For_Each_Directory_Node
           (Project, Change_Directory'Unrestricted_Access);
      end if;

      Set_Name_Of (Project.Node, Tree, Name);
      Set_Directory_Of
        (Project.Node,
         Tree,
         Get_String (+Full_Name (New_Path)));
      Set_Path_Name_Of (Project.Node, Tree, Get_String (+D));

      --  We do not want to reread the display_name from the source, which is
      --  no longer up-to-date, so we'll force its refresh from the tree
      Set_Location_Of (Project.Node, Tree, No_Location);

      --  Unregister the old name
      Prj.Tree.Tree_Private_Part.Projects_Htable.Set
        (Tree.Projects_HT,
         Prj.Tree.Name_Of (Project.Node, Tree),
         Prj.Tree.Tree_Private_Part.Project_Name_And_Node'
         (Name           => Old_Name,
          Display_Name   => Old_Name,
          Node           => Empty_Node,
          Canonical_Path => Path_Name_Type (Old_Name),
          Extended       => False,
          Proj_Qualifier => Unspecified));

      --  Register the new name
      Prj.Tree.Tree_Private_Part.Projects_Htable.Set
        (Tree.Projects_HT,
         Prj.Tree.Name_Of (Project.Node, Tree),
         Prj.Tree.Tree_Private_Part.Project_Name_And_Node'
         (Name           => Name,
          Display_Name   => Name,
          Canonical_Path => Get_String (+D),
          Node           => Project.Node,
          Extended       => False,
          Proj_Qualifier => Unspecified));

      Reset_Name_Table
        (Project_Registry (Get_Registry (Project).all),
         Project, Get_String (+Old_Name), New_Name);

      Set_Project_Modified (Project, True);
      Reset_Cache (Project, Imported_By => True);
      Reset_Cache (Project, Imported_By => False);

      --  This is no longer the default project, since it was
      --  renamed. Otherwise, Project_Path would still return "" when saving
      --  the default project.

      Set_Status (Project, From_File);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Rename_And_Move;

   --------------------
   -- Create_Project --
   --------------------

   function Create_Project
     (Registry : Projects.Registry.Project_Registry'Class;
      Name     : String;
      Path     : GNATCOLL.VFS.Virtual_File;
      Is_Config_File : Boolean := False) return Prj.Tree.Project_Node_Id
   is
      D            : constant Filesystem_String :=
                       Name_As_Directory (Full_Name (Path))
                       & To_File_Name (+Name) & Project_File_Extension;
   begin
      return Prj.Tree.Create_Project
        (In_Tree        => Get_Tree (Registry),
         Name           => Get_String (Name),
         Full_Path      => Get_String (+D),
         Is_Config_File => Is_Config_File);
   end Create_Project;

   --------------------
   -- Create_Project --
   --------------------

   function Create_Project
     (Registry : Projects.Registry.Project_Registry'Class;
      Name     : String;
      Path     : GNATCOLL.VFS.Virtual_File) return Project_Type
   is
      Tree    : constant Project_Node_Tree_Ref := Get_Tree (Registry);
      Project : constant Project_Node_Id :=
        Create_Project (Registry, Name, Path, Is_Config_File => False);
      P       : Project_Type;
   begin
      Reset_Project_Name_Hash (Registry, Prj.Tree.Name_Of (Project, Tree));
      P := Get_Project_From_Name (Registry, Prj.Tree.Name_Of (Project, Tree));
      Set_Project_Modified (P, True);
      return P;
   end Create_Project;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Str : String) return Name_Id is
   begin
      Name_Len := Str'Length;
      Name_Buffer (1 .. Name_Len) := Str;
      return Name_Find;
   end Get_String;

   function Get_String (Str : String) return Namet.File_Name_Type is
   begin
      Name_Len := Str'Length;
      Name_Buffer (1 .. Name_Len) := Str;
      return Name_Find;
   end Get_String;

   function Get_String (Str : String) return Namet.Path_Name_Type is
   begin
      Name_Len := Str'Length;
      Name_Buffer (1 .. Name_Len) := Str;
      return Name_Find;
   end Get_String;

   ---------------------------------
   -- Replace_Project_Occurrences --
   ---------------------------------

   procedure Replace_Project_Occurrences
     (Root_Project      : Project_Type;
      Project           : Project_Type;
      Use_Relative_Path : Boolean)
   is
      Tree          : constant Project_Node_Tree_Ref := Root_Project.Tree;
      Iterator      : Imported_Project_Iterator :=
                        Start (Root_Project, Recursive => True);
      With_Clause   : Project_Node_Id;
      P             : Project_Type;

   begin
      loop
         P := Current (Iterator);
         exit when P = No_Project;

         With_Clause := First_With_Clause_Of (P.Node, Tree);
         while With_Clause /= Empty_Node loop
            if Prj.Tree.Name_Of (Project_Node_Of (With_Clause, Tree), Tree) =
              Prj.Tree.Name_Of (Project.Node, Tree)
            then
               Set_With_Clause_Path
                 (Tree, With_Clause, Project_Path (Project),
                  Project.Node,
                  P.Node,
                  Use_Base_Name     => False,
                  Use_Relative_Path => Use_Relative_Path);
               Set_Project_Modified (P, True);
               Reset_Cache (P, Imported_By => True);
            end if;
            With_Clause := Next_With_Clause_Of (With_Clause, Tree);
         end loop;

         Next (Iterator);
      end loop;
      Reset_Cache (Project, Imported_By => False);
   end Replace_Project_Occurrences;

   ---------------------------------
   -- Create_Environment_Variable --
   ---------------------------------

   function Create_Environment_Variable
     (Project   : Project_Type;
      Name      : String;
      Type_Name : String;
      Env_Name  : String) return Scenario_Variable
   is
      Tree     : constant Project_Node_Tree_Ref := Project.Tree;
      Typ, Var : Project_Node_Id;
   begin
      if not Is_Editable (Project) then
         Trace (Me, "Project is not editable");
         return No_Variable;
      end if;

      Projects.Editor.Normalize.Normalize (Project);
      Typ := Create_Type (Tree, Project.Node, Type_Name);
      Var := Create_Typed_Variable
        (Tree, Project.Node, Name, Typ, Add_Before_First_Case_Or_Pkg => True);
      Set_Value_As_External (Tree, Var, Env_Name);

      Set_Project_Modified (Project, True);

      Reset_Scenario_Variables_Cache
        (Project_Registry'Class (Get_Registry (Project).all));

      return (Name        => Get_String (Env_Name),
              Default     => No_Name,
              String_Type => Typ);
   end Create_Environment_Variable;

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

   ---------------------
   -- Normalize_Cases --
   ---------------------

   procedure Normalize_Cases (Project : Projects.Project_Type) is

      Tree : constant Project_Node_Tree_Ref := Project.Tree;

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
   -- Set_Extended_Project --
   --------------------------

   procedure Set_Extended_Project
     (Project            : Projects.Project_Type;
      Extended           : Projects.Project_Type;
      Extend_All         : Boolean := False;
      Use_Relative_Paths : Boolean := False)
   is
   begin
      if Use_Relative_Paths then
         declare
            Path : constant Filesystem_String :=
                     Relative_Path (Project_Path (Extended),
                                    Project_Directory (Project));
         begin
            Set_Extended_Project_Path_Of
              (Project.Node,
               Project.Tree,
               To => Get_String (+Path));
         end;
      else
         Set_Extended_Project_Path_Of
           (Project.Node,
            Project.Tree,
            To => Get_String (+Full_Name (Project_Path (Extended))));
      end if;

      Set_Extended_Project_Of
        (Project_Declaration_Of (Project.Node, Project.Tree),
         Project.Tree,
         To => Extended.Node);

      if Extend_All then
         Set_Is_Extending_All (Project.Node, Project.Tree);
      end if;
   end Set_Extended_Project;

end Projects.Editor;
