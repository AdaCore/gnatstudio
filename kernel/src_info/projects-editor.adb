
with Ada.Unchecked_Deallocation;
with File_Utils;                use File_Utils;
with Glide_Intl;                use Glide_Intl;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Namet;                     use Namet;
with Prj.Attr;                  use Prj.Attr;
with Prj.Com;                   use Prj.Com;
with Prj.Ext;                   use Prj.Ext;
with Prj.Part;                  use Prj.Part;
with Prj.Tree;                  use Prj.Tree;
with Prj.Util;                  use Prj.Util;
with Prj;                       use Prj;
with Projects.Graphs;           use Projects.Graphs;
with Projects.Registry;         use Projects.Registry;
with Projects.Editor.Normalize; use Projects.Editor.Normalize;
with Snames;                    use Snames;
with String_Utils;              use String_Utils;
with Stringt;                   use Stringt;
with Traces;                    use Traces;
with Types;                     use Types;

package body Projects.Editor is

   Me : constant Debug_Handle := Create ("Projects.Editor");

   type Project_Node_Array_Access is access Project_Node_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Project_Node_Array, Project_Node_Array_Access);


   --------------
   -- Projects --
   --------------

   function Find_Project_In_Hierarchy
     (Root_Project : Project_Type; Name : Types.Name_Id)
      return Project_Node_Id;
   --  Find in the project tree starting at Root_Project a subproject called
   --  Name.
   --  This is different from using directly Prj.Tree.Projects_Htable, since we
   --  only look for the project in the hierarchy of Root_Project.

   procedure Set_With_Clause_Path
     (With_Clause               : Project_Node_Id;
      Imported_Project_Location : String;
      Imported_Project          : Project_Node_Id;
      Importing_Project         : Project_Node_Id;
      Use_Relative_Path         : Boolean);
   --  Set the attributes of the with_clause (imported project node, imported
   --  project path,....)

   ---------------
   -- Variables --
   ---------------

   function Find_Scenario_Variable
     (Project : Project_Type; External_Name : Types.Name_Id)
      return Project_Node_Id;
   --  Return the declaration of the scenario variable associated with
   --  the external variable External_Name.
   --  In normalized projects, there should be only such variable.

   function Length (Value : Variable_Value) return Integer;
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
      Ext_Variable_Name : String;
      Specific_Choice   : String;
      Action            : Environment_Variable_Callback);
   --  Iterate over all possible references to an external variable. This
   --  returns N_External_Value, N_Variable_Reference,
   --  N_Typed_Variable_Declaration and N_String_Type_Declaration (the last
   --  three are indirect references through a named variable.

   type Node_Callback is access procedure (Node : Project_Node_Id);

   procedure For_Each_Directory_Node
     (Project : Project_Type; Action  : Node_Callback);
   --  For each node that deals with a procedure, calls Action.

   procedure Remove_Variable_Declaration
     (Project_Or_Package : Project_Node_Id;
      Declaration        : Project_Node_Id);
   --  Remove the variable declaration from the list of variables in
   --  Project_Or_Package.

   ----------------
   -- Attributes --
   ----------------

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

   function Find_Last_Declaration_Of
     (Parent     : Project_Node_Id;
      Attr_Name  : Types.Name_Id;
      Attr_Index : String := "") return Project_Node_Id;
   --  Find the last declaration for the attribute Attr_Name, in the
   --  declarative list contained in Parent.
   --  The returned value is the last such declaration, or Empty_Node if there
   --  was none.
   --  This returns the current item of the declarative item

   procedure Remove_Attribute_Declarations
     (Parent          : Project_Node_Id;
      Attribute_Name  : Name_Id;
      Attribute_Index : String);
   --  Remove all declarations for Attribute_Name in the declarative item list
   --  of Parent.
   --  If Attribute_Index is Any_Attribute, no matching is done on the index.

   function Attribute_Matches
     (Node            : Project_Node_Id;
      Attribute_Name  : Name_Id;
      Attribute_Index : String) return Boolean;
   --  Return True if Node is an attribute declaration matching Attribute_Name
   --  and Attribute_Index.
   --  If Attribute_Index is Any_Attribute, no matching is done on the index.

   procedure Update_Attribute_Value_In_Scenario
     (Project            : Project_Node_Id;
      Pkg_Name           : String := "";
      Scenario_Variables : Scenario_Variable_Array;
      Attribute_Name     : String := "";
      Values             : GNAT.OS_Lib.Argument_List;
      Attribute_Index    : String := "";
      Prepend            : Boolean := False);
   procedure Update_Attribute_Value_In_Scenario
     (Project            : Project_Node_Id;
      Pkg_Name           : String := "";
      Scenario_Variables : Scenario_Variable_Array;
      Attribute_Name     : String := "";
      Value              : String;
      Attribute_Index    : String := "");
   --  Internal version of Update_Attribute_Value_In_Scenario

   --------------
   -- Packages --
   --------------

   function Get_Or_Create_Package
     (Project : Project_Node_Id; Pkg : String) return Project_Node_Id;
   --  Create (or get an existing) package in project.
   --
   --  ??? Should always create, since we can use a find_* function to get an
   --  existing one.

   function Find_Package_Declaration
     (Project : Project_Node_Id; Name : Types.Name_Id)
     return Project_Node_Id;
   --  Return the package whose name is Name, or Empty_Node if there is none

   -----------------
   -- Expressions --
   -----------------

   function Enclose_In_Expression
     (Node : Project_Node_Id) return Project_Node_Id;
   --  Enclose the Node inside a N_Expression node, and return this expression.

   function String_As_Expression
     (Value : Types.String_Id) return Project_Node_Id;
   --  Return an N_Expression node that represents the static string Value.
   --  ??? Could be implemented in terms of Concatenate.

   procedure Set_Expression
     (Var_Or_Attribute : Project_Node_Id; Expr : Project_Node_Id);
   --  Set Var as the expression to use for the value of Var. This
   --  properly handles standard variables and variables defined through
   --  references to external environment variables.

   ----------
   -- Misc --
   ----------

   function Create_Literal_String (Str : Types.String_Id)
      return Project_Node_Id;
   --  Create a literal string whose value is Str.

   function Find_Node_By_Name
     (Project : Project_Node_Id;
      Kind    : Project_Node_Kind;
      Name    : Name_Id) return Project_Node_Id;
   --  Find a node given its name

   procedure Remove_Node
     (Parent : Project_Node_Id; Node : Project_Node_Id);
   --  Remove Node from the declaration list in Parent.
   --  This doesn't search recursively inside nested packages, case
   --  constructions, ...

   procedure Move_From_Common_To_Case_Construct
     (Project            : Project_Node_Id;
      Pkg_Name           : String;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute_Name     : Types.Name_Id;
      Attribute_Index    : String := "");
   --  Move any declaration for the attribute from the common part of the
   --  project into each branch of the nested case construct. Nothing is done
   --  if there is no such declaration.

   procedure Add_Node_To_List
     (To   : in out Project_Node_Array_Access;
      Last : in out Natural;
      Node : Project_Node_Id);
   --  Add a new node into the list of nodes To.
   --  To is resized as needed

   ------------
   -- Length --
   ------------

   function Length (List : Prj.String_List_Id) return Natural is
      L     : String_List_Id := List;
      Count : Natural        := 0;
   begin
      while L /= Nil_String loop
         Count := Count + 1;
         L := String_Elements.Table (L).Next;
      end loop;
      return Count;
   end Length;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal (Str1 : String_Id; Str2 : String) return Boolean is
      pragma Suppress (All_Checks);
      L     : constant Int := String_Length (Str1);
      Index : Int := 1;
   begin
      if Integer (L) = Str2'Length then
         for S in Str2'Range loop
            if Get_Char_Code (Str2 (S)) /= Get_String_Char (Str1, Index) then
               return False;
            end if;

            Index := Index + 1;
         end loop;
      else
         return False;
      end if;

      return True;
   end Is_Equal;

   ---------------------------
   -- Create_Literal_String --
   ---------------------------

   function Create_Literal_String (Str : Types.String_Id)
      return Project_Node_Id
   is
      Node : Project_Node_Id;
   begin
      Node := Default_Project_Node (N_Literal_String, Prj.Single);
      Set_Next_Literal_String (Node, Empty_Node);
      Set_String_Value_Of (Node, Str);
      return Node;
   end Create_Literal_String;

   --------------------------
   -- String_As_Expression --
   --------------------------

   function String_As_Expression (Value : String_Id) return Project_Node_Id is
   begin
      return Enclose_In_Expression (Create_Literal_String (Value));
   end String_As_Expression;

   ---------------------------
   -- Enclose_In_Expression --
   ---------------------------

   function Enclose_In_Expression (Node : Project_Node_Id)
      return Project_Node_Id
   is
      Expr : constant Project_Node_Id :=
        Default_Project_Node (N_Expression, Single);
   begin
      Set_First_Term (Expr, Default_Project_Node (N_Term, Single));
      Set_Current_Term (First_Term (Expr), Node);
      return Expr;
   end Enclose_In_Expression;

   ------------------------------
   -- Find_Package_Declaration --
   ------------------------------

   function Find_Package_Declaration
     (Project : Project_Node_Id; Name : Types.Name_Id)
      return Project_Node_Id is
   begin
      return Find_Node_By_Name (Project, N_Package_Declaration, Name);
   end Find_Package_Declaration;

   ---------------------------
   -- Get_Or_Create_Package --
   ---------------------------

   function Get_Or_Create_Package
     (Project : Project_Node_Id; Pkg : String) return Project_Node_Id
   is
      Pack : Project_Node_Id;
      N    : Name_Id;
   begin
      N := Get_String (Pkg);

      --  Check if the package already exists

      Pack := First_Package_Of (Project);

      while Pack /= Empty_Node loop
         if Prj.Tree.Name_Of (Pack) = N then
            return Pack;
         end if;

         Pack := Next_Package_In_Project (Pack);
      end loop;

      --  Create the package and add it to the declarative item

      Pack := Default_Project_Node (N_Package_Declaration);
      Set_Name_Of (Pack, N);

      --  Find the correct package id to use

      for Index in Package_First .. Package_Attributes.Last loop
         if N = Package_Attributes.Table (Index).Name then
            Set_Package_Id_Of (Pack, Index);
            exit;
         end if;
      end loop;

      --  Add it to the list of packages
      Set_Next_Package_In_Project (Pack, First_Package_Of (Project));
      Set_First_Package_Of (Project, Pack);

      Add_At_End (Project_Declaration_Of (Project), Pack);

      return Pack;
   end Get_Or_Create_Package;

   -----------------------
   -- Find_Node_By_Name --
   -----------------------

   function Find_Node_By_Name
     (Project : Project_Node_Id;
      Kind    : Project_Node_Kind;
      Name    : Name_Id) return Project_Node_Id
   is
      Decl : Project_Node_Id := First_Declarative_Item_Of
        (Project_Declaration_Of (Project));
      Current : Project_Node_Id;
   begin
      while Decl /= Empty_Node loop
         Current := Current_Item_Node (Decl);
         if Kind_Of (Current) = Kind
           and then Prj.Tree.Name_Of (Current) = Name
         then
            return Current;
         end if;

         Decl := Next_Declarative_Item (Decl);
      end loop;
      return Empty_Node;
   end Find_Node_By_Name;

   ----------------------
   -- Create_Attribute --
   ----------------------

   function Create_Attribute
     (Prj_Or_Pkg : Project_Node_Id;
      Name       : String;
      Index_Name : String := "";
      Kind       : Variable_Kind := List) return Project_Node_Id
   is
      Node : constant Project_Node_Id :=
        Default_Project_Node (N_Attribute_Declaration, Kind);
   begin
      Set_Name_Of (Node, Get_String (Name));

      if Index_Name /= "" then
         Start_String;
         Store_String_Chars (Index_Name);
         Set_Associative_Array_Index_Of (Node, End_String);
      end if;

      Add_At_End (Prj_Or_Pkg, Node);

      return Node;
   end Create_Attribute;

   ------------
   -- Length --
   ------------

   function Length (Value : Variable_Value) return Integer is
   begin
      case Value.Kind is
         when Undefined => return 0;
         when Single    => return 1;
         when List      => return Length (Value.Values);
      end case;
   end Length;

   -----------------
   -- Remove_Node --
   -----------------

   procedure Remove_Node
     (Parent : Project_Node_Id; Node : Project_Node_Id)
   is
      P    : Project_Node_Id := Parent;
      Decl, Next : Project_Node_Id;
   begin
      --  ??? Should reset the list of Variables and Types if the node matches
      if Kind_Of (Parent) = N_Project then
         P := Project_Declaration_Of (Parent);
      end if;

      Decl := First_Declarative_Item_Of (P);

      if Current_Item_Node (Decl) = Node then
         Set_First_Declarative_Item_Of
           (P, Next_Declarative_Item (Decl));
      end if;

      while Decl /= Empty_Node loop
         Next := Next_Declarative_Item (Decl);
         if Next /= Empty_Node
           and then Current_Item_Node (Next) = Node
         then
            Set_Next_Declarative_Item
              (Decl, Next_Declarative_Item (Next));
            exit;
         end if;

         Decl := Next;
      end loop;
   end Remove_Node;

   --------------------
   -- Set_Expression --
   --------------------

   procedure Set_Expression
     (Var_Or_Attribute : Project_Node_Id; Expr : Project_Node_Id)
   is
      E : Project_Node_Id;
   begin
      E := Expression_Of (Var_Or_Attribute);

      if E = Empty_Node then
         Set_Expression_Of (Var_Or_Attribute, Expr);

      else
         case Kind_Of (E) is
            when N_Expression =>
               Set_Expression_Of (Var_Or_Attribute, Expr);
            when N_External_Value =>
               Set_External_Default_Of (E, Expr);
            when others =>
               raise Program_Error;
         end case;
      end if;
   end Set_Expression;

   ----------------------------
   -- Find_Scenario_Variable --
   ----------------------------

   function Find_Scenario_Variable
     (Project : Project_Type; External_Name : Name_Id)
      return Project_Node_Id
   is
      Decl : Project_Node_Id := First_Declarative_Item_Of
        (Project_Declaration_Of (Project.Node));
      Current : Project_Node_Id;
      Name : constant String := Get_String (External_Name);
   begin
      while Decl /= Empty_Node loop
         Current := Current_Item_Node (Decl);
         if Kind_Of (Current) = N_Typed_Variable_Declaration
           and then Is_External_Variable (Current)
         then
            String_To_Name_Buffer (External_Reference_Of (Current));
            if Name_Buffer (1 .. Name_Len) = Name then
               return Current;
            end if;
         end if;

         Decl := Next_Declarative_Item (Decl);
      end loop;
      return Empty_Node;
   end Find_Scenario_Variable;

   -------------------------------
   -- Find_Project_In_Hierarchy --
   -------------------------------

   function Find_Project_In_Hierarchy
     (Root_Project : Project_Type; Name : Types.Name_Id)
      return Project_Node_Id
   is
      Iter : Imported_Project_Iterator := Start (Root_Project);
   begin
      while Current (Iter) /= No_Project loop
         if Prj.Tree.Name_Of (Current (Iter).Node) = Name then
            return Current (Iter).Node;
         end if;

         Next (Iter);
      end loop;
      return Empty_Node;
   end Find_Project_In_Hierarchy;

   --------------
   -- Value_Of --
   --------------

   function Value_Of (Var : Scenario_Variable) return String_List_Iterator is
      V, Expr : Project_Node_Id;
   begin
      case Kind_Of (Var.String_Type) is
         when N_String_Type_Declaration =>
            return (Current => First_Literal_String (Var.String_Type));

         when N_Attribute_Declaration
           |  N_Typed_Variable_Declaration
           |  N_Variable_Declaration =>

            V := Expression_Of (Var.String_Type);

            case Kind_Of (V) is
               when N_Expression =>
                  Expr := First_Term (V);
                  pragma Assert (Kind_Of (Expr) = N_Term);
                  Expr := Current_Term (Expr);

                  case Kind_Of (Expr) is
                     when N_Literal_String_List =>
                        return (Current => First_Expression_In_List (Expr));

                     when N_External_Value =>
                        return (Current => External_Default_Of (Expr));

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
     (Var_Or_Type : Project_Node_Id) return String_List_Iterator
   is
      Typ : Project_Node_Id := Var_Or_Type;
   begin
      if Kind_Of (Var_Or_Type) = N_Typed_Variable_Declaration then
         Typ := String_Type_Of (Var_Or_Type);
      end if;

      return (Current => First_Literal_String (Typ));
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

   function Data (Iter : String_List_Iterator) return Types.String_Id is
   begin
      pragma Assert (Kind_Of (Iter.Current) = N_Literal_String);
      return String_Value_Of (Iter.Current);
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

   function Next (Iter : String_List_Iterator) return String_List_Iterator is
   begin
      pragma Assert (Iter.Current /= Empty_Node);

      case Kind_Of (Iter.Current) is
         when N_Literal_String =>
            return (Current => Next_Literal_String (Iter.Current));

         when N_Expression =>
            return (Current => Next_Expression_In_List (Iter.Current));

         when others =>
            raise Program_Error;
      end case;
   end Next;

   -------------------------
   -- Get_Attribute_Value --
   -------------------------

   function Get_Attribute_Value
     (Project        : Project_Type;
      Attribute_Name : String;
      Package_Name   : String := "";
      Index          : String := "") return Variable_Value
   is
      Pkg : Package_Id := No_Package;
      Value : Variable_Value := Nil_Variable_Value;
      Var : Variable_Id;
      Arr : Array_Id;
      Elem : Array_Element_Id;
      N    : Name_Id;
      Project_View : constant Project_Id := Get_View (Project);
   begin
      if Project_View = Prj.No_Project then
         return Nil_Variable_Value;
      end if;

      if Package_Name /= "" then
         Pkg := Value_Of
           (Get_String (Package_Name),
            In_Packages => Prj.Projects.Table (Project_View).Decl.Packages);
         if Pkg = No_Package then
            return Nil_Variable_Value;
         end if;
         Var := Packages.Table (Pkg).Decl.Attributes;
         Arr := Packages.Table (Pkg).Decl.Arrays;
      else
         Var := Prj.Projects.Table (Project_View).Decl.Attributes;
         Arr := Prj.Projects.Table (Project_View).Decl.Arrays;
      end if;

      N := Get_String (Attribute_Name);

      if Index /= "" then
         Elem := Value_Of (N, In_Arrays => Arr);
         if Elem /= No_Array_Element then
            Value := Value_Of (Index => Get_String (Index), In_Array => Elem);
         end if;
      else
         Value := Value_Of (N, Var);
      end if;

      return Value;
   end Get_Attribute_Value;

   ----------------------------------------
   -- Update_Attribute_Value_In_Scenario --
   ----------------------------------------

   procedure Update_Attribute_Value_In_Scenario
     (Project            : Project_Type;
      Pkg_Name           : String := "";
      Scenario_Variables : Scenario_Variable_Array;
      Attribute_Name     : String := "";
      Values             : GNAT.OS_Lib.Argument_List;
      Attribute_Index    : String := "";
      Prepend            : Boolean := False)
   is
      Pkg_Prj : constant Project_Type :=
        Find_Project_Of_Package (Project, Pkg_Name);
   begin
      Projects.Editor.Normalize.Normalize (Pkg_Prj);
      Update_Attribute_Value_In_Scenario
        (Pkg_Prj.Node, Pkg_Name, Scenario_Variables, Attribute_Name,
         Values, Attribute_Index, Prepend);
      Set_Project_Modified (Pkg_Prj, True);
   end Update_Attribute_Value_In_Scenario;

   ----------------------------------------
   -- Update_Attribute_Value_In_Scenario --
   ----------------------------------------

   procedure Update_Attribute_Value_In_Scenario
     (Project            : Project_Node_Id;
      Pkg_Name           : String := "";
      Scenario_Variables : Scenario_Variable_Array;
      Attribute_Name     : String := "";
      Values             : GNAT.OS_Lib.Argument_List;
      Attribute_Index    : String := "";
      Prepend            : Boolean := False)
   is
      Attribute_N     : Name_Id;
      List            : Project_Node_Id := Empty_Node;
      Pkg, Term, Expr : Project_Node_Id;
      Rename_Prj      : Project_Node_Id := Project;

      procedure Add_Or_Replace (Case_Item : Project_Node_Id);
      --  Add or replace the attribute Attribute_Name in the declarative list
      --  for Case_Item

      --------------------
      -- Add_Or_Replace --
      --------------------

      procedure Add_Or_Replace (Case_Item : Project_Node_Id) is
         Previous_Decl, Decl, Expr : Project_Node_Id;
      begin
         Previous_Decl := Find_Last_Declaration_Of
           (Case_Item, Attribute_N, Attribute_Index);

         --  Do we already have some declarations for this attribute ?
         --  If we found a previous declaration, update it

         if Previous_Decl /= Empty_Node then
            Previous_Decl := Expression_Of (Previous_Decl);
            if Prepend then
               Expr := First_Expression_In_List (List);
               while Next_Expression_In_List (Expr) /= Empty_Node loop
                  Expr := Next_Expression_In_List (Expr);
               end loop;

               Set_Next_Expression_In_List
                 (Expr, First_Expression_In_List
                  (Current_Term (First_Term (Previous_Decl))));
            else
               Set_Next_Expression_In_List (Previous_Decl, Empty_Node);
               Set_Next_Term (First_Term (Previous_Decl), Empty_Node);
            end if;

            Set_Current_Term (First_Term (Previous_Decl), List);

         --  Else create the new instruction to be added to the project

         else
            Decl := Create_Attribute
              (Case_Item, Attribute_Name, Attribute_Index);
            Expr := Enclose_In_Expression (List);

            if Prepend then
               Set_Next_Term
                 (First_Term (Expr),
                  Default_Project_Node (N_Term, Prj.List));
               Term := Next_Term (First_Term (Expr));
               Set_Current_Term
                 (Term,
                  Default_Project_Node (N_Attribute_Reference, Prj.List));
               Term := Current_Term (Term);

               Set_Name_Of (Term, Attribute_N);
               Set_Project_Node_Of (Term, Rename_Prj);
            end if;

            Set_Expression_Of (Decl, Expr);
         end if;
      end Add_Or_Replace;

   begin
      Attribute_N := Get_String (Attribute_Name);

      if Pkg_Name /= "" then
         Pkg := Get_Or_Create_Package (Project, Pkg_Name);

         --  If we have a renamed package, modify the target package.
         Rename_Prj := Project_Of_Renamed_Package_Of (Pkg);
         if Rename_Prj /= Empty_Node then
            Pkg := Get_Or_Create_Package (Rename_Prj, Pkg_Name);
         else
            Rename_Prj := Project;
         end if;
      else
         Pkg := Empty_Node;
      end if;

      Move_From_Common_To_Case_Construct
        (Rename_Prj, Pkg_Name, Scenario_Variables,
         Attribute_N, Attribute_Index);

      --  Create the string list for the new values.
      --  This can be prepended later on to the existing list of values.

      List := Default_Project_Node (N_Literal_String_List, Prj.List);

      for A in reverse Values'Range loop
         Start_String;
         Store_String_Chars (Values (A).all);
         Expr := String_As_Expression (End_String);
         Set_Next_Expression_In_List
           (Expr, First_Expression_In_List (List));
         Set_First_Expression_In_List (List, Expr);
      end loop;

      For_Each_Scenario_Case_Item
        (Rename_Prj, Pkg, Scenario_Variables,
         Add_Or_Replace'Unrestricted_Access);
   end Update_Attribute_Value_In_Scenario;

   ----------------------------------------
   -- Update_Attribute_Value_In_Scenario --
   ----------------------------------------

   procedure Update_Attribute_Value_In_Scenario
     (Project            : Project_Type;
      Pkg_Name           : String := "";
      Scenario_Variables : Scenario_Variable_Array;
      Attribute_Name     : String := "";
      Value              : String;
      Attribute_Index    : String := "")
   is
      Pkg_Prj : constant Project_Type :=
        Find_Project_Of_Package (Project, Pkg_Name);
   begin
      Projects.Editor.Normalize.Normalize (Pkg_Prj);
      Update_Attribute_Value_In_Scenario
        (Pkg_Prj.Node, Pkg_Name, Scenario_Variables, Attribute_Name,
         Value, Attribute_Index);
      Set_Project_Modified (Pkg_Prj, True);
   end Update_Attribute_Value_In_Scenario;

   ----------------------------------------
   -- Update_Attribute_Value_In_Scenario --
   ----------------------------------------

   procedure Update_Attribute_Value_In_Scenario
     (Project            : Project_Node_Id;
      Pkg_Name           : String := "";
      Scenario_Variables : Scenario_Variable_Array;
      Attribute_Name     : String := "";
      Value              : String;
      Attribute_Index    : String := "")
   is
      Attribute_N : Name_Id;
      Val         : Project_Node_Id := Empty_Node;
      Pkg         : Project_Node_Id;
      Rename_Prj  : Project_Node_Id := Project;

      procedure Add_Or_Replace (Case_Item : Project_Node_Id);
      --  Add or replace the attribute Attribute_Name in the declarative list
      --  for Case_Item

      --------------------
      -- Add_Or_Replace --
      --------------------

      procedure Add_Or_Replace (Case_Item : Project_Node_Id) is
         Previous_Decl, Decl : Project_Node_Id;
      begin
         Previous_Decl := Find_Last_Declaration_Of
           (Case_Item, Attribute_N, Attribute_Index);

         --  If we found a previous declaration, update it

         if Previous_Decl /= Empty_Node then
            Previous_Decl := Expression_Of (Previous_Decl);
            Set_Current_Term (First_Term (Previous_Decl), Val);

         --  Else create the new instruction to be added to the project

         else
            Decl := Create_Attribute
              (Case_Item, Attribute_Name, Attribute_Index, Prj.Single);
            Set_Expression_Of (Decl, Enclose_In_Expression (Val));
         end if;
      end Add_Or_Replace;

   begin
      Attribute_N := Get_String (Attribute_Name);

      if Pkg_Name /= "" then
         Pkg := Get_Or_Create_Package (Project, Pkg_Name);

         --  If we have a renamed package, modify the target package.
         Rename_Prj := Project_Of_Renamed_Package_Of (Pkg);
         if Rename_Prj /= Empty_Node then
            Pkg := Get_Or_Create_Package (Rename_Prj, Pkg_Name);
         else
            Rename_Prj := Project;
         end if;
      else
         Pkg := Empty_Node;
      end if;

      Move_From_Common_To_Case_Construct
        (Rename_Prj, Pkg_Name, Scenario_Variables,
         Attribute_N, Attribute_Index);

      --  Create the node for the new value

      Start_String;
      Store_String_Chars (Value);
      Val := Create_Literal_String (End_String);

      For_Each_Scenario_Case_Item
        (Rename_Prj, Pkg, Scenario_Variables,
         Add_Or_Replace'Unrestricted_Access);
   end Update_Attribute_Value_In_Scenario;

   ------------------------------
   -- Find_Last_Declaration_Of --
   ------------------------------

   function Find_Last_Declaration_Of
     (Parent     : Project_Node_Id;
      Attr_Name  : Name_Id;
      Attr_Index : String := "") return Project_Node_Id
   is
      Decl, Expr : Project_Node_Id;
      Result     : Project_Node_Id := Empty_Node;
   begin
      Decl := First_Declarative_Item_Of (Parent);

      while Decl /= Empty_Node loop
         Expr := Current_Item_Node (Decl);

         if Attribute_Matches (Expr, Attr_Name, Attr_Index) then
            Result := Expr;
         end if;

         Decl := Next_Declarative_Item (Decl);
      end loop;
      return Result;
   end Find_Last_Declaration_Of;

   ----------------------------------------
   -- Move_From_Common_To_Case_Construct --
   ----------------------------------------

   procedure Move_From_Common_To_Case_Construct
     (Project            : Project_Node_Id;
      Pkg_Name           : String;
      Scenario_Variables : Scenario_Variable_Array;
      Attribute_Name     : Types.Name_Id;
      Attribute_Index    : String := "")
   is
      Parent          : Project_Node_Id;
      Pkg             : Project_Node_Id := Empty_Node;
      Node, Tmp       : Project_Node_Id;
      Case_Items      : Project_Node_Array_Access :=
        new Project_Node_Array (1 .. 100);
      Case_Items_Last : Natural := Case_Items'First - 1;

      procedure Add_Item (Case_Item : Project_Node_Id);
      --  Add the declaration Node to Case_Item, in front, since the
      --  declaration necessarily occured before the case item

      procedure Add_Item (Case_Item : Project_Node_Id) is
      begin
         Add_Node_To_List (Case_Items, Case_Items_Last, Case_Item);
      end Add_Item;

   begin
      if Pkg_Name /= "" then
         Parent := Get_Or_Create_Package (Project, Pkg_Name);
         Pkg := Parent;
      else
         Parent := Project_Declaration_Of (Project);
      end if;

      --  First, create the nested case for the scenario, and memorize each of
      --  them. This will easily allow to keep the order of all the
      --  declarations for this attribute that are currently in the common part
      For_Each_Scenario_Case_Item (Project, Pkg, Scenario_Variables, null);
      For_Each_Matching_Case_Item
        (Project, Pkg, All_Case_Items, Add_Item'Unrestricted_Access);

      --  Nothing to do if there are no case items
      if Case_Items_Last > Case_Items'First then
         Node := First_Declarative_Item_Of (Parent);
         while Node /= Empty_Node loop
            if Attribute_Matches
              (Current_Item_Node (Node), Attribute_Name, Attribute_Index)
            then
               for Parent in Case_Items'First .. Case_Items_Last loop
                  Tmp := Default_Project_Node (N_Declarative_Item);
                  Set_Current_Item_Node
                    (Tmp, Clone_Node (Current_Item_Node (Node), True));

                  if Kind_Of (Case_Items (Parent)) /= N_Declarative_Item then
                     Set_Next_Declarative_Item
                       (Tmp, First_Declarative_Item_Of (Case_Items (Parent)));
                     Set_First_Declarative_Item_Of (Case_Items (Parent), Tmp);

                  else
                     Set_Next_Declarative_Item
                       (Tmp, Next_Declarative_Item (Case_Items (Parent)));
                     Set_Next_Declarative_Item (Case_Items (Parent), Tmp);
                  end if;
                  Case_Items (Parent) := Tmp;
               end loop;
            end if;

            Node := Next_Declarative_Item (Node);
         end loop;

         Free (Case_Items);

         Remove_Attribute_Declarations
           (Parent, Attribute_Name, Attribute_Index);
      end if;
   end Move_From_Common_To_Case_Construct;

   -----------------------------------
   -- Remove_Attribute_Declarations --
   -----------------------------------

   procedure Remove_Attribute_Declarations
     (Parent          : Project_Node_Id;
      Attribute_Name  : Name_Id;
      Attribute_Index : String)
   is
      Decl : Project_Node_Id := First_Declarative_Item_Of (Parent);
      Previous : Project_Node_Id := Empty_Node;
   begin
      while Decl /= Empty_Node loop
         if Attribute_Matches
           (Current_Item_Node (Decl), Attribute_Name, Attribute_Index)
         then
            if Previous = Empty_Node then
               Set_First_Declarative_Item_Of
                 (Parent, Next_Declarative_Item (Decl));
            else
               Set_Next_Declarative_Item
                 (Previous, Next_Declarative_Item (Decl));
            end if;
         else
            Previous := Decl;
         end if;

         Decl := Next_Declarative_Item (Decl);
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
     (Node            : Project_Node_Id;
      Attribute_Name  : Name_Id;
      Attribute_Index : String) return Boolean is
   begin
      return Kind_Of (Node) = N_Attribute_Declaration
        and then Prj.Tree.Name_Of (Node) = Attribute_Name
        and then
        (Attribute_Index = Any_Attribute
         or else (Attribute_Index = ""
          and then Associative_Array_Index_Of (Node) = No_String)
         or else (Attribute_Index /= ""
                  and then Associative_Array_Index_Of (Node) /= No_String
                  and then Is_Equal (Associative_Array_Index_Of (Node),
                                     Attribute_Index)));
   end Attribute_Matches;

   ----------------------
   -- Delete_Attribute --
   ----------------------

   procedure Delete_Attribute
     (Project            : Project_Type;
      Pkg_Name           : String := "";
      Scenario_Variables : Scenario_Variable_Array;
      Attribute_Name     : String;
      Attribute_Index    : String := "")
   is
      Attribute_N : Name_Id;
      Pkg : Project_Node_Id;
      Pkg_Prj : constant Project_Type :=
        Find_Project_Of_Package (Project, Pkg_Name);

      procedure Delete_Attr (Case_Item : Project_Node_Id);
      --  Remove all definitions for the attribute in the case item

      -----------------
      -- Delete_Attr --
      -----------------

      procedure Delete_Attr (Case_Item : Project_Node_Id) is
      begin
         Remove_Attribute_Declarations
           (Case_Item, Attribute_N, Attribute_Index);
      end Delete_Attr;

   begin
      Projects.Editor.Normalize.Normalize (Pkg_Prj);
      Attribute_N := Get_String (Attribute_Name);

      if Pkg_Name /= "" then
         Pkg := Find_Package_Declaration (Pkg_Prj.Node, Get_String (Pkg_Name));

         --  If the package doesn't exist, no need to do anything
         if Pkg = Empty_Node then
            return;
         end if;
      else
         Pkg := Empty_Node;
      end if;

      Move_From_Common_To_Case_Construct
        (Pkg_Prj.Node, Pkg_Name, Scenario_Variables, Attribute_N,
         Attribute_Index);

      For_Each_Scenario_Case_Item
        (Pkg_Prj.Node, Pkg, Scenario_Variables,
         Delete_Attr'Unrestricted_Access);

      Set_Project_Modified (Pkg_Prj, True);
   end Delete_Attribute;

   ---------------------------
   -- Create_Typed_Variable --
   ---------------------------

   function Create_Typed_Variable
     (Prj_Or_Pkg                   : Project_Node_Id;
      Name                         : String;
      Typ                          : Project_Node_Id;
      Add_Before_First_Case_Or_Pkg : Boolean := False) return Project_Node_Id
   is
      Node : constant Project_Node_Id :=
        Default_Project_Node (N_Typed_Variable_Declaration, Prj.Single);
   begin
      Set_Name_Of (Node, Get_String (Name));
      Set_String_Type_Of (Node, Typ);

      Add_At_End (Prj_Or_Pkg, Node, Add_Before_First_Case_Or_Pkg);

      Set_Next_Variable (Node, First_Variable_Of (Prj_Or_Pkg));
      Set_First_Variable_Of (Prj_Or_Pkg, Node);

      return Node;
   end Create_Typed_Variable;

   ------------------------
   -- Add_Possible_Value --
   ------------------------

   procedure Add_Possible_Value (Typ : Project_Node_Id; Choice : String_Id) is
      Str, S2 : Project_Node_Id;
   begin
      pragma Assert (Kind_Of (Typ) = N_String_Type_Declaration);

      Str := First_Literal_String (Typ);

      while Str /= Empty_Node loop
         if String_Equal (String_Value_Of (Str), Choice) then
            return;
         end if;

         Str := Next_Literal_String (Str);
      end loop;

      S2 := Create_Literal_String (Choice);
      Set_Next_Literal_String (S2, First_Literal_String (Typ));
      Set_First_Literal_String (Typ, S2);
   end Add_Possible_Value;

   ---------------------------
   -- Find_Type_Declaration --
   ---------------------------

   function Find_Type_Declaration
     (Project : Project_Node_Id; Name : Types.Name_Id)
      return Project_Node_Id is
   begin
      return Find_Node_By_Name (Project, N_String_Type_Declaration, Name);
   end Find_Type_Declaration;

   -----------------
   -- Create_Type --
   -----------------

   function Create_Type
     (Prj_Or_Pkg : Project_Node_Id;
      Name       : String) return Project_Node_Id
   is
      Node : Project_Node_Id;
   begin
      Node := Default_Project_Node (N_String_Type_Declaration);
      Set_Name_Of (Node, Get_String (Name));
      Add_At_End (Prj_Or_Pkg, Node, True);

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
      Delete_Variable : exception;
      Type_Decl       : Project_Node_Id := Empty_Node;

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each matching node for the env. variable.

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Project, Choice);
         C, C2 : Project_Node_Id;
      begin
         case Kind_Of (Node) is
            when N_String_Type_Declaration =>
               Type_Decl := Node;

               C := First_Literal_String (Node);

               if Next_Literal_String (C) = Empty_Node then
                  raise Delete_Variable;
               end if;

               if Is_Equal (String_Value_Of (C), Value_Name) then
                  Set_First_Literal_String (Node, Next_Literal_String (C));
                  return;
               end if;

               loop
                  C2 := Next_Literal_String (C);
                  exit when C2 = Empty_Node;

                  if Is_Equal (String_Value_Of (C2), Value_Name) then
                     Set_Next_Literal_String (C, Next_Literal_String (C2));
                     exit;
                  end if;
                  C := C2;
               end loop;

            when N_External_Value =>
               if External_Default_Of (Node) /= Empty_Node
                 and then Is_Equal
                 (String_Value_Of (External_Default_Of (Node)),
                  Value_Name)
               then
                  Set_External_Default_Of (Node, Empty_Node);
               end if;

            when N_Case_Item =>
               C := First_Case_Item_Of (Current_Item_Node (Parent));
               if C = Node then
                  Set_First_Case_Item_Of
                    (Current_Item_Node (Parent), Next_Case_Item (C));
                  return;
               end if;

               loop
                  C2 := Next_Case_Item (C);
                  exit when C2 = Empty_Node;

                  if C2 = Node then
                     Set_Next_Case_Item (C, Next_Case_Item (C2));
                  end if;

                  C := C2;
               end loop;

            when others =>
               null;
         end case;
      end Callback;

   begin
      Projects.Editor.Normalize.Normalize (Root_Project);
      For_Each_Environment_Variable
        (Root_Project, Ext_Variable_Name, Value_Name,
         Callback'Unrestricted_Access);

      --  Reset the value of the external variable if needed

      if Is_Equal (Value_Of (Get_String (Ext_Variable_Name)), Value_Name) then
         if Type_Decl /= Empty_Node then
            Add (Ext_Variable_Name,
                 Get_String (String_Value_Of
                             (First_Literal_String (Type_Decl))));
         else
            Add (Ext_Variable_Name, "");
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

   ----------------------------------------
   -- Rename_Value_For_External_Variable --
   ----------------------------------------

   procedure Rename_Value_For_External_Variable
     (Root_Project      : Project_Type;
      Ext_Variable_Name : String;
      Old_Value_Name    : String;
      New_Value_Name    : Types.String_Id)
   is
      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable.

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Project, Parent);
         C : Project_Node_Id;
      begin
         case Kind_Of (Node) is
            when N_External_Value =>
               if External_Default_Of (Node) /= Empty_Node
                 and then Is_Equal
                 (String_Value_Of (External_Default_Of (Node)), Old_Value_Name)
               then
                  Set_String_Value_Of
                    (External_Default_Of (Node), New_Value_Name);
               end if;

            when N_String_Type_Declaration =>
               C := First_Literal_String (Node);
               while C /= Empty_Node loop
                  if Is_Equal (String_Value_Of (C), Old_Value_Name) then
                     Set_String_Value_Of (C, New_Value_Name);
                     exit;
                  end if;
                  C := Next_Literal_String (C);
               end loop;

            when N_Case_Item =>
               Set_String_Value_Of (Choice, New_Value_Name);

            when others =>
               null;
         end case;
      end Callback;

      N : Name_Id;
   begin
      Projects.Editor.Normalize.Normalize (Root_Project);
      For_Each_Environment_Variable
        (Root_Project, Ext_Variable_Name, Old_Value_Name,
         Callback'Unrestricted_Access);

      N := Get_String (Ext_Variable_Name);

      if Value_Of (N) /= No_String
        and then Is_Equal (Value_Of (N), Old_Value_Name)
      then
         Add (Ext_Variable_Name, Get_String (New_Value_Name));
      end if;

      Set_Project_Modified (Root_Project, True);
   end Rename_Value_For_External_Variable;

   -----------------------------------
   -- For_Each_Environment_Variable --
   -----------------------------------

   procedure For_Each_Environment_Variable
     (Root_Project      : Project_Type;
      Ext_Variable_Name : String;
      Specific_Choice   : String;
      Action            : Environment_Variable_Callback)
   is
      Variable_Nodes : Project_Node_Array_Access :=
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
         case Kind_Of (Node) is
            when N_External_Value =>
               String_To_Name_Buffer
                 (String_Value_Of (Prj.Tree.External_Reference_Of (Node)));
               return Name_Buffer (1 .. Name_Len) = Ext_Variable_Name;

            when N_Variable_Reference =>
               for J in Variable_Nodes'First .. Variable_Nodes_Last loop
                  if Prj.Tree.Name_Of (Node) =
                    Prj.Tree.Name_Of (Variable_Nodes (J))
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
            Term := First_Term (Expr);
            while Term /= Empty_Node loop
               case Kind_Of (Current_Term (Term)) is

                  --  Handles ("-g" & A, "-O2" & external ("A"))
                  when N_Literal_String_List =>
                     Process_Expression
                       (Project,
                        First_Expression_In_List (Current_Term (Term)));

                  --  Handles "-g" & external ("A")
                  --  Replace A by the constant string representing its value
                  when N_External_Value =>
                     if Is_Reference_To_Ext (Current_Term (Term)) then
                        Action (Project, Term, Current_Term (Term),
                                Empty_Node);
                     end if;

                  --  Handles "-g" & Var
                  --  Where Var is a reference to the external variable
                  when N_Variable_Reference =>
                     if Is_Reference_To_Ext (Current_Term (Term)) then
                        Action (Project, Term, Current_Term (Term),
                                Empty_Node);
                     end if;

                  when others =>
                     null;
               end case;

               Term := Next_Term (Term);
            end loop;

            Expr := Next_Expression_In_List (Expr);
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
            Decl := First_Declarative_Item_Of (Pkg_Or_Case_Item);
         else
            Decl := First_Declarative_Item_Of
              (Project_Declaration_Of (Project));
         end if;

         while Decl /= Empty_Node loop
            Current := Current_Item_Node (Decl);
            case Kind_Of (Current) is
               when N_Typed_Variable_Declaration =>

                  if Is_External_Variable (Current)
                    and then Is_Equal
                       (External_Reference_Of (Current), Ext_Variable_Name)
                  then
                     Add_Node_To_List
                       (Variable_Nodes, Variable_Nodes_Last, Current);

                     Action
                       (Project, Empty_Node, String_Type_Of (Current),
                        Empty_Node);
                     Action (Project, Decl, Current, Empty_Node);
                  end if;

                  Process_Expression (Project, Expression_Of (Current));

               when N_Case_Construction =>
                  if Is_Reference_To_Ext
                    (Case_Variable_Reference_Of (Current))
                  then
                     Case_Item := First_Case_Item_Of (Current);

                     while Case_Item /= Empty_Node loop
                        Choice := First_Choice_Of (Case_Item);

                        --  If we have reached Empty_Node and nothing matched
                        --  before, then that is the case item we want to keep.
                        --  This corresponds to "when others"
                        Match := Choice = Empty_Node
                          or else Specific_Choice = "";

                        if not Match then
                           while Choice /= Empty_Node loop
                              if Is_Equal
                                (String_Value_Of (Choice), Specific_Choice)
                              then
                                 Match := True;
                                 exit;
                              end if;

                              Choice := Next_Literal_String (Choice);
                           end loop;
                        end if;

                        if Match then
                           Action (Project, Decl, Case_Item, Choice);
                        end if;

                        Recurse_In_Project (Project, Case_Item);
                        Case_Item := Next_Case_Item (Case_Item);
                     end loop;

                  else
                     Case_Item := First_Case_Item_Of (Current);
                     while Case_Item /= Empty_Node loop
                        Recurse_In_Project (Project, Case_Item);
                        Case_Item := Next_Case_Item (Case_Item);
                     end loop;
                  end if;

               when N_Package_Declaration =>
                  Recurse_In_Project (Project, Current);

               when N_Variable_Declaration
                 |  N_Attribute_Declaration =>
                  Process_Expression (Project, Expression_Of (Current));

               when others =>
                  null;
            end case;

            Decl := Next_Declarative_Item (Decl);
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
     (Root_Project      : Project_Type;
      Ext_Variable_Name : String;
      Keep_Choice       : String;
      Delete_Direct_References : Boolean := True)
   is
      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable.

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Choice);
      begin
         case Kind_Of (Node) is
            when N_External_Value =>
               if Delete_Direct_References then
                  Start_String;
                  Store_String_Chars (Keep_Choice);
                  Set_Current_Term
                    (Parent, Create_Literal_String (End_String));
               end if;

            when N_Variable_Reference =>
               Start_String;
               Store_String_Chars (Keep_Choice);
               Set_Current_Term (Parent, Create_Literal_String (End_String));

            when N_Typed_Variable_Declaration =>
               Remove_Node (Project, Node);
               Remove_Variable_Declaration (Project, Node);

            when N_String_Type_Declaration =>
               Remove_Node (Project, Node);

            when N_Case_Item =>
               --  The first declarative item might be null when there was no
               --  actual "when ..." for Keep_Choice. In that case, Prj.Proc
               --  inserts an entry with no declarative item.

               if First_Declarative_Item_Of (Node) /= Empty_Node then
                  Tree_Private_Part.Project_Nodes.Table (Parent) :=
                    Tree_Private_Part.Project_Nodes.Table
                    (First_Declarative_Item_Of (Node));

               else
                  Set_Current_Item_Node (Parent, Empty_Node);
               end if;

            when others =>
               null;
               pragma Assert (False, "Unexpected node type");

         end case;
      end Callback;

   begin
      Projects.Editor.Normalize.Normalize (Root_Project);
      For_Each_Environment_Variable
        (Root_Project, Ext_Variable_Name, Keep_Choice,
         Callback'Unrestricted_Access);

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
     (Project_Or_Package : Project_Node_Id;
      Declaration        : Project_Node_Id)
   is
      Tmp, Next : Project_Node_Id;
      Pkg : Project_Node_Id := Project_Or_Package;
   begin
      while Pkg /= Empty_Node loop
         Tmp := First_Variable_Of (Pkg);

         if Tmp = Declaration then
            Set_First_Variable_Of (Pkg, Next_Variable (Tmp));
            return;
         else
            loop
               Next := Next_Variable (Tmp);
               exit when Next = Empty_Node;

               if Next = Declaration then
                  Set_Next_Variable (Tmp, Next_Variable (Next));
                  return;
               end if;
            end loop;
         end if;

         if Kind_Of (Pkg) = N_Project then
            Pkg := First_Package_Of (Pkg);
         else
            Pkg := Next_Package_In_Project (Pkg);
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
      Default           : String_Id)
   is
      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable.

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Project, Parent, Choice);
      begin
         if Kind_Of (Node) = N_Typed_Variable_Declaration then
            Set_External_Default_Of
              (Current_Term (First_Term (Expression_Of (Node))),
               Create_Literal_String (Default));
         end if;
      end Callback;

   begin
      Projects.Editor.Normalize.Normalize (Root_Project);
      For_Each_Environment_Variable
        (Root_Project, Ext_Variable_Name, "",
         Callback'Unrestricted_Access);
      Set_Project_Modified (Root_Project, True);
   end Set_Default_Value_For_External_Variable;

   ------------------------------
   -- Rename_External_Variable --
   ------------------------------

   procedure Rename_External_Variable
     (Root_Project : Project_Type;
      Variable     : in out Scenario_Variable;
      New_Name     : Types.String_Id)
   is
      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable.

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Project, Parent, Choice);
      begin
         if Kind_Of (Node) = N_External_Value then
            Set_String_Value_Of (External_Reference_Of (Node), New_Name);
         end if;
      end Callback;

      N : Name_Id;
   begin
      Projects.Editor.Normalize.Normalize (Root_Project);
      For_Each_Environment_Variable
        (Root_Project, External_Reference_Of (Variable), "",
         Callback'Unrestricted_Access);
      Set_Project_Modified (Root_Project, True);

      --  Create the new variable, to avoid errors when computing the view of
      --  the project.
      N := Get_String (External_Reference_Of (Variable));

      String_To_Name_Buffer (New_Name);
      Variable.Name := Name_Find;

      if Value_Of (N) /= No_String then
         Set_Value (Variable, Get_String (Value_Of (N)));
      end if;
   end Rename_External_Variable;

   ----------------------------------
   -- Add_Scenario_Variable_Values --
   ----------------------------------

   procedure Add_Scenario_Variable_Values
     (Root_Project           : Project_Type;
      External_Var           : Scenario_Variable;
      Values                 : String_Id_Array)
   is
      Type_Node, Var : Project_Node_Id;
      Iter : Imported_Project_Iterator := Start (Root_Project);
      P    : Project_Type;
   begin
      loop
         P := Current (Iter);
         exit when P = No_Project;

         Projects.Editor.Normalize.Normalize (P);
         Var := Find_Scenario_Variable (P, External_Var.Name);

         --  If variable is defined in the current project, then modify the
         --  type to Values.

         if Var /= Empty_Node then
            Type_Node := String_Type_Of (Var);
            pragma Assert (Type_Node /= Empty_Node);
            --  Set_First_Literal_String (Type_Node, Empty_Node);

            for J in Values'Range loop
               Add_Possible_Value (Type_Node, Values (J));
            end loop;

            Set_Project_Modified (P, True);
         end if;

         Next (Iter);
      end loop;
   end Add_Scenario_Variable_Values;

   ----------------------
   -- To_Argument_List --
   ----------------------

   function To_Argument_List (Value : Variable_Value) return Argument_List is
      S : Argument_List (1 .. Length (Value)) := (others => null);
      V : String_List_Id;

   begin
      case Value.Kind is
         when Undefined =>
            null;

         when Single =>
            String_To_Name_Buffer (Value.Value);
            S (1) := new String'(Name_Buffer (1 .. Name_Len));

         when List =>
            V := Value.Values;

            for J in S'Range loop
               String_To_Name_Buffer (String_Elements.Table (V).Value);
               S (J) := new String'(Name_Buffer (1 .. Name_Len));
               V := String_Elements.Table (V).Next;
            end loop;
      end case;
      return S;
   end To_Argument_List;


   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Variable_Value) return String is
      Buffer     : String (1 .. 1024);
      Current    : Prj.String_List_Id;
      The_String : String_Element;
      Index      : Natural := Buffer'First;

   begin
      case Value.Kind is
         when Prj.Undefined =>
            return "";

         when Prj.Single =>
            String_To_Name_Buffer (Value.Value);
            return Name_Buffer (1 .. Name_Len);

         when Prj.List =>
            Current := Value.Values;

            while Current /= Prj.Nil_String loop
               The_String := String_Elements.Table (Current);
               String_To_Name_Buffer (The_String.Value);

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
     (Var_Or_Attribute : Project_Node_Id) return String_Id
   is
      Ext : Project_Node_Id;
   begin
      --  ??? We do not correctly detect constructions like
      --         A : A_Type := "A" & external ("OS");
      --  The external reference must be by itself in the reference

      pragma Assert (Var_Or_Attribute /= Empty_Node);

      Ext := Expression_Of (Var_Or_Attribute);
      pragma Assert (Kind_Of (Ext) = N_Expression);
      Ext := First_Term (Ext);
      pragma Assert (Kind_Of (Ext) = N_Term);
      Ext := Current_Term (Ext);

      if Kind_Of (Ext) = N_External_Value then
         Ext := External_Reference_Of (Ext);
         if Kind_Of (Ext) = N_Expression then
            Ext := Current_Term (First_Term (Ext));
         end if;

         pragma Assert (Kind_Of (Ext) = N_Literal_String);
         return String_Value_Of (Ext);
      else
         return No_String;
      end if;
   end Get_Environment;

   ---------------------------
   -- Set_Value_As_External --
   ---------------------------

   procedure Set_Value_As_External
     (Var : Project_Node_Id; External_Name : String; Default : String := "")
   is
      Ext : Project_Node_Id;
      Str : Project_Node_Id;
   begin
      pragma Assert (Expression_Kind_Of (Var) = Prj.Single);

      --  Create the expression if required

      Ext := Default_Project_Node (N_External_Value, Single);
      Set_Expression (Var, Enclose_In_Expression (Ext));

      Start_String;
      Store_String_Chars (External_Name);
      Str := Create_Literal_String (End_String);

      Set_External_Reference_Of (Ext, Str);

      if Default /= "" then
         Start_String;
         Store_String_Chars (Default);
         Str := Create_Literal_String (End_String);
         Set_External_Default_Of (Ext, Str);
      end if;
   end Set_Value_As_External;

   -------------------------------
   -- Create_Variable_Reference --
   -------------------------------

   function Create_Variable_Reference (Var : Project_Node_Id)
      return Project_Node_Id
   is
      Ref : Project_Node_Id;
   begin
      Assert (Me,
              Kind_Of (Var) = N_Typed_Variable_Declaration
              or else Kind_Of (Var) = N_Variable_Declaration,
              "Create_Variable_Reference: unexpected node type "
              & Kind_Of (Var)'Img);

      Ref := Default_Project_Node (N_Variable_Reference);
      Set_Name_Of (Ref, Prj.Tree.Name_Of (Var));
      Set_Expression_Kind_Of (Ref, Expression_Kind_Of (Var));

      if Kind_Of (Var) = N_Typed_Variable_Declaration then
         Set_String_Type_Of (Ref, String_Type_Of (Var));
      end if;
      return Ref;
   end Create_Variable_Reference;

   -------------------
   -- Convert_Paths --
   -------------------

   function Convert_Paths
     (Project                : Project_Type;
      Use_Relative_Paths     : Boolean := False;
      Update_With_Statements : Boolean := False) return Boolean
   is
      procedure Convert_Path (Node : Project_Node_Id);
      --  Convert the path to an absolute path

      Base : constant String := Project_Directory (Project);
      Changed : Boolean := False;

      ------------------
      -- Convert_Path --
      ------------------

      procedure Convert_Path (Node : Project_Node_Id) is
         Old : constant String := Get_String (String_Value_Of (Node));
      begin
         if Use_Relative_Paths then
            declare
               Conv : constant String := Relative_Path_Name (Old, Base);
            begin
               if Conv /= Old then
                  Start_String;
                  Store_String_Chars (Conv);
                  Set_String_Value_Of (Node, End_String);
                  Changed := True;
               end if;
            end;
         else
            declare
               Conv : constant String := Normalize_Pathname (Old, Base);
            begin
               if Conv /= Old then
                  Start_String;
                  Store_String_Chars (Conv);
                  Set_String_Value_Of (Node, End_String);
                  Changed := True;
               end if;
            end;
         end if;
      end Convert_Path;

      With_Clause : Project_Node_Id := First_With_Clause_Of (Project.Node);
   begin
      --  First replace the with clauses
      if Update_With_Statements then
         while With_Clause /= Empty_Node loop
            Convert_Path (With_Clause);
            With_Clause := Next_With_Clause_Of (With_Clause);
         end loop;
      end if;

      --  Then replace all the paths
      For_Each_Directory_Node (Project, Convert_Path'Unrestricted_Access);

      if Changed then
         Set_Project_Modified (Project, True);
      end if;

      return Changed;
   end Convert_Paths;

   ------------------------------
   -- Post_Process_After_Clone --
   ------------------------------

   procedure Post_Process_After_Clone
     (Project : Project_Node_Id; Pkg : Project_Node_Id := Empty_Node)
   is
      Last_Var     : Project_Node_Id := Empty_Node;
      Last_Type    : Project_Node_Id := Empty_Node;
      Last_Package : Project_Node_Id := Empty_Node;
      Decl_Item    : Project_Node_Id;
      Current_Node : Project_Node_Id;

   begin
      if Pkg = Empty_Node then
         Decl_Item := First_Declarative_Item_Of
           (Project_Declaration_Of (Project));
      else
         pragma Assert (Kind_Of (Pkg) = N_Package_Declaration);
         Decl_Item := First_Declarative_Item_Of (Pkg);
      end if;

      while Decl_Item /= Empty_Node loop
         Current_Node := Current_Item_Node (Decl_Item);
         case Kind_Of (Current_Node) is
            when N_Package_Declaration =>
               if Last_Package /= Empty_Node then
                  Set_Next_Package_In_Project (Last_Package, Current_Node);
                  Last_Package := Current_Node;
               else
                  Last_Package := Current_Node;
                  Tree_Private_Part.Project_Nodes.Table (Project).Packages
                    := Last_Package;
               end if;

               Post_Process_After_Clone (Project, Last_Package);

            when N_Variable_Declaration | N_Typed_Variable_Declaration =>
               if Last_Var /= Empty_Node then
                  Set_Next_Variable (Last_Var, Current_Node);
                  Set_Next_Variable (Current_Node, Empty_Node);
                  Last_Var := Current_Node;
               else
                  Last_Var := Current_Node;
                  Set_Next_Variable (Last_Var, Empty_Node);

                  if Pkg /= Empty_Node then
                     Tree_Private_Part.Project_Nodes.Table (Pkg).Variables
                       := Last_Var;
                  else
                     Tree_Private_Part.Project_Nodes.Table (Project).Variables
                       := Last_Var;
                  end if;
               end if;

               --  Make sure that we do reference the type defined in the new
               --  project, not in some older project
               if Kind_Of (Current_Node) = N_Typed_Variable_Declaration then
                  Set_String_Type_Of
                    (Current_Node, Find_Type_Declaration
                     (Project,
                      Prj.Tree.Name_Of (String_Type_Of (Current_Node))));
               end if;

            when N_Variable_Reference =>
               if String_Type_Of (Current_Node) /= Empty_Node then
                  Set_String_Type_Of
                    (Current_Node, Find_Type_Declaration
                     (Project,
                      Prj.Tree.Name_Of (String_Type_Of (Current_Node))));
               end if;

               if Package_Node_Of (Current_Node) /= Empty_Node then
                  Set_Package_Node_Of
                    (Current_Node, Find_Package_Declaration
                     (Project,
                      Prj.Tree.Name_Of (Package_Node_Of (Current_Node))));
               end if;

            when N_Attribute_Reference =>
               if Package_Node_Of (Current_Node) /= Empty_Node then
                  Set_Package_Node_Of
                    (Current_Node, Find_Package_Declaration
                     (Project,
                      Prj.Tree.Name_Of (Package_Node_Of (Current_Node))));
               end if;

            when N_String_Type_Declaration =>
               if Last_Type /= Empty_Node then
                  Set_Next_String_Type (Last_Type, Current_Node);
                  Last_Type := Current_Node;
               else
                  Last_Type := Current_Node;
                  Set_First_String_Type_Of (Project, Last_Type);
               end if;

            when others =>
               null;

         end case;

         Decl_Item := Next_Declarative_Item (Decl_Item);
      end loop;
   end Post_Process_After_Clone;

   ----------------
   -- Clone_Node --
   ----------------

   function Clone_Node (Node : Project_Node_Id; Deep_Clone : Boolean := False)
      return Project_Node_Id
   is
      New_Node : Project_Node_Id;

   begin
      if Node = Empty_Node then
         return Empty_Node;
      end if;

      Tree_Private_Part.Project_Nodes.Increment_Last;
      New_Node := Tree_Private_Part.Project_Nodes.Last;

      --  Simple copy of all the fields. There is no need to duplicate
      --  String_Id at this point, since nobody will modify them later on
      --  anyway. So we save some memory and keep them as is.
      --  Only the node ids will need to be copied for deep copies.

      Tree_Private_Part.Project_Nodes.Table (New_Node) :=
        Tree_Private_Part.Project_Nodes.Table (Node);

      if Deep_Clone then
         case Kind_Of (Node) is
            when N_Project =>
               --  Packages, Variables, First_String_Type_Of must be outside of
               --  this subprogram
               Set_First_With_Clause_Of
                 (New_Node, Clone_Node (First_With_Clause_Of (Node), True));
               Set_Project_Declaration_Of
                 (New_Node, Clone_Node (Project_Declaration_Of (Node), True));
               Set_First_String_Type_Of (New_Node, Empty_Node);

            when N_With_Clause =>
               Set_Next_With_Clause_Of
                 (New_Node, Clone_Node (Next_With_Clause_Of (Node), True));

            when N_Project_Declaration =>
               Set_First_Declarative_Item_Of
                 (New_Node,
                  Clone_Node (First_Declarative_Item_Of (Node), True));

            when N_Declarative_Item =>
               Set_Current_Item_Node
                 (New_Node, Clone_Node (Current_Item_Node (Node), True));
               Set_Next_Declarative_Item
                 (New_Node, Clone_Node (Next_Declarative_Item (Node), True));

            when N_Package_Declaration =>
               --  Next_Package_In_Project and Variables must be set outside of
               --  this subprogram
               --  Pkg_Id doesn't need to be cloned, as per 9509-010.
               Set_First_Declarative_Item_Of
                 (New_Node,
                  Clone_Node (First_Declarative_Item_Of (Node), True));
               Set_Next_Package_In_Project (New_Node, Empty_Node);

            when N_String_Type_Declaration =>
               --  Next_String_Type must be set outside of this
               Set_First_Literal_String
                 (New_Node, Clone_Node (First_Literal_String (Node), True));
               Set_Next_String_Type (New_Node, Empty_Node);

            when N_Literal_String =>
               Set_Next_Literal_String
                 (New_Node, Clone_Node (Next_Literal_String (Node), True));

            when N_Attribute_Declaration =>
               Set_Expression_Of
                 (New_Node, Clone_Node (Expression_Of (Node), True));

            when N_Typed_Variable_Declaration =>
               --  Next_Variable must be set outside of this
               --  String_Type_Of is set to the same value as for Node, and
               --  this needs to be fixed in a post-processing phase.
               Set_Expression_Of
                 (New_Node, Clone_Node (Expression_Of (Node), True));
               Set_String_Type_Of (New_Node, String_Type_Of (Node));
               Set_Next_Variable (New_Node, Empty_Node);

            when N_Variable_Declaration =>
               --  Next_Variable must be set outside of this
               Set_Expression_Of
                 (New_Node, Clone_Node (Expression_Of (Node), True));
               Set_Next_Variable (New_Node, Empty_Node);

            when N_Expression =>
               Set_First_Term (New_Node, Clone_Node (First_Term (Node), True));
               Set_Next_Expression_In_List
                 (New_Node, Clone_Node (Next_Expression_In_List (Node), True));

            when N_Term =>
               Set_Current_Term
                 (New_Node, Clone_Node (Current_Term (Node), True));
               Set_Next_Term
                 (New_Node, Clone_Node (Next_Term (Node), True));

            when N_Literal_String_List =>
               Set_First_Expression_In_List
                 (New_Node, Clone_Node (First_Expression_In_List (Node),
                                        True));

            when N_Variable_Reference =>
               --  String_Type_Of is set to the same value as for Node, and
               --  this needs to be fixed in a post-processing phase.
               --  Same for Package_Node_Of
               null;

            when N_External_Value =>
               Set_External_Reference_Of
                 (New_Node, Clone_Node (External_Reference_Of (Node), True));
               Set_External_Default_Of
                 (New_Node, Clone_Node (External_Default_Of (Node), True));

            when N_Attribute_Reference =>
               --  Package_Node_Of is set to the same value of for Node, and
               --  this needs to be fixed in a post-processing phase.
               null;

            when N_Case_Construction =>
               Set_Case_Variable_Reference_Of
                 (New_Node,
                  Clone_Node (Case_Variable_Reference_Of (Node), True));
               Set_First_Case_Item_Of
                 (New_Node, Clone_Node (First_Case_Item_Of (Node), True));

            when N_Case_Item =>
               Set_First_Choice_Of
                 (New_Node, Clone_Node (First_Choice_Of (Node), True));
               Set_First_Declarative_Item_Of
                 (New_Node,
                  Clone_Node (First_Declarative_Item_Of (Node), True));
         end case;
      end if;

      return New_Node;
   end Clone_Node;

   ----------------
   -- Add_At_End --
   ----------------

   procedure Add_At_End
     (Parent                       : Project_Node_Id;
      Expr                         : Project_Node_Id;
      Add_Before_First_Case_Or_Pkg : Boolean := False)
   is
      Real_Parent : Project_Node_Id;
      New_Decl, Decl, Next : Project_Node_Id;
   begin
      if Kind_Of (Expr) /= N_Declarative_Item then
         New_Decl := Default_Project_Node (N_Declarative_Item);
         Set_Current_Item_Node (New_Decl, Expr);
      else
         New_Decl := Expr;
      end if;

      if Kind_Of (Parent) = N_Project then
         Real_Parent := Project_Declaration_Of (Parent);
      else
         Real_Parent := Parent;
      end if;

      Decl := First_Declarative_Item_Of (Real_Parent);

      if Decl = Empty_Node then
         Set_First_Declarative_Item_Of (Real_Parent, New_Decl);
      else
         loop
            Next := Next_Declarative_Item (Decl);
            exit when Next = Empty_Node
              or else
              (Add_Before_First_Case_Or_Pkg
               and then (Kind_Of (Current_Item_Node (Next))
                         = N_Case_Construction
                         or else Kind_Of (Current_Item_Node (Next))
                         = N_Package_Declaration));
            Decl := Next;
         end loop;

         Set_Next_Declarative_Item (New_Decl, Next);
         Set_Next_Declarative_Item (Decl, New_Decl);
      end if;
   end Add_At_End;

   ------------------
   -- Add_In_Front --
   ------------------

   procedure Add_In_Front
     (Parent : Project_Node_Id;
      Node   : Project_Node_Id)
   is
      Real_Parent : Project_Node_Id;
      New_Decl, Decl : Project_Node_Id;
   begin
      if Kind_Of (Node) /= N_Declarative_Item then
         New_Decl := Default_Project_Node (N_Declarative_Item);
         Set_Current_Item_Node (New_Decl, Node);
      else
         New_Decl := Node;
      end if;

      if Kind_Of (Parent) = N_Project then
         Real_Parent := Project_Declaration_Of (Parent);
      else
         Real_Parent := Parent;
      end if;

      Decl := New_Decl;
      while Next_Declarative_Item (Decl) /= Empty_Node loop
         Decl := Next_Declarative_Item (Decl);
      end loop;

      Set_Next_Declarative_Item
        (Decl, First_Declarative_Item_Of (Real_Parent));
      Set_First_Declarative_Item_Of (Real_Parent, New_Decl);
   end Add_In_Front;

   -----------------------------
   -- Find_Project_Of_Package --
   -----------------------------

   function Find_Project_Of_Package
     (Project : Project_Type; Pkg_Name : String) return Project_Type
   is
      Pkg : Project_Node_Id;
      P   : Project_Type := No_Project;
   begin
      Pkg := Find_Package_Declaration
        (Project.Node, Get_String (Pkg_Name));

      if Pkg /= Empty_Node
        and then Project_Of_Renamed_Package_Of (Pkg) /= Empty_Node
      then
         Pkg := Project_Of_Renamed_Package_Of (Pkg);
         P := Get_Project_From_Name
           (Project_Registry'Class (Get_Registry (Project)),
            Prj.Tree.Name_Of (Pkg));
      end if;

      if P = No_Project then
         P := Project;
      end if;
      return P;
   end Find_Project_Of_Package;

   -----------------------------
   -- Remove_Imported_Project --
   -----------------------------

   procedure Remove_Imported_Project
     (Project : Project_Type; Imported_Project : Project_Type)
   is
      With_Clause : Project_Node_Id := First_With_Clause_Of (Project.Node);
      Next : Project_Node_Id;
      Name : Name_Id;
   begin
      --  ??? When the project is no longer found in the hierarchy, it should
      --  ??? also be removed from the htable in Prj.Tree, so that another
      --  ??? project by that nane can be loaded.

      --  Cleanup the name

      Name := Get_String
        (Base_Name (Project_Name (Imported_Project), Project_File_Extension));

      if With_Clause /= Empty_Node
        and then Prj.Tree.Name_Of (With_Clause) = Name
      then
         Set_First_With_Clause_Of
           (Project.Node, Next_With_Clause_Of (With_Clause));
      else
         loop
            Next := Next_With_Clause_Of (With_Clause);
            exit when Next = Empty_Node;

            if Prj.Tree.Name_Of (Next) = Name then
               Set_Next_With_Clause_Of
                 (With_Clause, Next_With_Clause_Of (Next));
            end if;

            With_Clause := Next;
         end loop;
      end if;

      Set_Project_Modified (Project, True);

      --  Need to reset all the caches, since the caches contain the indirect
      --  dependencies as well.
      Reset_Cache (Project, Imported_By => False);
      Reset_Cache (Imported_Project, Imported_By => True);
   end Remove_Imported_Project;

   --------------------------
   -- Set_With_Clause_Path --
   --------------------------

   procedure Set_With_Clause_Path
     (With_Clause               : Project_Node_Id;
      Imported_Project_Location : String;
      Imported_Project          : Project_Node_Id;
      Importing_Project         : Project_Node_Id;
      Use_Relative_Path         : Boolean) is
   begin
      Start_String;

      if Use_Relative_Path then
         Store_String_Chars
           (Relative_Path_Name
            (Imported_Project_Location,
             Dir_Name (Get_String (Path_Name_Of (Importing_Project)))));
      else
         Store_String_Chars (Imported_Project_Location);
      end if;

      Set_String_Value_Of (With_Clause, End_String);

      Set_Path_Name_Of (With_Clause, Prj.Tree.Path_Name_Of (Imported_Project));
      Set_Project_Node_Of (With_Clause, Imported_Project);
   end Set_With_Clause_Path;

   --------------------------
   -- Add_Imported_Project --
   --------------------------

   function Add_Imported_Project
     (Project                   : Project_Type;
      Imported_Project_Location : String;
      Report_Errors             : Output.Output_Proc := null;
      Use_Relative_Path         : Boolean)
      return Import_Project_Error
   is
      use Prj.Tree.Tree_Private_Part;
      use type Output.Output_Proc;

      procedure Fail (S1 : String; S2 : String := ""; S3 : String := "");
      --  Replaces Osint.Fail

      ----------
      -- Fail --
      ----------

      procedure Fail (S1 : String; S2 : String := ""; S3 : String := "") is
      begin
         Report_Errors (S1 & S2 & S3);
      end Fail;

      With_Clause      : Project_Node_Id;
      Imported_Project : Project_Node_Id := Empty_Node;

      Basename : constant String := Base_Name
        (Imported_Project_Location, Project_File_Extension);
      Dep_ID   : Name_Id;
      Dep_Name : Prj.Tree.Tree_Private_Part.Project_Name_And_Node;
      Imported : constant String := Normalize_Pathname
        (Name          => Imported_Project_Location,
         Resolve_Links => True);

   begin
      Output.Set_Special_Output (Report_Errors);
      Prj.Com.Fail := Fail'Unrestricted_Access;

      Dep_ID := Get_String (Basename);

      Dep_Name := Tree_Private_Part.Projects_Htable.Get (Dep_ID);

      if Dep_Name /= No_Project_Name_And_Node then
         if not File_Equal
           (Format_Pathname (Get_String (Path_Name_Of (Dep_Name.Node))),
            Imported)
         then
            if Report_Errors /= null then
               Report_Errors
                 (-"A different project with the same name"
                  & " already exists in the project tree.");
            end if;
            Output.Set_Special_Output (null);
            Prj.Com.Fail := null;
            return Project_Already_Exists;
         else
            Imported_Project := Dep_Name.Node;
         end if;

      else
         Prj.Part.Parse (Imported_Project, Imported,
                         Always_Errout_Finalize => True);
      end if;

      if Imported_Project = Empty_Node then
         Trace (Me, "Add_Imported_Project: imported project not found ("
                & Imported_Project_Location & ")");
         Output.Set_Special_Output (null);
         Prj.Com.Fail := null;
         return Imported_Project_Not_Found;
      end if;

      --  Make sure we are not trying to import ourselves, since otherwise it
      --  would result in an infinite loop when manipulating the project

      if Prj.Tree.Name_Of (Project.Node) =
        Prj.Tree.Name_Of (Imported_Project)
      then
         if Report_Errors /= null then
            Report_Errors (-"Cannot add dependency to self");
         end if;
         Output.Set_Special_Output (null);
         Prj.Com.Fail := null;
         return Dependency_On_Self;
      end if;

      --  Check if it is already there. If we have the same name but not the
      --  same path, we replace it anyway

      With_Clause := First_With_Clause_Of (Project.Node);
      while With_Clause /= Empty_Node loop
         if Prj.Tree.Name_Of (Project_Node_Of (With_Clause)) =
           Prj.Tree.Name_Of (Imported_Project)
         then
            if Report_Errors /= null then
               Report_Errors (-"This dependency already exists");
            end if;
            Output.Set_Special_Output (null);
            Prj.Com.Fail := null;
            return Dependency_Already_Exists;
         end if;
         With_Clause := Next_With_Clause_Of (With_Clause);
      end loop;

      With_Clause := Default_Project_Node (N_With_Clause);
      Set_Name_Of (With_Clause, Prj.Tree.Name_Of (Imported_Project));

      Set_Next_With_Clause_Of
        (With_Clause, First_With_Clause_Of (Project.Node));
      Set_First_With_Clause_Of (Project.Node, With_Clause);

      Set_With_Clause_Path
        (With_Clause, Normalize_Pathname (Imported_Project_Location),
         Imported_Project, Project.Node, Use_Relative_Path);

      if Has_Circular_Dependencies (Project.Node) then
         Set_First_With_Clause_Of
           (Project.Node, Next_With_Clause_Of (With_Clause));
         if Report_Errors /= null then
            Report_Errors
              (-"Circular dependency detected in the project hierarchy");
         end if;
         Trace (Me, "Circular dependency detected in the project hierarchy");
         Output.Set_Special_Output (null);
         Prj.Com.Fail := null;
         return Circular_Dependency;
      end if;

      Output.Set_Special_Output (null);
      Prj.Com.Fail := null;

      Set_Project_Modified (Project, True);

      Reset_Cache (Project, Imported_By => False);
      Reset_Cache
        (Get_Project_From_Name
           (Project_Registry'Class (Get_Registry (Project)), Dep_ID),
         Imported_By => True);
      return Success;

   exception
      when others =>
         Output.Set_Special_Output (null);
         Prj.Com.Fail := null;
         raise;
   end Add_Imported_Project;

   -----------------------------
   -- For_Each_Directory_Node --
   -----------------------------

   procedure For_Each_Directory_Node
     (Project : Project_Type; Action  : Node_Callback)
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
            Current := Current_Item_Node (Node);
            case Kind_Of (Current) is
               when N_Attribute_Declaration =>
                  if Prj.Tree.Name_Of (Current) = Name_Source_Dirs
                    or else Prj.Tree.Name_Of (Current) = Name_Object_Dir
                  then
                     Expr := Expression_Of (Current);
                     while Expr /= Empty_Node loop
                        Term := First_Term (Expr);
                        while Term /= Empty_Node loop
                           Current := Current_Term (Term);

                           case Kind_Of (Current) is
                              when N_Literal_String_List =>
                                 Expr2 := First_Expression_In_List (Current);
                                 while Expr2 /= Empty_Node loop
                                    Current := Current_Term
                                      (First_Term (Expr2));
                                    if Kind_Of (Current) /= N_Literal_String
                                      or else Next_Term (First_Term (Expr2)) /=
                                        Empty_Node
                                    then
                                       Trace
                                         (Me, "Cannot process lists of "
                                          & " non-literal string "
                                          & Kind_Of (Current)'Img);
                                    else
                                       Action (Current);
                                    end if;

                                    Expr2 := Next_Expression_In_List (Expr2);
                                 end loop;

                              when N_Literal_String =>
                                 Action (Current);

                              when others =>
                                 Trace (Me, "Ignoring "
                                        & Kind_Of (Current)'Img);
                                 null;
                           end case;

                           Term := Next_Term (Term);
                        end loop;

                        Expr := Next_Expression_In_List (Expr);
                     end loop;
                  end if;

               when N_Case_Construction =>
                  Expr := First_Case_Item_Of (Current);
                  while Expr /= Empty_Node loop
                     Process_List (First_Declarative_Item_Of (Expr));
                     Expr := Next_Case_Item (Expr);
                  end loop;

               when others =>
                  null;
            end case;

            Node := Next_Declarative_Item (Node);
         end loop;

      end Process_List;

   begin
      Process_List (First_Declarative_Item_Of
           (Project_Declaration_Of (Project.Node)));
   end For_Each_Directory_Node;

   ---------------------
   -- Rename_And_Move --
   ---------------------

   procedure Rename_And_Move
     (Root_Project  : Project_Type;
      Project       : Project_Type;
      New_Name      : String;
      New_Path      : String;
      Report_Errors : Output.Output_Proc := null)
   is
      Old_Path : constant String := Project_Directory (Project);

      procedure Change_Directory (Node : Project_Node_Id);
      --  Change the directory refered to by Node

      ----------------------
      -- Change_Directory --
      ----------------------

      procedure Change_Directory (Node : Project_Node_Id) is
      begin
         case Kind_Of (Node) is
            when N_Literal_String =>
               declare
                  D : constant String := Get_String (String_Value_Of (Node));
               begin
                  if not Is_Absolute_Path (D) then
                     Start_String;
                     Store_String_Chars
                       (Relative_Path_Name
                        (Normalize_Pathname
                           (D, Old_Path, Resolve_Links => False),
                         New_Path));
                     Set_String_Value_Of (Node, End_String);
                  end if;
               end;

            when others =>
               Trace (Me, "For_Each_Directory_Node: unknown node type: "
                      & Kind_Of (Node)'Img);
         end case;
      end Change_Directory;

      D           : constant String :=
        New_Path & To_File_Name (New_Name) & Project_File_Extension;
      Full_Path   : String_Id := No_String;
      Name        : constant Name_Id := Get_String (New_Name);
      Old_Name    : constant Name_Id := Prj.Tree.Name_Of (Project.Node);
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

      Iterator := Find_All_Projects_Importing
        (Root_Project, Project, Direct_Only => False);

      loop
         Imported := Current (Iterator);
         exit when Imported = No_Project;

         With_Clause := First_With_Clause_Of (Imported.Node);
         Modified := False;

         while With_Clause /= Empty_Node loop
            if Project_Node_Of (With_Clause) = Project.Node then
               Set_Name_Of (With_Clause, Name);

               Set_Path_Name_Of (With_Clause, Path_Name_Of (Project.Node));

               if Full_Path = No_String then
                  Start_String;
                  Store_String_Chars (D);
                  Full_Path := End_String;
               end if;

               Set_String_Value_Of (With_Clause, Full_Path);
               Modified := True;
            end if;

            With_Clause := Next_With_Clause_Of (With_Clause);
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

      Set_Name_Of (Project.Node, Name);

      Name_Len := New_Path'Length;
      Name_Buffer (1 .. Name_Len) := New_Path;
      Set_Directory_Of (Project.Node, Name_Find);

      Name_Len := D'Length;
      Name_Buffer (1 .. Name_Len) := D;
      Set_Path_Name_Of (Project.Node, Name_Find);

      --  Unregister the old name
      Prj.Tree.Tree_Private_Part.Projects_Htable.Set
        (Prj.Tree.Name_Of (Project.Node),
         Prj.Tree.Tree_Private_Part.Project_Name_And_Node'
         (Name => Old_Name,
          Node => Empty_Node,
          Extended => False));

      --  Register the new name
      Prj.Tree.Tree_Private_Part.Projects_Htable.Set
        (Prj.Tree.Name_Of (Project.Node),
         Prj.Tree.Tree_Private_Part.Project_Name_And_Node'
         (Name => Name,
          Node => Project.Node,
          Extended => False));

      Reset_Name_Table
        (Project_Registry (Get_Registry (Project)),
         Project, Get_String (Old_Name), New_Name);

      Set_Project_Modified (Project, True);
      Reset_Cache (Project, Imported_By => True);
      Reset_Cache (Project, Imported_By => False);

      --  This is no longer the default project, since it was
      --  renamed. Otherwise, Project_Path would still return "" when saving
      --  the default project.

      Set_Is_Default (Project, False);
   end Rename_And_Move;

   ----------------------------
   -- Create_Default_Project --
   ----------------------------

   function Create_Default_Project
     (Registry : Projects.Registry.Project_Registry'Class;
      Name, Path : String)
      return Project_Type
   is
      Project     : Project_Type;
      No_Scenario : constant Scenario_Variable_Array (1 .. 0) :=
        (others => No_Variable);
      Values      : Argument_List (1 .. 1);

   begin
      Project := Create_Project (Registry, Name, Path);

      Values (1) := new String'(".");
      Update_Attribute_Value_In_Scenario
        (Project,
         Scenario_Variables => No_Scenario,
         Attribute_Name     => "source_dirs",
         Values             => Values (1 .. 1));
      Free (Values (1));

      Update_Attribute_Value_In_Scenario
        (Project,
         Scenario_Variables => No_Scenario,
         Attribute_Name     => "object_dir",
         Value              => ".");

      Update_Attribute_Value_In_Scenario
        (Project,
         Scenario_Variables => No_Scenario,
         Attribute_Name     => "default_switches",
         Values             => Default_Builder_Switches,
         Attribute_Index    => Ada_String,
         Pkg_Name           => "builder");

      Update_Attribute_Value_In_Scenario
        (Project,
         Scenario_Variables => No_Scenario,
         Attribute_Name     => "default_switches",
         Values             => Default_Compiler_Switches,
         Attribute_Index    => Ada_String,
         Pkg_Name           => "compiler");

      Update_Attribute_Value_In_Scenario
        (Project,
         Scenario_Variables => No_Scenario,
         Attribute_Name     => "default_switches",
         Values             => Default_Linker_Switches,
         Attribute_Index    => Ada_String,
         Pkg_Name           => "linker");

      return Project;
   end Create_Default_Project;

   --------------------
   -- Create_Project --
   --------------------

   function Create_Project
     (Registry : Projects.Registry.Project_Registry'Class;
      Name, Path : String) return Project_Type
   is
      D       : constant String :=
        Path & To_File_Name (Name) & Project_File_Extension;
      Project : constant Project_Node_Id := Default_Project_Node (N_Project);
      Project_Name : Name_Id;
      P : Project_Type;

   begin
      --  Adding the name of the project
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      Project_Name := Name_Find;
      Set_Name_Of (Project, Project_Name);

      --  Adding the project path
      Name_Len := Path'Length;
      Name_Buffer (1 .. Name_Len) := Path;
      Set_Directory_Of (Project, Name_Find);

      Name_Len := D'Length;
      Name_Buffer (1 .. Name_Len) := D;
      Set_Path_Name_Of (Project, Name_Find);

      --  Create the project declaration
      Set_Project_Declaration_Of
        (Project, Default_Project_Node (N_Project_Declaration));

      --  Register the name of the project so that we can retrieve it from one
      --  of its views
      Prj.Tree.Tree_Private_Part.Projects_Htable.Set
        (Prj.Tree.Name_Of (Project),
         Prj.Tree.Tree_Private_Part.Project_Name_And_Node'
         (Name => Project_Name,
          Node => Project,
          Extended => False));

      P := Get_Project_From_Name (Registry, Prj.Tree.Name_Of (Project));
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

   ---------------------------------
   -- Replace_Project_Occurrences --
   ---------------------------------

   procedure Replace_Project_Occurrences
     (Root_Project      : Project_Type;
      Project           : Project_Type;
      Use_Relative_Path : Boolean)
   is
      Imported_Path : constant String := Project_Path (Project);
      Iterator : Imported_Project_Iterator := Start
        (Root_Project, Recursive => True);
      With_Clause : Project_Node_Id;
      P : Project_Type;

   begin
      loop
         P := Current (Iterator);
         exit when P = No_Project;

         With_Clause := First_With_Clause_Of (P.Node);
         while With_Clause /= Empty_Node loop
            if Prj.Tree.Name_Of (Project_Node_Of (With_Clause)) =
              Prj.Tree.Name_Of (Project.Node)
            then
               Set_With_Clause_Path
                 (With_Clause, Imported_Path, Project.Node,
                  P.Node, Use_Relative_Path);
               Set_Project_Modified (P, True);
               Reset_Cache (P, Imported_By => True);
            end if;
            With_Clause := Next_With_Clause_Of (With_Clause);
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
      Typ, Var : Project_Node_Id;
   begin
      Projects.Editor.Normalize.Normalize (Project);
      Typ := Create_Type (Project.Node, Type_Name);
      Var := Create_Typed_Variable
        (Project.Node, Name, Typ, Add_Before_First_Case_Or_Pkg => True);
      Set_Value_As_External (Var, Env_Name);

      Set_Project_Modified (Project, True);

      Name_Len := Env_Name'Length;
      Name_Buffer (1 .. Name_Len) := Env_Name;

      Reset_Scenario_Variables_Cache
        (Project_Registry'Class (Get_Registry (Project)));

      return (Name        => Name_Find,
              Default     => No_String,
              String_Type => Typ);
   end Create_Environment_Variable;

end Projects.Editor;
