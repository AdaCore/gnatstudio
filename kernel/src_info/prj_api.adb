-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  Gnat sources dependencies
with Prj;              use Prj;
pragma Elaborate_All (Prj);
with Prj.Tree;         use Prj.Tree;
with Prj.Part;         use Prj.Part;
with Prj.Attr;         use Prj.Attr;
with Prj.Util;         use Prj.Util;
with Prj.Ext;          use Prj.Ext;
with Prj_Normalize;    use Prj_Normalize;
with Snames;           use Snames;
pragma Elaborate_All (Snames);
with Namet;            use Namet;
pragma Elaborate_All (Namet);
with Types;            use Types;
with Csets;            use Csets;
pragma Elaborate_All (Csets);
with Stringt;          use Stringt;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with String_Utils;     use String_Utils;
with Ada.Exceptions;   use Ada.Exceptions;
with Project_Browsers; use Project_Browsers;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Glide_Intl;    use Glide_Intl;

with Traces; use Traces;

package body Prj_API is

   Me : Debug_Handle := Create ("Prj_API");

   File_Specific_Switches_Attribute : constant String := "switches";
   --  Name of the attribute used for file specific switches. Its index is the
   --  unit name

   Default_Switches_Attribute : constant String := "default_switches";
   --  Name of the attribute used for default switches.

   procedure Set_Expression
     (Var_Or_Attribute : Project_Node_Id; Expr : Project_Node_Id);
   --  Set Var as the expression to use for the value of Var. This
   --  properly handles standard variables and variables defined through
   --  references to external environment variables.

   function Find_Last_Declaration_Of
     (Parent     : Project_Node_Id;
      Attr_Name  : Types.Name_Id;
      Attr_Index : String := "") return Project_Node_Id;
   --  Find the last declaration for the attribute Attr_Name, in the
   --  declarative list contained in Parent.
   --  The returned value is the last such declaration, or Empty_Node if there
   --  was none.

   function Find_Node_By_Name
     (Project : Project_Node_Id;
      Kind    : Project_Node_Kind;
      Name    : Name_Id) return Project_Node_Id;
   --  Find a node given its name

   procedure Move_From_Common_To_Case_Construct
     (Project            : Project_Node_Id;
      Pkg_Name           : String;
      Scenario_Variables : Project_Node_Array;
      Attribute_Name     : Types.Name_Id;
      Attribute_Index    : String := "");
   --  Move any declaration for the attribute from the common part of the
   --  project into each branch of the nested case construct. Nothing is done
   --  if there is no such declaration.

   function Attribute_Matches
     (Node            : Project_Node_Id;
      Attribute_Name  : Name_Id;
      Attribute_Index : String) return Boolean;
   --  Return True if Node is an attribute declaration matching Attribute_Name
   --  and Attribute_Index.

   procedure Remove_Attribute_Declarations
     (Parent          : Project_Node_Id;
      Attribute_Name  : Name_Id;
      Attribute_Index : String);
   --  Remove all declarations for Attribute_Name in the declarative item list
   --  of Parent.

   procedure Add_Node_To_List
     (To   : in out Project_Node_Array_Access;
      Last : in out Natural;
      Node : Project_Node_Id);
   --  Add a new node into the list of nodes To.
   --  To is resized as needed

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
     (Root_Project      : Project_Node_Id;
      Ext_Variable_Name : String;
      Specific_Choice   : String;
      Action            : Environment_Variable_Callback);
   --  Iterate over all possible references to an external variable. This
   --  returns N_External_Value, N_Variable_Reference,
   --  N_Typed_Variable_Declaration and N_String_Type_Declaration (the last
   --  three are indirect references through a named variable.

   procedure Remove_Node
     (Parent : Project_Node_Id; Node : Project_Node_Id);
   --  Remove Node from the declaration list in Parent.
   --  This doesn't search recursively inside nested packages, case
   --  constructions, ...

   procedure Remove_Variable_Declaration
     (Project_Or_Package : Project_Node_Id;
      Declaration        : Project_Node_Id);
   --  Remove the variable declaration from the list of variables in
   --  Project_Or_Package.

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Str : Types.String_Id) return String is
      R : String (1 .. Natural (String_Length (Str)));
   begin
      for J in R'Range loop
         R (J) := Get_Character (Get_String_Char (Str, Int (J)));
      end loop;
      return R;
   end Get_String;

   --------------------
   -- Create_Project --
   --------------------

   function Create_Project (Name, Path : String) return Project_Node_Id is
      Project : Project_Node_Id := Default_Project_Node (N_Project);
      Project_Name : Name_Id;
   begin
      --  Adding the name of the project
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      Project_Name := Name_Enter;
      Set_Name_Of (Project, Project_Name);

      --  Adding the project path
      Name_Len := Path'Length;
      Name_Buffer (1 .. Name_Len) := Path;
      Set_Path_Name_Of (Project, Name_Enter);
      Set_Directory_Of (Project, Path_Name_Of (Project));

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
          Modified => False));
      return Project;
   end Create_Project;

   ---------------------
   -- Create_Variable --
   ---------------------

   function Create_Variable
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Kind : Variable_Kind := List)
      return Project_Node_Id
   is
      Node : Project_Node_Id :=
        Default_Project_Node (N_Variable_Declaration, Kind);
   begin
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      Set_Name_Of (Node, Name_Find);

      Add_At_End (Prj_Or_Pkg, Node);

      Set_Next_Variable (Node, First_Variable_Of (Prj_Or_Pkg));
      Set_First_Variable_Of (Prj_Or_Pkg, Node);
      return Node;
   end Create_Variable;

   ----------------------
   -- Create_Attribute --
   ----------------------

   function Create_Attribute
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Index_Name : String := "";
      Kind : Variable_Kind := List)
      return Project_Node_Id
   is
      Node : Project_Node_Id :=
        Default_Project_Node (N_Attribute_Declaration, Kind);
   begin
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      Set_Name_Of (Node, Name_Find);

      if Index_Name /= "" then
         Start_String;
         Store_String_Chars (Index_Name);
         Set_Associative_Array_Index_Of (Node, End_String);
      end if;

      Add_At_End (Prj_Or_Pkg, Node);
      return Node;
   end Create_Attribute;

   ---------------------------
   -- Create_Typed_Variable --
   ---------------------------

   function Create_Typed_Variable
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Typ  : Project_Node_Id;
      Add_Before_First_Case_Or_Pkg : Boolean := False)
      return Project_Node_Id
   is
      Node : Project_Node_Id :=
        Default_Project_Node (N_Typed_Variable_Declaration, Prj.Single);
   begin
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      Set_Name_Of (Node, Name_Find);
      Set_String_Type_Of (Node, Typ);

      Add_At_End (Prj_Or_Pkg, Node, Add_Before_First_Case_Or_Pkg);

      Set_Next_Variable (Node, First_Variable_Of (Prj_Or_Pkg));
      Set_First_Variable_Of (Prj_Or_Pkg, Node);
      return Node;
   end Create_Typed_Variable;

   -----------------
   -- Create_Type --
   -----------------

   function Create_Type
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String)
      return Project_Node_Id
   is
      Node : Project_Node_Id;
   begin
      Node := Default_Project_Node (N_String_Type_Declaration);
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      Set_Name_Of (Node, Name_Find);
      Add_At_End (Prj_Or_Pkg, Node, True);
      return Node;
   end Create_Type;

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

   --------------------
   -- Delete_Package --
   --------------------

   procedure Delete_Package (Project : Project_Node_Id; Pkg_Name : String) is
      Node, Next : Project_Node_Id;
      Current : Project_Node_Id := Empty_Node;
   begin
      --  Remove the package from the list
      Node := First_Package_Of (Project);
      if Get_Name_String (Prj.Tree.Name_Of (Node)) = Pkg_Name then
         Current := First_Package_Of (Project);
         Set_First_Package_Of (Project, Next_Package_In_Project (Node));
      else
         loop
            Next := Next_Package_In_Project (Node);
            exit when Next = Empty_Node;

            if Get_Name_String (Prj.Tree.Name_Of (Next)) = Pkg_Name then
               Current := Next;
               Set_Next_Package_In_Project
                 (Node, Next_Package_In_Project (Next));
            end if;
            Node := Next;
         end loop;
      end if;

      --  Remove the declaration from the list of decl. items
      if Current /= Empty_Node then
         Remove_Node (Project, Current);
      end if;
   end Delete_Package;

   ---------------------------
   -- Get_Or_Create_Package --
   ---------------------------

   function Get_Or_Create_Package
     (Project : Project_Node_Id; Pkg : String) return Project_Node_Id
   is
      Pack : Project_Node_Id;
      N : Name_Id;
   begin
      Name_Len := Pkg'Length;
      Name_Buffer (1 .. Name_Len) := Pkg;
      N := Name_Find;

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

      for Index in Package_Attributes.First .. Package_Attributes.Last loop
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

   --------------------
   -- Append_To_List --
   --------------------

   procedure Append_To_List (Var : Project_Node_Id; Value : String) is
      Expr : Project_Node_Id;
   begin
      pragma Assert (Expression_Kind_Of (Var) = Prj.List);

      Start_String;
      Store_String_Chars (Value);
      Expr := Expression_Of (Var);
      Concatenate_List (Expr, String_As_Expression (End_String));
      Set_Expression_Of (Var, Expr);
   end Append_To_List;

   --------------------------
   -- String_As_Expression --
   --------------------------

   function String_As_Expression (Value : String_Id) return Project_Node_Id is
   begin
      return Enclose_In_Expression (Create_Literal_String (Value));
   end String_As_Expression;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Var : Project_Node_Id; Value : String) is
      Str : Project_Node_Id;
   begin
      pragma Assert (Expression_Kind_Of (Var) = Prj.Single);

      --  ??? Should reuse the existing N_Expression for the variable, instead
      --  of creating a new one.

      case Kind_Of (Var) is
         when N_Typed_Variable_Declaration =>
            pragma Assert (String_Type_Of (Var) /= Empty_Node);
            Str := First_Literal_String (String_Type_Of (Var));

            --  Check that the value is valid, and reuse the string_id.
            while Str /= Empty_Node loop
               String_To_Name_Buffer (String_Value_Of (Str));
               if Name_Buffer (Name_Buffer'First .. Name_Len) = Value then
                  Set_Expression
                    (Var, String_As_Expression (String_Value_Of (Str)));
                  return;
               end if;
               Str := Next_Literal_String (Str);
            end loop;

            raise Invalid_Value;

         when N_Variable_Declaration | N_Attribute_Declaration =>
            Start_String;
            Store_String_Chars (Value);
            Set_Expression (Var, String_As_Expression (End_String));

         when others =>
            raise Program_Error;
      end case;
   end Set_Value;

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

   --------------------
   -- Set_Expression --
   --------------------

   procedure Set_Expression
     (Var_Or_Attribute : Project_Node_Id; Expr : Project_Node_Id) is
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

   ---------------------
   -- Get_Environment --
   ---------------------

   function Get_Environment (Var_Or_Attribute : Project_Node_Id)
      return String_Id
   is
      Ext : Project_Node_Id;
   begin
      --  ??? We do not correctly detect constructions like
      --  ???    A : A_Type := "A" & external ("OS");
      --  ??? The external reference must be by itself in the reference

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

   -----------------
   -- Type_Values --
   -----------------

   function Type_Values (Var_Or_Type : Project_Node_Id)
      return String_List_Iterator
   is
      Typ : Project_Node_Id := Var_Or_Type;
   begin
      if Kind_Of (Var_Or_Type) = N_Typed_Variable_Declaration then
         Typ := String_Type_Of (Var_Or_Type);
      end if;

      return (Current => First_Literal_String (Typ));
   end Type_Values;

   --------------
   -- Value_Of --
   --------------

   function Value_Of (Var : Project_Node_Id) return String_List_Iterator is
      V, Expr : Project_Node_Id;
   begin
      pragma Assert
        (Kind_Of (Var) = N_Typed_Variable_Declaration
         or else Kind_Of (Var) = N_Variable_Declaration);

      V := Expression_Of (Var);

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
   end Value_Of;

   -----------------
   -- Concatenate --
   -----------------

   procedure Concatenate
     (Expr : in out Project_Node_Id; Node : Project_Node_Id)
   is
      Term : Project_Node_Id;
   begin
      pragma Assert (Expr = Empty_Node
                     or else Kind_Of (Expr) = N_Expression);
      pragma Assert (Node /= Empty_Node);
      pragma Assert (Kind_Of (Node) /= N_Expression);

      --  Create the expression if needed

      if Expr = Empty_Node then
         Expr := Default_Project_Node (N_Expression, Single);
      end if;

      --  Create the first term if needed

      Term := First_Term (Expr);
      pragma Assert (Term = Empty_Node or else Kind_Of (Term) = N_Term);
      if Term = Empty_Node then
         if Kind_Of (Node) = N_Term then
            Set_Next_Term (Node, Empty_Node);
            Term := Node;
         else
            Term := Default_Project_Node (N_Term, Single);
            Set_Current_Term (Term, Node);
         end if;
         Set_First_Term (Expr, Term);

      else
         --  We append at the end

         while Next_Term (Term) /= Empty_Node loop
            Term := Next_Term (Term);
         end loop;

         if Kind_Of (Node) = N_Term then
            Set_Next_Term (Node, Empty_Node);
            Set_Next_Term (Term, Node);
            Term := Node;
         else
            Expr := Default_Project_Node (N_Term, Single);
            Set_Next_Term (Term, Expr);
            Set_Current_Term (Expr, Node);
         end if;
      end if;
   end Concatenate;

   ----------------------
   -- Concatenate_List --
   ----------------------

   procedure Concatenate_List
     (Expr : in out Project_Node_Id; Expr2 : Project_Node_Id)
   is
      Term, L, E : Project_Node_Id;
   begin
      pragma Assert (Expr = Empty_Node or else Kind_Of (Expr) = N_Expression);
      pragma Assert (Kind_Of (Expr2) = N_Expression);

      if Expr = Empty_Node then
         L := Default_Project_Node (N_Literal_String_List, List);
         Set_First_Expression_In_List (L, Expr2);
         Expr := Enclose_In_Expression (L);

      else
         Term := First_Term (Expr);
         pragma Assert (Term /= Empty_Node and then Kind_Of (Term) = N_Term);
         L := Current_Term (Term);
         pragma Assert
           (L /= Empty_Node and then Kind_Of (L) = N_Literal_String_List);

         E := First_Expression_In_List (L);
         pragma Assert (Kind_Of (E) = N_Expression);

         while Next_Expression_In_List (E) /= Empty_Node loop
            E := Next_Expression_In_List (E);
         end loop;
         Set_Next_Expression_In_List (E, Expr2);
      end if;
   end Concatenate_List;

   ------------------
   -- Get_Switches --
   ------------------

   procedure Get_Switches
     (Project          : Project_Id;
      In_Pkg           : String;
      File             : String;
      Language         : Types.Name_Id;
      Value            : out Variable_Value;
      Is_Default_Value : out Boolean)
   is
      Pkg : Package_Id := Projects.Table (Project).Decl.Packages;
      Pkg_Name : Name_Id;
      Switches : Name_Id;
      Var_Value : Variable_Value := Nil_Variable_Value;
      The_Array : Array_Element_Id;

   begin
      Name_Len := In_Pkg'Length;
      Name_Buffer (1 .. Name_Len) := In_Pkg;
      Pkg_Name := Name_Find;

      Name_Len := File_Specific_Switches_Attribute'Length;
      Name_Buffer (1 .. Name_Len) := File_Specific_Switches_Attribute;
      Switches := Name_Find;

      Pkg  := Prj.Util.Value_Of
        (Name        => Pkg_Name,
         In_Packages => Projects.Table (Project).Decl.Packages);

      --  Do we have some file-specific switches ?
      if Pkg /= No_Package and then File /= "" then
         The_Array := Value_Of
             (Name      => Switches,
              In_Arrays => Packages.Table (Pkg).Decl.Arrays);
         Name_Len := File'Length;
         Name_Buffer (1 .. Name_Len) := File;
         Var_Value := Value_Of
             (Index    => Name_Find,
              In_Array => The_Array);

         if Var_Value /= Nil_Variable_Value then
            Value := Var_Value;
            Is_Default_Value := False;
            return;
         end if;
      end if;

      --  Else use the default switches

      if Pkg /= No_Package
        and then Packages.Table (Pkg).Decl.Attributes /= No_Variable
      then
         Name_Len := Default_Switches_Attribute'Length;
         Name_Buffer (1 .. Name_Len) := Default_Switches_Attribute;
         Switches := Name_Find;

         --  Indexed by the language ?

         The_Array := Value_Of
             (Name      => Switches,
              In_Arrays => Packages.Table (Pkg).Decl.Arrays);
         Var_Value := Value_Of
             (Index    => Language,
              In_Array => The_Array);

         if Var_Value /= Nil_Variable_Value then
            Value := Var_Value;
            Is_Default_Value := True;
            return;
         end if;
      end if;

      Is_Default_Value := True;
      Value := Nil_Variable_Value;
   end Get_Switches;

   ------------
   -- Length --
   ------------

   function Length (Value : Variable_Value) return Integer is
      V : String_List_Id;
      Num : Natural := 0;
   begin
      case Value.Kind is
         when Undefined =>
            return 0;

         when Single =>
            return 1;

         when List =>
            V := Value.Values;
            while V /= Nil_String loop
               Num := Num + 1;
               V := String_Elements.Table (V).Next;
            end loop;
            return Num;
      end case;
   end Length;

   --------------------------
   -- Is_External_Variable --
   --------------------------

   function Is_External_Variable (Var : Project_Node_Id) return Boolean is
   begin
      return Kind_Of (Current_Term (First_Term (Expression_Of (Var))))
        = N_External_Value;
   end Is_External_Variable;

   -----------------------------
   -- Find_Scenario_Variables --
   -----------------------------

   function Find_Scenario_Variables
     (Project : Project_Node_Id;
      Parse_Imported : Boolean := True) return Project_Node_Array
   is
      function Count_Vars (In_Project : Project_Node_Id) return Natural;
      --  Return the number of scenario variables in In_Project, its
      --  packages and imported projects

      procedure Register_Vars (In_Project : Project_Node_Id);
      --  Register all the scenario variables from In_Projects, its
      --  packages and imported projects in List.

      procedure Add_If_Not_In_List (Var : Project_Node_Id);
      --  Add Var in the list of scenario if it is not already there (see the
      --  documentation for Find_Scenario_Variables for the exact rules used to
      --  detect aliases).

      ----------------
      -- Count_Vars --
      ----------------

      function Count_Vars (In_Project : Project_Node_Id) return Natural is
         Prj : Project_Node_Id;
         Count : Natural := 0;
         Pkg : Project_Node_Id := In_Project;
         Var : Project_Node_Id;
      begin
         --  For all the packages and the common section
         while Pkg /= Empty_Node loop
            Var := First_Variable_Of (Pkg);
            while Var /= Empty_Node loop
               if Kind_Of (Var) = N_Typed_Variable_Declaration
                 and then Is_External_Variable (Var)
               then
                  Count := Count + 1;
               end if;

               Var := Next_Variable (Var);
            end loop;

            if Pkg = In_Project then
               Pkg := First_Package_Of (In_Project);
            else
               Pkg := Next_Package_In_Project (Pkg);
            end if;
         end loop;

         if Parse_Imported then
            --  Add the modified project
            Prj := Modified_Project_Of (Project_Declaration_Of (In_Project));
            if  Prj /= Empty_Node then
               Count := Count + Count_Vars (Prj);
            end if;

            --  For all the imported projects
            Prj := First_With_Clause_Of (In_Project);
            while Prj /= Empty_Node loop
               Count := Count + Count_Vars (Project_Node_Of (Prj));
               Prj := Next_With_Clause_Of (Prj);
            end loop;
         end if;

         return Count;
      end Count_Vars;


      List : Project_Node_Array (1 .. Count_Vars (Project));
      Current : Positive := 1;

      ------------------------
      -- Add_If_Not_In_List --
      ------------------------

      procedure Add_If_Not_In_List (Var : Project_Node_Id) is
         V : constant String_Id := External_Reference_Of (Var);
      begin
         for Index in 1 .. Current - 1 loop
            if String_Equal (External_Reference_Of (List (Index)), V) then
               return;
            end if;
         end loop;
         List (Current) := Var;
         Current := Current + 1;
      end Add_If_Not_In_List;

      -------------------
      -- Register_Vars --
      -------------------

      procedure Register_Vars (In_Project : Project_Node_Id) is
         Prj : Project_Node_Id;
         Pkg : Project_Node_Id := In_Project;
         Var : Project_Node_Id;
      begin
         --  For all the packages and the common section
         while Pkg /= Empty_Node loop
            Var := First_Variable_Of (Pkg);
            while Var /= Empty_Node loop
               if Kind_Of (Var) = N_Typed_Variable_Declaration
                 and then Is_External_Variable (Var)
               then
                  Add_If_Not_In_List (Var);
               end if;

               Var := Next_Variable (Var);
            end loop;

            if Pkg = In_Project then
               Pkg := First_Package_Of (In_Project);
            else
               Pkg := Next_Package_In_Project (Pkg);
            end if;
         end loop;

         if Parse_Imported then
            --  Add the modified project
            Prj := Modified_Project_Of (Project_Declaration_Of (In_Project));
            if  Prj /= Empty_Node then
               Register_Vars (Prj);
            end if;

            --  For all the imported projects
            --  ??? Should avoid parsing again the projects we have already
            --  ??? parsed.
            Prj := First_With_Clause_Of (In_Project);
            while Prj /= Empty_Node loop
               Register_Vars (Project_Node_Of (Prj));
               Prj := Next_With_Clause_Of (Prj);
            end loop;
         end if;
      end Register_Vars;

   begin
      Register_Vars (Project);
      return List (1 .. Current - 1);
   end Find_Scenario_Variables;

   ---------------------------
   -- External_Reference_Of --
   ---------------------------

   function External_Reference_Of (Var : Project_Node_Id) return String_Id is
      Expr : Project_Node_Id := Expression_Of (Var);
   begin
      Expr := First_Term   (Expr);
      Expr := Current_Term (Expr);

      if Kind_Of (Expr) = N_External_Value then
         Expr := External_Reference_Of (Expr);
         return String_Value_Of (Expr);
      else
         return No_String;
      end if;
   end External_Reference_Of;

   ----------------------
   -- External_Default --
   ----------------------

   function External_Default (Var : Project_Node_Id)
      return Project_Node_Id
   is
      Expr : Project_Node_Id := Expression_Of (Var);
   begin
      Expr := First_Term   (Expr);
      Expr := Current_Term (Expr);

      if Kind_Of (Expr) = N_External_Value then
         return External_Default_Of (Expr);
      else
         return Empty_Node;
      end if;
   end External_Default;

   --------------------------------
   -- Get_Project_View_From_Name --
   --------------------------------

   function Get_Project_View_From_Name (Name : Name_Id) return Project_Id is
   begin
      for J in Projects.Table'First .. Projects.Last loop
         if Projects.Table (J).Name = Name then
            return J;
         end if;
      end loop;
      return No_Project;
   end Get_Project_View_From_Name;

   ---------------------------
   -- Get_Project_From_View --
   ---------------------------

   function Get_Project_From_View (View : Project_Id) return Project_Node_Id is
   begin
      return Tree_Private_Part.Projects_Htable.Get
        (Projects.Table (View).Name).Node;
   end Get_Project_From_View;

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

   ------------------------
   -- Typed_Values_Count --
   ------------------------

   function Typed_Values_Count (Var_Or_Attribute : Project_Node_Id)
      return Positive
   is
      Count : Natural := 0;
      T     : Project_Node_Id;
   begin
      if Kind_Of (Var_Or_Attribute) = N_Typed_Variable_Declaration
        or else (Kind_Of (Var_Or_Attribute) = N_Variable_Reference
                 and then String_Type_Of (Var_Or_Attribute) /= Empty_Node)
      then
         T := String_Type_Of (Var_Or_Attribute);
         T := First_Literal_String (T);
         while T /= Empty_Node loop
            Count := Count + 1;
            T := Next_Literal_String (T);
         end loop;
         return Count;

      else
         return Positive'Last;
      end if;
   end Typed_Values_Count;

   --------------------------
   -- Add_Imported_Project --
   --------------------------

   procedure Add_Imported_Project
     (Project : Project_Node_Id;
      Imported_Project_Location : String)
   is
      use Prj.Tree.Tree_Private_Part;
      With_Clause : Project_Node_Id := First_With_Clause_Of (Project);
      Imported_Project : Project_Node_Id;

      Ext      : constant String := GNAT.Directory_Operations.File_Extension
        (Imported_Project_Location);
      Basename : constant String := Base_Name
        (Imported_Project_Location, Ext);
      Dep_ID   : Name_Id;
      Dep_Name : Prj.Tree.Tree_Private_Part.Project_Name_And_Node;
   begin
      Name_Len := Basename'Length;
      Name_Buffer (1 .. Name_Len) := Basename;
      Dep_ID := Name_Find;

      Dep_Name := Tree_Private_Part.Projects_Htable.Get (Dep_ID);

      if Dep_Name /= No_Project_Name_And_Node then
         if Get_Name_String (Path_Name_Of (Dep_Name.Node)) /=
           GNAT.Directory_Operations.Normalize_Pathname
           (Imported_Project_Location)
         then
            Raise_Exception
              (Project_Error'Identity,
               -"A different project with the same name"
               & " already exists in the project tree.");
            return;
         else
            Imported_Project := Dep_Name.Node;
         end if;
      else
         Prj.Part.Parse (Imported_Project, Imported_Project_Location,
                         Always_Errout_Finalize => True);
      end if;

      --  Make sure we are not trying to import ourselves, since otherwise it
      --  would result in an infinite loop when manipulating the project

      if Prj.Tree.Name_Of (Project) = Prj.Tree.Name_Of (Imported_Project) then
         Raise_Exception
           (Project_Warning'Identity, -"Cannot add dependency to self");
         return;
      end if;

      --  Check if it is already there. If we have the same name but not the
      --  same path, we replace it anyway

      while With_Clause /= Empty_Node loop
         if Prj.Tree.Name_Of (Project_Node_Of (With_Clause)) =
           Prj.Tree.Name_Of (Imported_Project)
         then
            Raise_Exception
              (Project_Warning'Identity, -"This dependency already exists");
            return;
         end if;
         With_Clause := Next_With_Clause_Of (With_Clause);
      end loop;

      With_Clause := Default_Project_Node (N_With_Clause);
      Set_Project_Node_Of (With_Clause, Imported_Project);
      Set_Name_Of (With_Clause, Prj.Tree.Name_Of (Imported_Project));
      Set_Path_Name_Of (With_Clause, Path_Name_Of (Imported_Project));

      Set_Next_With_Clause_Of (With_Clause, First_With_Clause_Of (Project));
      Set_First_With_Clause_Of (Project, With_Clause);

      Start_String;
      Store_String_Chars (Get_Name_String (Path_Name_Of (Imported_Project)));
      Set_String_Value_Of (With_Clause, End_String);

      if Has_Circular_Dependencies (Project) then
         Set_First_With_Clause_Of (Project, Next_With_Clause_Of (With_Clause));
         Raise_Exception
           (Project_Error'Identity,
            -"Circular dependency detected in the project hierarchy");
      end if;
   end Add_Imported_Project;

   -----------------------------
   -- Remove_Imported_Project --
   -----------------------------

   procedure Remove_Imported_Project
     (Project : Project_Node_Id; Imported_Project : String)
   is
      With_Clause : Project_Node_Id := First_With_Clause_Of (Project);
      Next : Project_Node_Id;
      Name : Name_Id;
   begin
      --  ??? When the project is no longer found in the hierarchy, it should
      --  ??? also be removed from the htable in Prj.Tree, so that another
      --  ??? project by that nane can be loaded.

      --  Cleanup the name

      declare
         Clean_Name : constant String := Base_Name
           (Imported_Project, Project_File_Extension);
      begin
         Name_Len := Clean_Name'Length;
         Name_Buffer (1 .. Name_Len) := Clean_Name;
         Name := Name_Find;
      end;

      if With_Clause /= Empty_Node
        and then Prj.Tree.Name_Of (With_Clause) = Name
      then
         Set_First_With_Clause_Of (Project, Next_With_Clause_Of (With_Clause));
         return;
      end if;

      loop
         Next := Next_With_Clause_Of (With_Clause);
         exit when Next = Empty_Node;

         if Prj.Tree.Name_Of (Next) = Name then
            Set_Next_With_Clause_Of
              (With_Clause, Next_With_Clause_Of (Next));
            return;
         end if;
         With_Clause := Next;
      end loop;
   end Remove_Imported_Project;

   ----------------------------
   -- External_Variable_Name --
   ----------------------------

   function External_Variable_Name
     (Current_Project : Project_Node_Id; Ref : Project_Node_Id)
      return String_Id
   is
      N : constant Name_Id := Prj.Tree.Name_Of (Ref);
      Pkg : Project_Node_Id;
      Variable : Variable_Node_Id;
      Recurse_In_Pkg : Boolean := False;

   begin
      if Package_Node_Of (Ref) /= Empty_Node then
         Pkg := Package_Node_Of (Ref);
      elsif Project_Node_Of (Ref) /= Empty_Node then
         Pkg := Project_Node_Of (Ref);
      else
         Pkg := Current_Project;
         Recurse_In_Pkg := True;
      end if;

      --  Should the project parser set the package_of field when the variable
      --  is defined inside a package ? Currently, it only sets this field if
      --  it is specified in the file itself.

      while Pkg /= Empty_Node loop
         Variable := First_Variable_Of (Pkg);
         while Variable /= Empty_Node loop
            if (Kind_Of (Variable) = N_Variable_Declaration
                or else Kind_Of (Variable) = N_Typed_Variable_Declaration)
              and then Prj.Tree.Name_Of (Variable) = N
            then
               return External_Reference_Of (Variable);
            end if;

            Variable := Next_Variable (Variable);
         end loop;

         if Recurse_In_Pkg then
            if Pkg = Current_Project then
               Pkg := First_Package_Of (Pkg);
            else
               Pkg := Next_Package_In_Project (Pkg);
            end if;
         end if;
      end loop;

      return No_String;
   end External_Variable_Name;

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

   ---------------------------
   -- Enclose_In_Expression --
   ---------------------------

   function Enclose_In_Expression (Node : Project_Node_Id)
      return Project_Node_Id
   is
      Expr : Project_Node_Id := Default_Project_Node (N_Expression, Single);
   begin
      Set_First_Term (Expr, Default_Project_Node (N_Term, Single));
      Set_Current_Term (First_Term (Expr), Node);
      return Expr;
   end Enclose_In_Expression;

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
            S (1) := new String' (Name_Buffer (1 .. Name_Len));

         when List =>
            V := Value.Values;

            for J in S'Range loop
               String_To_Name_Buffer (String_Elements.Table (V).Value);
               S (J) := new String' (Name_Buffer (1 .. Name_Len));
               V := String_Elements.Table (V).Next;
            end loop;
      end case;
      return S;
   end To_Argument_List;

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
        ((Attribute_Index = ""
          and then Associative_Array_Index_Of (Node) = No_String)
         or else (Attribute_Index /= ""
                  and then Associative_Array_Index_Of (Node) /= No_String
                  and then Get_String (Associative_Array_Index_Of (Node)) =
                  Attribute_Index));
   end Attribute_Matches;

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

   ------------------------------
   -- Find_Last_Declaration_Of --
   ------------------------------

   function Find_Last_Declaration_Of
     (Parent  : Project_Node_Id;
      Attr_Name  : Name_Id;
      Attr_Index : String := "") return Project_Node_Id
   is
      Decl, Expr : Project_Node_Id;
      Result : Project_Node_Id := Empty_Node;
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
   -- Update_Attribute_Value_In_Scenario --
   ----------------------------------------

   procedure Update_Attribute_Value_In_Scenario
     (Project            : Project_Node_Id;
      Pkg_Name           : String := "";
      Scenario_Variables : Project_Node_Array;
      Attribute_Name     : String := "";
      Values             : GNAT.OS_Lib.Argument_List;
      Attribute_Index    : String := "";
      Prepend            : Boolean := False)
   is
      Attribute_N : Name_Id;
      List : Project_Node_Id := Empty_Node;
      Pkg, Term, Expr : Project_Node_Id;

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

         --  Do we already have some declarations for this attribute ? If yes,
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
               Set_Project_Node_Of (Term, Project);
            end if;

            Set_Expression_Of (Decl, Expr);
         end if;
      end Add_Or_Replace;

   begin
      Name_Len := Attribute_Name'Length;
      Name_Buffer (1 .. Name_Len) := Attribute_Name;
      Attribute_N := Name_Find;

      Move_From_Common_To_Case_Construct
        (Project, Pkg_Name, Scenario_Variables, Attribute_N, Attribute_Index);

      --  Create the string list for the new values.
      --  This can be prepended later on to the existing list of values.

      List := Default_Project_Node (N_Literal_String_List, Prj.List);

      for A in Values'Range loop
         Start_String;
         Store_String_Chars (Values (A).all);
         Expr := String_As_Expression (End_String);
         Set_Next_Expression_In_List
           (Expr, First_Expression_In_List (List));
         Set_First_Expression_In_List (List, Expr);
      end loop;

      if Pkg_Name /= "" then
         Pkg := Get_Or_Create_Package (Project, Pkg_Name);
      else
         Pkg := Empty_Node;
      end if;

      For_Each_Scenario_Case_Item
        (Project, Pkg, Scenario_Variables, Add_Or_Replace'Unrestricted_Access);
   end Update_Attribute_Value_In_Scenario;

   ----------------------------------------
   -- Move_From_Common_To_Case_Construct --
   ----------------------------------------

   procedure Move_From_Common_To_Case_Construct
     (Project            : Project_Node_Id;
      Pkg_Name           : String;
      Scenario_Variables : Project_Node_Array;
      Attribute_Name     : Types.Name_Id;
      Attribute_Index    : String := "")
   is
      Parent : Project_Node_Id;
      Pkg  : Project_Node_Id := Empty_Node;
      Node, Tmp : Project_Node_Id;
      Case_Items : Project_Node_Array_Access :=
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

      Node := First_Declarative_Item_Of (Parent);
      while Node /= Empty_Node loop
         if Attribute_Matches
           (Current_Item_Node (Node), Attribute_Name, Attribute_Index)
         then
            for Parent in Case_Items'First .. Case_Items_Last loop
               Tmp := Default_Project_Node (N_Declarative_Item);
               Set_Current_Item_Node
                 (Tmp, Clone_Node (Current_Item_Node (Node), True));

               if Kind_Of (Case_Items (Parent)) = N_Case_Item then
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
   end Move_From_Common_To_Case_Construct;

   ----------------------------------------
   -- Update_Attribute_Value_In_Scenario --
   ----------------------------------------

   procedure Update_Attribute_Value_In_Scenario
     (Project            : Project_Node_Id;
      Pkg_Name           : String := "";
      Scenario_Variables : Project_Node_Array;
      Attribute_Name     : String := "";
      Value              : String;
      Attribute_Index    : String := "")
   is
      Attribute_N : Name_Id;
      Val : Project_Node_Id := Empty_Node;
      Pkg : Project_Node_Id;

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
      Name_Len := Attribute_Name'Length;
      Name_Buffer (1 .. Name_Len) := Attribute_Name;
      Attribute_N := Name_Find;

      Move_From_Common_To_Case_Construct
        (Project, Pkg_Name, Scenario_Variables, Attribute_N, Attribute_Index);

      --  Create the node for the new value

      Start_String;
      Store_String_Chars (Value);
      Val := Create_Literal_String (End_String);

      if Pkg_Name /= "" then
         Pkg := Get_Or_Create_Package (Project, Pkg_Name);
      else
         Pkg := Empty_Node;
      end if;

      For_Each_Scenario_Case_Item
        (Project, Pkg, Scenario_Variables, Add_Or_Replace'Unrestricted_Access);
   end Update_Attribute_Value_In_Scenario;

   ----------------------
   -- Delete_Attribute --
   ----------------------

   procedure Delete_Attribute
     (Project            : Project_Node_Id;
      Pkg_Name           : String := "";
      Scenario_Variables : Project_Node_Array;
      Attribute_Name     : String;
      Attribute_Index    : String := "")
   is
      Attribute_N : Name_Id;
      Pkg : Project_Node_Id;

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
      Name_Len := Attribute_Name'Length;
      Name_Buffer (1 .. Name_Len) := Attribute_Name;
      Attribute_N := Name_Find;

      Move_From_Common_To_Case_Construct
        (Project, Pkg_Name, Scenario_Variables, Attribute_N, Attribute_Index);

      if Pkg_Name /= "" then
         Pkg := Get_Or_Create_Package (Project, Pkg_Name);
      else
         Pkg := Empty_Node;
      end if;

      For_Each_Scenario_Case_Item
        (Project, Pkg, Scenario_Variables, Delete_Attr'Unrestricted_Access);
   end Delete_Attribute;

   ---------------------------------
   -- Get_Unit_Part_From_Filename --
   ---------------------------------

   function Get_Unit_Part_From_Filename
     (Filename : String;
      Project  : Prj.Project_Id) return Unit_Part
   is
      Arr : Array_Element_Id;
   begin
      Arr := Projects.Table (Project).Naming.Specification_Suffix;
      while Arr /= No_Array_Element loop
         pragma Assert (Array_Elements.Table (Arr).Value.Kind = Prj.Single);
         if Suffix_Matches
           (Filename, Get_String (Array_Elements.Table (Arr).Value.Value))
         then
            return Unit_Spec;
         end if;
         Arr := Array_Elements.Table (Arr).Next;
      end loop;

      Arr := Projects.Table (Project).Naming.Implementation_Suffix;
      while Arr /= No_Array_Element loop
         pragma Assert (Array_Elements.Table (Arr).Value.Kind = Prj.Single);
         if Suffix_Matches
           (Filename, Get_String (Array_Elements.Table (Arr).Value.Value))
         then
            return Unit_Body;
         end if;
         Arr := Array_Elements.Table (Arr).Next;
      end loop;

      return Unit_Separate;
   end Get_Unit_Part_From_Filename;

   ------------------------
   -- Delete_File_Suffix --
   ------------------------

   function Delete_File_Suffix
     (Filename : String;
      Project  : Prj.Project_Id)
      return Natural
   is
      Arr : Array_Element_Id;
   begin
      Arr := Projects.Table (Project).Naming.Specification_Suffix;
      while Arr /= No_Array_Element loop
         pragma Assert (Array_Elements.Table (Arr).Value.Kind = Prj.Single);
         declare
            Suff : constant String :=
              Get_String (Array_Elements.Table (Arr).Value.Value);
         begin
            if Suffix_Matches (Filename, Suff) then
               return Filename'Last - Suff'Last;
            end if;
         end;
         Arr := Array_Elements.Table (Arr).Next;
      end loop;

      Arr := Projects.Table (Project).Naming.Implementation_Suffix;
      while Arr /= No_Array_Element loop
         pragma Assert (Array_Elements.Table (Arr).Value.Kind = Prj.Single);
         declare
            Suff : constant String :=
              Get_String (Array_Elements.Table (Arr).Value.Value);
         begin
            if Suffix_Matches (Filename, Suff) then
               return Filename'Last - Suff'Last;
            end if;
         end;
         Arr := Array_Elements.Table (Arr).Next;
      end loop;

      return Filename'Last;
   end Delete_File_Suffix;

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

   ---------------------------
   -- Find_Type_Declaration --
   ---------------------------

   function Find_Type_Declaration
     (Project : Project_Node_Id; Name : Types.Name_Id)
      return Project_Node_Id is
   begin
      return Find_Node_By_Name (Project, N_String_Type_Declaration, Name);
   end Find_Type_Declaration;

   ------------------------------
   -- Find_Package_Declaration --
   ------------------------------

   function Find_Package_Declaration
     (Project : Project_Node_Id; Name : Types.Name_Id)
      return Project_Node_Id is
   begin
      return Find_Node_By_Name (Project, N_Package_Declaration, Name);
   end Find_Package_Declaration;

   ----------------------------
   -- Find_Scenario_Variable --
   ----------------------------

   function Find_Scenario_Variable
     (Project : Project_Node_Id; External_Name : String_Id)
      return Project_Node_Id
   is
      Decl : Project_Node_Id := First_Declarative_Item_Of
        (Project_Declaration_Of (Project));
      Current : Project_Node_Id;
   begin
      while Decl /= Empty_Node loop
         Current := Current_Item_Node (Decl);
         if Kind_Of (Current) = N_Typed_Variable_Declaration
           and then Is_External_Variable (Current)
           and then String_Equal
              (External_Name, External_Reference_Of (Current))
         then
            return Current;
         end if;

         Decl := Next_Declarative_Item (Decl);
      end loop;
      return Empty_Node;
   end Find_Scenario_Variable;

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
                  Last_Var := Current_Node;
               else
                  Last_Var := Current_Node;

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

   -------------------------------
   -- Find_Project_In_Hierarchy --
   -------------------------------

   function Find_Project_In_Hierarchy
     (Root_Project : Project_Node_Id; Name : Types.Name_Id)
      return Project_Node_Id
   is
      With_Clause : Project_Node_Id;
      Result : Project_Node_Id := Empty_Node;
   begin
      if Prj.Tree.Name_Of (Root_Project) = Name then
         Result := Root_Project;
      else
         With_Clause := First_With_Clause_Of (Root_Project);
         while With_Clause /= Empty_Node loop
            Result := Find_Project_In_Hierarchy
              (Project_Node_Of (With_Clause), Name);
            exit when Result /= Empty_Node;

            With_Clause := Next_With_Clause_Of (With_Clause);
         end loop;
      end if;

      return Result;
   end Find_Project_In_Hierarchy;

   ------------
   -- Rename --
   ------------

   procedure Rename
     (Root_Project : Project_Node_Id;
      Project      : Project_Node_Id;
      New_Name     : String)
   is
      Old : Project_Node_Id;
      Name : Name_Id;

      procedure Replace_In_Hierarchy (Prj : Project_Node_Id);
      --  Replace all the with statements in the hierarchy that referenced
      --  Project.

      function Substitute (Str : String) return String;
      --  Substitute the old name by the new name in Str.
      --  Old_Name must be found at the end of Str, possibly followed by
      --  a suffix Project_File_Extension.

      ----------------
      -- Substitute --
      ----------------

      function Substitute (Str : String) return String is
         Dir  : constant String := Dir_Name (Str);
      begin
         if Dir /= "" then
            return Dir_Name (Str) & Directory_Separator & New_Name;
         else
            return New_Name;
         end if;
      end Substitute;

      --------------------------
      -- Replace_In_Hierarchy --
      --------------------------

      procedure Replace_In_Hierarchy (Prj : Project_Node_Id) is
         With_Clause : Project_Node_Id := First_With_Clause_Of (Prj);
      begin
         --  No child of Project will have a with clause for Project.
         if Prj = Project then
            return;
         end if;

         while With_Clause /= Empty_Node loop
            if Project_Node_Of (With_Clause) = Project then
               declare
                  String_Value : constant String := Substitute
                    (Get_String (String_Value_Of (With_Clause)));
               begin
                  Set_Name_Of (With_Clause, Name);
                  Set_Path_Name_Of (With_Clause, Path_Name_Of (Project));

                  Start_String;
                  Store_String_Chars (String_Value);
                  Set_String_Value_Of (With_Clause, End_String);
               end;

            else
               Replace_In_Hierarchy (Project_Node_Of (With_Clause));
            end if;

            With_Clause := Next_With_Clause_Of (With_Clause);
         end loop;
      end Replace_In_Hierarchy;

   begin
      Name_Len := New_Name'Length;
      Name_Buffer (1 .. Name_Len) := New_Name;
      Name := Name_Find;

      Old := Find_Project_In_Hierarchy (Root_Project, Name);
      if Old /= Empty_Node then
         Raise_Exception
           (Project_Error'Identity,
            -"There is already a project by this name in the project tree.");
      end if;

      declare
         Path : constant String := Substitute
           (Get_Name_String (Path_Name_Of (Project)));
      begin
         Set_Name_Of (Project, Name);

         Name_Len := Path'Length;
         Name_Buffer (1 .. Name_Len) := Path;
         Set_Path_Name_Of (Project, Name_Find);
      end;

      Replace_In_Hierarchy (Root_Project);
   end Rename;

   ----------------------------------
   -- Add_Scenario_Variable_Values --
   ----------------------------------

   procedure Add_Scenario_Variable_Values
     (Root_Project           : Project_Node_Id;
      External_Variable_Name : String_Id;
      Values                 : String_Id_Array)
   is
      Type_Node, With_Clause, Var : Project_Node_Id;
   begin
      Var := Find_Scenario_Variable (Root_Project, External_Variable_Name);

      --  If variable is defined in the current project, then modify the type
      --  to Values.

      if Var /= Empty_Node then
         Type_Node := String_Type_Of (Var);
         pragma Assert (Type_Node /= Empty_Node);
         --  Set_First_Literal_String (Type_Node, Empty_Node);

         for J in Values'Range loop
            Add_Possible_Value (Type_Node, Values (J));
         end loop;
      end if;

      With_Clause := First_With_Clause_Of (Root_Project);
      while With_Clause /= Empty_Node loop
         Add_Scenario_Variable_Values
           (Project_Node_Of (With_Clause), External_Variable_Name, Values);
         With_Clause := Next_With_Clause_Of (With_Clause);
      end loop;
   end Add_Scenario_Variable_Values;

   -----------------------------------
   -- For_Each_Environment_Variable --
   -----------------------------------

   procedure For_Each_Environment_Variable
     (Root_Project      : Project_Node_Id;
      Ext_Variable_Name : String;
      Specific_Choice   : String;
      Action            : Environment_Variable_Callback)
   is
      Variable_Nodes : Project_Node_Array_Access :=
        new Project_Node_Array (1 .. 100);
      Variable_Nodes_Last : Natural := Variable_Nodes'First - 1;
      --  List of all the variables that reference Ext_Variable_Name
      --  in the current project.

      procedure Process_Expression (Expression : Project_Node_Id);
      --  Delete all references to the variable in Expr

      procedure Recurse_In_Project (Pkg_Or_Case_Item : Project_Node_Id);
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
        (Node             : Project_Node_Id) return Boolean is
      begin
         case Kind_Of (Node) is
            when N_External_Value =>
               return Get_String
                 (String_Value_Of (Prj.Tree.External_Reference_Of (Node))) =
                 Ext_Variable_Name;

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

      procedure Process_Expression (Expression : Project_Node_Id) is
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
                       (First_Expression_In_List (Current_Term (Term)));

                  --  Handles "-g" & external ("A")
                  --  Replace A by the constant string representing its value
                  when N_External_Value =>
                     if Is_Reference_To_Ext (Current_Term (Term)) then
                        Action (Root_Project, Term, Current_Term (Term),
                                Empty_Node);
                     end if;

                  --  Handles "-g" & Var
                  --  Where Var is a reference to the external variable
                  when N_Variable_Reference =>
                     if Is_Reference_To_Ext (Current_Term (Term)) then
                        Action (Root_Project, Term, Current_Term (Term),
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

      procedure Recurse_In_Project (Pkg_Or_Case_Item : Project_Node_Id) is
         Decl, Current, Case_Item, Choice : Project_Node_Id;
         Match : Boolean;
      begin
         if Pkg_Or_Case_Item /= Empty_Node then
            Decl := First_Declarative_Item_Of (Pkg_Or_Case_Item);
         else
            Decl := First_Declarative_Item_Of
              (Project_Declaration_Of (Root_Project));
         end if;

         while Decl /= Empty_Node loop
            Current := Current_Item_Node (Decl);
            case Kind_Of (Current) is
               when N_Typed_Variable_Declaration =>

                  if Is_External_Variable (Current)
                    and then Get_String (External_Reference_Of (Current)) =
                    Ext_Variable_Name
                  then
                     Add_Node_To_List
                       (Variable_Nodes, Variable_Nodes_Last, Current);

                     Action
                       (Root_Project, Empty_Node, String_Type_Of (Current),
                        Empty_Node);
                     Action (Root_Project, Decl, Current, Empty_Node);
                  end if;

                  Process_Expression (Expression_Of (Current));

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
                              if Get_String
                                (String_Value_Of (Choice)) = Specific_Choice
                              then
                                 Match := True;
                                 exit;
                              end if;

                              Choice := Next_Literal_String (Choice);
                           end loop;
                        end if;

                        if Match then
                           Action (Root_Project, Decl, Case_Item, Choice);
                        end if;

                        Recurse_In_Project (Case_Item);
                        Case_Item := Next_Case_Item (Case_Item);
                     end loop;

                  else
                     Case_Item := First_Case_Item_Of (Current);
                     while Case_Item /= Empty_Node loop
                        Recurse_In_Project (Case_Item);
                        Case_Item := Next_Case_Item (Case_Item);
                     end loop;
                  end if;

               when N_Package_Declaration =>
                  Recurse_In_Project (Current);

               when N_Variable_Declaration
                 |  N_Attribute_Declaration =>
                  Process_Expression (Expression_Of (Current));

               when others =>
                  null;
            end case;

            Decl := Next_Declarative_Item (Decl);
         end loop;
      end Recurse_In_Project;

      With_Clause              : Project_Node_Id;
   begin
      Recurse_In_Project (Empty_Node);

      With_Clause := First_With_Clause_Of (Root_Project);
      while With_Clause /= Empty_Node loop
         For_Each_Environment_Variable
           (Project_Node_Of (With_Clause),
            Ext_Variable_Name,
            Specific_Choice,
            Action);
         With_Clause := Next_With_Clause_Of (With_Clause);
      end loop;

      Free (Variable_Nodes);
   end For_Each_Environment_Variable;

   ------------------------------
   -- Rename_External_Variable --
   ------------------------------

   procedure Rename_External_Variable
     (Root_Project : Project_Node_Id;
      Old_Name     : String;
      New_Name     : Types.String_Id)
   is
      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable.

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
      begin
         if Kind_Of (Node) = N_External_Value then
            Set_String_Value_Of (External_Reference_Of (Node), New_Name);
         end if;
      end Callback;

   begin
      For_Each_Environment_Variable
        (Root_Project, Old_Name, "", Callback'Unrestricted_Access);

      --  Create the new variable, to avoid errors when computing the view of
      --  the project.
      Name_Len := Old_Name'Length;
      Name_Buffer (1 .. Name_Len) := Old_Name;
      Add (Get_String (New_Name), Get_String (Value_Of (Name_Find)));
   end Rename_External_Variable;

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

   ------------------------------
   -- Delete_External_Variable --
   ------------------------------

   procedure Delete_External_Variable
     (Root_Project      : Project_Node_Id;
      Ext_Variable_Name : String;
      Keep_Choice       : String;
      Delete_Direct_References : Boolean := True)
   is
      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable.

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
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
               Tree_Private_Part.Project_Nodes.Table (Parent) :=
                 Tree_Private_Part.Project_Nodes.Table
                 (First_Declarative_Item_Of (Node));

            when others =>
               null;
               pragma Assert (False, "Unexpected node type");

         end case;
      end Callback;

   begin
      For_Each_Environment_Variable
        (Root_Project, Ext_Variable_Name, Keep_Choice,
         Callback'Unrestricted_Access);
   end Delete_External_Variable;

   ----------------------------------------
   -- Rename_Value_For_External_Variable --
   ----------------------------------------

   procedure Rename_Value_For_External_Variable
     (Root_Project      : Project_Node_Id;
      Ext_Variable_Name : String;
      Old_Value_Name    : String;
      New_Value_Name    : Types.String_Id)
   is
      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable.

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         C : Project_Node_Id;
      begin
         case Kind_Of (Node) is
            when N_External_Value =>
               if External_Default_Of (Node) /= Empty_Node
                 and then Get_String
                 (String_Value_Of (External_Default_Of (Node))) =
                 Old_Value_Name
               then
                  Set_String_Value_Of
                    (External_Default_Of (Node), New_Value_Name);
               end if;

            when N_String_Type_Declaration =>
               C := First_Literal_String (Node);
               while C /= Empty_Node loop
                  if Get_String (String_Value_Of (C)) = Old_Value_Name then
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

   begin
      For_Each_Environment_Variable
        (Root_Project, Ext_Variable_Name, Old_Value_Name,
         Callback'Unrestricted_Access);

      Name_Len := Ext_Variable_Name'Length;
      Name_Buffer (1 .. Name_Len) := Ext_Variable_Name;

      if Get_String (Value_Of (Name_Find)) = Old_Value_Name then
         Add (Ext_Variable_Name, Get_String (New_Value_Name));
      end if;
   end Rename_Value_For_External_Variable;

   ---------------------------------------------
   -- Set_Default_Value_For_External_Variable --
   ---------------------------------------------

   procedure Set_Default_Value_For_External_Variable
     (Root_Project      : Project_Node_Id;
      Ext_Variable_Name : String;
      Default           : String_Id)
   is
      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable.

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
      begin
         if Kind_Of (Node) = N_Typed_Variable_Declaration then
            Set_External_Default_Of
              (Current_Term (First_Term (Expression_Of (Node))),
               Create_Literal_String (Default));
         end if;
      end Callback;

   begin
      For_Each_Environment_Variable
        (Root_Project, Ext_Variable_Name, "",
         Callback'Unrestricted_Access);
   end Set_Default_Value_For_External_Variable;

   ------------------
   -- Remove_Value --
   ------------------

   procedure Remove_Value
     (Root_Project      : Project_Node_Id;
      Ext_Variable_Name : String;
      Value_Name        : String)
   is
      Delete_Variable : exception;
      Type_Decl : Project_Node_Id := Empty_Node;

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable.

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         C, C2 : Project_Node_Id;
      begin
         case Kind_Of (Node) is
            when N_String_Type_Declaration =>
               Type_Decl := Node;

               C := First_Literal_String (Node);

               if Next_Literal_String (C) = Empty_Node then
                  raise Delete_Variable;
               end if;

               if Get_String (String_Value_Of (C)) = Value_Name then
                  Set_First_Literal_String (Node, Next_Literal_String (C));
                  return;
               end if;

               loop
                  C2 := Next_Literal_String (C);
                  exit when C2 = Empty_Node;

                  if Get_String (String_Value_Of (C2)) = Value_Name then
                     Set_Next_Literal_String (C, Next_Literal_String (C2));
                     exit;
                  end if;
                  C := C2;
               end loop;

            when N_External_Value =>
               if External_Default_Of (Node) /= Empty_Node
                 and then Get_String
                 (String_Value_Of (External_Default_Of (Node))) =
                 Value_Name
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
      For_Each_Environment_Variable
        (Root_Project, Ext_Variable_Name, Value_Name,
         Callback'Unrestricted_Access);

      --  Reset the value of the external variable if needed
      Name_Len := Ext_Variable_Name'Length;
      Name_Buffer (1 .. Name_Len) := Ext_Variable_Name;
      if Get_String (Value_Of (Name_Find)) = Value_Name then
         if Type_Decl /= Empty_Node then
            Add (Ext_Variable_Name,
                 Get_String (String_Value_Of
                             (First_Literal_String (Type_Decl))));
         else
            Add (Ext_Variable_Name, "");
         end if;
      end if;

   exception
      when Delete_Variable =>
         Delete_External_Variable
           (Root_Project,
            Ext_Variable_Name        => Ext_Variable_Name,
            Keep_Choice              => Value_Name,
            Delete_Direct_References => False);
   end Remove_Value;

   ------------------
   -- Project_Name --
   ------------------

   function Project_Name (Project_View : Project_Id) return String is
   begin
      return Get_Name_String (Projects.Table (Project_View).Name);
   end Project_Name;

   ----------------------------
   -- Create_Default_Project --
   ----------------------------

   function Create_Default_Project (Name, Path : String)
     return Project_Node_Id
   is
      Project : Project_Node_Id;
      No_Scenario : constant Project_Node_Array (1 .. 0) :=
        (others => Empty_Node);
      Values : Argument_List (1 .. 1);
   begin
      Project := Create_Project (Name, Path);

      Values := (1 => new String' ("."));
      Update_Attribute_Value_In_Scenario
        (Project,
         Scenario_Variables => No_Scenario,
         Attribute_Name     => "source_dirs",
         Values             => Values);
      Free (Values (1));

      Update_Attribute_Value_In_Scenario
        (Project,
         Scenario_Variables => No_Scenario,
         Attribute_Name     => "object_dir",
         Value              => ".");

      return Project;
   end Create_Default_Project;

   -----------------
   -- Source_Dirs --
   -----------------

   function Source_Dirs (Project : Project_Id) return String_Id_Array is
      Src   : String_List_Id := Projects.Table (Project).Source_Dirs;
      Count : Natural := 0;
   begin
      while Src /= Nil_String loop
         Count := Count + 1;
         Src := String_Elements.Table (Src).Next;
      end loop;

      declare
         Sources : String_Id_Array (1 .. Count);
      begin
         --  Store the directories
         Count := Sources'First;
         Src := Projects.Table (Project).Source_Dirs;
         while Src /= Nil_String loop
            Sources (Count) := String_Elements.Table (Src).Value;
            Count := Count + 1;
            Src := String_Elements.Table (Src).Next;
         end loop;
         return Sources;
      end;
   end Source_Dirs;

   -------------------------------------
   -- Register_Default_Naming_Schemes --
   -------------------------------------

   procedure Register_Default_Naming_Schemes is
      Spec, Impl : Name_Id;
   begin
      --  ??? Each known language should have its own separately registered
      --  ??? subprogram.
      Name_Len := 4;
      Name_Buffer (1 .. Name_Len) := ".ads";
      Spec := Name_Find;
      Name_Buffer (1 .. Name_Len) := ".adb";
      Impl := Name_Find;

      Register_Default_Naming_Scheme
        (Language => Name_Ada,
         Default_Spec_Suffix => Spec,
         Default_Impl_Suffix => Impl);

      Name_Len := 2;
      Name_Buffer (1 .. Name_Len) := ".h";
      Spec := Name_Find;
      Name_Buffer (1 .. Name_Len) := ".c";
      Impl := Name_Find;

      Register_Default_Naming_Scheme
        (Language => Name_C,
         Default_Spec_Suffix => Spec,
         Default_Impl_Suffix => Impl);

      Name_Len := 2;
      Name_Buffer (1 .. Name_Len) := ".h";
      Spec := Name_Find;
      Name_Len := 3;
      Name_Buffer (1 .. Name_Len) := ".cc";
      Impl := Name_Find;

      Register_Default_Naming_Scheme
        (Language => Name_CPP,
         Default_Spec_Suffix => Spec,
         Default_Impl_Suffix => Impl);

   end Register_Default_Naming_Schemes;

begin
   Namet.Initialize;
   Csets.Initialize;
   Snames.Initialize;
   Prj.Initialize;
   Prj.Tree.Initialize;
end Prj_API;
