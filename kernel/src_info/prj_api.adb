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

--  Gnat sources dependencies
with Prj;           use Prj;
pragma Elaborate_All (Prj);
with Prj.Tree;      use Prj.Tree;
with Prj.Attr;      use Prj.Attr;
with Prj.Util;      use Prj.Util;
with Prj_Normalize; use Prj_Normalize;
with Snames;        use Snames;
pragma Elaborate_All (Snames);
with Namet;         use Namet;
pragma Elaborate_All (Namet);
with Types;         use Types;
with Csets;         use Csets;
pragma Elaborate_All (Csets);
with Stringt;       use Stringt;
with Ada.Text_IO;   use Ada.Text_IO;
with GNAT.OS_Lib;   use GNAT.OS_Lib;
with String_Utils;  use String_Utils;

with Traces; use Traces;

package body Prj_API is

   Me : Debug_Handle := Create ("Prj_API");

   function Internal_Get_Or_Create_Attribute
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Array_Index : String_Id := No_String;
      Item_Kind : Project_Node_Kind := N_Attribute_Declaration;
      Kind : Variable_Kind := List;
      Add_Before_First_Case_Or_Pkg : Boolean := False)
      return Project_Node_Id;
   --  Internal version for Get_Or_Create_Attribute. If Array_Index is not
   --  No_String, then the variable is defined for a specific index.

   procedure Set_Expression
     (Var_Or_Attribute : Project_Node_Id; Expr : Project_Node_Id);
   --  Set Var as the expression to use for the value of Var. This
   --  properly handles standard variables and variables defined through
   --  references to external environment variables.

   function Find_Last_Declaration_Of
     (Parent     : Project_Node_Id;
      Attr_Name  : Types.Name_Id;
      Attr_Index : Types.String_Id := Types.No_String) return Project_Node_Id;
   --  Find the last declaration for the attribute Attr_Name, in the
   --  declarative list contained in Parent.
   --  The returned value is the expression_of of the last such declaration, or
   --  Empty_Node if there was none.

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
   begin
      --  Adding the name of the project
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      Set_Name_Of (Project, Name_Enter);

      --  Adding the project path
      Name_Len := Path'Length;
      Name_Buffer (1 .. Name_Len) := Path;
      Set_Path_Name_Of (Project, Name_Enter);
      return Project;
   end Create_Project;

   -------------------------------
   -- Get_Or_Create_Declaration --
   -------------------------------

   function Get_Or_Create_Declaration (Project : Project_Node_Id)
      return Project_Node_Id
   is
      Decl : Project_Node_Id := Project_Declaration_Of (Project);
   begin
      if Decl = Empty_Node then
         Decl := Default_Project_Node (N_Project_Declaration);
         Set_Project_Declaration_Of (Project, Decl);
      end if;
      return Decl;
   end Get_Or_Create_Declaration;

   --------------------------------------
   -- Internal_Get_Or_Create_Attribute --
   --------------------------------------

   function Internal_Get_Or_Create_Attribute
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Array_Index : String_Id := No_String;
      Item_Kind : Project_Node_Kind := N_Attribute_Declaration;
      Kind : Variable_Kind := List;
      Add_Before_First_Case_Or_Pkg : Boolean := False)
      return Project_Node_Id
   is
      Var, Decl, Item : Project_Node_Id;
   begin
      pragma Assert
        (Kind_Of (Prj_Or_Pkg) = N_Package_Declaration
         or else Kind_Of (Prj_Or_Pkg) = N_Project
         or else Kind_Of (Prj_Or_Pkg) = N_Case_Item);

      --  Create the variable

      Var      := Default_Project_Node (Item_Kind, Kind);
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      Set_Name_Of (Var, Name_Find);
      if Item_Kind = N_Attribute_Declaration then
         Set_Associative_Array_Index_Of (Var, Array_Index);
      end if;

      --  Create a declarative item around the variable
      Item := Default_Project_Node (N_Declarative_Item);
      Set_Current_Item_Node (Item, Var);

      --  First step is to create the declarative item that will contain the
      --  variable. This is dependent on the kind of node for Prj_Or_Pkg

      if Kind_Of (Prj_Or_Pkg) = N_Project then
         Decl := Get_Or_Create_Declaration (Prj_Or_Pkg);
      else
         Decl := Prj_Or_Pkg;
      end if;

      Add_At_End (Decl, Item, Add_Before_First_Case_Or_Pkg);

      --  Insert the attribute or the variable in the list Prj_Or_Pkg.Variables
      --  if needed. This might mean that the variable is inserted several
      --  times. However, since we inserted in front, the first item in the
      --  list will always contain a reference to the *first* declaration of
      --  the variable.

      if Item_Kind = N_Typed_Variable_Declaration
        or else Item_Kind = N_Variable_Declaration
      then
         Set_Next_Variable (Var, First_Variable_Of (Prj_Or_Pkg));
         Set_First_Variable_Of (Prj_Or_Pkg, Var);
      end if;

      return Var;
   end Internal_Get_Or_Create_Attribute;

   ----------------------------
   -- Get_Or_Create_Variable --
   ----------------------------

   function Get_Or_Create_Variable
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Kind : Variable_Kind := List)
      return Project_Node_Id is
   begin
      return Internal_Get_Or_Create_Attribute
        (Prj_Or_Pkg, Name, No_String, N_Variable_Declaration, Kind);
   end Get_Or_Create_Variable;

   -----------------------------
   -- Get_Or_Create_Attribute --
   -----------------------------

   function Get_Or_Create_Attribute
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Index_Name : String_Id := No_String;
      Kind : Variable_Kind := List)
      return Project_Node_Id is
   begin
      return Internal_Get_Or_Create_Attribute
        (Prj_Or_Pkg, Name, Index_Name, N_Attribute_Declaration, Kind);
   end Get_Or_Create_Attribute;

   ------------------------
   -- Get_Or_Create_Type --
   ------------------------

   function Get_Or_Create_Type
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String)
      return Project_Node_Id is
   begin
      return Internal_Get_Or_Create_Attribute
        (Prj_Or_Pkg, Name, No_String, N_String_Type_Declaration, Undefined);
   end Get_Or_Create_Type;

   ----------------------------------
   -- Get_Or_Create_Typed_Variable --
   ----------------------------------

   function Get_Or_Create_Typed_Variable
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Typ  : Project_Node_Id;
      Add_Before_First_Case_Or_Pkg : Boolean := False)
      return Project_Node_Id
   is
      V : Project_Node_Id;
   begin
      pragma Assert (Kind_Of (Typ) = N_String_Type_Declaration);
      V := Internal_Get_Or_Create_Attribute
        (Prj_Or_Pkg, Name, No_String, N_Typed_Variable_Declaration, Single,
         Add_Before_First_Case_Or_Pkg);
      Set_String_Type_Of (V, Typ);
      return V;
   end Get_Or_Create_Typed_Variable;

   ------------------------
   -- Add_Possible_Value --
   ------------------------

   function Add_Possible_Value (Typ : Project_Node_Id; Choice : String)
      return String_Id
   is
      Str, S2 : Project_Node_Id;
      S   : String_Id;
   begin
      pragma Assert (Kind_Of (Typ) = N_String_Type_Declaration);

      Start_String;
      Store_String_Chars (Choice);
      S := End_String;

      S2 := Default_Project_Node (N_Literal_String, Single);
      Set_String_Value_Of (S2, S);

      Str := First_Literal_String (Typ);

      if Str = Empty_Node then
         Set_First_Literal_String (Typ, S2);

      else
         while Next_Literal_String (Str) /= Empty_Node loop
            Str := Next_Literal_String (Str);
         end loop;
         Set_Next_Literal_String (Str, S2);
      end if;
      return S;
   end Add_Possible_Value;

   ---------------------------
   -- Get_Or_Create_Package --
   ---------------------------

   function Get_Or_Create_Package
     (Project : Project_Node_Id; Pkg : String) return Project_Node_Id
   is
      Decl : constant Project_Node_Id := Get_Or_Create_Declaration (Project);
      Decl_Item, Item : Project_Node_Id;
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

      --  Otherwise create the declarative item, and put it at the end. We can
      --  not put it at the beginning, since otherwise this would skip the
      --  section were scenario variables are declared, and they might be
      --  referenced in the new package.

      Decl_Item := Default_Project_Node (N_Declarative_Item);
      Item := First_Declarative_Item_Of (Decl);
      if Item = Empty_Node then
         Set_First_Declarative_Item_Of (Decl, Decl_Item);
      else
         while Next_Declarative_Item (Item) /= Empty_Node loop
            Item := Next_Declarative_Item (Item);
         end loop;

         Set_Next_Declarative_Item (Item, Decl_Item);
      end if;

      --  Create the package and add it to the declarative item
      Pack := Default_Project_Node (N_Package_Declaration);
      Set_Current_Item_Node (Decl_Item, Pack);

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

      Set_Name_Of (Pack, N);

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
      Str : Project_Node_Id := Default_Project_Node (N_Literal_String);
   begin
      Set_String_Value_Of (Str, Value);
      return Enclose_In_Expression (Str);
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

         when others => null;
            Put_Line ("Set_Value: " & Kind_Of (Var)'Img);
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
      Str := Default_Project_Node (N_Literal_String, Single);
      Set_String_Value_Of (Str, End_String);

      Set_External_Reference_Of (Ext, Str);

      if Default /= "" then
         Start_String;
         Store_String_Chars (Default);
         Str := Default_Project_Node (N_Literal_String, Single);
         Set_String_Value_Of (Str, End_String);
         Set_External_Default_Of (Ext, Str);
      end if;
   end Set_Value_As_External;

   --------------------
   -- Set_Expression --
   --------------------

   --  ??? Should be renamed to avoid confusion with the new Prj.Tree function
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
               Put_Line ("Set_Expression: Invalid contents for variable: "
                         & Kind_Of (E)'Img);
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
            Put_Line ("Next: " & Kind_Of (Iter.Current)'Img);
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
            Put_Line ("Value_Of: " & Kind_Of (V)'Img);
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
      File             : Name_Id;
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

      Name_Len := 8;
      Name_Buffer (1 .. Name_Len) := "switches";
      Switches := Name_Find;

      Pkg  := Prj.Util.Value_Of
        (Name        => Pkg_Name,
         In_Packages => Projects.Table (Project).Decl.Packages);

      --  Do we have some file-specific switches ?
      if Pkg /= No_Package then
         The_Array := Value_Of
             (Name      => Switches,
              In_Arrays => Packages.Table (Pkg).Decl.Arrays);
         Var_Value := Value_Of
             (Index    => File,
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
         Var_Value := Prj.Util.Value_Of
           (Variable_Name => Switches,
            In_Variables  => Packages.Table (Pkg).Decl.Attributes);

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
      pragma Assert
        (Kind_Of (Var) = N_Typed_Variable_Declaration
         or else Kind_Of (Var) = N_Variable_Declaration);

      Ref := Default_Project_Node (N_Variable_Reference);
      Set_Name_Of (Ref, Prj.Tree.Name_Of (Var));
      Set_Expression_Kind_Of (Ref, Expression_Kind_Of (Var));

      if Kind_Of (Var) = N_Typed_Variable_Declaration then
         Set_String_Type_Of (Ref, String_Type_Of (Var));
      end if;
      return Ref;
   end Create_Variable_Reference;

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
     (Project : Project_Node_Id; Imported_Project : Project_Node_Id)
   is
      With_Clause : Project_Node_Id := First_With_Clause_Of (Project);
   begin
      --  Check if it is already there.
      while With_Clause /= Empty_Node loop
         if Project_Node_Of (With_Clause) = Imported_Project then
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
      Store_String_Chars
        (Get_Name_String (Prj.Tree.Name_Of (Imported_Project)));
      Set_String_Value_Of (With_Clause, End_String);
   end Add_Imported_Project;

   ----------------------------
   -- External_Variable_Name --
   ----------------------------

   function External_Variable_Name
     (Current_Project : Project_Node_Id; Ref : Project_Node_Id)
      return String_Id
   is
      N : constant Name_Id := Prj.Tree.Name_Of (Ref);
      Project : Project_Node_Id := Current_Project;
      Pkg : Project_Node_Id;
      Variable : Variable_Node_Id;

   begin
      if Project_Node_Of (Ref) /= Empty_Node then
         Project := Project_Node_Of (Ref);
      end if;

      Pkg := Project;

      --  ??? The commented out code searches for the variable declaration in
      --  all the packages. However, the normalization process should really
      --  put it at the top-level of the project.

      --      while Pkg /= Empty_Node loop
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

      --     if Pkg = Project then
      --        Pkg := First_Package_Of (Project);
      --     else
      --        Pkg := Next_Package_In_Project (Pkg);
      --     end if;
      --  end loop;

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
      New_Decl, Decl, Next : Project_Node_Id;
   begin
      if Kind_Of (Expr) /= N_Declarative_Item then
         New_Decl := Default_Project_Node (N_Declarative_Item);
         Set_Current_Item_Node (New_Decl, Expr);
      else
         New_Decl := Expr;
      end if;

      Decl := First_Declarative_Item_Of (Parent);

      if Decl = Empty_Node then
         Set_First_Declarative_Item_Of (Parent, New_Decl);
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

   ------------------------------
   -- Find_Last_Declaration_Of --
   ------------------------------

   function Find_Last_Declaration_Of
     (Parent  : Project_Node_Id;
      Attr_Name  : Name_Id;
      Attr_Index : String_Id := No_String) return Project_Node_Id
   is
      Decl, Expr : Project_Node_Id;
   begin
      Decl := First_Declarative_Item_Of (Parent);
      while Decl /= Empty_Node loop
         Expr := Current_Item_Node (Decl);

         if Kind_Of (Expr) = N_Attribute_Declaration
           and then Prj.Tree.Name_Of (Expr) = Attr_Name
         then
            if (Attr_Index = No_String
                and then Associative_Array_Index_Of (Expr) = No_String)
              or else (Attr_Index /= No_String
                       and then Associative_Array_Index_Of (Expr) /= No_String
                       and then
                       String_Equal (Associative_Array_Index_Of (Expr),
                                     Attr_Index))
            then
               return Expression_Of (Expr);
            end if;
         end if;

         Decl := Next_Declarative_Item (Decl);
      end loop;
      return Empty_Node;
   end Find_Last_Declaration_Of;

   ----------------------------------------
   -- Update_Attribute_Value_In_Scenario --
   ----------------------------------------

   procedure Update_Attribute_Value_In_Scenario
     (Project         : Project_Node_Id;
      Pkg_Name        : String := "";
      Attribute_Name  : String := "";
      Values          : GNAT.OS_Lib.Argument_List;
      Attribute_Index : Types.String_Id := Types.No_String;
      Prepend         : Boolean := False)
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
            if Prepend then
               Expr := First_Expression_In_List (List);
               while Next_Expression_In_List (Expr) /= Empty_Node loop
                  Expr := Next_Expression_In_List (Expr);
               end loop;

               Set_Next_Expression_In_List
                 (Expr, First_Expression_In_List
                  (Current_Term (First_Term (Previous_Decl))));
            end if;

            Set_Current_Term (First_Term (Previous_Decl), List);

         --  Else create the new instruction to be added to the project

         else
            Decl := Get_Or_Create_Attribute
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
      if Values'Length = 0 then
         --  ??? Shouldn't exist if there is already such a package => remove
         --  ??? the previous declaration.
         return;
      end if;

      Name_Len := Attribute_Name'Length;
      Name_Buffer (1 .. Name_Len) := Attribute_Name;
      Attribute_N := Name_Find;

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
        (Project, Pkg, Add_Or_Replace'Unrestricted_Access);
   end Update_Attribute_Value_In_Scenario;

   ----------------------------------------
   -- Update_Attribute_Value_In_Scenario --
   ----------------------------------------

   procedure Update_Attribute_Value_In_Scenario
     (Project         : Project_Node_Id;
      Pkg_Name        : String := "";
      Attribute_Name  : String := "";
      Value           : String;
      Attribute_Index : Types.String_Id := Types.No_String)
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
            Set_Current_Term (First_Term (Previous_Decl), Val);

         --  Else create the new instruction to be added to the project

         else
            Decl := Get_Or_Create_Attribute
              (Case_Item, Attribute_Name, Attribute_Index, Prj.Single);
            Set_Expression_Of (Decl, Enclose_In_Expression (Val));
         end if;
      end Add_Or_Replace;

   begin
      Name_Len := Attribute_Name'Length;
      Name_Buffer (1 .. Name_Len) := Attribute_Name;
      Attribute_N := Name_Find;

      --  Create the node for the new value

      Val := Default_Project_Node (N_Literal_String, Prj.Single);
      Start_String;
      Store_String_Chars (Value);
      Set_String_Value_Of (Val, End_String);

      if Pkg_Name /= "" then
         Pkg := Get_Or_Create_Package (Project, Pkg_Name);
      else
         Pkg := Empty_Node;
      end if;

      For_Each_Scenario_Case_Item
        (Project, Pkg, Add_Or_Replace'Unrestricted_Access);
   end Update_Attribute_Value_In_Scenario;

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
            Trace (Me, "Unit part for " & Filename & " is Unit_Spec");
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
            Trace (Me, "Unit part for " & Filename & " is Unit_Body");
            return Unit_Body;
         end if;
         Arr := Array_Elements.Table (Arr).Next;
      end loop;

            Trace (Me, "Unit part for " & Filename & " is unknown");
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

begin
   Namet.Initialize;
   Csets.Initialize;
   Snames.Initialize;
   Prj.Initialize;
   Prj.Tree.Initialize;
end Prj_API;
