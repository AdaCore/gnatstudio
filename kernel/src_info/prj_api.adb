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
with Prj;       use Prj;
pragma Elaborate_All (Prj);
with Prj.Tree;  use Prj.Tree;
with Prj.Attr;  use Prj.Attr;
with Prj.Util;  use Prj.Util;
with Snames;    use Snames;
pragma Elaborate_All (Snames);
with Namet;     use Namet;
pragma Elaborate_All (Namet);
with Types;     use Types;
with Csets;     use Csets;
pragma Elaborate_All (Csets);
with Stringt;   use Stringt;
with Ada.Text_IO; use Ada.Text_IO;

package body Prj_API is

   function Internal_Get_Or_Create_Attribute
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Array_Index : String_Id := No_String;
      Item_Kind : Project_Node_Kind := N_Attribute_Declaration;
      Kind : Variable_Kind := List)
      return Project_Node_Id;
   --  Internal version for Get_Or_Create_Attribute. If Array_Index is not
   --  No_String, then the variable is defined for a specific index.

   procedure Set_Expression
     (Var_Or_Attribute : Project_Node_Id; Expr : Project_Node_Id);
   --  Set Var as the expression to use for the value of Var. This
   --  properly handles standard variables and variables defined through
   --  references to external environment variables.

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
      Kind : Variable_Kind := List)
      return Project_Node_Id
   is
      Var : Project_Node_Id;
   begin
      pragma Assert
        (Kind_Of (Prj_Or_Pkg) = N_Package_Declaration
         or else Kind_Of (Prj_Or_Pkg) = N_Project
         or else Kind_Of (Prj_Or_Pkg) = N_Case_Item);

      --  Create the variable

      Var := Default_Project_Node (Item_Kind, Kind);
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      Set_Name_Of (Var, Name_Find);
      if Item_Kind = N_Attribute_Declaration then
         Set_Associative_Array_Index_Of (Var, Array_Index);
      end if;

      --  First step is to create the declarative item that will contain the
      --  variable. This is dependent on the kind of node for Prj_Or_Pkg

      if Kind_Of (Prj_Or_Pkg) = N_Project then
         Add_At_End (Get_Or_Create_Declaration (Prj_Or_Pkg), Var);
      else
         Add_At_End (Prj_Or_Pkg, Var);
      end if;

      --  Insert the attribute or the variable in the list Prj_Or_Pkg.Variables
      --  if needed
      --  If the variable is already there, do not add it to the list. But
      --  then, if it already existed we have exited this function already.

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
      Typ  : Project_Node_Id)
      return Project_Node_Id
   is
      V : Project_Node_Id;
   begin
      --  ??? Shouldn't be needed, Set_String_Type_Of should do it.
      pragma Assert (Kind_Of (Typ) = N_String_Type_Declaration);
      V := Internal_Get_Or_Create_Attribute
        (Prj_Or_Pkg, Name, No_String, N_Typed_Variable_Declaration, Single);
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

   ----------------------
   -- Create_Case_Item --
   ----------------------

   procedure Create_Case_Item (In_Case : Project_Node_Id; Value : String_Id) is
      Item, Choice : Project_Node_Id;
   begin
      pragma Assert (Kind_Of (In_Case) = N_Case_Construction);

      Item := Default_Project_Node (N_Case_Item);
      Set_Next_Case_Item (Item, First_Case_Item_Of (In_Case));
      Set_First_Case_Item_Of (In_Case, Item);

      Choice := Default_Project_Node (N_Literal_String);
      Set_String_Value_Of (Choice, Value);
      Set_First_Choice_Of (Item, Choice);
   end Create_Case_Item;

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
      Decl, Item, Pkg : Project_Node_Id;
   begin
      if Project_Node_Of (Ref) /= Empty_Node then
         Project := Project_Node_Of (Ref);
      end if;

      if Package_Node_Of (Ref) /= Empty_Node then
         Pkg := First_Package_Of (Project);
         while Pkg /= Package_Node_Of (Ref) loop
            Pkg := Next_Package_In_Project (Pkg);
         end loop;
         Decl := First_Declarative_Item_Of (Pkg);
      else
         Decl := First_Declarative_Item_Of (Project_Declaration_Of (Project));
      end if;

      while Decl /= Empty_Node loop
         Item := Current_Item_Node (Decl);
         if (Kind_Of (Item) = N_Variable_Declaration
             or else Kind_Of (Item) = N_Typed_Variable_Declaration)
           and then Prj.Tree.Name_Of (Item) = N
         then
            return External_Reference_Of (Item);
         end if;

         Decl := Next_Declarative_Item (Decl);
      end loop;

      return No_String;
   end External_Variable_Name;

   ----------------
   -- Add_At_End --
   ----------------

   procedure Add_At_End
     (Parent : Project_Node_Id; Expr : Project_Node_Id)
   is
      New_Decl, Decl : Project_Node_Id;
   begin
      New_Decl := Default_Project_Node (N_Declarative_Item);
      Set_Current_Item_Node (New_Decl, Expr);

      Decl := First_Declarative_Item_Of (Parent);

      if Decl = Empty_Node then
         Set_First_Declarative_Item_Of (Parent, New_Decl);
      else
         while Next_Declarative_Item (Decl) /= Empty_Node loop
            Decl := Next_Declarative_Item (Decl);
         end loop;

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

begin
   Namet.Initialize;
   Csets.Initialize;
   Snames.Initialize;
   Prj.Initialize;
   Prj.Tree.Initialize;
end Prj_API;
