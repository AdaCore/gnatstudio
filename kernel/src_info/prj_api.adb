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

   function String_As_Expression (Value : String_Id) return Project_Node_Id;
   --  Return an N_Expression node that represents the static string Value.
   --  ??? Could be implemented in terms of Concatenate

   --------------------
   -- Create_Project --
   --------------------

   function Create_Project (Name, Path : String) return Project_Node_Id is
      Project : Project_Node_Id;
   begin
      Project_Nodes.Append (Default_Project_Node (N_Project));
      Project := Project_Nodes.Last;
      pragma Assert (Project /= Empty_Node);

      --  Adding the name of the project
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      Project_Nodes.Table (Project).Name := Name_Enter;

      --  Adding the project path
      Name_Len := Path'Length;
      Name_Buffer (1 .. Name_Len) := Path;
      Project_Nodes.Table (Project).Path_Name := Name_Enter;
      return Project;
   end Create_Project;

   -------------------------------
   -- Get_Or_Create_Declaration --
   -------------------------------

   function Get_Or_Create_Declaration (Project : Project_Node_Id)
      return Project_Node_Id
   is
      Decl : Project_Node_Id := Project_Nodes.Table (Project).Field2;
   begin
      if Decl = Empty_Node then
         Project_Nodes.Append (Default_Project_Node (N_Project_Declaration));
         Decl := Project_Nodes.Last;
         Project_Nodes.Table (Project).Field2 := Decl;
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
      Decl, Decl_Item, Var : Project_Node_Id;
      Previous_Decl : Project_Node_Id := Empty_Node;
      N : Name_Id;
   begin
      --  ??? Should insert the attribute or the variable in the list
      --  Prj_Or_Pkg.Variables if needed

      pragma Assert
        (Project_Nodes.Table (Prj_Or_Pkg).Kind = N_Package_Declaration
         or else Project_Nodes.Table (Prj_Or_Pkg).Kind = N_Project);

      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      N := Name_Find;

      --  First step is to create the declarative item that will contain the
      --  variable. This is dependent on the kind of node for Prj_Or_Pkg

      case Project_Nodes.Table (Prj_Or_Pkg).Kind is
         when N_Project =>
            Decl := Get_Or_Create_Declaration (Prj_Or_Pkg);
            Decl_Item := Project_Nodes.Table (Decl).Field1;

         when N_Package_Declaration =>
            Decl := Empty_Node;
            Decl_Item := Project_Nodes.Table (Prj_Or_Pkg).Field2;

         when others =>
            null;
      end case;

      --  Check if the variable already exists.
      --  If it does, nothing to do, and we just exit.

      while Decl_Item /= Empty_Node loop
         Var := Project_Nodes.Table (Decl_Item).Field1;
         if (Project_Nodes.Table (Var).Kind = N_Variable_Declaration
             or else Project_Nodes.Table (Var).Kind = N_Attribute_Declaration
             or else
             Project_Nodes.Table (Var).Kind = N_Typed_Variable_Declaration)
           and then Project_Nodes.Table (Var).Name = N
         then
            pragma Assert (Project_Nodes.Table (Var).Kind = Item_Kind);
            pragma Assert (Project_Nodes.Table (Var).Expr_Kind = Kind);
            return Var;
         end if;
         Previous_Decl := Decl_Item;
         Decl_Item := Project_Nodes.Table (Decl_Item).Field2;
      end loop;

      --  Otherwise create the declarative item

      Project_Nodes.Append (Default_Project_Node (N_Declarative_Item));
      Decl_Item := Project_Nodes.Last;

      --  Insert it in the appropriate list

      if Previous_Decl /= Empty_Node then
         Project_Nodes.Table (Previous_Decl).Field2 := Decl_Item;

      else
         case Project_Nodes.Table (Prj_Or_Pkg).Kind is
            when N_Project =>
               Project_Nodes.Table (Decl_Item).Field2 :=
                 Project_Nodes.Table (Decl).Field1;
               Project_Nodes.Table (Decl).Field1 := Decl_Item;

            when N_Package_Declaration =>
               Project_Nodes.Table (Decl_Item).Field2 :=
                 Project_Nodes.Table (Prj_Or_Pkg).Field2;
               Project_Nodes.Table (Prj_Or_Pkg).Field2 := Decl_Item;

            when others =>
               null;
         end case;
      end if;

      --  Create the variable

      Project_Nodes.Append (Default_Project_Node (Item_Kind, Kind));
      Var := Project_Nodes.Last;
      Project_Nodes.Table (Decl_Item).Field1 := Var;

      Project_Nodes.Table (Var).Name := N;

      if Array_Index /= No_String then
         Project_Nodes.Table (Var).Value := Array_Index;
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
      Index_Name : String := "";
      Kind : Variable_Kind := List)
      return Project_Node_Id is
   begin
      if Index_Name /= "" then
         Start_String;
         Store_String_Chars (Index_Name);
         return Internal_Get_Or_Create_Attribute
           (Prj_Or_Pkg, Name, End_String, N_Attribute_Declaration, Kind);
      else
         return Internal_Get_Or_Create_Attribute
           (Prj_Or_Pkg, Name, No_String, N_Attribute_Declaration, Kind);
      end if;
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
      pragma Assert
        (Project_Nodes.Table (Typ).Kind = N_String_Type_Declaration);
      V := Internal_Get_Or_Create_Attribute
        (Prj_Or_Pkg, Name, No_String, N_Typed_Variable_Declaration, Single);
      Project_Nodes.Table (V).Field2 := Typ;
      return V;
   end Get_Or_Create_Typed_Variable;

   ------------------------
   -- Add_Possible_Value --
   ------------------------

   procedure Add_Possible_Value (Typ : Project_Node_Id; Choice : String) is
      Str, S2 : Project_Node_Id;
      S   : String_Id;
   begin
      pragma Assert
        (Project_Nodes.Table (Typ).Kind = N_String_Type_Declaration);

      Start_String;
      Store_String_Chars (Choice);
      S := End_String;

      Project_Nodes.Append (Default_Project_Node (N_Literal_String, Single));
      S2 := Project_Nodes.Last;
      Project_Nodes.Table (S2).Value := S;

      Str := Project_Nodes.Table (Typ).Field1;

      if Str = Empty_Node then
         Project_Nodes.Table (Typ).Field1 := S2;

      else
         while Project_Nodes.Table (Str).Field1 /= Empty_Node loop
            Str := Project_Nodes.Table (Str).Field1;
         end loop;
         Project_Nodes.Table (Str).Field1 := S2;
      end if;
   end Add_Possible_Value;

   ---------------------------
   -- Get_Or_Create_Package --
   ---------------------------

   function Get_Or_Create_Package
     (Project : Project_Node_Id; Pkg : String) return Project_Node_Id
   is
      Decl : constant Project_Node_Id := Get_Or_Create_Declaration (Project);
      Decl_Item : Project_Node_Id;
      Pack : Project_Node_Id;
      N : Name_Id;
   begin
      Name_Len := Pkg'Length;
      Name_Buffer (1 .. Name_Len) := Pkg;
      N := Name_Find;

      --  Check if the package already exists
      --  ??? It seems that packages and variables should be stored in
      --  different lists
      Decl_Item := Project_Nodes.Table (Decl).Field1;
      while Decl_Item /= Empty_Node loop
         Pack := Project_Nodes.Table (Decl_Item).Field1;
         if Project_Nodes.Table (Pack).Kind = N_Package_Declaration
           and then Project_Nodes.Table (Pack).Name = N
         then
            return Pack;
         end if;
         Decl_Item := Project_Nodes.Table (Decl_Item).Field2;
      end loop;

      --  Otherwise create the declarative item
      Project_Nodes.Append (Default_Project_Node (N_Declarative_Item));
      Decl_Item := Project_Nodes.Last;
      Project_Nodes.Table (Decl_Item).Field2 :=
        Project_Nodes.Table (Decl).Field1;
      Project_Nodes.Table (Decl).Field1 := Decl_Item;

      --  Create the variable
      Project_Nodes.Append
        (Default_Project_Node (N_Package_Declaration, Undefined));
      Pack := Project_Nodes.Last;
      Project_Nodes.Table (Pack).Field3 := Project_Nodes.Table (Decl).Field1;
      Project_Nodes.Table (Decl_Item).Field1 := Pack;

      Project_Nodes.Table (Pack).Name := N;

      return Pack;
   end Get_Or_Create_Package;

   --------------------
   -- Append_To_List --
   --------------------

   procedure Append_To_List (Var : Project_Node_Id; Value : String) is
   begin
      pragma Assert (Var /= Empty_Node);
      pragma Assert (Project_Nodes.Table (Var).Expr_Kind = Prj.List);

      Start_String;
      Store_String_Chars (Value);
      Concatenate_List
        (Project_Nodes.Table (Var).Field1, String_As_Expression (End_String));
   end Append_To_List;

   --------------------------
   -- String_As_Expression --
   --------------------------

   function String_As_Expression (Value : String_Id) return Project_Node_Id is
      Expr, Term, Str : Project_Node_Id;
   begin
      --  Create the expression if required
      Project_Nodes.Append (Default_Project_Node (N_Expression));
      Expr := Project_Nodes.Last;
      Project_Nodes.Table (Expr).Field2 := Empty_Node; --  No next in the list

      --  Create the term (??? test is kept in case we manage to reuse an
      --  existing unused expression node).
      Term := Project_Nodes.Table (Expr).Field1;
      if Term = Empty_Node then
         Project_Nodes.Append (Default_Project_Node (N_Term));
         Term := Project_Nodes.Last;
         Project_Nodes.Table (Expr).Field1 := Term;
      else
         pragma Assert (Project_Nodes.Table (Term).Kind = N_Term);
         null;
      end if;

      Project_Nodes.Append (Default_Project_Node (N_Literal_String));
      Str := Project_Nodes.Last;
      Project_Nodes.Table (Term).Field1 := Str;
      Project_Nodes.Table (Term).Field2 := Empty_Node;

      Project_Nodes.Table (Str).Value := Value;
      return Expr;
   end String_As_Expression;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Var : Project_Node_Id; Value : String) is
      Str : Project_Node_Id;
   begin
      pragma Assert (Var /= Empty_Node);
      pragma Assert (Project_Nodes.Table (Var).Expr_Kind = Prj.Single);

      --  ??? Should reuse the existing N_Expression for the variable, instead
      --  of creating a new one.

      case Project_Nodes.Table (Var).Kind is
         when N_Typed_Variable_Declaration =>
            pragma Assert (Project_Nodes.Table (Var).Field2 /= Empty_Node);
            Str :=
              Project_Nodes.Table (Project_Nodes.Table (Var).Field2).Field1;

            --  Checl that the value is valid, and reuse the string_id.
            while Str /= Empty_Node loop
               String_To_Name_Buffer (Project_Nodes.Table (Str).Value);
               if Name_Buffer (Name_Buffer'First .. Name_Len) = Value then
                  Set_Expression
                    (Var,
                     String_As_Expression (Project_Nodes.Table (Str).Value));
                  return;
               end if;
               Str := Project_Nodes.Table (Str).Field1;
            end loop;

            raise Invalid_Value;

         when N_Variable_Declaration | N_Attribute_Declaration =>
            Start_String;
            Store_String_Chars (Value);
            Set_Expression (Var, String_As_Expression (End_String));

         when others => null;
            Put_Line ("Set_Value: " & Project_Nodes.Table (Var).Kind'Img);
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
   begin
      pragma Assert (Var /= Empty_Node);
      pragma Assert (Project_Nodes.Table (Var).Expr_Kind = Prj.Single);

      --  Create the expression if required
      Project_Nodes.Append (Default_Project_Node (N_External_Value, Single));
      Ext := Project_Nodes.Last;
      Project_Nodes.Table (Var).Field1 := Ext;

      Start_String;
      Store_String_Chars (External_Name);
      Project_Nodes.Table (Ext).Field1 := String_As_Expression (End_String);

      --  Always set the default value if we have a typed variable, so that
      --  we can check if this is a valid value.
      if Default /= ""
        or else Project_Nodes.Table (Var).Kind = N_Typed_Variable_Declaration
      then
         Set_Value (Var, Default);
      end if;
   end Set_Value_As_External;

   --------------------
   -- Set_Expression --
   --------------------

   procedure Set_Expression
     (Var_Or_Attribute : Project_Node_Id; Expr : Project_Node_Id) is
      E : Project_Node_Id;
   begin
      pragma Assert (Var_Or_Attribute /= Empty_Node);
      E := Project_Nodes.Table (Var_Or_Attribute).Field1;

      if E = Empty_Node then
         Project_Nodes.Table (Var_Or_Attribute).Field1 := Expr;

      else
         case Project_Nodes.Table (E).Kind is
            when N_Expression =>
               Project_Nodes.Table (Var_Or_Attribute).Field1 := Expr;
            when N_External_Value =>
               Project_Nodes.Table (E).Field2 := Expr;
            when others =>
               Put_Line ("Set_Expression: Invalid contents for variable: "
                         & Project_Nodes.Table (E).Kind'Img);
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
      pragma Assert (Var_Or_Attribute /= Empty_Node);

      Ext := Project_Nodes.Table (Var_Or_Attribute).Field1;
      pragma Assert (Ext /= Empty_Node);

      if Project_Nodes.Table (Ext).Kind = N_External_Value then
         Ext := Project_Nodes.Table (Ext).Field1;
         case Project_Nodes.Table (Ext).Kind is
            when N_Expression =>
               Ext := Project_Nodes.Table (Ext).Field1; --  N_Term
               case Project_Nodes.Table (Ext).Kind is
                  when N_Term =>
                     Ext := Project_Nodes.Table (Ext).Field1;
                     if Project_Nodes.Table (Ext).Kind = N_Literal_String then
                        return Project_Nodes.Table (Ext).Value;
                     else
                        raise Program_Error;
                     end if;

                  when N_Literal_String =>
                     return Project_Nodes.Table (Ext).Value;

                  when others =>
                     raise Program_Error;
               end case;

            when N_Literal_String =>
               return Project_Nodes.Table (Ext).Value;

            when others =>
               raise Program_Error;
         end case;

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
      case Project_Nodes.Table (Iter.Current).Kind is
         when N_Literal_String =>
            return (Current => Project_Nodes.Table (Iter.Current).Field1);

         when N_Expression =>
            return (Current => Project_Nodes.Table (Iter.Current).Field2);

         when others =>
            Put_Line ("Next: " & Project_Nodes.Table (Iter.Current).Kind'Img);
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
      pragma Assert (Iter.Current /= Empty_Node);
      pragma Assert
        (Project_Nodes.Table (Iter.Current).Kind = N_Literal_String);
      return Project_Nodes.Table (Iter.Current).Value;
   end Data;

   -----------------
   -- Type_Values --
   -----------------

   function Type_Values (Var_Or_Type : Project_Node_Id)
      return String_List_Iterator
   is
      Typ : Project_Node_Id;
   begin
      pragma Assert (Var_Or_Type /= Empty_Node);
      case Project_Nodes.Table (Var_Or_Type).Kind is
         when N_Typed_Variable_Declaration =>
            Typ := Project_Nodes.Table (Var_Or_Type).Field2;
            return (Current => Project_Nodes.Table (Typ).Field1);

         when N_String_Type_Declaration =>
            return (Current => Project_Nodes.Table (Var_Or_Type).Field1);

         when others =>
            raise Program_Error;
      end case;
   end Type_Values;

   --------------
   -- Value_Of --
   --------------

   function Value_Of (Var : Project_Node_Id) return String_List_Iterator is
      V, Expr : Project_Node_Id;
   begin
      pragma Assert (Var /= Empty_Node);
      pragma Assert
        (Project_Nodes.Table (Var).Kind = N_Typed_Variable_Declaration
         or else Project_Nodes.Table (Var).Kind = N_Variable_Declaration);

      V := Project_Nodes.Table (Var).Field1;

      case Project_Nodes.Table (V).Kind is
         when N_Expression =>
            Expr := Project_Nodes.Table (V).Field1;
            pragma Assert
              (Expr /= Empty_Node
               and then Project_Nodes.Table (Expr).Kind = N_Term);
            Expr := Project_Nodes.Table (Expr).Field1;

            if Expr /= Empty_Node
              and then Project_Nodes.Table (Expr).Kind = N_Literal_String_List
            then
               return (Current => Project_Nodes.Table (Expr).Field1);
            else
               return (Current => V);
            end if;

         when N_External_Value =>
            return (Current => Project_Nodes.Table (V).Field2);

         when others =>
            Put_Line ("Value_Of: " & Project_Nodes.Table (V).Kind'Img);
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
                     or else Project_Nodes.Table (Expr).Kind = N_Expression);
      pragma Assert (Node /= Empty_Node);
      pragma Assert (Project_Nodes.Table (Node).Kind /= N_Expression);

      --  Create the expression if needed

      if Expr = Empty_Node then
         Project_Nodes.Append (Default_Project_Node (N_Expression, Single));
         Expr := Project_Nodes.Last;
      end if;

      --  Create the first term if needed

      Term := Project_Nodes.Table (Expr).Field1;
      pragma Assert (Term = Empty_Node
                     or else Project_Nodes.Table (Term).Kind = N_Term);
      if Term = Empty_Node then
         if Project_Nodes.Table (Node).Kind = N_Term then
            Project_Nodes.Table (Node).Field2 := Empty_Node;
            Term := Node;
         else
            Project_Nodes.Append (Default_Project_Node (N_Term, Single));
            Term := Project_Nodes.Last;
            Project_Nodes.Table (Term).Field1 := Node;
         end if;
         Project_Nodes.Table (Expr).Field1 := Term;

      else
         --  We append at the end

         while Project_Nodes.Table (Term).Field2 /= Empty_Node loop
            Term := Project_Nodes.Table (Term).Field2;
         end loop;

         if Project_Nodes.Table (Node).Kind = N_Term then
            Project_Nodes.Table (Node).Field2 := Empty_Node;
            Project_Nodes.Table (Term).Field2 := Node;
            Term := Node;
         else
            Project_Nodes.Append (Default_Project_Node (N_Term, Single));
            Project_Nodes.Table (Term).Field2 := Project_Nodes.Last;
            Term := Project_Nodes.Last;
            Project_Nodes.Table (Term).Field1 := Node;
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
      pragma Assert (Expr = Empty_Node
                     or else Project_Nodes.Table (Expr).Kind = N_Expression);
      pragma Assert (Expr2 /= Empty_Node);
      pragma Assert (Project_Nodes.Table (Expr2).Kind = N_Expression);

      if Expr = Empty_Node then
         Project_Nodes.Append (Default_Project_Node (N_Expression, List));
         Expr := Project_Nodes.Last;
         Project_Nodes.Append (Default_Project_Node (N_Term, List));
         Term := Project_Nodes.Last;
         Project_Nodes.Table (Expr).Field1 := Term;
         Project_Nodes.Append
           (Default_Project_Node (N_Literal_String_List, List));
         L := Project_Nodes.Last;
         Project_Nodes.Table (Term).Field1 := L;
         Project_Nodes.Table (L).Field1 := Expr2;
      else
         Term := Project_Nodes.Table (Expr).Field1;
         pragma Assert (Term /= Empty_Node
                        and then Project_Nodes.Table (Term).Kind = N_Term);
         L := Project_Nodes.Table (Term).Field1;
         pragma Assert
           (L /= Empty_Node
            and then Project_Nodes.Table (L).Kind = N_Literal_String_List);

         E := Project_Nodes.Table (L).Field1;
         pragma Assert (Project_Nodes.Table (E).Kind = N_Expression);

         while Project_Nodes.Table (E).Field2 /= Empty_Node loop
            E := Project_Nodes.Table (E).Field2;
         end loop;
         Project_Nodes.Table (E).Field2 := Expr2;
      end if;
   end Concatenate_List;


begin
   Namet.Initialize;
   Csets.Initialize;
   Snames.Initialize;
   Prj.Initialize;
   Project_Nodes.Set_Last (Empty_Node);
end Prj_API;
