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

with Text_IO; use Text_IO;
with Prj_API; use Prj_API;

with Prj;      use Prj;
with Prj.Ext;  use Prj.Ext;
with Prj.Tree; use Prj.Tree;
with Types;    use Types;
with Namet;    use Namet;
with Stringt;  use Stringt;

package body Prj_Normalize is

   type Literal_String_Array is array (Positive range <>) of String_Id;

   type Matching_Item_Callback is access
     function (Item : Project_Node_Id) return Boolean;
   --  A callback function called for each case item that matches a specific
   --  set of values (see For_Each_Matching_Item).
   --  If the callback returns False, we stop searching for other matches

   procedure Clone_Node (From : Project_Node_Id; To : Project_Node_Id);
   --  Make an exact copy of From to To. This overwrites all the fields in To.

   function Clone_Project (Project : Project_Node_Id) return Project_Node_Id;
   --  Return a duplicate of Project and its declarations. We do not duplicate
   --  declarative items.
   --  The new project is not independent of the old one, since most of the
   --  nodes are shared between the two for efficiency reasons.

   function Create_Nested_Case (Scenario_Vars : Project_Node_Array)
      return Project_Node_Id;
   --  Return one case statement (with nested case statements) that forms
   --  the skeleton for the normalized project.
   --      case Var1 is
   --         when Val1_1 =>
   --            case Var2 is
   --               when Val2_1 => null;
   --               when Val2_2 => null;
   --            end case;
   --         when Val1_2 =>
   --            case Var2 is
   --             .......

   procedure Add_To_Case_Item
     (In_Case : Project_Node_Id;
      Var_Values : Literal_String_Array;
      Expression : Project_Node_Id);
   --  Add Expression to the list of instructions in the appropriate
   --  case_item(s) in In_Case, given the current values for all variables as
   --  given in Var_values.

   procedure Add_At_End
     (Expr : Project_Node_Id;
      Parent, Decl_Item : in out Project_Node_Id);
   --  Add expression at the end of the declarative_item list in Parent.
   --  Decl_Item must point to the last declarative_item in the list.

   procedure For_Each_Matching_Item
     (In_Case    : Project_Node_Id;
      Var_Values : Literal_String_Array;
      Action     : Matching_Item_Callback);
   --  Iterates other all the possible nested case items in In_Case, and for
   --  each that matches Var_Values, calls Action.
   --  A No_String value in Var_Values matches any possible value for the
   --  variable

   ----------------
   -- Add_At_End --
   ----------------

   procedure Add_At_End
     (Expr : Project_Node_Id;
      Parent, Decl_Item : in out Project_Node_Id) is
   begin
      if Decl_Item = Empty_Node then
         Decl_Item := Default_Project_Node (N_Declarative_Item);
         Set_First_Declarative_Item_Of (Parent, Decl_Item);
      else
         Set_Next_Declarative_Item
           (Decl_Item, Default_Project_Node (N_Declarative_Item));
         Decl_Item := Next_Declarative_Item (Decl_Item);
      end if;

      Set_Current_Item_Node (Decl_Item, Expr);
   end Add_At_End;

   ----------------
   -- Clone_Node --
   ----------------

   procedure Clone_Node (From : Project_Node_Id; To : Project_Node_Id)
   is
   begin
      pragma Assert (From /= Empty_Node and then To /= Empty_Node);
      Tree_Private_Part.Project_Nodes.Table (To) :=
        Tree_Private_Part.Project_Nodes.Table (From);
   end Clone_Node;

   -------------------
   -- Clone_Project --
   -------------------

   function Clone_Project (Project : Project_Node_Id) return Project_Node_Id is
      Project2 : Project_Node_Id := Default_Project_Node (N_Project);
      Decl : Project_Node_Id := Default_Project_Node (N_Project_Declaration);
   begin
      Clone_Node (From => Project, To => Project2);

      --  Redefine the declaration
      Set_Project_Declaration_Of (Project2, Decl);
      Clone_Node (From => Project_Declaration_Of (Project), To => Decl);

      Set_First_Declarative_Item_Of (Decl, Empty_Node);
      return Project2;
   end Clone_Project;

   ------------------------
   -- Create_Nested_Case --
   ------------------------

   function Create_Nested_Case (Scenario_Vars : Project_Node_Array)
      return Project_Node_Id
   is
      procedure Add_Nested (To_Case : Project_Node_Id; Ref : Project_Node_Id);
      --  Add a nested case statement to all branches of the current big case.

      procedure Add_Case_Items
        (To_Case : Project_Node_Id; Var : Project_Node_Id);
      --  Add all the case items for all the possible values of Var to To_Case.
      --  This also sets the name of the referenced variable in To_Case

      --------------------
      -- Add_Case_Items --
      --------------------

      procedure Add_Case_Items
        (To_Case : Project_Node_Id; Var : Project_Node_Id)
      is
         Str : Project_Node_Id;
         Case_Item : Project_Node_Id := Empty_Node;
         First_Choice : Project_Node_Id;
      begin
         Set_Case_Variable_Reference_Of (To_Case, Var);

         Str := First_Literal_String (String_Type_Of (Var));
         while Str /= Empty_Node loop
            if Case_Item = Empty_Node then
               Set_First_Case_Item_Of
                 (To_Case, Default_Project_Node (N_Case_Item));
               Case_Item := First_Case_Item_Of (To_Case);
            else
               Set_Next_Case_Item
                 (Case_Item, Default_Project_Node (N_Case_Item));
               Case_Item := Next_Case_Item (Case_Item);
            end if;

            --  We have to duplicate the literal string, since otherwise the
            --  case item will be a "or" for all the items in the list
            First_Choice := Default_Project_Node (N_Literal_String);
            Clone_Node (From => Str, To => First_Choice);
            Set_Next_Literal_String (First_Choice, Empty_Node);

            Set_First_Choice_Of (Case_Item, First_Choice);

            Str := Next_Literal_String (Str);
         end loop;
      end Add_Case_Items;

      ----------------
      -- Add_Nested --
      ----------------

      procedure Add_Nested (To_Case : Project_Node_Id; Ref : Project_Node_Id)
      is
         Case_Item : Project_Node_Id := First_Case_Item_Of (To_Case);
         Decl_Item,
         Current_Case : Project_Node_Id;
      begin
         while Case_Item /= Empty_Node loop
            Decl_Item := First_Declarative_Item_Of (Case_Item);
            if Decl_Item /= Empty_Node then
               Decl_Item := Current_Item_Node (Decl_Item);
               pragma Assert (Kind_Of (Decl_Item) = N_Case_Construction);
               Add_Nested (Decl_Item, Ref);

            else
               Decl_Item := Default_Project_Node (N_Declarative_Item);
               Set_First_Declarative_Item_Of (Case_Item, Decl_Item);

               Current_Case := Default_Project_Node (N_Case_Construction);
               Set_Current_Item_Node (Decl_Item, Current_Case);

               Add_Case_Items (Current_Case, Ref);
            end if;

            Case_Item := Next_Case_Item (Case_Item);
         end loop;
      end Add_Nested;

      Nested_Case : Project_Node_Id := Empty_Node;

   begin
      for J in Scenario_Vars'Range loop
         if Nested_Case = Empty_Node then
            Nested_Case := Default_Project_Node (N_Case_Construction);
            Add_Case_Items
              (Nested_Case,
               Create_Variable_Reference (Scenario_Vars (J)));

         else
            Add_Nested
              (Nested_Case,
               Create_Variable_Reference (Scenario_Vars (J)));
         end if;
      end loop;
      return Nested_Case;
   end Create_Nested_Case;

   ----------------------------
   -- For_Each_Matching_Item --
   ----------------------------

   procedure For_Each_Matching_Item
     (In_Case    : Project_Node_Id;
      Var_Values : Literal_String_Array;
      Action     : Matching_Item_Callback)
   is
      type Case_Item_Array is array (Var_Values'Range) of Project_Node_Id;
      Case_Items : Case_Item_Array := (others => Empty_Node);
      J : Natural := Case_Items'First;
   begin
      Case_Items (Case_Items'First) := First_Case_Item_Of (In_Case);

      while Case_Items (Case_Items'First) /= Empty_Node loop

         if Var_Values (J) = No_String
           or else String_Equal
           (String_Value_Of (First_Choice_Of (Case_Items (J))), Var_Values (J))
         then
            if J = Var_Values'Last then
               if not Action (Case_Items (J)) then
                  return;
               end if;
               Case_Items (J) := Next_Case_Item (Case_Items (J));

            else
               --  Go down to the deeper case statement
               Case_Items (J + 1) := First_Case_Item_Of
                 (Current_Item_Node
                  (First_Declarative_Item_Of (Case_Items (J))));
               J := J + 1;
            end if;
         else
            Case_Items (J) := Next_Case_Item (Case_Items (J));
         end if;

         --  We have already moved to the next case item in the current case.
         --  However, if we have reached the end, we need to go up one level.
         loop
            exit when J = Case_Items'First
              or else Case_Items (J) /= Empty_Node;
            J := J - 1;
            Case_Items (J) := Next_Case_Item (Case_Items (J));
         end loop;
      end loop;
   end For_Each_Matching_Item;

   ----------------------
   -- Add_To_Case_Item --
   ----------------------

   procedure Add_To_Case_Item
     (In_Case : Project_Node_Id;
      Var_Values : Literal_String_Array;
      Expression : Project_Node_Id)
   is
      function Add_Value (Item : Project_Node_Id) return Boolean;
      --  Add the expression at the end of the declarative items for the case
      --  item Item.

      ---------------
      -- Add_Value --
      ---------------

      function Add_Value (Item : Project_Node_Id) return Boolean is
         Decl_Item : Project_Node_Id;
      begin
         --  Add instruction
         Decl_Item := First_Declarative_Item_Of (Item);
         if Decl_Item = Empty_Node then
            Decl_Item := Default_Project_Node (N_Declarative_Item);
            Set_First_Declarative_Item_Of (Item, Decl_Item);
         else
            --  ??? Loop could be avoided if we store the last decl_items
            --  in an array
            while Next_Declarative_Item (Decl_Item) /= Empty_Node loop
               Decl_Item := Next_Declarative_Item (Decl_Item);
            end loop;
            Set_Next_Declarative_Item
              (Decl_Item, Default_Project_Node (N_Declarative_Item));
            Decl_Item := Next_Declarative_Item (Decl_Item);
         end if;
         Set_Current_Item_Node (Decl_Item, Expression);
         return True;
      end Add_Value;

   begin
      For_Each_Matching_Item
        (In_Case, Var_Values, Add_Value'Unrestricted_Access);
   end Add_To_Case_Item;

   -----------------------
   -- Normalize_Project --
   -----------------------

   procedure Normalize_Project (Project : Project_Node_Id) is
      Scenario_Variables : constant Project_Node_Array :=
        Find_Scenario_Variables (Project, Parse_Imported => False);
      Var_Values : Literal_String_Array (Scenario_Variables'Range) :=
        (others => No_String);
      Decl2, Decl_Item2 : Project_Node_Id := Empty_Node;

      procedure Recurse
        (Declarative_Item : Project_Node_Id;
         To_Case : in out Project_Node_Id;
         Parent, Decl_Item_Out : in out Project_Node_Id);
      --  Append all the instructions in Declarative_Item list to the
      --  normalized project, at the correct location.
      --  Declarations are either appended to To_Case (in one of the nested
      --  cases), or at the end of the declarative_item list in Parent, where
      --  Decl_Item_Out must point to the last item in the list

      -------------
      -- Recurse --
      -------------

      procedure Recurse
        (Declarative_Item : Project_Node_Id;
         To_Case : in out Project_Node_Id;
         Parent, Decl_Item_Out : in out Project_Node_Id)
      is
         Decl_Item : Project_Node_Id := Declarative_Item;
         Case_Item : Project_Node_Id;
         J, K : Natural;
      begin
         while Decl_Item /= Empty_Node loop
            case Kind_Of (Current_Item_Node (Decl_Item)) is
               when N_Case_Construction =>
                  K := Scenario_Variables'First;
                  while K <= Scenario_Variables'Last loop
                     exit when Name_Of (Scenario_Variables (K)) =
                       Name_Of (Case_Variable_Reference_Of
                                (Current_Item_Node (Decl_Item)));
                     K :=  K + 1;
                  end loop;

                  if K > Scenario_Variables'Last then
                     Put_Line ("Cannot import projects that have case "
                               & " statements not associated with"
                               & " typed and external variables");
                     pragma Assert (False);
                     return;
                  end if;

                  Case_Item := First_Case_Item_Of
                    (Current_Item_Node (Decl_Item));
                  while Case_Item /= Empty_Node loop
                     --  Do we have a "when others" ?
                     if First_Choice_Of (Case_Item) = Empty_Node then
                        Var_Values (K) := No_String;
                     else
                        Var_Values (K) :=
                          String_Value_Of (First_Choice_Of (Case_Item));
                     end if;

                     Recurse (First_Declarative_Item_Of (Case_Item),
                              To_Case, Parent, Decl_Item_Out);
                     Case_Item := Next_Case_Item (Case_Item);
                  end loop;
                  Var_Values (K) := No_String;

               when N_Package_Declaration => null;

               when N_String_Type_Declaration =>
                  --  Note: this always happens only in N_Project, not in
                  --  package declaration.
                  Add_At_End
                    (Current_Item_Node (Decl_Item), Parent, Decl_Item_Out);

               when N_Typed_Variable_Declaration =>
                  if Kind_Of (Parent) = N_Package_Declaration
                    and then
                    Is_External_Variable (Current_Item_Node (Decl_Item))
                  then
                     --  A scenario variable: add its declaration in the
                     --  enclosing project, not in the package
                     Add_At_End
                       (Current_Item_Node (Decl_Item), Decl2, Decl_Item2);

                  else
                     Add_At_End
                       (Current_Item_Node (Decl_Item), Parent, Decl_Item_Out);
                  end if;

               when others =>
                  --  First, a quick check to make sure that at least one
                  --  var_values is not null. Otherwise, we end up appending
                  --  the same expression to all the nodes in the case
                  --  statement.
                  J := Var_Values'First;
                  while J <= Var_Values'Last loop
                     exit when Var_Values (J) /= No_String;
                     J := J + 1;
                  end loop;

                  if J > Var_Values'Last then
                     Add_At_End
                       (Current_Item_Node (Decl_Item), Parent, Decl_Item_Out);
                  else
                     if To_Case = Empty_Node then
                        To_Case := Create_Nested_Case (Scenario_Variables);
                     end if;
                     Add_To_Case_Item
                       (To_Case, Var_Values,
                        Current_Item_Node (Decl_Item));
                  end if;

            end case;
            Decl_Item := Next_Declarative_Item (Decl_Item);
         end loop;
      end Recurse;

      Project_Norm : Project_Node_Id := Clone_Project (Project);
      Project_Nested_Case : Project_Node_Id := Empty_Node;
      Decl, Pkg : Project_Node_Id;
      Pkg_Decl_Item, Pkg_Nested_Case : Project_Node_Id;

   begin
      Decl := Project_Declaration_Of (Project);
      Decl2 := Project_Declaration_Of (Project_Norm);

      Recurse (First_Declarative_Item_Of (Decl), Project_Nested_Case,
               Decl2, Decl_Item2);

      Pkg := First_Package_Of (Project);
      while Pkg /= Empty_Node loop
         Pkg_Decl_Item := Empty_Node;
         Pkg_Nested_Case := Empty_Node;
         Recurse (First_Declarative_Item_Of (Pkg), Pkg_Nested_Case,
                  Pkg, Pkg_Decl_Item);
         if Pkg_Nested_Case /= Empty_Node then
            Add_At_End (Pkg_Nested_Case, Pkg, Pkg_Decl_Item);
         end if;

         Add_At_End (Pkg, Decl2, Decl_Item2);

         Pkg := Next_Package_In_Project (Pkg);
      end loop;

      if Project_Nested_Case /= Empty_Node then
         Add_At_End (Project_Nested_Case, Decl2, Decl_Item2);
      end if;

      --  Directly replace in the table, so that all references to this project
      --  are automatically updated. There is a small memory leak, but since
      --  most of the project tree is shared, it doesn't really matter in the
      --  life of the project editor

      Tree_Private_Part.Project_Nodes.Table (Project) :=
        Tree_Private_Part.Project_Nodes.Table (Project_Norm);
   end Normalize_Project;

   --------------------------------
   -- Current_Scenario_Case_Item --
   --------------------------------

   function Current_Scenario_Case_Item
     (Project : Prj.Tree.Project_Node_Id;
      Pkg     : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node)
      return Project_Node_Id
   is
      Matching_Case : Project_Node_Id := Empty_Node;
      Decl_Item : Project_Node_Id;
      Previous_Decl : Project_Node_Id := Empty_Node;
      Nested_Case : Project_Node_Id := Empty_Node;

      Scenario_Variables : constant Project_Node_Array :=
        Find_Scenario_Variables (Project, Parse_Imported => False);
      Var_Values : Literal_String_Array (Scenario_Variables'Range) :=
        (others => No_String);

      function Find (Item : Project_Node_Id) return Boolean;
      --  Saves the id of the case item that matches the current value for the
      --  environment variables.

      ----------
      -- Find --
      ----------

      function Find (Item : Project_Node_Id) return Boolean is
      begin
         Matching_Case := Item;

         --  Stop search, since we have found what we wanted.
         return False;
      end Find;

   begin
      --  If there are no scenario variables, we simply add at the end of the
      --  project or the package

      if Scenario_Variables'Length = 0 then
         if Pkg = Empty_Node then
            return Project_Declaration_Of (Project);
         else
            return Pkg;
         end if;
      end if;


      if Pkg = Empty_Node then
         Decl_Item := First_Declarative_Item_Of
           (Project_Declaration_Of (Project));
      else
         Decl_Item := First_Declarative_Item_Of (Pkg);
      end if;

      --  First, look for the beginning of the nested case statement.

      while Decl_Item /= Empty_Node loop
         case Kind_Of (Current_Item_Node (Decl_Item)) is
            when N_Case_Construction =>
               Nested_Case := Current_Item_Node (Decl_Item);
               exit;

            when others =>
               null;
         end case;
         Previous_Decl := Decl_Item;
         Decl_Item := Next_Declarative_Item (Decl_Item);
      end loop;

      --  If there is no nested case statement, created one

      if Nested_Case = Empty_Node then
         Nested_Case := Create_Nested_Case (Scenario_Variables);
         if Pkg = Empty_Node then
            Decl_Item := Project_Declaration_Of (Project);
         else
            Decl_Item := Pkg;
         end if;
         Add_At_End (Nested_Case, Decl_Item, Previous_Decl);
      end if;

      --  Get the current value of all the variables

      for J in Scenario_Variables'Range loop
         String_To_Name_Buffer
           (External_Reference_Of (Scenario_Variables (J)));

         Var_Values (J) := Prj.Ext.Value_Of (Name_Find);
      end loop;

      --  Find the appropriate case item.

      pragma Assert (Nested_Case /= Empty_Node);

      For_Each_Matching_Item
        (Nested_Case, Var_Values, Find'Unrestricted_Access);
      return Matching_Case;
   end Current_Scenario_Case_Item;

end Prj_Normalize;

