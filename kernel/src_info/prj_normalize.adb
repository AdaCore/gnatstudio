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

with Prj_API; use Prj_API;

with Prj;      use Prj;
with Prj.Ext;  use Prj.Ext;
with Prj.Tree; use Prj.Tree;
with Types;    use Types;
with Namet;    use Namet;
with Stringt;  use Stringt;

pragma Warnings (Off);
with Prj.PP;   use Prj.PP;
with Text_IO;  use Text_IO;
pragma Warnings (On);

with Unchecked_Deallocation;

package body Prj_Normalize is

   type Literal_String_Array is array (Positive range <>) of String_Id;

   type Matching_Item_Callback is access
     function (Item : Project_Node_Id) return Boolean;
   --  A callback function called for each case item that matches a specific
   --  set of values (see For_Each_Matching_Item).
   --  If the callback returns False, we stop searching for other matches


   type Value_Status is (Never_Seen, In_Process, Processed);
   type Variable_Value is record
      Value  : String_Id;
      Status : Value_Status := Never_Seen;
   end record;
   --  The two types above represent the current setup of the scenario
   --  variables while normalizing a project file. Status indicates if the
   --  value was already processed for the variable, and is used to handle the
   --  "when others" case (which matches all values that have never been seen
   --  before in the current case construction).

   type Value_Array is array (Positive range <>) of Variable_Value;
   type Value_Array_Access is access Value_Array;

   type Variable_Values_Record;
   type Variable_Values is access Variable_Values_Record;
   type Variable_Values_Record is record
      Variable : Project_Node_Id;  --  N_Variable_Reference
      Name     : String_Id;        --  External reference name
      Values   : Value_Array_Access;
      Seen     : Boolean;          --  Internal use only
      Next     : Variable_Values;
   end record;
   --  This list is used while parsing a case statement to store the current
   --  value of the scenario variables. We use a list instead of an array so
   --  that we do not have to search for all the scenario variables
   --  initially. Instead, we add variables every time we encounter a new
   --  variable.

   procedure Free (Variable : in out Variable_Values);
   --  Free the element pointed to by Variable

   procedure Clone_Node (From : Project_Node_Id; To : Project_Node_Id);
   --  Make an exact copy of From to To. This overwrites all the fields in To.

   function Clone_Project (Project : Project_Node_Id) return Project_Node_Id;
   --  Return a duplicate of Project and its declarations. We do not duplicate
   --  declarative items.
   --  The new project is not independent of the old one, since most of the
   --  nodes are shared between the two for efficiency reasons.

   procedure For_Each_Matching_Item
     (In_Case    : Project_Node_Id;
      Var_Values : Literal_String_Array;
      Action     : Matching_Item_Callback);
   --  Iterates other all the possible nested case items in In_Case, and for
   --  each that matches Var_Values, calls Action.
   --  A No_String value in Var_Values matches any possible value for the
   --  variable

   function Duplicate_Decl_Item_List (Decl : Project_Node_Id)
      return Project_Node_Id;
   --  Return a duplicate of the declarative items list in Decl.
   --  Note: to spare some memory, only the declarative item node itself is
   --  shared. The underlying current item is shared.

   function Create_Case_Construction
     (Value : Variable_Values; Child : Project_Node_Id)
      return Project_Node_Id;
   --  Return a N_Case_Construction for the variable defined in Value. Note
   --  that this doesn't create a nested case construction, only the first
   --  level for the first variable in Value.
   --  The list of declarative items in Child is added to each of the items
   --  in the case construction.

   ----------
   -- Free --
   ----------

   procedure Free (Variable : in out Variable_Values) is
      procedure Internal is new Unchecked_Deallocation
        (Variable_Values_Record, Variable_Values);
      procedure Internal is new Unchecked_Deallocation
        (Value_Array, Value_Array_Access);
   begin
      Internal (Variable.Values);
      Internal (Variable);
   end Free;

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

   ------------------------------
   -- Duplicate_Decl_Item_List --
   ------------------------------

   function Duplicate_Decl_Item_List (Decl : Project_Node_Id)
      return Project_Node_Id
   is
      D    : Project_Node_Id := Decl;
      Item, Start, Tmp : Project_Node_Id := Empty_Node;
   begin
      while D /= Empty_Node loop
         Tmp := Default_Project_Node (N_Declarative_Item, Single);

         if Item = Empty_Node then
            Start := Tmp;
         else
            Set_Next_Declarative_Item (Item, Tmp);
         end if;

         Item := Tmp;
         Set_Current_Item_Node (Tmp, Current_Item_Node (D));

         D := Next_Declarative_Item (D);
      end loop;
      return Start;
   end Duplicate_Decl_Item_List;

   ------------------------------
   -- Create_Case_Construction --
   ------------------------------

   function Create_Case_Construction
     (Value : Variable_Values; Child : Project_Node_Id)
      return Project_Node_Id
   is
      Construct, Str, S, Item : Project_Node_Id;
   begin
      Construct := Default_Project_Node (N_Case_Construction);
      Set_Case_Variable_Reference_Of (Construct, Value.Variable);

      Str := First_Literal_String (String_Type_Of (Value.Variable));
      while Str /= Empty_Node loop

         --  Construct the case item, and add it to the list of
         --  case items
         Item := Default_Project_Node (N_Case_Item);
         S := Default_Project_Node (N_Literal_String);
         Set_String_Value_Of (S, String_Value_Of (Str));
         Set_First_Choice_Of (Item, S);
         Set_Next_Case_Item (Item, First_Case_Item_Of (Construct));
         Set_First_Case_Item_Of (Construct, Item);

         Set_First_Declarative_Item_Of
           (Item, Duplicate_Decl_Item_List (Child));

         Str := Next_Literal_String (Str);
      end loop;

      return Construct;
   end Create_Case_Construction;

   -----------------------
   -- Normalize_Project --
   -----------------------

   procedure Normalize_Project (Project : Project_Node_Id) is
      Values : Variable_Values := null;
      Project_Norm         : Project_Node_Id := Clone_Project (Project);

      procedure Process_Declarative_List
        (From, To : Project_Node_Id; Case_Stmt : in out Project_Node_Id);
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

      procedure Add_To_Case_Stmt
        (Case_Stmt  : in out Project_Node_Id;
         Decl_Item  : Project_Node_Id);
      --  Add Decl_Item to the appropriate nested case items in Case_Stmt,
      --  depending on the current setup of Values.

      ----------------------
      -- Add_To_Case_Stmt --
      ----------------------

      procedure Add_To_Case_Stmt
        (Case_Stmt  : in out Project_Node_Id;
         Decl_Item  : Project_Node_Id)
      is
         procedure Add_To_Case_Recursive (Case_Stmt : Project_Node_Id);

         ---------------------------
         -- Add_To_Case_Recursive --
         ---------------------------

         procedure Add_To_Case_Recursive (Case_Stmt : Project_Node_Id) is
            --  ??? Case_Variable_Reference_Of might not be the most efficient
            Name : constant String_Id := External_Variable_Name
              (Project_Norm, Case_Variable_Reference_Of (Case_Stmt));
            Choice : String_Id;
            Current_Item, New_Case : Project_Node_Id;
            V : Variable_Values;
            Found : Boolean;
            Match : Variable_Values;

         begin
            pragma Assert (Name /= No_String);

            --  For each item in the case (ie all possible values of the
            --  variable).

            Current_Item := First_Case_Item_Of (Case_Stmt);
            while Current_Item /= Empty_Node loop

               --  Note: since we created the case internally, there is only
               --  one choice, we don't need to check several of them.

               Choice := String_Value_Of (First_Choice_Of (Current_Item));

               --  Do we have a matching reference in the current scenario
               --  * The variable is not in values (default "when others")
               --  * The choice in the case item is in the list of values
               --    for the variable

               V := Values;
               Found := False;
               Match := null;

               while Match = null and then V /= null loop
                  if String_Equal (V.Name, Name) then
                     Found := True;
                     V.Seen := True;

                     for C in V.Values'Range loop
                        if V.Values (C).Status = In_Process
                          and then String_Equal (Choice, V.Values (C).Value)
                        then
                           Match := V;
                           exit;
                        end if;
                     end loop;

                     exit;
                  end if;

                  V := V.Next;
               end loop;

               if Match /= null or else not Found then
                  New_Case := First_Declarative_Item_Of (Current_Item);

                  --  If we have a nested case statement, we need to choose the
                  --  right branches there as well.
                  if New_Case /= Empty_Node
                    and then Kind_Of (New_Case) = N_Case_Construction
                  then
                     Add_To_Case_Recursive (New_Case);

                  --  Else, check if we need to create a nested case.
                  --  This nested case replaces the current declarative item,
                  --  which is then assigned to each of the branches of the
                  --  nested case.
                  else
                     V := Values;
                     while V /= null loop
                        if not V.Seen then
                           New_Case := Create_Case_Construction (V, New_Case);
                           Add_To_Case_Recursive (New_Case);
                           Set_First_Declarative_Item_Of
                             (Current_Item, New_Case);
                           exit;
                        end if;
                        V := V.Next;
                     end loop;

                     --  All variables have been seen => Add the new
                     --  declarative item at the end of the current list.
                     if V = null then
                        Add_At_End (Current_Item, Decl_Item);
                     end if;
                  end if;

                  if Match /= null then
                     Match.Seen := False;
                  end if;
               end if;

               Current_Item := Next_Case_Item (Current_Item);
            end loop;
         end Add_To_Case_Recursive;

         V : Variable_Values := Values;
      begin

         --  Reset the variables
         V := Values;
         while V /= null loop
            V.Seen := False;
            V := V.Next;
         end loop;

         if Case_Stmt = Empty_Node then
            Case_Stmt := Create_Case_Construction (Values, Empty_Node);
         end if;

         Add_To_Case_Recursive (Case_Stmt);
      end Add_To_Case_Stmt;

      ------------------------------
      -- Process_Declarative_List --
      ------------------------------

      procedure Process_Declarative_List
        (From, To : Project_Node_Id; Case_Stmt : in out Project_Node_Id)
      is
         Decl_Item : Project_Node_Id := From;
         Next_Item, Choice : Project_Node_Id;
         V         : Variable_Values;
         Name      : Name_Id;
         Case_Item : Project_Node_Id;
         Num_Values : Natural;
      begin
         pragma Assert (Kind_Of (Decl_Item) = N_Declarative_Item);

         while Decl_Item /= Empty_Node loop

            --  Save the next item, since the current item will be inserted in
            --  a different list, and thus its next field will be modified.
            Next_Item := Next_Declarative_Item (Decl_Item);
            Set_Next_Declarative_Item (Decl_Item, Empty_Node);

            case Kind_Of (Current_Item_Node (Decl_Item)) is

               when N_Package_Declaration =>
                  --  Skip subpackages, since these must appear after every
                  --  other declarative item in the normalized project.
                  null;

               when N_Case_Construction =>
                  Name := Name_Of (Case_Variable_Reference_Of
                                   (Current_Item_Node (Decl_Item)));

                  --  Do we already have this variable in the list of values.
                  --  If yes, this means we have something similar to:
                  --    case A is
                  --       when "1" =>
                  --          case A is
                  --              when "1" => keep;
                  --              when "2" => ignore;
                  --  We should only keep the item in the nested case that
                  --  matches the value of the outer item.

                  V := null;

                  --  V := Values;

                  --  For_Each_Known_Variable :
                  --  while V /= null loop
                  --     if External_Reference_Of (V.Variable) = Name then
                  --        --  Skip to the case_item matching the value
                  --        Case_Item := First_Case_Item_Of
                  --          (Current_Item_Node (Decl_Item));
                  --        while Case_Item /= Empty_Node loop

                  --           Choice := First_Choice_Of (Case_Item);
                  --           while Choice /= Empty_Node loop
                  --              if String_Value_Of (Choice) = V.Values then
                  --                 Process_Declarative_List
                  --              (First_Declarative_Item_Of (Case_Item), To);
                  --                 exit For_Each_Known_Variable;
                  --              end if;

                  --              Choice := Next_Literal_String (Choice);
                  --           end loop;

                  --           Case_Item := Next_Case_Item (Case_Item);
                  --        end loop;
                  --     end if;

                  --     V := V.Next;
                  --  end loop For_Each_Known_Variable;

                  --  If we didn't find any previous ref to the variable

                  if V = null then
                     Case_Item := First_Case_Item_Of
                       (Current_Item_Node (Decl_Item));

                     Values := new Variable_Values_Record'
                       (Variable => Case_Variable_Reference_Of
                          (Current_Item_Node (Decl_Item)),
                        Name     => No_String,
                        Values   => null,
                        Seen     => False,
                        Next     => Values);
                     Values.Name := External_Variable_Name
                       (Project, Values.Variable);

                     --  Count the number of possible values for the variable

                     Num_Values := 0;
                     Choice := String_Type_Of (Case_Variable_Reference_Of
                        (Current_Item_Node (Decl_Item)));
                     Choice := First_Literal_String (Choice);

                     while Choice /= Empty_Node loop
                        Num_Values := Num_Values + 1;
                        Choice := Next_Literal_String (Choice);
                     end loop;

                     --  Then allocate the list of values for the variable

                     Values.Values := new Value_Array (1 .. Num_Values);
                     Num_Values := 0;
                     Choice := String_Type_Of (Case_Variable_Reference_Of
                        (Current_Item_Node (Decl_Item)));
                     Choice := First_Literal_String (Choice);
                     while Choice /= Empty_Node loop
                        Num_Values := Num_Values + 1;
                        Values.Values (Num_Values).Value :=
                          String_Value_Of (Choice);
                        Choice := Next_Literal_String (Choice);
                     end loop;

                     --  For all the case items in the current case
                     --  construction

                     while Case_Item /= Empty_Node loop
                        --  Do we have a "when others" ? If yes, the values
                        --  processed are all the ones we haven't seen yet.

                        if First_Choice_Of (Case_Item) = Empty_Node then
                           for C in Values.Values'Range loop
                              if Values.Values (C).Status = Never_Seen then
                                 Values.Values (C).Status := In_Process;
                              end if;
                           end loop;

                           --  Else: a standard list of possible choices.

                        else
                           Choice := First_Choice_Of (Case_Item);
                           while Choice /= Empty_Node loop
                              for C in Values.Values'Range loop
                                 if String_Equal
                                   (Values.Values (C).Value,
                                    String_Value_Of (Choice))
                                 then
                                    Values.Values (C).Status := In_Process;
                                    exit;
                                 end if;
                              end loop;
                              Choice := Next_Literal_String (Choice);
                           end loop;
                        end if;

                        --  Process the declarative list of items

                        Process_Declarative_List
                          (First_Declarative_Item_Of (Case_Item),
                           To, Case_Stmt);
                        Case_Item := Next_Case_Item (Case_Item);

                        --  Report the values as processed (in case we see a
                        --  "when others" later on)
                        for C in Values.Values'Range loop
                           if Values.Values (C).Status = In_Process then
                              Values.Values (C).Status := Processed;
                           end if;
                        end loop;
                     end loop;

                     V := Values;
                     Values := Values.Next;
                     Free (V);
                  end if;


               when others =>
                  --  If we already had a case statement before, add to it
                  if Case_Stmt /= Empty_Node then
                     Add_To_Case_Stmt (Case_Stmt, Decl_Item);

                  --  Else create the case statement
                  elsif Values /= null then
                     Add_To_Case_Stmt (Case_Stmt, Decl_Item);
                     Add_At_End (To, Case_Stmt);

                  else
                     Add_At_End (To, Decl_Item);
                  end if;
            end case;

            Decl_Item := Next_Item;
         end loop;
      end Process_Declarative_List;


      Decl, Pkg, Case_Stmt : Project_Node_Id;

   begin
      --  The top-level part of the project
      Case_Stmt := Empty_Node;
      Process_Declarative_List
        (From => First_Declarative_Item_Of (Project_Declaration_Of (Project)),
         To   => Project_Declaration_Of (Project_Norm),
         Case_Stmt => Case_Stmt);
      pragma Assert (Values = null);

      --  All the subpackages

      Pkg := First_Package_Of (Project);
      while Pkg /= Empty_Node loop
         Decl := First_Declarative_Item_Of (Pkg);
         Set_First_Declarative_Item_Of (Pkg, Empty_Node);

         Case_Stmt := Empty_Node;
         Process_Declarative_List
           (From      => Decl,
            To        => Pkg,
            Case_Stmt => Case_Stmt);
         pragma Assert (Values = null);

         Add_At_End (Project_Declaration_Of (Project_Norm), Pkg);

         Pkg := Next_Package_In_Project (Pkg);
      end loop;

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

      --  if Nested_Case = Empty_Node then
      --     Nested_Case := Create_Nested_Case (Scenario_Variables);
      --     if Pkg = Empty_Node then
      --        Decl_Item := Project_Declaration_Of (Project);
      --     else
      --        Decl_Item := Pkg;
      --     end if;
      --     Add_At_End (Decl_Item, Nested_Case);
      --  end if;

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

