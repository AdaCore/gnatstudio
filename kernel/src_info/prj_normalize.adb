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
      Name     : String_Id;          --  External reference name
      Type_Def : Project_Node_Id;    --  N_String_Type_Declaration
      Values   : Value_Array_Access; --  Current values
      Seen     : Boolean;            --  Internal use only
      Next     : Variable_Values;
   end record;
   --  This list is used while parsing a case statement to store the current
   --  value of the scenario variables. We use a list instead of an array so
   --  that we do not have to search for all the scenario variables
   --  initially. Instead, we add variables every time we encounter a new
   --  variable.

   procedure Free (Variable : in out Variable_Values);
   --  Free the element pointed to by Variable

   function Clone_Node (From : Project_Node_Id) return Project_Node_Id;
   --  Return a clone of From. Note that it shares all the nodes that are
   --  linked, like declarative items,...

   function Clone_Project (Project : Project_Node_Id) return Project_Node_Id;
   --  Return a duplicate of Project and its declarations. We do not duplicate
   --  declarative items.
   --  The new project is not independent of the old one, since most of the
   --  nodes are shared between the two for efficiency reasons.

   function Duplicate_Decl_Item_List (Decl : Project_Node_Id)
      return Project_Node_Id;
   --  Return a duplicate of the declarative items list in Decl.
   --  Note: to spare some memory, only the declarative item node itself is
   --  shared. The underlying current item is shared.

   function Create_Case_Construction
     (Project : Project_Node_Id;
      Value   : Variable_Values;
      Child   : Project_Node_Id)
      return Project_Node_Id;
   --  Return a N_Case_Construction for the variable defined in Value. Note
   --  that this doesn't create a nested case construction, only the first
   --  level for the first variable in Value.
   --  The declaration for the variable itself is added at the beginning of the
   --  project if needed.
   --  The list of declarative items in Child is added to each of the items
   --  in the case construction.

   procedure For_Each_Case_Stmt
     (Values    : Variable_Values := null;
      Project   : Project_Node_Id;
      Case_Stmt : in out Project_Node_Id;
      Action    : Matching_Item_Callback);
   --  Calls Action for each of the nested case items that match Values.
   --  Values must not be null if Case_Stmt hasn't been created yet.
   --  Case_Stmt is created if it doesn't exist yet.
   --  This is the internal version of For_Each_Scenario_Case_Item.
   --  Project is the project that should be used to retrieve the name of the
   --  environment variables referenced by the variables.

   function Create_Scenario_Variable
     (Project : Project_Node_Id; Variable : Project_Node_Id)
      return Variable_Values;
   --  Return a new Variable_Values structure for Variable.
   --  Variable must be a N_Typed_Variable_Declaration or a
   --  N_Variable_Reference.

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

   function Clone_Node (From : Project_Node_Id) return Project_Node_Id is
      To : Project_Node_Id;
   begin
      pragma Assert (From /= Empty_Node);
      To := Default_Project_Node (Kind_Of (From));
      Tree_Private_Part.Project_Nodes.Table (To) :=
        Tree_Private_Part.Project_Nodes.Table (From);
      return To;
   end Clone_Node;

   -------------------
   -- Clone_Project --
   -------------------

   function Clone_Project (Project : Project_Node_Id) return Project_Node_Id is
      Project2, Decl : Project_Node_Id;
   begin
      Project2 := Clone_Node (Project);
      Decl     := Clone_Node (Project_Declaration_Of (Project));
      Set_Project_Declaration_Of    (Project2, Decl);
      Set_First_Declarative_Item_Of (Decl, Empty_Node);
      return Project2;
   end Clone_Project;

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
     (Project : Project_Node_Id;
      Value   : Variable_Values;
      Child   : Project_Node_Id)
      return Project_Node_Id
   is
      Construct, Str, S, Item : Project_Node_Id;
      Ref : String_Id;
   begin
      --  Makee sure there is a definition for this variable (and its type) at
      --  the top-level of the project (not in a package nor in another
      --  project).
      --  This is required so that normalized projects might be standalone.


      --  Check if there is already a definition for the variable, and if not
      --  add it. Note: we do this before testing for the type, since we are
      --  adding in front of the declarative items list.

      Item := First_Variable_Of (Project);
      while Item /= Empty_Node loop
         if Kind_Of (Item) = N_Typed_Variable_Declaration then
            Ref := External_Reference_Of (Item);
            exit when Ref /= No_String and then String_Equal (Ref, Value.Name);
         end if;
         Item := Next_Variable (Item);
      end loop;

      --  If not, add the variable.
      --  ??? Currently, if the variable was already defined in a package, it
      --  ??? will be duplicated (once at the beginning of the project, one in
      --  ??? the package). Doesn't seem very important, though.

      if Item = Empty_Node then
         String_To_Name_Buffer (Value.Name);
         Item := Get_Or_Create_Typed_Variable
           (Project, Name_Buffer (1 .. Name_Len), Value.Type_Def,
            Add_Before_First_Case_Or_Pkg => True);
      end if;


      --  Check if there is already a definition for the type, and if not add
      --  it.
      --  ??? To be implemented

      if Expression_Of (Item) = Empty_Node then
         String_To_Name_Buffer (Value.Name);
         Set_Value_As_External (Item, Name_Buffer (1 .. Name_Len));
      end if;

      Construct := Default_Project_Node (N_Case_Construction);
      Set_Case_Variable_Reference_Of
        (Construct, Create_Variable_Reference (Item));

      Str := First_Literal_String (Value.Type_Def);
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

   ------------------------------
   -- Create_Scenario_Variable --
   ------------------------------

   function Create_Scenario_Variable
     (Project : Project_Node_Id; Variable : Project_Node_Id)
      return Variable_Values
   is
      Values     : Variable_Values;
      Num_Values : Natural;
      Choice     : Project_Node_Id;
   begin
      pragma Assert (Kind_Of (Variable) = N_Typed_Variable_Declaration
                     or else Kind_Of (Variable) = N_Variable_Reference);

      Values := new Variable_Values_Record'
        (Name     => External_Variable_Name (Project, Variable),
         Type_Def => String_Type_Of (Variable),
         Values   => null,
         Seen     => False,
         Next     => null);

      --  Count the number of possible values for the variable

      Num_Values := 0;
      Choice := First_Literal_String (Values.Type_Def);
      while Choice /= Empty_Node loop
         Num_Values := Num_Values + 1;
         Choice := Next_Literal_String (Choice);
      end loop;

      --  Then allocate the list of values for the variable

      Values.Values := new Value_Array (1 .. Num_Values);
      Num_Values := 0;
      Choice := First_Literal_String (Values.Type_Def);

      while Choice /= Empty_Node loop
         Num_Values := Num_Values + 1;
         Values.Values (Num_Values).Value := String_Value_Of (Choice);
         Choice := Next_Literal_String (Choice);
      end loop;

      return Values;
   end Create_Scenario_Variable;

   ------------------------
   -- For_Each_Case_Stmt --
   ------------------------

   procedure For_Each_Case_Stmt
     (Values    : Variable_Values := null;
      Project   : Project_Node_Id;
      Case_Stmt : in out Project_Node_Id;
      Action    : Matching_Item_Callback)
   is
      procedure Process_Case_Recursive (Case_Stmt : Project_Node_Id);

      ----------------------------
      -- Process_Case_Recursive --
      ----------------------------

      procedure Process_Case_Recursive (Case_Stmt : Project_Node_Id) is
         --  ??? Case_Variable_Reference_Of might not be the most efficient
         Name : constant String_Id := External_Variable_Name
           (Project, Case_Variable_Reference_Of (Case_Stmt));
         Choice : String_Id;
         Current_Item, New_Case, Declarative : Project_Node_Id;
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
                  Process_Case_Recursive (New_Case);

                  --  Else, check if we need to create a nested case.
                  --  This nested case replaces the current declarative item,
                  --  which is then assigned to each of the branches of the
                  --  nested case.
               else
                  V := Values;
                  while V /= null loop
                     if not V.Seen then
                        New_Case := Create_Case_Construction
                          (Project, V, New_Case);
                        Process_Case_Recursive (New_Case);

                        Declarative := Default_Project_Node
                          (N_Declarative_Item, Prj.Single);
                        Set_Current_Item_Node (Declarative, New_Case);

                        Set_First_Declarative_Item_Of
                          (Current_Item, Declarative);
                        exit;
                     end if;
                     V := V.Next;
                  end loop;

                  --  All variables have been seen => Add the new
                  --  declarative item at the end of the current list.
                  if V = null then
                     Action (Current_Item);
                  end if;
               end if;

               if Match /= null then
                  Match.Seen := False;
               end if;
            end if;

            Current_Item := Next_Case_Item (Current_Item);
         end loop;
      end Process_Case_Recursive;

      V : Variable_Values := Values;
   begin
      pragma Assert (Values /= null or else Case_Stmt /= Empty_Node);

      --  Reset the variables
      V := Values;
      while V /= null loop
         V.Seen := False;
         V := V.Next;
      end loop;

      if Case_Stmt = Empty_Node then
         Case_Stmt := Create_Case_Construction (Project, Values, Empty_Node);
      end if;

      Process_Case_Recursive (Case_Stmt);
   end For_Each_Case_Stmt;

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

         procedure Add_Decl_Item (To_Case_Item : Project_Node_Id);
         --  Add Decl_Item to To_Case_Item.

         procedure Add_Decl_Item (To_Case_Item : Project_Node_Id) is
         begin
            Add_At_End (To_Case_Item, Decl_Item);
         end Add_Decl_Item;

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
                     V := Create_Scenario_Variable
                       (Project, Case_Variable_Reference_Of
                        (Current_Item_Node (Decl_Item)));
                     V.Next := Values;
                     Values := V;

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
                     For_Each_Case_Stmt
                       (Values, Project_Norm, Case_Stmt,
                        Add_Decl_Item'Unrestricted_Access);

                  --  Else create the case statement
                  elsif Values /= null then
                     For_Each_Case_Stmt
                       (Values, Project_Norm, Case_Stmt,
                        Add_Decl_Item'Unrestricted_Access);
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

   ---------------------------------
   -- For_Each_Scenario_Case_Item --
   ---------------------------------

   procedure For_Each_Scenario_Case_Item
     (Project : Prj.Tree.Project_Node_Id;
      Pkg     : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node;
      Action  : Matching_Item_Callback)
   is
      Values         : Variable_Values := null;
      Decl_Item, Top : Project_Node_Id;
      Nested_Case    : Project_Node_Id := Empty_Node;
   begin
      --  ??? This information should be transmitted through the kernel by the
      --  ??? scenario editor. In fact, we could even cache the current case
      --  ??? item there

      declare
         --  ??? This call is wrong in fact, since it doesn't fully reflect the
         --  ??? scenario in case some of the variables were defined in some
         --  ??? other package (even parent packages)
         Scenario_Variables : constant Project_Node_Array :=
           Find_Scenario_Variables (Project, Parse_Imported => False);
      begin
         for J in Scenario_Variables'Range loop
            String_To_Name_Buffer
              (External_Reference_Of (Scenario_Variables (J)));
            Values := new Variable_Values_Record'
              (Name     => External_Reference_Of (Scenario_Variables (J)),
               Type_Def => String_Type_Of (Scenario_Variables (J)),
               Values   => new Value_Array'
                 (1 => (Prj.Ext.Value_Of (Name_Find), In_Process)),
               Seen     => False,
               Next     => Values);
         end loop;
      end;

      --  Find the top-level item to which we will add

      if Pkg = Empty_Node then
         Top := Project_Declaration_Of (Project);
      else
         Top := Pkg;
      end if;

      --  No scenario variable ? This also means there is no nested case at
      --  all, so we can simply process the top once

      if Values = null then
         Action (Top);

      else
         Decl_Item := First_Declarative_Item_Of (Top);

         --  Find the case statement to which we should add. There is only such
         --  case construction per package or normalized project, so we stop as
         --  soon as we find it.

         while Decl_Item /= Empty_Node loop
            if Kind_Of (Current_Item_Node (Decl_Item))
              = N_Case_Construction
            then
               Nested_Case := Current_Item_Node (Decl_Item);
               For_Each_Case_Stmt (Values, Project, Nested_Case, Action);
               return;
            end if;
            Decl_Item := Next_Declarative_Item (Decl_Item);
         end loop;

         --  Create a case construction, since none was found

         For_Each_Case_Stmt (Values, Project, Nested_Case, Action);
         Add_At_End (Top, Nested_Case);
      end if;
   end For_Each_Scenario_Case_Item;

end Prj_Normalize;

