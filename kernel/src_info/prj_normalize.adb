-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Prj_API; use Prj_API;
with Traces;  use Traces;
with String_Utils; use String_Utils;
with Glide_Intl; use Glide_Intl;

with Prj;      use Prj;
with Prj.Ext;  use Prj.Ext;
with Prj.Tree; use Prj.Tree;
with Types;    use Types;
with Namet;    use Namet;
with Stringt;  use Stringt;

with Unchecked_Deallocation;

package body Prj_Normalize is

   Me : Debug_Handle := Create ("Prj_Normalize");

   type External_Variable_Value_Array_Access is access
     External_Variable_Value_Array;
   procedure Free is new Unchecked_Deallocation
     (External_Variable_Value_Array, External_Variable_Value_Array_Access);

   function Internal_Is_Normalized
     (Project           : Prj.Tree.Project_Node_Id;
      Node              : Prj.Tree.Project_Node_Id;
      Nested_Case_Names : External_Variable_Value_Array)
      return String;
   --  Internal version of Is_Normalized.
   --  Nested_Case_Names should contain the name of all the variables that are
   --  involved in the case constructions we are currently in.

   function Clone_Project (Project : Project_Node_Id) return Project_Node_Id;
   --  Return a duplicate of Project and its declarations. We do not duplicate
   --  declarative items.
   --  The new project is not independent of the old one, since most of the
   --  nodes are shared between the two for efficiency reasons.

   procedure Add_Value (To   : in out External_Variable_Value_Array_Access;
                        Last : in out Natural;
                        V    : External_Variable_Value);
   --  Add V to the array To. To is reallocated as necessary.
   --  Last is the index of the last item that was set in To.

   function Find_Case_Statement (Decl_List : Project_Node_Id)
      return Project_Node_Id;
   --  Return the first case statement in Decl_List, or Empty_Node if none.
   --  In a normalized project, this returns the only case statement that
   --  exists in a package or project.

   function Create_Case_Construction
     (Project       : Project_Node_Id;
      External_Name : String_Id;
      Var_Type      : Project_Node_Id)
      return Project_Node_Id;
   --  Return a N_Case_Construction for the external variable Name.
   --  The declaration for the variable itself is added at the beginning of the
   --  project if no variable was found that already referenced Name.

   function Values_Matches
     (Var_Name  : String_Id;
      Case_Item : Project_Node_Id;
      Values    : External_Variable_Value_Array) return Boolean;
   --  Return True if (Var_Name, Var_Value) is valid with regards to Values

   procedure Add_To_Case_Items
     (Case_Construction : Project_Node_Id;
      Decl_List         : Project_Node_Id);
   --  Copy all the declarative items from Decl_List into each of the case
   --  items of Case_Construction (at the beginning of each case item)

   procedure Set_Uniq_Type_Name
     (Project  : Project_Node_Id; Var_Type : Project_Node_Id);
   --  Set the name for the N_String_Type_Declaration Var_Type, so that it is
   --  uniq in the project.
   --  Var_Type shouldn't have been added to the project yet.

   ----------------------------
   -- Internal_Is_Normalized --
   ----------------------------

   function Internal_Is_Normalized
     (Project           : Prj.Tree.Project_Node_Id;
      Node              : Prj.Tree.Project_Node_Id;
      Nested_Case_Names : External_Variable_Value_Array)
      return String
   is
      Decl_List               : Project_Node_Id;
      Case_Construction_Found : Boolean := False;
      Standard_Node_Found     : Boolean := False;
      Case_Item               : Project_Node_Id;
      Var_Name                : String_Id;
      Var_Type, Current_Node  : Project_Node_Id;

   begin
      --  ??? This doesn't check that two references to the same external
      --  variable have the same type. In fact, this needs to be checked
      --  for a project and all its imported projects, so outside of this
      --  subprogram
      --
      --  Should check that an attribute isn't declared both in a case
      --  construction and in the common part.
      --
      --  Should check that all the scenario variables point to different
      --  types, so that they can be modified at will.
      --
      --  Should check that all instances of a scenario variable have the
      --  same default, or none. This isn't really important, however,
      --  since default values are used only once at the beginning by
      --  Glide.

      if Kind_Of (Node) = N_Project then
         Decl_List := First_Declarative_Item_Of
           (Project_Declaration_Of (Node));
      else
         Decl_List := First_Declarative_Item_Of (Node);
      end if;

      while Decl_List /= Empty_Node loop
         Current_Node := Current_Item_Node (Decl_List);

         case Kind_Of (Current_Node) is
            when N_Case_Construction =>
               if Case_Construction_Found then
                  Trace (Me, "Two case constructions at the same level");
                  return -"Two case constructions at the same level";
               end if;

               if Standard_Node_Found
                 and then Kind_Of (Node) = N_Case_Item
               then
                  Trace (Me, "Mix of case construction and items in a"
                         & " case item");
                  return -"Mix of case construction and items in a case item";
               end if;

               Var_Name := External_Variable_Name
                 (Project, Case_Variable_Reference_Of (Current_Node));
               Var_Type := String_Type_Of
                 (Case_Variable_Reference_Of (Current_Node));

               for J in Nested_Case_Names'Range loop
                  if String_Equal
                    (Nested_Case_Names (J).Variable_Name, Var_Name)
                  then
                     Trace (Me, "Duplicate variables in case constructions");
                     return -"Duplicate variables in case constructions";
                  end if;
               end loop;

               Case_Construction_Found := True;

               Case_Item := First_Case_Item_Of (Current_Node);

               while Case_Item /= Empty_Node loop
                  if First_Choice_Of (Case_Item) = Empty_Node then
                     Trace
                       (Me, "when others not allowed in normalized projects");
                     return -"""when others"" not allowed in normalized"
                       & "projects";
                  end if;

                  if Next_Literal_String (First_Choice_Of (Case_Item)) /=
                    Empty_Node
                  then
                     Trace (Me, "case items must have a single choice");
                     return -"Case items must have a single choice in"
                       & " normalized projects";
                  end if;

                  declare
                     S : constant String := Internal_Is_Normalized
                       (Project, Case_Item, Nested_Case_Names
                        & External_Variable_Value'
                        (Var_Type, Var_Name, No_String, False));

                  begin
                     if S /= "" then
                        Trace (Me, "One of the case items isn't normalized");
                        return S;
                     end if;
                  end;

                  Case_Item := Next_Case_Item (Case_Item);
               end loop;

            when N_Package_Declaration =>
               declare
                  S : constant String := Internal_Is_Normalized
                    (Project, Current_Node, Nested_Case_Names);
               begin
                  if S /= "" then
                     Trace (Me, "One of the packages isn't normalized");
                     return S;
                  end if;
               end;

            when others =>
               Standard_Node_Found := True;

               if Case_Construction_Found
                 and then Kind_Of (Node) = N_Case_Item
               then
                  Trace (Me, "Mix of case construction and items in a"
                         & " case item");
                  return -"Mix of case construction and items in a case item";
               end if;

               if Kind_Of (Current_Node) = N_Typed_Variable_Declaration
                 and then Is_External_Variable (Current_Node)
                 and then Kind_Of (Node) = N_Package_Declaration
               then
                  Trace (Me, "Scenario variable declared in package");
                  return -"Scenario variable declared in package";
               end if;
         end case;

         Decl_List := Next_Declarative_Item (Decl_List);
      end loop;

      return "";
   end Internal_Is_Normalized;

   -------------------
   -- Is_Normalized --
   -------------------

   function Is_Normalized
     (Project : Prj.Tree.Project_Node_Id) return String
   is
      No_Names : External_Variable_Value_Array (1 .. 0);
   begin
      if Debug_Mode then
         Assert (Me, Kind_Of (Project) = N_Project,
                 "Is_Normalized doesn't work for "
                 & Kind_Of (Project)'Img & " nodes");
      end if;

      return Internal_Is_Normalized (Project, Project, No_Names);
   end Is_Normalized;

   -------------------------
   -- Has_Been_Normalized --
   -------------------------

   function Has_Been_Normalized
     (Project : Prj.Tree.Project_Node_Id) return Boolean is
   begin
      return Tree_Private_Part.Project_Nodes.Table (Project).Location /=
        No_Location;
   end Has_Been_Normalized;

   ---------------------------
   -- Reset_Normalized_Flag --
   ---------------------------

   procedure Reset_Normalized_Flag (Project : Prj.Tree.Project_Node_Id) is
      Iter : Imported_Project_Iterator := Start (Project);
   begin
      while Current (Iter) /= Empty_Node loop
         Tree_Private_Part.Project_Nodes.Table (Current (Iter)).Location :=
           No_Location;
         Next (Iter);
      end loop;
   end Reset_Normalized_Flag;

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

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize
     (Root_Project : Project_Node_Id;
      Print_Error  : Prj.Put_Line_Access := null;
      Recurse      : Boolean := False)
   is
      Values       : External_Variable_Value_Array_Access := null;
      Last_Values  : Natural;
      --  Representation of the state of the case construction that is being
      --  parsed.
      --  Each time a new case item is seen in the original project, an entry
      --  is added into this array for all the possible values of the variable.
      --  On exit of the case item, the items are negated, so that it is still
      --  possible to process "when others".
      --  On exit of the case construction, the entries for the variable are
      --  removed from this list.

      Project_Norm : Project_Node_Id;
      Current_Pkg  : Project_Node_Id;

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
         Decl_Item, Current : Project_Node_Id := From;
         Next_Item, Choice : Project_Node_Id;
         Name      : String_Id;
         Case_Item : Project_Node_Id;
         Index     : Natural;
         Var_Type  : Project_Node_Id;
         Already_Have_Var, Match : Boolean;

         procedure Add_Decl_Item (To_Case_Item : Project_Node_Id);
         --  Add Decl_Item to To_Case_Item.

         procedure Add_Decl_Item (To_Case_Item : Project_Node_Id) is
         begin
            Add_At_End (To_Case_Item, Clone_Node (Decl_Item, True));
         end Add_Decl_Item;

      begin
         --  Nothing to do if there is no project
         if From = Empty_Node then
            return;
         end if;

         pragma Assert (Kind_Of (Decl_Item) = N_Declarative_Item);

         while Decl_Item /= Empty_Node loop
            Current := Current_Item_Node (Decl_Item);

            --  Save the next item, since the current item will be inserted in
            --  a different list, and thus its next field will be modified.

            Next_Item := Next_Declarative_Item (Decl_Item);
            Set_Next_Declarative_Item (Decl_Item, Empty_Node);

            case Kind_Of (Current) is
               when N_Package_Declaration =>
                  --  Skip subpackages, since these must appear after every
                  --  other declarative item in the normalized project.
                  null;

               when N_Case_Construction =>
                  Name := External_Variable_Name
                    (Project_Norm, Case_Variable_Reference_Of (Current));

                  if Name = No_String then
                     Trace (Me, "Normalizing a project with a non-scenario "
                            & "variable in case construction");
                     if Print_Error /= null then
                        Print_Error
                          (-"Case constructions referencing non-external"
                           & " variables can not be modified");
                     end if;
                     raise Normalize_Error;
                  end if;

                  Var_Type := String_Type_Of
                    (Case_Variable_Reference_Of (Current));

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
                     if String_Equal (Values (J).Variable_Name, Name) then
                        Already_Have_Var := True;
                        exit;
                     end if;
                  end loop;

                  Case_Item := First_Case_Item_Of (Current);

                  --  For all the case items in the current case construction

                  while Case_Item /= Empty_Node loop
                     Index := Last_Values + 1;

                     if Already_Have_Var then
                        Match := Values_Matches
                          (Name, Case_Item,
                           Values (Values'First .. Last_Values));

                     else
                        Match := True;
                        Choice := First_Choice_Of (Case_Item);
                        while Choice /= Empty_Node loop
                           Add_Value
                             (Values, Last_Values,
                              External_Variable_Value'
                              (Var_Type,
                               Name,
                               String_Value_Of (Choice),
                               False));
                           Choice := Next_Literal_String (Choice);
                        end loop;
                     end if;

                     if Match then
                        --  Process the declarative list of items

                        Process_Declarative_List
                          (First_Declarative_Item_Of (Case_Item),
                           To, Case_Stmt);

                        --  Negate all the values

                        for J in Index .. Last_Values loop
                           Values (J).Negated := True;
                        end loop;
                     end if;

                     Case_Item := Next_Case_Item (Case_Item);
                  end loop;

                  --  Remove all the entries for the variable in the array
                  --  Note that we do not need to use String_Equal, since we
                  --  know exactly the String_Id we started with.

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
                       (Current);
                  begin
                     --  Make sure that the type declaration is unique for that
                     --  typed variable, since if we decide to remove the
                     --  variable we should remove the type as well.

                     Var_Type := Clone_Node (String_Type_Of (Current), True);
                     Set_Uniq_Type_Name (Project_Norm, Var_Type);
                     Set_String_Type_Of (Current, Var_Type);
                     Add_At_End (Project_Norm, Var_Type,
                                 Add_Before_First_Case_Or_Pkg => True);

                     --  Scenario variables must be defined at the project
                     --  level
                     if Current_Pkg /= Empty_Node
                       and then Is_External_Variable (Current)
                     then
                        Add_At_End
                          (Project_Norm,
                           Clone_Node (Decl_Item, True),
                           Add_Before_First_Case_Or_Pkg => True);
                     else
                        For_Each_Matching_Case_Item
                          (Project_Norm, Current_Pkg,
                           Values (Values'First .. Last_Values),
                           Add_Decl_Item'Unrestricted_Access);
                     end if;

                     Set_String_Type_Of (Current, Save_Type);
                  end;


               when N_String_Type_Declaration =>
                  null;

               when others =>
                  For_Each_Matching_Case_Item
                    (Project_Norm, Current_Pkg,
                     Values (Values'First .. Last_Values),
                     Add_Decl_Item'Unrestricted_Access);
            end case;

            Decl_Item := Next_Item;
         end loop;
      end Process_Declarative_List;

      Decl, Case_Stmt : Project_Node_Id;
      Iter : Imported_Project_Iterator := Start (Root_Project, Recurse);
      Project : Project_Node_Id;

   begin
      while Current (Iter) /= Empty_Node loop
         Project := Current (Iter);
         Values := new External_Variable_Value_Array (1 .. 50);
         Last_Values := Values'First - 1;

         Project_Norm := Clone_Project (Project);
         Current_Pkg := Empty_Node;

         --  Memorize the project as normalized, to avoid recursions
         Tree_Private_Part.Project_Nodes.Table (Project).Location :=
           Standard_Location;

         --  The top-level part of the project
         Case_Stmt := Empty_Node;

         Process_Declarative_List
           (From => First_Declarative_Item_Of
              (Project_Declaration_Of (Project)),
            To   => Project_Declaration_Of (Project_Norm),
            Case_Stmt => Case_Stmt);

         if Last_Values /= Values'First - 1 then
            Free (Values);
            raise Normalize_Error;
         end if;

         --  All the subpackages

         Current_Pkg := First_Package_Of (Project);

         while Current_Pkg /= Empty_Node loop
            Decl := First_Declarative_Item_Of (Current_Pkg);
            Set_First_Declarative_Item_Of (Current_Pkg, Empty_Node);

            Case_Stmt := Empty_Node;
            Process_Declarative_List
              (From      => Decl,
               To        => Current_Pkg,
               Case_Stmt => Case_Stmt);

            if Last_Values /= Values'First - 1 then
               Free (Values);
               raise Normalize_Error;
            end if;

            Add_At_End (Project_Declaration_Of (Project_Norm), Current_Pkg);
            Current_Pkg := Next_Package_In_Project (Current_Pkg);
         end loop;

         Free (Values);

         Post_Process_After_Clone (Project_Norm);

         --  Directly replace in the table, so that all references to this
         --  project are automatically updated. There is a small memory leak,
         --  but since most of the project tree is shared, it doesn't really
         --  matter in the life of the project editor

         Tree_Private_Part.Project_Nodes.Table (Project) :=
           Tree_Private_Part.Project_Nodes.Table (Project_Norm);

         Next (Iter);
      end loop;
   end Normalize;

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

   ---------------------------------
   -- For_Each_Scenario_Case_Item --
   ---------------------------------

   procedure For_Each_Scenario_Case_Item
     (Project : Prj.Tree.Project_Node_Id;
      Pkg     : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node;
      Scenario_Variables : Project_Node_Array;
      Action  : Matching_Item_Callback)
   is
      Values : External_Variable_Value_Array (1 .. Scenario_Variables'Length);
      Last_Values : Natural := Values'First - 1;
   begin
      for J in Scenario_Variables'Range loop
         String_To_Name_Buffer
           (External_Reference_Of (Scenario_Variables (J)));
         Last_Values := Last_Values + 1;
         Values (Last_Values) := External_Variable_Value'
           (Variable_Type  => String_Type_Of (Scenario_Variables (J)),
            Variable_Name  => External_Reference_Of (Scenario_Variables (J)),
            Variable_Value => Prj.Ext.Value_Of (Name_Find),
            Negated        => False);
      end loop;
      For_Each_Matching_Case_Item (Project, Pkg, Values, Action);
   end For_Each_Scenario_Case_Item;

   -------------------------
   -- Find_Case_Statement --
   -------------------------

   function Find_Case_Statement (Decl_List : Project_Node_Id)
      return Project_Node_Id
   is
      Decl_Item : Project_Node_Id := Decl_List;
   begin
      while Decl_Item /= Empty_Node loop
         if Kind_Of (Current_Item_Node (Decl_Item)) = N_Case_Construction then
            return Current_Item_Node (Decl_Item);
         end if;
         Decl_Item := Next_Declarative_Item (Decl_Item);
      end loop;
      return Empty_Node;
   end Find_Case_Statement;

   --------------------
   -- Values_Matches --
   --------------------

   function Values_Matches
     (Var_Name  : String_Id;
      Case_Item : Project_Node_Id;
      Values    : External_Variable_Value_Array) return Boolean
   is
      --  The rule is the following: if there is any non-negated item,
      --  then we must match at least one of them. If there are none,
      --  then the case item matches if non of the negated item matches
      Match  : Boolean := True;
      Choice : Project_Node_Id := First_Choice_Of (Case_Item);
   begin
      Choice_Loop :
      while Choice /= Empty_Node loop
         for J in Values'Range loop
            if String_Equal (Values (J).Variable_Name, Var_Name) then
               --  Change the default value if needed
               Match := Values (J).Negated;

               if String_Equal
                 (Values (J).Variable_Value, String_Value_Of (Choice))
               then
                  Match := not Values (J).Negated;
                  exit Choice_Loop;
               end if;
            end if;
         end loop;

         Choice := Next_Literal_String (Choice);
      end loop Choice_Loop;

      return Match;
   end Values_Matches;

   ---------------------------------
   -- For_Each_Matching_Case_Item --
   ---------------------------------

   procedure For_Each_Matching_Case_Item
     (Project : Prj.Tree.Project_Node_Id;
      Pkg     : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node;
      Values  : External_Variable_Value_Array;
      Action  : Matching_Item_Callback)
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
         Match    : Boolean;
         New_Case : Project_Node_Id;
      begin
         for J in Values'Range loop
            Match := False;
            for K in Var_Seen'First .. Last_Var_Seen loop
               if String_Equal
                 (Values (J).Variable_Name, Var_Seen (K).Variable_Name)
               then
                  Match := True;
                  exit;
               end if;
            end loop;

            if not Match then
               New_Case := Create_Case_Construction
                 (Project, Values (J).Variable_Name, Values (J).Variable_Type);
               return New_Case;
            end if;
         end loop;
         return Empty_Node;
      end Create_Case_If_Necessary;

      ----------------------------
      -- Process_Case_Recursive --
      ----------------------------

      procedure Process_Case_Recursive (Case_Stmt : Project_Node_Id) is
         Name : constant String_Id := External_Variable_Name
           (Project, Case_Variable_Reference_Of (Case_Stmt));
         Current_Item, New_Case : Project_Node_Id;
         Handling_Done : Boolean;

      begin
         pragma Assert (Name /= No_String);

         --  Memorise the name of the variable we are processing, so that we
         --  can create missing case constructions at the end

         Add_Value (Var_Seen, Last_Var_Seen,
                    (Empty_Node, Name, No_String, False));

         --  For all possible values of the variable

         Current_Item := First_Case_Item_Of (Case_Stmt);
         while Current_Item /= Empty_Node loop
            if Values_Matches (Name, Current_Item, Values) then
               Handling_Done := False;
               New_Case := First_Declarative_Item_Of (Current_Item);

               --  Are there any nested case ?
               while New_Case /= Empty_Node loop
                  if Kind_Of (Current_Item_Node (New_Case))
                    = N_Case_Construction
                  then
                     Process_Case_Recursive (Current_Item_Node (New_Case));
                     Handling_Done := True;
                     exit;
                  end if;

                  New_Case := Next_Declarative_Item (New_Case);
               end loop;

               if not Handling_Done then
                  New_Case := Create_Case_If_Necessary;
                  Handling_Done := New_Case /= Empty_Node;

                  if Handling_Done then
                     --  Move all the declarative items currently in the case
                     --  item to the nested case construction, so that we only
                     --  have declarative items in the most-nested case
                     --  constructions.
                     if First_Declarative_Item_Of (Current_Item) /=
                       Empty_Node
                     then
                        Add_To_Case_Items
                          (New_Case, First_Declarative_Item_Of (Current_Item));
                        Set_First_Declarative_Item_Of
                          (Current_Item, Empty_Node);
                     end if;

                     Add_At_End (Current_Item, New_Case);
                     Process_Case_Recursive (New_Case);
                  end if;
               end if;

               --  We can now report the matching case item
               if not Handling_Done and then Action /= null then
                  Action (Current_Item);
               end if;
            end if;

            Current_Item := Next_Case_Item (Current_Item);
         end loop;

         Last_Var_Seen := Last_Var_Seen - 1;
      end Process_Case_Recursive;

      Case_Construct : Project_Node_Id;
      Top : Project_Node_Id;
   begin
      if Pkg /= Empty_Node then
         Top := Pkg;
      else
         Top := Project_Declaration_Of (Project);
      end if;

      Case_Construct := Find_Case_Statement (First_Declarative_Item_Of (Top));
      if Case_Construct = Empty_Node then
         Case_Construct := Create_Case_If_Necessary;
         if Case_Construct /= Empty_Node then
            Add_At_End (Top, Case_Construct);
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
   -- Add_To_Case_Items --
   -----------------------

   procedure Add_To_Case_Items
     (Case_Construction : Project_Node_Id;
      Decl_List         : Project_Node_Id)
   is
      Case_Item : Project_Node_Id;
   begin
      Case_Item := First_Case_Item_Of (Case_Construction);
      while Case_Item /= Empty_Node loop
         Add_In_Front (Case_Item, Clone_Node (Decl_List, True));
         Case_Item := Next_Case_Item (Case_Item);
      end loop;
   end Add_To_Case_Items;

   ------------------------
   -- Get_Uniq_Type_Name --
   ------------------------

   procedure Set_Uniq_Type_Name
     (Project  : Project_Node_Id;
      Var_Type : Project_Node_Id)
   is
      Candidate : Name_Id;
      Attempt   : Natural := 1;
   begin
      --  Check the type itself
      Candidate := Name_Of (Var_Type);

      while Find_Type_Declaration (Project, Candidate) /= Empty_Node loop
         Get_Name_String (Candidate);

         Get_Name_String (Name_Of (Var_Type));
         Add_Str_To_Name_Buffer (Image (Attempt));
         Attempt := Attempt + 1;

         Candidate := Name_Find;
      end loop;

      Set_Name_Of (Var_Type, Candidate);
   end Set_Uniq_Type_Name;

   ------------------------------
   -- Create_Case_Construction --
   ------------------------------

   function Create_Case_Construction
     (Project       : Project_Node_Id;
      External_Name : String_Id;
      Var_Type      : Project_Node_Id)
      return Project_Node_Id
   is
      Construct, Str, S : Project_Node_Id;
      Item : Project_Node_Id := Empty_Node;
      Ref : String_Id;
      Decl : Project_Node_Id;
      New_Type : Project_Node_Id;
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
        (Project_Declaration_Of (Project));
      while Decl /= Empty_Node loop
         Item := Current_Item_Node (Decl);
         if Kind_Of (Item) = N_Typed_Variable_Declaration then
            Ref := External_Reference_Of (Item);
            exit when Ref /= No_String
              and then String_Equal (Ref, External_Name);
         end if;
         Item := Empty_Node;
         Decl := Next_Declarative_Item (Decl);
      end loop;

      --  If not, add the variable and its expression

      if Item = Empty_Node then
         String_To_Name_Buffer (External_Name);
         Item := Create_Typed_Variable
           (Project, Name_Buffer (1 .. Name_Len), Var_Type,
            Add_Before_First_Case_Or_Pkg => True);
         String_To_Name_Buffer (External_Name);
         Set_Value_As_External (Item, Name_Buffer (1 .. Name_Len));

         --  Make sure the type is only used for that variable, so that is can
         --  be freely modified. If we already have a type by the same name,
         --  find a new name.

         New_Type := Clone_Node (Var_Type, True);
         Set_Uniq_Type_Name (Project, New_Type);
         Set_String_Type_Of (Item, New_Type);
         Add_In_Front (Project, New_Type);
      end if;

      Construct := Default_Project_Node (N_Case_Construction);
      Set_Case_Variable_Reference_Of
        (Construct, Create_Variable_Reference (Item));

      Str := First_Literal_String (Var_Type);
      while Str /= Empty_Node loop
         --  Construct the case item, and add it to the list of
         --  case items
         Item := Default_Project_Node (N_Case_Item);
         S := Default_Project_Node (N_Literal_String);
         Set_String_Value_Of (S, String_Value_Of (Str));
         Set_First_Choice_Of (Item, S);
         Set_Next_Case_Item (Item, First_Case_Item_Of (Construct));
         Set_First_Case_Item_Of (Construct, Item);

         Str := Next_Literal_String (Str);
      end loop;

      return Construct;
   end Create_Case_Construction;

end Prj_Normalize;
