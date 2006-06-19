-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
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

with Ada_Analyzer.Utils;     use Ada_Analyzer.Utils;
with Language.Ada;           use Language.Ada;
with Language.Documentation; use Language.Documentation;

package body Completion.Constructs_Extractor is

   use Completion_List_Pckg;
   use Completion_List_Extensive_Pckg;
   use Extensive_List_Pckg;

   ---------------------------------------
   -- New_Construct_Completion_Resolver --
   ---------------------------------------

   function New_Construct_Completion_Resolver
     (Tree : Construct_Tree_Access; Current_File : Virtual_File)
      return Construct_Completion_Resolver
   is
   begin
      return (Manager => null, Tree => Tree, Current_File => Current_File);
   end New_Construct_Completion_Resolver;

   --------------------
   -- Get_Completion --
   --------------------

   function Get_Completion
     (Proposal : Construct_Completion_Proposal) return UTF8_String is
   begin
      if Proposal.Is_All then
         return "all";
      else
         return Get_Construct (Proposal.Tree_Node).Name.all;
      end if;
   end Get_Completion;

   --------------
   -- Get_Type --
   --------------

   function Get_Category
     (Proposal : Construct_Completion_Proposal) return Language_Category
   is
   begin
      if Proposal.Is_All then
         return Cat_Unknown;
      else
         return Get_Construct (Proposal.Tree_Node).Category;
      end if;
   end Get_Category;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Proposal : Construct_Completion_Proposal) return UTF8_String is
   begin
      return Get_Documentation
        (Ada_Lang,
         Get_Buffer (Proposal.Resolver.Manager.all).all,
         Construct_Completion_Resolver (Proposal.Resolver.all).Tree.all,
         Proposal.Tree_Node);
   end Get_Documentation;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location
     (Proposal : Construct_Completion_Proposal) return File_Location is
   begin
      return (Construct_Completion_Resolver
              (Proposal.Resolver.all).Current_File,
              Get_Construct (Proposal.Tree_Node).Sloc_Start.Line,
              Visible_Column_Type
                (Get_Construct (Proposal.Tree_Node).Sloc_Start.Column));
   end Get_Location;

   --------------------
   -- Get_Compositon --
   --------------------

   procedure Get_Composition
     (Proposal   : Construct_Completion_Proposal;
      Identifier : String;
      Offset     : Positive;
      Is_Partial : Boolean;
      Result     : in out Completion_List)
   is
      Tree : constant Construct_Tree_Access :=
               Construct_Completion_Resolver (Proposal.Resolver.all).Tree;

      Parent_Sloc_Start, Parent_Sloc_End : Source_Location;
      Parent_Found                       : Boolean;

      procedure Add_To_List
        (List      : in out Extensive_List_Pckg.List;
         Tree_Node : Construct_Tree_Iterator;
         Is_All    : Boolean := False;
         Name      : String := "");

      procedure Add_To_List
        (List      : in out Extensive_List_Pckg.List;
         Tree_Node : Construct_Tree_Iterator;
         Is_All    : Boolean := False;
         Name      : String := "") is
      begin
         if (Name = ""
             and then Match
               (Identifier,
                Get_Construct (Tree_Node).Name.all,
                Is_Partial))
           or else Match
             (Identifier,
              Name,
              Is_Partial)
         then
            Append
              (List,
               Construct_Completion_Proposal'
                 (Show_Identifiers,
                  Get_Resolver (Proposal),
                  Tree_Node,
                  Is_All,
                  0));
         end if;
      end Add_To_List;

      List : Extensive_List_Pckg.List;

   begin
      case Get_Construct (Proposal.Tree_Node).Category is
         when Cat_Variable | Cat_Local_Variable | Cat_Field | Cat_Parameter
            | Cat_Class .. Cat_Subtype | Subprogram_Category =>

            if Get_Construct (Proposal.Tree_Node).Category in
              Subprogram_Category
              and then Proposal.Params_In_Expression
                > Get_Number_Of_Parameters (Proposal)
            then
               return;
            end if;

            Get_Referenced_Entity
              (Ada_Lang,
               Get_Buffer (Get_Resolver (Proposal).Manager.all).all,
               Get_Construct (Proposal.Tree_Node).all,
               Parent_Sloc_Start,
               Parent_Sloc_End,
               Parent_Found);

            declare
               List : Completion_List := Get_Initial_Completion_List
                 (Get_Resolver (Proposal).Manager.all,
                  Parent_Sloc_End.Index,
                  False);

               It : Completion_Iterator := First (List);

            begin
               while not At_End (It) loop
                  Get_Composition
                    (Get_Proposal (It),
                     Identifier,
                     1,
                     Is_Partial,
                     Result);

                  Next (It);
               end loop;

               Free (List);
            end;

            if Is_Access
              (Get_Buffer (Get_Resolver (Proposal).Manager.all).all,
               Get_Construct (Proposal.Tree_Node).all)
              and then not Proposal.Is_All
            then
               Add_To_List (List, Proposal.Tree_Node, True, "all");
            end if;

            if Get_Construct (Proposal.Tree_Node).Category
               in Cat_Class .. Cat_Subtype
            then
               declare
                  Child_Iterator : Construct_Tree_Iterator;
                  Type_Iterator  : constant Construct_Tree_Iterator
                    := Proposal.Tree_Node;
               begin
                  Child_Iterator := Next (Tree.all, Type_Iterator, Jump_Into);

                  while
                    Get_Parent_Scope (Tree.all, Child_Iterator) = Type_Iterator
                  loop
                     Add_To_List (List, Child_Iterator, False);
                     Child_Iterator := Next
                       (Tree.all, Child_Iterator, Jump_Over);
                  end loop;
               end;
            end if;

            Append (Result.List, To_Extensive_List (List));

         when Cat_Package =>
            declare
               Spec_It         : Construct_Tree_Iterator;
               Body_It         : Construct_Tree_Iterator;
               Child_Iterator  : Construct_Tree_Iterator;
               Spec_Visibility : Boolean;
               Body_Visibility : Boolean;
            begin
               Spec_It := Get_Spec (Tree.all, Proposal.Tree_Node);
               Body_It := Get_First_Body (Tree.all, Proposal.Tree_Node);

               Body_Visibility :=
                 Get_Construct (Body_It).Sloc_Start.Index <= Offset
                 and then Get_Construct
                   (Body_It).Sloc_End.Index >= Offset;

               Spec_Visibility :=
                 Body_Visibility
                 or else (Get_Construct (Spec_It).Sloc_Start.Index <= Offset
                          and then Get_Construct
                            (Spec_It).Sloc_End.Index >= Offset);

               Child_Iterator := Next (Tree.all, Spec_It, Jump_Into);

               --  Add the entities from the specification

               while
                 Get_Parent_Scope (Tree.all, Child_Iterator) = Spec_It
               loop
                  if Spec_Visibility
                    or else
                      Get_Construct (Child_Iterator).Visibility
                      = Visibility_Public
                  then
                     Add_To_List (List, Child_Iterator, False);
                  end if;

                  Child_Iterator := Next
                    (Tree.all, Child_Iterator, Jump_Over);
               end loop;

               --  If we have to add the entries in the body if needed

               if Body_Visibility and then Body_It /= Spec_It then
                  Child_Iterator := Next (Tree.all, Body_It, Jump_Into);

                  --  Add the entities from the specification

                  while
                    Get_Parent_Scope (Tree.all, Child_Iterator) = Body_It
                  loop
                     Add_To_List (List, Child_Iterator, False);

                     Child_Iterator := Next
                       (Tree.all, Child_Iterator, Jump_Over);
                  end loop;
               end if;

               Append (Result.List, To_Extensive_List (List));

               return;
            end;

         when others =>
            null;
      end case;
   end Get_Composition;

   ------------------------------
   -- Get_Number_Of_Parameters --
   ------------------------------

   function Get_Number_Of_Parameters
     (Proposal : Construct_Completion_Proposal) return Natural
   is
      Resolver : Construct_Completion_Resolver :=
        Construct_Completion_Resolver (Proposal.Resolver.all);
      Total : Integer := 0;
      It    : Construct_Tree_Iterator :=
        Next (Resolver.Tree.all, Proposal.Tree_Node, Jump_Into);
   begin
      while Get_Parent_Scope (Resolver.Tree.all, It) = Proposal.Tree_Node loop
         if Get_Construct (It).Category = Cat_Parameter then
            Total := Total + 1;
         end if;

         It := Next (Resolver.Tree.all, It, Jump_Over);
      end loop;

      return Total;
   end Get_Number_Of_Parameters;

   -----------------------
   -- Append_Expression --
   -----------------------

   procedure Append_Expression
     (Proposal             : in out Construct_Completion_Proposal;
      Number_Of_Parameters : Natural)
   is
   begin
      Proposal.Params_In_Expression := Number_Of_Parameters;
   end Append_Expression;

   ----------
   -- Free --
   ----------

   procedure Free (Proposal : in out Construct_Completion_Proposal) is
      pragma Unreferenced (Proposal);
   begin
      null;
   end Free;

   -----------------------
   -- Get_Possibilities --
   -----------------------

   procedure Get_Possibilities
     (Resolver   : access Construct_Completion_Resolver;
      Identifier : String;
      Is_Partial : Boolean;
      Offset     : Integer;
      Filter     : Possibilities_Filter;
      Result     : in out Completion_List)
   is
   begin
      if Offset <= 0 then
         return;
      end if;

      declare
         Result_Array : constant Construct_Tree_Iterator_Array :=
           Get_Visible_Constructs
             (Resolver.Tree.all, Offset, Identifier,
              True, Is_Partial);

         List         : Extensive_List_Pckg.List;
      begin
         Result.Searched_Identifier := new String'(Identifier);

         if (Filter and All_Visible_Entities) /= 0 then
            for J in Result_Array'Range loop
               Append
                 (List,
                  Construct_Completion_Proposal'
                    (Show_Identifiers,
                     Completion_Resolver_Access (Resolver),
                     Result_Array (J),
                     False,
                     0));
            end loop;

            Append (Result.List, To_Extensive_List (List));
         end if;
      end;
   end Get_Possibilities;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Construct_Completion_Resolver) is
      pragma Unreferenced (This);
   begin
      null;
   end Free;

end Completion.Constructs_Extractor;
