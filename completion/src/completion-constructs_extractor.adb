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

with Ada_Analyzer.Utils; use Ada_Analyzer.Utils;
with Language.Ada;       use Language.Ada;

package body Completion.Constructs_Extractor is

   ---------------------------------------
   -- New_Construct_Completion_Resolver --
   ---------------------------------------

   function New_Construct_Completion_Resolver (Tree : Construct_Tree_Access)
      return Construct_Completion_Resolver
   is
   begin
      return (Manager => null, Tree => Tree);
   end New_Construct_Completion_Resolver;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Proposal : Construct_Completion_Proposal) return String
   is
   begin
      if Proposal.Is_All then
         return "all";
      else
         return Get_Construct (Proposal.Tree_Node).Name.all;
      end if;
   end Get_Name;

   --------------
   -- Get_Type --
   --------------

   function Get_Category (Proposal : Construct_Completion_Proposal)
     return Language_Category
   is
   begin
      if Proposal.Is_All then
         return Cat_Unknown;
      else
         return Get_Construct (Proposal.Tree_Node).Category;
      end if;
   end Get_Category;

   --------------------
   -- Get_Compositon --
   --------------------

   function Get_Composition
     (Proposal : Construct_Completion_Proposal; Offset : Positive)
      return Completion_List
   is
      Tree : constant Construct_Tree_Access :=
               Construct_Completion_Resolver (Proposal.Resolver.all).Tree;

      Parent_Sloc_Start, Parent_Sloc_End : Source_Location;
      Parent_Found                       : Boolean;

      function Get_Composition_Of_Type
        (Type_Iterator : Construct_Tree_Iterator; Recursive : Boolean := True)
         return Completion_List;

      -----------------------------
      -- Get_Composition_Of_Type --
      -----------------------------

      function Get_Composition_Of_Type
        (Type_Iterator : Construct_Tree_Iterator; Recursive : Boolean := True)
         return Completion_List
      is
         Child_Iterator : Construct_Tree_Iterator;
         Result         : Completion_List;
      begin
         Get_Referenced_Entity
           (Ada_Lang,
            Get_Buffer (Get_Resolver (Proposal).Manager.all).all,
            Get_Construct (Type_Iterator).all,
            Parent_Sloc_Start,
            Parent_Sloc_End,
            Parent_Found);

         if Recursive and then Parent_Found then
            declare
               Parent_Name : constant String :=
                 Get_Buffer (Get_Resolver (Proposal).Manager.all).all
                 (Parent_Sloc_Start.Index .. Parent_Sloc_End.Index);

               Parents : constant Construct_Tree_Iterator_Array :=
                           Get_Visible_Constructs
                             (Tree.all, Type_Iterator, Parent_Name);
            begin
               if Parents'Length >= 1 then
                  Concat (Result, Get_Composition_Of_Type
                    (Parents (1),
                     not Is_Access
                         (Get_Buffer (Get_Resolver (Proposal).Manager.all).all,
                          Get_Construct (Type_Iterator).all)));
               end if;
            end;
         end if;

         Child_Iterator := Next (Tree.all, Type_Iterator, Jump_Into);

         while
           Get_Parent_Scope (Tree.all, Child_Iterator) = Type_Iterator
         loop
            Append
              (Result,
               Construct_Completion_Proposal'
                 (Show_Identifiers,
                  Get_Resolver (Proposal),
                  Child_Iterator,
                  False));
               Child_Iterator := Next (Tree.all, Child_Iterator, Jump_Over);
         end loop;

         if Is_Access
           (Get_Buffer (Get_Resolver (Proposal).Manager.all).all,
            Get_Construct (Type_Iterator).all)
           and then not Proposal.Is_All
         then
            Append
              (Result,
               Construct_Completion_Proposal'
                 (Show_Identifiers,
                  Get_Resolver (Proposal),
                  Proposal.Tree_Node,
                  True));
         end if;

         return Result;
      end Get_Composition_Of_Type;

   begin
      case Get_Construct (Proposal.Tree_Node).Category is
         when Cat_Variable | Cat_Local_Variable | Cat_Field =>
            Get_Referenced_Entity
              (Ada_Lang,
               Get_Buffer (Get_Resolver (Proposal).Manager.all).all,
               Get_Construct (Proposal.Tree_Node).all,
               Parent_Sloc_Start,
               Parent_Sloc_End,
               Parent_Found);

            declare
               Id : constant Composite_Identifier := To_Composite_Identifier
                 (Get_Buffer (Get_Resolver (Proposal).Manager.all).all
                    (Parent_Sloc_Start.Index .. Parent_Sloc_End.Index));
               Type_Iterator  : Construct_Tree_Iterator;

               Visible_Types  : constant Construct_Tree_Iterator_Array :=
                 Get_Visible_Constructs
                   (Tree.all,
                    Get_Construct (Proposal.Tree_Node).Sloc_Start.Index, Id);
            begin
               if Visible_Types'Length >= 1 then
                  Type_Iterator := Visible_Types (1);

                  return Get_Composition_Of_Type (Type_Iterator);
               else
                  --  If type is not found in the current file, then we
                  --  have to look elsewhere

                  declare
                     Resolver    : constant Completion_Resolver_Access :=
                                     Next (Proposal.Resolver);
                     Completions : Completion_List;
                     It          : Completion_Iterator;
                     Result      : Completion_List;
                  begin
                     if Resolver /= null then
                        Completions := Get_Possibilities
                          (Next (Proposal.Resolver),
                           To_String (Id),
                           False,
                           Get_Construct (Proposal.Tree_Node).
                             Sloc_Start.Index,
                           All_Visible_Entities);

                        It := First (Completions);

                        while It /= Null_Completion_Iterator loop
                           Concat
                             (Result, Get_Composition
                                (Get_Proposal (It), 1));

                           It := Next (It);
                        end loop;

                        return Result;
                     end if;
                  end;
               end if;
            end;

         when Cat_Package =>
            declare
               Spec_It         : Construct_Tree_Iterator;
               Body_It         : Construct_Tree_Iterator;
               Child_Iterator  : Construct_Tree_Iterator;
               Result          : Completion_List;
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
                     Append
                       (Result,
                        Construct_Completion_Proposal'
                          (Show_Identifiers,
                           Get_Resolver (Proposal),
                           Child_Iterator,
                           False));
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
                     Append
                       (Result,
                        Construct_Completion_Proposal'
                          (Show_Identifiers,
                           Get_Resolver (Proposal),
                           Child_Iterator,
                           False));

                     Child_Iterator := Next
                       (Tree.all, Child_Iterator, Jump_Over);
                  end loop;
               end if;

               return Result;
            end;
         when others =>
            null;
      end case;

      return Null_Completion_List;
   end Get_Composition;

   ------------------------------
   -- Get_Number_Of_Parameters --
   ------------------------------

   function Get_Number_Of_Parameters (Proposal : Construct_Completion_Proposal)
     return Natural
   is
      pragma Unreferenced (Proposal);
   begin
      return 0;
   end Get_Number_Of_Parameters;

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

   function Get_Possibilities
     (Resolver   : access Construct_Completion_Resolver;
      Identifier : String;
      Is_Partial : Boolean;
      Offset     : Natural;
      Filter     : Possibilities_Filter) return Completion_List
   is
      Result       : Completion_List;
      Result_Array : constant Construct_Tree_Iterator_Array :=
                       Get_Visible_Constructs
                         (Resolver.Tree.all, Offset, Identifier,
                          True, Is_Partial);
   begin
      if (Filter and All_Visible_Entities) /= 0 then
         for J in Result_Array'Range loop
            Append
              (Result, Construct_Completion_Proposal'
                 (Show_Identifiers,
                  Completion_Resolver_Access (Resolver),
                  Result_Array (J),
                  False));
         end loop;
      end if;

      return Result;
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
