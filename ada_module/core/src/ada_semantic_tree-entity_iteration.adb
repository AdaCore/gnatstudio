------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Language.Ada;                   use Language.Ada;
with Ada_Semantic_Tree.Lang;         use Ada_Semantic_Tree.Lang;
with Ada_Semantic_Tree.Parts;        use Ada_Semantic_Tree.Parts;
with Ada_Semantic_Tree.Type_Tree;    use Ada_Semantic_Tree.Type_Tree;
with Ada_Semantic_Tree.Declarations; use Ada_Semantic_Tree.Declarations;

package body Ada_Semantic_Tree.Entity_Iteration is

   procedure Initialize_Body_Entity (It : in out Semantic_Tree_Iterator);
   --  Initializes the body entity, and moves the cursor to the spec entity
   --  if currently set to the body. This applies only on packages, tasks and
   --  protected types.

   -------------------------------
   -- To_Semantic_Tree_Iterator --
   -------------------------------

   function To_Semantic_Tree_Iterator
     (Info                 : Semantic_Information;
      From_Visibility      : Visibility_Context;
      References_To_Follow : References_To_Follow_Array := All_References;
      Excluded_Entities    : Excluded_Stack_Type := Null_Excluded_Stack;
      Ignored_Expressions  : Expressions_List.List :=
        Expressions_List.Empty_List)
      return Semantic_Tree_Iterator
   is
      Result        : Semantic_Tree_Iterator;
      Gen_Info      : Instance_Info;
      Real_Sem_Info : Semantic_Information;
      Inst_Entity   : Entity_Access;
   begin
      Real_Sem_Info := Info;

      if Info.Entity = Null_Entity_Access then
         return Null_Semantic_Tree_Iterator;
      end if;

      if Is_Generic_Instance (Info.Entity) then
         Gen_Info := Get_Generic_Instance_Information (Info.Entity)
           & Info.Generic_Context;
      else
         Gen_Info := Info.Generic_Context;
      end if;

      --  Loop over all of the generic definitions to find the first actual
      --  parameter

      while Get_Construct (Real_Sem_Info.Entity).Is_Generic_Spec loop
         if Gen_Info = Null_Instance_Info then
            --  In this case, we've got a generic spec - but there isn't any
            --  generic context yet. We need to look at all the know
            --  instanciations of the enclosing package, and see if there's
            --  visible through a use clause.

            declare
               It            : Clause_Iterator :=
                 To_Clause_Iterator (From_Visibility, Cat_Use);
               Entity        : Entity_Access;
               Instance      : Instance_Info;
               Gen_Package   : Entity_Access;
               Enclosing_Package : Entity_Access;
            begin
               Enclosing_Package := To_Entity_Access
                 (Get_File (Real_Sem_Info.Entity),
                  Get_Parent_Scope
                    (Get_Tree (Get_File (Real_Sem_Info.Entity)),
                     To_Construct_Tree_Iterator (Real_Sem_Info.Entity)));

               while not At_End (It) loop
                  Entity := Resolve_Package (It);

                  Instance := Get_Generic_Instance_Information (Entity);

                  if Instance /= Null_Instance_Info then
                     Gen_Package :=
                       Get_First_Occurence
                         (Get_Generic_Entity (Instance));

                     if Gen_Package = Enclosing_Package then
                        Gen_Info := Get_Generic_Context (It)
                          & Instance
                          & Info.Generic_Context;

                        exit;
                     end if;
                  end if;

                  --  If this instance info is not used, then free it by a
                  --  ref / unref sequence

                  Ref (Instance);
                  Unref (Instance);

                  Prev (It);
               end loop;
            end;
         end if;

         if Gen_Info /= Null_Instance_Info then
            --  If we're iterating over a generic parameter in the context of
            --  an instance, then resolve the actual parameter instead of using
            --  the formal one.

            Inst_Entity :=
              Get_Actual_For_Generic_Param
                (Gen_Info, Real_Sem_Info.Entity);

            if Inst_Entity /= Null_Entity_Access then
               Real_Sem_Info.Entity := Inst_Entity;
            else
               exit;
            end if;
         else
            exit;
         end if;
      end loop;

      Result.Root_Entity := Real_Sem_Info;
      Result.Db := Get_Database (Get_File (Real_Sem_Info.Entity));
      Result.Initial_File := Get_File (Real_Sem_Info.Entity);
      Result.Current_File := Result.Initial_File;
      Result.Current_Construct :=
        To_Construct_Tree_Iterator (Real_Sem_Info.Entity);
      Result.Step_Has_Started := False;
      Result.References_To_Follow := References_To_Follow;
      Result.From_Visibility := From_Visibility;
      Result.Generic_Context := Gen_Info;
      Ref (Result.Generic_Context);

      Result.Excluded_Entities := Excluded_Entities;
      Ref (Result.Excluded_Entities);
      Result.Ignored_Expressions := Ignored_Expressions;

      Push_Entity (Result.Excluded_Entities, Real_Sem_Info.Entity);

      Initialize_Body_Entity (Result);

      case Get_Construct (Real_Sem_Info.Entity).Category is
         when Cat_Variable | Cat_Local_Variable | Cat_Field | Cat_Parameter
            | Cat_Class .. Cat_Subtype | Subprogram_Category =>

            if Info.Kind = All_Access then
               Result.Step := All_Access;
            else
               Result.Step := Referenced_Entity;
            end if;

         when Cat_Package =>
            Result.Step := Referenced_Entity_From_Package;

         when others =>
            Result.Step := All_Access;  --  Step is not set by default
            Free (Result);
            return Null_Semantic_Tree_Iterator;

      end case;

      if not Is_Valid (Result) then
         Next (Result);
      end if;

      return Result;
   end To_Semantic_Tree_Iterator;

   ------------
   -- At_End --
   ------------

   function At_End (It : Semantic_Tree_Iterator) return Boolean is
   begin
      return It.Step = Finished;
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Semantic_Tree_Iterator) is

      procedure Next_All_Access;
      procedure Next_Referenced_Entity;
      procedure Next_Contents;
      procedure Next_Child_Packages;
      procedure Next_Tagged_Type_Contents;

      ---------------------
      -- Next_All_Access --
      ---------------------

      procedure Next_All_Access is
      begin
         if not It.Step_Has_Started then
            It.Sub_It := new Semantic_Tree_Iterator'
              (To_Semantic_Tree_Iterator
                 ((It.Root_Entity.Entity, None, It.Generic_Context),
                  It.From_Visibility,
                  All_References,
                  It.Excluded_Entities));

            if At_End (It.Sub_It.all) then
               It.Step_Has_Started := False;
               It.Step := Contents;
            else
               It.Step_Has_Started := True;
            end if;
         else
            Next (It.Sub_It.all);

            if At_End (It.Sub_It.all) then
               It.Step_Has_Started := False;
               It.Step := Contents;
            end if;
         end if;
      end Next_All_Access;

      ----------------------------
      -- Next_Referenced_Entity --
      ----------------------------

      procedure Next_Referenced_Entity is
         Sub_References_Allowed : References_To_Follow_Array :=
           It.References_To_Follow;

         procedure Set_Sub_It;
         --  Set the sub it field, and set It.Step_Has_Started to false if it
         --  is not possible. It.Decl_It has to be set before.

         ----------------
         -- Set_Sub_It --
         ----------------

         procedure Set_Sub_It is
         begin
            if not At_End (It.Decl_It) then
               if Get_Construct (It.Current_Construct).
                     Attributes (Ada_Access_Attribute)
               then
                  It.Is_All := True;
                  Sub_References_Allowed (Dereferences) := False;
                  It.Step_Has_Started := True;
               else
                  Sub_References_Allowed (Dereferences) := True;
               end if;

               if It.Sub_It /= null then
                  Free (It.Sub_It.all);
                  Free (It.Sub_It);
               end if;

               declare
                  Generic_Context : Instance_Info :=
                    Null_Instance_Info;
                  View            : Entity_View;
               begin
                  View := Get_View (It.Decl_It);

                  --  ??? these two conditions selects the generic context,
                  --  either coming from the sub iterator, or the main one.
                  --  What we probably need to have is the sub iterator
                  --  "stacking" the two generic contexts somehow instead of
                  --  replacing the old one by a new one. Not yet clear where
                  --  that should be done.

                  if View.all in Declaration_View_Record'Class then
                     Generic_Context :=
                       Declaration_View_Record (View.all).Generic_Context;
                  end if;

                  if Generic_Context = Null_Instance_Info then
                     Generic_Context := It.Generic_Context;
                  end if;

                  It.Sub_It := new Semantic_Tree_Iterator'
                    (To_Semantic_Tree_Iterator
                       ((Get_Entity (View), None, Generic_Context),
                        It.From_Visibility,
                        Sub_References_Allowed,
                        It.Excluded_Entities));

                  Free (View);
               end;

               if At_End (It.Sub_It.all)
                 and then It.Is_All = False
               then
                  It.Step_Has_Started := False;
               else
                  It.Step_Has_Started := True;
               end if;
            else
               It.Step_Has_Started := False;
            end if;
         end Set_Sub_It;

         Expression : Parsed_Expression;

      begin
         if It.Is_All then
            It.Is_All := False;

            if It.Sub_It = null or else At_End (It.Sub_It.all) then
               It.Step_Has_Started := False;
            end if;
         elsif Is_Tagged
           (Get_Ada_Type (It.Root_Entity.Entity), It.From_Visibility)
         then
            --  If we're on a tagged type, then the type contents will be
            --  handled directly through the type hierarchy package.

            It.Step := Tagged_Type_Contents;
            It.Step_Has_Started := False;

            return;
         elsif not It.Step_Has_Started then
            declare
               Ref_Id : Normalized_Symbol;
            begin
               Ref_Id := Get_Identifier
                 (Get_Referenced_Identifiers (It.Current_Construct));

               if (not It.References_To_Follow (Dereferences)
                   and then Get_Construct
                     (It.Current_Construct).Attributes (Ada_Access_Attribute))
                 or else
                   (Get_Construct (It.Current_Construct).Category
                    = Cat_Function
                    and then not It.References_To_Follow (Returned_Type))
               then
                  It.Step_Has_Started := False;

               elsif Ref_Id /= No_Normalized_Symbol then
                  Expression := Parse_Expression_Backward (Get (Ref_Id));

                  if not It.Ignored_Expressions.Is_Empty
                    and then
                      It.Ignored_Expressions.Contains (To_String (Expression))
                  then
                     while not Is_Valid (It.Decl_It) loop
                        Next (It.Decl_It);
                     end loop;
                  else

                     It.Decl_List := Find_Declarations
                       ((From_File,
                        It.Generic_Context,
                        It.Current_File,
                        String_Index_Type
                          (Get_Construct
                             (It.Current_Construct).Sloc_End.Index)),
                        Expression           => Expression,
                        Filter               => Null_Filter,
                        Is_Partial           => False,
                        Excluded_Entities    => It.Excluded_Entities,
                        Analyzed_Expressions => It.Ignored_Expressions,
                        From_Visibility      => It.From_Visibility);

                     It.Decl_It := First (It.Decl_List);
                  end if;

                  Free (Expression);

                  if Get_Construct (It.Current_Construct).Category
                    = Cat_Function
                    and then not Get_Construct
                      (It.Current_Construct).Attributes (Ada_Renames_Attribute)
                  then
                     --  If the referenced entity of a function is not a
                     --  renaming, then it's a returned and we don't want to
                     --  follow a potential wrongly referenced function.

                     Sub_References_Allowed (Returned_Type) := False;
                  end if;

                  Set_Sub_It;
               else
                  It.Step_Has_Started := False;
               end if;
            end;
         elsif It.Sub_It /= null then
            Next (It.Sub_It.all);

            if At_End (It.Sub_It.all) then
               loop
                  while At_End (It.Sub_It.all)
                    and then not At_End (It.Decl_It)
                  loop
                     Next (It.Decl_It);

                     Set_Sub_It;
                  end loop;

                  if At_End (It.Sub_It.all)
                    and then At_End (It.Decl_It)
                  then
                     exit;
                  end if;

                  if not At_End (It.Sub_It.all) then
                     exit;
                  end if;
               end loop;
            end if;

            if At_End (It.Sub_It.all) and then At_End (It.Decl_It) then
               It.Step_Has_Started := False;
            end if;
         else
            It.Step_Has_Started := False;
         end if;

         if not It.Step_Has_Started then
            if It.Step = Referenced_Entity then
               It.Step := Contents;
            else
               It.Step := Child_Packages;
            end if;
         end if;
      end Next_Referenced_Entity;

      -------------------
      -- Next_Contents --
      -------------------

      procedure Next_Contents is
      begin
         if not It.Step_Has_Started then
            It.Current_Tree := Get_Tree (It.Current_File);

            if (Get_Construct (It.Current_Construct).Category
                in Cat_Class .. Cat_Subtype
                or else
                  Get_Construct (It.Current_Construct).Category
                = Cat_Package
               or else Get_Construct (It.Current_Construct).Category
                = Cat_Protected
               or else Get_Construct (It.Current_Construct).Category
                = Cat_Task)
              and then not Is_Enum_Type
                (It.Current_Tree, It.Current_Construct)
            then
               It.Content_It := Next
                 (It.Current_Tree, It.Current_Construct, Jump_Into);

               It.Step_Has_Started := True;
            else
               if It.Step = Package_Spec_Contents
                 and then It.Body_Entity /= Null_Construct_Tree_Iterator
               then
                  It.Step := Package_Body_Contents;
                  It.Current_Construct := It.Body_Entity;
                  It.Current_File := It.Body_File;
                  It.Current_Tree := It.Body_Tree;
                  It.Step_Has_Started := False;
               else
                  It.Step := Finished;
               end if;
            end if;
         else
            if Is_Enum_Type (It.Current_Tree, It.Content_It) then
               --  If we are on a enumeration, then jump in:

               It.Content_It :=
                 Next (It.Current_Tree, It.Content_It, Jump_Into);
            elsif
              Get_Construct (It.Content_It).Category = Cat_Case_Inside_Record
            then
               --  If we are on the case of a record, then jump in:

               It.Content_It :=
                 Next (It.Current_Tree, It.Content_It, Jump_Into);
            else
               --  Otherwise, jump over.

               It.Content_It :=
                 Next (It.Current_Tree, It.Content_It, Jump_Over);
            end if;
         end if;

         if It.Step /= Finished then

            if Get_Construct (It.Content_It).Visibility
              /= Visibility_Public
              and then It.Package_Relation in None .. Public_Spec_Hierarchy
            then
               --  If we reached an non-public entity and are not allowed to
               --  return any, then move over.

               It.Step := Finished;
            elsif It.Content_It = Next
              (It.Current_Tree, It.Current_Construct, Jump_Over)
            then
               --  If we reached the end of the iteration, see if we have to
               --  look for the package body

               if It.Step = Package_Spec_Contents
                 and then It.Body_Entity /= Null_Construct_Tree_Iterator
                 and then
                   (It.From_Visibility.File = null
                    or else Get_Location_Relation
                      (Get_Tree (It.Current_File),
                       It.Body_Entity,
                       Get_Tree (It.From_Visibility.File),
                       It.From_Visibility.Offset) = Package_Body)
               then
                  --  If we should see the package body, then move to it.

                  It.Step := Package_Body_Contents;
                  It.Current_Construct := It.Body_Entity;
                  It.Current_File := It.Body_File;
                  It.Current_Tree := It.Body_Tree;
                  It.Step_Has_Started := False;
               else
                  --  Otherwise, move over

                  It.Step := Finished;
               end if;
            end if;
         end if;
      end Next_Contents;

      -------------------------
      -- Next_Child_Packages --
      -------------------------

      procedure Next_Child_Packages is
      begin
         if not It.Step_Has_Started then
            if Is_Compilation_Unit (It.Current_Construct) then
               --  If we are completing a unit name, then look for its
               --  children

               It.Child_Pckg_It := Get_Children
                 (Get_Unit_Access
                    (To_Entity_Access
                       (It.Current_File, It.Current_Construct)));

               It.Step_Has_Started := True;
            else
               It.Step := Package_Spec_Contents;
            end if;
         else
            Next (It.Child_Pckg_It);
         end if;

         if It.Step = Child_Packages then
            --  If this is still true (we didn't dismiss it in the
            --  previous case

            if At_End (It.Child_Pckg_It) then
               It.Step_Has_Started := False;
               It.Step := Package_Spec_Contents;
            end if;
         end if;

         --  If we moved to spec contents, see if we come from the body,
         --  and in this case, go to the spec.

         if It.Step = Package_Spec_Contents then
            Initialize_Body_Entity (It);

            if not Get_Construct (It.Current_Construct).Is_Declaration then
               It.Step := Package_Body_Contents;
            end if;

            if It.From_Visibility.File /= null then
               It.Package_Relation := Get_Location_Relation
                 (Get_Tree (It.Current_File),
                  It.Current_Construct,
                  Get_Tree (It.From_Visibility.File),
                  It.From_Visibility.Offset);
            else
               It.Package_Relation := Full_Spec_Hierarchy;
            end if;
         end if;
      end Next_Child_Packages;

      -------------------------------
      -- Next_Tagged_Type_Contents --
      -------------------------------

      procedure Next_Tagged_Type_Contents is
         Parent_Info : Ada_Type_Access;
         Number_Of_Parents : Integer := 0;
      begin
         if not It.Step_Has_Started then
            Parent_Info := Get_Ada_Type (It.Root_Entity.Entity);

            while Parent_Info /= Null_Ada_Type_Access loop
               Number_Of_Parents := Number_Of_Parents + 1;
               Parent_Info := Get_Tagged_Parent (Parent_Info);
            end loop;

            It.Parents := new Entity_Array (1 .. Number_Of_Parents);

            Parent_Info := Get_Ada_Type (It.Root_Entity.Entity);

            for J in reverse 1 .. Number_Of_Parents loop
               if Is_Accessible
                 (Get_Entity (Parent_Info),
                  It.From_Visibility.File,
                  It.From_Visibility.Offset)
               then
                  It.Parents (J) := Get_Entity (Parent_Info);
               else
                  It.Parents (J) := Null_Entity_Access;
               end if;

               Parent_Info := Get_Tagged_Parent (Parent_Info);
            end loop;

            It.Parent_It := 1;
            It.Parent_Entity := It.Parents (It.Parent_It);

            if It.Parents (It.Parent_It) /= Null_Entity_Access then
               It.Parent_File := Get_File (It.Parent_Entity);
               It.Parent_Field := Next
                 (Get_Tree (It.Parent_File),
                  To_Construct_Tree_Iterator (It.Parent_Entity), Jump_Into);
            end if;

            It.Step_Has_Started := True;
         elsif It.Parent_It <= It.Parents'Last then
            It.Parent_Field := Next
              (Get_Tree (It.Parent_File),
               It.Parent_Field,
               Jump_Into);
         else
            It.Dotted_Subprograms_Index := It.Dotted_Subprograms_Index + 1;
         end if;

         if It.Parent_It <= It.Parents'Last then
            while It.Parents (It.Parent_It) = Null_Entity_Access
              or else not Encloses
                (Get_Tree (Get_File (It.Parent_Entity)),
                 To_Construct_Tree_Iterator (It.Parent_Entity),
                 It.Parent_Field)
            loop
               It.Parent_It := It.Parent_It + 1;

               exit when It.Parent_It > It.Parents'Last;

               It.Parent_Entity := It.Parents (It.Parent_It);

               if It.Parents (It.Parent_It) /= Null_Entity_Access then
                  It.Parent_File := Get_File (It.Parent_Entity);
                  It.Parent_Field := Next
                    (Get_Tree (It.Parent_File),
                     To_Construct_Tree_Iterator (It.Parent_Entity), Jump_Into);
               end if;
            end loop;
         end if;

         if It.Parent_It > It.Parents'Last
           and then It.Dotted_Subprograms = null
         then
            It.Dotted_Subprograms := new Entity_Persistent_Array'
              (Extract_Dotted_Notation_Sb
                 (Get_Ada_Type (It.Root_Entity.Entity)));
            It.Dotted_Subprograms_Index := It.Dotted_Subprograms'First;
         end if;

         if It.Dotted_Subprograms /= null
           and then It.Dotted_Subprograms_Index > It.Dotted_Subprograms'Last
         then
            It.Step := Finished;
         end if;
      end Next_Tagged_Type_Contents;

   begin
      loop
         case It.Step is
            when All_Access =>
               Next_All_Access;

            when Referenced_Entity | Referenced_Entity_From_Package =>
               Next_Referenced_Entity;

            when Contents | Package_Spec_Contents | Package_Body_Contents =>
               Next_Contents;

            when Child_Packages =>
               Next_Child_Packages;

            when Tagged_Type_Contents =>
               Next_Tagged_Type_Contents;

            when others =>
               raise Program_Error;

         end case;

         exit when Is_Valid (It);
      end loop;
   end Next;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (It : Semantic_Tree_Iterator) return Boolean is
   begin
      if At_End (It) then
         return True;
      elsif not It.Step_Has_Started then
         return False;
      end if;

      case It.Step is
         when Package_Spec_Contents | Package_Body_Contents =>

            --  If the element retreived from the package is declared after
            --  the given location, then it's not valid to return it.

            if It.From_Visibility.File = It.Current_File
              and then Natural (It.From_Visibility.Offset) < Get_Construct
                (It.Content_It).Sloc_Start.Index
            then
               return False;
            end if;
         when others =>
            null;
      end case;

      if (It.From_Visibility.Filter and All_Visible_Entities) = 0 then
         if (It.From_Visibility.Filter and All_Accessible_Units) /= 0 then
            case It.Step is
               when Contents | Package_Body_Contents | Package_Spec_Contents =>
                  return Get_Construct (It.Content_It).Category = Cat_Package
                    or else Is_Compilation_Unit (It.Content_It);

               when All_Access =>
                  return False;

               when others =>
                  return True;

            end case;
         else
            return False;
         end if;
      else
         return True;
      end if;
   end Is_Valid;

   ---------
   -- Get --
   ---------

   function Get
     (It : Semantic_Tree_Iterator) return Semantic_Information
   is
   begin
      case It.Step is
         when All_Access =>
            return Get (It.Sub_It.all);

         when Referenced_Entity | Referenced_Entity_From_Package =>
            if It.Is_All then
               return
                 (Get_Entity (It.Decl_It), All_Access, It.Generic_Context);
            else
               return Get (It.Sub_It.all);
            end if;

         when Contents | Package_Body_Contents | Package_Spec_Contents =>
            return
              (Entity =>
               To_Entity_Access
                 (File       => It.Current_File,
                  Construct  => It.Content_It),
               Kind => None,
               Generic_Context => It.Generic_Context);

         when Child_Packages =>
            return
              (Entity => Get (It.Child_Pckg_It),
               Kind => None,
               Generic_Context => It.Generic_Context);

         when Tagged_Type_Contents =>
            if It.Parent_It <= It.Parents'Last then
               return
                 (Entity => To_Entity_Access
                    (It.Parent_File, It.Parent_Field),
                  Kind => None,
                  Generic_Context => It.Generic_Context);
            else
               return
                 (Entity => To_Entity_Access
                    (It.Dotted_Subprograms (It.Dotted_Subprograms_Index)),
                  Kind => Prefix_Notation,
                  Generic_Context => It.Generic_Context);
            end if;

         when others =>
            raise Program_Error;

      end case;
   end Get;

   ----------
   -- Free --
   ----------

   procedure Free (It : in out Semantic_Tree_Iterator) is
   begin
      if It /= Null_Semantic_Tree_Iterator then
         Unref (It.Generic_Context);
         Pop_Entity (It.Excluded_Entities);
         Unref (It.Excluded_Entities);

         if It.Sub_It /= null then
            Free (It.Sub_It.all);
            Free (It.Sub_It);
         end if;

         Free (It.Decl_It);
         Free (It.Decl_List);
         Free (It.Dotted_Subprograms);
         Free (It.Parents);
      end if;
   end Free;

   ----------------------------
   -- Initialize_Body_Entity --
   ----------------------------

   procedure Initialize_Body_Entity (It : in out Semantic_Tree_Iterator) is
      The_Body : Entity_Access;
      The_Spec : Entity_Access;
   begin
      if Get_Construct (It.Current_Construct).Category = Cat_Package
        or else Get_Construct (It.Current_Construct).Category = Cat_Task
        or else Get_Construct (It.Current_Construct).Category = Cat_Protected
      then
         if Get_Construct (It.Current_Construct).Is_Declaration then
            The_Body := Get_Second_Occurence
              (To_Entity_Access (It.Current_File, It.Current_Construct));

            if The_Body /= Null_Entity_Access then
               It.Body_Entity :=
                 To_Construct_Tree_Iterator (The_Body);
               It.Body_File := Get_File (The_Body);
               It.Body_Tree := Get_Tree (It.Current_File);
            end if;
         else
            It.Body_Entity := It.Current_Construct;
            It.Body_File := It.Current_File;
            It.Body_Tree := It.Current_Tree;

            The_Spec := Get_First_Occurence
              (To_Entity_Access
                 (It.Body_File, It.Body_Entity));

            if To_Entity_Access (It.Body_File, It.Body_Entity) /= The_Spec then
               It.Current_Construct := To_Construct_Tree_Iterator (The_Spec);
               It.Current_File := Get_File (The_Spec);
               It.Current_Tree := Get_Tree (It.Current_File);
            end if;
         end if;
      end if;
   end Initialize_Body_Entity;

end Ada_Semantic_Tree.Entity_Iteration;
