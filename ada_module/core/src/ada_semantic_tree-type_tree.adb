------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;

with Language.Ada;                   use Language.Ada;

with Ada_Semantic_Tree.Lang;         use Ada_Semantic_Tree.Lang;
with Ada_Semantic_Tree.Parts;        use Ada_Semantic_Tree.Parts;
with Ada_Semantic_Tree.Visibility;   use Ada_Semantic_Tree.Visibility;
with Ada_Semantic_Tree.Declarations; use Ada_Semantic_Tree.Declarations;
with Ada_Semantic_Tree.Generics;     use Ada_Semantic_Tree.Generics;
with GNATCOLL.Symbols;               use GNATCOLL.Symbols;

package body Ada_Semantic_Tree.Type_Tree is

   Ada_Type_Assistant_Id : constant String := "ADA_TYPE_ASSISTANT";

   type Ada_Type_Assistant is new Database_Assistant with record
      Ada_Type_Key      : Construct_Annotations_Pckg.Annotation_Key;

      Ada_Primitive_Key : Construct_Annotations_Pckg.Annotation_Key;
      --  This key is used to store the primitive information on primitive
      --  operations, and the "primitivity" of parameters (as boolean).
   end record;

   type Ada_Type_Annotation is
     new Construct_Annotations_Pckg.General_Annotation_Record
   with record
      Ada_Type : Ada_Type_Access;
   end record;

   procedure Free_Parents_Array (The_Type : Ada_Type_Access);
   --  Disconect the type from its parents, and free the parent array.

   overriding
   procedure Free (Annotation : in out Ada_Type_Annotation);

   type Ada_Primitive_Annotation is
     new Construct_Annotations_Pckg.General_Annotation_Record
   with record
      Ada_Primitive : Ada_Primitive_Access;
   end record;

   overriding
   procedure Free (Annotation : in out Ada_Primitive_Annotation);

   overriding
   procedure File_Updated
     (Assistant : access Ada_Type_Assistant;
      File      : Structured_File_Access;
      Old_Tree  : Construct_Tree;
      Kind      : Update_Kind);

   function Get_Type_Info
     (Key    : Construct_Annotations_Pckg.Annotation_Key;
      Entity : Entity_Access) return Ada_Type_Access;

   function Get_Primitive_Info
     (Key    : Construct_Annotations_Pckg.Annotation_Key;
      Entity : Entity_Access) return Ada_Primitive_Access;
   pragma Unreferenced (Get_Primitive_Info);

   function Is_Primitive_Param
     (Key    : Construct_Annotations_Pckg.Annotation_Key;
      Entity : Entity_Access) return Boolean;

   procedure Perform_Type_Analyzis_If_Needed
     (Entity_Type : Entity_Access;
      Excluded    : in out Excluded_Stack_Type);
   --  This function will perform the needed type analysis if the type
   --  information isn't up to date anymore

   package Primitive_List is new Standard.Ada.Containers.Doubly_Linked_Lists
     (Ada_Primitive_Access);
   use Primitive_List;

   package Entity_Persistent_List is new
     Standard.Ada.Containers.Doubly_Linked_Lists (Entity_Persistent_Access);
   use Entity_Persistent_List;

   package Type_List is new Standard.Ada.Containers.Doubly_Linked_Lists
     (Ada_Type_Access);
   use Type_List;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Annotation : in out Ada_Primitive_Annotation) is
   begin
      if Annotation.Ada_Primitive /= null then
         Unref (Annotation.Ada_Primitive);
      end if;
   end Free;

   ------------------------
   -- Free_Parents_Array --
   ------------------------

   procedure Free_Parents_Array (The_Type : Ada_Type_Access) is
      use Language.Tree.Construct_Annotations_Pckg;

      Tmp : Entity_Persistent_Access;

      Assistant    : Database_Assistant_Access;
      Ada_Type_Key : Annotation_Key;

   begin
      for J in The_Type.Parents'Range loop
         if Exists (The_Type.Parents (J).Entity) then
            Assistant := Get_Assistant
              (Get_Database (Get_File (The_Type.Entity)),
               Ada_Type_Assistant_Id);
            Ada_Type_Key :=
              Ada_Type_Assistant (Assistant.all).Ada_Type_Key;

            declare
               --  WARNING! Do not use Get_Ada_Type here, as it will recompute
               --  the type and parts, may be in the process of being freed.
               Parent_Info : constant Ada_Type_Access :=
                 Get_Type_Info
                   (Ada_Type_Key,
                    To_Entity_Access (The_Type.Parents (J).Entity));
            begin
               if Parent_Info /= null then
                  if Parent_Info.Children.Contains (The_Type.Entity) then
                     Parent_Info.Children.Delete (The_Type.Entity);

                     --  We just need to decrement the reference counter here,
                     --  but we don't want to reset the entity. What we really
                     --  do is to remove the reference from the child list.
                     Tmp := The_Type.Entity;
                     Unref (Tmp);
                  end if;
               end if;
            end;
         end if;
      end loop;

      Free (The_Type.Parents);
   end Free_Parents_Array;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Annotation : in out Ada_Type_Annotation) is
   begin
      if Annotation.Ada_Type /= null then
         Free_Parents_Array (Annotation.Ada_Type);

         declare
            C : Entity_Lists_Pck.Cursor :=
              Annotation.Ada_Type.Children.First;
            E : Entity_Persistent_Access;
         begin
            while C /= Entity_Lists_Pck.No_Element loop
               E := Element (C);
               Unref (E);
               C := Next (C);
            end loop;

            Annotation.Ada_Type.Children.Clear;
         end;

         Unref (Annotation.Ada_Type.Entity);

         for J in Annotation.Ada_Type.Primitives'Range loop
            Unref (Annotation.Ada_Type.Primitives (J));
         end loop;

         Free (Annotation.Ada_Type.Primitives);

         if Annotation.Ada_Type.Dotted_Notation_Sb /= null then
            for J in Annotation.Ada_Type.Dotted_Notation_Sb'Range loop
               Unref (Annotation.Ada_Type.Dotted_Notation_Sb (J));
            end loop;

            Free (Annotation.Ada_Type.Dotted_Notation_Sb);
         end if;

         Free (Annotation.Ada_Type);
      end if;
   end Free;

   ------------------------
   -- Register_Assistant --
   ------------------------

   procedure Register_Assistant (Db : Construct_Database_Access) is
      use Construct_Annotations_Pckg;

      Ada_Type_Key      : Construct_Annotations_Pckg.Annotation_Key;
      Ada_Primitive_Key : Construct_Annotations_Pckg.Annotation_Key;
   begin
      Ada_Type_Key := Get_Ref_Key (Db);

      Get_Annotation_Key
        (Get_Construct_Annotation_Key_Registry (Db).all,
         Ada_Primitive_Key);

      Register_Assistant
        (Db,
         Ada_Type_Assistant_Id,
         new Ada_Type_Assistant'
           (Database_Assistant with
            Ada_Type_Key      => Ada_Type_Key,
            Ada_Primitive_Key => Ada_Primitive_Key));
   end Register_Assistant;

   ------------------
   -- File_Updated --
   ------------------

   overriding procedure File_Updated
     (Assistant : access Ada_Type_Assistant;
      File      : Structured_File_Access;
      Old_Tree  : Construct_Tree;
      Kind      : Update_Kind)
   is
      pragma Unreferenced (File, Assistant, Kind);
   begin
      --  Nothing as to be done here - everything is checked against unit
      --  timestamps & consistency.

      null;
   end File_Updated;

   -------------------
   -- Get_Type_Info --
   -------------------

   function Get_Type_Info
     (Key    : Construct_Annotations_Pckg.Annotation_Key;
      Entity : Entity_Access) return Ada_Type_Access
   is
      use Construct_Annotations_Pckg;
      Type_Annotation : Annotation;
   begin
      if Get_Construct (Entity).Category in Cat_Class .. Cat_Subtype then
         Get_Annotation
           (Get_Annotation_Container
              (Get_Tree (Get_File (Entity)),
               To_Construct_Tree_Iterator
                 (Entity)).all,
            Key,
            Type_Annotation);

         if Type_Annotation /= Null_Annotation then
            return Ada_Type_Annotation
              (Type_Annotation.Other_Val.all).Ada_Type;
         else
            return null;
         end if;
      else
         return null;
      end if;
   end Get_Type_Info;

   ------------------------
   -- Get_Primitive_Info --
   ------------------------

   function Get_Primitive_Info
     (Key    : Construct_Annotations_Pckg.Annotation_Key;
      Entity : Entity_Access) return Ada_Primitive_Access
   is
      use Construct_Annotations_Pckg;
      Prim_Annotation : Annotation;
   begin
      Get_Annotation
        (Get_Annotation_Container
           (Get_Tree (Get_File (Entity)),
            To_Construct_Tree_Iterator
              (Entity)).all,
         Key,
         Prim_Annotation);

      if Prim_Annotation /= Null_Annotation then
         return Ada_Primitive_Annotation
           (Prim_Annotation.Other_Val.all).Ada_Primitive;
      else
         return null;
      end if;
   end Get_Primitive_Info;

   ------------------------
   -- Is_Primitive_Param --
   ------------------------

   function Is_Primitive_Param
     (Key    : Construct_Annotations_Pckg.Annotation_Key;
      Entity : Entity_Access) return Boolean
   is
      use Construct_Annotations_Pckg;
      Prim_Annotation : Annotation;
   begin
      Get_Annotation
        (Get_Annotation_Container
           (Get_Tree (Get_File (Entity)),
            To_Construct_Tree_Iterator
              (Entity)).all,
         Key,
         Prim_Annotation);

      if Prim_Annotation /= Null_Annotation then
         return Prim_Annotation.Boolean_Val;
      else
         return False;
      end if;
   end Is_Primitive_Param;

   -------------------------------------
   -- Perform_Type_Analyzis_If_Needed --
   -------------------------------------

   procedure Perform_Type_Analyzis_If_Needed
     (Entity_Type : Entity_Access;
      Excluded    : in out Excluded_Stack_Type)
   is
      use Construct_Annotations_Pckg;

      --  We'll put all information on the most complete view of the entity.
      The_Type : constant Entity_Access :=
        Get_Most_Complete_View (Entity_Type);

      Assistant         : constant Database_Assistant_Access :=
        Get_Assistant
          (Get_Database (Get_File (The_Type)), Ada_Type_Assistant_Id);
      Ada_Type_Key      : constant Annotation_Key :=
        Ada_Type_Assistant (Assistant.all).Ada_Type_Key;
      Ada_Primitive_Key : constant Annotation_Key :=
        Ada_Type_Assistant (Assistant.all).Ada_Primitive_Key;

      Tree   : Construct_Tree;

      New_Primitives : Primitive_List.List;
      --  This variable holds the new primitives declared for this type.

      function Same_Overriding_Subprogram
        (S_1, S_2             : Entity_Access;
         Check_Returned_Types : Boolean) return Boolean;
      --  Return true if S1 and S2 are two same overriding subprograms, that
      --  is to say the controlling primitive parameters are on the same
      --  location, other parameters are the same, and names matches. Primitive
      --  annotation have to be computed before calling this subprogram.
      --
      --  Returned type is not checked except if the flag is true. In that
      --  case, only a name match will be done, and no primitivity test.

      function Find_Overridden_Primitive
        (Subprogram : Entity_Access) return Ada_Primitive_Access;
      --  If there is a primitive overridden by the subprogram passed in
      --  parameter, return it.

      --------------------------------
      -- Same_Overriding_Subprogram --
      --------------------------------

      function Same_Overriding_Subprogram
        (S_1, S_2             : Entity_Access;
         Check_Returned_Types : Boolean) return Boolean
      is
         It_1, It_2     : Construct_Tree_Iterator;
         File_1, File_2 : Structured_File_Access;
         Tree_1, Tree_2 : Construct_Tree;
         Scope_1, Scope_2 : Construct_Tree_Iterator;
      begin
         if Get_Identifier (S_1) /= Get_Identifier (S_2)
           or else Get_Construct (S_1).Category /= Get_Construct (S_2).Category
         then
            return False;
         end if;

         File_1 := Get_File (S_1);
         File_2 := Get_File (S_2);

         Tree_1 := Get_Tree (File_1);
         Tree_2 := Get_Tree (File_2);

         Scope_1 := To_Construct_Tree_Iterator (S_1);
         Scope_2 := To_Construct_Tree_Iterator (S_2);

         It_1 := Next (Tree_1, Scope_1, Jump_Into);
         It_2 := Next (Tree_2, Scope_2, Jump_Into);

         while Get_Construct (It_1).Category = Cat_Parameter
           and then Get_Construct (It_2).Category = Cat_Parameter
           and then Is_Parent_Scope (Scope_1, It_1)
           and then Is_Parent_Scope (Scope_2, It_2)
         loop
            if Is_Primitive_Param
              (Ada_Primitive_Key, To_Entity_Access (File_1, It_1))
            then
               if not Is_Primitive_Param
                 (Ada_Primitive_Key, To_Entity_Access (File_2, It_2))
               then
                  --  One of the two parameters is not primitive, while the
                  --  other is. Subprograms doesn't match.

                  return False;
               else
                  --  Do nothing - the two parameters are primitive, and OK
                  --  regarding the equality.
                  null;
               end if;
            else
               if Get_Referenced_Identifiers (It_1)
                 /= Get_Referenced_Identifiers (It_2)
               then
                  return False;
               end if;
            end if;

            It_1 := Next (Tree_1, It_1, Jump_Over);
            It_2 := Next (Tree_2, It_2, Jump_Over);
         end loop;

         --  If one of the two iterations is not finished, there is not the
         --  same number of parameters - otherwise it's OK.
         if (Get_Construct (It_1).Category = Cat_Parameter
             and then Is_Parent_Scope (Scope_1, It_1))
           or else
             (Get_Construct (It_2).Category = Cat_Parameter
              and then Is_Parent_Scope (Scope_2, It_2))
         then
            return False;
         end if;

         if Get_Construct (Scope_1).Category /= Cat_Function
           or else not Check_Returned_Types
         then
            return True;
         end if;

         if Get_Referenced_Identifiers (Scope_1)
           /= Get_Referenced_Identifiers (Scope_2)
         then
            return False;
         end if;

         return True;
      end Same_Overriding_Subprogram;

      -------------------------------
      -- Find_Overridden_Primitive --
      -------------------------------

      function Find_Overridden_Primitive
        (Subprogram : Entity_Access) return Ada_Primitive_Access
      is
         Return_Is_Primitive : Boolean := False;
         C : Primitive_List.Cursor := First (New_Primitives);
      begin
         if Get_Construct (Subprogram).Category = Cat_Function
           and then not Get_Construct (Subprogram).Attributes
           (Ada_Class_Attribute)
         then
            if Get_Referenced_Identifiers
              (To_Construct_Tree_Iterator (Subprogram)) =
              Get_Identifier (The_Type)
            then
               Return_Is_Primitive := True;
            end if;
         end if;

         while C /= Primitive_List.No_Element loop
            if Element (C).Entity = Null_Entity_Persistent_Access
              and then not
                (Return_Is_Primitive
                 xor Element (C).Is_Returned_Primitive)
              and then Same_Overriding_Subprogram
                (Get_Entity_Or_Overridden (Element (C)),
                 Subprogram,
                 not Return_Is_Primitive)
            then
               return Element (C);
            end if;

            C := Next (C);
         end loop;

         return null;
      end Find_Overridden_Primitive;

      Type_Info : Ada_Type_Access := Get_Type_Info (Ada_Type_Key, The_Type);

      It    : Construct_Tree_Iterator;
      Scope : Construct_Tree_Iterator;

      Parent_Types   : Type_List.List;

      Dotted_Notation_Sb : Entity_Persistent_List.List;

      Ref_Ids : Referenced_Identifiers_List;

      This_Unit : constant Unit_Access := Get_Owning_Unit (The_Type);

      This_Unit_Timestamp : Unit_Hierarchy_Timestamp;

      Do_Analysis : Boolean := False;
   begin
      if This_Unit = Null_Unit_Access then
         --  In case of malformed units, it may not be possible to retreive the
         --  enclosing unit of a type. In this case, avoid the analysis.

         return;
      end if;

      This_Unit_Timestamp := Get_Unit_Hierarchy_Timestamp (This_Unit);
      Push_Entity (Excluded, The_Type);

      --  First, if there's already a computed information, check if it's up
      --  to date.

      if Type_Info /= null then
         if This_Unit_Timestamp = Type_Info.Enclosing_Unit_Timestamp then
            for J in Type_Info.Parents'Range loop
               if Exists (Type_Info.Parents (J).Entity) then
                  declare
                     Parent      : Entity_Access;
                     Parent_Info : Ada_Type_Access;
                  begin
                     Parent := To_Entity_Access (Type_Info.Parents (J).Entity);

                     if not Is_Excluded (Excluded, Parent) then
                        Perform_Type_Analyzis_If_Needed (Parent, Excluded);

                        Parent_Info := Get_Type_Info (Ada_Type_Key, Parent);

                        --  If there has been an analysis on this parent since
                        --  last time, perhaps because of the last call to
                        --  Perform_Type_Analysis, then we'll have to
                        --  re-analyze this type as well.

                        if Type_Info.Parents (J).Timestamp
                          /= Parent_Info.Analysis_Timestamp
                        then
                           Do_Analysis := True;
                        end if;
                     end if;
                  end;
               end if;
            end loop;
         else
            --  If this unit hierarchy has changed, in any case, we'll have
            --  to re-do the analysis.

            Do_Analysis := True;
         end if;

         if Do_Analysis and then Active (Test_Trace) then
            Trace
              (Test_Trace,
               "TYPE OUT OF DATE: "
               & Get (Get_Construct (Entity_Type).Name).all);
         end if;
      else
         --  Check if we want to do analysis on this kind of construct

         if
           Get_Construct (The_Type).Category not in Cat_Class .. Cat_Subtype
           or else
             (not Get_Construct (The_Type).Attributes (Ada_Tagged_Attribute)
              and then not
                Get_Construct (The_Type).Attributes (Ada_Interface_Attribute))
         then
            return;
         end if;

         Do_Analysis := True;

         Type_Info := new Ada_Type_Record;
         Type_Info.Entity := To_Entity_Persistent_Access (The_Type);

         if Active (Test_Trace) then
            Trace
              (Test_Trace,
               "NEW TYPE: "
               & Get (Get_Construct (Entity_Type).Name).all);
         end if;
      end if;

      if not Do_Analysis then
         Pop_Entity (Excluded);

         return;
      end if;

      --  Increment the timestamp, so that possible children know that
      --  something may have changed.

      Type_Info.Analysis_Timestamp := Type_Info.Analysis_Timestamp + 1;
      Type_Info.Enclosing_Unit_Timestamp := This_Unit_Timestamp;

      --  Initialize parent type fields

      if Type_Info.Parents /= null then
         Free_Parents_Array (Type_Info);
      end if;

      --  First analyze parent types.

      Ref_Ids := Get_Referenced_Identifiers
        (To_Construct_Tree_Iterator (The_Type));

      while Ref_Ids /= Null_Referenced_Identifiers_List loop
         --  Extracts all the inherited primitives and add them in the type's
         --  list. We'll see later if the primitive is overridden.

         declare
            Expression : Parsed_Expression := Parse_Expression_Backward
              (Get (Get_Identifier (Ref_Ids)));

            Decl_List   : Entity_List;
            It          : Entity_Iterator;
            Parent_Type : Entity_Access;
            Parent_Info : Ada_Type_Access;
         begin
            Decl_List :=
              Find_Declarations
                ((From_File,
                 Null_Instance_Info,
                 Get_File (The_Type),
                 String_Index_Type (Get_Construct (The_Type).Sloc_End.Index)),
                 Expression        => Expression,
                 Excluded_Entities => Excluded);
            It := First (Decl_List);

            Free (Expression);

            --  ??? We should have more visiblity constraints here, and do
            --  some additional work in case of multiple matches.

            if not At_End (It) then
               Parent_Type := Get_Most_Complete_View (Get_Entity (It));

               --  At this stage, there may be illegal code - that is to say
               --  loop in the type references. The use of an excluding stack
               --  avoids infinite loops

               if Parent_Type /= Null_Entity_Access and then
                 not Is_Excluded (Excluded, Parent_Type)
               then
                  Perform_Type_Analyzis_If_Needed (Parent_Type, Excluded);

                  Parent_Info := Get_Type_Info (Ada_Type_Key, Parent_Type);

                  if Parent_Info /= null then
                     Append (Parent_Types, Parent_Info);

                     declare
                        Primitives : constant Primitive_Array :=
                          Extract_Primitives (Parent_Info);
                        Dotted_Sb  : Entity_Persistent_Array :=
                          Extract_Dotted_Notation_Sb (Parent_Info);
                        New_Primitive : Ada_Primitive_Access;
                     begin
                        for J in Primitives'Range loop
                           if Find_Overridden_Primitive
                             (To_Entity_Access (Primitives (J).Entity)) = null
                           then
                              --  Add the primitive to the primitive list, only
                              --  if this is the first occurence of such a
                              --  primitive. In case the same profile comes
                              --  from different parents, only consider the
                              --  first one.

                              New_Primitive := new Primitive_Subprogram'
                                (Entity
                                 => Null_Entity_Persistent_Access,
                                 Overridden_Entities
                                 => new Primitive_Array'(1 => Primitives (J)),
                                 Is_Returned_Primitive =>
                                   Primitives (J).Is_Returned_Primitive,
                                 Refs => 0);

                              Append (New_Primitives, New_Primitive);
                           end if;
                        end loop;

                        for J in Dotted_Sb'Range loop
                           Append (Dotted_Notation_Sb, Dotted_Sb (J));
                           Ref (Dotted_Sb (J));
                        end loop;
                     end;

                     Parent_Info.Children.Insert (Type_Info.Entity);
                  end if;

                  Ref (Type_Info.Entity);
               end if;
            end if;

            Free (It);
            Free (Decl_List);
         end;

         Ref_Ids := Get_Next_Referenced_Identifiers (Ref_Ids);
      end loop;

      Type_Info.Parents :=
        new Timestamp_Entity_Array (1 .. Integer (Length (Parent_Types)));

      declare
         C : Type_List.Cursor := First (Parent_Types);
      begin
         for J in Type_Info.Parents'Range loop
            Type_Info.Parents (J).Timestamp := Element (C).Analysis_Timestamp;
            Type_Info.Parents (J).Entity := Element (C).Entity;
            Ref (Type_Info.Parents (J).Entity);

            C := Next (C);
         end loop;
      end;

      --  Set the start of the search for dispatching primitives - we'll start
      --  as the first occurence of the type

      It := To_Construct_Tree_Iterator (Get_First_Occurence (The_Type));

      --  Analyze the subprogram in the current scope.

      Tree := Get_Tree (Get_File (The_Type));
      Scope := Get_Parent_Scope (Tree, It);

      while Is_Parent_Scope (Scope, It) loop
         if Get_Construct (It).Category = Cat_Function
           or else Get_Construct (It).Category = Cat_Procedure
         then
            --  Check if this subprogram is a primitive of the analyzed type.

            declare
               Param_It : Construct_Tree_Iterator :=
                 Next (Tree, It, Jump_Into);

               Primitive_Info : Ada_Primitive_Access;
               Is_Primitive   : Boolean := False;
               Subprogram     : constant Entity_Access := To_Entity_Access
                 (Get_File (The_Type), It);
               Param_Number   : Integer := 0;
               Add_To_Dotted  : Boolean := False;
            begin

               --  First, look for all parameters, and extract primitives
               --  ones if any.

               while Is_Parent_Scope (It, Param_It)
                 and then Get_Construct (Param_It).Category = Cat_Parameter
               loop
                  Param_Number := Param_Number + 1;

                  if not Get_Construct (Param_It).Attributes
                    (Ada_Class_Attribute) or else Param_Number = 1
                  then
                     if Get_Referenced_Identifiers (Param_It)
                       = Get_Identifier (The_Type)
                     then
                        if not Get_Construct (Param_It).Attributes
                          (Ada_Class_Attribute)
                        then
                           Set_Annotation
                             (Get_Annotation_Container (Tree, Param_It).all,
                              Ada_Primitive_Key,
                              (Kind        => Boolean_Kind,
                               Boolean_Val => True));

                           Is_Primitive := True;
                        end if;

                        if Param_Number = 1 then
                           Add_To_Dotted := True;
                        end if;
                     end if;
                  end if;

                  Param_It := Next (Tree, Param_It, Jump_Over);
               end loop;

               --  If we're on a function, the we check the returned type as
               --  well.

               if Get_Construct (Subprogram).Category = Cat_Function
                 and then not Get_Construct (Subprogram).Attributes
                 (Ada_Class_Attribute)
               then
                  if Get_Referenced_Identifiers
                    (To_Construct_Tree_Iterator (Subprogram)) =
                    Get_Identifier (The_Type)
                  then
                     Is_Primitive := True;
                  end if;
               end if;

               if Is_Primitive then
                  --  If this subprogram is a primitive of the given type,
                  --  then do its analysis and set the primitive annotation.

                  Primitive_Info := Find_Overridden_Primitive (Subprogram);

                  if Primitive_Info = null then
                     --  We are on a new primitive (not inherited)

                     Primitive_Info := new Primitive_Subprogram;
                     Append (New_Primitives, Primitive_Info);
                  else
                     --  If we find it in the already computed
                     --  primitives, we've already list that subprogram
                     --  in the dotted list, so we don't want to add
                     --  it in any case.

                     Add_To_Dotted := False;
                  end if;

                  Primitive_Info.Entity :=
                    To_Entity_Persistent_Access (Subprogram);
                  Ref (Primitive_Info);

                  Set_Annotation
                    (Get_Annotation_Container (Tree, It).all,
                     Ada_Primitive_Key,
                     (Kind      => Other_Kind,
                      Other_Val => new Ada_Primitive_Annotation'
                        (Ada_Primitive => Primitive_Info)));
               end if;

               if Add_To_Dotted then
                  declare
                     Persistent_Sb : constant Entity_Persistent_Access :=
                       To_Entity_Persistent_Access (Subprogram);
                  begin
                     Append (Dotted_Notation_Sb, Persistent_Sb);
                  end;
               end if;
            end;
         end if;

         It := Next (Tree, It, Jump_Over);
      end loop;

      --  Add the new primitives to the type.

      if Type_Info.Primitives /= null then
         for J in Type_Info.Primitives'Range loop
            Unref (Type_Info.Primitives (J));
         end loop;

         Free (Type_Info.Primitives);
      end if;

      Type_Info.Primitives := new Primitive_Array
        (1 .. Integer (Length (New_Primitives)));

      declare
         Cursor : Primitive_List.Cursor := First (New_Primitives);
         Primitive_Info : Ada_Primitive_Access;
      begin
         for J in Type_Info.Primitives'Range loop
            Primitive_Info := Element (Cursor);
            Type_Info.Primitives (J) := Primitive_Info;
            Ref (Primitive_Info);

            if Primitive_Info.Overridden_Entities /= null then
               for J in Primitive_Info.Overridden_Entities'Range loop
                  Ref (Primitive_Info.Overridden_Entities (J));
               end loop;
            end if;

            Cursor := Next (Cursor);
         end loop;
      end;

      if Type_Info.Dotted_Notation_Sb /= null then
         for J in Type_Info.Dotted_Notation_Sb'Range loop
            Unref (Type_Info.Dotted_Notation_Sb (J));
         end loop;

         Free (Type_Info.Dotted_Notation_Sb);
      end if;

      Type_Info.Dotted_Notation_Sb := new Entity_Persistent_Array
        (1 .. Integer (Length (Dotted_Notation_Sb)));

      declare
         Cursor : Entity_Persistent_List.Cursor := First (Dotted_Notation_Sb);
      begin
         for J in Type_Info.Dotted_Notation_Sb'Range loop
            Type_Info.Dotted_Notation_Sb (J) := Element (Cursor);

            Cursor := Next (Cursor);
         end loop;
      end;

      if Is_Set
        (Get_Annotation_Container
           (Tree, To_Construct_Tree_Iterator (The_Type)).all,
         Ada_Type_Key)
      then
         --  If the annotation is already set, we want to avoid to delete the
         --  type pointer (which we've jut modified). That's why we remove set
         --  it to null.

         declare
            Annot : Construct_Annotations_Pckg.Annotation;
         begin
            Get_Annotation
              (Get_Annotation_Container
                 (Tree, To_Construct_Tree_Iterator (The_Type)).all,
               Ada_Type_Key,
               Annot);

            pragma Assert
              (Type_Info = Ada_Type_Annotation (Annot.Other_Val.all).Ada_Type);

            Ada_Type_Annotation (Annot.Other_Val.all).Ada_Type := null;
         end;
      end if;

      Set_Annotation
        (Get_Annotation_Container
           (Tree, To_Construct_Tree_Iterator (The_Type)).all,
         Ada_Type_Key,
         (Kind      => Other_Kind,
          Other_Val => new Ada_Type_Annotation'
            (Ada_Type => Type_Info)));

      Pop_Entity (Excluded);
   end Perform_Type_Analyzis_If_Needed;

   ------------------
   -- Get_Ada_Type --
   ------------------

   function Get_Ada_Type (Entity : Entity_Access) return Ada_Type_Access is
      use Construct_Annotations_Pckg;

      Assistant    : Database_Assistant_Access;
      Ada_Type_Key : Annotation_Key;

      Excluded : Excluded_Stack_Type := Null_Excluded_Stack;
   begin
      if Get_Construct (Entity).Category in Cat_Class .. Cat_Subtype then
         Assistant := Get_Assistant
           (Get_Database (Get_File (Entity)), Ada_Type_Assistant_Id);
         Ada_Type_Key := Ada_Type_Assistant (Assistant.all).Ada_Type_Key;
         Ref (Excluded);
         Perform_Type_Analyzis_If_Needed (Entity, Excluded);
         Unref (Excluded);

         return Get_Type_Info (Ada_Type_Key, Get_Most_Complete_View (Entity));
      else
         return Null_Ada_Type_Access;
      end if;
   end Get_Ada_Type;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity (Ada_Type : Ada_Type_Access) return Entity_Access is
   begin
      if Ada_Type /= null and then Exists (Ada_Type.Entity) then
         return To_Entity_Access (Ada_Type.Entity);
      else
         return Null_Entity_Access;
      end if;
   end Get_Entity;

   ------------------------
   -- Extract_Primitives --
   ------------------------

   function Extract_Primitives
     (Ada_Type : Ada_Type_Access) return Primitive_Array is
   begin
      return Ada_Type.Primitives.all;
   end Extract_Primitives;

   ---------------
   -- Is_Tagged --
   ---------------

   function Is_Tagged
     (Ada_Type : Ada_Type_Access; From_Visibility : Visibility_Context)
      return Boolean
   is
      Entity : Entity_Access;

      function Is_Tagged_And_Visible (E : Entity_Access) return Boolean;

      function Is_Tagged_And_Visible (E : Entity_Access) return Boolean is
      begin
         if Get_Construct (E).Attributes (Ada_Tagged_Attribute)
           or else Get_Construct (E).Attributes (Ada_Interface_Attribute)
         then
            return Is_Accessible
              (E, From_Visibility.File, From_Visibility.Offset);
         end if;

         return False;
      end Is_Tagged_And_Visible;

   begin
      if Ada_Type = null then
         return False;
      else
         Entity := To_Entity_Access (Ada_Type.Entity);

         return Is_Tagged_And_Visible (Get_First_Occurence (Entity))
           or else Is_Tagged_And_Visible (Get_Second_Occurence (Entity))
           or else Is_Tagged_And_Visible (Get_Third_Occurence (Entity));
      end if;
   end Is_Tagged;

   --------------------------------
   -- Extract_Dotted_Notation_Sb --
   --------------------------------

   function Extract_Dotted_Notation_Sb
     (Ada_Type : Ada_Type_Access) return Entity_Persistent_Array
   is
   begin
      return Ada_Type.Dotted_Notation_Sb.all;
   end Extract_Dotted_Notation_Sb;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Primitive : Ada_Primitive_Access) return Entity_Access
   is
   begin
      return To_Entity_Access (Primitive.Entity);
   end Get_Entity;

   -----------------------
   -- Get_Tagged_Parent --
   -----------------------

   function Get_Tagged_Parent
     (Ada_Type : Ada_Type_Access) return Ada_Type_Access
   is
      use Construct_Annotations_Pckg;

      Assistant    : Database_Assistant_Access;
      Ada_Type_Key : Annotation_Key;
      Parent       : Entity_Access;
   begin
      if Ada_Type.Parents'Length > 0
        and then Exists (Ada_Type.Parents (1).Entity)
      then
         Parent := To_Entity_Access (Ada_Type.Parents (1).Entity);

         Assistant := Get_Assistant
           (Get_Database (Get_File (Parent)), Ada_Type_Assistant_Id);
         Ada_Type_Key :=
           Ada_Type_Assistant (Assistant.all).Ada_Type_Key;

         if not
           Get_Construct (Parent).Attributes (Ada_Interface_Attribute)
         then
            return Get_Type_Info (Ada_Type_Key, Parent);
         end if;
      end if;

      return null;
   end Get_Tagged_Parent;

   ------------------------------
   -- Get_Entity_Or_Overridden --
   ------------------------------

   function Get_Entity_Or_Overridden
     (Primitive : Ada_Primitive_Access) return Entity_Access
   is
      Local_Primitive : Ada_Primitive_Access := Primitive;
   begin
      while Local_Primitive.Entity = Null_Entity_Persistent_Access loop
         Local_Primitive := Local_Primitive.Overridden_Entities (1);
      end loop;

      return To_Entity_Access (Local_Primitive.Entity);
   end Get_Entity_Or_Overridden;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children
     (Ada_Type : Ada_Type_Access) return Entity_Persistent_Array
   is
      Result : Entity_Persistent_Array
        (1 .. Integer (Ada_Type.Children.Length));
      C : Entity_Lists_Pck.Cursor := Ada_Type.Children.First;
      J : Integer := 1;
   begin
      while C /= Entity_Lists_Pck.No_Element loop
         Result (J) := Element (C);
         J := J + 1;
         C := Next (C);
      end loop;

      return Result;
   end Get_Children;

   -----------------
   -- Get_Parents --
   -----------------

   function Get_Parents
     (Ada_Type : Ada_Type_Access) return Entity_Persistent_Array
   is
      Result : Entity_Persistent_Array
        (1 .. Ada_Type.Parents'Last);
   begin
      for J in Result'Range loop
         Result (J) := Ada_Type.Parents (J).Entity;
      end loop;

      return Result;
   end Get_Parents;

   --------------------------
   -- First_Private_Parent --
   --------------------------

   function First_Private_Parent
     (Ada_Type        : Ada_Type_Access;
      From_Visibility : Visibility_Context) return Ada_Type_Access
   is
      Parent : Ada_Type_Access;
   begin
      if Ada_Type /= Null_Ada_Type_Access then
         if not Is_Accessible
           (To_Entity_Access (Ada_Type.Entity),
            From_Visibility.File, From_Visibility.Offset)
         then
            return Ada_Type;
         else
            Parent := Get_Tagged_Parent (Ada_Type);

            if Parent /= Null_Ada_Type_Access then
               return First_Private_Parent (Parent, From_Visibility);
            else
               return Null_Ada_Type_Access;
            end if;
         end if;
      else
         return Null_Ada_Type_Access;
      end if;
   end First_Private_Parent;

   ---------------------
   -- Get_Fields_From --
   ---------------------

   function Get_Fields_From
     (Ada_Type       : Ada_Type_Access;
      Starting_After : Ada_Type_Access) return Entity_Array
   is
   begin
      if Ada_Type /= Null_Ada_Type_Access
        and then  Starting_After /= Ada_Type
      then
         declare
            Entity : constant Entity_Access :=
              To_Entity_Access (Ada_Type.Entity);
            File   : constant Structured_File_Access := Get_File (Entity);
            Tree   : constant Construct_Tree := Get_Tree (File);

            Result : Entity_Array
              (1 .. Get_Child_Number (To_Construct_Tree_Iterator (Entity)));

            Result_It : Integer := 1;
            Scope : constant Construct_Tree_Iterator :=
              To_Construct_Tree_Iterator (Entity);
            It : Construct_Tree_Iterator := Next (Tree, Scope, Jump_Into);

            Parent : Ada_Type_Access;
         begin

            for J in Result'Range loop
               if Get_Construct (It).Category = Cat_Field
                 or else Get_Construct (It).Category = Cat_Discriminant
               then
                  Result (Result_It) := To_Entity_Access (File, It);
                  Result_It := Result_It + 1;
               end if;

               It := Next (Tree, It, Jump_Into);
            end loop;

            Parent := Get_Tagged_Parent (Ada_Type);

            return Get_Fields_From (Parent, Starting_After)
              & Result (1 .. Result_It - 1);
         end;
      end if;

      return Entity_Array'(1 .. 0 => Null_Entity_Access);
   end Get_Fields_From;

   -----------------------
   -- Analyze_All_Types --
   -----------------------

   procedure Analyze_All_Types (File : Structured_File_Access) is
      Tree : constant Construct_Tree := Get_Tree (File);
      It   : Construct_Tree_Iterator := First (Tree);
   begin
      while It /= Null_Construct_Tree_Iterator loop
         if Get_Construct (It).Category = Cat_Class then
            declare
               Excluded : Excluded_Stack_Type;
            begin
               Ref (Excluded);
               Perform_Type_Analyzis_If_Needed
                 (To_Entity_Access (File, It), Excluded);
               Unref (Excluded);
            end;
         end if;

         It := Next (Tree, It, Jump_Into);
      end loop;
   end Analyze_All_Types;

   ---------
   -- Ref --
   ---------

   procedure Ref (Sb : Ada_Primitive_Access) is
   begin
      Sb.Refs := Sb.Refs + 1;
   end Ref;

   ----------
   -- Unref --
   -----------

   procedure Unref (Sb : in out Ada_Primitive_Access) is
      procedure Free is new Standard.Ada.Unchecked_Deallocation
        (Primitive_Subprogram, Ada_Primitive_Access);
   begin
      Sb.Refs := Sb.Refs - 1;

      if Sb.Refs = 0 then
         Unref (Sb.Entity);

         if Sb.Overridden_Entities /= null then
            for J in Sb.Overridden_Entities'Range loop
               Unref (Sb.Overridden_Entities (J));
            end loop;

            Free (Sb.Overridden_Entities);
         end if;

         Free (Sb);
      end if;
   end Unref;

end Ada_Semantic_Tree.Type_Tree;
