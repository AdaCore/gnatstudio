-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2007, AdaCore                    --
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

with Ada.Containers.Ordered_Maps;
with Ada.Characters.Handling;     use Ada.Characters.Handling;

with String_Utils;              use String_Utils;

with Language;                   use Language;
with Language.Tree.Ada;          use Language.Tree.Ada;
with Ada_Semantic_Tree.Declarations;  use Ada_Semantic_Tree.Declarations;
with Ada_Semantic_Tree.Entity_Iteration;
use Ada_Semantic_Tree.Entity_Iteration;
with Ada_Semantic_Tree.Parts;         use Ada_Semantic_Tree.Parts;
with Ada_Semantic_Tree.Visibility;    use Ada_Semantic_Tree.Visibility;

package body Ada_Semantic_Tree.Dependency_Tree is

   Ada_Dependency_Assistant_Id : constant String := "ADA_DEPENDENCY_ASSISTANT";

   type Dependency_Info_Annotation is new
     Construct_Annotations_Pckg.General_Annotation_Record
   with record
      Entity : Entity_Persistent_Access;
   end record;

   type Dependency_Assistant is new Database_Assistant with record
      null;
   end record;

   procedure File_Updated
     (Assistant : access Dependency_Assistant;
      File      : Structured_File_Access;
      Kind      : Update_Kind);

   function Get_Parents_List (Unit : Unit_Access) return Unit_Array_Access;

   function Get_Dep_Entity
     (Key    : Construct_Annotations_Pckg.Annotation_Key;
      Entity : Entity_Access) return Entity_Access;

   procedure Update_Dependency_Information
     (Units                 : Unit_Array_Access;
      First_Index_To_Update : Integer);

   type Scope_Info;

   type Scope_Info_Access is access all Scope_Info;

   type Scope_Info is record
      Visible_Nested_Packages : Entity_List.List;
      With_Clauses            : Entity_List.List;
      Use_Clauses             : Entity_List.List;

      Parent                  : Scope_Info_Access := null;
      Spec                    : Scope_Info_Access := null;

      Entity                  : Entity_Access;
   end record;
   --  This type holds the dependency list related to a given scope. It's just
   --  dependencies added for that scope - in order to have all dep infos,
   --  it's necessary to analyze parent scopes as well.

   package Dependency_Assocation is new Ada.Containers.Ordered_Maps
     (Entity_Access, Scope_Info_Access);

   use Dependency_Assocation;

   procedure Initialize_Ordered_Results
     (It : in out Local_Visible_Construct_Iterator);
   --  Initialize the ordered result field of the iterator given in parameter.
   --  Other iterating information have to be set first.

   ---------------
   -- Is_Hidden --
   ---------------

   function Is_Hidden
     (Resolver : Visibility_Resolver; Name : String) return Boolean
   is
      Name_Lower : constant String := To_Lower (Name);

      Cur        : Entity_List.Cursor;
      Cur_Entity : Entity_Access;
      Construct  : access Simple_Construct_Information;
   begin
      if not Contains (Resolver.Hiding_Entities.all, Name_Lower) then
         return False;
      end if;

      Cur := First (Element (Resolver.Hiding_Entities.all, Name_Lower).all);

      while Cur /= Entity_List.No_Element loop
         Cur_Entity := Element (Cur);
         Construct := Get_Construct (Cur_Entity);

         --  Check category

         if Construct.Category not in Subprogram_Category then
            --  If we found a non subprogram, then it's an hinding entity.

            return True;
         end if;

         Cur := Next (Cur);
      end loop;

      return False;
   end Is_Hidden;

   ---------------
   -- Is_Hidden --
   ---------------

   function Is_Hidden
     (Resolver : Visibility_Resolver; Entity : Entity_Access) return Boolean
   is
      Name_Lower : constant String :=
        To_Lower (Get_Construct (Entity).Name.all);

      Cur        : Entity_List.Cursor;
      Cur_Entity : Entity_Access;
      Construct  : access Simple_Construct_Information;
   begin
      if not Contains (Resolver.Hiding_Entities.all, Name_Lower) then
         return False;
      end if;

      Cur := First (Element (Resolver.Hiding_Entities.all, Name_Lower).all);

      while Cur /= Entity_List.No_Element loop
         Cur_Entity := Element (Cur);
         Construct := Get_Construct (Cur_Entity);

         --  Check category

         if Construct.Category not in Subprogram_Category then
            --  If we found a non subprogram, then it's an hinding entity.

            return True;
         end if;

         --  ??? If it's a subprogram, we should check the profile. Similar
         --  profiles will lead to hiding

         Cur := Next (Cur);
      end loop;

      return False;
   end Is_Hidden;

   -----------------------
   -- Add_Hiding_Entity --
   -----------------------

   procedure Add_Hiding_Entity
     (Resolver : in out Visibility_Resolver; Entity : Entity_Access)
   is
      Name_Lower : constant String := To_Lower
        (Get_Construct (Entity).Name.all);
      List       : Entity_List_Access;
   begin
      if not Contains (Resolver.Hiding_Entities.all, Name_Lower) then
         List := new Entity_List.List;

         Insert (Resolver.Hiding_Entities.all, Name_Lower, List);
      else
         List := Element (Resolver.Hiding_Entities.all, Name_Lower);
      end if;

      Append (List.all, Entity);
   end Add_Hiding_Entity;

   -----------
   -- Clear --
   -----------

   procedure Clear (Resolver : in out Visibility_Resolver) is
      Cur : Named_Entities.Cursor := First (Resolver.Hiding_Entities.all);

      procedure Free is new Standard.Ada.Unchecked_Deallocation
        (Entity_List.List, Entity_List_Access);
      List       : Entity_List_Access;
   begin
      while Cur /= Named_Entities.No_Element loop
         List := Element (Cur);
         Free (List);

         Cur := Next (Cur);
      end loop;

      Clear (Resolver.Hiding_Entities.all);
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear (Resolver : in out Visibility_Resolver; Name : String) is
      Name_Lower : constant String := To_Lower (Name);
      List       : Entity_List_Access;

      procedure Free is new Standard.Ada.Unchecked_Deallocation
        (Entity_List.List, Entity_List_Access);
   begin
      if not Contains (Resolver.Hiding_Entities.all, Name_Lower) then
         return;
      else
         List := Element (Resolver.Hiding_Entities.all, Name_Lower);
      end if;

      Delete (Resolver.Hiding_Entities.all, Name_Lower);
      Free (List);
   end Clear;

   ----------
   -- Free --
   ----------

   procedure Free (Resolver : in out Visibility_Resolver) is
      procedure Free is new Standard.Ada.Unchecked_Deallocation
        (Named_Entities.Map, Named_Entities_Access);
   begin
      Clear (Resolver);
      Free (Resolver.Hiding_Entities);
   end Free;

   ----------------------------
   -- Get_Visible_Constructs --
   ----------------------------

   function Get_Local_Visible_Constructs
     (File       : Structured_File_Access;
      Offset     : Natural;
      Name       : Distinct_Identifier;
      Visibility : not null access Visibility_Resolver;
      Use_Wise   : Boolean := True;
      Is_Partial : Boolean := False)
      return Entity_Array
   is
      Units : Unit_Array_Access;

      Construct_At_Location : constant Construct_Tree_Iterator :=
        Get_Iterator_At
          (Tree              => Get_Tree (File),
           Location          => To_Location (Offset),
           From_Type         => Start_Construct,
           Position          => Enclosing,
           Categories_Seeked => Null_Category_Array);
      Unit_At_Location : constant Unit_Access := Get_Owning_Unit
        (File, Offset);
      Entity_At_Location : constant Entity_Access := To_Entity_Access
        (File, Construct_At_Location);

      Use_Entity : Entity_Access;

      Tmp_List : Entity_List.List;

      Current_Unit : Unit_Access;

      type Visibility_Priority_Enum is
        (Left_Visible, Right_Visible, Both_Visible);

      function Get_Visibility_Priority
        (Left, Right : Entity_Access) return Visibility_Priority_Enum;
      --  Given two constructs, this procedure states wether one should hide
      --  the other, of if the two are visible. This visibility is performed
      --  trough a scope aware test.

      procedure Add_If_Needed
        (Entity : Entity_Access; From_Main_Loop : Boolean);

      procedure Handle_Enumeration
        (File : Structured_File_Access; It : in out Construct_Tree_Iterator);

      -----------------------------
      -- Get_Visibility_Priority --
      -----------------------------

      function Get_Visibility_Priority
        (Left, Right : Entity_Access) return Visibility_Priority_Enum
      is
         Left_It  : constant Construct_Tree_Iterator :=
           To_Construct_Tree_Iterator (Left);
         Right_It : constant Construct_Tree_Iterator :=
           To_Construct_Tree_Iterator (Right);

         Left_Tree : constant Construct_Tree := Get_Tree (Get_File (Left));
         Right_Tree : constant Construct_Tree := Get_Tree (Get_File (Right));
      begin
         if Get_Construct (Left).Category in Subprogram_Category
           and then Get_Construct (Right).Category in Subprogram_Category
         then
            return Both_Visible;
         end if;

         declare
            Left_Path : constant Construct_Tree_Iterator_Array :=
              Full_Construct_Path (Left_Tree, Left_It);
            Right_Path : constant Construct_Tree_Iterator_Array :=
              Full_Construct_Path (Right_Tree, Right_It);

            --  Going here is rare - that's why it's better to call this
            --  construct path here instead of storing it at the upper level,
            --  which would make it called systematically.
            Construct_Path : constant Construct_Tree_Iterator_Array :=
              Full_Construct_Path
                (Tree   => Get_Tree (File),
                 Offset => Offset);

            Left_Is_Prefix : constant Boolean :=
              Get_Location_Relation
                (Left_Tree,
                 Left_Path (Left_Path'First .. Left_Path'Last - 1),
                 Get_Tree (File),
                 Construct_Path) /= None;

            Right_Is_Prefix : constant Boolean :=
              Get_Location_Relation
                (Right_Tree,
                 Right_Path (Right_Path'First .. Right_Path'Last - 1),
                 Get_Tree (File),
                 Construct_Path) /= None;
         begin
            if not Left_Is_Prefix and then not Right_Is_Prefix then
               return Both_Visible;
            elsif Left_Is_Prefix and then not Right_Is_Prefix then
               return Left_Visible;
            elsif not Left_Is_Prefix and then Right_Is_Prefix then
               return Right_Visible;
            elsif Left_Path'Length > Right_Path'Length then
               return Left_Visible;
            elsif Left_Path'Length < Right_Path'Length then
               return Right_Visible;
            else
               return Both_Visible;
            end if;
         end;
      end Get_Visibility_Priority;

      -------------------
      -- Add_If_Needed --
      -------------------

      procedure Add_If_Needed
        (Entity : Entity_Access; From_Main_Loop : Boolean)
      is
      begin
         if Is_Compilation_Unit (To_Construct_Tree_Iterator (Entity)) then
            --  If we're on a compilation unit, then we have to retreive the
            --  unit composite name and consider only the relevant (last) part
            --  of it.

            declare
               Comp_Name : constant Composite_Identifier :=
                 Get_Name (Current_Unit);
            begin
               if not Match
                 (Name,
                  Get_Database (File).Get_Identifier
                  (Get_Item (Comp_Name, Length (Comp_Name))),
                  Is_Partial)
               then
                  return;
               end if;
            end;
         else
            if Get_Construct (Entity).Name = null
              or else Get_Construct (Entity).Category
            not in Cat_Package .. Cat_Literal
              or else not Match
                (Name, Get_Identifier (Entity), Is_Partial)
            then
               return;
            end if;
         end if;

         declare
            Tmp : Entity_List.Cursor;
            Found_It : Entity_List.Cursor;
            Found    : Entity_Access;
            Do_Addition : Boolean := True;
         begin
            Found_It := First (Tmp_List);

            while Found_It /= Entity_List.No_Element loop
               Found := Element (Found_It);

               if Get_Identifier (Found) = Get_Identifier (Entity) then
                  case Get_Visibility_Priority (Found, Entity) is
                  when Left_Visible =>
                     --  If we found a entity more visible than this one, we
                     --  dismiss this one.

                     Do_Addition := False;
                     exit;
                  when Right_Visible =>
                     --  If this entity is more visible than an other one
                     --  we remove that other one.

                     Tmp := Found_It;
                     Found_It := Next (Found_It);
                     Delete (Tmp_List, Tmp);

                  when Both_Visible =>
                     --  If both entities have the same visibility, then
                     --  either they are two parts of the same one, and we
                     --  keep only the relevant one, or we keep the two.

                     if Are_Same_Entity (Found, Entity) then
                        case Get_Construct (Entity).Category is
                           when Cat_Package =>
                              Do_Addition := False;
                              exit;

                           when others =>
                              Tmp := Found_It;
                              Found_It := Next (Found_It);
                              Delete (Tmp_List, Tmp);

                        end case;
                     else
                        Found_It := Next (Found_It);
                     end if;

                  end case;
               else
                  Found_It := Next (Found_It);
               end if;
            end loop;

            if Do_Addition then
               Prepend (Tmp_List, Entity);

               if From_Main_Loop then
                  --  If this is coming from the main loop, it means that this
                  --  entity is on the hierarchy scope, and thus will hide any
                  --  other entity.

                  Add_Hiding_Entity (Visibility.all, Entity);

                  --  ??? We should remove entities that are hidden by further
                  --  ones, even if it doesn't do any harm right now in the
                  --  algorithm.
               end if;
            end if;
         end;
      end Add_If_Needed;

      ------------------------
      -- Handle_Enumeration --
      ------------------------

      procedure Handle_Enumeration
        (File : Structured_File_Access; It : in out Construct_Tree_Iterator)
      is
         Tree : constant Construct_Tree := Get_Tree (File);
         Enum : constant Construct_Tree_Iterator := It;
      begin
         It := Get_Last_Child (Tree, It);

         while It /= Enum loop
            Add_If_Needed
              (To_Entity_Access (File, It), True);

            It := Prev (Tree, It);
         end loop;

         It := Next (Tree, Enum, Jump_Over);
      end Handle_Enumeration;

      Current_File : Structured_File_Access;
      Current_Tree : Construct_Tree;
      Current_Entity : Entity_Access;
      It : Construct_Tree_Iterator;
      Last_Timestamp : Integer := 0;
      Parts_Assistant : constant Database_Assistant_Access :=
        Parts.Get_Assistant (Get_Database (File));
      End_Entity : Entity_Access;
      End_Entity_It : Construct_Tree_Iterator;
   begin
      --  Set up and clear the visibility resolver

      if Visibility.Hiding_Entities = null then
         Visibility.Hiding_Entities := new Named_Entities.Map;
      end if;

      Clear (Visibility.all);

      if Construct_At_Location = Null_Construct_Tree_Iterator
        or else Unit_At_Location = Null_Unit_Access
      then
         --  If there is no enclosing construct or enclosing unit, there is
         --  no local construct to be found - return an empty array.

         declare
            Result : Entity_Array (1 .. 0);
         begin
            return Result;
         end;
      end if;

      Parts.Analyze_Unit (Parts_Assistant, Unit_At_Location);

      Units := Get_Parents_List (Get_Owning_Unit (File, Offset));

      --  See if we need to recompute with / use info somewhere.

      for J in Units'Range loop
         if not Has_Updated_Dependencies (Units (J))
           or else
             (J > Units'First
              and then Get_Dependency_Timestamp (Units (J)) < Last_Timestamp)
         then
            --  If there's at least one file not up to date, then we've got
            --  to recompute the whole dependency hierarchy.

            Update_Dependency_Information (Units, J);
            exit;
         end if;

         Last_Timestamp := Get_Current_Timestamp (Units (J));
      end loop;

      for Unit_Index in Units'Range loop
         Parts.Analyze_Unit (Parts_Assistant, Units (Unit_Index));
         Current_File := Get_File (Get_Entity (Units (Unit_Index)));
         Current_Tree := Get_Tree (Current_File);
         It := To_Construct_Tree_Iterator
           (Get_Start_Entity (Units (Unit_Index)));
         Current_Unit := Units (Unit_Index);
         End_Entity := Get_End_Entity (Units (Unit_Index));
         End_Entity_It := To_Construct_Tree_Iterator (End_Entity);

         while It /= End_Entity_It
           and then
             (Unit_Index /= Units'Last
              or else Get_Construct (It).Sloc_Start.Index < Offset)
         loop
            Current_Entity := To_Entity_Access (Current_File, It);

            Add_If_Needed (Current_Entity, True);

            if Unit_Index = Units'Last and then Encloses (It, Offset) then
               --  If the offset is in this package, jump into

               It := Next (Current_Tree, It, Jump_Into);
            elsif Get_Construct (It).Category = Cat_Package
              and then Get_Construct (It).Is_Declaration
              and then
                (Is_Compilation_Unit (It)
                 or else Unchecked_Is_In_Scope
                   (Parts_Assistant, Current_Entity, Entity_At_Location))
            then
               --  We jump into in several cases:
               --    we are on the same direct scope hierarchy
               --      (tested by is_same_scope)
               --    we are on the compilation unit, that is to say probably
               --      a parent of the actual unit.

               It := Next (Current_Tree, It, Jump_Into);
            elsif Get_Construct (It).Category = Cat_Use
              and then Use_Wise then
               --  Seek this package and jump in its spec if it's a local one,
               --  that is to say if it's found in a parent file.

               Use_Entity :=
                 Get_Dep_Entity
                   (Get_Ref_Key (Get_Database (File)),
                    Current_Entity);
               --  ??? Add a mechanism to avoid going twice on the same
               --  package!

               if Use_Entity /= Null_Entity_Access
                 and then Is_In_Parents
                   (Get_Owning_Unit (Use_Entity), Units (Unit_Index))
               then
                  declare
                     Use_Scope    : constant Construct_Tree_Iterator
                       := To_Construct_Tree_Iterator (Use_Entity);
                     Use_Iterator : Construct_Tree_Iterator := Next
                       (Get_Tree (Get_File (Use_Entity)),
                        Use_Scope,
                        Jump_Into);
                  begin
                     while Is_Parent_Scope (Use_Scope, Use_Iterator)
                       and then Get_Construct
                         (Use_Iterator).Visibility = Visibility_Public
                     loop
                        Add_If_Needed
                          (To_Entity_Access
                             (Get_File (Use_Entity), Use_Iterator),
                           False);

                        if Is_Enum_Type
                          (Get_Tree (Get_File (Use_Entity)), Use_Iterator)
                        then
                           Handle_Enumeration
                             (Get_File (Use_Entity), Use_Iterator);
                        else
                           Use_Iterator := Next
                             (Get_Tree (Get_File (Use_Entity)),
                              Use_Iterator, Jump_Over);
                        end if;
                     end loop;
                  end;
               end if;

               It := Next (Current_Tree, It, Jump_Over);
            elsif Is_Enum_Type (Current_Tree, It) then
               --  It seems more logical to have the enumerations in the
               --  right order, that's why we have a special treatment here.

               Handle_Enumeration (Current_File, It);
            else
               It := Next (Current_Tree, It, Jump_Over);
            end if;
         end loop;
      end loop;

      declare
         Result : Entity_Array
           (1 .. Integer (Length (Tmp_List)));

         Node : Entity_List.Cursor := First (Tmp_List);
      begin
         for J in Result'Range loop
            Result (J) := Element (Node);

            Node := Next (Node);
         end loop;

         Free (Units);

         return Result;
      end;
   end Get_Local_Visible_Constructs;

   ------------------------
   -- Is_Locally_Visible --
   ------------------------

   function Is_Locally_Visible
     (File     : Structured_File_Access;
      Offset   : Natural;
      Entity   : Entity_Access;
      Use_Wise : Boolean := True) return Boolean
   is
      Entity_Unit : constant Unit_Access := Get_Owning_Unit (Entity);
      Local_Unit  : constant Unit_Access := Get_Owning_Unit
        (File, Offset);
      Visibility : aliased Visibility_Resolver;
   begin
      if Get_Construct (Entity).Name = null
        or else not Is_In_Parents (Entity_Unit, Local_Unit)
      then
         return False;
      end if;

      declare
         Entities : constant Entity_Array := Get_Local_Visible_Constructs
           (File,
            Offset,
            Get_Identifier (Entity),
            Visibility'Access,
            Use_Wise,
            False);
      begin
         for J in Entities'Range loop
            if Entity = Entities (J) then
               return True;
            end if;
         end loop;
      end;

      return False;
   end Is_Locally_Visible;

   ------------------------
   -- Register_Assistant --
   ------------------------

   procedure Register_Assistant (Database : Construct_Database_Access) is
   begin
      Register_Assistant
        (Database, Ada_Dependency_Assistant_Id, new Dependency_Assistant);
   end Register_Assistant;

   ------------------
   -- File_Updated --
   ------------------

   procedure File_Updated
     (Assistant : access Dependency_Assistant;
      File      : Structured_File_Access;
      Kind      : Update_Kind)
   is
      pragma Unreferenced (Assistant);
   begin
      case Kind is
         when Minor_Change =>
            null;

         when Full_Change | Structural_Change =>
            --  Reset the dependency timestamp for all units.

            declare
               It   : Unit_Iterator := Get_Units (File);
            begin
               while not At_End (It) loop
                  Set_Updated_Dependencies (Get (It), False);

                  Next (It);
               end loop;
            end;
      end case;
   end File_Updated;

   ----------------------
   -- Get_Parents_List --
   ----------------------

   function Get_Parents_List (Unit : Unit_Access) return Unit_Array_Access is
      Number_Of_Units : Integer := 0;
      First_Unit      : Unit_Access;
      Current_Unit    : Unit_Access;

      Result          : Unit_Array_Access;
   begin
      First_Unit := Unit;
      Current_Unit := First_Unit;

      while Current_Unit /= Null_Unit_Access loop
         Number_Of_Units := Number_Of_Units + 1;

         Current_Unit := Get_Parent (Current_Unit);
      end loop;

      Result := new Unit_Array (1 .. Number_Of_Units);

      Current_Unit := First_Unit;

      for J in reverse Result'Range loop
         Result (J) := Current_Unit;

         Current_Unit := Get_Parent (Current_Unit);
      end loop;

      return Result;
   end Get_Parents_List;

   --------------------
   -- Get_Dep_Entity --
   --------------------

   function Get_Dep_Entity
     (Key    : Construct_Annotations_Pckg.Annotation_Key;
      Entity : Entity_Access) return Entity_Access
   is
      use Construct_Annotations_Pckg;

      Obj : Construct_Annotations_Pckg.Annotation;
   begin
      Get_Annotation
        (Get_Annotation_Container
           (Get_Tree (Get_File (Entity)),
            To_Construct_Tree_Iterator (Entity)).all,
         Key,
         Obj);

      if Obj = Construct_Annotations_Pckg.Null_Annotation then
         return Null_Entity_Access;
      else
         return To_Entity_Access
           (Dependency_Info_Annotation (Obj.Other_Val.all).Entity);
      end if;
   end Get_Dep_Entity;

   -----------------------------------
   -- Update_Dependency_Information --
   -----------------------------------

   procedure Update_Dependency_Information
     (Units                 : Unit_Array_Access;
      First_Index_To_Update : Integer)
   is
      It           : Construct_Tree_Iterator;
      Tree         : Construct_Tree;
      Dependencies : Dependency_Assocation.Map;

      Max_Depth   : Integer := 0;
      --  This variable holds the maximum depth of scopes -  needed when we
      --  want to constraint an array for a scope hierarchy analysis.

      Current_Scope  : Scope_Info_Access;

      Ref_Key : constant Construct_Annotations_Pckg.Annotation_Key :=
        Get_Ref_Key
          (Get_Database (Get_File (Get_Entity (Units (Units'First)))));

      procedure Handle_Current_Location (It : Construct_Tree_Iterator);

      function Get_Dependency_Information
        (Entity : Entity_Access; Parent_Info : Scope_Info_Access)
         return Scope_Info_Access;

      -----------------------------
      -- Handle_Current_Location --
      -----------------------------

      procedure Handle_Current_Location (It : Construct_Tree_Iterator) is
         function Is_Parent
           (Parent, Child : Entity_Access) return Boolean;

         function Get_Unit
           (Construct : Simple_Construct_Information) return Entity_Access;

         function Is_Parent
           (Parent, Child : Entity_Access) return Boolean
         is
            Parent_Name : constant Composite_Identifier :=
              To_Composite_Identifier
                (Get_Construct (Parent).Name.all);
            Child_Name : constant Composite_Identifier :=
              To_Composite_Identifier
                (Get_Construct (Child).Name.all);
         begin
            return Equal
              (Get_Slice
                 (Child_Name, 1, Length (Child_Name) - 1), Parent_Name, False);
         end Is_Parent;

         --------------
         -- Get_Unit --
         --------------

         function Get_Unit
           (Construct : Simple_Construct_Information) return Entity_Access
         is
            Id : constant Composite_Identifier :=
              To_Composite_Identifier (Construct.Name.all);

            Name_It : Construct_Db_Iterator;

            Root   : Entity_Access := Null_Entity_Access;
            Entity : Entity_Access;

            Potential_Packages : Entity_List.List;
            Entity_Iterator    : Entity_List.Cursor;

            Result : Entity_Access;

            type Scope_Info_Array is array
              (Integer range <>) of Scope_Info_Access;

            Ordered_Scopes   : Scope_Info_Array (1 .. Max_Depth * 2);
            --  We want to store a whole scope hierarchy in this variable,
            --  potentially a spec and a body for each element of the scope
            --  hierarchy.

            Ordered_Length : Integer := 0;
            Length_Increment : Integer;
         begin

            if Length (Id) = 0 then
               return Null_Entity_Access;
            end if;

            --  Set the order of the deps from which we've got to look for,
            --  that is to say on each scope, we look first from the use from
            --  the spec and then retreive the ones from the parent,
            --  recursively.

            Ordered_Scopes (1) := Current_Scope;
            Ordered_Length := 1;

            while Ordered_Length <= Ordered_Scopes'Length
              and then
                (Ordered_Scopes (Ordered_Length).Parent /= null
                 or else Ordered_Scopes (Ordered_Length).Spec /= null)
            loop
               Length_Increment := 0;

               if Ordered_Scopes (Ordered_Length).Spec /= null
                 and then
                   (Ordered_Scopes (Ordered_Length).Parent = null
                    or else
                      Ordered_Scopes (Ordered_Length).Parent /=
                      Ordered_Scopes (Ordered_Length).Spec)
               then
                  Ordered_Scopes (Ordered_Length + 1) :=
                    Ordered_Scopes (Ordered_Length).Spec;
                  Length_Increment := Length_Increment + 1;
               end if;

               if Ordered_Scopes (Ordered_Length).Parent /= null then
                  Ordered_Scopes (Ordered_Length + 1 + Length_Increment) :=
                    Ordered_Scopes (Ordered_Length).Parent;
                  Length_Increment := Length_Increment + 1;
               end if;

               Ordered_Length := Ordered_Length + Length_Increment;
            end loop;

            --  Look from the local nested packages

            if Root = Null_Entity_Access then
               Unit_Loop : for J in 1 .. Ordered_Length loop
                  Entity_Iterator := First
                    (Ordered_Scopes (J).Visible_Nested_Packages);

                  while Entity_Iterator /= Entity_List.No_Element loop
                     Entity := Element (Entity_Iterator);

                     if Equal
                       (Get_Construct (Entity).Name.all,
                        Get_Item (Id, 1),
                        False)
                     then
                        Root := Entity;

                        exit Unit_Loop;
                     end if;

                     Entity_Iterator := Next (Entity_Iterator);
                  end loop;
               end loop Unit_Loop;
            end if;

            --  If we still didn't find the root, check if there's a root unit
            --  with this exact name.

            if Root = Null_Entity_Access then
               Unit_Loop_1 : for J in 1 .. Ordered_Length loop
                  Name_It := Start
                    (Db         => Get_Database
                       (Get_File (Ordered_Scopes (J).Entity)),
                     Prefix     => To_Lower (Get_Item (Id, 1)),
                     Is_Partial => False);

                  while not At_End (Name_It) loop
                     Entity :=
                       To_Entity_Access
                         (File       => Get_File (Name_It),
                          Construct  => Get_Construct (Name_It));

                     if Get_Parent_Scope
                       (Get_Tree (Get_File (Entity)),
                        To_Construct_Tree_Iterator (Entity))
                       = Null_Construct_Tree_Iterator
                     then
                        --  If the name is a root name, then that's it
                        if Length
                          (To_Composite_Identifier
                             (Get_Construct
                                (Get_Construct (Name_It)).Name.all))
                          = 1
                        then
                           Root := Entity;

                           Free (Name_It);

                           exit Unit_Loop_1;
                        end if;

                        --  Otherwise, we'll see later on if it's a child of a
                        --  used package.
                        Append (Potential_Packages, Entity);
                     end if;

                     Next (Name_It);
                  end loop;

                  Free (Name_It);
               end loop Unit_Loop_1;
            end if;

            --  If we didn't find the root unit, then check if one of the
            --  potential package is a parent of one of the already used units.

            if Root = Null_Entity_Access then
               Unit_Loop_2 : for J in 1 .. Ordered_Length loop
                  Entity_Iterator := First (Potential_Packages);

                  while Entity_Iterator /= Entity_List.No_Element loop

                     Entity := Element (Entity_Iterator);

                     declare
                        Use_Entity_Iterator : Entity_List.Cursor;
                        Use_Entity : Entity_Access;
                     begin
                        Use_Entity_Iterator := First
                          (Ordered_Scopes (J).Use_Clauses);

                        while Use_Entity_Iterator /= Entity_List.No_Element
                        loop
                           --  If the use is the parent of the potential name,
                           --  then we the potential name is our root !

                           Use_Entity := Element (Use_Entity_Iterator);

                           if Use_Entity /= Null_Entity_Access
                             and then Is_Parent
                               (Use_Entity, Element (Entity_Iterator))
                           then
                              Root := Element (Entity_Iterator);

                              exit Unit_Loop_2;
                           end if;

                           Use_Entity_Iterator := Next (Use_Entity_Iterator);
                        end loop;
                     end;

                     Entity_Iterator := Next (Entity_Iterator);
                  end loop;
               end loop Unit_Loop_2;
            end if;

            --  If we still didn't find the root, check if there's a child of
            --  a used package

            if Root = Null_Entity_Access then
               Unit_Loop_3 : for J in 1 .. Ordered_Length loop
                  declare
                     Use_Iterator : Entity_List.Cursor :=
                       First (Ordered_Scopes (J).Use_Clauses);
                  begin
                     while Use_Iterator /= Entity_List.No_Element loop
                        if Element (Use_Iterator) /= Null_Entity_Access then
                           declare
                              Root_It : constant Construct_Tree_Iterator :=
                                To_Construct_Tree_Iterator
                                  (Element (Use_Iterator));
                              Tree : constant Construct_Tree :=
                                Get_Tree (Get_File (Element (Use_Iterator)));
                              Nested_It : Construct_Tree_Iterator := Next
                                (Tree, Root_It, Jump_Into);
                           begin
                              while Nested_It /= Null_Construct_Tree_Iterator
                                and then Is_Parent_Scope (Root_It, Nested_It)
                              loop
                                 if Get_Construct (Nested_It).Category
                                   = Cat_Package
                                   and then Equal
                                     (Get_Construct (Nested_It).Name.all,
                                      Get_Item (Id, 1),
                                      False)
                                 then
                                    Root :=
                                      To_Entity_Access
                                        (File       => Get_File
                                             (Element (Use_Iterator)),
                                         Construct  => Nested_It);

                                    exit Unit_Loop_3;
                                 end if;

                                 Nested_It := Next
                                   (Tree, Nested_It, Jump_Over);
                              end loop;
                           end;
                        end if;

                        Use_Iterator := Next (Use_Iterator);
                     end loop;
                  end;
               end loop Unit_Loop_3;
            end if;

            --  If no root is found at this stage, then we can't retreive the
            --  package, return null.

            if Root = Null_Entity_Access then
               return Null_Entity_Access;
            end if;

            --  For all remaining items of the clause name

            Result := Root;

            for J in 2 .. Length (Id) loop
               --  ??? We probably could save some time here by re-using the
               --  dependency structure that we've just built instead of
               --  recomputing everything trough a semantic iterator...

               Result := Null_Entity_Access;

               declare
                  Sem_It : Semantic_Tree_Iterator;
                  Visibility : Visibility_Context := Null_Visibility_Context;
               begin
                  Visibility.Filter := Everything;

                  Sem_It := To_Semantic_Tree_Iterator
                    ((Root, None), Visibility);

                  while not At_End (Sem_It) loop
                     if Get_Construct (Get (Sem_It).Entity).Category
                       = Cat_Package
                       and then Equal
                         (Get_Construct (Get (Sem_It).Entity).Name.all,
                          Get_Item (Id, J),
                          False)
                     then
                        Result := Get (Sem_It).Entity;

                        exit;
                     end if;

                     Next (Sem_It);
                  end loop;

                  Free (Sem_It);
               end;

               if Result = Null_Entity_Access then
                  return Result;
               end if;

               Root := Result;
            end loop;

            return Result;
         end Get_Unit;

         Unit : Entity_Access;
      begin
         if Get_Construct (It).Category = Cat_With
           or else Get_Construct (It).Category = Cat_Use
         then
            Unit := Get_Unit (Get_Construct (It).all);

            if Unit /= Null_Entity_Access then
               Construct_Annotations_Pckg.Set_Annotation
                 (Get_Annotation_Container (Tree, It).all,
                  Ref_Key,
                  (Kind      => Construct_Annotations_Pckg.Other_Kind,
                   Other_Val =>
                   new Dependency_Info_Annotation'
                     (Entity => To_Entity_Persistent_Access (Unit))));

               if Get_Construct (It).Category = Cat_With then
                  Append (Current_Scope.With_Clauses, Unit);
               else
                  Append (Current_Scope.Use_Clauses, Unit);
               end if;
            end if;
         elsif Get_Construct (It).Category = Cat_Package
           and then Get_Construct (It).Is_Declaration
         then
            --  Store this in the list of nested packages - we may get further
            --  use clauses to it...

            Prepend
              (Current_Scope.Visible_Nested_Packages,
               To_Entity_Access
                 (File       => Get_File (Current_Scope.Entity),
                  Construct  => It));
         end if;
      end Handle_Current_Location;

      --------------------------------
      -- Get_Dependency_Information --
      --------------------------------

      function Get_Dependency_Information
        (Entity : Entity_Access; Parent_Info : Scope_Info_Access)
         return Scope_Info_Access
      is
         It : constant Construct_Tree_Iterator :=
           To_Construct_Tree_Iterator (Entity);
         Result : Scope_Info_Access;
      begin
         if Get_Construct (It).Is_Declaration then
            --  In this case, we're on a new package scope, create
            --  the corresponding package dep.

            Result := new Scope_Info'
              (Parent => Parent_Info,
               Entity => Entity,
               others => <>);
         else
            --  Otherwise, we're on a body. See if we can find the
            --  spec, and merge it with the current info.

            declare
               First_Occ : constant Entity_Access :=
                 Get_First_Occurence (Entity);
            begin
               if First_Occ /= Null_Entity_Access
                 and then First_Occ /= Entity
                 and then Contains (Dependencies, First_Occ)
               then
                  Result := new Scope_Info'
                    (Parent  => Parent_Info,
                     Spec    => Element (Dependencies, First_Occ),
                     Entity  => Entity,
                     others  => <>);
               else
                  Result := new Scope_Info'
                    (Parent => Parent_Info,
                     Entity => Entity,
                     others => <>);
               end if;
            end;
         end if;

         Max_Depth := Max_Depth + 1;

         Insert (Dependencies, Result.Entity, Result);

         return Result;
      end Get_Dependency_Information;

      New_It : Construct_Tree_Iterator;
      End_Entity : Entity_Access;
   begin
      --  First, put the proper update flags. It's fundamental that these data
      --  are set before any further analysis, as we may need to resolve
      --  declaration on internal packages (generics or renamed) which may lead
      --  in a nested dependency analysis.

      for J in First_Index_To_Update .. Units'Last loop
         if J > 1 then
            Set_Dependency_Timestamp
              (Units (J), Get_Current_Timestamp (Units (J - 1)));
         end if;

         Set_Updated_Dependencies (Units (J), True);
      end loop;

      for J in Units'Range loop
         if Get_Start_Entity (Units (J)) /= Null_Entity_Access then

            Tree := Get_Tree (Get_File (Get_Start_Entity (Units (J))));
            It := To_Construct_Tree_Iterator (Get_Start_Entity (Units (J)));

            Current_Scope := Get_Dependency_Information
              (Get_Entity (Units (J)), null);
            End_Entity := Get_End_Entity (Units (J));

            while Current_Scope /= null
              and then It /= To_Construct_Tree_Iterator (End_Entity)
            loop
               --  Manage the current location

               if J < First_Index_To_Update then
                  --  If we're still in the up to date units, just add the
                  --  known reference to the list.

                  case Get_Construct (It).Category is
                  when Cat_Use =>
                     Prepend
                       (Current_Scope.Use_Clauses,
                        Get_Dep_Entity
                          (Ref_Key,
                           To_Entity_Access
                             (Get_File (Get_Start_Entity (Units (J))),
                              It)));

                  when Cat_With =>
                     Prepend
                       (Current_Scope.With_Clauses,
                        Get_Dep_Entity
                          (Ref_Key,
                           To_Entity_Access
                             (Get_File (Get_Start_Entity (Units (J))),
                              It)));

                  when Cat_Package =>
                     Prepend
                       (Current_Scope.Visible_Nested_Packages,
                        To_Entity_Access
                          (Get_File (Get_Start_Entity (Units (J))),
                           It));

                  when others =>
                     null;
                  end case;
               else
                  --  Otherwise, use the target use resolution

                  Handle_Current_Location (It);
               end if;

               if (Get_Construct (It).Category = Cat_Package
                   or else
                     (J >= First_Index_To_Update
                      and then Get_Construct (It).Category in
                        Cat_Package .. Cat_Entry))
                 and then not Is_Compilation_Unit (It)
               then
                  --  We stack a dep scope of there are possible items on it,
                  --  if it's not a compil unit (stacked by the enclosing loop)
                  --  and if we're potentially going to look into it.

                  Current_Scope := Get_Dependency_Information
                    (To_Entity_Access
                       (Get_File (Get_Start_Entity (Units (J))),
                        It),
                     Current_Scope);
               end if;

               --  Iterate, and create the relevant dependency information if
               --  needed

               --  We jump into the scope in two cases:
               --    The file is already up to date, in which case we only need
               --    to load the relevant use info, we jump only on the
               --    packages.
               --    The file is not up to date, so we need to update all the
               --    use clauses including the ones located within subprograms.

               if Get_Construct (It).Category = Cat_Package
                 or else J >= First_Index_To_Update
               then
                  New_It := Next (Tree, It, Jump_Into);
               else
                  New_It := Next (Tree, It, Jump_Over);
               end if;

               --  Then analyze the new iterator, if it's out the parent, we
               --  need to unroll the scopes until we reach the current one.

               if New_It /= Null_Construct_Tree_Iterator then
                  while Current_Scope /= null
                    and then not Encloses
                      (To_Construct_Tree_Iterator (Current_Scope.Entity),
                       Get_Construct (New_It).Sloc_Start.Index)
                  loop
                     Current_Scope := Current_Scope.Parent;

                     Max_Depth := Max_Depth - 1;
                  end loop;
               end if;

               It := New_It;
            end loop;
         end if;
      end loop;

      declare
         Cur : Dependency_Assocation.Cursor := First (Dependencies);

         procedure Free is new Ada.Unchecked_Deallocation
           (Scope_Info, Scope_Info_Access);

         Obj : Scope_Info_Access;
      begin
         while Cur /= Dependency_Assocation.No_Element loop
            Obj := Element (Cur);
            Free (Obj);

            Cur := Next (Cur);
         end loop;
      end;
   end Update_Dependency_Information;

   ---------------
   -- Is_Before --
   ---------------

   function Is_Before (Left, Right : Entity_Access) return Boolean is
   begin
      return To_Construct_Tree_Iterator (Left)
        < To_Construct_Tree_Iterator (Right);
   end Is_Before;

   -------------------------------
   -- Initialize_Ordered_Result --
   -------------------------------

   procedure Initialize_Ordered_Results
     (It : in out Local_Visible_Construct_Iterator)
   is
      Local_It : Local_Construct_Iterator;
   begin
      Clear (It.Ordered_Results.all);

      Local_It := First
        (It.Units (It.It_In_Units), It.Name.all, It.Is_Partial);

      while not At_End (Local_It) loop
         Insert (It.Ordered_Results.all, Get (Local_It));

         Next (Local_It);
      end loop;

      Free (Local_It);

      It.It := Last (It.Ordered_Results.all);
   end Initialize_Ordered_Results;

   -----------
   -- First --
   -----------

   function First
     (File       : Structured_File_Access;
      Offset     : Natural;
      Name       : String;
      Use_Wise   : Boolean := True;
      Is_Partial : Boolean := False)
      return Local_Visible_Construct_Iterator
   is
      pragma Unreferenced (Use_Wise);

      Result : Local_Visible_Construct_Iterator;

      Construct_At_Location : constant Construct_Tree_Iterator :=
        Get_Iterator_At
          (Tree              => Get_Tree (File),
           Location          => To_Location (Offset),
           From_Type         => Start_Construct,
           Position          => Enclosing,
           Categories_Seeked => Null_Category_Array);
      Unit_At_Location : constant Unit_Access := Get_Owning_Unit
        (File, Offset);
      Last_Timestamp : Integer := 0;
      Ref_Key        : constant Construct_Annotations_Pckg.Annotation_Key :=
        Get_Ref_Key (Get_Database (File));
   begin
      if Construct_At_Location = Null_Construct_Tree_Iterator
        or else Unit_At_Location = Null_Unit_Access
      then
         --  If there is no enclosing construct or enclosing unit, there is
         --  no local construct to be found - return an empty result.

         return Null_Local_Visible_Construct_Iterator;
      end if;

      Result.Entity_At_Location := To_Entity_Access
        (File, Construct_At_Location);
      Result.Parts_Assistant := Parts.Get_Assistant (Get_Database (File));
      Result.Units := Get_Parents_List (Get_Owning_Unit (File, Offset));

      --  See if we need to recompute with / use info somewhere.

      for J in Result.Units'Range loop
         if not Has_Updated_Dependencies (Result.Units (J))
           or else
             (J > Result.Units'First
              and then
                Get_Dependency_Timestamp (Result.Units (J)) < Last_Timestamp)
         then
            --  If there's at least one file not up to date, then we've got
            --  to recompute the whole dependency hierarchy.

            Update_Dependency_Information (Result.Units, J);
            exit;
         end if;

         Last_Timestamp := Get_Current_Timestamp (Result.Units (J));
      end loop;

      for J in Result.Units'Range loop
         --  Since we're going to use Unchecked_Is_In_Scope, we need to ensure
         --  that units are up to date.

         Parts.Analyze_Unit
           (Result.Parts_Assistant, Result.Units (J));

         --  Then we need to store all the use clause

         Result.Used_Packages := new Ordered_Entities.Set;

         declare
            File      : constant Structured_File_Access :=
              Get_File (Get_Start_Entity (Result.Units (J)));
            Tree      : constant Construct_Tree := Get_Tree  (File);
            Use_It : Construct_Tree_Iterator := To_Construct_Tree_Iterator
              (Get_Start_Entity (Result.Units (J)));
            Use_Entity : Entity_Access;
            End_It    : constant Construct_Tree_Iterator :=
              To_Construct_Tree_Iterator
                (Get_End_Entity (Result.Units (J)));
            Construct : access Simple_Construct_Information;
            Dep_Entity : Entity_Access;
         begin
            while Use_It /= End_It loop
               Construct := Get_Construct (Use_It);
               Use_Entity := To_Entity_Access (File, Use_It);

               if Construct.Category = Cat_Use then
                  Dep_Entity := Get_Dep_Entity (Ref_Key, Use_Entity);

                  if Dep_Entity /= Null_Entity_Access
                    and then not Contains
                      (Result.Used_Packages.all, Dep_Entity)
                  then
                     Insert
                       (Result.Used_Packages.all,
                        Get_Dep_Entity (Ref_Key, Use_Entity));
                  end if;

                  Use_It := Next (Tree, Use_It, Jump_Over);
               elsif J = Result.Units'Last
                 and then Encloses (Use_It, Offset)
               then
                  --  If the offset is in this package, jump into

                  Use_It := Next (Tree, Use_It, Jump_Into);
               elsif Construct.Category in Cat_Package .. Cat_Namespace
                 and then Construct.Is_Declaration
                 and then Unchecked_Is_In_Scope
                   (Result.Parts_Assistant,
                    Use_Entity,
                    Result.Entity_At_Location)
               then
                  --  If the offset is in this package, jump into

                  Use_It := Next (Tree, Use_It, Jump_Into);
               else
                  Use_It := Next (Tree, Use_It, Jump_Over);
               end if;
            end loop;
         end;
      end loop;

      Result.Name := new String'(Name);
      Result.Is_Partial := Is_Partial;
      Result.It_In_Units := Result.Units'Last;

      Result.Ordered_Results := new Ordered_Entities.Set;

      Initialize_Ordered_Results (Result);

      if not Is_Valid (Result) then
         Next (Result);
      end if;

      return Result;
   end First;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Local_Visible_Construct_Iterator) is
   begin
      loop
         if It.It /= Ordered_Entities.No_Element then
            It.It := Previous (It.It);
         elsif It.It_In_Units >= 2 then
            It.It_In_Units := It.It_In_Units - 1;

            Initialize_Ordered_Results (It);
         else
            It.It_In_Units := It.It_In_Units - 1;
         end if;

         exit when Is_Valid (It);
      end loop;
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (It : Local_Visible_Construct_Iterator) return Boolean is
   begin
      return It.It_In_Units = 0 and then It.It = Ordered_Entities.No_Element;
   end At_End;

   ---------
   -- Get --
   ---------

   function Get (It : Local_Visible_Construct_Iterator) return Entity_Access is
   begin
      return Element (It.It);
   end Get;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (It : Local_Visible_Construct_Iterator) return Boolean is
      Found              : Entity_Access;
      Parent_Found_Scope : Entity_Access;
   begin
      if At_End (It) then
         return True;
      end if;

      if It.It = Ordered_Entities.No_Element then
         return False;
      end if;

      Found := Get (It);

      --  First, check if the entity found is in the current scope of the
      --  search.

      Parent_Found_Scope := To_Entity_Access
        (Get_File (Found),
         Get_Parent_Scope
           (Get_Tree (Get_File (Found)),
            To_Construct_Tree_Iterator (Found)));

      if Parent_Found_Scope = Null_Entity_Access
        or else Unchecked_Is_In_Scope
          (It.Parts_Assistant, Parent_Found_Scope, It.Entity_At_Location)
      then
         --  If the entity found is at the highest level, then it's always
         --  visible from the entity. Otherwise, check if both are in the same
         --  hierarchy.

         return True;
      end if;

      --  If not, check if it is in a used package

      if Contains (It.Used_Packages.all, Parent_Found_Scope) then
         return True;
      end if;

      --  ??? For with visibility, we should check that it can be from a
      --  withed package.

      --  If not, then this entity is not directly visible.

      return False;
   end Is_Valid;

   ----------
   -- Free --
   ----------

   procedure Free (It : in out Local_Visible_Construct_Iterator) is
   begin
      Free (It.Units);
      Free (It.Ordered_Results);
      Free (It.Used_Packages);
   end Free;

end Ada_Semantic_Tree.Dependency_Tree;
