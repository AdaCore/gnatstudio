-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2007-2010, AdaCore                  --
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

with GNAT.Strings;     use GNAT.Strings;
with GNATCOLL.Utils;   use GNATCOLL.Utils;
with Language.Ada;     use Language.Ada;
with Ada_Semantic_Tree.Declarations; use Ada_Semantic_Tree.Declarations;
with Ada_Semantic_Tree.Units; use Ada_Semantic_Tree.Units;

package body Ada_Semantic_Tree.Visibility is

   Ada_Visibility_Id : constant String := "ADA_LIB_VISIBILITY";

   type Ada_Visibibility_Assistant is new Database_Assistant with record
      Clause_Key : Construct_Annotations_Pckg.Annotation_Key;
   end record;

   overriding procedure File_Updated
     (Assistant : access Ada_Visibibility_Assistant;
      File      : Structured_File_Access;
      Old_Tree  : Construct_Tree;
      Kind      : Update_Kind);

   function Resolve_Clause
     (Assistant : Ada_Visibibility_Assistant'Class;
      Entity    : Entity_Access)
      return Entity_Access;

   type Clause_Val is new Construct_Annotations_Pckg.General_Annotation_Record
   with record
      Unit : Entity_Persistent_Access;
   end record;

   overriding procedure Free (This : in out Clause_Val);

   ------------------------
   -- Register_Assistant --
   ------------------------

   procedure Register_Assistant (Db : Construct_Database_Access) is
      use Construct_Annotations_Pckg;

      Clause_Key : Construct_Annotations_Pckg.Annotation_Key;
   begin
      Get_Annotation_Key
        (Get_Construct_Annotation_Key_Registry (Db).all,
         Clause_Key);

      Register_Assistant
        (Db, Ada_Visibility_Id,
         new Ada_Visibibility_Assistant'
           (Database_Assistant with Clause_Key));
   end Register_Assistant;

   -------------------
   -- Get_Assistant --
   -------------------

   function Get_Assistant
     (Db : Construct_Database_Access) return Database_Assistant_Access
   is
   begin
      return Get_Assistant (Db, Ada_Visibility_Id);
   end Get_Assistant;

   -------------------------------
   -- Is_Public_Library_Visible --
   -------------------------------

   function Is_Public_Library_Visible (Entity : Entity_Access) return Boolean
   is
   begin
      return Get_Construct (Entity).Attributes
        (Ada_Library_Visibility_Attribute);
   end Is_Public_Library_Visible;

   ------------------
   -- File_Updated --
   ------------------

   overriding procedure File_Updated
     (Assistant : access Ada_Visibibility_Assistant;
      File      : Structured_File_Access;
      Old_Tree  : Construct_Tree;
      Kind      : Update_Kind)
   is
      pragma Unreferenced (Old_Tree);

      --  We're doing this analyzis right after file update - because it
      --  doesn't seem to be an expensive one and there's no reason to delay
      --  it. If it appears not to be the case in the future, it's perfectly
      --  reasonable to delay it until the first Is_Public_Visible call is
      --  made.

      use Construct_Annotations_Pckg;

      Tree : constant Construct_Tree := Get_Tree (File);

      procedure Add_Scope
        (Scope           : Construct_Tree_Iterator;
         Parameters_Only : Boolean := False);

      procedure Mark_Scope_Not_Vible (Scope : Construct_Tree_Iterator);

      procedure Remove_Clause_Annotations;

      ---------------
      -- Add_Scope --
      ---------------

      procedure Add_Scope
        (Scope           : Construct_Tree_Iterator;
         Parameters_Only : Boolean := False)
      is
         It        : Construct_Tree_Iterator;
         Scope_End : Construct_Tree_Iterator;
      begin
         if Scope = Null_Construct_Tree_Iterator then
            It := First (Tree);
         else
            It := Next (Tree, Scope, Jump_Into);
         end if;

         while Is_Parent_Scope (Scope, It) loop
            if Parameters_Only and then
              Get_Construct (It).Category /= Cat_Parameter
            then
               exit;
            end if;

            if Get_Construct (It).Visibility = Visibility_Public then
               Get_Construct (It).Attributes
                 (Ada_Library_Visibility_Attribute) := True;

               if not Parameters_Only then
                  if (Get_Construct (It).Category = Cat_Package
                      and then Get_Construct (It).Is_Declaration)
                    or else Get_Construct (It).Category
                  in Cat_Class .. Cat_Type
                  then
                     Add_Scope (It);
                  elsif Get_Construct (It).Category
                  in Cat_Task .. Cat_Entry
                  then
                     Add_Scope (It, True);
                  else
                     Mark_Scope_Not_Vible (It);
                  end if;
               end if;
            else
               exit;
            end if;

            It := Next (Tree, It, Jump_Over);
         end loop;

         if Scope /= Null_Construct_Tree_Iterator then
            Scope_End := Next (Tree, Scope, Jump_Over);
         else
            Scope_End := Null_Construct_Tree_Iterator;
         end if;

         while It /= Scope_End loop
            Get_Construct (It).Attributes
              (Ada_Library_Visibility_Attribute) := False;

            It := Next (Tree, It, Jump_Into);
         end loop;
      end Add_Scope;

      ----------------------------
      -- Mark_Scope_Not_Visible --
      ----------------------------

      procedure Mark_Scope_Not_Vible (Scope : Construct_Tree_Iterator) is
         It : Construct_Tree_Iterator := Next (Tree, Scope, Jump_Into);
         Scope_End : constant Construct_Tree_Iterator :=
           Next (Tree, Scope, Jump_Over);
      begin
         while It /= Scope_End loop
            Get_Construct (It).Attributes
              (Ada_Library_Visibility_Attribute) := False;

            It := Next (Tree, It, Jump_Into);
         end loop;
      end Mark_Scope_Not_Vible;

      -------------------------------
      -- Remove_Clause_Annotations --
      -------------------------------

      procedure Remove_Clause_Annotations is
         It : Construct_Tree_Iterator := First (Tree);
      begin
         while It /= Null_Construct_Tree_Iterator loop
            if Get_Construct (It).Category in Cat_With .. Cat_Use then
               Construct_Annotations_Pckg.Free_Annotation
                 (Get_Annotation_Container (Tree, It).all,
                  Assistant.Clause_Key);
            end if;

            It := Next (Tree, It, Jump_Into);
         end loop;
      end Remove_Clause_Annotations;

   begin
      case Kind is
         when Minor_Change | Removed =>
            null;

         when Full_Change | Structural_Change =>
            Add_Scope (Null_Construct_Tree_Iterator);
            Remove_Clause_Annotations;

      end case;
   end File_Updated;

   ---------------------------
   -- Get_Location_Relation --
   ---------------------------

   function Get_Location_Relation
     (Tree_To     : Construct_Tree;
      Object_To   : Construct_Tree_Iterator;
      Tree_From   : Construct_Tree;
      Offset_From : String_Index_Type) return Location_Relation
   is
      Path_To   : constant Construct_Tree_Iterator_Array :=
        Full_Construct_Path (Tree_To, Object_To);

      Path_From : constant Construct_Tree_Iterator_Array :=
        Full_Construct_Path (Tree_From, Offset_From);
   begin
      return Get_Location_Relation (Tree_To, Path_To, Tree_From, Path_From);
   end Get_Location_Relation;

   function Get_Location_Relation
     (Tree_To   : Construct_Tree;
      Path_To   : Construct_Tree_Iterator_Array;
      Tree_From : Construct_Tree;
      Path_From : Construct_Tree_Iterator_Array)
      return Location_Relation
   is
   begin
      if Path_To'Length > Path_From'Length then
         return None;
      elsif Path_To'Length = 0 or else Path_From'Length = 0 then
         return None;
      elsif Get_Construct (Path_To (1)).Name = null
        or else Get_Construct (Path_From (1)).Name = null
      then
         return None;
      end if;

      declare
         Root_To_Name   : constant Composite_Identifier :=
           To_Composite_Identifier
             (Get_Construct (Path_To (1)).Name.all);
         Root_From_Name : constant Composite_Identifier :=
           To_Composite_Identifier (Get_Construct (Path_From (1)).Name.all);

         Index_In_To_Root, Index_In_From_Root : Integer := 1;
         Index_In_To_Path, Index_In_From_Path : Integer := 1;
      begin
         loop
            if Index_In_To_Path > Path_To'Last then
               --  If we successfully iterate over all the "to" path, it means
               --  that either "from" is the same entity, or a unit child of
               --  this entity. Return the max potential visibility

               if Tree_To = Tree_From
                 and then Encloses
                   (Path_To (Path_To'Last),
                    String_Index_Type
                      (Get_Construct
                         (Path_From (Path_From'Last)).Sloc_Start.Index))
                 and then not Get_Construct
                   (Path_To (Path_To'Last)).Is_Declaration
               then
                  return Package_Body;
               else
                  return Full_Spec_Hierarchy;
               end if;
            elsif Index_In_From_Path > Path_From'Last then
               --  If we iterated over all the "from" path, but there are
               --  still "to" to be iterated, then "from" is a child or a
               --  content of "to". Thus, "to" doesn't have any particular
               --  visibility priviledge other than public.

               return None;
            elsif Index_In_To_Path > 1
              and then Index_In_From_Path > 1
            then
               --  If we analyzed the first item, then we only need to check
               --  the name of the remaining ones - no composite identifier is
               --  expected pass this point.

               if Get_Construct (Path_From (Index_In_From_Path)).Name = null
                 or else Get_Identifier (Path_To (Index_In_To_Path))
                 /= Get_Identifier (Path_From (Index_In_From_Path))
               then
                  return None;
               end if;
            elsif Index_In_From_Path = 1
              and then Index_In_To_Path = 1
            then
               --  We're still analyzing the first index - which may be a
               --  composite identifier.

               if not Equal
                 (Get_Item (Root_To_Name, Index_In_To_Root),
                  Get_Item (Root_From_Name, Index_In_From_Root),
                  False)
               then
                  return None;
               end if;
            elsif Index_In_From_Path > 1 then
               --  We're still analyzing the first index of "from", but already
               --  completely analyzed to. Check the next index of the
               --  composite "from" against the next path element of "to"

               if Get_Construct (Path_From (Index_In_From_Path)).Name = null
                 or else not Equal
                   (Get_Item (Root_To_Name, Index_In_To_Root),
                    Get_Construct (Path_From (Index_In_From_Path)).Name.all,
                    False)
               then
                  return None;
               end if;
            else
               --  We're still analyzing the first index of "to", but already
               --  completely analyzed to. Check the next index of the
               --  composite "to" against the next path element of "from"

               if Get_Construct
                 (Path_To (Index_In_To_Path)).Name = null
                 or else not Equal
                   (Get_Item (Root_From_Name, Index_In_From_Root),
                    Get_Construct
                      (Path_To (Index_In_To_Path)).Name.all,
                    False)
               then
                  return None;
               end if;
            end if;

            if Index_In_From_Root < Length (Root_From_Name) then
               Index_In_From_Root := Index_In_From_Root + 1;
            else
               Index_In_From_Path := Index_In_From_Path + 1;
            end if;

            if Index_In_To_Root < Length (Root_To_Name) then
               Index_In_To_Root := Index_In_To_Root + 1;
            else
               Index_In_To_Path := Index_In_To_Path + 1;
            end if;
         end loop;
      end;
   end Get_Location_Relation;

   -------------------
   -- Is_Accessible --
   -------------------

   function Is_Accessible
     (Entity      : Entity_Access;
      From_File   : Structured_File_Access;
      From_Offset : String_Index_Type) return Boolean
   is
      Rel : Location_Relation;
   begin
      --  If the entity is public library visible, then we can have access to
      --  it.

      if Is_Public_Library_Visible (Entity) then
         return True;
      end if;

      --  Otherwise, we've got to have visibility on the proper hidden package
      --  part

      Rel := Get_Location_Relation
        (Tree_To     => Get_Tree (Get_File (Entity)),
         Object_To   => Get_Parent_Scope
           (Get_Tree (Get_File (Entity)), To_Construct_Tree_Iterator (Entity)),
         Tree_From   => Get_Tree (From_File),
         Offset_From => From_Offset);

      case Rel is
         when  Package_Body =>
            --  If we've got visibility on the entire package, then
            --  it's OK.

            return True;

         when Full_Spec_Hierarchy =>
            --  If we've got visibility only on the private spec, then
            --  check if the entity is indeed coming from the spec.

            return Get_Construct (Get_Parent_Scope
              (Get_Tree
                 (Get_File (Entity)),
                 To_Construct_Tree_Iterator (Entity))).Is_Declaration;

         when None | Public_Spec_Hierarchy =>
            --  If we have just visibilty on the public part, since
            --  this is not a public library visible package, we can't
            --  see it.

            return False;

      end case;
   end Is_Accessible;

   ----------------------------
   -- To_Use_Clause_Iterator --
   ----------------------------

   function To_Clause_Iterator
     (Visibility_Info : Visibility_Context;
      Category        : Language_Category) return Clause_Iterator
   is
      Tree   : constant Construct_Tree := Get_Tree (Visibility_Info.File);
      It     : Construct_Tree_Iterator;
      Result : Clause_Iterator;
   begin
      It := Get_Iterator_At
        (Tree     => Tree,
         Location => To_Location (Visibility_Info.Offset),
         Position => Before);

      Result.Current := To_Entity_Access (Visibility_Info.File, It);
      Result.Category := Category;

      if not Is_Valid (Result) then
         Prev (Result);
      end if;

      return Result;
   end To_Clause_Iterator;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : Clause_Iterator) return Boolean is
   begin
      return At_End (This)
        or else
          (This.Category = Cat_Use
           and then Get_Construct (This.Current).Category = Cat_Use)
        or else
          (This.Category = Cat_With
           and then Get_Construct (This.Current).Category = Cat_With)
        or else
          (This.Category = Cat_Unknown
           and then Get_Construct
             (This.Current).Category in Cat_With .. Cat_Use);
   end Is_Valid;

   ----------
   -- Prev --
   ----------

   procedure Prev (This : in out Clause_Iterator) is
      Tree : constant Construct_Tree := Get_Tree (Get_File (This.Current));
      It   : Construct_Tree_Iterator := To_Construct_Tree_Iterator
        (This.Current);
      Unit : Unit_Access;
   begin
      loop
         It := Prev (Tree, It, Jump_Over);

         exit when It = Null_Construct_Tree_Iterator
           or else
             (This.Category = Cat_Use
              and then Get_Construct (It).Category = Cat_Use)
           or else
             (This.Category = Cat_With
              and then Get_Construct (It).Category = Cat_With)
           or else
             (This.Category = Cat_Unknown
              and then Get_Construct (It).Category in Cat_With .. Cat_Use);
      end loop;

      if It = Null_Construct_Tree_Iterator then
         Unit := Get_Owning_Unit (This.Current);

         if Get_Parent (Unit) /= Null_Unit_Access then
            declare
               Start_E   : constant Entity_Access :=
                 Get_Entity (Get_Parent (Unit));
               Unit_It   : Construct_Tree_Iterator :=
                 To_Construct_Tree_Iterator (Start_E);
               Scope_It  : constant Construct_Tree_Iterator := Unit_It;
               Last_It  : Construct_Tree_Iterator;
               Unit_File : constant Structured_File_Access :=
                 Get_File (Start_E);
               Unit_Tree : constant Construct_Tree := Get_Tree (Unit_File);
            begin
               Last_It := Unit_It;
               Unit_It := Next (Unit_Tree, Unit_It, Jump_Into);

               while Is_Parent_Scope (Scope_It, Unit_It) loop
                  Last_It := Unit_It;
                  Unit_It := Next (Unit_Tree, Unit_It, Jump_Over);
               end loop;

               This.Current := To_Entity_Access (Unit_File, Last_It);
            end;
         else
            This.Current := Null_Entity_Access;
         end if;
      else
         This.Current := To_Entity_Access (Get_File (This.Current), It);
      end if;
   end Prev;

   ------------
   -- At_End --
   ------------

   function At_End (This : Clause_Iterator) return Boolean is
   begin
      return This.Current = Null_Entity_Access;
   end At_End;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity (This : Clause_Iterator) return Entity_Access is
   begin
      return This.Current;
   end Get_Entity;

   ---------------------
   -- Resolve_Package --
   ---------------------

   function Resolve_Package (This : Clause_Iterator) return Entity_Access is
   begin
      return Resolve_Clause (This.Current);
   end Resolve_Package;

   -----------------------------
   -- Is_Visible_From_Clauses --
   -----------------------------

   function Is_Visible_From_Clauses
     (Entity         : Entity_Access;
      From_Visiblity : Visibility_Context) return Boolean
   is
      Assistant    : constant Database_Assistant_Access :=
        Get_Assistant (Get_Database (Get_File (Entity)));
      My_Assistant : Ada_Visibibility_Assistant renames
        Ada_Visibibility_Assistant (Assistant.all);

      Clause_It : Clause_Iterator := To_Clause_Iterator
        (From_Visiblity, Cat_Unknown);

      Clause : Entity_Access;

      Entity_Id : constant Composite_Identifier :=
        To_Composite_Identifier
          (Get_Identifier (To_Construct_Tree_Iterator (Entity)).all);
   begin
      while not At_End (Clause_It) loop
         Clause := Get_Entity (Clause_It);

         case Get_Construct (Clause).Category is
            when Cat_Use =>
               declare
                  Unit : constant Entity_Access :=
                    Resolve_Clause (My_Assistant, Clause);
               begin
                  if Contains (Unit, Entity) then
                     return True;
                  end if;
               end;

            when Cat_With =>
               if Get_Construct (Entity).Category = Cat_Package
                 or else Get_Construct (Entity).Category in
                 Cat_Procedure .. Cat_Function
               then
                  declare
                     With_Id : constant Composite_Identifier :=
                       To_Composite_Identifier
                         (Get_Identifier
                              (To_Construct_Tree_Iterator (Clause)).all);
                  begin
                     if Length (With_Id) >= Length (Entity_Id) then
                        if Get_Slice
                          (With_Id, 1, Length (Entity_Id)) = Entity_Id
                        then
                           return True;
                        end if;
                     end if;
                  end;
               end if;

            when others =>
               null;

         end case;

         Prev (Clause_It);
      end loop;

      return False;
   end Is_Visible_From_Clauses;

   --------------------
   -- Resolve_Clause --
   --------------------

   function Resolve_Clause (Entity : Entity_Access) return Entity_Access is
      Assistant : constant Database_Assistant_Access :=
        Get_Assistant (Get_Database (Get_File (Entity)));
   begin
      return Resolve_Clause
        (Ada_Visibibility_Assistant (Assistant.all), Entity);
   end Resolve_Clause;

   --------------------
   -- Resolve_Clause --
   --------------------

   function Resolve_Clause
     (Assistant : Ada_Visibibility_Assistant'Class;
      Entity    : Entity_Access)
      return Entity_Access
   is
      use Construct_Annotations_Pckg;

      function Resolve_With return Entity_Access;
      function Resolve_Use return Entity_Access;

      ------------------
      -- Resolve_With --
      ------------------

      function Resolve_With return Entity_Access is
         Unit : constant Unit_Access :=
           Get_Unit
             (Get_Database (Get_File (Entity)),
              Get_Construct (Entity).Name.all);
      begin
         return Get_Entity (Unit);
      end Resolve_With;

      -----------------
      -- Resolve_Use --
      -----------------

      function Resolve_Use return Entity_Access is
         Expression : Parsed_Expression := Ada_Lang.Parse_Expression_Backward
           (Buffer       => Get_Construct (Entity).Name,
            Start_Offset => String_Index_Type
              (Get_Construct (Entity).Name'Last));

         Package_Resolution : Entity_List;
         Package_It         : Entity_Iterator;
         Result             : Entity_Access := Null_Entity_Access;
      begin
         Package_Resolution := Find_Declarations
           (Context           =>
              (From_File,
               Get_File (Entity),
               String_Index_Type
                 (Get_Construct (Entity).Sloc_Start.Index - 1)),
            From_Visibility   =>
              (File                      => Get_File (Entity),
               Offset                    =>
                 String_Index_Type
                   (Get_Construct (Entity).Sloc_Start.Index - 1),
               Filter                    => Everything,
               Min_Visibility_Confidence => Use_Visible),
            Expression        => Expression,
            Categories        => (1 => Cat_Package));

         Package_It := First (Package_Resolution);

         if not At_End (Package_It) then
            Result := Get_Entity (Package_It);
         end if;

         Free (Package_It);
         Free (Package_Resolution);
         Free (Expression);

         return Result;
      end Resolve_Use;

      Annot  : Annotation;
      Clause : Clause_Val;
   begin
      Get_Annotation
        (Get_Annotation_Container
           (Get_Tree (Get_File (Entity)),
            To_Construct_Tree_Iterator (Entity)).all,
         Assistant.Clause_Key,
         Annot);

      if Annot /= Null_Annotation then
         Clause := Clause_Val (Annot.Other_Val.all);
      end if;

      if not Exists (Clause.Unit) then
         case Get_Construct (Entity).Category is
            when Cat_With =>
               Clause.Unit := To_Entity_Persistent_Access (Resolve_With);

            when Cat_Use =>
               Clause.Unit := To_Entity_Persistent_Access (Resolve_Use);

            when others =>
               null;

         end case;

         Ref (Clause.Unit);

         Annot := (Other_Kind, new Clause_Val'(Clause));

         Set_Annotation
           (Get_Annotation_Container
              (Get_Tree (Get_File (Entity)),
               To_Construct_Tree_Iterator (Entity)).all,
            Assistant.Clause_Key,
            Annot);
      end if;

      return To_Entity_Access (Clause.Unit);
   end Resolve_Clause;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Clause_Val) is
   begin
      Unref (This.Unit);
   end Free;

end Ada_Semantic_Tree.Visibility;
