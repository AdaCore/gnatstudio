------------------------------------------------------------------------------
--                                  G P S                                   --
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

with GNATCOLL.Symbols;               use GNATCOLL.Symbols;
with GNATCOLL.Utils;                 use GNATCOLL.Utils;
with Language.Ada;                   use Language.Ada;
with String_Utils;                   use String_Utils;
with Ada_Semantic_Tree.Lang;         use Ada_Semantic_Tree.Lang;
with Ada_Semantic_Tree.Units;        use Ada_Semantic_Tree.Units;
with Ada_Semantic_Tree.Parts;        use Ada_Semantic_Tree.Parts;
with Ada_Semantic_Tree.Cache;        use Ada_Semantic_Tree.Cache;
with Ada_Semantic_Tree.Dependency_Tree; use Ada_Semantic_Tree.Dependency_Tree;

package body Ada_Semantic_Tree.Visibility is

   Ada_Visibility_Id : constant String := "ADA_LIB_VISIBILITY";

   type Ada_Visibibility_Assistant is new Database_Assistant with record
      null;
   end record;

   overriding procedure File_Updated
     (Assistant : access Ada_Visibibility_Assistant;
      File      : Structured_File_Access;
      Old_Tree  : Construct_Tree;
      Kind      : Update_Kind);

   type Clause_Val is new Cached_Information
   with record
      Unit            : Entity_Persistent_Access;
      Generic_Context : Persistent_Instance_Info :=
        Null_Persistent_Instance_Info;
   end record;

   overriding procedure Free (This : in out Clause_Val);

   ------------------------
   -- Register_Assistant --
   ------------------------

   procedure Register_Assistant (Db : Construct_Database_Access) is
   begin
      Register_Assistant
        (Db, Ada_Visibility_Id, new Ada_Visibibility_Assistant);
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
      pragma Unreferenced (Old_Tree, Assistant);

      --  We're doing this analyzis right after file update - because it
      --  doesn't seem to be an expensive one and there's no reason to delay
      --  it. If it appears not to be the case in the future, it's perfectly
      --  reasonable to delay it until the first Is_Public_Visible call is
      --  made.

      Tree : constant Construct_Tree := Get_Tree (File);

      procedure Add_Scope
        (Scope           : Construct_Tree_Iterator;
         Parameters_Only : Boolean := False);

      procedure Mark_Scope_Not_Vible (Scope : Construct_Tree_Iterator);

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
               if Get_Construct (It).Category = Cat_Package
                 or else Get_Construct (It).Category in Subprogram_Category
               then
                  --  In case of packages or subprograms, only the declaration
                  --  can be marked as public library visible. This applies in
                  --  particular to compilation units, where we need to mark
                  --  only the delcaration as visible, and not the completion.

                  Get_Construct (It).Attributes
                    (Ada_Library_Visibility_Attribute) :=
                    Get_Construct (It).Is_Declaration;
               else
                  Get_Construct (It).Attributes
                    (Ada_Library_Visibility_Attribute) := True;
               end if;

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

      --------------------------
      -- Mark_Scope_Not_Vible --
      --------------------------

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

   begin
      case Kind is
         when Minor_Change | Removed =>
            null;

         when Full_Change | Structural_Change | Project_Change =>
            Add_Scope (Null_Construct_Tree_Iterator);

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
      end if;

      declare
         Root_To_Name   : constant Composite_Identifier :=
           To_Composite_Identifier
             (Get (Get_Construct (Path_To (1)).Name).all);
         Root_From_Name : constant Composite_Identifier :=
           To_Composite_Identifier
             (Get (Get_Construct (Path_From (1)).Name).all);

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

               if Get_Identifier (Path_To (Index_In_To_Path))
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

               if not Equal
                 (Get_Item (Root_To_Name, Index_In_To_Root),
                  Get
                    (Get_Construct (Path_From (Index_In_From_Path)).Name).all,
                  False)
               then
                  return None;
               end if;
            else
               --  We're still analyzing the first index of "to", but already
               --  completely analyzed to. Check the next index of the
               --  composite "to" against the next path element of "from"

               if not Equal
                 (Get_Item (Root_From_Name, Index_In_From_Root),
                  Get (Get_Construct
                    (Path_To (Index_In_To_Path)).Name).all,
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

      if From_File = null then
         return False;
      end if;

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

   ------------------------
   -- To_Clause_Iterator --
   ------------------------

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

               if not Is_Valid (This) then
                  Prev (This);
               end if;
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

   -------------------------
   -- Get_Generic_Context --
   -------------------------

   function Get_Generic_Context
     (This : Clause_Iterator) return Instance_Info
   is
   begin
      return Get_Generic_Context (Get_Clause_Info (This.Current));
   end Get_Generic_Context;

   -----------------------------
   -- Is_Visible_From_Clauses --
   -----------------------------

   function Is_Visible_From_Clauses
     (Entity         : Entity_Access;
      From_Visiblity : Visibility_Context) return Entity_Access
   is
      Clause_It : Clause_Iterator := To_Clause_Iterator
        (From_Visiblity, Cat_Unknown);

      Clause : Entity_Access;

      Entity_Id : constant Normalized_Symbol :=
        Get_Identifier (To_Construct_Tree_Iterator (Entity));

      function Is_Prefix_Of (Prefix, Full : String) return Boolean;
      --  Return if Prefix is the prefix of full, ignoring spaces and taking
      --  into account dots.

      function Is_Prefix_Of (Prefix, Full : String) return Boolean is
         Prefix_Id, Full_Id : Integer;
      begin
         Prefix_Id := Prefix'First;
         Full_Id := Full'First;

         while Full_Id <= Full'Last
           and then Prefix_Id <= Prefix'Last
         loop
            Skip_Blanks (Prefix, Prefix_Id);
            Skip_Blanks (Full, Full_Id);

            if Full_Id > Full'Last then
               return Prefix_Id > Prefix'Last;
            end if;

            while Prefix_Id <= Prefix'Last
              and then Full_Id <= Full'Last
              and then not Is_Blank (Full (Full_Id))
              and then not Is_Blank (Prefix (Prefix_Id))
            loop
               if Prefix (Prefix_Id) /= Full (Full_Id) then
                  return False;
               end if;

               Prefix_Id := Prefix_Id + 1;
               Full_Id := Full_Id + 1;
            end loop;

            if Full_Id < Full'Last
              and then Full (Full_Id) = '.'
              and then Prefix_Id > Prefix'Last
            then
               --  In this case, full is of the form A.B.C, and Prefix is
               --  of the form A.B

               return True;
            end if;

            Full_Id := Full_Id + 1;
            Prefix_Id := Prefix_Id + 1;
         end loop;

         return True;
      end Is_Prefix_Of;

   begin
      Update_Dependency_Information_If_Needed (Get_Owning_Unit (Entity));

      while not At_End (Clause_It) loop
         Clause := Get_Entity (Clause_It);

         case Get_Construct (Clause).Category is
            when Cat_Use =>
               declare
                  Unit : Entity_Access :=
                    Get_Target (Get_Clause_Info (Clause));
                  View : Entity_View;
                  Use_Clause : Entity_Access;
               begin
                  if Is_Generic_Instance (Unit) then
                     Use_Clause := Unit;
                     View := Get_Generic_Entity (Unit);
                     Unit := Get_First_Occurence (Get_Entity (View));
                     Free (View);
                  else
                     Use_Clause := Unit;
                  end if;

                  if Contains (Unit, Entity) then
                     return Use_Clause;
                  end if;
               end;

            when Cat_With =>
               if (Get_Construct (Entity).Category = Cat_Package
                   or else Get_Construct (Entity).Category in
                     Cat_Procedure .. Cat_Function)
                 and then Is_Compilation_Unit
                   (To_Construct_Tree_Iterator (Entity))
               then
                  if Is_Prefix_Of
                    (Get (Entity_Id).all,
                     Get (Get_Identifier
                       (To_Construct_Tree_Iterator (Clause))).all)
                  then
                     return Get_Target (Get_Clause_Info (Clause));
                  end if;
               end if;

            when others =>
               null;

         end case;

         Prev (Clause_It);
      end loop;

      return Null_Entity_Access;
   end Is_Visible_From_Clauses;

   --------------------
   -- Resolve_Clause --
   --------------------

   function Resolve_Clause (Entity : Entity_Access) return Entity_Access is
   begin
      return Get_Target (Get_Clause_Info (Entity));
   end Resolve_Clause;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Clause_Val) is
   begin
      Free (This.Generic_Context);
      Unref (This.Unit);
   end Free;

end Ada_Semantic_Tree.Visibility;
