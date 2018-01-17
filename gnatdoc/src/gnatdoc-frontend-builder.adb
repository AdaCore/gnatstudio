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

with Ada.Containers.Indefinite_Hashed_Maps;

with GNAT.HTable;
with Basic_Types;             use Basic_Types;
with GNATdoc.Utils;           use GNATdoc.Utils;
with Language.Ada;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with Xref.Docgen;             use Xref.Docgen;
with Xref;
with GNAT.IO;

package body GNATdoc.Frontend.Builder is
   Me : constant Trace_Handle := Create ("GNATdoc.1-Frontend-Builder");

   function Build_Ada_File_Tree
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type) return Entity_Id;
   --  Subsidiary of Build_File_Tree

   function Build_C_File_Tree
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type) return Entity_Id;
   --  Subsidiary of Build_File_Tree

   -----------------------------
   -- Unique_Entity_Allocator --
   -----------------------------

   --  This package gives support to unique allocation of tree entities (that
   --  is, it ensures no entity is duplicated in the trees composing a full
   --  project). This is required to ensure consistency and also to facilitate
   --  to the backend the generation of entity dependency graphs.

   package Unique_Entity_Allocator is

      procedure Append_To_List
        (List : access EInfo_List.Vector; E : Entity_Id);
      --  Append to List the entity E

      procedure Get_Unique_Entity
        (Entity  : out Entity_Id;
         Context : access constant Docgen_Context;
         File    : Virtual_File;
         E       : Root_Entity'Class;
         Forced  : Boolean := False);
      --  Search for E in a hash table containing all the project entities.
      --  If found then return such entity; if not found then allocate a new
      --  unique entity for E. If Forced is True then the entity is searched
      --  and built even if it is defined in another file.

      function Is_Access_Type
        (Entity : Entity_Id) return Boolean;

      function Number_Of_Progenitors
        (Entity : Entity_Id) return Natural;

      procedure Set_Progenitor_As_Parent (Entity : Entity_Id);
      --  Set the unique progenitor of Entity as its parent

      ----------------
      -- Hash_Table --
      ----------------

      --  Hash table containing all the entities of the project. Used to avoid
      --  generating duplicate entities.

      package Hash_Table is

         function Hash
           (Key : General_Location) return Ada.Containers.Hash_Type;

         function Equivalent_Keys
           (Left, Right : General_Location) return Boolean;

         package EInfo_Map is new Ada.Containers.Indefinite_Hashed_Maps
           (Key_Type        => General_Location,
            Element_Type    => Entity_Id,
            Hash            => Hash,
            Equivalent_Keys => Equivalent_Keys);

         Entities_Map : EInfo_Map.Map;

         procedure Append_To_Map (E : Entity_Id);
         --  Append the entity of E to Entities_Map

      private
         pragma Inline (Append_To_Map);
      end Hash_Table;
      use Hash_Table;

      ------------------------------------------
      --  Debugging routines (for use in gdb) --
      ------------------------------------------

      procedure uplid (Unique_Id : Natural);
      --  (gdb) Search for and entity in the hash-table with Unique_Id and
      --  print the list of entities defined in the scope of E. No output
      --  generated if the entity is not found.

      procedure upnid (Unique_Id : Natural);
      --  (gdb) Search for and entity in the hash-table with Unique_Id and
      --  prints its contents. No output generated if the entity is not found.

   private
      pragma Inline (Append_To_List);
      pragma Inline (Number_Of_Progenitors);

      pragma Export (Ada, uplid);
      pragma Export (Ada, upnid);
   end Unique_Entity_Allocator;

   use Unique_Entity_Allocator;
   use Unique_Entity_Allocator.Hash_Table;

   ----------------------
   -- Entity_Allocator --
   ----------------------

   package body Unique_Entity_Allocator is

      ----------------
      -- Hash_Table --
      ----------------

      package body Hash_Table is

         -------------------
         -- Append_To_Map --
         -------------------

         procedure Append_To_Map (E : Entity_Id) is
         begin
            pragma Assert (Present (E));

            if not Entities_Map.Contains (LL.Get_Location (E)) then
               Entities_Map.Insert (LL.Get_Location (E), E);
            end if;
         end Append_To_Map;

         ---------------------
         -- Equivalent_Keys --
         ---------------------

         function Equivalent_Keys
           (Left, Right : General_Location) return Boolean
         is
            use type Basic_Types.Visible_Column_Type;

         begin
            return Left.File = Right.File
              and then Left.Line = Right.Line
              and then Left.Column = Right.Column;
         end Equivalent_Keys;

         ----------
         -- Hash --
         ----------

         function Hash (Key : General_Location) return Ada.Containers.Hash_Type
         is
            type Internal_Hash_Type is range 0 .. 2 ** 31 - 1;
            function Internal is new GNAT.HTable.Hash
              (Header_Num => Internal_Hash_Type);
         begin
            return Ada.Containers.Hash_Type
              (Internal
                 (+Key.File.Full_Name
                  & Natural'Image (Key.Line)
                  & Basic_Types.Visible_Column_Type'Image (Key.Column)));
         end Hash;

      end Hash_Table;

      --------------------
      -- Append_To_List --
      --------------------

      procedure Append_To_List
        (List : access EInfo_List.Vector;
         E    : Entity_Id) is
      begin
         pragma Assert (not List.Contains (E));
         List.Append (E);
      end Append_To_List;

      --------------------
      -- Is_Access_Type --
      --------------------

      function Is_Access_Type (Entity : Entity_Id) return Boolean is
      begin
         return LL.Is_Type (Entity)
           and then LL.Is_Access (Entity);
      end Is_Access_Type;

      -----------------------
      -- Get_Unique_Entity --
      -----------------------

      procedure Get_Unique_Entity
        (Entity  : out Entity_Id;
         Context : access constant Docgen_Context;
         File    : Virtual_File;
         E       : Root_Entity'Class;
         Forced  : Boolean := False)

      is
         E_Loc : constant General_Location := Get_Location (E);

         Lang        : constant Language_Access :=
                         Get_Language_From_File (Context.Lang_Handler, File);
         In_Ada_Lang : constant Boolean :=
                         Lang.all in Language.Ada.Ada_Language'Class;

         function Build_New_Entity return Entity_Id;
         --  Local routine which factorizes code used to allocate a new
         --  entity

         function Full_View_Needed (New_E : Entity_Id) return Boolean;
         --  Evaluate if New_E requires a full view

         ----------------------
         -- Build_New_Entity --
         ----------------------

         function Build_New_Entity return Entity_Id is
            New_E : Entity_Id := New_Entity (Context, Lang, E, E_Loc);
         begin
            if No (New_E) then
               return Atree.No_Entity;
            else
               if Is_Package (New_E)
                 and then Present (LL.Get_Parent_Package (New_E))
               then
                  Set_Parent_Package (New_E,
                    Get_Unique_Entity
                      (Context, File, LL.Get_Parent_Package (New_E)));
               end if;

               if Full_View_Needed (New_E) then
                  declare
                     Partial_View  : constant Entity_Id := New_E;

                     Full_View_Loc : constant General_Location :=
                                       LL.Get_Body_Loc (Partial_View);

                     Full_View     : Entity_Id :=
                                       Find_Unique_Entity (Full_View_Loc);

                  begin
                     if No (Full_View) then
                        Full_View :=
                          New_Entity (Context, Lang, E, Full_View_Loc);
                        Append_To_Map (Full_View);
                     end if;

                     --  Xref does not help us to differentiate if New_E is a
                     --  private type, an incomplete declaration, or a formal
                     --  of a subprogram (because in this latter case Body_Loc
                     --  references the same formal in the subprogram body).
                     --  For this reasons at this stage the best action we can
                     --  do is to decorate the entity as "incomplete". At later
                     --  stages the entity will be fully decorated (and this
                     --  value will be reset in subprogram formals).

                     Set_Is_Incomplete (New_E);

                     --  For private types the entity associated with the full
                     --  view is not available available in Xref since the
                     --  compiler does not generate it; by contrast, for
                     --  incomplete types the compiler generates two entities.

                     Set_Full_View (New_E, Full_View);

                     --  Link the full view with its partial view

                     Set_Partial_View (Get_Full_View (New_E), New_E);

                     --  Adding a minimum high level decoration to the full
                     --  view. For incomplete types this value may be updated
                     --  later (when the type declaration is processed).

                     Set_Kind (Get_Full_View (New_E), Get_Kind (New_E));
                  end;

               --  The Xref service Child_Types returns direct references
               --  to the full view of the child types. For homoneneity in
               --  creation of entities we build here the partial view and
               --  full view and we return the reference to the partial view.

               elsif Present (LL.Get_Body_Loc (New_E))
                 and then LL.Is_Type (New_E)
                 and then LL.Get_Location (New_E).File
                            = LL.Get_Body_Loc (New_E).File
                 and then LL.Get_Location (New_E).Line
                            > LL.Get_Body_Loc (New_E).Line
               then
                  declare
                     Full_View        : constant Entity_Id := New_E;

                     Partial_View_Loc : constant General_Location :=
                                          LL.Get_Body_Loc (New_E);
                     Partial_View     : Entity_Id :=
                                          Find_Unique_Entity
                                            (Partial_View_Loc);
                  begin
                     if No (Partial_View) then
                        Partial_View :=
                          New_Entity (Context, Lang, E, Partial_View_Loc);
                     end if;

                     Set_Is_Incomplete (Partial_View);
                     Set_Full_View (Partial_View, Full_View);
                     Set_Partial_View (Full_View, Partial_View);

                     New_E := Partial_View;
                  end;
               end if;

               return New_E;
            end if;
         end Build_New_Entity;

         ----------------------
         -- Full_View_Needed --
         ----------------------

         function Full_View_Needed (New_E : Entity_Id) return Boolean is
         begin
            if No (LL.Get_Body_Loc (New_E)) then
               return False;

            elsif Is_Concurrent_Type_Or_Object (New_E) then
               return
                 Is_Spec_File
                   (Context.Kernel, LL.Get_Location (New_E).File)
                 and then
                   LL.Get_Location (New_E).File = LL.Get_Body_Loc (New_E).File
                 and then
                   LL.Get_Location (New_E).Line < LL.Get_Body_Loc (New_E).Line;

            --  Formals and variables found in the scope of entries

            elsif Get_Kind (New_E) = E_Variable
              and then
                LL.Get_Ekind (LL.Get_Scope (New_E), In_Ada_Lang) = E_Entry
            then
               return False;

            elsif LL.Is_Type (New_E)
              or else Get_Kind (New_E) = E_Variable
            then
               --  This is not correct if New_E is a single concurrent type
               --  defined in the public part of a package. However, at this
               --  stage we cannot differentiate such case.

               return
                 LL.Get_Location (New_E).File /= LL.Get_Body_Loc (New_E).File
                 or else
                   LL.Get_Location (New_E).Line < LL.Get_Body_Loc (New_E).Line;

            else
               return False;
            end if;
         end Full_View_Needed;

         --  Local variables

         Is_Prim : constant Boolean := Is_Primitive_Of (E)'Length /= 0;
         --  Avoid calling twice this service???

      begin
         Entity := null;

         if In_Ada_Lang then

            if E_Loc.File /= File
              and then not Is_Prim
              and then not Forced
            then
               return;
            end if;

            --  Before creating the entity we search for it in the hash
            --  table (to avoid duplicating it!)

            declare
               Prev_E : constant Entity_Id := Find_Unique_Entity (E_Loc);

            begin
               if Present (Prev_E) then
                  Entity := Prev_E;
               else
                  Entity := Build_New_Entity;
                  Append_To_Map (Entity);
               end if;

               return;
            end;

         --  C / C++

         else
            if E_Loc.File /= File
              and then not Forced
            then
               return;
            end if;

            declare
               Kind   : constant Entity_Kind :=
                          LL.Get_Ekind (E, In_Ada_Lang => False);
               Prev_E : Entity_Id;

            begin
               if Kind = E_Include_File
                 or else Kind = E_Unknown
               then
                  return;
               end if;

               --  Before creating the entity we search for it in the hash
               --  table (to avoid duplicating it!)

               Prev_E := Find_Unique_Entity (E_Loc);

               if Present (Prev_E) then
                  Entity := Prev_E;
               else
                  Entity := Build_New_Entity;
                  Append_To_Map (Entity);
               end if;

               return;
            end;
         end if;

      exception
         when E : others =>
            Trace (Me, E);
            raise;
      end Get_Unique_Entity;

      ---------------------------
      -- Number_Of_Progenitors --
      ---------------------------

      function Number_Of_Progenitors
        (Entity : Entity_Id) return Natural is
      begin
         return Natural (Get_Progenitors (Entity).all.Length);
      end Number_Of_Progenitors;

      ------------------------------
      -- Set_Progenitor_As_Parent --
      ------------------------------

      procedure Set_Progenitor_As_Parent (Entity : Entity_Id) is
      begin
         Set_Parent (Entity,
           Get_Progenitors (Entity).First_Element);
         Get_Progenitors (Entity).Delete_First;
      end Set_Progenitor_As_Parent;

      ---------------------------------------------------------------
      --                   Debugging routines                      --
      ---------------------------------------------------------------

      -----------
      -- uplid --
      -----------

      procedure uplid (Unique_Id : Natural) is
      begin
         for E of Entities_Map loop
            if Get_Unique_Id (E) = Unique_Id then
               pl (E);
               exit;
            end if;
         end loop;
      end uplid;

      -----------
      -- upnid --
      -----------

      procedure upnid (Unique_Id : Natural) is
      begin
         for E of Entities_Map loop
            if Get_Unique_Id (E) = Unique_Id then
               pn (E);
               return;
            end if;
         end loop;

         GNAT.IO.Put_Line ("Entity" & Unique_Id'Img & " not found");
      end upnid;

   end Unique_Entity_Allocator;

   -----------------
   -- Scope_Stack --
   -----------------

   --  Subsidiary package used to build the tree

   package Scopes_Stack is

      procedure Clear;
      --  Clear the contents of the Scope Stack and unregister the entity
      --  which represents the standard entity.

      function Current_Scope return Entity_Id;
      --  Return the entity in the top of the stack

      procedure Enter_Scope (Scope : Entity_Id);
      --  Push Scope

      procedure Exit_Scope;
      --  Pop an entity from the stack

   end Scopes_Stack;

   -------------------
   -- Append_To_Map --
   -------------------

   procedure Append_To_Map (E : Entity_Id) is
   begin
      Hash_Table.Append_To_Map (E);
   end Append_To_Map;

   -------------------------
   -- Build_Ada_File_Tree --
   -------------------------

   function Build_Ada_File_Tree
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type) return Entity_Id
   is
      Lang          : constant Language_Access :=
                        Get_Language_From_File (Context.Lang_Handler, File);
      In_Ada_Lang   : constant Boolean :=
                        Lang.all in Language.Ada.Ada_Language'Class;
      pragma Assert (In_Ada_Lang);

      Std_Entity    : constant Entity_Id :=
                        New_Internal_Entity
                          (Context  => Context,
                           Language => Lang,
                           Name     => Std_Entity_Name);
      use Scopes_Stack;

      procedure Append_To_File_Entities (E : Entity_Id);
      --  Append E to File_Entities.All_Entities

      procedure Complete_Decoration (E : Entity_Id);
      --  Complete the decoration of entity E

      -----------------------------
      -- Append_To_File_Entities --
      -----------------------------

      procedure Append_To_File_Entities (E : Entity_Id) is
      begin
         --  Do not append to the list of entities of this file entities which
         --  inherited from other files (for example, inherited dispatching
         --  primitives)

         if LL.Get_Location (E).File /= File then
            return;
         end if;

         if not File_Entities.All_Entities.Contains (E) then
            Append_To_List (File_Entities.All_Entities'Access, E);
         end if;
      end Append_To_File_Entities;

      -------------------------
      -- Complete_Decoration --
      -------------------------

      procedure Complete_Decoration (E : Entity_Id) is

         procedure Decorate_Array_Type (E : Entity_Id);
         --  Complete the decoration of an array type

         procedure Decorate_Generic_Formals (E : Entity_Id);
         --  Complete the decoration of a subprogram entity

         procedure Decorate_Record_Type (E : Entity_Id);
         --  Complete the decoration of a record type entity

         procedure Decorate_Subprogram_Formals (E : Entity_Id);
         --  Complete the decoration of a subprogram entity

         -------------------------
         -- Decorate_Array_Type --
         -------------------------

         procedure Decorate_Array_Type (E : Entity_Id) is
            Index_Types : Xref.Entity_Array :=
                            Xref.Index_Types (LL.Get_Entity (E));
            Etype : Entity_Id;
         begin
            for J in Index_Types'Range loop
               Get_Unique_Entity
                 (Etype, Context, File, Index_Types (J).all, Forced => True);

               --  Workaround an Xref problem: in string subtypes the parent
               --  type is returned by Xref as its last index.

               --  Another Xref problem is that when the components of the
               --  array are of predefined types no entity is provided by
               --  Xref through its service Xref.Component()

               if LL.Is_Predefined_Entity (Etype)
                 and then Get_Kind (Etype) = E_Array_Type
                 and then Get_Full_Name (Etype) = "String"
               then
                  null;
               else
                  Append_Array_Index_Type (E, Etype);
               end if;
            end loop;

            Free (Index_Types);
         end Decorate_Array_Type;

         ------------------------------
         -- Decorate_Generic_Formals --
         ------------------------------

         procedure Decorate_Generic_Formals (E : Entity_Id) is
            Formals : Xref.Entity_Array :=
                       Formal_Parameters (LL.Get_Entity (E));
            Formal  : Entity_Id;

         begin
            Enter_Scope (E);

            for J in Formals'Range loop
               Get_Unique_Entity
                 (Formal, Context, File, Formals (J).all,
                  Forced => True);

               --  Adding minimum decoration to undecorated generic formals

               if Get_Kind (Formal) = E_Unknown then
                  Set_Kind (Formal, E_Generic_Formal);
               end if;

               Set_Is_Generic_Formal (Formal);
               Set_Scope (Formal, E);

               Append_Generic_Formal (E, Formal);

               Set_Is_Decorated (Formal);
            end loop;

            Free (Formals);

            Exit_Scope;
         end Decorate_Generic_Formals;

         --------------------------
         -- Decorate_Record_Type --
         --------------------------

         procedure Decorate_Record_Type (E : Entity_Id) is

            procedure Append_Parent_And_Progenitors
              (Parents  : Xref.Entity_Array);

            function Is_Inherited_Primitive
              (Typ  : Entity_Id;
               Prim : Root_Entity'Class) return Boolean;
            --  Return true if primitive Prim of tagged type Typ has been
            --  inherited from some parent or progenitor type

            procedure Append_Parent_And_Progenitors
              (Parents : Xref.Entity_Array)
            is
               Parent : Entity_Id;

            begin
               --  Identify the parent and the progenitors of the type. When
               --  the parent of a type T is an interface type and it T covers
               --  also several interfaces then Xref does not provide us with
               --  enough information to know which interface is the parent
               --  type. The best we can do for now is to catch the parent
               --  as part of processing the corresponding source file (see
               --  Get_Record_Type_Source).

               for J in Parents'Range loop
                  Get_Unique_Entity
                    (Parent, Context, File, Parents (J).all, Forced => True);

                  --  Avoid adding a self reference as parent or progenitor

                  if Parents (J).all = LL.Get_Entity (E) then
                     null;

                  elsif not Has_Parent_Type (E, Parent) then
                     LL.Append_Parent_Type (E, Parent);

                     --  The list of parents returned by Xref is not ordered
                     --  and hence it does not help to differentiate the parent
                     --  type from the progenitors.

                     if Get_Kind (Parent) /= E_Interface then

                        --  For partial views we append the parent to the
                        --  full view and we take care of completing the
                        --  decoration of the partial view when we process
                        --  the file to retrieve sources and comments

                        if Is_Partial_View (E) then
                           Set_Parent (Get_Full_View (E), Parent);
                        else
                           Set_Parent (E, Parent);
                        end if;
                     else
                        Append_Progenitor (E, Parent);
                     end if;
                  end if;
               end loop;
            end Append_Parent_And_Progenitors;

            ----------------------------
            -- Is_Inherited_Primitive --
            ----------------------------

            function Is_Inherited_Primitive
              (Typ  : Entity_Id;
               Prim : Root_Entity'Class) return Boolean
            is
               function Check_Primitives
                 (Typ : Entity_Id) return Boolean;
               --  Return true if Prim is a primitive of Typ

               function Check_Progenitors
                 (Typ : Entity_Id) return Boolean;
               --  Return True if Prim is a primitive defined in some
               --  progenitor of Typ

               ----------------------
               -- Check_Primitives --
               ----------------------

               function Check_Primitives
                 (Typ : Entity_Id) return Boolean
               is
               begin
                  for E of Get_Methods (Typ).all loop
                     if LL.Get_Entity (E) = Prim then
                        return True;
                     end if;
                  end loop;

                  return False;
               end Check_Primitives;

               -----------------------
               -- Check_Progenitors --
               -----------------------

               function Check_Progenitors
                 (Typ : Entity_Id) return Boolean
               is
               begin
                  for Progenitor of Get_Progenitors (Typ).all loop
                     if Check_Primitives (Progenitor) then
                        return True;
                     end if;
                  end loop;

                  return False;
               end Check_Progenitors;

            begin
               if Get_Kind (Typ) = E_Interface then
                  return Check_Progenitors (Typ);

               else
                  declare
                     Parent : Entity_Id;
                  begin
                     Parent := Get_Parent (Typ);
                     while Present (Parent) loop
                        if Check_Primitives (Parent) then
                           return True;
                        end if;

                        Parent := Get_Parent (Parent);
                     end loop;

                     return False;
                  end;
               end if;
            end Is_Inherited_Primitive;

         --  Start of processing for Decorate_Record_Type

         begin
            if Get_Kind (E) /= E_Interface then
               declare
                  Discrim : Xref.Entity_Array := Discriminants
                    (LL.Get_Entity (E));
                  Entity  : Entity_Id;
               begin
                  for J in Discrim'Range loop
                     Get_Unique_Entity
                       (Entity, Context, File, Discrim (J).all);

                     --  If the entity is not available that means that
                     --  this is an incomplete type whose discriminants
                     --  are defined in the package body.

                     if Entity = null then
                        null;

                     else
                        --  For incomplete types whose discriminant is
                        --  repeated in the partial and full-view the
                        --  compiler generates two entities and we must
                        --  handle just one.

                        if not Get_Entities (E).Contains (Entity) then
                           Set_Kind (Entity, E_Discriminant);
                           Set_Is_Decorated (Entity);

                           pragma Assert (not Is_Full_View (Entity));

                           --  For partial and incomplete types in the ALI file
                           --  in their full view we don't have available their
                           --  discriminants.

                           if Is_Partial_View (Entity) then
                              Append_To_Scope (E, Entity);

                              --  If the discriminant is visible in the partial
                              --  and full view of E then append its full view
                              --  to the full view of E and we remove it from
                              --  the scope of E

                              if Is_Partial_View (E) then
                                 declare
                                    Discr_Full_V : constant Entity_Id :=
                                      Get_Full_View (Entity);
                                 begin
                                    Append_To_File_Entities (Discr_Full_V);
                                    Set_Kind
                                      (Discr_Full_V, E_Discriminant);
                                    Remove_From_Scope (Discr_Full_V);

                                    Append_To_Scope (Get_Full_View (E),
                                      Discr_Full_V);

                                    Set_Full_View
                                      (Entity, Atree.No_Entity);
                                    Set_Partial_View
                                      (Discr_Full_V, Atree.No_Entity);
                                    Set_Is_Incomplete (Entity, False);

                                    Set_Scope (Discr_Full_V,
                                      Get_Full_View (E));

                                    Set_Is_Decorated (Discr_Full_V);
                                 end;
                              else
                                 Remove_Full_View (Entity);
                                 Set_Is_Incomplete (Entity, False);
                              end if;

                           --  Unknown discriminant of a private or limited
                           --  type

                           elsif Is_Partial_View (E) then
                              Set_Has_Unknown_Discriminants (E);
                              Append_To_Scope (Get_Full_View (E), Entity);

                           else
                              if Get_Scope (Entity) /= E then
                                 Remove_From_Scope (Entity);
                                 Append_To_Scope (E, Entity);
                              end if;
                           end if;
                        end if;
                     end if;
                  end loop;

                  Free (Discrim);
               end;
            end if;

            --  Check_Record_Components

            if Get_Kind (E) /= E_Interface then
               declare
                  Components : Xref.Entity_Array := Fields (LL.Get_Entity (E));
                  Entity     : Entity_Id;

               begin
                  --  For now skip processing the components of an incomplete
                  --  type declaration whose full view is defined in the
                  --  package body since Xref provides weird components.
                  --  Seems a bug in Xref but more investigation needed???

                  if LL.Is_Type (E)
                    and then Is_Partial_View (E)
                    and then
                      LL.Get_Location (Get_Full_View (E)).File
                        /= LL.Get_Location (E).File
                  then
                     null;
                  else
                     for J in Components'Range loop
                        Get_Unique_Entity
                          (Entity, Context, File, Components (J).all);

                        --  If the entity is not available that means that
                        --  this is an incomplete type whose components are
                        --  defined in the package body.

                        if Entity = null then
                           null;

                        --  Workaround Xref problem: for incomplete types with
                        --  discriminants in the partial and full view, Xref
                        --  returns discriminants in the list of fields.

                        elsif Get_Kind (Entity) = E_Discriminant then
                           null;

                        else
                           --  In C++ we have here formals of primitives???
                           Set_Kind (Entity, E_Component);
                           Append_To_File_Entities (Entity);

                           Set_Is_Decorated (Entity);
                        end if;
                     end loop;
                  end if;

                  Free (Components);
               end;

               pragma Assert
                 (not Has_Duplicated_Entities (Get_Entities (E).all));
               if Is_Partial_View (E) then
                  pragma Assert
                    (not Has_Duplicated_Entities
                          (Get_Entities (Get_Full_View (E)).all));
               end if;
            end if;

            Append_Parent_And_Progenitors
              (Xref.Parent_Types
                 (Entity    => LL.Get_Entity (E),
                  Recursive => False));

            if No (Get_Parent (E))
              and then Number_Of_Progenitors (E) = 1
            then
               Set_Progenitor_As_Parent (E);
            end if;

            declare
               Childs : Xref.Entity_Array :=
                          Child_Types
                           (LL.Get_Entity (E),
                            Recursive => False);
               Child  : Entity_Id;
               Loc    : General_Location;

            begin
               for J in Childs'Range loop

                  --  Do not add as a child type the second entity generated
                  --  by the compiler for named typedef structs (the compiler
                  --  generates two entites in the LI file with the same name)

                  Get_Unique_Entity
                    (Child, Context, File, Childs (J).all, Forced => True);

                  Loc := LL.Get_Location (Child);

                  --  Avoid adding a self reference as a child type

                  if Childs (J).all = LL.Get_Entity (E) then
                     null;

                  --  Avoid problems with wrong Xref decoration that I can
                  --  reproduce with gnatcoll-refcount-weakref.ads. To
                  --  be investigated???

                  elsif not Is_Class_Or_Record_Type (Child) then
                     null;  --  Free (Child) ???

                  --  Avoid adding to the tree entities defined in package
                  --  bodies since they cannot be fully decorated and
                  --  hence cause problems to the backend (for example,
                  --  we cannot set their scope!)

                  elsif not Is_Spec_File (Context.Kernel, Loc.File) then
                     null; --  Free (Child) ???

                  else
                     LL.Append_Child_Type (E, Child);
                  end if;
               end loop;

               Free (Childs);
            end;

            if Is_Tagged (E)
              or else Get_Kind (E) = E_Class
            then
               declare
                  All_Methods : Xref.Entity_Array :=
                                  Methods
                                    (LL.Get_Entity (E),
                                     Include_Inherited => True);

                  Method : Entity_Id;
               begin
                  for J in All_Methods'Range loop
                     Get_Unique_Entity
                       (Method, Context, File, All_Methods (J).all,
                        Forced => True);

                     if Is_Inherited_Primitive (E, All_Methods (J).all) then
                        if not Is_Partial_View (E) then
                           Append_Inherited_Method (E, Method);
                        else
                           Append_Inherited_Method
                             (Get_Full_View (E), Method);
                        end if;

                     else
                        Append_To_File_Entities (Method);

                        Decorate_Subprogram_Formals (Method);

                        if not Is_Partial_View (E) then
                           if not Get_Methods (E).Contains (Method) then
                              Append_Method (E, Method);
                           end if;
                        else
                           if not
                             Get_Methods (Get_Full_View (E)).Contains (Method)
                           then
                              Append_Method (Get_Full_View (E), Method);
                           end if;
                        end if;

                        Set_Is_Decorated (Method);
                     end if;
                  end loop;

                  Free (All_Methods);
               end;
            end if;
         end Decorate_Record_Type;

         ---------------------------------
         -- Decorate_Subprogram_Formals --
         ---------------------------------

         procedure Decorate_Subprogram_Formals (E : Entity_Id) is
         begin
            --  No extra action needed if the formals are already present

            if not EInfo_List.Is_Empty (Get_Entities (E).all) then
               return;
            end if;

            Enter_Scope (E);

            declare
               Formals : Xref.Parameter_Array :=
                           Parameters (LL.Get_Entity (E));
               Formal  : Entity_Id;

            begin
               for J in Formals'Range loop

                  --  Handle weird case found processing the file gimple.h of
                  --  the gcc sources: the entity associated with a function is
                  --  also associated with one of its formals. It seems a bug
                  --  in the generated LI file. To be investigated???

                  if LL.Get_Entity (E) = Formals (J).Parameter.all then
                     null;

                  else
                     Get_Unique_Entity
                       (Formal, Context, File, Formals (J).Parameter.all,
                        Forced => True);

                     if Present (Formal) then
                        pragma Assert
                          (LL.Get_Entity (Formal) = Formals (J).Parameter.all);

                        --  Correct previous wrong decoration (done by
                        --  Atree.New_Internal_Entity).

                        Remove_Full_View (Formal);
                        Set_Is_Incomplete (Formal, False);

                        --  Complete decoration

                        Set_Kind (Formal, E_Formal);
                        Append_To_File_Entities (Formal);

                        Set_Is_Decorated (Formal);
                     end if;
                  end if;
               end loop;

               Free (Formals);
            end;

            Exit_Scope;
         end Decorate_Subprogram_Formals;

      --  Start of processing for Complete_Decoration

      begin
         if Is_Class_Or_Record_Type (E) then
            Decorate_Record_Type (E);

         --  For concurrent types reuse the routine which processes record
         --  types to collect their progenitors (if any)

         elsif Is_Concurrent_Type_Or_Object (E) then
            Decorate_Record_Type (E);

         elsif Is_Generic (E) then
            Decorate_Generic_Formals (E);

            if Is_Subprogram_Or_Entry (E) then
               Decorate_Subprogram_Formals (E);
            end if;

         elsif Is_Subprogram_Or_Entry (E) or else Is_Generic (E) then
            Decorate_Subprogram_Formals (E);

         elsif Get_Kind (E) = E_Interface then
            Decorate_Record_Type (E);

         --  Decorate access to subprogram types

         elsif Is_Access_Type (E) then
            Decorate_Subprogram_Formals (E);

         elsif LL.Is_Array (E) then
            Decorate_Array_Type (E);
         end if;

         if Is_Partial_View (E) then
            declare
               Full_View : constant Entity_Id := Get_Full_View (E);
            begin
               Append_To_Map (Full_View);

               Set_Is_Decorated (Full_View);
            end;
         end if;
      end Complete_Decoration;

      --  Local variables

      --  This entity represents the outermost scope (ie. the standard scope).
      --  It is needed to associate some scope to generic formals of library
      --  level units.

      New_E                : Entity_Id;
      File_Entities_Cursor : Entities_In_File_Cursor;
      Entities_Count       : Natural := 0;
      Is_Large_File        : Boolean := False;

   --  Start of processing for Build_Ada_File_Tree

   begin
      Set_Kind (Std_Entity, E_Package);
      Append_To_List (File_Entities.All_Entities'Access, Std_Entity);

      File_Entities_Cursor := Context.Database.Entities_In_File
         (File, No_Project);
      while not At_End (File_Entities_Cursor) loop
         Entities_Count := Entities_Count + 1;

         if not Context.Options.Quiet_Mode
           and then not Is_Large_File
           and then Entities_Count mod 3000 = 0
         then
            GNAT.IO.Put_Line
              ("info: processing large file " & (+File.Base_Name));
            Is_Large_File := True;
         end if;

         Get_Unique_Entity
           (New_E, Context, File, File_Entities_Cursor.Get);

         if Present (New_E) then
            Complete_Decoration (New_E);
            Set_Is_Decorated (New_E);
            Append_To_File_Entities (New_E);

            if Is_Partial_View (New_E)
              and then LL.Get_Location (Get_Full_View (New_E)).File = File
            then
               Complete_Decoration (Get_Full_View (New_E));
               Append_To_File_Entities (Get_Full_View (New_E));
               Set_Is_Decorated (Get_Full_View (New_E));
            end if;
         end if;

         File_Entities_Cursor.Next;
      end loop;

      if Entities_Count = 0 then
         return Atree.No_Entity;
      else
         return Std_Entity;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         raise;
   end Build_Ada_File_Tree;

   -----------------------
   -- Build_C_File_Tree --
   -----------------------

   function Build_C_File_Tree
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type) return Entity_Id
   is
      Lang          : constant Language_Access :=
                        Get_Language_From_File (Context.Lang_Handler, File);
      In_Ada_Lang   : constant Boolean :=
                        Lang.all in Language.Ada.Ada_Language'Class;
      In_C_Lang     : constant Boolean := not In_Ada_Lang;
      pragma Assert (In_C_Lang);

      Std_Entity    : constant Entity_Id :=
                        New_Internal_Entity
                          (Context  => Context,
                           Language => Lang,
                           Name     => Std_Entity_Name);
      use Scopes_Stack;

      procedure Append_To_File_Entities (E : Entity_Id);
      --  Append E to File_Entities.All_Entities

      procedure Complete_Decoration (E : Entity_Id);
      --  Complete the decoration of entity E

      -----------------------------
      -- Append_To_File_Entities --
      -----------------------------

      procedure Append_To_File_Entities (E : Entity_Id) is
      begin
         if not File_Entities.All_Entities.Contains (E) then
            Append_To_List (File_Entities.All_Entities'Access, E);
         end if;
      end Append_To_File_Entities;

      -------------------------
      -- Complete_Decoration --
      -------------------------

      procedure Complete_Decoration (E : Entity_Id) is

         procedure Decorate_Struct_Or_Class_Type (E : Entity_Id);
         --  Complete the decoration of a record type entity

         procedure Decorate_Subprogram_Formals (E : Entity_Id);
         --  Complete the decoration of a subprogram entity

         -----------------------------------
         -- Decorate_Struct_Or_Class_Type --
         -----------------------------------

         procedure Decorate_Struct_Or_Class_Type (E : Entity_Id) is

            procedure Append_Parents
              (Parents  : Xref.Entity_Array);

            function Is_Inherited_Primitive
              (Typ  : Entity_Id;
               Prim : Root_Entity'Class) return Boolean;
            --  Return true if primitive Prim of tagged type Typ has been
            --  inherited from some parent or progenitor type

            --------------------
            -- Append_Parents --
            --------------------

            procedure Append_Parents
              (Parents : Xref.Entity_Array)
            is
               Parent : Entity_Id;

            begin
               for J in Parents'Range loop
                  Get_Unique_Entity
                    (Parent, Context, File, Parents (J).all, Forced => True);

                  if not Has_Parent_Type (E, Parent) then
                     LL.Append_Parent_Type (E, Parent);
                  end if;
               end loop;
            end Append_Parents;

            ----------------------------
            -- Is_Inherited_Primitive --
            ----------------------------

            function Is_Inherited_Primitive
              (Typ  : Entity_Id;
               Prim : Root_Entity'Class) return Boolean
            is
               function Check_Primitives
                 (Typ : Entity_Id) return Boolean;
               --  Return true if Prim is a primitive of Typ

               ----------------------
               -- Check_Primitives --
               ----------------------

               function Check_Primitives
                 (Typ : Entity_Id) return Boolean
               is
               begin
                  for E of Get_Methods (Typ).all loop
                     if LL.Get_Entity (E) = Prim then
                        return True;
                     end if;
                  end loop;

                  return False;
               end Check_Primitives;

            begin
               declare
                  Parent : Entity_Id;
               begin
                  Parent := Get_Parent (Typ);
                  while Present (Parent) loop
                     if Check_Primitives (Parent) then
                        return True;
                     end if;

                     Parent := Get_Parent (Parent);
                  end loop;

                  return False;
               end;
            end Is_Inherited_Primitive;

         --  Start of processing for Decorate_Struct_Or_Class_Type

         begin
            --  Check components

            declare
               Components : Xref.Entity_Array := Fields (LL.Get_Entity (E));
               Entity     : Entity_Id;

            begin
               for J in Components'Range loop
                  Get_Unique_Entity
                    (Entity, Context, File, Components (J).all);

                  --  If the entity is not available that means that
                  --  this is an incomplete type whose components are
                  --  defined in the package body.

                  if Entity = null then
                     null;

                  else
                     --  In C++ we have here formals of primitives???
                     Set_Kind (Entity, E_Component);

                     if Is_Partial_View (E) then
                        Append_To_Scope (Get_Full_View (E), Entity);

                     else
                        Append_To_Scope (E, Entity);
                     end if;

                     Append_To_File_Entities (Entity);

                     Set_Is_Decorated (Entity);
                  end if;
               end loop;

               Free (Components);
            end;

            pragma Assert
              (not Has_Duplicated_Entities (Get_Entities (E).all));
            if Is_Partial_View (E) then
               pragma Assert
                 (not Has_Duplicated_Entities
                       (Get_Entities (Get_Full_View (E)).all));
            end if;

            Append_Parents
              (Xref.Parent_Types
                 (Entity    => LL.Get_Entity (E),
                  Recursive => False));

            declare
               Childs : Xref.Entity_Array :=
                          Child_Types
                           (LL.Get_Entity (E),
                            Recursive => False);
               Child  : Entity_Id;
               Loc    : General_Location;

            begin
               for J in Childs'Range loop

                  --  Do not add as a child type the second entity generated
                  --  by the compiler for named typedef structs (the compiler
                  --  generates two entites in the LI file with the same name)

                  if not LL.Is_Self_Referenced_Type
                           (E    => Childs (J).all,
                            Lang => Get_Language (E))
                  then
                     Get_Unique_Entity
                       (Child, Context, File, Childs (J).all, Forced => True);

                     Loc := LL.Get_Location (Child);

                     --  Avoid problems with wrong Xref decoration that I can
                     --  reproduce with gnatcoll-refcount-weakref.ads. To
                     --  be investigated???

                     if not Is_Class_Or_Record_Type (Child) then
                        null;  --  Free (Child) ???

                     --  Avoid adding to the tree entities defined in package
                     --  bodies since they cannot be fully decorated and
                     --  hence cause problems to the backend (for example,
                     --  we cannot set their scope!)

                     elsif not Is_Spec_File (Context.Kernel, Loc.File) then
                        null; --  Free (Child) ???

                     else
                        LL.Append_Child_Type (E, Child);
                     end if;
                  end if;
               end loop;

               Free (Childs);
            end;

            if Is_Tagged (E)
              or else Get_Kind (E) = E_Class
            then
               declare
                  All_Methods : Xref.Entity_Array :=
                                  Methods
                                    (LL.Get_Entity (E),
                                     Include_Inherited => True);

                  Method : Entity_Id;
               begin
                  for J in All_Methods'Range loop
                     Get_Unique_Entity
                       (Method, Context, File, All_Methods (J).all,
                        Forced => True);

                     if Is_Inherited_Primitive (E, All_Methods (J).all) then
                        if not Is_Partial_View (E) then
                           Append_Inherited_Method (E, Method);
                        else
                           Append_Inherited_Method
                             (Get_Full_View (E), Method);
                        end if;

                     else
                        if LL.Get_Scope (Method) = LL.Get_Entity (E) then
                           Append_To_Scope (E, Method);
                           Append_To_File_Entities (Method);

                           Decorate_Subprogram_Formals (Method);
                           Append_Method (E, Method);

                        --  For inherited primitives defined in other
                        --  scopes we cannot set their scope.

                        else
                           Decorate_Subprogram_Formals (Method);
                           Append_Inherited_Method (E, Method);
                        end if;
                     end if;
                  end loop;

                  Free (All_Methods);
               end;
            end if;
         end Decorate_Struct_Or_Class_Type;

         ---------------------------------
         -- Decorate_Subprogram_Formals --
         ---------------------------------

         procedure Decorate_Subprogram_Formals (E : Entity_Id) is
         begin
            --  No extra action needed if the formals are already present

            if not EInfo_List.Is_Empty (Get_Entities (E).all) then
               return;
            end if;

            Enter_Scope (E);

            declare
               Formals : Xref.Parameter_Array :=
                           Parameters (LL.Get_Entity (E));
               Formal  : Entity_Id;

            begin
               for J in Formals'Range loop

                  --  Handle weird case found processing the file gimple.h of
                  --  the gcc sources: the entity associated with a function is
                  --  also associated with one of its formals. It seems a bug
                  --  in the generated LI file. To be investigated???

                  if LL.Get_Entity (E) = Formals (J).Parameter.all then
                     null;

                  else
                     Get_Unique_Entity
                       (Formal, Context, File, Formals (J).Parameter.all,
                        Forced => True);

                     if Present (Formal) then
                        pragma Assert
                          (LL.Get_Entity (Formal) = Formals (J).Parameter.all);

                        --  Correct previous wrong decoration (done by
                        --  Atree.New_Internal_Entity).

                        Remove_Full_View (Formal);
                        Set_Is_Incomplete (Formal, False);

                        --  Complete decoration

                        Set_Kind (Formal, E_Formal);

                        Append_To_Scope (Current_Scope, Formal);
                        Set_Scope (Formal, E);

                        Append_To_File_Entities (Formal);

                        Set_Is_Decorated (Formal);
                     end if;
                  end if;
               end loop;

               Free (Formals);
            end;

            Exit_Scope;
         end Decorate_Subprogram_Formals;

      --  Start of processing for Complete_Decoration

      begin
         if Is_Class_Or_Record_Type (E) then
            Decorate_Struct_Or_Class_Type (E);

         elsif Is_Subprogram (E) then
            Decorate_Subprogram_Formals (E);

         --  Decorate access to subprogram types

         elsif Is_Access_Type (E) then
            Decorate_Subprogram_Formals (E);
         end if;
      end Complete_Decoration;

      --  Local variables

      --  This entity represents the outermost scope (ie. the standard scope).
      --  It is needed to associate some scope to generic formals of library
      --  level units.

      New_E                : Entity_Id;
      Skip_This_Entity     : Boolean := False;
      File_Entities_Cursor : Entities_In_File_Cursor;
      Entities_Count       : Natural := 0;
      Is_Large_File        : Boolean := False;

   --  Start of processing for Build_C_File_Tree

   begin
      Set_Kind (Std_Entity, E_Package);
      Enter_Scope (Std_Entity);

      File_Entities_Cursor := Context.Database.Entities_In_File
         (File, No_Project);

      --  Process all its entities

      while not At_End (File_Entities_Cursor) loop
         Entities_Count := Entities_Count + 1;

         if not Context.Options.Quiet_Mode
           and then not Is_Large_File
           and then Entities_Count mod 3000 = 0
         then
            GNAT.IO.Put_Line
              ("info: processing large file " & (+File.Base_Name));
            Is_Large_File := True;
         end if;

         Skip_This_Entity := False;
         Get_Unique_Entity
           (New_E, Context, File, File_Entities_Cursor.Get);

         if Present (New_E) then

            --  Decorate the new entity

            if Kind_In (Get_Kind (New_E), E_Formal,
                                          E_Component)
            then
               Skip_This_Entity := True;

            elsif Get_Kind (New_E) = E_Variable
              and then not LL.Is_Global (New_E)
            then
               Skip_This_Entity := True;

            --  Skip methods since they are entered in the tree as part of
            --  processing its class/tagged type

            elsif LL.Is_Primitive (New_E) then
               Skip_This_Entity := True;

            elsif Present (LL.Get_Scope (New_E))
              and then LL.Is_Global (New_E)
            then
               --  Handle named typedef structs since the compiler generates
               --  two entites in the LI file with the same name: decorate
               --  the attribute Alias of the second entity referencing the
               --  first one.

               if Is_Class_Or_Record_Type (New_E)
                 and then Get_Kind (New_E) /= E_Class
               then
                  declare
                     Scope_Id : constant Root_Entity'Class :=
                                  LL.Get_Scope (New_E);
                  begin
                     if Get_Name (LL.Get_Entity (New_E))
                       = Get_Name (Scope_Id)
                     then
                        declare
                           Prev_E : constant Entity_Id :=
                             Find_Unique_Entity
                               (Get_Location (Scope_Id));
                        begin
                           pragma Assert (Present (Prev_E));
                           Set_Alias (New_E, Prev_E);
                        end;
                     end if;
                  end;

               elsif Get_Kind (New_E) = E_Variable then
                  declare
                     Scope_Id : constant Root_Entity'Class :=
                                  LL.Get_Scope (New_E);

                  begin
                     --  Handle fields of structs. Must use the Xref support
                     --  directly since we may have not seen yet the full
                     --  declaration of the struct.

                     if Is_Type (Scope_Id) then
                        declare
                           Kind : constant Entity_Kind :=
                             LL.Get_Ekind (Scope_Id,
                                           In_Ada_Lang => False);
                        begin
                           pragma Assert (Kind_In (Kind, E_Record_Type,
                                          E_Class));
                           Skip_This_Entity := True;
                        end;
                     end if;
                  end;
               end if;
            end if;

            if Skip_This_Entity then

               --  Ensure that all the entities found in the ALI file are
               --  appended to the list of entities of this file. Required
               --  because there are cases in which LL_Scope is not available
               --  by mistake which causes erroneously skipping this entity
               --  (which affects later processings in the new frontend)

               if Get_Kind (New_E) /= E_Discriminant then
                  Append_To_File_Entities (New_E);

                  if LL.Is_Primitive (New_E) then
                     Set_Is_Decorated (New_E);
                  end if;
               end if;

               --  Free (New_E);

            else
               Append_To_File_Entities (New_E);

               if Is_Partial_View (New_E)
                 and then Present (Get_Full_View (New_E))
                 and then LL.Get_Location (Get_Full_View (New_E)).File = File
               then
                  Append_To_File_Entities (Get_Full_View (New_E));
               end if;

               --  Full views were unconditionally added to the scope as part
               --  of processing their partial view (since for private types
               --  the compiler does not generate two entities in the ALI
               --  file). Hence we avoid adding them twice to their scope.

               if not Is_Full_View (New_E) then
                  Append_To_Scope (Current_Scope, New_E);
               end if;

               if not Is_Decorated (New_E) then
                  Complete_Decoration (New_E);
                  Set_Is_Decorated (New_E);
               end if;
            end if;
         end if;

         File_Entities_Cursor.Next;
      end loop;

      --  Exit all the scopes: required to ensure that we complete the
      --  decoration of all the private entities

      while not Is_Standard_Entity (Current_Scope) loop
         Exit_Scope;
      end loop;

      Scopes_Stack.Clear;

      return Std_Entity;
   exception
      when E : others =>
         Trace (Me, E);
         raise;
   end Build_C_File_Tree;

   ---------------------
   -- Build_File_Tree --
   ---------------------

   function Build_File_Tree
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type) return Entity_Id
   is
      Lang          : constant Language_Access :=
                        Get_Language_From_File (Context.Lang_Handler, File);
      In_Ada_Lang   : constant Boolean :=
                        Lang.all in Language.Ada.Ada_Language'Class;
   begin
      if In_Ada_Lang then
         return Build_Ada_File_Tree (Context, File, File_Entities);
      else
         return Build_C_File_Tree (Context, File, File_Entities);
      end if;
   end Build_File_Tree;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      EInfo_Map.Clear (Entities_Map);
   end Initialize;

   ------------------------
   -- Find_Unique_Entity --
   ------------------------

   function Find_Unique_Entity
      (Location      : General_Location;
       In_References : Boolean := False) return Entity_Id
   is
      Map_Cursor : constant EInfo_Map.Cursor := Entities_Map.Find (Location);
      use type EInfo_Map.Cursor;
   begin
      if Map_Cursor /= EInfo_Map.No_Element then
         return EInfo_Map.Element (Map_Cursor);
      end if;

      --  If not found then search for an entity which is referenced from
      --  this location

      if In_References then
         return Atree.Find_Entity (Location);
      end if;

      return Atree.No_Entity;
   end Find_Unique_Entity;

   ------------------------
   -- Find_Unique_Entity --
   ------------------------

   function Find_Unique_Entity
     (Full_Name : String; Must_Be_Package : Boolean := False) return Entity_Id
   is
   begin
      for E of Entities_Map loop
         if Get_Full_Name (E) = Full_Name then

            --  Return this entity if the search is not restricted to
            --  entities of packages

            if not Must_Be_Package then
               return E;
            elsif Is_Package (E) then
               return E;
            end if;
         end if;
      end loop;

      return Atree.No_Entity;
   end Find_Unique_Entity;

   -----------------------
   -- Get_Unique_Entity --
   -----------------------

   function Get_Unique_Entity
     (Context : access constant Docgen_Context;
      File    : Virtual_File;
      E       : Root_Entity'Class) return Entity_Id
   is
      Loc    : constant General_Location := Get_Location (E);
      New_E  : Entity_Id;
      Result : Entity_Id;
   begin
      pragma Assert (Present (E));

      Result := Find_Unique_Entity (Loc);

      if Present (Result) then
         return Result;
      else
         Get_Unique_Entity (New_E, Context, File, E, Forced => True);
         return New_E;
      end if;
   end Get_Unique_Entity;

   -----------------
   -- Scope_Stack --
   -----------------

   package body Scopes_Stack is

      package Alloc_Entity_List is new Ada.Containers.Vectors
        (Index_Type => Natural, Element_Type => Entity_Id);
      --  procedure Free (List : in out Alloc_Entity_List.Vector);

      Stack : Alloc_Entity_List.Vector;

      procedure Clear is
      begin
         Stack.Clear;
      end Clear;

      function Current_Scope return Entity_Id is
      begin
         return Stack.Element (0);
      end Current_Scope;

      -----------------
      -- Enter_Scope --
      -----------------

      procedure Enter_Scope (Scope : Entity_Id) is
      begin
         Stack.Prepend (Scope);
      end Enter_Scope;

      ----------------
      -- Exit_Scope --
      ----------------

      procedure Exit_Scope is
      begin
         if Is_Package (Current_Scope)
           and then
             Present (Get_First_Private_Entity_Loc (Current_Scope))
         then
            EInfo_Vector_Sort_Loc.Sort
              (Get_Entities (Current_Scope).all);
         end if;

         Stack.Delete_First;
      end Exit_Scope;

   end Scopes_Stack;

end GNATdoc.Frontend.Builder;
