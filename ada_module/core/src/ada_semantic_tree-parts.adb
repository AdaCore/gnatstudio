------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2017, AdaCore                     --
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

with Ada.Characters.Handling;      use Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Unchecked_Deallocation;
with Ada_Semantic_Tree.Lang;       use Ada_Semantic_Tree.Lang;
with Ada_Semantic_Tree.Visibility; use Ada_Semantic_Tree.Visibility;
with GNATCOLL.Symbols;             use GNATCOLL.Symbols;

package body Ada_Semantic_Tree.Parts is

   Ada_Part_Assistant_Id : constant String := "ADA_PART_ASSISTANT";

   type Ada_Part_Db_Assistant is new Database_Assistant with record
      Parts_Key : Construct_Annotations_Pckg.Annotation_Key;
   end record;

   overriding
   procedure File_Updated
     (Assistant : access Ada_Part_Db_Assistant;
      File      : Structured_File_Access;
      Old_Tree  : Construct_Tree;
      Kind      : Update_Kind);

   type Ada_Relation is record
      First_Occurence : Entity_Persistent_Access :=
        Null_Entity_Persistent_Access;

      Second_Occurence : Entity_Persistent_Access :=
        Null_Entity_Persistent_Access;
      --  If the entity is a subprogram, this stores the position of its body.
      --  If it's a type, it stores the position of the declaration (the public
      --  declaration in case there is a full one in the private one as well).

      Third_Occurence : Entity_Persistent_Access :=
        Null_Entity_Persistent_Access;
      --  Only used in the construct is a type. Stores the position of the
      --  full declaration in the private part.
   end record;

   type Ada_Relation_Access is access all Ada_Relation;

   procedure Free is new Standard.Ada.Unchecked_Deallocation
     (Ada_Relation, Ada_Relation_Access);

   type Relation_Side is (Nothing, First, Second, Third);

   type Ada_Relation_Annotation is new
     Construct_Annotations_Pckg.General_Annotation_Record
   with record
      Relation : Ada_Relation_Access := null;
      Side     : Relation_Side := Nothing;
   end record;

   overriding
   procedure Free (Obj : in out Ada_Relation_Annotation);

   package Construct_List_Pckg is new
     Standard.Ada.Containers.Doubly_Linked_Lists (Entity_Access);

   type Construct_List_Access is access Construct_List_Pckg.List;

   procedure Free is new Ada.Unchecked_Deallocation
     (Construct_List_Pckg.List, Construct_List_Access);

   package Local_Dico_Pckg is new
     Standard.Ada.Containers.Indefinite_Ordered_Maps
       (String, Construct_List_Access);

   type Dico_Access is access Local_Dico_Pckg.Map;

   procedure Free is new Ada.Unchecked_Deallocation
     (Local_Dico_Pckg.Map, Dico_Access);

   package Dicos_List_Pckg is new
     Standard.Ada.Containers.Indefinite_Ordered_Maps
       (Entity_Access, Dico_Access);

   use Dicos_List_Pckg;
   use Local_Dico_Pckg;
   use Construct_List_Pckg;

   function Get_Relation (Entity : Entity_Access) return Ada_Relation_Access;

   function Get_Relation
     (Entity    : Entity_Access;
      Assistant : Ada_Part_Db_Assistant'Class)
      return Ada_Relation_Access;

   procedure Disconnect_Relation_Annotation
     (Entity : Entity_Access; Key : Construct_Annotations_Pckg.Annotation_Key);
   --  This subprogram will disconnect the annotation from the entity, without
   --  freeing the relation. The caller is responsible for managing the
   --  reference, or the deallocation, of the former relation.

   ------------------------
   -- Register_Assistant --
   ------------------------

   procedure Register_Assistant (Db : Construct_Database_Access) is
      Ada_Part_Entity_Key : Construct_Annotations_Pckg.Annotation_Key;

   begin
      Construct_Annotations_Pckg.Get_Annotation_Key
        (Get_Construct_Annotation_Key_Registry (Db).all,
         Ada_Part_Entity_Key);

      Register_Assistant
        (Db,
         Ada_Part_Assistant_Id,
         new Ada_Part_Db_Assistant'
           (Database_Assistant with
            Parts_Key      => Ada_Part_Entity_Key));
   end Register_Assistant;

   -------------------
   -- Get_Assistant --
   -------------------

   function Get_Assistant
     (Db : Construct_Database_Access) return Database_Assistant_Access
   is
   begin
      return Get_Assistant (Db, Ada_Part_Assistant_Id);
   end Get_Assistant;

   ------------------
   -- File_Updated --
   ------------------

   overriding procedure File_Updated
     (Assistant : access Ada_Part_Db_Assistant;
      File      : Structured_File_Access;
      Old_Tree  : Construct_Tree;
      Kind      : Update_Kind)
   is
      pragma Unreferenced (Assistant, Old_Tree);

      It : Unit_Iterator;

   begin
      if Kind /= Minor_Change then
         --  If this is not a minor change, then the entities relations may
         --  have changed.

         It := Get_Units (File);

         while not At_End (It) loop
            Set_Parts_Up_To_Date (Get (It), False);

            Next (It);
         end loop;

         Free (It);
      end if;
   end File_Updated;

   ------------------
   -- Analyze_Unit --
   ------------------

   procedure Analyze_Unit
     (Assistant : Database_Assistant_Access;
      Unit      : Unit_Access)
   is
      Parts_Key : constant Construct_Annotations_Pckg.Annotation_Key
        := Ada_Part_Db_Assistant (Assistant.all).Parts_Key;

      function Can_Have_Parts (Category : Language_Category) return Boolean;
      --  Return true if this category may denote a construct with multiple
      --  parts.

      procedure Analyze_Scope (Scope : Entity_Access);
      --  This subprogram analyzes the scope given in parameter, and create the
      --  relevant links between different parts of the same entity (spec /
      --  body, partial view / full view, etc.)

      function Same_Entity (Left, Right : Entity_Access) return Boolean;
      --  Check that these two entities are the same. This assumes that they
      --  have the same name, and are located on the same scope.

      Dico_List : Dicos_List_Pckg.Map;

      --------------------
      -- Can_Have_Parts --
      --------------------

      function Can_Have_Parts (Category : Language_Category) return Boolean is
      begin
         return Category in Cat_Package .. Cat_Variable
           or else Category = Cat_Parameter
           or else Category = Cat_Discriminant;
      end Can_Have_Parts;

      -------------------
      -- Analyze_Scope --
      -------------------

      procedure Analyze_Scope (Scope : Entity_Access) is
         use Construct_Annotations_Pckg;

         Scope_It         : constant Construct_Tree_Iterator :=
           To_Construct_Tree_Iterator (Scope);
         It               : Construct_Tree_Iterator;
         Dico             : Dico_Access;
         Parts_Annotation : Annotation (Other_Kind);

         Scope_File : constant Structured_File_Access := Get_File (Scope);
         Scope_Tree : constant Construct_Tree := Get_Tree (Scope_File);

         First_Occ : Entity_Access;
      begin
         --  If we're on a scope, then get the corresponding entity
         --  dictionnary.

         if Is_Set
           (Get_Annotation_Container (Scope_Tree, Scope_It).all, Parts_Key)
         then
            --  The dico is stored against the spec name, so get the spec
            --  entity and get the dico associated to this entity.

            Get_Annotation
              (Get_Annotation_Container (Scope_Tree, Scope_It).all,
               Parts_Key,
               Parts_Annotation);

            First_Occ :=
              To_Entity_Access
                (Ada_Relation_Annotation
                     (Parts_Annotation.Other_Val.all)
                 .Relation.First_Occurence);

            if Dicos_List_Pckg.Contains (Dico_List, First_Occ) then
               Dico := Dicos_List_Pckg.Element
                 (Dico_List,
                  To_Entity_Access
                    (Ada_Relation_Annotation
                       (Parts_Annotation.Other_Val.all)
                     .Relation.First_Occurence));
            else
               Dico := new Local_Dico_Pckg.Map;

               Dicos_List_Pckg.Insert (Dico_List, First_Occ, Dico);
            end if;
         else
            Dico := new Local_Dico_Pckg.Map;

            Parts_Annotation.Other_Val := new Ada_Relation_Annotation'
              (Relation => new Ada_Relation'
                 (First_Occurence  => To_Entity_Persistent_Access (Scope),
                  Second_Occurence => Null_Entity_Persistent_Access,
                  Third_Occurence  => Null_Entity_Persistent_Access),
               Side     => First);

            Set_Annotation
              (Get_Annotation_Container (Scope_Tree, Scope_It).all,
               Parts_Key,
               Parts_Annotation);

            Dicos_List_Pckg.Insert (Dico_List, Scope, Dico);
         end if;

         It := Next (Scope_Tree, Scope_It, Jump_Into);

         while Is_Parent_Scope (Scope_It, It) loop

         --  For each construct found in the scope:
         --    If we found the corresponding construct in the dico, then
         --      we create (or update) the parts record
         --    If not, then we add the construct in the dico
         --    If we are on a scope, then we call Analyze_Scope on recursively
         --      on it.

            if Can_Have_Parts (Get_Construct (It).Category)
              and then Get_Construct (It).Name /= No_Symbol
            then
               if Is_Set
                 (Get_Annotation_Container (Scope_Tree, It).all,
                  Parts_Key)
               then
                  --  If the key is already set, it means that we already have
                  --  computed the information from a previous version of the
                  --  unit. It's not valid anymore - remove it.

                  Free_Annotation
                    (Get_Annotation_Container (Scope_Tree, It).all,
                     Parts_Key);
               end if;

               declare
                  Similar_Names : Construct_List_Access;
                  Lower_Name    : constant String :=
                    To_Lower (Get (Get_Construct (It).Name).all);
                  Cur           : Construct_List_Pckg.Cursor;
                  Found         : Boolean := False;
                  Relation      : Ada_Relation_Access := null;

                  Cur_File : Structured_File_Access;
                  Cur_Tree : Construct_Tree;
               begin
                  if Contains (Dico.all, Lower_Name) then
                     Similar_Names := Element (Dico.all, Lower_Name);
                     Cur := First (Similar_Names.all);

                     --  Iterate over the various constructs which have the
                     --  same name on the current scope. If we find one that
                     --  has the same signature, then it's a part of the
                     --  current entity.

                     while Cur /= Construct_List_Pckg.No_Element loop
                        Cur_File := Get_File (Element (Cur));
                        Cur_Tree := Get_Tree (Cur_File);

                        if Same_Entity
                          (Element (Cur),
                           To_Entity_Access (Scope_File, It))
                        then
                           --  We found a similary entity. We either extract
                           --  the already existing annotation, or create a
                           --  new one an set its 'First' value.

                           if Is_Set
                             (Get_Annotation_Container
                                (Cur_Tree,
                                 To_Construct_Tree_Iterator
                                   (Element (Cur))).all,
                              Parts_Key)
                           then
                              Get_Annotation
                                (Get_Annotation_Container
                                   (Cur_Tree,
                                    To_Construct_Tree_Iterator
                                      (Element (Cur))).all,
                                 Parts_Key,
                                 Parts_Annotation);

                              Relation := Ada_Relation_Annotation
                                (Parts_Annotation.Other_Val.all).Relation;

                              if not Exists (Relation.First_Occurence) then
                                 --  If the spec doesn't exit anymore, it means
                                 --  that we're in a merge process, and we
                                 --  are actually on the first occurence of
                                 --  the entity.

                                 Unref (Relation.First_Occurence);

                                 Relation.First_Occurence :=
                                   To_Entity_Persistent_Access
                                     (Element (Cur));
                              end if;
                           else
                              Relation := new Ada_Relation'
                                (First_Occurence  =>
                                   To_Entity_Persistent_Access
                                     (Element (Cur)),
                                 Second_Occurence =>
                                   Null_Entity_Persistent_Access,
                                 Third_Occurence  =>
                                   Null_Entity_Persistent_Access);

                              Parts_Annotation.Other_Val :=
                                new Ada_Relation_Annotation'(Relation, First);

                              Set_Annotation
                                (Get_Annotation_Container
                                   (Cur_Tree,
                                    To_Construct_Tree_Iterator
                                      (Element (Cur))).all,
                                 Parts_Key,
                                 Parts_Annotation);
                           end if;

                           --  If there's a previous annotation on that entity,
                           --  then we have first to disconnect it, and then
                           --  do an updated analysis.

                           Disconnect_Relation_Annotation
                             (To_Entity_Access (Scope_File, It),
                              Parts_Key);

                           --  Now set the second or third value, depending
                           --  on the contents of the relation.

                           if not Exists (Relation.Second_Occurence) then
                              --  If the first body doesn't exist, we've just
                              --  found it.

                              Parts_Annotation := Copy (Parts_Annotation);

                              Unref (Relation.Second_Occurence);

                              Relation.Second_Occurence :=
                                To_Entity_Persistent_Access
                                  (To_Entity_Access (Scope_File, It));

                              Ada_Relation_Annotation
                                (Parts_Annotation.Other_Val.all).Side :=
                                Second;
                           elsif not Exists (Relation.Third_Occurence) then
                              --  If the second occurence exists, and is
                              --  different from the entity just found, then
                              --  we've found the third occurence

                              Parts_Annotation := Copy (Parts_Annotation);

                              Unref (Relation.Third_Occurence);

                              Relation.Third_Occurence :=
                                To_Entity_Persistent_Access
                                  (To_Entity_Access (Scope_File, It));

                              Ada_Relation_Annotation
                                (Parts_Annotation.Other_Val.all).Side :=
                                Third;
                           else
                              --  If everything is already set, there's an
                              --  error, we just don't do anything.

                              exit;
                           end if;

                           Set_Annotation
                             (Get_Annotation_Container (Scope_Tree, It).all,
                              Parts_Key,
                              Parts_Annotation);

                           Found := True;

                           exit;
                        end if;

                        Cur := Next (Cur);
                     end loop;

                     --  If we didn't found other parts for this construct,
                     --  then add it to the list of similar names.

                     if not Found then
                        Append
                          (Similar_Names.all,
                           To_Entity_Access (Scope_File, It));
                     end if;
                  else
                     Similar_Names := new Construct_List_Pckg.List;
                     Append
                       (Similar_Names.all, To_Entity_Access (Scope_File, It));
                     Insert (Dico.all, Lower_Name, Similar_Names);
                  end if;
               end;
            end if;

            if Is_Parent_Scope (It, Next (Scope_Tree, It, Jump_Into)) then
               Analyze_Scope (To_Entity_Access (Scope_File, It));
            end if;

            It := Next (Scope_Tree, It, Jump_Over);
         end loop;
      end Analyze_Scope;

      -----------------
      -- Same_Entity --
      -----------------

      function Same_Entity (Left, Right : Entity_Access) return Boolean is
         function Compatible_Type_Category
           (Cat : Language_Category) return Boolean;

         function Compatible_Type_Category
           (Cat : Language_Category) return Boolean
         is
         begin
            return Cat in Cat_Class .. Cat_Subtype or else Cat = Cat_Protected
              or else Cat = Cat_Task;
         end Compatible_Type_Category;

         Left_Construct : constant access Simple_Construct_Information :=
           Get_Construct (Left);
         Right_Construct : constant access Simple_Construct_Information :=
           Get_Construct (Right);
      begin
         if not
           ((Left_Construct.Category = Cat_Type
             and then Compatible_Type_Category (Right_Construct.Category))
            or else
              (Right_Construct.Category = Cat_Type
               and then Compatible_Type_Category (Left_Construct.Category)))
           and then Left_Construct.Category /= Right_Construct.Category
         then
            return False;
         end if;

         case Left_Construct.Category is
            when Cat_Function | Cat_Procedure | Cat_Entry =>
               --  Here check parameters types.
               return Same_Profile
                 (Left_Tree    => Get_Tree (Get_File (Left)),
                  Left_Sb      => To_Construct_Tree_Iterator (Left),
                  Right_Tree   => Get_Tree (Get_File (Right)),
                  Right_Sb     => To_Construct_Tree_Iterator (Right));

            when others =>
               --  In all other cases, name + category match is enough.
               return True;

         end case;
      end Same_Entity;

      use Construct_Annotations_Pckg;

      Spec_Unit        : Unit_Access;
      Body_Unit        : Unit_Access;
      Relation         : Ada_Relation_Access;
      Parts_Annotation : Annotation (Other_Kind);

      Spec_Entity      : Entity_Access;
      Body_Entity      : Entity_Access;
   begin
      if Unit = Null_Unit_Access then
         return;
      end if;

      --  Analyze this spec

      Spec_Unit := Get_Unit_Spec (Unit);
      Body_Unit := Get_Unit_Body (Unit);

      if ((Spec_Unit /= Null_Unit_Access
           and then Get_Parts_Up_To_Date (Spec_Unit))
          or else Spec_Unit = Null_Unit_Access)
        and then
          ((Body_Unit /= Null_Unit_Access
            and then Get_Parts_Up_To_Date (Body_Unit))
           or else Body_Unit = Null_Unit_Access)
      then
         --  The unit is completely up to date - we don't have to do anything
         --  here.

         return;
      end if;

      Spec_Entity := Get_Entity (Spec_Unit);

      if Spec_Entity = Null_Entity_Access then
         --  If no spec can be found, then the body acts as a spec
         Spec_Entity := Get_Entity (Body_Unit);
         Spec_Unit := Body_Unit;
         Body_Unit := Null_Unit_Access;
      else
         --  Otherwise, threat the body as his own.
         Body_Entity := Get_Entity (Body_Unit);
      end if;

      Relation := new Ada_Relation'
        (First_Occurence  =>
           To_Entity_Persistent_Access (Spec_Entity),
         Second_Occurence =>
           To_Entity_Persistent_Access (Body_Entity),
         Third_Occurence  => Null_Entity_Persistent_Access);

      if Spec_Unit /= Null_Unit_Access then
         Parts_Annotation.Other_Val :=
           new Ada_Relation_Annotation'
             (Relation => Relation,
              Side     => First);

         Set_Annotation
           (Get_Annotation_Container
              (Get_Tree (Get_File (Spec_Entity)),
               To_Construct_Tree_Iterator (Spec_Entity)).all,
            Parts_Key,
            Parts_Annotation);
      end if;

      if Body_Unit /= Null_Unit_Access then
         Parts_Annotation.Other_Val :=
           new Ada_Relation_Annotation'
             (Relation => Relation,
              Side     => Second);

         Set_Annotation
           (Get_Annotation_Container
              (Get_Tree (Get_File (Body_Entity)),
               To_Construct_Tree_Iterator (Body_Entity)).all,
            Parts_Key,
            Parts_Annotation);
      end if;

      if Spec_Unit /= Null_Unit_Access then
         Analyze_Scope (Get_Entity (Spec_Unit));

         Set_Parts_Up_To_Date (Spec_Unit, True);
      end if;

      if Body_Unit /= Null_Unit_Access then
         Analyze_Scope (Get_Entity (Body_Unit));

         Set_Parts_Up_To_Date (Body_Unit, True);
      end if;

      --  Free all the temporary dictionnaries

      declare
         It         : Dicos_List_Pckg.Cursor := First (Dico_List);
         Dico       : Dico_Access;
         Dico_It    : Local_Dico_Pckg.Cursor;
         Constructs : Construct_List_Access;
      begin
         while It /= Dicos_List_Pckg.No_Element loop
            Dico := Element (It);
            Dico_It := First (Dico.all);

            while Dico_It /= Local_Dico_Pckg.No_Element loop
               Constructs := Element (Dico_It);
               Free (Constructs);

               Dico_It := Next (Dico_It);
            end loop;

            Free (Dico);
            It := Next (It);
         end loop;
      end;
   end Analyze_Unit;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Obj : in out Ada_Relation_Annotation) is
   begin
      if Obj.Relation /= null then
         case Obj.Side is
            when First =>
               Unref (Obj.Relation.First_Occurence);
               Obj.Relation.First_Occurence := Null_Entity_Persistent_Access;

            when Second =>
               Unref (Obj.Relation.Second_Occurence);
               Obj.Relation.Second_Occurence := Null_Entity_Persistent_Access;

            when Third =>
               Unref (Obj.Relation.Third_Occurence);
               Obj.Relation.Third_Occurence := Null_Entity_Persistent_Access;

            when others =>
               null;

         end case;

         if Obj.Relation.First_Occurence = Null_Entity_Persistent_Access
           and then Obj.Relation.Second_Occurence
             = Null_Entity_Persistent_Access
           and then Obj.Relation.Third_Occurence
             = Null_Entity_Persistent_Access
         then
            Free (Obj.Relation);
         end if;

         Obj.Relation := null;
      end if;
   end Free;

   ------------------------------------
   -- Disconnect_Relation_Annotation --
   ------------------------------------

   procedure Disconnect_Relation_Annotation
     (Entity : Entity_Access; Key : Construct_Annotations_Pckg.Annotation_Key)
   is
      use Construct_Annotations_Pckg;

      Annot : Construct_Annotations_Pckg.Annotation;
      Rel   : access Ada_Relation_Annotation;
   begin
      Construct_Annotations_Pckg.Get_Annotation
        (Container => Get_Annotation_Container
           (Get_Tree (Get_File (Entity)),
            To_Construct_Tree_Iterator (Entity)).all,
         Key       => Key,
         Result    => Annot);

      if Annot = Construct_Annotations_Pckg.Null_Annotation then
         return;
      end if;

      Rel := Ada_Relation_Annotation (Annot.Other_Val.all)'Access;

      case Rel.Side is
         when First =>
            Unref (Rel.Relation.First_Occurence);
            Rel.Relation.First_Occurence := Null_Entity_Persistent_Access;

         when Second =>
            Unref (Rel.Relation.Second_Occurence);
            Rel.Relation.Second_Occurence := Null_Entity_Persistent_Access;

         when Third =>
            Unref (Rel.Relation.Third_Occurence);
            Rel.Relation.Third_Occurence := Null_Entity_Persistent_Access;

         when others =>
            null;
      end case;

      Rel.Relation := null;

      Construct_Annotations_Pckg.Free_Annotation
        (Container => Get_Annotation_Container
           (Get_Tree (Get_File (Entity)),
            To_Construct_Tree_Iterator (Entity)).all,
         Key       => Key);
   end Disconnect_Relation_Annotation;

   ------------------
   -- Get_Relation --
   ------------------

   function Get_Relation (Entity : Entity_Access) return Ada_Relation_Access is
      Assistant : Database_Assistant_Access;
      Unit :  Unit_Access;
   begin
      if Entity = Null_Entity_Access then
         return null;
      end if;

      Assistant :=
        Get_Assistant
          (Get_Database (Get_File (Entity)), Ada_Part_Assistant_Id);

      Unit := Get_Owning_Unit (Entity);

      Analyze_Unit (Assistant, Unit);

      return Get_Relation
        (Entity,
         Ada_Part_Db_Assistant (Assistant.all));
   end Get_Relation;

   ------------------
   -- Get_Relation --
   ------------------

   function Get_Relation
     (Entity    : Entity_Access;
      Assistant : Ada_Part_Db_Assistant'Class)
      return Ada_Relation_Access
   is
      use type Language.Tree.Construct_Annotations_Pckg.Annotation;

      It                  : Construct_Tree_Iterator;
      Relation_Annotation : Construct_Annotations_Pckg.Annotation;

   begin
      --  Then, extract the relation from the annotations.

      It := To_Construct_Tree_Iterator (Entity);

      Construct_Annotations_Pckg.Get_Annotation
        (Get_Annotation_Container (Get_Tree (Get_File (Entity)), It).all,
         Assistant.Parts_Key,
         Relation_Annotation);

      if Relation_Annotation = Construct_Annotations_Pckg.Null_Annotation then
         return null;

      else
         return Ada_Relation_Annotation
           (Relation_Annotation.Other_Val.all).Relation;
      end if;
   end Get_Relation;

   -------------------------
   -- Get_First_Occurence --
   -------------------------

   function Get_First_Occurence
     (Entity : Entity_Access) return Entity_Access
   is
      Relation : constant Ada_Relation_Access := Get_Relation (Entity);
   begin
      if Relation = null then
         return Entity;
      end if;

      return To_Entity_Access (Relation.First_Occurence);
   end Get_First_Occurence;

   --------------------------
   -- Get_Second_Occurence --
   --------------------------

   function Get_Second_Occurence
     (Entity : Entity_Access) return Entity_Access
   is
      Relation : constant Ada_Relation_Access := Get_Relation (Entity);
   begin
      if Relation = null then
         return Null_Entity_Access;
      end if;

      return To_Entity_Access (Relation.Second_Occurence);
   end Get_Second_Occurence;

   -------------------------
   -- Get_Third_Occurence --
   -------------------------

   function Get_Third_Occurence
     (Entity : Entity_Access) return Entity_Access
   is
      Relation : constant Ada_Relation_Access := Get_Relation (Entity);
   begin
      if Relation = null then
         return Null_Entity_Access;
      end if;

      return To_Entity_Access (Relation.Third_Occurence);
   end Get_Third_Occurence;

   ----------------------------
   -- Get_Most_Complete_View --
   ----------------------------

   function Get_Most_Complete_View
     (Entity : Entity_Access) return Entity_Access
   is
      Relation : constant Ada_Relation_Access := Get_Relation (Entity);
   begin
      if Relation = null then
         return Entity;
      end if;

      if Relation.Third_Occurence /= Null_Entity_Persistent_Access then
         return To_Entity_Access (Relation.Third_Occurence);
      elsif Relation.Second_Occurence /= Null_Entity_Persistent_Access then
         return To_Entity_Access (Relation.Second_Occurence);
      else
         return To_Entity_Access (Relation.First_Occurence);
      end if;
   end Get_Most_Complete_View;

   ----------------------------
   -- Is_Most_Complete_View --
   ----------------------------

   function Is_Most_Complete_View (Entity : Entity_Access) return Boolean is
      Relation : constant Ada_Relation_Access := Get_Relation (Entity);
   begin
      if Relation = null then
         return True;
      end if;

      if Relation.Third_Occurence /= Null_Entity_Persistent_Access then
         return To_Entity_Access (Relation.Third_Occurence) = Entity;
      elsif Relation.Second_Occurence /= Null_Entity_Persistent_Access then
         return To_Entity_Access (Relation.Second_Occurence) = Entity;
      else
         return True;
      end if;
   end Is_Most_Complete_View;

   ------------------------
   -- Is_First_Occurence --
   ------------------------

   function Is_First_Occurence (Entity : Entity_Access) return Boolean is
      Relation : constant Ada_Relation_Access := Get_Relation (Entity);
   begin
      if Relation = null then
         return True;
      end if;

      return To_Entity_Access (Relation.First_Occurence) = Entity;
   end Is_First_Occurence;

   ---------------------
   -- Are_Same_Entity --
   ---------------------

   function Are_Same_Entity (Left, Right : Entity_Access) return Boolean is
      Relation_Left : constant Ada_Relation_Access := Get_Relation (Left);
      Relation_Right : constant Ada_Relation_Access := Get_Relation (Right);
   begin
      return Left = Right
        or else
          (Relation_Left /= null and then Relation_Left = Relation_Right);
   end Are_Same_Entity;

   -------------------------------
   -- Unchecked_Are_Same_Entity --
   -------------------------------

   function Unchecked_Are_Same_Entity
     (Assistant   : Database_Assistant_Access;
      Left, Right : Entity_Access) return Boolean
   is
      Ada_Assistant : Ada_Part_Db_Assistant renames Ada_Part_Db_Assistant
        (Assistant.all);

      Left_Relation : constant Ada_Relation_Access :=
        Get_Relation (Left, Ada_Assistant);
      Left_Entity_Cmp : Entity_Persistent_Access;

      Right_Relation : constant Ada_Relation_Access :=
        Get_Relation (Right, Ada_Assistant);
      Right_Entity_Cmp : Entity_Persistent_Access;
   begin
      if Left_Relation = null then
         Left_Entity_Cmp := To_Unrefed_Entity_Persistent_Access (Left);
      else
         Left_Entity_Cmp := Left_Relation.First_Occurence;
      end if;

      if Right_Relation = null then
         Right_Entity_Cmp := To_Unrefed_Entity_Persistent_Access (Right);
      else
         Right_Entity_Cmp := Right_Relation.Second_Occurence;
      end if;

      return Left_Entity_Cmp = Right_Entity_Cmp;
   end Unchecked_Are_Same_Entity;

   ----------------------------------
   -- Get_Last_Visible_Declaration --
   ----------------------------------

   function Get_Last_Visible_Declaration
     (Entity : Entity_Access;
      File   : Structured_File_Access;
      Offset : String_Index_Type) return Entity_Access
   is
      Relation : constant Ada_Relation_Access := Get_Relation (Entity);

      Relation_Given : Location_Relation;

      First_Occ, Second_Occ, Third_Occ : Entity_Access;
   begin
      if Relation = null then
         return Entity;
      end if;

      if File = null then
         --  If we don't have proper visibility information, then we just
         --  return the most complete view.

         return Get_Most_Complete_View (Entity);
      end if;

      Relation_Given := Get_Location_Relation
        (Tree_To     => Get_Tree (Get_File (Entity)),
         Object_To   => Get_Parent_Scope
           (Get_Tree (Get_File (Entity)),
            To_Construct_Tree_Iterator (Entity)),
         Tree_From   => Get_Tree (File),
         Offset_From => Offset);

      case Relation_Given is
         when Package_Body =>
            return Get_Most_Complete_View (Entity);

         when Full_Spec_Hierarchy =>
            --  ??? We should check that we're indeed in a package spec here,
            --  and not a package body...
            return Get_Most_Complete_View (Entity);

         when None | Public_Spec_Hierarchy =>
            First_Occ := To_Entity_Access (Relation.First_Occurence);
            Second_Occ := To_Entity_Access (Relation.Second_Occurence);
            Third_Occ := To_Entity_Access (Relation.Third_Occurence);

            if Third_Occ /= Null_Entity_Access
              and then Is_Public_Library_Visible (Third_Occ)
            then
               return Third_Occ;
            elsif Second_Occ /= Null_Entity_Access
              and then Is_Public_Library_Visible (Second_Occ)
            then
               return Second_Occ;
            else
               return First_Occ;
            end if;

      end case;
   end Get_Last_Visible_Declaration;

   ---------------------------
   -- Unchecked_Is_In_Scope --
   ---------------------------

   function Unchecked_Is_In_Scope
     (Assistant   : Database_Assistant_Access;
      Scope       : Entity_Access;
      Entity      : Entity_Access) return Boolean
   is
      Ada_Assistant : Ada_Part_Db_Assistant renames Ada_Part_Db_Assistant
        (Assistant.all);

      Scope_Relation : constant Ada_Relation_Access :=
        Get_Relation (Scope, Ada_Assistant);
      Scope_Entity_Cmp : Entity_Persistent_Access;

      Entity_Relation : Ada_Relation_Access :=
        Get_Relation (Entity, Ada_Assistant);
      Entity_It       : Entity_Access := Entity;
      Entity_Cmp      : Entity_Persistent_Access;

      File : constant Structured_File_Access := Get_File (Entity);
      Tree : constant Construct_Tree := Get_Tree (File);
   begin
      if Scope_Relation = null then
         Scope_Entity_Cmp := To_Unrefed_Entity_Persistent_Access (Scope);
      else
         Scope_Entity_Cmp := Scope_Relation.First_Occurence;
      end if;

      loop
         if Entity_Relation = null then
            Entity_Cmp := To_Unrefed_Entity_Persistent_Access (Entity_It);
         else
            Entity_Cmp := Entity_Relation.First_Occurence;
         end if;

         if Entity_Cmp = Scope_Entity_Cmp then
            return True;
         end if;

         Entity_It := To_Entity_Access
           (File,
            Get_Parent_Scope (Tree, To_Construct_Tree_Iterator (Entity_It)));

         exit when Entity_It = Null_Entity_Access;

         Entity_Relation := Get_Relation (Entity_It, Ada_Assistant);
      end loop;

      return False;
   end Unchecked_Is_In_Scope;

end Ada_Semantic_Tree.Parts;
