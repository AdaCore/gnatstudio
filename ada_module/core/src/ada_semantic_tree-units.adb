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

with System;                  use System;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada_Semantic_Tree.Lang;  use Ada_Semantic_Tree.Lang;
with GNATCOLL.Symbols;        use GNATCOLL.Symbols;
with GNATCOLL.Projects;       use GNATCOLL.Projects;

package body Ada_Semantic_Tree.Units is

   Ada_Unit_Assistant_Id : constant String := "ADA_UNIT_ASSISTANT";

   type Ada_Unit_Assistant is new Database_Assistant with record
      Unit_Key      : Construct_Annotations_Pckg.Annotation_Key;
      Unit_List_Key : Tree_Annotations_Pckg.Annotation_Key;
      Units_Db      : aliased Construct_Unit_Tries.Construct_Trie;
      --  The unit information is duplicated in order to make as efficient as
      --  possible three critical operations:
      --    give the unit corresponding to an entity
      --    give the units contained in a file
      --    give the units of a given name database-wise

      Waiting_For_Parent : aliased Construct_Unit_Tries.Construct_Trie;
      --  This database holds the units that are not linked with their parent
      --  yet. They are stored by their expected parent name. Once their parent
      --  is found, they are supposed to be removed from this list.
   end record;

   overriding
   procedure Free (Assistant : in out Ada_Unit_Assistant);

   type Unit_List_Annotation is new
     Tree_Annotations_Pckg.General_Annotation_Record
   with record
      List : Persistent_Entity_List.Set;
   end record;

   overriding
   procedure File_Updated
     (Assistant : access Ada_Unit_Assistant;
      File      : Structured_File_Access;
      Old_Tree  : Construct_Tree;
      Kind      : Update_Kind);

   function Get_Unit_Info
     (Key    : Construct_Annotations_Pckg.Annotation_Key;
      Entity : Entity_Access) return Unit_Access;

   procedure Initialize_Local_Trie (Unit : Unit_Access);
   --  Initialize the local construct trie container.

   procedure Unlink_Parent (Unit : in out Unit_Access_Record);
   --  Unlink this unit and its parent relationship if any.

   procedure Reset_Unit_Links (Unit : in out Unit_Access_Record);
   --  Reset all links for this entity - including links to the database. The
   --  only thing that remains is the link between the construct and the
   --  entity, at the annotation level. Note that this leaves the database
   --  in an inconsistent state which should be taken care of afterwards.

   ----------
   -- Free --
   ----------

   overriding procedure Free (Assistant : in out Ada_Unit_Assistant) is
   begin
      Clear (Assistant.Units_Db);
      Clear (Assistant.Waiting_For_Parent);
   end Free;

   -------------------
   -- Get_Unit_Info --
   -------------------

   function Get_Unit_Info
     (Key    : Construct_Annotations_Pckg.Annotation_Key;
      Entity : Entity_Access) return Unit_Access
   is
      use Construct_Annotations_Pckg;

      Obj : Annotation;
   begin
      if Is_Compilation_Unit (To_Construct_Tree_Iterator (Entity)) then
         Get_Annotation
           (Get_Annotation_Container
              (Get_Tree (Get_File (Entity)),
               To_Construct_Tree_Iterator (Entity)).all, Key, Obj);

         if Obj /= Construct_Annotations_Pckg.Null_Annotation then
            return Unit_Access (Obj.Other_Val);
         end if;
      end if;

      return null;
   end Get_Unit_Info;

   ------------------------
   -- Register_Assistant --
   ------------------------

   procedure Register_Assistant (Db : Construct_Database_Access) is
      use Construct_Annotations_Pckg;
      use Tree_Annotations_Pckg;

      Unit_Key : Construct_Annotations_Pckg.Annotation_Key;
      Tree_Unit_Key : Tree_Annotations_Pckg.Annotation_Key;
   begin
      Get_Annotation_Key
        (Get_Construct_Annotation_Key_Registry (Db).all,
         Unit_Key);

      Get_Annotation_Key
        (Get_Tree_Annotation_Key_Registry (Db).all,
         Tree_Unit_Key);

      Register_Assistant
        (Db,
         Ada_Unit_Assistant_Id,
         new Ada_Unit_Assistant'
           (Database_Assistant with
            Unit_Key           => Unit_Key,
            Unit_List_Key      => Tree_Unit_Key,
            Units_Db           => Construct_Unit_Tries.Empty_Construct_Trie,
            Waiting_For_Parent => Construct_Unit_Tries.Empty_Construct_Trie));
   end Register_Assistant;

   ---------------
   -- Get_Units --
   ---------------

   function Get_Units
     (Db         : Construct_Database_Access;
      Name       : String;
      Is_Partial : Boolean) return Unit_Iterator
   is
      It           : Unit_Iterator;
      Db_Assistant : Database_Assistant_Access;
   begin
      Db_Assistant := Get_Assistant (Db, Ada_Unit_Assistant_Id);

      It.Db_Iterator := Start
        (Ada_Unit_Assistant (Db_Assistant.all).Units_Db'Access,
         Name,
         Is_Partial);

      It.Unit_Key := Ada_Unit_Assistant (Db_Assistant.all).Unit_Key;
      It.Unit_List_Key := Ada_Unit_Assistant (Db_Assistant.all).Unit_List_Key;

      return It;
   end Get_Units;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Db : Construct_Database_Access; Name : String) return Unit_Access
   is
      Id : constant Composite_Identifier :=
        To_Composite_Identifier (To_Lower (Name));

      Unit_It   : Unit_Iterator;
      Root_Unit : Unit_Access;

      function Get_Child
        (Parent : Unit_Access; Index : Integer) return Unit_Access;

      function Get_Child
        (Parent : Unit_Access; Index : Integer) return Unit_Access
      is
         Cur : Persistent_Entity_List.Cursor;
         Child_U : Unit_Access;
      begin
         if Index > Length (Id) then
            return Parent;
         end if;

         Cur := Parent.Children_Units.First;

         while Cur /= Persistent_Entity_List.No_Element loop
            Child_U := Get_Unit_Access (To_Entity_Access (Element (Cur)));

            if To_Lower (Get_Item (Child_U.Name.all, Index))
              = Get_Item (Id, Index)
            then
               return Get_Child (Child_U, Index + 1);
            end if;

            Cur := Next (Cur);
         end loop;

         return Null_Unit_Access;
      end Get_Child;

   begin
      if Length (Id) = 0 then
         return Null_Unit_Access;
      end if;

      Unit_It := Get_Units (Db, Get_Item (Id, 1), False);

      while not At_End (Unit_It) loop
         Root_Unit := Get (Unit_It);

         if Length (Root_Unit.Name.all) = 1
           and then Get_Construct (Get_Entity (Root_Unit)).Is_Declaration
         then
            Free (Unit_It);

            return Get_Child (Root_Unit, 2);
         end if;

         Next (Unit_It);
      end loop;

      Free (Unit_It);

      return Null_Unit_Access;
   end Get_Unit;

   ---------------
   -- Get_Units --
   ---------------

   function Get_Units
     (File : Structured_File_Access) return Unit_Iterator
   is
      Tree            : Construct_Tree;
      It              : Unit_Iterator;
      Db_Assistant    : Database_Assistant_Access;
      Annotation_List : Tree_Annotations_Pckg.Annotation;
   begin
      Db_Assistant := Get_Assistant
        (Get_Database (File), Ada_Unit_Assistant_Id);
      Tree := Get_Tree (File);

      Tree_Annotations_Pckg.Get_Annotation
        (Get_Annotation_Container (Tree).all,
         Ada_Unit_Assistant (Db_Assistant.all).Unit_List_Key,
         Annotation_List);

      It.Unit_Cursor := First
        (Unit_List_Annotation (Annotation_List.Other_Val.all).List);

      It.Unit_Key := Ada_Unit_Assistant (Db_Assistant.all).Unit_Key;
      It.Unit_List_Key := Ada_Unit_Assistant (Db_Assistant.all).Unit_List_Key;

      return It;
   end Get_Units;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Unit_Iterator) is
   begin
      if It.Unit_Cursor /= Persistent_Entity_List.No_Element then
         It.Unit_Cursor := Next (It.Unit_Cursor);
      else
         Next (It.Db_Iterator);
      end if;
   end Next;

   ----------
   -- Free --
   ----------

   procedure Free (It : in out Unit_Iterator) is
   begin
      Free (It.Db_Iterator);
   end Free;

   ---------
   -- Get --
   ---------

   function Get (It : Unit_Iterator) return Entity_Access is
   begin
      if It.Unit_Cursor /= Persistent_Entity_List.No_Element then
         return To_Entity_Access (Element (It.Unit_Cursor));
      else
         return To_Entity_Access
           (Get_Additional_Data (It.Db_Iterator).Entity);
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (It : Unit_Iterator) return Unit_Access is
   begin
      if It.Unit_Cursor /= Persistent_Entity_List.No_Element then
         return Get_Unit_Info
           (It.Unit_Key, To_Entity_Access (Element (It.Unit_Cursor)));
      else
         return Get_Additional_Data (It.Db_Iterator);
      end if;
   end Get;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Unit : Unit_Access) return Unit_Access is
   begin
      if Unit = Null_Unit_Access then
         return Null_Unit_Access;
      end if;

      if not Exists (Unit.Parent) then
         return Null_Unit_Access;
      end if;

      return Get_Unit_Info (Unit.Unit_Key, To_Entity_Access (Unit.Parent));
   end Get_Parent;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity (Unit : Unit_Access) return Entity_Access is
   begin
      if Unit = Null_Unit_Access then
         return Null_Entity_Access;
      end if;

      return To_Entity_Access (Unit.Entity);
   end Get_Entity;

   ------------
   -- At_End --
   ------------

   function At_End (It : Unit_Iterator) return Boolean is
   begin
      return It.Unit_Cursor = Persistent_Entity_List.No_Element
        and then At_End (It.Db_Iterator);
   end At_End;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children (Unit : Unit_Access) return Unit_Iterator is
      It           : Unit_Iterator;
      Unit_Entity  : Entity_Access;
      Db_Assistant : Database_Assistant_Access;
      Spec         : Unit_Access;
      Spec_Entity  : Entity_Access;
   begin
      if Unit /= Null_Unit_Access then
         Db_Assistant := Get_Assistant
           (Get_Database (Get_File (Unit.Entity)), Ada_Unit_Assistant_Id);

         Unit_Entity := To_Entity_Access (Unit.Entity);

         It.Unit_Key :=
           Ada_Unit_Assistant (Db_Assistant.all).Unit_Key;
         It.Unit_List_Key :=
           Ada_Unit_Assistant (Db_Assistant.all).Unit_List_Key;

         if Get_Construct (Unit_Entity).Is_Declaration then
            It.Unit_Cursor := First (Unit.Children_Units);
         else
            Spec_Entity := To_Entity_Access (Unit.Spec_Unit);

            if Spec_Entity = Null_Entity_Access then
               return Null_Unit_Iterator;
            end if;

            Spec := Get_Unit_Info (It.Unit_Key, Spec_Entity);

            It.Unit_Cursor := First (Spec.Children_Units);
         end if;

         return It;
      else
         return Null_Unit_Iterator;
      end if;
   end Get_Children;

   ------------------
   -- File_Updated --
   ------------------

   overriding procedure File_Updated
     (Assistant : access Ada_Unit_Assistant;
      File      : Structured_File_Access;
      Old_Tree  : Construct_Tree;
      Kind      : Update_Kind)
   is
      use Tree_Annotations_Pckg;

      pragma Unreferenced (Old_Tree);

      Tree : constant Construct_Tree := Get_Tree (File);
      Unit_List_Annot : Tree_Annotations_Pckg.Annotation;

      procedure Analyze_Units (Start_It : Construct_Tree_Iterator);
      --  Remove all previous units from the file, and create the new ones.
      --  This does not create parent / child relationships (but removes the
      --  previous ones if any).

      function Is_Waiting_For_Parent
        (Unit : Unit_Access_Record) return Boolean;
      pragma Unreferenced (Is_Waiting_For_Parent);
      --  Return true if the unit given in parameter may have a parent set,
      --  that is to say either it has no parent, or it's a body currently
      --  linked with its parent unit - but for which we could find the spec.

      procedure Find_And_Link_Relations (Unit : Unit_Access);
      --  Find and linkd children & parent relations found in the unit
      --  database.

      procedure Invalidate_Local_Constructs;

      -------------------
      -- Analyze_Units --
      -------------------

      procedure Analyze_Units (Start_It : Construct_Tree_Iterator) is
         It        : Construct_Tree_Iterator := Start_It;
         Construct : access Simple_Construct_Information;

         Info            : Unit_Access;
         Already_Found   : Boolean := False;
         Last_End_Entity : Entity_Persistent_Access :=
           Null_Entity_Persistent_Access;
      begin
         declare
            Unit_It : Persistent_Entity_List.Cursor := First
              (Unit_List_Annotation (Unit_List_Annot.Other_Val.all).List);
            Tmp_It : Persistent_Entity_List.Cursor;
            Unit     : Unit_Access;
            Unit_Entity : Entity_Access;
         begin
            --  Clear all the unit of the file

            while Unit_It /= Persistent_Entity_List.No_Element loop
               Tmp_It := Next (Unit_It);

               Unit := Get_Unit_Info
                 (Assistant.Unit_Key, To_Entity_Access (Element (Unit_It)));

               if Exists (Unit.Entity) then
                  Unit_Entity := To_Entity_Access (Unit.Entity);

                  if Is_Compilation_Unit
                    (To_Construct_Tree_Iterator (Unit_Entity))
                  then
                     --  If this entity is still a compilation unit, we'll
                     --  update it in the next step, so we'll just unlink it
                     --  without deleting it.

                     Reset_Unit_Links (Unit.all);
                  else
                     --  Otherwise, we've got to delete the complete
                     --  annotation.

                     Construct_Annotations_Pckg.Free_Annotation
                       (Get_Annotation_Container
                          (Tree, To_Construct_Tree_Iterator (Unit_Entity)).all,
                        Assistant.Unit_Key);
                  end if;
               end if;

               Unit_It := Tmp_It;
            end loop;
         end;

         Clear (Unit_List_Annotation (Unit_List_Annot.Other_Val.all).List);

         --  Then look for all units trough the file, and put the corresponding
         --  annotations.

         while It /= Null_Construct_Tree_Iterator loop
            Construct := Get_Construct (It);

            if Is_Compilation_Unit (It)
              and then Construct /= null
              and then Construct.Name /= No_Symbol
            then
               Info := Get_Unit_Info
                 (Assistant.Unit_Key, To_Entity_Access (File, It));

               if Info /= null then
                  --  If the info is already here, we just increment its
                  --  timestamp.

                  Info.This_Timestamp := Info.This_Timestamp + 1;
               else
                  --  If the info is not here, this is a new unit. Compute it.

                  Info := new Unit_Access_Record;

                  Info.Entity := To_Entity_Persistent_Access
                    (To_Entity_Access (File, It));
                  Info.Name := new Composite_Identifier'
                    (To_Composite_Identifier (Get (Construct.Name).all));
                  Info.Unit_Key := Assistant.Unit_Key;
                  Construct_Annotations_Pckg.Set_Annotation
                    (Get_Annotation_Container (Tree, It).all,
                     Assistant.Unit_Key,
                     (Kind      => Construct_Annotations_Pckg.Other_Kind,
                      Other_Val =>
                        Construct_Annotations_Pckg.General_Annotation (Info)));

                  if Construct.Is_Declaration then
                     Info.Spec_Unit := Info.Entity;
                     Ref (Info.Spec_Unit);
                  else
                     Info.Body_Unit := Info.Entity;
                     Ref (Info.Body_Unit);
                  end if;
               end if;

               Insert
                 (Unit_List_Annotation (Unit_List_Annot.Other_Val.all).List,
                  Info.Entity);

               Insert
                 (Assistant.Units_Db'Access,
                  Get_Database (File).Symbols,
                  It,
                  Info,
                  Ada_Tree_Lang,
                  Info.Db_Index);

               --  In any case, we've got to recompute the Start and End
               --  markers, since they may have changed since last edition.

               if not Already_Found then
                  Info.Start_Entity :=
                    To_Entity_Persistent_Access
                      (To_Entity_Access (File, First (Tree)));
               else
                  Info.Start_Entity := Last_End_Entity;
                  Ref (Info.Start_Entity);
               end if;

               Info.End_Entity := To_Entity_Persistent_Access
                 (To_Entity_Access
                    (File, Next (Tree, It, Jump_Over)));

               Last_End_Entity := Info.End_Entity;

               Already_Found := True;
            end if;

            It := Next (Tree, It, Jump_Over);
         end loop;
      end Analyze_Units;

      ---------------------------
      -- Is_Waiting_For_Parent --
      ---------------------------

      function Is_Waiting_For_Parent
        (Unit : Unit_Access_Record) return Boolean
      is
      begin
         return not Exists (Unit.Parent)
           or else
             (not Get_Construct (Unit.Entity).Is_Declaration
              and then not Unit.Parent_Is_Spec);
      end Is_Waiting_For_Parent;

      -----------------------------
      -- Find_And_Link_Relations --
      -----------------------------

      procedure Find_And_Link_Relations (Unit : Unit_Access) is
         It           : Unit_Iterator;
         It_Construct : access Simple_Construct_Information;
         pragma Unreferenced (It_Construct);
         It_Unit      : Unit_Access;
         It_Entity    : Entity_Access;

         Unit_Entity : Entity_Access;
         Unit_Construct : access Simple_Construct_Information;

         Children_It  : Construct_Unit_Tries.Construct_Trie_Iterator;
         Child_Unit   : Unit_Access;
         Parent_Found : Boolean := False;
      begin
         if Unit = null then
            return;
         end if;

         Unit_Entity := To_Entity_Access (Unit.Entity);
         Unit_Construct := Get_Construct (Unit_Entity);

         if Unit_Construct.Is_Declaration then
            --  First, if this is a spec, look for all units that are waiting
            --  for a parent of this  name, and link them if this is indeed
            --  their  parent.

            Children_It := Start
              (Assistant.Waiting_For_Parent'Access,
               Get_Item (Unit.Name.all, Length (Unit.Name.all)),
               False);

            while not At_End (Children_It) loop
               Child_Unit := Get_Additional_Data (Children_It);

               --  If the full unit name is actually a prefix of the child,
               --  we found a child

               if Is_Prefix_Of (Unit.Name.all, Child_Unit.Name.all, False)
                 and then Get_Project (Get_File (Unit.Entity))
                 = Get_Project (Get_File (Child_Unit.Entity))
               then
                  Unlink_Parent (Child_Unit.all);

                  Child_Unit.Parent := Unit.Entity;

                  Insert (Unit.Children_Units, Child_Unit.Entity);

                  Ref (Child_Unit.Parent);

                  Delete
                    (Assistant.Waiting_For_Parent,
                     Child_Unit.Waiting_For_Parent_Index);

                  Child_Unit.Waiting_For_Parent_Index :=
                    Construct_Unit_Tries.Null_Construct_Trie_Index;
               end if;

               Next (Children_It);
            end loop;

            Free (Children_It);

            --  Then, look for its body

            It := Get_Units
              (Get_Database (File),
               Get_Item (Unit.Name.all, Length (Unit.Name.all)),
               False);

            while not At_End (It) loop
               It_Unit := Get (It);
               It_Entity := Get_Entity (It_Unit);

               if not Get_Construct (It_Entity).Is_Declaration
                 and then Equal (It_Unit.Name.all, Unit.Name.all, False)
                 and then Get_Project (Get_File (It_Unit.Entity))
                 = Get_Project (Get_File (Unit.Entity))
               then
                  Unlink_Parent (It_Unit.all);

                  if Unit.Body_Unit /= Null_Entity_Persistent_Access then
                     --  ??? we do not support multiple bodies for a spec -
                     --  the heuristic we take is unreferencing the previous
                     --  one if any.

                     Unref (Unit.Body_Unit);
                  end if;

                  It_Unit.Parent :=
                    To_Entity_Persistent_Access (Unit_Entity);
                  It_Unit.Parent_Is_Spec := True;

                  It_Unit.Spec_Unit := Unit.Entity;
                  Unit.Body_Unit := It_Unit.Entity;

                  Ref (It_Unit.Spec_Unit);
                  Ref (Unit.Body_Unit);

                  exit;
               end if;

               Next (It);
            end loop;

            Free (It);

            --  Finally, look for its parent, if it has one

            if Length (Unit.Name.all) > 1 then
               It := Get_Units
                 (Get_Database (File),
                  Get_Item (Unit.Name.all, Length (Unit.Name.all) - 1),
                  False);

               while not At_End (It) loop
                  It_Unit := Get (It);
                  It_Entity := Get_Entity (It_Unit);

                  if Get_Construct (It_Entity).Is_Declaration
                  and then Is_Prefix_Of
                      (It_Unit.Name.all, Unit.Name.all, False)
                  then
                     Unlink_Parent (Unit.all);

                     Unit.Parent := To_Entity_Persistent_Access (It_Entity);

                     Insert (It_Unit.Children_Units, Unit.Entity);

                     Parent_Found := True;

                     exit;
                  end if;

                  Next (It);
               end loop;

               if not Parent_Found then
                  Construct_Unit_Tries.Insert
                    (Trie         => Assistant.Waiting_For_Parent'Access,
                     Symbols      => Get_Database (File).Symbols,
                     Construct_It => To_Construct_Tree_Iterator
                       (To_Entity_Access (Unit.Entity)),
                     Name         => Get_Item
                       (Unit.Name.all, Length (Unit.Name.all) - 1),
                     Data         => Unit,
                     Lang         => Ada_Tree_Lang,
                     Index        => Unit.Waiting_For_Parent_Index);
               end if;

               Free (It);
            end if;
         else
            --  If this is a body, we'll only look for its spec.

            It := Get_Units
              (Get_Database (File),
               Get_Item (Unit.Name.all, Length (Unit.Name.all)),
               False);

            while not At_End (It) loop
               It_Unit := Get (It);
               It_Entity := Get_Entity (It_Unit);

               if Get_Construct (It_Entity).Is_Declaration
                 and then Equal (It_Unit.Name.all, Unit.Name.all, False)
                 and then Get_Project (Get_File (It_Unit.Entity))
                 = Get_Project (Get_File (Unit.Entity))
               then
                  Unlink_Parent (Unit.all);

                  if It_Unit.Body_Unit /= Null_Entity_Persistent_Access then
                     --  ??? we do not support multiple bodies for a spec -
                     --  the heuristic we take is unreferencing the previous
                     --  one if any.

                     Unref (It_Unit.Body_Unit);
                  end if;

                  Unit.Parent := To_Entity_Persistent_Access (It_Entity);
                  Unit.Parent_Is_Spec := True;

                  Unit.Spec_Unit := It_Unit.Entity;
                  It_Unit.Body_Unit := Unit.Entity;

                  Ref (Unit.Spec_Unit);
                  Ref (It_Unit.Body_Unit);

                  exit;
               end if;

               Next (It);
            end loop;

            Free (It);
         end if;
      end Find_And_Link_Relations;

      ---------------------------------
      -- Invalidate_Local_Constructs --
      ---------------------------------

      procedure Invalidate_Local_Constructs is
         Unit_It : Persistent_Entity_List.Cursor := First
           (Unit_List_Annotation (Unit_List_Annot.Other_Val.all).List);
      begin
         while Unit_It /= Persistent_Entity_List.No_Element loop
            Get_Unit_Info
              (Assistant.Unit_Key,
               To_Entity_Access (Element (Unit_It)))
              .Local_Constructs_Up_To_Date := False;

            Unit_It := Next (Unit_It);
         end loop;
      end Invalidate_Local_Constructs;

   begin
      --  Set the tree annotation - and create one if none.

      Get_Annotation
        (Get_Annotation_Container (Tree).all,
         Assistant.Unit_List_Key,
         Unit_List_Annot);

      if Unit_List_Annot = Null_Annotation then
         Unit_List_Annot :=
           (Kind      => Other_Kind,
            Other_Val => new Unit_List_Annotation);

         Set_Annotation
           (Get_Annotation_Container (Tree).all,
            Assistant.Unit_List_Key,
            Unit_List_Annot);
      end if;

      if Kind = Minor_Change then
         --  We've got nothing more to do in the case of a minor change.

         Invalidate_Local_Constructs;

         return;
      end if;

      --  Extract the units declared in this file, and add them in the
      --  database.

      Analyze_Units (First (Tree));

      --  Now, update the parent / child unit relationships.

      declare
         New_Unit_It : Construct_Tree_Iterator := First (Tree);
         New_Entity : Entity_Access;
         New_Unit : Unit_Access;
      begin
         while New_Unit_It /= Null_Construct_Tree_Iterator loop
            if Is_Compilation_Unit (New_Unit_It) then
               New_Entity := To_Entity_Access (File, New_Unit_It);
               New_Unit := Get_Unit_Info (Assistant.Unit_Key, New_Entity);

               Find_And_Link_Relations (New_Unit);
            end if;

            New_Unit_It := Next (Tree, New_Unit_It, Jump_Over);
         end loop;
      end;

      Invalidate_Local_Constructs;
   end File_Updated;

   ---------------------
   -- Get_Owning_Unit --
   ---------------------

   function Get_Owning_Unit
     (File : Structured_File_Access; Offset : String_Index_Type)
      return Unit_Access
   is
      Tree  : constant Construct_Tree := Get_Tree (File);
      Unit  : Construct_Tree_Iterator := First (Tree);

      Start_Unit_It : Construct_Tree_Iterator;
      End_Unit_It : Construct_Tree_Iterator;

      Unit_Info : Unit_Access;
      Db_Assistant : Database_Assistant_Access;
      Unit_Key     : Construct_Annotations_Pckg.Annotation_Key;
   begin
      Db_Assistant := Get_Assistant
        (Get_Database (File), Ada_Unit_Assistant_Id);

      Unit_Key := Ada_Unit_Assistant (Db_Assistant.all).Unit_Key;

      while Unit /= Null_Construct_Tree_Iterator loop
         if Is_Compilation_Unit (Unit) then
            Unit_Info := Get_Unit_Info
              (Unit_Key, To_Entity_Access (File, Unit));

            if Unit_Info = null then
               return null;
            end if;

            Start_Unit_It :=
              To_Construct_Tree_Iterator (Get_Start_Entity (Unit_Info));
            End_Unit_It :=
              To_Construct_Tree_Iterator (Get_End_Entity (Unit_Info));

            if Natural (Offset) >=
              Get_Construct (Start_Unit_It).Sloc_Start.Index
              and then
                (End_Unit_It = Null_Construct_Tree_Iterator
                 or else Natural (Offset) <
                   Get_Construct (End_Unit_It).Sloc_Start.Index)
            then
               return Unit_Info;
            end if;
         end if;

         Unit := Next (Tree, Unit, Jump_Over);
      end loop;

      return Null_Unit_Access;
   end Get_Owning_Unit;

   function Get_Owning_Unit (Entity : Entity_Access) return Unit_Access is
   begin
      return Get_Owning_Unit
        (Get_File (Entity),
         String_Index_Type (Get_Construct (Entity).Sloc_Start.Index));
   end Get_Owning_Unit;

   ----------------------
   -- Get_Start_Entity --
   ----------------------

   function Get_Start_Entity (Unit : Unit_Access) return Entity_Access is
   begin
      return To_Entity_Access (Unit.Start_Entity);
   end Get_Start_Entity;

   --------------------
   -- Get_End_Entity --
   --------------------

   function Get_End_Entity (Unit : Unit_Access) return Entity_Access is
   begin
      return To_Entity_Access (Unit.End_Entity);
   end Get_End_Entity;

   ---------------------
   -- Get_Unit_Access --
   ---------------------

   function Get_Unit_Access (Unit : Entity_Access) return Unit_Access is
      Db_Assistant : Database_Assistant_Access;
      Unit_Key     : Construct_Annotations_Pckg.Annotation_Key;
   begin
      if Unit /= Null_Entity_Access then
         Db_Assistant := Get_Assistant
           (Get_Database (Get_File (Unit)), Ada_Unit_Assistant_Id);

         Unit_Key := Ada_Unit_Assistant (Db_Assistant.all).Unit_Key;

         return Get_Unit_Info (Unit_Key, Unit);
      else
         return Null_Unit_Access;
      end if;
   end Get_Unit_Access;

   -------------------
   -- Get_Unit_Body --
   -------------------

   function Get_Unit_Body (Unit : Unit_Access) return Unit_Access is
      Db_Assistant : Database_Assistant_Access;
      Unit_Key     : Construct_Annotations_Pckg.Annotation_Key;
      Unit_Entity  : constant Entity_Access := To_Entity_Access (Unit.Entity);
   begin
      if not Get_Construct (Unit_Entity).Is_Declaration then
         return Unit;
      elsif not Exists (Unit.Body_Unit) then
         return Null_Unit_Access;
      else
         Db_Assistant := Get_Assistant
           (Get_Database (Get_File (Unit.Body_Unit)), Ada_Unit_Assistant_Id);

         Unit_Key := Ada_Unit_Assistant (Db_Assistant.all).Unit_Key;

         return Get_Unit_Info (Unit_Key, To_Entity_Access (Unit.Body_Unit));
      end if;
   end Get_Unit_Body;

   -------------------
   -- Get_Unit_Spec --
   -------------------

   function Get_Unit_Spec (Unit : Unit_Access) return Unit_Access is
      Db_Assistant : Database_Assistant_Access;
      Unit_Key     : Construct_Annotations_Pckg.Annotation_Key;
      Unit_Entity  : constant Entity_Access := To_Entity_Access (Unit.Entity);
   begin
      if Get_Construct (Unit_Entity).Is_Declaration then
         return Unit;
      elsif not Exists (Unit.Spec_Unit) then
         return Null_Unit_Access;
      else
         Db_Assistant := Get_Assistant
           (Get_Database (Get_File (Unit.Spec_Unit)), Ada_Unit_Assistant_Id);

         Unit_Key := Ada_Unit_Assistant (Db_Assistant.all).Unit_Key;

         return Get_Unit_Info (Unit_Key, To_Entity_Access (Unit.Spec_Unit));
      end if;
   end Get_Unit_Spec;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Unit : Unit_Access) return Composite_Identifier is
   begin
      return Unit.Name.all;
   end Get_Name;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Unit : in out Unit_Access_Record) is
   begin
      Reset_Unit_Links (Unit);

      Unref (Unit.Entity);
      Free (Unit.Name);
   end Free;

   -------------------
   -- Is_In_Parents --
   -------------------

   function Is_In_Parents
     (Parent : Unit_Access; Child : Unit_Access) return Boolean
   is
      Indirect_Parent : Unit_Access := Child;
      Db_Assistant : Database_Assistant_Access;
      Unit_Key     : Construct_Annotations_Pckg.Annotation_Key;
   begin
      if Child = null or else Parent = null then
         return False;
      end if;

      Db_Assistant := Get_Assistant
        (Get_Database (Get_File (Child.Start_Entity)), Ada_Unit_Assistant_Id);

      Unit_Key := Ada_Unit_Assistant (Db_Assistant.all).Unit_Key;

      while Indirect_Parent /= Null_Unit_Access loop
         if Indirect_Parent.Entity = Parent.Entity then
            return True;
         end if;

         Indirect_Parent := Get_Unit_Info
           (Unit_Key, To_Entity_Access (Indirect_Parent.Parent));
      end loop;

      return False;
   end Is_In_Parents;

   ------------------------------
   -- Get_Dependency_Timestamp --
   ------------------------------

   function Get_Dependency_Timestamp (Unit : Unit_Access) return Integer is
   begin
      return Unit.Dep_Timestamp;
   end Get_Dependency_Timestamp;

   ------------------------------
   -- Set_Dependency_Timestamp --
   ------------------------------

   procedure Set_Dependency_Timestamp
     (Unit : Unit_Access; Timestamp : Integer)
   is
   begin
      Unit.Dep_Timestamp := Timestamp;
   end Set_Dependency_Timestamp;

   ---------------------------
   -- Get_Current_Timestamp --
   ---------------------------

   function Get_Current_Timestamp (Unit : Unit_Access) return Integer is
   begin
      return Unit.This_Timestamp;
   end Get_Current_Timestamp;

   ------------------------------
   -- Has_Updated_Dependencies --
   ------------------------------

   function Has_Updated_Dependencies (Unit : Unit_Access) return Boolean is
   begin
      return Unit.Is_Up_To_Date;
   end Has_Updated_Dependencies;

   ------------------------------
   -- Set_Updated_Dependencies --
   ------------------------------

   procedure Set_Updated_Dependencies (Unit : Unit_Access; Dep : Boolean) is
   begin
      Unit.Is_Up_To_Date := Dep;
   end Set_Updated_Dependencies;

   ----------------------------------
   -- Get_Unit_Hierarchy_Timestamp --
   ----------------------------------

   function Get_Unit_Hierarchy_Timestamp
     (Unit : Unit_Access) return Unit_Hierarchy_Timestamp
   is
      It_Unit : Unit_Access := Unit;
      Result  : Unit_Hierarchy_Timestamp :=
        Unit_Hierarchy_Timestamp (It_Unit.This_Timestamp);
   begin
      while It_Unit.Parent /= Null_Entity_Persistent_Access loop
         It_Unit := Get_Unit_Info
           (It_Unit.Unit_Key, To_Entity_Access (It_Unit.Parent));

         Result := Result + Unit_Hierarchy_Timestamp (It_Unit.This_Timestamp);
      end loop;

      return Result;
   end Get_Unit_Hierarchy_Timestamp;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Unit_Access) return Boolean is
   begin
      if Left = null then
         return Right /= null;
      elsif Right = null then
         return False;
      else
         return Left.all'Address < Right.all'Address;
      end if;
   end "<";

   --------------------------
   -- Set_Parts_Up_To_Date --
   --------------------------

   procedure Set_Parts_Up_To_Date (Unit : Unit_Access; Value : Boolean) is
   begin
      Unit.Parts_Up_To_Date := Value;
   end Set_Parts_Up_To_Date;

   --------------------------
   -- Get_Parts_Up_To_Date --
   --------------------------

   function Get_Parts_Up_To_Date (Unit : Unit_Access) return Boolean is
   begin
      return Unit.Parts_Up_To_Date;
   end Get_Parts_Up_To_Date;

   ---------------------------
   -- Initialize_Local_Trie --
   ---------------------------

   procedure Initialize_Local_Trie (Unit : Unit_Access) is
      It : Construct_Tree_Iterator :=
        To_Construct_Tree_Iterator (To_Entity_Access (Unit.Start_Entity));
      End_It : constant Construct_Tree_Iterator :=
        To_Construct_Tree_Iterator (To_Entity_Access (Unit.End_Entity));
      Construct : access Simple_Construct_Information;
      File : constant Structured_File_Access :=
        Get_File (To_Entity_Access (Unit.Start_Entity));
      Tree : constant Construct_Tree := Get_Tree (File);

      Dummy_Index : Local_Construct_Trie.Construct_Trie_Index;
   begin
      if Unit.Local_Constructs_Up_To_Date then
         return;
      end if;

      Clear (Unit.Local_Constructs);

      while It /= End_It loop
         Construct := Get_Construct (It);

         if Construct.Name /= No_Symbol
           and then Construct.Category /= Cat_Field
           and then Construct.Category /= Cat_With
           and then Construct.Category /= Cat_Use
         then
            Insert
              (Trie         => Unit.Local_Constructs'Access,
               Symbols      => Get_Database (File).Symbols,
               Construct_It => It,
               Data         => 0,
               Lang         => Ada_Tree_Lang,
               Index        => Dummy_Index);
         end if;

         It := Next (Tree, It, Jump_Into);
      end loop;

      Unit.Local_Constructs_Up_To_Date := True;
   end Initialize_Local_Trie;

   -----------
   -- First --
   -----------

   function First
     (Unit : Unit_Access; Name : String; Is_Partial : Boolean)
      return Local_Construct_Iterator
   is
   begin
      Initialize_Local_Trie (Unit);

      return
        (File => Get_File (Unit.Start_Entity),
         It   => Start (Unit.Local_Constructs'Access, Name, Is_Partial));
   end First;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Local_Construct_Iterator) is
   begin
      Next (It.It);
   end Next;

   ---------
   -- Get --
   ---------

   function Get (It : Local_Construct_Iterator) return Entity_Access is
   begin
      return To_Entity_Access (It.File, Get_Construct_It (It.It));
   end Get;

   ------------
   -- At_End --
   ------------

   function At_End (It : Local_Construct_Iterator) return Boolean is
   begin
      return At_End (It.It);
   end At_End;

   ----------
   -- Free --
   ----------

   procedure Free (It : in out Local_Construct_Iterator) is
   begin
      Free (It.It);
   end Free;

   -------------------
   -- Unlink_Parent --
   -------------------

   procedure Unlink_Parent (Unit : in out Unit_Access_Record) is
      Parent_Unit : Unit_Access;
      Parent_Entity : Entity_Access;
   begin
      if Unit.Parent /= Null_Entity_Persistent_Access then
         Parent_Entity := To_Entity_Access (Unit.Parent);

         if Parent_Entity /= Null_Entity_Access then
            Parent_Unit :=
              Get_Unit_Info (Unit.Unit_Key, Parent_Entity);

            if Contains (Parent_Unit.Children_Units, Unit.Entity) then
               Delete (Parent_Unit.Children_Units, Unit.Entity);
            end if;
         end if;

         if Unit.Spec_Unit = Unit.Parent then
            Unref (Unit.Spec_Unit);
         end if;

         Unref (Unit.Parent);
      end if;
   end Unlink_Parent;

   ----------------------
   -- Reset_Unit_Links --
   ----------------------

   procedure Reset_Unit_Links (Unit : in out Unit_Access_Record) is
      Child_It   : Persistent_Entity_List.Cursor;
      Child      : Unit_Access;
      Assistant  : Database_Assistant_Access;
      List_Annot : Tree_Annotations_Pckg.Annotation;
      File       : constant Structured_File_Access := Get_File (Unit.Entity);

      Other_Part : Entity_Persistent_Access;
   begin
      Assistant := Get_Assistant
        (Get_Database (Get_File (Unit.Entity)), Ada_Unit_Assistant_Id);

      --  Disconnect children

      Child_It := First (Unit.Children_Units);

      while Child_It /= Persistent_Entity_List.No_Element loop
         if Exists (Element (Child_It)) then
            Child := Get_Unit_Info
              (Unit.Unit_Key,
               To_Entity_Access (Element (Child_It)));

            Construct_Unit_Tries.Insert
              (Trie         =>
                 Ada_Unit_Assistant (Assistant.all).Waiting_For_Parent'Access,
               Symbols       => Get_Database (File).Symbols,
               Construct_It => To_Construct_Tree_Iterator
                 (To_Entity_Access (Child.Entity)),
               Name         => Get_Item
                 (Child.Name.all, Length (Child.Name.all) - 1),
               Data         => Child,
               Lang         => Ada_Tree_Lang,
               Index        => Child.Waiting_For_Parent_Index);

            Unref (Child.Parent);
            Child.Parent := Null_Entity_Persistent_Access;
         end if;

         Child_It := Next (Child_It);
      end loop;

      Clear (Unit.Children_Units);

      --  Disconnect corresponding body / spec

      if Unit.Entity = Unit.Spec_Unit then
         Other_Part := Unit.Body_Unit;
      else
         Other_Part := Unit.Spec_Unit;
      end if;

      if Exists (Other_Part) then
         declare
            Unit_To_Disconnect : constant Unit_Access :=
              Get_Unit_Info (Unit.Unit_Key, To_Entity_Access (Other_Part));
         begin
            if Unit_To_Disconnect /= Null_Unit_Access then
               if Unit.Entity = Unit_To_Disconnect.Body_Unit then
                  Unref (Unit_To_Disconnect.Body_Unit);
               elsif Unit.Entity = Unit_To_Disconnect.Spec_Unit then
                  Unref (Unit_To_Disconnect.Spec_Unit);
               end if;
            end if;
         end;
      end if;

      --  Free remaining data

      Delete
        (Ada_Unit_Assistant (Assistant.all).Units_Db, Unit.Db_Index);

      Tree_Annotations_Pckg.Get_Annotation
        (Get_Annotation_Container (Get_Tree (File)).all,
         Ada_Unit_Assistant (Assistant.all).Unit_List_Key,
         List_Annot);

      Delete
        (Unit_List_Annotation (List_Annot.Other_Val.all).List,
         Unit.Entity);

      if Unit.Waiting_For_Parent_Index
        /= Construct_Unit_Tries.Null_Construct_Trie_Index
      then
         Delete
           (Ada_Unit_Assistant (Assistant.all).Waiting_For_Parent,
            Unit.Waiting_For_Parent_Index);

         Unit.Waiting_For_Parent_Index :=
           Construct_Unit_Tries.Null_Construct_Trie_Index;
      end if;

      Unlink_Parent (Unit);

      Unref (Unit.Spec_Unit);
      Unref (Unit.Body_Unit);
      Unref (Unit.Start_Entity);
      Unref (Unit.End_Entity);
   end Reset_Unit_Links;

end Ada_Semantic_Tree.Units;
