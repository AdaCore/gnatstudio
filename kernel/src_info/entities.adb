-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2005                       --
--                             AdaCore                               --
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

with Ada.Calendar;            use Ada.Calendar;
with Ada.Unchecked_Deallocation;
with VFS;                     use VFS;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Projects;                use Projects;
with Traces;                  use Traces;
with Language_Handlers.GPS; use Language_Handlers.GPS;
with GNAT.Heap_Sort_G;
with Namet;                   use Namet;
with Projects.Registry;       use Projects.Registry;
with File_Utils;              use File_Utils;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GPS.Intl;              use GPS.Intl;
with GNAT.Calendar.Time_IO;   use GNAT.Calendar.Time_IO;
with Language;                use Language;
with String_Utils;            use String_Utils;
with Entities.Queries;        use Entities.Queries;
with Entities.Debug; use Entities.Debug;

package body Entities is
   Assert_Me : constant Debug_Handle := Create ("Entities.Assert", Off);

   Dump_Me : constant Debug_Handle := Create ("ENTITIES.Dump", Off);
   --  If active, will generate a lot of dumps of the database while doing a
   --  reset of a Source_File. This should really only be used on very small
   --  files

   Ref_Me  : constant Debug_Handle := Create ("Entities.Ref", Off);
   --  If active, will trace all calls to Ref/Unref for Entity_Information.
   --  That generates a lot of output!

   Debug_Me  : constant Debug_Handle := Create ("Entities.Debug", Off);
   --  If True, no memory is freed, this makes the structure more robust and
   --  easier to debug, but will of course use more memory.

   Add_Reference_Force_Unique : constant Debug_Handle :=
     Create ("Entities.Force_Unique_Ref", On);
   --  Activate this to force a check that all references are unique. This
   --  will slow down the parsing a little, but might help customers work
   --  around bugs in GPS. This should never be needed however, and is just
   --  provided as a backdoor.
   --  ??? Comment above is obsolete and should be updated

   use Entities_Tries;
   use Files_HTable;
   use LI_HTable;
   use Entity_Information_Arrays;
   use Source_File_Arrays;
   use Entity_Reference_Arrays;
   use Dependency_Arrays;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Entity_Information_Record'Class, Entity_Information);

   function String_Hash is new HTables.Hash (HTable_Header);

   procedure Mark_Dependencies_As_Invalid
     (Entity : Entity_Information;
      Dependencies_On : Source_File;
      Can_Remove      : out Boolean);
   --  Remove all references to Dependencies_On in Entity.
   --  On exit, Can_Remove indicates whether the entity can now be deleted.
   --  This is the case if it is invalid, and all its references where in
   --  Dependencies_On.

   function Find
     (E : Dependency_List; File : Source_File)
      return Dependency_Arrays.Index_Type;
   --  Find a specific file in the list of dependencies

   procedure Isolate
     (Entity           : in out Entity_Information;
      Clear_References : Boolean);
   --  Isolate the entity from the rest of the LI structure. This should be
   --  used when we are removing the entity from the internal tables, but the
   --  user has kept a handle on it, to avoid memory leaks.
   --  The references are cleared only if Clear_References is True.

   procedure Remove  (D : in out Entities_Tries.Trie_Tree;
                      E : Entity_Information);
   --  Remove the information for a specific entity from the table.
   --  This doesn't Unref the entity, you need to do it explicitly

   procedure Unref_All_Entities_In_List
     (Trie : access Entities_Tries.Trie_Tree; Reason : String);
   procedure Unref_All_Entities_In_List
     (List : in out Entity_Information_List; Reason : String);
   --  Unref all the entities in the trie

   procedure Add_All_Entities
     (File : Source_File; Entity : Entity_Information);
   --  Add Entity to the list of All_Entities known in File, if Entity is not
   --  there yet and is not declared in File.

   procedure Mark_And_Isolate_Entities (File : Source_File);
   --  All entities declared in file are marked as valid, and all information
   --  known about them is cleared (we just keep their name and declaration).
   --  All occurrences internal to File are removed for all entities declared
   --  in file. However, we do not destroy references in other files, so that
   --  we can still do cross-references from these.

   procedure Mark_Dependencies_As_Invalid
     (Tree : access Entities_Tries.Trie_Tree; Dependencies_On : Source_File);
   --  All entities in Tree that are referenced elsewhere are marked as invalid

   procedure Remove_Marked_Entities (File : Source_File);
   --  Unref all entities declared in File that are marked as valid.

   procedure Remove_Marked_Entities_In_List
     (Tree : access Entities_Tries.Trie_Tree; File : Source_File);
   --  Remove from Tree all the entities declared in File and that are marked
   --  as valid. These entities are about to be removed from memory anyway

   procedure Get_Documentation_Before
     (Context       : Language_Context;
      Buffer        : String;
      Decl_Index    : Natural;
      Comment_Start : out Natural;
      Comment_End   : out Natural);
   procedure Get_Documentation_After
     (Context       : Language_Context;
      Buffer        : String;
      Decl_Index    : Natural;
      Comment_Start : out Natural;
      Comment_End   : out Natural);
   --  Get the comment just before or just after Decl_Index, skipping code
   --  lines as needed.

   function Internal_Get_Or_Create
     (Db            : Entities_Database;
      Full_Filename : VFS.Cst_UTF8_String_Access;
      File          : VFS.Virtual_File;
      LI            : LI_File := null;
      Timestamp     : Ada.Calendar.Time := VFS.No_Time;
      Allow_Create  : Boolean := True) return Source_File;
   --  Internal version for Get_Or_Create

   Is_Subprogram_Entity : constant array (E_Kinds) of Boolean :=
     (Procedure_Kind        => True,
      Function_Or_Operator  => True,
      Entry_Or_Entry_Family => True,
      Task_Kind             => True,
      others                => False);
   --  This table should contain true if the corresponding element is
   --  considered as a subprogram (see Is_Subprogram)

   Is_Container_Array : constant array (E_Kinds) of Boolean :=
     (Procedure_Kind        => True,
      Function_Or_Operator  => True,
      Entry_Or_Entry_Family => True,
      Task_Kind             => True,
      Package_Kind          => True,
      Overloaded_Entity     => True,
      --  ??? Should we check that at least one of the possible
      --  completions is a subprogram
      others                => False);

   Is_End_Reference_Array : constant Reference_Kind_Filter :=
     (End_Of_Spec           => True,
      End_Of_Body           => True,
      others                => False);
   --  True if the matching entity indicates an end-of-scope (end of subprogram
   --  declaration, end of record definition, ...)

   Is_Parameter_Array : constant Reference_Kind_Filter :=
     (Subprogram_In_Parameter                  => True,
      Subprogram_In_Out_Parameter              => True,
      Subprogram_Out_Parameter                 => True,
      Subprogram_Access_Parameter              => True,
      others                                   => False);

   Show_In_Call_Graph_Array : constant Reference_Kind_Filter :=
     (Reference                                => True,
      Modification                             => True,
      others                                   => False);

   ------------------------
   -- Show_In_Call_Graph --
   ------------------------

   function Show_In_Call_Graph (Kind : Reference_Kind) return Boolean is
   begin
      return Show_In_Call_Graph_Array (Kind);
   end Show_In_Call_Graph;

   -----------------------
   -- Is_Read_Reference --
   -----------------------

   function Is_Read_Reference  (Kind : Reference_Kind) return Boolean is
   begin
      return Read_Reference_Filter (Kind);
   end Is_Read_Reference;

   ------------------------
   -- Is_Write_Reference --
   ------------------------

   function Is_Write_Reference (Kind : Reference_Kind) return Boolean is
   begin
      return Write_Reference_Filter (Kind);
   end Is_Write_Reference;

   ----------------------------
   -- Is_Parameter_Reference --
   ----------------------------

   function Is_Parameter_Reference (Kind : Reference_Kind) return Boolean is
   begin
      return Is_Parameter_Array (Kind);
   end Is_Parameter_Reference;

   ----------------------
   -- Is_End_Reference --
   ----------------------

   function Is_End_Reference (Kind : Reference_Kind) return Boolean is
   begin
      return Is_End_Reference_Array (Kind);
   end Is_End_Reference;

   -----------------------
   -- Is_Real_Reference --
   -----------------------

   function Is_Real_Reference (Kind : Reference_Kind) return Boolean is
   begin
      return Real_References_Filter (Kind);
   end Is_Real_Reference;

   ------------------
   -- Is_Container --
   ------------------

   function Is_Container (Kind : E_Kinds) return Boolean is
   begin
      return Is_Container_Array (Kind);
   end Is_Container;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Entity : Entity_Information) return String_Access is
   begin
      if Entity = null then
         return null;
      else
         return Entity.Name;
      end if;
   end Get_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (D : Entity_Information_List_Access) return String_Access is
   begin
      if D = null
        or else Length (D.all) = 0
      then
         return null;
      else
         if Active (Assert_Me) then
            Assert
              (Assert_Me,
               D.Table (Entity_Information_Arrays.First).Ref_Count /= 0,
               "Entity has been freed: " &
               D.Table (Entity_Information_Arrays.First).Name.all);
         end if;

         return D.Table (Entity_Information_Arrays.First).Name;
      end if;
   end Get_Name;

   ---------
   -- Ref --
   ---------

   procedure Ref (F : Source_File) is
   begin
      if F /= null then
         F.Ref_Count := F.Ref_Count + 1;
      end if;
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (F : in out Source_File) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Source_File_Record'Class, Source_File);
   begin
      if F /= null then
         Assert (Assert_Me, F.Ref_Count > 0, "Too many unref");

         F.Ref_Count := F.Ref_Count - 1;
         if F.Ref_Count = 0 then
            --  File not removed from htable explicitely. Unref is already
            --  called internally when the file is removed from the htable,
            --  and the user is not supposed to call Unref more often than Ref
            Reset (F);

            if not Active (Debug_Me) then
               Unchecked_Free (F);
            else
               F := null;
            end if;
         end if;
      end if;
   end Unref;

   ----------------------
   -- Add_All_Entities --
   ----------------------

   procedure Add_All_Entities
     (File : Source_File; Entity : Entity_Information)
   is
   begin
      if Entity.Declaration.File /= File then
         --  ??? This might be costly, but we need to ensure there is a proper
         --  reference between the two files, for proper clean up

         --  ??? MANU If this call still needed, now that All_Entities only
         --  contain entities with actual references... It seems this call will
         --  already be done from Add_Reference anyway
         Add_Depends_On (File, Entity.Declaration.File);

         Add (File.All_Entities, Entity, Check_Duplicates => True);
      end if;
   end Add_All_Entities;

   -----------
   -- Reset --
   -----------

   procedure Reset (LI : LI_File) is
   begin
      Trace (Assert_Me, "Reseting LI " & Full_Name (LI.Name).all);
      for F in Source_File_Arrays.First .. Last (LI.Files) loop
         Reset (LI.Files.Table (F));
      end loop;
   end Reset;

   ------------
   -- Remove --
   ------------

   procedure Remove (E : in out Dependency_List; File : Source_File) is
      Index : constant Dependency_Arrays.Index_Type := Find (E, File);
   begin
      if Index >= Dependency_Arrays.First then
         Remove (E, Index);
      end if;
   end Remove;

   ----------
   -- Find --
   ----------

   function Find
     (E : Dependency_List; File : Source_File)
      return Dependency_Arrays.Index_Type is
   begin
      for L in Dependency_Arrays.First .. Last (E) loop
         if E.Table (L).File = File then
            return L;
         end if;
      end loop;
      return Dependency_Arrays.First - 1;
   end Find;

   -------------------------------
   -- Mark_And_Isolate_Entities --
   -------------------------------

   procedure Mark_And_Isolate_Entities (File : Source_File) is
      Iter   : Entities_Tries.Iterator := Start (File.Entities'Access, "");
      EL     : Entity_Information_List_Access;
      Entity, Entity2 : Entity_Information;
   begin
      loop
         EL := Get (Iter);
         exit when EL = null;

         for E in reverse Entity_Information_Arrays.First .. Last (EL.all) loop
            Entity := EL.Table (E);
            Entity.Is_Valid := True;

            --  If the entity has references in other files, do not mark it
            --  as valid, so that it isn't removed later on, or we would lose
            --  trace of these references.
            --  ??? Could be more efficient and remove ranges at once
            for R in reverse
              Entity_Reference_Arrays.First .. Last (Entity.References)
            loop
               if Entity.References.Table (R).Location.File /= File then
                  Entity.Is_Valid := False;
               else
                  if Entity.References.Table (R).Caller /= null then
                     Trace (Ref_Me, "Unref "
                            & Entity.References.Table (R).Caller.Name.all
                            & " from reference.caller");
                     Unref (Entity.References.Table (R).Caller);
                  end if;

                  Remove (Entity.References, R);
               end if;
            end loop;

            Isolate (Entity, Clear_References => False);
         end loop;

         Next (Iter);
      end loop;
      Free (Iter);

      --  Do not remove the references in File for all the valid entities in
      --  All_Entities: we want to keep those references, so that the xref in
      --  this other files still work as expected.
      --  However, if an entity was marked as invalid already, it means that
      --  its original source file no longer knows it, so we can remove its
      --  references

      Iter := Start (File.All_Entities'Access, "");
      loop
         EL := Get (Iter);
         exit when EL = null;
         for E in reverse Entity_Information_Arrays.First .. Last (EL.all) loop
            Entity := EL.Table (E);

            if not Entity.Is_Valid then
               for R in reverse
                 Entity_Reference_Arrays.First .. Last (Entity.References)
               loop
                  --  Do not touch references in other files, if there is any.
                  if Entity.References.Table (R).Location.File = File then
                     if Entity.References.Table (R).Caller /= null then
                        Trace (Ref_Me, "Unref "
                               & Entity.References.Table (R).Caller.Name.all
                               & " from reference.caller");
                        Unref (Entity.References.Table (R).Caller);
                     end if;

                     Remove (Entity.References, R);
                  end if;
               end loop;

               Entity2 := Entity;  --  In case we need two unref below
               Remove (File.All_Entities, Entity);
               if Active (Assert_Me) then
                  Trace (Assert_Me, "Unref " & Entity.Name.all
                         & " from all_entities in "
                         & Base_Name (File.Name));
               end if;
               Unref (Entity);
               --  Entity.Ref_Count is at least 1 at this point

               if Length (Entity2.References) = 0 then
                  --  The entity still existed in memory because it was
                  --  referenced from one or more external files, and we just
                  --  removed it from the last such file => free it
                  Trace (Assert_Me, "Unref " & Entity2.Name.all
                         & " since it has no more references");
                  Unref (Entity2);
               end if;
            end if;
         end loop;
         Next (Iter);
      end loop;
      Free (Iter);
   end Mark_And_Isolate_Entities;

   ----------------------------------
   -- Mark_Dependencies_As_Invalid --
   ----------------------------------

   procedure Mark_Dependencies_As_Invalid
     (Tree : access Entities_Tries.Trie_Tree; Dependencies_On : Source_File)
   is
      Iter : Entities_Tries.Iterator := Start (Tree, "");
      EL   : Entity_Information_List_Access;
      Can_Remove : Boolean;
      Entity : Entity_Information;
   begin
      loop
         EL := Get (Iter);
         exit when EL = null;

         for E in reverse Entity_Information_Arrays.First .. Last (EL.all) loop
            Entity := EL.Table (E);
            Mark_Dependencies_As_Invalid (Entity, Dependencies_On, Can_Remove);
            if Can_Remove then
               if Entity_Information_Arrays.Length (EL.all) = 1 then
                  Remove (Tree.all, Entity.Name.all);
               else
                  Remove (EL.all, E);
               end if;

               Trace (Ref_Me, "Unref " & Entity.Name.all
                      & " from Mark_Dependencies_As_Invalid");
               Unref (Entity);
            end if;
         end loop;

         Next (Iter);
      end loop;
      Free (Iter);
   end Mark_Dependencies_As_Invalid;

   ----------------------------
   -- Remove_Marked_Entities --
   ----------------------------

   procedure Remove_Marked_Entities (File : Source_File) is
      Iter   : Entities_Tries.Iterator := Start (File.Entities'Access, "");
      EL     : Entity_Information_List_Access;
      Entity : Entity_Information;
   begin
      loop
         EL := Get (Iter);
         exit when EL = null;

         for E in reverse Entity_Information_Arrays.First .. Last (EL.all) loop
            Entity := EL.Table (E);

            if Entity.Is_Valid
              or else Entity.Ref_Count = 1
            then
               if Entity_Information_Arrays.Length (EL.all) = 1 then
                  Remove (File.Entities, Entity.Name.all);
               else
                  Remove (EL.all, E);
               end if;

               Trace (Ref_Me, "Unref " & Entity.Name.all
                      & " from Remove_Marked_Entities");
               Unref (Entity);
            end if;

            --  Entities that are now marked as invalid still have a ref_count
            --  owned by their source file. The next time we parse that file,
            --  we will clear them if their ref_count is now done to 1.
         end loop;

         Next (Iter);
      end loop;
      Free (Iter);
   end Remove_Marked_Entities;

   ------------------------------------
   -- Remove_Marked_Entities_In_List --
   ------------------------------------

   procedure Remove_Marked_Entities_In_List
     (Tree : access Entities_Tries.Trie_Tree; File : Source_File)
   is
      Iter   : Entities_Tries.Iterator := Start (Tree, "");
      EL     : Entity_Information_List_Access;
      Entity : Entity_Information;
   begin
      loop
         EL := Get (Iter);
         exit when EL = null;

         for E in reverse Entity_Information_Arrays.First .. Last (EL.all) loop
            Entity := EL.Table (E);

            if Entity.Declaration.File = File
              and then Entity.Is_Valid
            then
               if Entity_Information_Arrays.Length (EL.all) = 1 then
                  Remove (Tree.all, Entity.Name.all);
               else
                  Remove (EL.all, E);
               end if;

               Trace (Ref_Me, "Unref " & Entity.Name.all
                      & " from Remove_Marked_Entities_In_List");
               Unref (Entity);
            end if;
         end loop;

         Next (Iter);
      end loop;
      Free (Iter);
   end Remove_Marked_Entities_In_List;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : Source_File) is
   begin
      Trace (Assert_Me, "Reseting " & Full_Name (File.Name).all);

      --  Mark all entities as valid temporarily
      --  We also clean them up, so that they don't reference anything else

      Entities.Debug.Set_Default_Output;
      if Active (Dump_Me) then
         Trace (Dump_Me, "Before Reset");
         Entities.Debug.Dump (Get_Database (File));
      end if;

      Mark_And_Isolate_Entities (File);

      if Active (Dump_Me) then
         Trace (Dump_Me, "After Mark_And_Isolate_Entities");
         Entities.Debug.Dump (Get_Database (File));
      end if;

      --  Mark as invalid all entities that are pointed to by other files.
      --  At the same time, references in File are removed, since they are no
      --  longer valid.

      for F in Dependency_Arrays.First .. Last (File.Depended_On) loop
         Mark_Dependencies_As_Invalid
           (File.Depended_On.Table (F).File.Entities'Access,
            Dependencies_On => File);
      end loop;

      if Active (Dump_Me) then
         Trace (Dump_Me, "After Mark_Dependencies_As_Invalid");
         Entities.Debug.Dump (Get_Database (File));
      end if;

      --  Other files can no longer reference entities in File that are no
      --  longer valid

      for F in Dependency_Arrays.First .. Last (File.Depends_On) loop
         Remove_Marked_Entities_In_List
           (File.Depends_On.Table (F).File.All_Entities'Access, File);
      end loop;

      for F in Dependency_Arrays.First .. Last (File.Depended_On) loop
         Remove_Marked_Entities_In_List
           (File.Depended_On.Table (F).File.All_Entities'Access, File);
      end loop;

      if Active (Dump_Me) then
         Trace (Dump_Me, "After Remove_Marked_Entities_In_List");
         Entities.Debug.Dump (Get_Database (File));
      end if;

      --  Remove all entities that are still marked as valid (ie that are not
      --  referenced)

      Remove_Marked_Entities (File);

      if Active (Dump_Me) then
         Trace (Dump_Me, "After Remove_Marked_Entities");
         Entities.Debug.Dump (Get_Database (File));
      end if;

      --  We can't reset the Depended_On field, since these other files still
      --  referenced File (we haven't broken up the links between entities).
      --  The current file itself might depend on others, in case some entities
      --  had references in other files. But such references do not need to be
      --  in the Depends_On array

      for F in Dependency_Arrays.First .. Last (File.Depends_On) loop
         Remove (File.Depends_On.Table (F).File.Depended_On, File);
      end loop;

      if not Active (Debug_Me) then
         Free (File.Depends_On);
      end if;

      if Active (Dump_Me) then
         Trace (Dump_Me, "After Removing dependencies");
         Entities.Debug.Dump (Get_Database (File));
      end if;

      --  Clean up various other fields

      File.Scope_Tree_Computed := False;

      if not Active (Debug_Me) then
         Free (File.Unit_Name);
      end if;

      --  Fields which have not been cleaned (see comments above):
      --     - Entities
      --     - Depended_On (cleaned "magically" when the other files are Reset)
   end Reset;

   --------------------------------
   -- Unref_All_Entities_In_List --
   --------------------------------

   procedure Unref_All_Entities_In_List
     (Trie : access Entities_Tries.Trie_Tree; Reason : String)
   is
      Iter   : Entities_Tries.Iterator := Entities_Tries.Start (Trie, "");
      EL     : Entity_Information_List_Access;
      Entity : Entity_Information;
   begin
      loop
         EL := Get (Iter);
         exit when EL = null;

         for E in reverse Entity_Information_Arrays.First .. Last (EL.all) loop
            Entity := EL.Table (E);
            Trace (Ref_Me, "Unref " & Entity.Name.all
                   & " from Unref_All_Entities_In_List ("
                   & Reason & ")");
            Unref (Entity);
         end loop;

         Entities_Tries.Next (Iter);
      end loop;

      Free (Iter);
      Clear (Trie.all);
   end Unref_All_Entities_In_List;

   --------------------------------
   -- Unref_All_Entities_In_List --
   --------------------------------

   procedure Unref_All_Entities_In_List
     (List : in out Entity_Information_List; Reason : String) is
   begin
      if Entity_Information_Arrays.Length (List) /= 0 then
         for J in reverse
           Entity_Information_Arrays.First .. Last (List)
         loop
            Trace (Ref_Me, "Unref " & List.Table (J).Name.all
                   & " from Unref_All_Entities_In_List ("
                   & Reason & ")");
            Unref (List.Table (J));
         end loop;
         Free (List);
      end if;
   end Unref_All_Entities_In_List;

   -------------
   -- Isolate --
   -------------

   procedure Isolate
     (Entity : in out Entity_Information; Clear_References : Boolean)
   is
      Entity2 : Entity_Information;
   begin
      Trace (Ref_Me, "Isolate " & Entity.Name.all);
      if Entity.Caller_At_Declaration /= null then
         Trace (Ref_Me, "Unref " & Entity.Caller_At_Declaration.Name.all
                & " from caller_at_decl");
         Unref (Entity.Caller_At_Declaration);
      end if;

      Entity.End_Of_Scope    := No_E_Reference;

      if Entity.Pointed_Type /= null then
         Trace (Ref_Me, "Unref " & Entity.Pointed_Type.Name.all
                & " from pointed_type");
         Unref (Entity.Pointed_Type);
      end if;

      if Entity.Returned_Type /= null then
         Trace (Ref_Me, "Unref " & Entity.Returned_Type.Name.all
                  & " from returned_type");
         Unref (Entity.Returned_Type);
      end if;

      if Entity.Primitive_Op_Of /= null then
         Trace (Ref_Me, "Unref " & Entity.Primitive_Op_Of.Name.all
                  & " from primitive_op");
         Unref (Entity.Primitive_Op_Of);
      end if;

      if Entity.Rename /= null then
         Trace (Ref_Me, "Unref " & Entity.Rename.Name.all
                  & " from rename");
         Unref (Entity.Rename);
      end if;

      Unref_All_Entities_In_List (Entity.Called_Entities'Access,
                                  "called_entities");

      if Entity_Information_Arrays.Length (Entity.Parent_Types) /= 0 then
         for J in reverse
           Entity_Information_Arrays.First .. Last (Entity.Parent_Types)
         loop
            --  It is possible that the entity is not in the list of
            --  Child_Types for the parent, depending on the order the LI files
            --  were refreshed. We need to check before we can unreference the
            --  entity. In addition, we take into account cases where the
            --  entity appears multiple times in the list of child_types (that
            --  should never happen in practice)

            for P in Entity_Information_Arrays.First ..
              Last (Entity.Parent_Types.Table (J).Child_Types)
            loop
               if Entity.Parent_Types.Table (J).Child_Types.Table (P) =
                 Entity
               then
                  Entity2 := Entity; --  Since Unref puts it back to null
                  Remove (Entity2.Parent_Types.Table (J).Child_Types, P);
                  Trace (Assert_Me, "Unref " & Entity2.Name.all
                         & " From Isol.Parent_Types.Child_Types");
                  Unref (Entity2);  --  will never actually free Entity
               end if;
            end loop;
         end loop;

         Unref_All_Entities_In_List (Entity.Parent_Types, "parent_types");
      end if;

      Unref_All_Entities_In_List
        (Entity.Primitive_Subprograms, "primitive_subprograms");
      Unref_All_Entities_In_List (Entity.Child_Types, "child_types");

      if Clear_References then
         for R in reverse
           Entity_Reference_Arrays.First .. Last (Entity.References)
         loop
            if Entity.References.Table (R).Caller /= null then
               Trace (Ref_Me, "Unref "
                      & Entity.References.Table (R).Caller.Name.all
                      & " since reference.caller");
               Unref (Entity.References.Table (R).Caller);
            end if;
         end loop;

         Free (Entity.References);
      end if;
   end Isolate;

   ----------------------------------
   -- Mark_Dependencies_As_Invalid --
   ----------------------------------

   procedure Mark_Dependencies_As_Invalid
     (Entity          : Entity_Information;
      Dependencies_On : Source_File;
      Can_Remove      : out Boolean)
   is
      procedure Check_And_Remove (E : in out Entity_Information);
      procedure Check_And_Remove (E : in out Entity_Information_List);
      procedure Check_And_Remove (E : in out Entity_Reference_List);
      procedure Check_And_Remove (E : in out E_Reference);
      procedure Check_And_Remove (E : access Entities_Tries.Trie_Tree);
      --  Remove all references to File in E

      procedure Check_And_Remove (E : in out Entity_Information) is
      begin
         if E /= null and then E.Declaration.File = Dependencies_On then
            E.Is_Valid := False;
         end if;
         Can_Remove := Can_Remove and then
           (E = null or else not E.Is_Valid);
      end Check_And_Remove;

      procedure Check_And_Remove (E : in out Entity_Information_List) is
      begin
         for J in reverse Entity_Information_Arrays.First .. Last (E) loop
            Assert (Assert_Me, E.Table (J) /= null, "Invalid entity in list");

            if Active (Debug_Me) then
               if E.Table (J).Ref_Count = 0 then
                  Trace (Debug_Me, "Entity should no longer be referenced "
                         & E.Table (J).Name.all
                         & "But is still referenced from "
                         & Entity.Name.all & ":"
                         & Base_Name (Get_Filename (Entity.Declaration.File)));
               end if;
            end if;

            Assert
              (Assert_Me, E.Table (J).Declaration.File /= null,
               "Invalid declaration");
            if E.Table (J).Declaration.File = Dependencies_On then
               E.Table (J).Is_Valid := False;
            end if;
         end loop;
         Can_Remove := Can_Remove
           and then Entity_Information_Arrays.Length (E) = 0;
      end Check_And_Remove;

      procedure Check_And_Remove (E : access Entities_Tries.Trie_Tree) is
         Iter : Entities_Tries.Iterator := Start (E, "");
         EL   : Entity_Information_List_Access;
      begin
         loop
            EL := Get (Iter);
            exit when EL = null;

            Check_And_Remove (EL.all);
            Next (Iter);
         end loop;
      end Check_And_Remove;

      procedure Check_And_Remove (E : in out E_Reference) is
      begin
         if E.Location.File = Dependencies_On then
            if E.Caller /= null then
               Trace (Ref_Me, "Unref " & E.Caller.Name.all
                      & " from reference.caller");
               Unref (E.Caller);
            end if;
            E := No_E_Reference;
         end if;
         Can_Remove := Can_Remove and then E = No_E_Reference;
      end Check_And_Remove;

      procedure Check_And_Remove (E : in out Entity_Reference_List) is
      begin
         for R in reverse Entity_Reference_Arrays.First .. Last (E) loop
            if E.Table (R).Location.File = Dependencies_On then
               Remove (E, R);

            elsif E.Table (R).Caller /= null
              and then E.Table (R).Caller.Declaration.File = Dependencies_On
            then
               E.Table (R).Caller.Is_Valid := False;
            end if;
         end loop;
         Can_Remove := Can_Remove
           and then Entity_Reference_Arrays.Length (E) = 0;
      end Check_And_Remove;

   begin
      Can_Remove := not Entity.Is_Valid;
      Assert (Assert_Me, Entity.Declaration.File /= Dependencies_On,
              "Entity should have been on .Entities list, not .All_Entities");
      Check_And_Remove (Entity.Caller_At_Declaration);
      Check_And_Remove (Entity.End_Of_Scope);
      Check_And_Remove (Entity.Parent_Types);
      Check_And_Remove (Entity.Pointed_Type);
      Check_And_Remove (Entity.Returned_Type);
      Check_And_Remove (Entity.Primitive_Op_Of);
      Check_And_Remove (Entity.Called_Entities'Access);
      Check_And_Remove (Entity.Rename);
      Check_And_Remove (Entity.Primitive_Subprograms);
      Check_And_Remove (Entity.Child_Types);
      Check_And_Remove (Entity.References);
   end Mark_Dependencies_As_Invalid;

   ------------
   -- Remove --
   ------------

   procedure Remove  (D : in out Entities_Tries.Trie_Tree;
                      E : Entity_Information)
   is
      Pointer : Cell_Pointer;
      EL      : Entity_Information_List_Access;
   begin
      Find_Cell_Child (D, E.Name.all, Pointer);
      EL := Get (Pointer);

      if EL /= null then
         if Length (EL.all) = 1 then
            if EL.Table (Entity_Information_Arrays.First) = E then
               Remove (D, Pointer);
            end if;
         else
            Remove (EL.all, E);
         end if;
      end if;
   end Remove;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Tree : in out Scope_Tree) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Scope_Tree_Node, Scope_Tree);
   begin
      if Tree /= null then
         if Tree.Sibling /= null then
            Destroy (Tree.Sibling);
         end if;

         if Tree.Contents /= null then
            Destroy (Tree.Contents);
         end if;

         if not Active (Debug_Me) then
            Unchecked_Free (Tree);
         else
            Tree := null;
         end if;
      end if;
   end Destroy;

   -----------
   -- Unref --
   -----------

   procedure Unref (LI : in out LI_File) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (LI_File_Record'Class, LI_File);
   begin
      if LI /= null then
         Assert (Assert_Me, LI.Ref_Count > 0, "Too many calls to unref");
         LI.Ref_Count := LI.Ref_Count - 1;
         if LI.Ref_Count = 0 then
            --  Do not remove the file from the htable, since Unref is
            --  called after a removal, and the user shouldn't call Unref
            --  more often than Ref
            for L in Source_File_Arrays.First .. Last (LI.Files) loop
               LI.Files.Table (L).LI := null;
            end loop;

            if not Active (Debug_Me) then
               Free (LI.Files);
               Unchecked_Free (LI);
            else
               LI := null;
            end if;
         end if;
      end if;
   end Unref;

   ---------
   -- Ref --
   ---------

   procedure Ref (LI : LI_File) is
   begin
      if LI /= null then
         LI.Ref_Count := LI.Ref_Count + 1;
      end if;
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Entity : in out Entity_Information) is
      Tmp : String_Access;
   begin
      if Entity /= null then
         Assert (Assert_Me, Entity.Ref_Count > 0,
                 "too many calls to unref for " & Entity.Name.all);
         Entity.Ref_Count := Entity.Ref_Count - 1;
         if Entity.Ref_Count = 0 then
            --  Temporarily fool the system, otherwise we cannot remove the
            --  entity from the trie because of a call to Get_Name
            Entity.Ref_Count := 1;
            Remove (Entity.Declaration.File.Entities, Entity);
            Entity.Ref_Count := 0;
            Isolate (Entity, Clear_References => True);

            --  If we are debugging, we do not free the memory, to keep a
            --  debuggable structure.
            if Active (Debug_Me) then
               Tmp := Entity.Name;
               Entity.Name := new String'
                 (Entity.Name.all
                  & ':' & Base_Name (Entity.Declaration.File.Name));
               Free (Tmp);
               --  Trace (Debug_Me, "Freeing entity " & Entity.Name.all);
            else
               Free (Entity.Name);
               Unchecked_Free (Entity);
            end if;
         end if;

         --  This need to be always done, since the caller can no longer depend
         --  on the entity after doing that.
         Entity := null;
      end if;
   end Unref;

   ---------
   -- Ref --
   ---------

   procedure Ref (Entity : Entity_Information) is
   begin
      if Entity /= null then
         Entity.Ref_Count := Entity.Ref_Count + 1;
      end if;
   end Ref;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (File : Source_File) return VFS.Virtual_File is
   begin
      Assert (Assert_Me, File /= null, "Null source file in Get_Filename");
      return File.Name;
   end Get_Filename;

   ---------------------
   -- Get_LI_Filename --
   ---------------------

   function Get_LI_Filename (LI : LI_File) return VFS.Virtual_File is
   begin
      Assert (Assert_Me, LI /= null, "Null LI in Get_LI_Filename");
      return LI.Name;
   end Get_LI_Filename;

   ------------
   -- Create --
   ------------

   function Create
     (Registry : Projects.Registry.Project_Registry_Access)
      return Entities_Database
   is
      Db : Entities_Database;
   begin
      Db          := new Entities_Database_Record;
      Db.Registry := Registry;
      return Db;
   end Create;

   -------------------------------
   -- Register_Language_Handler --
   -------------------------------

   procedure Register_Language_Handler
     (Db   : Entities_Database;
      Lang : access Language_Handlers.Language_Handler_Record'Class) is
   begin
      Db.Lang := Language_Handlers.Language_Handler (Lang);
   end Register_Language_Handler;

   ----------
   -- Hash --
   ----------

   function Hash (Key : VFS.Cst_UTF8_String_Access) return HTable_Header is
   begin
      if Filenames_Are_Case_Sensitive then
         return String_Hash (Key.all);
      else
         return String_Hash (To_Lower (Key.all));
      end if;
   end Hash;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next (E : Source_File_Item; Next : Source_File_Item) is
   begin
      E.Next := Next;
   end Set_Next;

   ----------
   -- Next --
   ----------

   function Next (E : Source_File_Item) return Source_File_Item is
   begin
      return E.Next;
   end Next;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (E : Source_File_Item) return VFS.Cst_UTF8_String_Access is
   begin
      return Full_Name (E.File.Name);
   end Get_Key;

   -----------
   -- Equal --
   -----------

   function Equal (K1, K2 : VFS.Cst_UTF8_String_Access) return Boolean is
   begin
      if Filenames_Are_Case_Sensitive then
         return K1 = K2 or else K1.all = K2.all;
      else
         return K1 = K2 or else To_Lower (K1.all) = To_Lower (K2.all);
      end if;
   end Equal;

   ----------
   -- Free --
   ----------

   procedure Free (E : in out Source_File_Item) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Source_File_Item_Record, Source_File_Item);
   begin
      Unref (E.File);
      if not Active (Debug_Me) then
         Unchecked_Free (E);
      else
         E := null;
      end if;
   end Free;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next (E : LI_File_Item; Next : LI_File_Item) is
   begin
      E.Next := Next;
   end Set_Next;

   ----------
   -- Next --
   ----------

   function Next (E : LI_File_Item) return LI_File_Item is
   begin
      return E.Next;
   end Next;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (E : LI_File_Item) return VFS.Cst_UTF8_String_Access is
   begin
      return Full_Name (E.File.Name);
   end Get_Key;

   ----------
   -- Free --
   ----------

   procedure Free (E : in out LI_File_Item) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (LI_File_Item_Record, LI_File_Item);
   begin
      Unref (E.File);

      if not Active (Debug_Me) then
         Unchecked_Free (E);
      else
         E := null;
      end if;
   end Free;

   -----------
   -- Reset --
   -----------

   procedure Reset (Db : Entities_Database) is
      procedure Fast_Reset (File : Source_File);
      --  Free the memory occupied by File and its entities. No attempt is made
      --  to respect the reference counter of the entities, and therefore all
      --  existing Entity_Information become obsolete.
      --  This must only be called when reseting the database itself

      ----------------
      -- Fast_Reset --
      ----------------

      procedure Fast_Reset (File : Source_File) is
         Iter   : Entities_Tries.Iterator := Start (File.Entities'Access, "");
         EL     : Entity_Information_List_Access;
      begin
         Trace (Assert_Me, "Fast_Reset All_Entities for "
                & Full_Name (Get_Filename (File)).all);
         loop
            EL := Get (Iter);
            exit when EL = null;

            for E in Entity_Information_Arrays.First .. Last (EL.all) loop
               if EL.Table (E) /= null then
                  Free  (EL.Table (E).Parent_Types);
                  Free  (EL.Table (E).Primitive_Subprograms);
                  Free  (EL.Table (E).Child_Types);
                  Free  (EL.Table (E).References);
                  Clear (EL.Table (E).Called_Entities);

                  if not Active (Debug_Me) then
                     Free (EL.Table (E).Name);
                     Unchecked_Free (EL.Table (E));
                  else
                     EL.Table (E) := null;
                  end if;
               end if;
            end loop;

            Next (Iter);
         end loop;
         Free (Iter);

         if not Active (Debug_Me) then
            Clear (File.All_Entities);
            Clear (File.Entities);
            Free (File.Unit_Name);
            Free (File.Depends_On);
            Free (File.Depended_On);
         end if;
      end Fast_Reset;

      Iter : Files_HTable.Iterator;
      File : Source_File_Item;
   begin
      Get_First (Db.Files, Iter);
      loop
         File := Get_Element (Iter);
         exit when File = null;
         Fast_Reset (File.File);
         Get_Next (Db.Files, Iter);
      end loop;

      Reset (Db.LIs);
   end Reset;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Db : in out Entities_Database) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Entities_Database_Record, Entities_Database);
   begin
      Reset (Db);
      if not Active (Debug_Me) then
         Unchecked_Free (Db);
      else
         Db := null;
      end if;
   end Destroy;

   ----------------------------
   -- Internal_Get_Or_Create --
   ----------------------------

   function Internal_Get_Or_Create
     (Db            : Entities_Database;
      Full_Filename : VFS.Cst_UTF8_String_Access;
      File          : VFS.Virtual_File;
      LI            : LI_File := null;
      Timestamp     : Ada.Calendar.Time := VFS.No_Time;
      Allow_Create  : Boolean := True) return Source_File
   is
      S : Source_File_Item := Get (Db.Files, Full_Filename);
      F : Source_File;
   begin
      if S = null and then not Allow_Create then
         null;

      elsif S = null then
         if File = VFS.No_File then
            F := new Source_File_Record'
              (Db             => Db,
               Timestamp      => Timestamp,
               Unit_Name      => null,
               Name           => Create (Full_Filename => Full_Filename.all),
               Entities       => Empty_Trie_Tree,
               Depends_On     => Null_Dependency_List,
               Depended_On    => Null_Dependency_List,
               All_Entities   => Empty_Trie_Tree,
               Scope_Tree_Computed => False,
               LI             => LI,
               Ref_Count      => 1);
         else
            F := new Source_File_Record'
              (Db             => Db,
               Timestamp      => Timestamp,
               Unit_Name      => null,
               Name           => File,
               Entities       => Empty_Trie_Tree,
               Depends_On     => Null_Dependency_List,
               Depended_On    => Null_Dependency_List,
               All_Entities   => Empty_Trie_Tree,
               Scope_Tree_Computed => False,
               LI             => LI,
               Ref_Count      => 1);
         end if;

         S := new Source_File_Item_Record'(File => F, Next => null);
         Set (Db.Files, S);

         if LI /= null then
            Append (LI.Files, S.File);
         end if;

      else
         if Timestamp /= No_Time then
            S.File.Timestamp := Timestamp;
         end if;

         if LI /= null and then S.File.LI = null then
            S.File.LI := LI;
            Append (LI.Files, S.File);
         end if;
      end if;

      if S = null then
         return null;
      else
         return S.File;
      end if;
   end Internal_Get_Or_Create;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Db           : Entities_Database;
      File         : VFS.Virtual_File;
      LI           : LI_File := null;
      Timestamp    : Ada.Calendar.Time := VFS.No_Time;
      Allow_Create : Boolean := True) return Source_File is
   begin
      return Internal_Get_Or_Create
        (Db, Full_Name (File), File, LI, Timestamp, Allow_Create);
   end Get_Or_Create;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Db            : Entities_Database;
      Full_Filename : String;
      LI            : LI_File := null;
      Timestamp     : Ada.Calendar.Time := VFS.No_Time;
      Allow_Create  : Boolean := True) return Source_File is
   begin
      if Is_Absolute_Path (Full_Filename) then
         return Internal_Get_Or_Create
           (Db, Full_Filename'Unrestricted_Access,
            VFS.No_File, LI, Timestamp, Allow_Create);

      else
         Get_Full_Path_From_File
           (Db.Registry.all,
            Full_Filename,
            Use_Source_Path => True,
            Use_Object_Path => False);

         if Name_Len /= 0 then
            return Internal_Get_Or_Create
              (Db, Name_Buffer (1 .. Name_Len)'Unrestricted_Access,
               VFS.No_File, LI, Timestamp, Allow_Create);
         else
            declare
               Str : aliased constant String := Normalize_Pathname
                 (Full_Filename, Resolve_Links => False);
            begin
               return Internal_Get_Or_Create
                 (Db, Str'Unrestricted_Access,
                  VFS.No_File, LI, Timestamp, Allow_Create);
            end;
         end if;
      end if;
   end Get_Or_Create;

   ----------
   -- Find --
   ----------

   function Find
     (List   : Entity_Information_List;
      Loc    : File_Location) return Entity_Information is
   begin
      for L in Entity_Information_Arrays.First .. Last (List) loop
         if List.Table (L).Declaration.File = Loc.File
           and then List.Table (L).Declaration.Line = Loc.Line
           and then List.Table (L).Declaration.Column = Loc.Column
         then
            return List.Table (L);
         end if;
      end loop;
      return null;
   end Find;

   --------------------
   -- Add_Depends_On --
   --------------------

   procedure Add_Depends_On
     (File                : Source_File;
      Depends_On          : Source_File;
      Explicit_Dependency : Boolean := False)
   is
      Index : Dependency_Arrays.Index_Type;
   begin
      Assert (Assert_Me, Depends_On /= File, "File cannot depend on itself");

      Index := Find (File.Depends_On, Depends_On);

      if Index < Dependency_Arrays.First then
         Append (File.Depends_On, (Depends_On, Explicit_Dependency));
         Append (Depends_On.Depended_On, (File, Explicit_Dependency));

      elsif Explicit_Dependency then
         --  This is only added when File itself is actually parse, so be sure
         --  to reflect the dependency status
         File.Depends_On.Table (Index).Explicit := Explicit_Dependency;

         Index := Find (Depends_On.Depended_On, File);
         if Index < Dependency_Arrays.First then
            Append (Depends_On.Depended_On, (File, Explicit_Dependency));
         else
            Depends_On.Depended_On.Table (Index).Explicit :=
              Explicit_Dependency;
         end if;
      end if;
   end Add_Depends_On;

   ---------
   -- Add --
   ---------

   procedure Add
     (Entities         : in out Entities_Tries.Trie_Tree;
      Entity           : Entity_Information;
      Check_Duplicates : Boolean)
   is
      Pointer : Cell_Pointer;
      EL      : Entity_Information_List_Access;
   begin
      Find_Cell_Child (Entities, Entity.Name.all, Pointer);
      EL := Get (Pointer);

      if EL = null then
         EL := new Entity_Information_List'(Null_Entity_Information_List);
         Append (EL.all, Entity);

         Insert (Entity.Name.all, Pointer, EL);
         Trace (Ref_Me, "Ref " & Entity.Name.all
                & " from Add");
         Ref (Entity);

      elsif not Check_Duplicates
        or else Find (EL.all, Entity.Declaration) = null
      then
         Append (EL.all, Entity);
         Trace (Ref_Me, "Ref " & Entity.Name.all
                & " from Add");
         Ref (Entity);
      end if;

   end Add;

   --------------------
   -- Set_Time_Stamp --
   --------------------

   procedure Set_Time_Stamp
     (LI : LI_File; Timestamp : Ada.Calendar.Time := VFS.No_Time) is
   begin
      LI.Timestamp := Timestamp;
   end Set_Time_Stamp;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Db        : Entities_Database;
      File      : VFS.Virtual_File;
      Project   : Projects.Project_Type) return LI_File
   is
      L : LI_File_Item := Get (Db.LIs, Full_Name (File));
   begin
      Assert (Assert_Me, File /= VFS.No_File, "No LI filename");
      Assert (Assert_Me, Project /= No_Project, "No project specified");
      if L = null then
         L := new LI_File_Item_Record'
           (File => new LI_File_Record'
              (Db        => Db,
               Name      => File,
               Timestamp => No_Time,
               Project   => Project,
               Files     => Null_Source_File_List,
               Ref_Count => 1),
            Next => null);
         Set (Db.LIs, L);
      end if;

      return L.File;
   end Get_Or_Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (D : in out Entity_Information_List_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Entity_Information_List, Entity_Information_List_Access);
   begin
      if D /= null then
         Free (D.all);

         if not Active (Debug_Me) then
            Unchecked_Free (D);
         else
            D := null;
         end if;
      end if;
   end Destroy;

   --------------
   -- Set_Kind --
   --------------

   procedure Set_Kind (Entity : Entity_Information; Kind : E_Kind) is
   begin
      Entity.Kind := Kind;
   end Set_Kind;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
     (Entity     : Entity_Information;
      Attributes : Entity_Attributes) is
   begin
      Entity.Attributes := Attributes;
   end Set_Attributes;

   ----------------------
   -- Set_End_Of_Scope --
   ----------------------

   procedure Set_End_Of_Scope
     (Entity   : Entity_Information;
      Location : File_Location;
      Kind     : Reference_Kind)
   is
   begin
      Assert (Assert_Me, Location.File /= null, "Invalid End_Of_Scope");

      --  In End_Of_Scope is currently the spec, save this in the standard list
      --  of references
      if Entity.End_Of_Scope /= No_E_Reference
        and then (Entity.End_Of_Scope.Location /= Location
                  or else Entity.End_Of_Scope.Kind /= Kind)
      then
         if Entity.End_Of_Scope.Kind = End_Of_Spec then
            Add_Reference
              (Entity,
               Location => Entity.End_Of_Scope.Location,
               Kind     => Entity.End_Of_Scope.Kind);
            Entity.End_Of_Scope := (Location, null, Kind);
         else
            Add_Reference
              (Entity,
               Location => Location,
               Kind     => Kind);
         end if;
      else
         Entity.End_Of_Scope := (Location, null, Kind);
      end if;
   end Set_End_Of_Scope;

   ----------------------
   -- Get_End_Of_Scope --
   ----------------------

   procedure Get_End_Of_Scope
     (Entity   : Entity_Information;
      Location : out File_Location;
      Kind     : out Reference_Kind) is
   begin
      Location := Entity.End_Of_Scope.Location;
      Kind     := Entity.End_Of_Scope.Kind;
   end Get_End_Of_Scope;

   ------------------------
   -- Set_Is_Renaming_Of --
   ------------------------

   procedure Set_Is_Renaming_Of
     (Entity : Entity_Information; Renaming_Of : Entity_Information) is
   begin
      Assert (Assert_Me, Renaming_Of /= null, "Invalid renamed entity");
      Entity.Rename := Renaming_Of;
      Trace (Ref_Me, "Ref " & Renaming_Of.Name.all
             & " from Set_Is_Renaming_Of");
      Ref (Renaming_Of);
   end Set_Is_Renaming_Of;

   -------------------
   -- Add_Reference --
   -------------------

   procedure Add_Reference
     (Entity   : Entity_Information;
      Location : File_Location;
      Kind     : Reference_Kind) is
   begin
      Assert (Assert_Me, Location.File /= null, "Invalid file in reference");

      if Active (Ref_Me) then
         Trace (Ref_Me, "Adding reference for " & Entity.Name.all
                & " in " & Full_Name (Get_Filename (Location.File)).all
                & Location.Line'Img & Location.Column'Img);
      end if;

      if Active (Add_Reference_Force_Unique) then
         for R in
           Entity_Reference_Arrays.First .. Last (Entity.References)
         loop
            if Entity.References.Table (R).Location = Location then
               return;
            end if;
         end loop;
      end if;

      Append (Entity.References, (Location, null, Kind));
      Add_All_Entities (Location.File, Entity);
   end Add_Reference;

   -----------------
   -- Set_Type_Of --
   -----------------

   procedure Set_Type_Of
     (Entity     : Entity_Information;
      Is_Of_Type : Entity_Information;
      Is_Subtype : Boolean := False)
   is
      pragma Unreferenced (Is_Subtype);
   begin
      Assert (Assert_Me, Is_Of_Type /= null, "Invalid type for entity");
      Append (Entity.Parent_Types, Is_Of_Type);
      Trace (Ref_Me, "Ref " & Is_Of_Type.Name.all & " from Is_Of_Type");
      Ref (Is_Of_Type);

      if Entity.Kind.Is_Type then
         Append (Is_Of_Type.Child_Types, Entity);
         Trace (Ref_Me, "Ref " & Entity.Name.all
                & " from Set_Type_Of.Child_Types");
         Ref (Entity);
      end if;
   end Set_Type_Of;

   -----------------
   -- Get_Type_Of --
   -----------------

   function Get_Type_Of
     (Entity : Entity_Information) return Entity_Information is
   begin
      if Length (Entity.Parent_Types) /= 0 then
         return Entity.Parent_Types.Table (Entity_Information_Arrays.First);
      else
         return null;
      end if;
   end Get_Type_Of;

   ------------------------------
   -- Add_Primitive_Subprogram --
   ------------------------------

   procedure Add_Primitive_Subprogram
     (Entity : Entity_Information; Primitive : Entity_Information) is
   begin
      Assert (Assert_Me, Primitive /= null, "Invalid primitive subprogram");
      Append (Entity.Primitive_Subprograms, Primitive);
      Primitive.Primitive_Op_Of := Entity;
      Trace (Ref_Me, "Ref " & Primitive.Name.all
             & " from Add_Primitive_Subprogram");
      Ref (Primitive);
   end Add_Primitive_Subprogram;

   ----------------------
   -- Set_Pointed_Type --
   ----------------------

   procedure Set_Pointed_Type
     (Entity : Entity_Information; Points_To : Entity_Information) is
   begin
      Assert (Assert_Me, Points_To /= null, "Invalid pointed type");
      Entity.Pointed_Type := Points_To;
      Trace (Ref_Me, "Ref " & Points_To.Name.all
             & " from Set_Pointed_Type");
      Ref (Points_To);
   end Set_Pointed_Type;

   -----------------------
   -- Set_Returned_Type --
   -----------------------

   procedure Set_Returned_Type
     (Entity : Entity_Information; Returns : Entity_Information) is
   begin
      Assert (Assert_Me, Returns /= null, "Invalid returned type");
      Entity.Returned_Type := Returns;
      Trace (Ref_Me, "Ref " & Returns.Name.all
             & " from Set_Returned_Type");
      Ref (Returns);
   end Set_Returned_Type;

   -----------------------
   -- Get_Returned_Type --
   -----------------------

   function Get_Returned_Type
     (Entity : Entity_Information) return Entity_Information is
   begin
      return Entity.Returned_Type;
   end Get_Returned_Type;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Name         : String;
      File         : Source_File;
      Line         : Natural;
      Column       : Natural;
      Allow_Create : Boolean := True) return Entity_Information
   is
      EL  : Entity_Information_List_Access;
      E   : Entity_Information;
      Pointer : Cell_Pointer;
   begin
      Assert (Assert_Me, Name /= "", "No name specified for Get_Or_Create");

      Find_Cell_Child (File.Entities, Name, Pointer);
      EL := Get (Pointer);

      if EL = null then
         if Allow_Create then
            EL := new Entity_Information_List'
              (Null_Entity_Information_List);
            Insert (Name, Pointer, EL);
         end if;
      else
         E := Find (EL.all, (File, Line, Column_Type (Column)));
      end if;

      if E = null and then Allow_Create then
         E := new Entity_Information_Record'
           (Name                  => new String'(Name),
            Kind                  => Unresolved_Entity_Kind,
            Attributes            => (others => False),
            Declaration           => (File, Line, Column_Type (Column)),
            Caller_At_Declaration => null,
            End_Of_Scope          => No_E_Reference,
            Parent_Types          => Null_Entity_Information_List,
            Pointed_Type          => null,
            Returned_Type         => null,
            Primitive_Op_Of       => null,
            Rename                => null,
            Called_Entities       => Empty_Trie_Tree,
            Primitive_Subprograms => Null_Entity_Information_List,
            Child_Types           => Null_Entity_Information_List,
            References            => Null_Entity_Reference_List,
            Is_Valid              => True,
            Ref_Count             => 1);

         Append (EL.all, E);

      elsif E /= null then
         E.Is_Valid := True;
      end if;

      return E;
   end Get_Or_Create;

   ------------------
   -- Get_Database --
   ------------------

   function Get_Database (File : Source_File) return Entities_Database is
   begin
      return File.Db;
   end Get_Database;

   function Get_Database (LI : LI_File) return Entities_Database is
   begin
      return LI.Db;
   end Get_Database;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project (LI : LI_File) return Projects.Project_Type is
   begin
      return LI.Project;
   end Get_Project;

   -------------------------
   -- Get_Predefined_File --
   -------------------------

   function Get_Predefined_File (Db : Entities_Database) return Source_File is
   begin
      if Db.Predefined_File = null then
         Db.Predefined_File := Get_Or_Create
           (Db, Create_From_Base ("<predefined>"), null);
      end if;
      return Db.Predefined_File;
   end Get_Predefined_File;

   -------------------
   -- Is_Subprogram --
   -------------------

   function Is_Subprogram (Entity : Entity_Information) return Boolean is
   begin
      return Is_Subprogram_Entity (Entity.Kind.Kind);
   end Is_Subprogram;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Handler : in out LI_Handler_Record) is
      pragma Unreferenced (Handler);
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Handler : in out LI_Handler) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (LI_Handler_Record'Class, LI_Handler);
   begin
      Destroy (Handler.all);
      Unchecked_Free (Handler);
   end Destroy;

   ------------
   -- Get_LI --
   ------------

   function Get_LI (File : Source_File) return LI_File is
   begin
      return File.LI;
   end Get_LI;

   --------------------
   -- Get_LI_Handler --
   --------------------

   function Get_LI_Handler
     (Db              : Entities_Database;
      Source_Filename : VFS.Virtual_File) return LI_Handler is
   begin
      return Get_LI_Handler_From_File
        (GPS_Language_Handler (Db.Lang), Source_Filename);
   end Get_LI_Handler;

   -------------------
   -- Get_Timestamp --
   -------------------

   function Get_Timestamp (LI : LI_File) return Ada.Calendar.Time is
   begin
      return LI.Timestamp;
   end Get_Timestamp;

   -----------------
   -- Update_Xref --
   -----------------

   procedure Update_Xref
     (File                  : Source_File;
      File_Has_No_LI_Report : File_Error_Reporter := null)
   is
      F : Source_File;
      pragma Unreferenced (F);
      H : LI_Handler;
   begin
      if File /= null then
         H := Get_LI_Handler (File.Db, File.Name);

         if H /= null then
            F := Get_Source_Info
              (H, File.Name, File_Has_No_LI_Report);
         end if;
      end if;
   end Update_Xref;

   -------------------
   -- Is_Up_To_Date --
   -------------------

   function Is_Up_To_Date (File : Source_File) return Boolean is
      From_Disk : constant Time := File_Time_Stamp (File.Name);
   begin
      if Active (Assert_Me) then
         Trace (Assert_Me, "Is_Up_To_Date: "
                & Base_Name (Get_Filename (File))
                & " file time:" & Image (From_Disk, "%D-%T")
                & " memory: " & Image (File.Timestamp, "%D-%T"));
      end if;
      return From_Disk = File.Timestamp;
   end Is_Up_To_Date;

   ------------------------
   -- Get_Declaration_Of --
   ------------------------

   function Get_Declaration_Of
     (Entity : Entity_Information) return File_Location is
   begin
      return Entity.Declaration;
   end Get_Declaration_Of;

   --------------
   -- Get_File --
   --------------

   function Get_File (Loc : File_Location) return Source_File is
   begin
      return Loc.File;
   end Get_File;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Loc : File_Location) return Natural is
   begin
      return Loc.Line;
   end Get_Line;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column (Loc : File_Location) return Natural is
   begin
      return Natural (Loc.Column);
   end Get_Column;

   -------------------------
   -- Check_LI_And_Source --
   -------------------------

   function Check_LI_And_Source
     (LI : LI_File; Source : Virtual_File) return Boolean is
   begin
      for F in Source_File_Arrays.First .. Last (LI.Files) loop
         if LI.Files.Table (F).Name = Source then
            return True;
         end if;
      end loop;

      return False;
   end Check_LI_And_Source;

   ----------
   -- Free --
   ----------

   procedure Free (LI : in out LI_Handler_Iterator_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (LI_Handler_Iterator'Class, LI_Handler_Iterator_Access);
   begin
      if LI /= null then
         Destroy (LI.all);
         Unchecked_Free (LI);
      end if;
   end Free;

   --------------------
   -- Get_Time_Stamp --
   --------------------

   function Get_Time_Stamp (File : Source_File) return Ada.Calendar.Time is
   begin
      return File.Timestamp;
   end Get_Time_Stamp;

   --------------------
   -- Set_Time_Stamp --
   --------------------

   procedure Set_Time_Stamp
     (File : Source_File; Timestamp : Ada.Calendar.Time) is
   begin
      if Timestamp = VFS.No_Time then
         File.Timestamp := File_Time_Stamp (File.Name);
      else
         File.Timestamp := Timestamp;
      end if;
   end Set_Time_Stamp;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind (Entity : Entity_Information) return E_Kind is
   begin
      return Entity.Kind;
   end Get_Kind;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (Ref : Entity_Reference) return File_Location is
   begin
      if Ref.Entity /= null
        and then Ref.Entity.References /= Null_Entity_Reference_List
        and then Ref.Index <= Last (Ref.Entity.References)
      then
         return Ref.Entity.References.Table (Ref.Index).Location;

      elsif Ref.Index = Entity_Reference_Arrays.Index_Type'Last then
         return Ref.Entity.Declaration;

      else
         return No_File_Location;
      end if;
   end Get_Location;

   --------------------------
   -- Is_Predefined_Entity --
   --------------------------

   function Is_Predefined_Entity
     (Entity : Entity_Information) return Boolean is
   begin
      return Entity /= null
        and then Entity.Declaration.File =
          Get_Predefined_File (Entity.Declaration.File.Db);
   end Is_Predefined_Entity;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind (Ref : Entity_Reference) return Reference_Kind is
   begin
      if Ref.Entity /= null
        and then Ref.Entity.References /= Null_Entity_Reference_List
        and then Ref.Index <= Last (Ref.Entity.References)
      then
         return Ref.Entity.References.Table (Ref.Index).Kind;

      elsif Ref.Index = Entity_Reference_Arrays.Index_Type'Last then
         return Declaration;

      else
         return Reference;
      end if;
   end Get_Kind;

   --------------------
   -- Kind_To_String --
   --------------------

   function Kind_To_String (Kind : E_Kind) return String is
      function Get_Value (Typ, Obj : String) return String;
      --  Return the appropriate string, depending on the properties of Kind
      --  (generic type, generic object, type, object)

      function Get_Value (Typ, Obj : String) return String is
      begin
         if Kind.Is_Type then
            if Kind.Is_Generic then
               return "generic " & Typ;
            else
               return Typ;
            end if;
         elsif Kind.Is_Generic then
            return "generic " & Obj;
         else
            return Obj;
         end if;
      end Get_Value;

   begin
      --  ??? Would be nice to do it as a primitive subprogram of the
      --  LI_Handlers, unfortunately they currently don't have access to
      --  GPS.Intl for proper translations.

      --  Special comments are put in place so that the script to find
      --  translatable string find these as well

      case Kind.Kind is
         when Overloaded_Entity =>
            return "???";
         when Unresolved_Entity =>
            return "unknown"; --  -"unknown"
         when Access_Kind =>
            return Get_Value ("access type", "pointer");
            --  -"access type"  -"pointer"
         when Array_Kind =>
            return Get_Value ("array type", "array");
            --  -"array type"   -"array"
         when Boolean_Kind =>
            return Get_Value ("boolean type", "boolean");
            --  -"boolean type"  -"boolean"
         when Class_Wide =>
            return Get_Value ("class wide type", "class wide");
            --  -"class wide type"   -"class wide"
         when Class =>
            return Get_Value ("class type", "class instance");
            --  -"class type"    -"class instance"
         when Decimal_Fixed_Point =>
            return Get_Value
              ("decimal fixed point type", "decimal fixed point");
            --  -"decimal fixed point type"   -"decimal fixed point"
         when Entry_Or_Entry_Family =>
            return "entry";   --  -"entry";
         when Enumeration_Literal =>
            return "enumeration literal";
            --  -"enumeration literal"
         when Enumeration_Kind =>
            return Get_Value ("enumeration type", "enumeration");
            --  -"enumeration type"   -"enumeration"
         when Exception_Entity =>
            return "exception";  --  -"exception"
         when Floating_Point =>
            return Get_Value ("floating point type", "floating point");
            --  -"floating point type"  -"floating point"
         when Function_Or_Operator =>
            return Get_Value ("function", "function"); --  -"function"
         when Package_Kind =>
            return Get_Value ("package", "package");  --  -"package"
         when Procedure_Kind =>
            return Get_Value ("procedure", "procedure");  --  -"procedure"
         when Label_On_Block =>
            return "block label";              --  -"block label"
         when Label_On_Loop =>
            return "loop label";               --  -"loop label"
         when Label_On_Statement =>
            return "statement label";          --  -"statement label"
         when Macro =>
            return "macro";  -- -"macro"
         when Modular_Integer =>
            return Get_Value ("unsigned integer type", "unsigned integer");
            --  -"unsigned integer type"   -"unsigned integer"
         when Named_Number =>
            return "named number"; --  -"named number"
         when Ordinary_Fixed_Point =>
            return Get_Value ("fixed point type", "fixed point");
            --  -"fixed point type"   -"fixed point"
         when Reference =>
            return "reference";  --  -"reference"
         when Private_Type =>
            return "generic formal";
            --  -"generic formal"
         when Protected_Kind =>
            return Get_Value ("protected type", "protected object");
            --  -"protected type"   -"protected object"
         when Record_Kind =>
            return Get_Value ("record type", "record");
            --  -"record type"   -"record"
         when Signed_Integer =>
            return Get_Value ("integer type", "integer");
            --  -"integer type"   -"integer"
         when String_Kind =>
            return Get_Value ("string type", "string");
            --  -"string type"   -"string"
         when Task_Kind =>
            return Get_Value ("task type", "task");
            --  -"task type"   -"task"
         when Union =>
            return "union";   --  -"union"
      end case;
   end Kind_To_String;

   --------------------
   -- Kind_To_String --
   --------------------

   function Kind_To_String (Kind : Reference_Kind) return String is
   begin
      case Kind is
         when Reference               => return -"read reference";
         when Modification            => return -"write reference";
         when Instantiation_Reference => return -"instantiation";
         when Body_Entity             => return -"body";
         when Completion_Of_Private_Or_Incomplete_Type =>
            return -"full declaration";
         when Discriminant            => return -"discriminant";
         when Declaration             => return -"declaration";
         when Type_Extension          => return -"type extension";
         when Implicit                => return -"implicit reference";
         when Primitive_Operation     => return -"primitive operation";
         when Overriding_Primitive_Operation =>
            return -"overriding primitive operation";
         when With_Line               => return -"with line";
         when Label                   => return -"label";
         when Subprogram_In_Parameter => return -"in parameter";
         when Subprogram_Out_Parameter => return -"out parameter";
         when Subprogram_In_Out_Parameter => return -"in out parameter";
         when Subprogram_Access_Parameter => return -"access parameter";
         when Formal_Generic_Parameter   => return -"formal generic parameter";
         when Parent_Package             => return -"parent package";
         when End_Of_Spec                => return -"end of spec";
         when End_Of_Body                => return -"end of body";
      end case;
   end Kind_To_String;

   --------------------------
   -- Attributes_To_String --
   --------------------------

   function Attributes_To_String (Attr : Entity_Attributes) return String is
      Str   : String (1 .. 1024);
      Index : Natural := Str'First;
   begin
      if Attr (Global) then
         Str (Index .. Index + 6) := "global ";
         Index := Index + 7;
      else
         Str (Index .. Index + 5) := "local ";
         Index := Index + 6;
      end if;

      if Attr (Class_Static) or else Attr (Static_Local) then
         Str (Index .. Index + 6) := "static ";
         Index := Index + 7;
      end if;

      if Attr (Protected_Field) then
         Str (Index .. Index + 9) := "protected ";
         Index := Index + 10;
      end if;

      if Attr (Private_Field) then
         Str (Index .. Index + 7) := "private ";
         Index := Index + 8;
      end if;

      if Attr (Virtual) then
         Str (Index .. Index + 7) := "virtual ";
         Index := Index + 8;
      end if;

      if Attr (Abstract_Entity) then
         Str (Index .. Index + 8) := "abstract ";
         Index := Index + 9;
      end if;

      return Str (Str'First .. Index - 2);
   end Attributes_To_String;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes
     (Entity : Entity_Information) return Entity_Attributes is
   begin
      return Entity.Attributes;
   end Get_Attributes;

   ---------------------------
   -- Parse_File_Constructs --
   ---------------------------

   procedure Parse_File_Constructs
     (Handler      : access LI_Handler_Record;
      Languages    : access Language_Handlers.Language_Handler_Record'Class;
      File_Name    : VFS.Virtual_File;
      Result       : out Language.Construct_List)
   is
      pragma Unreferenced (Handler);
      use Language;

      Lang : constant Language.Language_Access :=
        Get_Language_From_File (GPS_Language_Handler (Languages), File_Name);

   begin
      --  Call the language specific syntax analyzer

      Parse_File_Constructs (Lang, File_Name, Result);
   end Parse_File_Constructs;

   ---------
   -- "<" --
   ---------

   function "<" (Ref1, Ref2 : Entity_Reference) return Boolean is
   begin
      return Get_Location (Ref1) < Get_Location (Ref2);
   end "<";

   ---------
   -- "<" --
   ---------

   function "<" (Loc1, Loc2 : File_Location) return Boolean is
   begin
      if Loc1.File /= Loc2.File then
         return Loc1.File.Name < Loc2.File.Name;
      elsif Loc1.Line < Loc2.Line then
         return True;
      elsif Loc1.Line = Loc2.Line
        and then Loc1.Column < Loc2.Column
      then
         return True;
      else
         return False;
      end if;
   end "<";

   ---------
   -- "<" --
   ---------

   function "<" (Entity1, Entity2 : Entity_Information) return Boolean is
   begin
      return Entity1.Name.all < Entity2.Name.all;
   end "<";

   ------------------------------
   -- Declaration_As_Reference --
   ------------------------------

   function Declaration_As_Reference
     (Entity : Entity_Information) return Entity_Reference
   is
   begin
      return (Entity, Entity_Reference_Arrays.Index_Type'Last);
   end Declaration_As_Reference;

   -------------------
   -- Set_Unit_Name --
   -------------------

   procedure Set_Unit_Name (File : Source_File; Name : String) is
   begin
      Free (File.Unit_Name);
      File.Unit_Name := new String'(Name);
   end Set_Unit_Name;

   -------------------
   -- Get_Unit_Name --
   -------------------

   function Get_Unit_Name (File : Source_File) return String is
   begin
      if File.Unit_Name = null then
         return "";
      else
         return File.Unit_Name.all;
      end if;
   end Get_Unit_Name;

   ---------
   -- "<" --
   ---------

   function "<" (Source1, Source2 : Source_File) return Boolean is
   begin
      return Source1.Name < Source2.Name;
   end "<";

   ----------
   -- Sort --
   ----------

   procedure Sort
     (Sources  : Source_File_Arrays.Instance;
      Criteria : Source_File_Sort_Criteria)
   is
      First : constant Integer := Integer (Source_File_Arrays.First - 1);
      Tmp   : Source_File;

      procedure Move (From, To : Natural);
      function Lt   (Op1, Op2 : Natural) return Boolean;

      procedure Move (From, To : Natural) is
      begin
         if From = 0 then
            Sources.Table (Source_File_Arrays.Index_Type (To + First)) := Tmp;
         elsif To = 0 then
            Tmp :=
              Sources.Table (Source_File_Arrays.Index_Type (From + First));
         else
            Sources.Table (Source_File_Arrays.Index_Type (To + First)) :=
              Sources.Table (Source_File_Arrays.Index_Type (From + First));
         end if;
      end Move;

      function Lt (Op1, Op2 : Natural) return Boolean is
         S1, S2 : Source_File;
      begin
         if Op1 = 0 then
            S1 := Tmp;
         else
            S1 := Sources.Table (Source_File_Arrays.Index_Type (Op1 + First));
         end if;

         if Op2 = 0 then
            S2 := Tmp;
         else
            S2 := Sources.Table (Source_File_Arrays.Index_Type (Op2 + First));
         end if;

         case Criteria is
            when Source_File_Sort_Criteria'(Full_Name) =>
               return S1 < S2;
            when Source_File_Sort_Criteria'(Base_Name) =>
               return Base_Name (S1.Name) < Base_Name (S2.Name);
            when Source_File_Sort_Criteria'(Unit_Name) =>
               if S1.Unit_Name = null then
                  if S2.Unit_Name = null then
                     return Base_Name (S1.Name) < Base_Name (S2.Name);
                  else
                     return True;
                  end if;
               elsif S2.Unit_Name = null then
                  return False;
               else
                  return S1.Unit_Name.all < S2.Unit_Name.all;
               end if;
         end case;
      end Lt;

      package Sort is new GNAT.Heap_Sort_G (Move, Lt);
   begin
      Sort.Sort (Integer (Last (Sources)) - First);
   end Sort;

   ------------------------------
   -- Get_Documentation_Before --
   ------------------------------

   procedure Get_Documentation_Before
     (Context       : Language_Context;
      Buffer        : String;
      Decl_Index    : Natural;
      Comment_Start : out Natural;
      Comment_End   : out Natural)
   is
      Current : Integer;
   begin

      Current := Decl_Index;
      Skip_Lines (Buffer, -1, Current);
      Comment_Start := Current;
      Skip_To_Current_Comment_Block_Start (Context, Buffer, Comment_Start);

      if Comment_Start /= 0 then
         Comment_End := Line_End (Buffer, Current);
         Trace (Assert_Me,
                "Get_Documentation: Found a comment before the entity,"
                & " from" & Comment_Start'Img & " to" & Comment_End'Img);
      end if;

   end Get_Documentation_Before;

   -----------------------------
   -- Get_Documentation_After --
   -----------------------------

   procedure Get_Documentation_After
     (Context       : Language_Context;
      Buffer        : String;
      Decl_Index    : Natural;
      Comment_Start : out Natural;
      Comment_End   : out Natural) is
   begin
      --  Else look after the comment after the declaration (which is the
      --  first block of comments after the declaration line, and not
      --  separated by a blank line)
      Comment_Start := Decl_Index;
      Skip_To_Next_Comment_Start (Context, Buffer, Comment_Start);

      if Comment_Start /= 0 then
         Comment_End := Comment_Start;
         Skip_To_Current_Comment_Block_End (Context, Buffer, Comment_End);
         Comment_End := Line_End (Buffer, Comment_End);
         Trace (Assert_Me,
                "Get_Documentation: Found a comment after the entity,"
                & " from" & Comment_Start'Img & " to" & Comment_End'Img);
      end if;
   end Get_Documentation_After;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Entity                    : Entity_Information;
      Declaration_File_Contents : String := "") return String
   is
      Buffer           : String_Access :=
        Declaration_File_Contents'Unrestricted_Access;
      Index            : Natural := Declaration_File_Contents'First;
      Declaration_File : constant Virtual_File :=
        Get_Filename (Get_Declaration_Of (Entity).File);
      Lang             : constant Language_Access := Get_Language_From_File
        (GPS_Language_Handler (Entity.Declaration.File.Db.Lang),
         Declaration_File);
      Current, Beginning : Natural;
      Context            : Language_Context_Access;
      Location           : File_Location;
      Must_Free_Buffer   : Boolean := False;

   begin
      if Lang = null then
         Trace (Assert_Me, "Get_Documentation language unknown for "
                & Full_Name (Declaration_File).all);
         return "";
      end if;

      Context := Get_Language_Context (Lang);

      if Declaration_File_Contents = "" then
         Buffer := Read_File (Declaration_File);
         if Buffer = null then
            Trace (Assert_Me, "Get_Documentation, no file found "
                   & Full_Name (Declaration_File).all);
            return "";
         end if;
         Must_Free_Buffer := True;
         Index := Buffer'First;
      end if;

      Skip_Lines (Buffer.all, Get_Declaration_Of (Entity).Line - 1, Index);
      Get_Documentation_Before
        (Context       => Context.all,
         Buffer        => Buffer.all,
         Decl_Index    => Index,
         Comment_Start => Beginning,
         Comment_End   => Current);

      if Beginning = 0 then
         Get_Documentation_After
           (Context       => Context.all,
            Buffer        => Buffer.all,
            Decl_Index    => Index,
            Comment_Start => Beginning,
            Comment_End   => Current);
      end if;

      --  If not found, check the comment just before the body

      if Beginning = 0  then
         if Must_Free_Buffer then
            Free (Buffer);
         end if;

         Find_Next_Body (Entity, Location => Location);

         if Location /= No_File_Location
           and then Location /= Get_Declaration_Of (Entity)
         then
            Buffer := Read_File (Get_Filename (Location.File));

            --  Buffer = null means that reading the file failed (e.g.
            --  the file does not exist or is not readable).

            if Buffer /= null then
               Must_Free_Buffer := True;
               Index := Buffer'First;

               Skip_Lines (Buffer.all, Location.Line - 1, Index);
               Get_Documentation_Before
                 (Context       => Context.all,
                  Buffer        => Buffer.all,
                  Decl_Index    => Index,
                  Comment_Start => Beginning,
                  Comment_End   => Current);
            end if;
         end if;
      end if;

      if Beginning /= 0 then
         declare
            Result : constant String := Comment_Block
              (Lang,
               Buffer (Line_Start (Buffer.all, Beginning) .. Current),
               Comment => False,
               Clean   => True);
         begin
            if Must_Free_Buffer then
               Free (Buffer);
            end if;

            return Result;
         end;
      else
         if Must_Free_Buffer then
            Free (Buffer);
         end if;

         return "";
      end if;
   end Get_Documentation;

end Entities;
