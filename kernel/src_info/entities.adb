------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Calendar;               use Ada.Calendar;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with GNAT.Calendar.Time_IO;      use GNAT.Calendar.Time_IO;
with GNAT.Heap_Sort;             use GNAT.Heap_Sort;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Symbols;           use GNATCOLL.Symbols;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;         use GNATCOLL.VFS_Utils;
with Basic_Types;                use Basic_Types;
with Entities.Debug;             use Entities.Debug;
with GPS.Intl;                   use GPS.Intl;
with Language;                   use Language;
with Language.Tree.Database;     use Language.Tree.Database;
with Language_Handlers;          use Language_Handlers;
with Language_Utils;             use Language_Utils;
with Projects;                   use Projects;
with String_Utils;
with Remote;                     use Remote;

package body Entities is
   Assert_Me : constant Trace_Handle := Create ("Entities.Assert", Off);

   Ref_Me  : constant Trace_Handle := Create ("Entities.Ref", Off);
   --  If active, will trace all calls to Ref/Unref for Entity_Information.
   --  That generates a lot of output!

   Debug_Me  : constant Trace_Handle := Create ("Entities.Debug", Off);
   --  If True, no memory is freed, this makes the structure more robust and
   --  easier to debug, but will of course use more memory.

   Add_Reference_Force_Unique : constant Trace_Handle :=
     Create ("Entities.Force_Unique_Ref", On);
   --  Activate this to force a check that all references are unique. This
   --  will slow down the parsing a little, but might help customers work
   --  around bugs in GPS. This should never be needed however, and is just
   --  provided as a backdoor.
   --  ??? Comment above is obsolete and should be updated
   --  ??? Given that we now have an ordered list of references, this should
   --  probably be removed, or given a different semantic.

   use Entities_Hash;
   use Files_HTable;
   use LI_HTable;
   use Entity_Information_Arrays;
   use Source_File_Arrays;
   use Entity_File_Maps;
   use Entities_In_File_Sets;
   use Dependency_Arrays;
   use Instantiation_Arrays;
   use Entities_Search_Tries;
   use Virtual_File_Indexes;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Entity_Information_Record'Class, Entity_Information);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Entity_Information_List, Entity_Information_List_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Entity_Informations_Record,
      Entity_Informations);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (E_Instantiation_Record, Entity_Instantiation);

   function String_Hash is new String_Utils.Hash (HTable_Header);
   function Hash is new String_Utils.Hash (Header_Num);
   function Case_Insensitive_Hash
      is new String_Utils.Case_Insensitive_Hash (Header_Num);

   function Find
     (E : Dependency_List; File : Source_File)
      return Dependency_Arrays.Index_Type;
   --  Find a specific file in the list of dependencies

   procedure Ref (Entity : Entity_Information; Reason : String);
   procedure Unref (Entity : in out Entity_Information; Reason : String);
   --  Internal version of Unref and Ref, which outputs some debug traces in
   --  addition.
   pragma Inline (Ref, Unref);

   procedure Isolate
     (Entity           : Entity_Information;
      Clear_References : Boolean);
   --  Isolate the entity from the rest of the LI structure. This should be
   --  used when we are removing the entity from the internal tables, but the
   --  user has kept a handle on it, to avoid memory leaks.
   --  The references are cleared only if Clear_References is True.

   procedure Remove
     (D : in out Entities_Hash.Instance;
      E : Entity_Information);
   --  Remove the information for E in the table. The entity is not Unrefed
   --  explicitly, since this call should only happen as a result of Unref

   procedure Add
     (Entities         : in out Entities_Hash.Instance;
      Entity           : Entity_Information;
      Check_Duplicates : Boolean);
   --  Add a new entity, if not already there, to D.
   --  Entity shouldn't have a Shared_Name yet if Name is "".

   procedure Check_Entity_Is_Valid
     (Entity : Entity_Information; Reason : String);
   --  Check whether the entity has not been freed. This should only be used
   --  when Debug_Me is active.

   procedure Unref_All_Entities_In_List
     (List : in out Entity_Information_List; Reason : String);
   --  Unref all the entities in the trie

   procedure Add_All_Entities
     (File : Source_File; Entity : Entity_Information);
   --  Add Entity to the list of All_Entities known in File, if Entity is not
   --  there yet and is not declared in File.

   procedure Mark_And_Isolate_Entities (File : Source_File);
   --  Remove references in file for all entities declared in File or
   --  referenced in it.
   --  The entities declared in File are also marked as valid.

   procedure Unref (Instantiation : in out Entity_Instantiation);
   --  Unref all the entities referenced in Instantiation, and free
   --  Instantiation itself.

   function Internal_Get_Or_Create
     (Db            : Entities_Database;
      Full_Filename : GNATCOLL.VFS.Virtual_File;
      File          : GNATCOLL.VFS.Virtual_File;
      Handler       : access LI_Handler_Record'Class;
      LI            : LI_File := null;
      Allow_Create  : Boolean := True) return Source_File;
   --  Internal version for Get_Or_Create

   Is_Subprogram_Entity : constant array (E_Kinds) of Boolean :=
     (Procedure_Kind        => True,
      Function_Or_Operator  => True,
      Entry_Or_Entry_Family => True,
      Task_Kind             => True,
      Overloaded_Entity     => True,
      --  ??? Should we check that at least one of the possible
      --  completions is a subprogram.
      Function_Macro        => True,
      others                => False);
   --  This table should contain true if the corresponding element is
   --  considered as a subprogram (see Is_Subprogram).

   Is_Container_Array : constant array (E_Kinds) of Boolean :=
     (Procedure_Kind        => True,
      Function_Or_Operator  => True,
      Entry_Or_Entry_Family => True,
      Task_Kind             => True,
      Package_Kind          => True,
      Enumeration_Kind      => True,
      Record_Kind           => True,
      Overloaded_Entity     => True,
      Class                 => True,
      --  ??? Should we check that at least one of the possible
      --  completions is a container.
      others                => False);

   Body_Is_Full_Declaration_Array : constant array (E_Kinds) of Boolean :=
     (Record_Kind      => True,
      Enumeration_Kind => True,
      others           => False);
   --  This table should contain true if the corresponding element is a
   --  container and his body is actually a full declaration.

   Is_End_Reference_Array : constant Reference_Kind_Filter :=
     (End_Of_Spec           => True,
      End_Of_Body           => True,
      others                => False);
   --  True if the matching entity indicates an end-of-scope (end of subprogram
   --  declaration, end of record definition, ...)

   Is_Parameter_Array : constant Reference_Kind_Filter :=
     (Subprogram_In_Parameter     => True,
      Subprogram_In_Out_Parameter => True,
      Subprogram_Out_Parameter    => True,
      Subprogram_Access_Parameter => True,
      others                      => False);

   ---------
   -- Ref --
   ---------

   procedure Ref (Entity : Entity_Information; Reason : String) is
   begin
      if Entity /= null and then Active (Ref_Me) then
         Trace (Ref_Me, "Ref " & Debug_Name (Entity) & " (" & Reason & ")");
      end if;
      Ref (Entity);
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Entity : in out Entity_Information; Reason : String) is
   begin
      if Entity /= null and then Active (Ref_Me) then
         Trace
           (Ref_Me, "Unref " & Debug_Name (Entity)
            & " (" & Reason & ")"
            & Entity.Ref_Count'Img & " " & Boolean'Image (Entity.Is_Valid));
      end if;
      Unref (Entity);
   end Unref;

   ------------------------
   -- Show_In_Call_Graph --
   ------------------------

   function Show_In_Call_Graph
     (Db : Entities_Database; Kind : Reference_Kind) return Boolean
   is
   begin
      return Db.Show_In_Call_Graph_Array (Kind);
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

   ------------------------------
   -- Body_Is_Full_Declaration --
   ------------------------------

   function Body_Is_Full_Declaration (Kind : E_Kinds) return Boolean is
   begin
      return Body_Is_Full_Declaration_Array (Kind);
   end Body_Is_Full_Declaration;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Entity : Entity_Information) return Symbol is
   begin
      if Entity = null then
         return No_Symbol;
      else
         return Entity.Name;
      end if;
   end Get_Name;

   ----------------
   -- Debug_Name --
   ----------------

   function Debug_Name (Entity : Entity_Information) return String is
   begin
      if Entity = null then
         return "<null>";
      else
         return Get (Entity.Name).all
           & ":" & Entity.Live_Declaration.File.Name.Display_Full_Name
           & ":" & Image (Entity.Live_Declaration.Line, Min_Width => 1)
           & ":" & Image
           (Integer (Entity.Live_Declaration.Column), Min_Width => 1);
      end if;
   end Debug_Name;

   ----------
   -- Hash --
   ----------

   function Hash (S : Cased_String) return Header_Num is
   begin
      if S.Case_Sensitive then
         return Hash (Get (S.Str).all);
      else
         return Case_Insensitive_Hash (Get (S.Str).all);
      end if;
   end Hash;

   -----------
   -- Equal --
   -----------

   function Equal (S1, S2 : Cased_String) return Boolean is
   begin
      if S1.Case_Sensitive then
         return S1.Str = S2.Str;
      else
         return Equal
           (Get (S1.Str).all, Get (S2.Str).all, S1.Case_Sensitive);
      end if;
   end Equal;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (D : Entity_Informations) return Cased_String is
   begin
      if D = null then
         return Empty_Cased_String;

      else
         if Active (Assert_Me) then
            Assert
              (Assert_Me,
               D.List.Table (Entity_Information_Arrays.First).Ref_Count /= 0,
               "Entity has been freed: "
               & Debug_Name
                 (D.List.Table (Entity_Information_Arrays.First)));
         end if;

         return
           (Str => D.List.Table
              (Entity_Information_Arrays.First).Name,
            Case_Sensitive => not Case_Insensitive_Identifiers
              (D.List.Table
                 (Entity_Information_Arrays.First).
                  Live_Declaration.File.Handler));
      end if;
   end Get_Name;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (D : in out Entity_Informations) is
   begin
      if D.List /= null then
         Free (D.List.all);
         Unchecked_Free (D.List);
      end if;
      Unchecked_Free (D);
   end Destroy;

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
            --  Do not free the contents of the file though (in fact, since
            --  DB.Files owns a reference to the file, the only way we can
            --  reach 0 here is when going through Free (Source_File_Item),
            --  and in that case we have already reset the contents.
            --  If we were trying to do it here, we would in fact never reach
            --  0, since the entities of the file still own a reference to the
            --  file.

            if Active (Debug_Me) then
               F := null;
            else
               Entities_Hash.Reset (F.Entities);
               Unchecked_Free (F);
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
      if Entity.LI_Declaration.File /= File then
         Add_Depends_On (File, Entity.LI_Declaration.File);
         Add (File.All_Entities, Entity, Check_Duplicates => True);
      end if;
   end Add_All_Entities;

   -----------
   -- Reset --
   -----------

   procedure Reset (LI : LI_File) is
   begin
      if Active (Assert_Me) then
         Trace (Assert_Me, "Reseting LI " & Display_Full_Name (LI.Name));
      end if;

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

   ---------------------
   -- Clear_File_Sets --
   ---------------------

   procedure Clear_File_Sets (Set : in out Entities_In_File_Sets.Set) is
      It   : Entities_In_File_Sets.Cursor := First (Set);
      Copy : E_Reference;
   begin
      while Has_Element (It) loop
         Copy := Element (It);
         Unref (Copy.Caller, "reference.caller");
         Next (It);
      end loop;

      Entities_In_File_Sets.Clear (Set);
   end Clear_File_Sets;

   ----------
   -- Free --
   ----------

   procedure Free (Refs : in out File_With_Refs_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (File_With_Refs, File_With_Refs_Access);
   begin
      Clear_File_Sets (Refs.Refs);
      Unchecked_Free (Refs);
   end Free;

   --------------------
   -- Clear_Ref_List --
   --------------------

   procedure Clear_Ref_List (List : in out Entity_Reference_List) is
      It  : Entity_File_Maps.Cursor := Entity_File_Maps.First (List);
      Tmp : File_With_Refs_Access;
   begin
      while Has_Element (It) loop
         Tmp := Element (It);
         Free (Tmp);
         Next (It);
      end loop;

      Clear (List);
   end Clear_Ref_List;

   -------------------------------
   -- Mark_And_Isolate_Entities --
   -------------------------------

   procedure Mark_And_Isolate_Entities (File : Source_File) is

      procedure Clean_References
        (EL         : Entity_Information_List_Access;
         Mark_Valid : Boolean);
      --  Remove all references in File for the entities in EL
      --  If Mark_Valid is true, then all entities are marked as valid.
      --  Otherwise, their status is not changed.
      --  None of the entity is unref'ed or destroyed

      procedure Clean_References
        (EL         : Entity_Information_List_Access;
         Mark_Valid : Boolean)
      is
         Entity : Entity_Information;
         Cursor : Entity_File_Maps.Cursor;
      begin
         for E in reverse Entity_Information_Arrays.First .. Last (EL.all) loop
            Entity := EL.Table (E);
            if Active (Debug_Me) then
               Check_Entity_Is_Valid
                 (Entity,
                  "Mark_And_Isolate_Entities "
                  & Display_Base_Name (Get_Filename (File)));
            end if;

            Cursor := Entity.References.Find (File.Ordered_Index);
            if Has_Element (Cursor) then
               declare
                  Refs : File_With_Refs_Access := Element (Cursor);
               begin
                  Free (Refs);
                  Entity.References.Delete (Cursor);
                  Entity.File_Timestamp_In_References :=
                    Entity.File_Timestamp_In_References + 1;
               end;
            end if;

            if Mark_Valid then
               Isolate (Entity, Clear_References => False);
            end if;
         end loop;
      end Clean_References;

      Iter   : Entities_Hash.Cursor;
      Iter2  : Entities_Hash.Cursor;
      UEI    : Entity_Informations;
   begin
      Get_First (File.Entities, Iter);
      loop
         UEI := Get_Element (Iter);
         exit when UEI = null;
         Get_Next (File.Entities, Iter);
         Clean_References (UEI.List, True);
      end loop;

      Get_First (File.All_Entities, Iter2);
      loop
         UEI := Get_Element (Iter2);
         exit when UEI = null;
         Get_Next (File.All_Entities, Iter2);
         Clean_References (UEI.List, False);
      end loop;
   end Mark_And_Isolate_Entities;

   -----------
   -- Unref --
   -----------

   procedure Unref (Instantiation : in out Entity_Instantiation) is
      Tmp : Entity_Instantiation;
   begin
      while Instantiation /= null loop
         Unref (Instantiation.Entity);
         Tmp := Instantiation.Generic_Parent;
         Unchecked_Free (Instantiation);
         Instantiation := Tmp;
      end loop;
   end Unref;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : Source_File) is
      use LI_File_Arrays;

      Tmp : Source_File;

   begin
      if Active (Assert_Me) then
         Trace (Assert_Me, "Reseting " & Display_Full_Name (File.Name));
         if Equal (Base_Name (Get_Filename (File)), "atree.ads") then
            Debug.Dump (File, Show_Entities => True, Full => True);
         end if;
      end if;

      Mark_And_Isolate_Entities (File);

      --  We can't reset the Depended_On field, since these other files still
      --  reference File (we haven't broken up the links between entities).
      --  The current file itself might depend on others, in case some entities
      --  had references in other files. But such references do not need to be
      --  in the Depends_On array.
      --
      --  If B depends on A, this means that some entities of B depends on
      --  some entities of A. Therefore we can't remove A until B itself has
      --  been removed, and therefore B should own a reference to A

      for F in Dependency_Arrays.First .. Last (File.Depends_On) loop
         Tmp := File.Depends_On.Table (F).File;
         Remove (Tmp.Depended_On, File);
         Unref (Tmp);  --  ref added in Add_Depends_On
      end loop;

      if not Active (Debug_Me) then
         Free (File.Depends_On);
      else
         File.Depends_On := Null_Dependency_List;
      end if;

      --  Reset the All_Entities list, since this file is no longer referencing
      --  any of the entity
      declare
         Iter   : Entities_Hash.Cursor;
         EL     : Entity_Information_List_Access;
         UEI    : Entity_Informations;
      begin
         Get_First (File.All_Entities, Iter);
         loop
            UEI := Get_Element (Iter);
            exit when UEI = null;
            Get_Next (File.All_Entities, Iter);

            EL := UEI.List;
            for E in reverse Entity_Information_Arrays.First ..
              Last (EL.all)
            loop
               Unref (EL.Table (E), "all_entities");
            end loop;

            Entity_Information_Arrays.Free (UEI.List.all);
            Unchecked_Free (UEI.List);
         end loop;
         Reset (File.All_Entities);
      end;

      --  Clean up .Entities field

      declare
         Iter   : Entities_Hash.Cursor;
         UEI    : Entity_Informations;
         Entity : Entity_Information;
      begin
         Get_First (File.Entities, Iter);
         loop
            UEI := Get_Element (Iter);
            exit when UEI = null;
            Get_Next (File.Entities, Iter);

            for J in reverse
              Entity_Information_Arrays.First .. Last (UEI.List.all)
            loop
               Entity := UEI.List.Table (J);
               --  If the entity was still under control of File
               if Entity.Is_Valid then
                  if Entity.Ref_Count /= 1 then
                     Entity.Is_Valid := False;
                  end if;
                  Unref (Entity, ".entities");
               end if;
            end loop;
         end loop;

         --  File.Entities might still contain references to entities (the one
         --  for which ref_count/=0). They are marked as invalid, but we need
         --  to keep them so that we properly reuse them in case, when we
         --  parse the file again, they still exist. Otherwise, all their
         --  references in other files will be lost
         --  Do not call Entities_Hash.Reset (File.Entities);
      end;

      --  Clean up the list of instantiations

      for J in Instantiation_Arrays.First .. Last (File.Instantiations) loop
         Unref (File.Instantiations.Table (J));
      end loop;
      Free (File.Instantiations);

      --  Clean up various other fields

      File.Scope_Tree_Computed := False;

      if File.LI_Files /= Null_LI_File_List then
         --  The LI may need to be reloaded afterwards if the same file is
         --  integrated again in the project sources (through e.g. a scenario
         --  variable). Reseting the timestamp ensure that the references of
         --  this file will be extracted again.

         for J in LI_File_Arrays.First .. Last (File.LI_Files) loop
            File.LI_Files.Table (J).Timestamp := No_Time;
         end loop;
      end if;

      --  Fields which have not been cleaned (see comments above):
      --     - Depended_On (cleaned "magically" when the other files are Reset)
   end Reset;

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
            Unref (List.Table (J), Reason);
            Remove (List, J);
         end loop;
         Free (List);
      end if;
   end Unref_All_Entities_In_List;

   -------------
   -- Isolate --
   -------------

   procedure Isolate
     (Entity : Entity_Information; Clear_References : Boolean)
   is
      Entity2 : Entity_Information;
   begin
      if Active (Ref_Me) then
         Trace (Ref_Me, "Isolate " & Debug_Name (Entity));
      end if;

      Unref (Entity.Caller_At_Declaration, "caller_at_decl");
      Entity.Caller_At_Declaration := null;
      Entity.End_Of_Scope    := No_E_Reference;
      Unref (Entity.Pointed_Type, "pointed_type");
      Entity.Pointed_Type := null;
      Unref (Entity.Returned_Type, "returned_type");
      Entity.Returned_Type := null;
      Unref (Entity.Primitive_Op_Of, "primitive_op");
      Entity.Primitive_Op_Of := null;
      Unref (Entity.Rename, "rename");
      Entity.Rename := null;
      Unref (Entity.Instantiation_Of, "instantiation_of");
      Entity.Instantiation_Of := null;
      Unref_All_Entities_In_List (Entity.Called_Entities, "called_entities");

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

            for P in reverse Entity_Information_Arrays.First ..
              Last (Entity.Parent_Types.Table (J).Child_Types)
            loop
               Entity2 := Entity.Parent_Types.Table (J).Child_Types.Table (P);
               if Entity2 = Entity then
                  Remove (Entity.Parent_Types.Table (J).Child_Types, P);
                  Unref (Entity2, "Iso.Parent_Types.Child_Types");
                  --  will never actually free Entity
               end if;
            end loop;
         end loop;

         Unref_All_Entities_In_List (Entity.Parent_Types, "parent_types");
      end if;

      Unref_All_Entities_In_List
        (Entity.Primitive_Subprograms, "primitive_subprograms");
      Unref_All_Entities_In_List (Entity.Child_Types, "child_types");

      if Clear_References then
         Clear_Ref_List (Entity.References);
      end if;
   end Isolate;

   ---------------------------
   -- Check_Entity_Is_Valid --
   ---------------------------

   procedure Check_Entity_Is_Valid
     (Entity : Entity_Information; Reason : String) is
   begin
      if Active (Debug_Me) then
         Assert (Debug_Me, Entity /= null, "Referencing null entity ("
                 & Reason & ")");
         Assert (Debug_Me,
                 Entity.Ref_Count /= 0,
                 "Entity should no longer be referenced "
                 & Debug_Name (Entity) & " (" & Reason & ") ");
      end if;
   end Check_Entity_Is_Valid;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (D : in out Entities_Hash.Instance;
      E : Entity_Information)
   is
      Str : constant Cased_String :=
              (Str            => E.Name,
               Case_Sensitive => not Case_Insensitive_Identifiers
                 (E.LI_Declaration.File.Handler));
      UEI : constant Entity_Informations := Get (D, Str);
      EL  : Entity_Information_List_Access;
   begin
      if UEI /= null then
         EL := UEI.List;
         if Length (EL.all) = 1 then
            if EL.Table (Entity_Information_Arrays.First) = E then
               Remove (D, Str);
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

         if Active (Debug_Me) then
            Tree := null;
         else
            Unchecked_Free (Tree);
         end if;
      end if;
   end Destroy;

   -----------
   -- Unref --
   -----------

   procedure Unref (LI : in out LI_File) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (LI_File_Record'Class, LI_File);
      use LI_File_Arrays;

   begin
      if LI /= null then
         Assert (Assert_Me, LI.Ref_Count > 0, "Too many calls to unref");
         LI.Ref_Count := LI.Ref_Count - 1;

         if LI.Ref_Count = 0 then
            --  Do not remove the file from the htable, since Unref is
            --  called after a removal, and the user shouldn't call Unref
            --  more often than Ref
            for Source_File in Source_File_Arrays.First .. Last (LI.Files) loop
               Free (LI.Files.Table (Source_File).LI_Files);
            end loop;

            if Active (Debug_Me) then
               LI := null;
            else
               Free (LI.Files);
               Unchecked_Free (LI);
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
   begin
      if Entity /= null and then Entity.Ref_Count /= Natural'Last then
         if Active (Assert_Me) then
            Assert (Assert_Me, Entity.Ref_Count > 0,
                    "too many calls to unref for "
                    & Debug_Name (Entity));
         end if;

         Entity.Ref_Count := Entity.Ref_Count - 1;
         if Entity.Ref_Count = 0 then
            if Active (Ref_Me) then
               Trace (Ref_Me, "Freeing " & Debug_Name (Entity)
                 & ":"
                 & (+Base_Name
                   (Get_Filename (Entity.Live_Declaration.File)))
                 & Entity.Live_Declaration.Line'Img
                 & " ref_count=" & Entity.Ref_Count'Img);
            end if;

            Isolate (Entity, Clear_References => True);

            --  Temporarily fool the system, otherwise we cannot remove the
            --  entity from the trie because of a call to Get_Name.
            --  This might free Entity.Shared_Name!
            --  ??? Review this comment, sinc we now use a htable instead of
            --  a trie. Also, wouldn't Entity.Ref_Count := 1 be sufficient
            --  and potentially avoid a Constaint_Error if someone temporarily
            --  does a Ref in Remove ?

            Entity.Ref_Count := Natural'Last;

            if not Entity.Is_Dummy then
               Remove (Entity.LI_Declaration.File.Entities, Entity);
            end if;

            if Entity.Trie_Tree_Index /=
              Entities_Search_Tries.Null_Vector_Trie_Index
            then
               Entities_Search_Tries.Delete
                 (Entity.LI_Declaration.File.Handler.Name_Index,
                  Entity.Trie_Tree_Index);
            end if;

            Unref (Entity.LI_Declaration.File); --  ref added in Get_Or_Create

            Entity.Ref_Count := 0;

            if Active (Debug_Me) then
               null;
            else
               Unchecked_Free (Entity);
            end if;

            --  Only reset to null when the entity is indeed no longer valid.

            Entity := null;
         end if;
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

   function Get_Filename (File : Source_File) return Virtual_File is
   begin
      Assert (Assert_Me, File /= null, "Null source file in Get_Filename");
      return File.Name;
   end Get_Filename;

   ---------------------
   -- Get_LI_Filename --
   ---------------------

   function Get_LI_Filename (LI : LI_File) return GNATCOLL.VFS.Virtual_File is
   begin
      Assert (Assert_Me, LI /= null, "Null LI in Get_LI_Filename");
      return LI.Name;
   end Get_LI_Filename;

   ------------
   -- Create --
   ------------

   function Create
     (Registry     : Project_Registry_Access;
      Construct_Db : Language.Tree.Database.Construct_Database_Access;
      Normal_Ref_In_Call_Graph : Boolean := False) return Entities_Database
   is
      Db : Entities_Database;
   begin
      Db          := new Entities_Database_Record;
      Db.Registry := Registry;
      Db.Frozen   := Create_And_Update;
      Db.FS_Optimizer := Create;
      Db.Construct_Db := Construct_Db;
      Db.Show_In_Call_Graph_Array (Reference) := Normal_Ref_In_Call_Graph;

      return Db;
   end Create;

   ----------------------------------
   -- Set_Normal_Ref_In_Call_Graph --
   ----------------------------------

   procedure Set_Normal_Ref_In_Call_Graph
     (Db : Entities_Database; Val : Boolean) is
   begin
      Db.Show_In_Call_Graph_Array (Reference) := Val;
   end Set_Normal_Ref_In_Call_Graph;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Lock : in out Construct_Heuristics_Lock) is
   begin
      Unlock_Construct_Heuristics (Lock);
   end Finalize;

   -------------------------------
   -- Lock_Construct_Heuristics --
   -------------------------------

   function Lock_Construct_Heuristics
     (Db : Entities_Database)
      return Construct_Heuristics_Lock
   is
   begin
      Db.Construct_Db_Locks := Db.Construct_Db_Locks + 1;

      return (Ada.Finalization.Limited_Controlled with
              Previous_Level => Db.Construct_Db_Locks - 1, Db => Db);
   end Lock_Construct_Heuristics;

   ---------------------------------
   -- Unlock_Construct_Heuristics --
   ---------------------------------

   procedure Unlock_Construct_Heuristics
     (Lock : in out Construct_Heuristics_Lock)
   is
   begin
      if Lock.Db /= null then
         Lock.Db.Construct_Db_Locks := Lock.Previous_Level;
         Lock.Db := null;
      end if;
   end Unlock_Construct_Heuristics;

   ------------
   -- Freeze --
   ------------

   procedure Freeze
     (Db : Entities_Database; Mode : Freeze_Type := No_Create_Or_Update) is
   begin
      if Active (Assert_Me) then
         Increase_Indent
           (Assert_Me, "Freeze database " & Mode'Img
            & " count =" & Db.Count'Img);
      end if;

      Db.Count := Db.Count + 1;
      Freeze_Stack.Push (Db.Stack, Db.Frozen);

      --  If the Db is completely frozen, we do not want to go back to a
      --  "less frozen" mode.

      if Db.Frozen < Freeze_Type'Last then
         Db.Frozen := Mode;
      end if;
   end Freeze;

   ----------
   -- Thaw --
   ----------

   procedure Thaw (Db : Entities_Database) is
   begin
      Freeze_Stack.Pop (Db.Stack, Db.Frozen);
      Db.Count := Db.Count - 1;

      if Active (Assert_Me) then
         Decrease_Indent (Assert_Me, "Thaw database " & Db.Frozen'Img
                          & " count =" & Db.Count'Img);
      end if;
   end Thaw;

   ------------
   -- Frozen --
   ------------

   function Frozen (Db : Entities_Database) return Freeze_Type is
   begin
      return Db.Frozen;
   end Frozen;

   -------------------------------
   -- Register_Language_Handler --
   -------------------------------

   procedure Register_Language_Handler
     (Db   : Entities_Database;
      Lang : access Abstract_Language_Handler_Record'Class) is
   begin
      Db.Lang := Abstract_Language_Handler (Lang);
   end Register_Language_Handler;

   ----------
   -- Hash --
   ----------

   function Hash (Key : GNATCOLL.VFS.Virtual_File) return HTable_Header is
   begin
      if Is_Case_Sensitive (Get_Nickname (Build_Server)) then
         return String_Hash (+Key.Full_Name);
      else
         return String_Hash (To_Lower (+Key.Full_Name));
      end if;
   end Hash;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next (E, Next : Entity_Informations) is
   begin
      E.Next := Next;
   end Set_Next;

   ----------
   -- Next --
   ----------

   function Next (E : Entity_Informations)
                  return Entity_Informations is
   begin
      return E.Next;
   end Next;

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

   function Get_Key (E : Source_File_Item) return Virtual_File is
   begin
      return E.File.Name;
   end Get_Key;

   ----------
   -- Free --
   ----------

   procedure Free (E : in out Source_File_Item) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Source_File_Item_Record, Source_File_Item);
   begin
      --  Reset the contents of the file (entities declared or referenced,
      --  file dependencies,...), so that we break reference cycles (the file
      --  owns a ref to its entities, which in turn (through their
      --  declaration's file own a reference to the file).

      Reset (E.File);

      --  We might also be able to free the file itself

      Unref (E.File);
      if Active (Debug_Me) then
         E := null;
      else
         Unchecked_Free (E);
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

   function Get_Key (E : LI_File_Item) return GNATCOLL.VFS.Virtual_File is
   begin
      return E.File.Name;
   end Get_Key;

   ----------
   -- Free --
   ----------

   procedure Free (E : in out LI_File_Item) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (LI_File_Item_Record, LI_File_Item);
   begin
      if Active (Assert_Me) then
         Trace (Assert_Me, "Free LI_File " & E.File.Name.Display_Full_Name);
      end if;

      Unref (E.File);

      if Active (Debug_Me) then
         E := null;
      else
         Unchecked_Free (E);
      end if;
   end Free;

   -----------
   -- Reset --
   -----------

   procedure Reset (Db : Entities_Database) is
   begin
      Trace (Assert_Me, "Reset entities database");
      Db.Predefined_File := null;

      --  Reset Lis first, since this will indirectly change a field in the
      --  source files (which are therefore better kept in memory in the
      --  meantime)
      Reset (Db.LIs);
      Reset (Db.Files);
   end Reset;

   -------------------------
   -- Foreach_Source_File --
   -------------------------

   procedure Foreach_Source_File
     (Db     : Entities_Database;
      Action : access procedure (F : in out Source_File))
   is
      Iter : Files_HTable.Cursor;
      File : Source_File_Item;
   begin
      Get_First (Db.Files, Iter);
      loop
         File := Get_Element (Iter);
         exit when File = null;

         Get_Next (Db.Files, Iter);
         Action (File.File);
      end loop;
   end Foreach_Source_File;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Db : in out Entities_Database) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Entities_Database_Record, Entities_Database);
   begin
      Reset (Db);

      Destroy (Db.FS_Optimizer);

      if Active (Debug_Me) then
         Db := null;
      else
         Unchecked_Free (Db);
      end if;
   end Destroy;

   ----------------------------
   -- Internal_Get_Or_Create --
   ----------------------------

   function Internal_Get_Or_Create
     (Db            : Entities_Database;
      Full_Filename : GNATCOLL.VFS.Virtual_File;
      File          : GNATCOLL.VFS.Virtual_File;
      Handler       : access LI_Handler_Record'Class;
      LI            : LI_File := null;
      Allow_Create  : Boolean := True) return Source_File
   is
      S : Source_File_Item := Get (Db.Files, Full_Filename);
      F : Source_File;
      use LI_File_Arrays;

   begin
      if S = null and then not Allow_Create then
         Trace
           (Assert_Me, "MANU Internal_Get_Or_Create, not creation allowed");
         null;

      elsif S = null then
         F := new Source_File_Record;
         F.Db           := Db;
         F.Depends_On   := Null_Dependency_List;
         F.Depended_On  := Null_Dependency_List;
         F.Scope_Tree_Computed := False;
         F.Ref_Count    := 1;

         if LI /= null then
            Append (F.LI_Files, LI);
         end if;

         if File = GNATCOLL.VFS.No_File then
            F.Name := Full_Filename;
         else
            F.Name := File;
         end if;

         F.Ordered_Index := Get_Key (Db.FS_Optimizer, F.Name);

         F.Handler := LI_Handler (Handler);

         S := new Source_File_Item_Record'(File => F, Next => null);
         Files_HTable.Set (Db.Files, S);

         if LI /= null then
            Append (LI.Files, F);
         end if;

      else
         --  If the source was not already associated with LI

         if LI /= null
           and then Find (S.File.LI_Files, LI) = LI_File_Arrays.First - 1
         then
            if S.File.LI_Files /= Null_LI_File_List then
               for J in LI_File_Arrays.First .. Last (S.File.LI_Files) loop
                  Remove (S.File.LI_Files.Table (J).Files, S.File);
               end loop;
            end if;

            Append (S.File.LI_Files, LI);
            Append (LI.Files, S.File);
         end if;

         S.File.Handler := LI_Handler (Handler);
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
      File         : GNATCOLL.VFS.Virtual_File;
      Handler      : LI_Handler := null;
      LI           : LI_File := null;
      Allow_Create : Boolean := True) return Source_File
   is
      H : LI_Handler := Handler;
   begin
      if H = null then
         H := Get_LI_Handler (Db, File);
      end if;

      if H = null then
         return null;
      else
         return Internal_Get_Or_Create (Db, File, File, H, LI, Allow_Create);
      end if;
   end Get_Or_Create;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Db           : Entities_Database;
      Base_Name    : Filesystem_String;
      Handler      : access LI_Handler_Record'Class;
      LI           : LI_File := null;
      Allow_Create : Boolean := True) return Source_File
   is
      File : Virtual_File;
   begin
      if Is_Absolute_Path (Base_Name) then
         return Internal_Get_Or_Create
           (Db, Create (Base_Name),
            GNATCOLL.VFS.No_File, Handler, LI, Allow_Create);

      else
         File := Db.Registry.Tree.Create
           (Base_Name,
            Use_Source_Path => True,
            Use_Object_Path => False);
         if File = No_File then
            File := Create_From_Base (Base_Name);
         end if;

         return Internal_Get_Or_Create
           (Db, File, File, Handler, LI, Allow_Create);
      end if;
   end Get_Or_Create;

   ----------
   -- Find --
   ----------

   function Find
     (List : Entity_Information_List;
      Loc  : File_Location) return Entity_Information is
   begin
      for L in Entity_Information_Arrays.First .. Last (List) loop
         if List.Table (L).LI_Declaration.File = Loc.File
           and then List.Table (L).LI_Declaration.Line = Loc.Line
           and then List.Table (L).LI_Declaration.Column = Loc.Column
         then
            return List.Table (L);
         end if;
      end loop;
      return null;
   end Find;

   ----------------
   -- Lt_No_File --
   ----------------

   function Lt_No_File (Left, Right : E_Reference) return Boolean is
   begin
      return Left.Location.Line < Right.Location.Line
        or else
          (Left.Location.Line = Right.Location.Line
           and then Left.Location.Column < Right.Location.Column);
   end Lt_No_File;

   ----------
   -- Next --
   ----------

   function Next
     (Cursor : Entity_Reference_Cursor)
      return Entity_Reference_Cursor
   is

      Result : Entity_Reference_Cursor := Cursor;
   begin
      Result.Entity_Cursor := Next (Result.Entity_Cursor);

      if Result.Entity_Cursor = Entities_In_File_Sets.No_Element then
         loop
            Result.File_Cursor := Next (Result.File_Cursor);

            exit when Result.File_Cursor = Entity_File_Maps.No_Element;

            Result.Entity_Cursor := First (Element (Result.File_Cursor).Refs);

            exit when Result.Entity_Cursor /= Entities_In_File_Sets.No_Element;
         end loop;
      end if;

      return Result;
   end Next;

   -----------
   -- First --
   -----------

   function First
     (List : Entity_Reference_List) return Entity_Reference_Cursor
   is

      Result : Entity_Reference_Cursor;
   begin
      Result.File_Cursor := First (List);

      if Result.File_Cursor /= Entity_File_Maps.No_Element then
         Result.Entity_Cursor := First (Element (Result.File_Cursor).Refs);

         if Result.Entity_Cursor = Entities_In_File_Sets.No_Element then
            Result := Next (Result);
         end if;
      end if;

      return Result;
   end First;

   -------------
   -- Element --
   -------------

   function Element (Cursor : Entity_Reference_Cursor) return E_Reference is
   begin
      return Element (Cursor.Entity_Cursor);
   end Element;

   -------------
   -- Element --
   -------------

   function Element
     (List : Entity_Reference_List; Key : Entity_Reference_Index)
      return E_Reference
   is
      Map : constant File_With_Refs_Access :=
        List.Element (Key.Loc.File.Ordered_Index);
   begin
      return Element (Map.Refs.Find ((Location => Key.Loc, others => <>)));
   end Element;

   --------------
   -- Contains --
   --------------

   function Contains
     (Element : Entity_Reference_List; Key : Entity_Reference_Index)
      return Boolean
   is
      Refs : File_With_Refs_Access;
   begin
      if Key.Loc /= No_File_Location
        and then Element.Contains (Key.Loc.File.Ordered_Index)
      then
         Refs := Element.Element (Key.Loc.File.Ordered_Index);

         return Refs.Refs.Contains ((Location => Key.Loc, others => <>));
      else
         return False;
      end if;
   end Contains;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (List   : Entity_Reference_List;
      Key    : Entity_Reference_Index;
      Val    : E_Reference)
   is
   begin
      Element (List, Key.Loc.File.Ordered_Index).Refs.Replace (Val);
   end Replace;

   -----------
   -- Index --
   -----------

   function Index
     (Cursor : Entity_Reference_Cursor) return Entity_Reference_Index
   is
   begin
      return
        (Element (Cursor.Entity_Cursor).Location,
         Element (Cursor.Entity_Cursor).Is_Declaration);
   end Index;

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

         --  See comments in Reset as to why we are increasing refcount only
         --  when adding to Depends_On (that's because even when Depends_On is
         --  no longer valid, some entities of File might depend on entities of
         --  Depends_On, and therefore the latter should be kept)
         Ref (Depends_On);

      elsif Explicit_Dependency then
         if File.Depends_On.Table (Index).Explicit /= Explicit_Dependency then
            --  This is only added when File itself is actually parsed, so be
            --  sure to reflect the dependency status
            File.Depends_On.Table (Index).Explicit := Explicit_Dependency;

            Index := Find (Depends_On.Depended_On, File);
            if Index < Dependency_Arrays.First then
               Append (Depends_On.Depended_On, (File, Explicit_Dependency));
            else
               Depends_On.Depended_On.Table (Index).Explicit :=
                 Explicit_Dependency;
            end if;
         end if;
      end if;
   end Add_Depends_On;

   ---------
   -- Add --
   ---------

   procedure Add
     (Entities         : in out Entities_Hash.Instance;
      Entity           : Entity_Information;
      Check_Duplicates : Boolean)
   is
      UEI : Entity_Informations;
      Str : constant Cased_String :=
              (Str => Entity.Name,
               Case_Sensitive => not Case_Insensitive_Identifiers
                 (Entity.LI_Declaration.File.Handler));
   begin
      UEI := Get (Entities, Str);

      if UEI = null then
         UEI := new Entity_Informations_Record'
           (List => new Entity_Information_List'
              (Null_Entity_Information_List),
            Next => null);

         Append (UEI.List.all, Entity);
         Entities_Hash.Set (Entities, UEI);
         Ref (Entity, "Add");

      else
         if not Check_Duplicates
           or else Find (UEI.List.all, Entity.LI_Declaration) = null
         then
            Append (UEI.List.all, Entity);
            Ref (Entity, "Add");
         end if;
      end if;
   end Add;

   --------------------
   -- Set_Time_Stamp --
   --------------------

   procedure Set_Time_Stamp
     (LI : LI_File; Timestamp : Ada.Calendar.Time := No_Time) is
   begin
      LI.Timestamp := Timestamp;
   end Set_Time_Stamp;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Db      : Entities_Database;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : Project_Type) return LI_File
   is
      L : LI_File_Item := Get (Db.LIs, File);
   begin
      Assert (Assert_Me, File /= GNATCOLL.VFS.No_File, "No LI filename");
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
         LI_HTable.Set (Db.LIs, L);
      end if;

      return L.File;
   end Get_Or_Create;

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

      --  If End_Of_Scope is currently the spec, save this in the standard list
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
            Unref (Entity.End_Of_Scope.Caller);
            Entity.End_Of_Scope := (Location, null, null, Kind, False);
         else
            Add_Reference
              (Entity,
               Location => Location,
               Kind     => Kind);
         end if;
      else
         Unref (Entity.End_Of_Scope.Caller);
         Entity.End_Of_Scope := (Location, null, null, Kind, False);
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
      Unref (Entity.Rename, "replacing renaming_of");
      Entity.Rename := Renaming_Of;
      Ref (Renaming_Of, "Set_Is_Renaming_Of");
   end Set_Is_Renaming_Of;

   --------------------------
   -- Set_Overriden_Entity --
   --------------------------

   procedure Set_Overriden_Entity
     (Entity : Entity_Information; Overriden : Entity_Information) is
   begin
      Assert (Assert_Me, Overriden /= null, "Invalid overriden entity");
      Unref (Entity.Pointed_Type, "replacing pointed_type (overriden)");
      Entity.Pointed_Type := Overriden;
      Ref (Overriden, "Set_Overriden_Entity");
   end Set_Overriden_Entity;

   ----------------
   -- Add_Called --
   ----------------

   procedure Add_Called
     (Entity   : Entity_Information;
      Called   : Entity_Information) is
   begin
      --  Check whether it already exists
      if Find (Entity.Called_Entities, Called) <
        Entity_Information_Arrays.First
      then
         Ref (Called, "Add_Called");
         Append (Entity.Called_Entities, Called);
      end if;
   end Add_Called;

   --------------------
   -- Add_Index_Type --
   --------------------

   procedure Add_Index_Type
     (Entity : Entity_Information; Index : Entity_Information)
      renames Add_Called;

   -------------------------------
   -- Set_Caller_At_Declaration --
   -------------------------------

   procedure Set_Caller_At_Declaration
     (Entity   : Entity_Information;
      Caller   : Entity_Information) is
   begin
      Unref (Entity.Caller_At_Declaration, "replacing caller_at_decl");
      Entity.Caller_At_Declaration := Caller;
      Ref (Caller, "caller_at_declaration");
   end Set_Caller_At_Declaration;

   -------------------
   -- Add_Reference --
   -------------------

   procedure Add_Reference
     (Entity                : Entity_Information;
      Location              : File_Location;
      Kind                  : Reference_Kind;
      From_Instantiation_At : Entity_Instantiation := No_Instantiation)
   is
      Refs : File_With_Refs_Access;
   begin
      Assert (Assert_Me, Location.File /= null, "Invalid file in reference");

      if not Entity.References.Contains (Location.File.Ordered_Index) then
         Refs := new File_With_Refs;
         Refs.File := Location.File;

         Entity.References.Insert (Location.File.Ordered_Index, Refs);

         Entity.File_Timestamp_In_References :=
           Entity.File_Timestamp_In_References + 1;
      else
         Refs := Entity.References.Element (Location.File.Ordered_Index);

         --  We might have a change that the reference already existed

         if Active (Add_Reference_Force_Unique)
           and then Refs.Refs.Contains ((Location => Location, others => <>))
         then
            return;
         end if;
      end if;

      Refs.Refs.Insert
        ((Location              => Location,
          Caller                => null,
          From_Instantiation_At => From_Instantiation_At,
          Kind                  => Kind,
          Is_Declaration        => False));
      Add_All_Entities (Location.File, Entity);
   end Add_Reference;

   ---------------------
   -- Set_Is_Imported --
   ---------------------

   procedure Set_Is_Imported
     (Entity : Entity_Information; Value : Boolean := True) is
   begin
      Entity.Is_Imported := Value;
   end Set_Is_Imported;

   --------------------------
   -- Set_Is_Instantiation --
   --------------------------

   procedure Set_Is_Instantiation
     (Entity : Entity_Information; Of_Generic : Entity_Information) is
   begin
      Unref (Entity.Instantiation_Of, "instantiation_of");
      Entity.Instantiation_Of := Of_Generic;
      Ref (Entity.Instantiation_Of, "set_is_instantiation");

      Add_Reference
        (Of_Generic,
         Location => Get_Declaration_Of (Entity),
         Kind     => Instantiation_Reference);
   end Set_Is_Instantiation;

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
      if Active (Assert_Me) then
         Assert (Assert_Me, Is_Of_Type /= null, "Invalid type for entity");
         if Is_Of_Type = Entity then
            raise Program_Error with "Entity can't be its own parent";
         end if;
      end if;

      Append (Entity.Parent_Types, Is_Of_Type);
      Ref (Is_Of_Type, "Is_Of_Type");

      if Entity.Kind.Is_Type then
         Append (Is_Of_Type.Child_Types, Entity);
         Ref (Entity, "Set_Type_Of.Child_Types");
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

      if Find (Entity.Primitive_Subprograms, Primitive) <
        Entity_Information_Arrays.First
      then
         Append (Entity.Primitive_Subprograms, Primitive);
         Ref (Primitive, "Add_Primitive_Subprogram");

         Unref (Primitive.Primitive_Op_Of, "replacing parent of primitive");
         Primitive.Primitive_Op_Of := Entity;
         Ref (Entity, "parent of primitive");
      end if;
   end Add_Primitive_Subprogram;

   ----------------------
   -- Set_Pointed_Type --
   ----------------------

   procedure Set_Pointed_Type
     (Entity : Entity_Information; Points_To : Entity_Information) is
   begin
      Assert (Assert_Me, Points_To /= null, "Invalid pointed type");
      Unref (Entity.Pointed_Type, "replacing pointed_type");
      Entity.Pointed_Type := Points_To;
      Ref (Points_To, "Set_Pointed_Type");
   end Set_Pointed_Type;

   -----------------------
   -- Set_Returned_Type --
   -----------------------

   procedure Set_Returned_Type
     (Entity : Entity_Information; Returns : Entity_Information) is
   begin
      Assert (Assert_Me, Returns /= null, "Invalid returned type");
      Unref (Entity.Returned_Type, "replacing returned_type");
      Entity.Returned_Type := Returns;
      Ref (Returns, "Set_Returned_Type");
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
     (Name         : GNATCOLL.Symbols.Symbol;
      File         : Source_File;
      Line         : Natural;
      Column       : Basic_Types.Visible_Column_Type;
      Allow_Create : Boolean := True) return Entity_Information
   is
      UEI              : Entity_Informations;
      E                : Entity_Information;
      Must_Add_To_File : Boolean := False;
      Str              : constant Cased_String :=
                           (Str            => Name,
                            Case_Sensitive =>
                              not Case_Insensitive_Identifiers (File.Handler));
   begin
      Assert
        (Assert_Me, Name /= No_Symbol, "No name specified for Get_Or_Create");
      if Active (Ref_Me) then
         Trace (Ref_Me, "Get_Or_Create entity name=" & Get (Name).all
                & " File=" & (+Base_Name (Get_Filename (File)))
                & " Line=" & Line'Img);
      end if;

      UEI := Get (File.Entities, Str);

      if UEI = null then
         if Allow_Create then
            UEI := new Entity_Informations_Record'
              (List => new Entity_Information_List'
                 (Null_Entity_Information_List),
               Next => null);
            Must_Add_To_File := True;
         end if;

      else
         E := Find (UEI.List.all, (File, Line, Column));
      end if;

      if E = null and then Allow_Create then
         E := new Entity_Information_Record'
           (Name                         => Name,
            Kind                         => Unresolved_Entity_Kind,
            Attributes                   => (others => False),
            LI_Declaration               => (File, Line, Column),
            Live_Declaration             => (File, Line, Column),
            Caller_At_Declaration        => null,
            End_Of_Scope                 => No_E_Reference,
            Parent_Types                 => Null_Entity_Information_List,
            Pointed_Type                 => null,
            Returned_Type                => null,
            Primitive_Op_Of              => null,
            Rename                       => null,
            Instantiation_Of             => null,
            Called_Entities              => Null_Entity_Information_List,
            Primitive_Subprograms        => Null_Entity_Information_List,
            Child_Types                  => Null_Entity_Information_List,
            References                   => Entity_File_Maps.Empty_Map,
            File_Timestamp_In_References => 0,
            Is_Valid                     => True,
            Ref_Count                    => 1,
            Trie_Tree_Index              =>
              Entities_Search_Tries.Null_Vector_Trie_Index,
            Is_Dummy                     => False,
            Is_Imported                  => False);

         Ref (File);  --  Used in declaration
         Append (UEI.List.all, E);

         if Must_Add_To_File then
            Entities_Hash.Set (File.Entities, UEI);
         end if;

         if File.Db.Lang.Get_Language_From_File
           (File.Name).Entities_Indexed
         then
            Entities_Search_Tries.Insert
              (File.Handler.Name_Index'Access,
               File.Db.Symbols,
               E,
               Get (Name).all,
               E.Trie_Tree_Index);
         end if;

      elsif E /= null then
         if not E.Is_Valid then
            Ref (E);
         end if;
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

   function Get_Project (LI : LI_File) return Project_Type is
   begin
      return LI.Project;
   end Get_Project;

   -------------------------
   -- Get_Predefined_File --
   -------------------------

   function Get_Predefined_File
     (Db      : Entities_Database;
      Handler : access LI_Handler_Record'Class) return Source_File
   is
   begin
      if Db.Predefined_File = null then
         if Case_Insensitive_Identifiers (Handler) then
            Db.Predefined_File := Get_Or_Create
              (Db, Create_From_Base ("<case_insensitive_predefined>"),
               LI_Handler (Handler), null);
         else
            Db.Predefined_File := Get_Or_Create
              (Db, Create_From_Base ("<case_sensitive_predefined>"),
               LI_Handler (Handler), null);
         end if;
      end if;
      return Db.Predefined_File;
   end Get_Predefined_File;

   -------------------
   -- Is_Subprogram --
   -------------------

   function Is_Subprogram (Entity : Entity_Information) return Boolean is
   begin
      return Entity /= null and then Is_Subprogram_Entity (Entity.Kind.Kind);
   end Is_Subprogram;

   --------------
   -- Is_Array --
   --------------

   function Is_Array (Entity : Entity_Information) return Boolean is
   begin
      return Entity /= null and then Entity.Kind.Kind = Array_Kind;
   end Is_Array;

   -----------------
   -- Is_Imported --
   -----------------

   function Is_Imported
     (Entity : Entity_Information) return Boolean is
   begin
      return Entity.Is_Imported;
   end Is_Imported;

   -------------------------------
   -- Is_Primitive_Operation_Of --
   -------------------------------

   function Is_Primitive_Operation_Of
     (Entity : Entity_Information) return Entity_Information is
   begin
      return Entity.Primitive_Op_Of;
   end Is_Primitive_Operation_Of;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Handler : in out LI_Handler_Record) is
   begin
      Clear (Handler.Name_Index);
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
      use LI_File_Arrays;
   begin
      if File.LI_Files /= Null_LI_File_List
        and then Length (File.LI_Files) /= 0
      then
         return File.LI_Files.Table (LI_File_Arrays.First);
      else
         return null;
      end if;
   end Get_LI;

   --------------------
   -- Get_LI_Handler --
   --------------------

   function Get_LI_Handler
     (Db              : Entities_Database;
      Source_Filename : GNATCOLL.VFS.Virtual_File) return LI_Handler is
   begin
      return Get_LI_Handler_From_File
        (Language_Handler (Db.Lang), Source_Filename);
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
   begin
      if File /= null then
         if File.Handler /= null then
            F := Get_Source_Info
              (File.Handler, File.Name, File_Has_No_LI_Report);
         end if;
      end if;
   end Update_Xref;

   -------------------
   -- Is_Up_To_Date --
   -------------------

   function Is_Up_To_Date (File : Source_File) return Boolean is
      From_Disk, Src_From_Disk : Time;
      Result    : Boolean := False;
      use LI_File_Arrays;
   begin
      if File.LI_Files /= Null_LI_File_List then

         --  First check whether we have indeed loaded the latest LI files

         Result := True;

         for J in LI_File_Arrays.First .. Last (File.LI_Files) loop
            From_Disk := File_Time_Stamp (File.LI_Files.Table (J).Name);
            Result := Result and then
                        From_Disk = File.LI_Files.Table (J).Timestamp;

            if Active (Assert_Me) then
               Trace (Assert_Me, "Is_Up_To_Date: "
                   & (+Base_Name (Get_Filename (File)))
                   & " LI file time:" & Image (From_Disk, "%D-%T")
                   & " memory: "
                   & Image (File.LI_Files.Table (J).Timestamp, "%D-%T")
                   & " => " & Result'Img);
            end if;
         end loop;

         if Result then
            --  Then check that LI file was indeed more recent than the source.
            --  Otherwise the user will need to regenerate the LI file

            Src_From_Disk := File_Time_Stamp (Get_Filename (File));
            Result := Src_From_Disk <= From_Disk;

            if Active (Assert_Me) then
               Trace (Assert_Me, "Is_Up_To_Date, checking source: "
                      & (+Base_Name (Get_Filename (File)))
                      & " LI file time:" & Image (From_Disk, "%D-%T")
                      & " Src file time:" & Image (Src_From_Disk, "%D-%T")
                      & " => " & Result'Img);
            end if;
         end if;
      end if;
      return Result;
   end Is_Up_To_Date;

   ------------------------
   -- Get_Declaration_Of --
   ------------------------

   function Get_Declaration_Of
     (Entity : Entity_Information) return File_Location is
   begin
      return Entity.Live_Declaration;
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

   function Get_Column (Loc : File_Location) return Visible_Column_Type is
   begin
      return Loc.Column;
   end Get_Column;

   ---------------
   -- To_String --
   ---------------

   function To_String (Loc : File_Location) return String is
   begin
      if Loc = No_File_Location then
         return "null";
      else
         return String (Loc.File.Name.Full_Name.all)
           & ":" & Trim (Loc.Line'Img, Both)
           & ":" & Trim (Loc.Column'Img, Both);
      end if;
   end To_String;

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

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category
     (Entity : Entity_Information) return Entity_Category is
   begin
      case Entity.Kind.Kind is
         when Overloaded_Entity | Unresolved_Entity | Macro =>
            return Unknown;

         when Procedure_Kind | Function_Or_Operator |
              Entry_Or_Entry_Family | Function_Macro =>
            return Subprogram;

         when Package_Kind =>
            return Package_Or_Namespace;

         when Label_On_Block | Label_On_Loop | Label_On_Statement =>
            return Label;

         when Enumeration_Literal | Named_Number =>
            return Literal;

         when others =>
            if Entity.Kind.Is_Type then
               return Type_Or_Subtype;
            else
               return Object;
            end if;
      end case;
   end Get_Category;

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
      if Ref.Entity = null then
         return No_File_Location;
      elsif Ref.Index.Is_Declaration then
         return Ref.Entity.Live_Declaration;
      elsif Ref.Entity.References /= Entity_File_Maps.Empty_Map then
         return Ref.Index.Loc;
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
        and then Entity.LI_Declaration.File =
          Get_Predefined_File
            (Entity.LI_Declaration.File.Db,
             Entity.LI_Declaration.File.Handler);
   end Is_Predefined_Entity;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind (Ref : Entity_Reference) return Reference_Kind is
   begin
      if Ref.Entity = null then
         return Reference;
      elsif Ref.Index.Is_Declaration then
         return Declaration;
      elsif Ref.Entity.References /= Entity_File_Maps.Empty_Map then
         return Element (Ref.Entity.References, Ref.Index).Kind;
      else
         return Reference;
      end if;
   end Get_Kind;

   ---------------------------
   -- From_Instantiation_At --
   ---------------------------

   function From_Instantiation_At
     (Ref : Entity_Reference) return Entity_Instantiation is
   begin
      if Ref.Entity /= null
        and then Ref.Entity.References /= Entity_File_Maps.Empty_Map
        and then Contains (Ref.Entity.References, Ref.Index)
      then
         return Element
           (Ref.Entity.References, Ref.Index).From_Instantiation_At;
      else
         return null;
      end if;
   end From_Instantiation_At;

   ------------------------
   -- Category_To_String --
   ------------------------

   function Category_To_String (Category : Entity_Category) return String is
   begin
      case Category is
         when Label =>
            return "label";
         when Literal =>
            return "literal";
         when Object =>
            return "object";
         when Subprogram =>
            return "subprogram";
         when Package_Or_Namespace =>
            return "package/namespace";
         when Type_Or_Subtype =>
            return "type";
         when Unknown =>
            return "unknown";
      end case;
   end Category_To_String;

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
            return Get_Value ("class", "class instance");
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
         when Function_Macro =>
            return "function macro";  -- -"function macro"
         when Function_Or_Operator =>
            return Get_Value ("function", "function"); --  -"function"
         when Interface_Kind =>
            return Get_Value ("interface", "interface"); --  "interface
         when Package_Kind =>
            return Get_Value ("package", "package");  --  -"package"
         when Procedure_Kind =>
            return Get_Value ("procedure", "procedure");  --  -"procedure"
         when Include_File =>
            return "include file";  -- -"include"
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
            return "generic formal";  --  -"generic formal"
         when Private_Object =>
            return "private object";  --  -"private object"
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
         when Reference               => return -"reference";
         when Own_Reference           => return -"own reference";
         when Subprogram_Call         => return -"call";
         when Dispatching_Call        => return -"dispatching call";
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

   -----------
   -- Image --
   -----------

   function Image (Attr : Entity_Attributes_Names) return String is
   begin
      case Attr is
         when Global          => return "global";
         when Class_Static    => return "class_static";
         when Static_Local    => return "static";
         when Protected_Field => return "protected";
         when Public_Field    => return "public";
         when Private_Field   => return "private";
         when Virtual         => return "virtual";
         when Abstract_Entity => return "abstract";
      end case;
   end Image;

   --------------------------
   -- Attributes_To_String --
   --------------------------

   function Attributes_To_String (Attr : Entity_Attributes) return String is
      Str   : String (1 .. 1024);
      Index : Natural := Str'First;
   begin
      if Attr (Class_Static) or else Attr (Static_Local) then
         Str (Index .. Index + 6) := "static ";
         Index := Index + 7;
      elsif Attr (Global) then
         Str (Index .. Index + 6) := "global ";
         Index := Index + 7;
      else
         Str (Index .. Index + 5) := "local ";
         Index := Index + 6;
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
     (Handler   : access LI_Handler_Record;
      Languages : access Abstract_Language_Handler_Record'Class;
      File_Name : GNATCOLL.VFS.Virtual_File;
      Result    : out Language.Construct_List)
   is
      pragma Unreferenced (Handler);

      Lang : constant Language.Language_Access :=
        Get_Language_From_File (Language_Handler (Languages), File_Name);

   begin
      --  Call the language specific syntax analyzer

      Parse_File_Constructs (Lang, File_Name, Result);
   end Parse_File_Constructs;

   --------------------
   -- Get_Name_Index --
   --------------------

   function Get_Name_Index
     (LI : access LI_Handler_Record)
      return access Entities_Search_Tries.Vector_Trie is
   begin
      return LI.Name_Index'Access;
   end Get_Name_Index;

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
      return Get (Entity1.Name).all < Get (Entity2.Name).all;
   end "<";

   ------------------------------
   -- Declaration_As_Reference --
   ------------------------------

   function Declaration_As_Reference
     (Entity : Entity_Information) return Entity_Reference
   is
   begin
      return
        (Entity,
         (Loc => No_File_Location, Is_Declaration => True, others => <>));
   end Declaration_As_Reference;

   ---------------------------------
   -- Get_Or_Create_Instantiation --
   ---------------------------------

   function Get_Or_Create_Instantiation
     (File   : Source_File;
      Entity : Entity_Information;
      Nested : Entity_Instantiation := No_Instantiation)
      return Entity_Instantiation
   is
      Inst, Tmp, Previous : Entity_Instantiation;
   begin
      if Nested = No_Instantiation then
         for J in Instantiation_Arrays.First .. Last (File.Instantiations) loop
            if File.Instantiations.Table (J).Entity = Entity
              and then File.Instantiations.Table (J).Generic_Parent = null
            then
               return File.Instantiations.Table (J);
            end if;
         end loop;

         Inst := new E_Instantiation_Record'(Entity, null);
         Ref (Entity);
         Append (File.Instantiations, Inst);

      else
         for J in Instantiation_Arrays.First .. Last (File.Instantiations) loop
            Inst := Nested;
            Tmp  := File.Instantiations.Table (J);
            while Inst /= null
              and then Tmp /= null
              and then Inst.Entity = Tmp.Entity
            loop
               Inst := Inst.Generic_Parent;
               Tmp := Tmp.Generic_Parent;
            end loop;

            if Inst = null
              and then Tmp /= null
              and then Tmp.Entity = Entity
              and then Tmp.Generic_Parent = null
            then
               return File.Instantiations.Table (J);
            end if;
         end loop;

         Tmp := Nested;
         Previous := null;
         while Tmp /= null loop
            Inst := new E_Instantiation_Record'(Tmp.Entity, null);
            Ref (Tmp.Entity);

            if Previous /= null then
               Previous.Generic_Parent := Inst;
            else
               Append (File.Instantiations, Inst);
            end if;
            Previous := Inst;

            Tmp := Tmp.Generic_Parent;
         end loop;

         Previous.Generic_Parent := new E_Instantiation_Record'(Entity, null);
         Ref (Entity);
      end if;
      return File.Instantiations.Table (Last (File.Instantiations));
   end Get_Or_Create_Instantiation;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Instantiation : Entity_Instantiation) return Entity_Information is
   begin
      if Instantiation = null then
         return null;
      else
         return Instantiation.Entity;
      end if;
   end Get_Entity;

   --------------------
   -- Generic_Parent --
   --------------------

   function Generic_Parent
     (Instantiation : Entity_Instantiation) return Entity_Instantiation is
   begin
      if Instantiation = null then
         return null;
      else
         return Instantiation.Generic_Parent;
      end if;
   end Generic_Parent;

   ------------------------------
   -- Get_Index_In_Search_Tree --
   ------------------------------

   function Get_Name
     (Entities : Entity_Array_Access) return Cst_String_Access
   is
   begin
      return Get (Get_Name (Entities (Entities'First)));
   end Get_Name;

   ----------
   -- Sort --
   ----------

   procedure Sort
     (List    : in out Entity_Information_List;
      Sort_By : Sort_Type)
   is
      subtype IT is Entity_Information_Arrays.Index_Type;

      procedure Xchg (Op1, Op2 : Natural);
      function Lt    (Op1, Op2 : Natural) return Boolean;

      procedure Xchg (Op1, Op2 : Natural) is
         T : constant Entity_Information := List.Table (IT (Op1));
      begin
         List.Table (IT (Op1)) := List.Table (IT (Op2));
         List.Table (IT (Op2)) := T;
      end Xchg;

      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         if List.Table (IT (Op1)) = null then
            return True;
         elsif List.Table (IT (Op2)) = null then
            return False;
         elsif Sort_By = Sort_Alphabetical then
            return Get (Get_Name (List.Table (IT (Op1)))).all <
              Get (Get_Name (List.Table (IT (Op2)))).all;
         else
            return Get_Declaration_Of (List.Table (IT (Op1))) <
              Get_Declaration_Of (List.Table (IT (Op2)));
         end if;
      end Lt;

   begin
      Sort (Natural (Length (List)), Xchg'Unrestricted_Access,
            Lt'Unrestricted_Access);
   end Sort;

   ------------------------------
   -- Parse_All_LI_Information --
   ------------------------------

   procedure Parse_All_LI_Information
     (Handler          : access LI_Handler_Record'Class;
      Project          : Project_Type)
   is
      Iter : LI_Information_Iterator'Class :=
        Parse_All_LI_Information (Handler, Project);
      Count, Total : Natural;
   begin
      Next (Iter, Natural'Last, Count, Total); --  All steps
      Free (Iter);
   end Parse_All_LI_Information;

   -----------------
   -- Set_Symbols --
   -----------------

   procedure Set_Symbols
     (Self    : Entities_Database;
      Symbols : GNATCOLL.Symbols.Symbol_Table_Access) is
   begin
      Self.Symbols := Symbols;
   end Set_Symbols;

   -----------------
   -- Get_Symbols --
   -----------------

   function Get_Symbols
     (Self : Entities_Database) return GNATCOLL.Symbols.Symbol_Table_Access is
   begin
      return Self.Symbols;
   end Get_Symbols;

   ----------------------------------
   -- Has_Unresolved_Imported_Refs --
   ----------------------------------

   function Has_Unresolved_Imported_Refs
     (Handler : access LI_Handler_Record'Class) return Boolean is
   begin
      if Handler /= null then
         return Handler.Has_Unresolved_Imported_Refs;
      else
         return False;
      end if;
   end Has_Unresolved_Imported_Refs;

   procedure Set_Has_Unresolved_Imported_Refs
     (Handler : access LI_Handler_Record'Class;
      Value   : Boolean := True) is
   begin
      Handler.Has_Unresolved_Imported_Refs := Value;
   end Set_Has_Unresolved_Imported_Refs;

   ------------------------
   -- Is_Update_Required --
   ------------------------

   function Update_Forced
     (Handler : access LI_Handler_Record'Class) return Boolean is
   begin
      if Handler /= null then
         return Handler.Update_Forced;
      else
         return False;
      end if;
   end Update_Forced;

   procedure Set_Update_Forced
     (Handler : access LI_Handler_Record'Class;
      Value   : Boolean := True) is
   begin
      Handler.Update_Forced := Value;
   end Set_Update_Forced;

end Entities;
