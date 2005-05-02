-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2005                       --
--                              AdaCore                              --
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
with Language_Handlers.GPS;   use Language_Handlers.GPS;
with GNAT.Heap_Sort_G;
with Namet;                   use Namet;
with Projects.Registry;       use Projects.Registry;
with File_Utils;              use File_Utils;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GPS.Intl;                use GPS.Intl;
with GNAT.Calendar.Time_IO;   use GNAT.Calendar.Time_IO;
with Language;                use Language;
with Entities.Debug;          use Entities.Debug;

package body Entities is
   Assert_Me : constant Debug_Handle := Create ("Entities.Assert", Off);

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

   use Entities_Hash;
   use Shared_Entities_Hash;
   use Files_HTable;
   use LI_HTable;
   use Entity_Information_Arrays;
   use Source_File_Arrays;
   use Entity_Reference_Arrays;
   use Dependency_Arrays;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Entity_Information_Record'Class, Entity_Information);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Entity_Information_List, Entity_Information_List_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Entity_Informations_Record, Entity_Informations);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Unshared_Entity_Informations_Record,
      Unshared_Entity_Informations);

   function String_Hash is new HTables.Hash (HTable_Header);

   function Hash is new HTables.Hash (Header_Num);

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
     (Entity           : in out Entity_Information;
      Clear_References : Boolean);
   --  Isolate the entity from the rest of the LI structure. This should be
   --  used when we are removing the entity from the internal tables, but the
   --  user has kept a handle on it, to avoid memory leaks.
   --  The references are cleared only if Clear_References is True.

   procedure Remove
     (D      : in out Shared_Entities_Hash.HTable;
      E      : Entity_Information);
   --  Remove the information for E in the table. The entity is not Unrefed
   --  explicitly, since this call should only happen as a result of Unref

   procedure Add (Entities         : in out Entities_Hash.HTable;
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
     (Subprogram_In_Parameter     => True,
      Subprogram_In_Out_Parameter => True,
      Subprogram_Out_Parameter    => True,
      Subprogram_Access_Parameter => True,
      others                      => False);

   Show_In_Call_Graph_Array : constant Reference_Kind_Filter :=
     (Reference    => True,
      Modification => True,
      others       => False);

   ---------
   -- Ref --
   ---------

   procedure Ref (Entity : Entity_Information; Reason : String) is
   begin
      if Entity /= null and then Active (Ref_Me) then
         Trace (Ref_Me, "Ref " & Entity.Shared_Name.all & " (" & Reason & ")");
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
           (Ref_Me, "Unref " & Entity.Shared_Name.all
            & " (" & Reason & ")"
            & Entity.Ref_Count'Img & " " & Boolean'Image (Entity.Is_Valid));
      end if;
      Unref (Entity);
   end Unref;

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
         return Entity.Shared_Name;
      end if;
   end Get_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (D : Entity_Informations) return String_Access is
   begin
      return D.Name;
   end Get_Name;

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

   function Next (E : Entity_Informations) return Entity_Informations is
   begin
      return E.Next;
   end Next;

   ----------
   -- Hash --
   ----------

   function Hash (S : GNAT.OS_Lib.String_Access) return Header_Num is
   begin
      return Hash (S.all);
   end Hash;

   -----------
   -- Equal --
   -----------

   function Equal (S1, S2 : GNAT.OS_Lib.String_Access) return Boolean is
   begin
      return S1.all = S2.all;
   end Equal;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (D : Unshared_Entity_Informations) return GNAT.OS_Lib.String_Access is
   begin
      if D = null then
         return null;
      else
         if Active (Assert_Me) then
            Assert
              (Assert_Me,
               D.List.Table (Entity_Information_Arrays.First).Ref_Count /= 0,
               "Entity has been freed: "
               & D.List.Table
                 (Entity_Information_Arrays.First).Shared_Name.all);
         end if;

         return D.List.Table
           (Entity_Information_Arrays.First).Shared_Name;
      end if;
   end Get_Name;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (D : in out Unshared_Entity_Informations) is
   begin
      Unchecked_Free (D.List);
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
            --  File not removed from htable explicitely. Unref is already
            --  called internally when the file is removed from the htable,
            --  and the user is not supposed to call Unref more often than Ref
            Reset (F);

            if Active (Debug_Me) then
               F := null;
            else
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
      if Entity.Declaration.File /= File then
         Add_Depends_On (File, Entity.Declaration.File);
         Add (File.All_Entities, Entity, Check_Duplicates => True);
      end if;
   end Add_All_Entities;

   -----------
   -- Reset --
   -----------

   procedure Reset (LI : LI_File) is
   begin
      if Active (Assert_Me) then
         Trace (Assert_Me, "Reseting LI " & Full_Name (LI.Name).all);
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

   -------------------------------
   -- Mark_And_Isolate_Entities --
   -------------------------------

   procedure Mark_And_Isolate_Entities (File : Source_File) is
      procedure Clean_References
        (EL         : in out Entity_Information_List_Access;
         Mark_Valid : Boolean);
      --  Remove all references in File for the entities in EL
      --  If Mark_Valid is true, then all entities are marked as valid.
      --  Otherwise, their status is not changed.
      --  None of the entity is unref'ed or destroyed

      procedure Clean_References
        (EL         : in out Entity_Information_List_Access;
         Mark_Valid : Boolean)
      is
         Entity : Entity_Information;
         From, To : Entity_Reference_Arrays.Index_Type :=
           Entity_Reference_Arrays.Index_Type'Last;
      begin
         for E in reverse Entity_Information_Arrays.First .. Last (EL.all) loop
            Entity := EL.Table (E);
            if Active (Debug_Me) then
               Check_Entity_Is_Valid
                 (Entity,
                  "Mark_And_Isolate_Entities "
                  & Base_Name (Get_Filename (File)));
            end if;

            To := Entity_Reference_Arrays.Index_Type'Last;
            for R in reverse
              Entity_Reference_Arrays.First .. Last (Entity.References)
            loop
               if Entity.References.Table (R).Location.File = File then
                  Unref (Entity.References.Table (R).Caller,
                         "reference.caller");
                  Unref (Entity.References.Table (R).From_Instantiation_At,
                         "reference.instantiation");
                  if To = Entity_Reference_Arrays.Index_Type'Last then
                     To := R;
                  end if;
                  From := R;
               else
                  if To /= Entity_Reference_Arrays.Index_Type'Last then
                     Remove (Entity.References, From, To);
                     To := Entity_Reference_Arrays.Index_Type'Last;
                  end if;
               end if;
            end loop;

            if To /= Entity_Reference_Arrays.Index_Type'Last then
               Remove (Entity.References, From, To);
            end if;

            if Mark_Valid then
               Isolate (Entity, Clear_References => False);
            end if;
         end loop;
      end Clean_References;

      Iter   : Shared_Entities_Hash.Iterator;
      Iter2  : Entities_Hash.Iterator;
      EIS    : Entity_Informations;
      UEI    : Unshared_Entity_Informations;
   begin
      Get_First (File.Entities, Iter);
      loop
         EIS := Get_Element (Iter);
         exit when EIS = No_Entity_Informations;
         Get_Next (File.Entities, Iter);
         Clean_References (EIS.List, True);
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
   -- Reset --
   -----------

   procedure Reset (File : Source_File) is
   begin
      if Active (Assert_Me) then
         Trace (Assert_Me, "Reseting " & Full_Name (File.Name).all);
         if Base_Name (Get_Filename (File)) = "atree.ads" then
            Dump (File, Show_Entities => True, Full => True);
         end if;
      end if;

      Mark_And_Isolate_Entities (File);

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

      --  Reset the All_Entities list, since this file is no longer referencing
      --  any of the entity
      declare
         Iter   : Entities_Hash.Iterator;
         EL     : Entity_Information_List_Access;
         UEI    : Unshared_Entity_Informations;
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

            Unchecked_Free (UEI.List);
         end loop;
         Reset (File.All_Entities);
      end;

      --  Clean up .Entities field

      declare
         Iter   : Shared_Entities_Hash.Iterator;
         EIS    : Entity_Informations;
         Entity : Entity_Information;
      begin
         Get_First (File.Entities, Iter);
         loop
            EIS := Get_Element (Iter);
            exit when EIS = null;
            Get_Next (File.Entities, Iter);

            for J in reverse
              Entity_Information_Arrays.First .. Last (EIS.List.all)
            loop
               Entity := EIS.List.Table (J);
               --  If the entity was still under control of File
               if Entity.Is_Valid then
                  if Entity.Ref_Count /= 1 then
                     Entity.Is_Valid := False;
                  end if;
                  Unref (Entity, ".entities");
               end if;
            end loop;
         end loop;
      end;

      --  Clean up various other fields

      File.Scope_Tree_Computed := False;

      if not Active (Debug_Me) then
         Free (File.Unit_Name);
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
     (Entity : in out Entity_Information; Clear_References : Boolean)
   is
      Entity2 : Entity_Information;
   begin
      if Active (Ref_Me) then
         Trace (Ref_Me, "Isolate " & Entity.Shared_Name.all);
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
         for R in reverse
           Entity_Reference_Arrays.First .. Last (Entity.References)
         loop
            Unref (Entity.References.Table (R).Caller, "reference.caller");
            Entity.References.Table (R).Caller := null;
            Unref (Entity.References.Table (R).From_Instantiation_At,
                   "reference.instantiation");
            Entity.References.Table (R).From_Instantiation_At := null;
         end loop;

         Free (Entity.References);
      end if;
   end Isolate;

   ---------------------------
   -- Check_Entity_Is_Valid --
   ---------------------------

   procedure Check_Entity_Is_Valid
     (Entity : Entity_Information; Reason : String) is
   begin
      Assert (Debug_Me, Entity /= null, "Referencing null entity ("
              & Reason & ")");
      Assert (Debug_Me,
              Entity.Ref_Count /= 0,
              "Entity should no longer be referenced "
              & Entity.Shared_Name.all & " (" & Reason & ") ");
   end Check_Entity_Is_Valid;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (D      : in out Shared_Entities_Hash.HTable;
      E      : Entity_Information)
   is
      EIS : constant Entity_Informations := Get (D, E.Shared_Name);
      EL  : Entity_Information_List_Access;
   begin
      if EIS /= No_Entity_Informations then
         EL := EIS.List;
         if Length (EL.all) = 1 then
            if EL.Table (Entity_Information_Arrays.First) = E then
               Remove (D, E.Shared_Name);
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
      Shared : GNAT.OS_Lib.String_Access;
   begin
      if Entity /= null and then Entity.Ref_Count /= Natural'Last then
         if Active (Assert_Me) then
            Assert (Assert_Me, Entity.Ref_Count > 0,
                    "too many calls to unref for " & Entity.Shared_Name.all);
         end if;

         Entity.Ref_Count := Entity.Ref_Count - 1;
         if Entity.Ref_Count = 0 then
            if Active (Ref_Me) then
               Trace (Ref_Me, "Freeing " & Entity.Shared_Name.all
                      & ":"
                      & Base_Name (Get_Filename (Entity.Declaration.File))
                      & Entity.Declaration.Line'Img
                      & " ref_count=" & Entity.Ref_Count'Img);
            end if;
            Isolate (Entity, Clear_References => True);

            --  If we are debugging, we do not free the memory, to keep a
            --  debuggable structure. Use a temporary variable to store the
            --  future name: otherwise, the entity will not be properly removed
            --  from the htable since it won't be found there.
            if Active (Debug_Me) then
               Shared := new String'
                 (Entity.Shared_Name.all
                  & ':' & Base_Name (Entity.Declaration.File.Name));
            end if;

            --  Temporarily fool the system, otherwise we cannot remove the
            --  entity from the trie because of a call to Get_Name.
            --  This might free Entity.Shared_Name!
            Entity.Ref_Count := Natural'Last;
            Remove (Entity.Declaration.File.Entities, Entity);
            Entity.Ref_Count := 0;

            if Active (Debug_Me) then
               Entity.Shared_Name := Shared;
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

   procedure Set_Next (E, Next : Unshared_Entity_Informations) is
   begin
      E.Next := Next;
   end Set_Next;

   ----------
   -- Next --
   ----------

   function Next (E : Unshared_Entity_Informations)
                  return Unshared_Entity_Informations is
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
      procedure Fast_Reset (File : Source_File);
      --  Free the memory occupied by File and its entities. No attempt is made
      --  to respect the reference counter of the entities, and therefore all
      --  existing Entity_Information become obsolete.
      --  This must only be called when reseting the database itself

      ----------------
      -- Fast_Reset --
      ----------------

      procedure Fast_Reset (File : Source_File) is
         Iter   : Shared_Entities_Hash.Iterator;
         EIS    : Entity_Informations;
         EL     : Entity_Information_List_Access;
      begin
         Get_First (File.Entities, Iter);

         if Active (Assert_Me) then
            Trace (Assert_Me, "Fast_Reset All_Entities for "
                   & Full_Name (Get_Filename (File)).all);
         end if;

         loop
            EIS := Get_Element (Iter);
            exit when EIS = No_Entity_Informations;
            Get_Next (File.Entities, Iter);
            EL := EIS.List;

            for E in Entity_Information_Arrays.First .. Last (EL.all) loop
               if EL.Table (E) /= null then
                  Free  (EL.Table (E).Parent_Types);
                  Free  (EL.Table (E).Primitive_Subprograms);
                  Free  (EL.Table (E).Child_Types);
                  Free  (EL.Table (E).References);
                  Free (EL.Table (E).Called_Entities);
                  Unchecked_Free (EL.Table (E));
               end if;
            end loop;
         end loop;

         Reset (File.All_Entities);
         Reset (File.Entities);
         Free (File.Depends_On);
         Free (File.Depended_On);

         if not Active (Debug_Me) then
            Free (File.Unit_Name);
         end if;
      end Fast_Reset;

      Iter : Files_HTable.Iterator;
      File : Source_File_Item;
   begin
      Get_First (Db.Files, Iter);
      loop
         File := Get_Element (Iter);
         exit when File = null;
         Get_Next (Db.Files, Iter);
         Fast_Reset (File.File);
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
         F := new Source_File_Record;
         F.Db           := Db;
         F.Timestamp    := Timestamp;
         F.Unit_Name    := null;
         F.Depends_On   := Null_Dependency_List;
         F.Depended_On  := Null_Dependency_List;
         F.Scope_Tree_Computed := False;
         F.LI           := LI;
         F.Ref_Count    := 1;

         if File = VFS.No_File then
            F.Name         := Create (Full_Filename => Full_Filename.all);
         else
            F.Name         := File;
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
     (Entities         : in out Entities_Hash.HTable;
      Entity           : Entity_Information;
      Check_Duplicates : Boolean)
   is
      UEI     : Unshared_Entity_Informations;
   begin
      UEI := Get (Entities, Entity.Shared_Name);
      if UEI = null then
         UEI := new Unshared_Entity_Informations_Record'
           (List => new Entity_Information_List'
              (Null_Entity_Information_List),
            Next => null);

         Append (UEI.List.all, Entity);
         Set (Entities, UEI);
         Ref (Entity, "Add");
      else
         if not Check_Duplicates
           or else Find (UEI.List.all, Entity.Declaration) = null
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

   procedure Destroy (D : in out Entity_Informations) is
   begin
      if Active (Ref_Me) then
         Trace (Ref_Me, "Freeing shared name " & D.Name.all);
      end if;
      Free (D.Name);
      Unchecked_Free (D.List);
      Unchecked_Free (D);
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
            Unref (Entity.End_Of_Scope.Caller);
            Unref (Entity.End_Of_Scope.From_Instantiation_At);
            Entity.End_Of_Scope := (Location, null, null, Kind);
         else
            Add_Reference
              (Entity,
               Location => Location,
               Kind     => Kind);
         end if;
      else
         Unref (Entity.End_Of_Scope.Caller);
         Unref (Entity.End_Of_Scope.From_Instantiation_At);
         Entity.End_Of_Scope := (Location, null, null, Kind);
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
      From_Instantiation_At : Entity_Information := null) is
   begin
      Assert (Assert_Me, Location.File /= null, "Invalid file in reference");
      if Active (Add_Reference_Force_Unique) then
         for R in
           Entity_Reference_Arrays.First .. Last (Entity.References)
         loop
            if Entity.References.Table (R).Location = Location then
               return;
            end if;
         end loop;
      end if;

      Append
        (Entity.References, (Location, null, From_Instantiation_At, Kind));
      Ref (From_Instantiation_At, "add_reference.from_instantiation");
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
     (Name         : String;
      File         : Source_File;
      Line         : Natural;
      Column       : Natural;
      Allow_Create : Boolean := True) return Entity_Information
   is
      EIS : Entity_Informations;
      E   : Entity_Information;
   begin
      Assert (Assert_Me, Name /= "", "No name specified for Get_Or_Create");
      if Active (Ref_Me) then
         Trace (Ref_Me, "Get_Or_Create entity name=" & Name
                & " File=" & Base_Name (Get_Filename (File))
                & " Line=" & Line'Img);
      end if;

      EIS := Get (File.Entities, Name'Unrestricted_Access);

      if EIS = No_Entity_Informations then
         if Allow_Create then
            EIS := new Entity_Informations_Record'
              (Name => new String'(Name),
               List => new Entity_Information_List'
                 (Null_Entity_Information_List),
               Next => null);
            Set (File.Entities, EIS);
         end if;
      else
         E := Find (EIS.List.all, (File, Line, Column_Type (Column)));
      end if;

      if E = null and then Allow_Create then
         E := new Entity_Information_Record'
           (Shared_Name           => EIS.Name,
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
            Called_Entities       => Null_Entity_Information_List,
            Primitive_Subprograms => Null_Entity_Information_List,
            Child_Types           => Null_Entity_Information_List,
            References            => Null_Entity_Reference_List,
            Is_Valid              => True,
            Ref_Count             => 1);
         Append (EIS.List.all, E);

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

   ---------------------------
   -- From_Instantiation_At --
   ---------------------------

   function From_Instantiation_At
     (Ref : Entity_Reference) return Entity_Information is
   begin
      if Ref.Entity /= null
        and then Ref.Entity.References /= Null_Entity_Reference_List
        and then Ref.Index <= Last (Ref.Entity.References)
      then
         return Ref.Entity.References.Table (Ref.Index).From_Instantiation_At;
      else
         return null;
      end if;
   end From_Instantiation_At;

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
     (Handler   : access LI_Handler_Record;
      Languages : access Language_Handlers.Language_Handler_Record'Class;
      File_Name : VFS.Virtual_File;
      Result    : out Language.Construct_List)
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
      return Entity1.Shared_Name.all < Entity2.Shared_Name.all;
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

      ----------
      -- Move --
      ----------

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

      --------
      -- Lt --
      --------

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

end Entities;
