-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2004                       --
--                            ACT-Europe                             --
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

with Ada.Calendar; use Ada.Calendar;
with Ada.Unchecked_Deallocation;
with VFS;          use VFS;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with Projects;     use Projects;
with Traces;       use Traces;
with Entities.Queries; use Entities.Queries;

package body Entities is
   Assert_Me : constant Debug_Handle := Create ("Entities.Assert", Off);

   Debug_Me  : constant Debug_Handle := Create ("Entities.Debug", Off);
   --  If True, no memory is freed, this makes the structure more robust and
   --  easier to debug, but will of course use more memory.


   Manage_Global_Entities_Table : constant Boolean := False;
   --  True if we should try and create a global table for all entities
   --  defined in the project. The same information can be computed by
   --  traversing all files and their .Entities tables, and this saves some
   --  memory and time when creating the structure.

   use Entities_Tries;
   use Files_HTable;
   use LI_HTable;
   use Entity_Information_Arrays;
   use Source_File_Arrays;
   use Entity_Reference_Arrays;
   use Dependency_Arrays;

   function String_Hash is new HTables.Hash (HTable_Header);

   procedure Reset (Entity : Entity_Information; File : Source_File);
   --  Remove all references to File in Entity.

   function Find
     (E : Dependency_List; File : Source_File)
      return Dependency_Arrays.Index_Type;
   --  Find a specific file in the list of dependencies

   procedure Isolate (Entity : in out Entity_Information);
   --  Isolate the entity from the rest of the LI structure. This should be
   --  used when we are removing the entity from the internal tables, but the
   --  user has kept a handle on it, to avoid memory leaks.

   procedure Cleanup_All_Entities_Field (File : Source_File);
   --  Cleanup the All_Entities field for all the files in the database,
   --  to remove references to File.

   procedure Add_All_Entities
     (File : Source_File; Entity : Entity_Information);
   --  Add Entity to the list of All_Entities known in File, if Entity is not
   --  there yet and is not declared in File.

   procedure Reset_All_Entities (File : Source_File);
   --  Reset all the entities declared in File

   procedure Free_All_Entities (File : Source_File);
   --  Free all entities declared in File, and remove them from internal
   --  tables. This ensures that the entities that are still referenced
   --  externally will still be usable.

   procedure Free_If_Partial
     (Entity : in out Entity_Information; Not_In_File : Source_File);
   --  Free the memory occupied by Entity if it is a partial entity, and if
   --  it isn't declared in Not_In_File.

   Is_Subprogram_Entity : constant array (E_Kinds) of Boolean :=
     (Procedure_Kind        => True,
      Function_Or_Operator  => True,
      Entry_Or_Entry_Family => True,
      others                => False);
   --  This table should contain true if the corresponding element is
   --  considered as a subprogram (see Is_Subprogram)

   Is_End_Reference_Array : constant array (Reference_Kind) of Boolean :=
     (End_Of_Spec           => True,
      End_Of_Body           => True,
      others                => False);
   --  True if the matching entity indicates an end-of-scope (end of subprogram
   --  declaration, end of record definition, ...)

   Is_Real_Reference_Array : constant array (Reference_Kind) of Boolean :=
     (Reference                                => True,
      Instantiation_Reference                  => True,
      Modification                             => True,
      Body_Entity                              => True,
      Completion_Of_Private_Or_Incomplete_Type => True,
      Type_Extension                           => True,
      Label                                    => True,
      With_Line                                => True,
      others                                   => False);
   --  See Is_Real_Reference

   Is_Parameter_Array : constant array (Reference_Kind) of Boolean :=
     (Subprogram_In_Parameter                  => True,
      Subprogram_In_Out_Parameter              => True,
      Subprogram_Out_Parameter                 => True,
      Subprogram_Access_Parameter              => True,
      others                                   => False);

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
      return Is_Real_Reference_Array (Kind);
   end Is_Real_Reference;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (D : Entity_Information) return String_Access is
   begin
      if D = null then
         return null;
      else
         return D.Name;
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
            Trace (Assert_Me, "Freeing " & Base_Name (Get_Filename (F)));

            Unchecked_Free (F);
         end if;
      end if;
   end Unref;

   --------------------------------
   -- Cleanup_All_Entities_Field --
   --------------------------------

   procedure Cleanup_All_Entities_Field (File : Source_File) is
      procedure Clean_All_Entities_In_File (Dep : Source_File);
      --  Clean the All_Entities list in Dep

      procedure Clean_All_Entities_In_File (Dep : Source_File) is
         Iter : Entities_Tries.Iterator := Start (Dep.All_Entities, "");
         EL   : Entity_Information_List_Access;
      begin
         loop
            EL := Get (Iter);
            exit when EL = null;

            Next (Iter);

            for E in
               reverse Entity_Information_Arrays.First .. Last (EL.all)
            loop
               if EL.Table (E).Declaration.File = File then
                  if Length (EL.all) = 1 then
                     Remove (Dep.All_Entities, Get_Name (EL.Table (E)).all);
                  else
                     Remove (EL.all, E);
                  end if;
               end if;
            end loop;
         end loop;

         Free (Iter);
      end Clean_All_Entities_In_File;

   begin
      for F in Dependency_Arrays.First .. Last (File.Depended_On) loop
         Clean_All_Entities_In_File (File.Depended_On.Table (F).File);
      end loop;

      for F in Dependency_Arrays.First .. Last (File.Depends_On) loop
         Clean_All_Entities_In_File (File.Depends_On.Table (F).File);
      end loop;
   end Cleanup_All_Entities_Field;

   ----------------------
   -- Add_All_Entities --
   ----------------------

   procedure Add_All_Entities
     (File : Source_File; Entity : Entity_Information)
   is
      Predef : constant Source_File := Get_Predefined_File (File.Db);
   begin
      if Entity.Declaration.File /= File
        and then Entity.Declaration.File /= Predef
        and then File /= Predef
      then
         --  ??? This might be costly, but we need to ensure there is a proper
         --  reference between the two files, for proper clean up
         Add_Depends_On (File, Entity.Declaration.File);

         Add (File.All_Entities, Entity, Check_Duplicates => True);
      end if;
   end Add_All_Entities;

   ------------------------
   -- Reset_All_Entities --
   ------------------------

   procedure Reset_All_Entities (File : Source_File) is
      Iter : Entities_Tries.Iterator := Start (File.All_Entities, "");
      EL   : Entity_Information_List_Access;
   begin
      loop
         EL := Get (Iter);
         exit when EL = null;

         for E in reverse Entity_Information_Arrays.First .. Last (EL.all) loop
            Reset (EL.Table (E), File);
         end loop;

         Next (Iter);
      end loop;
      Free (Iter);
   end Reset_All_Entities;

   -----------------------
   -- Free_All_Entities --
   -----------------------

   procedure Free_All_Entities (File : Source_File) is
      Iter : Entities_Tries.Iterator := Start (File.Entities, "");
      EL   : Entity_Information_List_Access;
      Entity : Entity_Information;
   begin
      loop
         EL := Get (Iter);
         exit when EL = null;

         for E in reverse Entity_Information_Arrays.First .. Last (EL.all) loop
            Entity := EL.Table (E);

            if Manage_Global_Entities_Table then
               Remove (File.Db.Entities, Entity);
            end if;

            --  No need to remove from the list itself, since it will be
            --  freed anyway when it is removed from the trie
            if E = Entity_Information_Arrays.First then
               Remove (File.Entities, Get_Name (Entity).all);
            end if;

            if Entity.Ref_Count > 1 then
               Isolate (Entity);
            end if;

            Unref (Entity);
         end loop;

         Next (Iter);
      end loop;
      Free (Iter);
   end Free_All_Entities;

   -----------
   -- Reset --
   -----------

   procedure Reset (LI : LI_File) is
   begin
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

   -----------
   -- Reset --
   -----------

   procedure Reset (File : Source_File) is
   begin
      Cleanup_All_Entities_Field (File);
      Reset_All_Entities (File);

      Clear (File.All_Entities);

      Free_All_Entities (File);

      for F in Dependency_Arrays.First .. Last (File.Depended_On) loop
         Remove (File.Depended_On.Table (F).File.Depends_On, File);
      end loop;

      for F in Dependency_Arrays.First .. Last (File.Depends_On) loop
         Remove (File.Depends_On.Table (F).File.Depended_On, File);
      end loop;

      Clear (File.Entities);
      Free (File.Depends_On);
      Free (File.Depended_On);
      Destroy (File.Scope);
      File.Is_Valid := False;
   end Reset;

   -------------
   -- Isolate --
   -------------

   procedure Isolate (Entity : in out Entity_Information) is
   begin
      Entity.End_Of_Scope    := No_Entity_Reference;
      Entity.Pointed_Type    := null;
      Entity.Returned_Type   := null;
      Entity.Primitive_Op_Of := null;
      Entity.Rename          := null;
      Free (Entity.Parent_Types);
      Free (Entity.Primitive_Subprograms);
      Free (Entity.Child_Types);
      Free (Entity.References);
   end Isolate;

   -----------
   -- Reset --
   -----------

   procedure Reset (Entity : Entity_Information; File : Source_File) is
      procedure Check_And_Remove (E : in out Entity_Information);
      procedure Check_And_Remove (E : in out Entity_Information_List);
      procedure Check_And_Remove (E : in out Entity_Reference_List);
      procedure Check_And_Remove (E : in out Entity_Reference);
      --  Remove all references to File in E

      procedure Check_And_Remove (E : in out Entity_Information) is
      begin
         if E /= null and then E.Declaration.File = File then
            E := null;
         end if;
      end Check_And_Remove;

      procedure Check_And_Remove (E : in out Entity_Information_List) is
      begin
         for J in reverse Entity_Information_Arrays.First .. Last (E) loop
            Assert (Assert_Me, E.Table (J) /= null, "Invalid entity in list");
            Assert
              (Assert_Me, E.Table (J).Declaration.File /= null,
               "Invalid declaration");
            if E.Table (J).Declaration.File = File then
               Remove (E, J);
            end if;
         end loop;
      end Check_And_Remove;

      procedure Check_And_Remove (E : in out Entity_Reference) is
      begin
         if E.Location.File = File then
            E := No_Entity_Reference;
         end if;
      end Check_And_Remove;

      procedure Check_And_Remove (E : in out Entity_Reference_List) is
      begin
         for R in reverse Entity_Reference_Arrays.First .. Last (E) loop
            if E.Table (R).Location.File = File then
               Remove (E, R);
            end if;
         end loop;
      end Check_And_Remove;

   begin
      Assert (Assert_Me, Entity.Declaration.File /= File,
              "Entity should have been on .Entities list, not .All_Entities");

      Check_And_Remove (Entity.End_Of_Scope);
      Check_And_Remove (Entity.Parent_Types);
      Check_And_Remove (Entity.Pointed_Type);
      Check_And_Remove (Entity.Returned_Type);
      Check_And_Remove (Entity.Primitive_Op_Of);
      Check_And_Remove (Entity.Rename);
      Check_And_Remove (Entity.Primitive_Subprograms);
      Check_And_Remove (Entity.Child_Types);
      Check_And_Remove (Entity.References);
   end Reset;

   ------------
   -- Remove --
   ------------

   procedure Remove  (D : in out Entities_Tries.Trie_Tree;
                      E : Entity_Information)
   is
      EL : constant Entity_Information_List_Access :=
        Get (D, Get_Name (E).all);
   begin
      if EL /= null then
         if Length (EL.all) = 1 then
            Remove (D, Get_Name (E).all);
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

         Unchecked_Free (Tree);
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
            Free (LI.Files);
            Unchecked_Free (LI);
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

   ---------------------
   -- Free_If_Partial --
   ---------------------

   procedure Free_If_Partial
     (Entity : in out Entity_Information; Not_In_File : Source_File) is
   begin
      if Entity /= null
        and then Entity.Declaration.File /= Not_In_File
        and then Is_Partial_Entity (Entity)
      then
         Unref (Entity);
      end if;
      Entity := null;
   end Free_If_Partial;

   -----------
   -- Unref --
   -----------

   procedure Unref (Entity : in out Entity_Information) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Entity_Information_Record'Class, Entity_Information);
      Tmp : String_Access;
      F   : Source_File;
   begin
      if Entity /= null then
         Assert (Assert_Me, Entity.Ref_Count > 0, "too many calls to unref");
         Entity.Ref_Count := Entity.Ref_Count - 1;
         if Entity.Ref_Count = 0 then
            F := Entity.Declaration.File;

            Free (Entity.Parent_Types);
            Free (Entity.Primitive_Subprograms);
            Free (Entity.Child_Types);
            Free (Entity.References);

            Free_If_Partial (Entity.Pointed_Type, F);
            Free_If_Partial (Entity.Returned_Type, F);
            Free_If_Partial (Entity.Primitive_Op_Of, F);
            Free_If_Partial (Entity.Rename, F);

            --  If we are debugging, we do not free the memory, to keep a
            --  debuggable structure.
            if Active (Debug_Me) then
               Tmp := Entity.Name;
               Entity.Name := new String'
                 (Entity.Name.all
                  & ':' & Base_Name (Get_Filename (Entity.Declaration.File)));
               Free (Tmp);
            else
               Free (Entity.Name);
               Unchecked_Free (Entity);
            end if;
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

   function Create return Entities_Database is
   begin
      return new Entities_Database_Record;
   end Create;

   -------------------------------
   -- Register_Language_Handler --
   -------------------------------

   procedure Register_Language_Handler
     (Db : Entities_Database; Handler : LI_Handler) is
   begin
      if Db.ALI_Handlers = null then
         Db.ALI_Handlers := Handler;
      else
         Db.CPP_Handlers := Handler;
      end if;
   end Register_Language_Handler;

   ----------
   -- Hash --
   ----------

   function Hash (Key : VFS.Virtual_File) return HTable_Header is
   begin
      return String_Hash (Full_Name (Key).all);
   end Hash;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Db : in out Entities_Database) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Entities_Database_Record, Entities_Database);
      Iter : Files_HTable.Iterator;
      File : Source_File;
   begin
      Get_First (Db.Files, Iter);
      loop
         File := Get_Element (Iter);
         exit when File = null;
         Reset (File);
         Get_Next (Db.Files, Iter);
      end loop;

      Reset (Db.Files);
      Reset (Db.LIs);

      if Manage_Global_Entities_Table then
         Clear (Db.Entities);
      end if;
      Unchecked_Free (Db);
   end Destroy;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Db           : Entities_Database;
      File         : VFS.Virtual_File;
      LI           : LI_File := null;
      Timestamp    : Ada.Calendar.Time := VFS.No_Time;
      Allow_Create : Boolean := True) return Source_File
   is
      S : Source_File := Get (Db.Files, File);
   begin
      if S = null and then not Allow_Create then
         null;

      elsif S = null then
         S := new Source_File_Record'
           (Db             => Db,
            Timestamp      => Timestamp,
            Name           => File,
            Entities       => Empty_Trie_Tree,
            Depends_On     => Null_Dependency_List,
            Depended_On    => Null_Dependency_List,
            All_Entities   => Empty_Trie_Tree,
            Scope          => null,
            LI             => LI,
            Is_Valid       => True,
            Ref_Count      => 1);
         Set (Db.Files, File, S);

         if LI /= null then
            Append (LI.Files, S);
         end if;

      else
         if Timestamp /= No_Time then
            S.Timestamp := Timestamp;
         end if;

         if LI /= null and then S.LI = null then
            S.LI := LI;
            Append (LI.Files, S);
         end if;
      end if;

      --  ??? Should we reset this. Probably not, unless we have a parameter
      --  that tells us to do so.
      --  S.Is_Valid := True;

      return S;
   end Get_Or_Create;

   ----------
   -- Find --
   ----------

   function Find
     (List   : Entity_Information_List;
      Loc    : File_Location) return Entity_Information is
   begin
      for L in Entity_Information_Arrays.First .. Last (List) loop
         if List.Table (L).Declaration = Loc then
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
      Explicit_Dependency : Boolean := False) is
   begin
      Assert (Assert_Me, Depends_On /= File, "File cannot depend on itself");

      if Find (File.Depends_On, Depends_On) < Dependency_Arrays.First then
         Append (File.Depends_On, (Depends_On, Explicit_Dependency));
         Append (Depends_On.Depended_On, (File, Explicit_Dependency));
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
      EL : Entity_Information_List_Access :=
        Get (Entities, Get_Name (Entity).all);
      Is_Null : constant Boolean := (EL = null);
   begin
      if Is_Null then
         EL := new Entity_Information_List'(Null_Entity_Information_List);
      end if;

      if not Check_Duplicates
        or else Find (EL.all, Entity.Declaration) = null
      then
         Append (EL.all, Entity);

         if Is_Null then
            Insert (Entities, EL);
         end if;
      end if;
   end Add;

   ----------------------
   -- Update_Timestamp --
   ----------------------

   procedure Update_Timestamp
     (LI : LI_File; Timestamp : Ada.Calendar.Time := VFS.No_Time) is
   begin
      LI.Timestamp := Timestamp;
   end Update_Timestamp;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Db        : Entities_Database;
      File      : VFS.Virtual_File;
      Project   : Projects.Project_Type) return LI_File
   is
      L : LI_File := Get (Db.LIs, File);
   begin
      Assert (Assert_Me, File /= VFS.No_File, "No LI filename");
      Assert (Assert_Me, Project /= No_Project, "No project specified");
      if L = null then
         L := new LI_File_Record'
           (Db        => Db,
            Name      => File,
            Timestamp => No_Time,
            Project   => Project,
            Files     => Null_Source_File_List,
            Ref_Count => 1);
         Set (Db.LIs, File, L);
      end if;

      return L;
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
         Unchecked_Free (D);
      end if;
   end Destroy;

   --------------
   -- Set_Kind --
   --------------

   procedure Set_Kind (Entity : Entity_Information; Kind : E_Kind) is
   begin
      Entity.Kind := Kind;
   end Set_Kind;

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
      Entity.End_Of_Scope := (Location, Kind);
      Add_All_Entities (Location.File, Entity);
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
      Add_All_Entities (Renaming_Of.Declaration.File, Entity);
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
      Append (Entity.References, (Location, Kind));
      Add_All_Entities (Location.File, Entity);
   end Add_Reference;

   -----------------
   -- Set_Type_Of --
   -----------------

   procedure Set_Type_Of
     (Entity : Entity_Information; Is_Of_Type : Entity_Information)
   is
   begin
      Assert (Assert_Me, Is_Of_Type /= null, "Invalid type for entity");
      Append (Entity.Parent_Types, Is_Of_Type);

      if Entity.Kind.Is_Type then
         Append (Is_Of_Type.Child_Types, Entity);
      end if;

      Add_All_Entities (Entity.Declaration.File, Is_Of_Type);
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
      Add_All_Entities (Entity.Declaration.File, Primitive);
   end Add_Primitive_Subprogram;

   ----------------------
   -- Set_Pointed_Type --
   ----------------------

   procedure Set_Pointed_Type
     (Entity : Entity_Information; Points_To : Entity_Information) is
   begin
      Assert (Assert_Me, Points_To /= null, "Invalid pointed type");
      Entity.Pointed_Type := Points_To;
      Add_All_Entities (Points_To.Declaration.File, Entity);
   end Set_Pointed_Type;

   -----------------------
   -- Set_Returned_Type --
   -----------------------

   procedure Set_Returned_Type
     (Entity : Entity_Information; Returns : Entity_Information) is
   begin
      Assert (Assert_Me, Returns /= null, "Invalid returned type");
      Entity.Returned_Type := Returns;
      Add_All_Entities (Returns.Declaration.File, Entity);
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
     (Db           : Entities_Database;
      Name         : String := "";
      File         : Source_File;
      Line         : Natural;
      Column       : Natural;
      Allow_Create : Boolean := True) return Entity_Information
   is
      --  Speed up the search by checking in the file itself. However, we'll
      --  have to add the new entity to the global entities table as well.
      EL  : Entity_Information_List_Access;
      EL2 : Entity_Information_List_Access;
      E   : Entity_Information;
      Is_Null : Boolean;
      Is_Partial_Entity : constant Boolean := Name'Length = 0;
   begin
      if Is_Partial_Entity then
         E := null;
      else
         EL := Get (File.Entities, Name);
         Is_Null := EL = null;

         if Is_Null then
            if Allow_Create then
               EL := new Entity_Information_List'
                 (Null_Entity_Information_List);
            end if;
         else
            E := Find (EL.all, (File, Line, Column));
         end if;
      end if;

      if E = null and then Allow_Create then
         E := new Entity_Information_Record'
           (Name                  => new String'(Name),
            Kind                  => Unresolved_Entity_Kind,
            Declaration           => (File, Line, Column),
            End_Of_Scope          => No_Entity_Reference,
            Parent_Types          => Null_Entity_Information_List,
            Pointed_Type          => null,
            Returned_Type         => null,
            Primitive_Op_Of       => null,
            Rename                => null,
            Primitive_Subprograms => Null_Entity_Information_List,
            Child_Types           => Null_Entity_Information_List,
            References            => Null_Entity_Reference_List,
            Ref_Count             => 1);

         if not Is_Partial_Entity then
            Append (EL.all, E);

            if Is_Null then
               Insert (File.Entities, EL);
            end if;

            if Manage_Global_Entities_Table then
               EL2     := Get (Db.Entities, Name);
               Is_Null := (EL2 = null);

               if EL2 = null then
                  EL2 := new Entity_Information_List'
                    (Null_Entity_Information_List);
               end if;

               Append (EL2.all, E);

               if Is_Null then
                  Insert (Db.Entities, EL2);
               end if;
            end if;
         end if;
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
      --  ??? Hard coded, needs extensions in Language_Handlers.Glide

      if File_Extension (Source_Filename) = ".c"
        or else File_Extension (Source_Filename) = ".h"
        or else File_Extension (Source_Filename) = ".cc"
        or else File_Extension (Source_Filename) = ".cpp"
      then
         return Db.CPP_Handlers;
      else
         return Db.ALI_Handlers;
      end if;
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
         F := Get_Source_Info
           (Get_LI_Handler (File.Db, Get_Filename (File)),
            Get_Filename (File),
            File_Has_No_LI_Report);
      end if;
   end Update_Xref;

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
      return Loc.Column;
   end Get_Column;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Entity : Entity_Information) return String is
   begin
      return Entity.Name.all;
   end Get_Name;

   -------------------------
   -- Check_LI_And_Source --
   -------------------------

   function Check_LI_And_Source
     (LI : LI_File; Source : Virtual_File) return Boolean is
   begin
      for F in Source_File_Arrays.First .. Last (LI.Files) loop
         if Get_Filename (LI.Files.Table (F)) = Source then
            return True;
         end if;
      end loop;

      return False;
   end Check_LI_And_Source;

   ----------------------------
   -- Resolve_Partial_Entity --
   ----------------------------

   procedure Resolve_Partial_Entity (Entity : in out Entity_Information) is
      Result : Entity_Information;
      Status : Find_Decl_Or_Body_Query_Status;
   begin
      if Entity /= null and then Is_Partial_Entity (Entity) then
         Update_Xref (Entity.Declaration.File);

         --  We must search in declarations as well, since for a renaming of
         --  body in Ada we might have the following (see rename/rename.adb):
         --     32i4 X{integer} 33m24 50m4
         --     33i4 Y=33:24{integer} 51r4

         Find_Declaration
           (Db              => Get_Database (Get_File (Entity.Declaration)),
            File_Name       => Get_Filename (Get_File (Entity.Declaration)),
            Entity_Name     => "",
            Line            => Get_Line (Entity.Declaration),
            Column          => Get_Column (Entity.Declaration),
            Entity          => Result,
            Status          => Status,
            Check_Decl_Only => False);

         if Status = Success then
            Unref (Entity);
            Entity := Result;
         end if;
      end if;
   end Resolve_Partial_Entity;

   -----------------------
   -- Is_Partial_Entity --
   -----------------------

   function Is_Partial_Entity (Entity : Entity_Information) return Boolean is
   begin
      return Entity.Name'Length = 0;
   end Is_Partial_Entity;

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

   -----------------------
   -- Update_Time_Stamp --
   -----------------------

   procedure Update_Time_Stamp
     (File : Source_File; Timestamp : Ada.Calendar.Time := VFS.No_Time) is
   begin
      File.Timestamp := Timestamp;
   end Update_Time_Stamp;

   --------------------
   -- Get_Time_Stamp --
   --------------------

   function Get_Time_Stamp (File : Source_File) return Ada.Calendar.Time is
   begin
      return File.Timestamp;
   end Get_Time_Stamp;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind (Entity : Entity_Information) return E_Kind is
   begin
      return Entity.Kind;
   end Get_Kind;

end Entities;
