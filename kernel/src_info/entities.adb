-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
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

package body Entities is
   Assert_Me : constant Debug_Handle := Create ("Entities.Assert", Off);

   use Entities_Tries;
   use Files_HTable;
   use LI_HTable;
   use Entity_Information_Arrays;
   use Source_File_Arrays;
   use Entity_Reference_Arrays;
   use Dependency_Arrays;

   function String_Hash is new HTables.Hash (HTable_Header);

   procedure Add_Depended_On
     (File : Source_File; Depended_On  : Source_File);
   --  Add a new file that depends on File. We check first that Depended_On
   --  is not in the list. File is not added to the list of dependencies for
   --  Dependend_On, see Add_Depends_On instead.

   procedure Add_Entity
     (File : Source_File; Entity : Entity_Information);
   --  Add a new entity to the list of entities for this file. No check is done
   --  whether the entity is already there or not

   procedure Reset (Entity : Entity_Information; File : Source_File);
   --  Remove all references to File in Entity.

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

   ----------------------
   -- Is_End_Reference --
   ----------------------

   function Is_End_Reference (Kind : Reference_Kind) return Boolean is
   begin
      return Is_End_Reference_Array (Kind);
   end Is_End_Reference;

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
         return Get_Name (D.Table (Entity_Information_Arrays.First));
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
            Unchecked_Free (F);
         end if;
      end if;
   end Unref;

   --------------------------------
   -- Cleanup_All_Entities_Field --
   --------------------------------

   procedure Cleanup_All_Entities_Field (File : Source_File) is
      Iter : Entities_Tries.Iterator;
      EL   : Entity_Information_List_Access;
   begin
      for F in Source_File_Arrays.First .. Last (File.Depended_On) loop
         Iter := Start (File.Depended_On.Table (F).All_Entities, "");
         loop
            EL := Get (Iter);
            exit when EL = null;

            Next (Iter);

            for E in Entity_Information_Arrays.First .. Last (EL.all) loop
               if EL.Table (E).Declaration.File = File then
                  Remove
                    (File.Depended_On.Table (F).All_Entities, EL.Table (E));
               end if;
            end loop;
         end loop;
         Free (Iter);
      end loop;
   end Cleanup_All_Entities_Field;

   ----------------------
   -- Add_All_Entities --
   ----------------------

   procedure Add_All_Entities
     (File : Source_File; Entity : Entity_Information) is
   begin
      if Entity.Declaration.File /= File then
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

         for E in Entity_Information_Arrays.First .. Last (EL.all) loop
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
   begin
      loop
         EL := Get (Iter);
         exit when EL = null;

         for E in Entity_Information_Arrays.First .. Last (EL.all) loop
            Remove (File.Db.Entities, EL.Table (E));

            if EL.Table (E).Ref_Count > 1 then
               Isolate (EL.Table (E));
            end if;

            Unref (EL.Table (E));
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
   begin
      for L in Dependency_Arrays.First .. Last (E) loop
         if E.Table (L).File = File then
            Remove (E, L);
            exit;
         end if;
      end loop;
   end Remove;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : Source_File) is
   begin
      Cleanup_All_Entities_Field (File);

      for F in Source_File_Arrays.First .. Last (File.Depended_On) loop
         Remove (File.Depended_On.Table (F).Depends_On, File);
      end loop;

      for F in Dependency_Arrays.First .. Last (File.Depends_On) loop
         Remove (File.Depends_On.Table (F).File.Depended_On, File);
      end loop;

      Reset_All_Entities (File);
      Free_All_Entities (File);

      Clear (File.Entities);
      Free (File.Depends_On);
      Free (File.Depended_On);
      Destroy (File.Scope);
      Clear (File.All_Entities);
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

   -----------
   -- Unref --
   -----------

   procedure Unref (Entity : in out Entity_Information) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Entity_Information_Record'Class, Entity_Information);
   begin
      if Entity /= null then
         Assert (Assert_Me, Entity.Ref_Count > 0, "too many calls to unref");
         Entity.Ref_Count := Entity.Ref_Count - 1;
         if Entity.Ref_Count = 0 then
            Free (Entity.Name);
            Free (Entity.Parent_Types);
            Free (Entity.Primitive_Subprograms);
            Free (Entity.Child_Types);
            Free (Entity.References);
            Unchecked_Free (Entity);
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
      Db.Handlers := Handler;
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
   begin
      Reset (Db.Files);
      Clear (Db.Entities);
      Reset (Db.LIs);
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
            Depended_On    => Null_Source_File_List,
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
      Append (File.Depends_On, (Depends_On, Explicit_Dependency));
      Add_Depended_On (Depends_On, File);
   end Add_Depends_On;

   ---------------------
   -- Add_Depended_On --
   ---------------------

   procedure Add_Depended_On (File : Source_File; Depended_On : Source_File) is
   begin
      if Find (File.Depended_On, Depended_On) < Source_File_Arrays.First then
         Append (File.Depended_On, Depended_On);
      end if;
   end Add_Depended_On;

   ----------------
   -- Add_Entity --
   ----------------

   procedure Add_Entity (File : Source_File; Entity : Entity_Information) is
   begin
      Add (File.Entities, Entity, Check_Duplicates => False);
   end Add_Entity;

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
      if EL = null then
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
      Append (Entity.Parent_Types, Is_Of_Type);

      if Entity.Kind.Is_Type then
         Append (Is_Of_Type.Child_Types, Entity);
      end if;

      Add_All_Entities (Is_Of_Type.Declaration.File, Entity);
   end Set_Type_Of;

   ------------------------------
   -- Add_Primitive_Subprogram --
   ------------------------------

   procedure Add_Primitive_Subprogram
     (Entity : Entity_Information; Primitive : Entity_Information) is
   begin
      Append (Entity.Primitive_Subprograms, Primitive);
      Primitive.Primitive_Op_Of := Entity;
      Add_All_Entities (Primitive.Declaration.File, Entity);
   end Add_Primitive_Subprogram;

   ----------------------
   -- Set_Pointed_Type --
   ----------------------

   procedure Set_Pointed_Type
     (Entity : Entity_Information; Points_To : Entity_Information) is
   begin
      Entity.Pointed_Type := Points_To;
      Add_All_Entities (Points_To.Declaration.File, Entity);
   end Set_Pointed_Type;

   -----------------------
   -- Set_Returned_Type --
   -----------------------

   procedure Set_Returned_Type
     (Entity : Entity_Information; Returns : Entity_Information) is
   begin
      Entity.Returned_Type := Returns;
      Add_All_Entities (Returns.Declaration.File, Entity);
   end Set_Returned_Type;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Db     : Entities_Database;
      Name   : String;
      File   : Source_File;
      Line   : Natural;
      Column : Natural) return Entity_Information
   is
      EL : Entity_Information_List_Access := Get (Db.Entities, Name);
      E  : Entity_Information;
      Is_Null : constant Boolean := (EL = null);
   begin
      if EL = null then
         EL := new Entity_Information_List'(Null_Entity_Information_List);
      end if;

      E := Find (EL.all, (File, Line, Column));

      if E = null then
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
         Append (EL.all, E);

         Add_Entity (File, E);
      end if;

      if Is_Null then
         Insert (Db.Entities, EL);
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
         Db.Predefined_File := Get_Or_Create (Db, VFS.No_File, null);
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
      Source_Filename : VFS.Virtual_File) return LI_Handler
   is
      pragma Unreferenced (Source_Filename);
   begin
      return Db.Handlers;
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

   procedure Update_Xref (File : Source_File) is
      F : Source_File;
      pragma Unreferenced (F);
   begin
      if File /= null then
         F := Get_Source_Info
           (Get_LI_Handler (File.Db, Get_Filename (File)),
            Get_Filename (File));
      end if;
   end Update_Xref;

end Entities;
