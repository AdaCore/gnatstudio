with Ada.Calendar; use Ada.Calendar;
with Ada.Unchecked_Deallocation;
with Basic_Types;
with VFS;          use VFS;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with Traces;       use Traces;

package body Entities is

   Me : constant Debug_Handle := Create ("Entities");

   use Entities_Tries;
   use Files_HTable;
   use LI_HTable;

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

   procedure Add_And_Grow is new Basic_Types.Add_And_Grow
     (Data       => Source_File,
      Index      => Natural,
      Arr        => Source_File_Array,
      Arr_Access => Source_File_Array_Access,
      Multiplier => 1);

   procedure Add_And_Grow is new Basic_Types.Add_And_Grow
     (Data       => Entity_Information,
      Index      => Natural,
      Arr        => Entity_Information_Array,
      Arr_Access => Entity_Information_Array_Access,
      Multiplier => 1);

   procedure Add_And_Grow is new Basic_Types.Add_And_Grow
     (Data       => File_Location,
      Index      => Natural,
      Arr        => File_Location_Array,
      Arr_Access => File_Location_Array_Access,
      Multiplier => 1);

   procedure Reset (Entity : Entity_Information; File : Source_File);
   --  Remove all references to File in Entity.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Entity_Information_Array, Entity_Information_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (File_Location_Array, File_Location_Array_Access);

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
        or else D.List = null
        or else D.List'First >= D.Past_Last
      then
         return null;
      else
         return Get_Name (D.List (D.List'First));
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

   -----------
   -- Reset --
   -----------

   procedure Reset (File : Source_File) is
      Iter : Entities_Tries.Iterator;
      EL   : Entity_Information_List_Access;
   begin
      --  For all files that references some entity of File, no need to keep
      --  a reference
      if File.Depended_On.List /= null then
         for F in File.Depended_On.List'First ..
           File.Depended_On.Past_Last - 1
         loop
            Iter := Start (File.Depended_On.List (F).All_Entities, "");
            loop
               EL := Get (Iter);
               exit when EL = null;

               Next (Iter);

               for E in EL.List'First .. EL.Past_Last - 1 loop
                  if EL.List (E).Declaration.File = File then
                     Remove (File.Depended_On.List (F).All_Entities,
                             EL.List (E));
                  end if;
               end loop;
            end loop;
            Free (Iter);

            Remove (File.Depended_On.List (F).Depends_On, File);
         end loop;
      end if;

      if File.Depends_On.List /= null then
         for F in File.Depends_On.List'First ..
           File.Depends_On.Past_Last - 1
         loop
            Remove (File.Depends_On.List (F).Depended_On, File);
         end loop;
      end if;

      --  For all other entities referenced in File, make sure they no longer
      --  access File through one of their fields

      Iter := Start (File.All_Entities, "");
      loop
         EL := Get (Iter);
         exit when EL = null;

         for E in reverse EL.List'First .. EL.Past_Last - 1 loop
            Reset (EL.List (E), File);
         end loop;

         Next (Iter);
      end loop;
      Free (Iter);

      --  Free all the entities declared in this file. The user might of
      --  course still have a handle on these.
      --  This must be done last, since the tests above will access these
      --  entities.

      Iter := Start (File.Entities, "");
      loop
         EL := Get (Iter);
         exit when EL = null;

         for E in reverse EL.List'First .. EL.Past_Last - 1 loop
            Remove (File.LI.Db.Entities, EL.List (E));

            if EL.List (E).Ref_Count > 1 then
               EL.List (E).End_Of_Scope    := No_File_Location;
               Destroy (EL.List (E).Parent_Types);
               EL.List (E).Pointed_Type    := null;
               EL.List (E).Returned_Type   := null;
               EL.List (E).Primitive_Op_Of := null;
               EL.List (E).Rename          := null;
               Destroy (EL.List (E).Primitive_Subprograms);
               Destroy (EL.List (E).Child_Types);
               Destroy (EL.List (E).References);
            end if;

            Unref (EL.List (E));
         end loop;

         Next (Iter);
      end loop;
      Free (Iter);

      --  Free all other fields

      Clear (File.Entities);
      Destroy (File.Depends_On);
      Destroy (File.Depended_On);
      Destroy (File.Scope);
      Clear (File.All_Entities);
      File.Is_Valid := False;
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Entity : Entity_Information; File : Source_File) is
      procedure Check_And_Remove (E : in out Entity_Information);
      procedure Check_And_Remove (E : in out Entity_Information_List);
      procedure Check_And_Remove (E : in out File_Location_List);
      --  Remove all references to File in E

      procedure Check_And_Remove (E : in out Entity_Information) is
      begin
         if E /= null and then E.Declaration.File = File then
            E := null;
         end if;
      end Check_And_Remove;

      procedure Check_And_Remove (E : in out Entity_Information_List) is
         J : Integer;
      begin
         if E.List /= null then
            J := E.List'First;

            while J < E.Past_Last loop
               if E.List (J).Declaration.File = File then
                  if J + 1 <= E.Past_Last - 1 then
                     E.List (J .. E.Past_Last - 2) :=
                       E.List (J + 1 .. E.Past_Last - 1);
                  end if;
                  E.Past_Last := E.Past_Last - 1;
               else
                  J := J + 1;
               end if;
            end loop;

            if E.Past_Last = E.List'First then
               Unchecked_Free (E.List);
            end if;
         end if;
      end Check_And_Remove;

      procedure Check_And_Remove (E : in out File_Location_List) is
         J : Integer;
      begin
         if E.List /= null then
            J := E.List'First;
            while J < E.Past_Last loop
               if E.List (J).File = File then
                  E.List (J .. E.Past_Last - 2) :=
                    E.List (J + 1 .. E.Past_Last - 1);
                  E.Past_Last := E.Past_Last - 1;
               else
                  J := J + 1;
               end if;
            end loop;

            if E.Past_Last = E.List'First then
               Unchecked_Free (E.List);
            end if;
         end if;
      end Check_And_Remove;

   begin
      Assert (Me, Entity.Declaration.File /= File,
              "Entity should have been on .Entities list, not .All_Entities");

      if Entity.End_Of_Scope.File = File then
         Entity.End_Of_Scope := No_File_Location;
      end if;

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
   -- Length --
   ------------

   function Length (List : Entity_Information_List) return Natural is
   begin
      if List.List = null then
         return 0;
      else
         return List.Past_Last - List.List'First;
      end if;
   end Length;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (List : in out Entity_Information_List; Item : Entity_Information) is
   begin
      if List.List /= null then
         for L in List.List'First .. List.Past_Last - 1 loop
            if List.List (L) = Item then
               if L < List.Past_Last - 1 then
                  List.List (L .. List.Past_Last - 2) :=
                    List.List (L + 1 .. List.Past_Last - 1);
               end if;
               List.Past_Last := List.Past_Last - 1;

               if List.List'First = List.Past_Last then
                  Unchecked_Free (List.List);
               end if;
               exit;
            end if;
         end loop;
      end if;
   end Remove;

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

   -------------
   -- Destroy --
   -------------

   procedure Destroy (List : in out File_Location_List) is
   begin
      if List.List /= null then
         Unchecked_Free (List.List);
      end if;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (List : in out Source_File_List) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Source_File_Array, Source_File_Array_Access);
   begin
      if List.List /= null then
         Unchecked_Free (List.List);
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
         LI.Ref_Count := LI.Ref_Count - 1;
         if LI.Ref_Count = 0 then
            --  Do not remove the file from the htable, since Unref is
            --  called after a removal, and the user shouldn't call Unref
            --  more often than Ref
            for L in LI.Files.List'First .. LI.Files.Past_Last - 1 loop
               LI.Files.List (L).LI := null;
            end loop;
            Destroy (LI.Files);
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
         Entity.Ref_Count := Entity.Ref_Count - 1;
         if Entity.Ref_Count = 0 then
            Free (Entity.Name);
            Destroy (Entity.Parent_Types);
            Destroy (Entity.Primitive_Subprograms);
            Destroy (Entity.Child_Types);
            Destroy (Entity.References);
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

   -------------
   -- Destroy --
   -------------

   procedure Destroy (List : in out Entity_Information_List) is
   begin
      if List.List /= null then
         Unchecked_Free (List.List);
      end if;
   end Destroy;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (File : Source_File) return VFS.Virtual_File is
   begin
      if File = null then
         return VFS.No_File;
      else
         return File.Name;
      end if;
   end Get_Filename;

   ---------------------
   -- Get_LI_Filename --
   ---------------------

   function Get_LI_Filename (LI : LI_File) return VFS.Virtual_File is
   begin
      if LI = null then
         return VFS.No_File;
      else
         return LI.Name;
      end if;
   end Get_LI_Filename;

   ------------
   -- Create --
   ------------

   function Create return Entities_Database is
   begin
      return new Entities_Database_Record;
   end Create;

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
     (Db        : Entities_Database;
      File      : VFS.Virtual_File;
      LI        : LI_File;
      Timestamp : Ada.Calendar.Time := No_Time) return Source_File
   is
      S : Source_File := Get (Db.Files, File);
   begin
      if S = null then
         Assert (Me, LI /= null, "Null LI file passed to Get_Or_Create");
         S := new Source_File_Record'
           (Timestamp      => Timestamp,
            Name           => File,
            Entities       => Empty_Trie_Tree,
            Depends_On     => Null_Source_File_List,
            Depended_On    => Null_Source_File_List,
            All_Entities   => Empty_Trie_Tree,
            Scope          => null,
            LI             => LI,
            Is_Valid       => True,
            Ref_Count      => 1);
         Add (LI.Files, S);
         Set (Db.Files, File, S);

      else
         if Timestamp /= No_Time then
            S.Timestamp := Timestamp;
         end if;
      end if;

      S.Is_Valid := True;

      return S;
   end Get_Or_Create;

   ----------
   -- Find --
   ----------

   function Find
     (List : Source_File_List; File : Source_File) return Natural is
   begin
      if List.List /= null then
         for L in List.List'First .. List.Past_Last - 1 loop
            if List.List (L) = File then
               return L;
            end if;
         end loop;
      end if;
      return Not_Found;
   end Find;

   ----------
   -- Find --
   ----------

   function Find
     (List   : Entity_Information_List;
      Loc    : File_Location) return Entity_Information is
   begin
      if List.List /= null then
         for L in List.List'First .. List.Past_Last - 1 loop
            if List.List (L).Declaration = Loc then
               return List.List (L);
            end if;
         end loop;
      end if;
      return null;
   end Find;

   ---------
   -- Add --
   ---------

   procedure Add (List : in out Source_File_List; Item : Source_File) is
   begin
      Add_And_Grow (List.List, List.Past_Last, Item);
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove (List : in out Source_File_List; Item : Source_File) is
   begin
      if List.List /= null then
         for L in List.List'First .. List.Past_Last - 1 loop
            if List.List (L) = Item then
               List.List (L .. List.Past_Last - 2) :=
                 List.List (L + 1 .. List.Past_Last - 1);
               List.Past_Last := List.Past_Last - 1;
               exit;
            end if;
         end loop;
      end if;
   end Remove;

   ---------
   -- Add --
   ---------

   procedure Add
     (List : in out Entity_Information_List; Item : Entity_Information) is
   begin
      Add_And_Grow (List.List, List.Past_Last, Item);
   end Add;

   --------------------
   -- Add_Depends_On --
   --------------------

   procedure Add_Depends_On
     (File : Source_File; Depends_On : Source_File) is
   begin
      Add (File.Depends_On, Depends_On);
      Add_Depended_On (Depends_On, File);
   end Add_Depends_On;

   ---------------------
   -- Add_Depended_On --
   ---------------------

   procedure Add_Depended_On
     (File : Source_File; Depended_On  : Source_File) is
   begin
      if Find (File.Depended_On, Depended_On) = Not_Found then
         Add (File.Depended_On, Depended_On);
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
         Add (EL.all, Entity);

         if Is_Null then
            Insert (Entities, EL);
         end if;
      end if;
   end Add;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Db        : Entities_Database;
      File      : VFS.Virtual_File;
      Timestamp : Ada.Calendar.Time := No_Time) return LI_File
   is
      L : LI_File := Get (Db.LIs, File);
   begin
      if L = null then
         L := new LI_File_Record'
           (Db        => Db,
            Name      => File,
            Timestamp => Timestamp,
            Files     => Null_Source_File_List,
            Ref_Count => 1);
         Set (Db.LIs, File, L);

      elsif Timestamp /= No_Time then
         L.Timestamp := Timestamp;
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
         Destroy (D.all);
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
     (Entity : Entity_Information; Location : File_Location) is
   begin
      Entity.End_Of_Scope := Location;

      if Entity.Declaration.File /= Location.File then
         Add (Location.File.All_Entities, Entity, Check_Duplicates => True);
      end if;
   end Set_End_Of_Scope;

   ------------------------
   -- Set_Is_Renaming_Of --
   ------------------------

   procedure Set_Is_Renaming_Of
     (Entity : Entity_Information; Renaming_Of : Entity_Information) is
   begin
      Entity.Rename := Renaming_Of;

      if Entity.Declaration.File /= Renaming_Of.Declaration.File then
         Add (Renaming_Of.Declaration.File.All_Entities, Entity,
              Check_Duplicates => True);
      end if;
   end Set_Is_Renaming_Of;

   -------------------
   -- Add_Reference --
   -------------------

   procedure Add_Reference
     (Entity : Entity_Information; Location : File_Location) is
   begin
      Add (Entity.References, Location);

      if Entity.Declaration.File /= Location.File then
         Add (Location.File.All_Entities, Entity, Check_Duplicates => True);
      end if;
   end Add_Reference;

   ---------
   -- Add --
   ---------

   procedure Add (List : in out File_Location_List; Loc : File_Location) is
   begin
      Add_And_Grow (List.List, List.Past_Last, Loc);
   end Add;

   -----------------
   -- Set_Type_Of --
   -----------------

   procedure Set_Type_Of
     (Entity : Entity_Information; Is_Of_Type : Entity_Information)
   is
   begin
      Add (Entity.Parent_Types, Is_Of_Type);

      if Entity.Kind.Is_Type then
         Add (Is_Of_Type.Child_Types, Entity);
      end if;

      if Entity.Declaration.File /= Is_Of_Type.Declaration.File then
         Add (Is_Of_Type.Declaration.File.All_Entities, Entity,
              Check_Duplicates => True);
      end if;
   end Set_Type_Of;

   ------------------------------
   -- Add_Primitive_Subprogram --
   ------------------------------

   procedure Add_Primitive_Subprogram
     (Entity : Entity_Information; Primitive : Entity_Information) is
   begin
      Add (Entity.Primitive_Subprograms, Primitive);
      Primitive.Primitive_Op_Of := Entity;

      if Entity.Declaration.File /= Primitive.Declaration.File then
         Add (Primitive.Declaration.File.All_Entities, Entity,
              Check_Duplicates => True);
      end if;
   end Add_Primitive_Subprogram;

   ----------------------
   -- Set_Pointed_Type --
   ----------------------

   procedure Set_Pointed_Type
     (Entity : Entity_Information; Points_To : Entity_Information) is
   begin
      Entity.Pointed_Type := Points_To;

      if Entity.Declaration.File /= Points_To.Declaration.File then
         Add (Points_To.Declaration.File.All_Entities, Entity,
              Check_Duplicates => True);
      end if;
   end Set_Pointed_Type;

   -----------------------
   -- Set_Returned_Type --
   -----------------------

   procedure Set_Returned_Type
     (Entity : Entity_Information; Returns : Entity_Information) is
   begin
      Entity.Returned_Type := Returns;

      if Entity.Declaration.File /= Returns.Declaration.File then
         Add (Returns.Declaration.File.All_Entities, Entity,
              Check_Duplicates => True);
      end if;
   end Set_Returned_Type;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Name   : String;
      File   : Source_File;
      Line   : Natural;
      Column : Natural) return Entity_Information
   is
      EL : Entity_Information_List_Access := Get (File.LI.Db.Entities, Name);
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
            End_Of_Scope          => No_File_Location,
            Parent_Types          => Null_Entity_Information_List,
            Pointed_Type          => null,
            Returned_Type         => null,
            Primitive_Op_Of       => null,
            Rename                => null,
            Primitive_Subprograms => Null_Entity_Information_List,
            Child_Types           => Null_Entity_Information_List,
            References            => Null_File_Location_List,
            Ref_Count             => 1);
         Add (EL.all, E);

         Add_Entity (File, E);
      end if;

      if Is_Null then
         Insert (File.LI.Db.Entities, EL);
      end if;

      return E;
   end Get_Or_Create;

end Entities;
