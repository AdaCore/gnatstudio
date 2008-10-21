-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2008, AdaCore                 --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with System;            use System;

with Language.Tree.Ada; use Language.Tree.Ada;

package body Language.Tree.Database is

   procedure Internal_Update_Contents
     (File : Structured_File_Access; Is_New_File : Boolean);
   --  Same as Update_Contents, but takes into account differences depending on
   --  the fact that the file is a brand new one, or an existing one.

   ----------------
   -- Get_Buffer --
   ----------------

   overriding function Get_Buffer
     (Provider : access File_Buffer_Provider;
      File     : GNATCOLL.VFS.Virtual_File) return String_Access
   is
      pragma Unreferenced (Provider);
   begin
      return Read_File (File);
   end Get_Buffer;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Buffer_Provider_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Buffer_Provider'Class, Buffer_Provider_Access);
   begin
      Internal (This);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (File : in out Structured_File_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Structured_File, Structured_File_Access);
   begin
      Free (File.Cache_Buffer);
      Free_Annotations (File.Tree);
      Free (File.Tree);
      Free (File.Db_Data_Tree);
      Free (File.Line_Starts);

      Internal (File);
   end Free;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree (File : Structured_File_Access) return Construct_Tree is
   begin
      if File /= null then
         return File.Tree;
      else
         return Null_Construct_Tree;
      end if;
   end Get_Tree;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (File : Structured_File_Access) return GNAT.Strings.String_Access
   is
   begin
      if File.Cache_Buffer = null then
         File.Cache_Buffer := Get_Buffer (File.Db.Provider, File.File);
      end if;

      return File.Cache_Buffer;
   end Get_Buffer;

   -------------------
   -- Get_File_Path --
   -------------------

   function Get_File_Path
     (File : Structured_File_Access) return Virtual_File is
   begin
      return File.File;
   end Get_File_Path;

   ------------------------
   -- Get_Offset_Of_Line --
   ------------------------

   function Get_Offset_Of_Line
     (File : Structured_File_Access; Line : Integer) return Integer is
   begin
      if File.Line_Starts = null then
         declare
            Lines       : Line_Start_Indexes_Access :=
              new Line_Start_Indexes (1 .. 1000);
            Buffer      : constant String_Access := Get_Buffer (File);
            Lines_Index : Integer := 2;
            Tmp_Lines   : Line_Start_Indexes_Access;
         begin
            Lines (1) := 1;

            for J in Buffer'Range loop
               if Buffer (J) = ASCII.LF then
                  if Lines_Index > Lines'Last then
                     Tmp_Lines := Lines;
                     Lines := new Line_Start_Indexes (1 .. Lines'Last * 2);
                     Lines (1 .. Tmp_Lines'Last) := Tmp_Lines.all;
                     Free (Tmp_Lines);
                  end if;

                  Lines (Lines_Index) := J + 1;
                  Lines_Index := Lines_Index + 1;
               end if;
            end loop;

            File.Line_Starts := new Line_Start_Indexes'
              (Lines (1 .. Lines_Index - 1));
            Free (Lines);
         end;
      end if;

      return File.Line_Starts (Line);
   end Get_Offset_Of_Line;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
     (File : Structured_File_Access) return Tree_Language_Access is
   begin
      return File.Lang;
   end Get_Language;

   ---------------------
   -- Update_Contents --
   ---------------------

   procedure Update_Contents (File : Structured_File_Access) is
   begin
      Internal_Update_Contents (File, False);
   end Update_Contents;

   ------------------------------
   -- Internal_Update_Contents --
   ------------------------------

   procedure Internal_Update_Contents
     (File : Structured_File_Access; Is_New_File : Boolean)
   is
      Buffer     : GNAT.Strings.String_Access :=
                     Get_Buffer (File.Db.Provider, File.File);
      Constructs : aliased Construct_List;

      Current_Update_Kind : Update_Kind;

      New_Tree         : Construct_Tree;
      New_Db_Data_Tree : Construct_Db_Data_Access;

      procedure Add_New_Construct_If_Needed (It : Construct_Tree_Iterator);

      procedure Diff_Callback
        (Old_Obj, New_Obj : Construct_Tree_Iterator; Kind : Diff_Kind);

      ---------------------------------
      -- Add_New_Construct_If_Needed --
      ---------------------------------

      procedure Add_New_Construct_If_Needed (It : Construct_Tree_Iterator) is
         Data      : Trie_Additional_Data;
         Construct : constant access Simple_Construct_Information :=
                       Get_Construct (It);
      begin
         --  We add only named constructs in the database, and we dismiss some
         --  categories.

         if Construct.Name = null
           or else Construct.Category = Cat_Parameter
           or else Construct.Category = Cat_Field
           or else Construct.Category = Cat_With
           or else Construct.Category = Cat_Use
         then
            return;
         end if;

         Data.File := File;

         Construct_Db_Trie.Insert
           (File.Db.Entities_Db'Access,
            It,
            Data,
            File.Lang,
            New_Db_Data_Tree (It.Index));
      end Add_New_Construct_If_Needed;

      -------------------
      -- Diff_Callback --
      -------------------

      procedure Diff_Callback
        (Old_Obj, New_Obj : Construct_Tree_Iterator; Kind : Diff_Kind) is
      begin
         case Kind is
            when Removed =>
               Current_Update_Kind := Structural_Change;

               Delete
                 (File.Db.Entities_Db'Access,
                  File.Db_Data_Tree (Old_Obj.Index));

               Construct_Annotations_Pckg.Free
                 (Get_Annotation_Container (File.Tree, Old_Obj).all);

            when Added =>
               Current_Update_Kind := Structural_Change;
               Add_New_Construct_If_Needed (New_Obj);

            when Preserved =>
               if Get_Construct (Old_Obj).Attributes /=
                 Get_Construct (New_Obj).Attributes
                 or else Get_Construct (Old_Obj).Is_Declaration /=
                 Get_Construct (New_Obj).Is_Declaration
                 or else Get_Construct (Old_Obj).Visibility /=
                 Get_Construct (Old_Obj).Visibility
               then
                  Current_Update_Kind := Structural_Change;
               end if;

               New_Db_Data_Tree (New_Obj.Index) :=
                 File.Db_Data_Tree (Old_Obj.Index);

               --  Copy the annotation from the old obj to the new obj

               New_Tree.Contents (New_Obj.Index).Annotations :=
                 File.Tree.Contents (Old_Obj.Index).Annotations;

               --  If we're in a structural change, there may have been object
               --  inserted / removed before this construct, so we have to
               --  update the index and the persistent entity.

               if Current_Update_Kind = Structural_Change then
                  --  Update the construct wrapper if the construct is stored
                  --  in the database.

                  if New_Db_Data_Tree (New_Obj.Index) /=
                    Construct_Db_Trie.Null_Construct_Trie_Index
                  then
                     Replace
                       (File.Db.Entities_Db'Access,
                        New_Db_Data_Tree (New_Obj.Index),
                        New_Obj,
                        (File => File));
                  end if;

                  --  Update the persistent annotation if any

                  declare
                     use Construct_Annotations_Pckg;

                     Annotations : constant access Annotation_Container :=
                       Get_Annotation_Container (New_Tree, New_Obj);
                     Persistent_Annotation : Annotation (Other_Kind);
                  begin
                     if Is_Set
                       (Annotations.all, File.Db.Persistent_Entity_Key)
                     then
                        Get_Annotation
                          (Annotations.all,
                           File.Db.Persistent_Entity_Key,
                           Persistent_Annotation);

                        Entity_Persistent_Annotation
                          (Persistent_Annotation.Other_Val.all).Info.Index :=
                          New_Obj.Index;
                     end if;
                  end;
               end if;
         end case;
      end Diff_Callback;

   begin
      --  Phase 1 : analyze the new tree

      --  ??? We are assuming that Buffer is encoded in UTF8, is this the case?
      Parse_Constructs (Get_Language (File.Lang), Buffer.all, Constructs);
      New_Tree := To_Construct_Tree (Constructs'Access, True);

      Analyze_Referenced_Identifiers
        (Buffer.all, Get_Language (File.Lang), File.Db, New_Tree);
      Analyze_Constructs_Identifiers (File.Db, New_Tree);
      New_Db_Data_Tree := new Construct_Db_Data_Array
        (1 .. New_Tree.Contents'Length);
      New_Db_Data_Tree.all :=
        (others => Construct_Db_Trie.Null_Construct_Trie_Index);

      --  Phase 2 : replace previous content by the new one

      if Is_New_File then
         Current_Update_Kind := Full_Change;

         declare
            It : Construct_Tree_Iterator := First (New_Tree);
         begin
            while It /= Null_Construct_Tree_Iterator loop
               Add_New_Construct_If_Needed (It);

               It := Next (New_Tree, It, Jump_Into);
            end loop;
         end;
      else
         Current_Update_Kind := Minor_Change;
         New_Tree.Annotations := File.Tree.Annotations;

         Diff
           (File.Lang,
            File.Tree,
            New_Tree,
            Diff_Callback'Unrestricted_Access);

         File.Tree.Annotations :=
           Tree_Annotations_Pckg.Null_Annotation_Container;

         Free (File.Cache_Buffer);
         Free (File.Tree);
         Free (File.Db_Data_Tree);
         Free (File.Line_Starts);
      end if;

      Free (Constructs);
      Free (Buffer);

      File.Tree := New_Tree;
      File.Db_Data_Tree := New_Db_Data_Tree;

      --  Notify database assistants

      declare
         Cur : Assistant_List.Cursor;
      begin
         Cur := First (File.Db.Ordered_Assistants);

         while Cur /= Assistant_List.No_Element loop
            File_Updated (Element (Cur), File, Current_Update_Kind);
            Cur := Next (Cur);
         end loop;
      end;
   end Internal_Update_Contents;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Structured_File) return Boolean is
   begin
      return Left.File = Right.File;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Structured_File_Access) return Boolean is
   begin
      return Left.File < Right.File;
   end "<";

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Construct_Database_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Construct_Database, Construct_Database_Access);
   begin
      Destroy (This);
      Internal (This);
   end Free;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Db       : in out Construct_Database;
      Provider : Buffer_Provider_Access)
   is
   begin
      Db.Tree_Registry := Tree_Annotations_Pckg.Create_Annotation_Key_Registry;
      Db.Construct_Registry :=
        Construct_Annotations_Pckg.Create_Annotation_Key_Registry;
      Construct_Annotations_Pckg.Get_Annotation_Key
        (Db.Construct_Registry, Db.Persistent_Entity_Key);
      Db.Provider := Provider;
   end Initialize;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Db   : Construct_Database_Access;
      File : Virtual_File;
      Lang : Tree_Language_Access) return Structured_File_Access is
   begin
      if Lang /= Ada_Tree_Lang or else not Is_Regular_File (File) then
         return null;
      end if;

      if not Contains (Db.Files_Db, File) then
         declare
            New_File  : constant Structured_File_Access := new Structured_File;
         begin
            New_File.File := File;
            New_File.Lang := Lang;
            New_File.Db := Db;

            Insert (Db.Files_Db, File, New_File);
            Insert (Db.Sorted_Files_Db, New_File);

            Internal_Update_Contents (New_File, True);

            return New_File;
         end;
      else
         return Element (Db.Files_Db, File);
      end if;
   end Get_Or_Create;

   ---------------------
   -- Update_Contents --
   ---------------------

   procedure Update_Contents
     (Db : access Construct_Database; File : Virtual_File) is
   begin
      if Contains (Db.Files_Db, File) then
         Update_Contents (Element (Db.Files_Db, File));
      end if;
   end Update_Contents;

   -----------
   -- Clear --
   -----------

   procedure Clear (Db : access Construct_Database) is
      C : File_Map.Cursor := First (Db.Files_Db);
      Garbage : Structured_File_Access;
   begin
      while C /= File_Map.No_Element loop
         Garbage := Element (C);
         Free (Garbage);

         C := Next (C);
      end loop;

      Clear (Db.Entities_Db'Access);
      Clear (Db.Files_Db);
      Clear (Db.Sorted_Files_Db);
   end Clear;

   ----------
   -- Free --
   ----------

   procedure Destroy (Db : access Construct_Database) is
      Assistant_Cur : Assistant_Map.Cursor;
      Assistant     : Database_Assistant_Access;

      procedure Unchecked_Free_Assistant is new
        Standard.Ada.Unchecked_Deallocation
          (Database_Assistant'Class, Database_Assistant_Access);
      procedure Unchecked_Free is new Standard.Ada.Unchecked_Deallocation
        (Buffer_Provider'Class, Buffer_Provider_Access);
   begin
      Clear (Db);

      Assistant_Cur := First (Db.Assistants);

      while Assistant_Cur /= Assistant_Map.No_Element loop
         Assistant := Element (Assistant_Cur);

         Free (Assistant.all);
         Unchecked_Free_Assistant (Assistant);
         Assistant_Cur := Next (Assistant_Cur);
      end loop;

      Tree_Annotations_Pckg.Free (Db.Tree_Registry);
      Construct_Annotations_Pckg.Free (Db.Construct_Registry);

      Unchecked_Free (Db.Provider);
   end Destroy;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Construct_Db_Data_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Construct_Db_Data_Array, Construct_Db_Data_Access);
   begin
      Internal (This);
   end Free;

   -----------
   -- Start --
   -----------

   function Start
     (Db : access Construct_Database; Prefix : String; Is_Partial : Boolean)
      return Construct_Db_Iterator
   is
      It : Construct_Db_Iterator;
   begin
      It.It := Start (Db.Entities_Db'Access, Prefix, Is_Partial);

      return It;
   end Start;

   -------------------
   -- Get_Construct --
   -------------------

   function Get_Construct
     (It : Construct_Db_Iterator) return Construct_Tree_Iterator
   is
   begin
      return Get_Construct_It (It.It);
   end Get_Construct;

   --------------------
   -- Get_Current_Id --
   --------------------

   function Get_Current_Id (It : Construct_Db_Iterator) return String is
   begin
      return Get_Index (It.It);
   end Get_Current_Id;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (It : Construct_Db_Iterator) return Structured_File_Access is
   begin
      return Get_Additional_Data (It.It).File;
   end Get_File;

   ---------
   -- Get --
   ---------

   function Get (It : Construct_Db_Iterator) return Entity_Access is
   begin
      return
        (File => Get_Additional_Data (It.It).File,
         It   => Get_Construct_It (It.It));
   end Get;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Construct_Db_Iterator) is
   begin
      Next (It.It);
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (It : Construct_Db_Iterator) return Boolean is
   begin
      return At_End (It.It);
   end At_End;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (It : Construct_Db_Iterator) return Boolean is
   begin
      return Is_Valid (It.It);
   end Is_Valid;

   ----------
   -- Free --
   ----------

   procedure Free (It : in out Construct_Db_Iterator) is
   begin
      Free (It.It);
   end Free;

   --------------------------------------
   -- Get_Tree_Annotation_Key_Registry --
   --------------------------------------

   function Get_Tree_Annotation_Key_Registry
     (Db : Construct_Database_Access)
      return access Tree_Annotations_Pckg.Annotation_Key_Registry is
   begin
      return Db.Tree_Registry'Access;
   end Get_Tree_Annotation_Key_Registry;

   -------------------------------------------
   -- Get_Construct_Annotation_Key_Registry --
   -------------------------------------------

   function Get_Construct_Annotation_Key_Registry
     (Db : Construct_Database_Access)
      return access Construct_Annotations_Pckg.Annotation_Key_Registry
   is
   begin
      return Db.Construct_Registry'Access;
   end Get_Construct_Annotation_Key_Registry;

   ---------------
   -- Get_Files --
   ---------------

   function Start_File_Search
     (Db : Construct_Database) return File_Set.Cursor
   is
   begin
      return First (Db.Sorted_Files_Db);
   end Start_File_Search;

   ----------------------
   -- To_Entity_Access --
   ----------------------

   function To_Entity_Access
     (File       : Structured_File_Access;
      Construct  : Construct_Tree_Iterator) return Entity_Access
   is
      Result : Entity_Access;
   begin
      if Construct = Null_Construct_Tree_Iterator then
         return Null_Entity_Access;
      else
         pragma Assert (Construct.Index in File.Tree.Contents'Range);

         Result.File := File;
         Result.It := Construct;

         return Result;
      end if;
   end To_Entity_Access;

   --------------------------------
   -- To_Construct_Tree_Iterator --
   --------------------------------

   function To_Construct_Tree_Iterator
     (Entity : Entity_Access) return Construct_Tree_Iterator
   is
   begin
      return Entity.It;
   end To_Construct_Tree_Iterator;

   --------------
   -- Get_File --
   --------------

   function Get_File (Entity : Entity_Access) return Structured_File_Access is
   begin
      return Entity.File;
   end Get_File;

   -------------------
   -- Get_Construct --
   -------------------

   function Get_Construct
     (Entity : Entity_Access) return access Simple_Construct_Information is
   begin
      return Get_Construct (Entity.It);
   end Get_Construct;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Entity_Array_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Entity_Array, Entity_Array_Access);
   begin
      Internal (This);
   end Free;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Entity_Access) return Boolean is
   begin
      --  Since file comparison is very expensive, it has to be tried first

      return Left.It < Right.It
        or else (Left.It = Right.It
          and then Left.File < Right.File);
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Entity_Access) return Boolean is
   begin
      --  Since file comparison is very expensive, it has to be tried first

      return Left.It = Right.It and then Left.File = Right.File;
   end "=";

   ----------------------
   -- To_Entity_Access --
   ----------------------

   function To_Entity_Access
     (Entity : Entity_Persistent_Access) return Entity_Access is
   begin
      if not Exists (Entity) then
         return Null_Entity_Access;
      else
         return To_Entity_Access
           (Entity.File,
            (Get_Tree (Entity.File).Contents (Entity.Index)'Access,
             Entity.Index));
      end if;
   end To_Entity_Access;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Entity_Persistent_Access) return Boolean is
   begin
      if Left = null then
         return False;
      elsif Right = null then
         return True;
      else
         return Left.all'Address < Right.all'Address;
      end if;
   end "<";

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Entity_Persistent_Array_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Entity_Persistent_Array, Entity_Persistent_Array_Access);
   begin
      Internal (This);
   end Free;

   ---------------------------------
   -- To_Entity_Persistent_Access --
   ---------------------------------

   function To_Entity_Persistent_Access
     (Entity : Entity_Access) return Entity_Persistent_Access
   is
      use Construct_Annotations_Pckg;

      Db : Construct_Database_Access;

      It          : constant Construct_Tree_Iterator :=
        To_Construct_Tree_Iterator (Entity);
      Annotations : access Annotation_Container;

      Persistent_Annotation : Annotation (Other_Kind);
   begin
      if It = Null_Construct_Tree_Iterator then
         return Null_Entity_Persistent_Access;
      end if;

      Db := Get_Database (Get_File (Entity));
      Annotations := Get_Annotation_Container
        (Get_Tree (Get_File (Entity)), It);

      if Is_Set (Annotations.all, Db.Persistent_Entity_Key) then
         Get_Annotation
           (Annotations.all, Db.Persistent_Entity_Key, Persistent_Annotation);
      else
         Persistent_Annotation.Other_Val := new Entity_Persistent_Annotation'
           (Info => new Entity_Persistent_Info'
              (Exists => True,
               File   => Get_File (Entity),
               Index  => To_Construct_Tree_Iterator (Entity).Index,
               Refs   => 0));

         Set_Annotation
           (Annotations.all, Db.Persistent_Entity_Key, Persistent_Annotation);
      end if;

      return Entity_Persistent_Annotation
        (Persistent_Annotation.Other_Val.all).Info;
   end To_Entity_Persistent_Access;

   -------------------
   -- Get_Construct --
   -------------------

   function Get_Construct
     (Entity : Entity_Persistent_Access) return Simple_Construct_Information is
   begin
      return Get_Tree (Entity.File).
        Contents (Natural (Entity.Index)).Construct;
   end Get_Construct;

   ---------
   -- Ref --
   ---------

   procedure Ref (Entity : in out Entity_Persistent_Access) is
   begin
      if Entity /= Null_Entity_Persistent_Access then
         Entity.Refs := Entity.Refs + 1;
      end if;
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Entity : in out Entity_Persistent_Access) is
      procedure Free is new Standard.Ada.Unchecked_Deallocation
        (Entity_Persistent_Info, Entity_Persistent_Access);
   begin
      if Entity /= Null_Entity_Persistent_Access then
         Entity.Refs := Entity.Refs - 1;

         if Entity.Refs = 0 and then not Entity.Exists then
            Free (Entity);
         end if;

         Entity := Null_Entity_Persistent_Access;
      end if;
   end Unref;

   -------------
   -- Is_Null --
   -------------

   function Exists (Entity : Entity_Persistent_Access) return Boolean is
   begin
      return Entity /= null and then Entity.Exists;
   end Exists;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Entity : Entity_Persistent_Access) return Structured_File_Access is
   begin
      return Entity.File;
   end Get_File;

   ------------------------
   -- Register_Assistant --
   ------------------------

   procedure Register_Assistant
     (Db        : Construct_Database_Access;
      Name      : String;
      Assistant : Database_Assistant_Access)
   is
   begin
      Insert (Db.Assistants, Name, Assistant);
      Append (Db.Ordered_Assistants, Assistant);
   end Register_Assistant;

   -------------------
   -- Get_Assistant --
   -------------------

   function Get_Assistant
     (Db : Construct_Database_Access; Name : String)
      return Database_Assistant_Access
   is
   begin
      return Element (Db.Assistants, Name);
   end Get_Assistant;

   ------------------
   -- Get_Database --
   ------------------

   function Get_Database
     (File : Structured_File_Access) return Construct_Database_Access is
   begin
      return Construct_Database_Access (File.Db);
   end Get_Database;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Obj : in out Entity_Persistent_Annotation)
   is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Entity_Persistent_Info, Entity_Persistent_Access);
   begin
      Obj.Info.Exists := False;

      if Obj.Info.Refs = 0 then
         Internal (Obj.Info);
      end if;
   end Free;

   --------------------
   -- Get_Identifier --
   --------------------

   overriding function Get_Identifier
     (Manager : access Construct_Database; Name : String)
      return Distinct_Identifier
   is
   begin
      return Distinct_Identifier
        (Get_Name_Index (Manager.Entities_Db'Access, Name));
   end Get_Identifier;

   --------------------
   -- Get_Identifier --
   --------------------

   function Get_Identifier
     (Entity : Entity_Access) return Distinct_Identifier
   is
   begin
      return Entity.It.Node.Id;
   end Get_Identifier;

end Language.Tree.Database;
