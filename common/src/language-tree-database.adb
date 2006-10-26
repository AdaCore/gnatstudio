-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006                           --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation; use Ada;

with Basic_Types; use Basic_Types;

with Language.Tree.Ada; use Language.Tree.Ada;

package body Language.Tree.Database is

   procedure Internal_Update_Contents
     (File : Structured_File_Access; Is_New_File : Boolean);
   --  Same as Update_Contents, but takes into account differences depending on
   --  the fact that the file is a brand new one, or an existing one.

   ----------
   -- Free --
   ----------

   procedure Free (File : in out Structured_File_Access) is
      procedure Internal is new Unchecked_Deallocation
        (Structured_File, Structured_File_Access);
   begin
      Free (File.Cache_Tree);
      Free (File.Cache_Buffer);
      Free (File.Public_Tree);
      Free (File.Db_Data_Tree);

      Internal (File);
   end Free;

   -------------------
   -- Get_Full_Tree --
   -------------------

   function Get_Full_Tree
     (File : Structured_File_Access) return Construct_Tree is
   begin
      if File.Cache_Tree = null then
         declare
            Constructs : aliased Construct_List;
         begin
            Parse_Constructs
              (Get_Language (File.Lang), Get_Buffer (File).all, Constructs);
            File.Cache_Tree := To_Construct_Tree (Constructs'Access, True);
         end;
      end if;

      return File.Cache_Tree;
   end Get_Full_Tree;

   -------------------
   -- Get_Full_Tree --
   -------------------

   function Get_Public_Tree
     (File : Structured_File_Access) return Construct_Tree is
   begin
      return File.Public_Tree;
   end Get_Public_Tree;

   ---------------------
   -- Get_Parent_File --
   ---------------------

   function Get_Parent_File
     (File : Structured_File_Access) return Structured_File_Access
   is
   begin
      return File.Parent_File;
   end Get_Parent_File;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (File : Structured_File_Access) return GNAT.Strings.String_Access is
   begin
      if File.Cache_Buffer = null then
         File.Cache_Buffer := Read_File (File.File);
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

      function Is_Spec_Of
        (Supposed_Spec, Supposed_Body : Structured_File_Access) return Boolean;

      function Is_Spec_Of
        (Supposed_Spec, Supposed_Body : Structured_File_Access) return Boolean
      is
         Spec_Tree, Body_Tree : Construct_Tree;
      begin
         if Supposed_Spec = null or else Supposed_Body = null then
            return False;
         end if;

         Spec_Tree := Get_Public_Tree (Supposed_Spec);
         Body_Tree := Get_Public_Tree (Supposed_Body);

         return Get_Unit_Construct
           (File.Lang, Spec_Tree).Node.Construct.Is_Declaration
           and then not Get_Unit_Construct
             (File.Lang, Body_Tree).Node.Construct.Is_Declaration
           and then To_String (Get_Unit_Name (File.Lang, Spec_Tree))
           = To_String (Get_Unit_Name (File.Lang, Body_Tree));
      end Is_Spec_Of;

      Buffer     : GNAT.Strings.String_Access := Read_File (File.File);
      Full_Tree  : aliased Construct_Tree;
      Constructs : aliased Construct_List;
   begin
      --  Phase 1 : remove previous construct information from the database

      if not Is_New_File then
         if File.Public_Tree /= null then
            for J in File.Public_Tree.Contents'Range loop
               declare
                  Construct : constant Simple_Construct_Information :=
                    File.Public_Tree.Contents (J).Construct;
               begin
                  if Construct.Name /= null
                    and then Construct.Category /= Cat_Parameter
                    and then Construct.Category /= Cat_Field
                    and then Construct.Category /= Cat_With
                    and then Construct.Category /= Cat_Use
                  then
                     Delete (File.Db_Data_Tree (J).Position);
                  end if;
               end;
            end loop;
         end if;

      --  Remove all the links to this file

         declare
            C          : File_Map.Cursor := First (File.Db.Files_Db);
         begin
            while C /= File_Map.No_Element loop
               if Element (C).Parent_File = File then
                  Element (C).Parent_File := null;
               end if;

               C := Next (C);
            end loop;
         end;

         Free (File.Cache_Tree);
         Free (File.Cache_Buffer);
         Free (File.Public_Tree);
         Free (File.Db_Data_Tree);
         File.Parent_File := null;
      end if;

      --  Phase 2 : analyze the file and add data in the database

      Parse_Constructs (Get_Language (File.Lang), Buffer.all, Constructs);
      Full_Tree := To_Construct_Tree (Constructs'Access, True);
      File.Public_Tree := Get_Public_Tree (File.Lang, Full_Tree'Access, True);
      File.Db_Data_Tree := new Construct_Db_Data_Array
        (1 .. File.Public_Tree.Contents'Length);

      for J in File.Public_Tree.Contents'Range loop
         declare
            Wrapper : Construct_Node_Wrapper;
         begin
            Wrapper.Index := J;
            Wrapper.File := File;
            Wrapper.Node := File.Public_Tree.Contents (J);

            if Wrapper.Node.Construct.Name /= null
              and then Wrapper.Node.Construct.Category /= Cat_Parameter
              and then Wrapper.Node.Construct.Category /= Cat_Field
              and then Wrapper.Node.Construct.Category /= Cat_With
              and then Wrapper.Node.Construct.Category /= Cat_Use
            then
               declare
                  Name : constant String :=
                    Get_Name_Index (File.Lang, Wrapper.Node.Construct);
                  List : Construct_Node_List_Access := Get
                    (File.Db.Entities_Db'Access, Name);
               begin
                  if List = null then
                     List := new Construct_Node_List;
                     List.Name := new String'(Name);
                     List.Constructs :=
                       new Construct_Vector.Lazy_Vector_Record;
                     Insert (File.Db.Entities_Db, List);
                  end if;

                  Insert
                    (List.Constructs, Wrapper, File.Db_Data_Tree (J).Position);
               end;
            end if;
         end;
      end loop;

      --  Compute the child / parent information

      declare
         C          : File_Map.Cursor := First (File.Db.Files_Db);
         Parent     : Get_Parent_Tree_Result;
      begin
         while C /= File_Map.No_Element loop
            Parent := Get_Parent_Tree
              (File.Lang,
               File.Public_Tree,
               Get_Public_Tree (Element (C)));

            if Parent = Left
              and then not
                Is_Spec_Of (Element (C).Parent_File, Element (C))
            then
               Element (C).Parent_File := File;
            elsif Parent = Right
              and then not
                Is_Spec_Of (File.Parent_File, File)
            then
               File.Parent_File := Element (C);
            end if;

            C := Next (C);
         end loop;
      end;

      Free (Constructs);
      Free (Full_Tree);
      Free (Buffer);
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
      Left_Name : constant Composite_Identifier :=
        Get_Unit_Name (Left.Lang, Get_Public_Tree (Left));
      Right_Name : constant Composite_Identifier :=
        Get_Unit_Name (Right.Lang, Get_Public_Tree (Right));
   begin
      return Get_Item
        (Left_Name, Length (Left_Name))
        < Get_Item (Right_Name, Length (Right_Name));
   end "<";

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Construct_Database_Access) is
      procedure Internal is new Unchecked_Deallocation
        (Construct_Database, Construct_Database_Access);
   begin
      Clear (This);
      Clear (This.Entities_Db);
      Internal (This);
   end Free;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Db   : Construct_Database_Access;
      File : Virtual_File;
      Lang : Tree_Language_Access) return Structured_File_Access is
   begin
      if Lang /= Ada_Tree_Lang then
         return null;
      end if;

      if not Contains (Db.Files_Db, File) then
         declare
            New_File  : constant Structured_File_Access := new Structured_File;
         begin
            New_File.File := File;
            New_File.Lang := Lang;
            New_File.Db := Db;

            Internal_Update_Contents (New_File, True);

            Insert (Db.Files_Db, File, New_File);
            Insert (Db.Sorted_Files_Db, New_File);

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

      Clear (Db.Entities_Db);
      Clear (Db.Files_Db);
      Clear (Db.Sorted_Files_Db);
   end Clear;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Node : Construct_Node_List_Access) return GNAT.OS_Lib.String_Access is
   begin
      if Node /= null then
         return Node.Name;
      else
         return null;
      end if;
   end Get_Name;

   ----------
   -- Free --
   ----------

   procedure Free (Node : in out Construct_Node_List_Access) is
      procedure Internal is new Unchecked_Deallocation
        (Construct_Node_List, Construct_Node_List_Access);
   begin
      if Node /= null then
         Free (Node.Name);

         Free (Node.Constructs);
         Internal (Node);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Construct_Db_Data_Access) is
      procedure Internal is new Unchecked_Deallocation
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
      It.Is_Partial := Is_Partial;
      It.It_Db := Start (Db.Entities_Db'Access, Prefix);

      if not At_End (It.It_Db) then
         It.It_Vector := First (Get (It.It_Db).Constructs);

         if not It.Is_Partial
           and then Get (It.It_Db).Name.all /= Prefix
         then
            It.It_Db := Construct_Trie_Trees.Null_Iterator;
         end if;
      end if;

      if not Is_Valid (It) then
         Next (It);
      end if;

      return It;
   end Start;

   -------------------
   -- Get_Construct --
   -------------------

   function Get_Construct
     (It : Construct_Db_Iterator) return Construct_Tree_Iterator
   is
   begin
      return
        (Node => Get (It.It_Vector).Node, Index => Get (It.It_Vector).Index);
   end Get_Construct;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (It : Construct_Db_Iterator) return Structured_File_Access is
   begin
      return Get (It.It_Vector).File;
   end Get_File;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Construct_Db_Iterator) is
   begin
      if not At_End (It.It_Vector) then
         Next (It.It_Vector);
      else
         if It.Is_Partial then
            Next (It.It_Db);

            if not At_End (It.It_Db) then
               It.It_Vector := First (Get (It.It_Db).Constructs);
            end if;
         else
            It.It_Db := Construct_Trie_Trees.Null_Iterator;
         end if;
      end if;

      if not Is_Valid (It) then
         Next (It);
      end if;
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (It : Construct_Db_Iterator) return Boolean is
   begin
      return At_End (It.It_Db);
   end At_End;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (It : Construct_Db_Iterator) return Boolean is
   begin
      return At_End (It) or else not At_End (It.It_Vector);
   end Is_Valid;

   ----------
   -- Free --
   ----------

   procedure Free (It : in out Construct_Db_Iterator) is
   begin
      Free (It.It_Db);
   end Free;

   ---------------
   -- Get_Files --
   ---------------

   function Start_File_Search
     (Db : Construct_Database) return File_Set.Cursor
   is
   begin
      return First (Db.Sorted_Files_Db);
   end Start_File_Search;

end Language.Tree.Database;
