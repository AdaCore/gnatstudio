------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with Ada.Tags; use Ada.Tags;

package body Ada_Semantic_Tree.Cache is

   Cache_Assistant_Id : constant String := "ADA_CACHE_ASSISTANT";

   type Cache_Db_Assistant is new Database_Assistant with record
      Cache_Key : Construct_Annotations_Pckg.Annotation_Key;
   end record;

   use Construct_Annotations_Pckg;

   type Cache_Annotation_Record is new General_Annotation_Record
   with record
      Cached : Cache_Access;
   end record;

   overriding procedure Free
     (Obj : in out Cache_Annotation_Record);

   overriding procedure File_Updated
     (Assistant : access Cache_Db_Assistant;
      File      : Structured_File_Access;
      Old_Tree  : Construct_Tree;
      Kind      : Update_Kind);

   ------------------------
   -- Register_Assistant --
   ------------------------

   procedure Register_Assistant (Db : Construct_Database_Access) is
      Cache_Key : Construct_Annotations_Pckg.Annotation_Key;
   begin
      Get_Annotation_Key
        (Get_Construct_Annotation_Key_Registry (Db).all,
         Cache_Key);

      Register_Assistant
        (Db,
         Cache_Assistant_Id,
         new Cache_Db_Assistant'
           (Database_Assistant with Cache_Key => Cache_Key));
   end Register_Assistant;

   ---------------
   -- Set_Cache --
   ---------------

   procedure Set_Cache (Entity : Entity_Access; Info : Cache_Access) is
      Assistant : constant Database_Assistant_Access :=
        Get_Assistant
          (Get_Database (Get_File (Entity)),
           Cache_Assistant_Id);

      My_Assistant : Cache_Db_Assistant renames
        Cache_Db_Assistant (Assistant.all);
      Annot : Annotation;
   begin
      Get_Annotation
        (Get_Annotation_Container
           (Get_Tree (Get_File (Entity)),
            To_Construct_Tree_Iterator (Entity)).all,
         My_Assistant.Cache_Key,
         Annot);

      if Annot /= Null_Annotation
        and then Cache_Annotation_Record (Annot.Other_Val.all).Cached.all'Tag
        /= Info.all'Tag
      then
         raise Inconsistent_Cache;
      end if;

      Annot :=
        (Other_Kind, new Cache_Annotation_Record'
           (General_Annotation_Record with Cached => Info));

      Set_Annotation
        (Get_Annotation_Container
           (Get_Tree (Get_File (Entity)),
            To_Construct_Tree_Iterator (Entity)).all,
         My_Assistant.Cache_Key,
         Annot);
   end Set_Cache;

   ---------------
   -- Get_Cache --
   ---------------

   function Get_Cache (Entity : Entity_Access) return Cache_Access is
      Assistant : constant Database_Assistant_Access :=
        Get_Assistant
          (Get_Database (Get_File (Entity)),
           Cache_Assistant_Id);

      My_Assistant : Cache_Db_Assistant renames
        Cache_Db_Assistant (Assistant.all);
      Annot : Annotation;
   begin
      Get_Annotation
        (Get_Annotation_Container
           (Get_Tree (Get_File (Entity)),
            To_Construct_Tree_Iterator (Entity)).all,
         My_Assistant.Cache_Key,
         Annot);

      if Annot /= Null_Annotation then
         return Cache_Annotation_Record (Annot.Other_Val.all).Cached;
      else
         return null;
      end if;
   end Get_Cache;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Obj : in out Cache_Annotation_Record)
   is
      procedure Dealloc is new Ada.Unchecked_Deallocation
        (Cached_Information'Class, Cache_Access);
   begin
      Free (Obj.Cached.all);
      Dealloc (Obj.Cached);
   end Free;

   ------------------
   -- File_Updated --
   ------------------

   overriding procedure File_Updated
     (Assistant : access Cache_Db_Assistant;
      File      : Structured_File_Access;
      Old_Tree  : Construct_Tree;
      Kind      : Update_Kind)
   is
      pragma Unreferenced (Old_Tree);
      Tree : constant Construct_Tree := Get_Tree (File);
      It : Construct_Tree_Iterator := First (Tree);
   begin
      if Kind in Structural_Change .. Full_Change then
         while It /= Null_Construct_Tree_Iterator loop
            Free_Annotation
              (Get_Annotation_Container (Tree, It).all,
               Assistant.Cache_Key);

            It := Next (Tree, It, Jump_Into);
         end loop;
      end if;
   end File_Updated;

end Ada_Semantic_Tree.Cache;
