-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2007-2008, AdaCore                  --
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

with GNAT.Strings;     use GNAT.Strings;
with GNATCOLL.Utils;   use GNATCOLL.Utils;
with Language;         use Language;
with Language.Ada;     use Language.Ada;

package body Ada_Semantic_Tree.Visibility is

   Ada_Visibility_Id : constant String := "ADA_LIB_VISIBILITY";

   type Ada_Visibibility_Assistant is new Database_Assistant with record
      null;
   end record;

   overriding procedure File_Updated
     (Assistant : access Ada_Visibibility_Assistant;
      File      : Structured_File_Access;
      Kind      : Update_Kind);

   ------------------------
   -- Register_Assistant --
   ------------------------

   procedure Register_Assistant (Db : Construct_Database_Access) is
   begin
      Register_Assistant
        (Db, Ada_Visibility_Id,
         new Ada_Visibibility_Assistant'
           (Database_Assistant with null record));
   end Register_Assistant;

   -------------------
   -- Get_Assistant --
   -------------------

   function Get_Assistant
     (Db : Construct_Database_Access) return Database_Assistant_Access
   is
   begin
      return Get_Assistant (Db, Ada_Visibility_Id);
   end Get_Assistant;

   -------------------------------
   -- Is_Public_Library_Visible --
   -------------------------------

   function Is_Public_Library_Visible (Entity : Entity_Access) return Boolean
   is
   begin
      return Get_Construct (Entity).Attributes
        (Ada_Library_Visibility_Attribute);
   end Is_Public_Library_Visible;

   ------------------
   -- File_Updated --
   ------------------

   overriding procedure File_Updated
     (Assistant : access Ada_Visibibility_Assistant;
      File      : Structured_File_Access;
      Kind      : Update_Kind)
   is
      pragma Unreferenced (Assistant);

      --  We're doing this analyzis right after file update - because it
      --  doesn't seem to be an expensive one and there's no reason to delay
      --  it. If it appears not to be the case in the future, it's perfectly
      --  reasonable to delay it until the first Is_Public_Visible call is
      --  made.

      use Construct_Annotations_Pckg;

      Tree : constant Construct_Tree := Get_Tree (File);

      procedure Add_Scope
        (Scope           : Construct_Tree_Iterator;
         Parameters_Only : Boolean := False);

      procedure Mark_Scope_Not_Vible (Scope : Construct_Tree_Iterator);

      ---------------
      -- Add_Scope --
      ---------------

      procedure Add_Scope
        (Scope           : Construct_Tree_Iterator;
         Parameters_Only : Boolean := False)
      is
         It        : Construct_Tree_Iterator;
         Scope_End : Construct_Tree_Iterator;
      begin
         if Scope = Null_Construct_Tree_Iterator then
            It := First (Tree);
         else
            It := Next (Tree, Scope, Jump_Into);
         end if;

         while Is_Parent_Scope (Scope, It) loop
            if Parameters_Only and then
              Get_Construct (It).Category /= Cat_Parameter
            then
               exit;
            end if;

            if Get_Construct (It).Visibility = Visibility_Public then
               Get_Construct (It).Attributes
                 (Ada_Library_Visibility_Attribute) := True;

               if not Parameters_Only then
                  if (Get_Construct (It).Category = Cat_Package
                      and then Get_Construct (It).Is_Declaration)
                    or else Get_Construct (It).Category
                  in Cat_Class .. Cat_Type
                  then
                     Add_Scope (It);
                  elsif Get_Construct (It).Category
                  in Cat_Task .. Cat_Entry
                  then
                     Add_Scope (It, True);
                  else
                     Mark_Scope_Not_Vible (It);
                  end if;
               end if;
            else
               exit;
            end if;

            It := Next (Tree, It, Jump_Over);
         end loop;

         if Scope /= Null_Construct_Tree_Iterator then
            Scope_End := Next (Tree, Scope, Jump_Over);
         else
            Scope_End := Null_Construct_Tree_Iterator;
         end if;

         while It /= Scope_End loop
            Get_Construct (It).Attributes
              (Ada_Library_Visibility_Attribute) := False;

            It := Next (Tree, It, Jump_Into);
         end loop;
      end Add_Scope;

      ----------------------------
      -- Mark_Scope_Not_Visible --
      ----------------------------

      procedure Mark_Scope_Not_Vible (Scope : Construct_Tree_Iterator) is
         It : Construct_Tree_Iterator := Next (Tree, Scope, Jump_Into);
         Scope_End : constant Construct_Tree_Iterator :=
           Next (Tree, Scope, Jump_Over);
      begin
         while It /= Scope_End loop
            Get_Construct (It).Attributes
              (Ada_Library_Visibility_Attribute) := False;

            It := Next (Tree, It, Jump_Into);
         end loop;
      end Mark_Scope_Not_Vible;

   begin
      case Kind is
         when Minor_Change =>
            null;

         when Full_Change | Structural_Change =>
            Add_Scope (Null_Construct_Tree_Iterator);

      end case;
   end File_Updated;

   ---------------------------
   -- Get_Location_Relation --
   ---------------------------

   function Get_Location_Relation
     (Tree_To     : Construct_Tree;
      Object_To   : Construct_Tree_Iterator;
      Tree_From   : Construct_Tree;
      Offset_From : Integer) return Location_Relation
   is
      Path_To   : constant Construct_Tree_Iterator_Array :=
        Full_Construct_Path (Tree_To, Object_To);

      Path_From : constant Construct_Tree_Iterator_Array :=
        Full_Construct_Path (Tree_From, Offset_From);
   begin
      return Get_Location_Relation (Tree_To, Path_To, Tree_From, Path_From);
   end Get_Location_Relation;

   function Get_Location_Relation
     (Tree_To   : Construct_Tree;
      Path_To   : Construct_Tree_Iterator_Array;
      Tree_From : Construct_Tree;
      Path_From : Construct_Tree_Iterator_Array)
      return Location_Relation
   is
   begin
      if Path_To'Length > Path_From'Length then
         return None;
      elsif Path_To'Length = 0 or else Path_From'Length = 0 then
         return None;
      elsif Get_Construct (Path_To (1)).Name = null
        or else Get_Construct (Path_From (1)).Name = null
      then
         return None;
      end if;

      declare
         Root_To_Name   : constant Composite_Identifier :=
           To_Composite_Identifier
             (Get_Construct (Path_To (1)).Name.all);
         Root_From_Name : constant Composite_Identifier :=
           To_Composite_Identifier (Get_Construct (Path_From (1)).Name.all);

         Index_In_To_Root, Index_In_From_Root : Integer := 1;
         Index_In_To_Path, Index_In_From_Path : Integer := 1;
      begin
         loop
            if Index_In_To_Path > Path_To'Last then
               --  If we successfully iterate over all the "to" path, it means
               --  that either "from" is the same entity, or a unit child of
               --  this entity. Return the max potential visibility

               if Tree_To = Tree_From
                 and then Encloses
                   (Path_To (Path_To'Last),
                    Get_Construct
                      (Path_From (Path_From'Last)).Sloc_Start.Index)
                 and then not Get_Construct
                   (Path_To (Path_To'Last)).Is_Declaration
               then
                  return Package_Body;
               else
                  return Full_Spec_Hierarchy;
               end if;
            elsif Index_In_From_Path > Path_From'Last then
               --  If we iterated over all the "from" path, but there are
               --  still "to" to be iterated, then "from" is a child or a
               --  content of "to". Thus, "to" doesn't have any particular
               --  visibility priviledge other than public.

               return None;
            elsif Index_In_To_Path > 1
              and then Index_In_From_Path > 1
            then
               --  If we analyzed the first item, then we only need to check
               --  the name of the remaining ones - no composite identifier is
               --  expected pass this point.

               if Get_Construct (Path_From (Index_In_From_Path)).Name = null
                 or else Get_Identifier (Path_To (Index_In_To_Path))
                 /= Get_Identifier (Path_From (Index_In_From_Path))
               then
                  return None;
               end if;
            elsif Index_In_From_Path = 1
              and then Index_In_To_Path = 1
            then
               --  We're still analyzing the first index - which may be a
               --  composite identifier.

               if not Equal
                 (Get_Item (Root_To_Name, Index_In_To_Root),
                  Get_Item (Root_From_Name, Index_In_From_Root),
                  False)
               then
                  return None;
               end if;
            elsif Index_In_From_Path > 1 then
               --  We're still analyzing the first index of "from", but already
               --  completely analyzed to. Check the next index of the
               --  composite "from" against the next path element of "to"

               if Get_Construct (Path_From (Index_In_From_Path)).Name = null
                 or else not Equal
                   (Get_Item (Root_To_Name, Index_In_To_Root),
                    Get_Construct (Path_From (Index_In_From_Path)).Name.all,
                    False)
               then
                  return None;
               end if;
            else
               --  We're still analyzing the first index of "to", but already
               --  completely analyzed to. Check the next index of the
               --  composite "to" against the next path element of "from"

               if Get_Construct
                 (Path_To (Index_In_To_Path)).Name = null
                 or else not Equal
                   (Get_Item (Root_From_Name, Index_In_From_Root),
                    Get_Construct
                      (Path_To (Index_In_To_Path)).Name.all,
                    False)
               then
                  return None;
               end if;
            end if;

            if Index_In_From_Root < Length (Root_From_Name) then
               Index_In_From_Root := Index_In_From_Root + 1;
            else
               Index_In_From_Path := Index_In_From_Path + 1;
            end if;

            if Index_In_To_Root < Length (Root_To_Name) then
               Index_In_To_Root := Index_In_To_Root + 1;
            else
               Index_In_To_Path := Index_In_To_Path + 1;
            end if;
         end loop;
      end;
   end Get_Location_Relation;

   -------------------
   -- Is_Accessible --
   -------------------

   function Is_Accessible
     (Entity      : Entity_Access;
      From_File   : Structured_File_Access;
      From_Offset : Integer) return Boolean
   is
      Rel : Location_Relation;
   begin
      --  If the entity is public library visible, then we can have access to
      --  it.

      if Is_Public_Library_Visible (Entity) then
         return True;
      end if;

      --  Otherwise, we've got to have visibility on the proper hidden package
      --  part

      Rel := Get_Location_Relation
        (Tree_To     => Get_Tree (Get_File (Entity)),
         Object_To   => Get_Parent_Scope
           (Get_Tree (Get_File (Entity)), To_Construct_Tree_Iterator (Entity)),
         Tree_From   => Get_Tree (From_File),
         Offset_From => From_Offset);

      case Rel is
         when  Package_Body =>
            --  If we've got visibility on the entire package, then
            --  it's OK.

            return True;

         when Full_Spec_Hierarchy =>
            --  If we've got visibility only on the private spec, then
            --  check if the entity is indeed coming from the spec.

            return Get_Construct (Get_Parent_Scope
              (Get_Tree
                 (Get_File (Entity)),
                 To_Construct_Tree_Iterator (Entity))).Is_Declaration;

         when None | Public_Spec_Hierarchy =>
            --  If we have just visibilty on the public part, since
            --  this is not a public library visible package, we can't
            --  see it.

            return False;

      end case;
   end Is_Accessible;

end Ada_Semantic_Tree.Visibility;
