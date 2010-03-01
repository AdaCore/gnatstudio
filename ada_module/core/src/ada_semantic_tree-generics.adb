-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2010, AdaCore                   --
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

with Language.Ada;                    use Language.Ada;
with Ada_Semantic_Tree.Parts;         use Ada_Semantic_Tree.Parts;
with Ada_Semantic_Tree.Declarations;  use Ada_Semantic_Tree.Declarations;
with Ada.Unchecked_Deallocation;

package body Ada_Semantic_Tree.Generics is

   Generic_Assistant_Id : constant String := "ADA_GENERIC_ASSISTANT";

   type Generic_Db_Assistant is new Database_Assistant with record
      Instance_Key : Construct_Annotations_Pckg.Annotation_Key;
   end record;

   function Make_Generic_Instance
     (E : Entity_Access) return Generic_Instance_Information;

   ------------------------
   -- Register_Assistant --
   ------------------------

   procedure Register_Assistant (Db : Construct_Database_Access) is
      use Language.Tree.Construct_Annotations_Pckg;

      Instance_Key : Construct_Annotations_Pckg.Annotation_Key;
   begin
      Get_Annotation_Key
        (Get_Construct_Annotation_Key_Registry (Db).all,
         Instance_Key);

      Register_Assistant
        (Db,
         Generic_Assistant_Id,
         new Generic_Db_Assistant'
           (Database_Assistant with
            Instance_Key => Instance_Key));
   end Register_Assistant;

   ---------
   -- Ref --
   ---------

   procedure Ref (This : in out Generic_Instance_Information) is
   begin
      if This /= null then
         This.Refs := This.Refs + 1;
      end if;
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (This : in out Generic_Instance_Information) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Generic_Instance_Information_Record, Generic_Instance_Information);
   begin
      if This /= null then
         This.Refs := This.Refs - 1;

         if This.Refs <= 0 then
            Free (This.Resolver);
            Free (This);
         end if;
      end if;
   end Unref;

   ---------------------------
   -- Make_Generic_Instance --
   ---------------------------

   function Make_Generic_Instance
     (E : Entity_Access) return Generic_Instance_Information
   is
      Id : aliased UTF8_String := Get_Identifier
        (Get_Referenced_Identifiers (To_Construct_Tree_Iterator (E))).all;
      Expression : Parsed_Expression :=
        Ada_Lang.Parse_Expression_Backward
        (Id'Unchecked_Access, String_Index_Type (Id'Last));
      Generic_Resolution : Entity_List;
      It                 : Entity_Iterator;
      Generic_Package    : Entity_Access;

      Result : Generic_Instance_Information := null;
   begin
      Generic_Resolution := Find_Declarations
        (Context           =>
           (From_File,
            Get_File (E),
            String_Index_Type (Get_Construct (E).Sloc_Start.Index)),
         From_Visibility   =>
           (File                      => Get_File (E),
            Offset                    =>
              String_Index_Type (Get_Construct (E).Sloc_Start.Index),
            Filter                    => Everything,
            Min_Visibility_Confidence => Use_Visible),
         Expression        => Expression,
         Categories        => (1 => Cat_Package));

      It := First (Generic_Resolution);

      --  We only consider the first matching resolution, ignore the other
      --  potential ones.

      if not At_End (It) then
         Generic_Package := Get_First_Occurence (Get_Entity (It));

         declare
            Profile : constant List_Profile :=
              Get_List_Profile (Generic_Package, Generic_Profile);
         begin
            Result := new Generic_Instance_Information_Record;
            Result.Instance_Package := E;
            Result.Generic_Package := Generic_Package;
            Result.Resolver := new Actual_Parameter_Resolver'
              (Get_Actual_Parameter_Resolver (Profile));

            Append_Actuals
              (Result.Resolver.all,
               Get_Buffer (Get_File (E)),
               String_Index_Type (Get_Construct (E).Sloc_Start.Index));
         end;
      end if;

      Free (It);
      Free (Generic_Resolution);
      Free (Expression);

      return Result;
   end Make_Generic_Instance;

   -------------------------
   -- Is_Generic_Instance --
   -------------------------

   function Is_Generic_Instance (Entity : Entity_Access) return Boolean is
   begin
      return Get_Construct (Entity).Category = Cat_Package
        and then Get_Construct (Entity).Attributes (Ada_New_Attribute);
   end Is_Generic_Instance;

   --------------------------------------
   -- Get_Generic_Instance_Information --
   --------------------------------------

   function Get_Generic_Instance_Information
     (Entity : Entity_Access) return Generic_Instance_Information
   is
   begin
      if Is_Generic_Instance (Entity) then
         --  ??? see how to cache that information - or if we need to...
         return Make_Generic_Instance (Entity);
      else
         return null;
      end if;
   end Get_Generic_Instance_Information;

   ---------------------------
   -- Get_Generic_Instances --
   ---------------------------

   function Get_Generic_Instances
     (Entity     : Entity_Access;
      Visibility : Visibility_Context) return Generic_Instance_Array
   is
      pragma Unreferenced (Entity, Visibility);
      R : Generic_Instance_Array (1 .. 0);
   begin
      return R;
   end Get_Generic_Instances;

   ----------------------------
   -- Is_Viewed_From_Generic --
   ----------------------------

   function Is_Viewed_From_Generic
     (Entity : Entity_Access; Visibility : Visibility_Context)
      return Boolean
   is
      pragma Unreferenced (Entity, Visibility);
   begin
      return False;
   end Is_Viewed_From_Generic;

   -----------------------
   -- Is_Generic_Entity --
   -----------------------

   function Is_Generic_Entity (Entity : Entity_Access) return Boolean is
      It : Construct_Tree_Iterator :=
        Get_Parent_Scope
          (Get_Tree (Get_File (Entity)),
           To_Construct_Tree_Iterator (Entity));
      Enclosing_Entity : Entity_Access;
   begin
      --  ??? This may be a bit of a slow computation - should we cache the
      --  information instead?

      Enclosing_Entity :=
        Get_First_Occurence (To_Entity_Access (Get_File (Entity), It));
      It := To_Construct_Tree_Iterator (Enclosing_Entity);

      while It /= Null_Construct_Tree_Iterator loop
         Enclosing_Entity :=
           Get_First_Occurence
             (To_Entity_Access (Get_File (Enclosing_Entity), It));

         if Get_Construct (Enclosing_Entity).Attributes
           (Ada_Generic_Attribute)
         then
            return True;
         end if;

         It := Get_Parent_Scope
           (Get_Tree (Get_File (Enclosing_Entity)),
            To_Construct_Tree_Iterator (Enclosing_Entity));
      end loop;

      return False;
   end Is_Generic_Entity;

   ----------------------------------
   -- Get_Actual_For_Generic_Param --
   ----------------------------------

   function Get_Actual_For_Generic_Param
     (Info   : Generic_Instance_Information;
      Formal : Entity_Access) return Entity_Access
   is
      Formals : constant Entity_Array :=
        Get_Formals (Get_Profile (Info.Resolver.all));
      Result : Entity_Access := Null_Entity_Access;
   begin
      for J in Formals'Range loop
         if Formals (J) = Formal then
            declare
               Actual_Expression : constant Parsed_Expression :=
                 Get_Expression_For_Formal
                   (Info.Resolver.all, Get_Construct (Formals (J)).Name.all);
               Actual_Resolution : Entity_List;
               Actual_It         : Entity_Iterator;
            begin
               if Actual_Expression /= Null_Parsed_Expression then
                  Actual_Resolution :=
                    Find_Declarations
                      (Context           =>
                           (From_File,
                            Get_File (Info.Instance_Package),
                            String_Index_Type
                              (Get_Construct
                                 (Info.Instance_Package).Sloc_Start.Index)),
                       From_Visibility   =>
                         (File                 =>
                            Get_File (Info.Instance_Package),
                          Offset               =>
                            String_Index_Type
                              (Get_Construct
                                   (Info.Instance_Package).Sloc_Start.Index),
                          Filter                    => Everything,
                          Min_Visibility_Confidence => Use_Visible),
                       Expression        => Actual_Expression,
                       Categories        =>
                         (Cat_Class,
                          Cat_Structure,
                          Cat_Case_Inside_Record,
                          Cat_Union,
                          Cat_Type,
                          Cat_Subtype));
               end if;

               Actual_It := First (Actual_Resolution);

               if not At_End (Actual_It) then
                  Result := Get_Entity (Actual_It);
               end if;

               Free (Actual_It);
               Free (Actual_Resolution);
            end;

            exit;
         end if;
      end loop;

      return Result;
   end Get_Actual_For_Generic_Param;

   -------------------------
   -- Get_Generic_Package --
   -------------------------

   function Get_Generic_Package
     (Info : Generic_Instance_Information)
      return Entity_Access
   is
   begin
      return Info.Generic_Package;
   end Get_Generic_Package;

end Ada_Semantic_Tree.Generics;
