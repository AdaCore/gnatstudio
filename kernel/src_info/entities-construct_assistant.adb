-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

with Basic_Types; use Basic_Types;

package body Entities.Construct_Assistant is

   LI_Assistant_Id : constant String := "LI_ASSISTANT";

   type LI_Db_Assistant is new Database_Assistant with record
      LI_Key : Construct_Annotations_Pckg.Annotation_Key;
      LI_Db  : Entities_Database;
   end record;

   type LI_Db_Assistant_Access is access all LI_Db_Assistant'Class;

   type LI_Annotation is new
     Construct_Annotations_Pckg.General_Annotation_Record
   with record
      LI_Entity : Entity_Information;
   end record;

   overriding
   procedure Free (Obj : in out LI_Annotation);

   ------------------------
   -- Register_Assistant --
   ------------------------

   procedure Register_Assistant
     (Database : Construct_Database_Access;
      LI_Db    : Entities_Database)
   is
      use Construct_Annotations_Pckg;

      LI_Entity_Key : Construct_Annotations_Pckg.Annotation_Key;
   begin
      Get_Annotation_Key
        (Get_Construct_Annotation_Key_Registry (Database).all,
         LI_Entity_Key);

      Register_Assistant
        (Database,
         LI_Assistant_Id,
         new LI_Db_Assistant'
           (Database_Assistant with
            LI_Key => LI_Entity_Key,
            LI_Db  => LI_Db));
   end Register_Assistant;

   ------------------
   -- To_LI_Entity --
   ------------------

   function To_LI_Entity (E : Entity_Access) return Entity_Information is
      use Construct_Annotations_Pckg;

      Assistant : constant LI_Db_Assistant_Access := LI_Db_Assistant_Access
        (Get_Assistant (Get_Database (Get_File (E)), LI_Assistant_Id));

      Construct_Annotation : Construct_Annotations_Pckg.Annotation;
      New_Entity : Entity_Information;
   begin
      Get_Annotation
        (Get_Annotation_Container
           (Get_Tree (Get_File (E)), To_Construct_Tree_Iterator (E)).all,
         Assistant.LI_Key,
         Construct_Annotation);

      if Construct_Annotation = Construct_Annotations_Pckg.Null_Annotation then
         --  Create a new LI entity

         Construct_Annotation := (Other_Kind, Other_Val => new LI_Annotation);

         New_Entity :=
           new Entity_Information_Record'
             (Name                         =>
                 Assistant.LI_Db.Symbols.Find (Get_Construct (E).Name.all),
              Kind                         => Unresolved_Entity_Kind,
              Attributes                   => (others => False),
              Declaration                  =>
                (Get_Or_Create
                   (Db    => Assistant.LI_Db,
                    File  => Get_File_Path (Get_File (E))),
                 Get_Construct (E).Sloc_Entity.Line,
                 To_Visible_Column
                   (Get_File (E),
                    Get_Construct (E).Sloc_Entity.Line,
                    String_Index_Type (Get_Construct (E).Sloc_Entity.Column))),
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
              Is_Valid                     => False,
              Ref_Count                    => 1,
              Trie_Tree_Index              => 0,
              Is_Dummy                     => True);

         --  Make a simple association between construct categories and entity
         --  categories. This association is know to be unaccurate, but is
         --  helpful when trying to categorize entities.

         case Get_Construct (E).Category is
            when Cat_Package
              | Cat_Namespace =>

               New_Entity.Kind.Kind := Package_Kind;

            when Cat_Task
               | Cat_Procedure
               | Cat_Function
               | Cat_Method
               | Cat_Constructor
               | Cat_Destructor
               | Cat_Protected
               | Cat_Entry =>

               New_Entity.Kind.Kind := Procedure_Kind;

            when Cat_Class
               | Cat_Structure
               | Cat_Case_Inside_Record
               | Cat_Union
               | Cat_Type
               | Cat_Subtype =>

               New_Entity.Kind.Kind := Class;
               New_Entity.Kind.Is_Type := True;

            when Cat_Variable
               | Cat_Local_Variable
               | Cat_Parameter
               | Cat_Discriminant
               | Cat_Field =>

               New_Entity.Kind.Kind := Signed_Integer;

            when Cat_Literal =>

               New_Entity.Kind.Kind := Enumeration_Literal;

            when Cat_With
               | Cat_Use
               | Cat_Include =>

               New_Entity.Kind.Kind := Include_File;

            when Cat_Unknown
               | Cat_Representation_Clause
               | Cat_Loop_Statement
               | Cat_If_Statement
               | Cat_Case_Statement
               | Cat_Select_Statement
               | Cat_Accept_Statement
               | Cat_Declare_Block
               | Cat_Return_Block
               | Cat_Simple_Block
               | Cat_Exception_Handler
               | Cat_Pragma
               | Cat_Custom =>

               New_Entity.Kind.Kind := Unresolved_Entity;
         end case;

         Ref (New_Entity.Declaration.File);

         LI_Annotation (Construct_Annotation.Other_Val.all).LI_Entity :=
           New_Entity;
         Set_Annotation
           (Get_Annotation_Container
              (Get_Tree
                 (Get_File (E)), To_Construct_Tree_Iterator (E)).all,
            Assistant.LI_Key,
            Construct_Annotation);
      else
         --  Update the entity in case it has moved

         LI_Annotation (Construct_Annotation.Other_Val.all).
           LI_Entity.Declaration.Line := Get_Construct (E).Sloc_Entity.Line;
         LI_Annotation (Construct_Annotation.Other_Val.all).
           LI_Entity.Declaration.Column := To_Visible_Column
             (Get_File (E),
              Get_Construct (E).Sloc_Entity.Line,
              String_Index_Type (Get_Construct (E).Sloc_Entity.Column));
      end if;

      return LI_Annotation (Construct_Annotation.Other_Val.all).LI_Entity;
   end To_LI_Entity;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Obj : in out LI_Annotation) is
   begin
      Unref (Obj.LI_Entity);
   end Free;

end Entities.Construct_Assistant;
