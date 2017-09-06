------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2017, AdaCore                     --
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

--  This package provides ada compilation units analysis / extraction

with Ada.Containers.Ordered_Sets;

with Construct_Tries;

package Ada_Semantic_Tree.Units is

   procedure Register_Assistant (Db : Construct_Database_Access);
   --  This assistant has to be registered to the database before any of the
   --  queries in this file can work.

   type Unit_Iterator is private;
   --  This types iterated over a list of units, created from various
   --  constraints.

   Null_Unit_Iterator : constant Unit_Iterator;

   procedure Next (It : in out Unit_Iterator);
   --  Moves the iterator to the next unit.

   function At_End (It : Unit_Iterator) return Boolean;
   --  Return true if there's no more units to pick up

   procedure Free (It : in out Unit_Iterator);
   --  Free the data associated to this iterator.

   type Unit_Access is private;
   --  This type points to the information of a given unit. Spec & body of a
   --  same compilation unit are two distinct Unit_Access objects.

   Null_Unit_Access : constant Unit_Access;

   function Get (It : Unit_Iterator) return Entity_Access;
   --  Return the entity pointed by this iterator.

   function Get (It : Unit_Iterator) return Unit_Access;
   --  Return the unit pointed by this iterator.

   function Get_Parent (Unit : Unit_Access) return Unit_Access;
   --  Return the parent of this unit if any. Return Null_Unit_Access
   --  otherwise.

   function Get_Entity (Unit : Unit_Access) return Entity_Access;
   --  Return the entity referenced by this unit.

   function Get_Units
     (Db         : Construct_Database_Access;
      Name       : String;
      Is_Partial : Boolean) return Unit_Iterator;
   --  Return an iterator iterating over all the units of a given name,
   --  project-wise. If Is_Partial is true, all units starting with the given
   --  name will be iterated over. The name of the unit is the name of the
   --  package, without the dots.

   function Get_Unit
     (Db : Construct_Database_Access; Name : String) return Unit_Access;
   --  Return the unit of the given name - which may include dots if it's a
   --  child unit.

   function Get_Units
     (File : Structured_File_Access) return Unit_Iterator;
   --  Return all the units contained in that file.

   function Get_Children (Unit : Unit_Access) return Unit_Iterator;
   --  Return all the children of the unit given in parameter.

   function Get_Owning_Unit
     (File : Structured_File_Access;
      Offset : String_Index_Type) return Unit_Access;
   --  Return the unit related to this location in the file.

   function Get_Owning_Unit (Entity : Entity_Access) return Unit_Access;
   --  Return the unit on which this entity is related to - either because
   --  it's within the entity or because it's a clause applying to this
   --  unit. May return null if the unit can't be retreive, for example if
   --  there are major syntax errors or unclosed blocks.

   function Get_Start_Entity (Unit : Unit_Access) return Entity_Access;
   pragma Inline (Get_Start_Entity);
   --  Return the first entity related to this unit. It can be either the
   --  unit itself or the first clause applying to this unit.

   function Get_End_Entity (Unit : Unit_Access) return Entity_Access;
   pragma Inline (Get_End_Entity);
   --  Return the entity located after the last entity related to that unit.
   --  If there's no entity after, then this function returns a
   --  Null_Entity_Access;

   function Get_Unit_Access (Unit : Entity_Access) return Unit_Access;
   --  If the entity given in parameter is a unit, this will return the
   --  corresponding unit access. Otherwise, Null_Unit_Access will be returned.

   function Get_Unit_Body (Unit : Unit_Access) return Unit_Access;
   --  Return the body of this units, if any. Otherwise, Null_Unit_Access will
   --  be returned.

   function Get_Unit_Spec (Unit : Unit_Access) return Unit_Access;
   --  Return the spec of se units, if any.  Otherwise, Null_Unit_Access will
   --  be returned.

   function Get_Name (Unit : Unit_Access) return Composite_Identifier;
   --  Return the name of this unit as a composite identifier.

   function Is_In_Parents
     (Parent : Unit_Access; Child : Unit_Access) return Boolean;
   --  Return true is Parent is one of the parents (direct or indirect) of
   --  child.

   function Get_Dependency_Timestamp (Unit : Unit_Access) return Integer;
   --  Return the date when the dependency (from dependency_tree) have been
   --  computed for this unit.

   procedure Set_Dependency_Timestamp
     (Unit : Unit_Access; Timestamp : Integer);
   --  Set the date when the dependency (from dependency_tree) have been
   --  computed for this unit.

   function Get_Current_Timestamp (Unit : Unit_Access) return Integer;
   --  Return the timestamp of this unit. Each time the unit is modified, the
   --  timestamp will be incremented.

   function Has_Updated_Dependencies (Unit : Unit_Access) return Boolean;
   --  Return true if the dependency information (from dependency_tree) is
   --  up to date

   procedure Set_Updated_Dependencies (Unit : Unit_Access; Dep : Boolean);
   --  Set if the dependency information (from dependency_tree) is
   --  up to date

   type Unit_Hierarchy_Timestamp is private;
   --  This type holds timestamp representing a stamp for a
   --  hierarchy of units, namely, a unit and its parents. It can be used to
   --  know if a hierarchy has changed, and thus if references depending on
   --  this unit should change as well. Storing the timestamp of one single
   --  unit is often not enough, as some of its component might depend on
   --  with / use clauses located in the parents.

   function Get_Unit_Hierarchy_Timestamp
     (Unit : Unit_Access) return Unit_Hierarchy_Timestamp;
   --  Return the unit hierarchy timestamp starting at that unit

   procedure Set_Parts_Up_To_Date (Unit : Unit_Access; Value : Boolean);
   --  This is used by the Ada_Parts package, to keep track of the validity of
   --  the parts information

   function Get_Parts_Up_To_Date (Unit : Unit_Access) return Boolean;
   --  Return wether the parts information are up to date for this unit.

   type Local_Construct_Iterator is private;
   --  This types provides a way to iterate over the local entities of this
   --  unit, in the alphabetical order.

   Null_Local_Construct_Iterator : constant Local_Construct_Iterator;

   function First
     (Unit : Unit_Access; Name : String; Is_Partial : Boolean)
      return Local_Construct_Iterator;
   --  Initializes an iterator on the first entity of the given name in the
   --  given unit.

   procedure Next (It : in out Local_Construct_Iterator);
   --  Moves the iterator to the next entity.

   function Get (It : Local_Construct_Iterator) return Entity_Access;
   --  Return the entity pointed by this iterator.

   function At_End (It : Local_Construct_Iterator) return Boolean;
   --  Return true if this iterator doesn't have any more entities.

   procedure Free (It : in out Local_Construct_Iterator);
   --  Free the data associated with this iterator.

private

   type Unit_Access_Record;

   type Unit_Access is access all Unit_Access_Record;

   function "<" (Left, Right : Unit_Access) return Boolean;

   Null_Unit_Access : constant Unit_Access := null;

   package Construct_Unit_Tries is new
     Construct_Tries (Unit_Access, Null_Unit_Access);

   use Construct_Unit_Tries;
   use Construct_Unit_Tries.Construct_Trie_Trees;

   package Unit_Set_Pckg is new
     Ada.Containers.Ordered_Sets (Unit_Access);

   package Persistent_Entity_List is new
     Ada.Containers.Ordered_Sets (Entity_Persistent_Access);
   use Persistent_Entity_List;

   package Local_Construct_Trie is new Construct_Tries (Integer, 0);

   use Local_Construct_Trie;
   use Local_Construct_Trie.Construct_Trie_Trees;

   type Unit_Access_Record is new
     Construct_Annotations_Pckg.General_Annotation_Record
   with record
      Name             : Composite_Identifier_Access;

      Entity           : Entity_Persistent_Access :=
        Null_Entity_Persistent_Access;
      Parent           : Entity_Persistent_Access :=
        Null_Entity_Persistent_Access;
      Parent_Is_Spec   : Boolean := False;

      Body_Unit        : Entity_Persistent_Access :=
        Null_Entity_Persistent_Access;
      Spec_Unit        : Entity_Persistent_Access :=
        Null_Entity_Persistent_Access;

      Start_Entity     : Entity_Persistent_Access :=
        Null_Entity_Persistent_Access;
      End_Entity       : Entity_Persistent_Access :=
        Null_Entity_Persistent_Access;

      Unit_Key         : Construct_Annotations_Pckg.Annotation_Key;

      This_Timestamp   : Integer := 0;

      Dep_Timestamp    : Integer := 0;
      Is_Up_To_Date    : Boolean := False;
      --  These two fields are used by dependency analysis

      Db_Index         : Construct_Unit_Tries.Construct_Trie_Index;

      Children_Units   : Persistent_Entity_List.Set;

      Parts_Up_To_Date : Boolean := False;
      --  This field is used by part analysis

      Local_Constructs : aliased Local_Construct_Trie.Construct_Trie;
      Local_Constructs_Up_To_Date : Boolean := False;

      Waiting_For_Parent_Index : Construct_Unit_Tries.Construct_Trie_Index :=
        Construct_Unit_Tries.Null_Construct_Trie_Index;
      --  Index in the waiting for parents entity (see the assistant
      --  declaration in this unit body for more details).
   end record;

   overriding
   procedure Free (Unit : in out Unit_Access_Record);

   type Unit_Iterator is record
      Unit_Cursor : Persistent_Entity_List.Cursor;
      Db_Iterator : Construct_Unit_Tries.Construct_Trie_Iterator;

      Unit_Key      : Construct_Annotations_Pckg.Annotation_Key;
      Unit_List_Key : Tree_Annotations_Pckg.Annotation_Key;
   end record;

   Null_Unit_Iterator : constant Unit_Iterator :=
     (Persistent_Entity_List.No_Element,
      Construct_Unit_Tries.Null_Construct_Trie_Iterator,
      Construct_Annotations_Pckg.Null_Annotation_Key,
      Tree_Annotations_Pckg.Null_Annotation_Key);

   type Unit_Hierarchy_Timestamp is new Integer;
   --  We can represent a unit hierarchy timestamp as the sum of all indivdual
   --  timestamps. The modifications done on a project shouldn't exeed
   --  Integer'Last, and as all timestamps are systematically increasing
   --  there's no possible ambiguity.

   type Local_Construct_Iterator is record
      File : Structured_File_Access;
      It   : Local_Construct_Trie.Construct_Trie_Iterator;
   end record;

   Null_Local_Construct_Iterator : constant Local_Construct_Iterator :=
     (null, Local_Construct_Trie.Null_Construct_Trie_Iterator);

end Ada_Semantic_Tree.Units;
