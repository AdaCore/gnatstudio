------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

--  This packages manages with / use resolution, and gives a way of retreiving
--  entities from a unit hierarchy.

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;

with GNAT.Strings;     use GNAT.Strings;

with Ada_Semantic_Tree.Generics; use Ada_Semantic_Tree.Generics;
with Ada_Semantic_Tree.Units; use Ada_Semantic_Tree.Units;

private with Ada.Unchecked_Deallocation;

package Ada_Semantic_Tree.Dependency_Tree is

   procedure Register_Assistant (Database : Construct_Database_Access);
   --  This assistant has to be registered to the database before any of the
   --  queries in this file can work.

   type Visibility_Resolver is private;
   --  This type is used to store visibility status entities - it should be
   --  able to tell if an entity is visible according to scope visibility,
   --  use and with clauses.
   --  ??? currently only scope visibility is implemented.

   function Is_Hidden
     (Resolver : Visibility_Resolver; Name : String) return Boolean;
   --  Return true if all entities with the name given in parameter will be
   --  hidden according to the information stored in the visibility resolver.

   function Is_Hidden
     (Resolver : Visibility_Resolver; Entity : Entity_Access) return Boolean;
   --  Return true if this entity is hidden according to the visibilty resolver

   procedure Add_Hiding_Entity
     (Resolver : in out Visibility_Resolver; Entity : Entity_Access);
   --  Add an hiding entity in the resolver list

   procedure Clear (Resolver : in out Visibility_Resolver);
   --  Clear all information stored in the resolver - which can then be re-used
   --  for a new analysis.

   procedure Clear (Resolver : in out Visibility_Resolver; Name : String);
   --  Clear all information stored only for the given name.

   procedure Free (Resolver : in out Visibility_Resolver);
   --  Free the information associated to this resolver.

   function Get_Local_Visible_Constructs
     (File       : Structured_File_Access;
      Offset     : String_Index_Type;
      Name       : Normalized_Symbol;
      Visibility : not null access Visibility_Resolver;
      Filter     : Entity_Filter;
      Use_Wise   : Boolean := True;
      Is_Partial : Boolean := False)
      return Entity_Array;
   --  Return the constructs visible from the location given in parameter.
   --  Visiblity_Resolver will be used and updated during this process. It's
   --  expected to be cleared at the beginning, and will be set to the
   --  visibility information computed afterwards.

   function Is_Locally_Visible
     (File     : Structured_File_Access;
      Offset   : String_Index_Type;
      Entity   : Entity_Access;
      Use_Wise : Boolean := True) return Boolean;
   --  Return true if the entity is locally visible from the file. This is just
   --  a convenient way of calling Get_Local_Visible_Constructs and comparing
   --  the entity with its result - but the complexity of the algorithm is the
   --  same.

   type Local_Visible_Construct_Iterator is private;
   --  ??? This type is not usable yet - but it offer a nice alternative to the
   --  use of the expensive Get_Local_Visible_Constructs, and could even be
   --  used for datababase wide entities. To be investigated.

   Null_Local_Visible_Construct_Iterator : constant
     Local_Visible_Construct_Iterator;

   function First
     (File       : Structured_File_Access;
      Offset     : String_Index_Type;
      Name       : String;
      Use_Wise   : Boolean := True;
      Is_Partial : Boolean := False)
      return Local_Visible_Construct_Iterator;
   --  Return the first match from the location given in parameter

   procedure Next (It : in out Local_Visible_Construct_Iterator);
   --  Moves the iterator to the next match

   function At_End (It : Local_Visible_Construct_Iterator) return Boolean;
   --  Return true if there is no more entity to pick up.

   function Get (It : Local_Visible_Construct_Iterator) return Entity_Access;
   --  Return the entity pointed by this iterator

   function Is_Valid (It : Local_Visible_Construct_Iterator) return Boolean;
   --  Return true if this iterator is in a valid state.

   procedure Free (It : in out Local_Visible_Construct_Iterator);
   --  Free the data associated to this iterator.

   type Clause_Info is private;
   --  Represents the information of a use or with clause

   function Get_Clause_Info (Entity : Entity_Access) return Clause_Info;
   --  If this entity points to a use or with clause, return the clause
   --  information.

   function Get_Target (Clause : Clause_Info) return Entity_Access;
   --  Returns the target package of this clause, e.g. the withed or used
   --  package.

   function Get_Generic_Context (Clause : Clause_Info) return Instance_Info;
   --  Return the generic context associated to this clause if the clause is
   --  coming from an instantiation.

   procedure Update_Dependency_Information_If_Needed (Unit : Unit_Access);
   --  Update the with / use clause information from the unit if a unit of the
   --  hierarchy has changed.

private

   package Entity_List is new
     Ada.Containers.Doubly_Linked_Lists (Entity_Access);

   use Entity_List;

   type Unit_Array is array (Integer range <>) of Unit_Access;

   type Unit_Array_Access is access all Unit_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Unit_Array, Unit_Array_Access);

   function Is_Before (Left, Right : Entity_Access) return Boolean;

   package Ordered_Entities is new Ada.Containers.Ordered_Sets
     (Entity_Access, Is_Before);

   use Ordered_Entities;

   type Ordered_Entities_Access is access all Ordered_Entities.Set;

   procedure Free is new
     Ada.Unchecked_Deallocation
       (Ordered_Entities.Set, Ordered_Entities_Access);

   type Local_Visible_Construct_Iterator is record
      Units              : Unit_Array_Access;
      It_In_Units        : Integer;
      Name               : String_Access;
      Is_Partial         : Boolean;
      Parts_Assistant    : Database_Assistant_Access;
      Entity_At_Location : Entity_Access;
      Used_Packages      : Ordered_Entities_Access;
      Ordered_Results    : Ordered_Entities_Access;
      It                 : Ordered_Entities.Cursor;
   end record;

   Null_Local_Visible_Construct_Iterator : constant
     Local_Visible_Construct_Iterator :=
       (Units              => null,
        It_In_Units        => 0,
        Name               => null,
        Is_Partial         => False,
        Parts_Assistant    => null,
        Entity_At_Location => Null_Entity_Access,
        Used_Packages      => null,
        Ordered_Results    => null,
        It                 => Ordered_Entities.No_Element);

   type Entity_List_Access is access all Entity_List.List;

   package Named_Entities is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Entity_List_Access);

   use Named_Entities;

   type Named_Entities_Access is access all Named_Entities.Map;

   type Visibility_Resolver is record
      Hiding_Entities : Named_Entities_Access;
   end record;

   type Clause_Info is record
      Entity : Entity_Persistent_Access;
      Generic_Context : Persistent_Instance_Info :=
        Null_Persistent_Instance_Info;
   end record;

end Ada_Semantic_Tree.Dependency_Tree;
