------------------------------------------------------------------------------
--                                  G P S                                   --
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

--  This package computes various visibility information on trees.

with Ada_Semantic_Tree.Generics; use Ada_Semantic_Tree.Generics;

package Ada_Semantic_Tree.Visibility is

   procedure Register_Assistant (Db : Construct_Database_Access);
   --  This assistant has to be registered to the database before any of the
   --  queries in this file can work.

   function Get_Assistant
     (Db : Construct_Database_Access) return Database_Assistant_Access;
   --  Return the assistant responsible of doing visibility analysis.

   function Is_Public_Library_Visible (Entity : Entity_Access) return Boolean;
   pragma Inline (Is_Public_Library_Visible);
   --  Return true if the entity given in parameter is public library visible,
   --  false otherwise.

   type Location_Relation is
     (None,
      --  The location isn't located anywhere special.

      Public_Spec_Hierarchy,
      --  The location is in the public spec hierarchy. Ada-wise, the
      --  visibility rules are the same than None, but the location is clearly
      --  in the package hierarchy.
      --  ??? This is not computed by Get_Location_Relation yet.

      Full_Spec_Hierarchy,
      --  The location is in the full spec hierarchy, that is to say it has
      --  visibility on the public and the private part.

      Package_Body
      --  The location is in the given package body, and has visibility over
      --  all the parent's specs plus the last package body
     );

   --  ??? The computation of this subprogram is currently quite expensive -
   --  based on name comparisons. See if it would be more efficient to base
   --  it on computed parts informations
   function Get_Location_Relation
     (Tree_To     : Construct_Tree;
      Object_To   : Construct_Tree_Iterator;
      Tree_From   : Construct_Tree;
      Offset_From : String_Index_Type) return Location_Relation;
   --  Computes the relation of Tree/Object_To, as seen from
   --  Tree/Offset_From

   function Get_Location_Relation
     (Tree_To   : Construct_Tree;
      Path_To   : Construct_Tree_Iterator_Array;
      Tree_From : Construct_Tree;
      Path_From : Construct_Tree_Iterator_Array)
      return Location_Relation;
   --  Same as above, with pre-computed paths.

   function Is_Accessible
     (Entity      : Entity_Access;
      From_File   : Structured_File_Access;
      From_Offset : String_Index_Type) return Boolean;
   --  Return true if the entity is accessible, that is to say either
   --  it's a public library visible entity, or it's in one of the visible
   --  areas of the unit hierarchy from the location given in parameter.
   --  Note that getting access to that entity may require additional use or
   --  with clases, or prefix.

   function Is_Visible_From_Clauses
     (Entity         : Entity_Access;
      From_Visiblity : Visibility_Context) return Entity_Access;
   --  Return the with or use clause from which the entity is visible if there
   --  is such a clause, according visibility given in parameter.

   type Clause_Iterator is private;

   function To_Clause_Iterator
     (Visibility_Info : Visibility_Context;
      Category        : Language_Category) return Clause_Iterator;
   --  Create a iterator looking at all the use clauses from the context given
   --  in parameter. Category can be either Cat_Use
   --  (retreiving all use clauses), Cat_With (retreiving all with clauses) or
   --  Cat_Unknown (retreiving both)

   function Is_Valid (This : Clause_Iterator) return Boolean;
   --  Return true if the iterator either point to a valid node, or is at end.
   --  Should always be true.

   procedure Prev (This : in out Clause_Iterator);
   --  Moves to the previous clause

   function At_End (This : Clause_Iterator) return Boolean;
   --  Return true if the iterator is before the first clause

   function Get_Entity (This : Clause_Iterator) return Entity_Access;
   --  Return the entity pointed by this clause

   function Resolve_Package (This : Clause_Iterator) return Entity_Access;
   --  Computes the package name corresponding to the clause pointed by the
   --  iterator.

   function Get_Generic_Context
     (This : Clause_Iterator) return Instance_Info;
   --  If this clause points to a generic instance, retreives the corresponding
   --  generic context.

   function Resolve_Clause (Entity : Entity_Access) return Entity_Access;
   --  If the entity is a use or a with clause, return the corresponding
   --  withed or used entity.

private

   type Clause_Iterator is record
      Current   : Entity_Access;
      Category  : Language_Category;
   end record;

end Ada_Semantic_Tree.Visibility;
