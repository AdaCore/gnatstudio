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

--  This package provides a way to analyze type trees, and in particular
--  tagged type trees.

with Ada.Unchecked_Deallocation;

with Ada_Semantic_Tree.Units;     use Ada_Semantic_Tree.Units;
with Ada.Containers.Ordered_Sets;

package Ada_Semantic_Tree.Type_Tree is

   procedure Register_Assistant (Db : Construct_Database_Access);
   --  This assistant has to be registered to the database before any of the
   --  queries in this file can work.

   type Ada_Type_Access is private;
   --  This type points to a given type information.

   Null_Ada_Type_Access : constant Ada_Type_Access;

   type Ada_Primitive_Access is private;
   --  This type points to a primitive information.

   type Primitive_Array is array (Integer range <>) of Ada_Primitive_Access;

   type Primitive_Array_Access is access all Primitive_Array;

   function Get_Ada_Type (Entity : Entity_Access) return Ada_Type_Access;
   --  Computes the ada type information out of an entity access if any, and
   --  return it. Will return Null_Ada_Type_Access if none. Type information is
   --  cached, so that calling this function on the same entity a second time
   --  is efficient.

   function Get_Entity (Ada_Type : Ada_Type_Access) return Entity_Access;
   --  Return the entity pointed by this ada type.

   function Extract_Primitives
     (Ada_Type : Ada_Type_Access) return Primitive_Array;
   --  Return all the primitives known for this type - it includes inherited
   --  but not overridden primitives.

   function Is_Tagged
     (Ada_Type : Ada_Type_Access; From_Visibility : Visibility_Context)
      return Boolean;
   --  Return true if the type given in parameter is tagged according to the
   --  context (if the type is privately tagged and we only have visibility
   --  on the public view, will return false).

   function Extract_Dotted_Notation_Sb
     (Ada_Type : Ada_Type_Access) return Entity_Persistent_Array;
   --  If the type given in parameter is tagged, this function will return all
   --  the entities that can be called through an Ada 2005 dot notation.
   --  Otherwise, will return an empty array.

   function Get_Entity (Primitive : Ada_Primitive_Access) return Entity_Access;
   --  Return the entity (subprogram) pointed by this primitive info.

   function Get_Tagged_Parent
     (Ada_Type : Ada_Type_Access) return Ada_Type_Access;
   --  If this type has got a tagged type parent, then it's returned by this
   --  function, otherwise null is returned.

   function Get_Entity_Or_Overridden
     (Primitive : Ada_Primitive_Access)
      return Entity_Access;
   --  Return either the entity pointed by this primitive if it's explicitely
   --  declared, or the closed overridden subprogram. If serveal matches (e.g.
   --  in the case of multiple interfaces inheritence), one is picked up
   --  randomly.

   function Get_Children
     (Ada_Type : Ada_Type_Access) return Entity_Persistent_Array;
   --  Return the children of this type. Note that this subprogram is lazy - it
   --  will only work with computed information. Information is computed in two
   --  cases, either when children information is requested, or after an
   --  Analyze_All_Types has been queried on the file.

   function Get_Parents
     (Ada_Type : Ada_Type_Access) return Entity_Persistent_Array;
   --  Return the parents of this type

   function First_Private_Parent
     (Ada_Type        : Ada_Type_Access;
      From_Visibility : Visibility_Context) return Ada_Type_Access;
   --  Return the first parent not visible from the current context - null if
   --  the whole hierarchy is visible.

   function Get_Fields_From
     (Ada_Type       : Ada_Type_Access;
      Starting_After : Ada_Type_Access) return Entity_Array;
   --  Return fields from Ada_Type, ignoring the ones from Starting_After and
   --  its parents (if not null). This is typically usefull when computing
   --  extension aggregates.

   procedure Analyze_All_Types (File : Structured_File_Access);
   --  Analyzes and update all the type information for this file. Normally,
   --  type information is computed laizyly so that we don't spend time
   --  processing the information, but this may be needed when doing e.g.
   --  global searches. Queries of children of a type may not be up to date
   --  before having called that on every file.

private

   type Primitive_Subprogram is record
      Entity                : Entity_Persistent_Access;
      --  If the primitive is declared or overridden for this type, this holds
      --  the corresponding entity. Otherwise, it's null.

      Overridden_Entities : Primitive_Array_Access;

      Is_Returned_Primitive : Boolean := False;
      --  This is true when the returned type is a primitive type.

      Refs : Natural := 0;
   end record;

   procedure Ref (Sb : Ada_Primitive_Access);

   procedure Unref (Sb : in out Ada_Primitive_Access);

   type Ada_Primitive_Access is access all Primitive_Subprogram;

   procedure Free is new Standard.Ada.Unchecked_Deallocation
     (Primitive_Array, Primitive_Array_Access);

   type Timestamped_Entity is record
      Timestamp : Integer;
      Entity    : Entity_Persistent_Access;
   end record;

   type Timestamp_Entity_Array is array
     (Integer range <>) of Timestamped_Entity;

   type Timestamp_Entity_Array_Access is access all Timestamp_Entity_Array;

   procedure Free is new Standard.Ada.Unchecked_Deallocation
     (Timestamp_Entity_Array, Timestamp_Entity_Array_Access);

   package Entity_Lists_Pck is new Ada.Containers.Ordered_Sets
     (Entity_Persistent_Access);

   use Entity_Lists_Pck;

   type Ada_Type_Record is record
      Parents                : Timestamp_Entity_Array_Access;
      Children               : Entity_Lists_Pck.Set;

      Entity                 : Entity_Persistent_Access;

      Primitives             : Primitive_Array_Access;
      Dotted_Notation_Sb     : Entity_Persistent_Array_Access;

      Enclosing_Unit_Timestamp : Unit_Hierarchy_Timestamp;
      Analysis_Timestamp       : Integer := 0;
   end record;

   type Ada_Type_Access is access all Ada_Type_Record;

   procedure Free is new Standard.Ada.Unchecked_Deallocation
     (Ada_Type_Record, Ada_Type_Access);

   Null_Ada_Type_Access : constant Ada_Type_Access := null;

end Ada_Semantic_Tree.Type_Tree;
