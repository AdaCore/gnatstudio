-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2007, AdaCore                    --
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

--  This package provides a way to analyze type trees, and in particular
--  tagged type trees.

with Ada.Unchecked_Deallocation;

with Language.Tree.Database;    use Language.Tree.Database;
with Ada_Semantic_Tree.Units;        use Ada_Semantic_Tree.Units;
with Ada_Semantic_Tree.Declarations; use Ada_Semantic_Tree.Declarations;

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
   --  but not overriden primitives.

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

   function Get_Entity_Or_Overriden
     (Primitive : Ada_Primitive_Access)
      return Entity_Access;
   --  Return either the entity pointed by this primitive if it's explicitely
   --  declared, or the closed overriden subprogram. If serveal matches (e.g.
   --  in the case of multiple interfaces inheritence), one is picked up
   --  randomly.

private

   type Primitive_Subprogram is record
      Entity                : Entity_Persistent_Access;
      --  If the primitive is declared or overriden for this type, this holds
      --  the corresponding entity. Otherwise, it's null.

      Overriden_Entities : Primitive_Array_Access;

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

   type Ada_Type_Record is record
      Parents                : Timestamp_Entity_Array_Access;

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
