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

--  This package computes various visibility information on trees.

with Language.Tree;          use Language.Tree;
with Language.Tree.Database; use Language.Tree.Database;

package Ada_Semantic_Tree.Visibility is

   procedure Register_Assistant (Db : Construct_Database_Access);
   --  This assistant has to be registered to the database before any of the
   --  queries in this file can work.

   function Get_Assistant
     (Db : Construct_Database_Access) return Database_Assistant_Access;
   --  Return the assistant responsible of doing visibility analysis.

   function Is_Public_Library_Visible (Entity : Entity_Access) return Boolean;
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
   --  based on name comparaisons. See if it would be more efficient to base
   --  it on computed parts informations
   function Get_Location_Relation
     (Tree_To     : Construct_Tree;
      Object_To   : Construct_Tree_Iterator;
      Tree_From   : Construct_Tree;
      Offset_From : Integer) return Location_Relation;
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
      From_Offset : Integer) return Boolean;
   --  Return true if the entity is accessible, that is to say either
   --  it's a public library visible entity, or it's in one of the visible
   --  areas of the unit hierarchy from the location given in parameter.
   --  Note that getting access to that entity may require additional use or
   --  with clases, or prefix.

end Ada_Semantic_Tree.Visibility;
