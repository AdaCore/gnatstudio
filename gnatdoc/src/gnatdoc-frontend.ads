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

--  This package implements the frontend of gnatdoc which builds the tree
--  associated with a single file.

with Ada.Containers.Vectors;
with GNATdoc.Atree;     use GNATdoc.Atree;

private package GNATdoc.Frontend is

   type Tree_Type is record
      Tree_Root    : Entity_Id;
      --  Root of the tree of entities

      All_Entities : aliased EInfo_List.Vector;
      --  All the entities of Tree_Root. Used by the backend when it needs to
      --  do some processing with all the nodes without traversing the tree
      --  structure.

      File : Virtual_File;
      --  File of this tree

      Header_File : Virtual_File;
      --  (C/C+): Header file (.h) associated with File
   end record;

   No_Tree : constant Tree_Type :=
               (Tree_Root    => null,
                All_Entities => EInfo_List.Empty_Vector,
                File         => No_File,
                Header_File  => No_File);

   function Build_Tree
     (Context : access constant Docgen_Context;
      File    : Virtual_File) return Tree_Type;
   --  Analyze all the entities defined in File and build the corresponding
   --  tree.

   procedure Initialize;
   --  Initialize (or clear) internal data structures

   procedure Finalize;

   function Find_Unique_Entity
     (Location      : General_Location;
      In_References : Boolean := False) return Entity_Id;
   --  Search for the entity defined at Location in a hash table containing
   --  all the entities of the project. If the entity is not found in the
   --  hash table but In_References is True then search for the reference
   --  in the list of references associated with each entity stored in the
   --  hash table. No_Entity is returned if not found.

   package Tree_List is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Tree_Type);

end GNATdoc.Frontend;
