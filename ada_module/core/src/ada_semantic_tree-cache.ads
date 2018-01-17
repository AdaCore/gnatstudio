------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

private package Ada_Semantic_Tree.Cache is

   procedure Register_Assistant (Db : Construct_Database_Access);
   --  To be called before any call to the subprograms of that package

   type Cached_Information is abstract tagged private;
   --  This is the type of information that can be cached in the system.

   procedure Free (This : in out Cached_Information) is null;
   --  Called when the information is about to be freed.

   type Cache_Access is access all Cached_Information'Class;

   Inconsistent_Cache : exception;

   procedure Set_Cache (Entity : Entity_Access; Info : Cache_Access);
   --  Sets an information in the cache. There can be only one information per
   --  entity, it's the responsibility of the caller to check that in the
   --  architecture, no other module is caching anything here.
   --
   --  Cache information is entirely erased at each structural file update,
   --  for all the entities of the file.
   --
   --  Setting the cache for an entity that already has a cache information
   --  replaces the previous one if the objects are of the same type, otherwise
   --  it raises an exception Inconsistent_Cache.
   --
   --  Usage of that cache should be documented here. At the moment, it's used
   --  as follows:
   --
   --  with / use clauses -> see ada_semantic_tree-visiblity
   --  generics instances -> see ada_semantic_tree-generics

   function Get_Cache (Entity : Entity_Access) return Cache_Access;
   --  Return the cache associated to this entity, null if none.

private

   type Cached_Information is tagged record
      null;
   end record;

end Ada_Semantic_Tree.Cache;
