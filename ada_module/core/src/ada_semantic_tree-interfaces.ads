------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

with GNAT.Strings;  use GNAT.Strings;

package Ada_Semantic_Tree.Interfaces is

   procedure Register_Assistant (Db : Construct_Database_Access);
   --  This assistant has to be registered to the database before any of the
   --  queries in this file can work.

   function Get_Assistant
     (Db : Construct_Database_Access) return Database_Assistant_Access;
   --  Return assistant holding the knowledge about the interfaces pragmas

   function Get_Exported_Entity
     (Assistant : Database_Assistant_Access; Name : String)
      return Entity_Access;
   --  Return the entity exported to this name, Null_Entity_Access if none.

   type Imported_Entity is private;
   --  Type representing this entity imported data. This should not be stored
   --  for a long time, as its contents will be invalidated at each construct
   --  file update.

   Null_Imported_Entity : aliased constant Imported_Entity;

   function Get_Imported_Entity
     (Entity : Entity_Access) return Imported_Entity;
   --  Return the imported entity by this entity, Null_Imported_Entity if none.

   function Get_Name (Entity : Imported_Entity) return String;
   --  Return the imported name of the entity.

   function Get_Convention (Entity : Imported_Entity) return String;
   --  Return the convention from which the entity is imported

private

   type Imported_Entity is record
      Name       : String_Access;
      Convention : String_Access;
   end record;

   Null_Imported_Entity : aliased constant Imported_Entity := (null, null);

end Ada_Semantic_Tree.Interfaces;
