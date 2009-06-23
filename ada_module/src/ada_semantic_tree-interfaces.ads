-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2009, AdaCore                      --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Language;               use Language;
with Language.Tree;          use Language.Tree;
with Language.Tree.Database; use Language.Tree.Database;

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

   function Get_Imported_Entity (Entity : Entity_Access) return String;
   --  Return the name imported by this entity, if any.

end Ada_Semantic_Tree.Interfaces;
