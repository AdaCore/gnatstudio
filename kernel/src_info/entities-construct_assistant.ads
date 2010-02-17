-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

--  This package provides an easy way to do the connection between LI entities
--  and construct entities.

with Language;               use Language;
with Language.Tree;          use Language.Tree;
with Language.Tree.Database; use Language.Tree.Database;

package Entities.Construct_Assistant is

   procedure Register_Assistant
     (Database : Construct_Database_Access;
      LI_Db    : Entities_Database);
   --  This assistant must be registered to the database before using the
   --  services of the entities.

   function To_LI_Entity (E : Entity_Access) return Entity_Information;
   --  Return an LI entity based on a construct entity. Create one if none.

end Entities.Construct_Assistant;
