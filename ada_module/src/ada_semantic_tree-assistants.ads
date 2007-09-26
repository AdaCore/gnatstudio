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

--  This package provides the entry point registering all assistants needed
--  by the ada queries.

with Language.Tree.Database; use Language.Tree.Database;

package Ada_Semantic_Tree.Assistants is

   procedure Register_Ada_Assistants (Db : Construct_Database_Access);
   --  This procedure will register all the assistants needed to compute ada
   --  semantics. It has to be invoked before any file is added to the
   --  database, and as soon as possible in the assistant registration. No
   --  query should be made before the assistants have been registered.

end Ada_Semantic_Tree.Assistants;
