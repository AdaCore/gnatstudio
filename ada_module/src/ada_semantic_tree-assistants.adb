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

with Language.Tree.Ada;
with Ada_Semantic_Tree.Parts;
with Ada_Semantic_Tree.Units;
with Ada_Semantic_Tree.Dependency_Tree;
with Ada_Semantic_Tree.Visibility;
with Ada_Semantic_Tree.Type_Tree;

package body Ada_Semantic_Tree.Assistants is

   -----------------------------
   -- Register_Ada_Assistants --
   -----------------------------

   procedure Register_Ada_Assistants (Db : Construct_Database_Access) is
   begin
      --  Since the assistants are registring annotations keys, the order in
      --  which they are registered influences a lot the memory lost by the
      --  database to create annotation - to keep things simple, the
      --  annotations created on most constructs should be registered first.
      --  However, there may be logical dependencies between the assistants
      --  which puts some constraints on this order - e.g. units has to be
      --  registered very soon.

      Language.Tree.Ada.Register_Assistant (Db);
      Ada_Semantic_Tree.Visibility.Register_Assistant (Db);
      Ada_Semantic_Tree.Units.Register_Assistant (Db);
      Ada_Semantic_Tree.Parts.Register_Assistant (Db);
      Ada_Semantic_Tree.Type_Tree.Register_Assistant (Db);
      Ada_Semantic_Tree.Dependency_Tree.Register_Assistant (Db);
   end Register_Ada_Assistants;

end Ada_Semantic_Tree.Assistants;
