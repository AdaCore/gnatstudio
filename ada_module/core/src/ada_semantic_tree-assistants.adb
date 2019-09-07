------------------------------------------------------------------------------
--                               GNAT Studio                                --
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

with Ada_Semantic_Tree.Lang;
with Ada_Semantic_Tree.Parts;
with Ada_Semantic_Tree.Units;
with Ada_Semantic_Tree.Cache;
with Ada_Semantic_Tree.Dependency_Tree;
with Ada_Semantic_Tree.Visibility;
with Ada_Semantic_Tree.Type_Tree;
with Ada_Semantic_Tree.Interfaces;
with Ada_Semantic_Tree.Std_Entities;
with Ada_Semantic_Tree.Generics;

package body Ada_Semantic_Tree.Assistants is

   -----------------------------
   -- Register_Ada_Assistants --
   -----------------------------

   procedure Register_Ada_Assistants
     (Db                 : Construct_Database_Access;
      Std_Entities_Files : Virtual_File) is
   begin
      --  Since the assistants are registring annotations keys, the order in
      --  which they are registered influences a lot the memory lost by the
      --  database to create annotation - to keep things simple, the
      --  annotations created on most constructs should be registered first.
      --  However, there may be logical dependencies between the assistants
      --  which puts some constraints on this order - e.g. units has to be
      --  registered very soon.

      Ada_Semantic_Tree.Lang.Register_Assistant (Db);
      Ada_Semantic_Tree.Visibility.Register_Assistant (Db);
      Ada_Semantic_Tree.Units.Register_Assistant (Db);
      Ada_Semantic_Tree.Parts.Register_Assistant (Db);
      Ada_Semantic_Tree.Type_Tree.Register_Assistant (Db);
      Ada_Semantic_Tree.Dependency_Tree.Register_Assistant (Db);
      Ada_Semantic_Tree.Cache.Register_Assistant (Db);
      Ada_Semantic_Tree.Generics.Register_Assistant (Db);
      Ada_Semantic_Tree.Interfaces.Register_Assistant (Db);
      Ada_Semantic_Tree.Std_Entities.Register_Assistant
        (Db, Std_Entities_Files);
   end Register_Ada_Assistants;

end Ada_Semantic_Tree.Assistants;
