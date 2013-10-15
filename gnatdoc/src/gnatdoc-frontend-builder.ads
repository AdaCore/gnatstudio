------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

with Language;                use Language;
with Language.Tree;           use Language.Tree;
with Language.Tree.Database;  use Language.Tree.Database;

--  This package isolates the frontend code which takes care of building the
--  trees (thus leaving the abstraction levels of the implementation more
--  clear).

private package GNATdoc.Frontend.Builder is

   function Build_File_Tree
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type) return Entity_Id;
   --  Build the tree associated with File. Subsidiary function of Build_Tree
   --  which takes care of building the tree and leaves more clear the high
   --  level actions performed by Build_Tree.

   function Find_Unique_Entity (Location : General_Location) return Entity_Id;
   --  Search for the entity defined at Location in a hash table containing
   --  all the entities of the project. No_Entity is returned if not found.

   procedure Initialize;

end GNATdoc.Frontend.Builder;
