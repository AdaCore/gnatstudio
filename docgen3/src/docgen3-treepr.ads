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

--  This package provides internal support for printing the contents of the
--  tree. These routines are intended only for debugging use.

with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Docgen3.Frontend;  use Docgen3.Frontend;

private package Docgen3.Treepr is

   procedure Print_Short_Tree
     (Context     : access constant Docgen_Context;
      Tree        : access Tree_Type;
      With_Scopes : Boolean);
   --  Print the contents of the tree (simplified output). For each node a
   --  single line is generated in the output.

   procedure Print_Full_Tree
     (Context     : access constant Docgen_Context;
      Tree        : access Tree_Type;
      With_Scopes : Boolean);
   --  Print the full contents of the tree.

end Docgen3.Treepr;
