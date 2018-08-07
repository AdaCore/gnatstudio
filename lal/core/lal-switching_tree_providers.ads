------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2017-2018, AdaCore                   --
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
--  This package defines configurable Semantic_Tree_Provider for Ada language
--  to switch between legacy construct implemented and libAdaLang
--  implementation depending on call context.

with GNATCOLL.VFS;

with LAL.Semantic_Trees;
with Language.Abstract_Language_Tree;   use Language.Abstract_Language_Tree;

package LAL.Switching_Tree_Providers is

   type Provider is new Semantic_Tree_Provider with record
      Config   : Use_LAL_Configuration;
      Nested   : LAL.Semantic_Trees.Provider;
   end record;

   overriding function Get_Tree_For_File
     (Self    : Provider;
      Context : String;
      File    : GNATCOLL.VFS.Virtual_File) return Semantic_Tree'Class;

end LAL.Switching_Tree_Providers;
