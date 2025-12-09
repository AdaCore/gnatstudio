------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2017-2023, AdaCore                   --
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

package body LAL.Switching_Tree_Providers is

   -----------------------
   -- Get_Tree_For_File --
   -----------------------

   overriding function Get_Tree_For_File
     (Self    : in out Provider;
      Context : String;
      File    : GNATCOLL.VFS.Virtual_File)
      return Semantic_Tree'Class is
   begin
      return Self.Nested.Kernel.Default_Language_Tree_Provider.
        Get_Tree_For_File (Context, File);
   end Get_Tree_For_File;

end LAL.Switching_Tree_Providers;
