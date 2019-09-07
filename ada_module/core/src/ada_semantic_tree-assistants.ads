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

--  This package provides the entry point registering all assistants needed
--  by the ada queries.

with GNATCOLL.VFS;       use GNATCOLL.VFS;

package Ada_Semantic_Tree.Assistants is

   procedure Register_Ada_Assistants
     (Db                 : Construct_Database_Access;
      Std_Entities_Files : Virtual_File);
   --  This procedure will register all the assistants needed to compute ada
   --  semantics. It has to be invoked before any file is added to the
   --  database, and as soon as possible in the assistant registration. No
   --  query should be made before the assistants have been registered.
   --  If Std_Entities_Files is not No_File, then it should be a path to the
   --  xml file containing pramas / attributes descriptions.

end Ada_Semantic_Tree.Assistants;
