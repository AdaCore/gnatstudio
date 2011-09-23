-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                  Copyright (C) 2011, AdaCore                      --
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

with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;

package Entities_Db is

   procedure Parse_All_LI_Files
     (Session : Session_Type;
      Tree    : Project_Tree;
      Project : Project_Type;
      Env     : Project_Environment_Access := null;
      Database_Is_Empty : Boolean := False);
   --  Parse all the LI files for the project, and stores them in the
   --  database.
   --  If the caller knows that the database is empty, it should pass True for
   --  Database_Is_Empty. In this case, this package will avoid a number of
   --  calls to SELECT and significantly speed up the initial insertion.
   --  If Env is specified, the ALI files from the predefined object path will
   --  also be parsed.

end Entities_Db;
