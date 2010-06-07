-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2010, AdaCore                  --
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

with Entities;
with Language.Tree.Database;
with GPS.Editors;

package Refactoring is

   ---------------------
   -- Factory context --
   ---------------------

   type Factory_Context is record
      Buffer_Factory : GPS.Editors.Editor_Buffer_Factory_Access;
      Entity_Db      : Entities.Entities_Database;
      Construct_Db   : Language.Tree.Database.Construct_Database_Access;

      Add_Subprogram_Box : Boolean := True;
      --  Whether creating a subprogram body should first insert a subprogram
      --  box

      Add_In_Keyword     : Boolean := False;
      --  Whether adding "in" parameters should explicitly show the "in"
      --  keyword. If False, the keyword is omitted

      Create_Subprogram_Decl : Boolean := True;
      --  Whether to add a subprogram declaration when creating a new
      --  subprogram in a body.
   end record;
   --  This type groups the common data required by all the
   --  constructors/factories of the various refactoring algorithms.
   --  Among others, its role is to keep all necessary data from the kernel so
   --  that GNATBench does not need to depend on the kernel.
   --  This is a singleton, only one instance is needed in the application.
   --  It has been made a public report for ease of use. In practice, this is a
   --  read-only structure except at creation time.

end Refactoring;
