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

with Basic_Types;
with Entities;
with Language.Tree.Database;
with GNATCOLL.VFS;
with GPS.Editors;

package Refactoring is

   ---------------------
   -- Factory context --
   ---------------------

   type Factory_Context_Record is tagged record
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
   type Factory_Context is access all Factory_Context_Record'Class;
   --  This type groups the common data required by all the
   --  constructors/factories of the various refactoring algorithms.
   --  Among others, its role is to keep all necessary data from the kernel so
   --  that GNATBench does not need to depend on the kernel.
   --  This is a singleton, only one instance is needed in the application.
   --  It has been made a public report for ease of use. In practice, this is a
   --  read-only structure except at creation time.

   procedure Report_Error
     (Self : access Factory_Context_Record;
      Msg  : String) is null;
   --  Report an error message to the user.
   --  The message has *not* been translated, and is in English.

   procedure Report_Location
     (Self     : access Factory_Context_Record;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Natural;
      Column   : Basic_Types.Visible_Column_Type := 1;
      Text     : String) is null;
   --  Report a location where a change occurred.
   --  This can then tbe used by the user to quickly navigate across such
   --  locations to review the effects of a refactoring.

   type With_Factory is abstract tagged record
      Context : Factory_Context;
   end record;
   --  Base type for all types that store a factory context. This field is made
   --  public for ease of use and efficiency, rather than go through a
   --  primitive operation.
end Refactoring;
