-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2004                       --
--                            ACT-Europe                             --
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

with GPS.Kernel;
with Entities;

package Refactoring.Performers is

   type Refactor_Performer_Record is abstract tagged null record;
   type Refactor_Performer is access all Refactor_Performer_Record'Class;

   procedure Execute
     (Factory       : access Refactor_Performer_Record;
      Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Entity        : Entities.Entity_Information;
      Refs          : Location_Arrays.Instance;
      No_LI_List    : File_Arrays.Instance;
      Stale_LI_List : File_Arrays.Instance) is abstract;
   --  Called after we have found all the references to an entity, to perform
   --  some actual refactoring. Refs is the list of all known references to the
   --  entity. No_LI_List is the list of source files for which no LI file
   --  exist. Stale_LI_List is the list of LI files which are no longer
   --  up-to-date with respect to the sources they represent.
   --  When Execute is called, he has already confirmed that he wants to
   --  perform the refactoring even though some of the files are not up-to-date
   --
   --  Do not free the parameters

   procedure Free (Factor : in out Refactor_Performer_Record);
   --  Free the memory occupied by Factor.

   procedure Get_All_Locations
     (Kernel                : access GPS.Kernel.Kernel_Handle_Record'Class;
      Entity                : Entities.Entity_Information;
      On_Completion         : access Refactor_Performer_Record'Class;
      Auto_Compile          : Boolean := False;
      Background_Mode       : Boolean := True);
   --  Get all the locations in which Entity is referenced.
   --  In Errors, this procedure returns the list of files that are not
   --  up-to-date in the LI structure. References inside these files are still
   --  included though.
   --  On_Completion is automatically freed when the refactoring is finished.

end Refactoring.Performers;
