------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Basic_Types;               use Basic_Types;
with GNATCOLL.VFS;
with GPS.Kernel;
with Refactoring.UI;            use Refactoring.UI;
with Xref;                      use Xref;

package Refactoring.Performers is

   type Refactor_Performer_Record is abstract tagged null record;
   type Refactor_Performer is access all Refactor_Performer_Record'Class;

   procedure Execute
     (Factory       : access Refactor_Performer_Record;
      Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Entity        : Root_Entity'Class;
      Refs          : Location_Arrays.List;
      No_LI_List    : Source_File_Set;
      Stale_LI_List : Source_File_Set) is abstract;
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
      Entity                : Root_Entity'Class;
      On_Completion         : access Refactor_Performer_Record'Class;
      Auto_Compile          : Boolean := False;
      Overridden            : Boolean := True;
      Make_Writable         : Boolean := False;
      Background_Mode       : Boolean := True);
   --  Get all the locations in which Entity is referenced.
   --  In Errors, this procedure returns the list of files that are not
   --  up-to-date in the LI structure. References inside these files are still
   --  included though.
   --  On_Completion is automatically freed when the refactoring is finished.
   --  It is called once the user has confirmed potential warnings, and once
   --  relevant files have been made writable.
   --  If Overridden is true, then the location of entities that override
   --  Entity (for instance overridding subprograms, or, when Entity is a
   --  subprogram parameter, parameters of overridding subprograms) are also
   --  taken into account.

   ----------------------
   -- Editor interface --
   ----------------------

   procedure Delete_Text
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      In_File    : GNATCOLL.VFS.Virtual_File;
      Line_Start : Integer;
      Line_End   : Integer);
   --  Delete a range of text

   function Get_Text
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      From_File  : GNATCOLL.VFS.Virtual_File;
      Line       : Integer;
      Column     : Visible_Column_Type;
      Length     : Integer) return String;
   --  Get the contents of From_File

end Refactoring.Performers;
