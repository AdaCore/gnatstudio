-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2010, AdaCore              --
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

with Basic_Types;               use Basic_Types;
with GNATCOLL.VFS;
with GPS.Kernel;
with Refactoring.UI;            use Refactoring.UI;

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

   function Insert_Text
     (Kernel                    : access GPS.Kernel.Kernel_Handle_Record'Class;
      In_File                   : GNATCOLL.VFS.Virtual_File;
      Line                      : Integer;
      Column                    : Visible_Column_Type := 1;
      Text                      : String;
      Indent                    : Boolean;
      Skip_Comments_Backward    : Boolean := False;
      Surround_With_Blank_Lines : Boolean := False;
      Replaced_Length           : Integer := 0;
      Only_If_Replacing         : String := "") return Boolean;
   --  Insert some text in a source file.
   --  If Indent is True, the text is indented automatically.
   --  Replaced_Length is the number of characters that should first be removed
   --  to be replaced by Text.
   --  If Only_If_Replacing is specified, then the replacement of text will be
   --  done only if the text being replaced is Only_If_Replacing (case
   --  insensitive). If it isn't, False is returned.
   --  If Skip_Comments_Backward is True, then the actual insertion will occur
   --  on the first line before any comment lines preceding Line.
   --  If Surround_With_Blank_Lines is True, then the inserted text must end up
   --  with a blank line before and after it (so lines are inserted as needed).
   --  This function returns True if the new text could be inserted.

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

   procedure Start_Undo_Group
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File);
   procedure Finish_Undo_Group
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Interface to start/finish undo group

end Refactoring.Performers;
