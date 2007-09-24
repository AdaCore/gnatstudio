-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2007, AdaCore              --
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

--  This package provides support for the description of the various
--  contexts and selections in GPS.

with VFS;
with Basic_Types;
with Entities;
with Projects;

package GPS.Kernel.Contexts is

   procedure Register_Default_Filters
     (Kernel : access Kernel_Handle_Record'Class);
   --  Register a set of default filters that can be used in user-defined
   --  actions and for contextual menus

   -----------
   -- Files --
   -----------

   procedure Set_File_Information
     (Context           : in out Selection_Context;
      File              : VFS.Virtual_File := VFS.No_File;
      Project           : Projects.Project_Type := Projects.No_Project;
      Importing_Project : Projects.Project_Type := Projects.No_Project;
      Line              : Integer := 0;
      Column            : Basic_Types.Visible_Column_Type := 0;
      Revision          : String := "";
      Tag               : String := "");
   --  Set the information in this context.
   --  ??? We should use non-ambiguous types for Line and Column

   function Has_Directory_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has directory information
   function Directory_Information
     (Context : Selection_Context) return String;
   --  Return the information about the selected project. This is only relevant
   --  if Has_Directory_Information is True.
   --  This directory name always ends with a directory separator.

   function Has_File_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has file information
   function File_Information
     (Context : Selection_Context) return VFS.Virtual_File;
   --  Return the information about the selected file. This is only relevant
   --  if Has_File_Information is True.
   --  This is the base file name for the file. This name is UTF8-encoded.

   function Has_Line_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has line information
   function Line_Information
     (Context : Selection_Context) return Integer;
   --  Return the location of the cursor in the file, when in an editor, or
   --  the location in the file from the messages window or the explorer for
   --  instance.
   --  This information will not be set if multiple lines are selected.

   function Has_Column_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has column information
   function Column_Information
     (Context : Selection_Context) return Basic_Types.Visible_Column_Type;
   --  Return the column information. Same comment as for Line_Information.
   --  Column is the index of the character in the string representing the
   --  line. This means that tabs only count as one, and are not expanded.

   function Has_Project_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has project information
   function Project_Information
     (Context : Selection_Context) return Projects.Project_Type;
   --  Return the id of the project to which the file belongs. Note that this
   --  is computed automatically and cached otherwise.
   --  This function will return No_Project if the file stored in the context
   --  doesn't belong to any project.

   function Has_Importing_Project_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has information about the project that imports
   --  the one returned by Project_Information.
   function Importing_Project_Information
     (Context : Selection_Context) return Projects.Project_Type;
   --  Return the project that imports the one returned by Project_Information.
   --  This is never computed automatically, and unless provided by the creator
   --  of the project, this will be left empty.

   function Has_Revision_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has revision information
   function Revision_Information
     (Context : Selection_Context) return String;
   --  Return the revision information associated with the file. The revision
   --  is the number or tag used by the VCS to specify a uniq version of file.

   function Has_Tag_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has information about the tag/branch name
   --  associated with the file returned by File_Information
   function Tag_Information
     (Context : Selection_Context) return String;
   --  Return the tag/branch name associated with the file returned by
   --  File_Information

   -----------
   -- Areas --
   -----------

   procedure Set_Area_Information
     (Context    : in out Selection_Context;
      Text       : String;
      Start_Line : Integer := 0;
      End_Line   : Integer := 0);
   --  Set the area information in Context

   function Has_Area_Information (Context : Selection_Context) return Boolean;
   --  Whether the context contains information about a selected area.

   procedure Get_Area
     (Context    : Selection_Context;
      Start_Line : out Integer;
      End_Line   : out Integer);
   --  Return the area information in Context

   function Text_Information
     (Context : Selection_Context) return String;
   --  Return the text belonging to the selection

   type Is_Area_Context is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Is_Area_Context;
      Context : Selection_Context) return Boolean;
   --  Filter that checks that the user has clicked on a subprogram entity.
   --  This can be used for contextual menus for instance

   ---------------------
   -- Message_Context --
   ---------------------
   --  This context is emitted when the user clicks in the location. It is
   --  mostly used for error messages. The line and column information are
   --  the references in the error message.
   --  Line and columns are stored as Line_Information and Column_Information

   procedure Set_Message_Information
     (Context  : in out Selection_Context;
      Category : String := "";
      Message  : String := "");
   --  Set the information in the context

   function Has_Category_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has category information
   function Category_Information
     (Context : Selection_Context) return String;
   --  Return the category information associated with Context

   function Has_Message_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has message information
   function Message_Information
     (Context : Selection_Context) return String;
   --  Return the message information associated with Context.

   ---------------------
   -- Entity Contexts --
   ---------------------

   procedure Set_Entity_Information
     (Context       : in out Selection_Context;
      Entity_Name   : String := "";
      Entity_Column : Basic_Types.Visible_Column_Type := 0);
   --  Set the information in the context.
   --  Entity_Column should be the column on which the entity starts, not the
   --  current location of the cursor.
   --  The line at which the entity starts is the line set in
   --  Set_File_Information

   procedure Set_Entity_Information
     (Context : in out Selection_Context;
      Entity  : access Entities.Entity_Information_Record'Class);
   --  Same as above, but we provide directly the entity itself. This is more
   --  efficient when you already know the entity.
   --  This doesn't change the File_Information stored in the context, so that
   --  you can decide whether contextual menus should show file-related
   --  entries.

   function Has_Entity_Name_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has entity name information
   function Entity_Name_Information
     (Context : Selection_Context) return String;
   --  Return the entity name information. This is a UTF8-encoded string.

   function Has_Entity_Column_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has entity column information
   function Entity_Column_Information
     (Context : Selection_Context) return Basic_Types.Visible_Column_Type;
   --  Return entity column information associated with Context.
   --  The column returned is the column on which the entity starts, not the
   --  column on which the cursor currently is.

   function Get_Entity
     (Context           : Selection_Context;
      Ask_If_Overloaded : Boolean := False)
      return Entities.Entity_Information;
   --  Return the location of the declaration for the entity in Context.
   --  This information is automatically cached in the context, in case several
   --  modules need to compute it; However, if you first do a call with
   --  Ask_If_Overloaded set to False, then one set to True, the latter will
   --  override the former if we have more precise information.
   --  No_Entity_Information is returned if the information could not be found.
   --  Note also that in most cases you should set the busy cursor before
   --  calling this function, since it might take some time.
   --  You do not need to free the memory, since it will automatically be freed
   --  when the context is destroyed.
   --  If Ask_If_Overloaded is true and there are several possible matches for
   --  the entity, an interactive dialog is opened for the user. Otherwise, the
   --  closest matching entity is returned

   function Get_Closest_Ref
     (Context           : Selection_Context)
      return Entities.Entity_Reference;
   --  Return the entity reference corresponding to the current context. You
   --  should call Get_Entity first if you want to check for overloading
   --  entities.
   --  This information is cached in the context in case multiple places need
   --  to recompute it

   ----------------------
   -- Activity_Context --
   ----------------------

   procedure Set_Activity_Information
     (Context : in out Selection_Context; Id : String);
   --  Fill Context with an activity name

   function Has_Activity_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has activity information
   function Activity_Information
     (Context : Selection_Context) return String;
   --  Returns the name of the activity

private

   pragma Inline (Has_Project_Information);
   pragma Inline (Has_Directory_Information);
   pragma Inline (Has_Importing_Project_Information);
   pragma Inline (Importing_Project_Information);
   pragma Inline (Project_Information);
   pragma Inline (Has_File_Information);
   pragma Inline (Has_Entity_Name_Information);
   pragma Inline (Entity_Name_Information);
   pragma Inline (Has_Line_Information);
   pragma Inline (Line_Information);
   pragma Inline (Has_Column_Information);
   pragma Inline (Column_Information);
   pragma Inline (Has_Entity_Column_Information);
   pragma Inline (Entity_Column_Information);

end GPS.Kernel.Contexts;
