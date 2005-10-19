-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2005                       --
--                              AdaCore                              --
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
with Entities;
with Projects;

package GPS.Kernel.Contexts is

   procedure Register_Default_Filters
     (Kernel : access Kernel_Handle_Record'Class);
   --  Register a set of default filters that can be used in user-defined
   --  actions and for contextual menus

   ------------------------
   -- File_Name contexts --
   ------------------------

   type File_Selection_Context is new Selection_Context with private;
   type File_Selection_Context_Access is access all
     File_Selection_Context'Class;

   procedure Set_File_Information
     (Context           : access File_Selection_Context;
      File              : VFS.Virtual_File := VFS.No_File;
      Project           : Projects.Project_Type := Projects.No_Project;
      Importing_Project : Projects.Project_Type := Projects.No_Project;
      Line              : Integer := 0;
      Column            : Integer := 0);
   --  Set the information in this context.
   --  File_Name must be UTF8-encoded.

   function Has_Directory_Information
     (Context : access File_Selection_Context) return Boolean;
   --  True if the context has information about a selected directory.

   function Directory_Information
     (Context : access File_Selection_Context) return String;
   --  Return the information about the selected project. This is only relevant
   --  if Has_Directory_Information is True.
   --  This directory name always ends with a directory separator.

   function Has_File_Information
     (Context : access File_Selection_Context) return Boolean;
   --  True if the context has information about a selected file

   function File_Information
     (Context  : access File_Selection_Context) return VFS.Virtual_File;
   --  Return the information about the selected file. This is only relevant
   --  if Has_File_Information is True.
   --  This is the base file name for the file. This name is UTF8-encoded.

   function Has_Line_Information
     (Context : access File_Selection_Context) return Boolean;
   function Line_Information
     (Context : access File_Selection_Context) return Integer;
   --  Check whether there is some line information, and return it. This is the
   --  location of the cursor in the file, when in an editor, or the location
   --  in the file from the messages window or the explorer for instance.
   --  This information will not be set if multiple lines are selected.

   function Has_Column_Information
     (Context : access File_Selection_Context) return Boolean;
   function Column_Information
     (Context : access File_Selection_Context) return Integer;
   --  Check whether there is some column information, and return it. Same
   --  comment as for Line_Information.
   --  Column is the index of the character in the string representing the
   --  line. This means that tabs only count as one, and are not expanded.

   function Has_Project_Information
     (Context : access File_Selection_Context) return Boolean;
   --  True if the creator of the context provided information about the
   --  project.

   function Project_Information
     (Context : access File_Selection_Context) return Projects.Project_Type;
   --  Return the id of the project to which the file belongs. Note that this
   --  is computed automatically and cached otherwise.
   --  This function will return No_Project if the file stored in the context
   --  doesn't belong to any project.

   function Has_Importing_Project_Information
     (Context : access File_Selection_Context) return Boolean;
   --  True if the context contains the name of the project importing the
   --  current one.

   function Importing_Project_Information
     (Context : access File_Selection_Context) return Projects.Project_Type;
   --  Return the project that imports the one returned by Project_Information.
   --  This is never computed automatically, and unless provided by the creator
   --  of the project, this will be left empty.

   procedure Destroy (Context : in out File_Selection_Context);
   --  Free the memory associated with the context

   ------------------------
   -- File_Area contexts --
   ------------------------
   --  This context is used when one or more lines are selected in an editor,
   --  but more than a single entity.

   type File_Area_Context is new File_Selection_Context with private;
   type File_Area_Context_Access is access all File_Area_Context'Class;

   procedure Set_Area_Information
     (Context    : access File_Area_Context;
      Text       : String;
      Start_Line : Integer := 0;
      End_Line   : Integer := 0);
   --  Set the area information in Context

   procedure Get_Area
     (Context    : access File_Area_Context;
      Start_Line : out Integer;
      End_Line   : out Integer);
   --  Return the area information in Context

   function Text_Information
     (Context : access File_Area_Context) return String;
   --  Return the text belonging to the selection

   type Is_Area_Context is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Is_Area_Context;
      Context : access Selection_Context'Class) return Boolean;
   --  Filter that checks that the user has clicked on a subprogram entity.
   --  This can be used for contextual menus for instance

   ---------------------
   -- Message_Context --
   ---------------------
   --  This context is emitted when the user clicks in the location. It is
   --  mostly used for error messages. The line and column information are
   --  the references in the error message.
   --  Line and columns are stored as Line_Information and Column_Information

   type Message_Context is new File_Selection_Context with private;
   type Message_Context_Access is access all Message_Context;

   procedure Set_Message_Information
     (Context  : access Message_Context;
      Category : String := "";
      Message  : String := "");
   --  Set the information in the context

   function Has_Category_Information
     (Context : access Message_Context) return Boolean;
   function Category_Information
     (Context : access Message_Context) return String;
   --  Check whether there is some category information, and return it

   function Has_Message_Information
     (Context : access Message_Context) return Boolean;
   function Message_Information
     (Context : access Message_Context) return String;
   --  Check whether there is some message information, and return it

   ---------------------
   -- Entity Contexts --
   ---------------------

   type Entity_Selection_Context is new File_Selection_Context
     with private;
   type Entity_Selection_Context_Access is access all Entity_Selection_Context;

   procedure Set_Entity_Information
     (Context       : access Entity_Selection_Context;
      Entity_Name   : String := "";
      Entity_Column : Integer := 0);
   --  Set the information in the context.
   --  Entity_Column should be the column on which the entity starts, not the
   --  current location of the cursor.
   --  The line at which the entity starts is the line set in
   --  Set_File_Information

   function Has_Entity_Name_Information
     (Context : access Entity_Selection_Context) return Boolean;
   function Entity_Name_Information
     (Context : access Entity_Selection_Context) return String;
   --  Check whether there is some entity information, and return it. This is
   --  a UTF8-encoded string.

   function Has_Entity_Column_Information
     (Context : access Entity_Selection_Context) return Boolean;
   function Entity_Column_Information
     (Context : access Entity_Selection_Context) return Integer;
   --  Check whether there is some column information, and return it.
   --  The column returned is the column on which the entity starts, not the
   --  column on which the cursor currently is.

   function Get_Entity
     (Context : access Entity_Selection_Context)
      return Entities.Entity_Information;
   --  Return the location of the declaration for the entity in Context.
   --  This information is automatically cached in the context, in case several
   --  modules need to compute it;
   --  No_Entity_Information is returned if the information could not be found.
   --  Note also that in most cases you should set the busy cursor before
   --  calling this function, since it might take some time.
   --  You do not need to free the memory, since it will automatically be freed
   --  when the context is destroyed.

   procedure Destroy (Context : in out Entity_Selection_Context);
   --  Destroy the memory associated with the entity

   ----------------------
   -- Activity_Context --
   ----------------------

   type Activity_Context is new File_Selection_Context with private;
   type Activity_Context_Access is access all Activity_Context;

   procedure Set_Activity_Information
     (Context : access Activity_Context;
      Id      : String);

   function Activity_Information
     (Context : access Activity_Context) return String;
   --  Returns the name of the activity

   procedure Destroy (Context : in out Activity_Context);
   --  Free the memory associated with the context

private

   procedure Destroy (Context : in out File_Area_Context);

   type File_Selection_Context is new Selection_Context with record
      File              : VFS.Virtual_File      := VFS.No_File;
      Checked           : Boolean               := False;
      --  Set to True if File has already been validated
      Project           : Projects.Project_Type := Projects.No_Project;
      Importing_Project : Projects.Project_Type := Projects.No_Project;
      Line, Column      : Integer := 0;

      Creator_Provided_Project : Boolean := False;
      --  Set to True if the project_view was given by the creator, instead of
      --  being computed automatically
   end record;

   type Message_Context is new File_Selection_Context with record
      Category_Name : GNAT.OS_Lib.String_Access := null;
      Message       : GNAT.OS_Lib.String_Access := null;
   end record;

   type File_Area_Context is new File_Selection_Context with record
      Start_Line : Integer;
      End_Line   : Integer;
      Text       : GNAT.OS_Lib.String_Access;
   end record;

   type Entity_Selection_Context is new File_Selection_Context with record
      Entity_Name   : GNAT.OS_Lib.String_Access := null;
      Entity_Column : Integer := 0;
      Entity        : Entities.Entity_Information := null;

      Entity_Resolved : Boolean := False;
      --  Set to True when we have called Get_Entity at least once. This is
      --  used to differentiate cases where Entity is null because we have
      --  never tested and cases where it is null because there is none to be
      --  found.
   end record;

   type Activity_Context is new File_Selection_Context with record
      Id : GNAT.OS_Lib.String_Access := null;
   end record;

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
