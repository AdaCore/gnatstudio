------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

--  This package provides support for the description of the various
--  contexts and selections in GPS.

with GNATCOLL.Projects;
with GNATCOLL.VFS;
with Basic_Types;
with String_List_Utils;
with GPS.Kernel.Messages;
with Gtkada.Canvas_View;      use Gtkada.Canvas_View;

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
      Files             : GNATCOLL.VFS.File_Array :=
        GNATCOLL.VFS.Empty_File_Array;
      Project           : GNATCOLL.Projects.Project_Type :=
        GNATCOLL.Projects.No_Project;
      Importing_Project : GNATCOLL.Projects.Project_Type :=
        GNATCOLL.Projects.No_Project;
      Publish_Project   : Boolean := True;
      Line              : Integer := 0;
      Column            : Basic_Types.Visible_Column_Type := 0;
      Revision          : String  := "";
      Other_Revision    : String  := "";
      Tag               : String  := "";
      File_Line         : Natural := 0);
   --  Set the information in this context.
   --  ??? We should use non-ambiguous types for Line and Column
   --
   --  The context information should be provided when available, so that we
   --  can resolve the ambiguities when using aggregate projects. However, it
   --  is possible to set Publish_Project to False if Has_Project_Information
   --  should return True, which will have the result of removing some
   --  contextual menu entries that need an explicit project.

   function Has_Directory_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has directory information
   function Directory_Information
     (Context : Selection_Context) return GNATCOLL.VFS.Virtual_File;
   --  Return the information about the selected project. This is only relevant
   --  if Has_Directory_Information is True.
   --  This directory name always ends with a directory separator.

   function Has_File_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has file information
   function File_Information
     (Context : Selection_Context) return GNATCOLL.VFS.Virtual_File;
   function File_Information
     (Context : Selection_Context) return GNATCOLL.VFS.File_Array;
   --  Return the information about the selected file. This is only relevant
   --  if Has_File_Information is True.
   --  This is the base file name for the file. This name is UTF8-encoded.

   function Has_Line_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has line information
   function Line_Information
     (Context : Selection_Context) return Integer;
   --  Return the location of the cursor in the file/buffer, when in an editor,
   --  or the location in the file from the messages window or the explorer for
   --  instance.
   --  This information will not be set if multiple lines are selected.

   function Has_File_Line_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has file's line information

   function File_Line_Information
     (Context : Selection_Context) return Natural;
   --  Same as above but return the number of line in the file

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
     (Context : Selection_Context) return GNATCOLL.Projects.Project_Type;
   --  Return the id of the project to which the file belongs. Note that this
   --  is computed automatically and cached otherwise.
   --  This function will return No_Project if the file stored in the context
   --  doesn't belong to any project.
   --  Has_Project_Information will return False if the context creator
   --  decided not to publish project information. It is still possible that
   --  Project_Information return an actual project in this case.
   --  When no project information was provided via Set_File_Information, this
   --  function will try to guess which project should be used.

   function Has_Importing_Project_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has information about the project that imports
   --  the one returned by Project_Information.
   function Importing_Project_Information
     (Context : Selection_Context) return GNATCOLL.Projects.Project_Type;
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

   function Has_Other_Revision_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has an other revision information
   function Other_Revision_Information
     (Context : Selection_Context) return String;
   --  Return the other revision information associated with the file

   function Has_Tag_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has information about the tag/branch name
   --  associated with the file returned by File_Information
   function Tag_Information
     (Context : Selection_Context) return String;
   --  Return the tag/branch name associated with the file returned by
   --  File_Information

   function Get_File_Language (Context : Selection_Context) return String;
   --  Return first file language.

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
   --  Whether the context contains information about a selected area

   procedure Get_Area
     (Context    : Selection_Context;
      Start_Line : out Integer;
      End_Line   : out Integer);
   --  Return the area information in Context

   function Text_Information
     (Context : Selection_Context) return String;
   --  Return the text belonging to the selection

   type Is_Area_Context is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Area_Context;
      Context : Selection_Context) return Boolean;
   --  Filter that checks that the user has clicked on a subprogram entity.
   --  This can be used for contextual menus for instance.

   ---------------------
   -- Message_Context --
   ---------------------
   --  This context is emitted when the user clicks in the location. It is
   --  mostly used for error messages. The line and column information are
   --  the references in the error message.
   --  Line and columns are stored as Line_Information and Column_Information

   procedure Set_Messages_Information
     (Context  : in out Selection_Context;
      Messages : GPS.Kernel.Messages.Message_Array);
   --  Set the information in the context

   function Has_Message_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has message information

   function Messages_Information
     (Context : Selection_Context) return GPS.Kernel.Messages.Message_Array;
   --  Return the message information associated with Context

   ---------------------
   -- Entity Contexts --
   ---------------------

   procedure Set_Entity_Information
     (Context         : in out Selection_Context;
      Entity_Name     : String;
      Entity_Column   : Basic_Types.Visible_Column_Type := 0;
      From_Expression : String := "");
   --  Set the information in the context.
   --  Entity_Column should be the column on which the entity starts, not the
   --  current location of the cursor.
   --  The line at which the entity starts is the line set in
   --  Set_File_Information, which must have been called first.
   --  From_Expression indicates the context of the entity. For instance, if
   --  the source code contains   A.Func (5).X, then the entity is "X", but
   --  From_Expression should be "A.Func (5).X". This expression is used when
   --  computing the full name of the entity to send it to the debugger for
   --  instance.

   procedure Set_Entity_Information
     (Context         : in out Selection_Context;
      Entity          : Xref.Root_Entity'Class;
      From_Expression : String := "");
   --  Same as above, but we provide directly the entity itself. This is more
   --  efficient when you already know the entity.
   --  This doesn't change the File_Information stored in the context, so that
   --  you can decide whether contextual menus should show file-related
   --  entries.

   function Has_Expression_Information
     (Context : Selection_Context) return Boolean;
   function Expression_Information
     (Context : Selection_Context) return String;
   --  Return expression context for the entity. See the description of
   --  From_Expression in the call to Set_Entity_Information.

   function Has_Entity_Name_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has entity name information
   function Entity_Name_Information
     (Context : Selection_Context) return String;
   --  Return the entity name information. This is a UTF8-encoded string

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
      Approximate_Search_Fallback : Boolean := True)
      return Xref.Root_Entity'Class;
   pragma Inline (Get_Entity);
   --  Return the xref entity stored in the context.

   function Get_Entity_Type_Of
     (Context           : Selection_Context)
      return Xref.Root_Entity'Class;
   --  Return the type of the xref entity stored in the context.

   function Has_Parent_Types
     (Context : Selection_Context) return Boolean;
   --  Whether the entity has at least one parent type (and thus is itself a
   --  type)

   function Get_Closest_Ref
     (Context : Selection_Context) return Xref.Root_Entity_Reference'Class;
   --  Return the entity reference corresponding to the current context. You
   --  should call Get_Entity first if you want to check for overloading
   --  entities.
   --  This information is cached in the context in case multiple places need
   --  to recompute it

   procedure Set_Is_Dispatching_Call
     (Context : Selection_Context; Is_Dispatching : Boolean);
   function Is_Dispatching_Call
     (Context : Selection_Context) return GNATCOLL.Tribooleans.Triboolean;
   --  Whether the user clicked on a dispatching call. This information is
   --  cached in the context the first time it is computed.

   procedure Get_Entity_Locations
     (Context       : Selection_Context;
      Spec_Location : out Xref.General_Location;
      Body_Location : out Xref.General_Location);
   --  Return the Entity's locations

   procedure Get_Entity_Spec_Locations
     (Context  : Selection_Context;
      Location : out Xref.General_Location);
   --  Return the Entity specification location

   ----------------------
   -- Activity_Context --
   ----------------------

   procedure Set_Activity_Information
     (Context : in out Selection_Context; Id : String);
   --  Fill Context with a single activity name

   procedure Set_Activity_Information
     (Context    : in out Selection_Context;
      Activities : String_List_Utils.String_List.Vector);
   --  Fill context with a set of activities

   function Has_Activity_Information
     (Context : Selection_Context) return Boolean;
   --  Return True if Context has some activity information

   function Activity_Information
     (Context : Selection_Context) return String_List_Utils.String_List.Vector;
   --  Returns a list of activity names

   --------------
   -- Browsers --
   --------------

   procedure Set_Browser_Information
     (Context : in out Selection_Context;
      Details : Gtkada.Canvas_View.Canvas_Event_Details);
   function Has_Browser_Information
     (Context : Selection_Context) return Boolean;
   function Browser_Information
     (Context : Selection_Context)
      return Gtkada.Canvas_View.Canvas_Event_Details;
   --  Store information as to where the user clicked in a browser.

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
