-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

--  This package provides access to the project characteristics such as
--  object and source path, options.
--
--  Several concepts are attached to projects, and this interface tries to
--  stay consistent with the following naming:
--
--  A --project-- is a tree representation of a project and its imported
--  projects.
--
--  A --scenario variable-- is any variable in a project that is typed (ie has
--  a defined set of possible values) and references external variables. In the
--  context of the GUI, we do not actually reference environment variables, but
--  these are set to the appropriate values before calling project-aware tools
--  like gnatmake, gnatbind,...
--
--  A --scenario-- is a named set of values for all the environment variables
--  in a project.
--
--  A --project view-- is the result of processing the tree in a given
--  scenario. Such a view needs to be re-eavaluted every time the scenario
--  changes. This is what is actually displayed in the project explorer and the
--  project viewer.
--  A fundamental assumption in the implementation is that the project
--  hierarchy (imported projects, modified projects,...) can not change when
--  the view changes.

with Basic_Types;
with Prj;
with Prj_API;
with Prj.Tree;

package Glide_Kernel.Project is

   function Get_Project_File_Name
     (Kernel : access Kernel_Handle_Record'Class) return String;
   --  Return the name of the current project, as should be used on the command
   --  line for all the tools that support projects
   --  The returned string includes the directory name for the project.
   --  If the current project is the default project (ie no other project was
   --  loaded by the user), then the empty string ("") is returned.

   function Find_Source_File
     (Kernel          : access Kernel_Handle_Record'Class;
      Short_File_Name : String;
      Use_Source_Path : Boolean := False)
      return String;
   --  Search in the project source path Short_File_Name and return its
   --  complete path, or an empty string in case of failure. If Use_Source_Path
   --  is set to True, then also try to locate the source file in the Source
   --  Path of the given Kernel Handle if the source file is not found in
   --  the project source path.

   function Find_Object_File
     (Kernel          : access Kernel_Handle_Record'Class;
      Short_File_Name : String;
      Use_Object_Path : Boolean := False)
      return String;
   --  This is the equivalent function of Find_Source_File for object files.
   --  This also works for ali files.

   function Scenario_Variables (Kernel : access Kernel_Handle_Record'Class)
      return Prj_API.Project_Node_Array;
   --  Return a list of all the scenario variables. This list is cached, so
   --  that future calls are fast.
   --  ??? This should be independent from any actual node, since the nodes
   --  ??? might be freed at some point.

   procedure Load_Project
     (Kernel : access Kernel_Handle_Record'Class; Project : String);
   --  Load project Project as the current project.
   --  This emits the "project_changed" and "project_view_changed" signals.
   --
   --  ??? What do we do if the project couldn't be loaded.

   function Get_Project (Handle : access Kernel_Handle_Record'Class)
      return Prj.Tree.Project_Node_Id;
   --  Return the current project tree. This tree can be fully manipulated, and
   --  extended. However, you should reevaluate the view after you have
   --  finished your changes, so as to report the changes to all the other
   --  tools.

   function Get_Project_View
     (Handle : access Kernel_Handle_Record'Class) return Prj.Project_Id;
   --  Return the current project view

   function Get_Current_Explorer_Context
     (Handle : access Kernel_Handle_Record'Class)
      return Selection_Context_Access;
   --  Return the currently selected project/directory/file in the
   --  explorer. This value is cached, and not computed directly from the
   --  explorer.

   procedure Recompute_View
     (Handle  : access Kernel_Handle_Record'Class);
   --  Recompute the view of the project, based on the current value of all
   --  scenario variables.
   --  This emits the "project_view_changed" signal.

   function Get_Source_Files
     (Handle : access Kernel_Handle_Record'Class)
      return Basic_Types.String_Array_Access;
   --  Return the list of source files belonging to the project described in
   --  Handle. Only the direct sources of the project are currently returned,
   --  i.e. not those found in subprojects.
   --  It is the caller's responsability to free the list.

   function Scenario_Variables_Cmd_Line
     (Handle : access Kernel_Handle_Record'Class)
      return String;
   --  Return the command line to use to set up the scenario variables when
   --  calling an external tool that handles project files

   function Directory_In_Source_Path
     (Handle         : access Kernel_Handle_Record'Class;
      Directory_Name : String) return Boolean;
   --  Return True if Directory_name belongs to the source path defined for the
   --  current view of the project.

   function File_In_Project_View
     (Handle          : access Kernel_Handle_Record'Class;
      Short_File_Name : String) return Boolean;
   --  Return True if Short_File_Name belongs to the current view of the
   --  project.
   --  Short_File_Name shouldn't include any directory specification.

end Glide_Kernel.Project;
