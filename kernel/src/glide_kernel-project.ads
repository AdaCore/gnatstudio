-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with GNAT.OS_Lib;
with Projects.Registry;

package Glide_Kernel.Project is

   -------------------
   -- Project files --
   -------------------

   procedure Load_Project
     (Kernel : access Kernel_Handle_Record'Class; Project : String);
   --  Load project Project as the current project.
   --  This emits the "project_changed" and "project_view_changed" signals.
   --
   --  ??? What do we do if the project couldn't be loaded.

   procedure Load_Default_Project
     (Kernel               : access Kernel_Handle_Record'Class;
      Directory            : String;
      Load_Default_Desktop : Boolean := True);
   --  Create and load a default project in Directory.
   --  If Load_Desktop is true, then all current MDI children are removed, and
   --  the default desktop is reloaded.

   procedure Save_Project
     (Kernel    : access Kernel_Handle_Record'Class;
      Project   : Projects.Project_Type;
      Recursive : Boolean := False);
   --  Save Project to a file. If Recursive is True, all the imported projects
   --  are saved at the same time.
   --  The kernel registers that it is no longer using the default project.
   --
   --  This procedure will also regenerate the Makefiles whenever possible and
   --  needed.
   --
   --  There must be a project_view associated with the project

   procedure Save_Single_Project
     (Kernel    : access Kernel_Handle_Record'Class;
      Project   : Projects.Project_Type;
      Langs     : GNAT.OS_Lib.Argument_List);
   --  Save project, but none of its imported projects. As opposed to
   --  Save_Project, the project doesn't need to have an associated view in
   --  this procedure.
   --
   --  This procedure will also regenerate the Makefiles whenever possible and
   --  needed.
   --
   --  Langs is the list of languages defined for this project, and must be
   --  freed by the caller.

   function Save_Project_Conditional
     (Kernel    : access Kernel_Handle_Record'Class;
      Force     : Boolean) return Save_Return_Value;
   --  Ask the user whether to save the project (if Force is False), or save it
   --  inconditionnaly if Force is True.
   --  False is returned if the user has selected the "Cancel" button in the
   --  popup dialog.

   function Get_Project
     (Handle : access Kernel_Handle_Record'Class) return Projects.Project_Type;
   --  Return the current project tree. This tree can be fully manipulated, and
   --  extended. However, you should reevaluate the view after you have
   --  finished your changes, so as to report the changes to all the other
   --  tools.

   function Get_Registry
     (Handle : access Kernel_Handle_Record'Class)
      return Projects.Registry.Project_Registry'Class;
   --  Return the projects registry

   procedure Recompute_View (Handle  : access Kernel_Handle_Record'Class);
   --  Recompute the view of the project, based on the current value of all
   --  scenario variables.
   --  This emits the "project_view_changed" signal.

   ------------------
   -- Source files --
   ------------------

   procedure Change_Project_Dir
     (Handle : access Kernel_Handle_Record'Class;
      Dir    : String);
   --  Change the current directory.

   --------------
   -- Scenarii --
   --------------

   function Scenario_Variables
     (Kernel : access Kernel_Handle_Record'Class)
      return Projects.Scenario_Variable_Array;
   --  Return a list of all the scenario variables. This list is cached, so
   --  that future calls are fast.
   --  See also the signal "variable_changed" for the kernel.
   --  ??? This should be independent from any actual node, since the nodes
   --  might be freed at some point.

   type Command_Syntax is (GNAT_Syntax, Make_Syntax);
   --  Type used in Scenario_Variables_Cmd_Line to determine the command line
   --  syntax used when setting variables.
   --  GNAT_Syntax means use the GNAT project file syntax (-XVAR=value)
   --  Make_Syntax means use the GNU make syntax (VAR=value)

   function Scenario_Variables_Cmd_Line
     (Handle : access Kernel_Handle_Record'Class;
      Syntax : Command_Syntax) return String;
   --  Return the command line to use to set up the scenario variables when
   --  calling an external tool that handles project files.

end Glide_Kernel.Project;
