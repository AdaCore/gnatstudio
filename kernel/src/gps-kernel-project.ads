------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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
with Projects;
with GNATCOLL.Projects;
with GNATCOLL.VFS;

with Gtk.Window;        use Gtk.Window;

package GPS.Kernel.Project is

   -------------------
   -- Project files --
   -------------------

   procedure Load_Project
     (Kernel       : access Kernel_Handle_Record'Class;
      Project      : GNATCOLL.VFS.Virtual_File;
      No_Save      : Boolean := False;
      Clear        : Boolean := True;
      Is_Default   : Boolean := False;
      Keep_Desktop : Boolean := False);
   --  Load project Project as the current project.
   --  This emits the "project_changed" and "project_view_changed" signals.
   --  If the project isn't found, nothing is done (the previous project is
   --  kept).
   --  If No_Save is True, then the current project will not be saved if it was
   --  modified.
   --  If Clear is true, the Messages window is cleared before loading the
   --  project
   --  If Is_Default is true, the project is considered the default project
   --  If Keep_Desktop is false, load saved desktop configuration, keep current
   --  otherwise

   procedure Load_Default_Project
     (Kernel               : access Kernel_Handle_Record'Class;
      Directory            : GNATCOLL.VFS.Virtual_File;
      Load_Default_Desktop : Boolean := True;
      Clear                : Boolean := True);
   --  Create and load a default project in Directory.
   --  If Load_Desktop is true, then all current MDI children are removed, and
   --  the default desktop is reloaded. The "project_changed" hook is run if
   --  needed.
   --  If Clear is true, the Messages window is cleared before loading the
   --  default project

   function Load_Default_Project
     (Kernel : not null access Kernel_Handle_Record'Class;
      Parent : not null access Gtk_Window_Record'Class) return Boolean;
   --  Same as above, but with a signature which compatible with the
   --  Welcome_Dialogs.Welcome_Dialog_Action callback type.
   --  Used to display a "Start with default project" option in the GPS
   --  Welcome Dialog.

   procedure Load_Empty_Project
     (Kernel : access Kernel_Handle_Record'Class);
   --  Create and load an empty project in memory.

   procedure Reload_Project_If_Needed
     (Kernel : access Kernel_Handle_Record'Class;
      Recompute_View : Boolean := False);
   --  If any of the project files on the disk have been modified, reload the
   --  project. This doesn't recompute the view, though.

   function Save_Project
     (Kernel    : access Kernel_Handle_Record'Class;
      Project   : GNATCOLL.Projects.Project_Type;
      Recursive : Boolean := False) return Boolean;
   --  Save Project to a file. If Recursive is True, all the imported projects
   --  are saved at the same time.
   --  The kernel registers that it is no longer using the default project.
   --
   --  There must be a project_view associated with the project
   --
   --  Return False if at least one of the projects couldn't be saved
   --  successfully.

   function Save_Single_Project
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : GNATCOLL.Projects.Project_Type) return Boolean;
   --  Save project, but none of its imported projects. As opposed to
   --  Save_Project, the project doesn't need to have an associated view in
   --  this procedure.
   --  Return True if the project could be successfully saved

   function Get_Project_Tree
     (Handle : access Kernel_Handle_Record'Class)
      return GNATCOLL.Projects.Project_Tree_Access;
   function Get_Project
     (Handle : access Kernel_Handle_Record'Class)
      return GNATCOLL.Projects.Project_Type;
   --  Return the current project tree. This tree can be fully manipulated, and
   --  extended. However, you should reevaluate the view after you have
   --  finished your changes, so as to report the changes to all the other
   --  tools.

   procedure Create_Registry
     (Handle : access Kernel_Handle_Record'Class;
      Result : out Projects.Project_Registry_Access);
   --  Create the project registry. This needs to be called exactly once.

   function Get_Registry
     (Handle : access Kernel_Handle_Record'Class)
      return Projects.Project_Registry_Access;
   --  Return the projects registry

   procedure Recompute_View (Handle : access Kernel_Handle_Record'Class);
   --  Recompute the view of the project, based on the current value of all
   --  scenario variables.
   --  This emits the "project_view_changed" signal.

   procedure Save_Scenario_Vars_On_Exit
     (Handle : not null access Kernel_Handle_Record'Class);
   --  Save the value of scenario variables for the current project, on exit.
   --  This must be called while the project is still loaded.

   ---------------
   -- Scenarios --
   ---------------

   function Scenario_Variables
     (Kernel : access Kernel_Handle_Record'Class)
      return GNATCOLL.Projects.Scenario_Variable_Array;
   --  Return a list of all the scenario variables. This list is cached, so
   --  that future calls are fast.
   --  See also the signal "variable_changed" for the kernel.

   ----------------
   -- Attributes --
   ----------------

   function Get_Switches
     (Project           : GNATCOLL.Projects.Project_Type;
      Tool              : Tool_Properties_Record;
      File              : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Use_Initial_Value : Boolean := False) return GNAT.OS_Lib.Argument_List;
   --  Same as Projects.Get_Switches.
   --  If the package or its attribute isn't defined, or Project is No_Project,
   --  and Use_Initial_Value is True, then the default switches
   --  registered for that package/language combination are returned. These
   --  default switches are defined in XML files.

   -------------
   -- Dialogs --
   -------------

   function Display_Open_Project_Dialog
     (Kernel : not null access Kernel_Handle_Record'Class;
      Parent : not null access Gtk_Window_Record'Class) return Boolean;
   --  Display a file selection dialog asking allowing the user to open a
   --  project.
   --
   --  Parent is set to be the transient window of the displayed file selection
   --  dialog.
   --
   --  Return True if the selected project has been correctly loaded by GPS and
   --  False if the dialog was cancelled by the user.

end GPS.Kernel.Project;
