-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--
--  A class that encapsulates a project and its current view.
--  It is responsible for reporting events when the project was modified,
--  when the current view has changed,...
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
--
--  </description>


with Gtk.Handlers;
with Gtk.Object;

with Prj;
with Prj.Tree;

with Prj_API;

package Prj_Manager is

   type Project_Manager_Record is new Gtk.Object.Gtk_Object_Record
     with private;
   type Project_Manager is access all Project_Manager_Record'Class;
   --  ??? Should really be a GObject_Record, but we can't connect callbacks to
   --  ??? them yet.

   package Object_Callback is new Gtk.Handlers.Callback
     (Gtk.Object.Gtk_Object_Record);
   --  Package that can be used to connect calllback to a Project_Manager.

   procedure Gtk_New
     (Manager : out Project_Manager; Project : Prj.Tree.Project_Node_Id);
   --  Create a new project manager that controls Project.

   procedure Initialize
     (Manager : access Project_Manager_Record'Class;
      Project : Prj.Tree.Project_Node_Id);
   --  Internal function used when creating new project managers.

   function Get_Project
     (Manager : access Project_Manager_Record) return Prj.Tree.Project_Node_Id;
   --  Return the current project tree. This tree can be fully manipulated.

   function Get_Project_View
     (Manager : access Project_Manager_Record) return Prj.Project_Id;
   --  Return the current project view

   function Find_Scenario_Variables
     (Manager : access Project_Manager_Record)
      return Prj_API.Variable_Decl_Array;
   --  Return the list of scenario variables defined in project or one of its
   --  imported projects. The results are cached, since it could take a while
   --  to compute them in big projects.

   -----------------------------------------------
   -- Modifying the project or the project view --
   -----------------------------------------------

   procedure Change_Scenario_Variable
     (Manager  : access Project_Manager_Record;
      Variable : String;
      Value    : String);
   --  Set the value of a specific scenario variable.
   --  Note that the view is not automatically recomputed, so that it is
   --  possible to change several variables at the same time efficiently. You
   --  need to explicitely call Recompute_View.

   procedure Recompute_View
     (Manager  : access Project_Manager_Record);
   --  Recompute the view of the project, based on the current value of all
   --  scenario variables.
   --  This emits the "project_view_changed" signal.

   procedure Normalize
     (Manager        : access Project_Manager_Record;
      Project_Filter : Prj.Tree.Project_Node_Id);
   --  Normalize, if needed, one of the projects in the hierarchy of Manager.
   --  This doesn't send any signal, since the view doesn't change.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "project_view_changed"
   --    procedure Handler (Manager : access Project_Manager_Record'Class);
   --
   --    Emitted when the project view has been changed
   --
   --  <signals>

private
   type Variable_Decl_Array_Access is access Prj_API.Variable_Decl_Array;

   type Project_Manager_Record is new Gtk.Object.Gtk_Object_Record with record
      Project : Prj.Tree.Project_Node_Id;
      Project_View : Prj.Project_Id;

      Is_Normalized : Boolean := False;
      --  True when Project has been normalized. See Prj_Normalize for more
      --  information.

      Scenario_Variables : Variable_Decl_Array_Access := null;
      --  Cached result of Find_Scenario_Variables. This is computed only the
      --  first time Find_Scenario_Variables is called for every project.
   end record;


end Prj_Manager;
