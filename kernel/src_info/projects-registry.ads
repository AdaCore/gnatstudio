-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002                            --
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

with Types;
with Prj.PP;

package Projects.Registry is

   type Project_Registry is new Projects.Abstract_Registry with private;
   type Project_Registry_Access is access all Project_Registry'Class;
   --  The registry is the name given to the set of currently loaded project
   --  files. Only one project hierarchy can be loaded at any given time.

   function Is_Valid_Project_Name (Name : String) return Boolean;
   --  Return True if Name is a valid project name

   procedure Destroy (Registry : in out Project_Registry);
   --  Destroy the registry

   procedure Initialize;
   --  Initialize this package. This function must be called before any use of
   --  the projects.* subprograms

   procedure Finalize;
   --  Free the memory used by this package (you should destroy any individuals
   --  Registry instances you may have)

   ----------------------
   -- Loading projects --
   ----------------------

   procedure Load
     (Registry           : in out Project_Registry;
      Root_Project_Path  : String;
      Errors             : Projects.Error_Report;
      New_Project_Loaded : out Boolean);
   --  Load a new project tree.
   --  Root_Project_Path might be relative to the current directory.
   --  On exit, New_Project_Loaded is set to True if a new project, different
   --  from the current one, was loaded.
   --  Recompute_View should be called afterward

   procedure Load_Default_Project
     (Registry  : in out Project_Registry;
      Directory : String);
   --  Load the default project for Directory.
   --  Recompute_View should be called afterward

   function Load_Or_Find
     (Registry     : Project_Registry;
      Project_Path : String) return Project_Type;
   --  Check if Project_Path is already loaded. If not, load it and add it to
   --  the list of currently loaded tree.

   procedure Recompute_View
     (Registry : in out Project_Registry;
      Errors   : Projects.Error_Report);
   --  Indicate to Register that the value for the environment values has
   --  changed, and that the project views need to be recomputed.

   function Scenario_Variables
     (Registry : Project_Registry)
      return Projects.Scenario_Variable_Array;
   --  Return the list of scenario variables used in the whole project
   --  hierarchy. The result is cached for efficiency

   procedure Reset_Scenario_Variables_Cache (Registry : Project_Registry);
   --  Reset the internal cache for all the scenario variables. This will force
   --  a (possibly long) recomputation the next time Scenario_Variables is
   --  called.
   --  This function is only needed for internal use in the projects hierarchy.

   function Get_Root_Project
     (Registry : Project_Registry)
      return Projects.Project_Type;
   --  Return the root project of the hierarchy

   procedure Pretty_Print
     (Project                            : Project_Type;
      Increment                          : Positive      := 3;
      Eliminate_Empty_Case_Constructions : Boolean       := False;
      Minimize_Empty_Lines               : Boolean       := False;
      W_Char                             : Prj.PP.Write_Char_Ap := null;
      W_Eol                              : Prj.PP.Write_Eol_Ap  := null;
      W_Str                              : Prj.PP.Write_Str_Ap  := null);
   --  See Prj.PP.Pretty_Print

   -------------------------
   -- Retrieving projects --
   -------------------------
   --  The following subprograms are provided to quickly retrieve a project
   --  previously loaded in the registry. They are all implemented very
   --  efficiently through hash-tables.
   --  In all cases, if no project matching the criterion is found, No_Project
   --  is returned.

   function Get_Project_From_Name
     (Registry : Project_Registry; Name : Types.Name_Id) return Project_Type;
   --  Select a project by name

   function Get_Project_From_File
     (Registry          : Project_Registry;
      Source_Filename   : String;
      Root_If_Not_Found : Boolean := True)
      return Project_Type;
   --  Select a project by one of its source files. If no project was found and
   --  Root_If_Not_Found is true, the root project is returned instead.

   -------------
   -- Sources --
   -------------

   function Get_Language_From_File
     (Registry : Project_Registry; Source_Filename : String)
      return Types.Name_Id;
   --  Return the language for a file

   function Language_Matches
     (Registry        : Project_Registry;
      Source_Filename : String;
      Filter          : Projects.Name_Id_Array) return Boolean;
   --  Return True if Source_Filename's language is in Filter

private

   type Project_Registry_Data;
   type Project_Registry_Data_Access is access Project_Registry_Data;
   type Project_Registry is new Abstract_Registry with record
      Data : Project_Registry_Data_Access;
      --  Use an access type, so that we do not have to expose the internal
      --  hash tables in this spec.
   end record;

end Projects.Registry;
