-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2003                       --
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
with VFS;
with Glib;

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

   ------------------------
   -- Predefined project --
   ------------------------
   --  The following subprograms are used to access the predefined environment
   --  for all the languages. This includes for instance the run time library
   --  for Ada, or the C library (/usr/include/...) for C files.

   function Get_Predefined_Source_Path
     (Registry : Project_Registry) return String;
   --  Return the predefined Source Path.
   --  Return the current directory if no source path has been set yet.

   function Get_Predefined_Object_Path
     (Registry : Project_Registry) return String;
   --  Return the predefined Object Path.
   --  Return the current directory if no source path has been set yet.

   function Get_Predefined_Source_Files
     (Registry : Project_Registry)
      return VFS.File_Array_Access;
   --  Return the list of sources found in the predefined project (e.g. the Ada
   --  runtime). Returned memory must be freed by the caller

   procedure Set_Predefined_Source_Path
     (Registry : in out Project_Registry; Path : String);
   --  Set the predefined source path

   procedure Set_Predefined_Object_Path
     (Registry : in out Project_Registry; Path : String);
   --  Set the predefined object path

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

   procedure Load_Custom_Project
     (Registry  : Project_Registry;
      Project   : Project_Type);
   --  Set Project as the root project. Recompute_View should be called
   --  afterward. The status of the project is automatically set to Default.
   --  It is recommended that you call Reset before this function, to free
   --  the memory.

   procedure Load_Default_Project
     (Registry  : in out Project_Registry;
      Directory : String);
   --  Load the default project for Directory.
   --  Recompute_View must be called afterwards.
   --  Use the function Glide_Kernel.Project.Load_Default_Project instead, so
   --  that default switches are properly initialized.

   function Load_Or_Find
     (Registry     : Project_Registry;
      Project_Path : String) return Project_Type;
   --  Check if Project_Path is already loaded. If not, load it and add it to
   --  the list of currently loaded tree.

   procedure Unload_Project
     (Registry  : Project_Registry; View_Only : Boolean := False);
   --  Reset the contents of the project registry. This should be called only
   --  if a new project is loaded, otherwise no project is accessible to the
   --  application any more.
   --  If View_Only is true, then the projects are not destroyed, but all the
   --  fields related to the current view are reset.
   --
   --  Use this function with care. No project is defined once this has been
   --  run, and most of GPS will crash or misbehave.
   --  Load_Custom_Project should be called immediately afterwards to
   --  prevent erroneous behavior.

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
      Source_Filename   : VFS.Virtual_File;
      Root_If_Not_Found : Boolean := True)
      return Project_Type;
   function Get_Project_From_File
     (Registry          : Project_Registry;
      Base_Name         : String;
      Root_If_Not_Found : Boolean := True)
      return Project_Type;
   --  Select a project by one of its source files. If no project was found and
   --  Root_If_Not_Found is true, the root project is returned instead.
   --  Source_Filename can either be the full pathname or the base name.

   procedure Reset_Name_Table
     (Registry           : Project_Registry;
      Project            : Project_Type;
      Old_Name, New_Name : String);
   --  Take into account a name change for a project.

   function Directory_Belongs_To_Project
     (Registry    : Project_Registry;
      Directory   : String;
      Direct_Only : Boolean := True) return Boolean;
   --  True if Directory belongs to one of the projects in the hierarchy.
   --  If Direct_Only is False, then True is returned if one of the
   --  subdirectories belong to the project, even if directory itself doesn't

   -------------
   -- Sources --
   -------------

   function Get_Full_Path_From_File
     (Registry        : Project_Registry;
      Filename        : Glib.UTF8_String;
      Use_Source_Path : Boolean;
      Use_Object_Path : Boolean;
      Project         : Project_Type := No_Project) return String;
   --  Return the directory to which Source_Filename belongs.
   --  the returned path is normalized, and includes directory/basename.
   --  If Use_Source_Path is true, the file is looked for on the include
   --  path. If Use_Object_Path is true, it is also looked for on the object
   --  path.
   --
   --  If the path is not found because the project doesn't belong to any of
   --  the source directories defined in the project hierarchy, then the empty
   --  string is returned.
   --
   --  This function also works for project files, which are looked among the
   --  loaded project tree.
   --
   --  Project is used to save time. This must be the project to which
   --  filename belongs, if it is known

   procedure Get_Full_Path_From_File
     (Registry        : Project_Registry;
      Filename        : Glib.UTF8_String;
      Use_Source_Path : Boolean;
      Use_Object_Path : Boolean;
      Project         : Project_Type := No_Project);
   --  Internal version of Get_Full_Path_From_File, which returns its result in
   --  Name_Buffer (1 .. Name_Len) for efficiency.
   --  Do not use outside of the projects.* hierarchy
   --
   --  Name_Len is set to 0 if the file wasn't found on the search path

   function Get_Language_From_File
     (Registry : Project_Registry; Source_Filename : VFS.Virtual_File)
      return Types.Name_Id;
   --  Return the language for a file

   procedure Add_Language_Extension
     (Registry      : Project_Registry;
      Language_Name : String;
      Extension     : String);
   --  Register Extension (which should include '.') as a valid extension for
   --  the language. This is used by Get_Language_From_File.
   --  Language_Name is case-insensitive.

   procedure Register_Default_Language_Extension
     (Registry            : Project_Registry;
      Language_Name       : String;
      Default_Spec_Suffix : String;
      Default_Body_Suffix : String);
   --  Register Default_Spec_Suffix and Default_Body_Suffix as the default
   --  extensions for the language. These are the extensions given by default
   --  in the naming scheme editor, and also when the user uses the
   --  "other file" navigation.
   --  Language_Name is case-insensitive.

   function Get_Registered_Extensions
     (Registry      : Project_Registry;
      Language_Name : String) return GNAT.OS_Lib.Argument_List;
   --  Return the list of registered extensions for Language_Name.
   --  The returned value must be freed by the user. Language_Name is
   --  case-insensitive.

   function Language_Matches
     (Registry        : Project_Registry;
      Source_Filename : VFS.Virtual_File;
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
