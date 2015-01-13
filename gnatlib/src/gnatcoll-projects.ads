------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2002-2015, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides an API to manipulate project files.
--
--  Projects are complex objects, and thus this API is extensive. A number of
--  simple subprograms are provided to access the most needed features (list
--  of files for instance), but it might be necessary to go into more complex
--  subprograms to access some properties of the projects.
--
--  Projects and Project_Trees
--  ==========================
--
--  A project is rarely used on its own. Most often, it is part of a whole set
--  of projects, importing each other, each of which is responsible for a small
--  part of the overall application. In the rest of this API, a Project is one
--  single .gpr file. A Project_Tree is a set of projects related through
--  "with" or "limited with" relations. This really is a cyclic graph, where
--  cycles can only occur through "limited with"s.
--
--  Multiple project trees can be loaded in memory at the same time.
--
--  Loading a Project_Tree (Project views)
--  ======================================
--
--  One never loads a project, we only ever load Project_Trees, as a consistent
--  whole. Loading a project is done in two steps
--     - create a syntactic tree representation in memory
--     - resolve the tree in the current scenario. The scenario can modify the
--       value of the attributes, the list of source dirs and source files,...
--       but never the relationships between projects. The result is called the
--       "Project View".
--  The first phase is extremely fast, whereas the second phase can take
--  several seconds (or more) on some complex projects when source files are
--  on remote file systems.
--  When you are doing modifications to an in-memory project, you should in
--  fact be doing the changes on the syntactic tree, and then recompute the
--  project view. Therefore, this API does not try to hide the two-steps
--  process, which are needed in some cases.
--
--  example:
--
--      with GNATCOLL.VFS;  use GNATCOLL.VFS;
--
--      Tree : Project_Tree;
--
--      Tree.Load (GNATCOLL.VFS.Create (+"/usr/local/projects/default.gpr");
--
--  Project attributes
--  ==================
--
--  A project's attributes describe all its properties. This API provides
--  various ways to access the attributes. Some of them are so often used that
--  specific subprograms exist (source files, source dirs,...), whereas in some
--  other cases you will have to use the more general API found later in this
--  package.
--
--  For instance, after you have loaded the project as above, here is how you
--  can find all the directories that might contain source files:
--
--     Dirs : File_Array :=
--        Source_Dirs (Tree.Root_Project, Recursive => True);

pragma Ada_05;

private with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;
private with Ada.Strings.Hash;
private with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with GNAT.Expect;
with GNAT.Strings;
with GNAT.OS_Lib;
with GNATCOLL.VFS;
pragma Warnings (Off);
pragma Warnings (Off, "*license of withed unit*");
private with Prj.Tree;
pragma Warnings (On, "*license of withed unit*");
pragma Warnings (On);
private with Namet;

package GNATCOLL.Projects is

   All_Packs : constant GNAT.Strings.String_List_Access;
   --  When used as the value of argument Packages_To_Check in procedures Load
   --  and Add_Imported_Project, all packages and attributes are checked. Any
   --  unknown package or attribute will result in an error.

   No_Packs : constant GNAT.Strings.String_List_Access;
   --  Default value for argument Packages_To_Check in procedures Load
   --  and Add_Imported_Project. All unknown packages and attributes will be
   --  ignored.

   type Project_Environment is tagged private;
   type Project_Environment_Access is access all Project_Environment'Class;
   --  This type describes the conditions under which a project is loaded. This
   --  includes scenario variables, various settings that affect the loading
   --  (trusted mode,...) as well as the default source and object directories
   --  in which the runtime files can be found.
   --  This environment might be common to a set of project trees loaded at the
   --  same time in memory.
   --  You can already create such types via the Initialize subprogram below.
   --  However, a default environment will be build automatically if you do
   --  not provide one when parsing a project.
   --  If you subclass this type, you should still call Initialize after
   --  allocating a variable of this type.

   type Project_Tree is tagged private;
   type Project_Tree_Access is access all Project_Tree'Class;
   --  A set of project files, related through "with"s or "limited with"s.
   --  This is a tagged object so that you can use the Ada05 dotted notation to
   --  access its primitive operations, and so that you can add your own
   --  fields (or precompute some data that you want to reuse).
   --  In practice, this is not necessarily a "tree" in the data structure
   --  sense, more like a graph, but the term Tree makes it more obvious that
   --  one of the projects plays a special role, the root project.

   procedure Initialize (Self : in out Project_Environment_Access);
   --  Allocate a new environment (if Self is null) and initialize internal
   --  data

   procedure Free (Self : in out Project_Environment_Access);
   procedure Free (Self : in out Project_Tree_Access);
   --  Free memory allocated for the pointer. You should first unload the tree.

   type Project_Type is tagged private;
   type Project_Type_Access is access all Project_Type'Class;
   No_Project : aliased constant Project_Type;
   --  This type represents a single .gpr project file, which is part of a
   --  Project_Tree.
   --  A Project_Type only makes sense in the context of a tree, so it contains
   --  an implicit reference to the tree that was used to load it.
   --
   --  This type is tagged, so that you can use Ada05 dotted notation (and
   --  it is implemented as a controlled type internally for reference
   --  counting). However, you cannot extend it (because instances are created
   --  implicitly by Load). If you need to add custom data to projects, see the
   --  use of Data_Factory below.
   --  It is always safe to store an instance of Project_Type in your records,
   --  you do not need to store an access on Project_Type'Class.

   Invalid_Project : exception;
   --  raised when attempting to load an invalid project.

   type Error_Report is access procedure (Msg : String);
   --  Callback used to report warnings and error messages to the caller.

   ----------------------
   -- Loading projects --
   ----------------------
   --  The following subprograms provide ways to load projects.
   --  In particular, they give access to the two phases of the loading, as
   --  described in the general comments of this package.

   procedure Load
     (Self               : in out Project_Tree;
      Root_Project_Path  : GNATCOLL.VFS.Virtual_File;
      Env                : Project_Environment_Access := null;
      Packages_To_Check  : GNAT.Strings.String_List_Access := No_Packs;
      Errors             : Error_Report := null;
      Recompute_View     : Boolean := True);
   --  Load a new set of project files, starting from a root project.
   --  Root_Project_Path is either an absolute path, or relative to the current
   --  directory. It should point to a readable existing file.
   --  The two steps of the loading (see general description of this package)
   --  are performed automatically.
   --  If the project itself or some of its dependencies should be found on the
   --  project path, the latter should be initialized properly (if you have
   --  already loaded a project, you might want to reuse the environment by
   --  passing a non-empty Env parameter).
   --
   --  A list of packages where all the attributes declared must be recognized
   --  may be indicated by Packages_To_Check. By default, no package is
   --  checked. There may be unknown attributes in packages that are not
   --  included in Packages_To_Check. If a value given for Packages_To_Check
   --  has been allocated, this value may be freed immediately after the call
   --  to Load if it is no longer needed.
   --
   --  Errors and warnings that occur during loading are reported through the
   --  Errors callback. If the project could not be loaded, the exception
   --  Invalid_Project is then raised. In such a case, any project set
   --  previously loaded is still in memory.
   --
   --  If no value is provided for Env, a default one will be created
   --  automatically. Passing a value is useful if you need to share the
   --  environment between separate project trees. This default value will
   --  never be freed though, resulting in a potential memory leak.
   --
   --  If that project is already loaded in Self, it will be reloaded if any of
   --  the .gpr files have changed on disk (see also Reload_If_Needed).
   --
   --  The previous project is automatically unloaded, and existing instances
   --  of Project_Type become invalid and should not be used anymore.
   --
   --  If Recompute_View is False, the subprogram Recompute_View will not be
   --  called automatically. This gives you a chance to do some dynamic
   --  changes on the project (changing attributes for instance), even though
   --  you will need to call Recompute_View yourself.

   procedure Set_Trusted_Mode
     (Self : in out Project_Environment; Trusted : Boolean := True);
   function Trusted_Mode (Self : Project_Environment) return Boolean;
   --  Set/Get the trusted mode for the project set:
   --  If it is True, then it is assumed that no links are used in the project,
   --  and that directory names cannot match file names according to the
   --  naming scheme. This provides much faster loading.
   --  The default is True.

   procedure Reload_If_Needed
     (Self     : in out Project_Tree;
      Reloaded : out Boolean;
      Recompute_View : Boolean := False;
      Errors   : Error_Report := null);
   --  If any of the project files have changed on the disk, reloads the whole
   --  project tree. This performs the two phases of the loading.
   --  On exit, Reloaded is set to false if no reloading took place.

   procedure Load_Empty_Project
     (Self : in out Project_Tree;
      Env  : Project_Environment_Access := null;
      Name : String := "empty";
      Recompute_View : Boolean := True);
   --  Load an empty project.
   --  There is no source .gpr file corresponding to that project, which is
   --  created in memory. It has no source file. In general this procedure is
   --  used to initialize a usable and valid project tree, which the user will
   --  later replace with an actual project.
   --  A default version of Env will be created if null is passed.

   procedure Recompute_View
     (Self   : in out Project_Tree;
      Errors : Error_Report := null);
   --  Recompute the view of the project (the second phase of the loading).
   --  This does not change the in-memory syntactic tree of the project, but
   --  based on the current value of the scenario variables it might change the
   --  list of source files, source directories,...
   --  This procedure only needs to be called after you have modified the
   --  project in memory. It is automatically called by the various Load*
   --  subprograms.

   procedure Unload (Self : in out Project_Tree);
   --  Unload the project loaded in Self, and free the associated memory.
   --  No project is accessible through this tree once this has been called,
   --  and existing instances of Project_Type have become invalid.

   procedure Finalize;
   --  Free the memory used by this package (you should unload any individuals
   --  trees instances you may have).
   --  This call is optional, but recommended if you want to monitor memory
   --  leaks in your application.

   type Project_Status is (From_File, Default, From_Executable, Empty);
   function Status (Self : Project_Tree) return Project_Status;
   procedure Set_Status (Self : Project_Tree; Status : Project_Status);
   --  How the project was created: either read from a file, automatically
   --  created from a directory, automatically created from an executable
   --  (debugger case), or default empty project. An actual project file exists
   --  on disk only in the From_File or Default cases.

   function Is_Aggregate_Project (Self : Project_Type) return Boolean;
   --  Return true if the current project is an aggregate project.

   function Is_Aggregate_Library (Self : Project_Type) return Boolean;
   --  Return true if the current project is an aggregate library project.

   ------------------
   -- Project data --
   ------------------
   --  To make it easier to store instances of Project_Type in a data
   --  structure, that type is not visibly tagged (you do not have to store an
   --  access to Project_Type'Class and find out when you can free it).
   --  However, it might be convient to associate your own custom data with a
   --  project (for instance extra caches for attributes that your application
   --  uses often, or other type of data).
   --  To do so, you should subclass Project_Data, as well as Project_Tree. For
   --  the latter, override the Data_Factory function to create a new instance
   --  of Project_Data. One such new instance will be associated with each
   --  projects that are loaded in the tree, and you can retrieve each
   --  project's own data with the Data function below.
   --  For instance:
   --      type My_Project_Data is new Project_Data with record
   --           ...
   --      end record;
   --
   --      type My_Project_Tree is new Project_Tree with null record;
   --      overriding function Data_Factory
   --        (Self : My_Project_Tree) return Project_Data_Access is
   --      begin
   --          return new My_Project_Data;
   --      end Data_Factory;
   --
   --      Tree : My_Project_Tree;
   --      Tree.Load (Create ("/usr/local/project.gpr"));
   --
   --      Data : My_Project_Data := My_Project_Data
   --         (Data (Tree.Root_Project).all);

   type Project_Data is tagged private;
   type Project_Data_Access is access Project_Data'Class;

   function Data_Factory (Self : Project_Tree) return Project_Data_Access;
   --  Returns a new instance of Project_Data.
   --  This can be overridden if you want to store additional data in a
   --  project. In this case, you should create your own child of Project_Data,
   --  and return an instance of that child from this factory.
   --  This function is called implicitly by Load whenever a new project file
   --  is parsed and a new instance of Project_Type created.

   function Data (Project : Project_Type) return Project_Data_Access;
   --  Return the data associated with the project. You must not free the
   --  resulting pointer.

   procedure On_Free (Self : in out Project_Data);
   --  Called when Self needs to be freed. If you have subclassed Project_Data,
   --  you should override this procedure to free the data. You also need to
   --  call the inherited version of On_Free.

   ----------------------
   -- Predefined paths --
   ----------------------
   --  Some directories are implicitly part of a project. These are in general
   --  the default directories used by compilers to access their runtime or
   --  look for other projects.
   --  You must tell GNATCOLL what those predefined directories are, although
   --  some facilities are provided to automatically extract them from gnatls
   --  in the case of GNAT Pro for Ada.
   --  When using C, you might want to add /usr/include to the predefined paths
   --  for instance.

   procedure Set_Predefined_Source_Path
     (Self : in out Project_Environment; Path : GNATCOLL.VFS.File_Array);
   procedure Set_Predefined_Object_Path
     (Self : in out Project_Environment; Path : GNATCOLL.VFS.File_Array);
   procedure Set_Predefined_Project_Path
     (Self : in out Project_Environment; Path : GNATCOLL.VFS.File_Array);
   --  Set the predefined environment.
   --  This should be called after loading the project.

   function Predefined_Source_Path
     (Self : Project_Environment) return GNATCOLL.VFS.File_Array;
   function Predefined_Object_Path
     (Self : Project_Environment) return GNATCOLL.VFS.File_Array;
   function Predefined_Project_Path
     (Self : Project_Environment) return GNATCOLL.VFS.File_Array;
   --  Return the predefined paths, or the current directory if no
   --  paths have been set yet.

   procedure Invalidate_Gnatls_Cache (Self : in out Project_Environment);
   --  Forces the recomputation of the predefined paths via gnatls.
   --  This should be called prior to calling Recompute_View, when the
   --  environment has changed (ADA_PROJECT_PATH, running gnatls on a
   --  different host,...)

   procedure Set_Default_Gnatls
     (Self         : in out Project_Environment;
      Gnatls       : String);
   --  Set the default gnatls to run (before a project is loaded).
   --  This impacts the default path on which projects are looked for, but
   --  will be overridden if the user has specified an IDE.Gnatlist attribute
   --  in his project.
   --  This procedure is now deprecated, and we recommend that project use the
   --  Runtime and Target attributes instead. See Set_Target_And_Runtime below.

   procedure Set_Target_And_Runtime
     (Self    : in out Project_Environment;
      Target  : String := "";
      Runtime : String := "");
   --  Override the Runtime and Target attributes. These values take priority
   --  over what is defined in the project file.
   --  These are generally set from --target and --RTS command line switches.

   procedure Set_Path_From_Gnatls
     (Self         : in out Project_Environment;
      Gnatls       : String;
      GNAT_Version : out GNAT.Strings.String_Access;
      Errors       : Error_Report := null);
   --  Execute the given "gnatls" command with switch "-v" and parse the
   --  default search paths and project path from it.
   --  This function returns the version of GNAT as read from gnatls. This
   --  string must be freed by the user (Set_GNAT_Version is also called).

   procedure Set_Path_From_Gnatls_Output
     (Self         : in out Project_Environment;
      Output       : String;
      Host         : String := GNATCOLL.VFS.Local_Host;
      GNAT_Version : out GNAT.Strings.String_Access);
   --  Same as Set_Path_From_Gnatls, but gets the output of "gnatls -v" in
   --  input (and does not spawn a command).
   --  This procedure also calls Set_GNAT_Version.

   procedure Spawn_Gnatls
     (Self         : Project_Environment;
      Fd           : out GNAT.Expect.Process_Descriptor_Access;
      Gnatls_Args  : GNAT.OS_Lib.Argument_List_Access;
      Errors       : Error_Report);
   --  Spawns the gnatls command passed in argument.
   --  This subprogram can be overridden if gnatls needs to be spawned on
   --  another machine (the default is to spawn on the local machine).

   function Gnatls_Host
     (Self : Project_Environment) return String;
   --  Returns the name of the remote host configuration responsible for
   --  executing gnatls. By default, returns the local host.

   procedure Set_GNAT_Version
      (Self    : Project_Environment;
       Version : String) is null;
   --  This procedure is called when the project manager spawns and parses
   --  gnatls. At that point, it finds the version of GNAT and calls this
   --  subprogram, which you can override if you wish to store that version
   --  somewhere.

   ------------------------
   -- Project properties --
   ------------------------
   --  The following subprograms give access to general properties of the
   --  project. See the section below to get access to the project's attributes

   Project_File_Extension : constant GNATCOLL.VFS.Filesystem_String;

   overriding function "=" (Prj1, Prj2 : Project_Type) return Boolean;
   --  Return true if Prj1 and Prj2 reference the same project

   function Name (Project : Project_Type) return String;
   --  Return the name of the project.

   function Project_Path
     (Project : Project_Type;
      Host    : String := GNATCOLL.VFS.Local_Host)
      return GNATCOLL.VFS.Virtual_File;
   --  Return the path to the project file
   --  If Host is given, the path will be the one on the specified host.

   function Extended_Project
     (Project : Project_Type) return Project_Type;
   --  Return the project extended by project, or No_Project is there is none.
   --  If Project is an "extends all", this will return the project mentioned
   --  in the "extends all" clause, in general the root of the extended project
   --  tree.

   function Extending_Project
     (Project : Project_Type; Recurse : Boolean := False) return Project_Type;
   --  Return the project that extends Project, or No_Project if Project is not
   --  extended within the hierarchy and Recurse is False.
   --  This is in the context of the Project_Tree in which Project was loaded,
   --  so there can be at most one extending project.
   --  If Recurse is True, then the lowest possible project is returned, even
   --  if it is Project itself. This is useful when looking for specific source
   --  files.

   -----------------
   -- Directories --
   -----------------

   function Source_Dirs
     (Project   : Project_Type;
      Recursive : Boolean := False) return GNATCOLL.VFS.File_Array;
   pragma Precondition (Project /= No_Project);
   --  Return the list of source directories.
   --  The directories are returned in the order in which they are defined in
   --  the project files, so that in the case of Ada files the file will first
   --  be searched in the first directories, and if not found in the second,...
   --  If Recursive is True, the source directories of the subtree rooted at
   --  Project (ie all the projects imported directly or indirectly by Project)
   --  will also be returned, but in this case the order of the directories in
   --  the result is undefined and the result cannot be considered as a search
   --  path for project sources.
   --  Note that duplicate directories might be returned when directories are
   --  shared by multiple projects in the same tree.

   function Directory_Belongs_To_Project
     (Self        : Project_Tree;
      Directory   : GNATCOLL.VFS.Filesystem_String;
      Direct_Only : Boolean := True) return Boolean;
   --  True if Directory belongs to one of the projects in the hierarchy.
   --  If Direct_Only is False, then True is returned if one of the
   --  subdirectories belong to the project, even if directory itself doesn't.
   --  This function is much more efficient than retrieving the source
   --  directories and doing the computation yourself, since it uses cached
   --  data.

   procedure Set_Object_Subdir
     (Self   : in out Project_Environment;
      Subdir : GNATCOLL.VFS.Filesystem_String);
   function Object_Subdir
     (Self   : Project_Environment) return GNATCOLL.VFS.Filesystem_String;
   --  The same project can be used in multiple contexts. In particular, the
   --  command line tools support the switch --subdirs, so that the sources can
   --  be built differently in various scenarios while putting the resulting
   --  object files in a separate object directory every time.
   --  This procedure lets you specify the name of a subdirectory of the object
   --  directory in which the object files are currently put. This directory
   --  is automatically taken into account by the Object_Path function below.

   procedure Set_Xrefs_Subdir
     (Self   : in out Project_Environment;
      Subdir : GNATCOLL.VFS.Filesystem_String);
   function Xrefs_Subdir
     (Self   : Project_Environment) return GNATCOLL.VFS.Filesystem_String;
   --  This is similar to Set_Object_Subdir, but is meant to be used when a
   --  second compiler is used to create the cross-references info. This info
   --  is put in a separate subdirectory of the object directory

   function Object_Dir
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File;
   --  Return the object directory for this project.
   --  This includes the subdirectory if any was set through Set_Object_Subdir.

   function Object_Path
     (Project             : Project_Type;
      Recursive           : Boolean := False;
      Including_Libraries : Boolean := False;
      Xrefs_Dirs          : Boolean := False) return GNATCOLL.VFS.File_Array;
   --  Return the object path for this project. The empty string is returned
   --  if the project doesn't have any object directory (i.e. the user
   --  explicitely set it to the empty string). If Including_Libraries is
   --  True and Project is a library project, it returns both object and ALI
   --  paths (in that order) or only ALI path if project doesn't have object
   --  directory.
   --  If an Xrefs Subdir is set in the project to a non-empty
   --  string, and Xrefs_Dir is set, then the corresponding subdirectory is
   --  returned if it exists. Else, the subdir corresponding to the current
   --  builder mode is returned.
   --  If Recursive is True, it also includes the object path (and ALI paths if
   --  requested) for all imported projects.
   --
   --  If the view is not fully recomputed, an empty path is returned.

   function Executables_Directory
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File;
   --  Return the directory that contains the executables generated for the
   --  main programs in Project. This is either Exec_Dir or Object_Dir.

   function Library_Directory
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File;
   --  If a library project, return the directory where the library resides.

   function Library_Ali_Directory
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File;
   --  If a library project, return where the ALI files are copied.

   ---------------
   -- File info --
   ---------------

   type Unit_Parts is (Unit_Body, Unit_Spec, Unit_Separate);
   --  A unit is usally composed of two parts: the spec and the body.
   --    - Unit_Spec represents package/subprogram/generic declarations
   --    - Unit_Body represents package/subprogram/generic bodies and subunits.
   --    - Unit_Separate is used for additional implementation code in Ada
   --      separates.

   type File_Info_Abstract is abstract tagged null record;
   function Less (L, R : File_Info_Abstract'Class) return Boolean;
   function "<" (L, R : File_Info_Abstract) return Boolean is abstract;

   type File_Info is new File_Info_Abstract with private;
   type File_Info_Access is access File_Info;
   function "<" (L, R : File_Info) return Boolean;
   --  Various information that can be gathered about a file

   procedure Free (Self : in out File_Info_Access);
   --  Free the memory used by Self

   function Project
     (Info              : File_Info'Class;
      Root_If_Not_Found : Boolean := False) return Project_Type;
   --  Retrieve the project that the file belongs to. If the file is not a
   --  source of the project, No_Project is returned, unless Root_If_Not_Found
   --  is true, in which case the root project is returned.

   function Unit_Part (Info : File_Info'Class) return Unit_Parts;
   function Unit_Name (Info : File_Info'Class) return String;
   function Language  (Info : File_Info'Class) return String;
   function File (Info : File_Info'Class) return GNATCOLL.VFS.Virtual_File;
   --  Retrieve information about the file.

   function Other_File
     (Self : Project_Tree;
      File : GNATCOLL.VFS.Virtual_File) return GNATCOLL.VFS.Virtual_File;
   --  If Info is a spec, returns the body of the same unit. If Info is a
   --  body, returns its spec.
   --  If there is no "other file" in the project, but we could compute the
   --  name it should have, that name is returned (the file is created in the
   --  same directory as File).
   --  Otherwise, File itself is returned.

   function Info
     (Self : Project_Tree'Class; File : GNATCOLL.VFS.Virtual_File)
      return File_Info;
   pragma Precondition (not Self.Root_Project.Is_Aggregate_Project);
   --  Retrieve information about the source file.
   --  The language is computed from the project's naming scheme and from the
   --  additional extensions registered through Add_Language_Extension.
   --  Can only be applied if root project is not an aggregate project,
   --  Program_Error raised otherwise.

   package File_Info_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (File_Info_Abstract'Class, Less);
   type File_Info_Set is new File_Info_Sets.Set with null record;

   function Info_Set
     (Self : Project_Tree'Class; File : GNATCOLL.VFS.Virtual_File)
      return File_Info_Set;
   --  Retrieve information about the source file.
   --  The language is computed from the project's naming scheme and from the
   --  additional extensions registered through Add_Language_Extension.
   --  Can be applied both to aggregate and regular projects. For aggregate
   --  project tree may return several elements in the set.
   --
   --  This function never returns an empty set. When the file does not belong
   --  to the project, the function returns a set with a single element. In
   --  this element, the project field is set to No_Project, but other fields
   --  are set to best guesses (like the language of the file for instance).

   -----------
   -- Files --
   -----------

   function Source_Files
     (Project   : Project_Type;
      Recursive : Boolean := False) return GNATCOLL.VFS.File_Array_Access;
   --  Return the list of source files belonging to the project.
   --  If Recursive is False, only the direct sources of the project are
   --  returned. Otherwise, the sources from imported projects are returned as
   --  well.
   --
   --  The returned value must be freed by the user
   --
   --  The sources that are returned are not necessarily the ones that are used
   --  when compiling the root project, since some of them might be overriden
   --  by extending projects. Instead, they are the sources that would be used
   --  when compiling from Project ("gnatmake -PProject"). Base names of
   --  returned files may not be unique in case when root project is an
   --  aggregate project. For languages other than Ada multiple sources with
   --  same base name can also be returned.

   type File_And_Project is record
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
   end record;

   type File_And_Project_Array is array (Natural range <>) of File_And_Project;
   type File_And_Project_Array_Access is access all File_And_Project_Array;

   procedure Free (Self : in out File_And_Project_Array_Access);
   --  Free the memory used by Self

   function Source_Files
     (Project   : Project_Type;
      Recursive : Boolean := False) return File_And_Project_Array_Access;
   --  Return the list of source files (recursively) for Project.
   --  For each file, include the name of its project, which is especially
   --  useful in the context of aggregate projects.
   --  Result must be freed by the caller.

   function Direct_Sources_Count (Project : Project_Type) return Natural;
   --  Return the number of direct source files for Project

   function Create
     (Self            : Project_Tree;
      Name            : GNATCOLL.VFS.Filesystem_String;
      Project         : Project_Type'Class := No_Project;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True)
      return GNATCOLL.VFS.Virtual_File;
   procedure Create
     (Self            : Project_Tree;
      Name            : GNATCOLL.VFS.Filesystem_String;
      Project         : Project_Type'Class := No_Project;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True;
      Ambiguous       : out Boolean;
      File            : out GNATCOLL.VFS.Virtual_File;
      Predefined_Only : Boolean := False);
   --  Create a new file. This will automatically try to solve Name to an
   --  absolute path if it currently is a base name.
   --
   --  If Name is an absolute path, it is returned as is. Otherwise, only the
   --  base name is used (ie we remove any directory information from Name).
   --
   --  If a source file matches Name and Use_Source_Path is true, it is always
   --  returned, whether it is part of Project or not. This is the most
   --  frequent use for this function. We never look at the cache when a
   --  specific project is specified, since you might be looking for sources
   --  that are in fact overridden in an extending project.
   --  Set Predefined_Only to True to disable looking in the project sources
   --  and only look in the predefined source files.
   --
   --  Otherwise, the file will be searched for in the source dirs and/or
   --  object dirs of either a specific Project or in the whole project tree.
   --  The result is cached for efficiency.
   --  As a special case, if Name ends with '.gpr', it is also looked for among
   --  the already loaded project, even if their directory is outside the
   --  source dirs and object dirs. See also Project_From_Name.
   --
   --  If no such file is found, GNATCOLL.VFS.No_File is returned and
   --  Ambiguous is set to False.
   --
   --  The matching from base source names to full path names is potentially
   --  ambiguous when using aggregate projects, because it is valid to have
   --  multiple files with the same base name within a given project tree.
   --  In such an ambiguous case, this function will return No_File.
   --  To lift this ambiguity, and if you know which project the file is found
   --  in, you must pass a Project argument. The file must be a direct source
   --  of that project.
   --
   --  If a given full path is part of the sources for several projects, this
   --  is also considered as ambiguous, because the associated object file,
   --  for instance, is different. However, in this case the returned value is
   --  set to the common source file, and Ambiguous is set to True.
   --
   --  When a file is ambiguous, No_File is returned, and Ambiguous (if given)
   --  is set To True.
   --
   --  If you are not sure which project the file belongs to, you can also use
   --  Create_From_Project below.

   function Create_From_Project
     (Self            : Project_Type'Class;
      Name            : GNATCOLL.VFS.Filesystem_String)
      return File_Info;
   pragma Precondition
     (Project_Type (Self) = No_Project
      or else not Self.Is_Aggregate_Project);
   --  This is similar to Create above (converts from a base name to a full
   --  path for a source file).
   --  Here, however, the source is searched in the specified project or
   --  any of the projects it imports (Create only searches in the direct
   --  sources of the project). This function also only works for source files,
   --  not for project files or ALI files.
   --  This function will also search in the predefined source path.
   --  Self must not be an aggregate project, to remove ambiguities.

   function Predefined_Source_Files
     (Self : access Project_Environment) return GNATCOLL.VFS.File_Array;
   --  Return the list of sources found in the predefined directories (e.g. the
   --  Ada runtime).
   --  Computing this information will take long the first time

   function Has_Multi_Unit_Sources (Project : Project_Type) return Boolean;
   --  Whether at least one source file from the project contains multiple
   --  units (language is unspecified, but will in general be Ada since that's
   --  currently the only unit-based language supported by project files).

   function Executable_Name
     (Project : Project_Type;
      File    : GNATCOLL.VFS.Filesystem_String)
      return GNATCOLL.VFS.Filesystem_String;
   --  Return the name of the executable, either read from the project or
   --  computed from File. This name does not include executable suffixes (like
   --  ".exe" for instance).
   --  If Project is No_Project, the default executable name for File is
   --  returned.

   function Is_Main_File
     (Project        : Project_Type;
      File           : GNATCOLL.VFS.Filesystem_String;
      Case_Sensitive : Boolean := True) return Boolean;
   --  Return True if File is one of the main files of Project.
   --  Case_Sensitive indicates whether the build machine is case sensitive.
   --  In general, this machine is the local machine on which the application
   --  is running, but sometimes you might actually want to process the project
   --  on a remote server.
   --  You need to specify the sensitivity of the remote server.

   function Library_Files
     (Self                : Project_Type;
      Recursive           : Boolean := False;
      Including_Libraries : Boolean := True;
      Xrefs_Dirs          : Boolean := False;
      ALI_Ext             : GNATCOLL.VFS.Filesystem_String := ".ali";
      Include_Predefined  : Boolean := False;
      Exclude_Overridden  : Boolean := True)
      return GNATCOLL.VFS.File_Array_Access;
   --  Return a list of all LI files for this project. This never returns null.
   --  The parameters are similar to that of Object_Path.
   --
   --  ALI_Ext is the suffix to use for those files. As a special case, if
   --  it starts with "^" it is considered as a regexp matching the basename of
   --  relevant files.
   --
   --  Including_Libraries controls whether the project's Library_Dir is
   --  taken into account. This has the following impacts:
   --     * if True: when a project only has a library_dir (for instance a
   --       third party library with Externally_Built set to "true"), then the
   --       ALI files are read in that directory. When a library project has
   --       both an object_dir and a library_dir, then only the former is
   --       searched, and the library_dir is ignored (since the object files
   --       are copied from object_dir to library_dir by the builder).
   --     * if False, then library_dir is always ignored. As such, a third
   --       party library project will have no ALI file.
   --       ??? In general, passing False is of little interest since some ALI
   --       files will be missing.
   --  If Include_Predefined is True, then the predefined object directories
   --  (generally the Ada runtime for instance) will also be searched. Setting
   --  this to True probably only makes sense when Recursive is also True,
   --  although this isn't enforced.
   --
   --  If Exclude_Overridden is true, then the files that also exist in an
   --  extending project are not included in the result. For instance, the
   --  extending project might also have a "pkg.ali" if "pkg.ads" was
   --  recompiled in the context of the extending project, and thus we do not
   --  need to look at "pkg.ali" from the extended project.

   type Library_Info is record
      Library_File : GNATCOLL.VFS.Virtual_File;
      LI_Project   : Project_Type_Access;
      Non_Aggregate_Root_Project : Project_Type_Access;
      Source       : File_Info_Access;
   end record;
   --  Source is set to null for ALI files found in the predefined source
   --  path, since we do not know the mapping to source files in this context.
   --  When is Source *not* set to null??? and what does it correspond to
   --  in that case???
   --
   --  LI_Project is the project in which the LI file was found. It might not
   --  be the same as the source's project, when using extending projects.
   --  null for predefined sources.
   --
   --  Non_Aggregate_Root_Project is the non-aggregated root project for the
   --  tree. When using aggregated projects, it will take the value of any of
   --  the aggregated project. In other cases, this is the project loaded by
   --  the user. Set to null for predefined sources.

   procedure Free (Self : in out Library_Info);
   --  Free the memory used by Self

   package Library_Info_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Library_Info);
   type Library_Info_List is new Library_Info_Lists.List with null record;
   overriding procedure Clear (Self : in out Library_Info_List);

   procedure Library_Files
     (Self                : Project_Type;
      Recursive           : Boolean := False;
      Including_Libraries : Boolean := True;
      Xrefs_Dirs          : Boolean := False;
      ALI_Ext             : GNATCOLL.VFS.Filesystem_String := ".ali";
      Include_Predefined  : Boolean := False;
      List                : in out Library_Info_List'Class;
      Exclude_Overridden  : Boolean := True);
   --  same as Library_Files, but also returns information about the source
   --  file associated with each LI file.
   --  The new files are appended to the list, as a way to collect multiple
   --  extensions (in addition to the support of regexp for ALI_Ext).

   ------------------
   -- Config files --
   ------------------

   procedure Set_Config_File
     (Self        : in out Project_Environment;
      Config_File : GNATCOLL.VFS.Virtual_File);
   --  Set the name of a configuration file to parse before loading the
   --  project. Such a file is in general generated when running 'gprconfig'
   --  on the command line, and will contain the default naming schemes (among
   --  other information) used for all projects.
   --  All the attributes defined in that file will provide the default value
   --  when loading projects later on.

   procedure Set_Automatic_Config_File
     (Self        : in out Project_Environment;
      Autoconf    : Boolean := True);
   --  Whether this package should spawn 'gprconfig' to generate a
   --  configuration file automatically.
   --  If a name was specified via Set_Config_File and the file exists, it is
   --  parsed (and not regenerated).
   --  The switch --target will be passed to gprconfig only if the project
   --  defines the Target attribute or Set_Target_And_Runtime was called.
   --  The target is NOT automatically extracted from IDE attributes
   --  (since their values are not yet known when gprconfig is spawned).

   procedure Add_Config_Dir
     (Self      : in out Project_Environment;
      Directory : GNATCOLL.VFS.Virtual_File);
   --  Add a new directory to be searched by gprconfig (when using
   --  Set_Automatic_Config_File) for XML files that will be used to generate
   --  the configuration file.

   --------------------
   -- Naming schemes --
   --------------------
   --  Through the naming scheme defined in a project, there are several
   --  information that can be computed: the type of source (implementation or
   --  specification), the name of the unit (in the case of Ada) or the
   --  programming language in which the file is written.

   procedure Register_Default_Language_Extension
     (Self                : in out Project_Environment;
      Language_Name       : String;
      Default_Spec_Suffix : String;
      Default_Body_Suffix : String;
      Obj_Suffix          : String := ".o");
   --  Register Default_Spec_Suffix and Default_Body_Suffix as the default
   --  extensions for the language. This procedure impacts the loading of
   --  projects (in particular the automatic search for source files in the
   --  source directories), so should be called before loading the project.
   --  Language_Name is case-insensitive.
   --  The two suffixes also become the default value returned when you
   --  query the value of the Spec_Suffix_Attribute or Impl_Suffix_Attribute
   --  for a project that does not explicit define them.
   --  The Obj_Suffix should be set to "-" or "" for languages that do not have
   --  object files (XML, txt,...) so that Library_Files does not try to
   --  match a .ali or .o file to the corresponding source. For Ada and
   --  C, the obj_suffix should be set to ".o".

   procedure Add_Language_Extension
     (Self          : in out Project_Environment;
      Language_Name : String;
      Extension     : String);
   --  Register Extension (which should include '.') as a valid extension for
   --  the language. This is used by Get_File_Info.
   --  Language_Name is case-insensitive.
   --  This procedure is meant to be called if you need more extensions than
   --  the ones provided by Register_Default_Language_Extension, or if the
   --  notion of spec/body does not apply to this specific language.

   function Registered_Extensions
     (Self          : Project_Environment;
      Language_Name : String) return GNAT.Strings.String_List;
   --  Return the list of registered extensions for Language_Name.
   --  The returned value must be freed by the user. Language_Name is
   --  case-insensitive.

   function File_From_Unit
     (Project         : Project_Type;
      Unit_Name       : String;
      Part            : Unit_Parts;
      Language        : String;
      File_Must_Exist : Boolean := True) return GNATCOLL.VFS.Filesystem_String;
   --  Return the base name for the given unit. The empty string is
   --  returned if this unit doesn't belong to the project, or if the concept
   --  of unit doesn't apply to the language. If File_Must_Exist is False, then
   --  the name of the file that would be used is returned, even if no such
   --  file currently exists in the project.
   --
   --  If Project is No_Project, the default naming scheme is used

   ------------------------
   -- Accessing projects --
   ------------------------

   function Root_Project (Self : Project_Tree'Class) return Project_Type;
   --  Returns the root project of the tree. From this project, all other
   --  projects can be reached through "with" or "limited with". This is the
   --  project that the user initially loaded through Load.

   function Project_From_Name
     (Self : Project_Tree'Class; Name : String) return Project_Type;
   --  Select a project by name.
   --  When using aggregate projects, there could be multiple projects with the
   --  same name. In this case, No_Project is returned.

   function Project_From_Path
     (Self : Project_Tree'Class;
      Path : GNATCOLL.VFS.Virtual_File) return Project_Type;
   --  Select a project by path

   type Inner_Project_Iterator is private;
   type Project_Iterator is private;
   --  Iterate over projects in a tree.
   --  There is no need to free such an iterator.
   --  Example of use:
   --       Iter : Project_Iterator := Start (Tree.Root_Project);
   --       loop
   --          Project := Current (Iter);
   --          exit when Project = No_Project;
   --          ...
   --          Next (Iter);
   --       end loop;

   function Start
     (Root_Project     : Project_Type;
      Recursive        : Boolean := True;
      Direct_Only      : Boolean := False;
      Include_Extended : Boolean := True) return Project_Iterator;
   pragma Precondition (Root_Project /= No_Project);
   --  Initialize the iterator to start at Root_Project.
   --  It will process Root_Project and all its subprojects, recursively, but
   --  without processing the same project twice.
   --
   --  The project nodes are returned sorted topologically (ie first the
   --  projects that don't depend on anything, then their parents, and so on
   --  until the root project). Extended projects are always returned before
   --  their extending project.
   --
   --  If Recursive is False, then the only project ever returned is
   --  Root_Project. This is provided only to simplify the caller's code
   --
   --  The projects extended by Root_Project, if any, are also returned if
   --  Include_Extended is true and if Direct_Only is False.
   --
   --  If Direct_Only is True and Recursive is True, then only the projects
   --  that are imported directly by Root_Project are returned.
   --
   --  Start should not be called before the view has been fully recomputed.

   function Start_Reversed
     (Root_Project     : Project_Type;
      Recursive        : Boolean := True;
      Direct_Only      : Boolean := False;
      Include_Extended : Boolean := True) return Project_Iterator;
   --  Same as above, but returns the project in the reverse order, thus:
   --     root_project, project, project_extended_by_project

   function Current (Iterator : Project_Iterator) return Project_Type;
   --  Return the project currently pointed to by the iterator.
   --  No_Project is returned if there are no more projects to process.

   procedure Next (Iterator : in out Project_Iterator);
   --  Move to the next imported project

   function Is_Limited_With (Iterator : Project_Iterator) return Boolean;
   --  Return true if the current project is imported directly and through a
   --  "limited with" clause. False otherwise.

   function Find_All_Projects_Importing
     (Project      : Project_Type;
      Include_Self : Boolean := False;
      Direct_Only  : Boolean := False) return Project_Iterator;
   --  Return the list of all the projects that import Project, either directly
   --  or indirectly. It also includes projects that extend Project, and their
   --  own extensions, so that a project and all its extensions are considered
   --  as the same project. Aggregate library projects are also included in the
   --  list, if Project or one of the projects importing it is aggregated by
   --  the aggregate library. Aggregate projects (not libraries) are not added
   --  to the list.
   --  If Project is No_Project, the resulting iterator returns all the
   --  projects in the hierarchy.
   --  If Include_Self is true, then Project will be included in the iterator
   --  (if it isn't No_Project, of course).
   --  If Direct_Only is true, then only the projects that directly import
   --  Project are returned

   function Has_Imported_Projects (Project : Project_Type) return Boolean;
   --  Return True if Project has at least one directly imported project

   procedure Project_Imports
     (Parent           : Project_Type;
      Child            : Project_Type'Class;
      Include_Extended : Boolean := False;
      Imports          : out Boolean;
      Is_Limited_With  : out Boolean);
   --  Return True if Parent imports directly Child.
   --  Is_Limited_With is set to true if the parent imports child through a
   --  "limited with" clause
   --  if Parents or Child is No_Project, True is returned.
   --  If Include_Extended is true, then True is also returned if Child is an
   --  extended project of Parent
   --  If Parent is an aggregate library and Child is one of it's aggregated
   --  projects, True is returned.

   ---------------
   -- Scenarios --
   ---------------
   --  The view of a project is potentially impacted by the value of special
   --  variables that take their value from the environment. Such variables are
   --  called scenario variables. Typically, the list of source files and
   --  switches will be different in various scenarios, although most aspects
   --  of a project can be changed that way.
   --
   --  Such a variable is typically written as follows in gpr files:
   --      type Build_Type is ("Debug", "Production");
   --      Build : Build_Type := external ("BUILD");
   --  where "BUILD" is the external_name, "Debug" and "Production" are the
   --  possible values.
   --  If however you have a variable declared as:
   --      type Build2_Type is ("Debug_Mode", "Production_Mode");
   --      Build2 : Build2_Type := external ("BUILD2") & "_Mode";
   --  then BUILD2 is not considered as a scenario variable: it is not
   --  possible in the general case to find the set of valid values for
   --  instance.

   type Scenario_Variable is private;
   type Scenario_Variable_Array is array (Natural range <>)
     of aliased Scenario_Variable;
   type Scenario_Variable_Array_Access is access Scenario_Variable_Array;

   No_Variable   : aliased constant Scenario_Variable;
   All_Scenarios : aliased constant Scenario_Variable_Array;

   function Scenario_Variables
     (Self : Project_Tree) return Scenario_Variable_Array;
   --  Return the list of scenario variables used in the whole project
   --  tree. The result is cached for efficiency
   --  Two variables are considered the same if they reference the same
   --  environment variable. The reason is that they might not have the same
   --  name internally in imported projects, however, they will always have the
   --  same value.
   --  The variables stored in the result have the value they had when the
   --  project was loaded.

   function Scenario_Variables
     (Self : Project_Tree; External_Name : String) return Scenario_Variable;
   --  Return the scenario variable associated with External_Name.
   --  If you call Value on the result, you get the current value it had when
   --  the project was loaded.
   --  If the project does not contain such a variable (for instance because
   --  you call this function before loading the project), a new variable is
   --  created.

   function External_Name (Var : Scenario_Variable) return String;
   --  Returns the name of the external variable referenced by Var.
   --  Empty string is returned if Var doesn't reference an external variable.

   function Possible_Values_Of
     (Self : Project_Tree; Var : Scenario_Variable)
      return GNAT.Strings.String_List;
   --  Return all the possible values for the variable given in parameter.
   --  The output value needs to be freed by the caller, for instance through
   --  GNATCOLL.Utils.Free

   function External_Default (Var : Scenario_Variable) return String;
   --  Return the default value for the external variable, computed for the
   --  current view of the project.

   procedure Set_Value
     (Var   : in out Scenario_Variable;
      Value : String);
   --  Change the value stored in Var.
   --  This does not affect the environment or the loaded project. In general,
   --  you would use it as:
   --      Vars : Scenario_Variable_Array := Tree.Scenario_Variables;
   --      Set_Value (Vars (Vars'First), "new_value");
   --      Set_Value (Vars (Vars'First + 1), "new_value2");
   --      Tree.Change_Environment (Vars);
   --      Tree.Recompute_View;
   --  Instead of calling Change_Environment, you could also use Vars in calls
   --  to Set_Attribute_Value for instance.
   --  This procedure does not check that the value is valid for this
   --  variable.

   procedure Change_Environment
     (Self : Project_Tree;
      Vars : Scenario_Variable_Array);
   procedure Change_Environment
     (Self        : Project_Environment;
      Name, Value : String);
   --  Change the environment value for all the variables in Vars (you do not
   --  need to have all the scenario variables from the project, only those
   --  you are interested to change). These values will be used when
   --  Recompute_View is called (which you should do).
   --  The second version (which applies to the environment) can be used before
   --  a project is loaded. It will not impact already loaded projects.

   function Value (Var : Scenario_Variable) return String;
   --  Return the values set for Var.
   --  This value is not necessary that when the project was loaded, if you
   --  have used Set_Value. However, it will be if the variable comes straight
   --  from the result of Tree.Scenario_Variables.

   function Value (Self : Project_Environment; Name : String) return String;
   --  Return the value that the variable will use when a project is loaded.
   --  This is different from Value above which reports the value as seen in
   --  the loaded project, but is only valid once a project has been loaded.

   ---------------
   -- Languages --
   ---------------

   function Languages
     (Project   : Project_Type;
      Recursive : Boolean := False) return GNAT.Strings.String_List;
   --  Return the value of the Languages attribute. You should use this
   --  function instead of Get_Attribute_Value, since it will correctly default
   --  to Ada if no language was defined by the user.
   --  If Recursive is true, then all the languages supported by Project
   --  or its imported projects will be returned.
   --  The list might be empty, if all language attributes in all projects
   --  were defined to the empty list by the user.
   --  The returned value must be freed by the user.

   function Has_Language
     (Project : Project_Type; Language : String) return Boolean;
   --  Whether the specified language is used by that project

   -----------------------
   -- Build environment --
   -----------------------

   function Get_Target (Project : Project_Type) return String;
   --  Return the target configured in the project, if any, and the empty
   --  string otherwise.

   function Get_Runtime (Project : Project_Type) return String;
   --  Return the runtime configured in the project, if any, and the empty
   --  string otherwise. This concerns only the runtime for Ada.

   --------------
   -- Switches --
   --------------

   procedure Switches
     (Project          : Project_Type;
      In_Pkg           : String;
      File             : GNATCOLL.VFS.Virtual_File;
      Language         : String;
      Value            : out GNAT.Strings.String_List_Access;
      Is_Default_Value : out Boolean);
   --  Return the switches to use for a file in a given package (gnatmake,
   --  compiler, ...).
   --  Value is the list of switches to use for that variable. The result must
   --  be freed by the caller (never null).
   --  Is_Default_Value is set to true if file-specific switches were not
   --  specified, and Value is in fact the list of default switches defined
   --  at the package level.
   --  File can be the empty string if you want to find the default switches to
   --  use for all files in the project. In that case, this procedure returns
   --  the switches to use for Language.

   ------------------------
   -- Project attributes --
   ------------------------
   --  The sections above have sometimes provided convenient accessors for the
   --  project's attributes. However, not all attributes have dedicated
   --  getters, and the subprograms in this section provide the necessary API
   --  to access the value of any attribute.
   --  To avoid typos, a set of constants is provided for all known attributes
   --  in a project.

   type Attribute_Pkg_String (<>) is private;
   type Attribute_Pkg_List (<>) is private;
   --  The name of attributes, and their type.

   function Attribute_Project
     (Project   : Project_Type;
      Attribute : Attribute_Pkg_String;
      Index     : String := "") return Project_Type;
   --  Returns the project in which the attribute was defined (which, in the
   --  case of 'renames' declarations might be different from Project).
   --  Returns No_Project if the attribute is not defined.
   --  The corresponding attribute would have been set in the returned project
   --  as:
   --      for Attribute use "value";
   --  or
   --      for Attribute (Index) use "value";

   function Attribute_Value
     (Project      : Project_Type;
      Attribute    : Attribute_Pkg_String;
      Index        : String := "";
      Default      : String := "";
      Use_Extended : Boolean := False) return String;
   --  Return the value for a string attribute.
   --  Default is returned if the attribute wasn't set by the user and
   --  has no default value.
   --  The corresponding attribute would have been set in the project as:
   --      for Attribute use "value";
   --  or
   --      for Attribute (Index) use "value";
   --
   --  If Use_Extended is true and the attribute is not defined in Project
   --  itself, then the attribute is looked up in the project extended by
   --  Project (if any).

   function Attribute_Value
     (Project      : Project_Type;
      Attribute    : Attribute_Pkg_List;
      Index        : String := "";
      Use_Extended : Boolean := False) return GNAT.Strings.String_List_Access;
   --  Same as above, but for an attribute whose value is a list.
   --
   --  The returned value is the one read in the project, or the default value
   --  if one is defined by the project manager. If none exist, this function
   --  could return null. If you need to find out whether the user has
   --  explicitly defined the attribute in his project, use Has_Attribute
   --  instead.
   --
   --  It is the responsability of the caller to free the memory.
   --  The corresponding attribute would have been set in the project as:
   --      for Attribute use ("value1", "value2");
   --  or
   --      for Attribute (Index) use ("value1", "value2");

   function Attribute_Indexes
     (Project      : Project_Type;
      Attribute    : Attribute_Pkg_String;
      Use_Extended : Boolean := False) return GNAT.Strings.String_List;
   function Attribute_Indexes
     (Project      : Project_Type;
      Attribute    : Attribute_Pkg_List;
      Use_Extended : Boolean := False) return GNAT.Strings.String_List;
   --  Return the list of indices that are in use for this attribute (ie the
   --  set of values that you can use in the call to Attribute_Value such that
   --  there is a corresponding attribute in the project file).
   --  The returned value must be freed by the user (see GNATCOLL.Utils.Free).

   function Has_Attribute
     (Project   : Project_Type;
      Attribute : Attribute_Pkg_String;
      Index     : String := "") return Boolean;
   function Has_Attribute
     (Project   : Project_Type;
      Attribute : Attribute_Pkg_List;
      Index     : String := "") return Boolean;
   --  True if the attribute was explicitly defined in the project through
   --      for Attribute (Index) use ...
   --  or  for Attribute use ...

   function Build
     (Package_Name, Attribute_Name : String) return Attribute_Pkg_String;
   function Build
     (Package_Name, Attribute_Name : String) return Attribute_Pkg_List;
   --  Build an attribute reference

   Builder_Package  : constant String;
   Compiler_Package : constant String;
   Linker_Package   : constant String;
   Binder_Package   : constant String;
   Naming_Package   : constant String;
   Ide_Package      : constant String;

   GNAT_Attribute                  : constant Attribute_Pkg_String;
   Gnatlist_Attribute              : constant Attribute_Pkg_String;
   Compiler_Command_Attribute      : constant Attribute_Pkg_String;
   Debugger_Command_Attribute      : constant Attribute_Pkg_String;
   Program_Host_Attribute          : constant Attribute_Pkg_String;
   Protocol_Attribute              : constant Attribute_Pkg_String;
   Library_Name_Attribute          : constant Attribute_Pkg_String;
   VCS_File_Check                  : constant Attribute_Pkg_String;
   VCS_Log_Check                   : constant Attribute_Pkg_String;
   VCS_Kind_Attribute              : constant Attribute_Pkg_String;
   VCS_Repository_Root             : constant Attribute_Pkg_String;
   VCS_Patch_Root                  : constant Attribute_Pkg_String;
   Global_Pragmas_Attribute        : constant Attribute_Pkg_String;
   Local_Pragmas_Attribute         : constant Attribute_Pkg_String;
   Locally_Removed_Files_Attribute : constant Attribute_Pkg_List;
   Documentation_Dir_Attribute     : constant Attribute_Pkg_String;

   Target_Attribute                : constant Attribute_Pkg_String;
   Runtime_Attribute               : constant Attribute_Pkg_String;

   --  Naming package
   Casing_Attribute                : constant Attribute_Pkg_String;
   Specification_Suffix_Attribute  : constant Attribute_Pkg_String;
   Implementation_Suffix_Attribute : constant Attribute_Pkg_String;
   Separate_Suffix_Attribute       : constant Attribute_Pkg_String;
   Spec_Suffix_Attribute           : constant Attribute_Pkg_String;
   Impl_Suffix_Attribute           : constant Attribute_Pkg_String;
   Dot_Replacement_Attribute       : constant Attribute_Pkg_String;
   Spec_Attribute                  : constant Attribute_Pkg_String;
   Body_Attribute                  : constant Attribute_Pkg_String;
   Spec_Exception_Attribute        : constant Attribute_Pkg_List;
   Impl_Exception_Attribute        : constant Attribute_Pkg_List;

   --  The following attributes should be read through specialized subprograms
   --  (Get_Languages,...)
   Source_Dirs_Attribute               : constant Attribute_Pkg_List;
   Source_Files_Attribute              : constant Attribute_Pkg_List;
   Source_List_File_Attribute          : constant Attribute_Pkg_String;
   Obj_Dir_Attribute                   : constant Attribute_Pkg_String;
   Languages_Attribute                 : constant Attribute_Pkg_List;
   Main_Attribute                      : constant Attribute_Pkg_List;
   Exec_Dir_Attribute                  : constant Attribute_Pkg_String;
   Builder_Default_Switches_Attribute  : constant Attribute_Pkg_List;
   Compiler_Default_Switches_Attribute : constant Attribute_Pkg_List;
   Linker_Default_Switches_Attribute   : constant Attribute_Pkg_List;
   Binder_Default_Switches_Attribute   : constant Attribute_Pkg_List;
   Executable_Attribute                : constant Attribute_Pkg_String;

   --  Configuration
   Compiler_Driver_Attribute           : constant Attribute_Pkg_String;

   --  GNATStack
   Stack_Switches_Attribute            : constant Attribute_Pkg_List;

   -----------------------
   -- Printing projects --
   -----------------------

   type Pretty_Printer is tagged null record;

   procedure Put (Self : in out Pretty_Printer; C : Character);
   --  Output a single character. By default, prints on stdout.

   procedure Put (Self : in out Pretty_Printer; S : String);
   procedure New_Line (Self : in out Pretty_Printer);
   --  Output a string or go to the next line. By default, these are
   --  implemented by calling Put for a single character

   procedure Put
     (Self                 : in out Pretty_Printer;
      Project              : Project_Type'Class;
      Increment            : Positive              := 3;
      Eliminate_Empty_Case_Constructions : Boolean := False);
   --  Output a project file, properly formatted.
   --  By default, all output is done on stdout, although you can change this
   --  behavior by modifying the primitive operations of the pretty printer.

   ----------------------
   -- Editing projects --
   ----------------------

   function Is_Editable (Project : Project_Type) return Boolean;
   --  Whether the project can be edited.
   --  This is not the case if there were errors loading the project, or if
   --  it contains constructs that prevent its edition (use of variables for
   --  instance).

   procedure Set_Modified (Project : Project_Type; Modified : Boolean);
   function Modified
     (Project : Project_Type; Recursive : Boolean := False) return Boolean;
   --  Return True if Project has been modified, but not saved.
   --  If Recursive is True, this function will also return True if one of the
   --  imported project has been modified.

   procedure Set_Attribute
     (Self      : Project_Type;
      Attribute : Attribute_Pkg_List;
      Values    : GNAT.Strings.String_List;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "";
      Prepend   : Boolean := False);
   procedure Set_Attribute
     (Self      : Project_Type;
      Attribute : Attribute_Pkg_String;
      Value     : String;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "";
      At_Index  : Natural := 0);
   --  Update the value of the attribute in the project.
   --  Values is the list of new values for the attribute. The caller is still
   --  responsible for freeing the memory when this call finished.
   --  Index is the index for the attribute (for instance the file name when
   --  modifying the switches).
   --  A null entry in Values is ignored.
   --  This subprogram properly handles renaming packages (i.e the project
   --  that contains the real definition of the package is modified, not
   --  necessarily Project itself).
   --  The change only occurs for the specified scenario, without affecting
   --  over scenarios. The project might need to be normalized in this case
   --  (see above). If Scenario is set to All_Scenarios, the change impacts all
   --  scenarios.
   --  If Prepend is False, these values are the only values for the
   --  variable, and they override any other value that was there before. If
   --  Prepend is True, the values in List are prepended to the current
   --  value of the attribute.
   --  You will need to call Recompute_View afterwards.
   --
   --  At_Index is used in some rare cases, and corresponds to the following
   --  construct in the project file:
   --     for Specification ("unit") use "file" at 1;

   Any_Attribute : constant String := "@@";
   --  Special value for all the subprograms that take an Attribute_Index
   --  parameter. When this is used, no matching is done on the indices.

   procedure Delete_Attribute
     (Self      : Project_Type;
      Attribute : Attribute_Pkg_String;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "");
   procedure Delete_Attribute
     (Self      : Project_Type;
      Attribute : Attribute_Pkg_List;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "");
   --  Remove all declarations for the attribute in the specified
   --  scenario. This effectively reverses to the default behavior for the
   --  attribute.
   --  If Index is Any_Attribute, then this subprogram will not try
   --  to match the index, and all declarations, whatever the index, will be
   --  removed. The default index ("") will never match attributes that do have
   --  an index.
   --  You will need to call Recompute_View afterwards.

   function Save
     (Project : Project_Type;
      Force   : Boolean := False;
      Errors  : Error_Report := null) return Boolean;
   --  Save the project on the disk.
   --  If Force is True, then the project is saved even if it isn't modified.
   --  Return whether the project has been saved (False if there was nothing to
   --  do or an error.

   function Create_Project
     (Tree     : Project_Tree'Class;
      Name     : String;
      Path     : GNATCOLL.VFS.Virtual_File) return Project_Type;
   --  Create a new empty project.
   --  This project does not replace the one currently loaded in the tree,
   --  although it becomes available for instance through calls to
   --  Project_From_Name.

   procedure Rename_And_Move
     (Self      : Project_Type;
      New_Name  : String;
      Directory : GNATCOLL.VFS.Virtual_File;
      Errors    : Error_Report := null);
   --  Rename Project to New_Name. All the nodes in the project tree that
   --  reference Project, are also updated accordingly.
   --  Also sets the directory of the project file (the project itself is not
   --  automatically saved into that directory, you need an explicit Save).
   --
   --  The paths internal to the project are not upgraded, and will remain
   --  relative paths if they were.
   --
   --  If there is already a project by that name in the project hierarchy, an
   --  error is reported through Errors.
   --
   --  You will need to call Recompute_View afterwards.

   procedure Remove_Imported_Project
     (Project          : Project_Type;
      Imported_Project : Project_Type);
   --  Remove a dependency from Project.
   --  If Imported_Project is not already a dependency, then this subprogram
   --  does nothing.
   --  You will need to call Recompute_View afterwards.

   type Import_Project_Error is (Success,
                                 Project_Already_Exists,
                                 Imported_Project_Not_Found,
                                 Dependency_On_Self,
                                 Dependency_Already_Exists,
                                 Circular_Dependency
                                 );

   function Add_Imported_Project
     (Tree                      : Project_Tree;
      Project                   : Project_Type'Class;
      Imported_Project_Location : GNATCOLL.VFS.Virtual_File;
      Packages_To_Check         : GNAT.Strings.String_List_Access := No_Packs;
      Errors                    : Error_Report := null;
      Use_Relative_Path         : Boolean := True;
      Use_Base_Name             : Boolean := False;
      Limited_With              : Boolean := False)
      return Import_Project_Error;
   --  Add a new with_statement for Imported_Project.
   --  Errors while parsing the project file are sent to Report_Errors.
   --  True is returned if the project was modified with success
   --  If Use_Base_Name is true then only the base name of the project is used
   --  in the with statement. Otherwise, if Use_Relative_Path is True, then a
   --  relative path is used in the with statement, otherwise an absolute path
   --  is used.
   --  You will need to call Recompute_View afterwards.
   --
   --  Doesn't work if Tree.Root_Project is an aggregate project.

   function Add_Imported_Project
     (Project           : Project_Type;
      Imported_Project  : Project_Type;
      Errors            : Error_Report := null;
      Use_Relative_Path : Boolean := True;
      Use_Base_Name     : Boolean := False;
      Limited_With      : Boolean := False) return Import_Project_Error;
   --  Same as above, but the project is already in memory

   function Register_New_Attribute
     (Name    : String;
      Pkg     : String;
      Is_List : Boolean := False;
      Indexed : Boolean := False;
      Case_Sensitive_Index : Boolean := False) return String;
   --  Register a new attribute that will be allowed in projects.
   --  This prevents error messages when loading the project.
   --  Attributes can only be added to packages, not at the toplevel of a
   --  project.
   --  Returns a non-empty string if there is an error creating the attribute

   function Rename_Path
     (Self               : Project_Type;
      Old_Path           : GNATCOLL.VFS.Virtual_File;
      New_Path           : GNATCOLL.VFS.Virtual_File;
      Use_Relative_Paths : Boolean) return Boolean;
   --  Replace all instances of Old_Path with New_Path.
   --  This returns True if all occurrences were successfully replaced, False
   --  otherwise.
   --  You will need to call Recompute_View afterwards.

   procedure Set_Extended_Project
     (Self               : GNATCOLL.Projects.Project_Type;
      Extended           : GNATCOLL.Projects.Project_Type;
      Extend_All         : Boolean := False;
      Use_Relative_Paths : Boolean := False);
   --  Set the project that Project extends. If Extend_All is True, then this
   --  is an "extend all" project.
   --  You will need to call Recompute_View afterwards.

   --------------------------------
   -- Editing scenario variables --
   --------------------------------

   procedure Delete_Scenario_Variable
     (Tree                     : Project_Tree'Class;
      External_Name            : String;
      Keep_Choice              : String;
      Delete_Direct_References : Boolean := True);
   --  Remove all scenario variables that reference External_Name.
   --  All the case constructions where this variable occur are replaced by
   --  the case item corresponding to Keep_Choice.
   --  If Delete_Direct_References is True, then all direct references (ie
   --  external() statements in the project file) to External_Name are also
   --  removed, in addition to the scenario variables that reference it.
   --
   --  You will need to call Recompute_View afterwards.

   function Create_Scenario_Variable
     (Project       : Project_Type;
      Name          : String;
      Type_Name     : String;
      External_Name : String) return Scenario_Variable;
   --  Create a new typed environment variable, referencing External_Name, and
   --  whose type is Type_Name. The declaration for the type is automatically
   --  created.

   procedure Change_External_Name
     (Tree     : Project_Tree'Class;
      Variable : in out Scenario_Variable;
      New_Name : String);
   --  Change the name of the environment variable associated with Variable.

   procedure Set_Default_Value
     (Tree          : Project_Tree'Class;
      External_Name : String;
      Default       : String);
   --  Change the default value for all the scenario variables based on
   --  External_Name.

   procedure Rename_Value
     (Tree          : Project_Tree'Class;
      External_Name : String;
      Old_Value     : String;
      New_Value     : String);
   --  Rename one of the choices in the list of possible values for the
   --  scenario variables asociated with External_Name. This also changes
   --  the default value for external references.

   procedure Remove_Value
     (Tree          : Project_Tree'Class;
      External_Name : String;
      Value         : String);
   --  Remove Value_Name from the list of possible values for the scenario
   --  variables that refer to External_Name. If this is the last possible
   --  value, then the result is the same as calling Delete_Scenario_Variable.

   procedure Add_Values
     (Tree     : Project_Tree'Class;
      Variable : Scenario_Variable;
      Values   : GNAT.Strings.String_List);
   --  Add some values to the list of possible values for Variable.
   --  The caller needs to free Values on return.

   --------------
   -- Internal --
   --------------

   function Start
     (Root_Project     : Project_Type;
      Recursive        : Boolean := True;
      Direct_Only      : Boolean := False;
      Include_Extended : Boolean := True) return Inner_Project_Iterator;
   --  Internal version of Start

   function Start_Reversed
     (Root_Project     : Project_Type;
      Recursive        : Boolean := True;
      Direct_Only      : Boolean := False;
      Include_Extended : Boolean := True) return Inner_Project_Iterator;
   --  Internal Version of Start_Reversed

   procedure Next (Iterator : in out Inner_Project_Iterator);
   --  Internal version of Next

   function Current (Iterator : Inner_Project_Iterator) return Project_Type;
   --  Internal version of Current

   function Find_All_Projects_Importing
     (Project      : Project_Type;
      Root_Project : Project_Type;
      Include_Self : Boolean := False;
      Direct_Only  : Boolean := False) return Inner_Project_Iterator;
   --  Inner version of Find_All_Projects_Importing

   function Is_Limited_With (Iterator : Inner_Project_Iterator) return Boolean;
   --  Inner version of Is_Limited_With

private

   All_Packs : constant GNAT.Strings.String_List_Access := null;

   No_Strings : aliased GNAT.Strings.String_List := (1 .. 0 => null);
   No_Packs   : constant GNAT.Strings.String_List_Access
                           := No_Strings'Access;

   Project_File_Extension : constant GNATCOLL.VFS.Filesystem_String :=
     GNATCOLL.VFS."+" (Prj.Project_File_Extension);
   --  The standard extension for a project file (".gpr")

   type File_Info is new File_Info_Abstract with record
      File         : GNATCOLL.VFS.Virtual_File;
      Project      : Project_Type;
      Root_Project : Project_Type;
      Part         : Unit_Parts    := Unit_Separate;
      Name         : Namet.Name_Id := Namet.No_Name;   --  Unit name
      Lang         : Namet.Name_Id := Namet.No_Name;
   end record;

   package Extensions_Languages is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,   --  file extension
      Element_Type    => Namet.Name_Id,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Namet."=");
   --  maps extensions with a language

   type Naming_Scheme_Record;
   type Naming_Scheme_Access is access Naming_Scheme_Record;
   type Naming_Scheme_Record is record
      Language            : GNAT.Strings.String_Access;
      Default_Spec_Suffix : GNAT.Strings.String_Access;
      Default_Body_Suffix : GNAT.Strings.String_Access;
      Obj_Suffix          : GNAT.Strings.String_Access;
      Next                : Naming_Scheme_Access;
   end record;

   type Project_Environment is tagged record
      Env : Prj.Tree.Environment;

      Autoconf    : Boolean := False;
      Config_File : GNATCOLL.VFS.Virtual_File;
      --  Name of the .cgpr file to parse for the project.

      Forced_Target : GNAT.Strings.String_Access;
      Forced_Runtime : GNAT.Strings.String_Access;
      --  force specific values for runtime and target

      Gnatls : GNAT.Strings.String_Access;
      --  The gnatls that was run to set the predefined paths (or unset if the
      --  paths were set manually).

      Predefined_Object_Path : GNATCOLL.VFS.File_Array_Access;
      --  Predefined object path for the runtime library

      Predefined_Source_Path : GNATCOLL.VFS.File_Array_Access;
      --  Predefined source paths for the runtime library

      Predefined_Project_Path : GNATCOLL.VFS.File_Array_Access;
      --  Predefined project path.
      --  prj-ext.ads does not expect an empty path, ever

      Predefined_Source_Files : GNATCOLL.VFS.File_Array_Access;
      --  The list of source files in Predefined_Source_Path

      Default_Gnatls : GNAT.Strings.String_Access := new String'("gnatls");
      --  The default gnatls command to run.

      Xrefs_Subdir : GNAT.Strings.String_Access;
      --  Object dirs subdirectory containing the cross-refs

      Trusted_Mode : Boolean := True;
      --  Whether we are in trusted mode when recomputing the project view

      Extensions : Extensions_Languages.Map;
      --  The additional extensions registered for each language

      Naming_Schemes : Naming_Scheme_Access;
      --  The list of default naming schemes for the languages known to GPS
   end record;

   type Name_Id_Array        is array (Positive range <>) of Namet.Name_Id;
   type Name_Id_Array_Access is access Name_Id_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Name_Id_Array, Name_Id_Array_Access);
   --  Still needed for some routines like Get_All_Possible_Values

   type Path_Name_Id_Array is array (Positive range <>)
     of Namet.Path_Name_Type;
   type Path_Name_Id_Array_Access is access Path_Name_Id_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Path_Name_Id_Array, Path_Name_Id_Array_Access);

   type Project_Tree_Data;
   type Project_Tree_Data_Access is access Project_Tree_Data;

   type Project_Data is tagged record
      Refcount : Integer := 1;

      Node : Prj.Tree.Project_Node_Id;
      View : Prj.Project_Id;

      Imported_Projects  : Path_Name_Id_Array_Access;
      Importing_Projects : Path_Name_Id_Array_Access;
      --  Sorted list of imported projects (Cache for Project_Iterator).
      --  Importing_Project always contains the project itself in last
      --  position.

      Non_Recursive_Include_Path : GNATCOLL.VFS.File_Array_Access;
      --  The include path for this project

      Tree : Project_Tree_Data_Access;
      --  Needed so that we can return other projects like imported projects

      Local_Tree : Prj.Project_Tree_Ref := null;
      --  If given project is an aggregated one this allows to access
      --  corresponding project tree data.

      Local_Node_Tree : Prj.Tree.Project_Node_Tree_Ref := null;
      --  If given project is an aggregated one this allows to access
      --  corresponding node tree data.

      View_Is_Complete : Boolean := True;
      --  True if the view for the project was correctly computed.
      --  Set to True by default, so that a project just created with
      --  Create_Project can be immediately edited.

      Files : GNATCOLL.VFS.File_Array_Access;
      --  The list of source files for this project

      Uses_Variables : Boolean := False;
      --  If the project uses variables ("foo := .."), then it cannot be
      --  edited graphically, since GPS would break it.

      Normalized : Boolean := False;
      --  True if the project has been normalized

      Modified : Boolean := False;
      --  True if the project has been modified by the user, and not saved
      --  yet.
   end record;

   type Project_Type is new Ada.Finalization.Controlled with record
      Data      : Project_Data_Access;
      --  This is an access type for several reasons:
      --    - Parameters do not have to be of type "access Project_Type", just
      --      Project_Type, which is lighter to write. At the same time,
      --      modifying the data of the project will impact all instances that
      --      reference the same project tree.
      --    - Since the user doesn't know this is an access type, he can not
      --      attempt to free the data. Memory is fully controlled by this type
      --      (and the projects registry).
   end record;

   overriding procedure Adjust (Self : in out Project_Type);
   overriding procedure Finalize (Self : in out Project_Type);

   function Tree_View
     (P : Project_Type'Class) return Prj.Project_Tree_Ref;
   function Tree_Tree
     (P : Project_Type'Class) return Prj.Tree.Project_Node_Tree_Ref;
   pragma Inline (Tree_View, Tree_Tree);
   --  Access to the project tree

   type Project_Tree is tagged record
      Data    : Project_Tree_Data_Access;
   end record;

   type Scenario_Variable is record
      Name        : Namet.Name_Id;
      Default     : Namet.Name_Id;
      String_Type : Prj.Tree.Project_Node_Id;
      Value       : Namet.Name_Id;
   end record;

   No_Variable   : aliased constant Scenario_Variable :=
     (Namet.No_Name, Namet.No_Name, Prj.Tree.Empty_Node,
      Namet.No_Name);

   All_Scenarios : aliased constant Scenario_Variable_Array (1 .. 0) :=
                   (others => No_Variable);

   type Inner_Project_Iterator is record
      Root      : Project_Type;
      Current   : Integer;

      Reversed  : Boolean;

      Importing : Boolean := False;
      --  True if we are looking for importing projects instead of imported
      --  projects.

      Include_Extended : Boolean := True;
      --  True if we should also return extended projects

      Direct_Only : Boolean := False;
      --  Relevant only when listing projects importing Root
   end record;

   package Project_Lists is new
     Ada.Containers.Vectors (Positive, Project_Type);
   use Project_Lists;

   type Project_Iterator is record
      Root         : Project_Type;

      Importing    : Boolean := False;
      --  True if we are looking for importing projects instead of imported
      --  projects. Only used by Is_Limited_With.

      Project_List : Project_Lists.Vector  := Project_Lists.Empty_Vector;
      Project_Idx  : Natural               := Project_Lists.No_Index;
   end record;

   type Attribute_Pkg_String is new String;
   type Attribute_Pkg_List is new String;

   Builder_Package  : constant String := "builder";
   Compiler_Package : constant String := "compiler";
   Linker_Package   : constant String := "linker";
   Binder_Package   : constant String := "binder";
   Naming_Package   : constant String := "naming";
   Ide_Package      : constant String := "ide";

   Source_Dirs_Attribute      : constant Attribute_Pkg_List := "source_dirs";
   Source_Files_Attribute     : constant Attribute_Pkg_List := "source_files";
   Source_List_File_Attribute : constant Attribute_Pkg_String :=
     "source_list_file";
   Locally_Removed_Files_Attribute : constant Attribute_Pkg_List :=
                                       "locally_removed_files";
   GNAT_Attribute            : constant Attribute_Pkg_String := "ide#gnat";
   Gnatlist_Attribute        : constant Attribute_Pkg_String := "ide#gnatlist";
   Compiler_Command_Attribute : constant Attribute_Pkg_String :=
                                  "ide#compiler_command";
   Debugger_Command_Attribute : constant Attribute_Pkg_String :=
                                  "ide#debugger_command";
   Remote_Host_Attribute     : constant Attribute_Pkg_String :=
                                  "ide#remote_host";
   Program_Host_Attribute    : constant Attribute_Pkg_String :=
                                  "ide#program_host";
   Protocol_Attribute        : constant Attribute_Pkg_String :=
                                  "ide#communication_protocol";
   Main_Attribute            : constant Attribute_Pkg_List := "main";
   Library_Name_Attribute    : constant Attribute_Pkg_String := "library_name";
   VCS_File_Check            : constant Attribute_Pkg_String :=
                                  "ide#vcs_file_check";
   VCS_Log_Check             : constant Attribute_Pkg_String :=
                                  "ide#vcs_log_check";
   Obj_Dir_Attribute         : constant Attribute_Pkg_String := "object_dir";
   VCS_Kind_Attribute        : constant Attribute_Pkg_String := "ide#vcs_kind";
   VCS_Repository_Root       : constant Attribute_Pkg_String :=
                                 "ide#vcs_repository_root";
   VCS_Patch_Root            : constant Attribute_Pkg_String :=
                                 "ide#vcs_patch_root";
   Documentation_Dir_Attribute : constant Attribute_Pkg_String :=
                                 "ide#documentation_dir";

   Target_Attribute  : constant Attribute_Pkg_String := "target";
   Runtime_Attribute : constant Attribute_Pkg_String := "runtime";

   Global_Pragmas_Attribute  : constant Attribute_Pkg_String :=
                                 "builder#global_configuration_pragmas";
   Local_Pragmas_Attribute   : constant Attribute_Pkg_String :=
                                 "compiler#local_configuration_pragmas";
   Builder_Default_Switches_Attribute : constant Attribute_Pkg_List :=
                                          "builder#default_switches";
   Compiler_Default_Switches_Attribute : constant Attribute_Pkg_List :=
                                           "compiler#default_switches";
   Linker_Default_Switches_Attribute : constant Attribute_Pkg_List :=
                                         "linker#default_switches";
   Binder_Default_Switches_Attribute   : constant Attribute_Pkg_List :=
                                           "binder#default_switches";
   Executable_Attribute       : constant Attribute_Pkg_String :=
                                  "builder#executable";
   Casing_Attribute           : constant Attribute_Pkg_String :=
                                  "naming#casing";
   Specification_Suffix_Attribute : constant Attribute_Pkg_String :=
                                      "naming#specification_suffix";
   Implementation_Suffix_Attribute : constant Attribute_Pkg_String :=
                                       "naming#implementation_suffix";
   Separate_Suffix_Attribute  : constant Attribute_Pkg_String :=
                                  "naming#separate_suffix";
   Spec_Suffix_Attribute      : constant Attribute_Pkg_String :=
                                  "naming#spec_suffix";
   Impl_Suffix_Attribute      : constant Attribute_Pkg_String :=
                                  "naming#body_suffix";
   Dot_Replacement_Attribute  : constant Attribute_Pkg_String :=
                                  "naming#dot_replacement";
   Spec_Attribute    : constant Attribute_Pkg_String := "naming#spec";
   Body_Attribute    : constant Attribute_Pkg_String := "naming#body";

   --  Configuration
   Compiler_Driver_Attribute  : constant Attribute_Pkg_String :=
                                  "compiler#driver";

   --  GNATStack
   Stack_Switches_Attribute   : constant Attribute_Pkg_List :=
                                  "stack#switches";

   --  For backward compatiblity
   Old_Specification_Attribute  : constant Attribute_Pkg_String :=
                                    "naming#specification";
   Old_Implementation_Attribute : constant Attribute_Pkg_String :=
                                    "naming#implementation";

   Spec_Exception_Attribute   : constant Attribute_Pkg_List :=
                                  "naming#specification_exceptions";
   Impl_Exception_Attribute   : constant Attribute_Pkg_List :=
                                  "naming#implementation_exceptions";

   --  The following attributes should be read through specialized subprograms
   --  (Get_Languages,...)
   Languages_Attribute        : constant Attribute_Pkg_List := "languages";
   Exec_Dir_Attribute         : constant Attribute_Pkg_String := "exec_dir";

   No_Project : aliased constant Project_Type :=
     (Ada.Finalization.Controlled with Data => null);

   function Get_View (Project : Project_Type'Class) return Prj.Project_Id;
   function Node
     (Project : Project_Type'Class) return Prj.Tree.Project_Node_Id;
   function Tree
     (Data : Project_Tree_Data_Access) return Prj.Tree.Project_Node_Tree_Ref;
   pragma Inline (Node, Tree, Get_View);
   --  Needed for the support packages for the edition of project files.

   function Project_From_Name
     (Tree : Project_Tree_Data_Access;
      Name : Namet.Name_Id) return Project_Type'Class;
   --  Internal version of Project_From_Name

   function Project_From_Path
     (Tree    : Project_Tree_Data_Access;
      Path_Id : Namet.Path_Name_Type) return Project_Type'Class;
   --  Internal version of Project_From_Path

   function Scenario_Variables
     (Tree : Project_Tree_Data_Access) return Scenario_Variable_Array;
   pragma Inline (Scenario_Variables);
   --  Internal version of Scenario_Variables

   procedure Reset_All_Caches (Tree : Project_Tree_Data_Access);
   --  Reset all the caches for imported/importing projects
   --  for the whole project hierarchy

   pragma Inline (Name, Project_Path, Has_Attribute);
end GNATCOLL.Projects;
