-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2006                      --
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

with Ada.Containers;
with GNAT.Strings;
with GNAT.OS_Lib;
with Prj.Tree;
with String_List_Utils;
with Types;
with VFS;
with Glib;

package Projects is

   type Project_Type is private;
   No_Project : constant Project_Type;
   --  This type groups both the project tree and its view (i.e the tree
   --  processed according to the current environment variables)

   Project_File_Extension : String renames Prj.Project_File_Extension;

   type Abstract_Registry is abstract tagged null record;
   type Abstract_Registry_Access is access all Abstract_Registry'Class;

   type Name_Id_Array      is array (Positive range <>) of Types.Name_Id;
   type Project_Type_Array is array (Natural range <>) of Project_Type;

   type Error_Report is access procedure (Msg : String);

   function "=" (Prj1, Prj2 : Project_Type) return Boolean;
   --  Return true if Prj1 and Prj2 reference the same project

   ---------------
   -- Languages --
   ---------------
   --  Strings used for the various languages supported by GPS by default.
   --  Other languages can be supported, these strings are for convenience
   --  only.
   --  ??? They should eventually be moved to each language-specific module

   Ada_String : constant String := "ada";  --  See also Snames.Name_Ada
   C_String   : constant String := "c";    --  See also Snames.Name_C
   Cpp_String : constant String := "c++";  --  See also Name_C_Plus_Plus

   Name_C_Plus_Plus : Types.Name_Id;
   --  The equivalent of Cpp_String. You should never use Name_CPP, which
   --  contains "cpp" instead of the expect "c++" in projects.

   All_Languages : constant Name_Id_Array;

   ----------
   -- Misc --
   ----------

   function Get_String (Id : Types.Name_Id) return String;
   --  Return the string in Name
   --  Same as Namet.Get_Name_String, but return "" in case of
   --  failure, instead of raising Assert_Failure.

   function Get_Registry (Project : Project_Type)
      return Abstract_Registry'Class;
   --  Return the registry that Project belongs to

   -----------
   -- Files --
   -----------
   --  The following subprograms are in used in conjunction with vfs.ads, so
   --  that the latter remains independent of GPS as much as possible.

   function Create
     (Base_Name       : Glib.UTF8_String;
      Project         : Projects.Project_Type;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True) return VFS.Virtual_File;
   --  Create a new instance of the file.

   -------------------
   -- Project files --
   -------------------

   function Save_Project
     (Project      : Project_Type;
      Report_Error : Error_Report := null) return Boolean;
   --  Save the project to the corresponding file.
   --  If Recursive is True, all the imported projects are saved as well.
   --  Return whether the project was modified and has been saved,
   --  Return False if the project either didn't need to be saved or couldn't
   --  be saved.

   function Project_Name (Project : Project_Type) return String;
   function Project_Name (Project : Project_Type) return Types.Name_Id;
   --  Return the name of the project.
   --  "default" is returned if the project is the empty project or No_Project.

   function Project_Name_Hash (Project : Project_Type)
                               return Ada.Containers.Hash_Type;
   --  Return a Hash_Type computed from the full name of the given Project.
   --  Could be used to instantiate an Ada 2005 container that uses a
   --  Project_type as key and requires a hash function.

   function Project_Path
     (Project : Project_Type;
      Host    : String := "") return VFS.Virtual_File;
   --  Return the full path name to the project file
   --  If Host is given, the path will be the one on the specified host.

   function Project_Directory
     (Project : Project_Type;
      Host    : String := "") return VFS.Virtual_File;
   --  Return the directory that contains the project file.
   --  No_File is returned if the project is No_Project.

   function Parent_Project (Project : Project_Type) return Project_Type;
   --  Return the project that Project is extending, or No_Project if there are
   --  none.

   function Extending_Project
     (Project : Project_Type; Recurse : Boolean := False) return Project_Type;
   --  Return the project that extends Project, or No_Project if Project is not
   --  extended within the hierarchy and Recurse is False.
   --  If Recurse is True, then the lowest possible project is returned, even
   --  if it is Project itself. This is useful when looking for specific source
   --  files.

   procedure Set_Project_Modified (Project : Project_Type; Modified : Boolean);
   --  Set the modified flag for Project.
   --  This is usually handled automatically by the other functions in this
   --  package, use it only to override the default behavior (e.g. mark the
   --  default project as unmodified).

   -----------------
   -- Directories --
   -----------------

   function Source_Dirs (Project : Project_Type) return Name_Id_Array;
   --  Return the list of source directories for Prj, as an array. The
   --  directories are absolute directories.

   function Source_Dirs
     (Project   : Project_Type;
      Recursive : Boolean;
      Has_VCS   : Boolean := False) return GNAT.Strings.String_List_Access;
   --  Return the list of source directories. The directories are normalized.
   --  If Recursive is True, directories from imported projects will also be
   --  returned. If Has_VCS is set to True only directories for projects having
   --  a VCS defined are returned.
   --  Note that duplicate directories might be returned when directories are
   --  shared by multiple projects in the same tree.
   --  Returned array must be freed by the user.

   function Include_Path
     (Project : Project_Type; Recursive : Boolean) return String;
   --  Return the source path for this project. If Recursive is True, it also
   --  includes the source path for all imported projects.
   --  The directories are not normalized.

   function Object_Path
     (Project             : Project_Type;
      Recursive           : Boolean;
      Including_Libraries : Boolean := True) return String;
   --  Return the object path for this project. If Recursive is True, it also
   --  includes the object path for all imported projects.
   --  The empty string is returned if the project doesn't have any object
   --  directory (i.e. the user explicitely set it to the empty string).

   function Directory_Contains_Files
     (Project   : Project_Type;
      Directory : String) return Boolean;
   --  True if Directory contains at least one source file of Project

   ------------------
   -- Source files --
   ------------------

   function Get_Source_Files
     (Project   : Project_Type;
      Recursive : Boolean) return VFS.File_Array_Access;
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
   --  when compiling from Project ("gnatmake -PProject"). The basenames of the
   --  returned files are always unique, since if the project has two or
   --  more duplicate files, only the first one found in the source path
   --  is returned.

   function Direct_Sources_Count (Project : Project_Type) return Natural;
   --  Return the number of direct source files for Project

   function Get_Source_File
     (Project : Project_Type; Index : Positive) return VFS.Virtual_File;
   --  Return the Index-th source file in the project, or VFS.No_File if there
   --  is no more direct source.

   --------------------
   -- Naming schemes --
   --------------------

   type Unit_Part is (Unit_Body, Unit_Spec, Unit_Separate);
   --  A unit is usally composed of two parts: the spec and the body.
   --    - Unit_Spec represents package/subprogram/generic declarations
   --    - Unit_Body represents package/subprogram/generic bodies and subunits.

   function Get_Unit_Part_From_Filename
     (Project : Project_Type; Filename : VFS.Virtual_File) return Unit_Part;
   --  Return the type of File.
   --  This function doesn't assume any knowledge of the language, and will
   --  check in all the languages known to the project.
   --  Unit_Separate is returned if the file is neither a spec nor a body.

   function Get_Unit_Name_From_Filename
     (Project : Project_Type; Filename : VFS.Virtual_File) return String;
   --  Return the unit name associated with a given file name, or the empty
   --  string if the name of the unit couldn't be computed.

   procedure Get_Unit_Part_And_Name_From_Filename
     (Filename  : Glib.UTF8_String;
      Project   : Project_Type;
      Part      : out Unit_Part;
      Unit_Name : out Types.Name_Id;
      Lang      : out Types.Name_Id);
   --  Return the unit name and unit part for Filename.
   --  This procedure doesn't fully handle krunched file name.
   --  Project can be No_Project, in which case the default naming scheme is
   --  used.

   function Delete_File_Suffix
     (Filename : String; Project : Project_Type) return Natural;
   --  Return the last index in Filename before the beginning of the file
   --  suffix. Suffixes are searched independently from the language.
   --  If not matching suffix is found in project, the returned value will
   --  simply be Filename'Last.

   function Other_File_Base_Name
     (Project : Project_Type; Source_Filename : VFS.Virtual_File)
      return String;
   --  Return the base name of the spec or body for Source_Filename.
   --  If the other file is not found in the project, then the other file base
   --  name, according to the GNAT naming scheme, is returned.
   --  Otherwise the other file base name is returned as is.

   function Get_Filename_From_Unit
     (Project                  : Project_Type;
      Unit_Name                : String;
      Part                     : Unit_Part;
      Check_Predefined_Library : Boolean := False;
      File_Must_Exist          : Boolean := True;
      Language                 : String) return String;
   --  Return the base name for the given unit. The empty string is
   --  returned if this unit doesn't belong to the project, or if the concept
   --  of unit doesn't apply to the language. If File_Must_Exist is False, then
   --  the name of the file that would be used is returned, even if no such
   --  file currently exists in the project.
   --  If Check_Predefined_Library is True, the default GNAT naming scheme is
   --  used (for runtime files).
   --  If Check_Predefined_Library is False, the file must by default be
   --  found in the project hierarchy. If File_Must_Exist is False, then
   --  the Project's naming scheme is used to compute the result.
   --  Project must never be No_Project.

   ----------------
   -- Attributes --
   ----------------

   Builder_Package            : constant String := "builder";
   Compiler_Package           : constant String := "compiler";
   Linker_Package             : constant String := "linker";
   Binder_Package             : constant String := "binder";
   Naming_Package             : constant String := "naming";
   Ide_Package                : constant String := "ide";

   type Attribute_Pkg (<>) is private;

   function Build (Package_Name, Attribute_Name : String) return Attribute_Pkg;
   --  Build an attribute reference

   Source_Dirs_Attribute      : constant Attribute_Pkg;
   Source_Files_Attribute     : constant Attribute_Pkg;
   Gnatlist_Attribute         : constant Attribute_Pkg;
   Compiler_Command_Attribute : constant Attribute_Pkg;
   Debugger_Command_Attribute : constant Attribute_Pkg;
   Program_Host_Attribute     : constant Attribute_Pkg;
   Protocol_Attribute         : constant Attribute_Pkg;
   Main_Attribute             : constant Attribute_Pkg;
   Vcs_File_Check             : constant Attribute_Pkg;
   Vcs_Log_Check              : constant Attribute_Pkg;
   Obj_Dir_Attribute          : constant Attribute_Pkg;
   Vcs_Kind_Attribute         : constant Attribute_Pkg;
   Vcs_Repository_Root        : constant Attribute_Pkg;
   Global_Pragmas_Attribute   : constant Attribute_Pkg;
   Local_Pragmas_Attribute    : constant Attribute_Pkg;
   Locally_Removed_Files_Attribute : constant Attribute_Pkg;

   --  Naming package
   Casing_Attribute                : constant Attribute_Pkg;
   Specification_Suffix_Attribute  : constant Attribute_Pkg;
   Implementation_Suffix_Attribute : constant Attribute_Pkg;
   Separate_Suffix_Attribute       : constant Attribute_Pkg;
   Spec_Suffix_Attribute           : constant Attribute_Pkg;
   Impl_Suffix_Attribute           : constant Attribute_Pkg;
   Dot_Replacement_Attribute       : constant Attribute_Pkg;
   Old_Specification_Attribute     : constant Attribute_Pkg;
   Old_Implementation_Attribute    : constant Attribute_Pkg;
   Specification_Attribute         : constant Attribute_Pkg;
   Implementation_Attribute        : constant Attribute_Pkg;
   Spec_Exception_Attribute        : constant Attribute_Pkg;
   Impl_Exception_Attribute        : constant Attribute_Pkg;

   --  The following attributes should be read through specialized subprograms
   --  (Get_Languages,...)
   Languages_Attribute                 : constant Attribute_Pkg;
   Exec_Dir_Attribute                  : constant Attribute_Pkg;
   Builder_Default_Switches_Attribute  : constant Attribute_Pkg;
   Compiler_Default_Switches_Attribute : constant Attribute_Pkg;
   Linker_Default_Switches_Attribute   : constant Attribute_Pkg;
   Binder_Default_Switches_Attribute   : constant Attribute_Pkg;
   Executable_Attribute                : constant Attribute_Pkg;

   type Associative_Array_Element is record
      Index : Types.Name_Id;
      Value : Prj.Variable_Value;
   end record;
   type Associative_Array is array (Natural range <>)
     of Associative_Array_Element;

   function Get_Attribute_Value
     (Project   : Project_Type;
      Attribute : Attribute_Pkg;
      Default   : String := "";
      Index     : String := "") return String;
   --  Return the value for a single-string attribute.
   --  Default is returned if the attribute wasn't set by the user and
   --  has no default value.
   --  Attribute_Pkg should be of the form:  "Package#Attribute" or
   --  "Attribute" if there is no package.

   function Get_Attribute_Value
     (Project   : Project_Type;
      Attribute : Attribute_Pkg;
      Index     : String := "") return GNAT.OS_Lib.Argument_List;
   --  Same as above, but for an attribute whose value is a list. An empty
   --  array is returned if the attribute isn't defined.
   --  It is the responsability of the caller to free the memory.

   function Get_Attribute_Value
     (Project   : Project_Type;
      Attribute : Attribute_Pkg) return Associative_Array;
   --  Same as above when the attribute is an associative array

   function Get_Languages
     (Project : Project_Type; Recursive : Boolean := False)
      return GNAT.OS_Lib.Argument_List;
   --  Return the value of the Languages attribute. You should use this
   --  function instead of Get_Attribute_Value, since it will correctly default
   --  to Ada if no language was defined by the user.
   --  The returned value must be freed by the user.
   --  If Recursive is true, then all the languages supported by Project
   --  or its imported projects will be returned.
   --  The list might be empty, if all language attributes in all projects
   --  were defined to the empty list by the user.

   function Get_Executable_Name
     (Project : Project_Type; File : String) return String;
   --  Return the name of the executable, either read from the project or
   --  computed from File
   --  If Project is No_Project, the default executable name for File is
   --  returned.

   function Is_Main_File
     (Project : Project_Type; File : String) return Boolean;
   --  Return True if File is one of the main files of Project

   function Executables_Directory (Project : Project_Type) return String;
   --  Return the directory that contains the executables generated for the
   --  main programs in Project. This is either Exec_Dir or Object_Dir.
   --  The returned string always ends with a directory separator.

   procedure Get_Switches
     (Project          : Project_Type;
      In_Pkg           : String;
      File             : VFS.Virtual_File;
      Language         : Types.Name_Id;
      Value            : out Prj.Variable_Value;
      Is_Default_Value : out Boolean);
   --  Return the switches to use for a file in a given package (gnatmake,
   --  compiler, ...).
   --  Value is the list of switches to use for that variable.
   --  Is_Default_Value is set to true if file-specific switches were not
   --  specified, and Value is in fact the list of default switches defined
   --  at the package level.
   --  File can be the empty string if you want to find the default switches to
   --  use for all files in the project. In that case, this procedure returns
   --  the switches to use for Language.
   --
   --  Use GPS.Kernel.Project.Get_Switches instead, which provides support
   --  for initial switches as specified by the user.

   -----------------------
   -- Imported projects --
   -----------------------

   type Imported_Project_Iterator is private;

   function Start
     (Root_Project     : Project_Type;
      Recursive        : Boolean := True;
      Direct_Only      : Boolean := False;
      Include_Extended : Boolean := True)
      return Imported_Project_Iterator;
   --  Initialize the iterator to start at Root_Project.
   --  It will process Root_Project and all its subprojects, recursively, but
   --  without processing the same project twice.
   --  The project nodes are returned sorted topologically (ie first the
   --  projects that don't depend on anything, then their parents, and so on
   --  until the root project).
   --
   --  If Recursive is False, then the only project ever returned is
   --  Root_Project. This is provided only to simplify the caller's code
   --
   --  In all cases, Root_Project itself is returned first by the iterator. The
   --  project extended by Root_Project, if any, is also returned if
   --  Include_Extended is true or if Direct_Only is False.
   --
   --  If Direct_Only is True and Recursive is True, then only the projects
   --  that are imported directly by Root_Project are returned.

   function Current
     (Iterator : Imported_Project_Iterator) return Project_Type;
   --  Return the project currently pointed to by the iterator.
   --  No_Project is returned if there are no more projects to process.

   procedure Next (Iterator : in out Imported_Project_Iterator);
   --  Move to the next imported project.

   function Is_Limited_With
     (Iterator : Imported_Project_Iterator) return Boolean;
   --  Return true if the current project is imported directly and through a
   --  "limited with" clause. False otherwise.

   function Find_All_Projects_Importing
     (Project      : Project_Type;
      Include_Self : Boolean := False;
      Direct_Only  : Boolean := False) return Imported_Project_Iterator;
   --  Return the list of all the projects that import Project, either directly
   --  or indirectly. It also includes projects that extend Project, and their
   --  own extensions, so that a project and all its extensions are considered
   --  as the same project.
   --  If Project is No_Project, the resulting array contains all the project
   --  in the hierarchy.
   --  If Include_Self is true, then Project will be included in the returned
   --  array (if it isn't No_Project, of course).
   --  If Direct_Only is true, then only the projects that directly import
   --  Project are returned

   function Has_Imported_Projects (Project : Project_Type) return Boolean;
   --  Return True if Project has at least one directly imported project

   procedure Project_Imports
     (Parent           : Project_Type;
      Child            : Project_Type;
      Include_Extended : Boolean := False;
      Imports          : out Boolean;
      Is_Limited_With  : out Boolean);
   --  Return True if Parent imports directly Child.
   --  Is_Limited_With is set to true if the parent imports child through a
   --  "limited with" clause
   --  if Parents or Child is No_Project, True is returned.
   --  If Include_Extended is true, then True is also returned if Child is an
   --  extended project of Parent

   ---------------
   -- Scenarios --
   ---------------

   type Scenario_Variable is private;
   type Scenario_Variable_Array is array (Natural range <>)
     of Scenario_Variable;
   type Scenario_Variable_Array_Access is access Scenario_Variable_Array;

   No_Variable : constant Scenario_Variable;
   No_Scenario : constant Scenario_Variable_Array;

   function Find_Scenario_Variables
     (Project        : Project_Type;
      Parse_Imported : Boolean := True) return Scenario_Variable_Array;
   --  Create and return an array that contains the declarations of all the
   --  scenario variables in Project and its packages. It also includes
   --  variables from imported projects if Parse_Imported is True.
   --  Two candidates are considered the same if they reference the same
   --  environment variable. The reason is that they might not have the same
   --  name internally in imported projects, however, they will always have the
   --  same value.
   --  We not check the list of possible values for efficiency reasons.
   --  ??? Probably we should report an error if they'don't have the same type.

   function External_Reference_Of (Var : Scenario_Variable) return String;
   --  Returns the name of the external variable referenced by Var.
   --  Empty string is returned if Var doesn't reference an external variable.

   function Value_Of (Var : Scenario_Variable) return String;
   --  Return the current value of the external variable

   function Enum_Values_Of
     (Var : Scenario_Variable; Registery : Abstract_Registry'Class)
      return String_List_Utils.String_List.List;
   --  Return all the possible values for the variable given in parameter

   procedure Set_Value (Var : Scenario_Variable; Value : String);
   --  Set the value of the external variable. You need to call Recompute_View
   --  to refresh the project

   function External_Default (Var : Scenario_Variable) return String;
   --  Return the default value for the external variable, computed for the
   --  current view of the project.

   procedure Ensure_External_Value
     (Var  : Scenario_Variable;
      Tree : Prj.Tree.Project_Node_Tree_Ref);
   --  Make sure that an external value is defined for the variable Var. If
   --  none exists, the default value defined in the project hierarchy is used.
   --  This function can be called before a view has been computed for the
   --  project.

   ----------
   -- Data --
   ----------
   --  Some GPS-specific data is associated with projects, and can be retrieved
   --  through this package.

   function Project_Modified
     (Project   : Project_Type;
      Recursive : Boolean := False) return Boolean;
   --  Return True if Project has been modified, but not saved.
   --  If Recursive is True, this function will also return True if one of the
   --  imported project has been modified.

   type Paths_Type_Information is (Relative, Absolute, From_Pref);

   procedure Set_Paths_Type
     (Project : Project_Type; Paths : Paths_Type_Information);
   --  Indicate how the types should be stored internally for the project

   function Get_Paths_Type (Project : Project_Type)
      return Paths_Type_Information;
   --  Indicate how the types are stored internally for the project

   function Is_Normalized (Project : Project_Type) return Boolean;
   --  Return the empy string if Project is normalized, or an error message if
   --  otherwise.

   type Project_Status is (From_File, Default, From_Executable, Empty);
   --  How the project was created: either read from a file, automatically
   --  created from a directory, automatically created from an executable
   --  (debugger case), or default empty project. An actual project file exists
   --  on disk only in the From_File or Default cases.

   function Status (Project : Project_Type) return Project_Status;
   --  Return true if the project is a default project, ie not associated with
   --  a physical file on the disk.

   procedure Set_Status
     (Project : Project_Type; Status : Project_Status);
   --  Indicate whether the project is a default project.
   --  You shouldn't use this function unless you are creating a new project.

   function View_Is_Complete (Project : Project_Type) return Boolean;
   --  Return True if the view was correctly computed for this project.
   --  Return False if the view couldn't be computed correctly because the
   --  project contained invalid references. Such a project can only partially
   --  be used, and it isn't safe to edit it.
   --  Note: the view of the unmodified default project is never considered as
   --  incomplete, since the user doesn't really have control over the contents
   --  of this project.

   function Get_Tree (Project : Project_Type) return Prj.Project_Tree_Ref;
   --  The tree to which the project belongs

private
   type Project_Type_Data;
   type Project_Type_Data_Access is access Project_Type_Data;
   type Project_Type is record
      Node      : Prj.Tree.Project_Node_Id;
      Tree      : Prj.Tree.Project_Node_Tree_Ref;
      View_Tree : Prj.Project_Tree_Ref;

      Data      : Project_Type_Data_Access;
      --  This is an access type for several reasons:
      --    - Hide the real contents in the body
      --    - Parameters do not have to be of type "access Project_Type", just
      --      Project_Type, which lighter to write. At the same time, modifying
      --      the data of the project willl impact all instances that reference
      --      the same project tree.
      --    - Since the user doesn't know this is an access type, he can not
      --      attempt to free the data. Memory is fully controlled by this type
      --      (and the projects registry).
   end record;

   procedure Reset (Project : in out Project_Type);
   pragma Inline (Reset);
   --  Reset the project after the view has been recomputed

   function Get_View (Project : Project_Type) return Prj.Project_Id;
   --  Return the view of the project

   procedure Set_Source_Files
     (Project      : Project_Type;
      Source_Files : VFS.File_Array_Access);
   --  Set the source file cache of the project

   procedure Create_From_Node
     (Project   : out Project_Type;
      Registry  : Abstract_Registry'Class;
      Tree      : Prj.Tree.Project_Node_Tree_Ref;
      View_Tree : Prj.Project_Tree_Ref;
      Node      : Prj.Tree.Project_Node_Id);
   --  Create a new project type from a tree node.
   --  Registry should really be of type Projects.Registry.Project_Registry.
   --  You should never call this function yourself, since the project also
   --  needs to be registered in the registry. Use Get_Project_From_Name from
   --  name instead.
   --  (Tree, View_Tree) are used to identify to which of the loaded project
   --  trees the project belongs. These must have been initialized first.

   procedure Update_Directory_Cache
     (Project   : Project_Type;
      Dir_Name  : String;
      Has_Files : Boolean);
   --  Update the directories cache.

   procedure Destroy (Project : in out Project_Type);
   --  Free the memory associated with the project.

   procedure Set_Is_Normalized (Project : Project_Type; Normalized : Boolean);
   --  Indicate the normalization status of the project

   procedure Set_View_Is_Complete (Project : Project_Type; Complete : Boolean);
   --  Indicate whether the view for the project was correctly computed.

   function Is_External_Variable
     (Var  : Prj.Tree.Project_Node_Id;
      Tree : Prj.Tree.Project_Node_Tree_Ref) return Boolean;
   --  Return True if Var is a reference to an external variable

   function External_Reference_Of
     (Var  : Prj.Tree.Project_Node_Id;
      Tree : Prj.Tree.Project_Node_Tree_Ref) return Types.Name_Id;
   --  Returns the name of the external variable referenced by Var.
   --  No_String is returned if Var doesn't reference an external variable.

   function Split_Package (Attribute : Attribute_Pkg) return Natural;
   --  Return the index in Attribute_Name of the '#' separator.
   --  Attribute_Name is of the form "Package#Attribute" or "Attribute".
   --  A value less than Attribute_Name'First is returned if there is no
   --  package indicator

   type Scenario_Variable is record
      Name        : Types.Name_Id;
      Default     : Types.Name_Id;
      String_Type : Prj.Tree.Project_Node_Id;
   end record;

   procedure Reset_Cache (Project : Project_Type; Imported_By : Boolean);
   --  Reset the importing or imported projects caches, depending on the value
   --  of Importing. See projects.adb for the definition of these two caches

   No_Variable : constant Scenario_Variable :=
     (Types.No_Name, Types.No_Name, Prj.Tree.Empty_Node);

   No_Scenario : constant Scenario_Variable_Array (1 .. 0) :=
     (others => No_Variable);

   No_Project : constant Project_Type :=
                  (Prj.Tree.Empty_Node, null, null, null);

   All_Languages : constant Name_Id_Array :=
     (1 .. 0 => Types.No_Name);

   type Imported_Project_Iterator is record
      Root      : Project_Type;
      Current   : Integer;

      Importing : Boolean := False;
      --  True if we are looking for importing projects instead of imported
      --  projects.

      Include_Extended : Boolean := True;
      --  True if we should also return extended projects

      Direct_Only : Boolean := False;
      --  Relevant only when listing projects importing Root

      Current_Cache : Project_Type;
      --  Cached value for the current project (avoid recomputing every time)
   end record;

   type Attribute_Pkg is new String;
   Source_Dirs_Attribute      : constant Attribute_Pkg := "source_dirs";
   Source_Files_Attribute     : constant Attribute_Pkg := "source_files";
   Locally_Removed_Files_Attribute : constant Attribute_Pkg :=
                                       "locally_removed_files";
   Gnatlist_Attribute         : constant Attribute_Pkg := "ide#gnatlist";
   Compiler_Command_Attribute : constant Attribute_Pkg :=
                                  "ide#compiler_command";
   Debugger_Command_Attribute : constant Attribute_Pkg :=
                                  "ide#debugger_command";
   Remote_Host_Attribute     : constant Attribute_Pkg := "ide#remote_host";
   Program_Host_Attribute    : constant Attribute_Pkg := "ide#program_host";
   Protocol_Attribute        : constant Attribute_Pkg :=
                                 "ide#communication_protocol";
   Main_Attribute            : constant Attribute_Pkg := "main";
   Vcs_File_Check            : constant Attribute_Pkg := "ide#vcs_file_check";
   Vcs_Log_Check             : constant Attribute_Pkg := "ide#vcs_log_check";
   Obj_Dir_Attribute         : constant Attribute_Pkg := "object_dir";
   Vcs_Kind_Attribute        : constant Attribute_Pkg := "ide#vcs_kind";
   Vcs_Repository_Root       : constant Attribute_Pkg :=
                                 "ide#vcs_repository_root";
   Global_Pragmas_Attribute  : constant Attribute_Pkg :=
                                 "builder#global_configuration_pragmas";
   Local_Pragmas_Attribute   : constant Attribute_Pkg :=
                                 "compiler#local_configuration_pragmas";
   Builder_Default_Switches_Attribute : constant Attribute_Pkg :=
                                          "builder#default_switches";
   Compiler_Default_Switches_Attribute : constant Attribute_Pkg :=
                                           "compiler#default_switches";
   Linker_Default_Switches_Attribute : constant Attribute_Pkg :=
                                         "linker#default_switches";
   Binder_Default_Switches_Attribute   : constant Attribute_Pkg :=
                                           "binder#default_switches";
   Executable_Attribute       : constant Attribute_Pkg :=
                                  "builder#executable";

   --  Naming package
   Casing_Attribute           : constant Attribute_Pkg := "naming#casing";
   Specification_Suffix_Attribute : constant Attribute_Pkg :=
                                      "naming#specification_suffix";
   --  Specific to Ada
   Implementation_Suffix_Attribute : constant Attribute_Pkg :=
                                       "naming#implementation_suffix";
   --  Specific to Ada
   Separate_Suffix_Attribute  : constant Attribute_Pkg :=
                                  "naming#separate_suffix";
   Spec_Suffix_Attribute      : constant Attribute_Pkg := "naming#spec_suffix";
   Impl_Suffix_Attribute      : constant Attribute_Pkg := "naming#body_suffix";
   Dot_Replacement_Attribute  : constant Attribute_Pkg :=
                                  "naming#dot_replacement";
   Specification_Attribute    : constant Attribute_Pkg := "naming#spec";
   Implementation_Attribute   : constant Attribute_Pkg := "naming#body";

   --  For backward compatiblity
   Old_Specification_Attribute  : constant Attribute_Pkg :=
                                    "naming#specification";
   Old_Implementation_Attribute : constant Attribute_Pkg :=
                                    "naming#implementation";

   Spec_Exception_Attribute   : constant Attribute_Pkg :=
                                  "naming#specification_exceptions";
   Impl_Exception_Attribute   : constant Attribute_Pkg :=
                                  "naming#implementation_exceptions";

   --  The following attributes should be read through specialized subprograms
   --  (Get_Languages,...)
   Languages_Attribute        : constant Attribute_Pkg := "languages";
   Exec_Dir_Attribute         : constant Attribute_Pkg := "exec_dir";

   pragma Inline (Project_Name, Project_Path);
end Projects;
