
--  This package converts from .adp projects to .gpr projects

with GNAT.OS_Lib;

package Convert.Adp is

   procedure Convert_From_Adp_To_Gpr
     (Adp_Filename : String;
      Spec_Extension, Body_Extension : GNAT.OS_Lib.String_Access);
   --  Convert project is sent to standard output

   function Create_Gpr_Files
     (Root_Project_Name : String;
      Output_Dir        : String;
      Source_Dirs       : GNAT.OS_Lib.String_List;
      Object_Dirs       : GNAT.OS_Lib.String_List;
      Spec_Extension    : String;
      Body_Extension    : String;
      Main_Units        : GNAT.OS_Lib.String_List_Access := null;
      Builder_Switches  : String;
      Compiler_Switches : String;
      Binder_Switches   : String;
      Linker_Switches   : String;
      Cross_Prefix      : String := "") return String;
   --  Attempt to create one or more .gpr project file to match the setup.
   --  If the application was not compiled, a single object directory is kept,
   --  and a single .gpr file is created. Otherwise, we try and generate a
   --  set of project files that match the current setup as much as possible.
   --
   --  Root_Project_Name is the name of the root project to create.
   --  Paths in Source_Dirs and Obj_dirs must be absolute paths
   --  The first directory in Object_Dirs is the one that will be kept if a
   --  single search directory is needed.

end Convert.Adp;
