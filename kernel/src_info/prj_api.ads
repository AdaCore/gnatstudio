with Types;
with Prj;
with Snames;

package Prj_API is

   ---------------------------
   -- Reading project files --
   ---------------------------

   function Parse_Project (Name : String) return Prj.Project_Id;
   --  Parse the project Name, and return a handler to it.

   function Get_File_Specific_Switches
     (Project    : Prj.Project_Id;
      File_Name  : String;
      In_Package : Types.Name_Id := Snames.Name_Compiler)
     return Prj.Variable_Value;
   --  Return the list of specific switches to use for a file.
   --  Note that the return value can be either a single switch or a list of
   --  them.
   --  In_Package should be one of Name_Compiler, Name_Binder, Name_Linker

   function Get_Default_Switches
     (Project    : Prj.Project_Id;
      In_Package : Types.Name_Id := Snames.Name_Compiler)
     return Prj.Variable_Value;
   --  Return the list of default switches to use for a file
   --  In_Package should be one of Name_Compiler, Name_Binder, Name_Linker

   function Find_Project_Containing
     (Root : Prj.Project_Id; File : String) return Prj.Project_Id;
   --  Find the sub-project of Root to which File_Name belong.

   function Find_Project (Name : String) return Prj.Project_Id;
   --  Find the project whose name is Name.

   --------------------------
   -- Output project files --
   --------------------------

   procedure Write_Project (Project : Prj.Project_Id);
   --  Print the contents of the project file in the correct format.

   -------------------------
   -- Setting up projects --
   -------------------------

   procedure Set_File_Specific_Switches
     (Project    : Prj.Project_Id;
      File_Name  : String;
      Switches   : String;
      In_Package : Types.Name_Id := Snames.Name_Compiler);
   --  Set the switches to use to compile File_Name.
   --  The string Switches is split as required.
   --
   --  In_Package should be one of "Name_Compiler", "Name_Binder",
   --  "Name_Linker".

   function Create_Project (Name : String) return Prj.Project_Id;
   --  Create a new empty project.

   procedure Import_Project (Parent, Imported : Prj.Project_Id);
   --  Make Parent import Imported

   procedure Add_Source_Dir (Project : Prj.Project_Id; Dir : String);
   --  Add a new directory to the list of source directories for Project.
   --  It doesn't check if the directory is already there.

end Prj_API;
