--  This package provides access to the project characteristics such as
--  object and source path, options.

package Glide_Kernel.Project is

   function Get_Project_File_Name (Kernel : Kernel_Handle) return String;

   function Find_Source_File
     (Kernel : Kernel_Handle; Short_File_Name : String)
      return String;
   --  Search in the project source path Short_File_Name and return its
   --  complete path, or an empty string in case of failure.

   procedure Load_Project (Kernel : in out Kernel_Handle; Project : String);
   --  Load project Project as the current project.

end Glide_Kernel.Project;
