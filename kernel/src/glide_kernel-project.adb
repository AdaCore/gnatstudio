
with Prj;         use Prj;
with Prj.Pars;    use Prj.Pars;
with Prj.Env;     use Prj.Env;
with Namet;       use Namet;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Glide_Kernel.Project is

   ----------------------
   -- Find_Source_File --
   ----------------------

   function Find_Source_File
     (Kernel : Kernel_Handle; Short_File_Name : String)
      return String
   is
      Path : String_Access;
   begin
      Path := Locate_Regular_File
        (Short_File_Name,
         Ada_Include_Path (Kernel.Project).all);

      if Path = null then
         return "";

      else
         declare
            Full_Path : constant String := Path.all;
         begin
            Free (Path);
            return Full_Path;
         end;
      end if;
   end Find_Source_File;

   ---------------------------
   -- Get_Project_File_Name --
   ---------------------------

   function Get_Project_File_Name (Kernel : Kernel_Handle) return String is
   begin
      return Get_Name_String (Projects.Table (Kernel.Project).Name);
   end Get_Project_File_Name;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project (Kernel : in out Kernel_Handle; Project : String) is
   begin
      Prj.Pars.Parse (Kernel.Project, Project);
   end Load_Project;

end Glide_Kernel.Project;

