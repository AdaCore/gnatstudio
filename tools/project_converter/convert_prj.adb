with GNAT.IO;                   use GNAT.IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Convert.Adp;               use Convert.Adp;
with Convert.Gpr;               use Convert.Gpr;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

procedure Convert_Prj is
   Project_Name : String_Access;
begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: " & Command_Name & " project_name.gpr");
      Put_Line ("  convert to .adp project file");
      Put_Line ("Usage: " & Command_Name & " project_name.adp");
      Put_Line ("  convert to .gpr project file");
      return;
   end if;

   Project_Name := new String'(Argument (1));

   if File_Extension (Project_Name.all) = ".gpr" then
      Convert_From_Gpr_To_Adp (Project_Name.all);

   elsif File_Extension (Project_Name.all) = ".adp" then
      Convert_From_Adp_To_Gpr (Project_Name.all);

   else
      Put_Line ("Unsupported file extension "
                & File_Extension (Project_Name.all));
   end if;

end Convert_Prj;
