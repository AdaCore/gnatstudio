with Prj;          use Prj;
with Prj_API;      use Prj_API;

procedure Test_Api is
   Project_Name : constant String := "prj1/root.apr";
   Project : Project_Id;
   Prj1 : Project_Id;
begin
   Project := Parse_Project (Project_Name);

   --  Modifies an initial list of parameters
   Set_File_Specific_Switches
     (Find_Project_Containing (Project, File => "child1_src.adb"),
      "child1_src.adb", "-O0 -gnaty");

   --  Modifies a single initial parameter
   Set_File_Specific_Switches
     (Find_Project_Containing (Project, File => "child11_src.ads"),
      "child11_src.ads", "-O0 -gnaty");

   --  No specific initial parameter (and not even a gnatmake package)
   Set_File_Specific_Switches
     (Find_Project_Containing (Project, File => "child2_src.adb"),
      "child2_src.adb",  "-O0 -gnaty");

   --  Add a new project (imported directly by the top-level project)
   Prj1 := Create_Project ("child4");
   Import_Project (Project, Prj1);

   --  Adding to existing variable
   Add_Source_Dir (Project, "manu");

   --  Adding to existing array
   Add_Source_Dir (Find_Project ("child1"), "manu");

   --  Creating new variable
   Add_Source_Dir (Find_Project ("child4"), "manu");
   Add_Source_Dir (Find_Project ("child4"), "manu");

   --  Write the output
   Write_Project (Project);

end Test_Api;
