
with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;

package Entities_Db is

   procedure Parse_All_LI_Files
     (Session : Session_Type;
      Tree    : Project_Tree;
      Project : Project_Type);
   --  Parse all the LI files for the project, and stores them in the
   --  database

end Entities_Db;
