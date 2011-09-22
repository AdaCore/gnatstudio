with Ada.Calendar;          use Ada.Calendar;
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.Strings;          use GNAT.Strings;
with GNATCOLL.SQL.Exec;     use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with Entities_Db;           use Entities_Db;

procedure Test_Entities is
   Env     : Project_Environment_Access;
   Tree    : Project_Tree;
   Start   : Time;
   GNAT_Version : String_Access;
begin
   GNATCOLL.Traces.Parse_Config_File;

   Start := Clock;

   --  Load project

   Initialize (Env);
   Env.Set_Path_From_Gnatls
     (Gnatls       => "gnatls",
      GNAT_Version => GNAT_Version,
      Errors       => Put_Line'Access);
   Env.Register_Default_Language_Extension
     (Language_Name       => "C",
      Default_Spec_Suffix => ".h",
      Default_Body_Suffix => ".c");
   Free (GNAT_Version);
   Tree.Load
     (Root_Project_Path => Create ("../gps/gps.gpr"),
      --  Create ("../gps/gps.gpr"),
      Env               => Env,
      Errors            => Put_Line'Access);
   Put_Line ("Done loading project:"
             & Duration'Image (Clock - Start) & " seconds");

   --  Prepare database

   GNATCOLL.SQL.Sessions.Setup
     (Descr        => GNATCOLL.SQL.Sqlite.Setup
        (Database => "entities.db"),
      Max_Sessions => 1);

   --  Parse ALI files

   Parse_All_LI_Files (Get_New_Session, Tree, Tree.Root_Project);

   --  Free memory

   Tree.Unload;
   Free (Env);
   GNATCOLL.Projects.Finalize;
end Test_Entities;
