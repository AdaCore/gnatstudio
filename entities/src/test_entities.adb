-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                  Copyright (C) 2011, AdaCore                      --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Calendar;          use Ada.Calendar;
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.Strings;          use GNAT.Strings;
with GNAT.OS_Lib;
with GNATCOLL.SQL.Exec;     use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Inspect;  use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with Entities_Db;           use Entities_Db;

procedure Test_Entities is
   DB_Name      : constant String := "entities.db";
   GPR_File     : constant Virtual_File :=
     --  Create ("entities.gpr");
     Create ("../gps/gps.gpr");
   DB_Schema_Descr : constant Virtual_File := Create ("../share/dbschema.txt");
   Initial_Data : constant Virtual_File := Create ("../share/initialdata.txt");

   Env     : Project_Environment_Access;
   Tree    : Project_Tree;
   Start   : Time;
   GNAT_Version : String_Access;

   Need_To_Create_DB : constant Boolean :=
     not GNAT.OS_Lib.Is_Regular_File (DB_Name);

begin
   GNATCOLL.Traces.Parse_Config_File;
   GNATCOLL.SQL.Exec.Perform_Queries := True;

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
     (Root_Project_Path => GPR_File,
      Env               => Env,
      Errors            => Put_Line'Access);
   Put_Line ("Done loading project:"
             & Duration'Image (Clock - Start) & " seconds");

   --  Prepare database

   GNATCOLL.SQL.Sessions.Setup
     (Descr        => GNATCOLL.SQL.Sqlite.Setup (Database => DB_Name),
      Max_Sessions => 1);

   --  Create the database if needed

   declare
      Session : constant Session_Type := Get_New_Session;
      Schema  : DB_Schema;
   begin
      --  Is this an empty database ?

      if Need_To_Create_DB then
         Start := Clock;

         --  Create it

         Schema := New_Schema_IO (DB_Schema_Descr).Read_Schema;
         New_Schema_IO (Session.DB).Write_Schema (Schema);

         --  Load initial data

         Load_Data
           (Session.DB,
            File   => Initial_Data,
            Schema => Schema);

         Session.Commit;

         Put_Line
           ("Created database:" & Duration'Image (Clock - Start) & " seconds");
      else
         Put_Line ("Database already exists, reusing");
      end if;
   end;

   --  Parse ALI files

   Parse_All_LI_Files
     (Get_New_Session,
      Tree              => Tree,
      Env               => Env,
      Project           => Tree.Root_Project,
      Database_Is_Empty => Need_To_Create_DB);

--     Put_Line ("Number of traces on SQL.SELECT: "
--               & GNATCOLL.Traces.Count
--                 (GNATCOLL.Traces.Create ("SQL.SELECT"))'Img);
--     Put_Line ("Number of traces on SQL: "
--               & GNATCOLL.Traces.Count (GNATCOLL.Traces.Create ("SQL"))'Img);

   --  Free memory

   Tree.Unload;
   Free (Env);
   GNATCOLL.Projects.Finalize;
end Test_Entities;
