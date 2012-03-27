------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;          use Ada.Calendar;
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.Command_Line;     use GNAT.Command_Line;
with GNAT.Strings;          use GNAT.Strings;
with GNAT.OS_Lib;
with GNATCOLL.SQL.Exec;     use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Inspect;  use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL.Postgres;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with Entities_Db;           use Entities_Db;

procedure Test_Entities is

   Use_Postgres : aliased Boolean := False;
   --  Whether to use sqlite or postgreSQL

   Do_Not_Perform_Queries : aliased Boolean := False;
   --  Whether to perform the queries in the database

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
   Cmdline_Config : Command_Line_Configuration;

   Need_To_Create_DB : Boolean;

begin
   GNATCOLL.Traces.Parse_Config_File;

   Define_Switch
     (Cmdline_Config, Do_Not_Perform_Queries'Access,
      Long_Switch => "--nodb",
      Help => "Disable all SQL commands (timing measurement only)");
   Define_Switch
     (Cmdline_Config, Use_Postgres'Access,
      Long_Switch => "--postgres",
      Help => "Use postgreSQL as the backend, instead of sqlite");

   Getopt (Cmdline_Config);

   GNATCOLL.SQL.Exec.Perform_Queries := not Do_Not_Perform_Queries;

   --  Prepare database

   if Use_Postgres then
      GNATCOLL.SQL.Sessions.Setup
        (Descr        => GNATCOLL.SQL.Postgres.Setup (Database => DB_Name),
         Max_Sessions => 1);
      Need_To_Create_DB := True;
   else
      GNATCOLL.SQL.Sessions.Setup
        (Descr        => GNATCOLL.SQL.Sqlite.Setup (Database => DB_Name),
         Max_Sessions => 1);
      Need_To_Create_DB := not GNAT.OS_Lib.Is_Regular_File (DB_Name);
   end if;

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

         if Session.DB.Success then
            --  Load initial data

            Load_Data
              (Session.DB,
               File   => Initial_Data,
               Schema => Schema);
            Session.Commit;
         end if;

         if not Session.DB.Success then
            Session.Rollback;
            return;
         end if;

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

   --  Free memory

   Tree.Unload;
   Free (Env);
   GNATCOLL.Projects.Finalize;

exception
   when GNAT.Command_Line.Exit_From_Command_Line =>
      null;
end Test_Entities;
