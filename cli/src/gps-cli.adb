------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2022, AdaCore                     --
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
--  Driver for command line version of GPS

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Strings;      use GNAT.Strings;

with GNATCOLL.Arg_Lists;
with GNATCOLL.Scripts;  use GNATCOLL.Scripts;
with GNATCOLL.Traces;   use GNATCOLL.Traces;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with GPS.CLI_Utils;     use GPS.CLI_Utils;
with GPS.CLI_Kernels;   use GPS.CLI_Kernels;

with Xref;              use Xref;

procedure GPS.CLI is

   procedure CLI_Driver;
   --  The driver for gnatstudio_cli

   procedure Adareducer_Driver;
   --  The driver for Adareducer

   Cmdline               : Command_Line_Configuration;
   Project_Name          : aliased GNAT.Strings.String_Access;
   Kernel                : constant GPS.CLI_Kernels.CLI_Kernel :=
     new GPS.CLI_Kernels.CLI_Kernel_Record;

   -----------------------
   -- Adareducer_Driver --
   -----------------------

   procedure Adareducer_Driver is
      Script : constant GNATCOLL.Scripts.Scripting_Language :=
                 Kernel.Scripts.Lookup_Scripting_Language ("python");
      Follow_Closure : aliased Boolean := False;
      Errors         : Boolean;
      Command        : Unbounded_String;
      Single_File    : aliased GNAT.Strings.String_Access;
      Oracle_File    : aliased GNAT.Strings.String_Access;

   begin
      --  Set comand line options
      Set_Usage
        (Cmdline,
         Help => "GNAT Studio command line interface (adareducer mode)");

      Define_Switch
        (Cmdline,
         Output      => Project_Name'Access,
         Switch      => "-P:",
         Long_Switch => "--project=",
         Help        => "Load the given project (mandatory)");
      Define_Switch
        (Cmdline,
         Output      => Single_File'Access,
         Switch      => "",
         Long_Switch => "--single_file=",
         Help        =>
           "Specify the base name of a file where processing should start.");
      Define_Switch
        (Cmdline,
         Output       => Follow_Closure'Access,
         Switch       => "",
         Long_Switch  => "--follow-closure",
         Help         => "Follow the closure of the file");
      Define_Switch
        (Cmdline,
         Output      => Oracle_File'Access,
         Switch      => "-s:",
         Long_Switch => "--oracle_script=",
         Help        => "The reduction oracle script.");

      --  Parse the command line
      begin
         Getopt (Cmdline);
      exception
         when GNAT.Command_Line.Exit_From_Command_Line =>
            --  User provided -h or --help option
            return;
      end;

      --  Base checks
      if Project_Name.all = "" then
         Put_Line ("Specify a project with the -P switch");
         return;
      end if;

      if Oracle_File.all = "" then
         Put_Line ("Specify an oracle with the -s/--oracle_script switch");
         return;
      end if;

      --  Adareducer is a Python program - craft the
      --  Python function call here.
      Command := To_Unbounded_String
         ("from ada_reducer import main;main._main(");

      if Single_File = null then
         Append (Command, "None, ");
      else
         Append (Command, """" & Single_File.all & """, ");
      end if;

      if Follow_Closure then
         Append (Command, "True, ");
      else
         Append (Command, "False, ");
      end if;

      Append (Command, """" & Project_Name.all);

      --  Add a ".gpr" if needed
      if not (Project_Name'Length > 3
        and then To_Lower (Project_Name
          (Project_Name'Last - 3 .. Project_Name'Last)) = ".gpr")
      then
         Append (Command, ".gpr");
      end if;
      Append (Command, """, ");
      Append (Command, """" & Oracle_File.all & """)");

      --  Launch the Python command
      Script.Execute_Command
        (CL           => GNATCOLL.Arg_Lists.Create (To_String (Command)),
         Hide_Output  => True,
         Errors       => Errors);
   end Adareducer_Driver;

   ----------------
   -- CLI_Driver --
   ----------------

   procedure CLI_Driver is
      Script       : Scripting_Language;
      Errors       : Boolean;
      Project_File : Virtual_File;
      Script_Name  : aliased GNAT.Strings.String_Access;

   begin
      --  Set command line options
      Set_Usage
        (Cmdline,
         Help => "GNAT Studio command line interface");

      Define_Switch
        (Cmdline,
         Output      => Project_Name'Access,
         Switch      => "-P:",
         Long_Switch => "--project=",
         Help        => "Load the given project (mandatory)");
      Define_Switch
        (Cmdline,
         Output      => Script_Name'Access,
         Switch      => "-l:",
         Long_Switch => "--load=",
         Help        =>
           "Execute an external file written in the language lang");
      Define_Switch
        (Cmdline,
         Switch       => "-X:",
         Help         => "Specify an external reference in the project");
      Define_Switch
        (Cmdline,
         Switch       => "-v",
         Long_Switch  => "--version",
         Help         => "show the version and exit");

      --  Retrieve command line option
      begin
         GPS.CLI_Utils.Parse_Command_Line (Cmdline, Kernel);
      exception
         when GNAT.Command_Line.Exit_From_Command_Line =>
            --  User provided -h or --help option. Just return
            return;
      end;

      --  Check project file path passed in command line
      --  Exit with message if no project file path found at all
      if not CLI_Utils.Is_Project_Path_Specified (Project_Name) then
         Put_Line ("No project file specified");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;

      --  Exit with message if path is not valid
      if not CLI_Utils.Project_File_Path_Exists (Project_Name) then
         Put_Line ("No such file: " & Project_Name.all);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;

      Project_File := Create (+Project_Name.all);

      --  Load script
      Parse_And_Execute_Script (Kernel, Script_Name.all, Script);

      --  Load project
      Kernel.Registry.Tree.Load
        (Root_Project_Path => Project_File,
         Env               => Kernel.Registry.Environment);

      Project_Changed (Kernel.Databases);
      Project_View_Changed (Kernel.Databases, Kernel.Registry.Tree);

      --  Start execute() script callback
      if Script /= null then
         Script.Execute_Command
           ("if vars().has_key('execute'): execute()", Errors => Errors);

         if Errors then
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         end if;
      end if;
   end CLI_Driver;

begin
   --  Retrieve log configuration
   GNATCOLL.Traces.Parse_Config_File;

   --  Initialize context
   GPS.CLI_Utils.Create_Kernel_Context (Kernel);

   if Argument_Count >= 1
     and then Argument (1) = "adareducer"
   then
      Adareducer_Driver;
   else
      CLI_Driver;
   end if;

   --  Destroy all
   GPS.CLI_Utils.Destroy_Kernel_Context (Kernel);
end GPS.CLI;
