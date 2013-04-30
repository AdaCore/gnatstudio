------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
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

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;                      use Ada.Text_IO;

with GNAT.Command_Line;                use GNAT.Command_Line;
with GNAT.OS_Lib;
with GNAT.Strings;                     use GNAT.Strings;

with GNATCOLL.Scripts;                 use GNATCOLL.Scripts;
with GNATCOLL.Traces;                  use GNATCOLL.Traces;
with GNATCOLL.VFS;                     use GNATCOLL.VFS;

with Build_Command_Utils;
with Build_Configurations;             use Build_Configurations;
with Commands.Builder.Scripts;
with Commands.Builder.Build_Output_Collectors;

with GPS.CLI_Kernels;
with GPS.Core_Kernels;
with GPS.Python_Core;
with GPS.Scripts.Entities;
with GPS.Scripts.File_Locations;
with GPS.Scripts.Files;
with GPS.Scripts.Projects;
with GPS.Tools_Output;                 use GPS.Tools_Output;

procedure GPS.CLI is
   procedure Parse_Command_Line (Switch, Parameter, Section : String);
   --  Handles some switches from the command line. Other switches are handled
   --  directly by Getopt and will set the corresponding local variables.

   procedure Execute_Batch
     (Kernel      : access GPS.CLI_Kernels.CLI_Kernel_Record;
      Lang_Name   : String;
      Script_Name : String);
   --  Execute a batch command file Script_Name in Lang_Name language.

   procedure Register_Classes
     (Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record);
   --  Register GPS script's classes

   procedure Register_Output_Parsers;
   --  Register tool output parsers.

   Output_Collector : aliased
     Commands.Builder.Build_Output_Collectors.Output_Parser_Fabric;

   Registry : Build_Config_Registry_Access;
   Builder  : aliased Build_Command_Utils.Builder_Context_Record;

   -------------------
   -- Execute_Batch --
   -------------------

   procedure Execute_Batch
     (Kernel      : access GPS.CLI_Kernels.CLI_Kernel_Record;
      Lang_Name   : String;
      Script_Name : String)
   is
      Script : Scripting_Language;
      Errors : Boolean;
   begin
      Script := Kernel.Scripts.Lookup_Scripting_Language (Lang_Name);

      if Script = null then
         Put_Line
           ("Language unknown for --load command line switch: " & Lang_Name);
      else
         Execute_File
           (Script   => Script,
            Filename => GNAT.OS_Lib.Normalize_Pathname
                          (Script_Name, Get_Current_Dir.Display_Full_Name),
            Show_Command => False,
            Errors   => Errors);
      end if;
   end Execute_Batch;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line (Switch, Parameter, Section : String) is
      pragma Unreferenced (Section);
   begin
      null;
   end Parse_Command_Line;

   ----------------------
   -- Register_Classes --
   ----------------------

   procedure Register_Classes
     (Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record)
 is
   begin
      GPS.Scripts.Entities.Register_Commands (Kernel);
      GPS.Scripts.File_Locations.Register_Commands (Kernel);
      GPS.Scripts.Files.Register_Commands (Kernel);
      GPS.Scripts.Projects.Register_Commands (Kernel);
      Commands.Builder.Scripts.Register_Commands (Kernel);
   end Register_Classes;

   -----------------------------
   -- Register_Output_Parsers --
   -----------------------------

   procedure Register_Output_Parsers is
   begin
      Register_Output_Parser (Output_Collector'Access, "output_collector");
      Output_Collector.Set (Builder'Unchecked_Access);
   end Register_Output_Parsers;

   Cmdline               : Command_Line_Configuration;
   Project_Name          : aliased GNAT.Strings.String_Access;
   Script_Name           : aliased GNAT.Strings.String_Access;
   Kernel                : constant GPS.CLI_Kernels.CLI_Kernel :=
     new GPS.CLI_Kernels.CLI_Kernel_Record;
   GNAT_Version          : GNAT.Strings.String_Access;
begin
   GNATCOLL.Traces.Parse_Config_File;
   Set_Usage
     (Cmdline,
      Help => "GPS command line interface");

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
      Help        => "Execute an external file written in the language lang");

   begin
      Getopt (Cmdline, Parse_Command_Line'Unrestricted_Access);
   exception
      when GNAT.Command_Line.Exit_From_Command_Line =>
         --  User provided -h or --help option. Just return
         return;
   end;

   if Project_Name.all = "" then
      Free (Project_Name);
      Project_Name := new String'(GNAT.Command_Line.Get_Argument);

      if Project_Name.all = "" then
         Put_Line ("No project file specified");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;
   end if;

   GPS.Core_Kernels.Initialize (Kernel);
   GPS.Python_Core.Register_Python (Kernel);
   Registry := Create;
   Builder.Initialize (GPS.Core_Kernels.Core_Kernel (Kernel), Registry);
   Register_Classes (Kernel);
   Register_Output_Parsers;

   declare
      Path : Virtual_File := Create (+Project_Name.all);
   begin
      if not Path.Is_Regular_File then
         Path := Create (+Project_Name.all & ".gpr");

         if not Path.Is_Regular_File then
            Put_Line ("No such file: " & Project_Name.all);
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            return;
         end if;
      end if;

      Kernel.Registry.Environment.Set_Path_From_Gnatls
         ("gnatls", GNAT_Version);
      Kernel.Registry.Tree.Load
        (Root_Project_Path => Path,
         Env => Kernel.Registry.Environment);
   end;

   if Script_Name.all /= "" then
      declare
         Colon : constant Natural :=
           Ada.Strings.Fixed.Index (Script_Name.all, ":");
      begin
         if Colon /= 0 then
            Execute_Batch
              (Kernel,
               Lang_Name   => Script_Name (Script_Name'First .. Colon - 1),
               Script_Name => Script_Name (Colon + 1 .. Script_Name'Last));
         else
            Put_Line ("No lang in --load=" & Script_Name.all);
         end if;
      end;
   end if;

   --  Destroy all
   Free (Registry);
   GPS.Core_Kernels.Destroy (Kernel);
end GPS.CLI;
