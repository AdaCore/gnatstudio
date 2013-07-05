------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with GPS.CLI_Buffer_Providers;
with GPS.CLI_Target_Loaders;
with GPS.CLI_Scripts;

with GPS.Core_Kernels;
with GPS.Python_Core;
with GPS.Scripts.Entities;
with GPS.Scripts.File_Locations;
with GPS.Scripts.Files;
with GPS.Scripts.Projects;
with GPS.Tools_Output;                         use GPS.Tools_Output;

with Ada_Semantic_Tree.Lang;
with Ada_Semantic_Tree.Assistants;
with Commands.Builder.Scripts;
with Commands.Builder.Build_Output_Collectors;
with Build_Command_Utils;
with Build_Configurations;                     use Build_Configurations;
with Language.Tree.Database;
with Language.Ada;

with GNAT.IO;                                  use GNAT.IO;
with Ada.Strings.Fixed;                        use Ada.Strings.Fixed;
with GNATCOLL.Scripts;                         use GNATCOLL.Scripts;
with GNATCOLL.VFS_Utils;
with GNATCOLL.Utils;
with GNAT.OS_Lib;

package body GPS.CLI_Utils is

   procedure Register_Classes
     (Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record);
   --  Register GPS script's classes

   procedure Register_Output_Parsers
     (Builder : in out Build_Command_Utils.Builder_Context_Record);
   --     --  Register tool output parsers.

   ----------------------
   -- Register_Classes --
   ----------------------

   procedure Register_Classes
     (Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record) is
   begin
      GPS.Scripts.Entities.Register_Commands (Kernel);
      GPS.Scripts.File_Locations.Register_Commands (Kernel);
      GPS.Scripts.Files.Register_Commands (Kernel);
      GPS.Scripts.Projects.Register_Commands (Kernel);
      Commands.Builder.Scripts.Register_Commands (Kernel);
      GPS.CLI_Scripts.Register_Commands (Kernel);
   end Register_Classes;

   -----------------------------
   -- Register_Output_Parsers --
   -----------------------------

   procedure Register_Output_Parsers
     (Builder : in out Build_Command_Utils.Builder_Context_Record)
   is
      use Commands.Builder;
      Output_Collector      : access Output_Parser_Fabric'Class;
   begin
      Output_Collector := new Build_Output_Collectors.Output_Parser_Fabric;
      Register_Output_Parser (Output_Collector, "output_collector");
      Build_Output_Collectors.Output_Parser_Fabric (Output_Collector.all).Set
        (Builder'Unchecked_Access);
   end Register_Output_Parsers;

   ----------------------------
   --  Create_Kernel_Context --
   ----------------------------

   procedure Create_Kernel_Context
     (Kernel                  : access GPS.CLI_Kernels.CLI_Kernel_Record;
      Install_Semantic_Parser : Boolean := True) is

      use Commands.Builder;

      GNAT_Version       : GNAT.Strings.String_Access;
      Std_Entities_Files : constant Virtual_File := Create_From_Dir
        (Kernel.Get_Share_Dir, "predefined_ada.xml");
      Registry           : Build_Config_Registry_Access;
      Builder            : constant Build_Command_Utils.Builder_Context :=
        new Build_Command_Utils.Builder_Context_Record;
      Target_Loader      : constant GPS.Core_Kernels.Abstract_Module :=
        new GPS.CLI_Target_Loaders.Target_Loader (Kernel);

   begin
      --  Initialize
      GPS.Core_Kernels.Initialize (Kernel);

      Language.Tree.Database.Set_Provider
        (Kernel.Databases.Constructs,
         new GPS.CLI_Buffer_Providers.CLI_Buffer_Provider);

      if Install_Semantic_Parser then
         Ada_Semantic_Tree.Assistants.Register_Ada_Assistants
           (Kernel.Databases.Constructs, Std_Entities_Files);
      end if;

      GPS.Python_Core.Register_Python (Kernel);
      Registry := Create;
      Builder.Initialize (GPS.Core_Kernels.Core_Kernel (Kernel), Registry);

      --  Register
      Register_Classes (Kernel);
      Register_Output_Parsers (Builder.all);
      Kernel.Register_Module (Target_Loader);
      Kernel.Lang_Handler.Register_Language
        (Lang      => Language.Ada.Ada_Lang,
         Tree_Lang => Ada_Semantic_Tree.Lang.Ada_Tree_Lang);

      --  Set GNAT version
      Kernel.Registry.Environment.Set_Path_From_Gnatls
        ("gnatls", GNAT_Version);
   end Create_Kernel_Context;

   ----------------------------
   -- Destroy_Kernel_Context --
   ----------------------------

   procedure Destroy_Kernel_Context
     (Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record) is
   begin
      GPS.Core_Kernels.Destroy (Kernel);
   end Destroy_Kernel_Context;

   ------------------------
   -- Parse_Command_line --
   ------------------------

   procedure Parse_Command_Line
     (Command_Line : Command_Line_Configuration;
      Kernel       : access GPS.CLI_Kernels.CLI_Kernel_Record)
   is
      procedure Local_Parse_Command_Line (Switch, Parameter, Section : String);
      --  Allow to manage every occurance of -X switch for scenario variable.

      ------------------------
      -- Parse_Command_Line --
      ------------------------

      procedure Local_Parse_Command_Line
        (Switch, Parameter, Section : String) is
         pragma Unreferenced (Section);
         Equal : Natural;
      begin
         if Switch = "-X" then
            Equal := Ada.Strings.Fixed.Index (Parameter, "=");
            if Equal /= 0 then
               Kernel.Registry.Environment.Change_Environment
                 (Name => Parameter (Parameter'First .. Equal - 1),
                  Value => Parameter (Equal + 1 .. Parameter'Last));
            else
               Put_Line
                 ("Ignoring switch -X, missing name or/and value for: " &
                    Switch & Parameter);
            end if;
         end if;

      end Local_Parse_Command_Line;
   begin
      Getopt (Command_Line, Local_Parse_Command_Line'Unrestricted_Access);
   end Parse_Command_Line;

   -------------------------------
   -- Is_Project_Path_Specified --
   -------------------------------

   function Is_Project_Path_Specified
     (Path : in out GNAT.Strings.String_Access) return Boolean is
   begin
      --  Check if the given path is not empty, if it is: look for
      --  the next element on the command line that is not a switch.
      if Path.all = "" then
         Free (Path);
         Path := new String'(GNAT.Command_Line.Get_Argument);

         --  If no project file has been specified in command line
         if Path.all = "" then
            return False;
         end if;
      end if;

      return True;
   end Is_Project_Path_Specified;

   ------------------------------
   -- Project_File_Path_Exists --
   ------------------------------

   function Project_File_Path_Exists
     (Path : in out GNAT.Strings.String_Access) return Boolean
   is
      File_Name        : constant String := Path.all;
      File_Extension   : constant String := ".gpr";
   begin
      --  Add ".gpr" extension if not mentionned
      if not GNATCOLL.Utils.Ends_With (Path.all, File_Extension) then
         Free (Path);
         Path := new String'(File_Name & File_Extension);
      end if;

      return GNATCOLL.VFS_Utils.Is_Regular_File (Filesystem_String (Path.all));
   end Project_File_Path_Exists;

   --------------------
   -- Execute_Batch --
   --------------------

   function Execute_Batch
     (Kernel      : access GPS.CLI_Kernels.CLI_Kernel_Record;
      Lang_Name   : String;
      Script_Name : String) return Boolean
   is
      Script : Scripting_Language;
      Errors : Boolean;
   begin
      Script := Kernel.Scripts.Lookup_Scripting_Language (Lang_Name);

      if Script = null then
         return False;
      else
         Execute_File
           (Script   => Script,
            Filename => GNAT.OS_Lib.Normalize_Pathname
                          (Script_Name, Get_Current_Dir.Display_Full_Name),
            Show_Command => False,
            Errors   => Errors);
         return True;
      end if;
   end Execute_Batch;

end GPS.CLI_Utils;
