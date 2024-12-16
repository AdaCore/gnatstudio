------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2019-2024, AdaCore                     --
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

with GNAT.Strings;

with VSS.Strings;

with GNATCOLL.Projects;

with GPS.Kernel.Charsets;
with GPS.Kernel.Project;
with GPS.Kernel.Preferences;
with VSS.Strings.Conversions;

package body GPS.LSP_Client.Configurations.ALS is

   Supported_Settings : constant array (Setting_Kind) of Boolean :=
     (Rename_In_Comments => True, Fold_Comments => True);

   Settings_Names :
     constant array (Setting_Kind) of VSS.Strings.Virtual_String :=
       (Rename_In_Comments => "renameInComments",
        Fold_Comments      => "foldComments");

   ----------------------------
   -- Configuration_Settings --
   ----------------------------

   overriding function Configuration_Settings
     (Self : ALS_Configuration) return GNATCOLL.JSON.JSON_Value
   is
      use GNATCOLL.Projects;

      Settings     : constant GNATCOLL.JSON.JSON_Value :=
                       GNATCOLL.JSON.Create_Object;
      Ada_Settings : constant GNATCOLL.JSON.JSON_Value :=
                       GNATCOLL.JSON.Create_Object;
      Scenarios    : constant GNATCOLL.JSON.JSON_Value :=
                       GNATCOLL.JSON.Create_Object;

      Project      : constant Project_Type := GPS.Kernel.Project.Get_Project
        (Self.Kernel);

   begin
      declare
         Build_Tree : constant String := +GPS.Kernel.Project.Get_Registry
           (Self.Kernel).Environment.Build_Tree_Dir;
         Root_Dir   : constant String := +GPS.Kernel.Project.Get_Registry
           (Self.Kernel).Environment.Root_Dir;
      begin
         if Build_Tree /= "" then
            Ada_Settings.Set_Field ("relocateBuildTree", Build_Tree);
         end if;

         if Root_Dir /= "" then
            Ada_Settings.Set_Field ("rootDir", Root_Dir);
         end if;
      end;

      if GPS.Kernel.Project.Get_Registry
        (Self.Kernel).Tree.Status = From_Executable
      then
         --  we are debugging executable so should create a "dummy"
         --  project on the disk
         declare
            F : Writable_File := Write_File (Project.Project_Path);
         begin
            Write (F, "project " & Project.Name & " is" & ASCII.LF);
            Write (F, "   for Source_Dirs use (");
            declare
               Dirs : constant GNATCOLL.VFS.File_Array := Project.Source_Dirs;
            begin
               for Index in Dirs'Range loop
                  Write
                    (F,
                     (if Index = Dirs'First then "" else ",") &
                       ASCII.LF &
                       "     """ & Dirs (Index).Display_Full_Name & """");
               end loop;
            end;
            Write (F, ");" & ASCII.LF);

            declare
               Lang : GNAT.Strings.String_List := Project.Languages;
            begin
               if Lang'Length > 0 then
                  Write (F, "   for Languages use (");

                  for Index in Lang'Range loop
                     Write
                       (F,
                        (if Index = Lang'First then "" else ",") &
                          """" & Lang (Index).all & """");
                     GNAT.Strings.Free (Lang (Index));
                  end loop;
                  Write (F, ");" & ASCII.LF);
               end if;
            end;

            Write (F, "end " & Project.Name & ";");
            Close (F);
         end;
      end if;

      Ada_Settings.Set_Field
        ("projectFile", Project.Project_Path.Display_Full_Name);

      declare
         Conf : constant GNATCOLL.VFS.Virtual_File :=
           GPS.Kernel.Project.Get_Registry
             (Self.Kernel).Environment.Get_Config_File;
      begin
         if Conf /= No_File then
            Ada_Settings.Set_Field
              ("gprConfigurationFile", Conf.Display_Full_Name);
         end if;
      end;

      --  Set the scenario variables
      for Variable of GPS.Kernel.Project.Scenario_Variables (Self.Kernel) loop
         declare
            External : constant String := GNATCOLL.Projects.External_Name
              (Variable);
         begin
            if External /= "" then
               Scenarios.Set_Field
                 (External, GNATCOLL.Projects.Value (Variable));
            end if;
         end;
      end loop;
      for Variable of GPS.Kernel.Project.Untyped_Variables (Self.Kernel) loop
         declare
            External : constant String := GNATCOLL.Projects.External_Name
              (Variable);
         begin
            if External /= "" then
               Scenarios.Set_Field
                 (External, GNATCOLL.Projects.Value (Variable));
            end if;
         end;
      end loop;

      Ada_Settings.Set_Field ("scenarioVariables", Scenarios);

      Ada_Settings.Set_Field
        ("defaultCharset", GPS.Kernel.Charsets.Get_Default_Charset);

      Ada_Settings.Set_Field
        ("enableDiagnostics",
         Boolean'(GPS.Kernel.Preferences.LSP_Ada_Diagnostics.Get_Pref));

      Ada_Settings.Set_Field
        ("projectDiagnostics",
         Boolean'(
           GPS.Kernel.Preferences.LSP_Ada_Project_Diagnostics.Get_Pref));

      Ada_Settings.Set_Field
        ("followSymlinks", not GPS.Kernel.Preferences.Trusted_Mode.Get_Pref);

      Ada_Settings.Set_Field
        ("insertWithClauses",
         Boolean'
           (GPS.Kernel.Preferences.LSP_Ada_Insert_With_Clauses.Get_Pref));

      --  Documentation options

      Ada_Settings.Set_Field
        ("documentationStyle",
         (if GPS.Kernel.Preferences.Doc_Search_Before_First.Get_Pref
          then "leading" else "gnat"));

      Ada_Settings.Set_Field
        ("namedNotationThreshold",
         Integer'(GPS.Kernel.Preferences.LSP_Ada_Param_Threshold.Get_Pref));

      Ada_Settings.Set_Field
        (VSS.Strings.Conversions.To_UTF_8_String
           (Settings_Names (Fold_Comments)),
         Boolean'(GPS.Kernel.Preferences.Fold_Comments.Get_Pref));

      Settings.Set_Field ("ada", Ada_Settings);

      return Settings;
   end Configuration_Settings;

   --------------------------------
   -- Is_Configuration_Supported --
   --------------------------------

   overriding function Is_Configuration_Supported
     (Self    : ALS_Configuration;
      Setting : Setting_Kind)
      return Boolean is
   begin
      return Supported_Settings (Setting);
   end Is_Configuration_Supported;

   ------------------------------
   -- Set_Configuration_Option --
   ------------------------------

   overriding function Set_Configuration_Option
     (Self    : in out ALS_Configuration;
      Setting : Setting_Kind;
      Value   : Configuration_Value) return GNATCOLL.JSON.JSON_Value
   is
      Settings     : constant GNATCOLL.JSON.JSON_Value :=
                       GNATCOLL.JSON.Create_Object;
      Ada_Settings : constant GNATCOLL.JSON.JSON_Value :=
                       GNATCOLL.JSON.Create_Object;

   begin
      if not Self.Is_Configuration_Supported (Setting)
        or else Self.Settings (Setting) = Value
      then
         return GNATCOLL.JSON.JSON_Null;
      end if;

      Self.Settings (Setting) := Value;

      case Value.Kind is
         when None_Type =>
            return GNATCOLL.JSON.JSON_Null;

         when Boolean_Type =>
            Ada_Settings.Set_Field
              (VSS.Strings.Conversions.To_UTF_8_String
                 (Settings_Names (Setting)),
               Value.vBoolean);
            Settings.Set_Field ("ada", Ada_Settings);
            return Settings;
      end case;
   end Set_Configuration_Option;

end GPS.LSP_Client.Configurations.ALS;
