------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Projects;
with GNATCOLL.Traces;       use GNATCOLL.Traces;

with GPS.Kernel.Charsets;
with GPS.Kernel.Project;

package body GPS.LSP_Client.Configurations.ALS is

   Me_Ada_Support_Diagnostics : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("GPS.LSP.ADA_SUPPORT.DIAGNOSTICS", Off);
   --  Whether to enable diagnostics. Useful in the testsuite.

   Supported_Settings : constant array (Setting_Kind) of Boolean :=
     (Rename_In_Comments => True);

   Settings_Names : constant array (Setting_Kind) of Unbounded_String :=
       (Rename_In_Comments => To_Unbounded_String ("renameInComments"));

   ----------------------------
   -- Configuration_Settings --
   ----------------------------

   overriding function Configuration_Settings
     (Self : ALS_Configuration) return GNATCOLL.JSON.JSON_Value
   is
      Settings     : constant GNATCOLL.JSON.JSON_Value :=
                       GNATCOLL.JSON.Create_Object;
      Ada_Settings : constant GNATCOLL.JSON.JSON_Value :=
                       GNATCOLL.JSON.Create_Object;
      Scenarios    : constant GNATCOLL.JSON.JSON_Value :=
                       GNATCOLL.JSON.Create_Object;

   begin
      Ada_Settings.Set_Field
        ("projectFile",
         GPS.Kernel.Project.Get_Project
           (Self.Kernel).Project_Path.Display_Full_Name);

      --  Set the scenario variables
      for Variable of GPS.Kernel.Project.Scenario_Variables (Self.Kernel) loop
         Scenarios.Set_Field
           (GNATCOLL.Projects.External_Name (Variable),
            GNATCOLL.Projects.Value (Variable));
      end loop;
      for Variable of GPS.Kernel.Project.Untyped_Variables (Self.Kernel) loop
         Scenarios.Set_Field
           (GNATCOLL.Projects.External_Name (Variable),
            GNATCOLL.Projects.Value (Variable));
      end loop;

      Ada_Settings.Set_Field ("scenarioVariables", Scenarios);

      Ada_Settings.Set_Field
        ("defaultCharset", GPS.Kernel.Charsets.Get_Default_Charset);

      --  Deactivate diagnostics for now, to be reactivated in master after
      --  the 20.0 branch.
      Ada_Settings.Set_Field
        ("enableDiagnostics", Active (Me_Ada_Support_Diagnostics));

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
              (To_String (Settings_Names (Setting)), Value.vBoolean);
            Settings.Set_Field ("ada", Ada_Settings);
            return Settings;
      end case;
   end Set_Configuration_Option;

end GPS.LSP_Client.Configurations.ALS;
