------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2020, AdaCore                   --
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

with GNATCOLL.Projects;
with GNATCOLL.Traces;   use GNATCOLL.Traces;

with GPS.Kernel.Charsets;
with GPS.Kernel.Project;

package body GPS.LSP_Client.Configurations.ALS is

   Me_Ada_Support_Diagnostics : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("GPS.LSP.ADA_SUPPORT.DIAGNOSTICS", Off);
   --  Whether to enable diagnostics. Useful in the testsuite.

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

end GPS.LSP_Client.Configurations.ALS;
