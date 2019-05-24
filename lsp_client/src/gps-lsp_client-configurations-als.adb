------------------------------------------------------------------------------
--                                  G P S                                   --
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

with GNATCOLL.Projects;

with GPS.Kernel.Charsets;
with GPS.Kernel.Project;

package body GPS.LSP_Client.Configurations.ALS is

   ----------------------------
   -- Configuration_Settings --
   ----------------------------

   overriding function Configuration_Settings
     (Self : ALS_Configuration) return GNATCOLL.JSON.JSON_Value
   is
      Variables    : constant GNATCOLL.Projects.Scenario_Variable_Array :=
                       GPS.Kernel.Project.Scenario_Variables (Self.Kernel);
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
           (Self.Kernel).Project_Path.Display_Base_Name);
      --  ??? Mush be synchronized with rootPath of Initialize request.

      for Variable of Variables loop
         Scenarios.Set_Field
           (GNATCOLL.Projects.External_Name (Variable),
            GNATCOLL.Projects.Value (Variable));
      end loop;

      Ada_Settings.Set_Field ("scenarioVariables", Scenarios);

      Ada_Settings.Set_Field
        ("defaultCharset", GPS.Kernel.Charsets.Get_Default_Charset);

      Settings.Set_Field ("ada", Ada_Settings);

      return Settings;
   end Configuration_Settings;

end GPS.LSP_Client.Configurations.ALS;
