------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

--  Organisation of the builder module.
--
--              clicks on toolbar,     scripts (2)   internal hooks
--             menu or key binding (1)    |        such as file save (1)
--                               |        |            |
--                               |        |            |
--                               |        |            |            Data type
--                               |        |            |             handled:
--                               |        |            |      -+
--                               |        |            |       |
--     Config GUI (6)            |        |            |       |  a target name
--           |                   v        v            v       |
--           |          High-level build command manager (3)  -+
--           v                        |                        | a command line
--     Config data (4)----------------+                        | with macros
--                                    |                        |
--            Macro expander (7)------+                       -+
--                                    |                        | an expanded
--                                    v                        | command line
--               Generation of a Build Command (5)            -+
--                                    |                        |
--                                    v                        | a GPS command
--               Command is launched in the Task_Manager       | encapsulating
--                                    |                        | a process and
--                                    v                        | its output
--                      Command takes care of its output      -+
--
--   (1) GUI elements registered in Builder_Facility_Module
--   (2) defined in Builder_Facility_Module.Scripts
--   (3) defined in Build_Command_Manager
--   (4) data defined in Build_Configurations, and launched through actions
--       defined in Builder_Facility_Module
--   (5) commands defined in Commands.Builder
--   (6) GUI defined in Build_Configurations.GtkAda
--   (7) in GPS.Kernel.Macros
--
--   Dependencies
--              (4) depends only on XmlAda
--              (6) depends on (4) and on GtkAda
--  (1) (2) (3) (5) depend on GPS internals
--
--  --------------------------------------------
--  Syntax of the XML to describes mode
--     <builder-mode name="NAME">
--        <description>DESCRIPTION</description>
--        <subdir>SUBDIR</subdir>
--        <shadow>SHADOW</shadow>
--        <supported-model>MODEL_1</supported-model>
--                    (...)
--        <supported-model>MODEL_N</supported-model>
--        <extra-args>
--           <arg>ARG_1</arg>
--               (...)
--           <arg>ARG_N</arg>
--        </extra-args>
--
--    Where:
--          NAME         is the name of the mode (displayed in the combo entry)
--          DESCRIPTION  a description of the model (displayed in the tooltip)
--          SUBDIR       is a base name of a subdirectory (for object and
--                        exec dirs, for instance) to use when this mode is
--                        active. This gets substituted to the %subdir argument
--          SHADOW      (optional, default False) a shadow mode is a mode that
--                       has the following properties:
--                         - it does not appear in the graphical elements
--                         - whenever a target is launched, if the shadow
--                           mode applies to the model of that target, then
--                           the target is launched again immediately with the
--                           extra arguments of the shadow project. This is
--                           done only if the mode is Active (see
--                            Activate/Deactivate_Mode below)
--                         - a shadow project always launches targets in Quiet
--                           mode
--          MODEL_X      is a model supported by the mode
--          ARG_1..N     are the extra arguments appended to the command line
--
--    As a convention, if there is no <supported-model> node, this means that
--    the mode supports all models.
--
--  --------------------------------------------
--
--  Items under the responsibility of Builder_Module
--
--   The Run... Menu
--   The loading of Xref

with GPS.Kernel;
with Build_Configurations;  use Build_Configurations;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.VFS;          use GNATCOLL.VFS;
with Build_Command_Utils;   use Build_Command_Utils;

package Builder_Facility_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module

   procedure Activate_Mode (Mode : String; Active : Boolean);
   --  Activate or deactivate Mode.
   --  By default, modes are not active.

   procedure Set_Subdir (Mode : String; Subdir : String);
   --  Modifies the Mode's subdir value.

   function Registry return Build_Configurations.Build_Config_Registry_Access;
   --  Return the registry stored in the module

   function Builder return Builder_Context;
   --  Return the builder context stored in the module

   procedure Refresh_All_Graphical_Elements;
   --  Recompute the menus and toolbar icons, based on the targets described
   --  in the Registry.

   procedure Refresh_Graphical_Elements (Target : Target_Access);
   --  Recompute the menu and toolba button only for the given target.

   procedure Save_Targets;
   procedure Load_Targets;
   --  Save/Load the targets in the user-defined XML

end Builder_Facility_Module;
