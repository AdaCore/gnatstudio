-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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

with GNAT.OS_Lib;
with GPS.Kernel;
with Build_Configurations;
with String_List_Utils;
with Remote;

package Builder_Facility_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module

   procedure Append_To_Build_Output
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Line   : String;
      Shadow : Boolean);
   --  Register Line as part of the current compilation output
   --  Shadow indicates whether to add it to the normal or the shadow output

   function Get_Build_Output
     (Shadow : Boolean) return String_List_Utils.String_List.List;
   --  Return the last build output.
   --  User should not free the result nor store a pointer to the result, as
   --  this might get invalidated as soon as a new compilation starts.
   --  Shadow indicates whether to get the normal or the shadow output

   function Get_Mains
     (Kernel : GPS.Kernel.Kernel_Handle) return GNAT.OS_Lib.Argument_List;
   --  Return the list of mains corresponding to the currently loaded project
   --  tree.
   --  Caller must free the result.

   function Get_Mode_Subdir (Mode : String) return String;
   --  Return the special directory ("subdir") for Mode

   function Get_Mode_Args
     (Model : String; Mode : String) return GNAT.OS_Lib.Argument_List;
   --  Return the extra args for mode Mode, if the mode applies to model Model
   --  Caller must *not* free the result.

   function Get_List_Of_Modes
     (Model : String) return GNAT.OS_Lib.Argument_List;
   --  Return the list of modes in which to build a target. This means
   --  the current mode, and any shadow mode pertaining to this model.
   --  Caller must free the result;

   procedure Activate_Mode (Mode : String; Active : Boolean);
   --  Activate or deactivate Mode.
   --  By default, modes are not active.

   function Is_Server_In_Mode (Mode : String) return Boolean;
   --  Return if a server is defined for the mode.

   function Get_Mode_Server (Mode : String) return Remote.Server_Type;
   --  Return the server defined for the mode.

private

   function Registry return Build_Configurations.Build_Config_Registry_Access;
   --  Return the registry stored in the module

end Builder_Facility_Module;
