------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
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

--  Provides a high-level interface to launch commands.
--  This interface is the one that should be shared by scripts, hooks,
--  menus, toolbar buttons, etc.
--
--  See spec of Builder_Facility_Module for an overview of the build system.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.OS_Lib;           use GNAT.OS_Lib;

with Projects;              use Projects;
with Remote;                use Remote;
with Toolchains;            use Toolchains;

with GNATCOLL.Arg_Lists;    use GNATCOLL.Arg_Lists;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with GNATCOLL.Projects;     use GNATCOLL.Projects;

with GPS_Preferences_Types; use GPS_Preferences_Types;

with Build_Configurations;  use Build_Configurations;

package Build_Command_Utils is

   function Get_Server
     (Registry   : Build_Config_Registry_Access;
      Mode       : String;
      Target     : Target_Access) return Server_Type;

   function Get_Mains (Registry : Project_Registry_Access)
   return GNATCOLL.VFS.File_Array;
   --  Return the list of mains corresponding to the loaded project tree.

   function Get_Mode_Subdir
     (Registry : Build_Config_Registry_Access;
      Mode : String) return Filesystem_String;
   --  Return the special directory ("subdir") for Mode

   function Apply_Mode_Args
     (Registry : Build_Config_Registry_Access;
      Model : String;
      Mode : String;
      Cmd_Line : GNAT.OS_Lib.Argument_List)
      return GNAT.OS_Lib.Argument_List_Access;
   --  Applies the mode arguments to the command_line passed as argument.
   --  The returned argument_list should be freed by the caller

   type Abstract_Build_Command_Adapter is abstract tagged private;
   type Abstract_Build_Command_Adapter_Access is access all
      Abstract_Build_Command_Adapter'Class;

   function Get_Last_Main_For_Background_Target
     (Adapter : Abstract_Build_Command_Adapter;
      Target : Target_Access) return Virtual_File is abstract;
   --  Return the Main to use for building Target as a background build.
   --  This is either the last main that was used, if it exists, or the first
   --  main defined for this target, if it exists.
   --  The full path to the target is returned.
   --  If the target is not found, "" is returned.

   function Get_Context_Project
     (Adapter : Abstract_Build_Command_Adapter) return Project_Type;

   function Get_Trusted_Mode_Preference
     (Adapter : Abstract_Build_Command_Adapter) return Boolean;

   function Get_Execute_Command_Preference
     (Adapter : Abstract_Build_Command_Adapter) return String;

   function Get_Multi_Language_Builder
     (Adapter : Abstract_Build_Command_Adapter)
      return Multi_Language_Builder_Policy;

   function Get_Context_Toolchains_Manager
     (Adapter : Abstract_Build_Command_Adapter)
      return Toolchain_Manager;

   function Get_Kernel_Macros_Special_Character
     (Adapter : Abstract_Build_Command_Adapter)
      return Character;
   --  The special character that is found before the macros

   function Get_Context_File_Information
     (Adapter : Abstract_Build_Command_Adapter) return Virtual_File;

   function Get_Kernel_Registry
     (Adapter : Abstract_Build_Command_Adapter) return Project_Registry_Access;

   function Get_Background_Project_Full_Name
     (Adapter : Abstract_Build_Command_Adapter)
        return Filesystem_String is abstract;

   function Substitute
     (Adapter : Abstract_Build_Command_Adapter;
      Param     : String;
      Quoted    : Boolean;
      Done      : access Boolean;
      Server    : Server_Type := GPS_Server;
      For_Shell : Boolean := False) return String is abstract;
   --  Wrapper around GPS.Kernel.Macros.Substitute

   function Get_Scenario_Variables
     (Adapter : Abstract_Build_Command_Adapter)
        return Scenario_Variable_Array is abstract;

   type Console_Message_Type is (Info, Error, Verbose);
   --  We are dealing with 3 types of messages :
   --   - Info for general information
   --   - Error for signaling errors
   --   - Verbose for detailed information

   procedure Console_Insert
     (Adapter : in out Abstract_Build_Command_Adapter;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Console_Message_Type := Info) is abstract;

   procedure Remove_Error_Builder_Message_From_File
     (Adapter : Abstract_Build_Command_Adapter;
      File     : Virtual_File) is abstract;
   --  Removes all messages for specified file in the error category.
   --  Do nothing when there is no such category or file.

   function Get_Background_Environment_File
     (Adapter : Abstract_Build_Command_Adapter)
        return Virtual_File is abstract;

   type Expansion_Result is record
      Args : Arg_List;
      --  The list of arguments

      Dir  : Virtual_File := No_File;
      --  The directory in which to launch the compilation

      Status : Unbounded_String := To_Unbounded_String ("");
   end record;

   function Args_Length (Result : Expansion_Result) return Integer;
   --  Return the length of the arguments. The command is not included in this
   --  count.
   --  Return 0 if there is only a command and no arguments.
   --  Return -1 if the command is empty.

   function Nth_Arg (Result : Expansion_Result; N : Natural) return String;
   --  Return the Nth argument. Nth_Arg (0) returns the command

   function Dir (Result : Expansion_Result) return Virtual_File;
   --  Return the directory in which to launch the compilation

   function Arguments (Result : Expansion_Result) return String;
   --  Return the list of arguments

   function Status (Result : Expansion_Result) return String;
   --  Return the status of Expand_Command_Line

   function Expand_Command_Line
     (Adapter     : Abstract_Build_Command_Adapter_Access;
      CL         : Argument_List;
      Target     : Target_Access;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : Virtual_File;
      Subdir     : Filesystem_String;
      Background : Boolean;
      Simulate   : Boolean) return Expansion_Result;
   --  Expand all macros contained in CL using the GPS macro language.
   --  User must free the result.
   --  CL must contain at least one element.
   --  If Simulate is true, never fail on unknown parameters.

   function Expand_Command_Line
     (Build_Registry   : Build_Config_Registry_Access;
      Proj_Registry    : Project_Registry_Access;
      Proj_Type        : Project_Type;
      Toolchains       : Toolchain_Manager;
      Command_Line     : String;
      Target_Name      : String;
      Mode_Name        : String;
      Project_File     : Virtual_File;
      Force_File       : Virtual_File;
      Main_File        : Virtual_File;
      Simulate         : Boolean;
      Trusted_Mode     : Boolean;
      Multi_Language_Builder : Multi_Language_Builder_Policy;
      Execute_Command  : String
      ) return Expansion_Result;

   procedure Initialize
     (Adapter     : in out Abstract_Build_Command_Adapter'Class;
      Kernel_Registry : Project_Registry_Access;
      Context_Project : Project_Type;
      Context_Toolchains_Manager : Toolchain_Manager;
      Context_File_Information : Virtual_File;
      Kernel_Macros_Special_Character : Character;
      Trusted_Mode_Preference : Boolean;
      Execute_Command_Preference : String;
      Multi_Language_Builder : Multi_Language_Builder_Policy);
   --  fills in one call all members handled by Abstract_Build_Command_Adapter
   --  getters.

private

   type Abstract_Build_Command_Adapter is abstract tagged record

      Kernel_Registry : Project_Registry_Access;

      Context_Project : Project_Type;
      Context_Toolchains_Manager : Toolchain_Manager;
      Context_File_Information : Virtual_File;

      Kernel_Macros_Special_Character : Character;

      Trusted_Mode_Preference : Boolean;
      Execute_Command_Preference : Unbounded_String;
      Multi_Language_Builder : Multi_Language_Builder_Policy;
   end record;

end Build_Command_Utils;
