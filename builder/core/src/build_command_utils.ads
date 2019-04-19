------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with GNAT.OS_Lib;                      use GNAT.OS_Lib;

with GNATCOLL.Arg_Lists;               use GNATCOLL.Arg_Lists;
with GNATCOLL.VFS;                     use GNATCOLL.VFS;
with GNATCOLL.Projects;                use GNATCOLL.Projects;
with GNATCOLL.Scripts;                 use GNATCOLL.Scripts;

with GPS.Core_Kernels;                 use GPS.Core_Kernels;
with GPS.Messages_Windows;             use GPS.Messages_Windows;

with Build_Configurations;             use Build_Configurations;
with Command_Lines;                    use Command_Lines;
with Commands;                         use Commands;
with Extending_Environments;           use Extending_Environments;
with GPS_Preferences_Types;            use GPS_Preferences_Types;
with Projects;                         use Projects;
with Remote;                           use Remote;
with Toolchains;                       use Toolchains;

package Build_Command_Utils is

   type Dialog_Mode is
     (Force_Dialog, Force_No_Dialog,
      Force_Dialog_Unless_Disabled_By_Target,
      Default);
   --  Force_Dialog means that the dialog should always be displayed
   --  Force_No_Dialog means that the dialog should not be displayed
   --  Force_Dialog_Unless_Disabled_By_Target means that the dialog should
   --    be displayed, unless the target launches with Manually_With_No_Dialog
   --  Default means that the target default should be enforced

   function Get_Server
     (Registry   : Build_Config_Registry_Access;
      Mode       : String;
      Target     : Target_Access) return Server_Type;

   type Project_And_Main is record
      Project_Path : GNATCOLL.VFS.Virtual_File;
      Main         : GNATCOLL.VFS.Virtual_File;
   end record;
   type Project_And_Main_Array is array (Positive range <>)
     of Project_And_Main;

   function Get_Project (P : Project_And_Main) return Project_Type;
   --  Return the project from P

   function Get_Mains
     (Registry : Project_Registry_Access) return Project_And_Main_Array;
   --  Return the list of mains corresponding to the loaded project tree.

   function Get_Mains_Files_Only (Registry : Project_Registry_Access)
   return GNATCOLL.VFS.File_Array;
   --  Return the list of mains corresponding to the loaded project tree.

   function Get_Mode_Subdir
     (Registry : Build_Config_Registry_Access;
      Mode : String) return Filesystem_String;
   --  Return the special directory ("subdir") for Mode

   type Abstract_Build_Command_Adapter is abstract tagged private;
   --  This type provides values to expand macros in command arguments.
   --  Actual expansion done in Expand_Command_Line subprogram

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

   function Get_Untyped_Variables
     (Adapter : Abstract_Build_Command_Adapter)
      return Untyped_Variable_Array is abstract;

   procedure Console_Insert
     (Adapter : in out Abstract_Build_Command_Adapter;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info) is abstract;

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
     (Adapter    : Abstract_Build_Command_Adapter_Access;
      Cmd_Line   : Command_Line;
      Target     : Target_Access;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : Virtual_File;
      Main_Project : Project_Type;
      Subdir     : Filesystem_String;
      Background : Boolean;
      Simulate   : Boolean) return Expansion_Result;
   --  Expand all macros contained in CL using the GPS macro language.
   --  User must free the result.
   --  CL must contain at least one element.
   --  If Simulate is true, never fail on unknown parameters.

   procedure Initialize
     (Adapter                    : in out Abstract_Build_Command_Adapter'Class;
      Kernel                     : not null access Core_Kernel_Record'Class;
      Context_Project                 : Project_Type;
      Context_Toolchains_Manager      : Toolchain_Manager;
      Context_File_Information        : Virtual_File;
      Kernel_Macros_Special_Character : Character;
      Trusted_Mode_Preference         : Boolean;
      Execute_Command_Preference      : String;
      Multi_Language_Builder          : Multi_Language_Builder_Policy);
   --  fills in one call all members handled by Abstract_Build_Command_Adapter
   --  getters.

   type Builder_Context_Record is new Abstract_Module_Record with private;
   --  Builder context stores set of last main for launched targets and
   --  background identifier for last launched command.
   type Builder_Context is access all Builder_Context_Record;

   procedure Initialize
     (Self     : access Builder_Context_Record'Class;
      Kernel   : GPS.Core_Kernels.Core_Kernel;
      Registry : Build_Config_Registry_Access);
   --  Initialize Builder_Context_Record members

   function Registry
     (Self : access Builder_Context_Record)
      return Build_Config_Registry_Access;
   --  Return build config registry associated with context

   function Kernel
     (Self : access Builder_Context_Record)
      return GPS.Core_Kernels.Core_Kernel;
   --  Return kernel associated with context

   ------------------
   -- Build Output --
   ------------------

   procedure Append_To_Build_Output
     (Self       : access Builder_Context_Record;
      Line       : String;
      Target     : String;
      Shadow     : Boolean;
      Background : Boolean);
   --  Register Line as part of the current compilation output
   --  Shadow indicates whether to add it to the normal or the shadow output

   function Get_Build_Output
     (Self       : access Builder_Context_Record;
      Target     : String;
      Shadow     : Boolean;
      Background : Boolean) return Unbounded_String;
   --  Return the last build output.
   --  Shadow indicates whether to get the normal or the shadow output
   --  If Target is null, get all output in the category.

   procedure Clear_Build_Output
     (Self       : access Builder_Context_Record;
      Shadow     : Boolean;
      Background : Boolean);
   --  Clear all saved build output

   package Target_Outputs is new Ada.Containers.Hashed_Maps
     (Unbounded_String, Unbounded_String, Ada.Strings.Unbounded.Hash, "=");

   function Clear_All_Build_Output
     (Self       : access Builder_Context_Record;
      Shadow     : Boolean;
      Background : Boolean) return Target_Outputs.Cursor;

   -----------------
   -- Latest main --
   -----------------

   --  Storing the latest Main on which a target was launched is useful
   --  for launching background commands working on mains

   procedure Set_Last_Main
     (Self   : access Builder_Context_Record;
      Target : String;
      Main   : Virtual_File);
   function Get_Last_Main
     (Self   : access Builder_Context_Record;
      Target : String) return Virtual_File;
   --  Get/Set the last main that was actually used when launching a manual
   --  build for Target

   function Get_List_Of_Modes
     (Current  : String;
      Registry : Build_Config_Registry_Access;
      Model    : String) return GNAT.OS_Lib.Argument_List;
   --  Return the list of modes in which to build a target. This means
   --  the Current mode, and any shadow mode pertaining to this model.
   --  Caller must free the result;

   --------------------------
   -- Background build ids --
   --------------------------

   --  For background builds, we do not want to erase the messages of build N-1
   --  until the end of build N, since that would create annoying highlighting
   --  removal and additions as the user types. To support this we introduce
   --  the notion of background build ID.

   function Previous_Background_Build_Id
     (Self : access Builder_Context_Record) return String;
   --  Return the ID of the previous background build

   function Current_Background_Build_Id
     (Self : access Builder_Context_Record) return String;
   --  Return the ID of the current background build

   procedure Background_Build_Finished (Self : access Builder_Context_Record);
   --  Inform the module that a background build has finished

   procedure Background_Build_Started
     (Self    : access Builder_Context_Record;
      Command : Command_Access);
   --  Inform the module that a background build has started, controlled by
   --  Command.

   procedure Interrupt_Background_Build
     (Self    : access Builder_Context_Record;
      Command : out Command_Access);
   --  Interrupt the currently running background build

   -----------------
   --  Last build --
   -----------------

   type Build_Information is record
      Target          : Target_Access;
      Main            : Virtual_File;
      Main_Project    : Project_Type;
      Force_File      : Virtual_File;
      Env             : Extending_Environment;
      Category        : Unbounded_String;
      Mode            : Unbounded_String;
      Background      : Boolean;
      Shadow          : Boolean;
      Quiet           : Boolean;
      Preserve_Output : Boolean;
      Console         : Abstract_Messages_Window_Access;
      Full            : Expansion_Result;
      Extra_Args      : Argument_List_Access;
      Dialog          : Dialog_Mode;
      Via_Menu        : Boolean;
      Launch          : Boolean;

      On_Exit         : Subprogram_Type := null;
      --  The scripting subprogram which should be called at the end of the
      --  build.
   end record;

   function Get_Last_Build
     (Self : access Builder_Context_Record) return Build_Information;
   procedure Set_Last_Build
     (Self   : access Builder_Context_Record;
      Build  : Build_Information);
   --  Get/Set the last built target

   overriding procedure Destroy (Self : in out Builder_Context_Record);
   --  Cleanup internal data

   function Expand_Command_Line
     (Builder      : Builder_Context;
      CL           : Command_Line;
      Target       : Target_Access;
      Server       : Server_Type;
      Force_File   : Virtual_File;
      Main         : Virtual_File;
      Main_Project : Project_Type;
      Subdir       : Filesystem_String;
      Background   : Boolean;
      Simulate     : Boolean) return Expansion_Result;
   --  Expand command line CL using trivial Build_Command_Adapter.

   function Expand_Command_Line
     (Build_Registry   : Build_Config_Registry_Access;
      Kernel           : not null access Core_Kernel_Record'Class;
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
      Execute_Command  : String)
     return Expansion_Result;
   --  Expand command line CL compatible with gnatbench objects.
   --  Do not remove, this function is needed for GNAT Bench

private
   type Abstract_Build_Command_Adapter is abstract tagged record
      Kernel          : access Core_Kernel_Record'Class;

      Context_Project : Project_Type;
      Context_Toolchains_Manager : Toolchain_Manager;
      Context_File_Information : Virtual_File;

      Kernel_Macros_Special_Character : Character;

      Trusted_Mode_Preference : Boolean;
      Execute_Command_Preference : Unbounded_String;
      Multi_Language_Builder : Multi_Language_Builder_Policy;
   end record;

   package Files is new Ada.Containers.Hashed_Maps
     (Unbounded_String, Virtual_File, Ada.Strings.Unbounded.Hash, "=");

   type Target_Output_Type is
     (Normal_Output, Background_Output, Shadow_Output);

   type Target_Output_Array is array (Target_Output_Type) of
     Target_Outputs.Map;

   type Builder_Context_Record is new Abstract_Module_Record with record
      Kernel : GPS.Core_Kernels.Core_Kernel;
      --  Kernel handle
      Registry : Build_Config_Registry_Access;
      --  Build Config Registry
      Last_Mains : Files.Map;
      --  The last launched main
      Background_Build_ID      : Integer := 0;
      --  The ID of the current background build.
      Background_Build_Command : Command_Access;
      --  The command holding the background build.
      Outputs : Target_Output_Array;
      --  Save output for target builds
      Build : Build_Information;
      --  The last build target
   end record;

end Build_Command_Utils;
