------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with GNATCOLL.VFS;          use GNATCOLL.VFS;

with GPS.Kernel;
with Build_Configurations;  use Build_Configurations;
with Commands;              use Commands;
with Commands.Interactive;  use Commands.Interactive;

package Build_Command_Manager is

   type Dialog_Mode is
     (Force_Dialog, Force_No_Dialog,
      Force_Dialog_Unless_Disabled_By_Target,
      Default);
   --  Force_Dialog means that the dialog should always be displayed
   --  Force_No_Dialog means that the dialog should not be displayed
   --  Force_Dialog_Unless_Disabled_By_Target means that the dialog should
   --    be displayed, unless the target launches with Manually_With_No_Dialog
   --  Default means that the target default should be enforced

   procedure Launch_Target
     (Kernel      : GPS.Kernel.Kernel_Handle;
      Registry    : Build_Config_Registry_Access;
      Target_Name : String;
      Mode_Name   : String;
      Force_File  : Virtual_File;
      Extra_Args  : Argument_List_Access;
      Quiet       : Boolean;
      Synchronous : Boolean;
      Dialog      : Dialog_Mode;
      Main        : Virtual_File;
      Background  : Boolean;
      Directory   : Virtual_File := No_File);
   --  Launch a build of target named Target_Name
   --  If Mode_Name is not the empty string, then the Mode Mode_Name will be
   --  used.
   --  If Force_File is not set to No_File, then force the command to work
   --  on this file. (This is needed to support GPS scripting).
   --  Extra_Args may point to a list of unexpanded args.
   --  If Quiet is true:
   --    - files are not saved before build launch
   --    - the console is not raised when launching the build
   --    - the console is not cleared when launching the build
   --  If Synchronous is True, GPS will block until the command is terminated.
   --  See document of Dialog_Mode for details on Dialog values.
   --  Main, if not empty, indicates the main to build.
   --  If Directory is not empty, indicates which directory the target should
   --  be run under. Default is the project's directory.
   --  If Background, run the compile in the background.

   -------------------
   -- Build_Command --
   -------------------

   --  The Build_Command acts simply as a wrapper around Launch_Target.
   --  Used for defining build actions.

   type Build_Command is new Interactive_Command with record
      Target_Name  : Unbounded_String;
      Main         : Virtual_File;
      Registry     : Build_Config_Registry_Access;
      Kernel       : GPS.Kernel.Kernel_Handle;
      Dialog       : Dialog_Mode;
      Quiet        : Boolean;
   end record;
   type Build_Command_Access is access all Build_Command'Class;

   overriding
   function Execute
     (Command : access Build_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See inherited documentation

   procedure Create
     (Item        : out Build_Command_Access;
      Kernel      : GPS.Kernel.Kernel_Handle;
      Registry    : Build_Config_Registry_Access;
      Target_Name : String;
      Main        : Virtual_File;
      Quiet       : Boolean;
      Dialog      : Dialog_Mode);
   --  Create a build command
   --  Force_Dialog indicates that the command should always be launched
   --  through the interactive dialog.

   ------------------------
   -- Build_Main_Command --
   ------------------------

   --  A command specialized in building a main, when knowing only its number

   type Build_Main_Command is new Interactive_Command with record
      Target_Name  : Unbounded_String;
      Target_Type  : Unbounded_String;
      Main         : Natural;
      Registry     : Build_Config_Registry_Access;
      Kernel       : GPS.Kernel.Kernel_Handle;
      Dialog       : Dialog_Mode;
      Quiet        : Boolean;
   end record;
   type Build_Main_Command_Access is access all Build_Main_Command'Class;

   overriding
   function Execute
     (Command : access Build_Main_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See inherited documentation

   procedure Create
     (Item        : out Build_Main_Command_Access;
      Kernel      : GPS.Kernel.Kernel_Handle;
      Registry    : Build_Config_Registry_Access;
      Target_Name : String;
      Target_Type : String;
      Main        : Natural;
      Quiet       : Boolean;
      Dialog      : Dialog_Mode);
   --  Create a build command
   --  Force_Dialog indicates that the command should always be launched
   --  through the interactive dialog.

end Build_Command_Manager;
