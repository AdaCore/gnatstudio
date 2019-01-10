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

--  Provides a high-level interface to launch commands.
--  This interface is the one that should be shared by scripts, hooks,
--  menus, toolbar buttons, etc.
--
--  See spec of Builder_Facility_Module for an overview of the build system.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

with GPS.Kernel.Messages;

with Command_Lines;          use Command_Lines;
with Commands;               use Commands;
with Commands.Interactive;   use Commands.Interactive;
with Build_Command_Utils;    use Build_Command_Utils;
with Build_Configurations;   use Build_Configurations;
with Extending_Environments; use Extending_Environments;
with Interactive_Consoles;   use Interactive_Consoles;
with Remote;                 use Remote;

package Build_Command_Manager is

   Builder_Message_Flags    : constant GPS.Kernel.Messages.Message_Flags :=
     GPS.Kernel.Messages.Side_And_Locations;
   Background_Message_Flags : constant GPS.Kernel.Messages.Message_Flags :=
     GPS.Kernel.Messages.Sides_Only;

   function Get_Build_Console
     (Kernel              : GPS.Kernel.Kernel_Handle;
      Shadow              : Boolean;
      Background          : Boolean;
      Create_If_Not_Exist : Boolean;
      New_Console_Name    : String := "";
      Toolbar_Name        : String := "") return Interactive_Console;
   --  Return the console appropriate for showing compiler errors
   --  If New_Console_Name is specified, create a new console with this name.

   function Expand_Command_Line
     (Builder    : Builder_Context;
      CL         : Command_Line;
      Target     : Target_Access;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : Virtual_File;
      Main_Project : Project_Type;
      Subdir     : Filesystem_String;
      Background : Boolean;
      Simulate   : Boolean;
      Background_Env : Extending_Environment) return Expansion_Result;
   --  Expand all macros contained in CL using the GPS macro language.
   --  CL must contain at least one element.
   --  If Simulate is true, never fail on unknown parameters.

   -------------------
   -- Build_Command --
   -------------------

   --  The Build_Command acts simply as a wrapper around Launch_Target.
   --  Used for defining build actions.

   type Build_Command is new Interactive_Command with record
      Target_Name  : Unbounded_String;
      Main         : Virtual_File;
      Main_Project : Project_Type;
      Dialog       : Dialog_Mode;
      Quiet        : Boolean;
      Builder      : Builder_Context;
   end record;
   type Build_Command_Access is access all Build_Command'Class;

   overriding function Execute
     (Command : access Build_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See inherited documentation

   procedure Create
     (Item        : out Build_Command_Access;
      Builder     : Builder_Context;
      Target_Name : String;
      Main        : Virtual_File;
      Main_Project : Project_Type;
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
      Dialog       : Dialog_Mode;
      Quiet        : Boolean;
      Builder      : Builder_Context;
   end record;
   type Build_Main_Command_Access is access all Build_Main_Command'Class;

   overriding
   function Execute
     (Command : access Build_Main_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See inherited documentation

   procedure Create
     (Item        : out Build_Main_Command_Access;
      Builder     : Builder_Context;
      Target_Name : String;
      Target_Type : String;
      Main        : Natural;
      Quiet       : Boolean;
      Dialog      : Dialog_Mode);
   --  Create a build command
   --  Force_Dialog indicates that the command should always be launched
   --  through the interactive dialog.

end Build_Command_Manager;
