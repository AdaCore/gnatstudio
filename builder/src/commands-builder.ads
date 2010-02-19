-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2010, AdaCore                  --
--                                                                  --
-- GPS is free software; you can redistribute it and/or modify  it   --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package handles build commands

with GPS.Kernel;
with Glib;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with Remote;       use Remote;
with Interactive_Consoles; use Interactive_Consoles;
with GNATCOLL.Arg_Lists;        use GNATCOLL.Arg_Lists;

package Commands.Builder is

   Error_Category   : constant String := "Builder results";
   --  -"Builder results"
   Warning_Category : constant String := "Builder warnings";
   --  -"Builder warnings"
   Style_Category   : constant String := "Style errors";
   --  -"Style errors"
   Shadow_Category  : constant String := "Syntax check";
   --  -"Syntax check"

   function Target_Name_To_Locations_Category (Name : String) return String;
   --  Return the name of the locations category associated with the build of
   --  target Name.

   procedure Process_Builder_Output
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : Commands.Command_Access;
      Output  : Glib.UTF8_String;
      Quiet   : Boolean;
      Shadow  : Boolean;
      Target  : String := "");
   --  Process the builder output: update the progress bar in Command as
   --  necessary, hide the progress output, and display the other outputs in
   --  the console. Error messages are displayed in the locations window.
   --
   --  If Quiet is False, output will be displayed in the Messages window
   --  Output can contain multiple lines.
   --
   --  Target indicates the name of the target being built.

   procedure Launch_Build_Command
     (Kernel        : GPS.Kernel.Kernel_Handle;
      CL            : Arg_List;
      Target_Name   : String;
      Mode_Name     : String;
      Category_Name : String := Error_Category;
      Server        : Server_Type;
      Quiet         : Boolean;
      Shadow        : Boolean;
      Synchronous   : Boolean;
      Use_Shell     : Boolean;
      New_Console   : Boolean;
      Directory     : Virtual_File);
   --  Launch a build command.
   --  CL is the command line. The first item in CL should be the executable
   --  and the rest are arguments.
   --  Target_Name is the name of the target being launched.
   --  Category_Name is the name of the target category being launched.
   --  If Use_Shell, and if the SHELL environment variable is defined,
   --  then call the command through $SHELL -c "command line".
   --  If New_Console, create a new interactive console to communicate with
   --  the process.
   --  See Build_Command_Manager.Launch_Target for the meanings of Quiet and
   --  Synchronous.

   procedure Display_Compiler_Message
     (Kernel  : GPS.Kernel.Kernel_Handle;
      Message : String;
      Shadow  : Boolean);
   --  Display Message

   function Get_Build_Console
     (Kernel              : GPS.Kernel.Kernel_Handle;
      Shadow              : Boolean;
      Create_If_Not_Exist : Boolean;
      New_Console_Name    : String := "") return Interactive_Console;
   --  Return the console appropriate for showing compiler errors
   --  If New_Console_Name is specified, create a new console with this name.

end Commands.Builder;
