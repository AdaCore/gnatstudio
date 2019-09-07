------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2013-2019, AdaCore                  --
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

--  Abstract root type for launching external processes.

with GNATCOLL.Arg_Lists;               use GNATCOLL.Arg_Lists;
with GNATCOLL.VFS;

with Commands;                         use Commands;
with GPS.Messages_Windows;             use GPS.Messages_Windows;
with GPS.Tools_Output;                 use GPS.Tools_Output;
with Remote;                           use Remote;

package GPS.Process_Launchers is

   type Process_Launcher_Record is abstract tagged null record;
   type Process_Launcher is access all Process_Launcher_Record'Class;

   procedure Launch_Process
     (Launcher             : access Process_Launcher_Record;
      CL                   : Arg_List;
      Server               : Server_Type := GPS_Server;
      Directory            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Output_Parser        : GPS.Tools_Output.Tools_Output_Parser_Access;
      Show_Command_To      : Abstract_Messages_Window_Access := null;
      Success              : out Boolean) is abstract;

   --  Launch a given CL command with arguments on Server.
   --  Arguments must be freed by the user.
   --
   --  Set Success to True if the command could be spawned.
   --
   --  All data is available from the process streams to Output_Parser.
   --  When the underlying process dies Output_Parser.End_Of_Stream is called.
   --  Output_Parser is freed automatically when the process terminates.
   --
   --  If Show_Command_To not null the command itself is displayed in
   --  the Show_Command_To console.
   --
   --  If Directory is not empty, move to Dir before launching the command,
   --  and change back to the current directory once the command is spawned.
   --  This applies to the local host, not the remote host.

   --  This procedure waits for the process to complete.

   procedure Launch_Process_In_Background
     (Launcher             : access Process_Launcher_Record;
      CL                   : Arg_List;
      Server               : Server_Type := GPS_Server;
      Directory            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Output_Parser        : GPS.Tools_Output.Tools_Output_Parser_Access;
      Show_Command_To      : Abstract_Messages_Window_Access := null;
      Success              : out Boolean;
      Show_In_Task_Manager : Boolean := True;
      Name_In_Task_Manager : String := "";
      Block_Exit           : Boolean := True;
      Created_Command      : out Command_Access) is abstract;
   --  This procedure is like Launch_Process but returns immediately after
   --  spawning process.
   --
   --  Show_In_Task_Manager tells whether the underlying command should be
   --  visible in the task manager. If Name_In_Task_Manager is different from
   --  the empty string, then this command will have this name, else the
   --  command will be used.
   --
   --  Block_Exit indicates whether the fact that this process is running
   --  should prevent GPS from closing.
   --
   --  It returns the created command in Created_Command parameter.

end GPS.Process_Launchers;
