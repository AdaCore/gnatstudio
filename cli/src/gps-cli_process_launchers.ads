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

--  This package defines process launcher for command line interface tool.

with GNATCOLL.Arg_Lists;               use GNATCOLL.Arg_Lists;
with GNATCOLL.VFS;

with Commands;                         use Commands;
with GPS.Messages_Windows;             use GPS.Messages_Windows;
with GPS.Process_Launchers;
with GPS.Tools_Output;                 use GPS.Tools_Output;
with Remote;                           use Remote;

package GPS.CLI_Process_Launchers is

   type CLI_Process_Launcher_Record is
     new GPS.Process_Launchers.Process_Launcher_Record with null record;

   overriding procedure Launch_Process
     (Launcher             : access CLI_Process_Launcher_Record;
      CL                   : Arg_List;
      Server               : Server_Type := GPS_Server;
      Directory            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Output_Parser        : GPS.Tools_Output.Tools_Output_Parser_Access;
      Show_Command_To      : Abstract_Messages_Window_Access := null;
      Success              : out Boolean);

   overriding procedure Launch_Process_In_Background
     (Launcher             : access CLI_Process_Launcher_Record;
      CL                   : Arg_List;
      Server               : Server_Type := GPS_Server;
      Directory            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Output_Parser        : GPS.Tools_Output.Tools_Output_Parser_Access;
      Show_Command_To      : Abstract_Messages_Window_Access := null;
      Success              : out Boolean;
      Show_In_Task_Manager : Boolean := True;
      Name_In_Task_Manager : String := "";
      Block_Exit           : Boolean := True;
      Created_Command      : out Command_Access);

end GPS.CLI_Process_Launchers;
