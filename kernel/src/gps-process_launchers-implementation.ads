------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
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

--  Implementation Process Launcher via GPS.Kernel.Timeout

with GNATCOLL.VFS;

with GPS.Process_Launchers;
with GPS.Core_Kernels;

package GPS.Process_Launchers.Implementation is

   type GPS_Process_Launcher_Record is new
     GPS.Process_Launchers.Process_Launcher_Record with
   record
      Kernel : GPS.Core_Kernels.Core_Kernel;
   end record;

   overriding procedure Launch_Process
     (Launcher             : access GPS_Process_Launcher_Record;
      CL                   : Arg_List;
      Server               : Server_Type := GPS_Server;
      Directory            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Output_Parser        : GPS.Tools_Output.Tools_Output_Parser_Access;
      Show_Command_To      : Messages_Windows.Abstract_Messages_Window_Access;
      Success              : out Boolean);

   overriding procedure Launch_Process_In_Background
     (Launcher             : access GPS_Process_Launcher_Record;
      CL                   : Arg_List;
      Server               : Server_Type := GPS_Server;
      Directory            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Output_Parser        : GPS.Tools_Output.Tools_Output_Parser_Access;
      Show_Command_To      : Messages_Windows.Abstract_Messages_Window_Access;
      Success              : out Boolean;
      Show_In_Task_Manager : Boolean := True;
      Name_In_Task_Manager : String := "";
      Block_Exit           : Boolean := True;
      Created_Command      : out Command_Access);

end GPS.Process_Launchers.Implementation;
