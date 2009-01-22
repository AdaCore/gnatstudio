-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

with Ada.Directories;
with GNAT.OS_Lib;

with GNATCOLL.Utils;
with GPS.Kernel.Console;
with GPS.Kernel.Project;
with GPS.Kernel.Timeout;
with Projects;

with Code_Peer.Bridge.Commands;

package body Code_Peer.Module.Bridge is

   type Bridge_Context is
     new GPS.Kernel.Timeout.Callback_Data_Record with record
      Module    : Code_Peer.Module.Code_Peer_Module_Id;
      File_Name : GNAT.Strings.String_Access;
   end record;

   overriding procedure Destroy (Data : in out Bridge_Context);

   procedure On_Bridge_Exit
     (Process : GPS.Kernel.Timeout.Process_Data;
      Status  : Integer);
   --  Called when gps_codepeer_bridge program execution is done

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Data : in out Bridge_Context) is
   begin
      GNAT.Strings.Free (Data.File_Name);
   end Destroy;

   --------------------
   -- On_Bridge_Exit --
   --------------------

   procedure On_Bridge_Exit
     (Process : GPS.Kernel.Timeout.Process_Data;
      Status  : Integer)
   is
      Context : Bridge_Context'Class
      renames Bridge_Context'Class (Process.Callback_Data.all);

   begin
      if Status = 0 then
         Context.Module.Load (Context.File_Name.all);
      end if;
   end On_Bridge_Exit;

   ----------------
   -- Inspection --
   ----------------

   procedure Inspection (Module : Code_Peer.Module.Code_Peer_Module_Id) is
      Project            : constant Projects.Project_Type :=
        GPS.Kernel.Project.Get_Project (Module.Kernel);
      Project_Name       : constant String := Projects.Project_Name (Project);
      Object_Directory   : constant String :=
        Projects.Object_Path (Project, False);
      Output_Directory   : constant String :=
        Ada.Directories.Compose
          (Object_Directory, Project_Name, "output");
      Command_File_Name  : constant String :=
        Ada.Directories.Compose
          (Object_Directory, "bridge_in", "xml");
      Reply_File_Name    : constant String :=
        Ada.Directories.Compose
          (Object_Directory, "bridge_out", "xml");
      Args               : GNAT.OS_Lib.Argument_List :=
        (1 => new String'(Command_File_Name));
      Success            : Boolean;
      pragma Warnings (Off, Success);

   begin
      --  Generate command file

      Code_Peer.Bridge.Commands.Inspection
        (Command_File_Name, Output_Directory, Reply_File_Name);

      --  Run gps_codepeer_bridge

      GPS.Kernel.Timeout.Launch_Process
        (Kernel        => GPS.Kernel.Kernel_Handle (Module.Kernel),
         Command       => "gps_codepeer_bridge",
         Arguments     => Args,
         Console       => GPS.Kernel.Console.Get_Console (Module.Kernel),
         Directory     => Object_Directory,
         Callback_Data =>
         new Bridge_Context'(Module, new String'(Reply_File_Name)),
         Success       => Success,
         Exit_Cb       => On_Bridge_Exit'Access);
      GNATCOLL.Utils.Free (Args);
   end Inspection;

end Code_Peer.Module.Bridge;
