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
with Ada.Text_IO;
with GNAT.OS_Lib;

with GNATCOLL.Utils;
with GPS.Kernel.Console;
with GPS.Kernel.Project; use GPS.Kernel.Project;
with GPS.Kernel.Timeout;
with Projects;

with Code_Peer.Module.Bridge;
with Code_Peer.Shell_Commands;

package body Code_Peer.Module.Codepeer is

   type Codepeer_Context is
     new GPS.Kernel.Timeout.Callback_Data_Record with record
      Module : Code_Peer.Module.Code_Peer_Module_Id;
   end record;

   procedure Create_Library_File (Project : Projects.Project_Type);

   procedure On_Codepeer_Exit
     (Process : GPS.Kernel.Timeout.Process_Data;
      Status  : Integer);

   -------------------------
   -- Create_Library_File --
   -------------------------

   procedure Create_Library_File (Project : Projects.Project_Type) is
      File : Ada.Text_IO.File_Type;

   begin
      Ada.Text_IO.Create
        (File,
         Ada.Text_IO.Out_File,
         +Codepeer_Library_File_Name (Project).Full_Name);

      Ada.Text_IO.Put_Line
        (File,
         "Output_Dir := """
         & (+Codepeer_Output_Directory (Project).Full_Name) & """;");
      Ada.Text_IO.New_Line (File);

      Ada.Text_IO.Put_Line
        (File,
         "Database_Dir := """
         & (+Codepeer_Database_Directory (Project).Full_Name) & """;");
      Ada.Text_IO.New_Line (File);

      Ada.Text_IO.Put_Line
        (File, "Source (Directory              => ""SCIL"",");
      Ada.Text_IO.Put_Line
        (File, "        Include_Subdirectories => True,");
      Ada.Text_IO.Put_Line
        (File, "        Files                  => (""*.scil""),");
      Ada.Text_IO.Put_Line
        (File, "        Language               => SCIL);");

      Ada.Text_IO.Close (File);
   end Create_Library_File;

   ----------------------
   -- On_Codepeer_Exit --
   ----------------------

   procedure On_Codepeer_Exit
     (Process : GPS.Kernel.Timeout.Process_Data;
      Status  : Integer)
   is
      pragma Unreferenced (Status);

      Context : Codepeer_Context'Class
                  renames Codepeer_Context'Class (Process.Callback_Data.all);
      Mode    : constant String :=
                  Code_Peer.Shell_Commands.Get_Build_Mode
                    (GPS.Kernel.Kernel_Handle (Context.Module.Kernel));
      Path : constant GNATCOLL.VFS.File_Array :=
        Projects.Object_Path
          (Get_Project (GPS.Kernel.Kernel_Handle (Context.Module.Kernel)),
           False, False, False);
      CodePeer_Subdir : constant Boolean :=
        GNATCOLL.VFS.Is_Directory
          (Create (Full_Name (Path (Path'First)) & "/codepeer",
                   Get_Host (Path (Path'First))));

   begin
      --  If a codepeer dir is found in the object dir, then use this
      --  directory, otherwise use the default object dir (advanced mode).

      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (GPS.Kernel.Kernel_Handle (Context.Module.Kernel), "codepeer");
      end if;

      Code_Peer.Module.Bridge.Inspection (Context.Module);

      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (GPS.Kernel.Kernel_Handle (Context.Module.Kernel), Mode);
      end if;
   end On_Codepeer_Exit;

   ------------
   -- Review --
   ------------

   procedure Review (Module : Code_Peer.Module.Code_Peer_Module_Id) is
      Project          : constant Projects.Project_Type :=
                           GPS.Kernel.Project.Get_Project (Module.Kernel);
      Object_Directory : constant Virtual_File :=
                           Projects.Object_Path (Project);
      --  ??? the list of arguments should not be hardcoded
      Args             : GNAT.OS_Lib.Argument_List :=
               (1 => new String'("-all"),
                2 => new String'("-global"),
                3 => new String'("-background"),
                4 => new String'("-dbg-on"),
                5 => new String'("ide_progress_bar"),
                6 => new String'("-lib"),
                7 => new String'
                           (+Codepeer_Library_File_Name (Project).Full_Name));
      Success          : Boolean;
      pragma Warnings (Off, Success);

   begin
      Create_Library_File (Project);

      GPS.Kernel.Timeout.Launch_Process
        (Kernel        => GPS.Kernel.Kernel_Handle (Module.Kernel),
         Command       => "codepeer",
         Arguments     => Args,
         Directory     => Object_Directory,
         Console       => GPS.Kernel.Console.Get_Console (Module.Kernel),
         Success       => Success,
         Callback_Data => new Codepeer_Context'(Module => Module),
         Exit_Cb       => On_Codepeer_Exit'Access);
      GNATCOLL.Utils.Free (Args);
   end Review;

end Code_Peer.Module.Codepeer;
