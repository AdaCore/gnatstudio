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

with GNAT.OS_Lib;

with GNATCOLL.Utils;
with GNATCOLL.VFS;       use GNATCOLL.VFS;

with GPS.Kernel.Console; use GPS.Kernel;
with GPS.Kernel.Project;
with GPS.Kernel.Timeout;
with GPS.Intl;           use GPS.Intl;
with Projects;

with Code_Peer.Bridge.Commands;
with Code_Peer.Shell_Commands;

package body Code_Peer.Module.Bridge is

   type Bridge_Mode is (Add_Audit, Audit_Trail, Inspection);

   type Bridge_Context (Mode : Bridge_Mode) is
     new GPS.Kernel.Timeout.Callback_Data_Record with record
      Module    : Code_Peer.Module.Code_Peer_Module_Id;
      File_Name : Virtual_File;

      case Mode is
         when Inspection =>
            null;

         when Add_Audit | Audit_Trail =>
            Message : Code_Peer.Message_Access;
      end case;
   end record;

   overriding procedure Destroy (Data : in out Bridge_Context);

   procedure On_Bridge_Exit
     (Process : GPS.Kernel.Timeout.Process_Data;
      Status  : Integer);
   --  Called when gps_codepeer_bridge program execution is done

   ----------------------
   -- Add_Audit_Record --
   ----------------------

   procedure Add_Audit_Record
     (Module  : Code_Peer.Module.Code_Peer_Module_Id;
      Message : Code_Peer.Message_Access)
   is
      Project           : constant Projects.Project_Type :=
                            GPS.Kernel.Project.Get_Project (Module.Kernel);
      Object_Directory  : constant Virtual_File :=
                            Projects.Object_Path (Project);
      Command_File_Name : constant Virtual_File :=
                            Create_From_Dir
                              (Object_Directory, "bridge_in.xml");
      Args              : GNAT.OS_Lib.Argument_List :=
                            (1 => new String'
                               (+Command_File_Name.Full_Name));
      Mode              : constant String :=
                            Code_Peer.Shell_Commands.Get_Build_Mode
                              (Kernel_Handle (Module.Kernel));
      CodePeer_Subdir   : constant Boolean :=
                            Use_CodePeer_Subdir
                              (Kernel_Handle (Module.Kernel));
      Success           : Boolean;
      pragma Warnings (Off, Success);

   begin
      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), "codepeer");
      end if;

      --  Generate command file

      if Message.Audit.First_Element.Probability_Changed then
         Code_Peer.Bridge.Commands.Add_Audit_Record
           (Command_File_Name,
            Codepeer_Output_Directory (Project),
            Message.Id,
            True,
            Message.Audit.First_Element.Probability,
            Message.Audit.First_Element.Comment.all);

      else
         Code_Peer.Bridge.Commands.Add_Audit_Record
           (Command_File_Name,
            Codepeer_Output_Directory (Project),
            Message.Id,
            False,
            Code_Peer.High,
            Message.Audit.First_Element.Comment.all);
      end if;

      --  Run gps_codepeer_bridge

      GPS.Kernel.Timeout.Launch_Process
        (Kernel        => GPS.Kernel.Kernel_Handle (Module.Kernel),
         Command       => "gps_codepeer_bridge",
         Arguments     => Args,
         Console       => GPS.Kernel.Console.Get_Console (Module.Kernel),
         Directory     => Object_Directory,
         Callback_Data =>
           new Bridge_Context'(Add_Audit, Module, No_File, Message),
         Success       => Success,
         Exit_Cb       => On_Bridge_Exit'Access);
      GNATCOLL.Utils.Free (Args);

      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
      end if;
   end Add_Audit_Record;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Data : in out Bridge_Context) is
   begin
      null;
   end Destroy;

   ----------------
   -- Inspection --
   ----------------

   procedure Inspection (Module : Code_Peer.Module.Code_Peer_Module_Id) is
      Project            : constant Projects.Project_Type :=
                             GPS.Kernel.Project.Get_Project (Module.Kernel);
      Object_Directory   : constant Virtual_File :=
                             Projects.Object_Path (Project);
      Command_File_Name  : constant Virtual_File :=
                             Create_From_Dir
                               (Object_Directory, "bridge_in.xml");
      Reply_File_Name    : constant Virtual_File :=
                             Create_From_Dir
                               (Object_Directory, "bridge_out.xml");
      Args               : GNAT.OS_Lib.Argument_List (1 .. 1);
      Output_Directory   : constant Virtual_File :=
                             Codepeer_Output_Directory (Project);
      Success            : Boolean;
      pragma Warnings (Off, Success);

   begin
      if not Is_Directory (Output_Directory) then
         Console.Insert
           (Module.Kernel,
            -"cannot find CodePeer output directory: " &
            Output_Directory.Display_Full_Name,
            Mode => Console.Error);
         return;
      end if;

      Args (1) := new String'(+Command_File_Name.Full_Name);
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
         new Bridge_Context'
           (Inspection, Module, Reply_File_Name),
         Success       => Success,
         Exit_Cb       => On_Bridge_Exit'Access);
      GNATCOLL.Utils.Free (Args);
   end Inspection;

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
         case Context.Mode is
            when Inspection =>
               Context.Module.Load (Context.File_Name);

            when Audit_Trail =>
               Context.Module.Review_Message
                 (Context.Message, Context.File_Name);

            when Add_Audit =>
               null;
         end case;

      else
         GPS.Kernel.Console.Insert
           (Context.Module.Get_Kernel,
            "gps_codepeer_bridge execution failed",
            True,
            GPS.Kernel.Console.Error);
      end if;
   end On_Bridge_Exit;

   --------------------
   -- Review_Message --
   --------------------

   procedure Review_Message
     (Module  : Code_Peer.Module.Code_Peer_Module_Id;
      Message : Code_Peer.Message_Access)
   is
      Project            : constant Projects.Project_Type :=
                             GPS.Kernel.Project.Get_Project (Module.Kernel);
      Object_Directory   : constant Virtual_File :=
                             Projects.Object_Path (Project);
      Command_File_Name  : constant Virtual_File :=
                             Create_From_Dir
                               (Object_Directory, "bridge_in.xml");
      Reply_File_Name    : constant Virtual_File :=
                             Create_From_Dir
                               (Object_Directory, "bridge_out.xml");
      Args               : GNAT.OS_Lib.Argument_List :=
                             (1 => new String'
                                (+Command_File_Name.Full_Name));
      Mode               : constant String :=
                             Code_Peer.Shell_Commands.Get_Build_Mode
                               (Kernel_Handle (Module.Kernel));
      CodePeer_Subdir    : constant Boolean :=
                             Use_CodePeer_Subdir
                               (Kernel_Handle (Module.Kernel));
      Success            : Boolean;
      pragma Warnings (Off, Success);

   begin
      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), "codepeer");
      end if;

      --  Generate command file

      Code_Peer.Bridge.Commands.Audit_Trail
        (Command_File_Name,
         Codepeer_Output_Directory (Project),
         Reply_File_Name,
         Message.Id);

      --  Run gps_codepeer_bridge

      GPS.Kernel.Timeout.Launch_Process
        (Kernel        => GPS.Kernel.Kernel_Handle (Module.Kernel),
         Command       => "gps_codepeer_bridge",
         Arguments     => Args,
         Console       => GPS.Kernel.Console.Get_Console (Module.Kernel),
         Directory     => Object_Directory,
         Callback_Data =>
           new Bridge_Context'
           (Audit_Trail, Module, Reply_File_Name, Message),
         Success       => Success,
         Exit_Cb       => On_Bridge_Exit'Access);
      GNATCOLL.Utils.Free (Args);

      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
      end if;
   end Review_Message;

end Code_Peer.Module.Bridge;
