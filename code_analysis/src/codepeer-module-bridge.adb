------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2017, AdaCore                     --
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

with Ada.Calendar;        use Ada.Calendar;
with GNAT.OS_Lib;         use GNAT.OS_Lib;
with GNATCOLL.Projects;   use GNATCOLL.Projects;
with GNATCOLL.VFS;        use GNATCOLL.VFS;

with GPS.Kernel.Project;
with GPS.Intl;            use GPS.Intl;
with Build_Command_Utils; use Build_Command_Utils;
with Commands.Builder;

with CodePeer.Bridge.Commands;

package body CodePeer.Module.Bridge is

   Audit_Request_File_Name      : constant Filesystem_String :=
     "audit_trail_request.xml";
   Audit_Reply_File_Name        : constant Filesystem_String :=
     "audit_trail_reply.xml";
   Add_Audit_File_Name          : constant Filesystem_String :=
     "add_audit_record.xml";
   Inspection_Request_File_Name : constant Filesystem_String :=
     "inspection_request.xml";
   Inspection_Reply_File_Name   : constant Filesystem_String :=
     "inspection_data.xml";
   Review_Status_File_Name      : constant Filesystem_String :=
     "review_status_data.xml";

   procedure Run_GPS_Codepeer_Bridge
     (Module       : not null access CodePeer.Module.Module_Id_Record'Class;
      Command_File : GNATCOLL.VFS.Virtual_File);
   --  Runs gps_codepeer_bridge

   ----------------------
   -- Add_Audit_Record --
   ----------------------

   procedure Add_Audit_Record
     (Module   : CodePeer.Module.CodePeer_Module_Id;
      Messages : CodePeer.Message_Vectors.Vector)
   is
      Project           : constant Project_Type :=
                            GPS.Kernel.Project.Get_Project (Module.Kernel);
      Mode              : constant String := Module.Kernel.Get_Build_Mode;
      Object_Directory  : Virtual_File;
      Command_File_Name : Virtual_File;
      Success           : Boolean;
      Ids               : Natural_Sets.Set;
      All_Messages      : Message_Vectors.Vector;
      pragma Warnings (Off, Success);

   begin
      Module.Kernel.Set_Build_Mode ("codepeer");

      --  Compute name of object directory and request file

      Object_Directory  := CodePeer_Object_Directory (Project);
      Command_File_Name :=
        Create_From_Dir (Object_Directory, Add_Audit_File_Name);

      --  Generate command file

      case Module.Version is
         when 3 =>
            Ids.Insert (Messages.First_Element.Id);
            Ids.Union (Messages.First_Element.Merged);

            CodePeer.Bridge.Commands.Add_Audit_Record_V3
              (Command_File_Name,
               Codepeer_Output_Directory (Module.Kernel),
               Ids,
               Messages.First_Element.Audit_V3.First_Element.Status,
               Messages.First_Element.Audit_V3.First_Element.Approved_By,
               Messages.First_Element.Audit_V3.First_Element.Comment);

         when 4 | 5 =>
            for Message of Messages loop
               All_Messages.Append (Message);

               for Id of Message.Merged loop
                  All_Messages.Append (Module.Messages (Id));
               end loop;
            end loop;

            CodePeer.Bridge.Commands.Add_Audit_Record_V4_V5
              (Command_File_Name,
               Codepeer_Output_Directory (Module.Kernel),
               Codepeer_Database_Directory (Project),
               All_Messages,
               Module.Version);
      end case;

      Module.Action := None;
      Run_GPS_Codepeer_Bridge (Module, Command_File_Name);
      Module.Kernel.Set_Build_Mode (Mode);
   end Add_Audit_Record;

   ----------------
   -- Inspection --
   ----------------

   procedure Inspection
     (Module : not null access CodePeer.Module.Module_Id_Record'Class)
   is
      Ensure_Build_Mode : CodePeer_Build_Mode (Module.Kernel);
      pragma Unreferenced (Ensure_Build_Mode);

      Project               : constant Project_Type :=
        GPS.Kernel.Project.Get_Project (Module.Kernel);
      Object_Directory      : constant Virtual_File :=
        CodePeer_Object_Directory (Project);
      Command_File_Name     : constant Virtual_File :=
        Create_From_Dir (Object_Directory, Inspection_Request_File_Name);
      Reply_File_Name       : constant Virtual_File :=
        Create_From_Dir (Object_Directory, Inspection_Reply_File_Name);
      Status_File_Name      : constant Virtual_File :=
        Create_From_Dir (Object_Directory, Review_Status_File_Name);
      DB_File_Name          : constant Virtual_File :=
        Create_From_Dir (Codepeer_Database_Directory (Project), "Sqlite.db");
      Output_Directory      : constant Virtual_File :=
        Codepeer_Output_Directory (Module.Kernel);
      Bts_Directory         : constant Virtual_File :=
        Output_Directory.Create_From_Dir ("bts");
      Success               : Boolean;
      pragma Warnings (Off, Success);

   begin
      if not Is_Directory (Output_Directory) then
         Module.Kernel.Insert
           (-"cannot find CodePeer output directory: " &
            Output_Directory.Display_Full_Name,
            Mode => GPS.Kernel.Error);
         return;
      end if;

      Module.Output_Directory := Output_Directory;
      Module.Bts_Directory := Bts_Directory;

      if DB_File_Name.Is_Regular_File
        and then Reply_File_Name.Is_Regular_File
        and then (DB_File_Name.File_Time_Stamp
                    < Reply_File_Name.File_Time_Stamp
                  or (Status_File_Name.Is_Regular_File
                      and then DB_File_Name.File_Time_Stamp
                        < Status_File_Name.File_Time_Stamp))
      then
         --  Inspection data file and review status data files are up to date,
         --  and can be loaded without run of gps_codepeer_bridge.

         Module.Load
           (Reply_File_Name,
            Status_File_Name,
            Bts_Directory,
            Output_Directory);

      else
         --  Generate command file

         CodePeer.Bridge.Commands.Inspection
           (Command_File_Name    => Command_File_Name,
            Output_Directory     => Output_Directory,
            DB_Directory         =>
              Codepeer_Database_Directory (Project),
            Inspection_File_Name => Reply_File_Name,
            Status_File_Name     => Status_File_Name,
            Import_Annotations   => Module.Import_Annotations.Get_Pref,
            Maximum_Version      =>
              Format_Version'Min
                (Module.Version_Limit, Supported_Format_Version'Last));

         Module.Action := Load_Bridge_Results;
         Module.Inspection_File := Reply_File_Name;
         Module.Status_File := Status_File_Name;
         Run_GPS_Codepeer_Bridge (Module, Command_File_Name);
      end if;
   end Inspection;

   ----------------------
   -- Load_Audit_Trail --
   ----------------------

   procedure Load_Audit_Trail
     (Module   : CodePeer.Module.CodePeer_Module_Id;
      Messages : CodePeer.Message_Vectors.Vector)
   is
      Project            : constant Project_Type :=
                             GPS.Kernel.Project.Get_Project (Module.Kernel);
      Object_Directory   : Virtual_File;
      Command_File_Name  : Virtual_File;
      Reply_File_Name    : Virtual_File;
      Mode               : constant String := Module.Kernel.Get_Build_Mode;
      Success            : Boolean;
      pragma Warnings (Off, Success);

   begin
      Module.Kernel.Set_Build_Mode ("codepeer");

      --  Compute directories' and files' names.

      Object_Directory := CodePeer_Object_Directory (Project);
      Command_File_Name :=
        Create_From_Dir
          (Object_Directory, Audit_Request_File_Name);
      Reply_File_Name :=
        Create_From_Dir (Object_Directory, Audit_Reply_File_Name);

      --  Generate command file

      CodePeer.Bridge.Commands.Audit_Trail
        (Command_File_Name,
         Codepeer_Output_Directory (Module.Kernel),
         Codepeer_Database_Directory (Project),
         Reply_File_Name,
         Messages,
         Module.Version);

      --  Run gps_codepeer_bridge

      Module.Action := Audit_Trail;
      Module.Inspection_File := Reply_File_Name;
      Module.Bridge_Messages := Messages;
      Run_GPS_Codepeer_Bridge (Module, Command_File_Name);
      Module.Kernel.Set_Build_Mode (Mode);
   end Load_Audit_Trail;

   ----------------------------------
   -- Remove_Inspection_Cache_File --
   ----------------------------------

   procedure Remove_Inspection_Cache_File
      (Module : not null access CodePeer.Module.Module_Id_Record'Class)
   is
      Project           : constant Project_Type :=
                            GPS.Kernel.Project.Get_Project (Module.Kernel);
      Object_Directory  : constant Virtual_File :=
                            CodePeer_Object_Directory (Project);
      Reply_File_Name   : constant Virtual_File :=
                            Create_From_Dir
                              (Object_Directory, Inspection_Reply_File_Name);
      Success           : Boolean;

   begin
      if Reply_File_Name.Is_Regular_File then
         Delete (Reply_File_Name, Success);

         if not Success then
            Module.Kernel.Insert
              (-"Unable to remove code review file");
         end if;
      end if;
   end Remove_Inspection_Cache_File;

   -----------------------------
   -- Run_GPS_Codepeer_Bridge --
   -----------------------------

   procedure Run_GPS_Codepeer_Bridge
     (Module       : not null access CodePeer.Module.Module_Id_Record'Class;
      Command_File : GNATCOLL.VFS.Virtual_File)
   is
      Builder    : constant Builder_Context := Builder_Context
        (Module.Kernel.Module (Builder_Context_Record'Tag));
      Extra_Args : Argument_List_Access;

   begin
      Extra_Args := new Argument_List (1 .. 1);
      Extra_Args (1) := new String'(+Command_File.Full_Name.all);
      Commands.Builder.Launch_Target
        (Builder     => Builder,
         Target_Name => "CodePeer Bridge",
         Mode_Name   => "codepeer",
         Force_File  => No_File,
         Extra_Args  => Extra_Args,
         Quiet       => False,
         Synchronous => False,
         Dialog      => Force_No_Dialog,
         Via_Menu    => False,
         Main        => No_File,
         Main_Project => No_Project,
         Background   => True,
         Directory    => No_File);
      Free (Extra_Args (1));
      Free (Extra_Args);
   end Run_GPS_Codepeer_Bridge;

end CodePeer.Module.Bridge;
