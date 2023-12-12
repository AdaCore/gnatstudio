------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;

with GNATCOLL.Projects;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;

with VSS.Strings.Conversions;

with GPS.Kernel.Project;

with DAP.Clients;
with DAP.Tools.Inputs;
with DAP.Tools.Outputs;
with DAP.Requests.Loaded_Sources;

with Toolchains_Old;

package body DAP.Requests.Launch is

   Me : constant Trace_Handle := Create ("GPS.DAP.Requests_Launch", On);

   procedure Initialize
     (Self   : in out Launch_DAP_Request;
      Client : DAP.Clients.DAP_Client_Access)
   is
      use Ada.Strings.Unbounded;
      use GNATCOLL.VFS;
      use DAP.Clients;

      type Extension_Array is array (Positive range <>) of
        Filesystem_String (1 .. 4);
      Extensions : constant Extension_Array := (".exe", ".out", ".vxe");
      Tmp        : Virtual_File;

      Exec : GNATCOLL.VFS.Virtual_File := Client.Get_Executable;
      Args : Unbounded_String := Client.Get_Executable_Args;

      End_Of_Exec  : Natural := 1;
      Blank_Pos    : Integer;

      --------------
      -- Get_Args --
      --------------

      function Get_Args return String;
      function Get_Args return String is
      begin
         if Length (Args) = 0
           or else End_Of_Exec >= Length (Args)
         then
            return "";
         else
            return " " & Slice (Args, End_Of_Exec, Length (Args));
         end if;
      end Get_Args;

   begin
      if Exec /= GNATCOLL.VFS.No_File then
         Self.Parameters.arguments.program := VSS.Strings.Conversions.
           To_Virtual_String
             (Ada.Strings.UTF_Encoding.UTF_8_String'
                (+Exec.Full_Name & Get_Args));

      elsif Args /= "" then
         Blank_Pos := Index (Args, " ");

         if Blank_Pos = 0 then
            End_Of_Exec := Length (Args);
         else
            End_Of_Exec := Blank_Pos - 1;
            Args := Unbounded_Slice
              (Args, Blank_Pos + 1, Length (Args));
         end if;

         declare
            Exec_Name : constant Filesystem_String :=
              +Slice (Args, 1, End_Of_Exec);

         begin
            --  First check whether Exec_Name is an absolute path
            Exec := Create (Full_Filename => Exec_Name);

            if not Exec.Is_Absolute_Path then
               --  If the Exec name is not an absolute path, check
               --  whether it corresponds to a file found from the
               --  current directory.

               Exec := Create
                 (Full_Filename =>
                    GNATCOLL.VFS_Utils.Normalize_Pathname
                      (Exec_Name, GNATCOLL.VFS_Utils.Get_Current_Dir));

               if not Exec.Is_Regular_File then
                  --  If the Exec is not an absolute path and it is not
                  --  found from the current directory, try to locate it
                  --  on path.

                  Exec := Toolchains_Old.Locate_Compiler_Executable
                    (Exec_Name);

                  if Exec = No_File then
                     Exec := Create_From_Base (Exec_Name);
                  end if;
               end if;
            end if;
         end;
         --  Check for a missing extension in module, and add it if needed
         --  Extensions currently checked in order: .exe, .out, .vxe

         if Exec = GNATCOLL.VFS.No_File then
            null;

         elsif Exec.Is_Regular_File then
            Client.Set_Executable (Exec);
            Self.Parameters.arguments.program := VSS.Strings.Conversions.
              To_Virtual_String
                (Ada.Strings.UTF_Encoding.UTF_8_String'
                   (+Exec.Full_Name & Get_Args));

         else
            for J in Extensions'Range loop
               Tmp := Create
                 (Full_Filename => Exec.Full_Name.all & Extensions (J));

               if Tmp.Is_Regular_File then
                  Exec := Tmp;
                  exit;
               end if;
            end loop;

            if Exec.Is_Regular_File then
               Client.Set_Executable (Exec);
               Self.Parameters.arguments.program := VSS.Strings.Conversions.
                 To_Virtual_String
                   (Ada.Strings.UTF_Encoding.UTF_8_String'
                      (+Exec.Full_Name & Get_Args));
            end if;
         end if;
      end if;
   end Initialize;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self   : Launch_DAP_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      DAP.Tools.Outputs.Output_LaunchRequest (Stream, Self.Parameters);
   end Write;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Launch_DAP_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out DAP_Request_Access)
   is
      Response : DAP.Tools.LaunchResponse;
   begin
      DAP.Tools.Inputs.Input_LaunchResponse (Stream, Response, Success);

      if Success then
         Launch_DAP_Request'Class
           (Self).On_Result_Message (Client, Response, New_Request);
      end if;
   end On_Result_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   procedure On_Result_Message
     (Self        : in out Launch_DAP_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.LaunchResponse;
      New_Request : in out DAP_Request_Access)
   is
      use GNATCOLL.Projects;
   begin
      New_Request := null;

      if GPS.Kernel.Project.Get_Registry
        (Self.Kernel).Tree.Status = From_Executable
        and then
          (not Client.Get_Capabilities.Is_Set
           or else Client.Get_Capabilities.
             Value.supportsLoadedSourcesRequest)
      then
         --  Debugging is started for executable, so prepare the
         --  source files list to prepare a project file for such debugging
         declare
            Sources : constant DAP.Requests.Loaded_Sources.
              Loaded_Sources_DAP_Request_Access :=
                new DAP.Requests.Loaded_Sources.
                  Loaded_Sources_DAP_Request (Self.Kernel);
         begin
            New_Request := DAP_Request_Access (Sources);
         end;

      else
         Client.On_Launched;
      end if;
   end On_Result_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Launch_DAP_Request;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      Trace (Me, "Rejected");
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Launch_DAP_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      Self.Kernel.Get_Messages_Window.Insert_Error
        ("[Debug] " &
           VSS.Strings.Conversions.To_UTF_8_String (Message));

      Trace (Me, VSS.Strings.Conversions.To_UTF_8_String (Message));
   end On_Error_Message;

   -------------
   -- Set_Seq --
   -------------

   overriding procedure Set_Seq
     (Self : in out Launch_DAP_Request;
      Id   : Integer) is
   begin
      Self.Parameters.seq := Id;
   end Set_Seq;

end DAP.Requests.Launch;
