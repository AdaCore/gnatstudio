------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

with GNATCOLL.VFS;             use GNATCOLL.VFS;

with VSS.Strings.Conversions;

with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Project;

with DAP.Clients.LoadedSources;
with DAP.Types;
with DAP.Requests;             use DAP.Requests;
with DAP.Requests.Launch;

package body DAP.Clients.Launch is

   type Launch_Request is
     new DAP.Requests.Launch.Launch_DAP_Request with null record;
   type Launch_Request_Access is access all Launch_Request'Class;

   function Create
     (Kernel          : not null Kernel_Handle;
      Executable      : GNATCOLL.VFS.Virtual_File;
      Executable_Args : VSS.String_Vectors.Virtual_String_Vector)
      return Launch_Request_Access;
   --  Create a new DAP 'launch' request.

   overriding procedure On_Result_Message
     (Self        : in out Launch_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.LaunchResponse;
      New_Request : in out DAP_Request_Access);

   overriding procedure On_Error_Message
     (Self    : in out Launch_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String);

   ------------
   -- Create --
   ------------

   function Create
     (Kernel          : not null Kernel_Handle;
      Executable      : GNATCOLL.VFS.Virtual_File;
      Executable_Args : VSS.String_Vectors.Virtual_String_Vector)
      return Launch_Request_Access
   is
      Self : constant Launch_Request_Access := new Launch_Request (Kernel);
   begin
      Self.Parameters.arguments.program := VSS.Strings.Conversions.
        To_Virtual_String (Executable.Display_Full_Name);
      Self.Parameters.arguments.args := Executable_Args;

      return Self;
   end Create;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Launch_Request;
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
         --  Debugging has been started directly on a pre-built executable,
         --  not from a project: send the DAP 'loadedSources' request to
         --  retrieve its source files.
         New_Request := DAP_Request_Access
           (DAP.Clients.LoadedSources.Create (Self.Kernel));
      else
         Client.On_Launched (Start_Method => DAP.Types.Launched);
      end if;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Launch_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      Self.Kernel.Get_Messages_Window.Insert_Error
        ("[Debug]:" &
           VSS.Strings.Conversions.To_UTF_8_String (Message));

      DAP.Requests.Launch.On_Error_Message
        (DAP.Requests.Launch.Launch_DAP_Request (Self),
         Client, Message);
   end On_Error_Message;

   -------------------------
   -- Send_Launch_Request --
   -------------------------

   procedure Send_Launch_Request
     (Client          : in out DAP.Clients.DAP_Client'Class;
      Executable      : GNATCOLL.VFS.Virtual_File;
      Executable_Args : VSS.String_Vectors.Virtual_String_Vector)
   is
      Launch_Req : Launch_Request_Access := DAP.Clients.Launch.Create
        (Kernel          => Client.Kernel,
         Executable      => Executable,
         Executable_Args => Executable_Args);
   begin
      Client.Process (DAP.Requests.DAP_Request_Access (Launch_Req));
   end Send_Launch_Request;

end DAP.Clients.Launch;
