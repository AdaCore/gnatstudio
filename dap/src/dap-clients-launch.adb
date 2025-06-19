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

with GNATCOLL.VFS;             use GNATCOLL.VFS;

with VSS.Strings.Conversions;

with GPS.Kernel;               use GPS.Kernel;

with DAP.Requests;             use DAP.Requests;
with DAP.Requests.Launch;

package body DAP.Clients.Launch is

   type Launch_Request is
     new DAP.Requests.Launch.Launch_DAP_Request with null record;
   type Launch_Request_Access is access all Launch_Request'Class;

   function Create
     (Kernel            : not null Kernel_Handle;
      Executable        : GNATCOLL.VFS.Virtual_File;
      Executable_Args   : VSS.String_Vectors.Virtual_String_Vector;
      Stop_At_Beginning : Boolean := False)
      return Launch_Request_Access;
   --  Create a new DAP 'launch' request.

   overriding procedure On_Result_Message
     (Self        : in out Launch_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.LaunchResponse;
      New_Request : in out DAP_Request_Access);
   --  gdb 17.x send the answer after the configurationDone request when the
   --  executable has been started.

   overriding procedure On_Error_Message
     (Self    : in out Launch_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String);

   ------------
   -- Create --
   ------------

   function Create
     (Kernel            : not null Kernel_Handle;
      Executable        : GNATCOLL.VFS.Virtual_File;
      Executable_Args   : VSS.String_Vectors.Virtual_String_Vector;
      Stop_At_Beginning : Boolean := False)
      return Launch_Request_Access
   is
      Self : constant Launch_Request_Access := new Launch_Request (Kernel);
   begin
      Self.Parameters.arguments.program := VSS.Strings.Conversions.
        To_Virtual_String (Executable.Display_Full_Name);
      Self.Parameters.arguments.args := Executable_Args;
      Self.Parameters.arguments.stopAtBeginningOfMainSubprogram :=
        Stop_At_Beginning;

      return Self;
   end Create;

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
     (Client            : in out DAP.Clients.DAP_Client'Class;
      Executable        : GNATCOLL.VFS.Virtual_File;
      Executable_Args   : VSS.String_Vectors.Virtual_String_Vector;
      Stop_At_Beginning : Boolean := False)
   is
      Launch_Req : Launch_Request_Access := DAP.Clients.Launch.Create
        (Kernel            => Client.Kernel,
         Executable        => Executable,
         Executable_Args   => Executable_Args,
         Stop_At_Beginning => Stop_At_Beginning);
   begin
      Client.Enqueue (DAP.Requests.DAP_Request_Access (Launch_Req));

      --  gdb 17.x does not send the answer until configurationDone request
      --  is sent. Move this code to On_Result when it is changed.
      Client.On_Launched_Sent (Start_Method => DAP.Types.Launched);
   end Send_Launch_Request;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Launch_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.LaunchResponse;
      New_Request : in out DAP_Request_Access) is
   begin
      New_Request := null;
      Client.Set_Status (Running);
   end On_Result_Message;

end DAP.Clients.Launch;
