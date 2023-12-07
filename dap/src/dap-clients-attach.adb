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

with GNATCOLL.Projects;

with VSS.Strings.Conversions;

with GPS.Kernel.Project;

with DAP.Requests.Loaded_Sources;

package body DAP.Clients.Attach is

   ----------------
   -- Initialize --
   ----------------

   function Create
     (Kernel : not null Kernel_Handle;
      Client : not null DAP_Client_Access;
      PID    : Integer := -1;
      Target : String := "") return Attach_Request_Access
   is
      Self : constant Attach_Request_Access := new Attach_Request (Kernel);
   begin
      Self.Client := Client;

      Self.Parameters.arguments.target :=
        VSS.Strings.Conversions.To_Virtual_String (Target);

      if PID /= -1 then
         Self.Parameters.arguments.pid := (Is_Set => True, Value => PID);
      end if;

      return Self;
   end Create;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Attach_Request;
      Result      : DAP.Tools.AttachResponse;
      New_Request : in out DAP_Request_Access)
   is
      use GNATCOLL.Projects;
   begin
      New_Request := null;

      if GPS.Kernel.Project.Get_Registry
        (Self.Kernel).Tree.Status = From_Executable
        and then
          (not Self.Client.Get_Capabilities.Is_Set
           or else Self.Client.Get_Capabilities.
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
         Self.Client.On_Launched;
      end if;
   end On_Result_Message;

end DAP.Clients.Attach;
