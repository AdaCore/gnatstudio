------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with GNATCOLL.JSON;
with GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with GPS.LSP_Client.Language_Servers.Interceptors;
with GPS.LSP_Client.Language_Servers.Real.Shutdowns;

package body GPS.LSP_Client.Language_Servers.Real is

   procedure Initialize (Self : in out Real_Language_Server'Class);
   --  Initialize language server object. Doesn't start server.

   ---------------------------
   -- Configuration_Changed --
   ---------------------------

   overriding procedure Configuration_Changed
     (Self : in out Real_Language_Server) is
   begin
      if Self.Client.Is_Ready then
         declare
            Settings : constant GNATCOLL.JSON.JSON_Value :=
                         Self.Configuration.Configuration_Settings;

         begin
            if not Settings.Is_Empty then
               --  Send WorkspaceDidChangeConfiguration notification when
               --  where is something to send.

               Self.Client.On_DidChangeConfiguration_Notification
                 ((settings => Settings));
            end if;
         end;
      end if;
   end Configuration_Changed;

   ------------
   -- Create --
   ------------

   function Create
     (Kernel        : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Configuration : not null access
        GPS.LSP_Client.Configurations.Server_Configuration'Class;
      Interceptor   : not null access Interceptors.Interceptor_Listener'Class;
      Language      : not null access Language_Root'Class)
      return not null Language_Server_Access is
   begin
      return Result : constant not null Language_Server_Access :=
        new Real_Language_Server (Kernel, Configuration, Interceptor, Language)
      do
         Real_Language_Server'Class (Result.all).Initialize;
      end return;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self    : in out Real_Language_Server;
      Request : in out GPS.LSP_Client.Requests.Request_Access) is
   begin
      Self.Client.Enqueue (Request);
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Real_Language_Server'Class) is
   begin
      Self.Client.Initialize;
   end Initialize;

   ---------------------------
   -- On_Response_Processed --
   ---------------------------

   overriding procedure On_Response_Processed
     (Self : in out Real_Language_Server;
      Data : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Self.Interceptor.On_Response_Processed (Self'Unchecked_Access, Data);
   end On_Response_Processed;

   -----------------------
   -- On_Server_Started --
   -----------------------

   overriding procedure On_Server_Started
     (Self : in out Real_Language_Server)
   is
      Settings : constant GNATCOLL.JSON.JSON_Value :=
                   Self.Configuration.Configuration_Settings;

   begin
      if not Settings.Is_Empty then
         --  Send WorkspaceDidChangeConfiguration notification to complete
         --  initialization of the language server.

         Self.Client.On_DidChangeConfiguration_Notification
           ((settings => Settings));
      end if;

      Self.Interceptor.On_Server_Started (Self'Unchecked_Access);
   end On_Server_Started;

   -----------------------
   -- On_Server_Stopped --
   -----------------------

   overriding procedure On_Server_Stopped
     (Self : in out Real_Language_Server) is
   begin
      Self.In_Shutdown := False;
   end On_Server_Stopped;

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Real_Language_Server'Class) is
   begin
      Self.Client.Start
        (+Self.Configuration.Server_Program.Full_Name.all,
         Self.Configuration.Server_Arguments);
   end Start;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown
     (Self               : in out Real_Language_Server'Class;
      Reject_Immediately : Boolean)
   is
      Request : GPS.LSP_Client.Requests.Request_Access :=
                  new Shutdowns.Shutdown_Request
                    (Server => Self'Unchecked_Access);

   begin
      Self.In_Shutdown := True;

      Self.Execute (Request);

      Self.Client.Stop (Reject_Immediately);
   end Shutdown;

end GPS.LSP_Client.Language_Servers.Real;
