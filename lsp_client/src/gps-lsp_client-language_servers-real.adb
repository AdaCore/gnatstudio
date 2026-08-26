------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2026, AdaCore                   --
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
with GNATCOLL.VFS; use GNATCOLL.VFS;

with GPS.LSP_Client.Requests.Internals;
with GPS.LSP_Client.Language_Servers.Interceptors;
with GPS.LSP_Client.Utilities;

package body GPS.LSP_Client.Language_Servers.Real is

   procedure Initialize (Self : in out Real_Language_Server'Class);
   --  Initialize language server object. Doesn't start server.

   ------------
   -- Cancel --
   ------------

   overriding
   procedure Cancel
     (Self    : in out Real_Language_Server;
      Request : in out GPS.LSP_Client.Requests.Request_Access) is
   begin
      Self.Client.Cancel (Request);
   end Cancel;

   ---------------------------
   -- Configuration_Changed --
   ---------------------------

   overriding
   procedure Configuration_Changed (Self : in out Real_Language_Server) is
   begin
      if Self.Client.Is_Ready then
         declare
            Settings : constant GNATCOLL.JSON.JSON_Value :=
              Self.Configuration.Configuration_Settings;

         begin
            if not Settings.Is_Empty then
               --  Send WorkspaceDidChangeConfiguration notification when
               --  where is something to send.

               Self
                 .Client
                 .Send_Notification
                 .On_DidChangeConfiguration_Notification
                    ((settings =>
                        GPS.LSP_Client.Utilities.To_LSP_Any (Settings)));
            end if;
         end;
      end if;
   end Configuration_Changed;

   ------------
   -- Create --
   ------------

   function Create
     (Kernel              :
        not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Configuration       :
        not null access
          GPS.LSP_Client.Configurations.Server_Configuration'Class;
      Server_Interceptor  : not null access Interceptors.Server_Listener'Class;
      Request_Interceptor :
        not null access Interceptors.Request_Listener'Class;
      Language            : not null access Language_Root'Class)
      return not null Language_Server_Access is
   begin
      return
         Result : constant not null Language_Server_Access :=
           new Real_Language_Server
                 (Kernel => Kernel,
                  Configuration => Configuration,
                  Server_Interceptor => Server_Interceptor,
                  Request_Interceptor => Request_Interceptor,
                  Language => Language)
      do
         Real_Language_Server'Class (Result.all).Initialize;
      end return;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute
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

   --------------------------------
   -- Is_Configuration_Supported --
   --------------------------------

   overriding
   function Is_Configuration_Supported
     (Self    : in out Real_Language_Server;
      Setting : GPS.LSP_Client.Configurations.Setting_Kind) return Boolean is
   begin
      return Self.Configuration.Is_Configuration_Supported (Setting);
   end Is_Configuration_Supported;

   ----------------------
   -- On_Receive_Reply --
   ----------------------

   overriding
   procedure On_Receive_Reply
     (Self    : in out Real_Language_Server;
      Request : GPS.LSP_Client.Requests.Request_Access) is
   begin
      if Self.Request_Interceptor /= null then
         Self.Request_Interceptor.On_Receive_Reply
           (GPS.LSP_Client.Requests.Internals.Create_Reference
              (Request, Self'Unchecked_Access));
      end if;
   end On_Receive_Reply;

   -----------------------
   -- On_Reject_Request --
   -----------------------

   overriding
   procedure On_Reject_Request
     (Self    : in out Real_Language_Server;
      Request : GPS.LSP_Client.Requests.Request_Access) is
   begin
      if Self.Request_Interceptor /= null then
         Self.Request_Interceptor.On_Reject_Request
           (GPS.LSP_Client.Requests.Internals.Create_Reference
              (Request, Self'Unchecked_Access));
      end if;
   end On_Reject_Request;

   ---------------------------
   -- On_Response_Processed --
   ---------------------------

   overriding
   procedure On_Response_Processed
     (Self   : in out Real_Language_Server;
      Data   : Ada.Strings.Unbounded.Unbounded_String;
      Method : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if not Self.Destroyed then
         Self.Server_Interceptor.On_Response_Processed
           (Self'Unchecked_Access, Data, Method);
      end if;
   end On_Response_Processed;

   ----------------------
   -- On_Response_Sent --
   ----------------------

   overriding
   procedure On_Response_Sent
     (Self : in out Real_Language_Server;
      Data : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if not Self.Destroyed then
         Self.Server_Interceptor.On_Response_Sent
           (Self'Unchecked_Access, Data);
      end if;
   end On_Response_Sent;

   --------------------
   -- On_Send_Cancel --
   --------------------

   overriding
   procedure On_Send_Cancel
     (Self    : in out Real_Language_Server;
      Request : GPS.LSP_Client.Requests.Request_Access) is
   begin
      if Self.Request_Interceptor /= null then
         Self.Request_Interceptor.On_Send_Cancel
           (GPS.LSP_Client.Requests.Internals.Create_Reference
              (Request, Self'Unchecked_Access));
      end if;
   end On_Send_Cancel;

   ---------------------
   -- On_Send_Request --
   ---------------------

   overriding
   procedure On_Send_Request
     (Self    : in out Real_Language_Server;
      Request : GPS.LSP_Client.Requests.Request_Access) is
   begin
      if Self.Request_Interceptor /= null then
         Self.Request_Interceptor.On_Send_Request
           (GPS.LSP_Client.Requests.Internals.Create_Reference
              (Request, Self'Unchecked_Access));
      end if;
   end On_Send_Request;

   ------------------------
   -- On_Progress_Begin --
   ------------------------

   overriding
   procedure On_Progress_Begin
     (Self  : in out Real_Language_Server;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkDoneProgressBegin) is
   begin
      Self.Server_Interceptor.On_Progress_Begin
        (Self'Unchecked_Access, Token, Value);
   end On_Progress_Begin;

   -------------------------
   -- On_Progress_Report --
   -------------------------

   overriding
   procedure On_Progress_Report
     (Self  : in out Real_Language_Server;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkDoneProgressReport) is
   begin
      Self.Server_Interceptor.On_Progress_Report
        (Self'Unchecked_Access, Token, Value);
   end On_Progress_Report;

   ----------------------
   -- On_Progress_End --
   ----------------------

   overriding
   procedure On_Progress_End
     (Self  : in out Real_Language_Server;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkDoneProgressEnd) is
   begin
      Self.Server_Interceptor.On_Progress_End
        (Self'Unchecked_Access, Token, Value);
   end On_Progress_End;

   -----------------------------------------
   -- On_PublishDiagnostics_Notification --
   -----------------------------------------

   overriding
   procedure On_PublishDiagnostics_Notification
     (Self  : in out Real_Language_Server;
      Value : LSP.Structures.PublishDiagnosticsParams) is
   begin
      Self.Server_Interceptor.On_Publish_Diagnostics
        (Self'Unchecked_Access, Value);
   end On_PublishDiagnostics_Notification;

   -----------------------------------
   -- On_ShowMessage_Notification --
   -----------------------------------

   overriding
   procedure On_ShowMessage_Notification
     (Self  : in out Real_Language_Server;
      Value : LSP.Structures.ShowMessageParams) is
   begin
      Self.Server_Interceptor.On_Show_Message (Self'Unchecked_Access, Value);
   end On_ShowMessage_Notification;

   ----------------------------------
   -- On_LogMessage_Notification --
   ----------------------------------

   overriding
   procedure On_LogMessage_Notification
     (Self  : in out Real_Language_Server;
      Value : LSP.Structures.LogMessageParams) is
   begin
      Self.Server_Interceptor.On_Log_Message (Self'Unchecked_Access, Value);
   end On_LogMessage_Notification;

   -----------------------
   -- On_Server_Started --
   -----------------------

   overriding
   procedure On_Server_Started (Self : in out Real_Language_Server) is
   begin
      Self.Server_Interceptor.On_Server_Started (Self'Unchecked_Access);
   end On_Server_Started;

   -----------------------
   -- On_Server_Stopped --
   -----------------------

   overriding
   procedure On_Server_Stopped (Self : in out Real_Language_Server) is
   begin
      Self.Server_Interceptor.On_Server_Stopped (Self'Unchecked_Access);
   end On_Server_Stopped;

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Real_Language_Server'Class) is
   begin
      Self.Client.Start
        (Executable             =>
           +Self.Configuration.Server_Program.Full_Name.all,
         Arguments              => Self.Configuration.Server_Arguments,
         Initialization_Options =>
           GPS.LSP_Client.Utilities.To_LSP_Any
             (Self.Configuration.Configuration_Settings));
   end Start;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown
     (Self : in out Real_Language_Server'Class; Reject_Immediately : Boolean)
   is
   begin
      Self.Destroyed := Reject_Immediately;
      Self.Client.Stop (Reject_Immediately);
   end Shutdown;

   -------------
   -- Restart --
   -------------

   procedure Restart (Self : in out Real_Language_Server'Class) is
   begin
      Self.Client.Restart
        (Initialization_Options =>
           GPS.LSP_Client.Utilities.To_LSP_Any
             (Self.Configuration.Configuration_Settings));
   end Restart;

   -------------------------
   -- Get_Running_Request --
   -------------------------

   function Get_Running_Request
     (Self : Real_Language_Server'Class;
      Id   : LSP.Structures.Integer_Or_Virtual_String)
      return GPS.LSP_Client.Requests.Request_Access is
   begin
      return Self.Client.Get_Running_Request (Id);
   end Get_Running_Request;

end GPS.LSP_Client.Language_Servers.Real;
