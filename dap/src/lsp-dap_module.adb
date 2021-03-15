------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

with Ada.Strings.UTF_Encoding;

with GNATCOLL.Traces; use GNATCOLL.Traces;

with GPS.Kernel;         use GPS.Kernel;
with GPS.Kernel.Actions; use GPS.Kernel.Actions;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;

with Commands;             use Commands;
with Commands.Interactive; use Commands.Interactive;

with GPS.Intl; use GPS.Intl;
with LSP.Types;

with VSS.Text_Streams.Memory_UTF8_Output;
with Spawn.String_Vectors;

with LSP.DAP_Clients;
with LSP.JSON_Streams;
with LSP.Messages;
with LSP.Client_Notification_Receivers;
with GPS.LSP_Client.Language_Servers.Interceptors;

package body LSP.DAP_Module is
   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.DAP_MODULE");
   DAP_Module_Name : constant String       := "DAP";

   type Module_Id_Record is new GPS.Kernel.Modules.Module_ID_Record and
     LSP.Client_Notification_Receivers.Client_Notification_Receiver and
     GPS.LSP_Client.Language_Servers.Interceptors.Server_Listener with record
      Nothing : Integer := 0;
      Client  : LSP.DAP_Clients.Client;
   end record;

   overriding procedure On_Publish_Diagnostics
     (Self   : access Module_Id_Record;
      Params : LSP.Messages.PublishDiagnosticsParams) is null;
   --  Overrides from lsp-client_notification_receivers.ads

   overriding procedure On_Show_Message
     (Self : access Module_Id_Record; Value : LSP.Messages.ShowMessageParams);
   --  Overrides from lsp-client_notification_receivers.ads

   overriding procedure On_Progress
     (Self : access Module_Id_Record; Value : LSP.Messages.Progress_Params);
   --  Overrides from lsp-client_notification_receivers.ads

   overriding procedure On_Log_Message
     (Self  : access Module_Id_Record;
      Value : LSP.Messages.LogMessageParams) is null;
   --  Overrides from lsp-client_notification_receivers.ads

   type DAP_Module_Id is access all Module_Id_Record'Class;

   Module : DAP_Module_Id;

   type Test_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Test_Command; Context : Interactive_Command_Context)
      return Command_Return_Type;

   overriding function Execute
     (Command : access Test_Command; Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      step   : constant Integer       := Module.Nothing;
      bin    : constant String        := "/usr/bin/echo_back";
      outst  : constant String        := "/tmp/echo_back";
      args   : Spawn.String_Vectors.UTF_8_String_Vector;

      pragma Unreferenced (Kernel);
      pragma Unreferenced (Command);
   begin
      if step = 0 then
         Trace (Me, "Set_Program " & bin);
         Module.Client.Set_Program (bin);

         args.Append (outst);
         Trace (Me, "Set_Arguments " & outst);
         Module.Client.Set_Arguments (args);

      elsif step = 1 then
         Trace (Me, "Start Client");
         Module.Client.Start;

      elsif step = 2 then
         declare
            JS : aliased LSP.JSON_Streams.JSON_Stream
              (Is_Server_Side => False, R => null);
            Output : aliased VSS.Text_Streams.Memory_UTF8_Output
              .Memory_UTF8_Output_Stream;
            Value : LSP.Messages.RequestMessage;

            function "+"
              (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
               return LSP.Types.LSP_String renames
              LSP.Types.To_LSP_String;

         begin
            Trace (Me, "Send request");
            JS.Set_Stream (Output'Unchecked_Access);
            Value.jsonrpc := +"2.0";
            LSP.Messages.RequestMessage'Class'Write (JS'Access, Value);
            JS.End_Document;
            Module.Client.Send_Buffer (Output.Buffer);
         end;

      elsif step = 3 then
         Trace (Me, "Stop Client");
         Module.Client.Stop;
         Module.Nothing := -1; --  able to loop
      end if;

      Module.Nothing := Module.Nothing + 1;
      return Commands.Success;
   end Execute;

   overriding procedure On_Show_Message
     (Self : access Module_Id_Record; Value : LSP.Messages.ShowMessageParams)
   is
   begin
      null;
   end On_Show_Message;

   overriding procedure On_Progress
     (Self : access Module_Id_Record; Value : LSP.Messages.Progress_Params)
   is
   begin
      null;
   end On_Progress;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Module := new Module_Id_Record;
      --  Module.Unknown_Server :=
      --    new GPS.LSP_Client.Language_Servers.Stub.Stub_Language_Server;
      Register_Module (Module_ID (Module), Kernel, "DAP_Client");
      Trace (Me, "Register " & DAP_Module_Name & " Module");

      Register_Action
        (Kernel   => Kernel, Name => "debug dap action",
         Command  => new Test_Command, Description => "DAP Action",
         Category => -"DAP");
   end Register_Module;

end LSP.DAP_Module;
