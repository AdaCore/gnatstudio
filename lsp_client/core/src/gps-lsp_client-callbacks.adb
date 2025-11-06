------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2025, AdaCore                       --
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

with GPS.LSP_Client.Language_Servers;
with GPS.LSP_Client.Utilities;
with VSS.Strings;

package body GPS.LSP_Client.Callbacks is

   overriding function Build_Did_Open_Params
     (Self : Null_Callback;
      File : GNATCOLL.VFS.Virtual_File)
      return LSP.Messages.DidOpenTextDocumentParams is
      pragma Unreferenced (Self);
      URI : constant LSP.Messages.DocumentUri :=
        GPS.LSP_Client.Utilities.To_URI (File);
   begin
      return
        (textDocument =>
           (uri        => URI,
            languageId => VSS.Strings.Empty_Virtual_String,
            version    => 0,
            text       => VSS.Strings.Empty_Virtual_String));
   end Build_Did_Open_Params;

   overriding procedure On_Document_Closed
     (Self : Null_Callback;
      File : GNATCOLL.VFS.Virtual_File) is
      pragma Unreferenced (Self, File);
   begin
      null;
   end On_Document_Closed;

   overriding procedure Apply_Workspace_Edit
     (Self  : Null_Callback;
      Edit  : LSP.Messages.WorkspaceEdit;
      Title : String;
      Error : out Boolean)
   is
      pragma Unreferenced (Self, Edit, Title);
   begin
      Error := True;
   end Apply_Workspace_Edit;

   overriding function Get_Server_Environment
     (Self : Null_Callback)
      return Spawn.Environments.Process_Environment is
      pragma Unreferenced (Self);
   begin
      return Spawn.Environments.System_Environment;
   end Get_Server_Environment;

   overriding function Get_Language_Server
     (Self : Null_Callback;
      Lang : not null Language.Language_Access)
      return GPS.LSP_Client.Language_Servers.Language_Server_Access is
      pragma Unreferenced (Self, Lang);
   begin
      return null;
   end Get_Language_Server;

   ---------------------
   -- Schedule_Timer --
   ---------------------

   overriding procedure Schedule_Timer
     (Self     : Null_Callback;
      Interval : Natural;
      Callback : Timer_Callback;
      Timer    : out Timer_Id)
   is
      pragma Unreferenced (Self, Interval, Callback);
   begin
      Timer := No_Timer;
      --  Null implementation: timers are disabled
      --  Server auto-restart will not work, but manual restart is fine
   end Schedule_Timer;

   -------------------
   -- Cancel_Timer --
   -------------------

   overriding procedure Cancel_Timer
     (Self  : Null_Callback;
      Timer : in out Timer_Id)
   is
      pragma Unreferenced (Self);
   begin
      Timer := No_Timer;
      --  Null implementation: nothing to cancel
   end Cancel_Timer;

end GPS.LSP_Client.Callbacks;
