------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2023, AdaCore                   --
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
--  Abstract language server to manage language server process and associated
--  text documents.

private with Ada.Unchecked_Deallocation;

with GPS.LSP_Clients;
with GPS.LSP_Client.Requests;
with GPS.LSP_Client.Configurations;

package GPS.LSP_Client.Language_Servers is

   type Abstract_Language_Server is tagged limited private;

   type Language_Server_Access is access all Abstract_Language_Server'Class;

   procedure Execute
     (Self    : in out Abstract_Language_Server;
      Request : in out GPS.LSP_Client.Requests.Request_Access);
   --  Executes request. Memory will be deallocated after execution.

   procedure Cancel
     (Self    : in out Abstract_Language_Server;
      Request : in out GPS.LSP_Client.Requests.Request_Access);
   --  Cancel request. Memory will be deallocated.

   procedure Configuration_Changed
     (Self : in out Abstract_Language_Server) is null;
   --  Called when GNAT Studio configuration is changed. Should send
   --  didConfigurationChange notification to the language server when
   --  necessary.

   function Get_Client
     (Self : Abstract_Language_Server)
      return GPS.LSP_Clients.LSP_Client_Access is (null);

   --  Configuration --

   function Is_Configuration_Supported
     (Self    : in out Abstract_Language_Server;
      Setting : GPS.LSP_Client.Configurations.Setting_Kind)
      return Boolean;
   --  Return True when server supports the configuration option.

   procedure Set_Configuration
     (Self    : in out Abstract_Language_Server;
      Setting : GPS.LSP_Client.Configurations.Setting_Kind;
      Value   : GPS.LSP_Client.Configurations.Configuration_Value) is null;
   --  Set configuration parameter on server side.

private

   type Abstract_Language_Server is tagged limited null record;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (GPS.LSP_Client.Requests.LSP_Request'Class,
        GPS.LSP_Client.Requests.Request_Access);

end GPS.LSP_Client.Language_Servers;
