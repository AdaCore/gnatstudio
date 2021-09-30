------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with VSS.Strings;
with LSP.Types;

with GPS.Kernel; use GPS.Kernel;
with GPS.LSP_Client.Language_Servers;
with GPS.LSP_Client.Partial_Responses;
with GPS.LSP_Client.Requests;
with Language;

package GPS.LSP_Module is

   procedure Register_Module (Kernel : Kernel_Handle);
   --  Register the module

   function LSP_Is_Enabled
     (Language : not null Standard.Language.Language_Access) return Boolean;
   --  Use of language server is enabled and is configured for given language.
   --  It doesn't mean that language server is up and running.

   function LSP_Ada_Support_Trace_Is_Active return Boolean;
   --  Return True if the LSP support trace for Ada is active.
   --  This does not mean that a language server for Ada is up and running: it
   --  just means that the LSP for will be used for Ada at some point.

   function LSP_Cpp_Support_Trace_Is_Active return Boolean;
   --  Return True if the LSP support trace for C++ is active.
   --  This does not mean that a language server for C++ is up and running: it
   --  just means that the LSP for will be used for C++ at some point.

   function Get_Language_Server
     (Language : not null Standard.Language.Language_Access)
      return GPS.LSP_Client.Language_Servers.Language_Server_Access;
   --  Return the language server currently used for the given language. It
   --  returns null if there is no language server configured for this
   --  language. This subprogram is intended to be used by
   --  GPS.LSP_Client.Requests package only.

   function Get_Running_Request
     (Server : not null GPS.LSP_Client.Language_Servers.Language_Server_Access;
      Id     : LSP.Types.LSP_Number_Or_String)
      return GPS.LSP_Client.Requests.Request_Access;
   --  If a request with the given Id is currently running, return it.
   --  Return null otherwise.
   --  Clients must NOT free or store the result: the request remains owned by
   --  the server.

   procedure Restart_Server
     (Server : not null
        GPS.LSP_Client.Language_Servers.Language_Server_Access);
   --  Restart the server

   procedure Register_Partial_Handler
     (Prefix  : VSS.Strings.Virtual_String;
      Handler : GPS.LSP_Client.Partial_Responses.
        Partial_Response_Handler_Access);
   --  Add handler for partial responses with the given Prefix.
   --  We use Starts_With for match a Token with a Handler.

   procedure Unregister_Partial_Handler
     (Prefix : VSS.Strings.Virtual_String);
   --  Remove handler for Prefix

end GPS.LSP_Module;
