------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with GPS.Kernel; use GPS.Kernel;
with GPS.LSP_Client.Language_Servers;
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

   function Get_Language_Server
     (Language : not null Standard.Language.Language_Access)
      return GPS.LSP_Client.Language_Servers.Language_Server_Access;
   --  Return the language server currently used for the given language. It
   --  returns null if there is no language server configured for this
   --  language. This subprogram is intended to be used by
   --  GPS.LSP_Client.Requests package only.

end GPS.LSP_Module;
