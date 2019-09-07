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
--  Interceptor for the language server interactions

with Ada.Strings.Unbounded;

package GPS.LSP_Client.Language_Servers.Interceptors is

   type Interceptor_Listener is limited interface;

   procedure On_Server_Started
     (Self   : in out Interceptor_Listener;
      Server : not null Language_Server_Access) is null;
   --  Called when language server process has been started, language server
   --  has been initialized and configured (if necessary) and ready to
   --  process requests.

   procedure On_Server_Stopped
     (Self   : in out Interceptor_Listener;
      Server : not null Language_Server_Access) is null;
   --  Called when language server has been stopped for any reason and
   --  unable to process requests anymore.

   procedure On_Response_Processed
     (Self   : in out Interceptor_Listener;
      Server : not null Language_Server_Access;
      Data   : Ada.Strings.Unbounded.Unbounded_String) is null;
   --  Called when response message from the server has been processed.

end GPS.LSP_Client.Language_Servers.Interceptors;
