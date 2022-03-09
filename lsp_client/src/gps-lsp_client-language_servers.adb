------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2022, AdaCore                   --
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

package body GPS.LSP_Client.Language_Servers is

   use type GPS.LSP_Client.Requests.Request_Access;

   ------------
   -- Cancel --
   ------------

   procedure Cancel
     (Self    : in out Abstract_Language_Server;
      Request : in out GPS.LSP_Client.Requests.Request_Access)
   is
      pragma Unreferenced (Self);

   begin
      if Request = null then
         raise Constraint_Error;
      end if;

      --  Stub for language server can't execute any requests, thus reject it.

      Request.On_Rejected;
      GPS.LSP_Client.Requests.Destroy (Request);
   end Cancel;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Self    : in out Abstract_Language_Server;
      Request : in out GPS.LSP_Client.Requests.Request_Access)
   is
      pragma Unreferenced (Self);

   begin
      if Request = null then
         raise Constraint_Error;
      end if;

      --  Stub for language server can't execute any requests, thus reject it.

      Request.On_Rejected;
      GPS.LSP_Client.Requests.Destroy (Request);
   end Execute;

   --------------------------------
   -- Is_Configuration_Supported --
   --------------------------------

   function Is_Configuration_Supported
     (Self    : in out Abstract_Language_Server;
      Setting : GPS.LSP_Client.Configurations.Setting_Kind)
      return Boolean
   is
      pragma Unreferenced (Self, Setting);
   begin
      return False;
   end Is_Configuration_Supported;

end GPS.LSP_Client.Language_Servers;
