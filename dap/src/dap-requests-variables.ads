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

--  "variables" request

with DAP.Tools;

package DAP.Requests.Variables is

   -- Variables_DAP_Request --

   type Variables_DAP_Request is abstract new DAP_Request with record
      Parameters : aliased DAP.Tools.VariablesRequest :=
        DAP.Tools.VariablesRequest'
          (seq       => 0,
           arguments => <>);
   end record;

   type Variables_DAP_Request_Access is access all Variables_DAP_Request;

   overriding procedure Write
     (Self   : Variables_DAP_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding procedure On_Result_Message
     (Self        : in out Variables_DAP_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      New_Request : in out DAP_Request_Access);

   procedure On_Result_Message
     (Self        : in out Variables_DAP_Request;
      Result      : in out DAP.Tools.VariablesResponse;
      New_Request : in out DAP_Request_Access) is abstract;

   overriding procedure Set_Seq
     (Self : in out Variables_DAP_Request;
      Id   : Integer);

   overriding function Method
     (Self : in out Variables_DAP_Request)
      return String is ("variables");

end DAP.Requests.Variables;
