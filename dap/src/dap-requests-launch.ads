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

--  "launch" request

with DAP.Tools;

package DAP.Requests.Launch is

   type Launch_DAP_Request is new DAP_Request with private;

   type Launch_DAP_Request_Access is access all Launch_DAP_Request;

   procedure Initialize
     (Self   : in out Launch_DAP_Request;
      Client : DAP.Clients.DAP_Client_Access);

   overriding procedure Write
     (Self   : Launch_DAP_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding procedure On_Result_Message
     (Self        : in out Launch_DAP_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out DAP_Request_Access);

   procedure On_Result_Message
     (Self        : in out Launch_DAP_Request;
      Result      : DAP.Tools.LaunchResponse;
      New_Request : in out DAP_Request_Access);

   overriding procedure On_Rejected (Self : in out Launch_DAP_Request);

   overriding procedure On_Error_Message
     (Self    : in out Launch_DAP_Request;
      Message : VSS.Strings.Virtual_String);

   overriding procedure Set_Seq
     (Self : in out Launch_DAP_Request;
      Id   : Integer);

private

   type Launch_DAP_Request is new DAP_Request with record
      Parameters : aliased DAP.Tools.LaunchRequest :=
        DAP.Tools.LaunchRequest'
          (seq       => 0,
           arguments =>
             (noDebug => False,
              others  => <>));
   end record;

   overriding function Method
     (Self : in out Launch_DAP_Request)
      return String is ("launch");

end DAP.Requests.Launch;
