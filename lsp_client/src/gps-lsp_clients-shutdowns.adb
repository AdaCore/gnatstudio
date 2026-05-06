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

with VSS.Strings.Conversions;

package body GPS.LSP_Clients.Shutdowns is

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message (Self : in out Shutdown_Request) is
   begin
      Self.Client.On_Exit_Notification;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Shutdown_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value)
   is
      pragma Unreferenced (Code, Data);
   begin
      Self.Client.On_Error
        (VSS.Strings.Conversions.To_UTF_8_String (Message));
      Self.Client.On_Exit_Notification;
   end On_Error_Message;

end GPS.LSP_Clients.Shutdowns;
