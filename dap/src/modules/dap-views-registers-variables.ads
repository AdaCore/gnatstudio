------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

with DAP.Requests.Variables;
with DAP.Tools;

private package DAP.Views.Registers.Variables is

   type Variables_Request is
     new DAP.Requests.Variables.Variables_DAP_Request
   with record
      Kind : Command_Kind := Update_Registers;
   end record;

   type Variables_Request_Access is access all Variables_Request;

   overriding procedure On_Result_Message
     (Self        : in out Variables_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.VariablesResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);

   procedure Select_Names
     (Self   : in out Variables_Request;
      Result : in out DAP.Tools.VariablesResponse);

   procedure Select_All_Names
     (Self   : in out Variables_Request;
      Result : in out DAP.Tools.VariablesResponse);

   procedure Update_Registers
     (Self   : in out Variables_Request;
      Result : in out DAP.Tools.VariablesResponse);

end DAP.Views.Registers.Variables;
